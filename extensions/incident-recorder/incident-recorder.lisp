(defpackage :lem-incident-recorder
  (:use :cl :lem)
  (:export :*incident-recording-enabled*
           :*incident-directory*
           :incident-directory
           :record-incident
           :incident-fingerprint
           :recent-commands
           :clear-incident-recorder-state)
  (:documentation "Writes each error that reaches the editor's handlers
(see lem-core:*error-report-hook*) to a JSON file under
<lem-home>/incidents/new/, with the commands that led to it, so that the
errors can be triaged and fixed later (.claude/skills/triage/)."))
(in-package :lem-incident-recorder)

(defvar *incident-recording-enabled* t
  "When NIL, errors are not written to the incident directory.")

(defvar *incident-directory* nil
  "The directory incidents are written under, or NIL for <lem-home>/incidents/.")

(defparameter *max-recent-commands* 50
  "How many of the last commands an incident records.")

(defparameter *max-incidents-per-session* 100
  "No more incidents are written in this session after this many.")

(defparameter *same-fingerprint-interval* 60
  "Seconds during which another error with the same fingerprint is not written again.")

(defparameter *max-message-length* 500
  "Characters kept of the condition's message, which may hold user text.")

(defparameter *max-frame-length* 400
  "Characters kept of each backtrace frame.")

(defparameter *max-backtrace-lines* 120
  "Lines kept of the backtrace.")

(defparameter *fingerprint-frame-count* 6
  "How many of the topmost frames, after the error machinery, the fingerprint uses.")

(defparameter *machinery-frame-regex*
  (ppcre:create-scanner
   (format nil "~{(?:~A)~^|~}"
           '(;; the Lisp, its debugger and uiop's backtrace printer
             "^\\(?(?:SB-[A-Z-]+|UIOP[A-Z/-]*|ASDF[A-Z/-]*)::?"
             ;; "foreign function: call_into_lisp" and the like
             "^\""
             ;; signalling functions
             "^(?:ERROR|CERROR|SIGNAL|WARN|EDITOR-ERROR|LEM[A-Z/-]*::?EDITOR-ERROR)$"
             ;; the editor's handlers and this recorder, also as the
             ;; parent of a (LAMBDA ... :IN ...) or (FLET ... :IN ...) frame
             "(?:BACKTRACE-STRING|SAFE-BACKTRACE|POP-UP-BACKTRACE|BAILOUT|REPORT-ERROR|RECORD-INCIDENT|CALL-TIMER-FUNCTION|CALL-BACKGROUND-JOB)\\)?$"
             ;; the handler of with-error-handler, expanded in these functions
             "^\\(LAMBDA \\(CONDITION\\) :IN LEM-CORE::?(?:TOPLEVEL-)?COMMAND-LOOP\\)$"))
   :case-insensitive-mode t)
  "Frames of the error machinery itself, left out of the fingerprint.")

(defvar *recent-commands* '()
  "Newest first: plists (:command name :keys string :count n).
Written only in the editor thread.")

(defvar *incident-count* 0
  "Incidents written in this session.")

(defvar *last-written-times* (make-hash-table :test 'equal)
  "Fingerprint -> universal time it was last written.")

(defvar *suppressed-counts* (make-hash-table :test 'equal)
  "Fingerprint -> errors not written since the last write, because of
*same-fingerprint-interval*.")

(defvar *recorder-lock* (bt2:make-lock :name "incident recorder")
  "Guards the counters: a crash can be reported from another thread.")

(defun incident-directory ()
  "Return the directory incidents are written under."
  (uiop:ensure-directory-pathname
   (or *incident-directory*
       (merge-pathnames "incidents/" (uiop:ensure-directory-pathname (lem-home))))))

(defun clear-incident-recorder-state ()
  "Forget the recent commands and the per-session counters (for tests)."
  (bt2:with-lock-held (*recorder-lock*)
    (setf *recent-commands* '()
          *incident-count* 0)
    (clrhash *last-written-times*)
    (clrhash *suppressed-counts*)))

(defun recent-commands ()
  "Return the last commands run, oldest first, as plists
(:command name :keys string :count n)."
  (reverse (copy-list *recent-commands*)))

(defun text-key-p (key)
  "Return true if KEY types a character: one character with no modifier
other than shift."
  (and (not (or (key-ctrl key) (key-meta key) (key-super key) (key-hyper key)))
       (= 1 (length (key-sym key)))))

(defun recorded-keys (keys)
  "Return KEYS as a string, or NIL when every key of them types a character.
Many commands run on typed text, not only self-insert (terminal-input,
isearch, completion, skk), so typed text is told by its keys and not by
the command: a password typed in a terminal or a prompt is not recorded."
  (when (and keys (notevery #'text-key-p keys))
    (keyseq-to-string keys)))

(defun record-command ()
  "Add the command about to run to *recent-commands*.
Repeats of the same command and keys are counted instead of listed."
  (let* ((command (this-command))
         (name (if command
                   (princ-to-string (command-name command))
                   "?"))
         (keys (recorded-keys (this-command-keys)))
         (last (first *recent-commands*)))
    (if (and last
             (equal (getf last :command) name)
             (equal (getf last :keys) keys))
        (incf (getf (first *recent-commands*) :count))
        (progn
          (push (list :command name :keys keys :count 1) *recent-commands*)
          (when (> (length *recent-commands*) *max-recent-commands*)
            (setf *recent-commands*
                  (subseq *recent-commands* 0 *max-recent-commands*)))))))

(defun cut-string (string max)
  "Return STRING cut to MAX characters, saying how long it was."
  (if (> (length string) max)
      (format nil "~A... (~D chars)" (subseq string 0 max) (length string))
      string))

(defun sanitize-backtrace (backtrace)
  "Return BACKTRACE with long frames and lines past *max-backtrace-lines* cut.
Its arguments are already cut: it comes from lem/common/backtrace:safe-backtrace,
one line per frame."
  (with-output-to-string (out)
    (with-input-from-string (in (or backtrace ""))
      (loop :for line := (read-line in nil)
            :for count :from 0
            :while (and line (< count *max-backtrace-lines*))
            :do (write-line (cut-string line *max-frame-length*) out)))))

(defun frame-function-name (line)
  "Return the function part of a backtrace LINE such as
\"3: (LEM-CORE::FOO #<BUFFER a> 1)\", or NIL if LINE is not a frame."
  (ppcre:register-groups-bind (rest) ("^\\s*\\d+: \\((.*)$" line)
    (if (and (plusp (length rest)) (char= #\( (char rest 0)))
        ;; (LAMBDA (X) :IN FOO) or (FLET BAR :IN FOO): up to the matching paren
        (loop :with depth := 0
              :for i :from 0 :below (length rest)
              :for c := (char rest i)
              :do (case c
                    (#\( (incf depth))
                    (#\) (decf depth)
                     (when (zerop depth)
                       (return (subseq rest 0 (1+ i))))))
              :finally (return rest))
        (subseq rest 0 (or (position-if (lambda (c) (member c '(#\Space #\))))
                                        rest)
                           (length rest))))))

(defun fnv-1a (string)
  "Return the 32-bit FNV-1a hash of STRING's UTF-8 octets as 8 hex digits.
Used instead of sxhash, which may change between Lisp versions."
  (let ((hash #x811c9dc5))
    (loop :for octet :across (babel:string-to-octets string :encoding :utf-8)
          :do (setf hash (ldb (byte 32 0) (* (logxor hash octet) #x01000193))))
    (format nil "~(~8,'0x~)" hash)))

(defun incident-fingerprint (condition backtrace)
  "Return a short string that is the same for errors of the same type
signalled from the same place: the condition type and the names of the
topmost frames below the error machinery. Frame arguments and the error
message are left out, since they change from one occurrence to the next."
  (let ((names (with-input-from-string (in (sanitize-backtrace backtrace))
                 (loop :for line := (read-line in nil)
                       :while line
                       :for name := (frame-function-name line)
                       :when (and name
                                  (not (ppcre:scan *machinery-frame-regex* name)))
                         :collect name))))
    ;; With *package* bound to KEYWORD every symbol is printed with its
    ;; package, whatever package was current when the error was signalled.
    ;; uiop:print-backtrace already binds *package* to CL for the frames.
    (fnv-1a (format nil "~A~{|~A~}"
                    (let ((*package* (find-package :keyword)))
                      (prin1-to-string (type-of condition)))
                    (subseq names 0 (min *fingerprint-frame-count* (length names)))))))

(defun utc-timestamp (universal-time &key compact)
  "Return UNIVERSAL-TIME as ISO 8601 in UTC, or as YYYYMMDDTHHMMSSZ when COMPACT."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil (if compact
                    "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ"
                    "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ")
            year month day hour minute second)))

(defun safe-string (function)
  "Return FUNCTION's result as a string, or NIL if it signals an error:
an incident is written while the editor may be in a broken state."
  (ignore-errors (let ((value (funcall function)))
                   (when value (princ-to-string value)))))

(defun buffer-context ()
  "Return a hash table describing the current buffer without its text."
  (let ((table (make-hash-table :test 'equal)))
    (ignore-errors
     (let ((buffer (current-buffer)))
       (setf (gethash "major_mode" table)
             (safe-string (lambda () (buffer-major-mode buffer)))
             (gethash "minor_modes" table)
             ;; a vector, since yason writes an empty list as null
             (coerce (ignore-errors (mapcar #'princ-to-string (buffer-minor-modes buffer)))
                     'vector)
             (gethash "line_count" table)
             (ignore-errors (buffer-nlines buffer))
             (gethash "file_type" table)
             (ignore-errors
              (let ((filename (buffer-filename buffer)))
                (when filename (pathname-type filename)))))))
    table))

(defun commands-json ()
  "Return the recent commands as a list of hash tables, oldest first."
  (mapcar (lambda (entry)
            (let ((table (make-hash-table :test 'equal)))
              (setf (gethash "command" table) (getf entry :command)
                    (gethash "keys" table) (getf entry :keys)
                    (gethash "count" table) (getf entry :count))
              table))
          (recent-commands)))

(defun incident-json (condition backtrace context fingerprint time suppressed)
  "Return the incident as a hash table for yason."
  (let ((table (make-hash-table :test 'equal))
        (condition-table (make-hash-table :test 'equal))
        (lem-table (make-hash-table :test 'equal)))
    (setf (gethash "type" condition-table) (safe-string (lambda () (type-of condition)))
          (gethash "message" condition-table)
          (cut-string (or (safe-string (lambda () condition)) "") *max-message-length*))
    (setf (gethash "revision" lem-table) (safe-string #'lem-git-revision)
          (gethash "frontend" lem-table)
          (safe-string (lambda () (type-of (implementation))))
          (gethash "lisp" lem-table)
          (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version))
          (gethash "os" lem-table)
          (format nil "~A ~A" (software-type) (machine-type)))
    (setf (gethash "format_version" table) 1
          (gethash "time" table) (utc-timestamp time)
          (gethash "context" table) (string-downcase (symbol-name context))
          (gethash "fingerprint" table) fingerprint
          (gethash "suppressed_since_last" table) suppressed
          (gethash "thread" table) (safe-string (lambda () (bt2:thread-name (bt2:current-thread))))
          (gethash "condition" table) condition-table
          (gethash "lem" table) lem-table
          (gethash "buffer" table) (buffer-context)
          (gethash "recent_commands" table) (commands-json)
          (gethash "backtrace" table) (sanitize-backtrace backtrace))
    table))

(defun claim-slot (fingerprint time)
  "Decide whether an incident with FINGERPRINT at TIME is written.
Return (values write-p suppressed-count-to-report)."
  (bt2:with-lock-held (*recorder-lock*)
    (let ((last (gethash fingerprint *last-written-times*)))
      (cond ((or (>= *incident-count* *max-incidents-per-session*)
                 (and last (< (- time last) *same-fingerprint-interval*)))
             (incf (gethash fingerprint *suppressed-counts* 0))
             (values nil 0))
            (t
             (incf *incident-count*)
             (setf (gethash fingerprint *last-written-times*) time)
             (values t (prog1 (gethash fingerprint *suppressed-counts* 0)
                         (remhash fingerprint *suppressed-counts*))))))))

(defun write-incident-file (table directory name)
  "Write TABLE as JSON to DIRECTORY/NAME, through a temporary file, so that
a reader never sees a partly written incident. Return the pathname."
  (let ((tmp (merge-pathnames (concatenate 'string "." name ".tmp") directory))
        (path (merge-pathnames name directory)))
    (ensure-directories-exist directory)
    (with-open-file (out tmp :direction :output
                             :if-exists :supersede
                             :external-format :utf-8)
      (yason:encode table (yason:make-json-output-stream out :indent 2)))
    (rename-file tmp path)
    path))

(defun record-incident (condition backtrace context)
  "Write CONDITION as an incident under (incident-directory)/new/ and return
its pathname, or NIL when recording is off or the incident was rate limited.
Meant for lem-core:*error-report-hook*."
  (when *incident-recording-enabled*
    (let* ((time (get-universal-time))
           (fingerprint (incident-fingerprint condition backtrace)))
      (multiple-value-bind (write-p suppressed) (claim-slot fingerprint time)
        (when write-p
          (let* ((directory (merge-pathnames "new/" (incident-directory)))
                 (name (format nil "~A-~A-~A.json"
                               (utc-timestamp time :compact t)
                               fingerprint
                               (string-downcase (symbol-name context))))
                 (path (write-incident-file
                        (incident-json condition backtrace context fingerprint time suppressed)
                        directory
                        name)))
            (ignore-errors (log:error "Incident ~A written to ~A" fingerprint path))
            path))))))

(add-hook *pre-command-hook* 'record-command)
(add-hook *error-report-hook* 'record-incident)
