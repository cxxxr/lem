(defpackage :lem-tests/incident-recorder
  (:use :cl :rove)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:local-nicknames (:ir :lem-incident-recorder)))
(in-package :lem-tests/incident-recorder)

(defun call-with-recorder (function)
  ;; Each test gets its own incident directory and recorder state, and only
  ;; the recorder on *error-report-hook*, so that running the tests inside a
  ;; Lem leaves its incidents and its hooks as they were.
  (let* ((directory (uiop:ensure-directory-pathname
                     (merge-pathnames (format nil "lem-incident-test-~36R/" (random (expt 36 8)))
                                      (uiop:temporary-directory))))
         (ir:*incident-directory* directory)
         (ir:*incident-recording-enabled* t)
         (lem:*error-report-hook* '())
         (ir::*recent-commands* '())
         (ir::*incident-count* 0)
         (ir::*failed-write-count* 0)
         (ir::*last-written-times* (make-hash-table :test 'equal))
         (ir::*suppressed-counts* (make-hash-table :test 'equal))
         (lem-core::*last-reported-condition* nil))
    ;; registered with add-hook, as the extension does, not bound as a list
    (lem:add-hook lem:*error-report-hook* 'ir:record-incident)
    (unwind-protect (funcall function directory)
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defmacro with-recorder ((directory) &body body)
  `(call-with-recorder (lambda (,directory)
                         (declare (ignorable ,directory))
                         ,@body)))

;; The buffer list is shared between tests (with-fake-interface gives each
;; use its own frames and typeout window, not its own buffer list). Start
;; each test that pops up or runs commands from an empty buffer list.
(defun call-with-isolated-editor (function)
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (funcall function))))

(defmacro with-isolated-editor (() &body body)
  `(call-with-isolated-editor (lambda () ,@body)))

(defun incident-files (directory)
  (directory (merge-pathnames "new/*.json" directory)))

(defun read-incident (pathname)
  (with-open-file (in pathname :external-format :utf-8)
    (yason:parse in)))

(defun signal-and-capture (function)
  "Call FUNCTION, which signals an error, and return the condition and the
backtrace taken where it was signalled, as the editor's handlers do."
  (let (condition backtrace)
    (block nil
      (handler-bind ((error (lambda (c)
                              (setf condition c
                                    backtrace (lem/common/backtrace:safe-backtrace))
                              (return))))
        (funcall function)))
    (values condition backtrace)))

(defun fail-in-place-a (message) (error "a: ~A" message))
(defun fail-in-place-b (message) (error "b: ~A" message))

(deftest fingerprint-ignores-the-message-but-not-the-place
  (flet ((fingerprint (function message)
           (multiple-value-bind (condition backtrace)
               (signal-and-capture (lambda () (funcall function message)))
             (ir:incident-fingerprint condition backtrace))))
    (ok (equal (fingerprint 'fail-in-place-a "one")
               (fingerprint 'fail-in-place-a "two")))
    (ng (equal (fingerprint 'fail-in-place-a "one")
               (fingerprint 'fail-in-place-b "one")))))

(deftest frame-function-name
  (ok (equal "LEM-CORE::FOO" (ir::frame-function-name "3: (LEM-CORE::FOO #<BUFFER a> 1)")))
  (ok (equal "(LAMBDA (X) :IN LEM-CORE::BAR)"
             (ir::frame-function-name "12: ((LAMBDA (X) :IN LEM-CORE::BAR) 1)")))
  (ok (equal "LEM-CORE::BAZ" (ir::frame-function-name "0: (LEM-CORE::BAZ)")))
  (ok (null (ir::frame-function-name "Backtrace for: #<THREAD>")))
  ;; the handler of with-error-handler, exported COMMAND-LOOP or not
  (ok (ppcre:scan ir::*machinery-frame-regex* "(LAMBDA (CONDITION) :IN LEM-CORE:COMMAND-LOOP)"))
  (ok (ppcre:scan ir::*machinery-frame-regex* "(LAMBDA (CONDITION) :IN LEM-CORE::TOPLEVEL-COMMAND-LOOP)"))
  (ok (ppcre:scan ir::*machinery-frame-regex* "LEM/COMMON/BACKTRACE:SAFE-BACKTRACE")))

;; debug 2 keeps the frames (no tail calls) and their arguments
(defun take-text (text)
  (declare (optimize (debug 2)))
  (error "cannot take ~D chars" (length text)))
(defun pass-text (tag text)
  (declare (optimize (debug 2)))
  (take-text text)
  tag)

(deftest long-strings-in-the-backtrace-are-cut
  ;; longer than the ~200 characters SBCL's printer writes unquoted, with
  ;; quotes and lines that look like frames
  (let ((text (format nil "~{~A~%~}"
                      (loop :for i :from 1 :to 40
                            :collect (format nil "12: (line \"~D\"" i)))))
    (multiple-value-bind (condition backtrace)
        (signal-and-capture (lambda () (pass-text "tag" text)))
      (declare (ignore condition))
      (let ((lines (uiop:split-string (string-right-trim '(#\Newline) backtrace)
                                      :separator '(#\Newline))))
        ;; one line per frame: no line of the text is taken as a frame
        (ok (every (lambda (line) (ppcre:scan "^\\d+: \\(" line)) lines))
        (ng (search "line \\\"40" backtrace))
        (ok (search (format nil "(~D chars)" (length text)) backtrace))
        ;; the frames after the string are kept
        (ok (find-if (lambda (line) (search "TAKE-TEXT" line)) lines))
        (ok (find-if (lambda (line) (search "PASS-TEXT \"tag\"" line)) lines))))))

(deftest a-long-message-is-cut
  (with-recorder (directory)
    (multiple-value-bind (condition backtrace)
        (signal-and-capture (lambda () (error "bad: ~A" (make-string 200000 :initial-element #\"))))
      (let ((path (ir:record-incident condition backtrace :command)))
        (ok path)
        (ok (< (length (gethash "message" (gethash "condition" (read-incident path)))) 1000))))))

(deftest error-report-hook-errors-are-dropped
  (let ((called '()))
    (let ((lem:*error-report-hook* '()))
      (lem:add-hook lem:*error-report-hook*
                    (lambda (c b x) (declare (ignore c b x)) (error "broken hook"))
                    1)
      (lem:add-hook lem:*error-report-hook*
                    (lambda (c b x) (declare (ignore b)) (push (list c x) called)))
      (ok (progn (lem-core::report-error (make-condition 'simple-error) "" :command) t))
      (ok (= 1 (length called)))
      (ok (eq :command (second (first called)))))))

(deftest pop-up-backtrace-records-an-incident
  (with-isolated-editor ()
    (with-recorder (directory)
      (block nil
        (handler-bind ((error (lambda (c)
                                (lem:pop-up-backtrace c)
                                (return))))
          (fail-in-place-a "from a command")))
      (let ((files (incident-files directory)))
        (ok (= 1 (length files)))
        (let ((incident (read-incident (first files))))
          (ok (equal "command" (gethash "context" incident)))
          (ok (search "FAIL-IN-PLACE-A" (gethash "backtrace" incident))))))))

(deftest an-incident-file-describes-the-error
  (with-isolated-editor ()
    (with-recorder (directory)
      (lem:execute-key-sequence (list (lem:make-key :sym "s")
                                      (lem:make-key :sym "e")
                                      (lem:make-key :sym "c")))
      (lem:execute-key-sequence (list (lem:make-key :ctrl t :sym "a")))
      (multiple-value-bind (condition backtrace)
          (signal-and-capture (lambda () (fail-in-place-a "detail")))
        (let ((path (ir:record-incident condition backtrace :command)))
          (ok path)
          (ok (equal (list path) (incident-files directory)))
          (let* ((incident (read-incident path))
                 (commands (gethash "recent_commands" incident)))
            (ok (equal "command" (gethash "context" incident)))
            (ok (equal "SIMPLE-ERROR" (gethash "type" (gethash "condition" incident))))
            (ok (equal "a: detail" (gethash "message" (gethash "condition" incident))))
            (ok (equal (ir:incident-fingerprint condition backtrace)
                       (gethash "fingerprint" incident)))
            (ok (search "FAIL-IN-PLACE-A" (gethash "backtrace" incident)))
            ;; three self-inserts become one entry without the typed keys
            (ok (= 2 (length commands)))
            (let ((insert (first commands)))
              (ok (search "SELF-INSERT" (gethash "command" insert)))
              (ok (= 3 (gethash "count" insert)))
              (ok (null (gethash "keys" insert))))
            ;; the keys come from *input-hook*, which keys given to
            ;; execute-key-sequence do not run, so only the command is checked
            (ok (search "MOVE-TO-BEGINNING-OF-LINE" (gethash "command" (second commands))))
            (ok (stringp (gethash "major_mode" (gethash "buffer" incident))))))))))

(deftest the-same-error-is-written-once-per-interval
  (with-recorder (directory)
    (multiple-value-bind (condition backtrace)
        (signal-and-capture (lambda () (fail-in-place-a "again")))
      (ok (ir:record-incident condition backtrace :command))
      (ok (null (ir:record-incident condition backtrace :command)))
      (ok (= 1 (length (incident-files directory))))
      ;; once the interval has passed, the next one says how many were dropped
      (let ((fingerprint (ir:incident-fingerprint condition backtrace)))
        (decf (gethash fingerprint ir::*last-written-times*)
              (1+ ir::*same-fingerprint-interval*)))
      (let ((path (ir:record-incident condition backtrace :command)))
        (ok path)
        (ok (= 1 (gethash "suppressed_since_last" (read-incident path))))))))

(deftest recording-can-be-turned-off
  (with-recorder (directory)
    (let ((ir:*incident-recording-enabled* nil))
      (multiple-value-bind (condition backtrace)
          (signal-and-capture (lambda () (fail-in-place-a "off")))
        (ok (null (ir:record-incident condition backtrace :command)))
        (ok (null (incident-files directory)))))))

(deftest a-timer-error-keeps-the-backtrace-of-the-timer
  (let* ((timer (lem:make-idle-timer (lambda () (fail-in-place-b "in a timer"))
                                     :name "failing timer"))
         (condition (handler-case (progn (lem/common/timer::call-timer-function timer) nil)
                      (lem/common/timer:timer-error (e) e))))
    (ok condition)
    (ok (search "FAIL-IN-PLACE-B" (lem/common/timer:timer-error-backtrace condition)))))

(deftest a-crash-records-an-incident-before-the-editor-exits
  (with-recorder (directory)
    (let ((report
            (block nil
              (handler-bind ((lem-core::exit-editor
                               (lambda (c) (return (lem-core::exit-editor-report c)))))
                (handler-bind ((error #'lem-core::bailout))
                  (fail-in-place-a "crash"))))))
      (ok (search "a: crash" report))
      (ok (search "FAIL-IN-PLACE-A" report))
      (let ((files (incident-files directory)))
        (ok (= 1 (length files)))
        (ok (equal "crash" (gethash "context" (read-incident (first files)))))))))

(deftest typed-keys-are-not-recorded-whatever-the-command
  ;; terminal-input, isearch and completion run on typed text too
  (ok (null (ir::recorded-keys (list (lem:make-key :sym "p") (lem:make-key :sym "w")))))
  (ok (null (ir::recorded-keys (list (lem:make-key :shift t :sym "P")))))
  (ok (equal "C-a" (ir::recorded-keys (list (lem:make-key :ctrl t :sym "a")))))
  (ok (equal "M-x" (ir::recorded-keys (list (lem:make-key :meta t :sym "x")))))
  (ok (null (ir::recorded-keys '()))))

(define-condition test-error (error) ())

(deftest the-fingerprint-does-not-depend-on-the-current-package
  (multiple-value-bind (condition backtrace)
      (signal-and-capture (lambda () (error 'test-error)))
    (ok (equal (let ((*package* (find-package :cl-user)))
                 (ir:incident-fingerprint condition backtrace))
               (let ((*package* (find-package :lem-tests/incident-recorder)))
                 (ir:incident-fingerprint condition backtrace))))))

(deftest a-timer-error-shown-by-its-handle-function-is-recorded-once
  (with-isolated-editor ()
    (with-recorder (directory)
      (let* ((timer (lem:make-idle-timer (lambda () (fail-in-place-b "handled"))
                                         :name "handled timer"
                                         :handle-function (lambda (c) (lem:pop-up-backtrace c))))
             (timer-error (handler-case (progn (lem/common/timer::call-timer-function timer) nil)
                            (lem/common/timer:timer-error (e) e))))
        ;; the command loop shows the timer-error too, as with-error-handler does
        (lem:pop-up-backtrace timer-error)
        (let ((files (incident-files directory)))
          (ok (= 1 (length files)))
          (let ((incident (read-incident (first files))))
            (ok (equal "timer" (gethash "context" incident)))
            (ok (search "FAIL-IN-PLACE-B" (gethash "backtrace" incident)))))))))

(defun incident-test-function-with-a-long-name-for-the-fingerprint-check (text)
  (declare (optimize (debug 3)))
  (labels ((handle-response-callback-with-another-rather-long-name (value)
             (declare (optimize (debug 3)))
             (error "failed on ~A" value)))
    (declare (notinline handle-response-callback-with-another-rather-long-name))
    (handle-response-callback-with-another-rather-long-name text)
    nil))

(deftest a-long-frame-name-is-kept-whole
  (flet ((capture (text)
           (multiple-value-list
            (signal-and-capture
             (lambda ()
               (incident-test-function-with-a-long-name-for-the-fingerprint-check text))))))
    (destructuring-bind (condition-1 backtrace-1) (capture "first argument")
      (destructuring-bind (condition-2 backtrace-2) (capture "another (argument")
        (let ((line (find-if (lambda (line) (search "HANDLE-RESPONSE-CALLBACK" line))
                             (uiop:split-string backtrace-1 :separator '(#\Newline)))))
          (ok (> (length (ir::frame-function-name line)) 120))
          (ok (equal (ir::frame-function-name line)
                     ;; printed in CL, as safe-backtrace prints names
                     (let ((*package* (find-package :cl)))
                       (format nil "(LABELS ~S :IN ~S)"
                               'handle-response-callback-with-another-rather-long-name
                               'incident-test-function-with-a-long-name-for-the-fingerprint-check)))))
        (ok (equal (ir:incident-fingerprint condition-1 backtrace-1)
                   (ir:incident-fingerprint condition-2 backtrace-2)))))))

(deftest parens-in-strings-of-a-frame-name-do-not-count
  (ok (equal "(LAMBDA () :IN \"/a (b.lisp\")"
             (ir::frame-function-name "4: ((LAMBDA () :IN \"/a (b.lisp\") 1 \"x)\")"))))

(deftest frame-names-are-normalized-across-builds
  ;; gensym counters change when a file compiled earlier gains a gensym
  (ok (equal (ir::normalize-frame-name "(LAMBDA (#:CONDITION0) :IN LEM-CORE::BAR)")
             (ir::normalize-frame-name "(LAMBDA (#:CONDITION7) :IN LEM-CORE::BAR)")))
  (ok (equal "(LAMBDA (#:G X) :IN FOO)" (ir::normalize-frame-name "(LAMBDA (#:G123 X) :IN FOO)")))
  ;; a top-level lambda is named by the absolute path of its file: two
  ;; checkouts, each a binary built from its own tree, give the same name
  (flet ((in-checkout (root)
           (let ((ir::*lem-source-root* root))
             (ir::normalize-frame-name
              (format nil "(LAMBDA () :IN \"~Asrc/lem.lisp\")" root)))))
    (ok (equal (in-checkout "/home/a/src/lem/") (in-checkout "/tmp/worktree-1234/")))
    (ok (equal "(LAMBDA () :IN \"src/lem.lisp\")" (in-checkout "/home/a/src/lem/"))))
  ;; a file outside the tree (init.lisp) keeps its file name only
  (let ((ir::*lem-source-root* "/home/a/src/lem/"))
    (ok (equal "(LAMBDA (BUFFER) :IN \"init.lisp\")"
               (ir::normalize-frame-name "(LAMBDA (BUFFER) :IN \"/home/a/.lem/init.lisp\")")))))

;; a lambda whose lambda list has a gensym, running when the error is signalled
(defun call-with-value (function)
  (declare (optimize (debug 3)))
  (funcall function 1)
  nil)

(defmacro with-gensym-lambda (() &body body)
  (let ((value (gensym "CONDITION")))
    `(call-with-value (lambda (,value)
                        (declare (ignore ,value) (optimize (debug 3)))
                        ,@body
                        nil))))

(deftest a-gensym-lambda-frame-gives-a-stable-fingerprint
  (multiple-value-bind (condition backtrace)
      (signal-and-capture
       (lambda () (with-gensym-lambda () (fail-in-place-a "gensym"))))
    (let ((line (find-if (lambda (line) (search "(#:CONDITION" line))
                         (uiop:split-string backtrace :separator '(#\Newline)))))
      ;; the frame of the macro's lambda is in the backtrace with its gensym
      (ok line)
      (let ((other (ppcre:regex-replace "#:CONDITION[0-9]+" backtrace "#:CONDITION99999")))
        (ok (equal (ir:incident-fingerprint condition backtrace)
                   (ir:incident-fingerprint condition other)))))))

(deftest machinery-frames-are-matched-by-their-full-names
  (dolist (name '("LEM-CORE::REPORT-ERROR"
                  "LEM-CORE:POP-UP-BACKTRACE"
                  "LEM/COMMON/TIMER::CALL-TIMER-FUNCTION"
                  "LEM-INCIDENT-RECORDER:RECORD-INCIDENT"
                  "LEM/BUFFER/ERRORS:EDITOR-ERROR"
                  "(LAMBDA (C) :IN LEM-CORE::CALL-BACKGROUND-JOB)"
                  "(LAMBDA (CONDITION) :IN LEM/COMMON/TIMER::CALL-TIMER-FUNCTION)"
                  "(LAMBDA () :IN (LAMBDA (C) :IN LEM-CORE::CALL-BACKGROUND-JOB))"))
    (ok (ppcre:scan ir::*machinery-frame-regex* name) name))
  ;; other functions whose names end the same way are where the error is
  (dolist (name '("CLYSM-UI::REPORT-ERROR"
                  "LEM-LSP-MODE::LSP-REPORT-ERROR"
                  "MY-MODE::BAILOUT"
                  "(LAMBDA () :IN MY-MODE::CALL-BACKGROUND-JOB)"
                  "MY-MODE::EDITOR-ERROR"))
    (ng (ppcre:scan ir::*machinery-frame-regex* name) name)))

(deftest frame-names-with-parens-in-characters-and-bars
  (ok (equal "(:METHOD LEM-CORE::FOO ((EQL #\\()))"
             (ir::frame-function-name "3: ((:METHOD LEM-CORE::FOO ((EQL #\\())) #\\( 1)")))
  (ok (equal "(:METHOD LEM-CORE::FOO ((EQL #\\))))"
             (ir::frame-function-name "3: ((:METHOD LEM-CORE::FOO ((EQL #\\)))) #\\) 1)")))
  (ok (equal "(LAMBDA () :IN LEM-CORE::|a(b|)"
             (ir::frame-function-name "3: ((LAMBDA () :IN LEM-CORE::|a(b|) 1)")))
  (ok (equal "LEM-CORE::|a b)c|"
             (ir::frame-function-name "3: (LEM-CORE::|a b)c| 1)"))))

(deftest gensyms-in-flet-names-and-bars-are-normalized
  (ok (equal (ir::normalize-frame-name "(FLET \"THUNK0\" :IN LEM-CORE::FOO)")
             (ir::normalize-frame-name "(FLET \"THUNK17\" :IN LEM-CORE::FOO)")))
  (ok (equal (ir::normalize-frame-name "(LABELS \"G0\" :IN LEM-CORE::FOO)")
             (ir::normalize-frame-name "(LABELS \"G12\" :IN LEM-CORE::FOO)")))
  (ok (equal (ir::normalize-frame-name "(LAMBDA (#:|params12|) :IN FOO)")
             (ir::normalize-frame-name "(LAMBDA (#:|params104|) :IN FOO)")))
  ;; a path in :IN keeps its digits
  (ok (search "lem2.lisp" (ir::normalize-frame-name "(LAMBDA () :IN \"/x/lem2.lisp\")"))))

(deftest windows-paths-are-made-relative-to-the-lem-tree
  (let ((ir::*lem-source-root* "C:\\Users\\a\\lem\\"))
    (ok (equal "(LAMBDA () :IN \"src/lem.lisp\")"
               (ir::normalize-frame-name
                "(LAMBDA () :IN \"C:\\\\Users\\\\a\\\\lem\\\\src\\\\lem.lisp\")")))
    (ok (equal "(LAMBDA () :IN \"init.lisp\")"
               (ir::normalize-frame-name
                "(LAMBDA () :IN \"C:\\\\Users\\\\a\\\\.lem\\\\init.lisp\")")))))

(deftest the-condition-type-in-the-fingerprint-does-not-depend-on-the-print-variables
  ;; only the type is printed here; safe-backtrace prints the frame names
  (multiple-value-bind (condition backtrace)
      (signal-and-capture (lambda () (error 'test-error)))
    (let ((fingerprint (ir:incident-fingerprint condition backtrace)))
      (ok (equal fingerprint
                 (let ((*print-case* :downcase)
                       (*print-gensym* nil)
                       (*readtable* (copy-readtable nil)))
                   (setf (readtable-case *readtable*) :invert)
                   (ir:incident-fingerprint condition backtrace)))))))

(deftest space-return-tab-and-backspace-are-not-recorded
  (dolist (sym '("Space" "Return" "Tab" "Backspace" "Delete"))
    (ok (null (ir::recorded-keys (list (lem:make-key :sym "p") (lem:make-key :sym sym))))
        sym))
  ;; some frontends keep shift on these keys while a capital is typed
  (dolist (sym '("Space" "Return" "Backspace"))
    (ok (null (ir::recorded-keys (list (lem:make-key :shift t :sym sym)))) sym))
  ;; a terminal sends C-h for Backspace, but C-h f is a help command
  (ok (null (ir::recorded-keys (list (lem:make-key :ctrl t :sym "h")))))
  (ok (equal "C-h f" (ir::recorded-keys (list (lem:make-key :ctrl t :sym "h")
                                              (lem:make-key :sym "f")))))
  ;; Shift-Tab and Shift-Delete run commands, they type nothing
  (ok (equal "Shift-Tab" (ir::recorded-keys (list (lem:make-key :shift t :sym "Tab")))))
  (ok (equal "Shift-Delete" (ir::recorded-keys (list (lem:make-key :shift t :sym "Delete")))))
  (ok (equal "C-x Return" (ir::recorded-keys (list (lem:make-key :ctrl t :sym "x")
                                                   (lem:make-key :sym "Return"))))))

(deftest recent-commands-is-an-empty-array-when-empty
  (with-recorder (directory)
    (multiple-value-bind (condition backtrace)
        (signal-and-capture (lambda () (fail-in-place-a "early")))
      (let ((path (ir:record-incident condition backtrace :command)))
        (ok (search "\"recent_commands\": []"
                    (uiop:read-file-string path :external-format :utf-8)))))))

(defun fail-with-type-error (x) (error 'type-error :datum x :expected-type 'integer))
(defun fail-in-a-timer (kind)
  (if (eq kind :type) (fail-with-type-error "x") (fail-in-place-b "simple")))

(deftest a-timer-error-is-recorded-as-the-error-it-wraps
  (with-recorder (directory)
    (flet ((record (kind)
             (let* ((timer (lem:make-idle-timer (lambda () (fail-in-a-timer kind))
                                                :name "failing timer"))
                    (timer-error (handler-case (progn (lem/common/timer::call-timer-function timer) nil)
                                   (lem/common/timer:timer-error (e) e))))
               (read-incident (ir:record-incident timer-error
                                                  (lem/common/timer:timer-error-backtrace timer-error)
                                                  :timer)))))
      (let ((type (record :type))
            (simple (record :simple)))
        (ok (equal "TYPE-ERROR" (gethash "type" (gethash "condition" type))))
        (ok (equal "SIMPLE-ERROR" (gethash "type" (gethash "condition" simple))))
        ;; the message is the timer-error's, which names the timer
        (ok (search "failing timer" (gethash "message" (gethash "condition" simple))))
        (ok (search "b: simple" (gethash "message" (gethash "condition" simple))))
        (ng (equal (gethash "fingerprint" type) (gethash "fingerprint" simple)))))))

(deftest a-failed-write-is-counted-as-dropped
  (with-recorder (directory)
    (multiple-value-bind (condition backtrace)
        (signal-and-capture (lambda () (fail-in-place-a "disk full")))
      ;; new/ is a file, so the incident cannot be written
      (ensure-directories-exist directory)
      (with-open-file (out (merge-pathnames "new" directory) :direction :output))
      (ok (handler-case (progn (ir:record-incident condition backtrace :command) nil)
            (error () t)))
      (delete-file (merge-pathnames "new" directory))
      ;; the slot stays taken: a directory that cannot be written is not
      ;; tried again on every error
      (ok (null (ir:record-incident condition backtrace :command)))
      (let ((fingerprint (ir:incident-fingerprint condition backtrace)))
        (decf (gethash fingerprint ir::*last-written-times*)
              (1+ ir::*same-fingerprint-interval*)))
      ;; the next one written counts the failed one and the suppressed one
      (let ((path (ir:record-incident condition backtrace :command)))
        (ok path)
        (ok (= 2 (gethash "suppressed_since_last" (read-incident path))))
        ;; only the written one counts toward *max-incidents-per-session*
        (ok (= 1 ir::*incident-count*))))))

(deftest failed-writes-stop-after-a-few
  (with-recorder (directory)
    (let ((ir::*max-failed-writes-per-session* 2))
      ;; new/ is a file, so no incident can be written
      (ensure-directories-exist directory)
      (with-open-file (out (merge-pathnames "new" directory) :direction :output))
      (dolist (function '(fail-in-place-a fail-in-place-b))
        (multiple-value-bind (condition backtrace)
            (signal-and-capture (lambda () (funcall function "x")))
          (ok (handler-case (progn (ir:record-incident condition backtrace :command) nil)
                (error () t)))))
      ;; a third fingerprint is not tried
      (multiple-value-bind (condition backtrace)
          (signal-and-capture (lambda () (error 'test-error)))
        (ok (null (ir:record-incident condition backtrace :command)))))))

(deftest the-type-in-an-incident-is-printed-the-standard-way
  (with-recorder (directory)
    (multiple-value-bind (condition backtrace)
        (signal-and-capture (lambda () (error 'test-error)))
      (let ((path (let ((*print-case* :downcase))
                    (ir:record-incident condition backtrace :command))))
        (ok (equal "LEM-TESTS/INCIDENT-RECORDER::TEST-ERROR"
                   (gethash "type" (gethash "condition" (read-incident path)))))))))

(deftest a-failed-write-leaves-no-temporary-file
  (with-recorder (directory)
    (let ((table (make-hash-table :test 'equal)))
      (setf (gethash "a" table) 1)
      ;; x.json is a directory with a file in it, so the rename fails after
      ;; the temporary file is written
      (ensure-directories-exist (merge-pathnames "x.json/y" directory))
      (with-open-file (out (merge-pathnames "x.json/y" directory) :direction :output))
      (ok (handler-case (progn (ir::write-incident-file table directory "x.json") nil)
            (error () t)))
      (ok (null (probe-file (merge-pathnames ".x.json.tmp" directory)))))))

(deftest a-long-frame-keeps-its-name-and-loses-its-arguments
  (let* ((name (format nil "(LAMBDA () :IN \"/~A/lem.lisp\")" (make-string 500 :initial-element #\a)))
         (line (format nil "3: (~A ~A)" name (make-string 500 :initial-element #\b)))
         (saved (string-right-trim '(#\Newline) (ir::sanitize-backtrace line))))
    (ok (search name saved))
    (ok (< (length saved) (+ (length name) 100)))
    ;; the length of the whole frame
    (ok (search (format nil "(~D chars)" (length line)) saved))))
