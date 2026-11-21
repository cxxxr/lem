(defpackage :lem/common/backtrace
  (:use :cl)
  (:export :safe-backtrace)
  (:documentation "A backtrace that is safe to store: one line per frame,
with the frame arguments cut, so that buffer text is not copied whole."))
(in-package :lem/common/backtrace)

(defparameter *max-string-length* 60
  "Characters kept of a string argument.")

(defparameter *max-object-length* 120
  "Characters kept of the printed form of any other argument.")

(defparameter *max-list-length* 5
  "Elements printed of a list argument.")

(defparameter *max-depth* 2
  "Levels of nested lists printed in an argument.")

(defclass limited-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((limit :initarg :limit
          :reader stream-limit
          :documentation "Characters kept; writing one more throws to the stream.")
   (count :initform 0
          :accessor stream-count)
   (column :initform 0
           :accessor stream-column)
   (output :initform (make-string-output-stream)
           :reader stream-output))
  (:documentation "A string output stream that stops the printer once it has
LIMIT characters: (throw stream nil) on the next character. A buffer line's
text in a structure slot is then not printed whole only to be cut."))

(defun one-line (text max)
  "Return TEXT with its newlines written as \\n and cut to MAX characters."
  (let ((text (with-output-to-string (out)
                (loop :for c :across text
                      :do (if (char= c #\Newline)
                              (write-string "\\n" out)
                              (write-char c out))))))
    (if (> (length text) max)
        (concatenate 'string (subseq text 0 max) "...")
        text)))

(defun escape-string (string)
  "Return STRING with backslashes, double quotes and newlines escaped."
  (with-output-to-string (out)
    (loop :for c :across string
          :do (case c
                (#\\ (write-string "\\\\" out))
                (#\" (write-string "\\\"" out))
                (#\Newline (write-string "\\n" out))
                (t (write-char c out))))))

(defmethod trivial-gray-streams:stream-write-char ((stream limited-output-stream) char)
  (when (>= (stream-count stream) (stream-limit stream))
    (throw stream nil))
  (incf (stream-count stream))
  (setf (stream-column stream)
        (if (char= char #\Newline) 0 (1+ (stream-column stream))))
  (write-char char (stream-output stream))
  char)

(defmethod trivial-gray-streams:stream-write-string ((stream limited-output-stream) string
                                                     &optional (start 0) end)
  ;; Character by character: at most LIMIT of them are written.
  (loop :for i :from start :below (or end (length string))
        :do (trivial-gray-streams:stream-write-char stream (char string i)))
  string)

(defmethod trivial-gray-streams:stream-line-column ((stream limited-output-stream))
  ;; Known, so that fresh-line and ~& in a print-object method do not start
  ;; every printed form with a newline.
  (stream-column stream))

(defun print-limited (object limit)
  "Return the first LIMIT characters of the printed form of OBJECT, and
whether it was longer: the printer is stopped there, not run to the end."
  (let ((stream (make-instance 'limited-output-stream :limit limit))
        (whole nil))
    (catch stream
      (prin1 object stream)
      (setf whole t))
    (values (get-output-stream-string (stream-output stream))
            (not whole))))

(defun call-with-printer-settings (function &key length level circle)
  "Call FUNCTION with the printer set as for every frame of safe-backtrace:
the standard settings, whatever the user's init.lisp set (*print-case*,
*print-gensym*...), since the frame names make the incident fingerprint;
symbols printed from the CL package, not readably, and LENGTH, LEVEL and
CIRCLE as *print-length*, *print-level* and *print-circle*."
  (with-standard-io-syntax
    (let ((*package* (find-package :cl))
          (*print-readably* nil)
          (*print-circle* circle)
          (*print-length* length)
          (*print-level* level))
      (funcall function))))

(defun print-object-safely (object)
  "Return the printed form of OBJECT cut to *max-object-length* characters
(longer ones end in \"...\"), or a placeholder if printing it fails: a
print-object method may signal on an object of a half-broken editor."
  (handler-case
      (one-line (call-with-printer-settings
                 (lambda ()
                   ;; One more than is kept, so that one-line sees it is too long.
                   (values (print-limited object (1+ *max-object-length*))))
                 :length *max-list-length*
                 :level *max-depth*
                 ;; Not needed, as the output is limited, and it would
                 ;; print the whole object once to look for shared parts.
                 :circle nil)
                *max-object-length*)
    (error () "#<error printing>")))

(defun print-frame-name (name)
  "Return the frame's function NAME on one line, whole: it is what tells
frames apart (the incident fingerprint is made of these), so it is not cut
and not limited in length or depth, unlike the arguments."
  (one-line (handler-case
                (call-with-printer-settings (lambda () (prin1-to-string name))
                                            :circle t)
              (error () "#<error printing>"))
            most-positive-fixnum))

(defun print-argument (object depth)
  "Return a one-line form of the frame argument OBJECT.
Strings are printed by hand, not by the Lisp printer: SBCL prints long
strings unquoted with their newlines, which cannot be cut afterwards."
  (cond ((stringp object)
         (if (> (length object) *max-string-length*)
             (format nil "\"~A...\" (~D chars)"
                     (escape-string (subseq object 0 *max-string-length*))
                     (length object))
             (format nil "\"~A\"" (escape-string object))))
        ((and (consp object) (< depth *max-depth*) (alexandria:proper-list-p object))
         (format nil "(~{~A~^ ~}~:[~; ...~])"
                 (loop :for element :in object
                       :repeat *max-list-length*
                       :collect (print-argument element (1+ depth)))
                 (> (length object) *max-list-length*)))
        ((consp object) "(...)")
        (t (print-object-safely object))))

(defun safe-backtrace (&key (count 100))
  "Return the backtrace of the current thread as a string, one line per
frame: \"N: (NAME ARG...)\", with names printed in the CL package and the
arguments cut (see print-argument). Call it where the error is signalled,
before the stack unwinds. COUNT frames at most, all of them if it is NIL.
It needs SBCL: elsewhere it returns a line saying so, with no frames, so
that incidents are told apart only by their condition type there. uiop's
backtrace is not a fallback: it prints the arguments whole (buffer text)
and a frame on several lines, which neither the frames nor the fingerprint
can be cut out of."
  #+sbcl
  (with-output-to-string (out)
    (let ((index 0))
      (sb-debug:map-backtrace
       (lambda (frame)
         (multiple-value-bind (name args)
             ;; frame-call is internal to SBCL; uiop's backtrace uses the
             ;; same machinery through sb-debug:print-backtrace.
             (ignore-errors (sb-debug::frame-call frame))
           (format out "~D: (~A~{ ~A~})~%"
                   index
                   (print-frame-name name)
                   (if (listp args)
                       (mapcar (lambda (arg) (print-argument arg 0)) args)
                       '())))
         (incf index))
       :count (or count most-positive-fixnum))))
  #-sbcl
  (progn
    count
    (format nil "(safe-backtrace needs SBCL; no frames on ~A)~%"
            (lisp-implementation-type))))
