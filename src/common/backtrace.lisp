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

(defun print-object-safely (object)
  "Return the printed form of OBJECT, or a placeholder if printing it fails:
a print-object method may signal on an object of a half-broken editor."
  (handler-case
      (let ((*print-pretty* nil)
            (*print-readably* nil)
            (*print-circle* t)
            (*print-length* *max-list-length*)
            (*print-level* *max-depth*))
        (prin1-to-string object))
    (error () "#<error printing>")))

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
        (t (one-line (print-object-safely object) *max-object-length*))))

(defun safe-backtrace (&key (count 100))
  "Return the backtrace of the current thread as a string, one line per
frame: \"N: (NAME ARG...)\", with names printed in the CL package and the
arguments cut (see print-argument). Call it where the error is signalled,
before the stack unwinds."
  (with-output-to-string (out)
    #+sbcl
    (let ((*package* (find-package :cl))
          (index 0))
      (sb-debug:map-backtrace
       (lambda (frame)
         (multiple-value-bind (name args)
             ;; frame-call is internal to SBCL; uiop's backtrace uses the
             ;; same machinery through sb-debug:print-backtrace.
             (ignore-errors (sb-debug::frame-call frame))
           (format out "~D: (~A~{ ~A~})~%"
                   index
                   (one-line (print-object-safely name) *max-object-length*)
                   (if (listp args)
                       (mapcar (lambda (arg) (print-argument arg 0)) args)
                       '())))
         (incf index))
       :count count))
    #-sbcl
    (write-string "(backtrace not available on this Lisp)" out)))
