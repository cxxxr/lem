(defpackage :lem-tests/backtrace
  (:use :cl :rove)
  (:local-nicknames (:bt :lem/common/backtrace)))
(in-package :lem-tests/backtrace)

(defclass endless-printer () ()
  (:documentation "An object whose printed form goes on and then fails, as a
structure with a buffer's whole text in a slot would take long to print."))

(defmethod print-object ((object endless-printer) stream)
  (write-string (make-string 1000 :initial-element #\a) stream)
  (error "Printed too far"))

(deftest an-argument-is-printed-only-up-to-its-limit
  (let ((printed (bt::print-argument (make-instance 'endless-printer) 0)))
    (testing "the printer is stopped at the limit, before the failure"
      (ok (string= (concatenate 'string
                                (make-string bt::*max-object-length* :initial-element #\a)
                                "...")
                   printed)))))

(defstruct holder text)

(deftest a-structure-with-a-long-string-is-cut
  (let ((printed (bt::print-argument
                  (make-holder :text (format nil "~{~A~%~}" (loop :repeat 10000 :collect "line")))
                  0)))
    (ok (<= (length printed) (+ bt::*max-object-length* 3)))
    (ok (not (find #\Newline printed)))))

(deftest frame-names-are-printed-the-same-whatever-the-printer-settings
  (let ((name (make-symbol "FRAME-NAME")))
    (let ((*print-case* :downcase)
          (*print-gensym* nil)
          (*print-escape* nil))
      (ok (string= "#:FRAME-NAME" (bt::print-frame-name name))))))

(deftest a-printed-backtrace-is-cut-line-by-line
  (let ((cut (bt::cut-printed-backtrace
              (format nil "0: (FOO ~A)~%~%   ~%1: (BAR)~%"
                      (make-string 1000 :initial-element #\x)))))
    (ok (= 2 (count #\Newline cut)))
    (ok (string= "1: (BAR)" (second (uiop:split-string (string-right-trim '(#\Newline) cut)
                                                     :separator '(#\Newline)))))
    (ok (< (length cut) 500))))

(defun recurse-then-capture (depth count)
  (if (zerop depth)
      (bt:safe-backtrace :count count)
      ;; Not a tail call, so that each level is a frame.
      (values (recurse-then-capture (1- depth) count) depth)))

(deftest every-frame-is-printed-when-the-count-is-nil
  (flet ((frames (text) (count #\Newline text)))
    (ok (= 100 (frames (recurse-then-capture 150 100))))
    (ok (< 150 (frames (recurse-then-capture 150 nil))))))

(deftest a-circular-argument-is-printed
  (let ((circular (list 1 2 3)))
    (setf (cdr (last circular)) circular)
    (let ((printed (bt::print-argument (make-holder :text circular) 0)))
      (ok (<= (length printed) (+ bt::*max-object-length* 3))))))
