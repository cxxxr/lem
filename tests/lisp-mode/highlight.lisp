(defpackage :lem-tests/lisp-mode/highlight
  (:use :cl :rove))
(in-package :lem-tests/lisp-mode/highlight)

(deftest the-highlight-timer-stops-when-it-fails
  ;; The timer and its handle-function are internal; the test calls the
  ;; handle-function as call-timer-function does, with the condition.
  (let ((lem-lisp-mode/highlight::*timer* nil)
        (lem/common/timer::*idle-timer-list* '()))
    (lem-lisp-mode/highlight::init-highlight-timer)
    (let ((timer lem-lisp-mode/highlight::*timer*))
      (ok (null (handler-case
                    (progn
                      (funcall (lem/common/timer::timer-handle-function timer)
                               (make-condition 'simple-error :format-control "x"))
                      nil)
                  (error (e) e))))
      (ok (null lem-lisp-mode/highlight::*timer*))
      (ok (lem/common/timer:timer-expired-p timer)))))
