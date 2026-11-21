(defpackage :lem-tests/lisp-mode/highlight
  (:use :cl :rove))
(in-package :lem-tests/lisp-mode/highlight)

(deftest the-highlight-timer-stops-when-it-fails
  ;; Its timer and lisp-highlight are internal, and so are the idle timer
  ;; lists, the editor's timer manager and timer-last-time, which the test
  ;; binds and sets to run the timer as the editor runs it (with-idle-timers,
  ;; update-idle-timers) outside of a running editor. lisp-highlight is
  ;; replaced by a failing function, so that its handle-function is called
  ;; with the condition.
  (let ((lem-lisp-mode/highlight::*timer* nil)
        (lem/common/timer::*idle-timer-list* '())
        (lem/common/timer::*processed-idle-timer-list* '())
        (lem/common/timer::*timer-manager* (make-instance 'lem-core::lem-timer-manager))
        (original (fdefinition 'lem-lisp-mode/highlight::lisp-highlight)))
    (unwind-protect
         (progn
           (setf (fdefinition 'lem-lisp-mode/highlight::lisp-highlight)
                 (lambda () (error "lisp-highlight failed")))
           (lem-lisp-mode/highlight::init-highlight-timer)
           (let ((timer lem-lisp-mode/highlight::*timer*))
             (let ((signalled (handler-case
                                  (lem/common/timer:with-idle-timers ()
                                    ;; Due now.
                                    (setf (lem/common/timer::timer-last-time timer) 1)
                                    (lem/common/timer:update-idle-timers)
                                    nil)
                                (lem/common/timer:timer-error (e)
                                  (princ-to-string (lem/common/timer:timer-error-condition e))))))
               (testing "the timer's own failure is signalled, not one of its handle-function"
                 (ok (equal "lisp-highlight failed" signalled))))
             (ok (null lem-lisp-mode/highlight::*timer*))
             (lem/common/timer:with-idle-timers ())
             (testing "it does not come back"
               (ok (not (member timer lem/common/timer::*idle-timer-list*)))
               (ok (lem/common/timer:timer-expired-p timer)))))
      (setf (fdefinition 'lem-lisp-mode/highlight::lisp-highlight) original))))
