(defpackage :lem-tests/timer
  (:use :cl
        :rove
        :lem/common/timer))
(in-package :lem-tests/timer)

(defparameter *wait-seconds* 10
  "How long a test waits for a timer's message before failing: a bound on a
broken timer, not on how soon a working one fires, which a loaded machine
can delay by much more than the timer's own interval.")

(defvar *current-time* 0)

(defclass testing-timer-manager (timer-manager) ())

(defmethod send-timer-notification ((timer-manager testing-timer-manager)
                                    continue)
  (funcall continue))

(defclass named-timer-manager (timer-manager)
  ((name :initarg :name :reader named-timer-manager-name)
   (mailbox :initarg :mailbox :reader named-timer-manager-mailbox))
  (:documentation "A timer manager that sends its name to MAILBOX for each
notification, so that a test sees which manager a timer was given."))

(defmethod send-timer-notification ((timer-manager named-timer-manager) continue)
  (lem-mailbox:send-message (named-timer-manager-mailbox timer-manager)
                            (named-timer-manager-name timer-manager))
  (funcall continue))

(defclass forwarding-timer-manager (timer-manager)
  ((target :initarg :target :reader forwarding-timer-manager-target))
  (:documentation "A timer manager that sends its notifications through TARGET
(the editor's, when the tests run in a Lem), or runs them if TARGET is nil."))

(defmethod send-timer-notification ((timer-manager forwarding-timer-manager) continue)
  (let ((target (forwarding-timer-manager-target timer-manager)))
    (if target
        (send-timer-notification target continue)
        (funcall continue))))

(defun testing-timer ())

;;TODO: These tests do work indeed with CCL but for some reason when launching the tests
;; pragmatically, it triggers an error. Fix this.
(deftest timer-name-tests
  (let* ((timer (make-timer 'testing-timer))
         (value (timer-name timer)))
    (ok (string= "TESTING-TIMER" value)))
  (let* ((timer (make-timer 'testing-timer))
         (value (timer-name timer)))
    (ok (string= "TESTING-TIMER" value)))
  (let* ((timer (make-timer
		 #+sbcl
		 (sb-int:named-lambda hello ())
		 #-sbcl
		 (alexandria:named-lambda hello ())
		 ))
         (value (timer-name timer)))
    (ok (string= "HELLO" value)))
  (let* ((timer (make-timer 'testing-timer :name "foo"))
         (value (timer-name timer)))
    (ok (string= "foo" value))))

(defmethod lem/common/timer::get-microsecond-time ((timer-manager testing-timer-manager))
  *current-time*)

(defmacro with-testing-timer-manager (() &body body)
  ;; Both lists of idle timers are bound: stop-idle-timers puts the processed
  ;; ones back in the list, and the global ones are a running Lem's.
  `(with-timer-manager (make-instance 'testing-timer-manager)
     (let ((*current-time* 0)
           (lem/common/timer::*idle-timer-list* '())
           (lem/common/timer::*processed-idle-timer-list* '()))
       ,@body)))

(defun receive (mailbox)
  "Return the next message of MAILBOX and whether one came within *wait-seconds*."
  (lem-mailbox::receive-message mailbox :timeout *wait-seconds*))

(defun no-message-p (mailbox)
  "Return true if MAILBOX has no message now."
  (not (nth-value 1 (lem-mailbox::receive-message-no-hang mailbox))))

(defun join-timer-thread (timer)
  ;; The thread of a timer is internal; when it is done, the timer sends no
  ;; more messages, which is what the tests check (rather than waiting a time
  ;; for a message that should not come).
  (bt2:join-thread (lem/common/timer::timer-thread timer)))

(deftest simple-timer-test
  ;; No check here depends on how soon the timer thread runs: messages that
  ;; should come are waited for, and that no more come is seen once the
  ;; timer's thread is done.
  (with-testing-timer-manager ()
    (testing "a timer that does not repeat runs once"
      (let* ((mailbox (lem-mailbox::make-mailbox))
             (timer (make-timer (lambda () (lem-mailbox::send-message mailbox t)))))
        (start-timer timer 2)
        (ok (nth-value 1 (receive mailbox)))
        (join-timer-thread timer)
        (ok (no-message-p mailbox))))

    (testing "a repeating timer runs until it is stopped"
      (let* ((mailbox (lem-mailbox::make-mailbox))
             (timer (make-timer (lambda () (lem-mailbox::send-message mailbox t)))))
        (start-timer timer 2 :repeat t)
        (ok (loop :repeat 2
                  :always (multiple-value-bind (value received) (receive mailbox)
                            (and received (eq value t)))))
        (stop-timer timer)
        ;; The thread may have sent messages before it saw the stop; that it
        ;; ends is what shows that no more come.
        (join-timer-thread timer)
        (ok (timer-expired-p timer))))))

(deftest compute-the-time-for-the-next-idle-timer-to-be-called
  (with-testing-timer-manager ()
    (ok (null (get-next-timer-timing-ms))))
  (with-testing-timer-manager ()
    (let ((timer1 (make-idle-timer (lambda ()) :name "idle-timer-1"))
          (timer2 (make-idle-timer (lambda ()) :name "idle-timer-2")))
      (start-timer timer1 10)
      (start-timer timer2 20)
      (with-idle-timers ()
        (ok (= 10 (get-next-timer-timing-ms))))
      (with-idle-timers ()
        (setf *current-time* 2)
        (ok (= 8 (get-next-timer-timing-ms))))
      (with-idle-timers ()
        (setf *current-time* 12)
        (ok (= 0 (get-next-timer-timing-ms))))
      (with-idle-timers ()
        (setf *current-time* 30)
        (ok (= -8 (get-next-timer-timing-ms)))))))

(deftest an-idle-timer-started-outside-of-idle-time-waits-for-it
  (with-testing-timer-manager ()
    (let ((ran nil))
      ;; Not in with-idle-timers: it has no time to count from yet.
      (start-timer (make-idle-timer (lambda () (setf ran t)) :name "not yet") 10)
      (setf *current-time* 100)
      (ok (null (get-next-timer-timing-ms)))
      (ok (not (update-idle-timers)))
      (ok (not ran))
      (testing "it counts from the next idle time"
        (with-idle-timers ()
          (ok (= 10 (get-next-timer-timing-ms))))))))

(deftest an-idle-timer-started-again-outside-of-idle-time-waits-for-it
  (with-testing-timer-manager ()
    (let ((timer (make-idle-timer (lambda ()) :name "started again")))
      (with-idle-timers ()
        (start-timer timer 10))
      (stop-timer timer)
      (setf *current-time* 100)
      (start-timer timer 10)
      (testing "its time of the earlier idle time is not counted from"
        (ok (null (get-next-timer-timing-ms)))))))

(deftest a-repeating-idle-timer-stopped-by-its-handle-function-stays-stopped
  ;; The idle timer lists are internal: the timer package exports no way to
  ;; list the idle timers.
  (with-testing-timer-manager ()
    (let ((timer nil))
      (setf timer (make-idle-timer (lambda () (error "An idle timer failed"))
                                   :name "stopping on failure"
                                   :handle-function (lambda (condition)
                                                      (declare (ignore condition))
                                                      (stop-timer timer))))
      (start-timer timer 10 :repeat t)
      (ignore-errors
       (with-idle-timers ()
         (setf *current-time* 100)
         (update-idle-timers)))
      (with-idle-timers ())
      (ok (not (member timer lem/common/timer::*idle-timer-list*)))
      (ok (not (member timer lem/common/timer::*processed-idle-timer-list*)))
      (ok (timer-expired-p timer)))))

(defun manager-of-a-timer-started-now (mailbox)
  "Start a timer in this thread and return the name of the manager that its
notification went through."
  (let ((timer (make-timer (lambda ()))))
    (start-timer timer 1)
    (multiple-value-prog1 (receive mailbox)
      (join-timer-thread timer))))

(deftest with-timer-manager-is-a-binding
  (let* ((mailbox (lem-mailbox:make-mailbox))
         (outer (make-instance 'named-timer-manager :name :outer :mailbox mailbox))
         (inner (make-instance 'named-timer-manager :name :inner :mailbox mailbox)))
    (with-timer-manager outer
      (with-timer-manager inner
        (ok (eq :inner (manager-of-a-timer-started-now mailbox)))
        (testing "threads are given only the innermost one"
          ;; (Clysm's bordeaux-threads binds every entry, the last one last.)
          (ok (= 1 (count 'lem/common/timer::*timer-manager*
                          bt2:*default-special-bindings* :key #'car)))))
      (testing "the manager before is back after it"
        (ok (eq :outer (manager-of-a-timer-started-now mailbox))))
      (testing "also when it is left by a non-local exit"
        (catch 'out
          (with-timer-manager inner
            (throw 'out nil)))
        (ok (eq :outer (manager-of-a-timer-started-now mailbox))))
      (testing "a thread made in it starts timers with its manager"
        (ok (eq :outer
                (bt2:join-thread
                 (bt2:make-thread
                  (lambda () (manager-of-a-timer-started-now mailbox))))))
        (with-timer-manager inner
          (ok (eq :inner
                  (bt2:join-thread
                   (bt2:make-thread
                    (lambda () (manager-of-a-timer-started-now mailbox)))))))))))

;; The global value of the manager is read and restored through the internal
;; *timer-manager*: nothing exported reads it, as start-timer only hands it to
;; a timer.
#+sbcl
(defun global-timer-manager ()
  "Return the global value of the timer manager and whether it has one."
  (handler-case
      (values (sb-ext:symbol-global-value 'lem/common/timer::*timer-manager*) t)
    (unbound-variable () (values nil nil))))

#+sbcl
(defun call-with-saved-global-timer-manager (function)
  "Call FUNCTION with a function that makes a manager to set globally, and
put the global manager back afterwards. The managers made forward to the
global one before, so that a running Lem that the tests are run in (whose
editor reads the global manager) goes on as before."
  (multiple-value-bind (saved boundp) (global-timer-manager)
    (unwind-protect
         (funcall function
                  (lambda ()
                    (make-instance 'forwarding-timer-manager :target saved)))
      (if boundp
          (set-global-timer-manager saved)
          (bt2:join-thread
           (bt2:make-thread
            (lambda ()
              (sb-ext:without-package-locks
                (makunbound 'lem/common/timer::*timer-manager*)))
            :initial-bindings nil))))))

(defun timer-manager-of-a-new-thread (&rest make-thread-arguments)
  "Return the timer manager that a thread made now reads."
  (bt2:join-thread
   (apply #'bt2:make-thread
          (lambda () lem/common/timer::*timer-manager*)
          make-thread-arguments)))

#+sbcl
(deftest set-global-timer-manager-sets-the-global-value
  (call-with-saved-global-timer-manager
   (lambda (make-manager)
     (let ((global (funcall make-manager))
           (bound (make-instance 'testing-timer-manager)))
       (with-timer-manager bound
         (testing "also when called in with-timer-manager"
           (set-global-timer-manager global)
           (ok (eq bound lem/common/timer::*timer-manager*))
           (ok (eq global (timer-manager-of-a-new-thread :initial-bindings nil)))))))))

#+sbcl
(deftest without-timer-manager-binding-gives-threads-the-global-manager
  (call-with-saved-global-timer-manager
   (lambda (make-manager)
     (let ((global (funcall make-manager))
           (next-global (funcall make-manager))
           (bound (make-instance 'testing-timer-manager)))
       (set-global-timer-manager global)
       (with-timer-manager bound
         (without-timer-manager-binding ()
           (ok (eq global (timer-manager-of-a-new-thread)))
           (testing "a thread made by such a thread reads the global manager of now"
             ;; As a thread that the editor makes, after the editor is
             ;; started again.
             (ok (eq next-global
                     (bt2:join-thread
                      (bt2:make-thread
                       (lambda ()
                         (set-global-timer-manager next-global)
                         (timer-manager-of-a-new-thread)))))))))))))

(deftest only-the-editors-manager-sends-to-the-editor
  ;; A manager of a test that does not say what to do with the notifications
  ;; must not run its timers in a running Lem's editor thread.
  (flet ((has-a-method-p (timer-manager)
           (not (null (compute-applicable-methods
                       #'send-timer-notification
                       (list timer-manager (lambda ())))))))
    (ok (has-a-method-p (make-instance 'lem-core::lem-timer-manager)))
    (ok (not (has-a-method-p (make-instance 'timer-manager))))))
