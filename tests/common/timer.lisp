(defpackage :lem-tests/timer
  (:use :cl
        :rove
        :lem/common/timer))
(in-package :lem-tests/timer)

(defparameter *wait-seconds* 10
  "How long a test waits for a timer's message before failing: a bound on a
broken timer, not on how soon a working one fires, which a loaded machine
can delay by much more than the timer's own interval.")

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

(defclass testing-timer-manager (timer-manager) ())

(defmethod send-timer-notification ((timer-manager testing-timer-manager)
                                    continue)
  (funcall continue))

(defvar *current-time* 0)

(defmethod lem/common/timer::get-microsecond-time ((timer-manager testing-timer-manager))
  *current-time*)

(defmacro with-testing-timer-manager (() &body body)
  `(with-timer-manager (make-instance 'testing-timer-manager)
     (let ((*current-time* 0)
           (lem/common/timer::*idle-timer-list* '()))
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
