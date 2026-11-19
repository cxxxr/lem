;;;; single-thread.lisp — Lem in the one thread of a page
;;;;
;;;; Lem runs its editor in a thread of its own, which waits for events in
;;;; a queue that other threads (the input of the frontend, timers) fill.
;;;; A page has one thread: Lem's threads (the editor, the one of the Lisp
;;;; mode that waits for micros's messages) and micros's are Clysm's green
;;;; threads, computations that take turns when they wait (with JavaScript
;;;; Promise Integration).  Where the editor would wait for the queue it
;;;; waits with JS:AWAIT for a promise that is resolved when an event is put
;;;; into the queue, or when the time to wait is over.  Meanwhile the page
;;;; runs: the listeners of the keyboard put the keys into the queue, the UI
;;;; framework updates the screen.  Timers are JavaScript timers that put
;;;; their functions into the queue.

(in-package :lem-clysm)

;; (functions of the page: made again in an image started from a heap image)
(sys::%defvar-host *set-timeout* (js:global "setTimeout"))
(sys::%defvar-host *clear-timeout* (js:global "clearTimeout"))
(sys::%defvar-host *set-interval* (js:global "setInterval"))
(sys::%defvar-host *clear-interval* (js:global "clearInterval"))

;;; Waiting for an event

(defvar *wake-editor* nil
  "The function that resolves the promise the editor waits for, or NIL.")

(sys::%defvar-host *make-wait-promise*
  (js:eval "(ms, setWake) => new Promise((resolve) => {
              setWake(resolve);
              if (ms !== null) setTimeout(() => resolve(null), ms);
            })"))

(defun wake-editor ()
  (let ((wake *wake-editor*))
    (when wake
      (setf *wake-editor* nil)
      (js:funcall wake nil))))

(defun wait-for-event (seconds)
  "Wait until an event is sent or SECONDS (NIL: no limit) have passed."
  (js:await (js:funcall *make-wait-promise*
                        (and seconds (max 0 (round (* seconds 1000))))
                        (lambda (resolve) (setf *wake-editor* resolve) nil))))

(defmethod lem/common/queue:dequeue ((queue lem/common/queue::concurrent-queue)
                                     &key timeout timeout-value)
  (let ((events (lem/common/queue::concurrent-queue-queue queue))
        (deadline (and timeout
                       (+ (get-internal-real-time)
                          (* timeout internal-time-units-per-second)))))
    (loop
      (unless (lem/common/queue:empty-p events)
        (return (lem/common/queue:dequeue events)))
      (let ((remaining (and deadline
                            (/ (- deadline (get-internal-real-time))
                               internal-time-units-per-second))))
        ;; where the computation cannot wait (not the editor), the queue is
        ;; only looked at
        (when (or (and remaining (<= remaining 0))
                  (not (js:can-await-p)))
          (return timeout-value))
        (wait-for-event remaining)))))

(defmethod lem/common/queue:enqueue :after ((queue lem/common/queue::concurrent-queue) object)
  (declare (ignore object))
  (wake-editor))

;;; Timers: a JavaScript timer sends the timer's function to the editor.

(defvar *timer-handles* (make-hash-table :test 'eq) "timer -> JavaScript timer id")

(defmethod lem/common/timer:start-timer ((timer lem/common/timer::timer) ms &key repeat)
  (setf (lem/common/timer::timer-ms timer) ms
        (lem/common/timer::timer-repeat-p timer) repeat
        (lem/common/timer::timer-mutex timer) (bt2:make-lock :name "timer internal mutex"))
  (let* ((manager lem/common/timer::*timer-manager*)
         (fire (lambda ()
                 (unless repeat (remhash timer *timer-handles*))
                 (lem/common/timer::send-timer-notification
                  manager
                  (lambda () (lem/common/timer::call-timer-function timer)))
                 nil)))
    (setf (gethash timer *timer-handles*)
          (js:funcall (if repeat *set-interval* *set-timeout*) fire (max 0 ms))))
  timer)

(defmethod lem/common/timer:stop-timer ((timer lem/common/timer::timer))
  (let ((handle (gethash timer *timer-handles*)))
    (when handle
      (remhash timer *timer-handles*)
      (js:funcall (if (lem/common/timer::timer-repeat-p timer) *clear-interval* *clear-timeout*)
                  handle)))
  (lem/common/timer::expire-timer timer))

;;; A background job runs at once; its continuation is an event.

(defun lem-core::call-background-job (function cont)
  (let ((result (funcall function)))
    (lem:send-event (lambda () (funcall cont result)))))
