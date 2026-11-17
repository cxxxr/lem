(defpackage :lem-sdl3/tests/event
  (:use :cl :rove)
  (:local-nicknames (:event :lem-sdl3/event)))
(in-package :lem-sdl3/tests/event)

;;; Headless: only SDL's events subsystem is started, no window.

(defconstant +init-events+ #x4000)

(cffi:defcfun ("SDL_InitSubSystem" %init-sub-system) :bool (flags :uint32))
(cffi:defcfun ("SDL_QuitSubSystem" %quit-sub-system) :void (flags :uint32))
(cffi:defcfun ("SDL_PollEvent" %poll-event) :bool (event :pointer))
(cffi:defcfun ("SDL_PushEvent" %push-event) :bool (event :pointer))

(defun call-with-events (function)
  (%init-sub-system +init-events+)
  (unwind-protect (funcall function)
    (%quit-sub-system +init-events+)))

(defmacro with-events (() &body body)
  `(call-with-events (lambda () ,@body)))

(defun poll-event-of-type (event type)
  "Poll until an event of TYPE is in EVENT; return true, or NIL if none."
  (loop :while (%poll-event event)
        :thereis (= type (event:event-type event))))

(deftest key-event-round-trips-through-sdl
  (with-events ()
    (event:with-event (out)
      ;; SDL_KeyboardEvent: key at 28, mod at 32 (see event.lisp).
      (setf (cffi:mem-ref out :uint32 0) event:+key-down+
            (cffi:mem-ref out :uint32 28) (char-code #\a)
            (cffi:mem-ref out :uint16 32) event:+kmod-lctrl+)
      (ok (%push-event out)))
    (event:with-event (in)
      (ok (poll-event-of-type in event:+key-down+))
      (ok (= (event:key-code in) (char-code #\a)))
      (ok (= (event:key-modifiers in) event:+kmod-lctrl+)))))

(deftest push-quit-event
  (with-events ()
    (ok (event:push-quit-event))
    (event:with-event (in)
      (ok (poll-event-of-type in event:+quit+)))))

(deftest function-keycodes
  (ok (= (event:sdlk-function-key 1) #x4000003a))
  (ok (= (event:sdlk-function-key 12) #x40000045))
  (ok (= (event:sdlk-function-key 13) #x40000068))
  (ok (= (event:sdlk-function-key 24) #x40000073)))

(cffi:defcfun ("SDL_WaitEventTimeout" %wait-event-timeout) :bool
  (event :pointer)
  (timeout-ms :int32))

(deftest call-in-main-thread-runs-on-the-waiting-thread
  ;; This thread plays SDL's main thread, waiting for events with a long
  ;; timeout; the worker's call must be run by it and return promptly.
  ;; Note: without a window SDL's wait polls, so this does not reproduce the
  ;; Cocoa case where only the pushed wake-up event (see WAKE-MAIN-THREAD)
  ;; ends the wait; that path is covered by running Lem itself.
  (with-events ()
    (let* ((result nil)
           (elapsed-ms nil)
           (worker (bt2:make-thread
                    (lambda ()
                      (let ((start (get-internal-real-time)))
                        (setf result (lem-sdl3/ffi:call-in-main-thread (lambda () 42)))
                        (setf elapsed-ms (/ (- (get-internal-real-time) start)
                                            (/ internal-time-units-per-second 1000)))
                        ;; Release the "main thread" from its wait.
                        (event:push-quit-event)))
                    :name "call-in-main-thread test")))
      (event:with-event (event)
        (loop :repeat 10
              :while (bt2:thread-alive-p worker)
              :do (%wait-event-timeout event 5000)))
      (bt2:join-thread worker)
      (ok (eql result 42))
      (ok (and elapsed-ms (< elapsed-ms 2000))))))
