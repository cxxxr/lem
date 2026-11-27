(defpackage :lem-sdl3/cursor-blink
  (:use :cl)
  (:export :*cursor-blink*
           :*cursor-blink-interval*
           :*cursor-blinks*
           :cursor-blink
           :make-cursor-blink
           :cursor-blink-off-p
           :cursor-blink-count
           :blink-step
           :restart-cursor-blink
           :stop-cursor-blink))
(in-package :lem-sdl3/cursor-blink)

(defvar *cursor-blink* t
  "When true, the cursor blinks while the editor waits for input.")

(defvar *cursor-blink-interval* 500
  "How long, in milliseconds, the cursor stays shown, and then hidden, in
each blink.")

(defvar *cursor-blinks* 10
  "How many times the cursor blinks after the last input before it stays
shown, so an idle editor stops redrawing; NIL to blink without end.")

(defclass cursor-blink ()
  ((off-p :initform nil
          :accessor cursor-blink-off-p
          :documentation "True while the blink hides the cursor.")
   (count :initform 0
          :accessor cursor-blink-count
          :documentation "How many blinks the cursor has made since it was
last restarted.")
   (timer :initform nil
          :accessor cursor-blink-timer
          :documentation "The idle timer that runs the next phase change, or
NIL while the cursor does not blink."))
  (:documentation "The state of a blinking cursor.  Its phases change on a
one-shot idle timer that each phase change starts again, so input, which
ends the editor's idle time, holds the cursor shown for a full interval
after it."))

(defun make-cursor-blink ()
  "Return the state of a cursor that is shown and does not blink yet."
  (make-instance 'cursor-blink))

(defun blink-step (blink)
  "Change BLINK's phase: hide a shown cursor, or show a hidden one, which
completes a blink.  Return true while BLINK should go on blinking, which
after *cursor-blinks* blinks it does not."
  (cond ((cursor-blink-off-p blink)
         (setf (cursor-blink-off-p blink) nil)
         (incf (cursor-blink-count blink))
         (or (null *cursor-blinks*)
             (< (cursor-blink-count blink) *cursor-blinks*)))
        (t
         (setf (cursor-blink-off-p blink) t)
         t)))

(defun blink-timer-function (blink)
  "Run when BLINK's timer fires: change its phase and, while it blinks,
start the timer for the next change.  The editor redraws after an idle
timer runs."
  (setf (cursor-blink-timer blink) nil)
  (if (and *cursor-blink* (blink-step blink))
      (start-blink-timer blink)
      (setf (cursor-blink-off-p blink) nil)))

(defun start-blink-timer (blink)
  (setf (cursor-blink-timer blink)
        (lem:start-timer (lem:make-idle-timer (lambda () (blink-timer-function blink))
                                              :name "lem-sdl3 cursor blink")
                         *cursor-blink-interval*)))

(defun stop-cursor-blink (blink)
  "Show BLINK's cursor and stop it blinking.  Return true if the cursor was
hidden, so the caller knows to redraw."
  (alexandria:when-let ((timer (cursor-blink-timer blink)))
    (lem:stop-timer timer)
    (setf (cursor-blink-timer blink) nil))
  (prog1 (cursor-blink-off-p blink)
    (setf (cursor-blink-off-p blink) nil
          (cursor-blink-count blink) 0)))

(defun restart-cursor-blink (blink)
  "Show BLINK's cursor and, if *cursor-blink* is true, blink it from the
start once the editor has been idle for *cursor-blink-interval*.  Return
true if the cursor was hidden, so the caller knows to redraw.  Must run on
the editor thread, which owns the idle timers."
  (prog1 (stop-cursor-blink blink)
    (when *cursor-blink*
      (start-blink-timer blink))))
