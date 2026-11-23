(defpackage :lem-sdl3
  (:use :cl
        :lem-sdl3/sdl3
        :lem-sdl3/keyboard
        :lem-sdl3/font
        :lem-sdl3/icon
        :lem-sdl3/platform
        :lem-sdl3/resource
        :lem-sdl3/log
        :lem-sdl3/mouse)
  (:local-nicknames (:display :lem-sdl3/display)
                    (:view :lem-sdl3/view)
                    (:event :lem-sdl3/event)
                    (:ffi :lem-sdl3/ffi))
  (:export :*smooth-scroll*
           :change-font
           :set-keyboard-layout
           :render
           :current-renderer))
(in-package :lem-sdl3)

(defconstant +display-width+ 100)
(defconstant +display-height+ 40)

(defvar *live-resize-redraw-timeout* 0.1
  "Seconds a live-resize step waits for the editor thread to redraw at the
window's new size before showing the last frame unscaled instead.")

(defvar *log-live-resize-timing* nil
  "When true, log how long each live-resize step takes the editor to redraw,
and how many main-thread tasks it ran, to Lem's log (~/.lem/logs/debug.log).")

(defvar *scroll-keeps-cursor* t
  "When true, the mouse wheel and trackpad scroll a window's view and leave
its cursor where it is, even off screen, as most GUI editors do; the view
returns to the cursor when it moves or the buffer changes (see
lem:scroll-view).  When NIL, the cursor is kept on screen, as the scroll
commands do.")

(defvar *scroll-animation* t
  "When true, a mouse wheel's notch scrolls a window over a short eased
animation instead of at once (see *smooth-scroll*).")

(defparameter *trackpad-points-per-unit* 10
  "Points of finger travel in one unit of a trackpad's wheel delta: SDL
reports macOS's precise scrolling deltas divided by 10.  The text moves with
the fingers, as in other macOS applications.")

(defparameter *scroll-stream-gap-ms* 50
  "A wheel event within this many milliseconds of a trackpad's is taken for
a trackpad's too, when the platform cannot say (see trackpad-scroll-p).")

(defvar *momentum* t
  "When true, a trackpad flick glides on after the fingers lift, where the
platform does not supply the momentum itself.")

(defparameter *momentum-start-ms* 40
  "Milliseconds without trackpad scrolling after which the fingers are taken
to have lifted and the glide starts.")

(defparameter *momentum-sample-ms* 100
  "The span of trackpad scrolling, in milliseconds before the fingers lift,
whose velocity the glide starts with.")

(defparameter *momentum-friction* 95/100
  "The share of its velocity a glide keeps each frame.")

(defparameter *momentum-minimum* 1/50
  "Pixels per millisecond below which a glide stops.")

(defparameter *scroll-animation-frame-ms* 16
  "Milliseconds between frames of a scroll animation.")

(defparameter *scroll-animation-easing* 3/10
  "The share of the remaining distance a scroll animation moves each frame;
the animation settles in about ten frames.")

(defvar *smooth-scroll* t
  "When true, the mouse wheel and trackpad scroll a window with a modeline
by pixels, including fractions of a line, instead of by whole lines.")

(defun view-point-anchor (window)
  "Return where WINDOW's view point is, as (LINE-NUMBER . CHARPOS)."
  (let ((point (lem:window-view-point window)))
    (cons (lem:line-number-at-point point) (lem:point-charpos point))))

(defun custom-rendered-buffer-p (window buffer)
  "True if BUFFER draws its own content in WINDOW, with a render method for
its class (the tree view, the colour picker), rather than as lines of text."
  (some (lambda (method)
          (not (eq (find-class t) (third (c2mop:method-specializers method)))))
        (compute-applicable-methods #'render (list nil window buffer))))

(defun smooth-scroll-window-p (window)
  "True if WINDOW is scrolled by pixels (see *smooth-scroll*).  A buffer that
draws its own content scrolls with its own scroll commands instead."
  (let ((view (lem:window-view window)))
    (and *smooth-scroll*
         (typep view 'view:view)
         (view:view-use-modeline view)
         (not (custom-rendered-buffer-p window (lem:window-buffer window))))))

(defun notch-pixels (display notches)
  "Return how many pixels NOTCHES of the wheel scroll on DISPLAY, positive
toward the end of the buffer: *scroll-speed* lines a notch, and SDL's
positive wheel values scroll toward the start."
  (- (round (* notches (display:display-char-height display) lem:*scroll-speed*))))

(defun trackpad-pixels (display delta)
  "Return how many pixels a trackpad's wheel DELTA scrolls on DISPLAY,
positive toward the end of the buffer: its finger travel, in the display's
pixels."
  (- (round (* delta *trackpad-points-per-unit*
               (second (display:display-scale display))))))

(defun scroll-window-by-pixels (window display pixels)
  "Scroll WINDOW's body by PIXELS (positive toward the end of the buffer).
Whole lines are scrolled by lem-core; the rest is the view's scroll offset.
Return NIL if the window could not move because it is at the top or bottom
of its buffer, true otherwise."
  (let* ((view (lem:window-view window))
         (char-height (display:display-char-height display))
         (anchor (view-point-anchor window))
         (lines (view:add-scroll-delta view pixels char-height))
         (moved t))
    (unless (zerop lines)
      (if *scroll-keeps-cursor*
          (lem:scroll-view window lines)
          (lem:with-current-window window
            (lem:call-command (lem:find-command "scroll-down") lines)))
      (when (equal anchor (view-point-anchor window))
        (setf (view:view-scroll-offset view) 0
              moved nil)))
    (setf (view:view-scroll-anchor view) (view-point-anchor window))
    moved))

(defun notch-delta-p (delta)
  "True if the wheel DELTA is a whole number of notches, as a mouse wheel
reports; a trackpad, and macOS's momentum after it, report fractions."
  (= delta (fround delta)))

(defun now-ms ()
  "Return the current time in milliseconds."
  (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(defun stop-scroll-motion (view)
  "End VIEW's scroll animation and glide."
  (setf (view:view-scroll-remaining view) 0
        (view:view-momentum-velocity view) 0
        (view:view-scroll-samples view) '()))

(defun notch-animation-step (window display)
  "Move WINDOW's body one frame of its notch animation: *scroll-animation-
easing* of what remains, at least a pixel.  Return true while some remains."
  (let* ((view (lem:window-view window))
         (step (view:next-scroll-step (view:view-scroll-remaining view)
                                      *scroll-animation-easing*)))
    (unless (zerop step)
      (decf (view:view-scroll-remaining view) step)
      (unless (scroll-window-by-pixels window display step)
        (setf (view:view-scroll-remaining view) 0)))
    (/= 0 (view:view-scroll-remaining view))))

(defun momentum-step (window display now)
  "Move WINDOW's body one frame of its glide.  While trackpad scrolls keep
arriving there is none; once they have stopped for *momentum-start-ms* the
glide starts at their velocity and slows by *momentum-friction* a frame.
Return true while the fingers are moving or the glide goes on."
  (let* ((view (lem:window-view window))
         (samples (view:view-scroll-samples view)))
    (cond ((and samples (< (- now (car (first samples))) *momentum-start-ms*))
           t)
          (t
           (when samples
             (let ((velocity (view:scroll-velocity samples (car (first samples))
                                                   *momentum-sample-ms*)))
               (setf (view:view-scroll-samples view) '()
                     (view:view-momentum-velocity view)
                     (if (< (abs velocity) *momentum-minimum*) 0 velocity))))
           (let ((velocity (view:view-momentum-velocity view)))
             (unless (zerop velocity)
               (setf (view:view-momentum-velocity view)
                     (if (scroll-window-by-pixels
                          window display (round (* velocity *scroll-animation-frame-ms*)))
                         (view:next-momentum-velocity velocity *momentum-friction*
                                                      *momentum-minimum*)
                         0))))
           (/= 0 (view:view-momentum-velocity view))))))

(defun scroll-animation-step (window display)
  "Run one frame of WINDOW's notch animation and glide; return true while
either goes on."
  (let ((notch (notch-animation-step window display))
        (momentum (momentum-step window display (now-ms))))
    (or notch momentum)))

(defun scroll-animation-frame (display)
  "Run one frame of every scroll animation on DISPLAY, and stop the
animation timer once none is left.  Runs on the editor thread, which redraws
after each frame."
  (setf (display:display-scrolling-windows display)
        (loop :for window :in (display:display-scrolling-windows display)
              :when (and (member window (lem:window-list))
                         (scroll-animation-step window display))
                :collect window))
  (when (and (null (display:display-scrolling-windows display))
             (display:display-scroll-animation-timer display))
    (lem:stop-timer (display:display-scroll-animation-timer display))
    (setf (display:display-scroll-animation-timer display) nil)))

(defun start-scroll-animation (window display)
  "Run WINDOW's scroll animation and glide on DISPLAY's animation timer,
starting it if needed."
  (pushnew window (display:display-scrolling-windows display))
  (unless (display:display-scroll-animation-timer display)
    (setf (display:display-scroll-animation-timer display)
          (lem:start-timer (lem:make-timer (lambda () (scroll-animation-frame display))
                                           :name "lem-sdl3 scroll animation")
                           *scroll-animation-frame-ms*
                           :repeat t))))

(defun animate-scroll (window display pixels)
  "Scroll WINDOW's body by PIXELS over a short eased animation, as an editor
scrolls a mouse wheel's notch, adding to any animation still running.  A
glide stops."
  (let ((view (lem:window-view window)))
    (setf (view:view-momentum-velocity view) 0
          (view:view-scroll-samples view) '())
    (incf (view:view-scroll-remaining view) pixels))
  (start-scroll-animation window display))

(defun record-trackpad-scroll (window display pixels)
  "Note a trackpad scroll of PIXELS in WINDOW, for the glide after the
fingers lift (see momentum-step).  A glide in progress stops: the fingers
are on the trackpad again."
  (let ((view (lem:window-view window))
        (now (now-ms)))
    (setf (view:view-momentum-velocity view) 0
          (view:view-scroll-samples view)
          (cons (cons now pixels)
                (remove-if (lambda (sample)
                             (< (car sample) (- now (* 2 *momentum-sample-ms*))))
                           (view:view-scroll-samples view)))))
  (start-scroll-animation window display))

(defun scrolled-char-y (display char-x char-y pixel-y)
  "Return the cell row under the pointer at PIXEL-Y, for a window with a
modeline drawn as view:render-view-texture-to-display draws it: its body
shifted up by the scroll offset, and its modeline at the bottom edge of the
view, which can be past its last cell.  CHAR-X and CHAR-Y are the cell
position SDL's coordinates give; other windows keep CHAR-Y."
  (let ((window (lem:focus-window-position (lem:current-frame) char-x char-y)))
    (if (null window)
        char-y
        (let ((view (lem:window-view window)))
          (if (not (and (typep view 'view:view) (view:view-use-modeline view)))
              char-y
              (destructuring-bind (view-y body-height)
                  (ffi:in-main-thread ()
                    (list (nth-value 1 (view:view-pixel-rect view display))
                          (view:view-body-height view display)))
                (let ((char-height (display:display-char-height display))
                      (window-y (lem:window-y window))
                      (body-rows (1- (lem:window-height window))))
                  (if (< pixel-y (+ view-y body-height))
                      (+ window-y
                         (min (floor (+ (- pixel-y view-y) (view:view-scroll-offset view))
                                     char-height)
                              (1- body-rows)))
                      (+ window-y body-rows)))))))))

(defun on-mouse-button-down (display button x y clicks)
  (show-cursor)
  (let ((button
          (cond ((eql button event:+button-left+) :button-1)
                ((eql button event:+button-right+) :button-3)
                ((eql button event:+button-middle+) :button-2)
                ((eql button event:+button-x1+) :button-4))))
    (when button
      (let ((char-x (display:scaled-char-width display x))
            (char-y (display:scaled-char-height display y))
            (pixel-x (display:scaled-pixel-x display x))
            (pixel-y (display:scaled-pixel-y display y)))
        (lem:send-event
         (lambda ()
           (lem:receive-mouse-button-down char-x
                                          (scrolled-char-y display char-x char-y pixel-y)
                                          pixel-x pixel-y button clicks)))))))

(defun on-mouse-button-up (display button x y)
  (show-cursor)
  (let ((button
          (cond ((eql button event:+button-left+) :button-1)
                ((eql button event:+button-right+) :button-3)
                ((eql button event:+button-middle+) :button-2)
                ((eql button event:+button-x1+) :button-4)))
        (char-x (display:scaled-char-width display x))
        (char-y (display:scaled-char-height display y))
        (pixel-x (display:scaled-pixel-x display x))
        (pixel-y (display:scaled-pixel-y display y)))
    (lem:send-event
     (lambda ()
       (lem:receive-mouse-button-up char-x
                                    (scrolled-char-y display char-x char-y pixel-y)
                                    pixel-x pixel-y button)))))

(defun on-mouse-motion (display x y state)
  (show-cursor)
  (let ((button (if (logtest event:+button-lmask+ state)
                    :button-1
                    nil)))
    (let ((char-x (display:scaled-char-width display x))
          (char-y (display:scaled-char-height display y))
          (pixel-x (display:scaled-pixel-x display x))
          (pixel-y (display:scaled-pixel-y display y)))
      (lem:send-event
       (lambda ()
         (lem:receive-mouse-motion char-x
                                   (scrolled-char-y display char-x char-y pixel-y)
                                   pixel-x pixel-y button))))))

(defun trackpad-scroll-p (precision delta gap previous-trackpad-p)
  "True if a wheel event scrolls precisely, with a trackpad or its momentum,
rather than by a mouse wheel's notches.  PRECISION is what the platform said
(:precise or :lines), or NIL if it could not tell.  Then a fraction is a
trackpad's, and so is a whole number arriving within *scroll-stream-gap-ms*
(GAP, or NIL for none before) of a trackpad's (PREVIOUS-TRACKPAD-P): a fast
flick reports whole numbers in the middle of its stream, while a wheel's
notches come apart."
  (case precision
    (:precise t)
    (:lines nil)
    (otherwise (or (not (notch-delta-p delta))
                   (and previous-trackpad-p
                        gap
                        (< gap *scroll-stream-gap-ms*))))))

(defun classify-wheel-event (display precision delta timestamp-ms)
  "Return true if the wheel event with DELTA at TIMESTAMP-MS is a
trackpad's (see trackpad-scroll-p), and remember it for the next one."
  (let* ((last (display:display-last-wheel-time display))
         (trackpad (trackpad-scroll-p precision delta
                                      (and last (- timestamp-ms last))
                                      (display:display-last-wheel-trackpad-p display))))
    (setf (display:display-last-wheel-time display) timestamp-ms
          (display:display-last-wheel-trackpad-p display) trackpad)
    trackpad))

(defun on-mouse-wheel (display wheel-x wheel-y precise-y precision timestamp-ms)
  "Scroll by a wheel event.  WHEEL-X and WHEEL-Y are whole notches;
PRECISE-Y is the vertical scroll with its fraction, used to scroll a window
by pixels (see *smooth-scroll*).  PRECISION is what the platform said about
its source, and TIMESTAMP-MS when it happened (see trackpad-scroll-p).  A
trackpad scrolls with the fingers, gliding on afterwards where the platform
gives no momentum; a wheel's notch animates."
  (show-cursor)
  (multiple-value-bind (x y) (mouse-position)
    (let ((char-x (display:scaled-char-width display x))
          (char-y (display:scaled-char-height display y))
          (pixel-x (display:scaled-pixel-x display x))
          (pixel-y (display:scaled-pixel-y display y))
          (trackpad (and (/= 0 precise-y)
                         (classify-wheel-event display precision precise-y timestamp-ms))))
      (lem:send-event
       (lambda ()
         (let ((window (lem:focus-window-position (lem:current-frame) char-x char-y)))
           (cond ((not (and window
                            (/= 0 precise-y)
                            (smooth-scroll-window-p window)))
                  (lem:receive-mouse-wheel char-x char-y pixel-x pixel-y wheel-x wheel-y))
                 (trackpad
                  (let ((pixels (trackpad-pixels display precise-y)))
                    (scroll-window-by-pixels window display pixels)
                    ;; Where the platform says nothing, it gives no momentum
                    ;; either (macOS on Apple silicon does both).
                    (when (and *momentum* (null precision))
                      (record-trackpad-scroll window display pixels))))
                 (*scroll-animation*
                  (animate-scroll window display (notch-pixels display precise-y)))
                 (t
                  (scroll-window-by-pixels window display
                                           (notch-pixels display precise-y)))))
         (when (= 0 (lem:event-queue-length))
           (lem:redraw-display)))))))

(defun on-textediting (text)
  (handle-textediting (get-platform) text)
  (lem:send-event #'lem:redraw-display))

(defun on-textinput (text)
  (hide-cursor)
  (handle-text-input (get-platform) text))

(defun on-keydown (key-event)
  (hide-cursor)
  (handle-key-down (get-platform) key-event))

(defun on-keyup (key-event)
  (handle-key-up (get-platform) key-event))

(defun on-window-event (display type)
  "Handle the SDL3 window event TYPE.  SDL2 delivered these as sub-codes of a
single :windowevent; SDL3 gives each its own event type."
  (alexandria:switch (type)
    (event:+window-shown+
     (display:notify-required-redisplay display))
    (event:+window-exposed+
     (display:notify-required-redisplay display))
    (event:+window-resized+
     (display:update-texture display)
     (display:notify-required-redisplay display))
    ;; SDL fires this on backing-pixel-size changes that do not correspond
    ;; to a user resize — most notably when the window moves between
    ;; displays with different DPI (Retina ↔ standard).  Re-derive the
    ;; high-DPI scale, re-open the font, and resize the backing texture.
    ;; (SDL2: SDL_WINDOWEVENT_SIZE_CHANGED.)
    (event:+window-pixel-size-changed+
     (display:handle-display-changed display))
    (event:+window-display-changed+
     (display:handle-display-changed display))
    ;; New in SDL3: the display's content scale changed (e.g. the user
    ;; changed the OS scaling setting) without the window moving.
    (event:+window-display-scale-changed+
     (display:handle-display-changed display))
    (event:+window-focus-gained+
     (setf (display:display-focus-p display) t))
    (event:+window-focus-lost+
     (setf (display:display-focus-p display) nil))))

(defun on-filedrop (file)
  (lem:send-event (lambda () (lem:find-file file))))

(defun mouse-position ()
  "Return the mouse position in window coordinates, rounded to integers."
  (multiple-value-bind (x y) (ffi:get-mouse-state)
    (values (round x) (round y))))

(defun dispatch-event (display event)
  "Handle one SDL event.  Return true when the event loop should stop."
  (let ((type (event:event-type event)))
    (alexandria:switch (type)
      (event:+quit+
       #+windows
       (cffi:foreign-funcall "_exit")
       t)
      (event:+text-input+
       (on-textinput (event:event-text event))
       nil)
      (event:+text-editing+
       (on-textediting (event:event-text event))
       nil)
      (event:+key-down+
       (on-keydown (make-key-event-from-sdl (event:key-code event) (event:key-modifiers event)))
       nil)
      (event:+key-up+
       (on-keyup (make-key-event-from-sdl (event:key-code event) (event:key-modifiers event)))
       nil)
      (event:+mouse-button-down+
       (on-mouse-button-down display
                             (event:mouse-button event)
                             (round (event:mouse-x event))
                             (round (event:mouse-y event))
                             (event:mouse-clicks event))
       nil)
      (event:+mouse-button-up+
       (on-mouse-button-up display
                           (event:mouse-button event)
                           (round (event:mouse-x event))
                           (round (event:mouse-y event)))
       nil)
      (event:+mouse-motion+
       (on-mouse-motion display
                        (round (event:mouse-x event))
                        (round (event:mouse-y event))
                        (event:mouse-state event))
       nil)
      (event:+mouse-wheel+
       (on-mouse-wheel display
                       (event:wheel-x event)
                       (event:wheel-y event)
                       (event:wheel-precise-y event)
                       (take-scroll-precision display event)
                       (floor (event:event-timestamp event) 1000000))
       nil)
      (event:+drop-file+
       (on-filedrop (event:drop-file-name event))
       nil)
      ;; Only wakes SDL_WaitEvent so queued main-thread calls run.
      ((ffi:wakeup-event-type)
       nil)
      (otherwise
       (on-window-event display type)
       nil))))

(defun present-texture-unscaled (display)
  "Show DISPLAY's texture as it is, at 1:1 in the window's top-left corner
on the background colour, rather than letting the window server stretch the
last frame to the window's new size."
  (let* ((renderer (display:display-renderer display))
         (texture (display:display-texture display))
         (previous (ffi:get-render-target renderer)))
    (multiple-value-bind (width height) (ffi:texture-size texture)
      (ffi:set-render-target renderer nil)
      (display:set-render-color display (display:display-background-color display))
      (ffi:render-clear renderer)
      (ffi:with-frect (rect 0 0 width height)
        (ffi:render-texture renderer texture rect rect))
      (display:update-display display)
      (ffi:set-render-target renderer previous))))

(defun request-editor-redraw (display)
  "Ask the editor thread to remake DISPLAY's texture at the window's size and
lay out and redraw the editor there, and return a semaphore it signals when
it is done.  The texture is remade only when the editor gets to the request,
so the last frame stays whole until then."
  (let ((done (bt2:make-semaphore :name "lem-sdl3 live resize")))
    (lem:send-event
     (lambda ()
       (unwind-protect
            (progn
              (ffi:in-main-thread () (display:update-texture display))
              (lem:update-on-display-resized))
         (bt2:signal-semaphore done))))
    done))

(defun wait-for-editor-redraw (done timeout)
  "Run the main-thread tasks the editor thread queues while it redraws, until
it signals DONE, and return true; or return NIL after TIMEOUT seconds.  The
second value is how many tasks ran."
  (loop :with deadline := (+ (get-internal-real-time)
                             (* timeout internal-time-units-per-second))
        :sum (ffi:run-pending-main-thread-tasks) :into tasks
        :do (when (bt2:wait-on-semaphore done :timeout 0.001)
              (return (values t tasks)))
        :while (< (get-internal-real-time) deadline)
        :finally (return (values nil tasks))))

(defun redraw-with-editor (display)
  "Have the editor thread redraw DISPLAY at the window's size, running the
drawing it asks of this, the main thread, until it is done; or, if that takes
longer than *live-resize-redraw-timeout*, show the last frame unscaled.  A
request still running from an earlier step is waited for, and if the window
has changed size again since, the editor is asked once more."
  (ffi:call-with-main-thread-tasks-drained
   (lambda ()
     (loop :with deadline := (+ (get-internal-real-time)
                                (* *live-resize-redraw-timeout*
                                   internal-time-units-per-second))
           :for request := (or (display:display-live-resize-request display)
                               (setf (display:display-live-resize-request display)
                                     (request-editor-redraw display)))
           :for start := (get-internal-real-time)
           :do (multiple-value-bind (drawn tasks)
                   (wait-for-editor-redraw
                    request
                    (max 0 (/ (- deadline (get-internal-real-time))
                              internal-time-units-per-second)))
                 (when *log-live-resize-timing*
                   (log:info "Live resize to ~Dx~D: ~:[timed out~;drawn~] in ~,1F ms, ~D main-thread tasks"
                             (display:display-width display)
                             (display:display-height display)
                             drawn
                             (/ (* 1000 (- (get-internal-real-time) start))
                                internal-time-units-per-second)
                             tasks))
                 (unless drawn
                   (present-texture-unscaled display)
                   (return)))
               (setf (display:display-live-resize-request display) nil)
           :while (display:texture-size-stale-p display)))))

(defun redraw-during-live-resize (display)
  "Redraw DISPLAY at the window's new size from inside the platform's resize
loop or animation, where the event loop cannot run.  The editor thread lays
out and redraws, as after any resize, while this thread runs the drawing it
asks for (see redraw-with-editor).  When the editor is waiting for this
thread, because the resize came from an editor command, it cannot, and the
last frame is shown unscaled instead."
  (handler-case
      (ffi:with-float-traps-masked
        (if (ffi:running-main-thread-task-p)
            (present-texture-unscaled display)
            (redraw-with-editor display)))
    ;; This runs in a C callback: an error must not unwind through it.
    (error (condition)
      (log:error "Redraw during live resize failed: ~A" condition))))

(defun record-scroll-precision (display event)
  "Remember whether the wheel EVENT, which SDL is adding now, scrolls
precisely, asking the platform while it still handles the scroll."
  (let ((precision (lem-darwin/scroll:current-scroll-precision))
        (table (display:display-scroll-precision display)))
    (when precision
      ;; Entries are taken as their events are handled; never let lost ones
      ;; pile up.
      (when (< 256 (hash-table-count table))
        (clrhash table))
      (setf (gethash (event:event-timestamp event) table) precision))))

(defun take-scroll-precision (display event)
  "Return, and forget, what record-scroll-precision learned about EVENT, or
NIL."
  (let* ((table (display:display-scroll-precision display))
         (timestamp (event:event-timestamp event)))
    (prog1 (gethash timestamp table)
      (remhash timestamp table))))

(cffi:defcallback live-resize-watch :bool ((userdata :pointer) (event :pointer))
  ;; While the user drags a window edge, and while macOS animates a zoom or
  ;; maximize, the platform runs its own loop and SDL_WaitEvent does not
  ;; return until it ends, so the window server stretches the last frame to
  ;; the window's changing size.  SDL reports each step with
  ;; SDL_EVENT_WINDOW_EXPOSED (data1 = 1 for a drag, 0 for an animation) and
  ;; lets an event watch redraw then; the watch redraws whenever the window's
  ;; pixel size has changed since the texture was made.  Watches run on the
  ;; thread that adds the event, so only the main thread's events are handled.
  (declare (ignore userdata))
  ;; A wheel event: note whether the platform says it is precise.
  (when (and (= (event:event-type event) event:+mouse-wheel+)
             (sdl3:is-main-thread)
             (display:current-display))
    (ignore-errors (record-scroll-precision (display:current-display) event)))
  (when (and (= (event:event-type event) event:+window-exposed+)
             (sdl3:is-main-thread)
             (display:current-display)
             (display:texture-size-stale-p (display:current-display)))
    (redraw-during-live-resize (display:current-display)))
  t)

(defun event-loop (display)
  (event:with-event (event)
    (loop :while (event:wait-event event)
          :until (dispatch-event display event))))

(defun init-application-icon (window)
  (let ((image (ffi:load-image (get-resource-pathname "resources/icon.png"))))
    (unwind-protect (ffi:set-window-icon window image)
      (ffi:destroy-surface image))))

(defun call-with-sdl (function)
  (lem-sdl3/wm:set-x11-wm-class)
  (ffi:init-video)
  (unwind-protect (funcall function)
    (ffi:quit)))

(defun create-display (function)
  (call-with-sdl
   (lambda ()
     (let* ((font-config (make-font-config))
            (font (open-font font-config))
            (char-width (font-char-width font))
            (char-height (font-char-height font))
            (window-width (* +display-width+ char-width))
            (window-height (* +display-height+ char-height))
            (window (ffi:create-window "Lem" window-width window-height)))
       (unwind-protect
            (let ((renderer (ffi:create-renderer window)))
              (init-application-icon window)
              (let ((text-engine (lem-sdl3/text-engine:make-text-engine renderer))
                    (text-shaper (lem-sdl3/text-shaping:make-text-shaper renderer)))
                (unwind-protect
                     (multiple-value-bind (renderer-width renderer-height)
                         (ffi:get-render-output-size renderer)
                       (let* ((scale-x (/ renderer-width window-width))
                              (scale-y (/ renderer-height window-height))
                              (texture (lem-sdl3/utils:create-texture renderer
                                                                      (* scale-x window-width)
                                                                      (* scale-y window-height)))
                              (display (make-instance 'display:display
                                                      :font-config font-config
                                                      :font font
                                                      :renderer renderer
                                                      :window window
                                                      :texture texture
                                                      :char-width (font-char-width font)
                                                      :char-height (font-char-height font)
                                                      :text-engine text-engine
                                                      :text-shaper text-shaper
                                                      :scale (list scale-x scale-y))))
                         (setf (display:current-display) display)
                         (display:adapt-high-dpi-font-size display)
                         (ffi:start-text-input window)
                         (funcall function)
                         (ffi:add-event-watch (cffi:callback live-resize-watch)
                                              (cffi:null-pointer))
                         (unwind-protect (event-loop display)
                           (ffi:remove-event-watch (cffi:callback live-resize-watch)
                                                   (cffi:null-pointer)))))
                  (when text-shaper
                    (lem-sdl3/text-shaping:destroy-text-shaper text-shaper))
                  (lem-sdl3/text-engine:destroy-text-engine text-engine)
                  (ffi:destroy-renderer renderer))))
         (ffi:destroy-window window))))))

(defun sbcl-on-darwin-p ()
  (or #+(and sbcl darwin)
      t
      nil))

(defun set-hints ()
  "Set SDL hints.  Must run before SDL_Init."
  ;; The user's preference; SDL expects "0" or "1".
  (ffi:set-hint "SDL_VIDEO_MAC_FULLSCREEN_SPACES"
                (if (lem:config :darwin-use-native-fullscreen) "1" "0"))
  ;; SDL should not install any signal handlers, since the Lisp runtime
  ;; already does so.
  (ffi:set-hint "SDL_NO_SIGNAL_HANDLERS" "1")
  ;; Lem is not a game; bypassing the KWin compositor brings no noticeable
  ;; performance gain.
  (ffi:set-hint "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR" "0")
  ;; SDL3 may report keycodes already changed by Shift/AltGr.  The keyboard
  ;; code applies shift itself (see make-key-with-shift-careful), so ask for
  ;; the unmodified keycodes SDL2 delivered.
  (ffi:set-hint "SDL_KEYCODE_OPTIONS" "unmodified")
  ;; Deliver the trackpad's momentum after the fingers lift as wheel events,
  ;; as macOS applications glide; SDL drops it by default.
  (ffi:set-hint "SDL_MAC_SCROLL_MOMENTUM" "1"))

(defmethod lem-if:invoke ((implementation sdl3) function)
  ;; Emoji sequences are drawn as one glyph, so the editor measures and moves
  ;; over them as one character.
  (setf lem:*grapheme-clusters* t)
  (flet ((thunk ()
           (let ((editor-thread
                   (funcall function
                            ;; initialize
                            (lambda ())
                            ;; finalize
                            (lambda (report)
                              (when report
                                (do-log report))
                              (event:push-quit-event)))))
             (declare (ignore editor-thread))
             nil)))
    (tmt:with-body-in-main-thread ()
      ;; Inside the main thread: SDL3 may record the main thread on its first
      ;; call (hints included), and IN-MAIN-THREAD relies on that being right.
      (set-hints)
      (ffi:with-float-traps-masked
        (handler-case
            (create-display #'thunk)
          ;; C-c in the terminal Lem was started from: leave the event loop
          ;; and shut SDL down cleanly.
          #+(and linux sbcl)
          (sb-sys:interactive-interrupt () nil))
        (when (sbcl-on-darwin-p)
          (cffi:foreign-funcall "_exit"))))))

(defmethod lem-if:get-background-color ((implementation sdl3))
  (with-debug ("lem-if:get-background-color")
    (display:with-display (display)
      (display:display-background-color display))))

(defmethod lem-if:get-foreground-color ((implementation sdl3))
  (with-debug ("lem-if:get-foreground-color")
    (display:with-display (display)
      (display:display-foreground-color display))))

(defmethod lem-if:update-foreground ((implementation sdl3) color)
  (with-debug ("lem-if:update-foreground" color)
    (display:with-display (display)
      (setf (display:display-foreground-color display)
            (lem:parse-color color)))))

(defmethod lem-if:update-background ((implementation sdl3) color)
  (with-debug ("lem-if:update-background" color)
    (display:with-display (display)
      (setf (display:display-background-color display)
            (lem:parse-color color)))))

(defmethod lem-if:update-cursor-shape ((implementation sdl3) cursor-type)
  (with-debug ("lem-if:update-cursor-type")
    (display:with-display (display)
      (setf (display:display-cursor-type display) cursor-type))))

(defmethod lem-if:display-width ((implementation sdl3))
  (with-debug ("lem-if:display-width")
    (display:with-display (display)
      (display:with-renderer (display)
        (floor (display:display-width display) (display:display-char-width display))))))

(defmethod lem-if:display-height ((implementation sdl3))
  (with-debug ("lem-if:display-height")
    (display:with-display (display)
      (display:with-renderer (display)
        (floor (display:display-height display) (display:display-char-height display))))))

(defmethod lem-if:set-frame-color ((implementation sdl3) mode)
  "Set the window frame to MODE, :dark or :light.
Implemented on macOS; elsewhere, or before the window exists, this does
nothing. Can be called from any thread."
  (let ((display (display:current-display)))
    (when display
      (lem-darwin/appearance:dispatch-set-window-appearance
       (ffi:cocoa-window (display:display-window display))
       mode))))

(defmethod lem-if:display-title ((implementation sdl3))
  (with-debug ("lem-if:display-title")
    (display:with-display (display)
      (ffi:get-window-title (display:display-window display)))))

(defmethod lem-if:set-display-title ((implementation sdl3) title)
  (with-debug ("lem-if:set-display-title")
    (ffi:in-main-thread ()
      (display:with-display (display)
        (display:with-renderer (display)
          (ffi:set-window-title (display:display-window display) title)
          ;; return the title instead of nil
          title)))))

(defmethod lem-if:display-fullscreen-p ((implementation sdl3))
  (with-debug ("lem-if:display-fullscreen-p")
    (display:with-display (display)
      (ffi:window-fullscreen-p (display:display-window display)))))

(defmethod lem-if:set-display-fullscreen-p ((implementation sdl3) fullscreen-p)
  (with-debug ("lem-if:set-display-fullscreen-p")
    (ffi:in-main-thread ()
      (display:with-display (display)
        (display:with-renderer (display)
          ;; Desktop (borderless) fullscreen, as SDL2's :desktop: real
          ;; fullscreen had weird bugs on macOS.
          (ffi:set-window-fullscreen (display:display-window display) fullscreen-p))))))

(defmethod lem-if:maximize-frame ((implementation sdl3))
  (with-debug ("lem-if:maximize-frame")
    (ffi:in-main-thread ()
      (display:with-display (display)
        (ffi:maximize-window (display:display-window display))))))

(defmethod lem-if:minimize-frame ((implementation sdl3))
  (with-debug ("lem-if:minimize-frame")
    (ffi:in-main-thread ()
      (display:with-display (display)
        (ffi:minimize-window (display:display-window display))))))

(defmethod lem-if:make-view ((implementation sdl3) window x y width height use-modeline)
  (with-debug ("lem-if:make-view" window x y width height use-modeline)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:create-view display window x y width height use-modeline)))))

(defmethod lem-if:delete-view ((implementation sdl3) view)
  (with-debug ("lem-if:delete-view")
    (display:with-display (display)
      (display:with-renderer (display)
        (view:delete-view view)))))

(defmethod lem-if:clear ((implementation sdl3) view)
  (with-debug ("lem-if:clear" view)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:render-clear view display)))))

(defmethod lem-if:set-view-size ((implementation sdl3) view width height)
  (with-debug ("lem-if:set-view-size" view width height)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:resize view display width height)))))

(defmethod lem-if:set-view-pos ((implementation sdl3) view x y)
  (with-debug ("lem-if:set-view-pos" view x y)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:move-position view x y)))))

(defmethod lem-if:make-view-with-pixels ((implementation sdl3) window x y width height
                                         pixel-x pixel-y pixel-width pixel-height
                                         use-modeline)
  "Create a view at cell X, Y of WIDTH by HEIGHT cells, placed and sized by
the PIXEL- arguments where they are not NIL."
  (with-debug ("lem-if:make-view-with-pixels" window x y width height
               pixel-x pixel-y pixel-width pixel-height use-modeline)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:create-view display window x y width height use-modeline
                          :pixel-x pixel-x
                          :pixel-y pixel-y
                          :pixel-width pixel-width
                          :pixel-height pixel-height)))))

(defmethod lem-if:set-view-pos-pixels ((implementation sdl3) view x y pixel-x pixel-y)
  "Move VIEW to cell X, Y, at PIXEL-X, PIXEL-Y where they are not NIL."
  (with-debug ("lem-if:set-view-pos-pixels" view x y pixel-x pixel-y)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:move-position view x y pixel-x pixel-y)))))

(defmethod lem-if:set-view-size-pixels ((implementation sdl3) view width height
                                        pixel-width pixel-height)
  "Resize VIEW to PIXEL-WIDTH by PIXEL-HEIGHT where they are not NIL,
keeping its cell size."
  ;; WIDTH and HEIGHT are the window's cell size, modeline row included, as
  ;; floating-window-set-pixel-size passes them; the view already has it.
  (declare (ignore width height))
  (with-debug ("lem-if:set-view-size-pixels" view pixel-width pixel-height)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:resize-pixels view display pixel-width pixel-height)))))

(defmethod lem-if:redraw-view-before ((implementation sdl3) view)
  (with-debug ("lem-if:redraw-view-before" view)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:render-border-using-view view display)))))

(defgeneric render (texture window buffer))

(defmethod lem-if:redraw-view-after ((implementation sdl3) view)
  (with-debug ("lem-if:redraw-view-after" view)
    ;; Something other than the wheel scrolled the window (the cursor moved
    ;; off screen, a recenter, a new buffer): drop the partial line.
    (let ((anchor (view-point-anchor (view:view-window view))))
      (unless (equal anchor (view:view-scroll-anchor view))
        (setf (view:view-scroll-offset view) 0
              (view:view-scroll-anchor view) anchor)
        (stop-scroll-motion view)))
    (display:with-display (display)
      (display:with-renderer (display)
        (multiple-value-bind (width height) (view:view-layout-size view display)
          (ffi:set-render-viewport (display:display-renderer display)
                                   0
                                   0
                                   width
                                   (- height (display:display-char-height display))))
        (render (view:view-texture view)
                (view:view-window view)
                (lem:window-buffer (view:view-window view)))
        (ffi:set-render-viewport (display:display-renderer display))
        (view:render-view-texture-to-display view display)))))

(defmethod lem-if:will-update-display ((implementation sdl3))
  (with-debug ("will-update-display")
    (display:with-display (display)
      (display:with-renderer (display)
        ;; Bound cache memory: full reset when cache exceeds threshold.
        ;; Runs inside with-renderer so textures are destroyed on the main thread.
        (lem-sdl3/text-surface-cache:sweep-if-oversize)
        (ffi:set-render-target (display:display-renderer display) (display:display-texture display))
        (display:set-render-color display (display:display-background-color display))
        (ffi:render-clear (display:display-renderer display))))))

(defun set-input-method (display)
  (let* ((view (lem:window-view (lem:current-window)))
         (cursor-x (view:last-cursor-x view display))
         (cursor-y (view:last-cursor-y view display))
         (text lem-sdl3/keyboard::*textediting-text*)
         (x (+ (nth-value 0 (view:view-pixel-rect view display)) cursor-x))
         (y (+ (nth-value 1 (view:view-pixel-rect view display)) cursor-y)))
    ;; x/y are in pixels; SDL_SetTextInputArea wants window coordinates.
    (destructuring-bind (scale-x scale-y) (display:display-scale display)
      (ffi:set-text-input-area (display:display-window display)
                               (/ x scale-x)
                               (/ y scale-y)
                               (/ (* (display:display-char-width display) (lem:string-width text))
                                  scale-x)
                               (/ (display:display-char-height display) scale-y)))
    (when (plusp (length text))
      (let* ((color (display:display-foreground-color display))
             (surface (ffi:render-text-blended (display:display-cjk-normal-font display)
                                               text
                                               (lem:color-red color)
                                               (lem:color-green color)
                                               (lem:color-blue color))))
        (when surface
          (let ((texture (ffi:create-texture-from-surface (display:display-renderer display)
                                                          surface)))
            (ffi:with-frect (rect x y (ffi:surface-width surface) (ffi:surface-height surface))
              (display:set-render-color display (display:display-background-color display))
              (ffi:render-fill-rect (display:display-renderer display) rect)
              (ffi:render-texture (display:display-renderer display) texture nil rect))
            (ffi:destroy-texture texture)
            (ffi:destroy-surface surface)))))))

(defmethod lem-if:update-display ((implementation sdl3))
  (with-debug ("lem-if:update-display")
    (display:with-display (display)
      (display:with-renderer (display)
        (setf (display:display-redraw-at-least-once-p display) t)
        (ffi:set-render-target (display:display-renderer display) nil)
        (ffi:render-texture (display:display-renderer display) (display:display-texture display) nil nil)
        (set-input-method display)
        (display:update-display display)))))

(defmethod lem-if:increase-font-size ((implementation sdl3))
  (with-debug ("increase-font-size")
    (display:with-display (display)
      (display:with-renderer (display)
        (let ((font-config (display:display-font-config display))
              (ratio (round (first (display:display-scale display)))))
          (display:change-font display
                               (change-size font-config
                                            (+ (font-config-size font-config) ratio))))))))

(defmethod lem-if:decrease-font-size ((implementation sdl3))
  (with-debug ("decrease-font-size")
    (display:with-display (display)
      (display:with-renderer (display)
        (let ((font-config (display:display-font-config display))
              (ratio (round (first (display:display-scale display)))))
          (display:change-font display
                               (change-size font-config
                                            (- (font-config-size font-config) ratio))))))))

(defmethod lem-if:set-font-size ((implementation sdl3) size)
  (display:with-display (display)
    (display:with-renderer (display)
      (let* ((font-config (display:display-font-config display))
             (ratio (round (first (display:display-scale display))))
             (scaled-size (* ratio size)))
        (setf (lem:config :sdl2-font-size) size)
        (display:change-font
         display
         (change-size font-config scaled-size)
         nil)))))

(defmethod lem-if:resize-display-before ((implementation sdl3))
  (with-debug ("resize-display-before")
    (display:with-display (display)
      (display:with-renderer (display)
        (display:clear display)))))

(defmethod lem-if:get-font-list ((implementation sdl3))
  (get-font-list (get-platform)))

(defmethod lem-if:get-mouse-position ((implementation sdl3))
  (display:with-display (display)
    (if (not (cursor-shown-p))
        (values 0 0)
        (multiple-value-bind (x y) (mouse-position)
          (values (display:scaled-char-width display x)
                  (display:scaled-char-height display y))))))

(defmethod lem-if:get-char-width ((implementation sdl3))
  (display:with-display (display)
    (display:display-char-width display)))

(defmethod lem-if:get-char-height ((implementation sdl3))
  (display:with-display (display)
    (display:display-char-height display)))

(defmethod lem-if:view-width ((implementation sdl3) view)
  (display:with-display (display)
    (nth-value 0 (view:view-layout-size view display))))

(defmethod lem-if:view-height ((implementation sdl3) view)
  (display:with-display (display)
    (view:view-render-height view display)))

#-windows
(defmethod lem-if:clipboard-paste ((implementation sdl3))
  (lem-sdl3/log:with-debug ("clipboard-paste")
    (display:with-display (display)
      (display:with-renderer (display)
        ;; SDL2 returned "" for an empty clipboard; keep that.
        (or (ffi:get-clipboard-text) "")))))

#+windows
(defmethod lem-if:clipboard-paste ((implementation sdl3))
  (lem-sdl3/log:with-debug ("clipboard-paste")
    (display:with-display (display)
      (display:with-renderer (display)
        (with-output-to-string (out)
          (let ((text (or (ffi:get-clipboard-text) "")))
            (loop :for string :in (split-sequence:split-sequence #\newline text)
                  :do (if (and (< 0 (length string))
                               (char= #\return (char string (1- (length string)))))
                          (write-line (subseq string 0 (1- (length string))) out)
                          (write-string string out)))))))))

(defmethod lem-if:clipboard-copy ((implementation sdl3) text)
  (lem-sdl3/log:with-debug ("clipboard-copy")
    (display:with-display (display)
      (display:with-renderer (display)
        (ffi:set-clipboard-text text)))))

(lem:enable-clipboard)


;; helpers
(defun char-width () (display:display-char-width (display:current-display)))
(defun char-height () (display:display-char-height (display:current-display)))
(defun current-renderer () (display:display-renderer (display:current-display)))

(defmethod lem-if:display-fullscreen-p ((implementation sdl3))
  (with-debug ("lem-if:display-fullscreen-p")
    (display:with-display (display)
      (ffi:window-fullscreen-p (display:display-window display)))))

;;;; Toggle Display Borderless

(defgeneric lem-if::display-borderless-p (implementation)
  (:documentation
   "Return `t' if FRAME is borderless, `nil' otherwise. 
Default IMPLEMENTATION should return `t' as fallback. ")
  (:method (implementation) t)
  (:method ((implementation sdl3))
    (with-debug ("display-borderless-p")
      (display:with-display (display)
        (ffi:window-borderless-p (display:display-window display))))))

(defmethod (setf lem-if::display-borderless-p) (boolean implementation)
  (declare (ignore boolean implementation)))

(defmethod (setf lem-if::display-borderless-p) (boolean (implementation sdl3))
  (declare (ignore implementation))
  (with-debug ("(setf lem-if::display-borderless-p)")
    (ffi:in-main-thread ()
      (display:with-display (display)
        (ffi:set-window-bordered (display:display-window display) (not boolean))))))

(in-package :lem-core)

(defun lem-core::display-borderless-p ()
  "Get/Set current display if borderless. 
Return `t' for borderless (no-titlebar), `nil' for otherwise. "
  (lem-if::display-borderless-p (implementation)))

(defun (setf lem-core::display-borderless-p) (boolean)
  (setf (lem-if::display-borderless-p (implementation)) boolean))

(define-command toggle-frame-borderless () ()
  "Toggles frame borderless. "
  (setf (display-borderless-p) (not (display-borderless-p))))