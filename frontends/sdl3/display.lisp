(defpackage :lem-sdl3/display
  (:use :cl)
  (:local-nicknames (:font :lem-sdl3/font)
                    (:icon-font :lem-sdl3/icon-font)
                    (:text-surface-cache :lem-sdl3/text-surface-cache)
                    (:utils :lem-sdl3/utils))
  (:export :with-display
           :display
           :current-display
           :display-cursor-type
           :display-latin-font
           :display-cjk-normal-font
           :display-redraw-at-least-once-p
           :display-font-config
           :display-scale
           :display-window
           :display-char-width
           :display-char-height
           :display-font-ascent
           :display-font-descent
           :display-focus-p
           :create-view-texture
           :display-renderer
           :set-render-color
           :display-background-color
           :display-foreground-color
           :display-texture
           :render-border
           :render-line
           :clear
           :get-display-font
           :render-margin-line
           :scaled-char-width
           :scaled-char-height
           :notify-required-redisplay
           :update-texture
           :update-display
           :display-width
           :display-height
           :display-window-width
           :adapt-high-dpi-font-size
           :handle-display-changed
           :*post-display-change-hooks*
           :add-post-display-change-hook
           :change-font
           :with-renderer
           :with-display-render-target
           :display-scratch-rect
           :call-with-scratch-rect
           :with-scratch-rect))
(in-package :lem-sdl3/display)

(defvar *display*)

(defvar *post-display-change-hooks* '()
  "Hook variable (analogous to Lem's other `*...-hooks*' registries): a list
of one-argument functions (DISPLAY) called after a DPI-driven font/char-metrics
change has been applied, before `update-on-display-resized'.
`lem-sdl3/view' registers a hook here to recreate per-view textures
whose pixel sizes depend on `display-char-width' / `display-char-height';
without it, the old view textures — sized at the *old* char metrics —
leak through as scaled/clipped artifacts (most visibly the buffer
occupying only the upper-left quarter of the window when moving from
Retina to a 1x display).")

(defun current-display ()
  *display*)

(defun (setf current-display) (display)
  (setf *display* display))

(defun call-with-display (function)
  (funcall function *display*))

(defmacro with-display ((display) &body body)
  `(call-with-display (lambda (,display) ,@body)))

(defclass display ()
  ((mutex :initform (bt2:make-lock :name "lem-sdl3 display mutex")
          :reader display-mutex)
   (cursor-type :initform :box
                :accessor display-cursor-type
                :type lem:cursor-type)
   (font-config :initarg :font-config
                :accessor display-font-config)
   (font :initarg :font
         :type font
         :accessor display-font)
   (renderer :initarg :renderer
             :reader display-renderer)
   (texture :initarg :texture
            :accessor display-texture)
   (window :initarg :window
           :reader display-window)
   (char-width :initarg :char-width
               :accessor display-char-width)
   (char-height :initarg :char-height
                :accessor display-char-height)
   (foreground-color :initform (lem:make-color #xff #xff #xff)
                     :accessor display-foreground-color)
   (background-color :initform (lem:make-color 0 0 0)
                     :accessor display-background-color)
   (focus :initform nil
          :accessor display-focus-p)
   (redraw-at-least-once :initform nil
                         :accessor display-redraw-at-least-once-p)
   (scale :initform '(1 1)
          :initarg :scale
          :accessor display-scale)
   (scratch-rect :initform (lem-sdl3/ffi:make-frect)
                 :reader display-scratch-rect
                 :documentation
                 "Pre-allocated SDL_FRect reused across all render calls to
avoid allocating a new rect on every draw-rect, fill-to-end-of-line,
render-texture, etc.  Lives as long as the display.  Mutated in-place by
`call-with-scratch-rect' / `with-scratch-rect'; do not rely on its
contents outside the dynamic extent of that call.")))

(declaim (inline call-with-scratch-rect))
(defun call-with-scratch-rect (display x y w h function)
  "Set the display's scratch SDL_FRect to (X Y W H) and call FUNCTION with it.
Single mutation + funcall — no heap allocation.  Used by
`with-scratch-rect'; expose the underlying function so callers that
want to pass a closure directly can skip the macro."
  (funcall function (lem-sdl3/ffi:set-frect (display-scratch-rect display) x y w h)))

(defmacro with-scratch-rect ((var display-form x y w h) &body body)
  "Bind VAR to the display's pre-allocated SDL_FRect with fields set to X Y W H.
The rect is mutated in-place — no heap allocation occurs.  Thin wrapper
over `call-with-scratch-rect'."
  `(call-with-scratch-rect ,display-form ,x ,y ,w ,h (lambda (,var) ,@body)))

(defmethod display-latin-font ((display display))
  (font:font-latin-normal-font (display-font display)))

(defmethod display-latin-bold-font ((display display))
  (font:font-latin-bold-font (display-font display)))

(defmethod display-cjk-normal-font ((display display))
  (font:font-cjk-normal-font (display-font display)))

(defmethod display-cjk-bold-font ((display display))
  (font:font-cjk-bold-font (display-font display)))

(defmethod display-emoji-font ((display display))
  (font:font-emoji-font (display-font display)))

(defmethod display-braille-font ((display display))
  (font:font-braille-font (display-font display)))

(defmethod display-font-ascent ((display display))
  "Latin-normal-font ascent in pixels (always positive). Defines the row's
target baseline relative to the cell top."
  (font:font-ascent (display-font display)))

(defmethod display-font-descent ((display display))
  "Latin-normal-font descent in pixels (SDL_ttf convention: negative number,
e.g. -3 for NotoSansMono at 12pt). |descent| pixels sit below the baseline."
  (font:font-descent (display-font display)))

(defmethod display-background-color ((display display))
  (or (lem:parse-color lem-if:*background-color-of-drawing-window*)
      (slot-value display 'background-color)))

(defun call-with-renderer (display function)
  (lem-sdl3/ffi:in-main-thread ()
    (bt2:with-recursive-lock-held ((display-mutex display))
      (funcall function))))

(defmacro with-renderer ((display) &body body)
  `(call-with-renderer ,display (lambda () ,@body)))

(defun call-with-display-render-target (display texture function)
  (let ((previous (lem-sdl3/ffi:get-render-target (display-renderer display))))
    (lem-sdl3/ffi:set-render-target (display-renderer display) texture)
    (unwind-protect (funcall function)
      (lem-sdl3/ffi:set-render-target (display-renderer display) previous))))

(defmacro with-display-render-target ((display texture) &body body)
  `(call-with-display-render-target ,display ,texture (lambda () ,@body)))

(defmethod clear ((display display))
  (lem-sdl3/ffi:set-render-target (display-renderer display) (display-texture display))
  (set-render-color display (display-background-color display))
  (lem-sdl3/ffi:render-fill-rect (display-renderer display) nil))

(defmethod get-display-font ((display display) &key type bold character)
  (check-type type lem-core::char-type)
  (cond ((eq type :control)
         (display-latin-font display))
        ((eq type :icon)
         (or (and character (icon-font:icon-font
                             character
                             (font:font-config-size (display-font-config display))))
             (display-emoji-font display)))
        ((eq type :emoji)
         (display-emoji-font display))
        ((eq type :braille)
         (display-braille-font display))
        (bold
         (case type
           ((:latin :zero-width) (display-latin-bold-font display))
           (otherwise (display-cjk-bold-font display))))
        (t
         (case type
           ((:latin :zero-width) (display-latin-font display))
           (otherwise (display-cjk-normal-font display))))))

(defmethod scaled-char-width ((display display) x)
  (let ((scale-x (round (first (display-scale display)))))
    (floor (* scale-x x) (display-char-width display))))

(defmethod scaled-char-height ((display display) y)
  (let ((scale-y (round (second (display-scale display)))))
    (floor (* scale-y y) (display-char-height display))))

(defmethod update-display ((display display))
  (lem-sdl3/ffi:render-present (display-renderer display)))

(defmethod display-width ((display display))
  (nth-value 0 (lem-sdl3/ffi:get-render-output-size (display-renderer display))))

(defmethod display-height ((display display))
  (nth-value 1 (lem-sdl3/ffi:get-render-output-size (display-renderer display))))

(defmethod display-window-width ((display display))
  (nth-value 0 (lem-sdl3/ffi:get-window-size (display-window display))))

(defmethod display-window-height ((display display))
  (nth-value 1 (lem-sdl3/ffi:get-window-size (display-window display))))

(defmethod update-texture ((display display))
  (bt2:with-lock-held ((display-mutex display))
    (lem-sdl3/ffi:destroy-texture (display-texture display))
    (setf (display-texture display)
          (utils:create-texture (display-renderer display)
                                (display-width display)
                                (display-height display)))))

(defmethod set-render-color ((display display) color)
  (when color
    (lem-sdl3/ffi:set-render-draw-color (display-renderer display)
                                        (lem:color-red color)
                                        (lem:color-green color)
                                        (lem:color-blue color))))

(defun adapt-high-dpi-display-scale (display)
  (with-renderer (display)
    (multiple-value-bind (renderer-width renderer-height)
        (lem-sdl3/ffi:get-render-output-size (display-renderer display))
      (let* ((window-width (display-window-width display))
             (window-height (display-window-height display))
             (scale-x (/ renderer-width window-width))
             (scale-y (/ renderer-height window-height)))
        (setf (display-scale display) (list scale-x scale-y))))))

(defun adapt-high-dpi-font-size (display)
  (with-renderer (display)
    (let ((font-config (display-font-config display))
          (ratio (round (first (display-scale display)))))
      (change-font display
                   (font:change-size font-config
                                     (* ratio (lem:config :sdl2-font-size (font:default-font-size))))
                   nil))))

(defun add-post-display-change-hook (function)
  "Register FUNCTION (a one-arg fn of DISPLAY) on `*post-display-change-hooks*'.
Idempotent."
  (pushnew function *post-display-change-hooks*))

(defun run-post-display-change-hooks (display)
  (dolist (hook *post-display-change-hooks*)
    (funcall hook display)))

(defmethod notify-required-redisplay ((display display))
  (with-renderer (display)
    (when (display-redraw-at-least-once-p display)
      (setf (display-redraw-at-least-once-p display) nil)
      (lem-sdl3/ffi:set-render-target (display-renderer display) (display-texture display))
      (set-render-color display (display-background-color display))
      (lem-sdl3/ffi:render-clear (display-renderer display))
      #+darwin
      (adapt-high-dpi-display-scale display)
      #+darwin
      (adapt-high-dpi-font-size display)
      (run-post-display-change-hooks display)
      (lem:update-on-display-resized))))

(defun handle-display-changed (display)
  "Handle a backing-size or display change for DISPLAY.
Re-creates the texture at the new drawable size, re-derives the
high-DPI scale, re-opens the font at the new scale, runs
`*post-display-change-hooks*' (which refresh per-view textures), and
relayouts windows.  Called when SDL fires `:size-changed' or
`:display-changed' for the window \u2014 i.e. the window moved to a
monitor with a different backing scale (Retina \u2194 standard DPI).
Unlike `notify-required-redisplay' this runs the DPI adaptation
unconditionally, since the event can arrive without a redisplay having
been requested.  The frame-scoped half is skipped when there is no
current frame, or the frame has no window tree yet: SDL also fires
these events when the window is mapped, and the event loop can pump
them before the editor thread has run `setup-first-frame'.

`update-texture' grabs the display mutex non-recursively, so it must
be called outside the `with-renderer' block (matching the `:resized'
event handler pattern in `on-windowevent')."
  (update-texture display)
  ;; SDL3 also fires these events at startup, after the frame exists but
  ;; before setup-first-frame has built its window tree.
  (when (and (lem:current-frame)
             (lem:frame-window-tree (lem:current-frame)))
    (with-renderer (display)
      #+darwin
      (adapt-high-dpi-display-scale display)
      #+darwin
      (adapt-high-dpi-font-size display)
      (run-post-display-change-hooks display)
      (lem:update-on-display-resized))))

(defmethod render-fill-rect ((display display) x y width height &key color)
  (let ((x (* x (display-char-width display)))
        (y (* y (display-char-height display)))
        (width (* width (display-char-width display)))
        (height (* height (display-char-height display))))
    (with-scratch-rect (rect display x y width height)
      (set-render-color display color)
      (lem-sdl3/ffi:render-fill-rect (display-renderer display) rect))))

(defmethod render-line (display x1 y1 x2 y2 &key color)
  (set-render-color display color)
  (lem-sdl3/ffi:render-line (display-renderer display) x1 y1 x2 y2))

(defmethod render-fill-rect-by-pixels ((display display) x y width height &key color)
  (with-scratch-rect (rect display x y width height)
    (set-render-color display color)
    (lem-sdl3/ffi:render-fill-rect (display-renderer display) rect)))

(defmethod render-border ((display display) x y w h &key border-type)
  (let* ((x1 (- (* x (display-char-width display)) (floor (display-char-width display) 2)))
         (y1 (- (* y (display-char-height display)) (floor (display-char-height display) 2)))
         (x2 (1- (+ x1 (* (+ w 1) (display-char-width display)))))
         (y2 (+ y1 (* (+ h 1) (display-char-height display)))))
    (with-scratch-rect (rect display x1 y1 (- x2 x1) (- y2 y1))
      (set-render-color display (display-background-color display))
      (lem-sdl3/ffi:render-fill-rect (display-renderer display) rect))
    (let ((renderer (display-renderer display)))
      (set-render-color display (display-foreground-color display))
      (case border-type
        (:drop-curtain
         ;; downleft -> upleft, upleft -> upright
         (lem-sdl3/ffi:render-lines renderer x1 y2 x1 y1)
         (lem-sdl3/ffi:render-lines renderer x1 y1 x2 y1))
        (:left-border
         ;; downleft -> upleft
         (lem-sdl3/ffi:render-lines renderer x1 y2 x1 y1))
        (otherwise
         ;; downleft -> upleft -> upright
         (lem-sdl3/ffi:render-lines renderer x1 y2 x1 y1 x2 y1)))
      ;; upright -> downright -> downleft
      (lem-sdl3/ffi:render-lines renderer x2 y1 x2 y2 x1 y2))))

(defmethod render-margin-line ((display display) x y height)
  (let ((attribute (lem:ensure-attribute 'lem:modeline-inactive)))
    (render-fill-rect display
                      (1- x)
                      y
                      1
                      height
                      :color (lem-core:attribute-background-color attribute))
    (render-fill-rect-by-pixels display
                                (+ (* (1- x) (display-char-width display))
                                   (floor (display-char-width display) 2)
                                   -1)
                                (* y (display-char-height display))
                                2
                                (* height (display-char-height display))
                                :color (lem-core:attribute-foreground-color attribute))))

(defmethod change-font ((display display) font-config &optional (save-font-size-p t))
  (let ((font-config (font:merge-font-config font-config (display-font-config display))))
    (font:close-font (display-font display))
    (let ((font (font:open-font font-config)))
      (setf (display-char-width display) (font:font-char-width font)
            (display-char-height display) (font:font-char-height font))
      (setf (display-font-config display) font-config)
      (setf (display-font display) font))
    (when save-font-size-p
      (font:save-font-size font-config (first (display-scale display))))
    (icon-font:clear-icon-font-cache)
    (text-surface-cache:clear-text-surface-cache)
    (lem:send-event :resize)))

(defmethod create-view-texture ((display display) width height)
  (utils:create-texture (display-renderer display)
                        (* width (display-char-width display))
                        (* height (display-char-height display))))

(defmethod lem-if:set-font-name ((implementation lem-sdl3/sdl3:sdl3) font-name)
  (lem-sdl3/display:change-font
    (lem-sdl3/display:current-display)
    (lem-sdl3/font:make-font-config 
      :latin-normal-file (lem-if:get-font-by-name-and-style font-name "Regular")
      :latin-bold-file (lem-if:get-font-by-name-and-style font-name "Bold"))))
