(defpackage :lem-sdl3/view
  (:use :cl)
  (:local-nicknames (:display :lem-sdl3/display))
  (:export
   :set-cursor-position
   :last-cursor-x
   :last-cursor-y
   :create-view
   :delete-view
   :render-clear
   :resize
   :move-position
   :draw-window-border
   :render-border-using-view
   :render-view-texture-to-display
   :refresh-all-view-textures
   :view-window
   :view-x
   :view-y
   :view-width
   :view-height
   :view-use-modeline
   :view-texture))
(in-package :lem-sdl3/view)

(defclass view ()
  ((window
    :initarg :window
    :reader view-window)
   (x
    :initarg :x
    :accessor view-x)
   (y
    :initarg :y
    :accessor view-y)
   (width
    :initarg :width
    :accessor view-width)
   (height
    :initarg :height
    :accessor view-height)
   (use-modeline
    :initarg :use-modeline
    :reader view-use-modeline)
   (texture
    :initarg :texture
    :accessor view-texture)
   (last-cursor-x
    :initform nil
    :accessor view-last-cursor-x)
   (last-cursor-y
    :initform nil
    :accessor view-last-cursor-y)))

(defmethod set-cursor-position ((view view) x y)
  (setf (view-last-cursor-x view) x
        (view-last-cursor-y view) y))

(defmethod last-cursor-x ((view view) display)
  (or (view-last-cursor-x view)
      ;; fallback to v1
      (* (lem:last-print-cursor-x (view-window view))
         (display:display-char-width display))))

(defmethod last-cursor-y ((view view) display)
  (or (view-last-cursor-y view)
      ;; fallback to v1
      (* (lem:last-print-cursor-y (view-window view))
         (display:display-char-height display))))

(defun create-view (display window x y width height use-modeline)
  (when use-modeline (incf height))
  (make-instance 'view
                 :window window
                 :x x
                 :y y
                 :width width
                 :height height
                 :use-modeline use-modeline
                 :texture (display:create-view-texture display (max width 1) (max height 1))))

(defmethod delete-view ((view view))
  (when (view-texture view)
    (lem-sdl3/ffi:destroy-texture (view-texture view))
    (setf (view-texture view) nil)))

(defmethod render-clear ((view view) display)
  (display::with-display-render-target (display (view-texture view))
    (display:set-render-color display (display:display-background-color display))
    (lem-sdl3/ffi:render-clear (display:display-renderer display))))

(defmethod resize ((view view) display width height)
  (when (view-use-modeline view) (incf height))
  (setf (view-width view) width
        (view-height view) height)
  (lem-sdl3/ffi:destroy-texture (view-texture view))
  (setf (view-texture view)
        (display:create-view-texture display (max width 1) (max height 1))))

(defmethod move-position ((view view) x y)
  (setf (view-x view) x
        (view-y view) y))

(defmethod draw-window-border (view display (window lem:floating-window))
  (when (and (lem:window-border window)
             (< 0 (lem:window-border window)))
    (lem-sdl3/ffi:set-render-target (display:display-renderer display) (display:display-texture display))
    (display:render-border display
                           (lem:window-x window)
                           (lem:window-y window)
                           (lem:window-width window)
                           (lem:window-height window)
                           :border-type (lem:floating-window-border-shape window))))

(defmethod draw-window-border (view display (window lem:window))
  (when (< 0 (lem:window-x window))
    (lem-sdl3/ffi:set-render-target (display:display-renderer display) (display:display-texture display))
    (display:render-margin-line display
                                          (lem:window-x window)
                                          (lem:window-y window)
                                          (lem:window-height window))))

(defmethod render-border-using-view ((view view) display)
  (draw-window-border view display (view-window view)))

(defun render-view-texture-to-display (view display)
  (lem-sdl3/ffi:set-render-target (display:display-renderer display) (display:display-texture display))
  (lem-sdl3/ffi:with-frect (dest-rect (* (view-x view) (display:display-char-width display))
                                     (* (view-y view) (display:display-char-height display))
                                     (* (view-width view) (display:display-char-width display))
                                     (* (view-height view) (display:display-char-height display)))
    (lem-sdl3/ffi:render-texture (display:display-renderer display)
                                 (view-texture view)
                                 nil
                                 dest-rect)))

(defun refresh-all-view-textures (display)
  "Recreate the SDL texture backing every live view at the current
display char dimensions.  Called via `display:*post-display-change-hooks*'
whenever a DPI-driven font swap changes `display:display-char-width' /
`display:display-char-height' without changing the col/row count of any
window (the canonical Retina \u2194 standard-DPI move case).  In that
situation `lem-if:set-view-size' is a no-op \u2014 `window-set-size'
short-circuits when width/height are unchanged \u2014 so view textures
would otherwise stay sized at the *old* char metrics and leak through
as scaled or clipped artifacts (the buffer occupying only the
upper-left quarter of the window)."
  (dolist (window (append (lem:window-list)
                          (lem:frame-floating-windows (lem:current-frame))))
    (let ((view (lem:window-view window)))
      (when (typep view 'view)
        (when (view-texture view)
          (lem-sdl3/ffi:destroy-texture (view-texture view)))
        (setf (view-texture view)
              (display:create-view-texture display
                                           (max (view-width view) 1)
                                           (max (view-height view) 1)))))))

(display:add-post-display-change-hook 'refresh-all-view-textures)
