(defpackage :lem-sdl3/graphics
  (:use :cl)
  (:export :delete-drawable
           :clear-drawables
           :draw-line
           :draw-rectangle
           :draw-point
           :draw-points
           :draw-string
           :load-image
           :delete-image
           :draw-image
           :image-width
           :image-height))
(in-package :lem-sdl3/graphics)

(defclass drawable ()
  ((target :initarg :target
           :reader drawable-target)
   (draw-function :initarg :draw-function
                  :reader drawable-draw-function)
   (cleanup-function :initarg :cleanup-function
                     :initform nil
                     :reader drawable-cleanup-function)
   (targets :initform '()
            :accessor drawable-targets)))

(defun buffer-drawables (buffer)
  (lem:buffer-value buffer 'drawables))

(defun (setf buffer-drawables) (drawables buffer)
  (setf (lem:buffer-value buffer 'drawables) drawables))

(defun window-drawables (window)
  (lem:window-parameter window 'drawables))

(defun (setf window-drawables) (drawables window)
  (setf (lem:window-parameter window 'drawables) drawables))

(defmethod drawables ((target lem:window))
  (window-drawables target))

(defmethod drawables ((target lem:buffer))
  (buffer-drawables target))

(defmethod (setf drawables) (drawables (target lem:window))
  (setf (window-drawables target) drawables))

(defmethod (setf drawables) (drawables (target lem:buffer))
  (setf (buffer-drawables target) drawables))

(defmethod add-drawable ((target lem:window) drawable)
  (push target (drawable-targets drawable))
  (push drawable (window-drawables target)))

(defmethod add-drawable ((target lem:buffer) drawable)
  (push target (drawable-targets drawable))
  (push drawable (buffer-drawables target)))

(defun delete-drawable (drawable)
  (dolist (target (drawable-targets drawable))
    (alexandria:deletef (drawables target) drawable))
  ;; Run cleanup callback (e.g. free native surfaces) when drawable is removed.
  (when (drawable-cleanup-function drawable)
    (handler-case (funcall (drawable-cleanup-function drawable))
      (error () nil))))

(defun clear-drawables (target)
  (mapc #'delete-drawable (drawables target))
  (values))

(defmethod lem-sdl3:render (texture window buffer)
  (dolist (drawable (window-drawables window))
    (funcall (drawable-draw-function drawable)))
  (dolist (drawable (buffer-drawables buffer))
    (funcall (drawable-draw-function drawable))))

(defun call-with-drawable (target draw-function &key cleanup-function)
  (let ((drawable
          (make-instance 'drawable
                         :target target
                         :draw-function draw-function
                         :cleanup-function cleanup-function)))
    (add-drawable target drawable)
    drawable))

(defmacro with-drawable ((target) &body body)
  `(call-with-drawable ,target (lambda () ,@body)))

(defun draw-line (target x1 y1 x2 y2 &key color)
  (with-drawable (target)
    (lem-sdl3/display::set-render-color lem-sdl3/display::*display* color)
    (lem-sdl3/ffi:render-line (lem-sdl3:current-renderer) x1 y1 x2 y2)))

(defun draw-rectangle (target x y width height &key filled color)
  (with-drawable (target)
    (lem-sdl3/display::set-render-color lem-sdl3/display::*display* color)
    (lem-sdl3/ffi:with-frect (rect x y width height)
      (if filled
          (lem-sdl3/ffi:render-fill-rect (lem-sdl3:current-renderer) rect)
          (lem-sdl3/ffi:render-rect (lem-sdl3:current-renderer) rect)))))

(defun draw-point (target x y &key color)
  (with-drawable (target)
    (lem-sdl3/display::set-render-color lem-sdl3/display::*display* color)
    (lem-sdl3/ffi:render-point (lem-sdl3:current-renderer) x y)))

(defun draw-points (target x-y-seq &key color)
  (check-type x-y-seq vector)
  (with-drawable (target)
    (lem-sdl3/display::set-render-color lem-sdl3/display::*display* color)
    (lem-sdl3/ffi:render-points (lem-sdl3:current-renderer) x-y-seq)))

(defun draw-string (target string x y
                    &key (font (lem-sdl3/font:font-latin-normal-font
                                (lem-sdl3/display::display-font lem-sdl3/display::*display*)))
                         (color (alexandria:required-argument :color)))
  (let ((surface (lem-sdl3/ffi:render-text-blended font
                                                   string
                                                   (lem:color-red color)
                                                   (lem:color-green color)
                                                   (lem:color-blue color))))
    (when surface
      (call-with-drawable
       target
       (lambda ()
         (let ((texture (lem-sdl3/ffi:create-texture-from-surface (lem-sdl3:current-renderer)
                                                                  surface)))
           (lem-sdl3/ffi:with-frect (dest-rect x
                                               y
                                               (lem-sdl3/ffi:surface-width surface)
                                               (lem-sdl3/ffi:surface-height surface))
             (lem-sdl3/ffi:render-texture (lem-sdl3:current-renderer) texture nil dest-rect))
           (lem-sdl3/ffi:destroy-texture texture)))
       :cleanup-function (lambda ()
                           (lem-sdl3/ffi:destroy-surface surface))))))

(defclass image ()
  ((surface :initarg :surface
            :reader image-surface)
   (width :initarg :width
          :reader image-width)
   (height :initarg :height
           :reader image-height)))

(defun load-image (pathname)
  (let ((surface (lem-sdl3/ffi:load-image pathname)))
    (make-instance 'image
                   :width (lem-sdl3/ffi:surface-width surface)
                   :height (lem-sdl3/ffi:surface-height surface)
                   :surface surface)))

(defun delete-image (image)
  (when image
    (let ((surface (image-surface image)))
      (when surface
        (lem-sdl3/ffi:destroy-surface surface))))
  (values))

(defun draw-image (target image
                   &key (x 0)
                        (y 0)
                        (width (image-width image))
                        (height (image-height image))
                        clip-rect)
  (with-drawable (target)
    (lem-sdl3/ffi:with-frect (dest-rect x y width height)
      (let ((texture (lem-sdl3/ffi:create-texture-from-surface (lem-sdl3:current-renderer)
                                                               (image-surface image))))
        (unwind-protect
             (if clip-rect
                 (destructuring-bind (cx cy cw ch) clip-rect
                   (lem-sdl3/ffi:with-frect (source-rect cx cy cw ch)
                     (lem-sdl3/ffi:render-texture (lem-sdl3:current-renderer)
                                                  texture source-rect dest-rect)))
                 (lem-sdl3/ffi:render-texture (lem-sdl3:current-renderer)
                                              texture nil dest-rect))
          (lem-sdl3/ffi:destroy-texture texture))))))
