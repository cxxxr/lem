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
    (sdl2:render-draw-line (lem-sdl3:current-renderer)
                           x1
                           y1
                           x2
                           y2)))

(defun draw-rectangle (target x y width height &key filled color)
  (with-drawable (target)
    (lem-sdl3/display::set-render-color lem-sdl3/display::*display* color)
    (sdl2:with-rects ((rect x y width height))
      (if filled
          (sdl2:render-fill-rect (lem-sdl3:current-renderer) rect)
          (sdl2:render-draw-rect (lem-sdl3:current-renderer) rect)))))

(defun draw-point (target x y &key color)
  (with-drawable (target)
    (lem-sdl3/display::set-render-color lem-sdl3/display::*display* color)
    (sdl2:render-draw-point (lem-sdl3:current-renderer) x y)))

(defun draw-points (target x-y-seq &key color)
  (let ((num-points (length x-y-seq)))
    (with-drawable (target)
      (plus-c:c-let ((c-points sdl2-ffi:sdl-point :count num-points))
        (etypecase x-y-seq
          (vector
           (loop :for i :from 0
                 :for (x . y) :across x-y-seq
                 :do (setf (c-points i :x) x
                           (c-points i :y) y))))
        (lem-sdl3/display::set-render-color lem-sdl3/display::*display* color)
        (sdl2:render-draw-points (lem-sdl3:current-renderer)
                                 (c-points plus-c:&)
                                 num-points)))))

(defun draw-string (target string x y
                    &key (font (lem-sdl3/font:font-latin-normal-font
                                (lem-sdl3/display::display-font lem-sdl3/display::*display*)))
                         (color (alexandria:required-argument :color)))
  (let* ((surface (sdl2-ttf:render-utf8-blended font
                                                string
                                                (lem:color-red color)
                                                (lem:color-green color)
                                                (lem:color-blue color)
                                                0)))
    (call-with-drawable
     target
     (lambda ()
       (let ((texture (sdl2:create-texture-from-surface (lem-sdl3:current-renderer) surface)))
         (sdl2:with-rects ((dest-rect x
                                      y
                                      (sdl2:surface-width surface)
                                      (sdl2:surface-height surface)))
           (sdl2:render-copy (lem-sdl3:current-renderer) texture :dest-rect dest-rect))
         (sdl2:destroy-texture texture)))
     :cleanup-function (lambda ()
                         (trivial-garbage:cancel-finalization surface)
                         (sdl2:free-surface surface)))))

(defclass image ()
  ((surface :initarg :surface
            :reader image-surface)
   (width :initarg :width
          :reader image-width)
   (height :initarg :height
           :reader image-height)))

(defun load-image (pathname)
  (let ((image (sdl2-image:load-image pathname)))
    (make-instance 'image
                   :width (sdl2:surface-width image)
                   :height (sdl2:surface-height image)
                   :surface image)))

(defun delete-image (image)
  (when image
    (let ((surface (image-surface image)))
      (when surface
        (trivial-garbage:cancel-finalization surface)
        (sdl2:free-surface surface))))
  (values))

(defun draw-image (target image
                   &key (x 0)
                        (y 0)
                        (width (image-width image))
                        (height (image-height image))
                        clip-rect)
  (with-drawable (target)
    (sdl2:with-rects ((dest-rect x y width height))
      (let ((source-rect
              (when clip-rect
                (destructuring-bind (x y w h) clip-rect
                  (sdl2:make-rect x y w h))))
            (texture (sdl2:create-texture-from-surface (lem-sdl3:current-renderer) (image-surface image))))
        (unwind-protect
             (sdl2:render-copy (lem-sdl3:current-renderer)
                               texture
                               :source-rect source-rect
                               :dest-rect dest-rect)
          (when source-rect
            (sdl2:free-rect source-rect))
          (sdl2:destroy-texture texture))))))
