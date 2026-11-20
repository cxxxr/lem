(in-package :lem-core)

(defclass attached-buffer (text-buffer)
  ((parent-buffer :initarg :parent-buffer
                  :reader attached-buffer-parent-buffer
                  :writer set-attached-buffer-parent-buffer
                  :type buffer)))

(defun attached-buffer-p (buffer)
  (typep buffer 'attached-buffer))

(defclass attached-window (window)
  ((parent-window
    :initarg :parent-window
    :reader attached-window-parent-window
    :writer set-attached-window-parent-window)))

(defmethod window-border ((window attached-window))
  1)

(defun compute-attached-window-position (parent-window &optional (height 1))
  (values (+ (window-x parent-window) 2)
          (+ (window-y parent-window)
             (window-height parent-window)
             (- (+ height
                   1 ; margin-y
                   (if (window-use-modeline-p parent-window)
                       1
                       0))))))

(defun compute-attached-window-width (parent-window)
  (- (window-width parent-window) 4))

(defun make-attached-window (parent-window
                             &key (buffer (alexandria:required-argument :buffer)))
  (multiple-value-bind (x y)
      (compute-attached-window-position parent-window)
    (let ((attached-window
            (make-instance 'attached-window
                           :parent-window parent-window
                           :buffer buffer
                           :x x
                           :y y
                           :width (compute-attached-window-width parent-window)
                           :height 1)))
      (set-window-attached-window attached-window parent-window)
      (attach-buffer (window-buffer parent-window) buffer)
      attached-window)))

(defun adjust-attached-window-size (parent-window)
  (window-set-size (window-attached-window parent-window)
                   (compute-attached-window-width parent-window)
                   (window-height (window-attached-window parent-window))))

(defmethod %delete-window ((window attached-window))
  (assert (attached-window-parent-window window))
  (when (eq window (current-window))
    (setf (current-window) (attached-window-parent-window window)))
  (let ((parent-window (attached-window-parent-window window)))
    (set-window-attached-window nil parent-window)
    (set-attached-window-parent-window nil window)))

(defun attached-window-rows (window)
  "The lines of WINDOW its buffer takes (wrapped lines counted), at most half
of its parent window: then it scrolls."
  (let ((buffer (window-buffer window)))
    (min (+ (buffer-nlines buffer)
            (window-wrapping-offset window (buffer-start-point buffer) (buffer-end-point buffer)))
         (max 1 (floor (window-height (attached-window-parent-window window)) 2)))))

(defmethod window-redraw ((window attached-window) force)
  (let ((height (attached-window-rows window)))
    (unless (= height (window-height window))
      (multiple-value-bind (x y)
          (compute-attached-window-position (attached-window-parent-window window) height)
        (window-set-pos window x y))
      (window-set-size window (window-width window) height)))
  (redraw-buffer (implementation) (window-buffer window) window force))

(defun attached-window-p (window)
  (typep window 'attached-window))

(defun buffer-attached-buffer (buffer)
  (buffer-value buffer 'attached-buffer))

(defun (setf buffer-attached-buffer) (attached-buffer buffer)
  (setf (buffer-value buffer 'attached-buffer) attached-buffer))

(defun attach-buffer (buffer attached-buffer)
  (change-class attached-buffer 'attached-buffer)
  (set-attached-buffer-parent-buffer buffer attached-buffer)
  (setf (buffer-attached-buffer buffer) attached-buffer)
  (values))

(pushnew :lem-attached-feature *features*)
