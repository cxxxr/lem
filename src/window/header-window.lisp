(in-package :lem-core)

(defclass header-window (window) ())

(defgeneric header-window-height (window)
  (:documentation "Return the preferred height in character cells for a header window.
Subclasses can specialize this to request more vertical space.")
  (:method ((window header-window))
    1))

(defmethod initialize-instance ((window header-window) &key &allow-other-keys)
  (with-slots (x y width height) window
    (setf x 0)
    (setf y (loop :for w :in (frame-header-windows (current-frame))
                  :sum (header-window-height w)))
    (setf width (display-width))
    (setf height (header-window-height window)))
  (add-header-window (current-frame) window)
  (notify-header-window-modified (current-frame))
  (call-next-method))

(defmethod %delete-window ((window header-window))
  ;; make-frame gives a new frame the header windows of the current one, so the
  ;; window can be in several frames: leave none of them with the deleted window.
  (dolist (frame (all-frames))
    (when (find window (frame-header-windows frame))
      (remove-header-window frame window)
      (notify-header-window-modified frame))))

(defun header-window-p (window)
  (typep window 'header-window))
