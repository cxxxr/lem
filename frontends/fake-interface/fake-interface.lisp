(defpackage :lem-fake-interface
  (:use :cl :lem)
  (:export :fake-interface
           :with-fake-interface))
(in-package :lem-fake-interface)

(defclass fake-interface (lem:implementation)
  ((foreground
    :initform nil
    :accessor fake-interface-foreground)
   (background
    :initform nil
    :accessor fake-interface-background)
   (display-width
    :initform 80
    :reader fake-interface-display-width)
   (display-height
    :initform 24
    :reader fake-interface-display-height))
  (:default-initargs
   :name :fake
   :redraw-after-modifying-floating-window t))

(defstruct view
  x
  y
  width
  height
  modeline)

(defmethod lem-if:invoke ((implementation fake-interface) function)
  (funcall function))

(defmethod lem-if:get-background-color ((implementation fake-interface))
  (make-color 0 0 0))

(defmethod lem-if:update-foreground ((implementation fake-interface) color-name)
  (setf (fake-interface-foreground implementation) color-name))

(defmethod lem-if:update-background ((implementation fake-interface) color-name)
  (setf (fake-interface-background implementation) color-name))

(defmethod lem-if:display-width ((implementation fake-interface))
  (fake-interface-display-width implementation))

(defmethod lem-if:display-height ((implementation fake-interface))
  (fake-interface-display-height implementation))

(defmethod lem-if:make-view ((implementation fake-interface) window x y width height use-modeline)
  (make-view
   :x x
   :y y
   :width width
   :height height
   :modeline use-modeline))

(defmethod lem-if:delete-view ((implementation fake-interface) view)
  nil)

(defmethod lem-if:clear ((implementation fake-interface) view)
  nil)

(defmethod lem-if:set-view-size ((implementation fake-interface) view width height)
  (setf (view-width view) width
        (view-height view) height))

(defmethod lem-if:set-view-pos ((implementation fake-interface) view x y)
  (setf (view-x view) x
        (view-y view) y))

(defmethod lem-if:update-display ((implementation fake-interface)))

(defmethod lem-if:view-width ((implementation fake-interface) view)
  (view-width view))

(defmethod lem-if:view-height ((implementation fake-interface) view)
  (view-height view))

(defmethod lem-if:object-width ((implementation fake-interface) object)
  1)

(defmethod lem-if:object-height ((implementation fake-interface) object)
  1)

(defmethod lem-if:render-line ((implementation fake-interface) view x y objects height)
  nil)

(defmethod lem-if:clear-to-end-of-window ((implementation fake-interface) view y)
  nil)

(defmethod lem-if:get-char-width ((implementation fake-interface))
  1)

(defmethod lem-if:get-char-height ((implementation fake-interface))
  1)

(defmethod lem-if:render-line-on-modeline ((implementation fake-interface)
                                           view
                                           left-objects
                                           right-objects
                                           default-attribute
                                           height)
  nil)

(defun call-with-fake-interface (function)
  ;; Give each use its own frames, current buffer, last focused window and
  ;; typeout window (the window of pop-up-typeout-window, *EDITOR ERROR*).
  ;; Frames made by an earlier use would stay in the frame list, where
  ;; delete-buffer switches their windows too, and the current buffer would
  ;; stay a buffer of an earlier use.  A typeout window left open by an
  ;; earlier use would be reused by the next popup, a window of frames that
  ;; are gone.  Inside a running Lem, binding them also
  ;; keeps the user's frames and current buffer as they were.  lem-core
  ;; exports no way to reset these variables, so they are bound directly
  ;; (the frame map made by lem-core::make-display-frame-map, as the global
  ;; one is, so that it can be read from another thread too).
  ;;
  ;; A thread made in FUNCTION starts with bindings of its own to the values
  ;; these variables have when it is made (bt2:*default-special-bindings*, as
  ;; with-implementation does for *implementation*).  So it sees this use's
  ;; frames, not the global ones, but what it sets afterwards (setf
  ;; current-buffer, a frame it makes) stays in that thread.  Such a thread
  ;; must be done before FUNCTION returns: the frames are torn down then.
  ;;
  ;; The frames are torn down at the end, so that the points of their windows
  ;; do not stay in buffers that outlive this use, such as *tmp*.
  ;; teardown-frame frees the window tree, the floating, side, attached and
  ;; header windows (a header window with the last frame that has it).
  (let* ((lem-core::*frames* '())
         (lem-core::*display-frame-map* (lem-core::make-display-frame-map))
         (lem-core::*last-focused-window* nil)
         (lem/buffer/internal::*current-buffer* (primordial-buffer))
         (lem-core::*typeout-window* nil)
         (lem-core::*typeout-before-window* nil)
         (lem-core::*typeout-window-rewinding-values* nil)
         (bt2:*default-special-bindings*
           (list* (cons 'lem-core::*frames* 'lem-core::*frames*)
                  (cons 'lem-core::*display-frame-map* 'lem-core::*display-frame-map*)
                  (cons 'lem-core::*last-focused-window* 'lem-core::*last-focused-window*)
                  (cons 'lem-core::*typeout-window* 'lem-core::*typeout-window*)
                  (cons 'lem-core::*typeout-before-window* 'lem-core::*typeout-before-window*)
                  (cons 'lem-core::*typeout-window-rewinding-values*
                        'lem-core::*typeout-window-rewinding-values*)
                  (cons 'lem/buffer/internal::*current-buffer*
                        'lem/buffer/internal::*current-buffer*)
                  bt2:*default-special-bindings*)))
    (with-implementation (make-instance 'fake-interface)
      (let ((returned nil))
        (unwind-protect
             (multiple-value-prog1
                 (progn
                   (setup-first-frame)
                   (setf (current-buffer) (window-buffer (current-window)))
                   (funcall function))
               (setf returned t))
          ;; teardown-frames tears down every frame, also after one failed.
          ;; When FUNCTION did not return (an error, a throw), its error is
          ;; only warned about: it would replace FUNCTION's own error.  This
          ;; is knowingly the case after a throw or return-from too, as they
          ;; cannot be told from an error unwinding without a flag that a
          ;; restart would fool.
          (if returned
              (teardown-frames)
              (handler-case (teardown-frames)
                (error (condition)
                  (warn "Tearing down the frames failed: ~A" condition)))))))))

(defmacro with-fake-interface (() &body body)
  `(call-with-fake-interface (lambda () ,@body)))
