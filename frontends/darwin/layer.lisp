(defpackage :lem-darwin/layer
  (:use :cl)
  (:export :pin-layer-contents
           :set-layer-background-color))
(in-package :lem-darwin/layer)

;;; What the window server shows of a Core Animation layer whose last frame
;;; no longer fits it.
;;;
;;; While the user drags a window edge, the window, and the layer a GUI
;;; frontend draws into, change size before the frontend can draw at the
;;; new size, and by default the window server stretches the last frame to
;;; fit.  Pinning the layer's contents to its top-left corner shows the last
;;; frame at its own size instead, with the layer's background colour in the
;;; strip the window has grown by, until the next frame arrives.
;;;
;;; These are plain Objective-C runtime calls through CFFI, not cl-objc, so
;;; they work on every macOS architecture; elsewhere they do nothing.

#+darwin
(progn

(cffi:define-foreign-library quartz-core
  (:darwin (:framework "QuartzCore")))

(cffi:define-foreign-library core-graphics
  (:darwin (:framework "CoreGraphics")))

(cffi:use-foreign-library quartz-core)
(cffi:use-foreign-library core-graphics)

(defun selector (name)
  "Return the Objective-C selector named NAME."
  (cffi:foreign-funcall "sel_registerName" :string name :pointer))

(defun send-pointer (object selector-name argument)
  "Send OBJECT the one-argument message SELECTOR-NAME with ARGUMENT, an
object pointer, ignoring its result."
  (cffi:foreign-funcall "objc_msgSend"
                        :pointer object
                        :pointer (selector selector-name)
                        :pointer argument
                        :void))

(defun send-bool (object selector-name value)
  "Send OBJECT the one-argument message SELECTOR-NAME with the BOOL VALUE."
  (cffi:foreign-funcall "objc_msgSend"
                        :pointer object
                        :pointer (selector selector-name)
                        :bool value
                        :void))

(defun string-constant (name)
  "Return the NSString held by the exported constant NAME, such as
kCAGravityTopLeft."
  (cffi:mem-ref (cffi:foreign-symbol-pointer name) :pointer))

(defun null-layer-p (layer)
  "Return true if LAYER is NIL or a null pointer."
  (or (null layer) (cffi:null-pointer-p layer)))

(defun set-layer-background-color (layer red green blue)
  "Set the background colour of LAYER, a pointer to a CALayer, to RED, GREEN
and BLUE (0-255, sRGB).  It shows wherever the layer's contents do not
cover it.  Must be called on the main thread.  Errors are logged: this runs
in an SDL event watch, which a Lisp error must not unwind through."
  (unless (null-layer-p layer)
    (handler-case
        (let ((color (cffi:foreign-funcall "CGColorCreateSRGB"
                                           :double (/ red 255d0)
                                           :double (/ green 255d0)
                                           :double (/ blue 255d0)
                                           :double 1d0
                                           :pointer)))
          (unwind-protect (send-pointer layer "setBackgroundColor:" color)
            (cffi:foreign-funcall "CGColorRelease" :pointer color :void)))
      (error (e)
        (log:error "Failed to set the layer's background colour: ~A" e)))))

(defun pin-layer-contents (layer red green blue)
  "Show LAYER's contents, a pointer to a CALayer, at their own size in its
top-left corner when they no longer fit it, rather than stretched, clipped
to the layer, on a background of RED, GREEN and BLUE (0-255).  Return true,
or NIL if LAYER is null or this fails.  Must be called on the main thread."
  (unless (null-layer-p layer)
    (handler-case
        (progn
          (send-pointer layer "setContentsGravity:"
                        (string-constant "kCAGravityTopLeft"))
          (send-bool layer "setMasksToBounds:" t)
          (set-layer-background-color layer red green blue)
          t)
      (error (e)
        (log:error "Failed to pin the layer's contents: ~A" e)
        nil))))

)

#-darwin
(progn
  (defun set-layer-background-color (layer red green blue)
    "Do nothing: there are no Core Animation layers outside macOS."
    (declare (ignore layer red green blue))
    nil)

  (defun pin-layer-contents (layer red green blue)
    "Return NIL: there are no Core Animation layers outside macOS."
    (declare (ignore layer red green blue))
    nil))
