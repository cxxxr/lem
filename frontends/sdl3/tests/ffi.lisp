(defpackage :lem-sdl3/tests/ffi
  (:use :cl :rove)
  (:local-nicknames (:ffi :lem-sdl3/ffi)))
(in-package :lem-sdl3/tests/ffi)

;;; These tests need SDL3 and SDL3_ttf but no window: rendering goes through
;;; SDL's software renderer onto an in-memory surface, so they run headless.

(cffi:defcfun ("SDL_ReadSurfacePixel" %read-surface-pixel) :bool
  (surface :pointer)
  (x :int)
  (y :int)
  (r (:pointer :uint8))
  (g (:pointer :uint8))
  (b (:pointer :uint8))
  (a (:pointer :uint8)))

(defun read-pixel (surface-pointer x y)
  "Return the pixel at (X, Y) as a list (R G B A)."
  (cffi:with-foreign-objects ((r :uint8) (g :uint8) (b :uint8) (a :uint8))
    (%read-surface-pixel surface-pointer x y r g b a)
    (list (cffi:mem-ref r :uint8) (cffi:mem-ref g :uint8)
          (cffi:mem-ref b :uint8) (cffi:mem-ref a :uint8))))

(defun call-with-ttf (function)
  (sdl3-ttf:init)
  (unwind-protect (funcall function)
    (sdl3-ttf:quit)))

(defmacro with-ttf (() &body body)
  `(call-with-ttf (lambda () ,@body)))

(defun call-with-software-renderer (width height function)
  "Call FUNCTION with a software renderer drawing onto a WIDTH x HEIGHT
surface, and that surface's pointer."
  (let* ((surface (sdl3:create-surface width height :rgba8888))
         (renderer (sdl3:create-software-renderer surface)))
    (unwind-protect (funcall function renderer surface)
      (sdl3:destroy-renderer renderer)
      (sdl3:destroy-surface surface))))

(defmacro with-software-renderer ((renderer surface width height) &body body)
  `(call-with-software-renderer ,width ,height (lambda (,renderer ,surface) ,@body)))

(defun call-with-default-font (function)
  (let ((font (ffi:open-font (asdf:system-relative-pathname
                              :lem-sdl3 "resources/fonts/NotoSansMono-Regular.ttf")
                             14)))
    (unwind-protect (funcall function font)
      (ffi:close-font font))))

(defmacro with-default-font ((var) &body body)
  `(call-with-default-font (lambda (,var) ,@body)))

(deftest platform-name
  (ok (member (ffi:platform-name)
              '("macOS" "Linux" "Windows" "FreeBSD" "OpenBSD" "NetBSD")
              :test #'string=)))

(deftest clipboard-text-without-video-subsystem
  ;; Without SDL_Init(VIDEO) SDL returns an empty string; we map it to NIL.
  (ok (null (ffi:get-clipboard-text))))

(deftest text-surface-lifecycle
  (with-ttf ()
    (with-default-font (font)
      (let ((surface (ffi:render-text-blended font "A" 255 255 255)))
        (testing "rendering a character yields a live surface with a size"
          (ok (ffi:surface-live-p surface))
          (ok (plusp (ffi:surface-width surface)))
          (ok (plusp (ffi:surface-height surface))))
        (testing "destroy-surface frees it and is idempotent"
          (ffi:destroy-surface surface)
          (ng (ffi:surface-live-p surface))
          (ok (progn (ffi:destroy-surface surface) t)))))))

(deftest empty-string-yields-no-surface
  (with-ttf ()
    (with-default-font (font)
      (ok (null (ffi:render-text-blended font "" 255 255 255))))))

(deftest target-texture-composites-opaquely
  ;; Lem draws into per-view target textures and then copies them to the
  ;; window.  SDL2 ignored the alpha Lem passed (0); SDL3 does not, so the
  ;; fill must stay opaque all the way through the copy.
  (with-software-renderer (renderer surface 16 16)
    (let ((texture (ffi:create-target-texture renderer 16 16)))
      (unwind-protect
           (progn
             (ffi:set-render-target renderer texture)
             (ffi:set-render-draw-color renderer 255 0 0)
             (ffi:render-fill-rect renderer nil)
             (ffi:set-render-target renderer nil)
             (ffi:set-render-draw-color renderer 0 0 255)
             (ffi:render-clear renderer)
             (ffi:with-frect (dest 0 0 16 16)
               (ffi:render-texture renderer texture nil dest))
             (sdl3:flush-renderer renderer)
             (ok (equal (read-pixel surface 8 8) '(255 0 0 255))))
        (ffi:destroy-texture texture)))))

(deftest text-is-visible-when-drawn-into-a-target-texture
  ;; Black text on a white view texture must darken some pixels; with a zero
  ;; foreground alpha (what the SDL2 code passed) the glyph would vanish.
  (with-ttf ()
    (with-default-font (font)
      (with-software-renderer (renderer surface 32 32)
        (let* ((glyph (ffi:render-text-blended font "M" 0 0 0))
               (glyph-texture (ffi:create-texture-from-surface renderer glyph))
               (view (ffi:create-target-texture renderer 32 32)))
          (unwind-protect
               (progn
                 (ffi:set-render-target renderer view)
                 (ffi:set-render-draw-color renderer 255 255 255)
                 (ffi:render-clear renderer)
                 (ffi:with-frect (dest 0 0 (ffi:surface-width glyph) (ffi:surface-height glyph))
                   (ffi:render-texture renderer glyph-texture nil dest))
                 (ffi:set-render-target renderer nil)
                 (ffi:render-texture renderer view nil nil)
                 (sdl3:flush-renderer renderer)
                 (ok (loop :for y :below 32
                           :thereis (loop :for x :below 32
                                          :thereis (< (first (read-pixel surface x y)) 128)))))
            (ffi:destroy-texture view)
            (ffi:destroy-texture glyph-texture)
            (ffi:destroy-surface glyph)))))))

(deftest render-lines-draws-every-segment
  ;; The border code passes a polyline as a flat coordinate list.
  (with-software-renderer (renderer surface 16 16)
    (ffi:set-render-draw-color renderer 0 0 0)
    (ffi:render-clear renderer)
    (ffi:set-render-draw-color renderer 255 255 255)
    (ffi:render-lines renderer 2 12 2 2 12 2)
    (sdl3:flush-renderer renderer)
    (ok (equal (read-pixel surface 2 7) '(255 255 255 255)))
    (ok (equal (read-pixel surface 7 2) '(255 255 255 255)))
    (ok (equal (read-pixel surface 7 7) '(0 0 0 255)))))

(deftest render-output-size
  (with-software-renderer (renderer surface 20 10)
    (declare (ignorable surface))
    (ok (equal (multiple-value-list (ffi:get-render-output-size renderer))
               '(20 10)))))
