(defpackage :lem-sdl3/tests/glyph-atlas
  (:use :cl :rove)
  (:local-nicknames (:atlas :lem-sdl3/glyph-atlas)
                    (:shaper :lem-sdl3/shaper)
                    (:ffi :lem-sdl3/ffi)
                    (:ft :lem-sdl3/ffi-freetype)
                    (:hb :lem-sdl3/ffi-harfbuzz)))
(in-package :lem-sdl3/tests/glyph-atlas)

;;; Glyphs drawn through the atlas with SDL's software renderer onto an
;;; in-memory surface, so no window is needed.

(cffi:defcfun ("SDL_ReadSurfacePixel" %read-surface-pixel) :bool
  (surface :pointer) (x :int) (y :int)
  (r (:pointer :uint8)) (g (:pointer :uint8)) (b (:pointer :uint8)) (a (:pointer :uint8)))

(defun read-pixel (surface x y)
  "Return the pixel at (X, Y) of SURFACE as a list (R G B A)."
  (cffi:with-foreign-objects ((r :uint8) (g :uint8) (b :uint8) (a :uint8))
    (%read-surface-pixel surface x y r g b a)
    (list (cffi:mem-ref r :uint8) (cffi:mem-ref g :uint8)
          (cffi:mem-ref b :uint8) (cffi:mem-ref a :uint8))))

(defun ink-box (surface width height)
  "The (LEFT TOP RIGHT BOTTOM) of SURFACE's non-black pixels, or NIL."
  (let (left top right bottom)
    (loop :for y :below height
          :do (loop :for x :below width
                    :when (> (reduce #'+ (subseq (read-pixel surface x y) 0 3)) 60)
                      :do (setf left (if left (min left x) x) right (if right (max right x) x)
                                top (or top y) bottom y)))
    (and left (list left top right bottom))))

(defun call-with-atlas (atlas-size function)
  "Call FUNCTION with a glyph atlas, the surface a software renderer draws
onto, and NotoSansMono at 15 points."
  (let* ((surface (sdl3:create-surface 64 32 :rgba8888))
         (renderer (sdl3:create-software-renderer surface))
         (font (shaper:open-shaping-font
                (asdf:system-relative-pathname :lem-sdl3 "resources/fonts/NotoSansMono-Regular.ttf")
                15))
         (atlas (atlas:make-glyph-atlas renderer atlas-size)))
    (unwind-protect
         (progn
           (ffi:set-render-draw-color renderer 0 0 0)
           (ffi:render-clear renderer)
           (funcall function atlas renderer surface font))
      (atlas:destroy-glyph-atlas atlas)
      (shaper:close-shaping-font font)
      (sdl3:destroy-renderer renderer)
      (sdl3:destroy-surface surface))))

(defun glyph-id (font character)
  (ft:char-index (shaper:shaping-font-face font) character))

(deftest glyph-lands-on-baseline-in-color
  (if (and (ft:available-p) (hb:available-p))
      (call-with-atlas
       256
       (lambda (atlas renderer surface font)
         (atlas:queue-glyph atlas font (glyph-id font #\H) 4 20 (lem:make-color 255 0 0))
         (atlas:flush-glyphs atlas)
         (sdl3:flush-renderer renderer)
         (destructuring-bind (left top right bottom) (ink-box surface 64 32)
           ;; H sits on the baseline (its last ink row is just above y = 20),
           ;; starts at the pen and is drawn in red.
           (ok (= 19 bottom))
           (ok (< 8 (- bottom top) 14))
           (ok (<= 4 left 6))
           (ok (< left right 13))
           (destructuring-bind (r g b a) (read-pixel surface (floor (+ left right) 2) (floor (+ top bottom) 2))
             (declare (ignore a))
             (ok (> r 200))
             (ok (< g 60))
             (ok (< b 60))))))
      (skip "FreeType or HarfBuzz is not installed")))

(deftest glyphs-are-stored-once
  (if (and (ft:available-p) (hb:available-p))
      (call-with-atlas
       256
       (lambda (atlas renderer surface font)
         (declare (ignore renderer surface))
         (atlas:queue-glyph atlas font (glyph-id font #\a) 0 20 (lem:make-color 255 255 255))
         (atlas:queue-glyph atlas font (glyph-id font #\a) 9 20 (lem:make-color 0 255 0))
         (atlas:queue-glyph atlas font (glyph-id font #\b) 18 20 (lem:make-color 255 255 255))
         (atlas:flush-glyphs atlas)
         (ok (= 2 (atlas:glyph-atlas-glyph-count atlas)))))
      (skip "FreeType or HarfBuzz is not installed")))

(deftest full-atlas-is-emptied-and-refilled
  (if (and (ft:available-p) (hb:available-p))
      (call-with-atlas
       ;; Room for about one 15pt glyph.
       16
       (lambda (atlas renderer surface font)
         (atlas:queue-glyph atlas font (glyph-id font #\H) 4 20 (lem:make-color 255 255 255))
         (atlas:queue-glyph atlas font (glyph-id font #\W) 20 20 (lem:make-color 255 255 255))
         (atlas:flush-glyphs atlas)
         (sdl3:flush-renderer renderer)
         ;; Only W is left in the atlas, but both glyphs were drawn.
         (ok (= 1 (atlas:glyph-atlas-glyph-count atlas)))
         (destructuring-bind (left top right bottom) (ink-box surface 64 32)
           (declare (ignore top bottom))
           (ok (<= left 6))
           (ok (>= right 22)))))
      (skip "FreeType or HarfBuzz is not installed")))
