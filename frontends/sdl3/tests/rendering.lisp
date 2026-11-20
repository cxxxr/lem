(defpackage :lem-sdl3/tests/rendering
  (:use :cl :rove)
  (:local-nicknames (:display :lem-sdl3/display)
                    (:drawing :lem-sdl3/drawing)
                    (:ffi :lem-sdl3/ffi)
                    (:font :lem-sdl3/font)
                    (:text-engine :lem-sdl3/text-engine)
                    (:text-shaping :lem-sdl3/text-shaping)))
(in-package :lem-sdl3/tests/rendering)

;;; Lines drawn by the real frontend code (redraw-physical-line) onto SDL's
;;; software renderer, with no window, then checked by where the ink lands.
;;; Only the fonts bundled with Lem are used, so results don't depend on the
;;; machine's fonts or the user's settings.

(defparameter *font-size* 28
  "Point size of the test fonts: about a 2x display at the default size.")

(defparameter *columns* 44
  "Width of the test surface in cells.")

(defparameter *rows* 2
  "Height of the test surface in rows.")

(defclass headless-display (display:display)
  ((width :initarg :width :reader headless-display-width))
  (:documentation "A display drawing onto a surface instead of a window."))

(defmethod display:display-window-width ((display headless-display))
  "The width of DISPLAY's surface, which stands in for the window."
  (headless-display-width display))

(cffi:defcfun ("SDL_ReadSurfacePixel" %read-surface-pixel) :bool
  (surface :pointer)
  (x :int)
  (y :int)
  (r (:pointer :uint8))
  (g (:pointer :uint8))
  (b (:pointer :uint8))
  (a (:pointer :uint8)))

(defun bundled-font-config ()
  "The bundled fonts at *font-size*, with no configured or system fallbacks."
  (flet ((bundled (name)
           (lem-sdl3/resource:get-resource-pathname
            (format nil "resources/fonts/~A" name))))
    (font::%make-font-config
     :size *font-size*
     :latin-normal-file (bundled "NotoSansMono-Regular.ttf")
     :latin-bold-file (bundled "NotoSansMono-Bold.ttf")
     :cjk-normal-file (bundled "NotoSansCJK-Regular.ttc")
     :cjk-bold-file (bundled "NotoSansCJK-Bold.ttc")
     :emoji-file (bundled "NotoColorEmoji.ttf")
     :braille-file (bundled "FreeMono.ttf")
     :fallback-files '()
     :system-fallback nil)))

(defun shaping-available-p ()
  (and (lem-sdl3/ffi-freetype:available-p) (lem-sdl3/ffi-harfbuzz:available-p)))

(defun call-with-headless-display (shaping function)
  "Call FUNCTION with a display drawing onto a surface *columns* × *rows*
cells big, and that surface, with text shaping on if SHAPING is true."
  (sdl3-ttf:init)
  (let* ((font (font:open-font (bundled-font-config)))
         (width (* *columns* (font:font-char-width font)))
         (height (* *rows* (font:font-char-height font)))
         (surface (sdl3:create-surface width height :rgba8888))
         (renderer (sdl3:create-software-renderer surface))
         (text-engine (text-engine:make-text-engine renderer))
         (text-shaper (and shaping (text-shaping:make-text-shaper renderer)))
         (text-shaping:*use-shaping* shaping))
    (unwind-protect
         (progn
           (when text-shaper
             ;; Default OpenType features, whatever the user's config says.
             (setf (text-shaping::text-shaper-font-features text-shaper)
                   (multiple-value-list (lem-sdl3/ffi-harfbuzz:parse-features '()))))
           (ffi:set-render-draw-color renderer 0 0 0)
           (ffi:render-clear renderer)
           (funcall function
                    (make-instance 'headless-display
                                   :font-config (bundled-font-config)
                                   :font font
                                   :renderer renderer
                                   :window nil
                                   :texture nil
                                   :char-width (font:font-char-width font)
                                   :char-height (font:font-char-height font)
                                   :text-engine text-engine
                                   :text-shaper text-shaper
                                   :width width)
                    surface))
      (when text-shaper
        (text-shaping:destroy-text-shaper text-shaper))
      (text-engine:destroy-text-engine text-engine)
      (sdl3:destroy-renderer renderer)
      (sdl3:destroy-surface surface)
      (font:close-font font)
      (sdl3-ttf:quit))))

(defmacro with-headless-display ((display surface &key shaping) &body body)
  `(call-with-headless-display ,shaping (lambda (,display ,surface) ,@body)))

(defun text-objects (string)
  "STRING as text objects split by character type, as lem-core splits a
line, in white on black."
  (let ((attribute (lem:make-attribute :foreground "#ffffff" :background "#000000")))
    (loop :with start := 0
          :while (< start (length string))
          :collect (let* ((type (lem-core/display:char-type (char string start)))
                          (end (or (position-if (lambda (character)
                                                  (not (eq type (lem-core/display:char-type character))))
                                                string :start start)
                                   (length string))))
                     (prog1 (make-instance 'lem-core/display:text-object
                                           :string (subseq string start end)
                                           :attribute attribute
                                           :type type)
                       (setf start end))))))

(defun draw-line (display row string)
  "Draw STRING on ROW of DISPLAY's surface as the frontend draws a line."
  (let ((height (display:display-char-height display)))
    (drawing::redraw-physical-line display nil 0 (* row height)
                                   (text-objects string) height)
    (sdl3:flush-renderer (display:display-renderer display))))

(defun ink-p (surface x y)
  "True if the pixel at (X, Y) of SURFACE is more than half lit."
  (cffi:with-foreign-objects ((r :uint8) (g :uint8) (b :uint8) (a :uint8))
    (%read-surface-pixel surface x y r g b a)
    (< 127 (cffi:mem-ref g :uint8))))

(defun ink-box (display surface row column &optional (cells 1))
  "The ink of the CELLS cells from COLUMN on ROW as (LEFT TOP RIGHT BOTTOM),
in pixels relative to the first cell's top left, or NIL if there is none.
The box includes one pixel either side of the cells, so ink that spills
into a neighbouring cell shows as a negative LEFT or a RIGHT past the cells."
  (let* ((width (display:display-char-width display))
         (height (display:display-char-height display))
         (x0 (* column width))
         (y0 (* row height))
         (left nil) (top nil) (right nil) (bottom nil))
    (loop :for y :from y0 :below (+ y0 height)
          :do (loop :for x :from (max 0 (1- x0))
                      :below (min (headless-display-width display)
                                  (+ x0 (* cells width) 1))
                    :when (ink-p surface x y)
                      :do (setf left (min (or left x) x)
                                right (max (or right x) x)
                                top (min (or top y) y)
                                bottom (max (or bottom y) y))))
    (and left (list (- left x0) (- top y0) (- right x0) (- bottom y0)))))

(defun call-with-each-mode (function)
  "Call FUNCTION with a fresh headless display and its surface, once
drawing letter by letter and once with text shaping, if it is installed."
  (dolist (shaping (if (shaping-available-p) '(nil t) '(nil)))
    (testing (if shaping "shaped" "letter by letter")
      (call-with-headless-display shaping function))))

(defmacro with-each-mode ((display surface) &body body)
  `(call-with-each-mode (lambda (,display ,surface) ,@body)))

(defun character-boxes (display surface row line)
  "The ink box (see ink-box) of each non-space character of LINE drawn on
ROW, as a list of (CHARACTER BOX)."
  (loop :with column := 0
        :for character :across line
        :for cells := (lem-core:char-width character 0)
        :unless (char= character #\Space)
          :collect (list character (ink-box display surface row column cells))
        :do (incf column cells)))

(defun lit-run-p (surface x0 y0 x1 y1)
  "True if every pixel from (X0, Y0) to (X1, Y1), inclusive, along a
horizontal or vertical line, is ink."
  (loop :for x :from x0 :to x1
        :always (loop :for y :from y0 :to y1
                      :always (ink-p surface x y))))

(deftest letters-share-the-latin-baseline
  ;; Greek and Cyrillic are :cjk to lem-core but drawn with the latin font;
  ;; their flat or round bottoms must rest on the same row as x's.
  (with-each-mode (display surface)
    (draw-line display 0 " x α Ж П ω Ω ẞ ")
    (let ((bottoms (mapcar (lambda (entry) (fourth (second entry)))
                           (character-boxes display surface 0 " x α Ж П ω Ω ẞ "))))
      (ok (every (lambda (bottom) (= bottom (first bottoms))) bottoms)
          (format nil "bottoms ~A" bottoms))
      (ok (<= (abs (- (first bottoms) (display:display-font-ascent display))) 1)
          "on the baseline, ascent pixels below the row's top"))))

(deftest cjk-sits-in-the-row
  ;; NotoSansCJK's glyphs sit a little below the latin baseline, by its
  ;; design, but must stay within the row and their two cells.
  (with-each-mode (display surface)
    (draw-line display 0 " 日 本 ")
    (let ((ascent (display:display-font-ascent display))
          (descent (abs (display:display-font-descent display)))
          (width (* 2 (display:display-char-width display))))
      (loop :for (character (left top right bottom)) :in (character-boxes display surface 0 " 日 本 ")
            :do (ok (and (<= 0 top) (<= ascent bottom (+ ascent descent)))
                    (format nil "~A between the row's top and its descent" character))
                (ok (and (<= 0 left) (< right width))
                    (format nil "~A inside its two cells" character))
                (when text-shaping:*use-shaping*
                  (ok (<= (abs (- left (- width 1 right))) 1)
                      (format nil "~A centred in its cells" character)))))))

(deftest box-lines-meet-across-cells
  ;; A cross with arms in the four neighbouring cells, over two rows:
  ;;   ─┼─
  ;;    │
  (with-each-mode (display surface)
    (draw-line display 0 " ─┼─ ")
    (draw-line display 1 "  │  ")
    (let* ((width (display:display-char-width display))
           (height (display:display-char-height display))
           (middle-x (+ (* 2 width) (floor width 2)))
           (middle-y (floor height 2)))
      (ok (lit-run-p surface width middle-y (1- (* 4 width)) middle-y)
          "the horizontal line runs unbroken across three cells")
      (ok (lit-run-p surface middle-x 0 middle-x (1- (* 2 height)))
          "the vertical line runs unbroken down two rows"))))

(deftest glyphs-stay-in-their-cells
  ;; Wide latin letters, descenders and symbols the latin font has must not
  ;; put ink in the cells beside them or the row below.
  (with-each-mode (display surface)
    (let ((line " W M m @ g Ж ‰ ∞ ¶ ƒ ẞ ﬁ 日 "))
      (draw-line display 0 line)
      (loop :for (character (left top right bottom)) :in (character-boxes display surface 0 line)
            :for cells := (lem-core:char-width character 0)
            :do (ok (and (<= 0 left) (< right (* cells (display:display-char-width display))))
                    (format nil "~A within its cell, from ~D to ~D" character left right))
                (ok (< bottom (display:display-char-height display))
                    (format nil "~A within its row" character)))
      (ok (notany (lambda (column) (ink-box display surface 1 column))
                  (loop :for column :below *columns* :collect column))
          "nothing reaches the row below"))))
