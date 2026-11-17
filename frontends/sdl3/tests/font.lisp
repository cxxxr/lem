(defpackage :lem-sdl3/tests/font
  (:use :cl :rove)
  (:local-nicknames (:font :lem-sdl3/font)))
(in-package :lem-sdl3/tests/font)

;;; Tests for `lem-sdl3/font:open-font' caching of the latin-normal-font's
;;; intrinsic TTF metrics (ascent / descent).  Those metrics are read by
;;; `lem-sdl3/drawing:draw-text-glyph-surface' through
;;; `lem-sdl3/display:display-font-ascent' / `display-font-descent' to anchor
;;; the per-glyph baseline; if the caching ever drifts from the live TTF
;;; values (for example because `open-font' forgets to re-read the metrics on
;;; a DPI swap), every glyph will land on the wrong y and the buffer will
;;; show a mis-aligned baseline.

(defun call-with-ttf (thunk)
  (sdl3-ttf:init)
  (unwind-protect (funcall thunk)
    (sdl3-ttf:quit)))

(defmacro with-ttf (() &body body)
  `(call-with-ttf (lambda () ,@body)))

(defun open-default-font (size)
  (font:open-font (font:make-font-config :size size)))

(deftest ascent-and-descent-are-cached-from-latin-normal-font
  (with-ttf ()
    (let ((font (open-default-font 14)))
      (unwind-protect
           (let* ((latin (font:font-latin-normal-font font))
                  (live-ascent  (sdl3-ttf:get-font-ascent  latin))
                  (live-descent (sdl3-ttf:get-font-descent latin)))
             (testing "cached ascent matches the live TTF_FontAscent"
               (ok (= (font:font-ascent font) live-ascent)))
             (testing "cached descent matches the live TTF_FontDescent"
               (ok (= (font:font-descent font) live-descent)))
             (testing "ascent is positive (pixels above baseline)"
               (ok (plusp (font:font-ascent font))))
             (testing "descent is non-positive (SDL_ttf convention: <= 0)"
               (ok (not (plusp (font:font-descent font))))))
        (font:close-font font)))))

(deftest baseline-band-fits-within-font-height
  (with-ttf ()
    (let ((font (open-default-font 14)))
      (unwind-protect
           ;; `draw-text-glyph-surface' anchors each glyph at
           ;;   glyph_y = bottom_y - ascent - |descent|
           ;; which places the descender's bottom at (bottom_y - 1) and the
           ;; ascender's top (ascent + |descent|) pixels above it.  That band
           ;; must fit inside the row's intrinsic font height (which may
           ;; additionally include a small line gap) -- otherwise the glyph
           ;; would clip into the next row.
           (let* ((latin (font:font-latin-normal-font font))
                  (height (sdl3-ttf:get-font-height latin)))
             (ok (<= (+ (font:font-ascent font)
                        (abs (font:font-descent font)))
                     height)))
        (font:close-font font)))))

(deftest metrics-refresh-when-font-is-reopened-at-a-new-size
  ;; Mirrors the DPI-swap flow: when the window moves between a Retina and a
  ;; standard-DPI display, `lem-sdl3/display:handle-display-changed' calls
  ;; `open-font' again at the new effective size.  The cached metrics must
  ;; track the new font, not stay pinned to the first one.
  (with-ttf ()
    (let* ((small (open-default-font 12))
           (large (open-default-font 24)))
      (unwind-protect
           (testing "ascent grows with font size"
             (ok (< (font:font-ascent small)
                    (font:font-ascent large))))
        (font:close-font small)
        (font:close-font large)))))

(defun call-with-display-for-font (size function)
  "Call FUNCTION with a display holding fonts opened at SIZE (no window)."
  (let* ((config (font:make-font-config :size size))
         (font (font:open-font config)))
    (unwind-protect
         (funcall function (make-instance 'lem-sdl3/display:display
                                          :font font
                                          :font-config config))
      (font:close-font font))))

(deftest box-drawing-characters-use-the-monospace-font
  ;; Lem classifies U+2500..U+259F as :cjk; the CJK font draws them
  ;; full-width in a one-cell slot, so lines and corners did not meet.
  (with-ttf ()
    (call-with-display-for-font
     14
     (lambda (display)
       (flet ((font-for (character &key bold)
                (lem-sdl3/display:get-display-font display
                                                   :type :cjk
                                                   :bold bold
                                                   :character character)))
         (testing "box drawing and block elements use the latin fonts"
           (dolist (character '(#\─ #\│ #\┌ #\┼ #\━ #\═ #\╬ #\█ #\▒))
             (ok (eq (font-for character)
                     (font:font-latin-normal-font (lem-sdl3/display::display-font display))))
             (ok (eq (font-for character :bold t)
                     (font:font-latin-bold-font (lem-sdl3/display::display-font display))))))
         (testing "CJK text still uses the CJK fonts"
           (ok (eq (font-for #\日)
                   (font:font-cjk-normal-font (lem-sdl3/display::display-font display))))
           (ok (eq (font-for #\日 :bold t)
                   (font:font-cjk-bold-font (lem-sdl3/display::display-font display))))))))))

(deftest box-drawing-band-is-stretched-only-for-box-drawing
  ;; Monospace fonts draw box-drawing glyphs inside their ascent/descent,
  ;; which is shorter than Lem's row, so vertical lines left gaps between
  ;; rows.  The band of rows U+2502 covers is stretched to the cell height.
  (with-ttf ()
    (call-with-display-for-font
     14
     (lambda (display)
       (let ((band (lem-sdl3/display:display-box-drawing-band display #\│ nil))
             (font-height (sdl3-ttf:get-font-height
                           (font:font-latin-normal-font (lem-sdl3/display::display-font display)))))
         (testing "the band lies within the glyph surface"
           (ok (consp band))
           (ok (<= 0 (car band)))
           (ok (< (car band) (cdr band)))
           (ok (<= (cdr band) font-height)))
         (testing "the bold font has its own band"
           (ok (consp (lem-sdl3/display:display-box-drawing-band display #\│ t))))
         (testing "other characters are not stretched"
           (ok (null (lem-sdl3/display:display-box-drawing-band display #\M nil)))
           (ok (null (lem-sdl3/display:display-box-drawing-band display #\日 nil)))))))))
