(defpackage :lem-sdl3/tests/shaper
  (:use :cl :rove)
  (:local-nicknames (:shaper :lem-sdl3/shaper)
                    (:ft :lem-sdl3/ffi-freetype)
                    (:hb :lem-sdl3/ffi-harfbuzz)))
(in-package :lem-sdl3/tests/shaper)

;;; Snapping and itemization are tested with made-up glyphs and fonts, so
;;; they need neither FreeType nor HarfBuzz.  The last tests shape with the
;;; fonts that ship with Lem.

(defun glyph (cluster advance &key (offset 0) (id cluster) (font :f))
  "A shaped glyph with ADVANCE and OFFSET in pixels."
  (shaper:make-shaped-glyph :font font :id id :cluster cluster
                            :x-advance (* 64 advance) :x-offset (* 64 offset)))

(defun xs (placed)
  (mapcar #'shaper:placed-glyph-x placed))

(defun snap (string glyphs &key (cell-width 9) (end (length string)))
  (shaper:snap-glyphs glyphs (shaper:character-columns string) cell-width end))

(deftest character-columns
  (ok (equalp #(0 1 2 3) (shaper:character-columns "abc")))
  ;; 日 takes two cells.
  (ok (equalp #(0 1 3 4) (shaper:character-columns "a日b"))))

(deftest snap-one-glyph-per-character
  (ok (equal '(0 9 18) (xs (snap "abc" (list (glyph 0 9) (glyph 1 9) (glyph 2 9)))))))

(deftest snap-ignores-fractional-advances
  ;; 9.4px advances would drift by a pixel every few characters if added
  ;; up; each glyph stays on its own cell instead.
  (ok (equal '(0 9 18 27 36)
             (xs (snap "abcde" (loop :for i :below 5
                                     :collect (shaper:make-shaped-glyph
                                               :font :f :cluster i :x-advance 602)))))))

(deftest snap-ligature
  ;; One glyph for "->", as wide as its two cells: drawn at the first cell.
  (ok (equal '(0 18) (xs (snap "->x" (list (glyph 0 18) (glyph 2 9)))))))

(deftest snap-centres-narrow-and-wide-glyphs
  ;; A 14px CJK glyph in two 9px cells is centred: (18 - 14) / 2 = 2.
  (ok (equal '(0 11 27) (xs (snap "a日b" (list (glyph 0 9) (glyph 1 14) (glyph 2 9))))))
  ;; A 22px fallback glyph in one 9px cell overhangs equally on both sides.
  (ok (equal '(-7) (xs (snap "★" (list (glyph 0 22))))))
  ;; Within a pixel of the cell width: not moved.
  (ok (equal '(0) (xs (snap "x" (list (shaper:make-shaped-glyph
                                       :font :f :cluster 0 :x-advance 600)))))))

(deftest snap-combining-mark
  ;; e + U+0301: the mark has no advance and an offset back over the e.
  (ok (equal '(0 4)
             (xs (snap (format nil "e~C" (code-char #x301))
                       (list (glyph 0 9) (glyph 1 0 :offset -5)))))))

(deftest snap-several-glyphs-in-one-cluster
  ;; Two glyphs for a cluster covering two characters: placed by their
  ;; advances from the cluster's first cell.
  (ok (equal '(0 9) (xs (snap "ab" (list (glyph 0 9 :id 1) (glyph 0 9 :id 2)))))))

(deftest snap-run-in-the-middle-of-a-string
  ;; A run from index 2 to 3 of "ab日c": columns come from the whole string.
  (ok (equal '(20) (xs (snap "ab日c" (list (glyph 2 14)) :end 3)))))

(deftest itemize-by-coverage
  (let ((covers (lambda (font character)
                  (case font
                    (:latin (char/= character #\日))
                    (:cjk t))))
        (chain (lambda (character)
                 (declare (ignore character))
                 '(:latin :cjk))))
    (ok (equal '((:latin 0 1) (:cjk 1 2) (:latin 2 4))
               (shaper:itemize "a日bc" chain covers)))
    ;; No font covers it: the first font of the chain.
    (ok (equal '((:latin 0 1))
               (shaper:itemize "x" chain (lambda (font character)
                                           (declare (ignore font character))
                                           nil))))))

(defun font-pathname (name)
  (asdf:system-relative-pathname :lem-sdl3 (format nil "resources/fonts/~A" name)))

(defun call-with-fonts (function)
  (let ((mono (shaper:open-shaping-font (font-pathname "NotoSansMono-Regular.ttf") 15))
        (cjk (shaper:open-shaping-font (font-pathname "NotoSansCJK-Regular.ttc") 15)))
    (unwind-protect (funcall function mono cjk)
      (shaper:clear-shaping-cache)
      (shaper:close-shaping-font mono)
      (shaper:close-shaping-font cjk))))

(deftest shape-text-with-fonts
  (if (and (ft:available-p) (hb:available-p))
      (call-with-fonts
       (lambda (mono cjk)
         (let* ((chain (lambda (character)
                         (declare (ignore character))
                         (list mono cjk)))
                (placed (shaper:shape-text "x日x" chain 9)))
           (ok (= 3 (length placed)))
           (ok (equal (list mono cjk mono) (mapcar #'shaper:placed-glyph-font placed)))
           ;; x at cell 0; 日 centred in cells 1-2; x at cell 3.
           (ok (= 0 (first (xs placed))))
           (ok (< 9 (second (xs placed)) 18))
           (ok (= 27 (third (xs placed)))))))
      (skip "FreeType or HarfBuzz is not installed")))

(deftest shape-text-cache
  (if (and (ft:available-p) (hb:available-p))
      (call-with-fonts
       (lambda (mono cjk)
         (declare (ignore cjk))
         (let ((chain (lambda (character)
                        (declare (ignore character))
                        (list mono))))
           (ok (eq (shaper:shape-text "abc" chain 9 :cache-key :mono)
                   (shaper:shape-text "abc" chain 9 :cache-key :mono)))
           (let ((shaper:*max-cached-runs* 1))
             (shaper:shape-text "def" chain 9 :cache-key :mono)
             (ok (equal '(0 9 18) (xs (shaper:shape-text "abc" chain 9 :cache-key :mono))))))))
      (skip "FreeType or HarfBuzz is not installed")))
