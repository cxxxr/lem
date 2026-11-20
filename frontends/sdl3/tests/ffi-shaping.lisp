(defpackage :lem-sdl3/tests/ffi-shaping
  (:use :cl :rove)
  (:local-nicknames (:ft :lem-sdl3/ffi-freetype)
                    (:hb :lem-sdl3/ffi-harfbuzz)))
(in-package :lem-sdl3/tests/ffi-shaping)

;;; Tests for the FreeType and HarfBuzz bindings, with the NotoSansMono font
;;; that ships with Lem.  They need no window.  They are skipped when either
;;; library is not installed.

(defun mono-font-pathname ()
  (asdf:system-relative-pathname :lem-sdl3 "resources/fonts/NotoSansMono-Regular.ttf"))

(defun libraries-available-p ()
  (and (ft:available-p) (hb:available-p)))

(defun call-with-mono-face (size function)
  (let ((face (ft:open-face (mono-font-pathname) size)))
    (unwind-protect (funcall function face)
      (ft:close-face face))))

(defun call-with-mono-font (size function)
  (call-with-mono-face size
                       (lambda (face)
                         (let ((font (hb:make-font face)))
                           (unwind-protect (funcall function face font)
                             (hb:destroy-font font))))))

(deftest struct-layout
  ;; The offsets FreeType 2.15 has on LP64 (Linux, macOS), from offsetof()
  ;; in C.  Windows (LLP64) has a 32-bit long and different offsets, which
  ;; CFFI computes from the same declarations.
  (if (= 8 (cffi:foreign-type-size :long))
      (progn
        (ok (= 152 (cffi:foreign-slot-offset '(:struct ft::face-rec) 'ft::glyph)))
        (ok (= 160 (cffi:foreign-slot-offset '(:struct ft::face-rec) 'ft::size)))
        (ok (= 24 (cffi:foreign-slot-offset '(:struct ft::size-rec) 'ft::metrics)))
        (ok (= 24 (cffi:foreign-slot-offset '(:struct ft::size-metrics) 'ft::ascender)))
        (ok (= 152 (cffi:foreign-slot-offset '(:struct ft::glyph-slot) 'ft::bitmap)))
        (ok (= 192 (cffi:foreign-slot-offset '(:struct ft::glyph-slot) 'ft::bitmap-left)))
        (ok (= 196 (cffi:foreign-slot-offset '(:struct ft::glyph-slot) 'ft::bitmap-top)))
        (ok (= 40 (cffi:foreign-type-size '(:struct ft::bitmap)))))
      (skip "Offsets are checked on LP64 platforms only"))
  (ok (= 20 (cffi:foreign-type-size '(:struct hb::glyph-info))))
  (ok (= 20 (cffi:foreign-type-size '(:struct hb::glyph-position))))
  (ok (= 16 (cffi:foreign-type-size '(:struct hb::feature)))))

(deftest face-metrics-and-coverage
  (if (libraries-available-p)
      (call-with-mono-face
       15
       (lambda (face)
         (ok (plusp (ft:face-ascender face)))
         (ok (minusp (ft:face-descender face)))
         (ok (plusp (ft:char-index face #\a)))
         (ok (zerop (ft:char-index face (code-char #x10fffd))))))
      (skip "FreeType or HarfBuzz is not installed")))

(deftest shape-monospace-text
  (if (libraries-available-p)
      (call-with-mono-font
       15
       (lambda (face font)
         (multiple-value-bind (count infos positions) (hb:shape font "abc")
           (ok (= 3 count))
           (ok (equal '(0 1 2)
                      (loop :for i :below count :collect (hb:glyph-cluster infos i))))
           (ok (equal (map 'list (lambda (c) (ft:char-index face c)) "abc")
                      (loop :for i :below count :collect (hb:glyph-id infos i))))
           (let ((advances (loop :for i :below count :collect (hb:glyph-x-advance positions i))))
             ;; Monospace, and hinted to whole pixels.
             (ok (= 1 (length (remove-duplicates advances))))
             (ok (zerop (mod (first advances) 64)))))))
      (skip "FreeType or HarfBuzz is not installed")))

(deftest shape-with-context
  (if (libraries-available-p)
      (call-with-mono-font
       15
       (lambda (face font)
         (declare (ignore face))
         ;; Clusters index the whole string, not the shaped range.
         (multiple-value-bind (count infos) (hb:shape font "xabcx" :start 1 :end 3)
           (ok (= 2 count))
           (ok (equal '(1 2)
                      (loop :for i :below count :collect (hb:glyph-cluster infos i)))))))
      (skip "FreeType or HarfBuzz is not installed")))

(deftest parse-features
  (if (hb:available-p)
      (multiple-value-bind (features count) (hb:parse-features '("-liga" "calt=0"))
        (ok (= 2 count))
        (hb:free-features features)
        (ok (signals (hb:parse-features '("")))))
      (skip "HarfBuzz is not installed")))

(deftest render-glyph
  (if (libraries-available-p)
      (call-with-mono-face
       15
       (lambda (face)
         (multiple-value-bind (buffer width rows pitch left top)
             (ft:render-glyph face (ft:char-index face #\x))
           (ok (not (cffi:null-pointer-p buffer)))
           (ok (plusp width))
           (ok (plusp rows))
           (ok (>= pitch width))
           (ok (<= 0 left 2))
           ;; An x-height glyph: its top is above the baseline, below the ascender.
           (ok (< 0 top (ft:face-ascender face))))))
      (skip "FreeType or HarfBuzz is not installed")))
