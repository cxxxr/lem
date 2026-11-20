(defpackage :lem-sdl3/shaper
  (:use :cl)
  (:local-nicknames (:ft :lem-sdl3/ffi-freetype)
                    (:hb :lem-sdl3/ffi-harfbuzz))
  (:export :*max-cached-runs*
           :shaping-font
           :shaping-font-face
           :shaping-font-hb-font
           :open-shaping-font
           :close-shaping-font
           :shaping-font-covers-p
           :shaped-glyph
           :make-shaped-glyph
           :placed-glyph
           :placed-glyph-font
           :placed-glyph-id
           :placed-glyph-x
           :placed-glyph-y-offset
           :character-columns
           :itemize
           :snap-glyphs
           :shape-text
           :shaping-cache
           :make-shaping-cache
           :clear-shaping-cache))
(in-package :lem-sdl3/shaper)

;;; Shaping text onto Lem's cell grid.
;;;
;;; HarfBuzz positions glyphs by adding up their (fractional) advances.  Lem
;;; instead gives every character a whole number of cells.  So a shaped
;;; run's advances are never accumulated across clusters: each cluster is
;;; placed at the first cell of its characters, and only glyphs within one
;;; cluster are positioned relative to each other.  Text can then not drift
;;; off the grid, whatever the font's advances.

(defparameter *max-cached-runs* 4000
  "When more shaped runs than this are cached, the cache is emptied; runs
still on screen are shaped again on the next frame.")

(defstruct shaping-cache
  "Shaped runs, so that a line drawn again is not shaped again."
  ;; Cache key -> (string -> list of placed glyphs).
  (runs (make-hash-table :test 'equal))
  ;; Number of runs in RUNS.
  (count 0))

(defstruct (shaping-font (:constructor %make-shaping-font))
  "A FreeType face and the HarfBuzz font shaping with it."
  face
  hb-font)

(defstruct shaped-glyph
  "A glyph as HarfBuzz shaped it.  CLUSTER indexes the shaped string; the
advance and offsets are in 26.6 fixed point."
  font
  (id 0)
  (cluster 0)
  (x-advance 0)
  (x-offset 0)
  (y-offset 0))

(defstruct placed-glyph
  "A glyph placed on the cell grid: X is in pixels from the start of the run,
Y-OFFSET in pixels up from the baseline."
  font
  (id 0)
  (x 0)
  (y-offset 0))

(defun open-shaping-font (pathname size)
  "Open the font file PATHNAME at point SIZE for shaping and rasterizing.
Close it with CLOSE-SHAPING-FONT."
  (let ((face (ft:open-face pathname size)))
    (%make-shaping-font :face face :hb-font (hb:make-font face))))

(defun close-shaping-font (font)
  "Close FONT, made by OPEN-SHAPING-FONT."
  (hb:destroy-font (shaping-font-hb-font font))
  (ft:close-face (shaping-font-face font)))

(defun shaping-font-covers-p (font character)
  "Return true if FONT has a glyph for CHARACTER."
  (plusp (ft:char-index (shaping-font-face font) character)))

(defun character-columns (string)
  "Return a vector of the column each character of STRING starts at, with
the column after the last character at the end."
  (let ((columns (make-array (1+ (length string))))
        (column 0))
    (loop :for i :from 0
          :for character :across string
          :do (setf (aref columns i) column
                    column (lem:char-width character column)))
    (setf (aref columns (length string)) column)
    columns))

(defun itemize (string chain-function &optional (covers-p #'shaping-font-covers-p))
  "Split STRING into runs of characters drawn with the same font.  For each
character, CHAIN-FUNCTION returns the fonts to try in order; the first that
COVERS-P the character is used, or the first of the chain if none does.
Return a list of (FONT START END)."
  (let ((runs '()))
    (loop :for i :from 0
          :for character :across string
          :for chain := (funcall chain-function character)
          :for font := (or (find-if (lambda (font) (funcall covers-p font character)) chain)
                           (first chain))
          :do (if (and runs (eq font (first (first runs))))
                  (setf (third (first runs)) (1+ i))
                  (push (list font i (1+ i)) runs)))
    (nreverse runs)))

(defun shape-run (font string start end features feature-count)
  "Shape the characters of STRING from START to END with FONT, the rest of
STRING as context, and return the glyphs as a list of shaped-glyph."
  (multiple-value-bind (count infos positions)
      (hb:shape (shaping-font-hb-font font) string
                :start start :end end
                :features features :feature-count feature-count)
    (loop :for i :below count
          :collect (make-shaped-glyph :font font
                                      :id (hb:glyph-id infos i)
                                      :cluster (hb:glyph-cluster infos i)
                                      :x-advance (hb:glyph-x-advance positions i)
                                      :x-offset (hb:glyph-x-offset positions i)
                                      :y-offset (hb:glyph-y-offset positions i)))))

(defun snap-glyphs (glyphs columns cell-width end)
  "Place GLYPHS, shaped from a run of characters ending at index END, on the
cell grid.  COLUMNS is from CHARACTER-COLUMNS for the whole string.  Each
cluster starts at its first character's cell.  A cluster of one glyph whose
advance differs from its cells' width by a pixel or more (a CJK or
proportional fallback glyph) is centred in its cells; glyphs with no
advance (combining marks) are not, since their offsets are relative to the
pen.  Return a list of placed-glyph."
  (let ((placed '()))
    (loop :while glyphs
          :do (let* ((cluster (shaped-glyph-cluster (first glyphs)))
                     (group (loop :while (and glyphs (= cluster (shaped-glyph-cluster (first glyphs))))
                                  :collect (pop glyphs)))
                     (next (if glyphs (shaped-glyph-cluster (first glyphs)) end))
                     (cell-x (* cell-width (aref columns cluster)))
                     (span (* cell-width (- (aref columns next) (aref columns cluster))))
                     (advance (/ (shaped-glyph-x-advance (first group)) 64))
                     (centre (if (and (null (rest group))
                                      (plusp advance)
                                      (>= (abs (- advance span)) 1))
                                 (floor (- span advance) 2)
                                 0))
                     (pen 0))
                (dolist (glyph group)
                  (push (make-placed-glyph :font (shaped-glyph-font glyph)
                                           :id (shaped-glyph-id glyph)
                                           :x (+ cell-x centre pen
                                                 (round (shaped-glyph-x-offset glyph) 64))
                                           :y-offset (round (shaped-glyph-y-offset glyph) 64))
                        placed)
                  (incf pen (round (shaped-glyph-x-advance glyph) 64)))))
    (nreverse placed)))

(defun clear-shaping-cache (cache)
  "Forget every run CACHE holds.  Call this before closing a font they use."
  (clrhash (shaping-cache-runs cache))
  (setf (shaping-cache-count cache) 0))

(defun shape-text (string chain-function cell-width
                   &key cache cache-key (features (cffi:null-pointer)) (feature-count 0))
  "Shape STRING onto the cell grid and return its glyphs as a list of
placed-glyph, X relative to the start of STRING.  CHAIN-FUNCTION gives each
character's fonts in fallback order (see ITEMIZE).  When CACHE, a
shaping-cache, and CACHE-KEY are given, the result is cached under the key
and STRING: the key must identify CHAIN-FUNCTION's choices, CELL-WIDTH and
FEATURES."
  (flet ((shape ()
           (let ((columns (character-columns string)))
             (loop :for (font start end) :in (itemize string chain-function)
                   :nconc (snap-glyphs (shape-run font string start end features feature-count)
                                       columns cell-width end)))))
    (if (or (null cache) (null cache-key))
        (shape)
        (let* ((all-runs (shaping-cache-runs cache))
               (runs (or (gethash cache-key all-runs)
                         (setf (gethash cache-key all-runs) (make-hash-table :test 'equal)))))
          (multiple-value-bind (placed found) (gethash string runs)
            (if found
                placed
                (progn
                  (when (>= (shaping-cache-count cache) *max-cached-runs*)
                    (clear-shaping-cache cache)
                    (setf runs (setf (gethash cache-key all-runs) (make-hash-table :test 'equal))))
                  (incf (shaping-cache-count cache))
                  (setf (gethash string runs) (shape)))))))))
