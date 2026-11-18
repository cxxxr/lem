(defpackage :lem-sdl3/text-shaping
  (:use :cl)
  (:local-nicknames (:atlas :lem-sdl3/glyph-atlas)
                    (:shaper :lem-sdl3/shaper)
                    (:hb :lem-sdl3/ffi-harfbuzz))
  (:export :*use-shaping*
           :start-shaping
           :stop-shaping
           :shaping-active-p
           :clear-shaping
           :draw-shaped-text))
(in-package :lem-sdl3/text-shaping)

;;; Text runs shaped with HarfBuzz (lem-sdl3/shaper), snapped to the cell
;;; grid and drawn from the glyph atlas (lem-sdl3/glyph-atlas).  This gives
;;; programming ligatures, puts glyphs from every font on one baseline and
;;; centres CJK and fallback glyphs in their cells.
;;;
;;; OpenType features come from the :sdl3-font-features config, a list of
;;; HarfBuzz feature strings; '("-liga" "-calt") turns ligatures off.  The
;;; config is read when shaping first draws and again after a font change,
;;; not per run: lem:config reads the settings file on every call.

(defvar *use-shaping* nil
  "When true, plain text runs are shaped with HarfBuzz and drawn from the
glyph atlas, if FreeType and HarfBuzz are installed.")

(defvar *atlas* nil
  "The glyph atlas of the display's renderer, or NIL when shaping is not
available.")

(defvar *shaping-cache* (shaper:make-shaping-cache)
  "The shaped runs drawn so far.")

(defvar *font-features* nil
  "The parsed :sdl3-font-features config as a list (ARRAY COUNT), or NIL
until it is next read.")

(defun start-shaping (renderer)
  "Create the glyph atlas for RENDERER if FreeType and HarfBuzz are there."
  (when (and (lem-sdl3/ffi-freetype:available-p) (hb:available-p))
    (setf *atlas* (atlas:make-glyph-atlas renderer))))

(defun free-features ()
  (when *font-features*
    (hb:free-features (first *font-features*))
    (setf *font-features* nil)))

(defun clear-shaping ()
  "Forget all shaped runs and atlas glyphs, and read the font features
config again on the next draw.  Call this before closing a font they use."
  (when *atlas*
    (atlas:flush-glyphs *atlas*)
    (atlas:clear-glyph-atlas *atlas*))
  (shaper:clear-shaping-cache *shaping-cache*)
  (free-features))

(defun stop-shaping ()
  "Free the glyph atlas and everything shaped, before the renderer goes."
  (clear-shaping)
  (when *atlas*
    (atlas:destroy-glyph-atlas *atlas*)
    (setf *atlas* nil)))

(defun shaping-active-p ()
  "Return true if text runs should be shaped: it is enabled and the glyph
atlas exists."
  (and *use-shaping* *atlas* t))

(defun features ()
  "Return the parsed :sdl3-font-features as values ARRAY and COUNT, reading
the config if it has not been read since shaping started or the font last
changed.  A feature HarfBuzz cannot parse is reported and the config
ignored."
  (unless *font-features*
    (multiple-value-bind (array count)
        (handler-case (hb:parse-features (lem:config :sdl3-font-features '()))
          (error (condition)
            (lem:message "~A" condition)
            (hb:parse-features '())))
      (setf *font-features* (list array count))))
  (values (first *font-features*) (second *font-features*)))

(defun draw-shaped-text (string chain-function cache-key x baseline cell-width color)
  "Draw STRING with its first cell at X, on the row whose baseline is at
y = BASELINE, in COLOR.  CHAIN-FUNCTION gives each character's shaping
fonts in fallback order, and CACHE-KEY identifies its choices (see
shaper:shape-text)."
  (multiple-value-bind (features feature-count) (features)
    (dolist (glyph (shaper:shape-text string chain-function cell-width
                                      :cache *shaping-cache*
                                      :cache-key cache-key
                                      :features features
                                      :feature-count feature-count))
      (atlas:queue-glyph *atlas*
                         (shaper:placed-glyph-font glyph)
                         (shaper:placed-glyph-id glyph)
                         (+ x (shaper:placed-glyph-x glyph))
                         (- baseline (shaper:placed-glyph-y-offset glyph))
                         color)))
  (atlas:flush-glyphs *atlas*))
