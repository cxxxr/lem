(defpackage :lem-sdl3/text-shaping
  (:use :cl)
  (:local-nicknames (:atlas :lem-sdl3/glyph-atlas)
                    (:shaper :lem-sdl3/shaper)
                    (:hb :lem-sdl3/ffi-harfbuzz))
  (:export :*use-shaping*
           :text-shaper
           :make-text-shaper
           :destroy-text-shaper
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

(defvar *use-shaping* t
  "When true, plain text runs are shaped with HarfBuzz and drawn from the
glyph atlas, if FreeType and HarfBuzz are installed.  Set it to NIL to draw
each character with SDL_ttf instead.")

(defstruct (text-shaper (:constructor %make-text-shaper (atlas)))
  "Shaped text for one renderer: its glyph atlas, the runs shaped so far,
and the font features they were shaped with."
  ;; The renderer's glyph atlas, or NIL to shape without drawing.
  atlas
  (cache (shaper:make-shaping-cache))
  ;; The parsed :sdl3-font-features config as a list (ARRAY COUNT), or NIL
  ;; until it is next read.
  (font-features nil))

(defun make-text-shaper (renderer)
  "Return a text shaper drawing with RENDERER, or NIL if FreeType or
HarfBuzz is not installed."
  (when (and (lem-sdl3/ffi-freetype:available-p) (hb:available-p))
    (%make-text-shaper (atlas:make-glyph-atlas renderer))))

(defun free-features (shaper)
  (when (text-shaper-font-features shaper)
    (hb:free-features (first (text-shaper-font-features shaper)))
    (setf (text-shaper-font-features shaper) nil)))

(defun clear-shaping (shaper)
  "Forget all the runs and atlas glyphs SHAPER holds, and read the font
features config again on the next draw.  Call this before closing a font
they use."
  (let ((atlas (text-shaper-atlas shaper)))
    (when atlas
      (atlas:flush-glyphs atlas)
      (atlas:clear-glyph-atlas atlas)))
  (shaper:clear-shaping-cache (text-shaper-cache shaper))
  (free-features shaper))

(defun destroy-text-shaper (shaper)
  "Free SHAPER's glyph atlas and everything it shaped, before the renderer
goes."
  (clear-shaping shaper)
  (when (text-shaper-atlas shaper)
    (atlas:destroy-glyph-atlas (text-shaper-atlas shaper))
    (setf (text-shaper-atlas shaper) nil)))

(defun shaping-active-p (shaper)
  "Return true if text runs should be shaped with SHAPER: it exists and
shaping is enabled."
  (and *use-shaping* shaper t))

(defun features (shaper)
  "Return the parsed :sdl3-font-features as values ARRAY and COUNT, reading
the config if SHAPER has not read it since it was made or last cleared.  If
HarfBuzz cannot parse a feature, the default features are used and the
editor-error is sent to the event loop, since signalling it here would abort
the redraw this runs in."
  (unless (text-shaper-font-features shaper)
    (multiple-value-bind (array count)
        (handler-case (hb:parse-features (lem:config :sdl3-font-features '()))
          (error (condition)
            (let ((message (princ-to-string condition)))
              (lem:send-event (lambda () (lem:editor-error "~A" message))))
            (hb:parse-features '())))
      (setf (text-shaper-font-features shaper) (list array count))))
  (values (first (text-shaper-font-features shaper))
          (second (text-shaper-font-features shaper))))

(defun draw-shaped-text (shaper string chain-function cache-key x baseline cell-width color)
  "Draw STRING with SHAPER, its first cell at X, on the row whose baseline
is at y = BASELINE, in COLOR.  CHAIN-FUNCTION gives each character's shaping
fonts in fallback order, and CACHE-KEY identifies its choices (see
shaper:shape-text)."
  (let ((atlas (text-shaper-atlas shaper)))
    (multiple-value-bind (features feature-count) (features shaper)
      (dolist (glyph (shaper:shape-text string chain-function cell-width
                                        :cache (text-shaper-cache shaper)
                                        :cache-key cache-key
                                        :features features
                                        :feature-count feature-count))
        (atlas:queue-glyph atlas
                           (shaper:placed-glyph-font glyph)
                           (shaper:placed-glyph-id glyph)
                           (+ x (shaper:placed-glyph-x glyph))
                           (- baseline (shaper:placed-glyph-y-offset glyph))
                           color)))
    (atlas:flush-glyphs atlas)))
