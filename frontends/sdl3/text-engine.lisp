(defpackage :lem-sdl3/text-engine
  (:use :cl)
  (:export :*use-text-engine*
           :text-engine
           :make-text-engine
           :destroy-text-engine
           :text-engine-active-p
           :clear-text-cache
           :draw-glyph))
(in-package :lem-sdl3/text-engine)

;;; Glyphs drawn through SDL_ttf's renderer text engine.
;;;
;;; Instead of rendering each (string, attribute) to its own surface and
;;; texture, every string is a TTF_Text whose glyphs live in the engine's
;;; shared texture atlas.  A text depends only on its string and font: the
;;; color is set before each draw, so one entry serves every face and theme.
;;;
;;; Texts are drawn one cell at a time, like the surfaces they replace.
;;; SDL_ttf positions a shaped run's glyphs at fractional advances, which
;;; drift off Lem's integer cell grid within a line, so runs are not shaped
;;; as a whole.

(defvar *use-text-engine* t
  "When true, text glyphs are drawn with SDL_ttf's text engine.  When false,
they are drawn from per-character surfaces, as before the engine was used.")

(defparameter *max-texts* 4000
  "When more texts than this are cached, all of them are destroyed; the
ones still in use are recreated on the next frame.")

(defstruct (text-engine (:constructor %make-text-engine (pointer)))
  "SDL_ttf's text engine for one renderer, and the texts drawn with it."
  pointer
  ;; TTF_Font pointer address -> (character -> TTF_Text) hash table.
  (texts (make-hash-table))
  ;; Number of texts in TEXTS.
  (text-count 0))

(defun make-text-engine (renderer)
  "Create the text engine for RENDERER."
  (%make-text-engine (lem-sdl3/ffi:create-text-engine renderer)))

(defun destroy-text-engine (engine)
  "Destroy every text ENGINE cached, and then ENGINE."
  (clear-text-cache engine)
  (lem-sdl3/ffi:destroy-text-engine (text-engine-pointer engine)))

(defun text-engine-active-p (engine)
  "Return true if glyphs should be drawn with ENGINE: it exists and the
text engine is enabled."
  (and *use-text-engine* engine t))

(defun clear-text-cache (engine)
  "Destroy every text ENGINE cached.  Call this before closing a font they
use."
  (maphash (lambda (address texts)
             (declare (ignore address))
             (maphash (lambda (string text)
                        (declare (ignore string))
                        (lem-sdl3/ffi:destroy-text text))
                      texts))
           (text-engine-texts engine))
  (clrhash (text-engine-texts engine))
  (setf (text-engine-text-count engine) 0))

(defun get-text (engine font character)
  "Return ENGINE's cached text drawing CHARACTER with FONT, creating it if
needed, or NIL if SDL_ttf cannot create it."
  (let* ((address (cffi:pointer-address font))
         (all-texts (text-engine-texts engine))
         (texts (or (gethash address all-texts)
                    (setf (gethash address all-texts) (make-hash-table)))))
    (or (gethash character texts)
        (progn
          (when (>= (text-engine-text-count engine) *max-texts*)
            (clear-text-cache engine)
            (setf texts (setf (gethash address all-texts) (make-hash-table))))
          (let ((text (lem-sdl3/ffi:create-text (text-engine-pointer engine) font
                                                (string character))))
            (when text
              (incf (text-engine-text-count engine))
              (setf (gethash character texts) text)))))))

(defun draw-glyph (engine font character x y color)
  "Draw CHARACTER with FONT through ENGINE in COLOR (a lem:color) with its
top-left corner at (X, Y).  Return true if it was drawn."
  (let ((text (get-text engine font character)))
    (when text
      (lem-sdl3/ffi:set-text-color text
                                   (lem:color-red color)
                                   (lem:color-green color)
                                   (lem:color-blue color))
      (lem-sdl3/ffi:draw-text text x y)
      t)))
