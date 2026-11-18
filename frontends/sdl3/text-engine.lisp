(defpackage :lem-sdl3/text-engine
  (:use :cl)
  (:export :*use-text-engine*
           :start-text-engine
           :stop-text-engine
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

(defvar *engine* nil
  "The text engine of the display's renderer, or NIL before it is created.")

(defvar *texts* (make-hash-table)
  "TTF_Font pointer address -> (string -> TTF_Text) hash table.")

(defvar *text-count* 0
  "Number of texts in *TEXTS*.")

(defparameter *max-texts* 4000
  "When more texts than this are cached, all of them are destroyed; the
ones still in use are recreated on the next frame.")

(defun start-text-engine (renderer)
  "Create the text engine for RENDERER."
  (setf *engine* (lem-sdl3/ffi:create-text-engine renderer)))

(defun stop-text-engine ()
  "Destroy every cached text and then the text engine."
  (clear-text-cache)
  (when *engine*
    (lem-sdl3/ffi:destroy-text-engine *engine*)
    (setf *engine* nil)))

(defun text-engine-active-p ()
  (and *use-text-engine* *engine* t))

(defun clear-text-cache ()
  "Destroy every cached text.  Call this before closing a font they use."
  (maphash (lambda (address texts)
             (declare (ignore address))
             (maphash (lambda (string text)
                        (declare (ignore string))
                        (lem-sdl3/ffi:destroy-text text))
                      texts))
           *texts*)
  (clrhash *texts*)
  (setf *text-count* 0))

(defun get-text (font string)
  "Return the cached text drawing STRING with FONT, creating it if needed,
or NIL if SDL_ttf cannot create it."
  (let* ((address (cffi:pointer-address font))
         (texts (or (gethash address *texts*)
                    (setf (gethash address *texts*) (make-hash-table :test 'equal)))))
    (or (gethash string texts)
        (progn
          (when (>= *text-count* *max-texts*)
            (clear-text-cache)
            (setf texts (setf (gethash address *texts*) (make-hash-table :test 'equal))))
          (let ((text (lem-sdl3/ffi:create-text *engine* font string)))
            (when text
              (incf *text-count*)
              (setf (gethash string texts) text)))))))

(defun draw-glyph (font string x y color)
  "Draw STRING with FONT in COLOR (a lem:color) with its top-left corner at
\(X, Y).  Return true if it was drawn."
  (let ((text (get-text font string)))
    (when text
      (lem-sdl3/ffi:set-text-color text
                                   (lem:color-red color)
                                   (lem:color-green color)
                                   (lem:color-blue color))
      (lem-sdl3/ffi:draw-text text x y)
      t)))
