(in-package :lem-core)

(deftype char-type ()
  "The kind of a character, as returned by `char-type'.  Frontends use it to
choose a font: :latin (ASCII and Latin-1 to Latin Extended-B), :cjk (wide
characters), :other (any other one-cell character), :braille, :emoji, :icon,
:control and :zero-width."
  '(member :latin :cjk :other :braille :emoji :icon :control :zero-width))

(defun cjk-char-code-p (code)
  (or (<= #x4E00 code #x9FFF)
      (<= #x3040 code #x309F)
      (<= #x30A0 code #x30FF)
      (<= #xAC00 code #xD7A3)))

(defun latin-char-code-p (code)
  (or (<= #x0000 code #x007F)
      (<= #x0080 code #x00FF)
      (<= #x0100 code #x017F)
      (<= #x0180 code #x024F)))

(defun emoji-char-code-p (code)
  (or (<= #x1F300 code #x1F6FF)
      (<= #x1F900 code #x1F9FF)
      (<= #x1F600 code #x1F64F)
      (<= #x1F700 code #x1F77F)))

(defun braille-char-code-p (code)
  (<= #x2800 code #x28ff))

(defun icon-char-code-p (code)
  (icon-value code :font))

(defun zero-width-char-code-p (code)
  (or (<= #x200B code #x200D)
      (<= #xFE00 code #xFE0F)
      (= code #xFEFF)))

(defun char-type (char)
  "Return CHAR's `char-type'.  A character in none of the known ranges is
:cjk if it is wide (two cells, see `wide-char-p') and :other otherwise,
e.g. Greek, Cyrillic, arrows, math symbols and box drawing."
  (let ((code (char-code char)))
    (cond ((control-char char)
           :control)
          ((eql code #x1f4c1)
           :folder)
          ((<= code 128)
           :latin)
          ((icon-char-code-p code)
           :icon)
          ((braille-char-code-p code)
           :braille)
          ((cjk-char-code-p code)
           :cjk)
          ((latin-char-code-p code)
           :latin)
          ((emoji-char-code-p code)
           :emoji)
          ((icon-code-p code)
           :latin)
          ((zero-width-char-code-p code)
           :zero-width)
          ((wide-char-p char)
           :cjk)
          (t
           :other))))
