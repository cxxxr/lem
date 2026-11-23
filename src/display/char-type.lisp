(in-package :lem-core)

(defparameter *emoji-presentation-ranges*
  '((#x231A . #x231B) (#x23E9 . #x23EC) (#x23F0 . #x23F0) (#x23F3 . #x23F3)
    (#x25FD . #x25FE) (#x2614 . #x2615) (#x2648 . #x2653) (#x267F . #x267F)
    (#x2693 . #x2693) (#x26A1 . #x26A1) (#x26AA . #x26AB) (#x26BD . #x26BE)
    (#x26C4 . #x26C5) (#x26CE . #x26CE) (#x26D4 . #x26D4) (#x26EA . #x26EA)
    (#x26F2 . #x26F3) (#x26F5 . #x26F5) (#x26FA . #x26FA) (#x26FD . #x26FD)
    (#x2705 . #x2705) (#x270A . #x270B) (#x2728 . #x2728) (#x274C . #x274C)
    (#x274E . #x274E) (#x2753 . #x2755) (#x2757 . #x2757) (#x2795 . #x2797)
    (#x27B0 . #x27B0) (#x27BF . #x27BF) (#x2B1B . #x2B1C) (#x2B50 . #x2B50)
    (#x2B55 . #x2B55))
  "The characters below U+1F000 that Unicode presents as emoji by default
(Emoji_Presentation=Yes), as (FIRST . LAST) code ranges.")

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
  "True if CODE is drawn as an emoji by default: the emoji blocks from
mahjong tiles to Symbols and Pictographs Extended-A, regional indicators
(flag letters), and the older symbols with emoji presentation."
  (or (<= #x1F000 code #x1F02F)         ; mahjong tiles
      (<= #x1F0A0 code #x1F0FF)         ; playing cards
      (<= #x1F18E code #x1F18E)         ; 🆎
      (<= #x1F191 code #x1F19A)         ; 🆑 .. 🆚
      (<= #x1F1E6 code #x1F1FF)         ; regional indicators
      (<= #x1F200 code #x1F2FF)         ; enclosed ideographs
      (<= #x1F300 code #x1F6FF)         ; pictographs, emoticons, transport
      (<= #x1F700 code #x1F77F)
      (<= #x1F7E0 code #x1F7FF)         ; coloured circles and squares
      (<= #x1F900 code #x1F9FF)         ; supplemental symbols and pictographs
      (<= #x1FA70 code #x1FAFF)         ; symbols and pictographs extended-A
      (and (<= #x2300 code #x2BFF)
           (find-if (lambda (range) (<= (car range) code (cdr range)))
                    *emoji-presentation-ranges*)
           t)))

(defun emoji-sequence-code-p (code)
  "True if CODE continues an emoji sequence rather than standing alone: the
zero-width joiner, the emoji variation selector, the keycap mark and the tag
characters of subdivision flags."
  (or (= code #x200D)
      (= code #xFE0F)
      (= code #x20E3)
      (<= #xE0020 code #xE007F)))

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
