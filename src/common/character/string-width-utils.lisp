(defpackage :lem/common/character/string-width-utils
  (:use :cl)
  (:export :+default-tab-size+
           :control-char
           :wide-char-p
           :char-width
           :*grapheme-clusters*
           :grapheme-continuation-p
           :string-char-width
           :string-width
           :wide-index))
(in-package :lem/common/character/string-width-utils)

(defconstant +default-tab-size+ 8)

(defvar *grapheme-clusters* nil
  "When true, text is measured, and the cursor moves and deletes, by grapheme
cluster, so an emoji sequence (a flag, a skin tone, a joined family) is one
character two cells wide.  A frontend that draws sequences as one glyph
turns it on; a terminal draws them as it pleases, so it is off by default.")

(defparameter *char-replacement*
  (let ((table (make-hash-table)))
    (setf (gethash (code-char 0) table) "^@")
    (setf (gethash (code-char 1) table) "^A")
    (setf (gethash (code-char 2) table) "^B")
    (setf (gethash (code-char 3) table) "^C")
    (setf (gethash (code-char 4) table) "^D")
    (setf (gethash (code-char 5) table) "^E")
    (setf (gethash (code-char 6) table) "^F")
    (setf (gethash (code-char 7) table) "^G")
    (setf (gethash (code-char 8) table) "^H")
    (setf (gethash (code-char 9) table) "^I")
    (setf (gethash (code-char 11) table) "^K")
    (setf (gethash (code-char 12) table) "^L")
    (setf (gethash (code-char 13) table) "^R")
    (setf (gethash (code-char 14) table) "^N")
    (setf (gethash (code-char 15) table) "^O")
    (setf (gethash (code-char 16) table) "^P")
    (setf (gethash (code-char 17) table) "^Q")
    (setf (gethash (code-char 18) table) "^R")
    (setf (gethash (code-char 19) table) "^S")
    (setf (gethash (code-char 20) table) "^T")
    (setf (gethash (code-char 21) table) "^U")
    (setf (gethash (code-char 22) table) "^V")
    (setf (gethash (code-char 23) table) "^W")
    (setf (gethash (code-char 24) table) "^X")
    (setf (gethash (code-char 25) table) "^Y")
    (setf (gethash (code-char 26) table) "^Z")
    (setf (gethash (code-char 27) table) "^[")
    (setf (gethash (code-char 28) table) "^\\")
    (setf (gethash (code-char 29) table) "^]")
    (setf (gethash (code-char 30) table) "^^")
    (setf (gethash (code-char 31) table) "^_")
    (setf (gethash (code-char 127) table) "^?")
    (loop :for i :from 0 :to #xff
          :do (setf (gethash (code-char (+ #xe000 i)) table)
                    (format nil "\\~D" i)))
    table))

(defun control-char (char)
  (gethash char *char-replacement*))

(defun wide-char-p (char)
  (declare (character char))
  (or (char= char #\▼)
      (lem/common/character/icon:icon-code-p (char-code char))
      (lem/common/character/eastasian:eastasian-code-p (char-code char))
      (control-char char)))

(defun char-width (char width &key (tab-size +default-tab-size+))
  (declare (character char) (fixnum width))
  (cond ((char= char #\tab)
         (+ (* (floor width tab-size) tab-size) tab-size))
        ((char= char #\newline)
         0)
        ((control-char char)
         (loop :for char :across (control-char char)
               :do (setf width (char-width char width :tab-size tab-size)))
         width)
        ((wide-char-p char)
         (+ width 2))
        (t
         (+ width 1))))

(defun regional-indicator-code-p (code)
  "True if CODE is a regional indicator, one letter of a flag."
  (<= #x1F1E6 code #x1F1FF))

(defun grapheme-continuation-p (string index)
  "True if the character at INDEX of STRING continues the grapheme cluster
before it, so the two are one character on screen: a zero-width joiner, a
variation selector, the keycap mark, a tag character, a skin-tone modifier,
the character a joiner joins, or the second regional indicator of a flag."
  (and (< 0 index (length string))
       (let ((code (char-code (char string index)))
             (previous (char-code (char string (1- index)))))
         (or (= code #x200D)
             (= code #xFE0E)
             (= code #xFE0F)
             (= code #x20E3)
             (<= #xE0020 code #xE007F)
             (<= #x1F3FB code #x1F3FF)
             (= previous #x200D)
             (and (regional-indicator-code-p code)
                  (regional-indicator-code-p previous)
                  ;; The previous one starts a flag: an even number of
                  ;; indicators come before it.
                  (evenp (loop :for i :downfrom (- index 2) :to 0
                               :while (regional-indicator-code-p (char-code (char string i)))
                               :count t)))))))

(defun emoji-form-requested-p (string index)
  "True if the character after INDEX of STRING asks for the emoji form of the
one at INDEX: the emoji variation selector or the keycap mark."
  (and (< (1+ index) (length string))
       (member (char-code (char string (1+ index))) '(#xFE0F #x20E3))
       t))

(defun string-char-width (string index width &key (tab-size +default-tab-size+))
  "Return WIDTH after the character at INDEX of STRING, as char-width does,
but by grapheme cluster: a character continuing one (see
grapheme-continuation-p) adds nothing, and a character asked for its emoji
form takes two cells, so an emoji sequence is as wide as one emoji."
  (let ((char (char string index)))
    (cond ((not *grapheme-clusters*)
           (char-width char width :tab-size tab-size))
          ((grapheme-continuation-p string index)
           width)
          ((or (and (emoji-form-requested-p string index)
                    (not (char= char #\tab))
                    (not (control-char char)))
               ;; A flag's first letter, drawn as one emoji with the second.
               (regional-indicator-code-p (char-code char)))
           (+ width 2))
          (t
           (char-width char width :tab-size tab-size)))))

(defun string-width (string &key (start 0) end (tab-size +default-tab-size+))
  (let* ((len (length string))
         (safe-end (min (or end len)
                        len)))
    (loop :with width := 0
          :for index :from start :below safe-end
          :do (setq width (string-char-width string index width :tab-size tab-size))
          :finally (return width))))

(defun wide-index (string goal &key (start 0) (tab-size +default-tab-size+))
  (loop :with width := 0
        :for index :from start :below (length string)
        :do (setq width (string-char-width string index width :tab-size tab-size))
            (when (< goal width)
              (return index))))
