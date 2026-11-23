(defpackage :lem-tests/char-type
  (:use :cl :rove)
  (:import-from :lem-core/display
                :char-type))
(in-package :lem-tests/char-type)

(deftest latin-characters
  (dolist (character '(#\a #\Z #\0 #\~ #\é #\ß #\ő #\ƀ))
    (ok (eq :latin (char-type character)) (format nil "~S" character))))

(deftest wide-characters-are-cjk
  (dolist (character '(#\日 #\あ #\ア #\한 #\ㄱ #\。 #\Ａ))
    (ok (eq :cjk (char-type character)) (format nil "~S" character))))

(deftest one-cell-characters-are-other
  ;; These used to be :cjk, so frontends drew them with the CJK font, which
  ;; draws many of them full-width in a one-cell slot.
  (dolist (character '(#\α #\Ω #\Ж #\я #\→ #\∀ #\≤ #\─ #\│ #\█ #\★))
    (ok (eq :other (char-type character)) (format nil "~S" character))
    (ok (not (lem:wide-char-p character)) (format nil "~S is one cell" character))))

(deftest other-types-are-unchanged
  (ok (eq :braille (char-type #\⠿)))
  (ok (eq :zero-width (char-type (code-char #x200b))))
  (ok (eq :control (char-type (code-char 1))))
  (ok (eq :emoji (char-type (code-char #x1f600)))))

(deftest char-type-results-are-of-type-char-type
  (dolist (character (list #\a #\日 #\α #\⠿ (code-char #x200b) (code-char 1)
                           (code-char #x1f600)))
    (ok (typep (char-type character) 'char-type) (format nil "~S" character))))

(defun string-of (&rest codes)
  (map 'string #'code-char codes))

(deftest emoji-in-every-block-are-emoji
  ;; ✋ ✅ ☕ ⚡ ⭐ (emoji presentation below U+1F000), a regional indicator,
  ;; a coloured circle, 🫠 and 🀄.
  (dolist (code '(#x270B #x2705 #x2615 #x26A1 #x2B50 #x1F1EF #x1F7E0 #x1FAE0 #x1F004
                  #x1F600 #x1F680))
    (ok (eq :emoji (char-type (code-char code))) (format nil "U+~X" code)))
  ;; Symbols that default to text keep their type: ☺ ♥ ✔ ✈ ❤.
  (dolist (code '(#x263A #x2665 #x2714 #x2708 #x2764))
    (ng (eq :emoji (char-type (code-char code))) (format nil "U+~X stays text" code))))

;;; split-string-by-character-type is internal to lem-core's display; it is
;;; tested directly because it decides which characters form one glyph.
(deftest emoji-sequences-are-one-run
  (flet ((runs (string)
           (lem-core::split-string-by-character-type string)))
    (ok (equal (list (cons :emoji (string-of #x1F468 #x200D #x1F469 #x200D #x1F467)))
               (runs (string-of #x1F468 #x200D #x1F469 #x200D #x1F467)))
        "a zero-width-joiner family")
    (ok (equal (list (cons :emoji (string-of #x1F1EF #x1F1F5)))
               (runs (string-of #x1F1EF #x1F1F5)))
        "a flag's two letters")
    (ok (equal (list (cons :emoji (string-of #x2764 #xFE0F)))
               (runs (string-of #x2764 #xFE0F)))
        "a text symbol asking for its emoji form")
    (ok (equal (list (cons :latin "a") (cons :emoji (string-of #x31 #xFE0F #x20E3)))
               (runs (string-of #x61 #x31 #xFE0F #x20E3)))
        "a keycap after a letter")
    (ok (equal (list (cons :emoji (string-of #x1F3C3 #x200D #x2640 #xFE0F)))
               (runs (string-of #x1F3C3 #x200D #x2640 #xFE0F)))
        "a joiner takes a text symbol and its variation selector")
    (ok (equal (list (cons :emoji (string-of #x1F44D #x1F3FD)) (cons :latin " ok"))
               (runs (string-of #x1F44D #x1F3FD #x20 #x6F #x6B)))
        "a skin tone stays with its emoji, and text after it is its own run")
    (ok (equal (list (cons :other (string-of #x2764)) (cons :latin " "))
               (runs (string-of #x2764 #x20)))
        "a bare text symbol stays text")))
