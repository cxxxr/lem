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
