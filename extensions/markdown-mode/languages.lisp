(defpackage :lem-markdown-mode/languages
  (:use :cl)
  (:export :find-mode-by-language-name))
(in-package :lem-markdown-mode/languages)

(defparameter *language-mode-pairs*
  '(("common-lisp" "LEM-LISP-MODE" "LISP-MODE")
    ("lisp" "LEM-LISP-MODE" "LISP-MODE")
    ("emacs-lisp" "LEM-ELISP-MODE" "ELISP-MODE")
    ("shell" "LEM-POSIX-SHELL-MODE" "POSIX-SHELL-MODE")
    ("json" "LEM-JSON-MODE" "JSON-MODE"))
  "Language names of code blocks and their modes, as (NAME PACKAGE SYMBOL):
a mode is used when its package is loaded.")

(defun language-pair-mode (pair)
  (destructuring-bind (package-name symbol-name) (rest pair)
    (let ((package (find-package package-name)))
      (and package (find-symbol symbol-name package)))))

(defun find-mode-by-language-name (language-name)
  (let ((pair (assoc language-name *language-mode-pairs* :test #'equal)))
    (or (and pair (language-pair-mode pair))
        (lem:find-mode language-name))))
