;;;; extensions.lisp — Lem's extensions, loaded when they are first used
;;;;
;;;; Loading all of Lem's language modes, vi-mode and the color themes when
;;;; the page starts would make it start seconds later.  Their systems are
;;;; compiled with the rest (prepare.lisp), and here each is known only by
;;;; what makes it needed: the file types of a mode, the command that turns
;;;; it on, LOAD-THEME for the themes.  The first use loads the system with
;;;; ASDF (in a second or so) and then does what was asked; the extension's
;;;; own definitions replace these.  They load on Clysm as they are; what
;;;; they would need a language server or a web browser for is in systems
;;;; of their own (setup.lisp).

(in-package :lem-clysm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *extensions*
    ;; (system package mode-command file-types file-names)
    '(;; Lem's Lisp mode, with micros (lem-clysm-lisp.asd): most of a second
      ("lem-clysm-lisp" "LEM-LISP-MODE" "LISP-MODE" ("lisp" "asd" "cl" "lsp" "ros"))
      ("lem-c-mode" "LEM-C-MODE" "C-MODE" ("c" "h"))
      ("lem-css-mode" "LEM-CSS-MODE" "CSS-MODE" ("css" "scss"))
      ("lem-go-mode" "LEM-GO-MODE" "GO-MODE" ("go"))
      ("lem-html-mode" "LEM-HTML-MODE" "HTML-MODE" ("html" "htm"))
      ("lem-js-mode" "LEM-JS-MODE" "JS-MODE" ("js" "jsx" "mjs" "cjs"))
      ("lem-json-mode" "LEM-JSON-MODE" "JSON-MODE" ("json"))
      ("lem-makefile-mode" "LEM-MAKEFILE-MODE" "MAKEFILE-MODE" ("mk") ("Makefile" "makefile" "GNUmakefile"))
      ("lem-markdown-mode" "LEM-MARKDOWN-MODE" "MARKDOWN-MODE" ("md" "markdown"))
      ("lem-posix-shell-mode" "LEM-POSIX-SHELL-MODE" "POSIX-SHELL-MODE" ("sh" "bash" "bashrc" "profile"))
      ("lem-python-mode" "LEM-PYTHON-MODE" "PYTHON-MODE" ("py"))
      ("lem-rust-mode" "LEM-RUST-MODE" "RUST-MODE" ("rs"))
      ("lem-sql-mode" "LEM-SQL-MODE" "SQL-MODE" ("sql"))
      ("lem-toml-mode" "LEM-TOML-MODE" "TOML-MODE" ("toml"))
      ("lem-typescript-mode" "LEM-TYPESCRIPT-MODE" "TYPESCRIPT-MODE" ("ts" "tsx"))
      ("lem-xml-mode" "LEM-XML-MODE" "XML-MODE" ("xml" "svg"))
      ("lem-yaml-mode" "LEM-YAML-MODE" "YAML-MODE" ("yaml" "yml"))
      ("lem-vi-mode" "LEM-VI-MODE" "VI-MODE" ()))
    "The extensions loaded when first used.")

  (defparameter *extension-commands*
    ;; (system package command key): other commands that load an extension
    '(("lem-clysm-lisp" "LEM-LISP-MODE" "START-LISP-REPL" nil)
      ("lem-clysm-lisp" "LEM-LISP-MODE" "LISP-EVAL-STRING" "M-:")
      ("lem-clysm-lisp" "LEM-CLYSM" "WORLD-START" nil)
      ("lem-clysm-lisp" "LEM-CLYSM" "WORLD-KILL" nil))
    "Commands of the extensions, besides their modes, that load them."))

(defparameter *theme-system* "lem-base16-themes"
  "The system of the color themes beyond Lem's own (base16, about 190).")

(defun extension-systems ()
  "The names of the systems of all the extensions (build-web.sh packs their fasls)."
  (cons *theme-system* (mapcar #'first *extensions*)))

(sys::%defvar-host *show-before-loading*
  (js:eval "() => new Promise((resolve) => requestAnimationFrame(() => setTimeout(resolve, 0)))"))

(defvar *after-extension-load* '()
  "(system . function): FUNCTION is called when the extension SYSTEM is loaded.")

(defun load-extension (system &key quiet)
  "Load the extension SYSTEM if it is not loaded yet; with QUIET, without
telling it (before the editor has started)."
  ;; (Lem was loaded without ASDF's search of systems: load-lem.lisp)
  (when (fboundp 'cl-user::configure-lem-asdf)
    (funcall 'cl-user::configure-lem-asdf))
  (unless (asdf:component-loaded-p system)
    (unless quiet
      (lem:message "Loading ~A..." system)
      (lem:redraw-display)
      ;; let the page show the message (the editor can wait)
      (when (js:can-await-p)
        (js:await (js:funcall *show-before-loading*))))
    (let ((*compile-verbose* nil) (*load-verbose* nil))
      (asdf:load-system system))
    (loop for (name . function) in *after-extension-load*
          when (string= name system)
            do (funcall function))
    (unless quiet
      (lem:message nil))))

;;; For each mode, a command named as the mode (M-x MODE), which loads the
;;; extension and then calls the mode's own command; Lem opens the files of
;;; the mode's types with it (it calls the mode found for a file as a
;;; function).  Loading the extension defines the mode's command and file
;;; types again, which replace these.

(defmacro define-extension-triggers ()
  `(progn
     ,@(loop for (system package mode types names) in *extensions*
             for symbol = (intern (format nil "AUTOLOAD-~A" mode) :lem-clysm)
             for command-name = (string-downcase mode)
             collect `(lem:define-command (,symbol (:name ,command-name)) () ()
                        ,(format nil "Load ~A and turn on ~A." system command-name)
                        (load-extension ,system)
                        (funcall (find-symbol ,mode ,package)))
             when types
               collect `(lem-core::associate-file-type ',types ',symbol)
             when names
               collect `(setf (lem-core::file-mode-associations ',symbol)
                              ',(mapcar (lambda (name) (list :file-namestring name)) names)))
     ,@(loop for (system package command key) in *extension-commands*
             for symbol = (intern (format nil "AUTOLOAD-~A" command) :lem-clysm)
             collect `(lem:define-command (,symbol (:name ,(string-downcase command))) () ()
                        ,(format nil "Load ~A and run ~(~A~)." system command)
                        (load-extension ,system)
                        (lem:call-command (find-symbol ,command ,package) nil))
             when key
               collect `(lem:define-key lem:*global-keymap* ,key ',symbol))))

(define-extension-triggers)

;;; The color themes: M-x load-theme offers them once they are loaded, so
;;; they are loaded before that command, and before Lem starts if the
;;; theme saved last time is one of them.

(defun load-themes-before-command ()
  (let ((command (lem:this-command)))
    (when (and command
               (string-equal (lem:command-name command) "load-theme"))
      (load-extension *theme-system*))))

(defun load-saved-theme ()
  (let ((theme (ignore-errors (lem-core::config :color-theme))))
    (when (and theme (not (lem-core::find-color-theme theme)))
      (ignore-errors (load-extension *theme-system* :quiet t)))))

(lem:add-hook lem:*pre-command-hook* 'load-themes-before-command)
