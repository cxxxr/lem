;;;; Welcome to Lem, the Common Lisp editor, running in your browser.
;;;;
;;;; Everything here is WebAssembly made by Clysm, a Common Lisp
;;;; implementation (https://github.com/cxxxr/clysm): Lem, its libraries,
;;;; and the Lisp compiler itself, which is in the page with the editor.
;;;; Nothing runs on a server.
;;;;
;;;; This file is a short tour.  The keys are Emacs's (C- is Control,
;;;; M- is Alt or Option):
;;;;
;;;;   C-x C-f  open a file          C-x C-s  save         C-x b  switch buffer
;;;;   C-x 2    split the window     C-x o    other window C-x 1  one window
;;;;   C-s      search               C-/      undo         C-g    cancel
;;;;   M-x      run a command by name (TAB completes)

;;; ---------------------------------------------------------------------
;;; 1. Evaluate Lisp
;;;
;;; Put the cursor after a form and press C-x C-e, or anywhere in a
;;; definition and press C-c C-c.  Each form is compiled to a new
;;; WebAssembly module, linked with the running image, and run.
;;;
;;; This is Lem's own Lisp mode, talking to micros (Lem's SWANK) in the
;;; same image: TAB completes a symbol, the arguments of the function you
;;; are calling are shown as you type, M-. goes to a definition (try it
;;; on REDUCE or GETHASH to read Clysm's implementation, or on LEM:MESSAGE
;;; for Lem's) and M-, comes back, C-c C-d d describes a symbol, C-c I
;;; inspects an object, and an error opens the debugger (q leaves it).

(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(time (fib 27))

(car 5)                                 ; an error: the debugger

(loop for i from 1 to 10 collect (expt 2 i))

(expt 2 200)

;;; ---------------------------------------------------------------------
;;; 2. Change the editor while it runs
;;;
;;; The editor is in the image your forms run in.  Evaluate these, then
;;; press C-c h, or M-x hello:

(lem:define-command hello () ()
  "Say hello from the image the editor runs in."
  (lem:message "Hello from ~A ~A, in ~A"
               (lisp-implementation-type) (lisp-implementation-version)
               (software-type)))

(lem:define-key lem:*global-keymap* "C-c h" 'hello)

;;; A command that works on the buffer: it counts the top-level forms.

(lem:define-command count-forms () ()
  (let ((n 0))
    (lem:with-point ((p (lem:buffer-start-point (lem:current-buffer))))
      (loop while (lem:form-offset p 1) do (incf n)))
    (lem:message "~D top-level forms" n)))

;;; C-c C-z (M-x start-lisp-repl) opens a REPL of the same image, and M-:
;;; evaluates a form read from the prompt.  What you evaluate runs in a
;;; thread of its own (a green thread of Clysm), so the editor goes on
;;; meanwhile: evaluate this endless loop in the REPL, switch windows,
;;; type, then press C-c C-c in the REPL to interrupt it.

(loop for i from 0 sum i)

;;; ---------------------------------------------------------------------
;;; 3. More of Lem
;;;
;;;   M-x load-theme     one of about 190 color themes: try dracula, nord,
;;;                      solarized-light, gruvbox-dark-medium (it is kept
;;;                      for the next visit)
;;;   M-x vi-mode        Vim's keys (M-x emacs-mode to go back)
;;;   C-x C-f hello.py   other languages: Python, JavaScript, TypeScript,
;;;                      C, Go, Rust, JSON, YAML, TOML, Markdown, HTML,
;;;                      CSS, SQL, shell scripts.  A mode is loaded the
;;;                      first time a file needs it.
;;;   C-x d              a tree of the files
;;;   M-x paredit-mode   structural editing of Lisp

;;; ---------------------------------------------------------------------
;;; 4. Files
;;;
;;; The files you save in the home directory are kept in your browser.
;;; M-x open-local-file opens a file of your computer, M-x download-file
;;; saves the buffer to your computer.
;;;
;;; What a page cannot do: run programs (shells, grep, git, language
;;; servers, other Lisps) and reach the network.
