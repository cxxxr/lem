;;;; Lem's terminal (M-x terminal) on Clysm, in the desktop application: the
;;;; pseudo-terminal and the screen are node-pty and @xterm/headless in the
;;;; page (src/terminal.lisp) instead of libvterm with CFFI, and the commands
;;;; and modes are Lem's own (extensions/terminal/terminal-mode.lisp).  An
;;;; extension of lem-clysm, loaded by the first M-x terminal
;;;; (src/extensions.lisp).
(defsystem "lem-clysm-terminal"
  :description "Lem's terminal with node-pty and xterm.js"
  :licence "MIT"
  :depends-on ("lem-clysm")
  :serial t
  :components ((:file "src/terminal")
               (:file "terminal-mode" :pathname "../../extensions/terminal/terminal-mode")))
