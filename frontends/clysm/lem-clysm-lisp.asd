;;;; Lem's Lisp mode on Clysm: Lem's own lem-lisp-mode, talking to micros in
;;;; Lem's image or in a world (contrib/world/, src/world.lisp).  An
;;;; extension of lem-clysm, loaded the first time a Lisp file is opened or
;;;; the REPL or a world started (src/extensions.lisp): loading it takes
;;;; most of a second.
(defsystem "lem-clysm-lisp"
  :description "Lem's Lisp mode, with micros in Lem's image and in worlds"
  :licence "MIT"
  :depends-on ("lem-clysm" "lem-lisp-mode" "clysm-world")
  :pathname "src/"
  :components ((:file "world")))
