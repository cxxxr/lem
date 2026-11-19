(defpackage :lem-vi-mode/lisp-mode-binds
  (:use :cl)
  (:import-from :lem-vi-mode/core
                :mode-specific-keymaps)
  (:import-from :lem-lisp-mode/sldb
                :sldb-mode
                :*sldb-keymap*)
  (:import-from :lem-lisp-mode/inspector
                :lisp-inspector-mode
                :*lisp-inspector-keymap*))
(in-package :lem-vi-mode/lisp-mode-binds)

(defmethod mode-specific-keymaps ((mode sldb-mode))
  (list *sldb-keymap*))

(defmethod mode-specific-keymaps ((mode lisp-inspector-mode))
  (list *lisp-inspector-keymap*))
