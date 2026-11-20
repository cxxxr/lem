(defpackage :lem/variable-pitch
  (:use :cl :lem)
  (:export :variable-pitch-mode))
(in-package :lem/variable-pitch)

(define-minor-mode variable-pitch-mode
    (:name "Variable-Pitch"
     :description "Draw the buffer's text in a proportional font, on frontends
that support it (see lem:variable-pitch)."
     :enable-hook 'enable-variable-pitch
     :disable-hook 'disable-variable-pitch))

(defun set-variable-pitch (value)
  "Set variable-pitch for the current buffer and redraw everything: lines
already drawn would otherwise keep their old font until they change."
  (setf (variable-value 'variable-pitch :buffer (current-buffer)) value)
  (redraw-display :force t))

(defun enable-variable-pitch ()
  "Lay the current buffer's text out proportionally."
  (set-variable-pitch t))

(defun disable-variable-pitch ()
  "Lay the current buffer's text out in monospace cells again."
  (set-variable-pitch nil))
