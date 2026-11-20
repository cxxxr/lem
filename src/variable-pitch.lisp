(in-package :lem-core)

(define-minor-mode variable-pitch-mode
    (:name "Variable-Pitch"
     :description "Draw the buffer's text in a proportional font, on frontends
that support it (see variable-pitch)."
     :enable-hook 'enable-variable-pitch
     :disable-hook 'disable-variable-pitch))

(defun set-variable-pitch (value)
  "Set variable-pitch for the current buffer and redraw everything: lines
already drawn would otherwise keep their old font until they change."
  (setf (variable-value 'variable-pitch :buffer (current-buffer)) value)
  (redraw-display :force t))

(defun enable-variable-pitch ()
  (set-variable-pitch t))

(defun disable-variable-pitch ()
  (set-variable-pitch nil))
