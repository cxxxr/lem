(in-package :lem-core)

(define-editor-variable kill-buffer-hook '())

(defun replacement-buffer (buffer frame)
  "Return the buffer to show in FRAME's windows in place of BUFFER, which is being killed.
It is the buffer before BUFFER among the ones FRAME offers, so that a workspace
does not suddenly show a buffer of another workspace.
When FRAME offers no other buffer, it is the primordial buffer (*tmp*),
created again if it was killed, rather than a buffer of another workspace."
  (let ((buffers (frame-buffer-list frame)))
    (or (loop :for prev := nil :then curr
              :for curr :in buffers
              :when (eq curr buffer)
                :return prev)
        (find-if (lambda (other) (not (eq other buffer)))
                 buffers
                 :from-end t)
        (primordial-buffer))))

(defun strip-buffer-from-frame-windows (buffer frame)
  ;; The replacement is chosen once: switch-to-buffer moves the buffer it leaves,
  ;; BUFFER, to the head of the buffer list, so choosing it again for the next
  ;; window would give another buffer.
  (alexandria:when-let ((windows (get-buffer-windows buffer
                                                     :frame frame
                                                     :include-floating-windows t)))
    (let ((replacement (replacement-buffer buffer frame)))
      (dolist (window windows)
        (with-current-window window
          (switch-to-buffer replacement))
        (when (buffer-attached-buffer buffer)
          (delete-buffer (buffer-attached-buffer buffer)))))))

(defmethod delete-buffer-using-manager :before
    ((manager buffer-list-manager)
     buffer)
  (dolist (frame (all-frames))
    (strip-buffer-from-frame-windows buffer frame))
  (run-hooks (make-per-buffer-hook :var 'kill-buffer-hook :buffer buffer) buffer))
