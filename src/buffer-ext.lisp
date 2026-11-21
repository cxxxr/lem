(in-package :lem-core)

(define-editor-variable kill-buffer-hook '())

(defun replacement-buffer (buffer frame)
  "Return the buffer to show in FRAME's windows in place of BUFFER, which is being killed.
It is the buffer before BUFFER among the ones FRAME offers, so that a workspace
does not suddenly show a buffer of another workspace.
When FRAME offers no other buffer, it is the primordial buffer (*tmp*),
created again if it was killed, rather than a buffer of another workspace.
When BUFFER is the primordial buffer itself, it is a new buffer named after it
(*tmp*<1>): the name *tmp* is still BUFFER's until it is deleted."
  (let ((buffers (frame-buffer-list frame)))
    (or (loop :for prev := nil :then curr
              :for curr :in buffers
              :when (eq curr buffer)
                :return prev)
        (find-if (lambda (other) (not (eq other buffer)))
                 buffers
                 :from-end t)
        (let ((primordial (primordial-buffer)))
          (if (eq primordial buffer)
              (make-buffer (unique-buffer-name (buffer-name buffer)))
              primordial)))))

(defun strip-buffer-from-frame-windows (buffer frame)
  "Show another buffer in every window of FRAME that shows BUFFER.
Return true if there was such a window."
  ;; The replacement is chosen once: switch-to-buffer moves the buffer it leaves,
  ;; BUFFER, to the head of the buffer list, so choosing it again for the next
  ;; window would give another buffer.
  (alexandria:when-let ((windows (get-buffer-windows buffer
                                                     :frame frame
                                                     :include-floating-windows t)))
    (let ((replacement (replacement-buffer buffer frame)))
      (dolist (window windows)
        (with-current-window window
          (switch-to-buffer replacement))))
    t))

(defmethod delete-buffer-using-manager :before
    ((manager buffer-list-manager)
     buffer)
  (let ((shown (loop :for frame :in (all-frames)
                     :count (strip-buffer-from-frame-windows buffer frame))))
    ;; Once, however many windows showed BUFFER: deleting the attached buffer
    ;; a second time fails, as it is already freed.
    (alexandria:when-let ((attached (buffer-attached-buffer buffer)))
      (when (and (plusp shown)
                 (not (deleted-buffer-p attached)))
        (delete-buffer attached))))
  (run-hooks (make-per-buffer-hook :var 'kill-buffer-hook :buffer buffer) buffer))
