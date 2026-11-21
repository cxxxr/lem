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
Header windows are not among them (see get-buffer-windows)."
  ;; The replacement is chosen once: switch-to-buffer moves the buffer it leaves,
  ;; BUFFER, to the head of the buffer list, so choosing it again for the next
  ;; window would give another buffer.
  (alexandria:when-let ((windows (get-buffer-windows buffer
                                                     :frame frame
                                                     :include-floating-windows t)))
    (let ((replacement (replacement-buffer buffer frame)))
      (dolist (window windows)
        (with-current-window window
          (switch-to-buffer replacement))))))

(defun buffer-header-window (buffer)
  "Return a header window of some frame that shows BUFFER, or NIL."
  (loop :for frame :in (all-frames)
        :thereis (find buffer (frame-header-windows frame) :key #'window-buffer)))

(defmethod delete-buffer-using-manager :before
    ((manager buffer-list-manager)
     buffer)
  ;; A header window (the frame-multiplexer tab bar, the server's tabbar) draws
  ;; into its own buffer, so it cannot be given another buffer: its owner deletes
  ;; the window first.  Refuse before anything is changed.
  ;; The attached buffer too, as it is deleted with BUFFER after the windows changed.
  (dolist (b (list buffer (buffer-attached-buffer buffer)))
    (alexandria:when-let ((window (and b (buffer-header-window b))))
      (error "Cannot delete ~A: the header window ~A shows ~:[its attached buffer ~A~;it~]; delete the window first."
             buffer window (eq b buffer) b)))
  (dolist (frame (all-frames))
    (strip-buffer-from-frame-windows buffer frame))
  ;; Whether or not BUFFER was shown: the attached buffer lives only with it.
  ;; Once, as deleting it a second time fails (it is already freed).
  (alexandria:when-let ((attached (buffer-attached-buffer buffer)))
    (unless (deleted-buffer-p attached)
      (delete-buffer attached)))
  (run-hooks (make-per-buffer-hook :var 'kill-buffer-hook :buffer buffer) buffer))
