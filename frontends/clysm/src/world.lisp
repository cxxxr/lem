;;;; world.lisp — worlds in Lem: the Lisp mode connected to another image
;;;;
;;;; Lem's Lisp mode evaluates in Lem's own image by default (its
;;;; self-connection): code that loops for ever there stops the editor, and
;;;; code that breaks the image breaks the editor.  A world
;;;; (contrib/world/) is an image of its own in a worker, with a micros
;;;; server.  M-x world-start makes one and connects the Lisp mode to it:
;;;; the REPL, C-M-x, completion, M-., the debugger and the inspector then
;;;; work there, while the editor runs in its own thread.  M-x world-kill
;;;; ends the world of the current connection, whatever it does, and the
;;;; Lisp mode is back in the editor's image (or in another world).

(in-package :lem-clysm)

;;; (loaded with the Lisp mode, after boot.lisp said where the boot file of
;;; a world is)
(when (boundp 'cl-user::*world-boot-file*)
  (setf clysm-world:*boot-file* (symbol-value 'cl-user::*world-boot-file*)))

(defun world-of-connection (connection)
  (and connection
       (clysm-world:find-world (lem-lisp-mode/connection:connection-hostname connection))))

(defun current-world ()
  "The world that the Lisp mode is connected to, or NIL (the editor's image)."
  (world-of-connection (lem-lisp-mode/connection:current-connection)))

(defun world-ready (world)
  (lem-lisp-mode/internal:connect-to-micros (clysm-world:world-name world)
                                            (clysm-world:world-port world))
  (lem-lisp-mode/internal:start-lisp-repl)
  (lem:message "~A started in ~,1F s" (clysm-world:world-name world)
               (clysm-world:world-seconds world)))

(lem:define-command world-start () ()
  "Start a world, an image of its own in a worker, and connect the Lisp mode to it."
  (unless clysm-world:*boot-file*
    ;; (a page in a browser: its files are in the page's memory, which a
    ;; worker does not have)
    (lem:editor-error "Worlds are in the desktop application only"))
  (let ((world (clysm-world:start-world
                ;; (called from a listener of the page: the editor does the rest)
                :on-ready (lambda (world)
                            (lem:send-event (lambda () (world-ready world))))
                :on-failure (lambda (world)
                              (lem:send-event
                               (lambda ()
                                 (lem:message "~A failed: ~A" (clysm-world:world-name world)
                                              (clysm-world:world-failure world))))))))
    (lem:message "Starting ~A…" (clysm-world:world-name world))))

(lem:define-command world-kill () ()
  "End the world that the Lisp mode is connected to, whatever it is doing."
  (let ((world (current-world)))
    (unless world
      (lem:editor-error "The Lisp mode is not connected to a world"))
    (let ((connections (remove world (lem-lisp-mode/connections:connection-list)
                               :key #'world-of-connection :test-not #'eq)))
      ;; the Lisp mode is never without a connection: if these are its
      ;; only ones, it goes back to the editor's image
      (when (null (set-difference (lem-lisp-mode/connections:connection-list) connections))
        (lem-lisp-mode/internal:self-connect))
      (clysm-world:kill-world world)
      (mapc #'lem-lisp-mode/internal::remove-and-change-connection connections))
    (lem:message "~A killed" (clysm-world:world-name world))))

;;; The lines that a world writes to its standard output outside the REPL
;;; (its threads, its start) go to the buffer *worlds*.
(setf clysm-world:*output-function*
      (lambda (world text)
        (lem:send-event
         (lambda ()
           (let* ((buffer (lem:make-buffer "*worlds*" :enable-undo-p nil))
                  (point (lem:buffer-end-point buffer)))
             (lem:insert-string point (format nil "[~A] ~A~%" (clysm-world:world-name world) text)))))))
