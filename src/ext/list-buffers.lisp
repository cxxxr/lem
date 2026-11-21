(defpackage :lem/list-buffers
  (:use :cl
        :lem)
  (:import-from :lem/multi-column-list
                :multi-column-list
                :multi-column-list-of-window
                :display
                :quit
                :update
                :collect-checked-items
                :delete-checked-items)
  (:export :list-buffers))
(in-package :lem/list-buffers)

(define-key *global-keymap* "C-x C-b" 'list-buffers)

(defun kill-buffers (window)
  (let ((multi-column-list (multi-column-list-of-window window)))
    (delete-checked-items multi-column-list)))

(defun save-buffers (window)
  "Save all buffers from the current multi-column-list window."
  (let ((multi-column-list (multi-column-list-of-window window)))
    (mapc #'save-buffer (collect-checked-items multi-column-list))
    (update multi-column-list)))

(defun buffer-attributes (buffer)
  (cond ((buffer-read-only-p buffer)
         (icon-string "lock"))
        ((buffer-modified-p buffer)
         (icon-string "bullet-point"))
        (t
         " ")))

(define-command list-buffers () ()
  (display
   (make-instance 'multi-column-list
                  :columns '("" "Buffer" "File")
                  :column-function (lambda (component buffer)
                                     (declare (ignore component))
                                     (list (buffer-attributes buffer)
                                           (buffer-name buffer)
                                           (or (buffer-filename buffer) "")))
                  :items (frame-buffer-list)
                  :filter-function #'completion-buffer
                  :select-callback (lambda (component buffer)
                                     (quit component)
                                     (switch-to-buffer buffer))
                  :delete-callback (lambda (component buffer)
                                     (declare (ignore component))
                                     (kill-buffer buffer))
                  :save-callback (lambda (component buffer)
                                   (declare (ignore component))
                                   (save-buffer buffer))
                  :use-check t
                  :context-menu (make-instance
                                 'lem/context-menu:context-menu
                                 :items (list (make-instance 'lem/context-menu:item
                                                             :label "Kill selected buffers"
                                                             :callback #'kill-buffers)
                                              (make-instance 'lem/context-menu:item
                                                             :label "Save selected buffers"
                                                             :callback #'save-buffers))))))
