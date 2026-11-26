(defpackage :lem-tests/frame-multiplexer
  (:use :cl :lem :rove)
  (:import-from :lem
                :with-current-buffers)
  (:import-from :lem/common/var
                :with-global-variable-value)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-tests/frame-multiplexer)

(defun call-with-frame-multiplexer (function)
  ;; Run frame-multiplexer on its own table of virtual frames and with the
  ;; variable off at the start, so that running these tests inside a Lem that
  ;; uses frame-multiplexer leaves its tabs and its setting as they were.
  ;; with-global-variable-value restores the previous value without running
  ;; the variable's hook.
  (with-current-buffers ()
    (with-fake-interface ()
      (let ((lem/frame-multiplexer::*virtual-frame-map* (make-hash-table))
            (lem/frame-multiplexer::*recent-list* nil))
        (with-global-variable-value (lem/frame-multiplexer::frame-multiplexer nil)
          (lem/frame-multiplexer::enable-frame-multiplexer)
          (unwind-protect (funcall function)
            (lem/frame-multiplexer::disable-frame-multiplexer)))))))

(defmacro with-frame-multiplexer (() &body body)
  `(call-with-frame-multiplexer (lambda () ,@body)))

(deftest turning-it-off-frees-the-other-tabs
  (with-frame-multiplexer ()
    (let ((first (current-frame)))
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      (lem/frame-multiplexer:frame-multiplexer-prev)
      (let ((shown (current-frame))
            (frames (length (all-frames))))
        (ok (not (eq first shown)))
        (lem/frame-multiplexer::disable-frame-multiplexer)
        (ok (= (- frames 2) (length (all-frames)))
            "the tabs that cannot be reached again are torn down")
        (ok (member shown (all-frames)) "the frame shown stays")
        (ok (eq shown (current-frame)))))))

(deftest turning-it-off-completes-when-a-tab-cannot-be-freed
  (with-frame-multiplexer ()
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (let ((hidden (current-frame)))
      (lem/frame-multiplexer:frame-multiplexer-prev)
      (let ((shown (current-frame)))
        ;; freeing the hidden tab's windows signals
        (setf (frame-window-tree hidden) :not-a-window)
        (lem/frame-multiplexer::disable-frame-multiplexer)
        (ng (variable-value 'lem/frame-multiplexer::frame-multiplexer :global)
            "frame-multiplexer is off")
        (ng (member hidden (all-frames)) "the hidden tab is forgotten")
        (ok (eq shown (current-frame)))
        (ng (some (lambda (window)
                    (typep window 'lem/frame-multiplexer::virtual-frame))
                  (frame-header-windows shown))
            "the tab bar is gone")))))

(deftest turning-it-off-completes-when-the-tab-bar-cannot-be-deleted
  (with-frame-multiplexer ()
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (let ((hidden (current-frame))
          (tab-bar (find-if (lambda (window)
                              (typep window 'lem/frame-multiplexer::virtual-frame))
                            (frame-header-windows (current-frame)))))
      (ok tab-bar)
      (push (lambda () (error "the tab bar cannot be deleted"))
            (window-delete-hook tab-bar))
      (lem/frame-multiplexer:frame-multiplexer-prev)
      (lem/frame-multiplexer::disable-frame-multiplexer)
      (ng (variable-value 'lem/frame-multiplexer::frame-multiplexer :global)
          "frame-multiplexer is off")
      (ng (member hidden (all-frames)) "the hidden tab is freed")
      (ok (zerop (hash-table-count lem/frame-multiplexer::*virtual-frame-map*))
          "the table of tabs is reset"))))
