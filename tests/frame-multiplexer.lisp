(defpackage :lem-tests/frame-multiplexer
  (:use :cl :lem :rove)
  (:import-from :lem
                :with-current-buffers)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-tests/frame-multiplexer)

(defun call-with-fresh-editor (function)
  ;; Frames made by earlier tests stay in lem-core::*frames*, and delete-buffer
  ;; switches the windows of every frame, so give each test its own frames.
  ;; The current buffer is also left from earlier tests; switch-to-buffer would
  ;; put it back into this test's buffer list.
  (let ((lem-core::*frames* '()))
    (with-current-buffers ()
      (with-fake-interface ()
        (setf (current-buffer) (window-buffer (current-window)))
        (funcall function)))))

(defmacro with-fresh-editor (() &body body)
  `(call-with-fresh-editor (lambda () ,@body)))

(defun call-with-frame-multiplexer (function)
  (with-fresh-editor ()
    (lem/frame-multiplexer::enable-frame-multiplexer)
    (unwind-protect (funcall function)
      (lem/frame-multiplexer::disable-frame-multiplexer))))

(defmacro with-frame-multiplexer (() &body body)
  `(call-with-frame-multiplexer (lambda () ,@body)))

(defun offered-buffer-names ()
  (mapcar #'buffer-name (frame-buffer-list)))

(defun visit-new-buffer (name)
  (let ((buffer (make-buffer name)))
    (switch-to-buffer buffer)
    buffer))

(deftest each-frame-offers-its-own-buffers
  (with-frame-multiplexer ()
    (visit-new-buffer "a")
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (visit-new-buffer "b")
    (testing "the new frame does not offer the buffers of the first frame"
      (ok (member "b" (offered-buffer-names) :test #'string=))
      (ng (member "a" (offered-buffer-names) :test #'string=)))
    (testing "buffer completion is narrowed too"
      (ok (equal '("b")
                 (mapcar #'buffer-name (completion-buffer "b"))))
      (ok (null (completion-buffer "a"))))
    (lem/frame-multiplexer:frame-multiplexer-prev)
    (testing "the first frame does not offer the buffers of the new frame"
      (ok (member "a" (offered-buffer-names) :test #'string=))
      (ng (member "b" (offered-buffer-names) :test #'string=)))
    (testing "next-buffer and previous-buffer stay in the frame"
      (loop :repeat 4
            :do (next-buffer)
                (ng (string= "b" (buffer-name (current-buffer)))))
      (loop :repeat 4
            :do (previous-buffer)
                (ng (string= "b" (buffer-name (current-buffer))))))))

(deftest a-buffer-no-frame-has-shown-is-offered-everywhere
  (with-frame-multiplexer ()
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (make-buffer "background")
    (ok (member "background" (offered-buffer-names) :test #'string=))
    (lem/frame-multiplexer:frame-multiplexer-prev)
    (ok (member "background" (offered-buffer-names) :test #'string=))))

(deftest a-buffer-shown-in-two-frames-is-shared
  (with-frame-multiplexer ()
    (let ((shared (visit-new-buffer "shared")))
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      (switch-to-buffer shared)
      (ok (member "shared" (offered-buffer-names) :test #'string=))
      (lem/frame-multiplexer:frame-multiplexer-prev)
      (ok (member "shared" (offered-buffer-names) :test #'string=)))))

(deftest killing-a-buffer-shows-a-buffer-of-the-same-frame
  (with-frame-multiplexer ()
    (visit-new-buffer "a")
    (let ((a2 (visit-new-buffer "a2")))
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      (visit-new-buffer "b")
      (lem/frame-multiplexer:frame-multiplexer-prev)
      (kill-buffer a2)
      (ng (string= "b" (buffer-name (current-buffer))))
      (ng (member a2 (frame-buffer-list))))))

(deftest separation-can-be-turned-off
  (with-frame-multiplexer ()
    (visit-new-buffer "a")
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (setf (variable-value 'lem/frame-multiplexer:frame-multiplexer-separate-buffers :global) nil)
    (unwind-protect
         (ok (member "a" (offered-buffer-names) :test #'string=))
      (setf (variable-value 'lem/frame-multiplexer:frame-multiplexer-separate-buffers :global)
            t))))

(deftest quitting-a-window-shows-a-buffer-of-the-same-frame
  (with-frame-multiplexer ()
    (visit-new-buffer "a")
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (visit-new-buffer "b")
    (visit-new-buffer "help")
    (lem/frame-multiplexer:frame-multiplexer-prev)
    ;; Put "a" at the head of the whole buffer list.
    (visit-new-buffer "a-other")
    (lem/frame-multiplexer:frame-multiplexer-next)
    (ok (eq (get-buffer "a") (first (buffer-list))))
    (quit-window (current-window))
    (ok (string= "b" (buffer-name (current-buffer))))))

(deftest killing-the-last-buffer-of-a-frame-shows-tmp
  (with-frame-multiplexer ()
    (visit-new-buffer "a")
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (let ((b (visit-new-buffer "b")))
      (kill-buffer (get-buffer "*tmp*"))
      (ok (equal '("b") (offered-buffer-names)))
      (kill-buffer b)
      (ok (string= "*tmp*" (buffer-name (current-buffer)))))))

(deftest deleting-the-only-buffer-does-not-fail
  (with-fresh-editor ()
    (let ((x (visit-new-buffer "x")))
      (delete-buffer (get-buffer "*tmp*"))
      (ok (equal (list x) (buffer-list)))
      (ok (progn (delete-buffer x) t))
      (ok (string= "*tmp*" (buffer-name (current-buffer)))))))
