(defpackage :lem-tests/frame-multiplexer
  (:use :cl :lem :rove)
  (:import-from :lem
                :with-current-buffers)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-tests/frame-multiplexer)

(defun call-with-frame-multiplexer (function)
  (with-current-buffers ()
    (with-fake-interface ()
      (lem/frame-multiplexer::enable-frame-multiplexer)
      (unwind-protect (funcall function)
        (lem/frame-multiplexer::disable-frame-multiplexer)))))

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
