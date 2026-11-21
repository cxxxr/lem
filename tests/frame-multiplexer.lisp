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

(deftest killing-a-buffer-shown-in-two-windows-shows-the-same-buffer-in-both
  (with-fresh-editor ()
    (let ((a (make-buffer "a"))
          (k (make-buffer "k"))
          (b (make-buffer "b"))
          (c (make-buffer "c")))
      (switch-to-buffer k)
      (split-window-vertically (current-window))
      (ok (= 2 (length (get-buffer-windows k))))
      ;; The buffer list is (a k b c ...): the buffer before k is a.
      (dolist (buffer (list c b k a))
        (unbury-buffer buffer))
      (kill-buffer k)
      (ok (equal (list a a) (mapcar #'window-buffer (window-list)))))))

(deftest a-buffer-shown-in-a-side-window-of-a-hidden-frame-stays-in-that-frame
  (with-frame-multiplexer ()
    (visit-new-buffer "a")
    (let ((killed (make-buffer "killed")))
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      (let ((b (visit-new-buffer "b")))
        (make-leftside-window killed)
        ;; The buffer before "killed" in this frame's list is "b".
        (unbury-buffer killed)
        (unbury-buffer b)
        (let ((side-window (frame-leftside-window (current-frame))))
          (lem/frame-multiplexer:frame-multiplexer-prev)
          (kill-buffer killed)
          (ok (eq b (window-buffer side-window)))
          (testing "the displayed frame does not get the side window's new buffer"
            (ng (member "b" (offered-buffer-names) :test #'string=))))))))

(deftest a-frame-whose-buffers-are-all-gone-offers-no-other-frames-buffers
  (with-frame-multiplexer ()
    (let ((a (visit-new-buffer "a")))
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      ;; A temporary buffer is not in the buffer list, so once *tmp* is killed
      ;; this frame has no buffer of the list left.
      (let ((temporary (make-buffer "temporary" :temporary t)))
        (switch-to-buffer temporary)
        (kill-buffer (get-buffer "*tmp*"))
        (ok (equal (list a) (buffer-list)))
        (ok (null (frame-buffer-list)))
        (ok (null (completion-buffer "a")))
        (testing "commands that need another buffer still work"
          (ok (progn (next-buffer) (previous-buffer) t))
          (ok (eq temporary (current-buffer)))
          (ok (other-buffer)))))))

(deftest the-workspace-buffer-list-reads-the-frame-table-once
  (with-frame-multiplexer ()
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (loop :for i :below 20
          :do (make-buffer (format nil "background-~D" i)))
    (let* ((count 0)
           (name 'lem/frame-multiplexer::virtual-frame-entries)
           (original (fdefinition name)))
      ;; Count the calls of virtual-frame-entries, which copies the whole table.
      (sb-ext:without-package-locks
        (setf (fdefinition name)
              (lambda (virtual-frame)
                (incf count)
                (funcall original virtual-frame)))
        (unwind-protect (frame-buffer-list)
          (setf (fdefinition name) original)))
      (ok (<= count 1)))))

(deftest a-frame-buffer-list-function-of-one-value-still-restricts
  (with-fresh-editor ()
    (let ((x (make-buffer "x")))
      (make-buffer "y")
      (let ((*frame-buffer-list-function* (lambda (frame)
                                                      (declare (ignore frame))
                                                      (list x))))
        (ok (equal (list x) (frame-buffer-list))))
      (let ((*frame-buffer-list-function* (constantly nil)))
        (ok (equal (buffer-list) (frame-buffer-list))))
      (let ((*frame-buffer-list-function* (lambda (frame)
                                                      (declare (ignore frame))
                                                      (values nil t))))
        (ok (null (frame-buffer-list)))))))
