(defpackage :lem-tests/fake-interface
  (:use :cl :rove :lem)
  (:import-from :lem
                :with-current-buffers)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-tests/fake-interface)

(deftest each-use-starts-with-its-own-frames-and-current-buffer
  ;; Start from an empty buffer list, so that the *tmp* of the frames made
  ;; here is not the shared one and does not stay in the global buffer list.
  (with-current-buffers ()
    (let ((outer-frames (all-frames)))
      (with-fake-interface ()
        (make-frame)
        (setf (current-buffer) (make-buffer "leftover" :temporary t)))
      (with-fake-interface ()
        (testing "the frames of the earlier use are gone"
          (ok (= 1 (length (all-frames)))))
        (testing "the current buffer is the buffer of the new frame's window"
          (ok (eq (window-buffer (current-window)) (current-buffer)))))
      (testing "the frames outside are left as they were"
        (ok (equal outer-frames (all-frames)))))))

(deftest each-use-starts-with-its-own-last-focused-window
  (with-current-buffers ()
    (let ((outer (last-focused-window)))
      (with-fake-interface ()
        (ok (null (last-focused-window)))
        (let ((first-window (current-window)))
          (split-window-vertically first-window)
          (switch-to-window (find first-window (window-list) :test-not #'eq))
          (ok (eq first-window (last-focused-window)))))
      (testing "the last focused window outside is left as it was"
        (ok (eq outer (last-focused-window))))
      (with-fake-interface ()
        (testing "the next use does not see the window of the earlier use"
          (ok (null (last-focused-window))))))))

(defun buffer-has-point-p (buffer point)
  ;; lem exports no way to list the points registered in a buffer.
  (and (member point (lem/buffer/internal::buffer-points buffer)) t))

(deftest the-windows-of-a-use-leave-no-point-in-the-buffers
  (with-current-buffers ()
    (let ((shared (make-buffer "shared"))
          (points '()))
      (with-fake-interface ()
        (switch-to-buffer shared)
        (split-window-vertically (current-window))
        (dolist (window (window-list))
          (ok (eq shared (window-buffer window)))
          (push (window-view-point window) points)
          ;; The window point of the current window is the buffer point.
          (unless (eq window (current-window))
            (push (window-point window) points)))
        (ok (every (lambda (point) (buffer-has-point-p shared point)) points)))
      (testing "the points of the windows are deleted from the buffer"
        (ok (= 3 (length points)))
        (ok (notany (lambda (point) (buffer-has-point-p shared point)) points))))))

(deftest unmap-frame-removes-the-frame-of-the-display
  (with-current-buffers ()
    (with-fake-interface ()
      (let ((frame (current-frame)))
        (ok (eq frame (get-frame (implementation))))
        (ok (eq frame (unmap-frame (implementation))))
        (ok (null (get-frame (implementation))))))))
