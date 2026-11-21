(defpackage :lem-tests/fake-interface
  (:use :cl :rove :lem)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-tests/fake-interface)

(deftest each-use-starts-with-its-own-frames-and-current-buffer
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
      (ok (equal outer-frames (all-frames))))))
