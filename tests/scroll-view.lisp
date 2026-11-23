(defpackage :lem-tests/scroll-view
  (:use :cl :rove)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-tests/scroll-view)

(defun view-line (window)
  (lem:line-number-at-point (lem:window-view-point window)))

(defun cursor-line (window)
  (declare (ignore window))
  (lem:line-number-at-point (lem:current-point)))

(defun call-with-long-buffer (function)
  "Call FUNCTION with the current window showing 200 lines, from the top."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let ((buffer (lem:make-buffer "scroll-view" :temporary t)))
        (lem:switch-to-buffer buffer)
        (dotimes (i 200)
          (lem:insert-string (lem:buffer-point buffer) (format nil "line ~D~%" i)))
        (lem:buffer-start (lem:buffer-point buffer))
        (lem:redraw-display :force t)
        (funcall function (lem:current-window))))))

(deftest scroll-view-leaves-the-cursor
  (call-with-long-buffer
   (lambda (window)
     (lem:scroll-view window 50)
     (lem:redraw-display)
     (ok (= 51 (view-line window)) "the view moved")
     (ok (= 1 (cursor-line window)) "the cursor did not, though it is off screen")
     (ok (lem:view-detached-p window))
     (lem:redraw-display)
     (ok (= 51 (view-line window)) "and the redisplay leaves the view there"))))

(deftest moving-the-cursor-brings-the-view-back
  (call-with-long-buffer
   (lambda (window)
     (lem:scroll-view window 50)
     (lem:redraw-display)
     (lem:line-offset (lem:current-point) 1)
     (lem:redraw-display)
     (ng (lem:view-detached-p window))
     (ok (<= (view-line window) (cursor-line window)) "the cursor is on screen again"))))

(deftest editing-brings-the-view-back
  (call-with-long-buffer
   (lambda (window)
     (lem:scroll-view window 50)
     (lem:redraw-display)
     (lem:insert-string (lem:current-point) "x")
     (lem:redraw-display)
     (ng (lem:view-detached-p window))
     (ok (= 1 (view-line window)) "the view is back at the cursor"))))

(deftest scroll-view-stops-at-the-start
  (call-with-long-buffer
   (lambda (window)
     (ng (lem:scroll-view window -5) "it could not scroll up from the top")
     (ok (= 1 (view-line window))))))
