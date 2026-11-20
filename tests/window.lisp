(defpackage :lem-tests/window
  (:use :cl :rove)
  (:import-from :lem-fake-interface
                :fake-interface
                :with-fake-interface))
(in-package :lem-tests/window)

(deftest delete-window-test
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (assert (lem:one-window-p))
      (ok (equal "Can not delete this window"
                 (handler-case (lem:delete-window (lem:current-window))
                   (error (e) (princ-to-string e)))))
      (ok (not (lem-core::window-deleted-p (lem:current-window)))))))

(defun pop-attached-window (buffer)
  (let ((window (lem:pop-to-buffer buffer)))
    (lem:switch-to-window window)))

(defun make-attached-buffer ()
  (let ((buffer (lem:make-buffer "test" :enable-undo-p nil))
        (attached-buffer (lem:make-buffer (format nil "~A (attached)" "test")
                                          :temporary t)))
    (lem:attach-buffer buffer attached-buffer)
    buffer))

(deftest attached-window/delete-other-windows
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (pop-attached-window (make-attached-buffer))

      (assert (= 2 (length (lem:window-list))))
      (assert (lem:attached-window-p (lem:current-window)))

      (lem:delete-other-windows)

      (ok (= 1 (length (lem:window-list))))
      (ok (lem:attached-window-p (lem:current-window))))))

(deftest attached-window/switch-to-buffer/change-attached-to-normal
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (pop-attached-window (make-attached-buffer))
      (assert (= 2 (length (lem:window-list))))
      (assert (lem:attached-window-p (lem:current-window)))
      (let* ((attached-window (lem:current-window))
             (attached-buffer (lem:window-buffer attached-window))
             (parent-window (lem-core::attached-window-parent-window attached-window))
             (new-buffer (lem:make-buffer "foo")))
        (assert (lem-core::attached-buffer-p attached-buffer))
        (assert parent-window)

        (lem:switch-to-buffer new-buffer)

        (ok (null (lem-core::attached-window-parent-window attached-window)))
        (ok (lem:deleted-window-p attached-window))
        (ok (eq attached-buffer (lem:window-buffer attached-window)))
        (ok (null (lem-core::window-attached-window parent-window)))
        (ok (eq new-buffer (lem:window-buffer parent-window)))))))

(deftest attached-window/switch-to-buffer/change-normal-to-attached
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let ((buffer (make-attached-buffer)))
        (assert (= 1 (length (lem:window-list))))

        (lem:switch-to-buffer buffer)

        (ok (= 1 (length (lem:window-list))))
        (ok (lem:attached-window-p (lem:current-window)))
        (ok (eq (lem-core::buffer-attached-buffer buffer)
                (lem:window-buffer (lem:current-window))))))))

;;; Floating Window Pixel Coordinates Tests

(deftest floating-window/make-with-pixel-coordinates
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (lem:make-buffer "test-floating" :temporary t))
             (window (lem:make-floating-window :buffer buffer
                                               :x 5 :y 3
                                               :width 20 :height 10
                                               :pixel-x 100 :pixel-y 60
                                               :pixel-width 400 :pixel-height 200)))
        ;; Check character coordinates
        (ok (= 5 (lem:window-x window)))
        (ok (= 3 (lem:window-y window)))
        (ok (= 20 (lem:window-width window)))
        (ok (= 10 (lem:window-height window)))
        ;; Check pixel coordinates
        (ok (= 100 (lem:floating-window-pixel-x window)))
        (ok (= 60 (lem:floating-window-pixel-y window)))
        (ok (= 400 (lem:floating-window-pixel-width window)))
        (ok (= 200 (lem:floating-window-pixel-height window)))
        ;; Cleanup
        (lem:delete-window window)))))

(deftest floating-window/make-without-pixel-coordinates
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (lem:make-buffer "test-floating" :temporary t))
             (window (lem:make-floating-window :buffer buffer
                                               :x 5 :y 3
                                               :width 20 :height 10)))
        ;; Check character coordinates
        (ok (= 5 (lem:window-x window)))
        (ok (= 3 (lem:window-y window)))
        ;; Pixel coordinates should be nil
        (ok (null (lem:floating-window-pixel-x window)))
        (ok (null (lem:floating-window-pixel-y window)))
        (ok (null (lem:floating-window-pixel-width window)))
        (ok (null (lem:floating-window-pixel-height window)))
        ;; Cleanup
        (lem:delete-window window)))))

(deftest floating-window/set-pixel-position
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (lem:make-buffer "test-floating" :temporary t))
             (window (lem:make-floating-window :buffer buffer
                                               :x 5 :y 3
                                               :width 20 :height 10)))
        ;; Initially nil
        (ok (null (lem:floating-window-pixel-x window)))
        (ok (null (lem:floating-window-pixel-y window)))
        ;; Set pixel position
        (lem:floating-window-set-pixel-position window 150 80)
        ;; Check updated values
        (ok (= 150 (lem:floating-window-pixel-x window)))
        (ok (= 80 (lem:floating-window-pixel-y window)))
        ;; Character coordinates should be unchanged
        (ok (= 5 (lem:window-x window)))
        (ok (= 3 (lem:window-y window)))
        ;; Cleanup
        (lem:delete-window window)))))

(deftest floating-window/set-pixel-size
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (lem:make-buffer "test-floating" :temporary t))
             (window (lem:make-floating-window :buffer buffer
                                               :x 5 :y 3
                                               :width 20 :height 10)))
        ;; Initially nil
        (ok (null (lem:floating-window-pixel-width window)))
        (ok (null (lem:floating-window-pixel-height window)))
        ;; Set pixel size
        (lem:floating-window-set-pixel-size window 500 300)
        ;; Check updated values
        (ok (= 500 (lem:floating-window-pixel-width window)))
        (ok (= 300 (lem:floating-window-pixel-height window)))
        ;; Character dimensions should be unchanged
        (ok (= 20 (lem:window-width window)))
        (ok (= 10 (lem:window-height window)))
        ;; Cleanup
        (lem:delete-window window)))))

(deftest floating-window/pixel-bounds-with-explicit-pixels
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (lem:make-buffer "test-floating" :temporary t))
             (window (lem:make-floating-window :buffer buffer
                                               :x 5 :y 3
                                               :width 20 :height 10
                                               :pixel-x 100 :pixel-y 60
                                               :pixel-width 400 :pixel-height 200)))
        (multiple-value-bind (px py pw ph)
            (lem:floating-window-pixel-bounds window)
          ;; Should return explicit pixel values
          (ok (= 100 px))
          (ok (= 60 py))
          (ok (= 400 pw))
          (ok (= 200 ph)))
        ;; Cleanup
        (lem:delete-window window)))))

(deftest floating-window/pixel-bounds-calculated-from-chars
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (lem:make-buffer "test-floating" :temporary t))
             (window (lem:make-floating-window :buffer buffer
                                               :x 5 :y 3
                                               :width 20 :height 10)))
        (multiple-value-bind (px py pw ph)
            (lem:floating-window-pixel-bounds window)
          ;; fake-interface returns 1 for char-width and char-height
          ;; So pixel values should equal character values * 1
          (ok (= 5 px))   ; 5 * 1
          (ok (= 3 py))   ; 3 * 1
          (ok (= 20 pw))  ; 20 * 1
          (ok (= 10 ph))) ; 10 * 1
        ;; Cleanup
        (lem:delete-window window)))))

(deftest floating-window/pixel-bounds-partial
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (lem:make-buffer "test-floating" :temporary t))
             (window (lem:make-floating-window :buffer buffer
                                               :x 5 :y 3
                                               :width 20 :height 10
                                               :pixel-x 100)))
        (multiple-value-bind (px py pw ph)
            (lem:floating-window-pixel-bounds window)
          ;; pixel-x is explicit, others calculated
          (ok (= 100 px))  ; explicit
          (ok (= 3 py))    ; calculated: 3 * 1
          (ok (= 20 pw))   ; calculated: 20 * 1
          (ok (= 10 ph)))  ; calculated: 10 * 1
        ;; Cleanup
        (lem:delete-window window)))))

(defclass pixel-fake-interface (fake-interface)
  ()
  (:default-initargs :support-pixel-positioning t)
  (:documentation "A fake interface that places floating windows by pixels,
with 10 by 20 pixel cells."))

(defmethod lem-if:get-char-width ((implementation pixel-fake-interface))
  10)

(defmethod lem-if:get-char-height ((implementation pixel-fake-interface))
  20)

(defun call-with-pixel-floating-window (function)
  "Call FUNCTION with a floating window whose cells say x 5, y 3, 20 by 10,
but which is placed by pixels at 300, 200, 100 by 40."
  (lem:with-current-buffers ()
    (lem:with-implementation (make-instance 'pixel-fake-interface)
      (lem:setup-first-frame)
      (let* ((buffer (lem:make-buffer "test-floating" :temporary t))
             (window (lem:make-floating-window :buffer buffer
                                               :x 5 :y 3
                                               :width 20 :height 10
                                               :pixel-x 300 :pixel-y 200
                                               :pixel-width 100 :pixel-height 40)))
        (unwind-protect (funcall function window)
          (lem:delete-window window))))))

(deftest floating-window/hit-test-by-pixels
  (call-with-pixel-floating-window
   (lambda (window)
     (multiple-value-bind (target x y)
         (lem:focus-window-position (lem:current-frame) 31 10 :pixel-x 315 :pixel-y 225)
       (ok (eq target window) "a click inside the pixel bounds hits it")
       (ok (equal (list x y) '(1 1)) "at the cell under the pointer"))
     (ng (eq window (lem:focus-window-position (lem:current-frame) 6 4
                                               :pixel-x 60 :pixel-y 80))
         "a click inside its cell bounds but outside its pixel bounds misses it")
     (ok (eq window (lem:focus-window-position (lem:current-frame) 6 4))
         "without a pixel position, cells are used"))))

(deftest floating-window/hit-test-ignores-pixels-without-support
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (lem:make-buffer "test-floating" :temporary t))
             (window (lem:make-floating-window :buffer buffer
                                               :x 5 :y 3
                                               :width 20 :height 10
                                               :pixel-x 300 :pixel-y 200)))
        (ok (eq window (lem:focus-window-position (lem:current-frame) 6 4
                                                  :pixel-x 6 :pixel-y 4))
            "a frontend without pixel positioning draws it at its cells")
        (lem:delete-window window)))))
