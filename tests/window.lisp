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

(defun set-display-size (width height)
  (setf (slot-value (lem:implementation) 'lem-fake-interface::display-width) width
        (slot-value (lem:implementation) 'lem-fake-interface::display-height) height))

(deftest balance-windows/fixed-height
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((upper (lem:current-window))
             (lower (progn (lem:split-window-vertically upper :height 15)
                           (lem:get-next-window upper)))
             (height (lem:window-height lower)))
        (setf (lem:window-height-fixed-p lower) t)
        (lem:balance-windows)
        (ok (= height (lem:window-height lower)) "the fixed window keeps its height")
        (setf (lem:window-height-fixed-p lower) nil)
        (lem:balance-windows)
        (ok (<= (abs (- (lem:window-height upper) (lem:window-height lower))) 1)
            "without it, the windows are balanced")))))

(deftest resize-display/proportional-and-fixed
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((left (lem:current-window))
             (right (progn (lem:split-window-horizontally left :width 20)
                           (lem:get-next-window left)))
             (input (progn (lem:split-window-vertically right :height 15)
                           (lem:get-next-window right)))
             (input-height (lem:window-height input))
             (left-share (/ (lem:window-width left) 80)))
        (setf (lem:window-height-fixed-p input) t)
        (set-display-size 160 48)
        (lem-core::adjust-all-window-size)
        (ok (<= (abs (- (/ (lem:window-width left) 160) left-share)) 1/80)
            "the windows side by side keep their share of the width")
        (ok (= input-height (lem:window-height input))
            "the window of fixed height keeps it")
        (ok (< 30 (lem:window-height right))
            "the window above it takes the rest of the height")
        (set-display-size 80 24)
        (lem-core::adjust-all-window-size)
        (ok (= input-height (lem:window-height input))
            "it keeps it when the display shrinks too")))))

(deftest resize-display/back-and-forth-without-drift
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((left (lem:current-window))
             (right (progn (lem:split-window-horizontally left :width 30)
                           (lem:get-next-window left)))
             (lower (progn (lem:split-window-vertically right :height 7)
                           (lem:get-next-window right))))
        (flet ((sizes ()
                 (mapcar (lambda (w) (list (lem:window-width w) (lem:window-height w)))
                         (list left right lower))))
          (let ((before (sizes)))
            (loop :for (width height) :in '((47 13) (113 41) (61 17) (80 24) (29 11) (150 50) (80 24))
                  :do (set-display-size width height)
                      (lem-core::adjust-all-window-size))
            (ok (equal before (sizes)) "the same display, the same windows")))))))

(deftest resize-display/leftside-window
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (lem-core::make-leftside-window (lem:make-buffer "side" :temporary t))
      (let ((side (lem-core::frame-leftside-window (lem:current-frame))))
        (set-display-size 120 40)
        (lem-core::adjust-all-window-size)
        (ok (= (lem-core::max-window-height (lem:current-frame)) (lem:window-height side))
            "as tall as the frame after it grew")
        (set-display-size 80 20)
        (lem-core::adjust-all-window-size)
        (ok (= (lem-core::max-window-height (lem:current-frame)) (lem:window-height side))
            "and after it shrank")
        (lem-core::delete-leftside-window)))))

(defun side-windows ()
  (list (lem:frame-leftside-window (lem:current-frame))
        (lem:frame-rightside-window (lem:current-frame))
        (lem:frame-bottomside-window (lem:current-frame))))

(defun call-with-side-windows-showing (buffer function)
  (lem:make-leftside-window buffer)
  (lem:make-rightside-window buffer)
  (lem:make-bottomside-window buffer)
  (unwind-protect (funcall function)
    (lem:delete-bottomside-window)
    (lem:delete-rightside-window)
    (lem:delete-leftside-window)))

(deftest get-buffer-windows/side-windows-once-each
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let ((buffer (lem:make-buffer "side")))
        (call-with-side-windows-showing
         buffer
         (lambda ()
           (let ((windows (lem:get-buffer-windows buffer :include-floating-windows t)))
             (ok (= 3 (length windows)))
             (ok (= 3 (length (remove-duplicates windows))))
             (ok (every (lambda (side) (member side windows)) (side-windows))))
           (testing "without floating windows, no side window"
             (ok (null (lem:get-buffer-windows buffer))))))))))

(deftest kill-buffer/side-windows-show-another-buffer
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let ((other (lem:make-buffer "other"))
            (killed (lem:make-buffer "killed")))
        (lem:switch-to-buffer other)
        (call-with-side-windows-showing
         killed
         (lambda ()
           (lem:kill-buffer killed)
           (ok (lem:deleted-buffer-p killed))
           (ok (notany (lambda (side)
                         (lem:deleted-buffer-p (lem:window-buffer side)))
                       (side-windows)))))))))

(deftest kill-buffer/attached-buffer-deleted-once
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (make-attached-buffer))
             (attached (lem-core::buffer-attached-buffer buffer))
             (count 0))
        (lem:make-buffer "other")
        (lem:switch-to-buffer buffer)
        (lem:split-window-vertically (lem:current-window))
        (assert (= 2 (length (lem:get-buffer-windows buffer))))
        (lem:add-hook (lem:variable-value 'lem:kill-buffer-hook :buffer attached)
                      (lambda (buffer)
                        (declare (ignore buffer))
                        (incf count)))
        (lem:kill-buffer buffer)
        (ok (lem:deleted-buffer-p attached))
        (ok (= 1 count))))))

(deftest kill-buffer/attached-buffer-of-a-hidden-buffer
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (make-attached-buffer))
             (attached (lem-core::buffer-attached-buffer buffer))
             (count 0))
        (lem:switch-to-buffer (lem:make-buffer "other"))
        (assert (null (lem:get-buffer-windows buffer :include-floating-windows t)))
        (lem:add-hook (lem:variable-value 'lem:kill-buffer-hook :buffer attached)
                      (lambda (buffer)
                        (declare (ignore buffer))
                        (incf count)))
        (lem:kill-buffer buffer)
        (ok (lem:deleted-buffer-p buffer))
        (testing "the attached buffer is deleted although BUFFER was in no window"
          (ok (lem:deleted-buffer-p attached))
          (ok (= 1 count)))))))

(deftest delete-buffer/refused-while-a-header-window-shows-it
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (lem:make-buffer "header" :temporary t))
             (header (make-instance 'lem:header-window :buffer buffer)))
        (unwind-protect
             (progn
               (ok (signals (lem:delete-buffer buffer)))
               (testing "nothing is changed"
                 (ng (lem:deleted-buffer-p buffer))
                 (ok (eq buffer (lem:window-buffer header)))))
          (lem:delete-window header))
        (testing "once the header window is deleted, the buffer can be"
          (lem:delete-buffer buffer)
          (ok (lem:deleted-buffer-p buffer)))))))

(deftest delete-buffer/refused-while-a-header-window-shows-the-attached-buffer
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (make-attached-buffer))
             (attached (lem-core::buffer-attached-buffer buffer))
             (header (make-instance 'lem:header-window :buffer attached)))
        (lem:switch-to-buffer buffer)
        (unwind-protect
             (let ((windows (lem:get-buffer-windows buffer)))
               (assert windows)
               (ok (signals (lem:delete-buffer buffer)))
               (testing "nothing is changed, the windows still show BUFFER"
                 (ng (lem:deleted-buffer-p buffer))
                 (ng (lem:deleted-buffer-p attached))
                 (ok (equal windows (lem:get-buffer-windows buffer)))))
          (lem:delete-window header))))))

(deftest delete-window/a-header-window-leaves-every-frame
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((buffer (lem:make-buffer "header" :temporary t))
             (header (make-instance 'lem:header-window :buffer buffer))
             ;; As frame-multiplexer's new tabs: the new frame gets the header windows.
             (other (lem:make-frame (lem:current-frame))))
        (lem:setup-frame other (lem:make-buffer "other"))
        (unwind-protect
             (progn
               (assert (member header (lem:frame-header-windows other)))
               (lem:delete-window header)
               (ok (null (lem:frame-header-windows other)))
               (testing "so its buffer can be deleted afterwards"
                 (lem:delete-buffer buffer)
                 (ok (lem:deleted-buffer-p buffer))))
          (lem:teardown-frame other))))))

(defun buffer-has-point-p (buffer point)
  ;; lem exports no way to list the points registered in a buffer.
  (and (member point (lem/buffer/internal::buffer-points buffer)) t))

(defun window-points (window)
  (list (lem:window-view-point window) (lem-core::%window-point window)))

(defun call-recording-freed-windows (function)
  "Call FUNCTION with the windows freed during it recorded, and return them."
  (let ((freed '())
        (original (fdefinition 'lem-core::%free-window)))
    ;; lem-core is a locked package on SBCL.
    (#+sbcl sb-ext:without-package-locks #-sbcl progn
      (unwind-protect
           (progn
             (setf (fdefinition 'lem-core::%free-window)
                   (lambda (window)
                     (push window freed)
                     (funcall original window)))
             (funcall function))
        (setf (fdefinition 'lem-core::%free-window) original)))
    freed))

(deftest teardown-frame/frees-side-and-header-windows-once
  (lem:with-current-buffers ()
    (let* ((shown (lem:make-buffer "shown"))
           (windows '())
           (header-points '())
           (freed
             (call-recording-freed-windows
              (lambda ()
                (with-fake-interface ()
                  (lem:make-leftside-window shown)
                  (lem:make-bottomside-window shown)
                  (let* ((header (make-instance 'lem:header-window :buffer shown))
                         ;; As frame-multiplexer's new tabs: the new frames get
                         ;; the header windows; one is never set up.
                         (other (lem:make-frame (lem:current-frame))))
                    (lem:make-frame (lem:current-frame))
                    (lem:setup-frame other (lem:make-buffer "other"))
                    (setf windows (list header
                                        (lem:frame-leftside-window (lem:current-frame))
                                        (lem:frame-bottomside-window (lem:current-frame)))
                          header-points (window-points header))
                    (testing "a header window another frame still has is kept"
                      (lem:teardown-frame other)
                      (ng (lem-core::window-deleted-p header))
                      (ok (every (lambda (point) (buffer-has-point-p shown point))
                                 header-points)))))))))
      (testing "the side and header windows are freed, each once"
        (ok (every (lambda (window) (= 1 (count window freed))) windows))
        (ok (= (length freed) (length (remove-duplicates freed)))))
      (testing "the points of the header window are deleted"
        (ok (notany (lambda (point) (buffer-has-point-p shown point))
                    header-points))))))

(deftest adjust-all-window-size/header-windows-move-up
  ;; adjust-all-window-size and topleft-window-y are internal: redisplay runs
  ;; them, which the fake interface does not draw.
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((upper (make-instance 'lem:header-window
                                   :buffer (lem:make-buffer "upper" :temporary t)))
             (lower (make-instance 'lem:header-window
                                   :buffer (lem:make-buffer "lower" :temporary t))))
        (unwind-protect
             (progn
               (assert (= 1 (lem:window-y lower)))
               (lem:delete-window upper)
               (lem-core::adjust-all-window-size)
               (testing "the header window under the deleted one moves up"
                 (ok (= 0 (lem:window-y lower))))
               (testing "and the editor windows begin under it"
                 (ok (= 1 (lem-core::topleft-window-y (lem:current-frame))))
                 (ok (= 1 (lem:window-y (lem:current-window))))))
          (lem:delete-window lower))))))

(deftest teardown-frame/frees-attached-windows
  ;; window-attached-window and window-deleted-p are internal: lem exports no
  ;; way to reach an attached window or to tell that a window was freed.
  (lem:with-current-buffers ()
    (let* ((buffer (make-attached-buffer))
           (attached (lem-core::buffer-attached-buffer buffer))
           (window nil)
           (points '()))
      (with-fake-interface ()
        (lem:switch-to-buffer buffer)
        (setf window (lem-core::window-attached-window
                      (first (lem:get-buffer-windows buffer))))
        (assert window)
        (setf points (window-points window))
        (ok (every (lambda (point) (buffer-has-point-p attached point)) points)))
      (testing "the attached window is freed with its frame"
        (ok (lem-core::window-deleted-p window))
        (ok (notany (lambda (point) (buffer-has-point-p attached point)) points))))))

(deftest teardown-frames/every-frame
  ;; window-deleted-p is internal: lem exports no way to tell that a window
  ;; was freed.
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((hidden-buffer (lem:make-buffer "hidden"))
             ;; As a hidden frame-multiplexer tab: made and set up, not mapped.
             (hidden (lem:make-frame (lem:current-frame)))
             (shown-window (lem:current-window)))
        (lem:setup-frame hidden hidden-buffer)
        (let* ((hidden-window (first (lem:window-list hidden)))
               (points (window-points hidden-window)))
          (lem:teardown-frames)
          (ok (null (lem:all-frames)))
          (ok (lem-core::window-deleted-p shown-window))
          (testing "the frame not mapped to the display is torn down too"
            (ok (lem-core::window-deleted-p hidden-window))
            (ok (notany (lambda (point) (buffer-has-point-p hidden-buffer point))
                        points))))))))

(deftest map-protected/the-first-error-is-signalled
  ;; map-protected is internal: a helper of teardown-frames and teardown-windows.
  (let ((called '())
        (conditions '()))
    (ok (equal "1"
               (handler-case
                   (handler-bind ((condition (lambda (c) (push c conditions))))
                     (lem-core::map-protected
                      (lambda (n)
                        (push n called)
                        (when (oddp n)
                          (error "~D" n)))
                      '(0 1 2 3 4)))
                 (error (e) (princ-to-string e)))))
    (testing "every element is called, also after the errors"
      (ok (equal '(0 1 2 3 4) (reverse called))))
    (testing "the later error is not signalled out of map-protected (it is logged)"
      (ok (= 1 (count-if (lambda (c) (typep c 'error)) conditions)))
      (ok (notany (lambda (c) (typep c 'warning)) conditions)))))

(deftest map-protected/nested-failures-signal-nothing-that-skips-the-rest
  ;; map-protected is internal: a helper of teardown-frames and teardown-windows.
  (let ((called '()))
    (block exit
      (handler-bind ((warning (lambda (w)
                                (declare (ignore w))
                                (return-from exit))))
        (handler-case
            (lem-core::map-protected
             (lambda (n)
               (push n called)
               (case n
                 (0 (error "0"))
                 ;; As teardown-windows inside teardown-frames: a nested
                 ;; map-protected whose two calls fail.  Nothing is signalled
                 ;; for the second, so the handler of warnings above, which
                 ;; exits, does not run.
                 (1 (lem-core::map-protected (lambda (x) (error "~A" x)) '(a b)))))
             '(0 1 2))
          (error () nil))))
    (ok (equal '(0 1 2) (reverse called)))))
