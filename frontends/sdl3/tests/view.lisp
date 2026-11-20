(defpackage :lem-sdl3/tests/view
  (:use :cl :rove)
  (:local-nicknames (:view :lem-sdl3/view)
                    (:display :lem-sdl3/display)))
(in-package :lem-sdl3/tests/view)

(defclass stub-cell-display ()
  ((char-width :initarg :char-width :reader display:display-char-width)
   (char-height :initarg :char-height :reader display:display-char-height))
  (:documentation "A stub display with only a cell size."))

(defun make-view (&rest initargs)
  "Make a view with no window or texture, at cell 2, 3 and 10 by 4 cells,
with INITARGS overriding those."
  (apply #'make-instance 'view:view
         (append initargs (list :window nil :x 2 :y 3 :width 10 :height 4
                                :use-modeline nil))))

(defun pixel-rect (view)
  (multiple-value-list
   (view:view-pixel-rect view (make-instance 'stub-cell-display
                                             :char-width 9 :char-height 17))))

(deftest pixel-rect-follows-the-cells
  (ok (equal (pixel-rect (make-view)) '(18 51 90 68))))

(deftest pixel-overrides-win
  (ok (equal (pixel-rect (make-view :pixel-x 5 :pixel-y 7 :pixel-width 100 :pixel-height 40))
             '(5 7 100 40))
      "all four")
  (ok (equal (pixel-rect (make-view :pixel-x 5 :pixel-height 40))
             '(5 51 90 40))
      "only the ones set"))

(deftest cell-changes-move-a-view-without-overrides
  (let ((view (make-view)))
    (setf (view:view-x view) 4
          (view:view-height view) 6)
    (ok (equal (pixel-rect view) '(36 51 90 102)))))
