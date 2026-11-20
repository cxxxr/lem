(defpackage :lem-sdl3/tests/view
  (:use :cl :rove)
  (:local-nicknames (:view :lem-sdl3/view)
                    (:display :lem-sdl3/display)))
(in-package :lem-sdl3/tests/view)

(defclass stub-cell-display ()
  ((char-width :initarg :char-width :reader display:display-char-width)
   (char-height :initarg :char-height :reader display:display-char-height)
   (width :initarg :width :initform 900 :reader display:display-width)
   (height :initarg :height :initform 680 :reader display:display-height))
  (:documentation "A stub display with a cell size and a pixel size, by
default a whole number of cells."))

(defun make-view (&rest initargs)
  "Make a view with no window or texture, at cell 2, 3 and 10 by 4 cells,
with INITARGS overriding those."
  (apply #'make-instance 'view:view
         (append initargs (list :window nil :x 2 :y 3 :width 10 :height 4
                                :use-modeline nil))))

(defun stub-display (&key (width 900) (height 680))
  (make-instance 'stub-cell-display :char-width 9 :char-height 17
                                    :width width :height height))

(defun pixel-rect (view &rest display-args)
  (multiple-value-list (view:view-pixel-rect view (apply #'stub-display display-args))))

(defun layout-size (view &rest display-args)
  (multiple-value-list (view:view-layout-size view (apply #'stub-display display-args))))

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

(deftest move-position-sets-pixel-overrides-when-given
  (let ((view (make-view :pixel-x 5 :pixel-y 7)))
    (view:move-position view 4 1)
    (ok (equal (pixel-rect view) '(5 7 90 68))
        "a cell move keeps the pixel position")
    (view:move-position view 4 1 11 13)
    (ok (equal (pixel-rect view) '(11 13 90 68))
        "a pixel move replaces it")))

(deftest views-at-the-edge-take-the-remainder
  ;; 905 by 690 pixels is 100 columns and 40 rows, 5 and 10 pixels over.
  (let ((view (make-view :x 90 :y 36)))
    (ok (equal (pixel-rect view :width 905 :height 690) '(810 612 95 78))
        "the last column and row reach the edge")
    (ok (equal (layout-size view :width 905 :height 690) '(90 68))
        "lem-core still lays lines out in whole cells"))
  (ok (equal (pixel-rect (make-view :x 90) :width 905 :height 690) '(810 51 95 68))
      "only the right edge")
  (ok (equal (pixel-rect (make-view) :width 905 :height 690) '(18 51 90 68))
      "away from the edges, nothing changes")
  (ok (equal (pixel-rect (make-view :x 90 :y 36 :pixel-width 50) :width 905 :height 690)
             '(810 612 50 68))
      "a view placed by pixels keeps its size"))

(deftest scroll-delta-keeps-less-than-a-line
  (let ((view (make-view)))
    (ok (= 0 (view:add-scroll-delta view 10 17)) "part of a line scrolls no lines")
    (ok (= 10 (view:view-scroll-offset view)))
    (ok (= 1 (view:add-scroll-delta view 10 17)) "crossing a line scrolls one")
    (ok (= 3 (view:view-scroll-offset view)) "and keeps the rest")
    (ok (= -1 (view:add-scroll-delta view -5 17)) "scrolling back past the line")
    (ok (= 15 (view:view-scroll-offset view)) "leaves the offset into the line above")
    (ok (= 2 (view:add-scroll-delta view 19 17)) "several lines at once")
    (ok (= 0 (view:view-scroll-offset view)))))

(deftest a-view-with-a-modeline-has-an-overscan-row
  (let ((display (stub-display))
        (view (make-view :use-modeline t)))
    (ok (= 85 (view:view-render-height view display))
        "lem-core draws one row more than the view shows")
    (ok (equal (multiple-value-list (view:view-texture-size view display)) '(90 85))
        "and the texture holds it")
    (ok (equal (pixel-rect view) '(18 51 90 68))
        "the view's rectangle on the display is unchanged"))
  (let ((display (stub-display))
        (view (make-view)))
    (ok (= 68 (view:view-render-height view display)) "no modeline, no overscan row")))
