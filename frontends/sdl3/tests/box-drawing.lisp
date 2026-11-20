(defpackage :lem-sdl3/tests/box-drawing
  (:use :cl :rove)
  (:local-nicknames (:box :lem-sdl3/box-drawing)))
(in-package :lem-sdl3/tests/box-drawing)

;;; Box-drawing characters are drawn as rectangles; these tests rasterize
;;; them into a bitmap and check the properties that make lines meet across
;;; cells and the shapes come out right.

(defparameter *cell-sizes* '((8 17) (9 19) (16 33) (17 34))
  "Cell sizes at 1x and 2x scale, even and odd.")

(defun bitmap (character width height)
  "A WIDTH × HEIGHT array, T where CHARACTER's rectangles have ink."
  (let ((bitmap (make-array (list height width) :initial-element nil)))
    (loop :for (x y w h) :in (box:glyph-rects character width height)
          :do (loop :for row :from y :below (+ y h)
                    :do (loop :for column :from x :below (+ x w)
                              :do (setf (aref bitmap row column) t))))
    bitmap))

(defun edge (bitmap side)
  "The ink along SIDE (:top :right :bottom :left) of BITMAP, as a list."
  (destructuring-bind (height width) (array-dimensions bitmap)
    (ecase side
      (:top (loop :for x :below width :collect (aref bitmap 0 x)))
      (:bottom (loop :for x :below width :collect (aref bitmap (1- height) x)))
      (:left (loop :for y :below height :collect (aref bitmap y 0)))
      (:right (loop :for y :below height :collect (aref bitmap y (1- width)))))))

(defun line-characters ()
  "The characters drawn from arms, and the rounded corners, with their arms
(UP RIGHT DOWN LEFT)."
  (append (loop :for code :from #x2500 :to #x257f
                :for character := (code-char code)
                :for arms := (box::line-arms character)
                :when arms
                  :collect (cons character arms))
          '((#\╭ nil #\L #\L nil) (#\╮ nil nil #\L #\L)
            (#\╯ #\L nil nil #\L) (#\╰ #\L #\L nil nil))))

(deftest rectangles-stay-inside-the-cell
  (loop :for (width height) :in *cell-sizes*
        :do (loop :for code :from #x2500 :to #x259f
                  :for rects := (box:glyph-rects (code-char code) width height)
                  :do (loop :for (x y w h) :in rects
                            :unless (and (<= 0 x) (<= 0 y) (plusp w) (plusp h)
                                         (<= (+ x w) width) (<= (+ y h) height))
                              :do (fail (format nil "U+~X at ~Dx~D: ~A"
                                                code width height (list x y w h))))))
  (pass "checked every character"))

(deftest arms-meet-the-next-cell
  ;; Wherever an arm reaches a cell edge its ink must be that of a straight
  ;; line of the same weight, or lines would not join between cells.
  (let ((straight '((#\L #\─ #\│) (#\H #\━ #\┃) (#\D #\═ #\║)))
        (failures '()))
    (loop :for (width height) :in *cell-sizes*
          :do (loop :for (character . arms) :in (line-characters)
                    :for bitmap := (bitmap character width height)
                    :do (loop :for weight :in arms
                              :for side :in '(:top :right :bottom :left)
                              :for vertical := (member side '(:top :bottom))
                              :when weight
                                :do (let* ((reference (assoc weight straight))
                                           (line (if vertical (third reference) (second reference))))
                                      (unless (equal (edge bitmap side)
                                                     (edge (bitmap line width height) side))
                                        (push (list character side width height) failures))))))
    (ok (null failures) (format nil "mismatched edges: ~S" failures))))

(deftest double-corner-has-an-inner-and-an-outer-line
  ;; ╔ at 16x34 with light 2: rails at 5-6 and 9-10 across the width,
  ;; 14-15 and 18-19 down the height.
  (let ((bitmap (bitmap #\╔ 16 34)))
    (ok (aref bitmap 14 5) "outer corner")
    (ok (aref bitmap 18 9) "inner corner")
    (ng (aref bitmap 16 7) "no ink between the rails")
    (ng (aref bitmap 14 4) "nothing outside the outer corner")
    (ng (aref bitmap 13 5) "nothing above the outer corner")))

(deftest double-cross-is-symmetric
  (let* ((bitmap (bitmap #\╬ 16 34))
         (mirrored (make-array '(34 16))))
    (dotimes (y 34)
      (dotimes (x 16)
        (setf (aref mirrored y x) (aref bitmap (- 33 y) (- 15 x)))))
    (ok (equalp bitmap mirrored))
    (ng (aref bitmap 17 8) "the centre is open")))

(deftest blocks-and-shades
  (flet ((ink (character)
           (let ((bitmap (bitmap character 16 32)))
             (loop :for i :below (array-total-size bitmap)
                   :count (row-major-aref bitmap i)))))
    (ok (= (ink #\█) (* 16 32)))
    (ok (= (ink #\▀) (* 16 16)))
    (ok (= (ink #\▌) (* 8 32)))
    (ok (= (ink #\▁) (* 16 4)))
    (ok (= (ink #\▙) (* 3 8 16)))
    (ok (= (ink #\▚) (* 2 8 16)))
    (ok (equal (multiple-value-list (box:glyph-rects #\▒ 16 32))
               '(((0 0 16 32)) 1/2)))))

(deftest dashed-lines-break-into-dashes
  (flet ((dashes (row)
           (loop :for (previous current) :on (cons nil row)
                 :count (and current (not previous)))))
    (ok (= 3 (dashes (loop :with bitmap := (bitmap #\┄ 16 34)
                           :for x :below 16
                           :collect (aref bitmap 16 x)))))
    (ok (= 4 (dashes (loop :with bitmap := (bitmap #\┊ 16 34)
                           :for y :below 34
                           :collect (aref bitmap y 7)))))))

(deftest arcs-are-round
  ;; ╭ at 16x34: the corner of the light lines' bounding box stays empty,
  ;; and there is ink on the diagonal between the two arms.
  (let ((bitmap (bitmap #\╭ 16 34)))
    (ng (aref bitmap 16 7) "the corner is cut")
    (ok (loop :for i :from 1 :below 8 :thereis (aref bitmap (+ 16 i) (+ 7 i)))
        "the curve crosses the diagonal")))

(deftest diagonals-and-other-characters-use-the-font
  (dolist (character '(#\╱ #\╲ #\╳ #\a #\日))
    (ok (null (box:glyph-rects character 16 34)) (string character))))

(deftest drawn-character-p-matches-glyph-rects
  (let ((mismatches
          (loop :for code :from #x2400 :to #x26ff
                :for character := (code-char code)
                :unless (eq (box:drawn-character-p character)
                            (and (box:glyph-rects character 16 34) t))
                  :collect character)))
    (ok (null mismatches) (format nil "mismatches: ~S" mismatches))))
