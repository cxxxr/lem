(defpackage :lem-sdl3/box-drawing
  (:use :cl)
  (:export :drawn-character-p
           :glyph-rects
           :light-thickness)
  (:documentation "Box-drawing and block-element characters (U+2500..U+259F)
as rectangles sized to the cell, as terminals draw them, so lines meet
across cells whatever the font and the scale."))
(in-package :lem-sdl3/box-drawing)

(defparameter *line-arms*
  (concatenate
   'string
   ".L.L.H.HL.L.H.H.????????????????????????????????.LL..HL..LH..HH."
   "..LL..LH..HL..HHLL..LH..HL..HH..L..LL..HH..LH..HLLL.LHL.HLL.LLH."
   "HLH.HHL.LHH.HHH.L.LLL.LHH.LLL.HLH.HLH.LHL.HHH.HH.LLL.LLH.HLL.HLH"
   ".LHL.LHH.HHL.HHHLL.LLL.HLH.LLH.HHL.LHL.HHH.LHH.HLLLLLLLHLHLLLHLH"
   "HLLLLLHLHLHLHLLHHHLLLLHHLHHLHHLHLHHHHLHHHHHLHHHH????????????????"
   ".D.DD.D..DL..LD..DD...LD..DL..DDLD..DL..DD..L..DD..LD..DLDL.DLD."
   "DDD.L.LDD.DLD.DD.DLD.LDL.DDDLD.DDL.LDD.DLDLDDLDLDDDD????????????"
   "????????????????...LL....L....L....HH....H....H..H.LL.H..L.HH.L.")
  "For each of U+2500..U+257F, four characters giving the up, right, down
and left arms: #\\. none, #\\L light, #\\H heavy, #\\D double; #\\? for the
dashed lines, arcs and diagonals, which are not drawn from arms.")

(defparameter *dashes*
  '((#\┄ :horizontal #\L 3) (#\┅ :horizontal #\H 3)
    (#\┆ :vertical #\L 3) (#\┇ :vertical #\H 3)
    (#\┈ :horizontal #\L 4) (#\┉ :horizontal #\H 4)
    (#\┊ :vertical #\L 4) (#\┋ :vertical #\H 4)
    (#\╌ :horizontal #\L 2) (#\╍ :horizontal #\H 2)
    (#\╎ :vertical #\L 2) (#\╏ :vertical #\H 2))
  "Dashed lines: (CHARACTER DIRECTION WEIGHT DASHES).")

(defparameter *arcs*
  '((#\╭ 1 1) (#\╮ -1 1) (#\╯ -1 -1) (#\╰ 1 -1))
  "Rounded corners: (CHARACTER HORIZONTAL VERTICAL), the directions of the
arm they turn into, 1 for right or down and -1 for left or up.")

(defun light-thickness (cell-width)
  "The pixel thickness of a light line in a cell CELL-WIDTH pixels wide;
heavy lines are twice that, and double lines are two light lines."
  (max 1 (round cell-width 8)))

(defun line-arms (character)
  "Return CHARACTER's arms as a list (UP RIGHT DOWN LEFT) of weight
characters, or NIL if it isn't drawn from arms."
  (let ((index (- (char-code character) #x2500)))
    (when (<= 0 index #x7f)
      (let ((arms (coerce (subseq *line-arms* (* 4 index) (* 4 (1+ index))) 'list)))
        (unless (member #\? arms)
          (substitute nil #\. arms))))))

(defun rails (weight size light)
  "The lines an arm of WEIGHT draws across SIZE pixels, centred, as
(POSITION . THICKNESS) in increasing position."
  (flet ((start (thickness) (floor (- size thickness) 2)))
    (ecase weight
      (#\L (list (cons (start light) light)))
      (#\H (list (cons (start (* 2 light)) (* 2 light))))
      (#\D (let ((outer (start (* 3 light))))
             (list (cons outer light)
                   (cons (+ outer (* 2 light)) light)))))))

(defun far-edge (rail)
  (+ (car rail) (cdr rail)))

(defun vertical-arm-rects (arms width height light)
  "Rectangles (X Y W H) for the up and down arms in ARMS, (UP RIGHT DOWN
LEFT), in a WIDTH × HEIGHT cell.  Each rail of an arm stops where it meets
the side arm next to it: on that arm's nearest rail (an inner corner), else
on the far rail of the arm on the other side (an outer corner), else at
the centre, where an arm going on the other way meets it."
  (destructuring-bind (up right down left) arms
    (labels ((cross-rails (weight) (rails weight height light))
             (up-end (near far)
               (cond (near (far-edge (first (cross-rails near))))
                     (far (far-edge (first (last (cross-rails far)))))
                     (t (far-edge (first (cross-rails #\L))))))
             (down-start (near far)
               (cond (near (car (first (last (cross-rails near)))))
                     (far (car (first (cross-rails far))))
                     (t (car (first (cross-rails #\L))))))
             (arm-rects (weight extent)
               (let ((rails (rails weight width light)))
                 (if (rest rails)
                     (list (funcall extent (first rails) left right)
                           (funcall extent (second rails) right left))
                     (list (funcall extent (first rails) left right :single t))))))
      (append
       (when up
         (arm-rects up (lambda (rail near far &key single)
                         (let ((end (if single
                                        (max (up-end near far) (up-end far near))
                                        (up-end near far))))
                           (list (car rail) 0 (cdr rail) end)))))
       (when down
         (arm-rects down (lambda (rail near far &key single)
                           (let ((start (if single
                                            (min (down-start near far) (down-start far near))
                                            (down-start near far))))
                             (list (car rail) start (cdr rail) (- height start))))))))))

(defun line-rects (arms width height light)
  "Rectangles for every arm in ARMS: the vertical ones directly, the
horizontal ones as the vertical arms of the transposed cell."
  (destructuring-bind (up right down left) arms
    (append (vertical-arm-rects arms width height light)
            (loop :for (y x h w) :in (vertical-arm-rects (list left down right up)
                                                         height width light)
                  :collect (list x y w h)))))

(defun dash-rects (direction weight dashes width height light)
  "Rectangles for a line of DASHES dashes running in DIRECTION."
  (let* ((length (if (eq direction :horizontal) width height))
         (gap (max 1 (round length (* 3 dashes)))))
    (loop :for i :below dashes
          :for start := (floor (* i length) dashes)
          :for end := (- (floor (* (1+ i) length) dashes) gap)
          :append (loop :for rail :in (rails weight (if (eq direction :horizontal) height width)
                                             light)
                        :collect (if (eq direction :horizontal)
                                     (list start (car rail) (- end start) (cdr rail))
                                     (list (car rail) start (cdr rail) (- end start)))))))

(defun arc-rects (horizontal vertical width height light)
  "Rectangles for a rounded corner joining light arms towards HORIZONTAL
and VERTICAL (see *arcs*): a quarter circle through the centre lines, then
straight lines to the cell edges, so the edges match │ and ─."
  (let* ((column (first (rails #\L width light)))
         (row (first (rails #\L height light)))
         (line-x (+ (car column) (/ light 2)))
         (line-y (+ (car row) (/ light 2)))
         (radius (max 1 (1- (min (if (plusp horizontal) (- width line-x) line-x)
                                 (if (plusp vertical) (- height line-y) line-y)))))
         (center-x (+ line-x (* horizontal radius)))
         (center-y (+ line-y (* vertical radius)))
         (rects '()))
    (flet ((ink-p (x y)
             (let ((dx (- (+ x 1/2) center-x))
                   (dy (- (+ y 1/2) center-y)))
               (and (<= (* dx horizontal) 0)
                    (<= (* dy vertical) 0)
                    (<= (abs (- (sqrt (+ (* dx dx) (* dy dy))) radius)) (/ light 2)))))
           (add (x y w h)
             (when (and (plusp w) (plusp h))
               (push (list x y w h) rects))))
      (dotimes (y height)
        (let ((start nil))
          (dotimes (x (1+ width))
            (let ((ink (and (< x width) (ink-p x y))))
              (cond ((and ink (not start)) (setf start x))
                    ((and (not ink) start)
                     (add start y (- x start) 1)
                     (setf start nil)))))))
      (if (plusp vertical)
          (add (car column) (floor center-y) light (- height (floor center-y)))
          (add (car column) 0 light (ceiling center-y)))
      (if (plusp horizontal)
          (add (floor center-x) (car row) (- width (floor center-x)) light)
          (add 0 (car row) (ceiling center-x) light)))
    (nreverse rects)))

(defun block-rects (character width height)
  "Rectangles for the block element CHARACTER (U+2580..U+259F), and as a
second value the shade (1/4, 1/2 or 3/4) for the shade characters."
  (let* ((code (char-code character))
         (half-x (floor width 2))
         (half-y (floor height 2))
         (quadrants (list (list :upper-left 0 0 half-x half-y)
                          (list :upper-right half-x 0 (- width half-x) half-y)
                          (list :lower-left 0 half-y half-x (- height half-y))
                          (list :lower-right half-x half-y (- width half-x) (- height half-y)))))
    (flet ((eighths (size n) (round (* size n) 8))
           (quadrants (&rest names)
             (mapcar (lambda (name) (rest (assoc name quadrants))) names)))
      (cond ((= code #x2580) (list (list 0 0 width half-y)))
            ((<= #x2581 code #x2588)
             (let ((h (eighths height (- code #x2580))))
               (list (list 0 (- height h) width h))))
            ((<= #x2589 code #x258f)
             (list (list 0 0 (eighths width (- #x2590 code)) height)))
            ((= code #x2590) (list (list half-x 0 (- width half-x) height)))
            ((<= #x2591 code #x2593)
             (values (list (list 0 0 width height)) (/ (- code #x2590) 4)))
            ((= code #x2594) (list (list 0 0 width (eighths height 1))))
            ((= code #x2595)
             (let ((w (eighths width 1)))
               (list (list (- width w) 0 w height))))
            (t
             (apply #'quadrants
                    (ecase code
                      (#x2596 '(:lower-left))
                      (#x2597 '(:lower-right))
                      (#x2598 '(:upper-left))
                      (#x2599 '(:upper-left :lower-left :lower-right))
                      (#x259a '(:upper-left :lower-right))
                      (#x259b '(:upper-left :upper-right :lower-left))
                      (#x259c '(:upper-left :upper-right :lower-right))
                      (#x259d '(:upper-right))
                      (#x259e '(:upper-right :lower-left))
                      (#x259f '(:upper-right :lower-left :lower-right)))))))))

(defun drawn-character-p (character)
  "Return true if CHARACTER is drawn as rectangles, that is if glyph-rects
returns any for it, without working them out."
  (let ((code (char-code character)))
    (and (<= #x2500 code #x259f)
         (or (<= #x2580 code)
             (char/= #\? (char *line-arms* (* 4 (- code #x2500))))
             (assoc character *arcs*)
             (assoc character *dashes*))
         t)))

(defun glyph-rects (character width height)
  "Return the rectangles (X Y W H), relative to the cell, that draw
CHARACTER in a WIDTH × HEIGHT cell, and as a second value a shade
(1/4, 1/2, 3/4) to blend the foreground into the background with, or NIL
for solid.  Return NIL for characters not drawn this way (anything outside
U+2500..U+259F, and the diagonals, which come from the font)."
  (let ((code (char-code character))
        (light (light-thickness width)))
    (cond ((<= #x2580 code #x259f) (block-rects character width height))
          ((assoc character *arcs*)
           (destructuring-bind (horizontal vertical) (rest (assoc character *arcs*))
             (arc-rects horizontal vertical width height light)))
          ((assoc character *dashes*)
           (destructuring-bind (direction weight dashes) (rest (assoc character *dashes*))
             (dash-rects direction weight dashes width height light)))
          (t
           (let ((arms (line-arms character)))
             (when arms
               (line-rects arms width height light)))))))
