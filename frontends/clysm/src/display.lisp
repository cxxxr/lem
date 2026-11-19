;;;; display.lisp — Lem's screen as components of the UI framework
;;;;
;;;; Lem draws a window (a view) line by line: RENDER-LINE gives the drawing
;;;; objects of a line (strings with attributes, the cursor, the rest of the
;;;; line in a colour...).  Each line becomes a list of segments
;;;;   (column width text style)
;;;; in the view's canvas, and when Lem has drawn everything (UPDATE-DISPLAY)
;;;; the lines of the views that changed are given to the observed slots of
;;;; the views.  The components that show them are rendered again; a line
;;;; that is EQUAL to the one before is not (a component called with EQUAL
;;;; arguments is not rendered again), so a key typed changes one line of
;;;; the page.

(in-package :lem-clysm)

;;; ---- the state of the screen

(ui:defstate *views* '() "The views on the screen, in the order they are drawn.")
(ui:defstate *char-width* 8 "The width of a character cell, in pixels.")
(ui:defstate *char-height* 16 "The height of a character cell, in pixels.")
(ui:defstate *foreground* "#d8d8d8")
(ui:defstate *background* "#1d1f21")
(ui:defstate *font-size* 14 "The size of the font, in pixels.")
(ui:defstate *cursor* '(0 0) "The cursor's (column line) on the screen.")

(defvar *font-family*
  "'JetBrains Mono', 'DejaVu Sans Mono', 'Noto Sans Mono CJK JP', 'Noto Sans Mono', ui-monospace, monospace")

(defstruct canvas
  (lines #() :type simple-vector)       ; a list of segments for each line
  (modeline nil)
  (background nil)                      ; of a floating window
  (dirty t))

(ui:defmodel view ()
  ((window :initarg :window :reader view-window)
   (kind :initarg :kind :reader view-kind)          ; :tile, :floating or :header
   (border :initarg :border :initform nil :reader view-border)
   (border-shape :initarg :border-shape :initform nil :reader view-border-shape)
   (x :initarg :x :accessor view-x)
   (y :initarg :y :accessor view-y)
   (width :initarg :width :accessor view-width)
   (height :initarg :height :accessor view-height)
   (modeline-p :initarg :modeline-p :reader view-modeline-p)
   (canvas :initform (make-canvas) :reader view-canvas)
   ;; what the page shows: copies of the canvas
   (lines :initform #() :accessor view-lines)
   (modeline :initform nil :accessor view-modeline)
   (background :initform nil :accessor view-background)))

(defun resize-canvas (view)
  (let* ((canvas (view-canvas view))
         (old (canvas-lines canvas))
         (new (make-array (max 0 (view-height view)) :initial-element nil)))
    (replace new old)
    (setf (canvas-lines canvas) new
          (canvas-dirty canvas) t)))

(defun publish-view (view)
  "Give what was drawn in VIEW to the page."
  (let ((canvas (view-canvas view)))
    (when (canvas-dirty canvas)
      (setf (canvas-dirty canvas) nil
            (view-lines view) (copy-seq (canvas-lines canvas))
            (view-modeline view) (canvas-modeline canvas)
            (view-background view) (canvas-background canvas)))))

;;; ---- colours and attributes

(defun color-string (color)
  (etypecase color
    (null nil)
    (string (let ((parsed (lem:parse-color color)))
              (if parsed (lem:color-to-hex-string parsed) color)))
    (lem:color (lem:color-to-hex-string color))))

(defvar *style-cache* (make-hash-table :test 'eq)
  "attribute -> CSS of a segment; emptied when the colours of the editor change")

(defun attribute-style (attribute)
  (cond
    ((null attribute)
     (let ((background lem-if:*background-color-of-drawing-window*))
       (if background (format nil "background:~A;" (color-string background)) "")))
    ((gethash attribute *style-cache*))
    (t
     (setf (gethash attribute *style-cache*)
           (let* ((cursorp (lem:cursor-attribute-p attribute))
                  (foreground (if cursorp
                                  (color-string (lem-if:get-background-color (lem:implementation)))
                                  (color-string (lem:attribute-foreground-with-reverse attribute))))
                  (background (if cursorp
                                  (or (color-string (lem:attribute-background attribute))
                                      (color-string (lem-if:get-foreground-color (lem:implementation))))
                                  (color-string (lem:attribute-background-with-reverse attribute))))
                  (underline (lem:attribute-underline attribute)))
             (format nil "color:~A;background:~A;~@[font-weight:bold;~*~]~@[font-style:italic;~*~]~
                          ~:[~;text-decoration:underline~@[ ~A~];~]~@[text-decoration:line-through;~*~]~
                          ~@[opacity:0.6;~*~]"
                     foreground background
                     (lem:attribute-bold attribute)
                     (lem:attribute-italic attribute)
                     underline (and (or (stringp underline) (typep underline 'lem:color))
                                    (color-string underline))
                     (lem:attribute-strikethrough attribute)
                     (lem:attribute-dim attribute)))))))

;;; ---- drawing objects -> segments

(defun object-width (object)
  (typecase object
    (lem-core/display:text-object
     (lem:string-width (lem-core/display:text-object-string object)))
    (t 0)))

(defun cursor-object-p (object)
  (typecase object
    (lem-core/display:eol-cursor-object
     (lem-core/display:eol-cursor-object-true-cursor-p object))
    (lem-core/display:text-object
     (let ((attribute (lem-core/display:text-object-attribute object)))
       (and attribute (lem:cursor-attribute-p (lem:ensure-attribute attribute nil)))))))

(defun object-segment (object x view)
  "The segment (column width text style) that draws OBJECT at column X, or NIL."
  (typecase object
    (lem-core/display:void-object nil)
    (lem-core/display:image-object nil)
    (lem-core/display:eol-cursor-object
     (list x 1 " " (format nil "background:~A;"
                           (color-string (lem-core/display:eol-cursor-object-color object)))))
    (lem-core/display:extend-to-eol-object
     (let ((width (view-width view)))
       (when (< x width)
         (list x (- width x) ""
               (format nil "background:~A;"
                       (color-string (lem-core/display:extend-to-eol-object-color object)))))))
    (lem-core/display:line-end-object
     (let ((string (lem-core/display:text-object-string object)))
       (list (+ x (lem-core/display:line-end-object-offset object))
             (lem:string-width string)
             string
             (attribute-style (lem:ensure-attribute
                               (lem-core/display:text-object-attribute object) nil)))))
    (lem-core/display:text-object
     (let ((string (lem-core/display:text-object-string object)))
       (list x (lem:string-width string) string
             (attribute-style (lem:ensure-attribute
                               (lem-core/display:text-object-attribute object) nil)))))))

(defun objects-segments (objects x y view)
  "The segments of OBJECTS drawn from column X of line Y; notes where the
cursor is."
  (let ((segments '()))
    (dolist (object objects (nreverse segments))
      (when (and y (cursor-object-p object))
        (lem:set-last-print-cursor (view-window view) x y))
      (let ((segment (object-segment object x view)))
        (when segment (push segment segments)))
      (incf x (object-width object)))))

(defun objects-segments-from-right (objects view)
  (let ((x (view-width view)) (segments '()))
    (dolist (object objects segments)
      (decf x (object-width object))
      (let ((segment (object-segment object x view)))
        (when segment (push segment segments))))))

(defun draw-line (view x y objects)
  "Draw OBJECTS on line Y of VIEW from column X (the line is cleared from X)."
  (let ((canvas (view-canvas view)))
    (when (< -1 y (length (canvas-lines canvas)))
      (let ((kept (remove-if-not (lambda (segment) (< (first segment) x))
                                 (svref (canvas-lines canvas) y)))
            (new (objects-segments objects x y view)))
        (setf (svref (canvas-lines canvas) y) (append kept new)
              (canvas-background canvas) (color-string lem-if:*background-color-of-drawing-window*)
              (canvas-dirty canvas) t)))))

(defun draw-modeline (view left-objects right-objects default-attribute)
  (let ((canvas (view-canvas view)))
    (setf (canvas-modeline canvas)
          (append (list (list 0 (view-width view) ""
                              (attribute-style (lem:ensure-attribute default-attribute nil))))
                  (objects-segments left-objects 0 nil view)
                  (objects-segments-from-right right-objects view))
          (canvas-dirty canvas) t)))

(defun clear-lines-from (view y)
  (let ((lines (canvas-lines (view-canvas view))))
    (loop for i from (max 0 y) below (length lines)
          do (setf (svref lines i) nil))
    (setf (canvas-dirty (view-canvas view)) t)))

;;; ---- components

(defun px (n) (format nil "~,2Fpx" n))

(ui:defcomponent screen-line (segments top char-width char-height)
  (:div :class "lem-line" :style (format nil "top:~A;height:~A;line-height:~A;"
                                         (px top) (px char-height) (px char-height))
    (mapcar (lambda (segment)
              (destructuring-bind (x width text style) segment
                (:span :style (format nil "left:~A;width:~A;~A" (px (* x char-width))
                                      (px (* width char-width)) style)
                  text)))
            segments)))

(defun view-class (view)
  (format nil "lem-view lem-~(~A~)" (view-kind view)))

(ui:defcomponent view-border-box (view)
  ;; the border of a floating window, half a cell around it
  (let ((cw *char-width*) (ch *char-height*))
    (:div :class (format nil "lem-border lem-border-~(~A~)" (or (view-border-shape view) :normal))
          :style (format nil "left:~A;top:~A;width:~A;height:~A;"
                         (px (* (- (view-x view) 0.5) cw)) (px (* (- (view-y view) 0.5) ch))
                         (px (* (+ (view-width view) 1) cw)) (px (* (+ (view-height view) 1) ch))))))

(ui:defcomponent view-separator (view)
  ;; the line between a window and the one on its left, in the column between them
  (let ((cw *char-width*) (ch *char-height*))
    (:div :class "lem-separator"
          :style (format nil "left:~A;top:~A;width:~A;height:~A;"
                         (px (* (- (view-x view) 1) cw)) (px (* (view-y view) ch))
                         (px cw) (px (* (+ (view-height view) (if (view-modeline-p view) 1 0)) ch))))))

(ui:defcomponent view-box (view)
  (let ((cw *char-width*) (ch *char-height*))
    (:div :class (view-class view)
          :style (format nil "left:~A;top:~A;width:~A;height:~A;~@[background:~A;~]"
                         (px (* (view-x view) cw)) (px (* (view-y view) ch))
                         (px (* (view-width view) cw))
                         (px (* (+ (view-height view) (if (view-modeline-p view) 1 0)) ch))
                         (view-background view))
      (loop for segments across (view-lines view)
            for y from 0
            when segments
              collect (ui:keyed y (screen-line segments (* y ch) cw ch)))
      (when (and (view-modeline-p view) (view-modeline view))
        (ui:keyed :modeline
                  (screen-line (view-modeline view) (* (view-height view) ch) cw ch))))))

(defun view-nodes (view)
  (list (when (and (view-border view) (plusp (view-border view)))
          (ui:keyed (list :border view) (view-border-box view)))
        (when (and (eq (view-kind view) :tile) (plusp (view-x view)))
          (ui:keyed (list :separator view) (view-separator view)))
        (ui:keyed view (view-box view))))
