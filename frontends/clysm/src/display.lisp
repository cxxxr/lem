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
   ;; which component shows it: :window (a window of the frame, with its
   ;; modeline apart), :tabbar (a header window), :minibuffer (the prompt
   ;; or the message of the frame) or :popup (another floating window)
   (role :initarg :role :accessor view-role)
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

;;; ---- roles

(defun window-role (window)
  "The component that shows WINDOW (see the slot ROLE of VIEW)."
  (cond ((lem:header-window-p window) :tabbar)
        ((or (lem:floating-window-p window) (lem:attached-window-p window))
         (let ((frame (lem:current-frame)))
           (if (or (eq window (lem:frame-prompt-window frame))
                   (eq window (lem:frame-message-window frame)))
               :minibuffer
               :popup)))
        (t :window)))

(defun update-role (view)
  "Note again which component shows VIEW: a floating window becomes the
prompt (or the message) of the frame only after it is made."
  (let ((role (window-role (view-window view))))
    (unless (eq role (view-role view))
      (setf (view-role view) role))))

;;; ---- components

(defun px (n) (format nil "~,2Fpx" n))

(defun cell-box (x y width height)
  "The CSS of a box of WIDTH x HEIGHT cells at column X, line Y."
  (let ((cw *char-width*) (ch *char-height*))
    (format nil "left:~A;top:~A;width:~A;height:~A;"
            (px (* x cw)) (px (* y ch)) (px (* width cw)) (px (* height ch)))))

(ui:defcomponent screen-line (segments top char-width char-height)
  (:div :class "lem-line" :style (format nil "top:~A;height:~A;line-height:~A;"
                                         (px top) (px char-height) (px char-height))
    (mapcar (lambda (segment)
              (destructuring-bind (x width text style &optional class) segment
                (:span :class class
                       :style (format nil "left:~A;width:~A;~A" (px (* x char-width))
                                      (px (* width char-width)) style)
                  text)))
            segments)))

(defun view-line-nodes (view)
  (let ((cw *char-width*) (ch *char-height*))
    (loop for segments across (view-lines view)
          for y from 0
          when segments
            collect (ui:keyed y (screen-line segments (* y ch) cw ch)))))

(defun background-style (view)
  (if (view-background view) (format nil "background:~A;" (view-background view)) ""))

(ui:defcomponent buffer-view (view)
  ;; the text of a window of the frame, without its modeline
  (:div :class "lem-view lem-window"
        :style (format nil "~A~A"
                       (cell-box (view-x view) (view-y view) (view-width view) (view-height view))
                       (background-style view))
    (view-line-nodes view)))

(ui:defcomponent modeline-bar (view)
  ;; the modeline of a window, on the line under it
  (:div :class (if (eq (view-window view) (lem:current-window))
                   "lem-modeline lem-modeline-active"
                   "lem-modeline")
        :style (cell-box (view-x view) (+ (view-y view) (view-height view)) (view-width view) 1)
    (when (view-modeline view)
      (screen-line (view-modeline view) 0 *char-width* *char-height*))))

(ui:defcomponent view-separator (view)
  ;; the line between a window and the one on its left, in the column between them
  (:div :class "lem-separator"
        :style (cell-box (- (view-x view) 1) (view-y view)
                         1 (+ (view-height view) (if (view-modeline-p view) 1 0)))))

(defun blank-segment-p (segment)
  (every (lambda (char) (char= char #\Space)) (third segment)))

(defun segment-background (segment)
  "The background colour in the style of SEGMENT, or NIL."
  (let* ((style (fourth segment))
         (start (search "background:" style)))
    (when start
      (let ((start (+ start (length "background:"))))
        (subseq style start (position #\; style :start start))))))

(defun tabbar-line (segments)
  "SEGMENTS of a line of the tab bar, the blank ones (the bar between the
tabs) marked as the class lem-fill, so that the tabs alone are drawn as tabs."
  (mapcar (lambda (segment)
            (if (blank-segment-p segment)
                (append segment (list "lem-fill"))
                segment))
          segments))

(ui:defcomponent tabbar-view (view)
  ;; a header window: the tabs of frame-multiplexer, ...; the bar has the
  ;; colour of its blank segments
  (let* ((cw *char-width*) (ch *char-height*)
         (first-line (and (plusp (length (view-lines view))) (svref (view-lines view) 0)))
         (fill (find-if #'blank-segment-p first-line))
         (bar (or (and fill (segment-background fill)) (view-background view))))
    (:div :class "lem-view lem-tabbar"
          :style (format nil "~A~@[background:~A;~]"
                         (cell-box (view-x view) (view-y view) (view-width view) (view-height view))
                         bar)
      (loop for segments across (view-lines view)
            for y from 0
            when segments
              collect (ui:keyed y (screen-line (tabbar-line segments) (* y ch) cw ch))))))

(ui:defcomponent floating-view (view)
  ;; a popup or the minibuffer: a box with its border, half a cell around
  ;; the window, and the window in it
  (let* ((border (and (view-border view) (plusp (view-border view))))
         (margin (if border 0.5 0)))
    (:div :class (format nil "lem-~(~A~)~:[~; lem-bordered~]" (view-role view) border)
          :style (cell-box (- (view-x view) margin) (- (view-y view) margin)
                           (+ (view-width view) (* 2 margin)) (+ (view-height view) (* 2 margin)))
      (when border
        (:div :class (format nil "lem-border lem-border-~(~A~)"
                             (or (view-border-shape view) :normal))
              :style (background-style view)))
      (:div :class "lem-view"
            :style (format nil "~A~A"
                           (cell-box margin margin (view-width view) (view-height view))
                           (background-style view))
        (view-line-nodes view)))))

(defun views-of (roles)
  (remove-if-not (lambda (view) (member (view-role view) roles)) *views*))

(ui:defcomponent window-layer ()
  (:div :class "lem-layer lem-windows"
    (mapcan (lambda (view)
              (list (when (plusp (view-x view))
                      (ui:keyed (list :separator view) (view-separator view)))
                    (ui:keyed view (buffer-view view))
                    (when (view-modeline-p view)
                      (ui:keyed (list :modeline view) (modeline-bar view)))))
            (views-of '(:window)))))

(ui:defcomponent tabbar-layer ()
  (:div :class "lem-layer lem-tabbars"
    (mapcar (lambda (view) (ui:keyed view (tabbar-view view)))
            (views-of '(:tabbar)))))

(ui:defcomponent minibuffer-layer ()
  (:div :class "lem-layer lem-minibuffers"
    (mapcar (lambda (view) (ui:keyed view (floating-view view)))
            (views-of '(:minibuffer)))))

(ui:defcomponent popup-layer ()
  (:div :class "lem-layer lem-popups"
    (mapcar (lambda (view) (ui:keyed view (floating-view view)))
            (views-of '(:popup)))))
