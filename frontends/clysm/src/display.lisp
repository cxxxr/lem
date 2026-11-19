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
(ui:defstate *accent* "#74ade8" "The colour of the cursor, the accent of the screen.")

(defvar *font-family*
  "'JetBrains Mono', 'DejaVu Sans Mono', 'Noto Sans Mono CJK JP', 'Noto Sans Mono', ui-monospace, monospace")

(defstruct canvas
  (lines #() :type simple-vector)       ; a list of segments for each line
  ;; the lines that each line takes: more than 1 for an image or larger
  ;; text, whose line covers the lines under it (Lem does not draw them,
  ;; and they are not cleared: Lem knows what they had)
  (heights #() :type simple-vector)
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
   (heights :initform #() :accessor view-heights)
   ;; a popup under the prompt is drawn as one card with it (a command
   ;; palette): :below for the prompt, :above for the popup, or NIL
   (joined :initform nil :accessor view-joined)
   ;; the part of the buffer shown, (top size) in thousandths, or NIL
   (scroll :initform nil :accessor view-scroll)
   (modeline :initform nil :accessor view-modeline)
   (background :initform nil :accessor view-background)))

(defun resize-canvas (view)
  (let* ((canvas (view-canvas view))
         (old (canvas-lines canvas))
         (new (make-array (max 0 (view-height view)) :initial-element nil))
         (heights (make-array (max 0 (view-height view)) :initial-element 1)))
    (replace new old)
    (replace heights (canvas-heights canvas))
    (setf (canvas-lines canvas) new
          (canvas-heights canvas) heights
          (canvas-dirty canvas) t)))

(defun publish-view (view)
  "Give what was drawn in VIEW to the page."
  (let ((canvas (view-canvas view)))
    (when (canvas-dirty canvas)
      (setf (canvas-dirty canvas) nil
            (view-lines view) (copy-seq (canvas-lines canvas))
            (view-heights view) (copy-seq (canvas-heights canvas))
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

;;; the names of attributes, as classes of the page (lem-a-NAME), so that
;;; lem.css can give the parts of the screen a look of their own whatever
;;; the colour theme

(defparameter *named-attributes*
  '(("LEM-CORE" "MODELINE") ("LEM-CORE" "MODELINE-INACTIVE")
    ;; (merged with the modeline's own: found by their colours)
    ("LEM-CORE" "MODELINE-POSLINE-ATTRIBUTE" :by-colors t)
    ("LEM-CORE" "INACTIVE-MODELINE-POSLINE-ATTRIBUTE" :by-colors t)
    ("LEM/FRAME-MULTIPLEXER" "FRAME-MULTIPLEXER-ACTIVE-FRAME-NAME-ATTRIBUTE")
    ("LEM/FRAME-MULTIPLEXER" "FRAME-MULTIPLEXER-FRAME-NAME-ATTRIBUTE")
    ("LEM/FRAME-MULTIPLEXER" "FRAME-MULTIPLEXER-BACKGROUND-ATTRIBUTE")
    ("LEM/POPUP-MENU" "POPUP-MENU-ATTRIBUTE")
    ("LEM/PROMPT-WINDOW" "PROMPT-ATTRIBUTE"))
  "The attributes that get a class, as (package name &key by-colors): the
packages are looked up when Lem runs, as some are loaded later.  BY-COLORS:
an attribute merged with another, with its colours, gets the class too.")

(defvar *attribute-classes* nil
  "attribute -> its class; made again when the colours change (an attribute
of a name is made again for a new theme)")

(defun attribute-colors (attribute)
  (format nil "~A|~A"
          (color-string (lem:attribute-foreground attribute))
          (color-string (lem:attribute-background attribute))))

(defun attribute-classes ()
  "Two tables: attribute -> class, and colours -> class."
  (or *attribute-classes*
      (let ((table (make-hash-table :test 'eq))
            (colors (make-hash-table :test 'equal)))
        (loop for (package name . options) in *named-attributes*
              for symbol = (and (find-package package) (find-symbol name package))
              for attribute = (and symbol (lem:ensure-attribute symbol nil))
              for class = (format nil "lem-a-~(~A~)" name)
              when attribute
                do (setf (gethash attribute table) class)
                   (when (getf options :by-colors)
                     (setf (gethash (attribute-colors attribute) colors) class)))
        (setf *attribute-classes* (cons table colors)))))

(defun attribute-class (attribute)
  (let ((attribute (and attribute (lem:ensure-attribute attribute nil))))
    (when attribute
      (destructuring-bind (table . colors) (attribute-classes)
        (or (gethash attribute table)
            (and (plusp (hash-table-count colors))
                 (gethash (attribute-colors attribute) colors)))))))

;;; ---- images
;;;
;;; An image is the value :IMAGE of an attribute (see image.lisp): Lem
;;; draws text with it as an image object, of the size :WIDTH x :HEIGHT of
;;; the attribute in pixels, or of the image's own.  It takes the cells
;;; under it, and is drawn as an element <img> over them.

(defstruct (image (:constructor %make-image (url width height)))
  "An image loaded in the page: a URL of it, and its size in pixels."
  url width height)

(defun image-object-pixels (object)
  "The width and height of the image OBJECT in pixels."
  (let ((image (lem-core/display:image-object-image object)))
    (values (or (lem-core/display:image-object-width object) (image-width image))
            (or (lem-core/display:image-object-height object) (image-height image)))))

(defun image-object-cells (object)
  "The columns and lines of the image OBJECT."
  (multiple-value-bind (width height) (image-object-pixels object)
    (values (max 1 (ceiling width *char-width*))
            (max 1 (ceiling height *char-height*)))))

;;; ---- text of other sizes
;;;
;;; Text whose attribute has :SCALE (a positive number: 2 is twice the
;;; size) is drawn that much larger: it takes (* SCALE WIDTH) columns, not
;;; rounded (Lem cuts text into objects where the cursor is, and where the
;;; kind of characters changes: rounded widths would move the text after a
;;; cut), and CEILING SCALE lines; the text of its line stands on the same
;;; baseline (the bottom of the line).

(defun attribute-scale (attribute)
  "The :SCALE of ATTRIBUTE, or NIL for text of the usual size."
  (let* ((attribute (and attribute (lem:ensure-attribute attribute nil)))
         (scale (and attribute (lem:attribute-value attribute :scale))))
    (and (realp scale) (plusp scale) (/= scale 1) (rationalize scale))))

(defun text-object-scale (object)
  (attribute-scale (lem-core/display:text-object-attribute object)))

;;; ---- drawing objects -> segments

(defun object-width (object)
  (typecase object
    (lem-core/display:image-object (values (image-object-cells object)))
    (lem-core/display:text-object
     (let ((width (lem:string-width (lem-core/display:text-object-string object)))
           (scale (text-object-scale object)))
       ;; (exact: a rational number of columns)
       (if scale (* width scale) width)))
    (t 0)))

(defun object-height (object)
  (typecase object
    (lem-core/display:image-object (nth-value 1 (image-object-cells object)))
    (lem-core/display:text-object
     (let ((scale (text-object-scale object)))
       (if scale (max 1 (ceiling scale)) 1)))
    (t 1)))

(defun cursor-object-p (object)
  (typecase object
    (lem-core/display:image-object
     (let ((attribute (lem-core/display:image-object-attribute object)))
       (and attribute (lem:cursor-attribute-p (lem:ensure-attribute attribute nil)))))
    (lem-core/display:eol-cursor-object
     (lem-core/display:eol-cursor-object-true-cursor-p object))
    (lem-core/display:text-object
     (let ((attribute (lem-core/display:text-object-attribute object)))
       (and attribute (lem:cursor-attribute-p (lem:ensure-attribute attribute nil)))))))

;;; Text of characters wider than a column (Japanese, Chinese, emoji...):
;;; the font that has them (the text's font has none) draws them in its own
;;; widths, not in the 2 columns Lem gives them, so that text after them
;;; would not be where Lem puts it (the cursor in a Japanese line was away
;;; from its character).  Each of their characters is drawn in a box of its
;;; columns (screen-line).

(defun wide-text-p (string)
  "Whether STRING has characters that do not take one column."
  (/= (lem:string-width string) (length string)))

(defun text-class (class string)
  "CLASS (of an attribute, or NIL), and lem-wide for a text of wide characters."
  (if (wide-text-p string)
      (format nil "~@[~A ~]lem-wide" class)
      class))

(defun object-segment (object x view)
  "The segment (column width text style class image scale) that draws OBJECT
at column X, or NIL: IMAGE is (url width height) of an image, in pixels, and
SCALE the size of the text (NIL: the usual one)."
  (typecase object
    (lem-core/display:void-object nil)
    (lem-core/display:image-object
     (multiple-value-bind (width height) (image-object-pixels object)
       (list x (object-width object) "" "" "lem-image"
             (list (image-url (lem-core/display:image-object-image object)) width height))))
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
                               (lem-core/display:text-object-attribute object) nil))
             (text-class (attribute-class (lem-core/display:text-object-attribute object)) string))))
    (lem-core/display:text-object
     (let ((string (lem-core/display:text-object-string object)))
       (list x (object-width object) string
             (attribute-style (lem:ensure-attribute
                               (lem-core/display:text-object-attribute object) nil))
             (text-class (attribute-class (lem-core/display:text-object-attribute object)) string)
             nil
             (text-object-scale object))))))

(defun objects-segments (objects x y view)
  "The segments of OBJECTS drawn from column X of line Y; notes where the
cursor is."
  (let ((segments '()))
    (dolist (object objects (nreverse segments))
      (when (and y (cursor-object-p object))
        ;; (a column after larger text is not a whole one)
        (lem:set-last-print-cursor (view-window view) (floor x) y))
      (let ((segment (object-segment object x view)))
        (when segment
          (push (if (cursor-object-p object) (mark-cursor segment) segment) segments)))
      (incf x (object-width object)))))

(defun mark-cursor (segment)
  "SEGMENT of the cursor, with the class lem-cursor."
  (destructuring-bind (x width text style &optional class image scale) segment
    (list x width text style (format nil "lem-cursor~@[ ~A~]" class) image scale)))

(defun objects-segments-from-right (objects view)
  (let ((x (view-width view)) (segments '()))
    (dolist (object objects segments)
      (decf x (object-width object))
      (let ((segment (object-segment object x view)))
        (when segment (push segment segments))))))

(defun draw-line (view x y objects height)
  "Draw OBJECTS on line Y of VIEW from column X (the line is cleared from X).
The line takes HEIGHT lines: it covers the HEIGHT - 1 lines under it."
  (let ((canvas (view-canvas view)))
    (when (< -1 y (length (canvas-lines canvas)))
      (setf (svref (canvas-heights canvas) y) (max 1 (or height 1))))
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
                              (attribute-style (lem:ensure-attribute default-attribute nil))
                              (attribute-class default-attribute)))
                  (objects-segments left-objects 0 nil view)
                  (objects-segments-from-right right-objects view))
          (canvas-dirty canvas) t)))

(defun clear-lines-from (view y)
  (let ((lines (canvas-lines (view-canvas view)))
        (heights (canvas-heights (view-canvas view))))
    (loop for i from (max 0 y) below (length lines)
          do (setf (svref lines i) nil
                   (svref heights i) 1))
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

;;; ---- the prompt and its completion as one card

(defun bordered-p (view)
  (and (view-border view) (plusp (view-border view))))

(defun update-joins (views)
  "Note which popup lies right under the prompt, at its column: the two are
drawn as one card."
  (let* ((window (lem:frame-prompt-window (lem:current-frame)))
         (prompt (find-if (lambda (view) (and (eq (view-window view) window)
                                              (eq (view-role view) :minibuffer)))
                          views))
         (below (and prompt (bordered-p prompt)
                     (find-if (lambda (view)
                                (and (eq (view-role view) :popup)
                                     (bordered-p view)
                                     (= (view-x view) (view-x prompt))
                                     (<= 0 (- (view-y view) (+ (view-y prompt) (view-height prompt))) 2)))
                              views))))
    (dolist (view views)
      (let ((joined (cond ((null below) nil)
                          ((eq view prompt) :below)
                          ((eq view below) :above))))
        (unless (eq joined (view-joined view))
          (setf (view-joined view) joined))))))

;;; ---- the scroll bar of a window

(defun window-scroll (window)
  "(top size) of the part of WINDOW's buffer it shows, in thousandths of the
buffer, or NIL if all of it is shown."
  (let ((lines (lem:buffer-nlines (lem:window-buffer window)))
        (height (lem:window-height window)))
    (when (> lines height)
      (let ((top (1- (lem:line-number-at-point (lem:window-view-point window)))))
        (list (floor (* 1000 top) lines)
              (max 1 (floor (* 1000 height) lines)))))))

(defun update-scroll (view)
  (when (eq (view-role view) :window)
    (let ((scroll (window-scroll (view-window view))))
      (unless (equal scroll (view-scroll view))
        (setf (view-scroll view) scroll)))))

;;; ---- components

(defun px (n)
  "N pixels, for CSS (N may be a rational number)."
  (format nil "~,2Fpx" (float n 1d0)))

(defun cell-box (x y width height)
  "The CSS of a box of WIDTH x HEIGHT cells at column X, line Y."
  (let ((cw *char-width*) (ch *char-height*))
    (format nil "left:~A;top:~A;width:~A;height:~A;"
            (px (* x cw)) (px (* y ch)) (px (* width cw)) (px (* height ch)))))

(defun segment-pixel-height (segment char-height)
  "The height of what SEGMENT draws, in pixels: an image's, or that of its
text; NIL for a background (the rest of a line...), which takes the line."
  (destructuring-bind (x width text style &optional class image scale) segment
    (declare (ignore x width style class))
    (cond (image (third image))
          ((and (equal text "") (null scale)) nil)
          (t (* (or scale 1) char-height)))))

(defun line-pixel-height (segments char-height)
  "The height of a line of SEGMENTS: the lines that its tallest segment takes."
  (* char-height
     (loop for segment in segments
           for height = (segment-pixel-height segment char-height)
           maximize (if height (max 1 (ceiling (- height 0.01) char-height)) 1))))

(ui:defcomponent screen-line (segments top char-width char-height)
  ;; a line taller than a cell (an image, larger text): its segments stand on
  ;; its bottom
  (let ((line-height (line-pixel-height segments char-height)))
    (:div :class "lem-line" :style (format nil "top:~A;height:~A;line-height:~A;"
                                           (px top) (px char-height) (px char-height))
      (mapcar (lambda (segment)
                (destructuring-bind (x width text style &optional class image scale) segment
                  (let ((height (segment-pixel-height segment char-height)))
                    (:span :class class
                           :style (format nil "left:~A;width:~A;~@[top:~A;~]~@[height:~A;~]~
                                               ~@[line-height:~A;~]~@[font-size:~,3Fem;~]~A"
                                          (px (* x char-width)) (px (* width char-width))
                                          (cond ((null height) (and (> line-height char-height) (px 0)))
                                                ((/= height line-height) (px (- line-height height))))
                                          (cond ((null height)
                                                 (and (> line-height char-height) (px line-height)))
                                                ((/= height char-height) (px height)))
                                          (and scale (px height))
                                          (and scale (float scale 1d0))
                                          style)
                      (cond (image
                             (destructuring-bind (url width height) image
                               (:img :src url :draggable "false"
                                     :style (format nil "width:~A;height:~A;" (px width) (px height)))))
                            ((and class (search "lem-wide" class))
                             ;; each character in a box of its columns
                             (map 'list (lambda (char)
                                          (:span :style (format nil "width:~A;"
                                                                (px (* (lem:string-width (string char))
                                                                       char-width (or scale 1))))
                                            (string char)))
                                  text))
                            (t text))))))
              segments))))

(defun view-line-nodes (view)
  "The lines of VIEW, but those that a taller line above covers."
  (let ((cw *char-width*) (ch *char-height*)
        (heights (view-heights view))
        (covered-until 0))
    (loop for segments across (view-lines view)
          for y from 0
          for height = (if (< y (length heights)) (svref heights y) 1)
          when (and segments (<= covered-until y))
            collect (ui:keyed y (screen-line segments (* y ch) cw ch))
          when (<= covered-until y)
            do (setf covered-until (+ y height)))))

(defun background-style (view)
  (if (view-background view) (format nil "background:~A;" (view-background view)) ""))

(ui:defcomponent buffer-view (view)
  ;; the text of a window of the frame, without its modeline
  (:div :class "lem-view lem-window"
        :style (format nil "~A~A"
                       (cell-box (view-x view) (view-y view) (view-width view) (view-height view))
                       (background-style view))
    (view-line-nodes view)
    (when (view-scroll view)
      (ui:keyed :scrollbar
                (:div :class "lem-scrollbar"
                      :style (destructuring-bind (top size) (view-scroll view)
                               (format nil "top:~,1F%;height:~,1F%;" (/ top 10) (/ size 10))))))))

(ui:defcomponent modeline-bar (view)
  ;; the modeline of a window, on the line under it (lem.css tells the
  ;; modeline of the current window by its attribute, lem-a-modeline)
  (:div :class "lem-modeline"
        :style (cell-box (view-x view) (+ (view-y view) (view-height view)) (view-width view) 1)
    (when (view-modeline view)
      (screen-line (view-modeline view) 0 *char-width* *char-height*))))

(ui:defcomponent view-separator (view)
  ;; the line between a window and the one on its left, in the column between them
  (:div :class "lem-separator"
        :style (cell-box (- (view-x view) 1) (view-y view)
                         1 (+ (view-height view) (if (view-modeline-p view) 1 0)))))

(ui:defcomponent tabbar-view (view)
  ;; a header window: the tabs of frame-multiplexer, ...
  (:div :class "lem-view lem-tabbar"
        :style (cell-box (view-x view) (view-y view) (view-width view) (view-height view))
    (view-line-nodes view)))

(ui:defcomponent floating-view (view)
  ;; a popup or the minibuffer: a box with its border, half a cell around
  ;; the window, and the window in it
  (let* ((border (and (view-border view) (plusp (view-border view))))
         (margin (if border 0.5 0)))
    (:div :class (format nil "lem-~(~A~)~:[~; lem-bordered~]~@[ lem-join-~(~A~)~]"
                         (view-role view) border (view-joined view))
          :style (cell-box (- (view-x view) margin) (- (view-y view) margin)
                           (+ (view-width view) (* 2 margin)) (+ (view-height view) (* 2 margin)))
      (when border
        (:div :class (format nil "lem-border lem-border-~(~A~)"
                             (or (view-border-shape view) :normal))))
      (:div :class "lem-view"
            :style (format nil "~A~A"
                           (cell-box margin margin (view-width view) (view-height view))
                           ;; (a card has the colour of lem.css)
                           (if border "" (background-style view)))
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
