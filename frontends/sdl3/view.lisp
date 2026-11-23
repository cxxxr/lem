(defpackage :lem-sdl3/view
  (:use :cl)
  (:local-nicknames (:display :lem-sdl3/display))
  (:export
   :view
   :set-cursor-position
   :last-cursor-x
   :last-cursor-y
   :create-view
   :delete-view
   :render-clear
   :resize
   :resize-pixels
   :move-position
   :draw-window-border
   :render-border-using-view
   :render-view-texture-to-display
   :refresh-all-view-textures
   :view-window
   :view-x
   :view-y
   :view-width
   :view-height
   :view-pixel-x
   :view-pixel-y
   :view-pixel-width
   :view-pixel-height
   :view-pixel-rect
   :view-layout-size
   :view-render-height
   :view-texture-size
   :view-body-height
   :view-scroll-offset
   :view-scroll-anchor
   :view-scroll-remaining
   :view-scroll-samples
   :view-momentum-velocity
   :scroll-velocity
   :next-momentum-velocity
   :add-scroll-delta
   :next-scroll-step
   :view-use-modeline
   :view-texture
   :sync-texture))
(in-package :lem-sdl3/view)

(defclass view ()
  ((window
    :initarg :window
    :reader view-window)
   (x
    :initarg :x
    :accessor view-x)
   (y
    :initarg :y
    :accessor view-y)
   (width
    :initarg :width
    :accessor view-width)
   (height
    :initarg :height
    :accessor view-height)
   (pixel-x
    :initarg :pixel-x
    :initform nil
    :accessor view-pixel-x
    :documentation "The view's left edge in pixels, or NIL to place it at its
cell column.")
   (pixel-y
    :initarg :pixel-y
    :initform nil
    :accessor view-pixel-y
    :documentation "The view's top edge in pixels, or NIL to place it at its
cell row.")
   (pixel-width
    :initarg :pixel-width
    :initform nil
    :accessor view-pixel-width
    :documentation "The view's width in pixels, or NIL for its cell width.")
   (pixel-height
    :initarg :pixel-height
    :initform nil
    :accessor view-pixel-height
    :documentation "The view's height in pixels, modeline included, or NIL for
its cell height.")
   (use-modeline
    :initarg :use-modeline
    :reader view-use-modeline)
   (texture
    :initarg :texture
    :initform nil
    :accessor view-texture)
   (texture-size
    :initform nil
    :accessor view-texture-pixel-size
    :documentation "The (WIDTH . HEIGHT) TEXTURE was created at.")
   (last-cursor-x
    :initform nil
    :accessor view-last-cursor-x)
   (last-cursor-y
    :initform nil
    :accessor view-last-cursor-y)
   (scroll-offset
    :initform 0
    :accessor view-scroll-offset
    :documentation "How many pixels, less than a line, the body is scrolled
past the line lem-core has at the top (see add-scroll-delta).")
   (scroll-remaining
    :initform 0
    :accessor view-scroll-remaining
    :documentation "Pixels a scroll animation has still to move the body,
positive toward the end of the buffer.")
   (scroll-samples
    :initform '()
    :accessor view-scroll-samples
    :documentation "Recent trackpad scrolls of the body as (MILLISECONDS .
PIXELS), newest first, for its velocity (see scroll-velocity).")
   (momentum-velocity
    :initform 0
    :accessor view-momentum-velocity
    :documentation "Pixels per millisecond the body glides after a trackpad
flick, when the platform does not supply momentum itself.")
   (scroll-anchor
    :initform nil
    :accessor view-scroll-anchor
    :documentation "Where the window's view point was when SCROLL-OFFSET was
last set, as (LINE-NUMBER . CHARPOS), so the offset can be dropped when
something else scrolls the window."))
  (:documentation "The SDL texture a Lem window draws into, and where it sits
on the display: in cells, and optionally in pixels (see view-pixel-rect)."))

(defun view-layout-size (view display)
  "Return the WIDTH and HEIGHT in pixels, modeline included, that lem-core
lays VIEW's lines out in: VIEW's pixel size override if it has one, and
otherwise its cell size times DISPLAY's cell size."
  (values (or (view-pixel-width view)
              (* (view-width view) (display:display-char-width display)))
          (or (view-pixel-height view)
              (* (view-height view) (display:display-char-height display)))))

(defun pixel-overrides-p (view)
  (or (view-pixel-x view) (view-pixel-y view)
      (view-pixel-width view) (view-pixel-height view)))

(defun edge-extra (view display)
  "Return the pixels VIEW extends past its cells to reach DISPLAY's right
and bottom edges, as values X and Y.  The window's pixel size is rarely a
whole number of cells; a view placed by cells whose cells reach the last
column or row takes the remainder, so nothing is left unpainted."
  (if (pixel-overrides-p view)
      (values 0 0)
      (let ((char-width (display:display-char-width display))
            (char-height (display:display-char-height display))
            (display-width (display:display-width display))
            (display-height (display:display-height display)))
        (values (if (<= (floor display-width char-width) (+ (view-x view) (view-width view)))
                    (mod display-width char-width)
                    0)
                (if (<= (floor display-height char-height) (+ (view-y view) (view-height view)))
                    (mod display-height char-height)
                    0)))))

(defun view-pixel-rect (view display)
  "Return VIEW's rectangle on DISPLAY as values X, Y, WIDTH and HEIGHT in
pixels, modeline included.  The position is VIEW's pixel override if it has
one, and otherwise its cell position times DISPLAY's cell size.  The size is
view-layout-size, plus the remainder past the last cell when VIEW reaches
the right or bottom edge (see edge-extra)."
  (multiple-value-bind (width height) (view-layout-size view display)
    (multiple-value-bind (extra-x extra-y) (edge-extra view display)
      (values (or (view-pixel-x view)
                  (* (view-x view) (display:display-char-width display)))
              (or (view-pixel-y view)
                  (* (view-y view) (display:display-char-height display)))
              (+ width extra-x)
              (+ height extra-y)))))

(defun overscan-rows (view)
  "How many rows past its last one VIEW's texture holds.  A view with a
modeline draws the modeline at its bottom edge and its body above it, so the
body can show lines below its last whole row: in the pixels past its last
cell (see edge-extra) and while it is scrolled by part of a line.  Each of
those is less than a row, so two rows cover both."
  (if (view-use-modeline view) 2 0))

(defun view-render-height (view display)
  "Return the height in pixels lem-core draws VIEW in: its layout height plus
the overscan row (see overscan-rows).  The modeline goes at the bottom of it,
below the overscan row."
  (+ (nth-value 1 (view-layout-size view display))
     (* (overscan-rows view) (display:display-char-height display))))

(defun view-texture-size (view display)
  "Return the WIDTH and HEIGHT in pixels of VIEW's texture on DISPLAY, at
least one pixel each way: its pixel rectangle's width, and the height
lem-core draws it in for a view with overscan rows (see view-render-height),
or its rectangle's height."
  (multiple-value-bind (x y width height) (view-pixel-rect view display)
    (declare (ignore x y))
    (values (max width 1)
            (max (if (zerop (overscan-rows view))
                     height
                     (view-render-height view display))
                 1))))

(defun replace-texture (view display)
  "Give VIEW a new texture of its texture size on DISPLAY, destroying the
old one."
  (multiple-value-bind (width height) (view-texture-size view display)
    (when (view-texture view)
      (lem-sdl3/ffi:destroy-texture (view-texture view)))
    (setf (view-texture view) (display:create-view-texture display width height)
          (view-texture-pixel-size view) (cons width height))))

(defun sync-texture (view display)
  "Recreate VIEW's texture if it no longer has VIEW's texture size on
DISPLAY, and return true if it did.  The size can change without lem-core
resizing the view: the remainder past the last cell (see edge-extra)
changes with the display's size and cell size."
  (unless (equal (view-texture-pixel-size view)
                 (multiple-value-call #'cons (view-texture-size view display)))
    (replace-texture view display)
    t))

(defun scroll-velocity (samples now window)
  "Return the velocity, in pixels per millisecond, of SAMPLES, scrolls as
(MILLISECONDS . PIXELS) newest first, over the WINDOW milliseconds before
NOW; 0 without at least two samples in it."
  (let ((recent (remove-if (lambda (sample) (< (car sample) (- now window))) samples)))
    ;; The oldest sample marks when the span starts; its own pixels moved
    ;; before it.
    (if (< (length recent) 2)
        0
        (/ (reduce #'+ (butlast recent) :key #'cdr)
           (max 8 (- (car (first recent)) (car (car (last recent)))))))))

(defun next-momentum-velocity (velocity friction minimum)
  "Return VELOCITY after one frame of FRICTION (the share it keeps), or 0
once that is below MINIMUM pixels per millisecond."
  (let ((next (* velocity friction)))
    (if (< (abs next) minimum) 0 next)))

(defun next-scroll-step (remaining easing)
  "Return the whole pixels a scroll animation with REMAINING pixels to go
moves in its next frame: EASING of what remains, so it slows as it settles,
but at least a pixel and at most what remains."
  (if (zerop remaining)
      0
      (* (signum remaining)
         (round (min (abs remaining)
                     (max 1 (* easing (abs remaining))))))))

(defun add-scroll-delta (view pixels line-height)
  "Scroll VIEW's body by PIXELS (positive moves the text up) and return how
many whole lines lem-core must scroll the window: the offset keeps the rest,
from 0 up to LINE-HEIGHT."
  (multiple-value-bind (lines offset)
      (floor (+ (view-scroll-offset view) pixels) line-height)
    (setf (view-scroll-offset view) offset)
    lines))

(defmethod set-cursor-position ((view view) x y)
  (setf (view-last-cursor-x view) x
        (view-last-cursor-y view) y))

(defmethod last-cursor-x ((view view) display)
  (or (view-last-cursor-x view)
      ;; fallback to v1
      (* (lem:last-print-cursor-x (view-window view))
         (display:display-char-width display))))

(defmethod last-cursor-y ((view view) display)
  (or (view-last-cursor-y view)
      ;; fallback to v1
      (* (lem:last-print-cursor-y (view-window view))
         (display:display-char-height display))))

(defun create-view (display window x y width height use-modeline
                    &key pixel-x pixel-y pixel-width pixel-height)
  "Return a view for WINDOW at cell X, Y of WIDTH by HEIGHT cells, plus a
modeline row if USE-MODELINE.  The PIXEL- arguments, when not NIL, place or
size it in pixels instead (see view-pixel-rect)."
  (when use-modeline (incf height))
  (let ((view (make-instance 'view
                             :window window
                             :x x
                             :y y
                             :width width
                             :height height
                             :pixel-x pixel-x
                             :pixel-y pixel-y
                             :pixel-width pixel-width
                             :pixel-height pixel-height
                             :use-modeline use-modeline)))
    (replace-texture view display)
    view))

(defmethod delete-view ((view view))
  (when (view-texture view)
    (lem-sdl3/ffi:destroy-texture (view-texture view))
    (setf (view-texture view) nil)))

(defmethod render-clear ((view view) display)
  (display::with-display-render-target (display (view-texture view))
    (display:set-render-color display (display:display-background-color display))
    (lem-sdl3/ffi:render-clear (display:display-renderer display))))

(defmethod resize ((view view) display width height)
  (when (view-use-modeline view) (incf height))
  (setf (view-width view) width
        (view-height view) height)
  (replace-texture view display))

(defun resize-pixels (view display pixel-width pixel-height)
  "Replace VIEW's pixel size overrides with PIXEL-WIDTH and PIXEL-HEIGHT
(NIL keeps the current one) and recreate its texture.  Its cell size is
unchanged."
  (when pixel-width (setf (view-pixel-width view) pixel-width))
  (when pixel-height (setf (view-pixel-height view) pixel-height))
  (replace-texture view display))

(defmethod move-position ((view view) x y &optional pixel-x pixel-y)
  "Move VIEW to cell X, Y.  PIXEL-X and PIXEL-Y, when not NIL, replace its
pixel position overrides."
  (setf (view-x view) x
        (view-y view) y)
  (when pixel-x (setf (view-pixel-x view) pixel-x))
  (when pixel-y (setf (view-pixel-y view) pixel-y)))

(defmethod draw-window-border (view display (window lem:floating-window))
  (when (and (lem:window-border window)
             (< 0 (lem:window-border window)))
    (lem-sdl3/ffi:set-render-target (display:display-renderer display) (display:display-texture display))
    (multiple-value-bind (x y width height) (view-pixel-rect view display)
      (display:render-border-by-pixels display x y width height
                                       :border-type (lem:floating-window-border-shape window)))))

(defmethod draw-window-border (view display (window lem:window))
  (when (< 0 (lem:window-x window))
    (lem-sdl3/ffi:set-render-target (display:display-renderer display) (display:display-texture display))
    (multiple-value-bind (x y width height) (view-pixel-rect view display)
      (declare (ignore width))
      (display:render-margin-line display x y height))))

(defmethod render-border-using-view ((view view) display)
  (draw-window-border view display (view-window view)))

(defun copy-texture-rect (display texture source-y x y width height)
  "Copy the WIDTH by HEIGHT rows of TEXTURE starting at row SOURCE-Y to X, Y
on DISPLAY's current render target."
  (lem-sdl3/ffi:with-frect (source-rect 0 source-y width height)
    (lem-sdl3/ffi:with-frect (dest-rect x y width height)
      (lem-sdl3/ffi:render-texture (display:display-renderer display)
                                   texture
                                   source-rect
                                   dest-rect))))

(defun view-body-height (view display)
  "Return the height in pixels VIEW's body takes on DISPLAY: everything above
its modeline, which sits at the bottom edge of its rectangle."
  (- (nth-value 3 (view-pixel-rect view display))
     (display:display-char-height display)))

(defun render-view-texture-to-display (view display)
  "Composite VIEW's texture onto DISPLAY's texture.  A view with overscan
rows is copied in two parts: the body, shifted up by the scroll offset, and
the modeline at the bottom edge, so it follows the edge pixel by pixel while
the body's rows fill the space above it."
  (lem-sdl3/ffi:set-render-target (display:display-renderer display) (display:display-texture display))
  (multiple-value-bind (x y width height) (view-pixel-rect view display)
    (if (zerop (overscan-rows view))
        (copy-texture-rect display (view-texture view) 0 x y width height)
        (let* ((char-height (display:display-char-height display))
               (body-height (view-body-height view display))
               (offset (if (< (view-scroll-offset view) char-height)
                           (view-scroll-offset view)
                           0)))
          (copy-texture-rect display (view-texture view) offset x y width body-height)
          (copy-texture-rect display (view-texture view)
                             (- (view-render-height view display) char-height)
                             x (+ y body-height) width char-height)))))

(defun refresh-all-view-textures (display)
  "Recreate the SDL texture backing every live view at the current
display char dimensions.  Called via `display:*post-display-change-hooks*'
whenever a DPI-driven font swap changes `display:display-char-width' /
`display:display-char-height' without changing the col/row count of any
window (the canonical Retina \u2194 standard-DPI move case).  In that
situation `lem-if:set-view-size' is a no-op \u2014 `window-set-size'
short-circuits when width/height are unchanged \u2014 so view textures
would otherwise stay sized at the *old* char metrics and leak through
as scaled or clipped artifacts (the buffer occupying only the
upper-left quarter of the window)."
  (dolist (window (append (lem:window-list)
                          (lem:frame-floating-windows (lem:current-frame))))
    (let ((view (lem:window-view window)))
      (when (typep view 'view)
        (replace-texture view display)))))

(display:add-post-display-change-hook 'refresh-all-view-textures)
