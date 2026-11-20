(defpackage :lem-sdl3/tests/drawing
  (:use :cl :rove)
  (:local-nicknames (:drawing :lem-sdl3/drawing)
                    (:display :lem-sdl3/display)))
(in-package :lem-sdl3/tests/drawing)

;;; Regression tests for `lem-sdl3/drawing::object-height' on the base
;;; `text-object' class.
;;;
;;; Earlier the method returned the SDL surface height of `(get-surface ...)',
;;; i.e. the per-string SDL_ttf surface height.  SDL_ttf can return slightly
;;; different surface heights for different strings (notably ones containing
;;; descenders like `p'/`g'/`y' vs ones without), which leaked through to the
;;; background rectangle drawn by `draw-text-glyph-surface' during the `:bg'
;;; phase -- producing an uneven, stepped highlight outline at attribute
;;; boundaries (the "padding around the problem letters" artefact visible on
;;; the dashboard's highlighted row).
;;;
;;; The fix pins the text-object's reported height to the stable cell-row
;;; height (`display-char-height') so adjacent attribute runs paint
;;; rectangles with identical vertical extent.  These tests lock that
;;; behaviour in so a future change to `object-height' cannot silently
;;; regress the visual artefact.
;;;
;;; The tests use a stub `display' that only exposes `display-char-height',
;;; so they run without an SDL window / renderer.  `object-height' on a
;;; text-object does not read the SDL surface -- it goes straight through
;;; `display-char-height' -- which is exactly the invariant we want to
;;; assert.

(defclass stub-display ()
  ((char-height :initarg :char-height :reader display::display-char-height)))

(defclass stub-grid-display (stub-display)
  ((char-width :initarg :char-width :reader display::display-char-width)
   (text-engine :initform nil :reader display:display-text-engine)
   (text-shaper :initform nil :reader display:display-text-shaper))
  (:documentation "A stub display with a cell width and no text engine or
shaper, for drawing whole runs letter by letter."))

(defclass stub-scaled-display (stub-grid-display)
  ((scale :initarg :scale :reader display::display-scale))
  (:documentation "A stub display with a display scale, for underlines."))

(defun make-stub-display (&key (char-height 17))
  (make-instance 'stub-display :char-height char-height))

(defun make-text-object (string)
  (make-instance 'lem-core/display:text-object
                 :string string
                 :attribute nil
                 :type :latin))

(deftest text-object-height-is-the-stable-cell-height
  (let ((display (make-stub-display :char-height 17)))
    (testing "object-height matches display-char-height for a plain ASCII string"
      (ok (= (drawing::object-height (make-text-object "hello") display)
             17)))
    (testing "object-height matches display-char-height for a string with descenders"
      (ok (= (drawing::object-height (make-text-object "pygmy") display)
             17)))
    (testing "object-height matches display-char-height for a string with ascenders only"
      (ok (= (drawing::object-height (make-text-object "ABCDEF") display)
             17)))))

(deftest text-object-height-is-independent-of-string-content
  ;; The core invariant: two text-objects with very different glyph shapes
  ;; (one with descenders, one without) must report the same height, so the
  ;; row-wide two-pass renderer paints uniform background rectangles across
  ;; attribute boundaries on a highlighted line.
  (let ((display (make-stub-display :char-height 17)))
    (ok (= (drawing::object-height (make-text-object "py")     display)
           (drawing::object-height (make-text-object "ABCDEF") display)
           (drawing::object-height (make-text-object "Tg")     display)
           (drawing::object-height (make-text-object "   ")    display)))))

(deftest text-object-height-tracks-display-char-height-changes
  ;; After a DPI swap `display-char-height' is re-derived from the freshly
  ;; opened font; the text-object height must follow it -- if it ever pinned
  ;; itself to the first display's value the highlight band would no longer
  ;; cover the new row height after a monitor change.
  (let ((small (make-stub-display :char-height 14))
        (large (make-stub-display :char-height 28))
        (obj   (make-text-object "py")))
    (ok (= (drawing::object-height obj small) 14))
    (ok (= (drawing::object-height obj large) 28))))

;;; The :bg phase of a multi-character run paints one rectangle and one
;;; underline across the run instead of one per letter (a per-letter loop
;;; was ~1600 fills and ~0.7 ms of throwaway letter objects per full
;;; redraw).  Cursor runs keep the per-cell path, which records the cursor
;;; position and draws bar and underline cursors per cell.

(defun call-recording (names function)
  "Call FUNCTION with the functions NAMES encapsulated so they record their
arguments instead of drawing; return the recorded (name . args) calls in
order."
  (let ((calls '()))
    (dolist (name names)
      (let ((name name))
        (sb-int:encapsulate name 'record
                            (lambda (original &rest args)
                              (declare (ignore original))
                              (push (cons name args) calls)))))
    (unwind-protect (funcall function)
      (dolist (name names)
        (sb-int:unencapsulate name 'record)))
    (nreverse calls)))

(defun bg-phase-calls (string attribute)
  (let ((display (make-instance 'stub-grid-display :char-height 17 :char-width 9))
        (object (make-instance 'lem-core/display:text-object
                               :string string :attribute attribute :type :latin)))
    (call-recording '(drawing::draw-rect
                      drawing::draw-underline
                      drawing::draw-text-glyph-surface)
                    (lambda ()
                      (drawing::draw-text-object-phase object 10 50 display nil :bg)))))

(deftest run-background-is-one-fill
  (let* ((attribute (lem:make-attribute :background "#112233" :underline t))
         (calls (bg-phase-calls "hello" attribute)))
    (ok (equal (mapcar #'first calls)
               '(drawing::draw-rect drawing::draw-underline))
        "one fill and one underline for the run")
    (destructuring-bind (display x y width height color) (rest (first calls))
      (declare (ignore display color))
      (ok (equal (list x y width height) '(10 33 45 17))
          "the fill covers every cell of the run"))
    (destructuring-bind (display x y width height underline-attribute) (rest (second calls))
      (declare (ignore display))
      (ok (equal (list x y width height) '(10 33 45 17)))
      (ok (eq underline-attribute attribute)))))

(deftest cursor-run-background-stays-per-cell
  (let ((attribute (lem:make-attribute :background "#112233")))
    (lem-core:set-cursor-attribute attribute)
    (ok (equal (mapcar #'first (bg-phase-calls "ab" attribute))
               '(drawing::draw-text-glyph-surface drawing::draw-text-glyph-surface)))))

;;; Underlines are one point thick at the display's scale, drawn inside the
;;; bottom of the cell and exactly as wide as the run.

(defun underline-rects (scale)
  (let ((display (make-instance 'stub-scaled-display
                                :char-height 17 :char-width 9 :scale scale)))
    (mapcar #'rest
            (call-recording '(drawing::draw-rect)
                            (lambda ()
                              (drawing::draw-underline display 10 33 45 17
                                                       (lem:make-attribute :underline t :foreground "#aabbcc")))))))

(deftest underline-scales-with-the-display
  (flet ((box (rect) (subseq rect 1 5)))
    (ok (equal (mapcar #'box (underline-rects '(1 1))) '((10 49 45 1)))
        "one pixel at 1x")
    (ok (equal (mapcar #'box (underline-rects '(2 2))) '((10 48 45 2)))
        "two pixels at 2x")
    (ok (equal (mapcar #'box (underline-rects '(3/2 3/2))) '((10 48 45 2)))
        "rounded at fractional scales"))
  (ok (null (call-recording '(drawing::draw-rect)
                            (lambda ()
                              (drawing::draw-underline
                               (make-instance 'stub-scaled-display
                                              :char-height 17 :char-width 9 :scale '(2 2))
                               0 0 9 17 (lem:make-attribute :background "#112233")))))
      "no underline without the attribute"))

;;; The :glyph phase of a run that isn't shaped draws each letter in its own
;;; cell.  Letters drawn as rectangles or by the text engine need no letter
;;; object; the rest (here, with no text engine, the plain letters) are drawn
;;; from per-letter surfaces.

(defun glyph-phase-calls (string type)
  (let ((display (make-instance 'stub-grid-display :char-height 17 :char-width 9))
        (object (make-instance 'lem-core/display:text-object
                               :string string
                               :attribute (lem:make-attribute :foreground "#aabbcc"
                                                              :background "#112233")
                               :type type)))
    (call-recording '(drawing::draw-letter
                      drawing::draw-text-glyph-surface)
                    (lambda ()
                      (drawing::draw-text-object-phase object 10 50 display nil :glyph)))))

(deftest run-glyphs-are-drawn-cell-by-cell
  (let ((calls (glyph-phase-calls "a─b" :latin)))
    (ok (equal (mapcar #'first calls)
               '(drawing::draw-text-glyph-surface
                 drawing::draw-letter
                 drawing::draw-text-glyph-surface)))
    (testing "surface letters get their own letter object and cell"
      (destructuring-bind (letter x bottom-y display view width &rest keys) (rest (first calls))
        (declare (ignore display view keys))
        (ok (equal (lem-core/display:text-object-string letter) "a"))
        (ok (equal (list x bottom-y width) '(10 50 9)))))
    (testing "box-drawing letters are drawn as rectangles in the next cell"
      (destructuring-bind (display character drawing x bottom-y width height
                           foreground background)
          (rest (second calls))
        (declare (ignore display foreground background))
        (ok (equal (list character drawing x bottom-y width height)
                   '(#\─ :box 19 50 9 17)))))
    (ok (= 28 (third (third calls))) "the last letter follows"))
  (testing "wide letters take two cells"
    (ok (equal (mapcar #'third (glyph-phase-calls "日本" :cjk)) '(10 28)))))
