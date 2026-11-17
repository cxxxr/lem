(defpackage :lem-sdl3/tests/drawing
  (:use :cl :rove)
  (:local-nicknames (:drawing :lem-sdl3/drawing)
                    (:display :lem-sdl3/display)))
(in-package :lem-sdl3/tests/drawing)

;;; Regression tests for `lem-sdl3/drawing::object-height' on the base
;;; `text-object' class.
;;;
;;; Earlier the method returned `(sdl2:surface-height (get-surface ...))',
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
