(defpackage :lem-sdl3/glyph-atlas
  (:use :cl)
  (:local-nicknames (:ft :lem-sdl3/ffi-freetype)
                    (:shaper :lem-sdl3/shaper))
  (:export :*atlas-size*
           :make-glyph-atlas
           :destroy-glyph-atlas
           :clear-glyph-atlas
           :glyph-atlas-glyph-count
           :queue-glyph
           :flush-glyphs))
(in-package :lem-sdl3/glyph-atlas)

;;; Glyphs rasterized with FreeType into one texture, and drawn in batches.
;;;
;;; Each glyph is stored once, as white with its coverage in alpha, and is
;;; tinted per vertex, so one copy serves every color.  Glyphs are packed in
;;; rows ("shelves"); when the texture is full it is emptied and refilled
;;; with what is drawn next.  Queued glyphs are drawn with one
;;; SDL_RenderGeometry call per flush instead of one call per glyph.

(defparameter *atlas-size* 2048
  "Width and height of the atlas texture, in pixels.")

(defconstant +max-quads+ 4096
  "Glyphs queued before the batch is flushed.")

(defconstant +vertex-floats+ 8
  "Floats per SDL_Vertex: position (2), color (4), texture coordinate (2).")

(defstruct (glyph-atlas (:constructor %make-glyph-atlas))
  renderer
  texture
  size
  ;; Where the next glyph goes, and the height of the current shelf.
  (x 1)
  (y 1)
  (shelf-height 0)
  ;; shaping-font -> (glyph id -> glyph-entry).
  (glyphs (make-hash-table :test 'eq))
  (glyph-count 0)
  ;; Foreign SDL_Vertex and index arrays for +max-quads+ quads.
  vertices
  indices
  (quads 0))

(defstruct glyph-entry
  "A glyph's place in the atlas and its bitmap's offset from the pen on the
baseline: LEFT pixels right, TOP pixels up."
  (u 0) (v 0) (width 0) (height 0) (left 0) (top 0))

(defun make-glyph-atlas (renderer &optional (size *atlas-size*))
  "Create an atlas drawing with RENDERER.  Destroy it with
DESTROY-GLYPH-ATLAS before the renderer is destroyed."
  (let ((indices (cffi:foreign-alloc :int :count (* 6 +max-quads+))))
    (loop :for quad :below +max-quads+
          :do (loop :for corner :in '(0 1 2 2 1 3)
                    :for i :from (* 6 quad)
                    :do (setf (cffi:mem-aref indices :int i) (+ (* 4 quad) corner))))
    (%make-glyph-atlas :renderer renderer
                       :texture (lem-sdl3/ffi:create-glyph-texture renderer size size)
                       :size size
                       :vertices (cffi:foreign-alloc :float :count (* 4 +vertex-floats+ +max-quads+))
                       :indices indices)))

(defun destroy-glyph-atlas (atlas)
  "Free ATLAS's texture and buffers."
  (lem-sdl3/ffi:destroy-texture (glyph-atlas-texture atlas))
  (cffi:foreign-free (glyph-atlas-vertices atlas))
  (cffi:foreign-free (glyph-atlas-indices atlas)))

(defun clear-glyph-atlas (atlas)
  "Forget every glyph in ATLAS.  Call this before closing a font its glyphs
come from; queued glyphs must have been flushed."
  (setf (glyph-atlas-x atlas) 1
        (glyph-atlas-y atlas) 1
        (glyph-atlas-shelf-height atlas) 0
        (glyph-atlas-glyph-count atlas) 0)
  (clrhash (glyph-atlas-glyphs atlas)))

(defun upload-coverage (atlas buffer width rows pitch u v)
  "Copy a FreeType coverage bitmap into ATLAS's texture at (U, V), as white
pixels with the coverage as alpha."
  (cffi:with-foreign-object (pixels :uint32 (* width rows))
    (loop :for y :below rows
          :do (loop :for x :below width
                    :do (setf (cffi:mem-aref pixels :uint32 (+ x (* y width)))
                              (logior (ash (cffi:mem-aref buffer :uint8 (+ x (* y pitch))) 24)
                                      #xffffff))))
    (lem-sdl3/ffi:update-texture (glyph-atlas-texture atlas) u v width rows pixels (* 4 width))))

(defun add-glyph (atlas font id)
  "Rasterize glyph ID of FONT into ATLAS and return its entry, or NIL if
the atlas has no room left."
  (multiple-value-bind (buffer width rows pitch left top)
      (ft:render-glyph (shaper:shaping-font-face font) id)
    (unless buffer
      (return-from add-glyph (make-glyph-entry)))
    (let ((size (glyph-atlas-size atlas)))
      (when (> (+ (glyph-atlas-x atlas) width 1) size)
        (setf (glyph-atlas-x atlas) 1
              (glyph-atlas-y atlas) (+ (glyph-atlas-y atlas) (glyph-atlas-shelf-height atlas) 1)
              (glyph-atlas-shelf-height atlas) 0))
      (when (or (> (+ (glyph-atlas-y atlas) rows 1) size)
                (> (+ width 2) size))
        (return-from add-glyph nil))
      (let ((u (glyph-atlas-x atlas))
            (v (glyph-atlas-y atlas)))
        (when (and (plusp width) (plusp rows))
          (upload-coverage atlas buffer width rows pitch u v))
        (incf (glyph-atlas-x atlas) (1+ width))
        (setf (glyph-atlas-shelf-height atlas) (max (glyph-atlas-shelf-height atlas) rows))
        (make-glyph-entry :u u :v v :width width :height rows :left left :top top)))))

(defun find-glyph (atlas font id)
  "Return the entry for glyph ID of FONT, adding it to ATLAS if needed, or
NIL if it does not fit."
  (let ((glyphs (or (gethash font (glyph-atlas-glyphs atlas))
                    (setf (gethash font (glyph-atlas-glyphs atlas)) (make-hash-table)))))
    (or (gethash id glyphs)
        (let ((entry (add-glyph atlas font id)))
          (when entry
            (incf (glyph-atlas-glyph-count atlas))
            (setf (gethash id glyphs) entry))))))

(defun put-vertex (vertices index x y r g b u v)
  (let ((base (* index +vertex-floats+)))
    (setf (cffi:mem-aref vertices :float base) x
          (cffi:mem-aref vertices :float (+ base 1)) y
          (cffi:mem-aref vertices :float (+ base 2)) r
          (cffi:mem-aref vertices :float (+ base 3)) g
          (cffi:mem-aref vertices :float (+ base 4)) b
          (cffi:mem-aref vertices :float (+ base 5)) 1.0
          (cffi:mem-aref vertices :float (+ base 6)) u
          (cffi:mem-aref vertices :float (+ base 7)) v)))

(defun queue-glyph (atlas font id x baseline color)
  "Queue glyph ID of FONT, in COLOR (a lem:color), with its pen at X on the
row whose baseline is at y = BASELINE.  It is drawn by the next
FLUSH-GLYPHS.  When the atlas is full, what is queued is drawn and the
atlas is emptied first."
  (let ((entry (or (find-glyph atlas font id)
                   (progn (flush-glyphs atlas)
                          (clear-glyph-atlas atlas)
                          (find-glyph atlas font id)))))
    (when (and entry (plusp (glyph-entry-width entry)) (plusp (glyph-entry-height entry)))
      (when (= (glyph-atlas-quads atlas) +max-quads+)
        (flush-glyphs atlas))
      (let* ((size (float (glyph-atlas-size atlas)))
             (x0 (float (+ x (glyph-entry-left entry))))
             (y0 (float (- baseline (glyph-entry-top entry))))
             (x1 (+ x0 (glyph-entry-width entry)))
             (y1 (+ y0 (glyph-entry-height entry)))
             (u0 (/ (glyph-entry-u entry) size))
             (v0 (/ (glyph-entry-v entry) size))
             (u1 (/ (+ (glyph-entry-u entry) (glyph-entry-width entry)) size))
             (v1 (/ (+ (glyph-entry-v entry) (glyph-entry-height entry)) size))
             (r (/ (lem:color-red color) 255.0))
             (g (/ (lem:color-green color) 255.0))
             (b (/ (lem:color-blue color) 255.0))
             (vertices (glyph-atlas-vertices atlas))
             (corner (* 4 (glyph-atlas-quads atlas))))
        (put-vertex vertices corner x0 y0 r g b u0 v0)
        (put-vertex vertices (+ corner 1) x1 y0 r g b u1 v0)
        (put-vertex vertices (+ corner 2) x0 y1 r g b u0 v1)
        (put-vertex vertices (+ corner 3) x1 y1 r g b u1 v1)
        (incf (glyph-atlas-quads atlas))))))

(defun flush-glyphs (atlas)
  "Draw every queued glyph of ATLAS on the current render target."
  (let ((quads (glyph-atlas-quads atlas)))
    (when (plusp quads)
      (lem-sdl3/ffi:render-geometry (glyph-atlas-renderer atlas)
                                    (glyph-atlas-texture atlas)
                                    (glyph-atlas-vertices atlas) (* 4 quads)
                                    (glyph-atlas-indices atlas) (* 6 quads))
      (setf (glyph-atlas-quads atlas) 0))))
