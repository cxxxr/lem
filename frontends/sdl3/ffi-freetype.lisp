(defpackage :lem-sdl3/ffi-freetype
  (:use :cl)
  (:export :available-p
           :open-face
           :close-face
           :face-ascender
           :face-descender
           :char-index
           :render-glyph))
(in-package :lem-sdl3/ffi-freetype)

;;; Minimal FreeType binding: open a face at a size, map characters to glyph
;;; indices, and rasterize a glyph to an 8-bit coverage bitmap.
;;;
;;; The structs are declared field by field up to the members used, rather
;;; than read at fixed offsets, so CFFI lays them out for the platform's
;;; `long' (64-bit on Linux and macOS, 32-bit on Windows).

(cffi:define-foreign-library libfreetype
  (:darwin (:or "/opt/homebrew/opt/freetype/lib/libfreetype.6.dylib"
                "/usr/local/opt/freetype/lib/libfreetype.6.dylib"
                "libfreetype.6.dylib"))
  (:windows (:or "libfreetype-6.dll" "freetype.dll"))
  (:unix (:or "libfreetype.so.6" "libfreetype.so"))
  (t (:default "libfreetype")))

(defvar *available*
  (handler-case (progn (cffi:use-foreign-library libfreetype) t)
    (error () nil))
  "True if the FreeType library was loaded.")

(defvar *library* nil
  "The FT_Library, created on first use.")

(defconstant +load-default+ 0
  "FT_LOAD_DEFAULT: hinted, FT_LOAD_TARGET_NORMAL, as SDL_ttf loads glyphs.")
(defconstant +render-mode-normal+ 0
  "FT_RENDER_MODE_NORMAL: 8-bit anti-aliased coverage.")

(cffi:defcstruct generic
  (data :pointer)
  (finalizer :pointer))

(cffi:defcstruct bbox
  (x-min :long) (y-min :long) (x-max :long) (y-max :long))

(cffi:defcstruct ft-vector
  (x :long) (y :long))

(cffi:defcstruct glyph-metrics
  (width :long) (height :long)
  (hori-bearing-x :long) (hori-bearing-y :long) (hori-advance :long)
  (vert-bearing-x :long) (vert-bearing-y :long) (vert-advance :long))

(cffi:defcstruct bitmap
  (rows :uint)
  (width :uint)
  (pitch :int)
  (buffer :pointer)
  (num-grays :ushort)
  (pixel-mode :uchar)
  (palette-mode :uchar)
  (palette :pointer))

;;; FT_FaceRec, up to `size'.
(cffi:defcstruct face-rec
  (num-faces :long) (face-index :long) (face-flags :long) (style-flags :long)
  (num-glyphs :long)
  (family-name :pointer) (style-name :pointer)
  (num-fixed-sizes :int) (available-sizes :pointer)
  (num-charmaps :int) (charmaps :pointer)
  (generic (:struct generic))
  (bbox (:struct bbox))
  (units-per-em :ushort)
  (ascender :short) (descender :short) (height :short)
  (max-advance-width :short) (max-advance-height :short)
  (underline-position :short) (underline-thickness :short)
  (glyph :pointer)
  (size :pointer))

(cffi:defcstruct size-metrics
  (x-ppem :ushort) (y-ppem :ushort)
  (x-scale :long) (y-scale :long)
  (ascender :long) (descender :long) (height :long) (max-advance :long))

;;; FT_SizeRec, up to `metrics'.
(cffi:defcstruct size-rec
  (face :pointer)
  (generic (:struct generic))
  (metrics (:struct size-metrics)))

;;; FT_GlyphSlotRec, up to `bitmap_top'.
(cffi:defcstruct glyph-slot
  (library :pointer) (face :pointer) (next :pointer)
  (glyph-index :uint)
  (generic (:struct generic))
  (metrics (:struct glyph-metrics))
  (linear-hori-advance :long) (linear-vert-advance :long)
  (advance (:struct ft-vector))
  (format :uint)
  (bitmap (:struct bitmap))
  (bitmap-left :int)
  (bitmap-top :int))

(cffi:defcfun ("FT_Init_FreeType" %init-freetype) :int (library :pointer))
(cffi:defcfun ("FT_New_Face" %new-face) :int
  (library :pointer) (path :string) (index :long) (face :pointer))
(cffi:defcfun ("FT_Done_Face" %done-face) :int (face :pointer))
(cffi:defcfun ("FT_Set_Char_Size" %set-char-size) :int
  (face :pointer) (width :long) (height :long) (hres :uint) (vres :uint))
(cffi:defcfun ("FT_Get_Char_Index" %get-char-index) :uint (face :pointer) (code :ulong))
(cffi:defcfun ("FT_Load_Glyph" %load-glyph) :int (face :pointer) (index :uint) (flags :int32))
(cffi:defcfun ("FT_Render_Glyph" %render-glyph) :int (slot :pointer) (mode :int))

(defun available-p ()
  "Return true if FreeType is installed and loaded."
  *available*)

(defun library ()
  (or *library*
      (cffi:with-foreign-object (library :pointer)
        (let ((error (%init-freetype library)))
          (unless (zerop error)
            (error "FT_Init_FreeType failed (~D)" error)))
        (setf *library* (cffi:mem-ref library :pointer)))))

(defun open-face (pathname size)
  "Open the font file PATHNAME at point SIZE and return the FT_Face.  The size
is set as SDL_ttf sets it (SIZE * 64 at 72 dpi), so glyphs and metrics match
a TTF_Font opened from the same file.  Close it with CLOSE-FACE."
  (cffi:with-foreign-object (face :pointer)
    (let ((error (%new-face (library) (uiop:native-namestring pathname) 0 face)))
      (unless (zerop error)
        (error "FT_New_Face failed for ~A (~D)" pathname error)))
    (let ((face (cffi:mem-ref face :pointer)))
      (%set-char-size face 0 (round (* size 64)) 72 72)
      face)))

(defun close-face (face)
  "Close FACE, made by OPEN-FACE."
  (%done-face face))

(defun size-metric (face slot)
  (let* ((size (cffi:foreign-slot-value face '(:struct face-rec) 'size))
         (metrics (cffi:foreign-slot-pointer size '(:struct size-rec) 'metrics)))
    (cffi:foreign-slot-value metrics '(:struct size-metrics) slot)))

(defun face-ascender (face)
  "FACE's ascender at its size, in pixels above the baseline."
  (/ (size-metric face 'ascender) 64))

(defun face-descender (face)
  "FACE's descender at its size, in pixels (negative: below the baseline)."
  (/ (size-metric face 'descender) 64))

(defun char-index (face character)
  "Return FACE's glyph index for CHARACTER, or 0 if FACE lacks it."
  (%get-char-index face (char-code character)))

(defun render-glyph (face glyph-index)
  "Rasterize GLYPH-INDEX of FACE.  Return as values the coverage bitmap's
BUFFER pointer, WIDTH, ROWS and PITCH, and its offset from the pen position
on the baseline: LEFT (pixels right) and TOP (pixels up).  The buffer is
FreeType's and is only valid until FACE loads another glyph.  Return NIL if
the glyph cannot be loaded."
  (when (and (zerop (%load-glyph face glyph-index +load-default+))
             (let ((slot (cffi:foreign-slot-value face '(:struct face-rec) 'glyph)))
               (zerop (%render-glyph slot +render-mode-normal+))))
    (let* ((slot (cffi:foreign-slot-value face '(:struct face-rec) 'glyph))
           (bitmap (cffi:foreign-slot-pointer slot '(:struct glyph-slot) 'bitmap)))
      (cffi:with-foreign-slots ((buffer width rows pitch) bitmap (:struct bitmap))
        (values buffer width rows pitch
                (cffi:foreign-slot-value slot '(:struct glyph-slot) 'bitmap-left)
                (cffi:foreign-slot-value slot '(:struct glyph-slot) 'bitmap-top))))))
