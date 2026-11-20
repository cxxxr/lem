(defpackage :lem-sdl3/ffi-harfbuzz
  (:use :cl)
  (:export :available-p
           :make-font
           :destroy-font
           :parse-features
           :free-features
           :shape
           :glyph-id
           :glyph-cluster
           :glyph-x-advance
           :glyph-x-offset
           :glyph-y-offset))
(in-package :lem-sdl3/ffi-harfbuzz)

;;; Minimal HarfBuzz binding: shape a string with a font made from a
;;; FreeType face (hb-ft), and read back glyph ids, clusters and positions.
;;; Positions are in 26.6 fixed point (1/64 pixel), as FreeType reports them.

(cffi:define-foreign-library libharfbuzz
  (:darwin (:or "/opt/homebrew/opt/harfbuzz/lib/libharfbuzz.0.dylib"
                "/usr/local/opt/harfbuzz/lib/libharfbuzz.0.dylib"
                "libharfbuzz.0.dylib"))
  (:windows (:or "libharfbuzz-0.dll" "harfbuzz.dll"))
  (:unix (:or "libharfbuzz.so.0" "libharfbuzz.so"))
  (t (:default "libharfbuzz")))

(defvar *available*
  (handler-case (progn (cffi:use-foreign-library libharfbuzz) t)
    (error () nil))
  "True if the HarfBuzz library was loaded.")

(defvar *buffer* nil
  "The hb_buffer_t reused by SHAPE, created on first use.  Shaping happens on
the main thread only, so one buffer is enough.")

(defconstant +cluster-level-monotone-characters+ 1
  "HB_BUFFER_CLUSTER_LEVEL_MONOTONE_CHARACTERS: clusters are character
indices that never decrease, and marks get their own cluster when they are
not merged with a base.")

(cffi:defcstruct glyph-info
  (codepoint :uint32)
  (mask :uint32)
  (cluster :uint32)
  (var1 :uint32)
  (var2 :uint32))

(cffi:defcstruct glyph-position
  (x-advance :int32)
  (y-advance :int32)
  (x-offset :int32)
  (y-offset :int32)
  (var :uint32))

(cffi:defcstruct feature
  (tag :uint32)
  (value :uint32)
  (start :uint)
  (end :uint))

(cffi:defcfun ("hb_ft_font_create_referenced" %ft-font-create-referenced) :pointer
  (ft-face :pointer))
(cffi:defcfun ("hb_font_destroy" %font-destroy) :void (font :pointer))
(cffi:defcfun ("hb_buffer_create" %buffer-create) :pointer)
(cffi:defcfun ("hb_buffer_clear_contents" %buffer-clear-contents) :void (buffer :pointer))
(cffi:defcfun ("hb_buffer_add_utf32" %buffer-add-utf32) :void
  (buffer :pointer) (text :pointer) (text-length :int) (item-offset :uint) (item-length :int))
(cffi:defcfun ("hb_buffer_set_cluster_level" %buffer-set-cluster-level) :void
  (buffer :pointer) (level :int))
(cffi:defcfun ("hb_buffer_guess_segment_properties" %buffer-guess-segment-properties) :void
  (buffer :pointer))
(cffi:defcfun ("hb_shape" %shape) :void
  (font :pointer) (buffer :pointer) (features :pointer) (num-features :uint))
(cffi:defcfun ("hb_buffer_get_glyph_infos" %buffer-get-glyph-infos) :pointer
  (buffer :pointer) (length :pointer))
(cffi:defcfun ("hb_buffer_get_glyph_positions" %buffer-get-glyph-positions) :pointer
  (buffer :pointer) (length :pointer))
(cffi:defcfun ("hb_feature_from_string" %feature-from-string) :bool
  (string :string) (length :int) (feature :pointer))

(defun available-p ()
  "Return true if HarfBuzz is installed and loaded."
  *available*)

(defun buffer ()
  (or *buffer* (setf *buffer* (%buffer-create))))

(defun make-font (ft-face)
  "Return an hb_font_t that shapes with FT-FACE's hinted metrics at its
current size.  FT-FACE must stay open until the font is destroyed with
DESTROY-FONT."
  (%ft-font-create-referenced ft-face))

(defun destroy-font (font)
  "Destroy FONT, made by MAKE-FONT."
  (%font-destroy font))

(defun parse-features (strings)
  "Parse feature STRINGS such as \"-liga\" or \"calt=0\" into a foreign
hb_feature_t array.  Return the array and its length; free the array with
FREE-FEATURES.  Signal an error for a string HarfBuzz cannot parse."
  (let ((features (cffi:foreign-alloc '(:struct feature) :count (max 1 (length strings)))))
    (loop :for string :in strings
          :for i :from 0
          :unless (%feature-from-string string -1 (cffi:mem-aptr features '(:struct feature) i))
            :do (cffi:foreign-free features)
                (error "Invalid OpenType feature: ~S" string))
    (values features (length strings))))

(defun free-features (features)
  "Free an array returned by PARSE-FEATURES."
  (cffi:foreign-free features))

(defun shape (font string &key (start 0) (end (length string))
                                (features (cffi:null-pointer)) (feature-count 0))
  "Shape the characters of STRING from START to END with FONT, the rest of
STRING serving as context.  Return as values the glyph count and the glyph
info and position arrays, read with GLYPH-ID, GLYPH-CLUSTER and the
GLYPH-*-OFFSET / GLYPH-X-ADVANCE readers.  Clusters are indices into STRING.
The arrays belong to a shared buffer and are only valid until the next
SHAPE."
  (let ((buffer (buffer))
        (length (length string)))
    (%buffer-clear-contents buffer)
    (cffi:with-foreign-object (text :uint32 (max 1 length))
      (loop :for i :below length
            :do (setf (cffi:mem-aref text :uint32 i) (char-code (char string i))))
      (%buffer-add-utf32 buffer text length start (- end start)))
    (%buffer-set-cluster-level buffer +cluster-level-monotone-characters+)
    (%buffer-guess-segment-properties buffer)
    (%shape font buffer features feature-count)
    (cffi:with-foreign-object (count :uint)
      (let ((infos (%buffer-get-glyph-infos buffer count))
            (positions (%buffer-get-glyph-positions buffer (cffi:null-pointer))))
        (values (cffi:mem-ref count :uint) infos positions)))))

(declaim (inline glyph-id glyph-cluster glyph-x-advance glyph-x-offset glyph-y-offset))

(defun glyph-id (infos i)
  "The glyph index of the Ith glyph of INFOS, from SHAPE."
  (cffi:foreign-slot-value (cffi:mem-aptr infos '(:struct glyph-info) i)
                           '(:struct glyph-info) 'codepoint))

(defun glyph-cluster (infos i)
  "The index in the shaped string of the first character of the Ith glyph's
cluster."
  (cffi:foreign-slot-value (cffi:mem-aptr infos '(:struct glyph-info) i)
                           '(:struct glyph-info) 'cluster))

(defun glyph-x-advance (positions i)
  "The Ith glyph's horizontal advance in 26.6 fixed point."
  (cffi:foreign-slot-value (cffi:mem-aptr positions '(:struct glyph-position) i)
                           '(:struct glyph-position) 'x-advance))

(defun glyph-x-offset (positions i)
  "The Ith glyph's horizontal offset from its pen position, in 26.6."
  (cffi:foreign-slot-value (cffi:mem-aptr positions '(:struct glyph-position) i)
                           '(:struct glyph-position) 'x-offset))

(defun glyph-y-offset (positions i)
  "The Ith glyph's vertical offset (up) from the baseline, in 26.6."
  (cffi:foreign-slot-value (cffi:mem-aptr positions '(:struct glyph-position) i)
                           '(:struct glyph-position) 'y-offset))
