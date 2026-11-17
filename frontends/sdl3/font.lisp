(defpackage :lem-sdl3/font
  (:use :cl)
  (:import-from :lem-sdl3/resource
                :get-resource-pathname)
  (:export :default-font-size
           :font
           :font-latin-normal-font
           :font-latin-bold-font
           :font-cjk-normal-font
           :font-cjk-bold-font
           :font-emoji-font
           :font-braille-font
           :font-char-width
           :font-char-height
           :font-ascent
           :font-descent
           :font-box-drawing-band
           :save-font-size
           :make-font-config
           :font-config-size
           :merge-font-config
           :change-size
           :open-font
           :close-font
           :get-font-list))
(in-package :lem-sdl3/font)

(defparameter *default-font-size* 15)

(defun default-font-size () *default-font-size*)

(defstruct (font-config (:constructor %make-font-config))
  size
  latin-normal-file
  latin-bold-file
  cjk-normal-file
  cjk-bold-file
  emoji-file
  braille-file)

(defstruct font
  latin-normal-font
  latin-bold-font
  cjk-normal-font
  cjk-bold-font
  emoji-font
  braille-font
  char-width
  char-height
  ;; Latin-normal-font TTF intrinsic metrics, cached at open-font time so the
  ;; glyph-draw path can anchor baselines without round-tripping into FFI on
  ;; every blit.  ASCENT is positive (pixels above baseline), DESCENT is
  ;; negative (pixels below baseline) per SDL_ttf convention.
  ascent
  descent
  ;; (TOP . BOTTOM) surface rows covered by U+2502 in the latin fonts, or
  ;; NIL; see box-drawing-band.
  latin-normal-box-band
  latin-bold-box-band)

(defun save-font-size (font-config &optional (ratio 1))
  (setf (lem:config :sdl2-font-size)
        (round (/ (font-config-size font-config) ratio))))

(defun make-font-config (&key (size (lem:config :sdl2-font-size *default-font-size*))
                              latin-normal-file
                              latin-bold-file
                              cjk-normal-file
                              cjk-bold-file
                              emoji-file
                              brail-file)
  (%make-font-config
   :size (lem:config :font-size (or size *default-font-size*))
   :latin-normal-file (or latin-normal-file
                          (lem:config
                           :sdl2-normal-font
                           (get-resource-pathname "resources/fonts/NotoSansMono-Regular.ttf")))
   :latin-bold-file (or latin-bold-file
                        (lem:config
                         :sdl2-bold-font
                         (get-resource-pathname "resources/fonts/NotoSansMono-Bold.ttf")))
   :cjk-normal-file (or cjk-normal-file
                        (lem:config
                         :sdl2-cjk-normal-font
                         (get-resource-pathname "resources/fonts/NotoSansCJK-Regular.ttc")))
   :cjk-bold-file (or cjk-bold-file
                      (lem:config
                       :sdl2-cjk-bold-font
                       (get-resource-pathname "resources/fonts/NotoSansCJK-Bold.ttc")))
   :emoji-file (or emoji-file
                   (lem:config
                    :sdl2-emoji-font
                    (get-resource-pathname "resources/fonts/NotoColorEmoji.ttf")))
   :braille-file (or brail-file
                     (get-resource-pathname "resources/fonts/FreeMono.ttf"))))

(defun merge-font-config (new old)
  (%make-font-config :size (or (font-config-size new)
                               (font-config-size old))
                     :latin-normal-file (or (font-config-latin-normal-file new)
                                            (font-config-latin-normal-file old))
                     :latin-bold-file (or (font-config-latin-bold-file new)
                                          (font-config-latin-bold-file old))
                     :cjk-normal-file (or (font-config-cjk-normal-file new)
                                          (font-config-cjk-normal-file old))
                     :cjk-bold-file (or (font-config-cjk-bold-file new)
                                        (font-config-cjk-bold-file old))
                     :emoji-file (or (font-config-emoji-file new)
                                     (font-config-emoji-file old))
                     :braille-file (or (font-config-braille-file new)
                                       (font-config-braille-file old))))

(defun change-size (font-config size)
  (let ((font-config (copy-font-config font-config)))
    (setf (font-config-size font-config) size)
    font-config))

(defun get-character-size (font)
  (let ((surface (lem-sdl3/ffi:render-text-solid font " " 0 0 0)))
    (unwind-protect (list (lem-sdl3/ffi:surface-width surface)
                          (lem-sdl3/ffi:surface-height surface))
      (lem-sdl3/ffi:destroy-surface surface))))

(defun box-drawing-band (ttf-font)
  "Return (TOP . BOTTOM), the rows of a rendered glyph surface that U+2502
(BOX DRAWINGS LIGHT VERTICAL) covers in TTF-FONT, or NIL if the font lacks
it.  Fonts draw box-drawing glyphs within their own ascent/descent, which is
slightly shorter than Lem's row, so stretching this band to the cell height
makes vertical lines meet across rows, as in a terminal."
  (when (lem-sdl3/ffi:font-has-glyph-p ttf-font #\│)
    (multiple-value-bind (minx maxx miny maxy)
        (lem-sdl3/ffi:glyph-metrics ttf-font #\│)
      (declare (ignore minx maxx))
      (when maxy
        (let ((ascent (lem-sdl3/ffi:font-ascent ttf-font)))
          (cons (- ascent maxy) (- ascent miny)))))))

(defun font-box-drawing-band (font bold)
  "Return the box-drawing band of FONT's bold or regular latin font."
  (if bold
      (font-latin-bold-box-band font)
      (font-latin-normal-box-band font)))

(defun open-font (&optional font-config)
  (let* ((font-config (or font-config (make-font-config)))
         (latin-normal-font (lem-sdl3/ffi:open-font (font-config-latin-normal-file font-config)
                                                (font-config-size font-config)))
         (latin-bold-font (lem-sdl3/ffi:open-font (font-config-latin-bold-file font-config)
                                              (font-config-size font-config)))
         (cjk-normal-font (lem-sdl3/ffi:open-font (font-config-cjk-normal-file font-config)
                                              (font-config-size font-config)))
         (cjk-bold-font (lem-sdl3/ffi:open-font (font-config-cjk-bold-file font-config)
                                            (font-config-size font-config)))
         (emoji-font (lem-sdl3/ffi:open-font (font-config-emoji-file font-config)
                                         (font-config-size font-config)))
         (braille-font (lem-sdl3/ffi:open-font (font-config-braille-file font-config)
                                           (font-config-size font-config))))
    (destructuring-bind (char-width char-height)
        (get-character-size latin-normal-font)
      (make-font :latin-normal-font latin-normal-font
                 :latin-bold-font latin-bold-font
                 :cjk-normal-font cjk-normal-font
                 :cjk-bold-font cjk-bold-font
                 :emoji-font emoji-font
                 :braille-font braille-font
                 :char-width char-width
                 :char-height char-height
                 :ascent (lem-sdl3/ffi:font-ascent latin-normal-font)
                 :descent (lem-sdl3/ffi:font-descent latin-normal-font)
                 :latin-normal-box-band (box-drawing-band latin-normal-font)
                 :latin-bold-box-band (box-drawing-band latin-bold-font)))))

(defun close-font (font)
  (lem-sdl3/ffi:close-font (font-latin-normal-font font))
  (lem-sdl3/ffi:close-font (font-latin-bold-font font))
  (lem-sdl3/ffi:close-font (font-cjk-normal-font font))
  (lem-sdl3/ffi:close-font (font-cjk-bold-font font))
  (lem-sdl3/ffi:close-font (font-emoji-font font))
  (lem-sdl3/ffi:close-font (font-braille-font font))
  (values))

(defgeneric get-font-list (platform))

(defmethod get-font-list (platform)
  '())

(defmethod get-font-list ((platform lem-sdl3/platform:linux))
  (loop :for line :in (split-sequence:split-sequence #\newline
                                                     (uiop:run-program "fc-list" :output :string)
                                                     :remove-empty-subseqs t)
        :collect (first (split-sequence:split-sequence #\: line :count 1))))

(defmethod get-font-list ((platform lem-sdl3/platform:mac))
  (loop :for line :in (split-sequence:split-sequence #\newline
                                                     (uiop:run-program "fc-list" :output :string)
                                                     :remove-empty-subseqs t)
        :collect (first (split-sequence:split-sequence #\: line :count 1))))

(defmethod get-font-list ((platform lem-sdl3/platform:windows))
  (loop :for file :in (lem:directory-files "C:/Windows/Fonts/*.[otOT][tT][fF]")
        :collect (uiop:native-namestring file)))
