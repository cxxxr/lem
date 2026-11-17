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
           :font-fallback-fonts
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
           :system-fallback-font-files
           :get-font-list))
(in-package :lem-sdl3/font)

(defparameter *default-font-size* 15)

(defvar *system-fallback-font-files* :unknown
  "Cache for `system-fallback-font-files': the list of system font files, or
:UNKNOWN before the first lookup.  Fonts are re-opened on every size or DPI
change, and finding them can mean running fc-match, so look them up once.")

(defparameter *mac-fallback-font-files*
  '("/System/Library/Fonts/Menlo.ttc"
    "/System/Library/Fonts/Apple Symbols.ttf"
    "/System/Library/Fonts/Supplemental/Arial Unicode.ttf"
    "/System/Library/Fonts/Supplemental/STIXGeneral.otf")
  "macOS fonts used as fallbacks, in order: monospace first, then symbols,
then broad script coverage.  macOS has no fontconfig by default, so these are
fixed paths; ones that do not exist are skipped.")

(defparameter *windows-fallback-font-files*
  '("C:/Windows/Fonts/consola.ttf"
    "C:/Windows/Fonts/seguisym.ttf"
    "C:/Windows/Fonts/segoeui.ttf"
    "C:/Windows/Fonts/Nirmala.ttf"
    "C:/Windows/Fonts/Nirmala.ttc"
    "C:/Windows/Fonts/ebrima.ttf"
    "C:/Windows/Fonts/cambria.ttc")
  "Windows fonts used as fallbacks, in order; ones that do not exist are
skipped.")

(defparameter *max-fontconfig-fallback-fonts* 12
  "How many of fontconfig's sorted monospace matches to use as fallbacks.")

(defun default-font-size () *default-font-size*)

(defstruct (font-config (:constructor %make-font-config))
  size
  latin-normal-file
  latin-bold-file
  cjk-normal-file
  cjk-bold-file
  emoji-file
  braille-file
  ;; Extra font files to fall back to, tried before the system's.
  fallback-files
  ;; Whether to also fall back to the system's fonts.
  system-fallback)

(defstruct font
  latin-normal-font
  latin-bold-font
  cjk-normal-font
  cjk-bold-font
  emoji-font
  braille-font
  ;; Fonts opened only to be fallbacks of the fonts above (see
  ;; add-fallback-fonts); closed after them.
  fallback-fonts
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
                              brail-file
                              (fallback-files (lem:config :sdl3-fallback-fonts))
                              (system-fallback (lem:config :sdl3-system-fallback-fonts t)))
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
                     (get-resource-pathname "resources/fonts/FreeMono.ttf"))
   :fallback-files fallback-files
   :system-fallback system-fallback))

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
                                       (font-config-braille-file old))
                     :fallback-files (or (font-config-fallback-files new)
                                         (font-config-fallback-files old))
                     :system-fallback (font-config-system-fallback old)))

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

(defgeneric platform-fallback-font-files (platform)
  (:documentation "Return the font files PLATFORM provides for glyphs the
configured fonts lack, most preferred first.  Files may not exist."))

(defmethod platform-fallback-font-files (platform)
  (declare (ignore platform))
  '())

(defmethod platform-fallback-font-files ((platform lem-sdl3/platform:mac))
  *mac-fallback-font-files*)

(defmethod platform-fallback-font-files ((platform lem-sdl3/platform:windows))
  *windows-fallback-font-files*)

(defmethod platform-fallback-font-files ((platform lem-sdl3/platform:linux))
  ;; fc-match -s sorts fonts by how well they match and drops ones that add
  ;; no coverage, which is the fallback list fontconfig itself would use.
  (let ((output (ignore-errors
                 (uiop:run-program '("fc-match" "-s" "--format=%{file}\\n" "monospace")
                                   :output :string))))
    (when output
      (let ((files (split-sequence:split-sequence #\newline output
                                                  :remove-empty-subseqs t)))
        (subseq files 0 (min (length files) *max-fontconfig-fallback-fonts*))))))

(defun system-fallback-font-files ()
  "Return the existing system font files to fall back to, most preferred
first.  The result is cached for the session."
  (when (eq *system-fallback-font-files* :unknown)
    (setf *system-fallback-font-files*
          (remove-duplicates
           (remove-if-not #'probe-file
                          (platform-fallback-font-files
                           (lem-sdl3/platform:get-platform)))
           :test #'equal
           :from-end t)))
  *system-fallback-font-files*)

(defun open-fallback-fonts (font-config)
  "Open the fallback font files FONT-CONFIG names (configured ones, then the
system's) at its size, skipping files that do not exist or fail to open."
  (loop :for file :in (append (font-config-fallback-files font-config)
                              (and (font-config-system-fallback font-config)
                                   (system-fallback-font-files)))
        :for font := (and (probe-file file)
                          (handler-case (lem-sdl3/ffi:open-font file (font-config-size font-config))
                            (error () nil)))
        :when font
          :collect font))

(defun add-fallback-fonts (font fallbacks)
  "Add FALLBACKS, in order, as FONT's fallback fonts."
  (dolist (fallback fallbacks)
    (lem-sdl3/ffi:add-fallback-font font fallback)))

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
                                           (font-config-size font-config)))
         (fallback-fonts (open-fallback-fonts font-config))
         ;; Measured before fallbacks are added: TTF_FontHasGlyph counts
         ;; fallbacks, and the band must come from the latin font itself.
         (latin-normal-box-band (box-drawing-band latin-normal-font))
         (latin-bold-box-band (box-drawing-band latin-bold-font)))
    ;; Lem picks one font per character type, so a glyph missing from it
    ;; used to draw as an empty box.  SDL_ttf only consults a font's own
    ;; fallbacks, not theirs, so each text font gets a flat list: the CJK
    ;; font (for the latin fonts), the configured and system fonts, then
    ;; emoji.  The CJK, emoji and braille fonts are only borrowed here; they
    ;; are closed as primaries.
    (add-fallback-fonts latin-normal-font
                        (append (list cjk-normal-font) fallback-fonts (list emoji-font)))
    (add-fallback-fonts latin-bold-font
                        (append (list cjk-bold-font) fallback-fonts (list emoji-font)))
    (add-fallback-fonts cjk-normal-font (append fallback-fonts (list emoji-font)))
    (add-fallback-fonts cjk-bold-font (append fallback-fonts (list emoji-font)))
    (destructuring-bind (char-width char-height)
        (get-character-size latin-normal-font)
      (make-font :latin-normal-font latin-normal-font
                 :latin-bold-font latin-bold-font
                 :cjk-normal-font cjk-normal-font
                 :cjk-bold-font cjk-bold-font
                 :emoji-font emoji-font
                 :braille-font braille-font
                 :fallback-fonts fallback-fonts
                 :char-width char-width
                 :char-height char-height
                 :ascent (lem-sdl3/ffi:font-ascent latin-normal-font)
                 :descent (lem-sdl3/ffi:font-descent latin-normal-font)
                 :latin-normal-box-band latin-normal-box-band
                 :latin-bold-box-band latin-bold-box-band))))

(defun close-font (font)
  (lem-sdl3/ffi:close-font (font-latin-normal-font font))
  (lem-sdl3/ffi:close-font (font-latin-bold-font font))
  (lem-sdl3/ffi:close-font (font-cjk-normal-font font))
  (lem-sdl3/ffi:close-font (font-cjk-bold-font font))
  (lem-sdl3/ffi:close-font (font-emoji-font font))
  (lem-sdl3/ffi:close-font (font-braille-font font))
  ;; Last: the fonts above use these as fallbacks.
  (mapc #'lem-sdl3/ffi:close-font (font-fallback-fonts font))
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
