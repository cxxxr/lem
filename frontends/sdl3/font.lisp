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
           :glyph-source
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
  latin-bold-box-band
  ;; Copies of the text fonts made before their fallbacks were added, so
  ;; that TTF_FontHasGlyph on them tells whether the font itself has a glyph.
  coverage-fonts
  ;; Pointer address of each text font -> list of (COVERAGE-FONT . ASCENT),
  ;; the font itself followed by its fallbacks, in the order SDL_ttf tries
  ;; them; see glyph-source.
  (fallback-chains (make-hash-table))
  ;; (address . char-code) -> (ASCENT . OWN-P); memoizes glyph-source.
  (glyph-sources (make-hash-table :test 'equal)))

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

(defun glyph-source (font ttf-font character)
  "Return, as values, the ascent of the font that draws CHARACTER when it is
rendered with TTF-FONT, and whether that is TTF-FONT itself rather than one
of its fallbacks.  SDL_ttf puts a fallback glyph on the fallback font's own
baseline (ascent pixels below the surface top), not TTF-FONT's, so the
caller must anchor the surface with this ascent.  For a font FONT did not
open (e.g. an attribute's own font) return TTF-FONT's ascent and T."
  (let* ((address (cffi:pointer-address ttf-font))
         (chain (gethash address (font-fallback-chains font))))
    (if (null chain)
        (values (lem-sdl3/ffi:font-ascent ttf-font) t)
        (let* ((key (cons address (char-code character)))
               (source (or (gethash key (font-glyph-sources font))
                           (setf (gethash key (font-glyph-sources font))
                                 (let ((link (or (find-if (lambda (link)
                                                            (lem-sdl3/ffi:font-has-glyph-p
                                                             (car link) character))
                                                          chain)
                                                 (first chain))))
                                   (cons (cdr link) (eq link (first chain))))))))
          (values (car source) (cdr source))))))

(defun register-fallback-chain (font ttf-font chain)
  "Record CHAIN, TTF-FONT's (COVERAGE-FONT . ASCENT) list, in FONT."
  (setf (gethash (cffi:pointer-address ttf-font) (font-fallback-chains font))
        chain))

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
    (let* ((text-fonts (list latin-normal-font latin-bold-font
                             cjk-normal-font cjk-bold-font))
           ;; Made before the fallbacks are added, so they have none.
           (coverage-fonts (mapcar #'lem-sdl3/ffi:copy-font text-fonts))
           (coverage (pairlis text-fonts coverage-fonts)))
      (flet ((link (ttf-font)
               (cons (or (cdr (assoc ttf-font coverage)) ttf-font)
                     (lem-sdl3/ffi:font-ascent ttf-font))))
        (let ((fallbacks
                (list (cons latin-normal-font
                            (append (list cjk-normal-font) fallback-fonts (list emoji-font)))
                      (cons latin-bold-font
                            (append (list cjk-bold-font) fallback-fonts (list emoji-font)))
                      (cons cjk-normal-font (append fallback-fonts (list emoji-font)))
                      (cons cjk-bold-font (append fallback-fonts (list emoji-font))))))
          ;; Lem picks one font per character type, so a glyph missing from it
          ;; used to draw as an empty box.  SDL_ttf only consults a font's own
          ;; fallbacks, not theirs, so each text font gets a flat list: the CJK
          ;; font (for the latin fonts), the configured and system fonts, then
          ;; emoji.  The CJK, emoji and braille fonts are only borrowed here;
          ;; they are closed as primaries.
          (loop :for (ttf-font . list) :in fallbacks
                :do (add-fallback-fonts ttf-font list))
          (destructuring-bind (char-width char-height)
              (get-character-size latin-normal-font)
            (let ((font (make-font :latin-normal-font latin-normal-font
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
                                   :latin-bold-box-band latin-bold-box-band
                                   :coverage-fonts coverage-fonts)))
              (loop :for (ttf-font . list) :in fallbacks
                    :do (register-fallback-chain font ttf-font
                                                 (mapcar #'link (cons ttf-font list))))
              font)))))))

(defun close-font (font)
  (lem-sdl3/ffi:close-font (font-latin-normal-font font))
  (lem-sdl3/ffi:close-font (font-latin-bold-font font))
  (lem-sdl3/ffi:close-font (font-cjk-normal-font font))
  (lem-sdl3/ffi:close-font (font-cjk-bold-font font))
  (lem-sdl3/ffi:close-font (font-emoji-font font))
  (lem-sdl3/ffi:close-font (font-braille-font font))
  (mapc #'lem-sdl3/ffi:close-font (font-coverage-fonts font))
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
