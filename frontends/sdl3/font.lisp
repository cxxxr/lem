(defpackage :lem-sdl3/font
  (:use :cl)
  (:import-from :lem-sdl3/resource
                :get-resource-pathname
                :find-resource-pathname)
  (:export :default-font-size
           :font
           :font-latin-normal-font
           :font-latin-bold-font
           :font-cjk-normal-font
           :font-cjk-bold-font
           :font-cjk-narrow-normal-font
           :font-cjk-narrow-bold-font
           :font-emoji-font
           :font-braille-font
           :font-fallback-fonts
           :font-char-width
           :font-char-height
           :font-ascent
           :font-descent
           :font-box-drawing-band
           :glyph-source
           :font-shaping-chain
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

(defparameter *system-emoji-font-files*
  '("/System/Library/Fonts/Apple Color Emoji.ttc")
  "The platform's own colour emoji fonts, tried in order before the bundled
Noto Color Emoji, so emoji look as they do in other applications.")

(defparameter *mac-default-font-files*
  '((:cjk-regular "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc"
                  "/System/Library/Fonts/Hiragino Sans GB.ttc"
                  "/System/Library/Fonts/STHeiti Light.ttc")
    (:cjk-bold "/System/Library/Fonts/ヒラギノ角ゴシック W6.ttc"
               "/System/Library/Fonts/Hiragino Sans GB.ttc"
               "/System/Library/Fonts/STHeiti Medium.ttc")
    (:braille "/System/Library/Fonts/Apple Braille.ttf"))
  "macOS fonts used for each role when the bundled font for it is missing, as
in the packaged Lem.app, which leaves the large Noto CJK fonts out.  Each
role lists candidates in order; the first that exists is used.")

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
  ;; The CJK fonts at CJK-NARROW-SIZE, where a full-width glyph is one cell
  ;; wide: the latin fonts' fallbacks for symbols only the CJK font has.
  cjk-narrow-normal-font
  cjk-narrow-bold-font
  cjk-narrow-size
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
  ;; Pointer address of each text font -> (char-code -> (ASCENT . OWN-P));
  ;; memoizes glyph-source.  Two levels so that lookups don't cons a key.
  (glyph-sources (make-hash-table))
  ;; The same font files opened for shaping (lem-sdl3/shaper), or NIL when
  ;; FreeType or HarfBuzz is missing.
  (shaping-fonts '())
  ;; Pointer address of each text font -> the shaping fonts to draw its
  ;; text with, in fallback order; see font-shaping-chain.
  (shaping-chains (make-hash-table)))

(defun save-font-size (font-config &optional (ratio 1))
  (setf (lem:config :sdl2-font-size)
        (round (/ (font-config-size font-config) ratio))))

(defgeneric platform-default-font-file (platform role)
  (:documentation "Return an existing font file PLATFORM provides for ROLE
(:cjk-regular, :cjk-bold or :braille), or NIL.  Used when the bundled font
for ROLE is missing."))

(defmethod platform-default-font-file (platform role)
  (declare (ignore platform role))
  nil)

(defmethod platform-default-font-file ((platform lem-sdl3/platform:mac) role)
  (find-if #'probe-file (rest (assoc role *mac-default-font-files*))))

(defun default-font-file (bundled-file role latin-file)
  "Return the font file to use for ROLE when none is configured: BUNDLED-FILE
if it exists, else the platform's font for ROLE, else LATIN-FILE, so that a
build without the bundled font still starts."
  (or (find-resource-pathname bundled-file)
      (platform-default-font-file (lem-sdl3/platform:get-platform) role)
      latin-file))

(defun default-emoji-font-file (latin-file)
  "Return the colour emoji font to use when none is configured: the first of
*system-emoji-font-files* that exists, else the bundled Noto Color Emoji,
else LATIN-FILE (no colour emoji, but Lem still starts)."
  (or (find-if #'probe-file *system-emoji-font-files*)
      (find-resource-pathname "resources/fonts/NotoColorEmoji.ttf")
      latin-file))

(defun make-font-config (&key (size (lem:config :sdl2-font-size *default-font-size*))
                              latin-normal-file
                              latin-bold-file
                              cjk-normal-file
                              cjk-bold-file
                              emoji-file
                              brail-file
                              (fallback-files (lem:config :sdl3-fallback-fonts))
                              (system-fallback (lem:config :sdl3-system-fallback-fonts t)))
  ;; The mono fonts always ship; the CJK, emoji and braille ones may be left
  ;; out of a packaged build (see *mac-default-font-files*).  Defaults are
  ;; computed only when nothing is configured, so a user's font settings
  ;; never cost a file lookup.
  (let* ((latin-normal-file
           (or latin-normal-file
               (lem:config :sdl2-normal-font)
               (get-resource-pathname "resources/fonts/NotoSansMono-Regular.ttf")))
         (latin-bold-file
           (or latin-bold-file
               (lem:config :sdl2-bold-font)
               (get-resource-pathname "resources/fonts/NotoSansMono-Bold.ttf"))))
    (%make-font-config
     :size (lem:config :font-size (or size *default-font-size*))
     :latin-normal-file latin-normal-file
     :latin-bold-file latin-bold-file
     :cjk-normal-file (or cjk-normal-file
                          (lem:config :sdl2-cjk-normal-font)
                          (default-font-file "resources/fonts/NotoSansCJK-Regular.ttc"
                                             :cjk-regular latin-normal-file))
     :cjk-bold-file (or cjk-bold-file
                        (lem:config :sdl2-cjk-bold-font)
                        (default-font-file "resources/fonts/NotoSansCJK-Bold.ttc"
                                           :cjk-bold latin-bold-file))
     :emoji-file (or emoji-file
                     (lem:config :sdl2-emoji-font)
                     (default-emoji-font-file latin-normal-file))
     :braille-file (or brail-file
                       (default-font-file "resources/fonts/FreeMono.ttf"
                                          :braille latin-normal-file))
     :fallback-files fallback-files
     :system-fallback system-fallback)))

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

(defun cjk-narrow-size (cjk-font size char-width)
  "Return the point size at which CJK-FONT, open at SIZE, draws a
full-width glyph CHAR-WIDTH pixels wide, or SIZE if it can't tell."
  (let ((advance (nth-value 4 (lem-sdl3/ffi:glyph-metrics cjk-font #\一))))
    (if (and advance (< char-width advance))
        (* size (/ char-width advance))
        size)))

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
        (let* ((sources (or (gethash address (font-glyph-sources font))
                            (setf (gethash address (font-glyph-sources font))
                                  (make-hash-table))))
               (code (char-code character))
               (source (or (gethash code sources)
                           (setf (gethash code sources)
                                 (let ((link (or (find-if (lambda (link)
                                                            (lem-sdl3/ffi:font-has-glyph-p
                                                             (car link) character))
                                                          chain)
                                                 (first chain))))
                                   (cons (cdr link) (eq link (first chain))))))))
          (values (car source) (cdr source))))))

(defun font-shaping-chain (font ttf-font)
  "Return the shaping fonts for text drawn with TTF-FONT, one of FONT's text
fonts: the same file first, then the fonts SDL_ttf falls back to for it,
in the same order (colour emoji excepted).  Return NIL for other fonts, or
when shaping is not available."
  (gethash (cffi:pointer-address ttf-font) (font-shaping-chains font)))

(defun open-shaping-fonts (font font-config fallback-files)
  "Open FONT's text font files and FALLBACK-FILES for shaping and record
each text font's chain.  If FreeType or HarfBuzz is missing, or any file
fails to open, FONT gets no chains and text is drawn as before."
  (when (and (lem-sdl3/ffi-freetype:available-p) (lem-sdl3/ffi-harfbuzz:available-p))
    (let ((opened '()))
      (flet ((open-file (file &optional (size (font-config-size font-config)))
               (let ((shaping-font (lem-sdl3/shaper:open-shaping-font file size)))
                 (push shaping-font opened)
                 shaping-font)))
        (handler-case
            (let* ((latin-normal (open-file (font-config-latin-normal-file font-config)))
                   (latin-bold (open-file (font-config-latin-bold-file font-config)))
                   (cjk-normal (open-file (font-config-cjk-normal-file font-config)))
                   (cjk-bold (open-file (font-config-cjk-bold-file font-config)))
                   (cjk-narrow-normal (open-file (font-config-cjk-normal-file font-config)
                                                 (font-cjk-narrow-size font)))
                   (cjk-narrow-bold (open-file (font-config-cjk-bold-file font-config)
                                               (font-cjk-narrow-size font)))
                   (braille (open-file (font-config-braille-file font-config)))
                   (fallbacks (mapcar #'open-file fallback-files)))
              (loop :for (ttf-font . chain)
                      :in (list (list* (font-latin-normal-font font)
                                       latin-normal (append fallbacks (list cjk-narrow-normal)))
                                (list* (font-latin-bold-font font)
                                       latin-bold (append fallbacks (list cjk-narrow-bold)))
                                (list* (font-cjk-normal-font font) cjk-normal fallbacks)
                                (list* (font-cjk-bold-font font) cjk-bold fallbacks)
                                (list* (font-braille-font font) braille fallbacks))
                    :do (setf (gethash (cffi:pointer-address ttf-font) (font-shaping-chains font))
                              chain))
              (setf (font-shaping-fonts font) opened))
          (error ()
            (mapc #'lem-sdl3/shaper:close-shaping-font opened)
            (clrhash (font-shaping-chains font))))))))

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

(defun fallback-font-files (font-config)
  "Return the existing fallback font files FONT-CONFIG names: configured
ones, then the system's."
  (remove-if-not #'probe-file
                 (append (font-config-fallback-files font-config)
                         (and (font-config-system-fallback font-config)
                              (system-fallback-font-files)))))

(defun open-fallback-fonts (font-config)
  "Open the fallback font files FONT-CONFIG names (configured ones, then the
system's) at its size, skipping files that fail to open."
  (loop :for file :in (fallback-font-files font-config)
        :for font := (handler-case (lem-sdl3/ffi:open-font file (font-config-size font-config))
                       (error () nil))
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
         (char-width (first (get-character-size latin-normal-font)))
         (cjk-narrow-size (cjk-narrow-size cjk-normal-font (font-config-size font-config)
                                           char-width))
         (cjk-narrow-normal-font (lem-sdl3/ffi:open-font (font-config-cjk-normal-file font-config)
                                                         cjk-narrow-size))
         (cjk-narrow-bold-font (lem-sdl3/ffi:open-font (font-config-cjk-bold-file font-config)
                                                       cjk-narrow-size))
         (fallback-fonts (open-fallback-fonts font-config))
         ;; Measured before fallbacks are added: TTF_FontHasGlyph counts
         ;; fallbacks, and the band must come from the latin font itself.
         (latin-normal-box-band (box-drawing-band latin-normal-font))
         (latin-bold-box-band (box-drawing-band latin-bold-font)))
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
                            (append fallback-fonts (list cjk-narrow-normal-font emoji-font)))
                      (cons latin-bold-font
                            (append fallback-fonts (list cjk-narrow-bold-font emoji-font)))
                      (cons cjk-normal-font (append fallback-fonts (list emoji-font)))
                      (cons cjk-bold-font (append fallback-fonts (list emoji-font))))))
          ;; Lem picks one font per character type, so a glyph missing from it
          ;; used to draw as an empty box.  SDL_ttf only consults a font's own
          ;; fallbacks, not theirs, so each text font gets a flat list: the
          ;; configured and system fonts, the CJK font (for the latin fonts),
          ;; then emoji.  The latin fonts only draw one-cell characters (wide
          ;; ones use the CJK font), so they try the configured and system
          ;; fonts, monospace first, before the CJK font, whose symbols such
          ;; as ★ and ⌘ are drawn full-width.  For the symbols only the CJK
          ;; font has, they use it at the narrow size, where those fit one
          ;; cell.  The CJK, emoji and braille fonts are only borrowed here;
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
                                   :cjk-narrow-normal-font cjk-narrow-normal-font
                                   :cjk-narrow-bold-font cjk-narrow-bold-font
                                   :cjk-narrow-size cjk-narrow-size
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
              (open-shaping-fonts font font-config (fallback-font-files font-config))
              font)))))))

(defun close-font (font)
  (mapc #'lem-sdl3/shaper:close-shaping-font (font-shaping-fonts font))
  (lem-sdl3/ffi:close-font (font-latin-normal-font font))
  (lem-sdl3/ffi:close-font (font-latin-bold-font font))
  (lem-sdl3/ffi:close-font (font-cjk-normal-font font))
  (lem-sdl3/ffi:close-font (font-cjk-bold-font font))
  (lem-sdl3/ffi:close-font (font-emoji-font font))
  (lem-sdl3/ffi:close-font (font-braille-font font))
  (lem-sdl3/ffi:close-font (font-cjk-narrow-normal-font font))
  (lem-sdl3/ffi:close-font (font-cjk-narrow-bold-font font))
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
  ;; Core Text, not fc-list: fontconfig is only there if Homebrew put it
  ;; there, and a packaged Lem.app must find fonts on any Mac.
  (lem-darwin/fonts:available-font-files))

(defmethod get-font-list ((platform lem-sdl3/platform:windows))
  (loop :for file :in (lem:directory-files "C:/Windows/Fonts/*.[otOT][tT][fF]")
        :collect (uiop:native-namestring file)))
