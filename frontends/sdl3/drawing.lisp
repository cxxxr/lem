(defpackage :lem-sdl3/drawing
  (:use :cl
        :lem-core/display)
  (:local-nicknames (:display :lem-sdl3/display)
                    (:view :lem-sdl3/view)))
(in-package :lem-sdl3/drawing)

(defgeneric get-surface (drawing-object display))

(defmethod get-surface :around (drawing-object display)
  ;; A cached surface may have been freed by clear-text-surface-cache (font
  ;; change or cache overflow) while this drawing object was still alive.
  (let ((surface (text-object-surface drawing-object)))
    (if (lem-sdl3/ffi:surface-live-p surface)
        surface
        (setf (text-object-surface drawing-object)
              (call-next-method)))))

(defun make-text-surface (display string attribute type)
  "Render STRING to a new surface, or return NIL if SDL_ttf produces nothing
(e.g. for an empty string)."
  (let ((foreground (lem-core:attribute-foreground-with-reverse attribute)))
    (lem-sdl3/ffi:render-text-blended
     (or (lem-core:attribute-font attribute)
         (display:get-display-font display
                                   :type type
                                   :bold (and attribute (lem:attribute-bold attribute))
                                   :character (and (plusp (length string))
                                                   (char string 0))))
     string
     (lem:color-red foreground)
     (lem:color-green foreground)
     (lem:color-blue foreground))))

(defun make-text-surface-with-cache (display string attribute type)
  (or (lem-sdl3/text-surface-cache:get-text-surface-cache string attribute type)
      (let ((surface (make-text-surface display string attribute type)))
        (when surface
          (lem-sdl3/text-surface-cache:register-text-surface-cache string attribute type surface))
        surface)))

(defmethod get-surface ((drawing-object text-object) display)
  (let ((string (text-object-string drawing-object))
        (attribute (text-object-attribute drawing-object))
        (type (text-object-type drawing-object)))
    (when (and (lem-core:cursor-attribute-p attribute)
               (eq :box (display:display-cursor-type display)))
      (setf attribute
            (lem-core:merge-attribute
             attribute (lem-core:make-attribute
                        :foreground (lem-if:get-background-color (lem:implementation))))))
    (make-text-surface-with-cache display string attribute type)))

;;; icon-object: no specialized get-surface needed.
;;; Falls through to the text-object method which uses make-text-surface-with-cache.
;;; make-text-surface now passes :character to get-display-font, which finds the
;;; correct icon font via icon-font:icon-font. The surface is cached by
;;; (string, attribute, :icon) key, preventing orphaned textures.

(defmethod get-surface ((drawing-object folder-object) display)
  "Cache the folder PNG surface in the text-surface-cache.
Uses a sentinel key so it participates in the normal cache lifecycle
(cleared on font change, cleared by sweep-if-oversize)."
  (or (lem-sdl3/text-surface-cache:get-text-surface-cache
       "__folder_icon__" nil :folder)
      (let ((surface (lem-sdl3/ffi:load-image
                      (lem-sdl3/resource:get-resource-pathname
                       "resources/open-folder.png"))))
        (lem-sdl3/text-surface-cache:register-text-surface-cache
         "__folder_icon__" nil :folder surface)
        surface)))

(defgeneric object-width (drawing-object display))

(defmethod object-width ((drawing-object void-object) display)
  0)

(defun text-cell-width (drawing-object display)
  "Cell-aligned pixel width of a text-object: string-width × char-width.
Mirrors lem-ncurses/drawing-object:object-width semantics (logical
column width) so SDL text aligns on the character grid regardless of
per-string SDL_ttf surface-width drift."
  (* (lem-core:string-width (text-object-string drawing-object))
     (display:display-char-width display)))

(defmethod object-width ((drawing-object text-object) display)
  (text-cell-width drawing-object display))

(defmethod object-width ((drawing-object control-character-object) display)
  (* 2 (display:display-char-width display)))

(defmethod object-width ((drawing-object icon-object) display)
  ;; Cell-aligned advance (typically 2 * char-width). The icon font's natural
  ;; glyph surface is usually wider than this; draw-object scales it to fit.
  (text-cell-width drawing-object display))

(defmethod object-width ((drawing-object folder-object) display)
  (* 2 (display:display-char-width display)))

(defmethod object-width ((drawing-object emoji-object) display)
  (* (display:display-char-width display) 2 (length (text-object-string drawing-object))))

(defmethod object-width ((drawing-object eol-cursor-object) display)
  0)

(defmethod object-width ((drawing-object extend-to-eol-object) display)
  0)

(defmethod object-width ((drawing-object line-end-object) display)
  (text-cell-width drawing-object display))

(defmethod object-width ((drawing-object image-object) display)
  (or (image-object-width drawing-object)
      (lem-sdl3/ffi:surface-width (image-object-image drawing-object))))


(defgeneric object-height (drawing-object display))

(defmethod object-height ((drawing-object void-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object text-object) display)
  ;; Use the stable row cell-height (derived from font metrics at the
  ;; display level) rather than the per-string SDL_ttf surface height.
  ;; SDL_ttf can return slightly different surface heights for different
  ;; strings (e.g. ones containing descenders like `p'/`g'/`y' versus
  ;; ones without), which would otherwise leak into the background
  ;; rectangle drawn by `draw-text-glyph-surface', producing the
  ;; uneven-extent "padding around the problem letters" artefact at
  ;; attribute boundaries on a highlighted row.  The natural surface
  ;; height is still read inside `draw-text-glyph-surface' directly
  ;; from the surface for baseline-anchored glyph blitting.
  (display:display-char-height display))

(defmethod object-height ((drawing-object icon-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object control-character-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object folder-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object emoji-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object eol-cursor-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object extend-to-eol-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object line-end-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object image-object) display)
  (or (image-object-height drawing-object)
      (lem-sdl3/ffi:surface-height (image-object-image drawing-object))))

(defmethod lem-if:object-width ((implementation lem-sdl3/sdl3:sdl3) drawing-object)
  (display:with-display (display)
    (object-width drawing-object display)))

(defmethod lem-if:object-height ((implementation lem-sdl3/sdl3:sdl3) drawing-object)
  (display:with-display (display)
    (object-height drawing-object display)))

(defmethod draw-object ((drawing-object void-object) x bottom-y display view)
  0)

(defun draw-rect (display x y width height color)
  (display:with-scratch-rect (rect display x y width height)
    (display:set-render-color display color)
    (lem-sdl3/ffi:render-fill-rect (display:display-renderer display) rect)))

(defun draw-cursor (display x y surface-width surface-height background)
  (ecase (display:display-cursor-type display)
    (:box
     (draw-rect display x y surface-width surface-height background))
    (:bar
     (draw-rect display x y 1 surface-height background))
    (:underline
     (draw-rect display x (+ y surface-height -1) surface-width 1 background))))

(defun draw-text-glyph-surface (drawing-object x bottom-y display view cell-width
                                &key clip (phase :both))
  "Draws DRAWING-OBJECT's cached SDL surface in a (CELL-WIDTH × cell-height) slot
at (X, BOTTOM-Y). Cell-height is taken from the drawing-object's OBJECT-HEIGHT so
non-text surfaces (folder PNG, icon font, emoji font) get scaled to the editor's
character row height instead of being placed at their natural pixel height.

CLIP / PHASE control how the glyph and its background are composited:

* CLIP NIL (icon / folder / emoji): the surface is rendered into the cell with
  `render-texture' allowed to scale it down to fit, preventing oversized glyph
  surfaces from spilling into adjacent columns or rows.

* CLIP T (text characters): the surface is rendered at its natural pixel size.
  At small font sizes the NotoSansMono rasterizer produces some glyphs (`p',
  `g', `T', ...) whose blended surface-width is one pixel wider than the
  advance width — that overhang is anti-aliasing tail that must not be
  squished by `render-texture' (which is what made `P' and the other exact-
  width letters look visually different from their neighbours).

PHASE selects what to draw:
  :BG     — background fill + cursor box + underline only (no glyph).
  :GLYPH  — glyph blit only (no background / cursor / underline).
  :BOTH   — bg first, then glyph, then underline. Default.

The two-phase split lets a multi-character text run paint all backgrounds
first and then all glyphs, so a 1-pixel right-edge AA overhang from one glyph
is not erased by the next glyph's background fill.

When there is no surface (SDL_ttf renders nothing for an empty string) only
the background phase draws anything."
  (let* ((surface (get-surface drawing-object display))
         (surface-width (if surface (lem-sdl3/ffi:surface-width surface) 0))
         (surface-height (if surface (lem-sdl3/ffi:surface-height surface) 0))
         (cell-height (object-height drawing-object display))
         (attribute (text-object-attribute drawing-object))
         (background (lem-core:attribute-background-with-reverse attribute))
         (y (- bottom-y cell-height))
         (draw-width (if clip surface-width (min surface-width cell-width)))
         (draw-height (min surface-height cell-height)))
    (when (member phase '(:bg :both))
      (cond ((and attribute (lem-core:cursor-attribute-p attribute))
             (lem-sdl3/view:set-cursor-position view x y)
             (draw-cursor display x y cell-width cell-height background))
            (t
             (draw-rect display x y cell-width cell-height background))))
    (when (and surface (member phase '(:glyph :both)))
      (let ((texture (lem-sdl3/text-surface-cache:get-or-create-texture
                      (display:display-renderer display)
                      surface)))
        (cond
          (clip
           ;; Natural-size render with explicit baseline anchoring.  The
           ;; `render-text-blended' surface places the glyph baseline at
           ;; (ascent) pixels down from the surface top, regardless of the
           ;; specific glyph's bounding box.  We anchor the surface so the
           ;; baseline lands at the row's intrinsic baseline:
           ;;     screen_baseline_y = bottom_y - |descent|
           ;;     glyph_dst_y       = screen_baseline_y - ascent
           ;;                       = bottom_y - ascent - |descent|
           ;; This is equivalent to the SDL_ttf docs' per-glyph formula
           ;;     dst.y = targetBaselineY - TTF_FontAscent + glyph.maxy
           ;; once the (ascent - maxy) per-glyph offset already baked into
           ;; the blended surface is accounted for, but it uses font-level
           ;; metrics rather than the surface dimensions — so the baseline
           ;; locks even if a particular glyph's surface comes back with a
           ;; non-uniform height.  Any 1-px right-edge AA overhang is
           ;; allowed to land in the next cell column; the preceding
           ;; phase :bg sweep ensures the next cell's background does not
           ;; erase it.
           (let* ((ascent (display:display-font-ascent display))
                  (descent (display:display-font-descent display))
                  (glyph-y (- bottom-y ascent (abs descent))))
             (display:with-scratch-rect (dst-rect display x glyph-y
                                                  surface-width surface-height)
               (lem-sdl3/ffi:render-texture (display:display-renderer display)
                                            texture
                                            nil
                                            dst-rect))))
          (t
           (display:with-scratch-rect (dst-rect display x y draw-width draw-height)
             (lem-sdl3/ffi:render-texture (display:display-renderer display)
                                          texture
                                          nil
                                          dst-rect))))))
    (when (and (member phase '(:bg :both))
               attribute
               (lem:attribute-underline attribute))
      (display:render-line display
                           x
                           (1- (+ y cell-height))
                           (+ x cell-width)
                           (1- (+ y cell-height))
                           :color (let ((underline (lem:attribute-underline attribute)))
                                    (if (eq underline t)
                                        (lem-core:attribute-foreground-color attribute)
                                        (or (lem:parse-color underline)
                                            (lem-core:attribute-foreground-color attribute))))))))

(defun text-object-letter-objects-and-widths (drawing-object display)
  "Return two parallel lists: per-character letter-objects and their cell
widths, for the multi-character text run DRAWING-OBJECT."
  (let ((attribute (text-object-attribute drawing-object)))
    (loop :for c :across (text-object-string drawing-object)
          :for letter := (make-letter-object c attribute)
          :collect letter :into letters
          :collect (object-width letter display) :into widths
          :finally (return (values letters widths)))))

(defun draw-text-object-phase (drawing-object x bottom-y display view phase)
  "Render the text-object DRAWING-OBJECT for one of the two-pass phases
(:BG or :GLYPH).  PHASE :BG paints backgrounds, cursors and underlines for
every cell of the run; PHASE :GLYPH blits each glyph at its natural surface
width so the rasterizer's right-edge anti-aliasing tail is preserved."
  (let ((string (text-object-string drawing-object)))
    (cond ((<= (length string) 1)
           (draw-text-glyph-surface drawing-object x bottom-y display view
                                    (object-width drawing-object display)
                                    :clip t :phase phase))
          (t
           (multiple-value-bind (letter-objects letter-widths)
               (text-object-letter-objects-and-widths drawing-object display)
             (loop :with current-x := x
                   :for letter-object :in letter-objects
                   :for letter-width :in letter-widths
                   :do (draw-text-glyph-surface letter-object current-x bottom-y
                                                display view letter-width
                                                :clip t :phase phase)
                       (incf current-x letter-width)))))))

(defmethod draw-object ((drawing-object text-object) x bottom-y display view)
  ;; Render each character individually on the cell grid. SDL_ttf's blended
  ;; surface for a multi-character string has metrics that do not equal the
  ;; sum of its per-character metrics, so a glyph would otherwise land on a
  ;; different pixel column when the cursor (or any overlay) splits the run
  ;; into shorter segments. Per-character rendering eliminates that drift —
  ;; the same character always lands on the same pixel column regardless of
  ;; how the line is partitioned, matching ncurses semantics.
  ;;
  ;; This per-text-object two-pass preserves the AA overhang within the run.
  ;; The cross-text-object equivalent (an adjacent text-object's :bg erasing
  ;; the previous text-object's AA tail) is handled by `redraw-physical-line',
  ;; which lifts the two-pass to span the entire physical line.
  (let ((total-width (object-width drawing-object display)))
    (draw-text-object-phase drawing-object x bottom-y display view :bg)
    (draw-text-object-phase drawing-object x bottom-y display view :glyph)
    total-width))

(defmethod draw-object ((drawing-object icon-object) x bottom-y display view)
  ;; Icon font glyphs typically render much wider than the 2-cell column the
  ;; layout reserves for them; draw-text-glyph-surface scales them down to fit.
  (let ((cell-width (object-width drawing-object display)))
    (draw-text-glyph-surface drawing-object x bottom-y display view cell-width)
    cell-width))

(defmethod draw-object ((drawing-object folder-object) x bottom-y display view)
  ;; Folder PNG surface is much wider than 2 cells; render once and scale to fit.
  ;; Overrides the text-object per-character loop so the icon stays atomic.
  (let ((cell-width (object-width drawing-object display)))
    (draw-text-glyph-surface drawing-object x bottom-y display view cell-width)
    cell-width))

(defmethod draw-object ((drawing-object emoji-object) x bottom-y display view)
  ;; Emoji strings may span multiple codepoints (base + variation selector,
  ;; ZWJ sequences, ...) that must be rendered as one composed glyph. Use the
  ;; single-surface path with cell-aligned scaling so we neither split the
  ;; sequence per-codepoint nor let an oversized emoji surface spill over.
  (let ((cell-width (object-width drawing-object display)))
    (draw-text-glyph-surface drawing-object x bottom-y display view cell-width)
    cell-width))

(defmethod draw-object ((drawing-object eol-cursor-object) x bottom-y display view)
  (display:set-render-color display (eol-cursor-object-color drawing-object))
  (let ((y (- bottom-y (object-height drawing-object display))))
    (lem-sdl3/view:set-cursor-position view x y)
    (draw-cursor display
                 x
                 y
                 (display:display-char-width display)
                 (object-height drawing-object display)
                 (eol-cursor-object-color drawing-object)))
  (object-width drawing-object display))

(defmethod draw-object ((drawing-object extend-to-eol-object) x bottom-y display view)
  (display:set-render-color display (extend-to-eol-object-color drawing-object))
  (display:with-scratch-rect (rect display
                              x
                              (- bottom-y (display:display-char-height display))
                              (- (lem-if:view-width (lem-core:implementation) view) x)
                              (display:display-char-height display))
    (lem-sdl3/ffi:render-fill-rect (display:display-renderer display) rect))
  (object-width drawing-object display))

(defmethod draw-object ((drawing-object line-end-object) x bottom-y display view)
  (call-next-method drawing-object
                    (+ x
                       (* (line-end-object-offset drawing-object)
                          (display:display-char-width display)))
                    bottom-y
                    display
                    view))

(defmethod draw-object ((drawing-object image-object) x bottom-y display view)
  (let* ((surface-width (object-width drawing-object display))
         (surface-height (object-height drawing-object display))
         (texture (lem-sdl3/ffi:create-texture-from-surface (display:display-renderer display)
                                                            (image-object-image drawing-object)))
         (y (- bottom-y surface-height)))
    (display:with-scratch-rect (dest-rect display x y surface-width surface-height)
      (lem-sdl3/ffi:render-texture (display:display-renderer display)
                                   texture
                                   nil
                                   dest-rect))
    (lem-sdl3/ffi:destroy-texture texture)
    surface-width))

(defun plain-text-object-p (object)
  "True when OBJECT is an instance of the base `text-object' class (and not
one of its specialised subclasses `icon-object', `folder-object', or
`emoji-object', which need their own scale-to-fit `draw-object' method
rather than the row-wide two-pass)."
  (eq (class-of object) (find-class 'text-object)))

(defun redraw-physical-line (display view x y objects height)
  ;; Two-pass over the whole physical line: paint every plain text-object's
  ;; backgrounds first, then blit every plain text-object's glyphs.  This
  ;; preserves the 1-pixel right-edge AA tail at attribute boundaries (e.g.
  ;; on the dashboard's highlighted row, where a `p' or `g' at the end of
  ;; one attribute run would otherwise be eroded by the next text-object's
  ;; full-width background fill).  Everything else (icon / folder / emoji
  ;; text-object subclasses, images, eol-cursor, extend-to-eol) draws fully
  ;; in the first pass via its own `draw-object' method — those don't have
  ;; AA overhang to preserve and need their bespoke rendering (scale-to-fit
  ;; for icon/folder/emoji surfaces, fill for extend-to-eol, etc.).
  (let* ((bottom-y (+ y height))
         (display-width (round (* (display:display-window-width display)
                                  (first (display:display-scale display)))))
         (placed (loop :with current-x := x
                       :for object :in objects
                       :while (< current-x display-width)
                       :collect (cons object current-x)
                       :do (incf current-x (object-width object display)))))
    (flet ((draw-text-pass (object obj-x phase)
             ;; Honour the wrap-to-letters branch the old code used when a
             ;; text-object would extend past the display width.
             (cond ((< display-width
                       (+ obj-x (object-width object display)))
                    (loop :with current-x := obj-x
                          :for c :across (text-object-string object)
                          :while (< current-x display-width)
                          :for letter := (make-letter-object
                                          c (text-object-attribute object))
                          :for letter-width := (object-width letter display)
                          :do (draw-text-glyph-surface letter current-x bottom-y
                                                       display view letter-width
                                                       :clip t :phase phase)
                              (incf current-x letter-width)))
                   (t
                    (draw-text-object-phase object obj-x bottom-y
                                            display view phase)))))
      ;; Pass 1: plain-text backgrounds + everything else (subclassed text-
      ;; objects and non-text objects) in normal order.
      (loop :for (object . obj-x) :in placed
            :do (if (plain-text-object-p object)
                    (draw-text-pass object obj-x :bg)
                    (draw-object object obj-x bottom-y display view)))
      ;; Pass 2: plain-text glyphs.
      (loop :for (object . obj-x) :in placed
            :when (plain-text-object-p object)
              :do (draw-text-pass object obj-x :glyph)))))

(defun redraw-physical-line-from-behind (display view objects)
  (loop :with current-x := (lem-if:view-width (lem-core:implementation) view)
        :and y := (lem-if:view-height (lem-core:implementation) view)
        :for object :in objects
        :do (decf current-x (object-width object display))
            (draw-object object current-x y display view)))

(defun fill-to-end-of-line (display view x y height &optional default-attribute)
  (display:with-scratch-rect (rect display x y (- (lem-if:view-width (lem-core:implementation) view) x) height)
    (display:set-render-color display
                              (lem-core:attribute-background-color default-attribute))
    (lem-sdl3/ffi:render-fill-rect (display:display-renderer display) rect)))

(defmethod lem-if:render-line ((implementation lem-sdl3/sdl3:sdl3) view x y objects height)
  (display:with-display (display)
    (fill-to-end-of-line display view x y height)
    (redraw-physical-line display view x y objects height)))

(defmethod lem-if:render-line-on-modeline ((implementation lem-sdl3/sdl3:sdl3)
                                           view
                                           left-objects
                                           right-objects
                                           default-attribute
                                           height)
  (display:with-display (display)
    (fill-to-end-of-line display
                         view
                         0
                         (- (lem-if:view-height (lem-core:implementation) view) height)
                         height
                         default-attribute)
    (redraw-physical-line display
                          view
                          0
                          (- (lem-if:view-height (lem-core:implementation) view)
                             (display:display-char-height display))
                          left-objects
                          height)
    (redraw-physical-line-from-behind display view right-objects)))

(defmethod lem-if:clear-to-end-of-window ((implementation lem-sdl3/sdl3:sdl3) view y)
  (display:with-display (display)
    (display:set-render-color display
                              (display:display-background-color display))
    (display:with-scratch-rect (rect display
                                0
                                y
                                (lem-if:view-width implementation view)
                                (- (lem-if:view-height implementation view) y))
      (lem-sdl3/ffi:render-fill-rect (display:display-renderer display) rect))))

(defmethod lem-core:redraw-buffer :before ((implementation lem-sdl3/sdl3:sdl3) buffer window force)
  (display:with-display (display)
    (lem-sdl3/ffi:set-render-target (display:display-renderer display)
                                    (view:view-texture (lem:window-view window)))))

(defmethod lem-core:redraw-buffer :around ((implementation lem-sdl3/sdl3:sdl3) buffer window force)
  (lem-sdl3/ffi:in-main-thread ()
    (call-next-method)))
