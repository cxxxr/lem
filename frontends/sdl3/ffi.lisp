(defpackage :lem-sdl3/ffi
  (:use :cl)
  (:export
   ;; misc
   :get-clipboard-text
   :platform-name
   :sdl-error
   ;; main thread
   :call-in-main-thread
   :in-main-thread
   :run-pending-main-thread-tasks
   :call-with-main-thread-tasks-drained
   :notify-main-thread-work
   :wait-for-main-thread-work
   :running-main-thread-task-p
   :wakeup-event-type
   ;; surfaces
   :surface
   :surface-pointer
   :surface-width
   :surface-height
   :surface-live-p
   :destroy-surface
   :load-image
   ;; fonts
   :open-font
   :close-font
   :font-ascent
   :font-descent
   :font-has-glyph-p
   :add-fallback-font
   :copy-font
   :glyph-metrics
   :render-text-blended
   :render-text-solid
   ;; text engine
   :create-text-engine
   :destroy-text-engine
   :create-text
   :destroy-text
   :set-text-color
   :draw-text
   ;; textures
   :create-target-texture
   :create-texture-from-surface
   :create-glyph-texture
   :update-texture
   :render-geometry
   :destroy-texture
   :get-render-target
   :set-render-target
   ;; rectangles
   :with-frect
   :make-frect
   :free-frect
   :set-frect
   ;; drawing
   :set-render-draw-color
   :render-clear
   :render-present
   :render-fill-rect
   :render-rect
   :render-line
   :render-lines
   :render-point
   :render-points
   :render-texture
   ;; sizes
   :get-render-output-size
   :get-window-size
   ;; application and windows
   :with-float-traps-masked
   :init-video
   :quit
   :set-hint
   :create-window
   :destroy-window
   :create-renderer
   :destroy-renderer
   :set-render-viewport
   :set-window-icon
   :get-window-title
   :set-window-title
   :window-fullscreen-p
   :set-window-fullscreen
   :window-borderless-p
   :set-window-bordered
   :maximize-window
   :minimize-window
   :cocoa-window
   :add-event-watch
   :texture-size
   :remove-event-watch
   :start-text-input
   :set-text-input-area
   :get-mouse-state
   :show-cursor
   :hide-cursor
   :set-clipboard-text))
(in-package :lem-sdl3/ffi)

;;; Thin layer between the frontend and cl-sdl3.
;;;
;;; cl-sdl3's DEFWRAP-FUN wrappers copy CLOS objects into fresh foreign memory
;;; on every call, which is too slow for per-glyph rendering, and the raw
;;; functions behind them are not exported.  The hot drawing calls are
;;; therefore bound here with plain pointer arguments.  Everything else is
;;; delegated to cl-sdl3, except where its binding doesn't fit: the clipboard
;;; text, which SDL3:GET-CLIPBOARD-TEXT returns without freeing SDL's buffer.
;;;
;;; Coordinates are accepted as any REAL and converted to single floats,
;;; since SDL3's renderer API is float-based.

(defmacro f (x)
  `(coerce ,x 'single-float))

;;; Misc

(cffi:defcfun ("SDL_free" %sdl-free) :void
  (pointer :pointer))

(cffi:defcfun ("SDL_GetClipboardText" %get-clipboard-text) :pointer)

(defun get-clipboard-text ()
  "Return the clipboard contents as a string, or NIL when it is empty.
Unlike SDL3:GET-CLIPBOARD-TEXT, this frees the buffer SDL allocates."
  (let ((pointer (%get-clipboard-text)))
    (unless (cffi:null-pointer-p pointer)
      (unwind-protect
           (let ((text (cffi:foreign-string-to-lisp pointer :encoding :utf-8)))
             (if (string= text "") nil text))
        (%sdl-free pointer)))))

(defun platform-name ()
  "Return the platform name reported by SDL, e.g. \"macOS\", \"Linux\" or \"Windows\"."
  (sdl3:get-platform))

(defun sdl-error (format-control &rest format-arguments)
  "Signal an error whose message ends with SDL's last error string."
  (error "~?: ~A" format-control format-arguments (sdl3:get-error)))

;;; Main thread
;;;
;;; The task queue and its flags are global, not passed as arguments,
;;; because they are shared between threads: the editor thread queues tasks
;;; and the main thread runs them, from SDL callbacks that take no Lisp
;;; arguments.  All of it is read and written with *main-thread-tasks-lock*
;;; held.

(defvar *main-thread-tasks* (make-hash-table))
(defvar *main-thread-tasks-lock* (bt2:make-lock :name "lem-sdl3 main thread tasks"))
(defvar *main-thread-task-counter* 0)
(defvar *draining-main-thread-tasks* nil
  "True while the main thread runs queued tasks itself, from code that keeps
it from pumping events (see call-with-main-thread-tasks-drained).  Read and
written only with *main-thread-tasks-lock* held.")
(defvar *main-thread-work* (bt2:make-semaphore :name "lem-sdl3 main thread work")
  "Signaled, while the main thread drains the queue itself, each time a task
is queued or something it waits for may be done (see notify-main-thread-work),
so that it can sleep until then instead of polling.")
(defvar *running-main-thread-task* nil
  "True on the main thread while it runs a task another thread is waiting
for (see call-in-main-thread).")

(defun register-main-thread-task (function)
  "Queue FUNCTION for the main thread and return its id, and whether the main
thread is draining the queue itself, so nothing else needs to run it."
  (multiple-value-bind (id draining)
      (bt2:with-lock-held (*main-thread-tasks-lock*)
        (let ((id (incf *main-thread-task-counter*)))
          (setf (gethash id *main-thread-tasks*) function)
          (values id *draining-main-thread-tasks*)))
    (when draining
      (notify-main-thread-work))
    (values id draining)))

(defun take-main-thread-task (id)
  (bt2:with-lock-held (*main-thread-tasks-lock*)
    (prog1 (gethash id *main-thread-tasks*)
      (remhash id *main-thread-tasks*))))

(defmacro with-float-traps-masked (&body body)
  #+sbcl
  `(sb-int:with-float-traps-masked (:underflow :overflow :inexact :invalid :divide-by-zero)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun run-main-thread-task-function (task)
  "Run TASK, a function queued by call-in-main-thread, on the main thread."
  (let ((*running-main-thread-task* t))
    (with-float-traps-masked
      (funcall task))))

(cffi:defcallback run-main-thread-task :void ((userdata :pointer))
  ;; Runs inside SDL's event processing on the main thread.  The task never
  ;; unwinds through this C frame: it captures its own values or error.  It
  ;; is gone if run-pending-main-thread-tasks already ran it.
  (let ((task (take-main-thread-task (cffi:pointer-address userdata))))
    (when task
      (run-main-thread-task-function task))))

(defun running-main-thread-task-p ()
  "True if the main thread is running a task another thread waits for, so
that thread cannot do anything else until the task returns."
  *running-main-thread-task*)

(defun call-with-main-thread-tasks-drained (function)
  "Call FUNCTION on the main thread while it runs queued tasks itself (with
run-pending-main-thread-tasks): other threads queue tasks without waking it
through SDL, and FUNCTION can sleep until there is work with
wait-for-main-thread-work.  Tasks queued before draining ends are run before
returning."
  ;; Forget wake-ups left over from an earlier drain.
  (loop :while (bt2:wait-on-semaphore *main-thread-work* :timeout 0))
  (bt2:with-lock-held (*main-thread-tasks-lock*)
    (setf *draining-main-thread-tasks* t))
  (unwind-protect (funcall function)
    (bt2:with-lock-held (*main-thread-tasks-lock*)
      (setf *draining-main-thread-tasks* nil))
    (run-pending-main-thread-tasks)))

(defun notify-main-thread-work ()
  "Wake the main thread if it sleeps in wait-for-main-thread-work: a task was
queued, or something it waits for may be done."
  (bt2:signal-semaphore *main-thread-work*))

(defun wait-for-main-thread-work (timeout)
  "Sleep on the main thread, while it drains the queue itself, until another
thread calls notify-main-thread-work or TIMEOUT seconds pass.  A notification
that came since the last wait returns at once, so none is missed between
checking for work and calling this."
  (bt2:wait-on-semaphore *main-thread-work* :timeout timeout))

(defun run-pending-main-thread-tasks ()
  "Run every task queued by call-in-main-thread that has not run yet, on the
main thread, and return how many ran.  For code that keeps the main thread
from pumping events, such as a platform's live-resize loop, while it waits
on another thread that queues tasks."
  (let ((tasks (bt2:with-lock-held (*main-thread-tasks-lock*)
                 (loop :for id :being :the :hash-keys :of *main-thread-tasks*
                         :using (:hash-value task)
                       :collect (cons id task)))))
    (loop :for (id . nil) :in (sort tasks #'< :key #'car)
          :for task := (take-main-thread-task id)
          :when task
            :do (run-main-thread-task-function task)
          :count task)))

(defvar *wakeup-event-type* nil
  "SDL event type registered for waking the main thread's event loop.")

(defun wakeup-event-type ()
  "Return the event type used to wake the event loop, registering it on
first use."
  (or *wakeup-event-type*
      (setf *wakeup-event-type* (sdl3:register-events 1))))

(defun wake-main-thread ()
  "Push an event so a main thread blocked in SDL_WaitEvent returns and runs
the queued main-thread callbacks."
  (cffi:with-foreign-object (event :uint8 128)
    (dotimes (i 128) (setf (cffi:mem-aref event :uint8 i) 0))
    (setf (cffi:mem-ref event :uint32 0) (wakeup-event-type))
    (sdl3:push-event event)))

(defun call-in-main-thread (function)
  "Call FUNCTION on SDL's main thread, wait for it, and return its values.
Runs FUNCTION directly when already on the main thread.  An error signaled by
FUNCTION is re-signaled in the calling thread.

SDL runs SDL_RunOnMainThread callbacks only while pumping events, and does
not reliably wake a main thread blocked in SDL_WaitEvent.  So the callback is
queued without SDL's own wait, a wake-up event is pushed, and the caller
blocks on a Lisp semaphore until the callback has run."
  (if (sdl3:is-main-thread)
      (funcall function)
      (let* ((values nil)
             (condition nil)
             (done (bt2:make-semaphore :name "lem-sdl3 main thread call")))
        (multiple-value-bind (id draining)
            (register-main-thread-task
             (lambda ()
               (unwind-protect
                    (handler-case (setf values (multiple-value-list (funcall function)))
                      (error (c) (setf condition c)))
                 (bt2:signal-semaphore done))))
          ;; While the main thread drains the queue itself it runs the task
          ;; directly; it may be inside an SDL event watch, where pushing the
          ;; wake-up event would block on SDL's watch lock until it returns.
          (unless draining
            (unless (sdl3:run-on-main-thread (cffi:callback run-main-thread-task)
                                             (cffi:make-pointer id)
                                             nil)
              (take-main-thread-task id)
              (sdl-error "SDL_RunOnMainThread failed"))
            (wake-main-thread)))
        (bt2:wait-on-semaphore done)
        (when condition
          (error condition))
        (values-list values))))

(defmacro in-main-thread (() &body body)
  "Evaluate BODY on SDL's main thread and return its values.
See CALL-IN-MAIN-THREAD."
  `(call-in-main-thread (lambda () ,@body)))

;;; Surfaces

(defstruct (surface (:constructor %make-surface (pointer width height)))
  "A Lisp handle for an SDL_Surface.  Keeping a stable Lisp object (rather than
a raw pointer, which SBCL may re-box) makes EQ-keyed caches reliable, and the
dimensions are read once instead of on every draw.  POINTER is NIL once the
surface has been destroyed."
  pointer
  (width 0 :type fixnum)
  (height 0 :type fixnum))

(defun wrap-surface (pointer)
  (unless (cffi:null-pointer-p pointer)
    (%make-surface pointer
                   (cffi:foreign-slot-value pointer '(:struct sdl3:surface) 'sdl3:%w)
                   (cffi:foreign-slot-value pointer '(:struct sdl3:surface) 'sdl3:%h))))

(defun surface-live-p (surface)
  "Return true if SURFACE has not been destroyed."
  (and surface (surface-pointer surface) t))

(defun destroy-surface (surface)
  "Free SURFACE's pixels.  Safe to call more than once."
  (alexandria:when-let ((pointer (surface-pointer surface)))
    (setf (surface-pointer surface) nil)
    (sdl3:destroy-surface pointer))
  (values))

(defun load-image (pathname)
  "Load the image file PATHNAME into a new SURFACE."
  (or (wrap-surface (sdl3-image:img-load (uiop:native-namestring pathname)))
      (sdl-error "Failed to load image ~A" pathname)))

;;; Fonts

(defun open-font (pathname size)
  "Open the font file PATHNAME at point SIZE and return the TTF_Font pointer."
  (let ((font (sdl3-ttf:open-font (uiop:native-namestring pathname) (f size))))
    (when (cffi:null-pointer-p font)
      (sdl-error "Failed to open font ~A" pathname))
    font))

(defun copy-font (font)
  "Return a new TTF_Font pointer with FONT's file, size and style, but none
of its fallback fonts.  Close it with CLOSE-FONT."
  (let ((copy (sdl3-ttf:copy-font font)))
    (when (cffi:null-pointer-p copy)
      (sdl-error "Failed to copy font"))
    copy))

(defun close-font (font)
  "Close FONT, a pointer returned by OPEN-FONT or COPY-FONT."
  (sdl3-ttf:close-font font))

(defun font-ascent (font)
  "Return FONT's ascent in pixels (positive)."
  (sdl3-ttf:get-font-ascent font))

(defun font-descent (font)
  "Return FONT's descent in pixels (negative, per SDL_ttf)."
  (sdl3-ttf:get-font-descent font))

(defun font-has-glyph-p (font character)
  "Return true if FONT provides a glyph for CHARACTER."
  (sdl3-ttf:font-has-glyph font (char-code character)))

(defun add-fallback-font (font fallback)
  "Make SDL_ttf draw glyphs FONT lacks with FALLBACK.  Fallbacks are tried in
the order added, and only one level deep: FALLBACK's own fallbacks are not
consulted.  FALLBACK must stay open until FONT is closed.  Return true on
success."
  (sdl3-ttf:add-fallback-font font fallback))

(defun glyph-metrics (font character)
  "Return CHARACTER's metrics in FONT as values MINX MAXX MINY MAXY ADVANCE
(pixels; Y up from the baseline), or NIL if the font lacks the glyph."
  (multiple-value-bind (ok minx maxx miny maxy advance)
      (sdl3-ttf:get-glyph-metrics font (char-code character))
    (when ok
      (values minx maxx miny maxy advance))))

(defun opaque-color (red green blue)
  "Return an opaque SDL3:COLOR.  SDL2_ttf ignored the alpha Lem passed (0);
SDL3_ttf honours it, so text must be drawn with alpha 255."
  (make-instance 'sdl3:color :%r red :%g green :%b blue :%a 255))

(defun render-text-blended (font string red green blue)
  "Render STRING with FONT in the given opaque color, anti-aliased.
Return a new SURFACE, or NIL if SDL_ttf produced nothing (e.g. an empty string)."
  (wrap-surface (sdl3-ttf:render-text-blended font string 0
                                              (opaque-color red green blue))))

(defun render-text-solid (font string red green blue)
  "Render STRING with FONT in the given color without anti-aliasing.
Return a new SURFACE, or NIL."
  (wrap-surface (sdl3-ttf:render-text-solid font string 0
                                            (opaque-color red green blue))))

;;; Text engine
;;;
;;; SDL_ttf's renderer text engine rasterizes glyphs into a texture atlas
;;; and draws a TTF_Text as textured quads.  cl-sdl3 binds these with plain
;;; DEFCFUNs, which take pointers without allocating, so they are used
;;; directly.

(defun create-text-engine (renderer)
  "Create a text engine drawing to RENDERER.  Destroy every text created
with it before calling DESTROY-TEXT-ENGINE."
  (let ((engine (sdl3-ttf:create-renderer-text-engine renderer)))
    (when (cffi:null-pointer-p engine)
      (sdl-error "TTF_CreateRendererTextEngine failed"))
    engine))

(defun destroy-text-engine (engine)
  "Destroy ENGINE, made by CREATE-TEXT-ENGINE."
  (sdl3-ttf:destroy-renderer-text-engine engine))

(defun create-text (engine font string)
  "Return a new TTF_Text drawing STRING with FONT through ENGINE, or NIL if
SDL_ttf fails.  FONT must stay open until the text is destroyed."
  (let ((text (sdl3-ttf:create-text engine font string 0)))
    (unless (cffi:null-pointer-p text)
      text)))

(defun destroy-text (text)
  "Destroy TEXT, made by CREATE-TEXT."
  (sdl3-ttf:destroy-text text))

(defun set-text-color (text red green blue)
  "Draw TEXT in the given opaque color from now on."
  (sdl3-ttf:set-text-color text red green blue 255))

(defun draw-text (text x y)
  "Draw TEXT with its top-left corner at (X, Y) on the current render target.
Each glyph sits on the baseline of the font that provides it, ascent pixels
below Y, as in a TTF_RenderText_Blended surface."
  (sdl3-ttf:draw-renderer-text text (f x) (f y)))

;;; Textures

(defun create-target-texture (renderer width height)
  "Create an opaque RGBA8888 render-target texture of WIDTH x HEIGHT pixels.
It uses nearest-neighbour sampling and blend mode NONE, matching SDL2's
defaults."
  (let ((texture (sdl3:create-texture renderer :rgba8888 :target
                                      (max 1 (round width))
                                      (max 1 (round height)))))
    (when (cffi:null-pointer-p texture)
      (sdl-error "SDL_CreateTexture failed"))
    (sdl3:set-texture-scale-mode texture :nearest)
    (sdl3:set-texture-blend-mode texture '(:sdl-blendmode-none))
    texture))

(defun create-texture-from-surface (renderer surface)
  "Create a texture from SURFACE (a SURFACE struct) using nearest-neighbour
sampling.  The texture keeps SDL's default blend mode, so alpha is honoured."
  (let ((texture (sdl3:create-texture-from-surface renderer (surface-pointer surface))))
    (when (cffi:null-pointer-p texture)
      (sdl-error "SDL_CreateTextureFromSurface failed"))
    (sdl3:set-texture-scale-mode texture :nearest)
    texture))

(cffi:defcfun ("SDL_UpdateTexture" %update-texture) :bool
  (texture :pointer) (rect :pointer) (pixels :pointer) (pitch :int))

(cffi:defcfun ("SDL_RenderGeometry" %render-geometry) :bool
  (renderer :pointer) (texture :pointer)
  (vertices :pointer) (num-vertices :int)
  (indices :pointer) (num-indices :int))

(defun create-glyph-texture (renderer width height)
  "Create a streaming ARGB8888 texture of WIDTH x HEIGHT pixels for glyph
coverage: alpha-blended and sampled nearest-neighbour, since glyphs are
drawn at whole pixels and their natural size."
  (let ((texture (sdl3:create-texture renderer :argb8888 :streaming width height)))
    (when (cffi:null-pointer-p texture)
      (sdl-error "SDL_CreateTexture failed"))
    (sdl3:set-texture-scale-mode texture :nearest)
    (sdl3:set-texture-blend-mode texture '(:sdl-blendmode-blend))
    texture))

(defun update-texture (texture x y width height pixels pitch)
  "Copy PIXELS, WIDTH x HEIGHT pixels PITCH bytes apart, into TEXTURE at
\(X, Y)."
  (cffi:with-foreign-object (rect :int 4)
    (setf (cffi:mem-aref rect :int 0) x
          (cffi:mem-aref rect :int 1) y
          (cffi:mem-aref rect :int 2) width
          (cffi:mem-aref rect :int 3) height)
    (%update-texture texture rect pixels pitch)))

(defun render-geometry (renderer texture vertices vertex-count indices index-count)
  "Draw triangles textured with TEXTURE: VERTICES is an array of
VERTEX-COUNT SDL_Vertex, INDICES of INDEX-COUNT ints, three per triangle."
  (%render-geometry renderer texture vertices vertex-count indices index-count))

(defun destroy-texture (texture)
  "Destroy TEXTURE."
  (sdl3:destroy-texture texture))

(defun get-render-target (renderer)
  "Return RENDERER's current target texture, or NIL for the window."
  (let ((texture (sdl3:get-render-target renderer)))
    (if (cffi:null-pointer-p texture) nil texture)))

(defun set-render-target (renderer texture)
  "Direct RENDERER's drawing at TEXTURE, or at the window when TEXTURE is NIL."
  (sdl3:set-render-target renderer (or texture (cffi:null-pointer))))

;;; Rectangles

(defun set-frect (pointer x y w h)
  "Store X Y W H into the SDL_FRect at POINTER and return POINTER."
  (setf (cffi:mem-aref pointer :float 0) (f x)
        (cffi:mem-aref pointer :float 1) (f y)
        (cffi:mem-aref pointer :float 2) (f w)
        (cffi:mem-aref pointer :float 3) (f h))
  pointer)

(defun make-frect (&optional (x 0) (y 0) (w 0) (h 0))
  "Allocate a heap SDL_FRect.  Free it with FREE-FRECT."
  (set-frect (cffi:foreign-alloc :float :count 4) x y w h))

(defun free-frect (pointer)
  "Free an SDL_FRect allocated by MAKE-FRECT."
  (cffi:foreign-free pointer))

(defmacro with-frect ((var x y w h) &body body)
  "Bind VAR to a stack-allocated SDL_FRect holding X Y W H during BODY."
  `(cffi:with-foreign-object (,var :float 4)
     (set-frect ,var ,x ,y ,w ,h)
     ,@body))

;;; Drawing

(cffi:defcfun ("SDL_RenderFillRect" %render-fill-rect) :bool
  (renderer :pointer)
  (rect :pointer))

(cffi:defcfun ("SDL_RenderRect" %render-rect) :bool
  (renderer :pointer)
  (rect :pointer))

(cffi:defcfun ("SDL_RenderTexture" %render-texture) :bool
  (renderer :pointer)
  (texture :pointer)
  (src :pointer)
  (dst :pointer))

(cffi:defcfun ("SDL_RenderLines" %render-lines) :bool
  (renderer :pointer)
  (points :pointer)
  (count :int))

(cffi:defcfun ("SDL_RenderPoints" %render-points) :bool
  (renderer :pointer)
  (points :pointer)
  (count :int))

(cffi:defcfun ("SDL_GetRenderOutputSize" %get-render-output-size) :bool
  (renderer :pointer)
  (w (:pointer :int))
  (h (:pointer :int)))

(defun set-render-draw-color (renderer red green blue &optional (alpha 255))
  "Set RENDERER's draw color.  ALPHA defaults to opaque."
  (sdl3:set-render-draw-color renderer red green blue alpha))

(defun render-clear (renderer)
  "Fill the current render target with the draw color."
  (sdl3:render-clear renderer))

(defun render-present (renderer)
  "Show everything drawn to the window since the last call."
  (sdl3:render-present renderer))

(defun render-fill-rect (renderer rect)
  "Fill RECT (an SDL_FRect pointer, or NIL for the whole target)."
  (%render-fill-rect renderer (or rect (cffi:null-pointer))))

(defun render-rect (renderer rect)
  "Outline RECT (an SDL_FRect pointer, or NIL for the whole target)."
  (%render-rect renderer (or rect (cffi:null-pointer))))

(defun render-texture (renderer texture source-rect dest-rect)
  "Copy TEXTURE to the render target.  Either rect may be NIL, meaning the
whole texture / whole target."
  (%render-texture renderer
                   texture
                   (or source-rect (cffi:null-pointer))
                   (or dest-rect (cffi:null-pointer))))

(defun render-line (renderer x1 y1 x2 y2)
  "Draw a line from (X1, Y1) to (X2, Y2)."
  (sdl3:render-line renderer (f x1) (f y1) (f x2) (f y2)))

(defun render-lines (renderer &rest coordinates)
  "Draw connected lines through the points given as X1 Y1 X2 Y2 ..."
  (let ((count (floor (length coordinates) 2)))
    (cffi:with-foreign-object (points :float (* 2 count))
      (loop :for value :in coordinates
            :for i :from 0
            :do (setf (cffi:mem-aref points :float i) (f value)))
      (%render-lines renderer points count))))

(defun render-point (renderer x y)
  "Draw a single point at (X, Y)."
  (sdl3:render-point renderer (f x) (f y)))

(defun render-points (renderer points)
  "Draw every point in POINTS, a sequence of (X . Y) conses."
  (let ((count (length points)))
    (cffi:with-foreign-object (array :float (* 2 count))
      (let ((i 0))
        (map nil (lambda (point)
                   (setf (cffi:mem-aref array :float i) (f (car point))
                         (cffi:mem-aref array :float (1+ i)) (f (cdr point)))
                   (incf i 2))
             points))
      (%render-points renderer array count))))

;;; Sizes

(defun get-render-output-size (renderer)
  "Return the output size of RENDERER in pixels as two values."
  (cffi:with-foreign-objects ((w :int) (h :int))
    (%get-render-output-size renderer w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

(defun get-window-size (window)
  "Return the size of WINDOW in window coordinates as two values."
  (multiple-value-bind (ok width height) (sdl3:get-window-size window)
    (declare (ignore ok))
    (values width height)))

;;; Application and windows

(cffi:defcfun ("SDL_SetRenderViewport" %set-render-viewport) :bool
  (renderer :pointer)
  (rect :pointer))

(defmacro with-int-rect ((var x y w h) &body body)
  "Bind VAR to a stack-allocated SDL_Rect (integer fields) during BODY."
  `(cffi:with-foreign-object (,var :int 4)
     (setf (cffi:mem-aref ,var :int 0) (round ,x)
           (cffi:mem-aref ,var :int 1) (round ,y)
           (cffi:mem-aref ,var :int 2) (round ,w)
           (cffi:mem-aref ,var :int 3) (round ,h))
     ,@body))

(defun init-video ()
  "Initialize SDL's video subsystem (and the events subsystem it implies)
and SDL_ttf."
  (unless (sdl3:init '(:video))
    (sdl-error "SDL_Init failed"))
  (unless (sdl3-ttf:init)
    (sdl-error "TTF_Init failed")))

(defun quit ()
  "Shut down SDL_ttf and SDL."
  (sdl3-ttf:quit)
  (sdl3:quit))

(defun set-hint (name value)
  "Set the SDL hint NAME to VALUE (a string)."
  (sdl3:set-hint name value))

(defun create-window (title width height)
  "Create a resizable, high-pixel-density window of WIDTH x HEIGHT (in window
coordinates)."
  (let ((window (sdl3:create-window title width height
                                   '(:resizable :high-pixel-density))))
    (when (cffi:null-pointer-p window)
      (sdl-error "SDL_CreateWindow failed"))
    window))

(defun destroy-window (window)
  "Destroy WINDOW."
  (sdl3:destroy-window window))

(defun create-renderer (window)
  "Create a renderer for WINDOW using SDL's preferred driver."
  (let ((renderer (sdl3:create-renderer window (cffi:null-pointer))))
    (when (cffi:null-pointer-p renderer)
      (sdl-error "SDL_CreateRenderer failed"))
    renderer))

(defun destroy-renderer (renderer)
  "Destroy RENDERER."
  (sdl3:destroy-renderer renderer))

(defun set-render-viewport (renderer &optional x y w h)
  "Restrict drawing to the rectangle X Y W H, or reset it when called with
no rectangle."
  (if x
      (with-int-rect (rect x y w h)
        (%set-render-viewport renderer rect))
      (%set-render-viewport renderer (cffi:null-pointer))))

(cffi:defcfun ("SDL_AddEventWatch" add-event-watch) :bool
  "Call the C callback FILTER with USERDATA and each event as it is added to
the queue, on the thread that adds it.  Return true on success."
  (filter :pointer)
  (userdata :pointer))

(cffi:defcfun ("SDL_GetTextureSize" %get-texture-size) :bool
  (texture :pointer)
  (w :pointer)
  (h :pointer))

(defun texture-size (texture)
  "Return TEXTURE's width and height in pixels."
  (cffi:with-foreign-objects ((w :float) (h :float))
    (%get-texture-size texture w h)
    (values (round (cffi:mem-ref w :float)) (round (cffi:mem-ref h :float)))))

(cffi:defcfun ("SDL_RemoveEventWatch" remove-event-watch) :void
  "Stop calling FILTER with USERDATA (see add-event-watch)."
  (filter :pointer)
  (userdata :pointer))

(defun cocoa-window (window)
  "Return the NSWindow behind WINDOW, as a foreign pointer, or NIL if
WINDOW is not a Cocoa window."
  (let ((props (sdl3:get-window-properties window)))
    (unless (zerop props)
      (let ((nswindow (sdl3:get-pointer-property props "SDL.window.cocoa.window"
                                                 (cffi:null-pointer))))
        (unless (cffi:null-pointer-p nswindow)
          nswindow)))))

(defun set-window-icon (window surface)
  "Use SURFACE (a SURFACE struct) as WINDOW's icon."
  (sdl3:set-window-icon window (surface-pointer surface)))

(defun get-window-title (window)
  "Return WINDOW's title."
  (sdl3:get-window-title window))

(defun set-window-title (window title)
  "Set WINDOW's title."
  (sdl3:set-window-title window title))

(defun window-fullscreen-p (window)
  "Return true if WINDOW is fullscreen."
  (and (member :fullscreen (sdl3:get-window-flags window)) t))

(defun set-window-fullscreen (window fullscreen-p)
  "Make WINDOW fullscreen or windowed.  SDL3 uses borderless desktop
fullscreen unless a display mode is set, matching SDL2's :desktop."
  (sdl3:set-window-fullscreen window (and fullscreen-p t)))

(defun window-borderless-p (window)
  "Return true if WINDOW has no decorations."
  (and (member :borderless (sdl3:get-window-flags window)) t))

(defun set-window-bordered (window bordered-p)
  "Show or hide WINDOW's decorations."
  (sdl3:set-window-bordered window (and bordered-p t)))

(defun maximize-window (window)
  "Maximize WINDOW."
  (sdl3:maximize-window window))

(defun minimize-window (window)
  "Minimize WINDOW."
  (sdl3:minimize-window window))

(defun start-text-input (window)
  "Start delivering text-input and IME events for WINDOW.  Unlike SDL2,
SDL3 does not enable text input by default."
  (sdl3:start-text-input window))

(defun set-text-input-area (window x y w h)
  "Tell the IME where text is being entered, in window coordinates."
  (sdl3:set-text-input-area window
                            (make-instance 'sdl3:rect :%x (round x) :%y (round y)
                                                      :%w (round w) :%h (round h))
                            0))

(defun get-mouse-state ()
  "Return the mouse position in window coordinates as two values."
  (multiple-value-bind (buttons x y) (sdl3:get-mouse-state)
    (declare (ignore buttons))
    (values x y)))

(defun show-cursor ()
  "Show the mouse cursor."
  (sdl3:show-cursor))

(defun hide-cursor ()
  "Hide the mouse cursor."
  (sdl3:hide-cursor))

(defun set-clipboard-text (text)
  "Put TEXT on the clipboard."
  (sdl3:set-clipboard-text text))
