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
   :render-text-blended
   :render-text-solid
   ;; textures
   :create-target-texture
   :create-texture-from-surface
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
   :render-texture
   ;; sizes
   :get-render-output-size
   :get-window-size))
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

(defvar *main-thread-tasks* (make-hash-table))
(defvar *main-thread-tasks-lock* (bt2:make-lock :name "lem-sdl3 main thread tasks"))
(defvar *main-thread-task-counter* 0)

(defun register-main-thread-task (function)
  (bt2:with-lock-held (*main-thread-tasks-lock*)
    (let ((id (incf *main-thread-task-counter*)))
      (setf (gethash id *main-thread-tasks*) function)
      id)))

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

(cffi:defcallback run-main-thread-task :void ((userdata :pointer))
  ;; Runs inside SDL's event processing on the main thread.  The task never
  ;; unwinds through this C frame: it captures its own values or error.
  (let ((task (take-main-thread-task (cffi:pointer-address userdata))))
    (when task
      (with-float-traps-masked
        (funcall task)))))

(defun call-in-main-thread (function)
  "Call FUNCTION on SDL's main thread, wait for it, and return its values.
Runs FUNCTION directly when already on the main thread.  An error signaled by
FUNCTION is re-signaled in the calling thread."
  (if (sdl3:is-main-thread)
      (funcall function)
      (let* ((values nil)
             (condition nil)
             (id (register-main-thread-task
                  (lambda ()
                    (handler-case (setf values (multiple-value-list (funcall function)))
                      (error (c) (setf condition c)))))))
        (unless (sdl3:run-on-main-thread (cffi:callback run-main-thread-task)
                                         (cffi:make-pointer id)
                                         t)
          (take-main-thread-task id)
          (sdl-error "SDL_RunOnMainThread failed"))
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

(defun close-font (font)
  "Close FONT, a pointer returned by OPEN-FONT."
  (sdl3-ttf:close-font font))

(defun font-ascent (font)
  "Return FONT's ascent in pixels (positive)."
  (sdl3-ttf:get-font-ascent font))

(defun font-descent (font)
  "Return FONT's descent in pixels (negative, per SDL_ttf)."
  (sdl3-ttf:get-font-descent font))

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
