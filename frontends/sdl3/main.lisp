(defpackage :lem-sdl3
  (:use :cl
        :lem-sdl3/sdl3
        :lem-sdl3/keyboard
        :lem-sdl3/font
        :lem-sdl3/icon
        :lem-sdl3/platform
        :lem-sdl3/resource
        :lem-sdl3/log
        :lem-sdl3/mouse)
  (:local-nicknames (:display :lem-sdl3/display)
                    (:view :lem-sdl3/view)
                    (:event :lem-sdl3/event)
                    (:ffi :lem-sdl3/ffi))
  (:export :change-font
           :set-keyboard-layout
           :render
           :current-renderer))
(in-package :lem-sdl3)

(defconstant +display-width+ 100)
(defconstant +display-height+ 40)

(defun on-mouse-button-down (display button x y clicks)
  (show-cursor)
  (let ((button
          (cond ((eql button event:+button-left+) :button-1)
                ((eql button event:+button-right+) :button-3)
                ((eql button event:+button-middle+) :button-2)
                ((eql button event:+button-x1+) :button-4))))
    (when button
      (let ((char-x (display:scaled-char-width display x))
            (char-y (display:scaled-char-height display y)))
        (lem:send-event
         (lambda ()
           (lem:receive-mouse-button-down char-x char-y x y button
                                          clicks)))))))

(defun on-mouse-button-up (display button x y)
  (show-cursor)
  (let ((button
          (cond ((eql button event:+button-left+) :button-1)
                ((eql button event:+button-right+) :button-3)
                ((eql button event:+button-middle+) :button-2)
                ((eql button event:+button-x1+) :button-4)))
        (char-x (display:scaled-char-width display x))
        (char-y (display:scaled-char-height display y)))
    (lem:send-event
     (lambda ()
       (lem:receive-mouse-button-up char-x char-y x y button)))))

(defun on-mouse-motion (display x y state)
  (show-cursor)
  (let ((button (if (logtest event:+button-lmask+ state)
                    :button-1
                    nil)))
    (let ((char-x (display:scaled-char-width display x))
          (char-y (display:scaled-char-height display y)))
      (lem:send-event
       (lambda ()
         (lem:receive-mouse-motion char-x char-y x y button))))))

(defun on-mouse-wheel (display wheel-x wheel-y)
  (show-cursor)
  (multiple-value-bind (x y) (mouse-position)
    (let ((char-x (display:scaled-char-width display x))
          (char-y (display:scaled-char-height display y)))
      (lem:send-event
       (lambda ()
         (lem:receive-mouse-wheel char-x char-y x y wheel-x wheel-y)
         (when (= 0 (lem:event-queue-length))
           (lem:redraw-display)))))))

(defun on-textediting (text)
  (handle-textediting (get-platform) text)
  (lem:send-event #'lem:redraw-display))

(defun on-textinput (text)
  (hide-cursor)
  (handle-text-input (get-platform) text))

(defun on-keydown (key-event)
  (hide-cursor)
  (handle-key-down (get-platform) key-event))

(defun on-keyup (key-event)
  (handle-key-up (get-platform) key-event))

(defun on-window-event (display type)
  "Handle the SDL3 window event TYPE.  SDL2 delivered these as sub-codes of a
single :windowevent; SDL3 gives each its own event type."
  (alexandria:switch (type)
    (event:+window-shown+
     (display:notify-required-redisplay display))
    (event:+window-exposed+
     (display:notify-required-redisplay display))
    (event:+window-resized+
     (display:update-texture display)
     (display:notify-required-redisplay display))
    ;; SDL fires this on backing-pixel-size changes that do not correspond
    ;; to a user resize — most notably when the window moves between
    ;; displays with different DPI (Retina ↔ standard).  Re-derive the
    ;; high-DPI scale, re-open the font, and resize the backing texture.
    ;; (SDL2: SDL_WINDOWEVENT_SIZE_CHANGED.)
    (event:+window-pixel-size-changed+
     (display:handle-display-changed display))
    (event:+window-display-changed+
     (display:handle-display-changed display))
    ;; New in SDL3: the display's content scale changed (e.g. the user
    ;; changed the OS scaling setting) without the window moving.
    (event:+window-display-scale-changed+
     (display:handle-display-changed display))
    (event:+window-focus-gained+
     (setf (display:display-focus-p display) t))
    (event:+window-focus-lost+
     (setf (display:display-focus-p display) nil))))

(defun on-filedrop (file)
  (lem:send-event (lambda () (lem:find-file file))))

(defun mouse-position ()
  "Return the mouse position in window coordinates, rounded to integers."
  (multiple-value-bind (x y) (ffi:get-mouse-state)
    (values (round x) (round y))))

(defun dispatch-event (display event)
  "Handle one SDL event.  Return true when the event loop should stop."
  (let ((type (event:event-type event)))
    (alexandria:switch (type)
      (event:+quit+
       #+windows
       (cffi:foreign-funcall "_exit")
       t)
      (event:+text-input+
       (on-textinput (event:event-text event))
       nil)
      (event:+text-editing+
       (on-textediting (event:event-text event))
       nil)
      (event:+key-down+
       (on-keydown (make-key-event-from-sdl (event:key-code event) (event:key-modifiers event)))
       nil)
      (event:+key-up+
       (on-keyup (make-key-event-from-sdl (event:key-code event) (event:key-modifiers event)))
       nil)
      (event:+mouse-button-down+
       (on-mouse-button-down display
                             (event:mouse-button event)
                             (round (event:mouse-x event))
                             (round (event:mouse-y event))
                             (event:mouse-clicks event))
       nil)
      (event:+mouse-button-up+
       (on-mouse-button-up display
                           (event:mouse-button event)
                           (round (event:mouse-x event))
                           (round (event:mouse-y event)))
       nil)
      (event:+mouse-motion+
       (on-mouse-motion display
                        (round (event:mouse-x event))
                        (round (event:mouse-y event))
                        (event:mouse-state event))
       nil)
      (event:+mouse-wheel+
       (on-mouse-wheel display (event:wheel-x event) (event:wheel-y event))
       nil)
      (event:+drop-file+
       (on-filedrop (event:drop-file-name event))
       nil)
      ;; Only wakes SDL_WaitEvent so queued main-thread calls run.
      ((ffi:wakeup-event-type)
       nil)
      (otherwise
       (on-window-event display type)
       nil))))

(defun event-loop (display)
  (event:with-event (event)
    (loop :while (event:wait-event event)
          :until (dispatch-event display event))))

(defun init-application-icon (window)
  (let ((image (ffi:load-image (get-resource-pathname "resources/icon.png"))))
    (unwind-protect (ffi:set-window-icon window image)
      (ffi:destroy-surface image))))

(defun call-with-sdl (function)
  (lem-sdl3/wm:set-x11-wm-class)
  (ffi:init-video)
  (unwind-protect (funcall function)
    (ffi:quit)))

(defun create-display (function)
  (call-with-sdl
   (lambda ()
     (let* ((font-config (make-font-config))
            (font (open-font font-config))
            (char-width (font-char-width font))
            (char-height (font-char-height font))
            (window-width (* +display-width+ char-width))
            (window-height (* +display-height+ char-height))
            (window (ffi:create-window "Lem" window-width window-height)))
       (unwind-protect
            (let ((renderer (ffi:create-renderer window)))
              (init-application-icon window)
              (unwind-protect
                   (multiple-value-bind (renderer-width renderer-height)
                       (ffi:get-render-output-size renderer)
                     (let* ((scale-x (/ renderer-width window-width))
                            (scale-y (/ renderer-height window-height))
                            (texture (lem-sdl3/utils:create-texture renderer
                                                                    (* scale-x window-width)
                                                                    (* scale-y window-height)))
                            (display (make-instance 'display:display
                                                    :font-config font-config
                                                    :font font
                                                    :renderer renderer
                                                    :window window
                                                    :texture texture
                                                    :char-width (font-char-width font)
                                                    :char-height (font-char-height font)
                                                    :scale (list scale-x scale-y))))
                       (setf (display:current-display) display)
                       (display:adapt-high-dpi-font-size display)
                       (ffi:start-text-input window)
                       (funcall function)
                       (event-loop display)))
                (ffi:destroy-renderer renderer)))
         (ffi:destroy-window window))))))

(defun sbcl-on-darwin-p ()
  (or #+(and sbcl darwin)
      t
      nil))

(defun set-hints ()
  "Set SDL hints.  Must run before SDL_Init."
  ;; The user's preference; SDL expects "0" or "1".
  (ffi:set-hint "SDL_VIDEO_MAC_FULLSCREEN_SPACES"
                (if (lem:config :darwin-use-native-fullscreen) "1" "0"))
  ;; SDL should not install any signal handlers, since the Lisp runtime
  ;; already does so.
  (ffi:set-hint "SDL_NO_SIGNAL_HANDLERS" "1")
  ;; Lem is not a game; bypassing the KWin compositor brings no noticeable
  ;; performance gain.
  (ffi:set-hint "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR" "0")
  ;; SDL3 may report keycodes already changed by Shift/AltGr.  The keyboard
  ;; code applies shift itself (see make-key-with-shift-careful), so ask for
  ;; the unmodified keycodes SDL2 delivered.
  (ffi:set-hint "SDL_KEYCODE_OPTIONS" "unmodified"))

(defmethod lem-if:invoke ((implementation sdl3) function)
  (flet ((thunk ()
           (let ((editor-thread
                   (funcall function
                            ;; initialize
                            (lambda ())
                            ;; finalize
                            (lambda (report)
                              (when report
                                (do-log report))
                              (event:push-quit-event)))))
             (declare (ignore editor-thread))
             nil)))
    (tmt:with-body-in-main-thread ()
      ;; Inside the main thread: SDL3 may record the main thread on its first
      ;; call (hints included), and IN-MAIN-THREAD relies on that being right.
      (set-hints)
      (ffi:with-float-traps-masked
        (handler-case
            (create-display #'thunk)
          ;; C-c in the terminal Lem was started from: leave the event loop
          ;; and shut SDL down cleanly.
          #+(and linux sbcl)
          (sb-sys:interactive-interrupt () nil))
        (when (sbcl-on-darwin-p)
          (cffi:foreign-funcall "_exit"))))))

(defmethod lem-if:get-background-color ((implementation sdl3))
  (with-debug ("lem-if:get-background-color")
    (display:with-display (display)
      (display:display-background-color display))))

(defmethod lem-if:get-foreground-color ((implementation sdl3))
  (with-debug ("lem-if:get-foreground-color")
    (display:with-display (display)
      (display:display-foreground-color display))))

(defmethod lem-if:update-foreground ((implementation sdl3) color)
  (with-debug ("lem-if:update-foreground" color)
    (display:with-display (display)
      (setf (display:display-foreground-color display)
            (lem:parse-color color)))))

(defmethod lem-if:update-background ((implementation sdl3) color)
  (with-debug ("lem-if:update-background" color)
    (display:with-display (display)
      (setf (display:display-background-color display)
            (lem:parse-color color)))))

(defmethod lem-if:update-cursor-shape ((implementation sdl3) cursor-type)
  (with-debug ("lem-if:update-cursor-type")
    (display:with-display (display)
      (setf (display:display-cursor-type display) cursor-type))))

(defmethod lem-if:display-width ((implementation sdl3))
  (with-debug ("lem-if:display-width")
    (display:with-display (display)
      (display:with-renderer (display)
        (floor (display:display-width display) (display:display-char-width display))))))

(defmethod lem-if:display-height ((implementation sdl3))
  (with-debug ("lem-if:display-height")
    (display:with-display (display)
      (display:with-renderer (display)
        (floor (display:display-height display) (display:display-char-height display))))))

(defmethod lem-if:display-title ((implementation sdl3))
  (with-debug ("lem-if:display-title")
    (display:with-display (display)
      (ffi:get-window-title (display:display-window display)))))

(defmethod lem-if:set-display-title ((implementation sdl3) title)
  (with-debug ("lem-if:set-display-title")
    (ffi:in-main-thread ()
      (display:with-display (display)
        (display:with-renderer (display)
          (ffi:set-window-title (display:display-window display) title)
          ;; return the title instead of nil
          title)))))

(defmethod lem-if:display-fullscreen-p ((implementation sdl3))
  (with-debug ("lem-if:display-fullscreen-p")
    (display:with-display (display)
      (ffi:window-fullscreen-p (display:display-window display)))))

(defmethod lem-if:set-display-fullscreen-p ((implementation sdl3) fullscreen-p)
  (with-debug ("lem-if:set-display-fullscreen-p")
    (ffi:in-main-thread ()
      (display:with-display (display)
        (display:with-renderer (display)
          ;; Desktop (borderless) fullscreen, as SDL2's :desktop: real
          ;; fullscreen had weird bugs on macOS.
          (ffi:set-window-fullscreen (display:display-window display) fullscreen-p))))))

(defmethod lem-if:maximize-frame ((implementation sdl3))
  (with-debug ("lem-if:maximize-frame")
    (ffi:in-main-thread ()
      (display:with-display (display)
        (ffi:maximize-window (display:display-window display))))))

(defmethod lem-if:minimize-frame ((implementation sdl3))
  (with-debug ("lem-if:minimize-frame")
    (ffi:in-main-thread ()
      (display:with-display (display)
        (ffi:minimize-window (display:display-window display))))))

(defmethod lem-if:make-view ((implementation sdl3) window x y width height use-modeline)
  (with-debug ("lem-if:make-view" window x y width height use-modeline)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:create-view display window x y width height use-modeline)))))

(defmethod lem-if:delete-view ((implementation sdl3) view)
  (with-debug ("lem-if:delete-view")
    (display:with-display (display)
      (display:with-renderer (display)
        (view:delete-view view)))))

(defmethod lem-if:clear ((implementation sdl3) view)
  (with-debug ("lem-if:clear" view)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:render-clear view display)))))

(defmethod lem-if:set-view-size ((implementation sdl3) view width height)
  (with-debug ("lem-if:set-view-size" view width height)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:resize view display width height)))))

(defmethod lem-if:set-view-pos ((implementation sdl3) view x y)
  (with-debug ("lem-if:set-view-pos" view x y)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:move-position view x y)))))

(defmethod lem-if:redraw-view-before ((implementation sdl3) view)
  (with-debug ("lem-if:redraw-view-before" view)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:render-border-using-view view display)))))

(defgeneric render (texture window buffer))

(defmethod lem-if:redraw-view-after ((implementation sdl3) view)
  (with-debug ("lem-if:redraw-view-after" view)
    (display:with-display (display)
      (display:with-renderer (display)
        (ffi:set-render-viewport (display:display-renderer display)
                                 0
                                 0
                                 (* (view:view-width view) (display:display-char-width display))
                                 (* (1- (view:view-height view)) (display:display-char-height display)))
        (render (view:view-texture view)
                (view:view-window view)
                (lem:window-buffer (view:view-window view)))
        (ffi:set-render-viewport (display:display-renderer display))
        (view:render-view-texture-to-display view display)))))

(defmethod lem-if:will-update-display ((implementation sdl3))
  (with-debug ("will-update-display")
    (display:with-display (display)
      (display:with-renderer (display)
        ;; Bound cache memory: full reset when cache exceeds threshold.
        ;; Runs inside with-renderer so textures are destroyed on the main thread.
        (lem-sdl3/text-surface-cache:sweep-if-oversize)
        (ffi:set-render-target (display:display-renderer display) (display:display-texture display))
        (display:set-render-color display (display:display-background-color display))
        (ffi:render-clear (display:display-renderer display))))))

(defun set-input-method (display)
  (let* ((view (lem:window-view (lem:current-window)))
         (cursor-x (view:last-cursor-x view display))
         (cursor-y (view:last-cursor-y view display))
         (text lem-sdl3/keyboard::*textediting-text*)
         (x (+ (* (view:view-x view) (display:display-char-width display))
               cursor-x))
         (y (+ (* (view:view-y view) (display:display-char-height display))
               cursor-y)))
    ;; x/y are in pixels; SDL_SetTextInputArea wants window coordinates.
    (destructuring-bind (scale-x scale-y) (display:display-scale display)
      (ffi:set-text-input-area (display:display-window display)
                               (/ x scale-x)
                               (/ y scale-y)
                               (/ (* (display:display-char-width display) (lem:string-width text))
                                  scale-x)
                               (/ (display:display-char-height display) scale-y)))
    (when (plusp (length text))
      (let* ((color (display:display-foreground-color display))
             (surface (ffi:render-text-blended (display:display-cjk-normal-font display)
                                               text
                                               (lem:color-red color)
                                               (lem:color-green color)
                                               (lem:color-blue color))))
        (when surface
          (let ((texture (ffi:create-texture-from-surface (display:display-renderer display)
                                                          surface)))
            (ffi:with-frect (rect x y (ffi:surface-width surface) (ffi:surface-height surface))
              (display:set-render-color display (display:display-background-color display))
              (ffi:render-fill-rect (display:display-renderer display) rect)
              (ffi:render-texture (display:display-renderer display) texture nil rect))
            (ffi:destroy-texture texture)
            (ffi:destroy-surface surface)))))))

(defmethod lem-if:update-display ((implementation sdl3))
  (with-debug ("lem-if:update-display")
    (display:with-display (display)
      (display:with-renderer (display)
        (setf (display:display-redraw-at-least-once-p display) t)
        (ffi:set-render-target (display:display-renderer display) nil)
        (ffi:render-texture (display:display-renderer display) (display:display-texture display) nil nil)
        (set-input-method display)
        (display:update-display display)))))

(defmethod lem-if:increase-font-size ((implementation sdl3))
  (with-debug ("increase-font-size")
    (display:with-display (display)
      (display:with-renderer (display)
        (let ((font-config (display:display-font-config display))
              (ratio (round (first (display:display-scale display)))))
          (display:change-font display
                               (change-size font-config
                                            (+ (font-config-size font-config) ratio))))))))

(defmethod lem-if:decrease-font-size ((implementation sdl3))
  (with-debug ("decrease-font-size")
    (display:with-display (display)
      (display:with-renderer (display)
        (let ((font-config (display:display-font-config display))
              (ratio (round (first (display:display-scale display)))))
          (display:change-font display
                               (change-size font-config
                                            (- (font-config-size font-config) ratio))))))))

(defmethod lem-if:set-font-size ((implementation sdl3) size)
  (display:with-display (display)
    (display:with-renderer (display)
      (let* ((font-config (display:display-font-config display))
             (ratio (round (first (display:display-scale display))))
             (scaled-size (* ratio size)))
        (setf (lem:config :sdl2-font-size) size)
        (display:change-font
         display
         (change-size font-config scaled-size)
         nil)))))

(defmethod lem-if:resize-display-before ((implementation sdl3))
  (with-debug ("resize-display-before")
    (display:with-display (display)
      (display:with-renderer (display)
        (display:clear display)))))

(defmethod lem-if:get-font-list ((implementation sdl3))
  (get-font-list (get-platform)))

(defmethod lem-if:get-mouse-position ((implementation sdl3))
  (display:with-display (display)
    (if (not (cursor-shown-p))
        (values 0 0)
        (multiple-value-bind (x y) (mouse-position)
          (values (display:scaled-char-width display x)
                  (display:scaled-char-height display y))))))

(defmethod lem-if:get-char-width ((implementation sdl3))
  (display:with-display (display)
    (display:display-char-width display)))

(defmethod lem-if:get-char-height ((implementation sdl3))
  (display:with-display (display)
    (display:display-char-height display)))

(defmethod lem-if:view-width ((implementation sdl3) view)
  (display:with-display (display)
    (* (display:display-char-width display)
       (lem-sdl3/view:view-width view))))

(defmethod lem-if:view-height ((implementation sdl3) view)
  (display:with-display (display)
    (* (display:display-char-height display)
       (lem-sdl3/view:view-height view))))

#-windows
(defmethod lem-if:clipboard-paste ((implementation sdl3))
  (lem-sdl3/log:with-debug ("clipboard-paste")
    (display:with-display (display)
      (display:with-renderer (display)
        ;; SDL2 returned "" for an empty clipboard; keep that.
        (or (ffi:get-clipboard-text) "")))))

#+windows
(defmethod lem-if:clipboard-paste ((implementation sdl3))
  (lem-sdl3/log:with-debug ("clipboard-paste")
    (display:with-display (display)
      (display:with-renderer (display)
        (with-output-to-string (out)
          (let ((text (or (ffi:get-clipboard-text) "")))
            (loop :for string :in (split-sequence:split-sequence #\newline text)
                  :do (if (and (< 0 (length string))
                               (char= #\return (char string (1- (length string)))))
                          (write-line (subseq string 0 (1- (length string))) out)
                          (write-string string out)))))))))

(defmethod lem-if:clipboard-copy ((implementation sdl3) text)
  (lem-sdl3/log:with-debug ("clipboard-copy")
    (display:with-display (display)
      (display:with-renderer (display)
        (ffi:set-clipboard-text text)))))

(lem:enable-clipboard)


;; helpers
(defun char-width () (display:display-char-width (display:current-display)))
(defun char-height () (display:display-char-height (display:current-display)))
(defun current-renderer () (display:display-renderer (display:current-display)))

(defmethod lem-if:display-fullscreen-p ((implementation sdl3))
  (with-debug ("lem-if:display-fullscreen-p")
    (display:with-display (display)
      (ffi:window-fullscreen-p (display:display-window display)))))

;;;; Toggle Display Borderless

(defgeneric lem-if::display-borderless-p (implementation)
  (:documentation
   "Return `t' if FRAME is borderless, `nil' otherwise. 
Default IMPLEMENTATION should return `t' as fallback. ")
  (:method (implementation) t)
  (:method ((implementation sdl3))
    (with-debug ("display-borderless-p")
      (display:with-display (display)
        (ffi:window-borderless-p (display:display-window display))))))

(defmethod (setf lem-if::display-borderless-p) (boolean implementation)
  (declare (ignore boolean implementation)))

(defmethod (setf lem-if::display-borderless-p) (boolean (implementation sdl3))
  (declare (ignore implementation))
  (with-debug ("(setf lem-if::display-borderless-p)")
    (ffi:in-main-thread ()
      (display:with-display (display)
        (ffi:set-window-bordered (display:display-window display) (not boolean))))))

(in-package :lem-core)

(defun lem-core::display-borderless-p ()
  "Get/Set current display if borderless. 
Return `t' for borderless (no-titlebar), `nil' for otherwise. "
  (lem-if::display-borderless-p (implementation)))

(defun (setf lem-core::display-borderless-p) (boolean)
  (setf (lem-if::display-borderless-p (implementation)) boolean))

(define-command toggle-frame-borderless () ()
  "Toggles frame borderless. "
  (setf (display-borderless-p) (not (display-borderless-p))))