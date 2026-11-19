;;;; frontend.lisp — the implementation of Lem's frontend protocol (LEM-IF)

(in-package :lem-clysm)

(defclass clysm (lem:implementation)
  ()
  (:default-initargs
   :name :clysm
   :redraw-after-modifying-floating-window t)
  (:documentation "Lem's frontend in a page of Clysm's UI framework."))

;;; ---- the screen

(ui:defcomponent screen ()
  (:div :class "lem-screen"
        :style (format nil "color:~A;background:~A;--lem-background:~A;font-size:~Apx;--lem-line-height:~A;"
                       *foreground* *background* *background* *font-size* (px *char-height*))
        :on-mount (lambda (node) (setf *screen-element* node))
        :on-mousedown #'on-mouse-down
        :on-mouseup #'on-mouse-up
        :on-mousemove #'on-mouse-move
        :on-wheel #'on-wheel
        :on-contextmenu (lambda (event) (js:call event "preventDefault"))
    ;; from below: the windows with their modelines, the tab bar, the
    ;; minibuffer, the popups
    (window-layer)
    (tabbar-layer)
    (minibuffer-layer)
    (popup-layer)
    (input-field)))

(sys::%defvar-host *measure-cell*
  (js:eval "(element, fontSize, fontFamily) => {
     const span = document.createElement('span');
     span.style.cssText = 'position:absolute;visibility:hidden;white-space:pre;line-height:1.25;'
                        + 'font-size:' + fontSize + 'px;font-family:' + fontFamily;
     span.textContent = 'x'.repeat(100);
     element.appendChild(span);
     const r = span.getBoundingClientRect();
     span.remove();
     return [r.width / 100, Math.ceil(r.height)]; }"))

(defvar *display-size* '(80 24) "The size of the screen in cells.")

(defun measure ()
  "Measure the cells and the screen; true if the number of cells changed."
  (let ((cell (js:funcall *measure-cell* *screen-element* *font-size* *font-family*)))
    (setf *char-width* (js:get cell 0)
          *char-height* (js:get cell 1)))
  (let* ((width (js:get *screen-element* "clientWidth"))
         (height (js:get *screen-element* "clientHeight"))
         (size (list (max 10 (floor width *char-width*))
                     (max 3 (floor height *char-height*)))))
    (unless (equal size *display-size*)
      (setf *display-size* size)
      t)))

(defun resized ()
  (when (measure)
    (lem:send-event :resize)))

(defvar *container* "#lem" "Where the screen is put on the page.")

(defun start-screen ()
  (ui:mount (screen) *container*)
  (ui:flush)
  (js:set (ui::backend-document ui:*backend*) "title" "Lem")
  (js:call (js:global) "addEventListener" "resize" (lambda (event)
                                                      (declare (ignore event))
                                                      (resized)
                                                      nil))
  (measure))

(sys::%defvar-host *show-exited*
  (js:eval "(container) => { window.close();
     /* a page that the browser does not let close */
     const element = document.querySelector(container);
     if (element) element.innerHTML = '<div id=\"status\">Lem has exited.  Reload the page to start it again.</div>'; }"))

(defun exit-application (report)
  (when report (format *error-output* "~&~A~%" report))
  ;; (a resident desktop application only hides its window when it closes)
  (let ((desktop (ignore-errors (let ((d (js:global "desktop"))) (and (js:js-value-p d) d)))))
    (when desktop (js:call desktop "call" "quit")))
  (js:funcall *show-exited* *container*))

;;; Files that the desktop application is asked to open (its command line,
;;; or frontends/clysm/desktop/lem while it is resident)

(defun open-files-of-desktop ()
  (let ((desktop (ignore-errors (let ((d (js:global "desktop"))) (and (js:js-value-p d) d)))))
    (when (and desktop (js:js-value-p (js:get desktop "onOpen")))
      (js:call desktop "onOpen"
               (lambda (file)
                 (lem:send-event (lambda ()
                                   (lem-core/commands/file:find-file file)
                                   (lem:redraw-display)))
                 nil)))))

(defmethod lem-if:invoke ((implementation clysm) function)
  (start-screen)
  (funcall function nil #'exit-application)
  (open-files-of-desktop))

;;; ---- colours

(defmethod lem-if:get-background-color ((implementation clysm))
  (lem:parse-color *background*))

(defmethod lem-if:get-foreground-color ((implementation clysm))
  (lem:parse-color *foreground*))

(defmethod lem-if:update-foreground ((implementation clysm) color-name)
  (let ((color (color-string color-name)))
    (when color
      (clrhash *style-cache*)
      (setf *foreground* color))))

(defmethod lem-if:update-background ((implementation clysm) color-name)
  (let ((color (color-string color-name)))
    (when color
      (clrhash *style-cache*)
      (setf *background* color))))

(defmethod lem-if:will-update-display ((implementation clysm))
  ;; attributes are changed in place (e.g. the cursor's colour)
  (clrhash *style-cache*))

;;; ---- the display

(defmethod lem-if:display-width ((implementation clysm)) (first *display-size*))
(defmethod lem-if:display-height ((implementation clysm)) (second *display-size*))

(defmethod lem-if:display-title ((implementation clysm))
  (js:get (ui::backend-document ui:*backend*) "title"))

(defmethod lem-if:set-display-title ((implementation clysm) title)
  (js:set (ui::backend-document ui:*backend*) "title" title))

(defmethod lem-if:display-fullscreen-p ((implementation clysm))
  (not (null (js:get (ui::backend-document ui:*backend*) "fullscreenElement"))))

(defmethod lem-if:set-display-fullscreen-p ((implementation clysm) fullscreen-p)
  (let ((document (ui::backend-document ui:*backend*)))
    (if fullscreen-p
        (js:call (js:get document "documentElement") "requestFullscreen")
        (when (js:get document "fullscreenElement")
          (js:call document "exitFullscreen")))))

(defmethod lem-if:make-view ((implementation clysm) window x y width height use-modeline)
  (let ((view (make-instance 'view
                             :window window :role (window-role window)
                             :border (lem:window-border window)
                             :border-shape (and (lem:floating-window-p window)
                                                (lem:floating-window-border-shape window))
                             :x x :y y :width width :height height
                             :modeline-p use-modeline)))
    (resize-canvas view)
    (setf *views* (append *views* (list view)))
    view))

(defmethod lem-if:delete-view ((implementation clysm) view)
  (setf *views* (remove view *views*)))

(defmethod lem-if:clear ((implementation clysm) view)
  (clear-lines-from view 0))

(defmethod lem-if:set-view-size ((implementation clysm) view width height)
  (setf (view-width view) width
        (view-height view) height)
  (resize-canvas view))

(defmethod lem-if:set-view-pos ((implementation clysm) view x y)
  (setf (view-x view) x
        (view-y view) y))

(defmethod lem-if:view-width ((implementation clysm) view) (view-width view))
(defmethod lem-if:view-height ((implementation clysm) view) (view-height view))

(defmethod lem-if:render-line ((implementation clysm) view x y objects height)
  (declare (ignore height))
  (draw-line view x y objects))

(defmethod lem-if:render-line-on-modeline ((implementation clysm) view left-objects right-objects
                                           default-attribute height)
  (declare (ignore height))
  (draw-modeline view left-objects right-objects default-attribute))

(defmethod lem-if:clear-to-end-of-window ((implementation clysm) view y)
  (clear-lines-from view y))

(defmethod lem-if:object-width ((implementation clysm) drawing-object)
  (object-width drawing-object))

(defmethod lem-if:object-height ((implementation clysm) drawing-object)
  (declare (ignore drawing-object))
  1)

(defmethod lem-if:update-display ((implementation clysm))
  (mapc #'update-role *views*)
  (mapc #'publish-view *views*)
  (let* ((window (lem:current-window))
         (view (lem:window-view window)))
    (when view
      (setf *cursor* (list (+ (view-x view) (lem:last-print-cursor-x window))
                           (+ (view-y view) (lem:last-print-cursor-y window)))))))

(defmethod lem-if:get-char-width ((implementation clysm)) (round *char-width*))
(defmethod lem-if:get-char-height ((implementation clysm)) (round *char-height*))

(defmethod lem-if:get-mouse-position ((implementation clysm))
  (values-list *mouse-position*))

;;; ---- fonts

(defun set-font-size (size)
  (setf *font-size* (max 6 (min 72 size)))
  (resized))

(defmethod lem-if:increase-font-size ((implementation clysm))
  (set-font-size (+ *font-size* 1)))

(defmethod lem-if:decrease-font-size ((implementation clysm))
  (set-font-size (- *font-size* 1)))

(defmethod lem-if:set-font-size ((implementation clysm) size)
  (set-font-size size))

(defmethod lem-if:get-font ((implementation clysm))
  (values *font-family* *font-size*))

;;; ---- the clipboard (Electron's, which is read without waiting: in the
;;; renderer itself before Electron 45, through the desktop shell's
;;; synchronous calls since)

(sys::%defvar-host *clipboard*
  (ignore-errors
   (let ((require (js:global "require")))
     (and require (js:get (js:funcall require "electron") "clipboard")))))

(sys::%defvar-host *desktop*
  (ignore-errors (let ((d (js:global "desktop"))) (and (js:js-value-p d) d))))

(defmethod lem-if:clipboard-copy ((implementation clysm) text)
  (cond (*clipboard* (js:call *clipboard* "writeText" text))
        (*desktop* (js:call *desktop* "call" "writeClipboard" text))
        (t (ui:write-clipboard ui:*backend* text))))

(defmethod lem-if:clipboard-paste ((implementation clysm))
  (cond (*clipboard* (js:call *clipboard* "readText"))
        (*desktop* (js:call *desktop* "call" "readClipboard"))))

;;; ---- starting Lem

(defun main (&rest arguments)
  "Start Lem in the page, with ARGUMENTS as on the command line (file names...)."
  (load-saved-theme)
  (apply #'lem:lem "--interface" "clysm" arguments))
