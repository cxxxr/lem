(in-package :lem-core)

(defvar *implementation*)

(defclass implementation ()
  ((name
    :initform (alexandria:required-argument :name)
    :initarg :name
    :reader implementation-name)
   (redraw-after-modifying-floating-window
    :initform nil
    :initarg :redraw-after-modifying-floating-window
    :reader redraw-after-modifying-floating-window)
   (support-floating-window
    :initform t
    :initarg :support-floating-window
    :reader support-floating-window)
   (window-left-margin
    :initform 1
    :initarg :window-left-margin
    :reader window-left-margin)
   (window-bottom-margin
    :initform 1
    :initarg :window-bottom-margin
    :reader window-bottom-margin)
   (html-support
    :initform nil
    :initarg :html-support
    :reader html-support-p)
   (no-force-needed
    :initform nil
    :initarg :no-force-needed
    :reader no-force-needed-p
    :documentation "When no-force-needed-p is T, the force argument of redraw-display is ignored.
In environments like ncurses, when the upper window is modified, the lower window needs to be redrawn.
In that case, set the force of redraw-display to T.
When rendering the DOM and a window in a one-to-one manner, no redraw is required.")
   (underline-color-support
    :initform nil
    :initarg :underline-color-support
    :reader underline-color-support-p
    :documentation "If a color different from the foreground color can be assigned to the underline, then it is T (in Terminal, it becomes nil).")
   (support-pixel-positioning
    :initform nil
    :initarg :support-pixel-positioning
    :reader support-pixel-positioning-p
    :documentation "When true, the frontend supports pixel-based floating window positioning.")))

(defun get-default-implementation (&key implementation)
  (let ((classes (c2mop:class-direct-subclasses (find-class 'implementation)))
        implementation-fallback
        class)
    (when (>= 0 (length classes))
      (error "Implementation does not exist.~
                             (probably because you didn't load the lem-ncurses system)"))

    ;; set interfaces as fallbacks if a non-existant interface is selected
    (setf implementation-fallback
          (mapcar (lambda (impl)
                    (find impl classes :test 'string= :key 'class-name))
                  ;; always try to find specified implementation first
                  (list implementation :webview :ncurses :sdl2 :sdl3)))

    ;; pick the first implementation that is available
    (setf class (funcall #'some #'identity implementation-fallback))

    (if (string= (class-name class) implementation)
         (log:info "Using interface: ~A" implementation)
         (log:warn "User specified non-existant interface ~A; Using ~A instead.
Available interfaces: ~A"
                   implementation class classes))
    
    (if class
      (make-instance class)
      (error "No interfaces found (is lem compiled with an interface?)"))))

(defvar lem-if:*background-color-of-drawing-window* nil)

(deftype cursor-type ()
  '(member :box :bar :underline))

(defgeneric lem-if:invoke (implementation function))
(defgeneric lem-if:get-background-color (implementation))
(defgeneric lem-if:get-foreground-color (implementation))
(defgeneric lem-if:update-foreground (implementation color-name))
(defgeneric lem-if:update-background (implementation color-name))
(defgeneric lem-if:update-cursor-shape (implementation cursor-type)
  (:method (implementation cursor-type)))
(defgeneric lem-if:display-width (implementation))
(defgeneric lem-if:display-height (implementation))
(defgeneric lem-if:display-title (implementation))
(defgeneric lem-if:set-display-title (implementation title))
(defgeneric lem-if:display-fullscreen-p (implementation))
(defgeneric lem-if:set-display-fullscreen-p (implementation fullscreen-p))
(defgeneric lem-if:maximize-frame (implementation)
  (:method (implementation)))
(defgeneric lem-if:minimize-frame (implementation)
  (:method (implementation)))
(defgeneric lem-if:make-view (implementation window x y width height use-modeline))
(defgeneric lem-if:view-width (implementation view))
(defgeneric lem-if:view-height (implementation view))
(defgeneric lem-if:delete-view (implementation view))
(defgeneric lem-if:clear (implementation view))
(defgeneric lem-if:set-view-size (implementation view width height))
(defgeneric lem-if:set-view-pos (implementation view x y))
(defgeneric lem-if:make-view-with-pixels (implementation window x y width height
                                          pixel-x pixel-y pixel-width pixel-height
                                          use-modeline)
  (:documentation "Create a view with both character and pixel coordinates.
X, Y, WIDTH, HEIGHT are in character units.
PIXEL-X, PIXEL-Y, PIXEL-WIDTH, PIXEL-HEIGHT are in pixels (may be nil for auto-calculate).")
  (:method (implementation window x y width height pixel-x pixel-y pixel-width pixel-height use-modeline)
    (declare (ignore pixel-x pixel-y pixel-width pixel-height))
    (lem-if:make-view implementation window x y width height use-modeline)))
(defgeneric lem-if:set-view-pos-pixels (implementation view x y pixel-x pixel-y)
  (:documentation "Set view position with both character and pixel coordinates.")
  (:method (implementation view x y pixel-x pixel-y)
    (declare (ignore pixel-x pixel-y))
    (lem-if:set-view-pos implementation view x y)))
(defgeneric lem-if:set-view-size-pixels (implementation view width height pixel-width pixel-height)
  (:documentation "Set view size with both character and pixel coordinates.")
  (:method (implementation view width height pixel-width pixel-height)
    (declare (ignore pixel-width pixel-height))
    (lem-if:set-view-size implementation view width height)))
(defgeneric lem-if:redraw-view-before (implementation view)
  (:method (implementation view)))
(defgeneric lem-if:redraw-view-after (implementation view)
  (:method (implementation view)))
(defgeneric lem-if:will-update-display (implementation)
  (:method (implementation)))
(defgeneric lem-if:update-display (implementation))

(defgeneric lem-if:display-popup-menu (implementation items
                                       &key action-callback
                                            print-spec
                                            style
                                         max-display-items)
  (:documentation "Create a popup-menu and display it. See `display-popup-menu`."))

(defgeneric lem-if:popup-menu-update
    (implementation popup-menu items &key print-spec max-display-items keep-focus))
(defgeneric lem-if:popup-menu-quit (implementation popup-menu))
(defgeneric lem-if:popup-menu-down (implementation popup-menu))
(defgeneric lem-if:popup-menu-up (implementation popup-menu))
(defgeneric lem-if:popup-menu-first (implementation popup-menu))
(defgeneric lem-if:popup-menu-last (implementation popup-menu))
(defgeneric lem-if:popup-menu-select (implementation popup-menu))
(defgeneric lem-if:display-context-menu (implementation context-menu style)
  (:method (implementation context-menu style)))

(defgeneric lem-if:clipboard-paste (implementation)
  (:method (implementation)))
(defgeneric lem-if:clipboard-copy (implementation text)
  (:method (implementation text)))

(defgeneric lem-if:increase-font-size (implementation)
  (:method (implementation)))
(defgeneric lem-if:decrease-font-size (implementation)
  (:method (implementation)))
(defgeneric lem-if:set-font-name (implementation font-name)
  (:method (implementation font-name) '()))
(defgeneric lem-if:set-font-size (implementation size)
  (:method (implementation size)))

(defgeneric lem-if:resize-display-before (implementation)
  (:method (implementation)))

(defgeneric lem-if:get-font-list (implementation)
  (:method (implementation) '()))

(defgeneric lem-if:get-font (implementation)
  (:method (implementation) (values nil nil)))

(defgeneric lem-if:get-mouse-position (implementation)
  (:method (implementation)
    (values -1 -1)))

(defgeneric lem-if:get-char-width (implementation))
(defgeneric lem-if:get-char-height (implementation))

(defgeneric lem-if:render-line (implementation view x y objects height))
(defgeneric lem-if:render-line-on-modeline (implementation view left-objects right-objects
                                            default-attribute height))
(defgeneric lem-if:object-width (implementation drawing-object))
(defgeneric lem-if:object-height (implementation drawing-object))
(defgeneric lem-if:clear-to-end-of-window (implementation view y))

(defgeneric lem-if:js-eval (implementation view code &key wait)
  (:method (implementation view code &key wait)
    (declare (ignore wait))
    (error "unimplemented")))

(defgeneric lem-if:set-frame-color (implementation mode)
  (:documentation "Set the window frame appearance to MODE (:dark or :light).
Frontends on any platform may implement this to control the native window
chrome. The default method is a no-op for frontends that do not support it.")
  (:method (implementation mode)
    (declare (ignore implementation mode))
    nil))

(defun set-frame-color (&optional (mode :dark))
  "Set the window frame appearance to MODE (:dark or :light)."
  (lem-if:set-frame-color (implementation) mode))

(defvar *display-background-mode* nil)

(defun implementation ()
  *implementation*)

(defmacro with-implementation (implementation &body body)
  `(let* ((*implementation* ,implementation)
          (bt2:*default-special-bindings*
            (acons '*implementation*
                   *implementation*
                   bt2:*default-special-bindings*)))
     ,@body))

(defun display-background-mode ()
  (or *display-background-mode*
      (if (light-color-p (lem-if:get-background-color (implementation)))
          :light
          :dark)))

(defun set-display-background-mode (mode)
  (check-type mode (member :light :dark nil))
  (setf *display-background-mode* mode)
  (when mode
    (set-frame-color mode)))

(defun set-foreground (name)
  (when name
    (lem-if:update-foreground (implementation) name)))

(defun set-background (name)
  (when name
    (lem-if:update-background (implementation) name)))

(defun attribute-foreground-color (attribute)
  (or (and attribute
           (parse-color (attribute-foreground attribute)))
      (lem-if:get-foreground-color (implementation))))

(defun attribute-background-color (attribute)
  (or (and attribute
           (parse-color (attribute-background attribute)))
      (lem-if:get-background-color (implementation))))

(defun attribute-foreground-with-reverse (attribute)
  (if (and attribute (attribute-reverse attribute))
      (attribute-background-color attribute)
      (attribute-foreground-color attribute)))

(defun attribute-background-with-reverse (attribute)
  (if (and attribute (attribute-reverse attribute))
      (attribute-foreground-color attribute)
      (attribute-background-color attribute)))

(defun display-width () (lem-if:display-width (implementation)))
(defun display-height () (lem-if:display-height (implementation)))
(defun display-title () (lem-if:display-title (implementation)))
(defun (setf display-title) (title)
  (lem-if:set-display-title (implementation) title))
(defun display-fullscreen-p () (lem-if:display-fullscreen-p (implementation)))
(defun (setf display-fullscreen-p) (fullscreen-p)
  (lem-if:set-display-fullscreen-p (implementation) fullscreen-p))

(defgeneric lem-if:update-screen-size (implementation)
  (:method (implementation)))

(defun set-font-name (font-name)
  (lem-if:set-font-name (implementation) font-name)
  (lem-if:update-screen-size (implementation)))

(defun set-font-size (font-size)
  (lem-if:set-font-size (implementation) font-size)
  (lem-if:update-screen-size (implementation)))

(defun set-font (&key (name nil name-p) (size nil size-p))
  (when name-p (lem-if:set-font-name (implementation) name))
  (when size-p (lem-if:set-font-size (implementation) size))
  (lem-if:update-screen-size (implementation)))

(defun invoke-frontend (function &key (implementation
                                       (get-default-implementation)))
  (setf *implementation* implementation)
  (lem-if:invoke implementation function))

(defun lem-if:get-font-by-name-and-style (name style)
  "GET-FONT-BY-NAME-AND-STYLE searches for a font with NAME in the path and ends with STYLE"
  (flet ((equal-downcase (s1 s2) (equal (string-downcase s1) (string-downcase s2))))
    (let ((fonts (loop :for font in (lem-if:get-font-list (implementation))
                       :for style-termination := (format nil "~a." style)
                       :when (and (search name font :test #'equal-downcase)
                                  (search style-termination font :test #'equal-downcase))
                       :collect font)))
      (if fonts
          (car fonts)
          (error "font not found for font-name=~s and style=~s" name style)))))
