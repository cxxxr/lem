;;;; image.lisp — images and text of other sizes in buffers, and a viewer of
;;;; image files
;;;;
;;;; LOAD-IMAGE reads an image file (with the file system of the page:
;;;; Node.js's in the desktop application, the files in memory in a
;;;; browser), makes a URL of it and waits until the page has decoded it;
;;;; then the image, with its size, is given to a function in the editor.
;;;; INSERT-IMAGE puts it into a buffer as text whose attribute has it (Lem
;;;; draws such text as an image, display.lisp).  Image files are opened in
;;;; an image buffer, which is zoomed with + - 0 and fitted with f w h as in
;;;; Lem's SDL2 frontend.  Text is drawn larger or smaller with an attribute
;;;; that has :SCALE (SCALED-ATTRIBUTE, INSERT-TEXT; display.lisp draws it).

(in-package :lem-clysm)

;;; ---- loading

(defparameter *image-types*
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("gif" . "image/gif") ("webp" . "image/webp") ("svg" . "image/svg+xml")
    ("bmp" . "image/bmp") ("ico" . "image/x-icon") ("avif" . "image/avif"))
  "The image files that are opened as images, and their media types.")

(defun image-file-p (pathname)
  (let ((type (pathname-type pathname)))
    (and type (assoc type *image-types* :test #'string-equal) t)))

(sys::%defvar-host *decode-image*
  (js:eval "(url, revoke, done) => {
     const img = new Image();
     img.onload = () => done(url, img.naturalWidth, img.naturalHeight, null);
     img.onerror = () => { if (revoke) URL.revokeObjectURL(url);
                           done(null, 0, 0, 'not an image the page can show'); };
     img.src = url; }"))

(sys::%defvar-host *file-url*
  (js:eval "(path, type) => {
     try {
       return URL.createObjectURL(new Blob([globalThis.__clysm_fs.readFileSync(path)], { type }));
     } catch (e) { return String(e && e.message || e); }}"))

(defun decode-image (url revoke function)
  "Have the page decode the image at URL, then call FUNCTION in the editor
with the image, or with NIL and why.  REVOKE: URL is a blob URL to let go if
it is not an image."
  (js:funcall *decode-image* url revoke
              (lambda (url width height error)
                (lem:send-event
                 (lambda ()
                   (if (stringp url)
                       (funcall function (%make-image url width height))
                       (funcall function nil (if (stringp error) error "cannot be loaded")))
                   (lem:redraw-display)))
                nil)))

(defun load-image (pathname function)
  "Load the image file PATHNAME; then call FUNCTION in the editor with the
image, or with NIL and a message saying why it could not be loaded."
  (let* ((path (namestring (truename pathname)))
         (type (or (cdr (assoc (pathname-type pathname) *image-types* :test #'string-equal))
                   "application/octet-stream"))
         (url (js:funcall *file-url* path type)))
    (if (and (stringp url) (eql 0 (search "blob:" url)))
        (decode-image url t function)
        (funcall function nil url))))

(defun load-image-url (url function)
  "Like LOAD-IMAGE, the image at URL (a data: URL, or one the page can
fetch)."
  (decode-image url nil function))

;;; ---- images in buffers

(defun image-attribute (image &key width height)
  "An attribute that draws its text as IMAGE, WIDTH x HEIGHT pixels (by
default, its own size; with one of them, the other in proportion)."
  (multiple-value-bind (width height)
      (cond ((and width height) (values width height))
            (width (values width (round (* width (image-height image)) (max 1 (image-width image)))))
            (height (values (round (* height (image-width image)) (max 1 (image-height image))) height))
            (t (values (image-width image) (image-height image))))
    (lem:make-attribute :plist (list :image image :width (max 1 width) :height (max 1 height)))))

(defun insert-image (point image &key width height (string " "))
  "Insert STRING at POINT, drawn as IMAGE (see IMAGE-ATTRIBUTE)."
  (lem:insert-string point string :attribute (image-attribute image :width width :height height)))

(lem:define-command insert-image-file (pathname) ((:file "Insert image: "))
  "Insert the image of the file PATHNAME at the point."
  (let ((point (lem:copy-point (lem:current-point) :left-inserting)))
    (load-image pathname
                (lambda (image &optional error)
                  (if image
                      (insert-image point image)
                      (lem:message "~A: ~A" (namestring pathname) error))
                  (lem:delete-point point)))))

;;; ---- text of other sizes

(defun scaled-attribute (scale &optional attribute)
  "An attribute that draws text SCALE times its usual size (2: twice as
large), with the colours and the rest of ATTRIBUTE (an attribute, the name
of one, or NIL)."
  (let ((scaled (lem:make-attribute :plist (list :scale scale)))
        (base (and attribute (lem:ensure-attribute attribute nil))))
    (if base (lem:merge-attribute base scaled) scaled)))

(defun insert-text (point string &key (scale 1) attribute)
  "Insert STRING at POINT, SCALE times its usual size, with ATTRIBUTE."
  (lem:insert-string point string :attribute (if (= scale 1)
                                                 attribute
                                                 (scaled-attribute scale attribute))))

(defparameter *sample-image*
  (concatenate 'string "data:image/svg+xml;utf8,"
               "<svg xmlns='http://www.w3.org/2000/svg' width='240' height='120'>"
               "<rect width='240' height='120' rx='12' fill='%23394b70'/>"
               "<circle cx='60' cy='60' r='34' fill='%237aa2f7'/>"
               "<rect x='112' y='34' width='100' height='14' rx='7' fill='%23c0caf5'/>"
               "<rect x='112' y='58' width='80' height='10' rx='5' fill='%23a9b1d6'/>"
               "<rect x='112' y='76' width='90' height='10' rx='5' fill='%23a9b1d6'/></svg>")
  "The image of RICH-TEXT-SAMPLE.")

(lem:define-command rich-text-sample () ()
  "Show a buffer with text of several sizes and an image."
  (let ((buffer (lem:make-buffer "*rich text*")))
    (lem:switch-to-buffer buffer)
    (load-image-url
     *sample-image*
     (lambda (image &optional error)
       (declare (ignore error))
       (let ((point (lem:buffer-point buffer))
             (lem:*inhibit-read-only* t))
         (lem:erase-buffer buffer)
         (insert-text point "Lem on Clysm" :scale 2.5 :attribute 'lem:syntax-keyword-attribute)
         (insert-text point (format nil "~%Text of several sizes and images in one buffer~%")
                      :scale 1.4 :attribute 'lem:syntax-function-name-attribute)
         (insert-text point (format nil "~%The usual size: the text of the buffer, which you edit as usual.~%"))
         (insert-text point (format nil "Small text, for notes.~%~%")
                      :scale 0.8 :attribute 'lem:syntax-comment-attribute)
         (when image
           (insert-image point image)
           (insert-text point "  an image, with text on its baseline" :scale 1.2))
         (insert-text point (format nil "~%~%Lisp: (lem-clysm:insert-text (lem:current-point) \"Big\" :scale 2)~%")
                      :attribute 'lem:syntax-string-attribute)
         (lem:buffer-start point))))))

;;; ---- the viewer

(lem:define-major-mode image-viewer-mode ()
    (:name "Image Viewer"
     :keymap *image-viewer-keymap*)
  (setf (lem:buffer-read-only-p (lem:current-buffer)) t))

(lem:define-key *image-viewer-keymap* "+" 'image-zoom-in)
(lem:define-key *image-viewer-keymap* "C-+" 'image-zoom-in)
(lem:define-key *image-viewer-keymap* "-" 'image-zoom-out)
(lem:define-key *image-viewer-keymap* "C--" 'image-zoom-out)
(lem:define-key *image-viewer-keymap* "0" 'image-zoom-reset)
(lem:define-key *image-viewer-keymap* "C-0" 'image-zoom-reset)
(lem:define-key *image-viewer-keymap* "f" 'image-fit-to-screen)
(lem:define-key *image-viewer-keymap* "w" 'image-fit-to-width)
(lem:define-key *image-viewer-keymap* "h" 'image-fit-to-height)

(defun buffer-image (buffer) (lem:buffer-value buffer 'image))
(defun buffer-scale (buffer) (or (lem:buffer-value buffer 'scale) 1))

(defun show-image (buffer scale)
  "Draw the image of the image buffer BUFFER at SCALE."
  (let ((image (buffer-image buffer))
        (lem:*inhibit-read-only* t))
    (when image
      (setf scale (max 0.05 (min 20 scale))
            (lem:buffer-value buffer 'scale) scale)
      (lem:erase-buffer buffer)
      (insert-image (lem:buffer-point buffer) image
                    :width (round (* scale (image-width image)))
                    :height (round (* scale (image-height image))))
      (lem:insert-string (lem:buffer-point buffer)
                         (format nil "~%~Dx~D  ~D%   + - 0 zoom   f w h fit"
                                 (image-width image) (image-height image) (round (* 100 scale)))
                         :attribute 'lem:syntax-comment-attribute)
      (lem:buffer-start (lem:buffer-point buffer)))))

(defun window-pixels (window)
  "The width and the height in pixels that an image has in WINDOW (the line
under it is left for the size)."
  (values (* *char-width* (max 1 (1- (lem:window-width window))))
          (* *char-height* (max 1 (- (lem:window-height window) 2)))))

(defun fit-image (buffer how)
  (let ((image (buffer-image buffer)))
    (when image
      (multiple-value-bind (width height) (window-pixels (lem:current-window))
        (let ((by-width (/ width (max 1 (image-width image))))
              (by-height (/ height (max 1 (image-height image)))))
          (show-image buffer (ecase how
                               (:width by-width)
                               (:height by-height)
                               ;; a small image is not made larger
                               (:screen (min 1 by-width by-height)))))))))

(defun image-buffer-p (buffer)
  (and (buffer-image buffer) t))

(defmacro define-image-command (name documentation &body body)
  `(lem:define-command ,name () ()
     ,documentation
     (let ((buffer (lem:current-buffer)))
       (if (image-buffer-p buffer)
           (progn ,@body)
           (lem:editor-error "Not an image buffer")))))

(define-image-command image-zoom-in "Make the image larger."
  (show-image buffer (* (buffer-scale buffer) 1.25)))

(define-image-command image-zoom-out "Make the image smaller."
  (show-image buffer (/ (buffer-scale buffer) 1.25)))

(define-image-command image-zoom-reset "Show the image at its own size."
  (show-image buffer 1))

(define-image-command image-fit-to-screen "Make the image as large as the window shows it whole."
  (fit-image buffer :screen))

(define-image-command image-fit-to-width "Make the image as wide as the window."
  (fit-image buffer :width))

(define-image-command image-fit-to-height "Make the image as tall as the window."
  (fit-image buffer :height))

(defun open-image-buffer (pathname)
  "A buffer that shows the image file PATHNAME (filled once it is loaded)."
  (let ((buffer (lem:make-buffer (file-namestring pathname))))
    (setf (lem:buffer-directory buffer)
          (namestring (uiop:pathname-directory-pathname pathname)))
    (lem:change-buffer-mode buffer 'image-viewer-mode)
    (let ((lem:*inhibit-read-only* t))
      (lem:insert-string (lem:buffer-point buffer) "Loading the image..."))
    (load-image pathname
                (lambda (image &optional error)
                  (let ((lem:*inhibit-read-only* t))
                    (cond (image
                           (setf (lem:buffer-value buffer 'image) image)
                           (if (eq (lem:current-buffer) buffer)
                               (fit-image buffer :screen)
                               (show-image buffer 1)))
                          (t
                           (lem:erase-buffer buffer)
                           (lem:insert-string (lem:buffer-point buffer)
                                              (format nil "~A: ~A" (namestring pathname) error)))))))
    buffer))

(defclass image-find-file-executor (lem:find-file-executor) ()
  (:documentation "Opens image files in an image buffer."))

(defmethod lem:execute-find-file ((executor image-find-file-executor) mode pathname)
  (if (and (image-file-p pathname) (probe-file pathname))
      (open-image-buffer pathname)
      (call-next-method)))

(setf lem:*find-file-executor* (make-instance 'image-find-file-executor))
