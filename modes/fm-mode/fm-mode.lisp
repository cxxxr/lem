(defpackage :lem-fm-mode
  (:use :cl :lem-base :lem :lem.button))
(in-package :lem-fm-mode)

(defconstant +fm-max-number-of-frames+ 256)
(defconstant +fm-max-width-of-each-frame-name+ 20)

(define-attribute fm-active-frame-name-attribute
  (t :foreground "white" :background "blue"))

(define-attribute fm-frame-name-attribute
  (t :foreground "white" :background "blue"))

(define-attribute fm-background-attribute
  (t :underline-p t))

(defstruct %frame
  (id 0 :type integer)
  (frame nil :type lem:frame))

(defun %make-frame (id frame)
  (make-%frame :id id :frame frame))

(defclass vf (header-window)
  ((implementation
    :initarg :impl
    :initform nil
    :accessor vf-impl
    :type lem:implementation)
   (frames
    :initarg :frames
    :accessor vf-frames
    :type array)
   (current
    :initarg :current
    :accessor vf-current
    :type %frame)
   (buffer-list-map
    :initarg :buffer-list-map
    :accessor vf-buffer-list-map)
   (display-width
    :initarg :width
    :accessor vf-width)
   (display-height
    :initarg :height
    :accessor vf-height)
   (changed
    :initform t
    :accessor vf-changed)
   (buffer
    :initarg :buffer
    :accessor vf-header-buffer)))

(defun make-vf (impl frame)
  (let* ((buffer (make-buffer "*fm*" :enable-undo-p nil :temporary t))
         (%frame (%make-frame 0 frame))
         (frames (make-array +fm-max-number-of-frames+ :initial-element nil)))
    (setf (aref frames 0) %frame)
    (setf (lem:variable-value 'truncate-lines :buffer buffer) nil)
    (let ((vf (make-instance 'vf
                             :impl impl
                             :buffer buffer
                             :width (display-width)
                             :height (display-height)
                             :frames frames
                             :current %frame
                             :buffer-list-map (make-hash-table))))
      vf)))

(defun all-buffer-list ()
  (remove-duplicates
   (loop
     :for k :being :each :hash-key :of *vf-map*
     :using (hash-value vf)
     :append (loop
               :for k :being :each :hash-key :of (vf-buffer-list-map vf)
               :using (hash-value buffer-list)
               :append buffer-list))))

(defvar *vf-map* nil)

(defun search-previous-frame (vf id)
  (let* ((frames (vf-frames vf))
         (len (length frames)))
    (flet ((wrap (n)
             (if (minusp n)
                 (+ (1- len) n)
                 n)))
      (loop
        :for n := (wrap (1- id)) :then (wrap (1- n))
        :until (= n id)
        :do (unless (null (aref frames n))
              (return-from search-previous-frame (aref frames n)))))))

(defun search-next-frame (vf id)
  (let* ((frames (vf-frames vf))
         (len (length frames)))
    (flet ((wrap (n)
             (if (>= n len)
                 (- len n)
                 n)))
      (loop
        :for n := (wrap (1+ id)) :then (wrap (1+ n))
        :until (= n id)
        :do (unless (null (aref frames n))
              (return-from search-next-frame (aref frames n)))))))

(defun vf-require-update (vf)
  (cond ((vf-changed vf) t)
        ((not (= (display-width)
                 (vf-width vf)))
         t)
        ((not (= (display-height)
                 (vf-height vf)))
         t)))

(defmethod window-redraw ((window vf) force)
  (when (or force
            (loop :for k :being :each :hash-key :of *vf-map*
                  :using (hash-value vf)
                  :thereis (vf-require-update vf)))
    ;; draw button for frames
    (let* ((buffer (vf-header-buffer window))
           (p (buffer-point buffer))
           (charpos (point-charpos p)))
      (erase-buffer buffer)
      (loop
        :for %frame :across (vf-frames window)
        :unless (null %frame)
        :do (let* ((focusp (eq %frame (vf-current window)))
                   (start-pos (point-charpos p)))
              (insert-button p
                             ;; virtual frame name on header
                             (let* ((frame (%frame-frame %frame))
                                    (buffer (window-buffer (lem:frame-current-window frame)))
                                    (name (buffer-name buffer)))
                               (format nil "~a~a:~a "
                                       (if focusp #\# #\space)
                                       (%frame-id %frame)
                                       (if (>= (length name) +fm-max-width-of-each-frame-name+)
                                           (format nil "~a..."
                                                   (subseq name 0 +fm-max-width-of-each-frame-name+))
                                           name)))
                             ;; set action when click
                             (let ((%frame %frame))
                               (lambda ()
                                 (setf (vf-current window) %frame)
                                 (setf (vf-changed window) t)))
                             :attribute (if focusp
                                            'fm-active-frame-name-attribute
                                            'fm-frame-name-attribute))
              ;; increment charpos
              (when focusp
                (let ((end-pos (point-charpos p)))
                  (unless (<= start-pos charpos (1- end-pos))
                    (setf charpos start-pos))))))
      ;; fill right margin
      (let ((margin-right (- (display-width) (point-column p))))
        (when (> margin-right 0)
          (insert-string p (make-string margin-right :initial-element #\space)
                         :attribute 'fm-background-attribute)))
      (line-offset p 0 charpos))
    ;; redraw windows in current frame
    (let* ((%frame (vf-current (gethash (lem:implementation) *vf-map*)))
           (frame (%frame-frame %frame)))
      (dolist (w (lem::window-tree-flatten (lem:frame-window-tree frame)))
        (lem:window-redraw w t))
      (dolist (w (lem:frame-floating-windows frame))
        (lem:window-redraw w t))
      (dolist (w (lem:frame-header-windows frame))
        (unless (eq w window)
          (lem:window-redraw w t))))
    (lem-if:update-display (lem:implementation)))
  ;; clear all vf-changed to nil because of applying redraw
  (maphash (lambda (k vf)
             (declare (ignore k))
             (setf (vf-changed vf) nil))
           *vf-map*)
  (call-next-method))

(defun frame-multiplexer-init ()
  (setf *vf-map* (make-hash-table))
  (loop
    :for impl :in (list (implementation))  ; for multi-frame support in the future...
    :do (let ((vf (make-vf impl (lem:get-frame impl))))
          (setf (gethash impl *vf-map*) vf)
          (setf (gethash (vf-current vf) (vf-buffer-list-map vf))
                (copy-list (buffer-list)))
          (lem:map-frame (implementation) (%frame-frame (vf-current vf))))))

(defun frame-multiplexer-on ()
  (unless (variable-value 'frame-multiplexer :global)
    (frame-multiplexer-init)))

(defun frame-multiplexer-off ()
  (when (variable-value 'frame-multiplexer :global)
    (maphash (lambda (k v)
               (declare (ignore k))
               (delete-window v))
             *vf-map*)
    (setf *vf-map* nil)))

(define-editor-variable frame-multiplexer nil ""
  (lambda (value)
    (if value
        (frame-multiplexer-on)
        (frame-multiplexer-off))))

(define-command fm () ()
  (setf (variable-value 'frame-multiplexer :global)
        (not (variable-value 'frame-multiplexer :global))))

(defun create-frame (new-buffer-list-p)
  (block exit
    (when (null *vf-map*)
      (editor-error "fm-mode is not enabled")
      (return-from exit))
    (let* ((vf (gethash (implementation) *vf-map*))
           (id (position-if #'null (vf-frames vf))))
      (when (null id)
        (editor-error "it's full of frames in virtual frame")
        (return-from exit))
      (let* ((frame (lem:make-frame))
             (%frame (%make-frame id frame))
             (tmp-buffer (find "*tmp*" (append (buffer-list) (all-buffer-list))
                               :key (lambda (b) (buffer-name b))
                               :test #'string=)))
        (setf (gethash (vf-current vf) (vf-buffer-list-map vf))
              (copy-list (buffer-list)))
        (let ((buffer-list (if new-buffer-list-p
                               (list tmp-buffer)
                               (copy-list (buffer-list)))))
          (setf (gethash %frame (vf-buffer-list-map vf)) buffer-list)
          (lem-base::set-buffer-list buffer-list))
        (lem:setup-frame frame)
        (push vf (lem:frame-header-windows frame))
        (when tmp-buffer
          (let ((new-window (lem::make-window tmp-buffer
                                              (lem::window-topleft-x) (lem::window-topleft-y)
                                              (lem::window-max-width) (lem::window-max-height)
                                              t)))
            (setf (lem:frame-window-tree frame) new-window
                  (lem:frame-current-window frame) new-window
                  (aref (vf-frames vf) id) %frame
                  (vf-current vf) %frame)
            (lem:map-frame (implementation) frame))))
      (setf (vf-changed vf) t))))

(define-key *global-keymap* "c-z c" 'fm-create-with-new-buffer-list)
(define-command fm-create-with-new-buffer-list () ()
  (create-frame t))

(define-key *global-keymap* "c-z C" 'fm-create)
(define-command fm-create () ()
  (create-frame nil))

(define-key *global-keymap* "c-z d" 'fm-delete)
(define-command fm-delete () ()
  (block exit
    (when (null *vf-map*)
      (editor-error "fm-mode is not enabled")
      (return-from exit))
    (let* ((vf (gethash (implementation) *vf-map*))
           (num (count-if-not #'null (vf-frames vf)))
           (id (position (vf-current vf) (vf-frames vf))))
      (when (= num 1)
        (editor-error "cannot delete this virtual frame")
        (return-from exit))
      (when (null id)
        (editor-error "something wrong... fm-mode broken?")
        (return-from exit))
      (remhash (vf-current vf) (vf-buffer-list-map vf))
      (setf (aref (vf-frames vf) id) nil)
      (let ((%frame (search-previous-frame vf id)))
        (setf (vf-current vf) %frame)
        (lem:map-frame (implementation) (%frame-frame %frame)))
      (setf (vf-changed vf) t))))

(define-key *global-keymap* "C-z p" 'fm-prev)
(define-command fm-prev () ()
  (block exit
    (when (null *vf-map*)
      (editor-error "fm-mode is not enabled")
      (return-from exit))
    (let* ((vf (gethash (implementation) *vf-map*))
           (id (position (vf-current vf) (vf-frames vf))))
      (when (null id)
        (editor-error "something wrong... fm-mode broken?")
        (return-from exit))
      (let ((%frame (search-previous-frame vf id)))
        (when %frame
          (let ((prev-current (vf-current vf))
                (buffer-list (copy-list (buffer-list))))
            (setf (gethash prev-current (vf-buffer-list-map vf)) buffer-list))
          (setf (vf-current vf) %frame)
          (lem-base::set-buffer-list (gethash %frame (vf-buffer-list-map vf)))
          (lem:map-frame (implementation) (%frame-frame %frame))))
      (lem::change-display-size-hook)
      (setf (vf-changed vf) t))))

(define-key *global-keymap* "C-z n" 'fm-next)
(define-command fm-next () ()
  (block exit
    (when (null *vf-map*)
      (editor-error "fm-mode is not enabled")
      (return-from exit))
    (let* ((vf (gethash (implementation) *vf-map*))
           (id (position (vf-current vf) (vf-frames vf))))
      (when (null id)
        (editor-error "something wrong... fm-mode broken?")
        (return-from exit))
      (let ((%frame (search-next-frame vf id)))
        (when %frame
          (let ((prev-current (vf-current vf))
                (buffer-list (copy-list (buffer-list))))
            (setf (gethash prev-current (vf-buffer-list-map vf)) buffer-list))
          (setf (vf-current vf) %frame)
          (lem-base::set-buffer-list (gethash %frame (vf-buffer-list-map vf)))
          (lem:map-frame (implementation) (%frame-frame %frame))))
      (lem::change-display-size-hook)
      (setf (vf-changed vf) t))))

(defun completion-buffer-name-from-all-frames (str)
  (completion-strings str (mapcar #'buffer-name (all-buffer-list))))

(define-key *global-keymap* "C-z b" 'fm-select-buffer-from-all-frames)
(define-command fm-select-buffer-from-all-frames (name)
    ((list (let ((lem:*minibuffer-buffer-complete-function*
                   #'completion-buffer-name-from-all-frames))
             (prompt-for-buffer "Use buffer: " (buffer-name (current-buffer))
                                t (all-buffer-list)))))
  (block exit
    (when (null *vf-map*)
      (editor-error "fm-mode is not enabled")
      (return-from exit))
    (let ((buffer (find name (all-buffer-list) :test #'string= :key #'buffer-name)))
      (select-buffer buffer))))
