(defpackage :lem-tests/proportional-text
  (:use :cl :rove)
  (:import-from :lem-fake-interface
                :fake-interface))
(in-package :lem-tests/proportional-text)

(defclass proportional-fake-interface (fake-interface)
  ((rendered-rows
    :initform '()
    :accessor rendered-rows
    :documentation "Each rendered row as (Y . TEXT), most recent first."))
  (:default-initargs :support-proportional-text t)
  (:documentation "A fake interface that measures in pixels with 10 by 10
cells, and draws proportional text with i 4 pixels wide, m 16 and every
other character 10."))

(defun advance (character)
  (case character
    (#\i 4)
    (#\m 16)
    (otherwise 10)))

(defmethod lem-if:get-char-width ((implementation proportional-fake-interface))
  10)

(defmethod lem-if:get-char-height ((implementation proportional-fake-interface))
  10)

(defmethod lem-if:char-pixel-width ((implementation proportional-fake-interface) character)
  (advance character))

(defmethod lem-if:view-width ((implementation proportional-fake-interface) view)
  (* 10 (call-next-method)))

(defmethod lem-if:view-height ((implementation proportional-fake-interface) view)
  (* 10 (call-next-method)))

(defmethod lem-if:object-width ((implementation proportional-fake-interface) object)
  (typecase object
    (lem-core/display:variable-text-object
     (reduce #'+ (lem-core/display:text-object-string object) :key #'advance))
    (lem-core/display:text-object
     (* 10 (lem:string-width (lem-core/display:text-object-string object))))
    (otherwise 10)))

(defmethod lem-if:object-height ((implementation proportional-fake-interface) object)
  10)

(defmethod lem-if:render-line ((implementation proportional-fake-interface) view x y objects height)
  (push (cons y (format nil "~{~A~}"
                        (loop :for object :in objects
                              :when (typep object 'lem-core/display:text-object)
                                :collect (lem-core/display:text-object-string object))))
        (rendered-rows implementation)))

(defun call-with-proportional-buffer (text function &key (variable-pitch t))
  "Call FUNCTION with the implementation and a window showing TEXT, laid
out proportionally when VARIABLE-PITCH."
  (lem:with-current-buffers ()
    (let ((implementation (make-instance 'proportional-fake-interface)))
      (lem:with-implementation implementation
        (lem:setup-first-frame)
        (let ((buffer (lem:make-buffer "proportional" :temporary t)))
          (lem:switch-to-buffer buffer)
          (setf (lem:variable-value 'lem:variable-pitch :buffer buffer) variable-pitch)
          (lem:insert-string (lem:buffer-point buffer) text)
          (lem:buffer-start (lem:buffer-point buffer))
          (funcall function implementation (lem:current-window)))))))

(defun row-text (implementation y)
  "The text last rendered at Y, without a trailing wrap character."
  (string-right-trim "\\" (or (cdr (assoc y (rendered-rows implementation))) "")))

(defun width-of (string)
  (reduce #'+ string :key #'advance))

(defparameter *long-line*
  (format nil "~{~A~}" (loop :repeat 30 :collect "mim ")))

(deftest wrapping-matches-the-drawn-rows
  (call-with-proportional-buffer
   *long-line*
   (lambda (implementation window)
     (lem:redraw-display :force t)
     (let ((wraps '()))
       (lem-core::map-wrapping-line window *long-line* (lambda (index) (push index wraps)))
       (setf wraps (nreverse wraps))
       (ok (= 1 (length wraps)) "the line wraps once")
       (ok (= (first wraps) (length (row-text implementation 0)))
           "where the display wrapped it")
       (ok (<= (width-of (row-text implementation 0)) (- 800 10))
           "leaving room for the wrap character")
       (ok (equal *long-line*
                  (concatenate 'string (row-text implementation 0) (row-text implementation 10)))
           "and the second row holds the rest")))))

(deftest cursor-x-is-in-pixels
  (call-with-proportional-buffer
   *long-line*
   (lambda (implementation window)
     (declare (ignore implementation))
     (lem:character-offset (lem:current-point) 5)
     (ok (= (width-of (subseq *long-line* 0 5)) (lem-core::window-cursor-x window))))))

(deftest next-line-keeps-the-pixel-x
  (call-with-proportional-buffer
   (format nil "mmmmmmmm~%iiiiiiiiiiiiiiiiiiii")
   (lambda (implementation window)
     (declare (ignore implementation window))
     (lem:character-offset (lem:current-point) 3)
     (lem:next-line 1)
     (ok (= 12 (lem:point-charpos (lem:current-point)))
         "48 pixels in: twelve i's under three m's"))))

(deftest a-click-lands-on-the-nearest-boundary
  (call-with-proportional-buffer
   "iiiiiiiiiiiiiiii"
   (lambda (implementation window)
     (declare (ignore implementation))
     (lem-core::set-last-mouse-event
      (make-instance 'lem-core::mouse-button-down :x 3 :y 0 :pixel-x 37 :pixel-y 5
                                                  :button :button-1 :clicks 1))
     (ok (= 9 (lem:point-charpos
               (lem:get-point-from-window-with-coordinates window 3 0)))
         "pixel 37 is nearest the boundary at 36, after nine i's"))))

(deftest monospace-buffers-keep-columns
  (call-with-proportional-buffer
   *long-line*
   (lambda (implementation window)
     (declare (ignore implementation))
     (lem:character-offset (lem:current-point) 5)
     (ok (= 5 (lem-core::window-cursor-x window))))
   :variable-pitch nil))
