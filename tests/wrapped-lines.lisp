(defpackage :lem-tests/wrapped-lines
  (:use :cl :rove)
  (:import-from :lem-fake-interface
                :fake-interface))
(in-package :lem-tests/wrapped-lines)

(defclass pixel-fake-interface (fake-interface)
  ((rendered-rows
    :initform '()
    :accessor rendered-rows
    :documentation "Each rendered row as (Y . TEXT), most recent first."))
  (:documentation "A fake interface that measures in pixels, as GUI
frontends do: each cell is 10 by 10 pixels."))

(defmethod lem-if:get-char-width ((implementation pixel-fake-interface))
  "Cells are 10 pixels wide."
  10)

(defmethod lem-if:get-char-height ((implementation pixel-fake-interface))
  "Cells are 10 pixels high."
  10)

(defmethod lem-if:view-width ((implementation pixel-fake-interface) view)
  "The view's width in pixels."
  (* 10 (call-next-method)))

(defmethod lem-if:view-height ((implementation pixel-fake-interface) view)
  "The view's height in pixels."
  (* 10 (call-next-method)))

(defmethod lem-if:object-width ((implementation pixel-fake-interface) object)
  "Text is 10 pixels a cell; anything else is one cell."
  (if (typep object 'lem-core/display:text-object)
      (* 10 (lem:string-width (lem-core/display:text-object-string object)))
      10))

(defmethod lem-if:object-height ((implementation pixel-fake-interface) object)
  "Every object is one cell high."
  10)

(defmethod lem-if:render-line ((implementation pixel-fake-interface) view x y objects height)
  "Record the row's text at Y instead of drawing it (see rendered-rows)."
  (push (cons y (format nil "~{~A~}"
                        (loop :for object :in objects
                              :when (typep object 'lem-core/display:text-object)
                                :collect (lem-core/display:text-object-string object))))
        (rendered-rows implementation)))

(defun row-at (implementation y)
  "The text last rendered at Y, or NIL."
  (cdr (assoc y (rendered-rows implementation))))

(deftest wrapped-lines-below-the-top-keep-their-rows
  ;; y is in the frontend's units (pixels here) while window-height is in
  ;; rows; a long line well below the top must still draw every row it
  ;; wraps to, and the next line must come after them.
  (lem:with-current-buffers ()
    (let ((implementation (make-instance 'pixel-fake-interface)))
      (lem:with-implementation implementation
        (lem:setup-first-frame)
        (let ((buffer (lem:make-buffer "wrapped" :temporary t)))
          (lem:switch-to-buffer buffer)
          (lem:insert-string (lem:buffer-point buffer)
                             (format nil "1~%2~%3~%4~%5~%~A~%end"
                                     (make-string 200 :initial-element #\a)))
          (lem:buffer-start (lem:buffer-point buffer))
          (lem:redraw-display :force t)
          (let ((wrapped-rows (loop :for y :from 50 :by 10
                                    :for text := (row-at implementation y)
                                    :while (and text (find #\a text))
                                    :collect text)))
            (ok (< 1 (length wrapped-rows))
                (format nil "the long line drew ~D rows" (length wrapped-rows)))
            (ok (= 200 (loop :for text :in wrapped-rows :sum (count #\a text)))
                "every character of it is drawn")
            (ok (equal "end"
                       (row-at implementation (+ 50 (* 10 (length wrapped-rows)))))
                "the next line follows its last row")))))))
