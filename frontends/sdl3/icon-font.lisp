(defpackage :lem-sdl3/icon-font
  (:use :cl)
  (:export :clear-icon-font-cache
           :icon-font))
(in-package :lem-sdl3/icon-font)

(defvar *icon-font-cache* (make-hash-table :test 'eql))

(defun clear-icon-font-cache ()
  (maphash (lambda (key font)
             (declare (ignore key))
             (handler-case (lem-sdl3/ffi:close-font font)
               (error () nil)))
           *icon-font-cache*)
  (clrhash *icon-font-cache*))

(defun icon-font (character font-size)
  ;; TODO: Fix problem with opening several of the same font file
  (or (gethash character *icon-font-cache*)
      (alexandria:when-let (font-name (lem:icon-value (char-code character) :font))
        (let ((pathname (lem-sdl3/resource:get-resource-pathname
                         (merge-pathnames font-name "resources/fonts/"))))
          (setf (gethash character *icon-font-cache*)
                (lem-sdl3/ffi:open-font pathname font-size))))))
