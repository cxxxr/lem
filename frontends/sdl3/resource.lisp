(defpackage :lem-sdl3/resource
  (:use :cl)
  (:export :get-resource-pathname))
(in-package :lem-sdl3/resource)

(defvar *resource-directory* nil)

(defun get-resource-pathname (pathname)
  (if *resource-directory*
      (merge-pathnames pathname *resource-directory*)
      (or (lem:lem-relative-pathname pathname)
          (asdf:system-relative-pathname :lem-sdl3 pathname))))
