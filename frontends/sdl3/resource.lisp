(defpackage :lem-sdl3/resource
  (:use :cl)
  (:export :*resource-directory*
           :get-resource-pathname
           :find-resource-pathname))
(in-package :lem-sdl3/resource)

(defvar *resource-directory* nil
  "The directory the frontend's resources/ tree is under, or NIL to look next
to the executable (deployed builds) or in the lem-sdl3 system (source runs).
A packaged app whose resources are elsewhere, such as macOS's
Contents/Resources, sets it at boot; it is not changed afterwards.")

(defun get-resource-pathname (pathname)
  "Return the pathname of the resource PATHNAME, e.g. \"resources/icon.png\".
In a deployed build without *resource-directory* the file must exist."
  (if *resource-directory*
      (merge-pathnames pathname *resource-directory*)
      (or (lem:lem-relative-pathname pathname)
          (asdf:system-relative-pathname :lem-sdl3 pathname))))

(defun find-resource-pathname (pathname)
  "Return the pathname of the resource PATHNAME if that file exists, else NIL.
Use it for resources a packaged build may leave out, such as the Noto CJK
fonts on macOS, where the system's fonts are used instead."
  ;; In a deployed build lem-relative-pathname calls TRUENAME, which signals
  ;; an error for a missing file; that means "not there" here too.
  (handler-case (probe-file (get-resource-pathname pathname))
    (error () nil)))
