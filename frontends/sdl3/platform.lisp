(defpackage :lem-sdl3/platform
  (:use :cl)
  (:export :platform
           :linux
           :mac
           :windows
           :get-platform))
(in-package :lem-sdl3/platform)

(defclass platform () ())
(defclass linux (platform) ())
(defclass windows (platform) ())
(defclass mac (platform) ())

(defvar *platform* nil)

(defun get-platform ()
  (or *platform*
      (setf *platform*
            (let ((platform-name (lem-sdl3/ffi:platform-name)))
              (alexandria:switch (platform-name :test #'equal)
                ("Windows"
                 (make-instance 'windows))
                ("macOS"
                 (make-instance 'mac))
                (otherwise
                 (make-instance 'linux)))))))
