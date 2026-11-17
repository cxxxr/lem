(defpackage :lem-sdl3/mouse
  (:use :cl)
  (:export :cursor-shown-p
           :show-cursor
           :hide-cursor))
(in-package :lem-sdl3/mouse)

(defvar *cursor-shown* t)

(defun cursor-shown-p ()
  *cursor-shown*)

(defun show-cursor ()
  (setf *cursor-shown* t)
  (lem-sdl3/ffi:show-cursor))

(defun hide-cursor ()
  (setf *cursor-shown* nil)
  (lem-sdl3/ffi:hide-cursor))
