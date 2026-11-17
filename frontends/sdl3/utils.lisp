(defpackage :lem-sdl3/utils
  (:use :cl)
  (:export :create-texture
           :render-texture))
(in-package :lem-sdl3/utils)

(defun create-texture (renderer width height)
  (lem-sdl3/ffi:create-target-texture renderer width height))

(defun render-texture (renderer texture x y width height)
  (lem-sdl3/ffi:with-frect (dest-rect x y width height)
    (lem-sdl3/ffi:render-texture renderer texture nil dest-rect)))
