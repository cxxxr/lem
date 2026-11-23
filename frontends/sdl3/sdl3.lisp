(defpackage :lem-sdl3/sdl3
  (:use :cl)
  (:export :sdl3))
(in-package :lem-sdl3/sdl3)

(defclass sdl3 (lem:implementation)
  ()
  (:default-initargs
   :name :sdl3
   :redraw-after-modifying-floating-window nil
   :support-pixel-positioning t
   :underline-color-support t
   :support-proportional-text t))

(pushnew :lem-sdl3 *features*)
