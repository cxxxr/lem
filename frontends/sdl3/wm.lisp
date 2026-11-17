(defpackage :lem-sdl3/wm
  (:use :cl)
  (:export :set-x11-wm-class))
(in-package :lem-sdl3/wm)

(defparameter +lem-app-id+ "lem")

;; SDL3 derives the X11 WM_CLASS and the Wayland app_id from SDL_HINT_APP_ID.
;; Must be called before SDL_Init.
;; cf. https://wiki.libsdl.org/SDL3/SDL_HINT_APP_ID
(defun set-x11-wm-class ()
  (sdl3:set-hint "SDL_APP_ID" +lem-app-id+))
