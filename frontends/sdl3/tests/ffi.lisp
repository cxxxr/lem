(defpackage :lem-sdl3/tests/ffi
  (:use :cl :rove))
(in-package :lem-sdl3/tests/ffi)

(deftest platform-name
  (ok (member (lem-sdl3/ffi:platform-name)
              '("macOS" "Linux" "Windows" "FreeBSD" "OpenBSD" "NetBSD")
              :test #'string=)))

(deftest clipboard-text-without-video-subsystem
  ;; Without SDL_Init(VIDEO) SDL returns an empty string; we map it to NIL.
  (ok (null (lem-sdl3/ffi:get-clipboard-text))))
