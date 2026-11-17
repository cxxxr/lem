(defpackage :lem-sdl3/tests/keyboard
  (:use :cl :rove)
  (:local-nicknames (:event :lem-sdl3/event)
                    (:keyboard :lem-sdl3/keyboard)))
(in-package :lem-sdl3/tests/keyboard)

(deftest keycodes-map-to-lem-key-names
  (flet ((sym (code) (nth-value 0 (keyboard::convert-to-sym code)))
         (text-input-p (code) (nth-value 1 (keyboard::convert-to-sym code))))
    (ok (equal (sym event:+sdlk-return+) "Return"))
    (ok (equal (sym event:+sdlk-pageup+) "PageUp"))
    (ok (equal (sym event:+sdlk-menu+) "ContextMenu"))
    (ok (equal (sym (event:sdlk-function-key 5)) "F5"))
    (ok (equal (sym (event:sdlk-function-key 24)) "F24"))
    (ok (equal (sym (char-code #\a)) "a"))
    (ok (text-input-p (char-code #\a)))
    (ng (text-input-p event:+sdlk-left+))))

(deftest modifiers-from-keymod
  (let ((modifier (keyboard::key-event-modifier
                   (keyboard:make-key-event-from-sdl
                    (char-code #\x)
                    (logior event:+kmod-lctrl+ event:+kmod-lalt+)))))
    (ok (keyboard::modifier-ctrl modifier))
    (ok (keyboard::modifier-meta modifier))
    (ng (keyboard::modifier-shift modifier))
    (ng (keyboard::modifier-super modifier))))
