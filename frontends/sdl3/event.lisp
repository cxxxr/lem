(defpackage :lem-sdl3/event
  (:use :cl)
  (:export
   ;; event types
   :+quit+
   :+key-down+
   :+key-up+
   :+text-editing+
   :+text-input+
   :+mouse-motion+
   :+mouse-button-down+
   :+mouse-button-up+
   :+mouse-wheel+
   :+drop-file+
   :+window-shown+
   :+window-exposed+
   :+window-resized+
   :+window-pixel-size-changed+
   :+window-display-changed+
   :+window-display-scale-changed+
   :+window-focus-gained+
   :+window-focus-lost+
   ;; mouse buttons
   :+button-left+
   :+button-middle+
   :+button-right+
   :+button-x1+
   :+button-lmask+
   ;; key modifiers
   :+kmod-lshift+
   :+kmod-rshift+
   :+kmod-lctrl+
   :+kmod-rctrl+
   :+kmod-lalt+
   :+kmod-ralt+
   :+kmod-lgui+
   :+kmod-rgui+
   ;; keycodes
   :+sdlk-backspace+
   :+sdlk-tab+
   :+sdlk-return+
   :+sdlk-escape+
   :+sdlk-space+
   :+sdlk-delete+
   :+sdlk-insert+
   :+sdlk-home+
   :+sdlk-end+
   :+sdlk-pageup+
   :+sdlk-pagedown+
   :+sdlk-application+
   :+sdlk-menu+
   :+sdlk-left+
   :+sdlk-right+
   :+sdlk-up+
   :+sdlk-down+
   :sdlk-function-key
   ;; reading events
   :with-event
   :wait-event
   :push-quit-event
   :event-type
   :key-code
   :key-modifiers
   :event-text
   :mouse-x
   :mouse-y
   :mouse-button
   :mouse-clicks
   :mouse-state
   :wheel-x
   :wheel-y
   :drop-file-name))
(in-package :lem-sdl3/event)

;;; Reading SDL_Event without cl-sdl3's event objects.
;;;
;;; cl-sdl3 converts each event into a CLOS instance and keycodes into enum
;;; keywords.  The event loop only needs a handful of fields, so they are read
;;; straight out of the SDL_Event union at the offsets below.  All values were
;;; taken from the SDL3 headers (SDL_events.h, SDL_keycode.h, SDL_mouse.h);
;;; they are part of SDL3's stable ABI.

(defconstant +sdl-event-size+ 128)

;;; Event types (SDL_EventType)
(defconstant +quit+ #x100)
(defconstant +window-shown+ #x202)
(defconstant +window-exposed+ #x204)
(defconstant +window-resized+ #x206)
(defconstant +window-pixel-size-changed+ #x207)
(defconstant +window-focus-gained+ #x20e)
(defconstant +window-focus-lost+ #x20f)
(defconstant +window-display-changed+ #x213)
(defconstant +window-display-scale-changed+ #x214)
(defconstant +key-down+ #x300)
(defconstant +key-up+ #x301)
(defconstant +text-editing+ #x302)
(defconstant +text-input+ #x303)
(defconstant +mouse-motion+ #x400)
(defconstant +mouse-button-down+ #x401)
(defconstant +mouse-button-up+ #x402)
(defconstant +mouse-wheel+ #x403)
(defconstant +drop-file+ #x1000)

;;; Mouse buttons
(defconstant +button-left+ 1)
(defconstant +button-middle+ 2)
(defconstant +button-right+ 3)
(defconstant +button-x1+ 4)
(defconstant +button-lmask+ #x1)

;;; Key modifiers (SDL_Keymod); same values as SDL2's KMOD_*
(defconstant +kmod-lshift+ #x0001)
(defconstant +kmod-rshift+ #x0002)
(defconstant +kmod-lctrl+ #x0040)
(defconstant +kmod-rctrl+ #x0080)
(defconstant +kmod-lalt+ #x0100)
(defconstant +kmod-ralt+ #x0200)
(defconstant +kmod-lgui+ #x0400)
(defconstant +kmod-rgui+ #x0800)

;;; Keycodes (SDL_Keycode); same values as SDL2's SDLK_*
(defconstant +sdlk-backspace+ #x08)
(defconstant +sdlk-tab+ #x09)
(defconstant +sdlk-return+ #x0d)
(defconstant +sdlk-escape+ #x1b)
(defconstant +sdlk-space+ #x20)
(defconstant +sdlk-delete+ #x7f)
(defconstant +sdlk-insert+ #x40000049)
(defconstant +sdlk-home+ #x4000004a)
(defconstant +sdlk-pageup+ #x4000004b)
(defconstant +sdlk-end+ #x4000004d)
(defconstant +sdlk-pagedown+ #x4000004e)
(defconstant +sdlk-right+ #x4000004f)
(defconstant +sdlk-left+ #x40000050)
(defconstant +sdlk-down+ #x40000051)
(defconstant +sdlk-up+ #x40000052)
(defconstant +sdlk-application+ #x40000065)
(defconstant +sdlk-menu+ #x40000076)

(defun sdlk-function-key (n)
  "Return the keycode of function key Fn, for N from 1 to 24."
  (check-type n (integer 1 24))
  (if (<= n 12)
      (+ #x4000003a (1- n))             ; SDLK_F1 .. SDLK_F12
      (+ #x40000068 (- n 13))))         ; SDLK_F13 .. SDLK_F24

;;; Field offsets within SDL_Event
(defconstant +offset-type+ 0)
(defconstant +offset-key-key+ 28)       ; SDL_KeyboardEvent.key
(defconstant +offset-key-mod+ 32)       ; SDL_KeyboardEvent.mod
(defconstant +offset-text+ 24)          ; SDL_TextInputEvent/SDL_TextEditingEvent.text
(defconstant +offset-motion-state+ 24)  ; SDL_MouseMotionEvent.state
(defconstant +offset-mouse-x+ 28)       ; SDL_MouseMotionEvent/SDL_MouseButtonEvent.x
(defconstant +offset-mouse-y+ 32)       ; ... .y
(defconstant +offset-button+ 24)        ; SDL_MouseButtonEvent.button
(defconstant +offset-clicks+ 26)        ; SDL_MouseButtonEvent.clicks
(defconstant +offset-wheel-x+ 24)       ; SDL_MouseWheelEvent.x (float)
(defconstant +offset-wheel-y+ 28)       ; SDL_MouseWheelEvent.y (float)
(defconstant +offset-wheel-integer-x+ 44) ; SDL_MouseWheelEvent.integer_x (SDL >= 3.2.12)
(defconstant +offset-wheel-integer-y+ 48) ; SDL_MouseWheelEvent.integer_y (SDL >= 3.2.12)
(defconstant +offset-drop-data+ 40)     ; SDL_DropEvent.data

(cffi:defcfun ("SDL_GetVersion" %get-version) :int)

(defvar *integer-wheel-p* :unknown
  "Whether the running SDL has SDL_MouseWheelEvent.integer_x/y (3.2.12+).
Computed on first use.")

(defun integer-wheel-p ()
  (when (eq *integer-wheel-p* :unknown)
    (setf *integer-wheel-p* (<= 3002012 (%get-version))))
  *integer-wheel-p*)

(defmacro with-event ((var) &body body)
  "Bind VAR to a zeroed, stack-allocated SDL_Event during BODY."
  `(cffi:with-foreign-object (,var :uint8 +sdl-event-size+)
     (dotimes (i +sdl-event-size+) (setf (cffi:mem-aref ,var :uint8 i) 0))
     ,@body))

(defun wait-event (event)
  "Block until an event arrives and store it in EVENT.  Return true on success."
  (sdl3:wait-event event))

(defun push-quit-event ()
  "Push an SDL_EVENT_QUIT so the event loop exits."
  (with-event (event)
    (setf (cffi:mem-ref event :uint32 +offset-type+) +quit+)
    (sdl3:push-event event)))

(defun event-type (event)
  "Return EVENT's SDL_EventType as an integer."
  (cffi:mem-ref event :uint32 +offset-type+))

(defun key-code (event)
  "Return the SDL_Keycode of a key event."
  (cffi:mem-ref event :uint32 +offset-key-key+))

(defun key-modifiers (event)
  "Return the SDL_Keymod bitmask of a key event."
  (cffi:mem-ref event :uint16 +offset-key-mod+))

(defun event-text (event)
  "Return the UTF-8 text of a text-input or text-editing event as a string."
  (let ((pointer (cffi:mem-ref event :pointer +offset-text+)))
    (if (cffi:null-pointer-p pointer)
        ""
        (cffi:foreign-string-to-lisp pointer :encoding :utf-8))))

(defun mouse-x (event)
  "Return the x position of a mouse motion or button event, in window coordinates."
  (cffi:mem-ref event :float +offset-mouse-x+))

(defun mouse-y (event)
  "Return the y position of a mouse motion or button event, in window coordinates."
  (cffi:mem-ref event :float +offset-mouse-y+))

(defun mouse-button (event)
  "Return the button index of a mouse button event."
  (cffi:mem-ref event :uint8 +offset-button+))

(defun mouse-clicks (event)
  "Return the click count (1 single, 2 double, ...) of a mouse button event."
  (cffi:mem-ref event :uint8 +offset-clicks+))

(defun mouse-state (event)
  "Return the button-state bitmask of a mouse motion event."
  (cffi:mem-ref event :uint32 +offset-motion-state+))

(defun wheel-x (event)
  "Return the horizontal scroll of a wheel event in whole ticks."
  (if (integer-wheel-p)
      (cffi:mem-ref event :int32 +offset-wheel-integer-x+)
      (round (cffi:mem-ref event :float +offset-wheel-x+))))

(defun wheel-y (event)
  "Return the vertical scroll of a wheel event in whole ticks."
  (if (integer-wheel-p)
      (cffi:mem-ref event :int32 +offset-wheel-integer-y+)
      (round (cffi:mem-ref event :float +offset-wheel-y+))))

(defun drop-file-name (event)
  "Return the file name of a drop-file event."
  (cffi:foreign-string-to-lisp (cffi:mem-ref event :pointer +offset-drop-data+)
                               :encoding :utf-8))
