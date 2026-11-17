(defpackage :lem-sdl3/ffi
  (:use :cl)
  (:export :get-clipboard-text
           :platform-name))
(in-package :lem-sdl3/ffi)

;;; Bindings that cl-sdl3 lacks or binds in a way unsuitable for Lem.

(cffi:defcfun ("SDL_free" %sdl-free) :void
  (pointer :pointer))

(cffi:defcfun ("SDL_GetClipboardText" %get-clipboard-text) :pointer)

(defun get-clipboard-text ()
  "Return the clipboard contents as a string, or NIL when it is empty.
Unlike SDL3:GET-CLIPBOARD-TEXT, this frees the buffer SDL allocates."
  (let ((pointer (%get-clipboard-text)))
    (unless (cffi:null-pointer-p pointer)
      (unwind-protect
           (let ((text (cffi:foreign-string-to-lisp pointer :encoding :utf-8)))
             (if (string= text "") nil text))
        (%sdl-free pointer)))))

(defun platform-name ()
  "Return the platform name reported by SDL, e.g. \"macOS\", \"Linux\" or \"Windows\"."
  (sdl3:get-platform))
