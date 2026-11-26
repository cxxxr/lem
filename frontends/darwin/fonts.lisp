(defpackage :lem-darwin/fonts
  (:use :cl)
  (:export :available-font-files))
(in-package :lem-darwin/fonts)

;;; The font files installed on macOS, asked of Core Text.
;;;
;;; GUI frontends list fonts so that a font can be chosen by name.  macOS
;;; has no fontconfig unless Homebrew installs it, so `fc-list' is missing
;;; on most Macs, and a packaged Lem.app cannot rely on it.  Core Text and
;;; CoreFoundation are plain C APIs, called here through CFFI, so this works
;;; on every macOS architecture; elsewhere it returns NIL.

(defparameter *path-buffer-size* 4096
  "Bytes in the buffer a font file's path is written into (PATH_MAX is 1024
on macOS, so this is ample).")

#+darwin
(progn

(cffi:define-foreign-library core-text
  (:darwin (:framework "CoreText")))

(cffi:use-foreign-library core-text)

(cffi:defcfun ("CTFontManagerCopyAvailableFontURLs" copy-available-font-urls) :pointer)

(cffi:defcfun ("CFArrayGetCount" array-count) :long
  (array :pointer))

(cffi:defcfun ("CFArrayGetValueAtIndex" array-value) :pointer
  (array :pointer)
  (index :long))

(cffi:defcfun ("CFURLGetFileSystemRepresentation" url-file-system-representation) :boolean
  (url :pointer)
  (resolve-against-base :boolean)
  (buffer :pointer)
  (buffer-size :long))

(cffi:defcfun ("CFRelease" cf-release) :void
  (object :pointer))

(defun url-native-namestring (url)
  "Return the file-system path of the CFURL URL, or NIL if it has none."
  (cffi:with-foreign-pointer (buffer *path-buffer-size*)
    (when (url-file-system-representation url t buffer *path-buffer-size*)
      (cffi:foreign-string-to-lisp buffer :encoding :utf-8))))

)

(defun available-font-files ()
  "Return the native namestrings of the font files installed on macOS,
without duplicates (a .ttc file holding several faces appears once), or NIL
on other systems or if Core Text cannot list them."
  #-darwin
  nil
  #+darwin
  (let ((urls (copy-available-font-urls)))
    (unless (cffi:null-pointer-p urls)
      (unwind-protect
           (remove-duplicates
            (loop :for index :below (array-count urls)
                  :for file := (url-native-namestring (array-value urls index))
                  :when file
                    :collect file)
            :test #'string=)
        (cf-release urls)))))
