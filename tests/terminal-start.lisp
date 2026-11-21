(defpackage :lem-tests/terminal-start
  (:use :cl :rove))
(in-package :lem-tests/terminal-start)

;;; A terminal that cannot be started (terminal_new returns NULL) must give
;;; an editor-error, not a terminal whose I/O thread reads a null pointer.
;;; It runs out of file descriptors with RLIMIT_NOFILE, whose number (7) and
;;; struct (two unsigned longs) are those of Linux on x86-64 and arm64.

(defconstant +rlimit-nofile+ 7)
(defconstant +emfile+ 24)

(defun library-available-p ()
  "True when terminal.so is loaded and has this failure handling: an older
build aborts in forkpty when it is out of file descriptors."
  (not (null (cffi:foreign-symbol-pointer "terminal_last_error"))))

(defun strerror (errno)
  (cffi:foreign-funcall "strerror" :int errno :string))

(defun terminal-thread-names ()
  (loop :for thread :in (bt2:all-threads)
        :for name := (bt2:thread-name thread)
        :when (and (stringp name) (alexandria:starts-with-subseq "Terminal " name))
          :collect name))

(defun call-with-no-free-fd (function)
  "Call FUNCTION while every file descriptor below a lowered soft
RLIMIT_NOFILE is in use, so that any pipe or open fails with EMFILE."
  (cffi:with-foreign-object (limit :unsigned-long 2)
    (assert (zerop (cffi:foreign-funcall "getrlimit" :int +rlimit-nofile+
                                                     :pointer limit :int)))
    (let ((soft (cffi:mem-aref limit :unsigned-long 0))
          (fillers '()))
      (unwind-protect
           (progn
             (setf (cffi:mem-aref limit :unsigned-long 0) (min soft 1024))
             (assert (zerop (cffi:foreign-funcall "setrlimit" :int +rlimit-nofile+
                                                              :pointer limit :int)))
             (loop :for fd := (cffi:foreign-funcall "open" :string "/dev/null"
                                                    :int 0 :int)
                   :repeat 2048
                   :while (<= 0 fd)
                   :do (push fd fillers))
             (funcall function))
        (dolist (fd fillers)
          (cffi:foreign-funcall "close" :int fd :int))
        (setf (cffi:mem-aref limit :unsigned-long 0) soft)
        (cffi:foreign-funcall "setrlimit" :int +rlimit-nofile+ :pointer limit :int)))))

(deftest a-terminal-that-cannot-start-is-an-editor-error
  #-(and linux (or x86-64 arm64)) (skip "needs Linux on x86-64 or arm64")
  #+(and linux (or x86-64 arm64))
  (if (library-available-p)
      (let ((buffer (lem:make-buffer "*terminal start test*" :temporary t))
            (terminals (length lem-terminal/terminal::*terminals*))
            (threads (terminal-thread-names))
            (terminal nil)
            (condition nil))
        (unwind-protect
             (call-with-no-free-fd
              (lambda ()
                (handler-case
                    (setf terminal
                          (lem-terminal/terminal:create
                           :rows 24 :cols 80 :buffer buffer
                           :directory "" :program "/bin/sh"
                           :argv (list "/bin/sh" "-c" "exit 0")))
                  (lem:editor-error (c) (setf condition c)))))
          (when terminal
            (lem-terminal/terminal:destroy terminal)))
        (ok condition "create signals editor-error")
        (when condition
          (ok (search (strerror +emfile+) (princ-to-string condition))
              (format nil "the message gives the reason: ~A" condition)))
        (ok (= terminals (length lem-terminal/terminal::*terminals*))
            "no terminal is registered")
        (ok (equal threads (terminal-thread-names)) "no I/O thread is started"))
      (skip "terminal.so is not built or is older than its failure handling")))
