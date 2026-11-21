(defpackage :lem-tests/terminal-start
  (:use :cl :rove))
(in-package :lem-tests/terminal-start)

;;; A terminal that cannot be started (terminal_new returns NULL) must give
;;; an editor-error, not a terminal whose I/O thread reads a null pointer.
;;; Linux only: it runs out of file descriptors with RLIMIT_NOFILE (7).

(defconstant +rlimit-nofile+ 7)

(defun library-available-p ()
  (not (null (cffi:foreign-symbol-pointer "terminal_new"))))

(defun call-with-no-free-fd (function)
  "Call FUNCTION while every file descriptor below a lowered soft
RLIMIT_NOFILE is in use, so that any pipe or open fails with EMFILE."
  (cffi:with-foreign-object (limit :uint64 2)
    (assert (zerop (cffi:foreign-funcall "getrlimit" :int +rlimit-nofile+
                                                     :pointer limit :int)))
    (let ((soft (cffi:mem-aref limit :uint64 0))
          (fillers '()))
      (unwind-protect
           (progn
             (setf (cffi:mem-aref limit :uint64 0) (min soft 1024))
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
        (setf (cffi:mem-aref limit :uint64 0) soft)
        (cffi:foreign-funcall "setrlimit" :int +rlimit-nofile+ :pointer limit :int)))))

(deftest a-terminal-that-cannot-start-is-an-editor-error
  #-linux (skip "needs Linux's RLIMIT_NOFILE")
  #+linux
  (if (library-available-p)
      (let ((buffer (lem:make-buffer "*terminal start test*" :temporary t))
            (terminals (length lem-terminal/terminal::*terminals*))
            (threads (length (bt2:all-threads)))
            (condition nil))
        (call-with-no-free-fd
         (lambda ()
           (handler-case
               (lem-terminal/terminal:create :rows 24 :cols 80 :buffer buffer
                                             :directory "" :program "/bin/sh"
                                             :argv (list "/bin/sh" "-c" "exit 0"))
             (lem:editor-error (c) (setf condition c)))))
        (ok condition "create signals editor-error")
        (when condition
          (ok (search "Too many open files" (princ-to-string condition))
              (format nil "the message gives the reason: ~A" condition)))
        (ok (= terminals (length lem-terminal/terminal::*terminals*))
            "no terminal is registered")
        (ok (= threads (length (bt2:all-threads))) "no I/O thread is started"))
      (skip "terminal.so is not built")))
