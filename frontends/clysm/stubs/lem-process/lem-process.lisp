(defpackage :lem-process
  (:use :cl)
  (:export #:run-process #:process-send-input #:process-alive-p #:delete-process))
(in-package :lem-process)

(defun run-process (&rest arguments)
  (declare (ignore arguments))
  (error "Lem on Clysm cannot run programs: there is only the Lisp that Lem runs in"))

(defun process-send-input (process string)
  (declare (ignore process string))
  nil)

(defun process-alive-p (process)
  (declare (ignore process))
  nil)

(defun delete-process (process)
  (declare (ignore process))
  nil)
