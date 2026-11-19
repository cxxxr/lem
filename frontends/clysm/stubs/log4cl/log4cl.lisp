(defpackage :log4cl
  (:nicknames :log)
  (:use :cl)
  (:shadow #:error #:warn #:debug #:trace)
  (:export #:config #:fatal #:error #:warn #:info #:debug #:trace #:*level*))
(in-package :log4cl)

(defvar *level* nil
  "NIL (nothing is logged) or one of :FATAL :ERROR :WARN :INFO :DEBUG :TRACE.")

(defparameter *levels* '(:fatal :error :warn :info :debug :trace))

(defun config (&rest options)
  (let ((level (find-if (lambda (o) (member o *levels*)) options)))
    (when level (setf *level* level)))
  nil)

(defun log-message (level format-control &rest args)
  (when (and *level* (<= (position level *levels*) (position *level* *levels*)))
    ;; (the page writes *error-output* to the console as errors)
    (format (if (member level '(:fatal :error :warn)) *error-output* *standard-output*)
            "~&[~A] ~?~%" level
            (if (stringp format-control) format-control "~S")
            (if (stringp format-control) args (cons format-control args))))
  nil)

(macrolet ((define-log-macro (name level)
             `(defmacro ,name (&rest args)
                (if args `(log-message ,',level ,@args) nil))))
  (define-log-macro fatal :fatal)
  (define-log-macro error :error)
  (define-log-macro warn :warn)
  (define-log-macro info :info)
  (define-log-macro debug :debug)
  (define-log-macro trace :trace))
