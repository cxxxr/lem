(defpackage :dexador
  (:nicknames :dex)
  (:use :cl)
  (:shadow #:get #:delete)
  (:export #:request #:get #:post #:head #:put #:patch #:delete #:fetch))
(in-package :dexador)

(macrolet ((define-requests (&rest names)
             `(progn
                ,@(mapcar (lambda (name)
                            `(defun ,name (uri &rest args)
                               (declare (ignore args))
                               (cl:error "No HTTP client on Clysm: cannot request ~A" uri)))
                          names))))
  (define-requests request get post head put patch delete fetch))
