;;; Load Lem, then compile the files in VERIFY_FILES again and report their
;;; warnings.  Redefinition warnings are expected and not reported.
;;; Exits with 0 when no file had a warning, 1 otherwise.

(ql:quickload '(:lem-fake-interface :lem-tests) :silent t)

(let ((count 0))
  (dolist (file (verify-env-list "VERIFY_FILES"))
    (handler-bind ((warning
                     (lambda (condition)
                       (unless (typep condition 'sb-kernel:redefinition-warning)
                         (incf count)
                         (format t "~&WARNING in ~a:~%  ~a~%" file condition))
                       (muffle-warning condition))))
      ;; Write the fasl where ASDF would (VERIFY_CACHE): Rove maps a test
      ;; file's fasl back to its source only there, and warns otherwise.
      (let ((fasl (compile-file file
                                :output-file (uiop:compile-file-pathname*
                                              (uiop:ensure-absolute-pathname
                                               file (uiop:getcwd))))))
        (if fasl
            (load fasl)
            (progn (incf count)
                   (format t "~&FAILED to compile ~a~%" file))))))
  (format t "~&~d warning~:p~%" count)
  (uiop:quit (if (zerop count) 0 1)))
