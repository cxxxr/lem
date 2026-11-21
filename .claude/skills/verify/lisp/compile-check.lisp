;;; Load Lem, then compile the files in VERIFY_FILES again and report their
;;; warnings.  Redefinition warnings are expected and not reported.
;;; Exits with 0 when no file had a warning, 1 otherwise.

(ql:quickload '(:lem-fake-interface :lem-tests) :silent t)

(defun verify-fasl-path (file)
  "Where to write FILE's fasl: under VERIFY_FASL_DIR at FILE's full path.
Not ASDF's cache: this image has not loaded every dependency of every file,
so ASDF must not take these fasls for its own."
  (let ((source (uiop:ensure-absolute-pathname file (uiop:getcwd))))
    (merge-pathnames
     (make-pathname :directory (cons :relative (cdr (pathname-directory source)))
                    :name (pathname-name source)
                    :type (pathname-type (compile-file-pathname "x.lisp")))
     (uiop:ensure-directory-pathname (uiop:getenv "VERIFY_FASL_DIR")))))

(defun rove-unmapped-fasl-warning-p (condition)
  "Rove warns once when it cannot map a fasl back to its source; the fasls
written here are outside ASDF's cache, so it does for a new test suite."
  (search "could not map FASL" (princ-to-string condition)))

(let ((count 0))
  (dolist (file (verify-env-list "VERIFY_FILES"))
    (handler-bind ((warning
                     (lambda (condition)
                       (unless (or (typep condition 'sb-kernel:redefinition-warning)
                                   (rove-unmapped-fasl-warning-p condition))
                         (incf count)
                         (format t "~&WARNING in ~a:~%  ~a~%" file condition))
                       (muffle-warning condition))))
      (let ((fasl (compile-file file
                                :output-file (ensure-directories-exist
                                              (verify-fasl-path file)))))
        (if fasl
            (load fasl)
            (progn (incf count)
                   (format t "~&FAILED to compile ~a~%" file))))))
  (format t "~&~d warning~:p~%" count)
  (uiop:quit (if (zerop count) 0 1)))
