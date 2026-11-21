;;; Load Lem, then compile the files in VERIFY_FILES again and report their
;;; warnings.  Redefinition warnings are expected and not reported.
;;; Exits with 0 when no file had a warning, 1 otherwise.

;; Loading compiles the changed files of these systems first: a warning there
;; must not end the run, so that the compile below reports it.
(let ((uiop:*compile-file-failure-behaviour* :warn))
  (ql:quickload '(:lem-fake-interface :lem-tests) :silent t))

(defun verify-absolute-file (file)
  "FILE (relative to the repository, the current directory) as an absolute pathname."
  (uiop:ensure-absolute-pathname file (uiop:getcwd)))

(defun verify-fasl-path (file)
  "Where to write FILE's fasl: under VERIFY_FASL_DIR at FILE's full path.
Not ASDF's cache: this image has not loaded every dependency of every file,
so ASDF must not take these fasls for its own."
  (let ((source (verify-absolute-file file)))
    (merge-pathnames
     (make-pathname :directory (cons :relative (cdr (pathname-directory source)))
                    :name (pathname-name source)
                    :type (pathname-type (compile-file-pathname "x.lisp")))
     (uiop:ensure-directory-pathname (uiop:getenv "VERIFY_FASL_DIR")))))

(defun verify-own-fasl-warning-p (condition)
  "True when CONDITION is Rove's warning that it could not map a fasl back to
its source file, for a fasl under VERIFY_FASL_DIR.  The fasls written here are
outside ASDF's cache, so Rove cannot map them and warns when a compiled file
defines a test package.  The same warning for another fasl means a broken ASDF
configuration, and is reported."
  (let ((message (princ-to-string condition)))
    (and (search "could not map FASL" message)
         (search (namestring (uiop:ensure-directory-pathname (uiop:getenv "VERIFY_FASL_DIR")))
                 message))))

(defun verify-component-files (component)
  "The source files of the ASDF COMPONENT and of its children."
  (typecase component
    (asdf:source-file (list (asdf:component-pathname component)))
    (asdf:parent-component (mapcan #'verify-component-files
                                   (copy-list (asdf:component-children component))))))

(defun verify-file-system (file)
  "The name of the ASDF system that has FILE as a component, or NIL.
Looks at the .asd files of FILE's directory and of the directories above it,
up to the repository."
  (let ((source (uiop:truenamize (verify-absolute-file file)))
        (repo (uiop:truenamize (uiop:getcwd))))
    (loop :for directory := (uiop:pathname-directory-pathname source)
            :then (uiop:pathname-parent-directory-pathname directory)
          :while (uiop:subpathp directory repo)
          :do (dolist (asd (uiop:directory-files directory "*.asd"))
                (unless (find asd (asdf:registered-systems)
                              :key (lambda (name)
                                     (let ((system (asdf:registered-system name)))
                                       (and system (asdf:system-source-file system))))
                              :test #'equal)
                  (asdf:load-asd asd))
                (dolist (name (asdf:registered-systems))
                  (let ((system (asdf:registered-system name)))
                    (when (and system
                               (equal (asdf:system-source-file system) asd)
                               (member source (verify-component-files system)
                                       :test #'uiop:pathname-equal))
                      (return-from verify-file-system name))))))
    nil))

(defun verify-load-file-system (file)
  "Load the system of FILE when it is not loaded yet (lem-tests does not load
every system, e.g. the frontends), so that FILE's packages exist.
Returns NIL, after printing why, when the system cannot be loaded here (a
frontend whose foreign library is missing, say); T otherwise."
  (handler-case
      (let ((system (verify-file-system file)))
        (when (and system (not (asdf:component-loaded-p system)))
          (format t "~&loading ~a for ~a~%" system file)
          ;; The system's load compiles FILE too: a warning there must not
          ;; fail the load, so that the compile below reports it.
          (let ((uiop:*compile-file-failure-behaviour* :warn))
            (ql:quickload system :silent t)))
        t)
    (error (condition)
      (format t "~&FAILED to load the system of ~a:~%  ~a~%" file condition)
      nil)))

(let ((count 0))
  (dolist (file (verify-env-list "VERIFY_FILES"))
    (if (not (verify-load-file-system file))
        (incf count)
        (handler-bind ((warning
                         (lambda (condition)
                           (unless (or (typep condition 'sb-kernel:redefinition-warning)
                                       (verify-own-fasl-warning-p condition))
                             (incf count)
                             (format t "~&WARNING in ~a:~%  ~a~%" file condition))
                           (muffle-warning condition))))
          ;; An error (an IN-PACKAGE of a package that does not exist here)
          ;; fails this file only, not the whole run.
          (handler-case
              (let ((fasl (compile-file file
                                        :output-file (ensure-directories-exist
                                                      (verify-fasl-path file)))))
                (if fasl
                    (load fasl)
                    (progn (incf count)
                           (format t "~&FAILED to compile ~a~%" file))))
            (error (condition)
              (incf count)
              (format t "~&FAILED to compile ~a:~%  ~a~%" file condition))))))
  (format t "~&~d warning~:p~%" count)
  (uiop:quit (if (zerop count) 0 1)))
