;;; Load Lem, then compile the files in VERIFY_FILES again and report their
;;; warnings and the files that failed to compile or to load.  Redefinition
;;; warnings are expected and not reported.
;;; Exits with 0 when no file had a warning or failed, 1 otherwise.

(defvar *verify-count* 0
  "The number of warnings and failures reported so far.")

(defvar *verify-failed-fasls* '()
  "The output files under VERIFY_CACHE of files whose compilation failed while
`verify-quickload' loaded them.")

(defvar *verify-failing-file* nil
  "The source file whose compile or load signalled the error that is unwinding,
or NIL.  Set by the :around method on `asdf:perform' below.")

(defvar *verify-failed-systems* '()
  "The systems that `verify-load-file-system' could not load.")

(defun verify-cache-file-p (file)
  "True when FILE is under VERIFY_CACHE."
  (uiop:subpathp file (uiop:ensure-directory-pathname (uiop:getenv "VERIFY_CACHE"))))

(defmethod asdf:perform :around ((operation asdf:compile-op) (component asdf:cl-source-file))
  ;; With *compile-file-failure-behaviour* :warn, a file that fails to compile
  ;; (a "caught ERROR" in a form, say) keeps its fasl in VERIFY_CACHE, newer
  ;; than its source.  `verify build' and `verify tests' would then load that
  ;; fasl without compiling the file and without failing.  Note its output
  ;; files so that `verify-quickload' can delete them.  Not the fasls of
  ;; other caches: other builds use them.
  (handler-bind ((uiop:compile-failed-warning
                   (lambda (condition)
                     (declare (ignore condition))
                     (setf *verify-failed-fasls*
                           (append (remove-if-not #'verify-cache-file-p
                                                  (asdf:output-files operation component))
                                   *verify-failed-fasls*)))))
    (call-next-method)))

(defmethod asdf:perform :around ((operation asdf:operation) (component asdf:cl-source-file))
  ;; Note the file of an error, so that `verify-quickload' can name it: a
  ;; top-level (error ...) says nothing of where it is.  An error the file
  ;; handles itself is forgotten when its perform returns.
  (handler-bind ((error (lambda (condition)
                          (declare (ignore condition))
                          (setf *verify-failing-file* (asdf:component-pathname component)))))
    (multiple-value-prog1 (call-next-method)
      (setf *verify-failing-file* nil))))

(defun verify-quickload (systems)
  "Quickload SYSTEMS, letting a file that fails to compile with a warning or
a \"caught ERROR\" go on: the compile below reports it.  Afterwards delete the
fasls of the files that failed, so that the next load compiles them again.
Returns T when the load succeeded; otherwise prints the error, with the file
that signalled it, and returns NIL."
  (setf *verify-failing-file* nil)
  (unwind-protect
       (handler-case
           (let ((uiop:*compile-file-failure-behaviour* :warn))
             (ql:quickload systems :silent t)
             t)
         (error (condition)
           (format t "~&FAILED to load ~{~(~a~)~^, ~}:~%  ~a~%"
                   (uiop:ensure-list systems) condition)
           (when *verify-failing-file*
             (format t "~&  in ~a~%"
                     (enough-namestring *verify-failing-file* (uiop:getcwd))))
           nil))
    (mapc #'uiop:delete-file-if-exists *verify-failed-fasls*)
    (setf *verify-failed-fasls* '())))

;; A file of these systems that does not load (unbalanced parentheses, a
;; top-level form that signals an error) must not end the run: the error is
;; printed with its file, and each file below whose system does not load
;; fails (without its own compile: the image lacks what the file needs).
(unless (verify-quickload '(:lem-fake-interface :lem-tests))
  (incf *verify-count*))

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

(defun verify-defpackage-file-p (source)
  "True when the first form of the file SOURCE is a DEFPACKAGE or a
UIOP:DEFINE-PACKAGE, as ASDF needs of a file of a package-inferred-system."
  (let ((form (ignore-errors
               (with-open-file (stream source)
                 (uiop:with-safe-io-syntax (:package :cl-user)
                   (read stream nil nil))))))
    (and (consp form)
         (symbolp (first form))
         (member (symbol-name (first form)) '("DEFPACKAGE" "DEFINE-PACKAGE")
                 :test #'string=))))

(defun verify-inferred-system (system source)
  "The name of the system SOURCE is in as a file of the package-inferred-system
SYSTEM that is not one of its components, or NIL.  Such a file is the system
SYSTEM/PATH (PATH relative to SYSTEM's directory, without the type), which
ASDF finds by that path when the file begins with a package definition."
  (when (and (typep system 'asdf:package-inferred-system)
             (uiop:subpathp source (asdf:component-pathname system))
             (verify-defpackage-file-p source))
    (let ((relative (uiop:enough-pathname source (asdf:component-pathname system))))
      (string-downcase
       (format nil "~a/~a" (asdf:component-name system)
               (namestring (make-pathname :type nil :defaults relative)))))))

(defun verify-file-system (file)
  "The name of the ASDF system that has FILE as a component or, in a
package-inferred-system, that FILE is (see `verify-inferred-system'), or
NIL.  Looks at the .asd files of FILE's directory and of the directories above
it, up to the repository.
The second value is a list of (ASD . ERROR) for the .asd files that could not
be loaded on the way: they are skipped, as the system may be in another one."
  (let ((source (uiop:truenamize (verify-absolute-file file)))
        (repo (uiop:truenamize (uiop:getcwd)))
        (failed '()))
    (loop :for directory := (uiop:pathname-directory-pathname source)
            :then (uiop:pathname-parent-directory-pathname directory)
          :while (uiop:subpathp directory repo)
          :do (dolist (asd (uiop:directory-files directory "*.asd"))
                (unless (find asd (asdf:registered-systems)
                              :key (lambda (name)
                                     (let ((system (asdf:registered-system name)))
                                       (and system (asdf:system-source-file system))))
                              :test #'equal)
                  (handler-case (asdf:load-asd asd)
                    (error (condition)
                      (push (cons asd condition) failed))))
                (dolist (name (asdf:registered-systems))
                  (let ((system (asdf:registered-system name)))
                    (when (and system (equal (asdf:system-source-file system) asd))
                      (let ((found (if (member source (verify-component-files system)
                                               :test #'uiop:pathname-equal)
                                       name
                                       (verify-inferred-system system source))))
                        (when found
                          (return-from verify-file-system
                            (values found (reverse failed))))))))))
    (values nil (nreverse failed))))

(defun verify-print-failed-asds (failed-asds)
  "Print the (ASD . ERROR) of FAILED-ASDS, one .asd file and its error each."
  (format t "~:{~&  could not load ~a:~%    ~a~%~}"
          (mapcar (lambda (failure)
                    (list (enough-namestring (car failure) (uiop:getcwd))
                          (cdr failure)))
                  failed-asds)))

(defun verify-load-file-system (file)
  "Load the system of FILE when it is not loaded yet (lem-tests does not load
every system, e.g. the frontends), so that FILE's packages exist.
Returns :NONE when FILE is in no system, NIL, after printing why, when the
system cannot be found or loaded here (a frontend whose foreign library is
missing, say), and T otherwise.  A system that failed to load is not loaded
again for the next file of it."
  (handler-case
      (multiple-value-bind (system failed-asds) (verify-file-system file)
        (cond (system
               (when failed-asds
                 ;; Not FILE's system: noted, as nothing else checks them.
                 (format t "~&note: skipped .asd files that do not load, looking for ~a:~%" file)
                 (verify-print-failed-asds failed-asds))
               (cond ((member system *verify-failed-systems* :test #'string=)
                      (format t "~&FAILED: ~a of ~a did not load, see above~%"
                              system file)
                      nil)
                     ((asdf:component-loaded-p system) t)
                     (t
                      (format t "~&loading ~a for ~a~%" system file)
                      (or (verify-quickload system)
                          (progn (push system *verify-failed-systems*) nil)))))
              (failed-asds
               ;; FILE may be in one of these.
               (format t "~&FAILED to find the system of ~a:~%" file)
               (verify-print-failed-asds failed-asds)
               nil)
              (t :none)))
    (error (condition)
      (format t "~&FAILED to load the system of ~a:~%  ~a~%" file condition)
      nil)))

(defun verify-compile-file (file load)
  "Compile FILE into VERIFY_FASL_DIR and report its warnings and errors.
Load the fasl when LOAD is true."
  (let ((before *verify-count*))
    (handler-bind ((warning
                     (lambda (condition)
                       (unless (or (typep condition 'sb-kernel:redefinition-warning)
                                   (verify-own-fasl-warning-p condition))
                         (incf *verify-count*)
                         (format t "~&WARNING in ~a:~%  ~a~%" file condition))
                       (muffle-warning condition))))
      ;; An error (an IN-PACKAGE of a package that does not exist here)
      ;; fails this file only, not the whole run.
      (handler-case
          (multiple-value-bind (fasl warnings-p failure-p)
              (compile-file file :output-file (ensure-directories-exist
                                               (verify-fasl-path file)))
            (declare (ignore warnings-p))
            (cond ((not fasl)
                   (incf *verify-count*)
                   (format t "~&FAILED to compile ~a~%" file))
                  ;; The warnings are muffled above, so this is an error
                  ;; the compiler caught ("caught ERROR" in its output).
                  ((and failure-p (= before *verify-count*))
                   (incf *verify-count*)
                   (format t "~&FAILED to compile ~a: an error in a form, ~
                              see \"caught ERROR\" above~%" file))
                  (load
                   (handler-case (load fasl)
                     (error (condition)
                       (incf *verify-count*)
                       (format t "~&FAILED to load the fasl of ~a:~%  ~a~%"
                               file condition))))))
        (error (condition)
          (incf *verify-count*)
          (format t "~&FAILED to compile ~a:~%  ~a~%" file condition))))))

(dolist (file (verify-env-list "VERIFY_FILES"))
  (let ((loaded (verify-load-file-system file)))
    (if (not loaded)
        (incf *verify-count*)
        ;; A file in no system (a script under scripts/, say) is compiled
        ;; in what is loaded, and not loaded: its top-level forms may build
        ;; and save an image, or load other systems.
        (verify-compile-file file (not (eq loaded :none))))))
(format t "~&~d warning~:p~%" *verify-count*)
(uiop:quit (if (zerop *verify-count*) 0 1))
