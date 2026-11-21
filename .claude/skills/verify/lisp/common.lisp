;;; Loaded by the verify scripts after .qlot/setup.lisp.

;; The Clysm submodule (frontends/clysm/clysm) has its own closer-mop.asd,
;; which ASDF would find before the real closer-mop and fail to load on SBCL.
;; Nothing the SBCL builds need lives under a directory named "clysm".
(push "clysm" asdf::*default-source-registry-exclusions*)
(asdf:clear-source-registry)

;; Compile the repository's files into VERIFY_CACHE (one per LEM_VERIFY_DIR),
;; not the shared ~/.cache/common-lisp, so that other builds and other verify
;; runs do not leave fasls of another version of the code here.  ASDF compares
;; whole seconds: after switching code versions in place, use `tests --fresh`.
;;
;; A file is put under VERIFY_CACHE at its full path, the way ASDF's own cache
;; does it, because Rove maps a fasl back to its source by removing a cache
;; root that it finds by translating /rove--fasl-probe/probe.lisp.  That probe
;; is translated into VERIFY_CACHE too.  Without both, Rove finds no package of
;; lem-tests and `rove:run :lem-tests` runs 0 tests (run-tests.lisp then fails
;; with "no test package of lem-tests found").  The probe's path is internal to
;; Rove (effective-fasl-cache-root in core/suite/file.lisp of the Rove that
;; qlfile.lock pins): check it there when updating Rove.
(let ((repo (uiop:ensure-directory-pathname (uiop:getenv "VERIFY_REPO")))
      (cache (uiop:ensure-directory-pathname (uiop:getenv "VERIFY_CACHE"))))
  (flet ((under-cache (directory)
           ;; CACHE/ followed by the components of the absolute DIRECTORY.
           (merge-pathnames "**/*.*"
                            (make-pathname :directory (append (pathname-directory cache)
                                                              (cdr (pathname-directory directory)))
                                           :defaults cache))))
    (asdf:initialize-output-translations
     `(:output-translations
       (,(merge-pathnames "**/*.*" repo) ,(under-cache repo))
       (,(merge-pathnames "**/*.*" #p"/rove--fasl-probe/") ,(under-cache #p"/rove--fasl-probe/"))
       :inherit-configuration))))

(defun verify-env-list (name)
  "The words of the environment variable NAME, or NIL."
  (let ((value (uiop:getenv name)))
    (and value
         (remove "" (uiop:split-string value :separator '(#\Space #\Newline #\Tab))
                 :test #'string=))))

