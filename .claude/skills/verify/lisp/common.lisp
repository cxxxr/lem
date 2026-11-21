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
(asdf:initialize-output-translations
 `(:output-translations
   (,(merge-pathnames "**/*.*" (uiop:getenv "VERIFY_REPO"))
    ,(merge-pathnames "**/*.*" (uiop:getenv "VERIFY_CACHE")))
   :inherit-configuration))

(defun verify-env-list (name)
  "The words of the environment variable NAME, or NIL."
  (let ((value (uiop:getenv name)))
    (and value
         (remove "" (uiop:split-string value :separator '(#\Space #\Newline #\Tab))
                 :test #'string=))))
