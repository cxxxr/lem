(in-package :lem-core)

(add-hook *after-init-hook*
          (lambda ()
            ;; PATH injection for macOS
            (setf (uiop:getenv "PATH")
                  (format nil "~{~A~^:~}" (exec-path)))
            ;; Prevent the startup directory from becoming root
            (when (deploy:deployed-p)
              (let ((dir (user-homedir-pathname)))
                (setq *default-pathname-defaults* dir)
                (uiop:chdir dir)))))

;; Tell the deploy library not to bundle tree-sitter native libraries.
;; They are optional and loaded at runtime only when available.
;; Only relevant while building; skip when running inside the deployed
;; binary so we don't mutate deploy state on every startup.
(unless (deploy:deployed-p)
  (when (find-package :tree-sitter/ffi)
    (let ((ts (find-symbol "TREE-SITTER" :tree-sitter/ffi))
          (tw (find-symbol "TS-WRAPPER" :tree-sitter/ffi)))
      (when ts
        (setf (deploy:library-dont-deploy-p (deploy:ensure-library ts)) t))
      (when tw
        (setf (deploy:library-dont-deploy-p (deploy:ensure-library tw)) t)))))

;; Tell the deploy library not to bundle OS-provided libraries. They exist
;; only in the dyld shared cache, not as files, so deploy cannot copy them
;; ("does not have a known shared library file path", or FILE-DOES-NOT-EXIST
;; for the CoreFoundation framework binary). Every macOS install has them, so
;; the deployed binary loads them from the system at boot.
(unless (deploy:deployed-p)
  (loop :for (package-name symbol-name) :in '((:lem-darwin/appearance "CORE-FOUNDATION")
                                              (:lem-darwin/fonts "CORE-TEXT")
                                              (:lem-darwin/layer "QUARTZ-CORE")
                                              (:lem-darwin/layer "CORE-GRAPHICS")
                                              (:objc-cffi "LIBOBJC")
                                              (:cffi "LIBFFI"))
        :for package := (find-package package-name)
        :for symbol := (and package (find-symbol symbol-name package))
        :when (and symbol (find symbol (cffi:list-foreign-libraries)
                                :key #'cffi:foreign-library-name))
        :do (setf (deploy:library-dont-deploy-p (deploy:ensure-library symbol)) t)))

;; Make the deployed app's library reloading at boot one linkage-table pass
;; instead of one per library.
;;
;; Deploy reopens each bundled library with LOAD-SHARED-OBJECT, and after
;; each one SBCL looks up again, with dlsym, every foreign symbol not yet
;; resolved: about 1,900 with the SDL3 frontend, several hundred of which
;; resolve only once their library is open.  With 17 libraries that took
;; about 8 seconds before the window appeared.  Opening a library does not
;; use the linkage table, so while deploy's reload hook runs (priority
;; most-positive-fixnum - 10) the update is skipped, and right after it one
;; full update resolves everything.  This uses SBCL's own exported
;; SB-SYS:UPDATE-ALIEN-LINKAGE-TABLE and SB-INT:ENCAPSULATE (as TRACE does).
#+sbcl
(progn
  (deploy:define-hook (:boot defer-linkage-table-updates (- most-positive-fixnum 5)) ()
    (sb-ext:without-package-locks
      (sb-int:encapsulate 'sb-sys:update-alien-linkage-table 'lem-deferred
                          (lambda (function &rest arguments)
                            (declare (ignore function arguments))
                            nil))))

  (deploy:define-hook (:boot update-linkage-table-once (- most-positive-fixnum 15)) ()
    (sb-ext:without-package-locks
      (sb-int:unencapsulate 'sb-sys:update-alien-linkage-table 'lem-deferred))
    (sb-sys:update-alien-linkage-table t)))
