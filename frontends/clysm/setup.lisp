;;;; setup.lisp — make Lem's systems loadable by ASDF on Clysm (after
;;;; (require :asdf)): the stubs of the libraries that Lem on Clysm does
;;;; without, Clysm's worlds and usocket for a page, and the system
;;;; lem-clysm.  The directories of Lem itself and of the libraries come
;;;; from the caller (prepare.lisp, or the registry that it writes).
;;;;
;;;; Lem's system definitions load on Clysm as they are: what Lem on Clysm
;;;; does without — the extension manager, the DeepL commands, the language
;;;; servers, the previews of Markdown, the dashboard — is in systems of
;;;; their own that lem-clysm does not load.  The stubs stand for libraries
;;;; that need a native library or the network (tree-sitter, subprocesses,
;;;; an HTTP client), for log4cl, and for deploy, which lem.asd loads.
;;;;
;;;; A stub's definition is stubs/NAME/NAME.system, not NAME.asd: Quicklisp
;;;; finds every .asd under a local project (and ASDF under a :tree), so
;;;; a Lem there would take the stubs for the real libraries.  Here a
;;;; search function of ASDF finds them first.

(in-package :asdf)

(defparameter cl-user::*lem-clysm-stubs*
  '("log4cl" "dexador" "deploy" "lem-tree-sitter" "lem-process")
  "The libraries that Lem on Clysm has stubs of (stubs/NAME/NAME.system).")

(let ((stubs (merge-pathnames "stubs/" (uiop:pathname-directory-pathname *load-truename*))))
  (defun cl-user::lem-clysm-stub-search (name)
    "The definition of the stub of the system NAME, if there is one."
    (when (member name cl-user::*lem-clysm-stubs* :test #'string=)
      (probe-file (merge-pathnames (format nil "~A/~A.system" name name) stubs)))))

(pushnew 'cl-user::lem-clysm-stub-search *system-definition-search-functions*)
;; One fasl a file, not a fasl of the whole system (Clysm's system fasls,
;; on by default since 2026-11): prepare.lisp records each file that ASDF
;; loads for the boot list, and the page has only the fasls that its pack
;; recorded, so a system fasl that the page's ASDF would load is not there.
(setf *clysm-system-fasls* nil)

(let ((here (uiop:pathname-directory-pathname *load-truename*))
      (world (merge-pathnames "contrib/world/" (sys::%clysm-root))))
  ;; worlds and the sockets of a page (Clysm's contrib/world/)
  (pushnew (merge-pathnames "usocket/" world) *central-registry* :test #'equal)
  (pushnew world *central-registry* :test #'equal)
  (pushnew here *central-registry* :test #'equal))
