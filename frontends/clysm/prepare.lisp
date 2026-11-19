;;;; prepare.lisp — compile Lem and its dependencies with the image of the
;;;; application (run by build.sh on Node.js):
;;;;   node host/run.mjs IMAGE prepare.lisp LEM-DIR MAILBOX-DIR MICROS-DIR OUT-DIR
;;;; Quicklisp downloads the libraries; the fasls go to the cache of ASDF,
;;;; where the application finds them.  OUT-DIR/config.lisp gets the
;;;; directories of all the systems, so that the application loads them with
;;;; ASDF alone (the extensions, when they are first used), and
;;;; OUT-DIR/boot-list.lisp what the application loads when it starts,
;;;; without ASDF (boot.lisp): the fasls in their order, and the systems.

;;; What the system definitions of Lem's dependencies do after loading a
;;; system (:perform (load-op :after ...)), done after its fasls when the
;;; application loads them without ASDF.
(defparameter cl-user::*after-load-forms*
  '(("trivia" "(trivia:in-optimizer :balland2006 nil)")
    ("micros" "(uiop:symbol-call :micros :before-init)")))

(defvar cl-user::*boot-items* nil
  "While Lem is loaded: (:fasl fasl source) and (:eval string), the latest first.")

(require :asdf)
(require :quicklisp)

(destructuring-bind (lem-dir mailbox-dir micros-dir out-dir) (cddr (uiop:raw-command-line-arguments))
  (push (uiop:ensure-directory-pathname lem-dir) asdf:*central-registry*)
  ;; Lem's extensions (each in a directory of its own)
  (dolist (directory (uiop:subdirectories (merge-pathnames "extensions/"
                                                           (uiop:ensure-directory-pathname lem-dir))))
    (push directory asdf:*central-registry*))
  (push (uiop:ensure-directory-pathname mailbox-dir) asdf:*central-registry*)
  (push (uiop:ensure-directory-pathname micros-dir) asdf:*central-registry*)
  (load (merge-pathnames "setup.lisp" *load-truename*))
  ;; what loading lem-clysm loads, in its order
  (defmethod asdf:perform :before ((o asdf:load-op) (c asdf:cl-source-file))
    (push (list :fasl (namestring (first (asdf:input-files o c)))
                (namestring (asdf:component-pathname c)))
          cl-user::*boot-items*))
  (defmethod asdf:perform :after ((o asdf:load-op) (s asdf:system))
    (let ((form (second (assoc (asdf:component-name s) cl-user::*after-load-forms* :test #'string=))))
      (when form (push (list :eval form) cl-user::*boot-items*))))
  (ql:quickload "lem-clysm")
  (let* ((items (reverse cl-user::*boot-items*))
         (systems (loop for s in (asdf:required-components (asdf:find-system "lem-clysm")
                                                           :other-systems t :component-type 'asdf:system
                                                           :goal-operation 'asdf:load-op)
                        for name = (asdf:component-name s)
                        for file = (asdf:system-source-file s)
                        unless (member name '("asdf" "uiop" "asdf-package-system") :test #'string=)
                          collect (list* name :version (asdf:component-version s)
                                         (and file (list :source-file (namestring file)))))))
    (with-open-file (out (merge-pathnames "boot-list.lisp" (uiop:ensure-directory-pathname out-dir))
                         :direction :output :if-exists :supersede)
      (format out ";;; what Lem on Clysm loads when it starts (written by frontends/clysm/prepare.lisp)~%")
      (let ((*print-pretty* t) (*package* (find-package :cl-user)) (*print-length* nil))
        (print `(defparameter cl-user::*lem-boot-systems* ',systems) out)
        (print `(defparameter cl-user::*lem-boot-items* ',items) out))
      (terpri out)))
  ;; the extensions that the editor loads when they are first used
  (ql:quickload (uiop:symbol-call :lem-clysm '#:extension-systems))
  (let ((directories
          (remove-duplicates
           (loop for name in (asdf:already-loaded-systems)
                 for directory = (asdf:system-source-directory name)
                 when directory collect (namestring directory))
           :test #'string=)))
    (with-open-file (out (merge-pathnames "config.lisp" (uiop:ensure-directory-pathname out-dir))
                         :direction :output :if-exists :supersede)
      (format out ";;; where the systems of Lem are (written by frontends/clysm/prepare.lisp)~%")
      (let ((*print-pretty* t) (*package* (find-package :cl-user)))
        (print `(setf asdf:*central-registry*
                      (append (mapcar #'pathname ',directories) asdf:*central-registry*))
               out)
        (print `(load ,(namestring (merge-pathnames "setup.lisp" *load-truename*))) out))
      (terpri out)))
  (format t "~&lem-clysm: compiled~%"))
