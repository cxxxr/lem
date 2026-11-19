;;;; load-lem.lisp — load Lem on Clysm without ASDF (desktop/boot.lisp,
;;;; web/boot.lisp): the fasls of boot-list.lisp in their order, which
;;;; prepare.lisp wrote when it compiled them.  ASDF would read the system
;;;; definitions of 67 systems, make a plan of 445 files and look at their
;;;; times, which took a second of the start; the systems are registered
;;;; instead as loaded and immutable, with their versions and definition
;;;; files (Lem and its libraries ask ASDF for them), so that the extensions
;;;; that Lem loads later with ASDF (src/extensions.lisp) find them loaded.
;;;; Before that, config.lisp gives ASDF the directories of the systems and
;;;; the fixups (setup.lisp).

(require :asdf)

(defvar cl-user::*lem-directory* (make-pathname :name nil :type nil :defaults *load-truename*))
(defvar cl-user::*lem-config* (merge-pathnames "config.lisp" cl-user::*lem-directory*)
  "The configuration of ASDF for loading Lem's systems, loaded the first time
an extension is loaded (then NIL).")

(let ((*package* (find-package "COMMON-LISP-USER")))
  (load (merge-pathnames "boot-list.lisp" cl-user::*lem-directory*)))

(defun cl-user::lem-boot-count ()
  "The number of fasls that LOAD-LEM loads."
  (count :fasl cl-user::*lem-boot-items* :key #'first))

(defun cl-user::load-lem (&optional progress)
  "Load Lem; PROGRESS, if given, is called with the number of fasls loaded
after each."
  (dolist (s cl-user::*lem-boot-systems*)
    (apply #'asdf:register-immutable-system s))
  (let ((*compile-verbose* nil) (*load-verbose* nil) (n 0))
    (dolist (item cl-user::*lem-boot-items*)
      (ecase (first item)
        (:fasl (load (second item))
         (incf n)
         (when progress (funcall progress n)))
        (:eval (eval (read-from-string (second item))))))))

(defun cl-user::configure-lem-asdf ()
  "Give ASDF the systems of Lem's extensions (once)."
  (when cl-user::*lem-config*
    (load (shiftf cl-user::*lem-config* nil))))

;;; The page (desktop/index.html, web/index.html) shows how far loading is:
;;; LOAD-LEM is then a computation of JS:ASYNC-FUNCTION, which lets the page
;;; run every 100 ms (a setTimeout: waiting for a frame would cost more) and
;;; calls lemBoot.progress(n, total).

(sys::%defvar-host cl-user::*page-progress*
  (ignore-errors
   (js:eval "(n, total) => { const boot = globalThis.lemBoot;
                             if (boot) boot.progress(n, total);
                             return new Promise((resolve) => setTimeout(resolve, 0)); }")))

(defvar cl-user::*progress-shown* 0)

(defun cl-user::show-progress-in-page (n)
  (when (and cl-user::*page-progress* (js:can-await-p)
             (> (- (get-internal-real-time) cl-user::*progress-shown*)
                (floor internal-time-units-per-second 10)))
    (js:await (js:funcall cl-user::*page-progress* n (cl-user::lem-boot-count)))
    (setf cl-user::*progress-shown* (get-internal-real-time))))

(defun cl-user::start-in-page (function)
  "Call FUNCTION as a computation started from the event loop (that can
wait); an error is shown by the page (lemBoot.failed)."
  (js:funcall (js:global "setTimeout")
              (js:async-function
               (lambda ()
                 (handler-bind ((error (lambda (e)
                                         (let ((boot (js:global "lemBoot")))
                                           (when (js:js-value-p boot)
                                             (js:call boot "failed" (princ-to-string e)))))))
                   (funcall function))
                 nil))
              0))
