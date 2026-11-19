;;;; boot.lisp — what the page of Lem runs: Lem and its frontend are loaded
;;;; from the fasls that build.sh compiled (load-lem.lisp), while the page
;;;; shows how far it is, then Lem starts.
;;;;
;;;; build.sh also runs it on Node.js with CL-USER::*LEM-SAVE-IMAGE* set to
;;;; a file: Lem is then loaded and the heap saved there (runtime/image.lisp),
;;;; and the page starts from that heap image instead (index.html), which
;;;; calls START-LEM.  The page runs this file only when it cannot.

(defvar cl-user::*lem-save-image* nil)
(load (merge-pathnames "load-lem.lisp" *load-truename*))
;; what a world loads (M-x world-start; src/world.lisp, loaded with the Lisp mode)
(defvar cl-user::*world-boot-file* (namestring (merge-pathnames "world-boot.lisp" *load-truename*)))

(defun cl-user::start-lem ()
  ;; files are found from the home directory
  (let ((home (user-homedir-pathname)))
    (uiop:chdir home)
    (setf *default-pathname-defaults* home))
  (uiop:symbol-call :lem-clysm '#:main))

(cond (cl-user::*lem-save-image*
       (cl-user::load-lem)
       ;; what Lem would do when it starts: ASDF configured for the
       ;; extensions, and the color themes (that a saved theme needs)
       (cl-user::configure-lem-asdf)
       (let ((*compile-verbose* nil) (*load-verbose* nil))
         (asdf:load-system (symbol-value (find-symbol "*THEME-SYSTEM*" "LEM-CLYSM"))))
       (sys::save-image cl-user::*lem-save-image*
                        (lambda () (cl-user::start-in-page #'cl-user::start-lem))))
      (t
       (cl-user::start-in-page
        (lambda ()
          (cl-user::load-lem #'cl-user::show-progress-in-page)
          (cl-user::start-lem)))))
