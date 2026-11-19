;;;; boot.lisp — what the page of Lem runs in a browser: Lem and its
;;;; frontend are loaded from the fasls in the pack of files
;;;; (host/web/vfs.mjs) without ASDF (load-lem.lisp), then Lem starts with
;;;; welcome.lisp, copied into the home directory the first time.
;;;;
;;;; The loading is a computation that lets the page run between files, so
;;;; that the page shows how far it is (lemBoot.progress of index.html).
;;;;
;;;; build-web.sh runs it on Node.js with CL-USER::*LEM-LOAD-ONLY* true to
;;;; see which files loading reads: then it loads the extensions as well
;;;; (src/extensions.lisp loads them in the page when they are first used),
;;;; and writes the number of files that the page loads into load-count,
;;;; and their sources into sources.txt.  With CL-USER::*LEM-SAVE-IMAGE*
;;;; set to a file, it loads Lem and saves the heap there
;;;; (runtime/image.lisp), from which the page starts (index.html), calling
;;;; START-LEM; the page runs this file only when it cannot.

(defvar cl-user::*lem-load-only* nil)
(defvar cl-user::*lem-save-image* nil)
(defvar cl-user::*welcome* (with-open-file (in (merge-pathnames "welcome.lisp" *load-truename*))
                             (let ((text (make-string (file-length in))))
                               (subseq text 0 (read-sequence text in)))))
(defvar cl-user::*boot-directory* (make-pathname :name nil :type nil :defaults *load-truename*))
(load (merge-pathnames "load-lem.lisp" *load-truename*))

(defun cl-user::file-text (path)
  (and (probe-file path) (uiop:read-file-string path)))

(defun cl-user::welcome-unchanged-p (welcome shipped)
  "True if the file WELCOME is as a page wrote it: its text is that of the
file SHIPPED (written with it), or, from before SHIPPED was written, that
of the first version of welcome.lisp."
  (let ((text (cl-user::file-text welcome)))
    (or (null text)
        (equal text (cl-user::file-text shipped))
        (and (not (probe-file shipped))
             (uiop:string-prefix-p ";;;; Welcome to Lem, the Common Lisp editor, running in your browser." text)
             (uiop:string-suffix-p text (format nil ";;; M-x lisp-repl opens a REPL of the same image.~%"))))))

(defun cl-user::start-lem ()
  (let* ((home (user-homedir-pathname))
         (welcome (merge-pathnames "welcome.lisp" home))
         (shipped (merge-pathnames ".welcome.lisp" home)))
    (uiop:chdir home)
    (setf *default-pathname-defaults* home)
    ;; a file to start with, in the home directory (where it is kept); a
    ;; newer one replaces it if it was not changed
    (when (and (cl-user::welcome-unchanged-p welcome shipped)
               (not (equal (cl-user::file-text welcome) cl-user::*welcome*)))
      (dolist (path (list welcome shipped))
        (with-open-file (out path :direction :output :if-exists :supersede)
          (write-string cl-user::*welcome* out))))
    (uiop:symbol-call :lem-clysm '#:main (namestring welcome))))

(cond (cl-user::*lem-save-image*
       (cl-user::load-lem)
       ;; (ASDF configured for the extensions, as when one is first loaded)
       (cl-user::configure-lem-asdf)
       (sys::save-image cl-user::*lem-save-image*
                        (lambda () (cl-user::start-in-page #'cl-user::start-lem))))
      (cl-user::*lem-load-only*
       (cl-user::load-lem)
       (with-open-file (out (merge-pathnames "load-count" cl-user::*boot-directory*)
                            :direction :output :if-exists :supersede)
         (format out "~D~%" (cl-user::lem-boot-count)))
       ;; their sources, which the page has for M-. (pack.mjs)
       (with-open-file (out (merge-pathnames "sources.txt" cl-user::*boot-directory*)
                            :direction :output :if-exists :supersede)
         (dolist (item cl-user::*lem-boot-items*)
           (when (eq (first item) :fasl) (write-line (third item) out))))
       (cl-user::configure-lem-asdf)
       (let ((*compile-verbose* nil) (*load-verbose* nil))
         (dolist (system (uiop:symbol-call :lem-clysm '#:extension-systems))
           (asdf:load-system system))))
      (t
       ;; from the event loop, as a computation that can wait
       (cl-user::start-in-page
        (lambda ()
          (cl-user::load-lem #'cl-user::show-progress-in-page)
          (cl-user::start-lem)))))
