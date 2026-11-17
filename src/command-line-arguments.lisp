(in-package :lem-core)

(defvar *help* "Usage: lem [ OPTION-OR-FILENAME ] ...
Options:
        -q, --without-init-file   do not load ~/.lem/init.lisp
        --debug                   enable debugger
        --log-filename FILENAME   file name of the log file
        -i, --interface INTERFACE interface to use: sdl2, sdl3, ncurses or webview
        -v, --version             print the version number and exit
        -h, --help                display this help and exit
        -e, --eval FORM           common lisp form (in quotes) to evaluate on startup"
  "Help output for cli")

(defun show-help ()
  (uiop:println *help*))

(define-condition command-line-arguments-error (simple-error) ())

(defstruct command-line-arguments
  args
  (help nil)
  (debug nil)
  (version nil)
  (without-init-file nil)
  (log-filename nil)
  (interface nil)
  (filenames '())
  (eval-form-str nil))

(defun use-splash-screen-p (args)
  (null (command-line-arguments-filenames args)))

(defun command-line-arguments-error (fmt &rest args)
  (error 'command-line-arguments-error :format-control fmt :format-arguments args))

(defun parse-args (args)
  "returns a `command-line-arguments` that represents the arguments passed in from `args`.
`args` should be a list of strings (from `uiop:command-line-arguments`)."
  (let ((help nil)
        (debug nil)
        (version nil)
        (without-init-file nil)
        (log-filename nil)
        (interface nil)
        (filenames '())
        (eval-form-str nil))
    (loop :while args
          :for arg := (pop args)
          :do (cond ((member arg '("-h" "--help") :test #'equal)
                     (setf help t))
                    ((member arg '("-q" "--without-init-file") :test #'equal)
                     (setf without-init-file t))
                    ((equal arg "--debug")
                     (setf debug t))
                    ((equal arg "--log-filename")
                     (let ((filename (pop args)))
                       (unless filename
                         (command-line-arguments-error "Please, specify a filename to log to."))
                       (setf log-filename filename)))
                    ((member arg '("-i" "--interface") :test #'equal)
                     (let ((arg (pop args)))
                       (if arg
                           (setf interface
                                 (alexandria:make-keyword (string-upcase arg)))
                           (command-line-arguments-error "Please specify an interface to use."))))
                    ((member arg '("-v" "--version") :test #'equal)
                     (setf version t))
                    ((member arg '("-e" "--eval") :test #'equal)
                     ;; assumes that FORM follows --eval and is in quotes.
                     (setf eval-form-str (pop args)))
                    ((or (stringp arg) (pathnamep arg))
                     (push arg filenames))
                    (t
                     (command-line-arguments-error "unexpected arg: ~A" arg))))
    (make-command-line-arguments :help help
                                 :debug debug
                                 :version version
                                 :without-init-file without-init-file
                                 :log-filename log-filename
                                 :interface interface
                                 :filenames (nreverse filenames)
                                 :eval-form-str eval-form-str)))

(defun apply-args (args)
  (declare (command-line-arguments args))
  
  (if (and (use-splash-screen-p args)
           *splash-function*)
      (funcall *splash-function*)
      (loop :for filename :in (command-line-arguments-filenames args)
            :do (uiop:symbol-call :lem :find-file (merge-pathnames filename (uiop:getcwd)))))

  
  (let (form-str form)
    (setf form-str (command-line-arguments-eval-form-str args))
    (when form-str 
      (setf form (read-from-string form-str nil))
    
      (if form
          (eval form)
          (editor-error "WARNING: -e/--eval supplied, but no form was provided!")))))