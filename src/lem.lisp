(in-package :lem-core)

(defvar *set-location-hook* '((push-buffer-point . 0)))
(defvar *before-init-hook* '())
(defvar *after-init-hook* '())
(defvar *splash-function* nil)

(defvar *in-the-editor* nil)

(defparameter *init-file-name* "init.lisp"
  "The name of the init file found within (lem-home)")

(defun setup-first-frame ()
  (let ((frame (make-frame nil)))
    (map-frame (implementation) frame)
    (setup-frame frame (primordial-buffer))))

(defclass lem-timer-manager (timer-manager) ()
  (:documentation "The editor's timer manager: a timer's function runs in the
editor thread (send-event), then the display is redrawn."))

(defmethod send-timer-notification ((timer-manager lem-timer-manager) continue)
  (send-event (lambda ()
                (funcall continue)
                (redraw-display))))

(let ((once nil))
  (defun setup ()
    (setup-first-frame)
    (unless once
      (setf once t)
      (init-syntax-scanner)
      (add-hook *find-file-hook* 'process-file 5000)
      (add-hook (variable-value 'before-save-hook :global) 'process-file)
      (add-hook *input-hook*
                (lambda (event)
                  (push event *this-command-keys*))))))

(defun teardown ()
  (teardown-frames))

(defun end-editor (report finalize)
  "Tear the editor down, then call FINALIZE with REPORT, which lets the
frontend end (invoke returns, and launch with it). By then the editor is
gone: a launch called next starts an editor rather than giving its
arguments to this one (*in-the-editor*). A failure of the teardown, any
serious condition, is logged with its backtrace and not signalled: the
frontend must end anyway, and a debugger in the editor thread would wait on
a display that is going away. FINALIZE is not called when the teardown is
left by a non-local exit (the process quitting)."
  (handler-case
      (handler-bind ((serious-condition
                       (lambda (condition)
                         ;; (No backtrace after a storage condition: little
                         ;; stack is left to walk it with.)
                         (let ((backtrace
                                 (unless (typep condition 'storage-condition)
                                   (ignore-errors
                                    (lem/common/backtrace:safe-backtrace)))))
                           (handler-case
                               (log:error "Tearing down the editor failed: ~A~@[~%~A~]"
                                          condition backtrace)
                             (serious-condition () nil))))))
        (teardown))
    (serious-condition () nil))
  (setf *in-the-editor* nil)
  (when finalize (funcall finalize report)))

(defun get-preferred-init-file-path ()
  "Returns the full path to the preferred init file"
  (merge-pathnames *init-file-name* (lem-home)))

(defun load-init-file ()
  (flet ((maybe-load (path)
           (when (probe-file path)
             (load path)
             (message "Load file: ~a" path)
             t)))
    (let ((home (user-homedir-pathname))
          (current-dir (probe-file "."))
          (*package* (find-package :lem-user)))
      (or (maybe-load (get-preferred-init-file-path))
          (maybe-load (merge-pathnames ".lemrc" home)))
      (unless (uiop:pathname-equal current-dir (user-homedir-pathname))
        (maybe-load (merge-pathnames ".lemrc" current-dir))))))

(defun init-at-build-time ()
  "This function is called when an lem executable file is built.
If a file named $HOME/.lem/build-init.lisp exists, it is loaded.
The difference is that init.lisp loading is called when the editor is started,
while build-init.lisp is called when the binary file is created.
See scripts/build-ncurses.lisp or scripts/build-sdl2.lisp"
  (let ((file (merge-pathnames "build-init.lisp" (lem-home))))
    (when (uiop:file-exists-p file)
      (load file))))

(defun init (args)
  (run-hooks *before-init-hook*)
  (unless (command-line-arguments-without-init-file args)
    (load-init-file))
  (run-hooks *after-init-hook*)
  (apply-args args))

(defun run-editor-thread (initialize args finalize)
  ;; The editor's timer manager is the global one, as invoke-frontend sets
  ;; the implementation: code run in a thread that the editor does not make
  ;; (the REPL of a self connection) can start timers, and a thread that the
  ;; editor makes and that outlives it uses the manager of the next editor.
  ;; So the editor thread starts with no binding of its own.
  (without-timer-manager-binding ()
    (bt2:make-thread
     (lambda ()
       (when initialize (funcall initialize))
       ;; When the command loop returns, end-editor ends the editor and the
       ;; frontend; nothing of this thread changes *in-the-editor* after it
       ;; has finalized, as the next editor may have started.  When the loop
       ;; or the teardown is left by a non-local exit (the process quitting),
       ;; only the flag is cleared.
       (let ((ended nil))
         (unwind-protect
              (let (#+lispworks (lw:*default-character-element-type* 'character))
                (with-editor-stream ()
                  (set-global-timer-manager (make-instance 'lem-timer-manager))
                  (setf *in-the-editor* t)
                  (setup)
                  (end-editor (toplevel-command-loop (lambda () (init args)))
                              ;; ENDED once end-editor has cleared the flag:
                              ;; a teardown left by a non-local exit clears
                              ;; it below.
                              (lambda (report)
                                (setf ended t)
                                (when finalize (funcall finalize report))))))
           (unless ended
             (setf *in-the-editor* nil)))))
     :name "editor")))

(defun find-editor-thread ()
  (find "editor" (bt2:all-threads)
        :test #'equal
        :key #'bt2:thread-name))

;; NOTE: so that it can be processed during compilation.
(defvar *version* (get-version-string))

(defun launch (args)
  (check-type args command-line-arguments)
  ;; for sbcl, set the default file encoding to utf-8
  ;; (on windows, the default is determined by code page (e.g. :cp932))
  #+sbcl
  (setf sb-impl::*default-external-format* :utf-8)
  (cond
    ((command-line-arguments-log-filename args)
     (apply #'log:config
            :sane
            :daily (command-line-arguments-log-filename args)
            (list
             (if (command-line-arguments-debug args)
                 :debug
                 :info))))
    (t
     (log:config :sane :daily (merge-pathnames "debug.log" (lem-logdir-pathname)) :info)))

  (when (command-line-arguments-help args)
    (show-help)
    (return-from launch))

  (when (command-line-arguments-version args)
    (uiop:println *version*)
    (return-from launch))

  (log:info "Starting Lem")

  (cond (*in-the-editor*
         (apply-args args))
        (t
         (let ((implementation (get-default-implementation
                                :implementation (command-line-arguments-interface args))))
           (unless implementation
             (error "implementation ~A not found"
                    (command-line-arguments-interface args)))
           
           (invoke-frontend
            (lambda (&optional initialize finalize)
              (run-editor-thread initialize args finalize))
            :implementation implementation)))))

(defun lem (&rest args)
  (launch (parse-args args)))

(defun main (&optional (args (uiop:command-line-arguments)))
  (apply #'lem args))

#+sbcl
(push #'(lambda (x)
          (if x
              (lem x)
              (lem))
          t)
      sb-ext:*ed-functions*)


(add-hook *after-init-hook*
          (lambda ()
            (when *editor-warnings*
              (let ((buffer (make-buffer "*EDITOR WARNINGS*")))
                (dolist (warning *editor-warnings*)
                  (insert-string (buffer-point buffer) warning)
                  (insert-character (buffer-point buffer) #\newline))
                (pop-to-buffer buffer)))))

(add-hook *after-init-hook* (lambda () (send-event #'attempt-automigrate-config-file)))
