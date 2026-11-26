(uiop:define-package :lem-terminal/terminal-mode
  (:use :cl :lem)
  (:local-nicknames (:ffi :lem-terminal/ffi))
  (:local-nicknames (:terminal :lem-terminal/terminal)))
(in-package :lem-terminal/terminal-mode)

;; FIXME: Think of a better name
(defvar *bypass-commands*
  '(next-window
    previous-window
    window-move-up
    window-move-down
    window-move-left
    window-move-right
    split-active-window-vertically
    split-active-window-horizontally
    delete-other-windows
    delete-active-window
    select-buffer
    kill-buffer
    find-file
    execute-command
    terminal-resize
    terminal-copy-mode-on
    terminal-send-next-key
    lem-core::<mouse-motion-event>
    lem-core::<mouse-event>
    lem/frame-multiplexer:frame-multiplexer-advice))

(defun %remote-terminal-command (path)
  "If PATH is a TRAMP-style remote path, return (values program argv) for
launching a terminal on the remote host.  Returns nil for non-TRAMP paths.
Looks up lem-tramp:tramp-terminal-command at runtime so no compile-time
dependency on the TRAMP extension is needed."
  (let* ((pkg (find-package :lem-tramp))
         (fn (and pkg (find-symbol "TRAMP-TERMINAL-COMMAND" pkg))))
    (when fn
      (funcall (symbol-function fn) path))))

(define-major-mode terminal-mode ()
    (:name "Terminal"
     :keymap *terminal-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t)
  (setf (variable-value 'highlight-line :buffer (current-buffer)) nil)
  (setf (variable-value 'line-wrap :buffer (current-buffer)) nil)
  (setf (variable-value 'lem/show-paren:enable :buffer (current-buffer)) nil)
  (add-hook (variable-value 'kill-buffer-hook :buffer (current-buffer)) 'on-kill-buffer)
  (alexandria:when-let (terminal (get-current-terminal))
    (terminal:copy-mode-off terminal)))

(define-major-mode terminal-copy-mode ()
    (:name "Terminal (Copy)"
     :keymap *terminal-copy-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t)
  (setf (variable-value 'highlight-line :buffer (current-buffer)) nil)
  (setf (variable-value 'line-wrap :buffer (current-buffer)) nil)
  (setf (variable-value 'lem/show-paren:enable :buffer (current-buffer)) nil)
  (add-hook (variable-value 'kill-buffer-hook :buffer (current-buffer)) 'on-kill-buffer)
  (alexandria:when-let (terminal (get-current-terminal))
    (terminal:copy-mode-on terminal)))

(define-key *terminal-mode-keymap* 'self-insert 'terminal-input)
(define-key *terminal-mode-keymap* 'undefined-key 'terminal-input)
(define-key *terminal-mode-keymap* "C-h" 'terminal-input)
;; C-c and C-z are prefix keys in Lem, which would delay SIGINT until the next
;; key and swallow job control; a shell needs them immediately.
(define-key *terminal-mode-keymap* "C-c" 'terminal-input)
(define-key *terminal-mode-keymap* "C-z" 'terminal-input)
(define-key *terminal-mode-keymap* "C-q" 'terminal-send-next-key)
(define-key *terminal-mode-keymap* "C-x [" 'terminal-copy-mode-on)
(define-key *terminal-copy-mode-keymap* "Escape" 'terminal-copy-mode-off)

(defun buffer-terminal (buffer)
  (buffer-value buffer 'terminal))

(defun (setf buffer-terminal) (terminal buffer)
  (setf (buffer-value buffer 'terminal) terminal))

(defun make-terminal-buffer (buffer-directory)
  (declare (type (string) buffer-directory))
  (let* ((buffer (make-buffer (unique-buffer-name "*Terminal*") :enable-undo-p nil))
         (terminal (terminal:create :cols 80 :rows 24 :buffer buffer
                                    :directory buffer-directory)))
    (setf (buffer-terminal buffer) terminal)
    (change-buffer-mode buffer 'terminal-mode)
    buffer))

(defun on-kill-buffer (buffer)
  (let ((terminal (buffer-terminal buffer)))
    (when terminal
      (terminal:destroy terminal))))

(defun create-terminal (buffer-directory)
  (declare (type (string) buffer-directory))
  (let* ((buffer (make-terminal-buffer buffer-directory))
         (window (pop-to-buffer buffer)))
    (resize-terminal (buffer-terminal buffer) window)
    (setf (current-window) window)))

(defun create-terminal-with-command (program argv &key (name "*Terminal*"))
  "Create a terminal buffer running PROGRAM with ARGV (a list of strings).
Unlike `create-terminal', this launches an arbitrary command instead of the
user's shell started in a directory."
  (declare (type (string) program))
  (let* ((buffer (make-buffer (unique-buffer-name name) :enable-undo-p nil))
         (terminal (terminal:create :cols 80 :rows 24 :buffer buffer
                                    :directory ""
                                    :program program :argv argv)))
    (setf (buffer-terminal buffer) terminal)
    (change-buffer-mode buffer 'terminal-mode)
    (let ((window (pop-to-buffer buffer)))
      (resize-terminal (buffer-terminal buffer) window)
      (setf (current-window) window))))

(define-command terminal (always-create-terminal-p) (:universal-nil)
  "Open a terminal buffer.  When the current buffer visits a TRAMP path
\(e.g. /sudo::/etc or /ssh:user@host:/var/log), the terminal is opened
on the remote host with appropriate privileges.

With a universal argument, always create a new terminal buffer."
  (let* ((buf (current-buffer))
         ;; Check both buffer-filename (file buffers) and buffer-directory
         ;; (directory-mode buffers) for a potential TRAMP path.
         (path (or (buffer-filename buf) (buffer-directory buf))))
    (multiple-value-bind (program argv)
        (%remote-terminal-command path)
      (cond
        ;; TRAMP/remote path: always create a dedicated terminal
        ((and program argv)
         (create-terminal-with-command program argv))
        ;; Local: reuse or create
        (always-create-terminal-p
         (create-terminal (buffer-directory buf)))
        (t
         (alexandria:if-let (buffer (terminal:find-terminal-buffer))
           (setf (current-window) (pop-to-buffer buffer))
           (create-terminal (buffer-directory buf))))))))

(defun get-current-terminal ()
  (buffer-terminal (current-buffer)))

(defun get-current-live-terminal ()
  "Return the current buffer's terminal, signalling `editor-error' when its
process has already exited (so there is nothing to send input to)."
  (let ((terminal (get-current-terminal)))
    (when (and terminal (terminal:process-exited-p terminal))
      (editor-error "Terminal process has exited"))
    terminal))

(defun send-key (terminal key)
  "Send the Lem KEY to TERMINAL, translating it to the matching vterm key or
character with its modifiers."
  (let ((mod (logior (if (key-ctrl key) ffi::vterm_mod_ctrl 0)
                     (if (key-meta key) ffi::vterm_mod_alt 0)
                     (if (key-shift key) ffi::vterm_mod_shift 0))))
    (alexandria:switch ((key-sym key) :test #'equal)
      ("Return" (terminal:input-key terminal ffi::vterm_key_enter :mod mod))
      ("Backspace" (terminal:input-key terminal ffi::vterm_key_backspace :mod mod))
      ("Tab" (terminal:input-key terminal ffi::vterm_key_tab :mod mod))
      ("Escape" (terminal:input-key terminal ffi::vterm_key_escape :mod mod))
      ("Up" (terminal:input-key terminal ffi::vterm_key_up :mod mod))
      ("Down" (terminal:input-key terminal ffi::vterm_key_down :mod mod))
      ("Left" (terminal:input-key terminal ffi::vterm_key_left :mod mod))
      ("Right" (terminal:input-key terminal ffi::vterm_key_right :mod mod))
      ("Insert" (terminal:input-key terminal ffi::vterm_key_ins :mod mod))
      ("Delete" (terminal:input-key terminal ffi::vterm_key_del :mod mod))
      ("Home" (terminal:input-key terminal ffi::vterm_key_home :mod mod))
      ("End" (terminal:input-key terminal ffi::vterm_key_end :mod mod))
      ("PageUp" (terminal:input-key terminal ffi::vterm_key_pageup :mod mod))
      ("PageDown" (terminal:input-key terminal ffi::vterm_key_pagedown :mod mod))
      ("F1" (terminal:input-key terminal (+ 1 ffi::vterm_key_function_0) :mod mod))
      ("F2" (terminal:input-key terminal (+ 2 ffi::vterm_key_function_0) :mod mod))
      ("F3" (terminal:input-key terminal (+ 3 ffi::vterm_key_function_0) :mod mod))
      ("F4" (terminal:input-key terminal (+ 4 ffi::vterm_key_function_0) :mod mod))
      ("F5" (terminal:input-key terminal (+ 5 ffi::vterm_key_function_0) :mod mod))
      ("F6" (terminal:input-key terminal (+ 6 ffi::vterm_key_function_0) :mod mod))
      ("F7" (terminal:input-key terminal (+ 7 ffi::vterm_key_function_0) :mod mod))
      ("F8" (terminal:input-key terminal (+ 8 ffi::vterm_key_function_0) :mod mod))
      ("F9" (terminal:input-key terminal (+ 9 ffi::vterm_key_function_0) :mod mod))
      ("F10" (terminal:input-key terminal (+ 10 ffi::vterm_key_function_0) :mod mod))
      ("F11" (terminal:input-key terminal (+ 11 ffi::vterm_key_function_0) :mod mod))
      ("F12" (terminal:input-key terminal (+ 12 ffi::vterm_key_function_0) :mod mod))
      ("Space" (terminal:input-character terminal #\Space :mod mod))
      (otherwise
       (when (= 1 (length (key-sym key)))
         (terminal:input-character terminal (char (key-sym key) 0) :mod mod))))))

(define-command terminal-input () ()
  "Send the keys that invoked this command to the terminal."
  (let ((terminal (get-current-live-terminal)))
    (dolist (key (last-read-key-sequence))
      (send-key terminal key))))

(define-command terminal-send-next-key () ()
  "Read the next key and send it to the terminal as is, even when it is bound
in Lem (e.g. C-x for nano, or C-q itself)."
  (let ((terminal (get-current-live-terminal))
        (key (read-key)))
    (send-key terminal key)))

(defmacro define-terminal-key-command (name keyspec vterm-key)
  (alexandria:with-unique-names (terminal)
    `(progn
       (define-key *terminal-mode-keymap* ,keyspec ',name)
       (define-command ,name () ()
         (let ((,terminal (get-current-live-terminal)))
           (terminal:input-key ,terminal ,vterm-key))))))

(define-terminal-key-command terminal-key-return "Return" ffi::vterm_key_enter)
(define-terminal-key-command terminal-key-backspace "Backspace" ffi::vterm_key_backspace)
(define-terminal-key-command terminal-key-tab "Tab" ffi::vterm_key_tab)
(define-terminal-key-command terminal-key-escape "Escape" ffi::vterm_key_escape)
(define-terminal-key-command terminal-key-up "Up" ffi::vterm_key_up)
(define-terminal-key-command terminal-key-down "Down" ffi::vterm_key_down)
(define-terminal-key-command terminal-key-left "Left" ffi::vterm_key_left)
(define-terminal-key-command terminal-key-right "Right" ffi::vterm_key_right)
(define-terminal-key-command terminal-key-insert "Insert" ffi::vterm_key_ins)
(define-terminal-key-command terminal-key-delete "Delete" ffi::vterm_key_del)
(define-terminal-key-command terminal-key-home "Home" ffi::vterm_key_home)
(define-terminal-key-command terminal-key-end "End" ffi::vterm_key_end)
(define-terminal-key-command terminal-key-pageup "PageUp" ffi::vterm_key_pageup)
(define-terminal-key-command terminal-key-pagedown "PageDown" ffi::vterm_key_pagedown)
(define-terminal-key-command terminal-key-f1 "F1" (+ 1 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f2 "F2" (+ 2 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f3 "F3" (+ 3 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f4 "F4" (+ 4 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f5 "F5" (+ 5 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f6 "F6" (+ 6 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f7 "F7" (+ 7 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f8 "F8" (+ 8 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f9 "F9" (+ 9 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f10 "F10" (+ 10 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f11 "F11" (+ 11 ffi::vterm_key_function_0))
(define-terminal-key-command terminal-key-f12 "F12" (+ 12 ffi::vterm_key_function_0))

(defun adjust-current-point ()
  (alexandria:if-let ((terminal (get-current-terminal)))
    (terminal:adjust-point terminal)))

(define-command terminal-copy-mode-on () ()
  (terminal-copy-mode))

(define-command terminal-copy-mode-off () ()
  (terminal-mode)
  (adjust-current-point))

(defmethod execute ((mode terminal-mode) command argment)
  (cond
    ((member command *bypass-commands* :test #'typep)
     (call-next-method))
    ((typep command 'scroll-up)
     ;; Populate buffer with scrollback lines so Lem can scroll up
     (alexandria:when-let ((terminal (get-current-terminal)))
       (terminal:activate-scrollback terminal))
     (call-next-method))
    ((typep command 'scroll-down)
     (call-next-method))
    ;; Once the process has exited the buffer is plain read-only text holding
    ;; the final screen, so let every other command behave as in any other buffer.
    ((alexandria:when-let ((terminal (get-current-terminal)))
       (terminal:process-exited-p terminal))
     (call-next-method))
    (t
     ;; Any other command (typing, etc.) — remove scrollback from buffer
     (alexandria:when-let ((terminal (get-current-terminal)))
       (terminal:deactivate-scrollback terminal))
     (terminal-input))))

(defun resize-terminal (terminal window)
  (terminal:resize terminal
                   :rows (1- (window-height window))
                   :cols (window-width window)))

(define-command terminal-resize () ()
  (alexandria:when-let ((terminal (get-current-terminal))
                        (window (current-window)))
    (resize-terminal terminal window)))

(defmethod lem-core:paste-using-mode ((mode terminal-mode) string)
  (let ((terminal (get-current-live-terminal)))
    (loop :for c :across string
          :do (terminal:input-character terminal c))))

(defun on-window-size-change (window)
  (alexandria:when-let (terminal (buffer-terminal (window-buffer window)))
    (resize-terminal terminal window)))

(add-hook *window-size-change-functions*
          'on-window-size-change)

(add-hook *post-command-hook*
          'terminal-resize)
