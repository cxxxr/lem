(uiop:define-package :lem-terminal/terminal-mode
  (:use :cl :lem)
  (:local-nicknames (:ffi :lem-terminal/ffi))
  (:local-nicknames (:terminal :lem-terminal/terminal)))
(in-package :lem-terminal/terminal-mode)

;; Bound to T while a bypass command runs.  Commands it runs in turn — the
;; command chosen in M-x, for instance — are what the user asked for, not
;; keystrokes for the shell, so they run as in any other buffer.
(defvar *running-bypass-command* nil)

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
    terminal-yank
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
(define-key *terminal-mode-keymap* "C-y" 'terminal-yank)
(define-key *terminal-mode-keymap* "C-x [" 'terminal-copy-mode-on)
(define-key *terminal-copy-mode-keymap* "Escape" 'terminal-copy-mode-off)

(defun buffer-terminal (buffer)
  (buffer-value buffer 'terminal))

(defun (setf buffer-terminal) (terminal buffer)
  (setf (buffer-value buffer 'terminal) terminal))

(defun call-with-new-terminal-buffer (name create-args setup)
  "Make a buffer named NAME, start a terminal in it with CREATE-ARGS for
`terminal:create', and call SETUP with the buffer; its value is returned.
Until SETUP returns, a failure (an editor-error when the terminal cannot
start, or an error setting the buffer up) destroys the terminal and deletes
the buffer: no empty buffer, and no shell nobody can kill, is left behind."
  (let ((buffer (make-buffer (unique-buffer-name name) :enable-undo-p nil))
        (terminal nil)
        (done nil))
    (unwind-protect
         (progn
           (setf terminal (apply #'terminal:create :cols 80 :rows 24 :buffer buffer
                                 create-args)
                 (buffer-terminal buffer) terminal)
           (prog1 (funcall setup buffer)
             (setf done t)))
      (unless done
        ;; The buffer goes even when destroying the terminal fails.
        (unwind-protect
             (when terminal
               (terminal:destroy terminal))
          ;; Lem on Clysm's create deletes a fresh buffer itself.
          (unless (deleted-buffer-p buffer)
            (delete-buffer buffer)))))))

(defun show-terminal-buffer (buffer)
  "Show BUFFER's terminal in a window, fitted to it, and select that window."
  (let ((window (pop-to-buffer buffer)))
    (resize-terminal (buffer-terminal buffer) window)
    (setf (current-window) window)))

(defun on-kill-buffer (buffer)
  (let ((terminal (buffer-terminal buffer)))
    (when terminal
      (terminal:destroy terminal))))

(defun create-terminal (buffer-directory)
  (declare (type (string) buffer-directory))
  (call-with-new-terminal-buffer
   "*Terminal*" (list :directory buffer-directory)
   (lambda (buffer)
     ;; A local shell: its working directory is the buffer's directory.
     ;; Terminals running other commands (e.g. TRAMP's ssh) are not tracked.
     (setf (buffer-value buffer 'track-directory) t)
     (change-buffer-mode buffer 'terminal-mode)
     (show-terminal-buffer buffer))))

(defun create-terminal-with-command (program argv &key (name "*Terminal*"))
  "Create a terminal buffer running PROGRAM with ARGV (a list of strings).
Unlike `create-terminal', this launches an arbitrary command instead of the
user's shell started in a directory."
  (declare (type (string) program))
  (call-with-new-terminal-buffer
   name (list :directory "" :program program :argv argv)
   (lambda (buffer)
     ;; Remembered so `terminal-restart' runs the same command again.
     (setf (buffer-value buffer 'terminal-command) (list program argv))
     (change-buffer-mode buffer 'terminal-mode)
     (show-terminal-buffer buffer))))

(define-command terminal (always-create-terminal-p) (:universal-nil)
  "Open a terminal buffer.  When the current buffer visits a TRAMP path
\(e.g. /sudo::/etc or /ssh:user@host:/var/log), the terminal is opened
on the remote host with appropriate privileges.

With a universal argument, always create a new terminal buffer.  Run from a
terminal buffer it also creates a new one, since C-u goes to the shell there
and returning to the same terminal would do nothing."
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
        ((or always-create-terminal-p
             (buffer-terminal buf))
         (create-terminal (buffer-directory buf)))
        (t
         (alexandria:if-let (buffer (terminal:find-terminal-buffer))
           (setf (current-window) (pop-to-buffer buffer))
           (create-terminal (buffer-directory buf))))))))

(define-command terminal-restart () ()
  "Start the terminal's command again in the current terminal buffer, after
its process has exited.  The old output is discarded once the new process has
started; when it cannot start, the buffer is left as it was.  The old terminal
is freed only then, so the restart needs room for a second PTY."
  (let* ((buffer (current-buffer))
         (old (buffer-terminal buffer)))
    (unless old
      (editor-error "Not a terminal buffer"))
    (unless (terminal:process-exited-p old)
      (editor-error "Terminal process is still running"))
    (let ((new (destructuring-bind (&optional program argv)
                   (buffer-value buffer 'terminal-command)
                 (terminal:create :cols 80 :rows 24 :buffer buffer
                                  :directory (if program "" (buffer-directory buffer))
                                  :program program :argv argv))))
      ;; The buffer owns the new terminal before the old one goes.  The old
      ;; process has exited, so its I/O thread no longer renders.
      (setf (buffer-terminal buffer) new)
      (terminal:destroy old)
      ;; The new terminal renders all its rows into the emptied buffer.
      (with-buffer-read-only buffer nil
        (erase-buffer buffer))
      ;; From copy mode (C-x [), back to the mode that sends keys to it.
      (unless (eq (buffer-major-mode buffer) 'terminal-mode)
        (change-buffer-mode buffer 'terminal-mode))
      (resize-terminal new (current-window)))))

(defun get-current-terminal ()
  (buffer-terminal (current-buffer)))

(defun get-current-live-terminal ()
  "Return the current buffer's terminal, signalling `editor-error' when it has
none (a buffer put in terminal-mode by hand) or its process has already
exited, so there is nothing to send input to."
  (let ((terminal (get-current-terminal)))
    (unless terminal
      (editor-error "Not a terminal buffer"))
    (when (terminal:process-exited-p terminal)
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
    (back-to-screen (current-buffer))
    (dolist (key (last-read-key-sequence))
      (send-key terminal key))))

(define-command terminal-send-next-key () ()
  "Read the next key and send it to the terminal as is, even when it is bound
in Lem (e.g. C-x for nano, or C-q itself)."
  (let ((terminal (get-current-live-terminal))
        (key (read-key)))
    (back-to-screen (current-buffer))
    (send-key terminal key)))

(defmacro define-terminal-key-command (name keyspec vterm-key)
  (alexandria:with-unique-names (terminal)
    `(progn
       (define-key *terminal-mode-keymap* ,keyspec ',name)
       (define-command ,name () ()
         (let ((,terminal (get-current-live-terminal)))
           (back-to-screen (current-buffer))
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

(defun sync-buffer-directory (buffer)
  "Set BUFFER's directory to its shell's current directory, so commands such as
find-file start where the shell is."
  (alexandria:when-let* ((terminal (and (buffer-value buffer 'track-directory)
                                        (buffer-terminal buffer)))
                         (directory (terminal:process-directory terminal)))
    (unless (equal directory (buffer-directory buffer))
      (setf (buffer-directory buffer) directory))))

(defun start-scrolling (terminal)
  "Freeze TERMINAL with its scrollback shown above the screen, unless it
already is, so output arriving while one reads the history does not take it
away.  Returns the number of scrollback lines in the buffer, or NIL when
there are none: TERMINAL is not frozen then, as there is nothing to read."
  (or (terminal:frozen-scrollback-lines terminal)
      (when (plusp (terminal:scrollback-count terminal))
        (let ((lines (terminal:copy-mode-on terminal)))
          (if (plusp lines)
              lines
              (progn (terminal:copy-mode-off terminal) nil))))))

(defun stop-scrolling (terminal)
  "Undo START-SCROLLING: remove the scrollback and show the whole screen again."
  (when (terminal:frozen-scrollback-lines terminal)
    (terminal:copy-mode-off terminal)
    (terminal:snap-to-bottom terminal)))

(defun back-to-screen (buffer)
  "Show the screen of BUFFER's terminal again, for input sent to it: what the
program does with it is to be seen.  Not in terminal-copy-mode (which does not
inherit terminal-mode), which keeps the history until one leaves it."
  (alexandria:when-let ((terminal (buffer-terminal buffer)))
    (when (typep (ensure-mode-object (buffer-major-mode buffer)) 'terminal-mode)
      (stop-scrolling terminal)
      (terminal:deactivate-scrollback terminal))))

(defun view-line (window)
  (line-number-at-point (window-view-point window)))

(defun show-screen-top (window lines)
  "Make WINDOW start at the screen's first row, below LINES lines of history."
  (move-to-line (window-view-point window) (1+ lines))
  (line-start (window-view-point window)))

(defun call-with-scrollback (terminal window downward-p scroll)
  "Call SCROLL, a scroll of WINDOW (towards the end when DOWNWARD-P), with
TERMINAL's scrollback shown.  A window that reaches the screen stops at its
first row; once every window of the buffer is there (in every frame, see
TERMINAL:BUFFER-WINDOWS), the terminal runs on as before.  Scrolling down
from the screen does nothing: there is nothing below it."
  (let* ((buffer (window-buffer window))
         (windows (adjoin window (terminal:buffer-windows buffer)))
         (scrolling (terminal:frozen-scrollback-lines terminal)))
    (unless (and downward-p
                 (or (not scrolling) (> (view-line window) scrolling)))
      (let ((lines (start-scrolling terminal)))
        (if (null lines)
            ;; No history: wheel up shows the screen from its first row.
            (show-screen-top window 0)
            (flet ((on-screen-p (window) (> (view-line window) lines)))
              (when (on-screen-p window)
                (show-screen-top window lines))
              (unwind-protect (funcall scroll)
                (cond ((every #'on-screen-p windows)
                       (stop-scrolling terminal))
                      ((on-screen-p window)
                       (show-screen-top window lines))))))))))

(defmethod execute ((mode terminal-mode) command argment)
  (sync-buffer-directory (current-buffer))
  (cond
    ;; Before the bypass commands: the mouse wheel scrolls from within
    ;; <mouse-event>, and scrolls up with SCROLL-DOWN by a negative count.
    ((and (typep command '(or scroll-up scroll-down))
          (get-current-terminal))
     (let ((terminal (get-current-terminal))
           (downward-p (eq (typep command 'scroll-down) (plusp (or argment 1)))))
       (cond
         ;; A scroll of zero lines does nothing (as SCROLL-DOWN does).
         ((eql argment 0)
          (call-next-method))
         ;; Once the process has exited the buffer is plain text: the
         ;; scrollback is put above it once, for scrolling up to reach it.
         ((terminal:process-exited-p terminal)
          (unless downward-p
            (terminal:activate-scrollback terminal))
          (call-next-method))
         (t
          (call-with-scrollback terminal (current-window) downward-p
                                (lambda () (call-next-method)))))))
    (*running-bypass-command*
     (call-next-method))
    ((member command *bypass-commands* :test #'typep)
     (let ((*running-bypass-command* t))
       (call-next-method)))
    ((typep command '(or scroll-up scroll-down))
     (call-next-method))
    ;; Once the process has exited the buffer is plain read-only text holding
    ;; the final screen, so let every other command behave as in any other buffer.
    ;; The same holds when there is no terminal (terminal-mode set by hand).
    ((let ((terminal (get-current-terminal)))
       (or (null terminal) (terminal:process-exited-p terminal)))
     (call-next-method))
    (t
     ;; Any other command (typing, etc.) goes to the terminal, which shows its
     ;; screen again (see BACK-TO-SCREEN).
     (terminal-input))))

(defun window-text-rows (window)
  (max 1 (- (window-height window) (if (window-use-modeline-p window) 1 0))))

(defun frame-shown-windows ()
  "The windows of the current frame: the tiled ones, the floating ones and the
side windows (a side window is usually, not always, floating too)."
  (let ((frame (current-frame)))
    (remove-duplicates
     (append (window-list)
             (frame-floating-windows frame)
             (remove nil (list (frame-leftside-window frame)
                               (frame-rightside-window frame)
                               (frame-bottomside-window frame)))))))

(defun reading-history-p (buffer terminal)
  "True while BUFFER's TERMINAL is frozen for its history to be read: by the
wheel, or in terminal-copy-mode.  It is not resized then: a resize reflows
the screen under the copy being read."
  (or (terminal:frozen-scrollback-lines terminal)
      (eq (buffer-major-mode buffer) 'terminal-copy-mode)))

(defun resize-terminal (terminal window)
  "Fit TERMINAL to the smallest window of the current frame showing WINDOW's
buffer, side and floating windows included, so no window loses the bottom
rows (where the prompt is) when the buffer is shown in windows of different
sizes.  Windows of other frames (hidden frame-multiplexer tabs) are not
seen: the terminal fits them when their tab is shown (see
FIT-SHOWN-TERMINALS)."
  (let ((windows (adjoin window (get-buffer-windows (window-buffer window)
                                                    :include-floating-windows t))))
    (terminal:resize terminal
                     :rows (reduce #'min windows :key #'window-text-rows)
                     :cols (reduce #'min windows :key #'window-width))))

(defun fit-shown-terminals ()
  "Fit each terminal shown in the current frame to its windows there, after
every command: the command may have shown another tab, or split or closed a
window, without the current buffer being the terminal.  A terminal whose
history is being read is left as it is (see READING-HISTORY-P), until a
command after it runs on."
  (let ((seen '()))
    (dolist (window (frame-shown-windows))
      (let ((buffer (window-buffer window)))
        (unless (member buffer seen)
          (push buffer seen)
          (alexandria:when-let ((terminal (buffer-terminal buffer)))
            (unless (reading-history-p buffer terminal)
              (resize-terminal terminal window))))))))

(define-command terminal-resize () ()
  (alexandria:when-let ((terminal (get-current-terminal))
                        (window (current-window)))
    (resize-terminal terminal window)))

(defun paste (string)
  (let ((terminal (get-current-live-terminal)))
    (back-to-screen (current-buffer))
    (terminal:paste-string terminal string)))

(defmethod lem-core:paste-using-mode ((mode terminal-mode) string)
  (paste string))

(define-command terminal-yank () ()
  "Paste the latest kill (or the clipboard) into the terminal."
  (alexandria:when-let ((string (yank-from-clipboard-or-killring)))
    (paste string)))

(defun on-window-size-change (window)
  ;; Only for a window of the current frame (not one of a frame being set up
  ;; or hidden): the terminal fits the windows shown.  Not while its history
  ;; is read (see READING-HISTORY-P): FIT-SHOWN-TERMINALS fits it afterwards.
  (let ((buffer (window-buffer window)))
    (alexandria:when-let (terminal (and (member window (frame-shown-windows))
                                        (buffer-terminal buffer)))
      (unless (reading-history-p buffer terminal)
        (resize-terminal terminal window)))))

(add-hook *window-size-change-functions*
          'on-window-size-change)

(defun release-read-terminals ()
  "Let each terminal the wheel froze run on again once no window showing it
reads its history: every window is on its screen, or none shows it.  The
window that read the history may have gone with its tab
(frame-multiplexer-delete, or frame-multiplexer turned off), and no wheel in
the others would let it run on: a window on the screen does not scroll down.
A terminal no window shows is not being read either, and a window showing it
again would start at the oldest line of the history.  Cheap per command:
only frozen terminals look at their windows."
  (dolist (buffer (buffer-list))
    (alexandria:when-let* ((terminal (buffer-terminal buffer))
                           (lines (and (typep (ensure-mode-object (buffer-major-mode buffer))
                                              'terminal-mode)
                                       (terminal:frozen-scrollback-lines terminal))))
      (when (every (lambda (window) (> (view-line window) lines))
                   (terminal:buffer-windows buffer))
        (stop-scrolling terminal)))))

(add-hook *post-command-hook*
          'fit-shown-terminals)

(add-hook *post-command-hook*
          'release-read-terminals)
