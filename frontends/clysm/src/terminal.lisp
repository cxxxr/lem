;;;; terminal.lisp — M-x terminal in the desktop application
;;;;
;;;; Lem's terminal (extensions/terminal/) runs a shell in a pseudo-terminal
;;;; and keeps its screen with libvterm, a C library loaded with CFFI, which a
;;;; page does not have.  Here the same two parts are JavaScript packages, in
;;;; the page of the desktop application: node-pty (the pseudo-terminal,
;;;; Node.js's native module that VS Code uses) and @xterm/headless
;;;; (xterm.js's terminal without its display), both installed with Electron
;;;; (desktop/package.json) at fixed versions.  They are driven from Lisp
;;;; with the JS package, as Lem's terminal drives libvterm with CFFI.
;;;; The screen is read as xterm.js keeps it: the cells of a row are an
;;;; array of 32-bit words (content, foreground, background) that is copied
;;;; into Lisp at once and decoded here (reading it cell by cell through
;;;; xterm.js's API would take a call per attribute of each cell, some 30
;;;; times slower).
;;;; This file defines what Lem's terminal-mode.lisp uses of the two packages
;;;; it is written against — the key constants of LEM-TERMINAL/FFI and the
;;;; terminal of LEM-TERMINAL/TERMINAL — and lem-clysm-terminal.asd loads
;;;; terminal-mode.lisp itself after it, so the commands, keys and modes (copy
;;;; mode, C-q, the keys that go to Lem) are Lem's.
;;;;
;;;; The terminal's output is parsed by xterm.js as it comes (in the page's
;;;; one thread, between the editor's events); when it has changed, the
;;;; editor renders it at most every TERMINAL-RENDER-THROTTLE-MS: it reads
;;;; the rows of the screen, and puts those that changed since the last time
;;;; in the lines of the buffer, as a string and runs of attributes.

(defpackage :lem-terminal/ffi
  (:use :cl)
  (:documentation "The key and modifier codes of libvterm that terminal-mode.lisp
sends (here they are encoded as xterm does, by KEY-SEQUENCE)."))
(in-package :lem-terminal/ffi)

(defconstant vterm_key_none 0)
(defconstant vterm_key_enter 1)
(defconstant vterm_key_tab 2)
(defconstant vterm_key_backspace 3)
(defconstant vterm_key_escape 4)
(defconstant vterm_key_up 5)
(defconstant vterm_key_down 6)
(defconstant vterm_key_left 7)
(defconstant vterm_key_right 8)
(defconstant vterm_key_ins 9)
(defconstant vterm_key_del 10)
(defconstant vterm_key_home 11)
(defconstant vterm_key_end 12)
(defconstant vterm_key_pageup 13)
(defconstant vterm_key_pagedown 14)
(defconstant vterm_key_function_0 256)

(defconstant vterm_mod_none #x00)
(defconstant vterm_mod_shift #x01)
(defconstant vterm_mod_alt #x02)
(defconstant vterm_mod_ctrl #x04)

(uiop:define-package :lem-terminal/terminal
  (:use :cl :lem)
  (:local-nicknames (:line :lem/buffer/line))
  (:export :find-terminal-buffer
           :create
           :destroy
           :copy-mode-on
           :copy-mode-off
           :process-exited-p
           :process-directory
           :render
           :update
           :input-character
           :input-key
           :paste-string
           :resize
           :adjust-point
           :scrollback-count
           :activate-scrollback
           :deactivate-scrollback
           :snap-to-bottom
           :buffer-windows
           :frozen-scrollback-lines
           :terminal-render-throttle-ms))
(in-package :lem-terminal/terminal)

(defvar *terminals* '()
  "The terminals, the most recent first.")

(defparameter *scrollback-limit* 5000
  "The number of lines that xterm.js keeps above the screen.")

(defparameter *bell-interval-seconds* 1
  "Minimum seconds between two bell notices from the same terminal.")

(defparameter *palette*
  (let ((palette (make-array 256)))
    ;; the 16 colours of xterm.js's own theme
    (replace palette #(#x2e3436 #xcc0000 #x4e9a06 #xc4a000 #x3465a4 #x75507b #x06989a #xd3d7cf
                       #x555753 #xef2929 #x8ae234 #xfce94f #x729fcf #xad7fa8 #x34e2e2 #xeeeeec))
    ;; then the 6x6x6 cube and the 24 greys of xterm
    (flet ((level (v) (if (zerop v) 0 (+ 55 (* v 40)))))
      (dotimes (i 216)
        (setf (aref palette (+ 16 i))
              (logior (ash (level (floor i 36)) 16)
                      (ash (level (mod (floor i 6) 6)) 8)
                      (level (mod i 6))))))
    (dotimes (i 24)
      (let ((v (+ 8 (* i 10))))
        (setf (aref palette (+ 232 i)) (logior (ash v 16) (ash v 8) v))))
    palette)
  "The RGB values of the 256 indexed colours.")

(defparameter *attribute-cache-limit* 4096
  "Maximum entries in a terminal's attribute cache (then it is cleared).")

(define-editor-variable terminal-render-throttle-ms 33
  "Minimum milliseconds between two renders of a terminal's screen.")

(define-editor-variable terminal-title-as-buffer-name t
  "When non-NIL, a terminal buffer is renamed after the window title its
program sets (e.g. bash's \"user@host: ~/dir\"), as \"*Terminal: TITLE*\".")

(defconstant +cell-octets+ 12
  "The octets of a cell in xterm.js's arrays of a row (BufferLine's _data):
three 32-bit words, little-endian: content, foreground, background.")

;;; ---- a terminal

(defclass terminal ()
  ((pty :initarg :pty
        :initform nil
        :accessor terminal-pty
        :documentation "The pseudo-terminal and its child process (node-pty).")
   (xterm :initarg :xterm
          :accessor terminal-xterm
          :documentation "The screen (@xterm/headless's Terminal).")
   (output-subscription :initform nil
                        :accessor terminal-output-subscription
                        :documentation "The handler of the pty's output (its dispose() removes it).")
   (closed :initform nil
           :accessor terminal-closed
           :documentation "T once the process has ended (and its output is
parsed) or was killed: nothing is written to it any more.")
   (render-notified :initform nil
                    :accessor terminal-render-notified
                    :documentation "T when the editor was told that the screen
changed and has not read it yet.")
   (shown-rows :initform (vector)
               :accessor terminal-shown-rows
               :documentation "The octets of each row of the screen as last rendered,
to render only the rows that changed.")
   (buffer :initarg :buffer
           :reader terminal-buffer)
   (rows :initarg :rows
         :accessor terminal-rows)
   (cols :initarg :cols
         :accessor terminal-cols)
   (copy-mode :initform nil
              :accessor terminal-copy-mode)
   (redraw-all :initform t
               :accessor terminal-redraw-all
               :documentation "T when the next render draws every row.")
   (scrollback-active :initform nil
                      :accessor terminal-scrollback-active
                      :documentation "T when the lines above the screen are in the buffer.")
   (scrollback-shown :initform 0
                     :accessor terminal-scrollback-shown
                     :documentation "How many lines above the screen are in the
buffer while SCROLLBACK-ACTIVE (the rows may change meanwhile).")
   (attribute-cache :initform (make-hash-table :test #'equal)
                    :accessor terminal-attribute-cache
                    :documentation "Attributes by their keys (see CELL-KEYS).")
   (render-timer :initform nil
                 :accessor terminal-render-timer)
   (render-timer-armed :initform nil
                       :accessor terminal-render-timer-armed)
   (last-render-time :initform 0
                     :accessor terminal-last-render-time)
   (last-bell-time :initform nil
                   :accessor terminal-last-bell-time)
   (dead :initform nil
         :accessor terminal-dead
         :documentation "T once DESTROY has run.")
   (exit-status :initform nil
                :accessor terminal-exit-status
                :documentation "NIL while the process runs, then its exit code
(128+signal when it was killed by a signal).")))

(defun process-exited-p (terminal)
  "Return T when TERMINAL's child process has exited."
  (not (null (terminal-exit-status terminal))))

(defun live-p (terminal)
  (not (or (terminal-dead terminal) (process-exited-p terminal))))

(defun find-terminal-buffer ()
  "Return the buffer of the most recently created terminal whose process is
still running, or NIL."
  (alexandria:when-let (terminal (find-if-not #'process-exited-p *terminals*))
    (terminal-buffer terminal)))

;;; ---- node-pty and xterm.js

(defun node-module (name)
  "The Node.js module NAME, with the page's require()."
  (let ((require (js:global "require")))
    (unless (eq (js:typeof require) :function)
      (editor-error "Cannot start the terminal: it runs in the desktop application"))
    (js:funcall require name)))

(defun child-environment ()
  "The environment of the shell: the editor's, as xterm-256color, without
the variables of Electron and Clysm."
  (let* ((object (js:global "Object"))
         (env (js:call object "assign" (js:object)
                       (js:get (js:global "process") "env")))
         (names (js:call object "keys" env)))
    (dotimes (i (js:get names "length"))
      (let ((name (js:get names i)))
        (when (or (alexandria:starts-with-subseq "ELECTRON_" name)
                  (alexandria:starts-with-subseq "CLYSM_" name))
          (js:call (js:global "Reflect") "deleteProperty" env name))))
    (js:set env "TERM" "xterm-256color")
    (js:set env "COLORTERM" "truecolor")
    env))

(defun existing-directory (directory)
  "DIRECTORY if it is a directory, otherwise the home directory."
  (if (and directory
           (ignore-errors
            (js:call (js:call (node-module "node:fs") "statSync" directory) "isDirectory")))
      directory
      (js:call (node-module "node:os") "homedir")))

(defun exit-status-code (event)
  "The exit status of node-pty's exit EVENT: 128+signal when it was killed."
  (let ((signal (js:get event "signal")))
    (if (and (integerp signal) (plusp signal))
        (+ 128 signal)
        (js:get event "exitCode"))))

(defun spawn-pty (xterm directory program args rows cols)
  "Start PROGRAM with ARGS, or the user's shell, in DIRECTORY in a new
pseudo-terminal of ROWS and COLS.  When it cannot be started, XTERM (the
screen made for it) is disposed of before the error goes on."
  (let ((pty nil))
    (unwind-protect
         (let ((env (child-environment)))
           (setf pty (js:call (node-module "node-pty") "spawn"
                              (or program (js:get env "SHELL") "/bin/sh")
                              (coerce args 'vector)
                              ;; the output as Buffers, given to xterm.js as they are
                              ;; (not made Lisp strings), which decodes UTF-8 itself
                              (js:object "name" "xterm-256color" "rows" rows "cols" cols
                                         "env" env "cwd" (existing-directory directory)
                                         "encoding" :null))))
      (unless pty
        (ignore-errors (js:call xterm "dispose"))))
    pty))

(defun start-process (terminal directory program args)
  "Start PROGRAM with ARGS, or the user's shell, in DIRECTORY, in a
pseudo-terminal whose output goes to a new xterm.js screen; the editor is
told of the changes of the screen, the exit, the title and the bell."
  (let* ((rows (terminal-rows terminal))
         (cols (terminal-cols terminal))
         (xterm (js:new (js:get (node-module "@xterm/headless") "Terminal")
                        (js:object "rows" rows "cols" cols
                                   "scrollback" *scrollback-limit*
                                   "allowProposedApi" t)))
         (pty (spawn-pty xterm directory program args rows cols))
         (parsed (lambda ()
                   ;; the editor is told once that the screen changed, until it reads it
                   (unless (or (terminal-render-notified terminal)
                               (terminal-closed terminal))
                     (setf (terminal-render-notified terminal) t)
                     (send-event (lambda () (arm-render-timer terminal))))
                   nil)))
    (setf (terminal-xterm terminal) xterm
          (terminal-pty terminal) pty)
    (setf (terminal-output-subscription terminal)
          (js:call pty "onData"
                   (lambda (data)
                     (unless (terminal-closed terminal)
                       (js:call xterm "write" data parsed))
                     nil)))
    (js:call pty "onExit"
             (lambda (event)
               (unless (terminal-closed terminal)
                 (let ((status (exit-status-code event)))
                   ;; after what was written before it is parsed
                   (js:call xterm "write" ""
                            (lambda ()
                              (unless (terminal-closed terminal)
                                (setf (terminal-closed terminal) t)
                                (send-event (lambda () (finish-process terminal status))))
                              nil))))
               nil))
    (js:call xterm "onTitleChange"
             (lambda (title)
               (unless (terminal-closed terminal)
                 (send-event (lambda () (apply-title terminal title))))
               nil))
    (js:call xterm "onBell"
             (lambda ()
               (unless (terminal-closed terminal)
                 (send-event (lambda () (notify-bell terminal))))
               nil))))

(defun write-to-process (terminal string)
  "Send STRING to TERMINAL's process, unless it has ended."
  (unless (terminal-closed terminal)
    (js:call (terminal-pty terminal) "write" string)))

(defun kill-process (terminal)
  "Kill TERMINAL's process if it still runs, and free its screen."
  (unless (terminal-closed terminal)
    (setf (terminal-closed terminal) t)
    (ignore-errors (js:call (terminal-pty terminal) "kill")))
  (alexandria:when-let ((subscription (terminal-output-subscription terminal)))
    ;; the screen is freed even when this fails
    (ignore-errors (js:call subscription "dispose")))
  (js:call (terminal-xterm terminal) "dispose"))

(defun active-screen (terminal)
  "xterm.js's active buffer (the normal screen or the alternate one)."
  (js:get (js:get (terminal-xterm terminal) "buffer") "active"))

(defun xterm-mode-p (terminal mode)
  "Whether the program has turned on xterm.js's MODE (e.g. \"bracketedPasteMode\")."
  (js:get (js:get (terminal-xterm terminal) "modes") mode))

(defun create (&key (rows (alexandria:required-argument :rows))
                    (cols (alexandria:required-argument :cols))
                    (buffer (alexandria:required-argument :buffer))
                    (directory (alexandria:required-argument :directory))
                    (program nil)
                    (argv nil))
  "Start PROGRAM with ARGV (its first element is the program's name), or the
user's shell in DIRECTORY, in a pseudo-terminal of ROWS and COLS whose screen
is rendered into BUFFER.  When it cannot be started, BUFFER is deleted
unless a terminal ran in it before (M-x terminal-restart keeps its buffer)."
  (let ((terminal (make-instance 'terminal :buffer buffer :rows rows :cols cols))
        (started nil))
    (unwind-protect
         (progn
           (handler-case
               (start-process terminal
                              ;; the native name as it is (made a pathname, a
                              ;; name with * ? [ would be a wild one)
                              (and (plusp (length directory)) directory)
                              program
                              (rest argv))
             (js:js-error (e)
               (editor-error "Cannot start the terminal: ~A" (js:js-error-message e))))
           (setf started t))
      (unless started
        ;; The process may run already (spawned, then an error setting up its
        ;; handlers): kill it and free the screen, as nothing else could.  The
        ;; error that made the start fail goes on.
        (alexandria:when-let ((pty (terminal-pty terminal)))
          ;; SIGKILL, not kill-process's SIGHUP, which a program can ignore;
          ;; kill-process then only frees the screen.
          (ignore-errors (js:call pty "kill" "SIGKILL"))
          (setf (terminal-closed terminal) t)
          (ignore-errors (kill-process terminal)))
        (unless (buffer-value buffer 'terminal-started)
          (delete-buffer buffer))))
    (setf (buffer-value buffer 'terminal-started) t)
    (push terminal *terminals*)
    terminal))

(defun arm-render-timer (terminal)
  "Render TERMINAL's screen soon: at once if the last render was more than
TERMINAL-RENDER-THROTTLE-MS ago, otherwise when that time has passed."
  (when (and (live-p terminal)
             (not (terminal-copy-mode terminal))
             (not (terminal-render-timer-armed terminal)))
    (setf (terminal-render-timer-armed terminal) t)
    (start-timer (or (terminal-render-timer terminal)
                     (setf (terminal-render-timer terminal)
                           (make-timer (lambda () (render-tick terminal))
                                       :name "terminal-render")))
                 (render-delay-ms terminal))))

(defun render-delay-ms (terminal)
  (let ((elapsed-ms (floor (* 1000 (- (get-internal-real-time)
                                      (terminal-last-render-time terminal)))
                           internal-time-units-per-second)))
    (max 1 (- (variable-value 'terminal-render-throttle-ms :global) elapsed-ms))))

(defun render-tick (terminal)
  (setf (terminal-render-timer-armed terminal) nil)
  (when (and (live-p terminal)
             (not (terminal-copy-mode terminal))
             (not (deleted-buffer-p (terminal-buffer terminal))))
    (render terminal)
    (setf (terminal-last-render-time terminal) (get-internal-real-time))))

(defun update (terminal)
  "Render TERMINAL now."
  (render terminal)
  (redraw-display))

(defun exit-notice (status)
  (format nil "~A  (M-x terminal-restart to start again)"
          (if (minusp status)
              "[Process exited]"
              (format nil "[Process exited with code ~D]" status))))

(defun append-exit-notice (terminal status)
  "Append the exit notice after the last non-blank line of the final screen."
  (let* ((buffer (terminal-buffer terminal))
         (point (buffer-point buffer)))
    (with-buffer-read-only buffer nil
      (buffer-end point)
      (with-point ((start point))
        (skip-whitespace-backward start)
        (delete-between-points start point))
      (unless (start-buffer-p point)
        (insert-string point (format nil "~2%")))
      (insert-string point (exit-notice status)))))

(defun finish-process (terminal status)
  "The child process has exited: render the final screen, freeze the
terminal and append an exit notice.  Scrolling through the lines above the
screen (and copy mode) ends there, as in Lem's terminal: RENDER removes
them, and the windows show the final output."
  (when (and (live-p terminal)
             (not (deleted-buffer-p (terminal-buffer terminal))))
    (let ((scrolled (terminal-scrollback-active terminal)))
      (setf (terminal-copy-mode terminal) nil)
      (render terminal)
      (setf (terminal-exit-status terminal) status)
      (append-exit-notice terminal status)
      (when scrolled
        (snap-to-bottom terminal))
      (redraw-display))))

(defgeneric destroy (terminal)
  (:documentation "Kill TERMINAL's process if it still runs.  Safe to call more than once."))

(defmethod destroy ((terminal terminal))
  (unless (terminal-dead terminal)
    (setf (terminal-dead terminal) t)
    (alexandria:when-let ((timer (terminal-render-timer terminal)))
      (ignore-errors (stop-timer timer)))
    (alexandria:deletef *terminals* terminal)
    (kill-process terminal)))

(defgeneric copy-mode-on (terminal)
  (:documentation "Freeze rendering and show the scrollback above the screen.
Returns the number of scrollback lines in the buffer (also when they were
shown already)."))
(defgeneric copy-mode-off (terminal)
  (:documentation "Remove the scrollback and render the screen again."))

(defmethod copy-mode-on ((terminal terminal))
  "Returns the number of scrollback lines in the buffer (also when they were
shown already).  In copy mode, the screen stays frozen: lines that left it
since show twice then."
  ;; Render what xterm.js parsed since the last render first: the lines it
  ;; pushed off the screen are above it already, and would show twice.  Not
  ;; when the scrollback is shown already: RENDER would take it away.
  (when (and (live-p terminal)
             (not (terminal-copy-mode terminal))
             (not (terminal-scrollback-active terminal))
             (not (deleted-buffer-p (terminal-buffer terminal))))
    (render terminal))
  (setf (terminal-copy-mode terminal) t)
  (if (terminal-scrollback-active terminal)
      (terminal-scrollback-shown terminal)
      (or (activate-scrollback terminal) 0)))

(defmethod copy-mode-off ((terminal terminal))
  (setf (terminal-copy-mode terminal) nil)
  (deactivate-scrollback terminal)
  (setf (terminal-redraw-all terminal) t)
  (arm-render-timer terminal))

(defun process-directory (terminal)
  "The current directory of TERMINAL's process (the shell) as a directory
namestring, or NIL when it is unknown (it is known on Linux)."
  (when (live-p terminal)
    (let ((cwd (ignore-errors
                (js:call (node-module "node:fs") "readlinkSync"
                         (format nil "/proc/~D/cwd" (js:get (terminal-pty terminal) "pid"))))))
      (when (stringp cwd)
        (uiop:native-namestring (uiop:ensure-directory-pathname
                                 (uiop:parse-native-namestring cwd)))))))

;;; ---- rendering

(defun octet-word-24 (octets index)
  "The low 24 bits of the little-endian 32-bit word at INDEX of OCTETS."
  (logior (aref octets index)
          (ash (aref octets (+ index 1)) 8)
          (ash (aref octets (+ index 2)) 16)))

(defun cell-content-index (cell)
  (* cell +cell-octets+))

(defun cell-code-point (octets cell)
  "The code point of CELL, 0 when it is empty."
  (ldb (byte 21 0) (octet-word-24 octets (cell-content-index cell))))

(defun cell-combined-p (octets cell)
  "Whether CELL holds several code points (BufferLine's _combined has them)."
  (logbitp 5 (aref octets (+ (cell-content-index cell) 2))))

(defun cell-width (octets cell)
  "The width of CELL: 0 for the second cell of a wide character."
  (ldb (byte 2 6) (aref octets (+ (cell-content-index cell) 2))))

(defun cell-extended-p (octets cell)
  "Whether CELL has extended attributes (an underline style, a URL)."
  (logbitp 4 (aref octets (+ (cell-content-index cell) 11))))

(defun cell-color (octets index)
  "The colour bits of the foreground (INDEX 4) or background (INDEX 8) word of
a cell: its mode (1 and 2 indexed, 3 RGB) and value, 26 bits; 0 for the
default colour."
  (let ((mode (ldb (byte 2 0) (aref octets (+ index 3)))))
    (if (zerop mode)
        0
        (logior (octet-word-24 octets index) (ash mode 24)))))

(defun cell-keys (octets cell extended-underline-p)
  "The attribute keys of CELL, two fixnums: the foreground colour with the
flags (1 bold, 2 underline, 4 reverse), and the background colour."
  (let* ((base (cell-content-index cell))
         (fg-flags (aref octets (+ base 7))))
    (values (logior (ash (cell-color octets (+ base 4)) 3)
                    (if (logbitp 3 fg-flags) 1 0)
                    (if (or (logbitp 4 fg-flags) extended-underline-p) 2 0)
                    (if (logbitp 2 fg-flags) 4 0))
            (cell-color octets (+ base 8)))))

(defun color-from-bits (bits)
  "The colour of 26 colour bits (see CELL-COLOR), NIL for the default."
  (let ((mode (ldb (byte 2 24) bits))
        (value (ldb (byte 24 0) bits)))
    (unless (zerop mode)
      (parse-color (format nil "#~(~6,'0x~)"
                           (if (= mode 3) value (aref *palette* (ldb (byte 8 0) value))))))))

(defun keys-attribute (terminal fg-key bg-key)
  "The attribute of the keys of CELL-KEYS, NIL for the default one."
  (unless (and (zerop fg-key) (zerop bg-key))
    (let ((cache (terminal-attribute-cache terminal))
          (key (cons fg-key bg-key)))
      (or (gethash key cache)
          (progn
            (when (>= (hash-table-count cache) *attribute-cache-limit*)
              (clrhash cache))
            (setf (gethash key cache)
                  (make-attribute :foreground (color-from-bits (ash fg-key -3))
                                  :background (color-from-bits bg-key)
                                  :bold (logtest fg-key 1)
                                  :underline (logtest fg-key 2)
                                  :reverse (logtest fg-key 4))))))))

(defun screen-line (terminal row)
  "xterm.js's line (BufferLine) of screen ROW, or NIL."
  (let* ((screen (active-screen terminal))
         (view (js:call screen "getLine" (+ (js:get screen "baseY") row))))
    (and view (js:get view "_line"))))

(defun line-octets (line cols)
  "The octets of the first COLS cells of xterm.js's LINE."
  (let ((octets (js:to-octets (js:get line "_data")))
        (length (* cols +cell-octets+)))
    (if (> (length octets) length) (subseq octets 0 length) octets)))

(defun row-unchanged-p (octets previous)
  "Whether a row of OCTETS shows what PREVIOUS showed: the same octets, and
no cell whose text or underline is kept outside them."
  (and previous
       (equalp octets previous)
       (loop :for cell :from 0 :below (floor (length octets) +cell-octets+)
             :never (or (cell-combined-p octets cell)
                        (cell-extended-p octets cell)))))

(defun cell-string (line octets cell)
  "The text of CELL: a space for an empty cell."
  (cond ((cell-combined-p octets cell)
         (js:get (js:get line "_combined") cell))
        ((zerop (cell-code-point octets cell)) " ")
        (t (string (code-char (cell-code-point octets cell))))))

(defun extended-underline-p (line octets cell)
  (and (cell-extended-p octets cell)
       (let ((extended (js:get (js:get line "_extendedAttrs") cell)))
         (and extended
              (plusp (js:get extended "underlineStyle"))))))

(defun row-text-and-attributes (terminal line octets)
  "The text of a row, one character per glyph (the second cell of a wide
character is skipped), without the blanks that pad it, and its attributes,
a list of (start end attribute) in characters."
  (let ((glyphs '())
        (count 0)
        (end 0))
    ;; the glyphs: (string fg-key bg-key), and how many of them are not padding
    (dotimes (cell (floor (length octets) +cell-octets+))
      (unless (zerop (cell-width octets cell))
        (let ((string (cell-string line octets cell)))
          (multiple-value-bind (fg-key bg-key)
              (cell-keys octets cell (extended-underline-p line octets cell))
            (push (list string fg-key bg-key) glyphs)
            (incf count)
            (when (or (string/= string " ")
                      (/= bg-key 0)
                      (logtest fg-key 6))
              (setf end count))))))
    (let ((text (make-string-output-stream))
          (attributes '())
          (index 0)
          (start 0)
          (run-fg nil)
          (run-bg nil))
      (flet ((close-run ()
               (alexandria:when-let ((attribute (and run-fg (keys-attribute terminal run-fg run-bg))))
                 (push (list start index attribute) attributes))))
        (loop :for (string fg-key bg-key) :in (subseq (nreverse glyphs) 0 end)
              :do (unless (and (eql fg-key run-fg) (eql bg-key run-bg))
                    (close-run)
                    (setf start index run-fg fg-key run-bg bg-key))
                  (write-string string text)
                  (incf index (length string)))
        (close-run))
      (values (get-output-stream-string text) (nreverse attributes)))))

(defun set-row (line text attributes)
  "Put TEXT and its ATTRIBUTES, (start end attribute)..., in LINE.  This sets
the line directly, as Lem's terminal does: the terminal buffer is only
rendered into (no undo, no change hooks)."
  (line:set-line-string text line)
  (setf (getf (line:line-plist line) :attribute) attributes))

(defun move-to-cursor (point row col)
  "Move POINT to screen ROW and COL, padding the line with spaces when the
cursor is past the end of its text."
  (move-to-line point (1+ row))
  (move-to-column point col)
  (let ((missing (- col (point-column point))))
    (when (plusp missing)
      (insert-string point (make-string missing :initial-element #\Space)))))

(defun fit-lines (buffer rows)
  "Make BUFFER have ROWS lines."
  (let ((point (buffer-point buffer))
        (nlines (buffer-nlines buffer)))
    (cond ((< nlines rows)
           (buffer-end point)
           (insert-string point (make-string (- rows nlines) :initial-element #\Newline)))
          ((> nlines rows)
           (move-to-line point (1+ rows))
           (line-start point)
           (with-point ((start point))
             (character-offset start -1)
             (buffer-end point)
             (delete-between-points start point))))))

(defgeneric render (terminal)
  (:documentation "Put the rows of TERMINAL's screen that changed in its buffer, and
the point at the cursor."))

(defmethod render ((terminal terminal))
  (let* ((buffer (terminal-buffer terminal))
         (point (buffer-point buffer))
         (rows (terminal-rows terminal))
         (cols (terminal-cols terminal))
         (screen (active-screen terminal)))
    (setf (terminal-render-notified terminal) nil)
    (when (or (terminal-redraw-all terminal)
              (/= (length (terminal-shown-rows terminal)) rows))
      (setf (terminal-shown-rows terminal) (make-array rows :initial-element nil)
            (terminal-redraw-all terminal) nil))
    (deactivate-scrollback terminal)
    (with-buffer-read-only buffer nil
      (fit-lines buffer rows)
      (move-to-line point 1)
      (loop :with shown := (terminal-shown-rows terminal)
            :for row :from 0 :below rows
            :for buffer-line := (point-line point) :then (line:line-next buffer-line)
            :for line := (screen-line terminal row)
            :for octets := (and line (line-octets line cols))
            :unless (and octets (row-unchanged-p octets (aref shown row)))
              :do (setf (aref shown row) octets)
                  (if line
                      (multiple-value-bind (text attributes)
                          (row-text-and-attributes terminal line octets)
                        (set-row buffer-line text attributes))
                      (set-row buffer-line "" nil)))
      (move-to-cursor point (js:get screen "cursorY") (js:get screen "cursorX")))))

(defgeneric adjust-point (terminal)
  (:documentation "Move the point of TERMINAL's buffer to the cursor."))

(defmethod adjust-point ((terminal terminal))
  ;; Not once the process has exited: the buffer holds the final text then,
  ;; maybe with the lines above the screen, and the cursor row is not a line
  ;; of it.
  (unless (or (terminal-dead terminal) (process-exited-p terminal))
    (let ((screen (active-screen terminal))
          (buffer (terminal-buffer terminal)))
      (with-buffer-read-only buffer nil
        (move-to-cursor (buffer-point buffer)
                        (js:get screen "cursorY")
                        (js:get screen "cursorX"))))))

;;; ---- the lines above the screen

(defun scrollback-count (terminal)
  "The number of lines above the screen."
  (if (terminal-dead terminal)
      0
      (js:get (active-screen terminal) "baseY")))

(defun scrollback-text (terminal)
  "The lines above the screen, each ended by a newline."
  (let ((screen (active-screen terminal)))
    (with-output-to-string (out)
      (dotimes (y (js:get screen "baseY"))
        (write-line (js:call (js:call screen "getLine" y) "translateToString" t) out)))))

(defun buffer-windows (buffer)
  "Every window showing BUFFER: in every frame (each frame-multiplexer tab is
one), side and floating windows too.  The scrollback goes in and out of
BUFFER for all of them, so all of them keep their place, and a terminal stays
frozen while any of them reads the history."
  (remove-duplicates
   (loop :for frame :in (all-frames)
         :append (get-buffer-windows buffer :frame frame :include-floating-windows t))))

(defun buffer-view-points (buffer)
  (cons (buffer-point buffer)
        (loop :for window :in (buffer-windows buffer)
              :collect (window-view-point window)
              ;; a window that is not the current one has a point of its own
              :unless (eq window (current-window))
                :collect (window-point window))))

(defun point-positions (points)
  (mapcar (lambda (point)
            (cons (line-number-at-point point) (point-charpos point)))
          points))

(defun shift-points-by-lines (points positions delta)
  (loop :for point :in points
        :for (line-number . charpos) :in positions
        :do (move-to-line point (+ line-number delta))
            (line-start point)
            (character-offset point (min charpos (length (line-string point))))))

(defun deactivate-scrollback (terminal)
  "Remove the lines above the screen from the buffer.  Not once the process
has exited: the buffer then holds the final text, and keeps it."
  (when (and (terminal-scrollback-active terminal)
             (not (process-exited-p terminal)))
    (setf (terminal-scrollback-active terminal) nil)
    (let ((buffer (terminal-buffer terminal))
          (shown (terminal-scrollback-shown terminal)))
      (when (plusp shown)
        (with-buffer-read-only buffer nil
          ;; Counted as they were put in: the rows may have changed since.
          (with-point ((start (buffer-point buffer))
                       (end (buffer-point buffer)))
            (buffer-start start)
            (move-to-line end (1+ shown))
            (line-start end)
            (delete-between-points start end)))))))

(defun activate-scrollback (terminal)
  "Insert the lines above the screen into the buffer, keeping the cursor and
the windows on what they showed.  Returns how many, or NIL when it inserts
none."
  (unless (or (terminal-scrollback-active terminal)
              (terminal-dead terminal))
    (let ((text (scrollback-text terminal)))
      (when (plusp (length text))
        (setf (terminal-scrollback-active terminal) t
              (terminal-scrollback-shown terminal) (count #\Newline text))
        (let* ((buffer (terminal-buffer terminal))
               (points (buffer-view-points buffer))
               (positions (point-positions points)))
          (with-buffer-read-only buffer nil
            (with-point ((start (buffer-point buffer)))
              (buffer-start start)
              (insert-string start text)))
          (let ((count (count #\Newline text)))
            (shift-points-by-lines points positions count)
            count))))))

(defun frozen-scrollback-lines (terminal)
  "The number of scrollback lines above the screen in the buffer while copy
mode freezes TERMINAL with them shown, or NIL."
  (and (terminal-copy-mode terminal)
       (terminal-scrollback-active terminal)
       (terminal-scrollback-shown terminal)))

(defun snap-to-bottom (terminal)
  "Show the screen in every window of the terminal's buffer."
  (dolist (window (buffer-windows (terminal-buffer terminal)))
    (buffer-start (window-view-point window))))

;;; ---- input

(defgeneric input-character (terminal character &key mod)
  (:documentation "Send CHARACTER with MOD (VTERM_MOD_...) to TERMINAL's process."))
(defgeneric input-key (terminal key &key mod)
  (:documentation "Send KEY (VTERM_KEY_...) with MOD (VTERM_MOD_...) to TERMINAL's process."))
(defgeneric paste-string (terminal string)
  (:documentation "Send STRING to TERMINAL's process as pasted text."))
(defgeneric resize (terminal &key rows cols)
  (:documentation "Make TERMINAL's screen and pseudo-terminal ROWS by COLS."))

(defun escape-sequence (&rest parts)
  "ESC followed by PARTS (strings, characters and numbers) as by PRINC."
  (format nil "~C~{~A~}" (code-char 27) parts))

(defun key-sequence (key mod application-cursor-keys)
  "What xterm sends for KEY, a VTERM_KEY_..., with MOD, VTERM_MOD_... (shift
1, alt 2, ctrl 4), or NIL for a key it does not know."
  (let ((modifier (if (zerop mod) "" (format nil ";~D" (1+ mod)))))
    (flet ((meta (string)
             (if (logtest mod 2) (escape-sequence string) string))
           (cursor (final)
             (cond ((plusp mod) (escape-sequence "[1" modifier final))
                   (application-cursor-keys (escape-sequence "O" final))
                   (t (escape-sequence "[" final))))
           (tilde (n)
             (escape-sequence "[" n modifier "~")))
      (case key
        (1 (meta (string #\Return)))
        (2 (if (logtest mod 1) (escape-sequence "[Z") (meta (string #\Tab))))
        (3 (meta (string (code-char (if (logtest mod 4) #x08 #x7f)))))
        (4 (escape-sequence))
        (5 (cursor "A"))
        (6 (cursor "B"))
        (7 (cursor "D"))
        (8 (cursor "C"))
        (9 (tilde 2))
        (10 (tilde 3))
        (11 (cursor "H"))
        (12 (cursor "F"))
        (13 (tilde 5))
        (14 (tilde 6))
        (t
         ;; F1 to F12 (VTERM_KEY_FUNCTION_0 + n)
         (let ((n (- key 256)))
           (cond ((<= 1 n 4)
                  (escape-sequence (if (zerop mod) "O" (format nil "[1~A" modifier))
                                   (char "PQRS" (1- n))))
                 ((<= 5 n 12)
                  (tilde (nth (- n 5) '(15 17 18 19 20 21 23 24)))))))))))

(defun control-code (code)
  "The code that xterm sends for the character CODE with control."
  (cond ((<= #x61 code #x7a) (- code #x60))           ; a-z
        ((<= #x40 code #x5f) (- code #x40))           ; @ A-Z [ \ ] ^ _
        ((or (= code #x20) (= code #x32)) 0)          ; space 2
        ((<= #x33 code #x37) (- code #x18))           ; 3-7: ESC to US
        ((or (= code #x3f) (= code #x38)) #x7f)       ; ? 8
        ((= code #x2f) #x1f)                          ; /
        (t code)))

(defun character-sequence (character mod)
  "What xterm sends for CHARACTER with MOD (control and meta)."
  (let* ((code (char-code character))
         (string (string (code-char (if (logtest mod 4) (control-code code) code)))))
    (if (logtest mod 2) (escape-sequence string) string)))

(defmethod input-character ((terminal terminal) character &key (mod 0))
  (when (live-p terminal)
    (write-to-process terminal (character-sequence character mod))))

(defmethod input-key ((terminal terminal) key &key (mod 0))
  (when (live-p terminal)
    (alexandria:when-let ((sequence (key-sequence key mod
                                                  (xterm-mode-p terminal "applicationCursorKeysMode"))))
      (write-to-process terminal sequence))))

(defun paste-text (string)
  "STRING with its line breaks as a terminal sends them when pasting: CR."
  (with-output-to-string (out)
    (loop :with previous := nil
          :for char :across string
          :do (cond ((char= char #\Newline)
                     (unless (eql previous #\Return)
                       (write-char #\Return out)))
                    (t (write-char char out)))
              (setf previous char))))

(defmethod paste-string ((terminal terminal) string)
  "Bracketed by ESC[200~ and ESC[201~ when the program has turned on
bracketed paste."
  (when (live-p terminal)
    (let ((text (paste-text string)))
      (write-to-process terminal
                        (if (xterm-mode-p terminal "bracketedPasteMode")
                            (concatenate 'string (escape-sequence "[200~") text (escape-sequence "[201~"))
                            text)))))

(defmethod resize ((terminal terminal)
                   &key (rows (alexandria:required-argument :rows))
                        (cols (alexandria:required-argument :cols)))
  (when (and (live-p terminal)
             (plusp rows) (plusp cols)
             (not (and (= rows (terminal-rows terminal))
                       (= cols (terminal-cols terminal)))))
    (setf (terminal-rows terminal) rows
          (terminal-cols terminal) cols
          (terminal-redraw-all terminal) t)
    (js:call (terminal-xterm terminal) "resize" cols rows)
    (unless (terminal-closed terminal)
      (ignore-errors (js:call (terminal-pty terminal) "resize" cols rows)))
    (arm-render-timer terminal)))

;;; ---- the title and the bell

(defun title-buffer-name (title)
  (if (string= title "")
      "*Terminal*"
      (format nil "*Terminal: ~A*" title)))

(defun apply-title (terminal title)
  "Rename the terminal's buffer after the title its program set."
  (let ((buffer (terminal-buffer terminal)))
    (when (and (not (terminal-dead terminal))
               (stringp title)
               (variable-value 'terminal-title-as-buffer-name :global)
               (not (deleted-buffer-p buffer)))
      (let ((name (title-buffer-name title)))
        (unless (unique-name-of-p (buffer-name buffer) name)
          (buffer-rename buffer (unique-buffer-name name)))))))

(defun unique-name-of-p (buffer-name name)
  "Whether BUFFER-NAME is NAME or one that UNIQUE-BUFFER-NAME makes of it,
NAME<n> (then the buffer keeps it, instead of going from <1> to <2> and back)."
  (or (string= buffer-name name)
      (let ((end (length name)))
        (and (> (length buffer-name) (+ end 2))
             (string= name buffer-name :end2 end)
             (char= (char buffer-name end) #\<)
             (char= (char buffer-name (1- (length buffer-name))) #\>)
             (every #'digit-char-p (subseq buffer-name (1+ end) (1- (length buffer-name))))))))

(defun notify-bell (terminal)
  "Tell the user of a bell when the terminal is not on the screen (a bell in
a terminal that is shown is left silent)."
  (let ((buffer (terminal-buffer terminal))
        (now (get-universal-time)))
    (when (and (not (terminal-dead terminal))
               (not (deleted-buffer-p buffer))
               (null (get-buffer-windows buffer))
               (or (null (terminal-last-bell-time terminal))
                   (>= (- now (terminal-last-bell-time terminal)) *bell-interval-seconds*)))
      (setf (terminal-last-bell-time terminal) now)
      (message "Bell in ~A" (buffer-name buffer)))))
