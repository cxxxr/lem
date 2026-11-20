;;;; terminal.lisp — M-x terminal in the desktop application
;;;;
;;;; Lem's terminal (extensions/terminal/) runs a shell in a pseudo-terminal
;;;; and keeps its screen with libvterm, a C library loaded with CFFI, which a
;;;; page does not have.  Here the same two parts are JavaScript, in the page
;;;; of the desktop application: node-pty (the pseudo-terminal, Node.js's
;;;; native module that VS Code uses) and @xterm/headless (xterm.js's terminal
;;;; without its display), both installed with Electron (desktop/package.json).
;;;; This file defines what Lem's terminal-mode.lisp uses of the two packages
;;;; it is written against — the key constants of LEM-TERMINAL/FFI and the
;;;; terminal of LEM-TERMINAL/TERMINAL — and lem-clysm-terminal.asd loads
;;;; terminal-mode.lisp itself after it, so the commands, keys and modes (copy
;;;; mode, C-q, the keys that go to Lem) are Lem's.
;;;;
;;;; The terminal's output is parsed by xterm.js as it comes (in the page's
;;;; one thread, between the editor's events); when it has changed, the
;;;; editor renders it at most every TERMINAL-RENDER-THROTTLE-MS: it asks
;;;; xterm.js for the rows that changed since the last time, each a string
;;;; and runs of attributes, and puts them in the lines of the buffer.

(defpackage :lem-terminal/ffi
  (:use :cl)
  (:documentation "The key and modifier codes of libvterm that terminal-mode.lisp
sends (here they are encoded as xterm does, in JavaScript)."))
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
           :terminal-render-throttle-ms))
(in-package :lem-terminal/terminal)

(defvar *terminals* '()
  "The terminals, the most recent first.")

(defparameter *scrollback-limit* 5000
  "The number of lines that xterm.js keeps above the screen.")

(defparameter *bell-interval-seconds* 1
  "Minimum seconds between two bell notices from the same terminal.")

(defparameter *attribute-cache-limit* 4096
  "Maximum entries in a terminal's attribute cache (then it is cleared).")

(define-editor-variable terminal-render-throttle-ms 33
  "Minimum milliseconds between two renders of a terminal's screen.")

(define-editor-variable terminal-title-as-buffer-name t
  "When non-NIL, a terminal buffer is renamed after the window title its
program sets (e.g. bash's \"user@host: ~/dir\"), as \"*Terminal: TITLE*\".")

;;; ---- the pseudo-terminal and the screen (JavaScript)

(sys::%defvar-host *terminal-spawn*
  (js:eval "(directory, program, args, rows, cols, scrollback, onOutput, onExit, onTitle, onBell) => {
     const pty = require('node-pty');
     const { Terminal } = require('@xterm/headless');
     const fs = require('node:fs'), os = require('node:os');
     const term = new Terminal({ rows, cols, scrollback, allowProposedApi: true });
     const exists = (d) => { try { return fs.statSync(d).isDirectory(); } catch { return false; } };
     const file = program || process.env.SHELL || '/bin/sh';
     const env = { ...process.env, TERM: 'xterm-256color', COLORTERM: 'truecolor' };
     for (const name of Object.keys(env)) if (name.startsWith('ELECTRON_') || name.startsWith('CLYSM_')) delete env[name];
     const child = pty.spawn(file, args || [], {
       name: 'xterm-256color', rows, cols, env,
       cwd: directory && exists(directory) ? directory : os.homedir() });
     // the editor is told once that the screen changed, until it reads it
     let notified = false, ended = false;
     child.onData((data) => {
       if (ended) return;
       term.write(data, () => { if (!notified && !ended) { notified = true; onOutput(); } });
     });
     child.onExit(({ exitCode, signal }) => {
       if (ended) return;
       // after what was written before it is parsed
       term.write('', () => { if (!ended) { ended = true; onExit(signal ? 128 + signal : exitCode); } });
     });
     term.onTitleChange((title) => { if (!ended) onTitle(title); });
     term.onBell(() => { if (!ended) onBell(); });

     // 16 colours of xterm.js's own theme; then the cube and the greys of xterm
     const palette = ['2e3436','cc0000','4e9a06','c4a000','3465a4','75507b','06989a','d3d7cf',
                      '555753','ef2929','8ae234','fce94f','729fcf','ad7fa8','34e2e2','eeeeec'];
     const hex = (n) => n.toString(16).padStart(6, '0');
     for (let i = 0; i < 216; i++) {
       const level = (v) => v ? 55 + v * 40 : 0;
       palette.push(hex((level(Math.floor(i / 36)) << 16) | (level(Math.floor(i / 6) % 6) << 8) | level(i % 6)));
     }
     for (let i = 0; i < 24; i++) { const v = 8 + i * 10; palette.push(hex((v << 16) | (v << 8) | v)); }
     const fg = (c) => c.isFgDefault() ? '' : c.isFgRGB() ? hex(c.getFgColor()) : palette[c.getFgColor()] || '';
     const bg = (c) => c.isBgDefault() ? '' : c.isBgRGB() ? hex(c.getBgColor()) : palette[c.getBgColor()] || '';

     // a row: its text, one character per glyph (the second cell of a wide
     // character is skipped), without the blanks that pad it, and runs
     // (start end key) of attributes, in characters; a key is FG/BG/FLAGS
     const row = (line, cols, cell) => {
       const glyphs = [];
       let end = 0;
       for (let x = 0; x < cols; x++) {
         line.getCell(x, cell);
         if (cell.getWidth() === 0) continue;
         const chars = cell.getChars() || ' ';
         const flags = (cell.isBold() ? 1 : 0) | (cell.isUnderline() ? 2 : 0) | (cell.isInverse() ? 4 : 0);
         const b = bg(cell);
         glyphs.push([chars, fg(cell) + '/' + b + '/' + flags]);
         if (chars !== ' ' || b !== '' || (flags & 6)) end = glyphs.length;
       }
       let text = '', index = 0, start = 0, key = null;
       const runs = [];
       for (let i = 0; i < end; i++) {
         const [chars, k] = glyphs[i];
         if (k !== key) {
           if (key !== null && key !== '//0') runs.push(start, index, key);
           start = index; key = k;
         }
         text += chars;
         index += [...chars].length;
       }
       if (key !== null && key !== '//0') runs.push(start, index, key);
       return [text, runs];
     };
     let previous = [];
     return {
       // [cursor-row cursor-col {row text runs}...]: the rows that changed
       // since the last snapshot, or all of them
       snapshot: (all) => {
         notified = false;
         const buffer = term.buffer.active, rows = term.rows, cols = term.cols, out = [buffer.cursorY, buffer.cursorX];
         const cell = buffer.getNullCell();
         if (all || previous.length !== rows) previous = new Array(rows).fill(null);
         for (let y = 0; y < rows; y++) {
           const line = buffer.getLine(buffer.baseY + y);
           const [text, runs] = line ? row(line, cols, cell) : ['', []];
           const id = text + '\\0' + runs.join(',');
           if (previous[y] !== id) { previous[y] = id; out.push(y, text, runs); }
         }
         return out;
       },
       cursor: () => [term.buffer.active.cursorY, term.buffer.active.cursorX],
       scrollbackCount: () => term.buffer.active.baseY,
       scrollback: () => {
         const buffer = term.buffer.active, lines = [];
         for (let y = 0; y < buffer.baseY; y++) lines.push(buffer.getLine(y).translateToString(true) + '\\n');
         return lines.join('');
       },
       cwd: () => { try { return fs.readlinkSync('/proc/' + child.pid + '/cwd'); } catch { return null; } },
       write: (text) => { if (!ended) child.write(text); },
       paste: (text) => {
         if (ended) return;
         child.write(term.modes.bracketedPasteMode ? '\\x1b[200~' + text + '\\x1b[201~' : text);
       },
       // what xterm sends for a key of libvterm (VTERM_KEY_...) with VTERM_MOD_... (shift 1, alt 2, ctrl 4)
       key: (key, mod) => {
         if (ended) return;
         const app = term.modes.applicationCursorKeysMode, m = mod ? ';' + (1 + mod) : '';
         const esc = (s) => (mod & 2 ? '\\x1b' : '') + s;
         const cursor = (c) => mod ? '\\x1b[1' + m + c : (app ? '\\x1bO' : '\\x1b[') + c;
         const tilde = (n) => '\\x1b[' + n + m + '~';
         const sequences = {
           1: () => esc('\\r'), 2: () => mod & 1 ? '\\x1b[Z' : esc('\\t'),
           3: () => esc(mod & 4 ? '\\x08' : '\\x7f'), 4: () => '\\x1b',
           5: () => cursor('A'), 6: () => cursor('B'), 7: () => cursor('D'), 8: () => cursor('C'),
           9: () => tilde(2), 10: () => tilde(3), 11: () => cursor('H'), 12: () => cursor('F'),
           13: () => tilde(5), 14: () => tilde(6) };
         let s = sequences[key] && sequences[key]();
         if (!s && key > 256 && key <= 268) {
           const n = key - 256;
           s = n <= 4 ? (mod ? '\\x1b[1' + m : '\\x1bO') + 'PQRS'[n - 1]
                      : tilde([15, 17, 18, 19, 20, 21, 23, 24][n - 5]);
         }
         if (s) child.write(s);
       },
       // a character, with control and meta as xterm sends them
       char: (code, mod) => {
         if (ended) return;
         if (mod & 4) {
           if (code >= 0x61 && code <= 0x7a) code -= 0x60;
           else if (code >= 0x40 && code <= 0x5f) code -= 0x40;
           else if (code === 0x20 || code === 0x32) code = 0;
           else if (code >= 0x33 && code <= 0x37) code -= 0x18;
           else if (code === 0x3f || code === 0x38) code = 0x7f;
           else if (code === 0x2f) code = 0x1f;
         }
         child.write((mod & 2 ? '\\x1b' : '') + String.fromCodePoint(code));
       },
       resize: (rows, cols) => {
         term.resize(cols, rows);
         if (!ended) { try { child.resize(cols, rows); } catch {} }
       },
       kill: () => {
         if (!ended) { ended = true; try { child.kill(); } catch {} }
         term.dispose();
       },
     };
   }"))

;;; ---- a terminal

(defclass terminal ()
  ((handle :initarg :handle
           :accessor terminal-handle
           :documentation "The pseudo-terminal and its screen (JavaScript).")
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
   (attribute-cache :initform (make-hash-table :test #'equal)
                    :accessor terminal-attribute-cache
                    :documentation "Attributes by the keys of xterm.js's runs.")
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

(defun create (&key (rows (alexandria:required-argument :rows))
                    (cols (alexandria:required-argument :cols))
                    (buffer (alexandria:required-argument :buffer))
                    (directory (alexandria:required-argument :directory))
                    (program nil)
                    (argv nil))
  "Start PROGRAM with ARGV (its first element is the program's name), or the
user's shell in DIRECTORY, in a pseudo-terminal of ROWS and COLS whose screen
is rendered into BUFFER."
  (let ((terminal (make-instance 'terminal :buffer buffer :rows rows :cols cols)))
    (setf (terminal-handle terminal)
          (handler-case
              (js:funcall *terminal-spawn*
                          (and (plusp (length directory))
                               (uiop:native-namestring (uiop:ensure-directory-pathname directory)))
                          program (rest argv) rows cols *scrollback-limit*
                          (lambda ()
                            (send-event (lambda () (arm-render-timer terminal)))
                            nil)
                          (lambda (status)
                            (send-event (lambda () (finish-process terminal status)))
                            nil)
                          (lambda (title)
                            (send-event (lambda () (apply-title terminal title)))
                            nil)
                          (lambda ()
                            (send-event (lambda () (notify-bell terminal)))
                            nil))
            (js:js-error (e)
              (editor-error "Cannot start the terminal: ~A~@[ (~A)~]"
                            (js:js-error-message e)
                            (and (not (js:funcall (js:eval "() => typeof require === 'function'")))
                                 "the terminal runs in the desktop application")))))
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
terminal and append an exit notice."
  (when (and (live-p terminal)
             (not (deleted-buffer-p (terminal-buffer terminal))))
    (unless (terminal-copy-mode terminal)
      (render terminal))
    (setf (terminal-exit-status terminal) status)
    (append-exit-notice terminal status)
    (redraw-display)))

(defgeneric destroy (terminal)
  (:documentation "Kill TERMINAL's process if it still runs.  Safe to call more than once."))

(defmethod destroy ((terminal terminal))
  (unless (terminal-dead terminal)
    (setf (terminal-dead terminal) t)
    (alexandria:when-let ((timer (terminal-render-timer terminal)))
      (ignore-errors (stop-timer timer)))
    (alexandria:deletef *terminals* terminal)
    (js:call (terminal-handle terminal) "kill")))

(defgeneric copy-mode-on (terminal))
(defgeneric copy-mode-off (terminal))

(defmethod copy-mode-on ((terminal terminal))
  "Freeze rendering and show the scrollback above the screen."
  (setf (terminal-copy-mode terminal) t)
  (activate-scrollback terminal))

(defmethod copy-mode-off ((terminal terminal))
  (setf (terminal-copy-mode terminal) nil)
  (deactivate-scrollback terminal)
  (setf (terminal-redraw-all terminal) t)
  (arm-render-timer terminal))

(defun process-directory (terminal)
  "The current directory of TERMINAL's process (the shell) as a directory
namestring, or NIL when it is unknown (it is known on Linux)."
  (when (live-p terminal)
    (let ((cwd (js:call (terminal-handle terminal) "cwd")))
      (when (stringp cwd)
        (uiop:native-namestring (uiop:ensure-directory-pathname
                                 (uiop:parse-native-namestring cwd)))))))

;;; ---- rendering

(defun parse-color-part (string)
  (and (plusp (length string))
       (parse-color (concatenate 'string "#" string))))

(defun run-attribute (terminal key)
  "The attribute of KEY, \"FG/BG/FLAGS\" (FLAGS: 1 bold, 2 underline, 4 reverse)."
  (let ((cache (terminal-attribute-cache terminal)))
    (or (gethash key cache)
        (let* ((slash1 (position #\/ key))
               (slash2 (position #\/ key :start (1+ slash1)))
               (flags (parse-integer key :start (1+ slash2))))
          (when (>= (hash-table-count cache) *attribute-cache-limit*)
            (clrhash cache))
          (setf (gethash key cache)
                (make-attribute :foreground (parse-color-part (subseq key 0 slash1))
                                :background (parse-color-part (subseq key (1+ slash1) slash2))
                                :bold (logtest flags 1)
                                :underline (logtest flags 2)
                                :reverse (logtest flags 4)))))))

(defun set-row (terminal line text runs)
  "Put TEXT and its RUNS (start end key...) of attributes in LINE.  This sets
the line directly, as Lem's terminal does: the terminal buffer is only
rendered into (no undo, no change hooks)."
  (line:set-line-string text line)
  (setf (getf (line:line-plist line) :attribute)
        (loop :for i :from 0 :below (or (js:get runs "length") 0) :by 3
              :collect (list (js:get runs i)
                             (js:get runs (+ i 1))
                             (run-attribute terminal (js:get runs (+ i 2)))))))

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

(defgeneric render (terminal))

(defmethod render ((terminal terminal))
  (let* ((buffer (terminal-buffer terminal))
         (point (buffer-point buffer))
         (rows (terminal-rows terminal))
         (snapshot (js:call (terminal-handle terminal) "snapshot"
                            (if (terminal-redraw-all terminal) t :false)))
         (length (js:get snapshot "length")))
    (setf (terminal-redraw-all terminal) nil)
    (deactivate-scrollback terminal)
    (with-buffer-read-only buffer nil
      (fit-lines buffer rows)
      (move-to-line point 1)
      (let ((lines (make-array rows)))
        (loop :for i :from 0 :below rows
              :for line := (point-line point) :then (line:line-next line)
              :do (setf (aref lines i) line))
        (loop :for i :from 2 :below length :by 3
              :for row := (js:get snapshot i)
              :when (< row rows)
                :do (set-row terminal (aref lines row)
                             (js:get snapshot (+ i 1))
                             (js:get snapshot (+ i 2)))))
      (move-to-cursor point (js:get snapshot 0) (js:get snapshot 1)))))

(defgeneric adjust-point (terminal))

(defmethod adjust-point ((terminal terminal))
  (unless (terminal-dead terminal)
    (let ((cursor (js:call (terminal-handle terminal) "cursor"))
          (buffer (terminal-buffer terminal)))
      (with-buffer-read-only buffer nil
        (move-to-cursor (buffer-point buffer) (js:get cursor 0) (js:get cursor 1))))))

;;; ---- the lines above the screen

(defun scrollback-count (terminal)
  "The number of lines above the screen."
  (if (terminal-dead terminal)
      0
      (js:call (terminal-handle terminal) "scrollbackCount")))

(defun buffer-view-points (buffer)
  (cons (buffer-point buffer)
        (mapcar #'window-view-point (get-buffer-windows buffer))))

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
  "Remove the lines above the screen from the buffer."
  (when (terminal-scrollback-active terminal)
    (setf (terminal-scrollback-active terminal) nil)
    (let* ((buffer (terminal-buffer terminal))
           (rows (terminal-rows terminal))
           (nlines (buffer-nlines buffer)))
      (when (> nlines rows)
        (with-buffer-read-only buffer nil
          (with-point ((start (buffer-point buffer))
                       (end (buffer-point buffer)))
            (buffer-start start)
            (move-to-line end (1+ (- nlines rows)))
            (line-start end)
            (delete-between-points start end)))))))

(defun activate-scrollback (terminal)
  "Insert the lines above the screen into the buffer, keeping the cursor and
the windows on what they showed."
  (unless (or (terminal-scrollback-active terminal)
              (terminal-dead terminal))
    (let ((text (js:call (terminal-handle terminal) "scrollback")))
      (when (plusp (length text))
        (setf (terminal-scrollback-active terminal) t)
        (let* ((buffer (terminal-buffer terminal))
               (points (buffer-view-points buffer))
               (positions (point-positions points)))
          (with-buffer-read-only buffer nil
            (with-point ((start (buffer-point buffer)))
              (buffer-start start)
              (insert-string start text)))
          (shift-points-by-lines points positions (count #\Newline text)))))))

(defun snap-to-bottom (terminal)
  "Show the screen in every window of the terminal's buffer."
  (dolist (window (get-buffer-windows (terminal-buffer terminal)))
    (buffer-start (window-view-point window))))

;;; ---- input

(defgeneric input-character (terminal character &key mod))
(defgeneric input-key (terminal key &key mod))
(defgeneric paste-string (terminal string))
(defgeneric resize (terminal &key rows cols))

(defmethod input-character ((terminal terminal) character &key (mod 0))
  (when (live-p terminal)
    (js:call (terminal-handle terminal) "char" (char-code character) mod)))

(defmethod input-key ((terminal terminal) key &key (mod 0))
  (when (live-p terminal)
    (js:call (terminal-handle terminal) "key" key mod)))

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
  "Send STRING as pasted text (between ESC[200~ and ESC[201~ when the program
has turned on bracketed paste)."
  (when (live-p terminal)
    (js:call (terminal-handle terminal) "paste" (paste-text string))))

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
    (js:call (terminal-handle terminal) "resize" rows cols)
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
        (unless (string= name (buffer-name buffer))
          (buffer-rename buffer (unique-buffer-name name)))))))

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
