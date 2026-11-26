(uiop:define-package :lem-terminal/terminal
  (:use :cl :lem)
  (:local-nicknames (:ffi :lem-terminal/ffi)
                    (:queue :lem/common/queue)
                    (:line :lem/buffer/line))
  (:export :find-terminal-buffer
           :create
           :destroy
           :copy-mode-on
           :copy-mode-off
           :process-exited-p
           :process-directory
           :clear
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

;;; --- module-level state and tunables ---------------------------------
;;; Kept at the top of the file per the contract's file_structure_rule.

(defvar *terminal-id-counter* 0
  "Monotonic counter for `generate-terminal-id'.")

(defvar *terminals* '()
  "All live terminals, used by callbacks to locate their Lisp object by id.")

(defparameter *scrollback-limit* 5000
  "Maximum number of scrollback lines stored in the ring buffer.")

(defparameter *write-timeout-ms* 12
  "Max milliseconds the I/O thread spends parsing PTY data per slice
(xterm.js WriteBuffer pattern).  Parsing now runs on the terminal's own
I/O thread while holding `terminal-io-lock', so this bounds how long the
lock is held — i.e. the worst-case wait a render or keypress can see —
rather than blocking the editor's command loop directly.")

(define-editor-variable terminal-render-throttle-ms 33
  "Minimum milliseconds between terminal renders (see ARM-RENDER-TIMER).
Rendering is self-paced: the next render is scheduled only after the previous
one fully completes plus this interval, so the editor is never driven faster
than it — or a slow frontend such as the webview's websocket — can flush.
This caps render frequency without losing the final frame; idle terminals
stop ticking entirely.

Set it from your init file, e.g.
  (setf (variable-value 'lem-terminal/terminal:terminal-render-throttle-ms :global) 50)
Higher values favour editor responsiveness under heavy output on slow
frontends; lower values give smoother terminal output on fast frontends.
Read with :global scope because it is consulted from the terminal I/O and
render-timer threads, which have no current-buffer context.")

(defparameter *attribute-cache-limit* 4096
  "Maximum entries in a terminal's attribute cache.  When exceeded the cache
is cleared wholesale — cheaper than maintaining LRU bookkeeping on the
render hot path, and the cache refills quickly for the active palette.")

(defparameter *fixed-blue* (parse-color "#3465A4")
  "Replacement colour used by `fix-blue-color' for vterm's unreadable default blue.")

(defparameter *blue-to-fix* (make-color 0 0 #xe0)
  "vterm's default \"blue\" — too dark on most terminals; replaced with `*fixed-blue*'.")

;; Byte offsets into the packed cell-data struct (16 bytes per cell):
;; uint32 ch (0), uint8 fg_r(4), fg_g(5), fg_b(6), bg_r(7), bg_g(8), bg_b(9),
;; bold(10), underline(11), reverse(12), width(13), default_fg(14),
;; default_bg(15)
(defconstant +cell-off-ch+ 0)
(defconstant +cell-off-fg-r+ 4)
(defconstant +cell-off-fg-g+ 5)
(defconstant +cell-off-fg-b+ 6)
(defconstant +cell-off-bg-r+ 7)
(defconstant +cell-off-bg-g+ 8)
(defconstant +cell-off-bg-b+ 9)
(defconstant +cell-off-bold+ 10)
(defconstant +cell-off-underline+ 11)
(defconstant +cell-off-reverse+ 12)
(defconstant +cell-off-width+ 13)
(defconstant +cell-off-default-fg+ 14)
(defconstant +cell-off-default-bg+ 15)

;;; --- id generator ----------------------------------------------------

(defun generate-terminal-id ()
  (incf *terminal-id-counter*))

;;; --- terminals registry ----------------------------------------------

(defun add-terminal (terminal)
  (push terminal *terminals*))

(defun find-terminal-by-id (id)
  (find id *terminals* :key #'terminal-id))

(defun remove-terminal (terminal)
  (alexandria:deletef *terminals* terminal))

(defun find-terminal-buffer ()
  "Return the buffer of the most recently created terminal whose process is
still running, or NIL.  Exited terminals are skipped so that `terminal'
opens a fresh shell instead of reusing a dead one."
  (alexandria:when-let (terminal (find-if-not #'process-exited-p *terminals*))
    (terminal-buffer terminal)))

;;; terminal
(defclass terminal ()
  ((id :initarg :id
       :reader terminal-id)
   (viscus :initarg :viscus
           :reader terminal-viscus)
   (thread :initarg :thread
           :accessor terminal-thread)
   (buffer :initarg :buffer
           :reader terminal-buffer)
   (rows :initarg :rows
         :accessor terminal-rows)
   (cols :initarg :cols
         :accessor terminal-cols)
   (copy-mode :initform nil
              :accessor terminal-copy-mode)
   (dirty-rows :initform nil
               :accessor terminal-dirty-rows
               :documentation "Bit vector tracking which rows need re-rendering.")
   (dirty-any-p :initform nil
                :accessor terminal-dirty-any-p
                :documentation "Fast-path flag: T if any bit in dirty-rows is set.
Avoids an O(rows) scan in the hot update loop.")
   (scrollback-ring :initform nil
                    :accessor terminal-scrollback-ring
                    :documentation "Simple-vector ring buffer of scrollback line strings.")
   (scrollback-head :initform 0
                    :accessor terminal-scrollback-head
                    :type fixnum
                    :documentation "Write position in the scrollback ring (next slot to fill).")
   (scrollback-count :initform 0
                     :accessor scrollback-count
                     :type fixnum
                     :documentation "Number of valid entries in the scrollback ring.")
   (scrollback-active :initform nil
                      :accessor terminal-scrollback-active
                      :documentation "T when scrollback lines are displayed in the buffer.")
   (attribute-cache :initform (make-hash-table :test #'eql)
                    :accessor terminal-attribute-cache
                    :documentation "Cache of packed-fixnum-key -> attribute.")
   (io-lock :initform (bt2:make-lock :name "terminal-io")
            :reader terminal-io-lock
            :documentation "Serializes all access to vterm state.  The I/O
thread holds it while parsing PTY data (vterm-input-write and its damage
callbacks); the editor thread holds it while rendering, sending input,
resizing, reading the cursor, or reading the scrollback ring.  libvterm is
not thread-safe, so parse and these operations must never overlap.")
   (render-lock :initform (bt2:make-lock :name "terminal-render")
                :reader terminal-render-lock
                :documentation "Guards RENDER-TIMER-ARMED.  Distinct from
IO-LOCK and never held while acquiring it.")
   (render-timer :initform nil
                 :accessor terminal-render-timer
                 :documentation "One-shot timer that drives rendering.  All
rendering is paced through it: it fires on the editor thread, renders, then
re-arms only if more output arrived — so the editor is never asked to render
faster than it (and a slow frontend like the websocket) can keep up.")
   (render-timer-armed :initform nil
                       :accessor terminal-render-timer-armed
                       :type boolean
                       :documentation "T while a render tick is pending;
coalesces wakeups into one outstanding tick.")
   (dead :initform nil
         :accessor terminal-dead
         :type boolean
         :documentation "T once DESTROY has run.  Guards the I/O thread and
queued render events from touching vterm state after it is freed.")
   (exit-status :initform nil
                :accessor terminal-exit-status
                :documentation "NIL while the child process runs.  Set on the
editor thread by FINISH-PROCESS once the child has exited: its exit code,
128+signal when killed by a signal, or -1 when unknown.  Once set, the
terminal no longer renders, resizes or accepts input, so the final screen
and the exit notice stay in the buffer.")))

(defun process-exited-p (terminal)
  "Return T when TERMINAL's child process has exited."
  (not (null (terminal-exit-status terminal))))

(defun process-directory (terminal)
  "Return the current directory of TERMINAL's child process (the shell) as a
directory namestring, or NIL when it is unknown: the process has exited, or
the OS has no /proc/PID/cwd (only Linux is supported for now)."
  (unless (or (terminal-dead terminal)
              (process-exited-p terminal))
    (let ((pid (ffi::terminal-pid (terminal-viscus terminal))))
      (when (plusp pid)
        (alexandria:when-let ((directory (ignore-errors
                                          (truename (format nil "/proc/~D/cwd/" pid)))))
          (namestring directory))))))

(defun mark-all-rows-dirty (terminal)
  "Mark every row as dirty so the next render redraws the full screen."
  (let ((dirty (terminal-dirty-rows terminal))
        (rows (terminal-rows terminal)))
    (if (and dirty (= (length dirty) rows))
        (fill dirty 1)
        (setf (terminal-dirty-rows terminal)
              (make-array rows :element-type 'bit :initial-element 1))))
  (setf (terminal-dirty-any-p terminal) t))

(defun mark-rows-dirty (terminal start-row end-row)
  "Mark rows from START-ROW (inclusive) to END-ROW (exclusive) as dirty."
  (let ((dirty (terminal-dirty-rows terminal)))
    (when dirty
      (loop :for r :from (max 0 start-row) :below (min end-row (length dirty))
            :do (setf (aref dirty r) 1))
      (setf (terminal-dirty-any-p terminal) t))))

(defun any-rows-dirty-p (terminal)
  "Return T if any row is marked dirty.  O(1) — reads the cached flag."
  (terminal-dirty-any-p terminal))

(defun arm-render-timer (terminal)
  "Ensure a render tick is scheduled (one outstanding at a time).
Safe to call from any thread.  Renders are paced entirely through this
one-shot timer: the editor is never asked to render again until the previous
tick has fully completed (including its blocking websocket flush) plus
TERMINAL-RENDER-THROTTLE-MS.  That self-pacing is what prevents heavy output from
flooding a slow frontend and stalling the editor."
  (when (and (not (terminal-dead terminal))
             (not (process-exited-p terminal))
             (not (terminal-copy-mode terminal))
             (any-rows-dirty-p terminal))
    (let ((arm nil))
      (bt2:with-lock-held ((terminal-render-lock terminal))
        (unless (terminal-render-timer-armed terminal)
          (setf (terminal-render-timer-armed terminal) t
                arm t)))
      (when arm
        (let ((timer (or (terminal-render-timer terminal)
                         (setf (terminal-render-timer terminal)
                               (make-timer (lambda () (render-tick terminal))
                                           :name "terminal-render")))))
          (start-timer timer (variable-value 'terminal-render-throttle-ms :global)))))))

(defun render-tick (terminal)
  "Editor-thread render, driven by ARM-RENDER-TIMER.  Reads vterm state under
IO-LOCK and writes the buffer; REDRAW-DISPLAY is invoked automatically by the
timer manager after this returns.  Re-arms only if more output arrived during
the render, so a settled terminal stops ticking (no idle flush)."
  (bt2:with-lock-held ((terminal-render-lock terminal))
    (setf (terminal-render-timer-armed terminal) nil))
  (when (and (not (terminal-dead terminal))
             (not (process-exited-p terminal))
             (not (terminal-copy-mode terminal))
             (any-rows-dirty-p terminal))
    (bt2:with-lock-held ((terminal-io-lock terminal))
      (render terminal)))
  ;; Output parsed during the render (or while paused) re-arms the next tick.
  (arm-render-timer terminal))

(defun update (terminal)
  "Synchronously drain the PTY and render now.  The live terminal renders
asynchronously (the I/O thread parses and calls ARM-RENDER-TIMER); this entry
point is kept for manual/REPL use and tests."
  (bt2:with-lock-held ((terminal-io-lock terminal))
    (process-input terminal)
    (render terminal))
  (redraw-display))

(defun create (&key (rows (alexandria:required-argument :rows))
                    (cols (alexandria:required-argument :cols))
                    (buffer (alexandria:required-argument :buffer))
                    (directory (alexandria:required-argument :directory))
                    (program nil)
                    (argv nil))
  (declare (type (string) directory)
           (type (integer) rows)
           (type (integer) cols))
  (let* ((id (generate-terminal-id))
         (terminal
           (make-instance 'terminal
                          :id id
                          :viscus (ffi::terminal-new directory id rows cols
                                                     :program program
                                                     :argv argv)
                          :buffer buffer
                          :rows rows
                          :cols cols)))
    (setf (terminal-scrollback-ring terminal)
          (make-array *scrollback-limit* :initial-element nil))
    (mark-all-rows-dirty terminal)
    ;; The I/O thread owns PTY parsing: it blocks in select (process-input-wait),
    ;; then drains and parses a budgeted slice into vterm under IO-LOCK, then asks
    ;; the editor thread to render via ARM-RENDER-TIMER.  Because this thread now
    ;; consumes the PTY itself, select no longer returns immediately on unread
    ;; data — there is no hot-spin and no need for a throttling sleep.  The editor
    ;; thread only renders (self-paced via the render timer), so heavy output can't
    ;; monopolise the command loop or outrun a slow frontend.
    (setf (terminal-thread terminal)
          (bt2:make-thread (lambda () (io-loop terminal))
                           :name (format nil "Terminal ~D" id)))
    (add-terminal terminal)
    terminal))

(defun io-loop (terminal)
  "Body of the terminal's I/O thread.  Blocks until the PTY has output, parses
it into vterm under IO-LOCK and schedules a render.  Returns when DESTROY wakes
it up, or when the PTY hangs up because the child process exited; in the
latter case the exit is reported to the editor thread via FINISH-PROCESS."
  (let ((viscus (terminal-viscus terminal)))
    (loop
      (let ((readable (= 1 (ffi::terminal-process-input-wait viscus))))
        (when (terminal-dead terminal)
          (return))
        (when readable
          (bt2:with-lock-held ((terminal-io-lock terminal))
            (unless (terminal-dead terminal)
              (process-input terminal))))
        (arm-render-timer terminal)
        ;; EOF is recorded by process-input on this thread, so no lock needed.
        (when (ffi::terminal-eof-p viscus)
          (let ((status (ffi::terminal-collect-exit-status viscus)))
            (send-event (lambda () (finish-process terminal status))))
          (return))
        (unless readable
          ;; Woken without DESTROY and without EOF: select failed.  Stop
          ;; rather than spin; DESTROY still cleans up.
          (return))))))

(defun exit-notice (status)
  (if (minusp status)
      "[Process exited]"
      (format nil "[Process exited with code ~D]" status)))

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
  "Editor-thread handler for the child process exiting.  Flushes the final
output, freezes the terminal (see EXIT-STATUS) and appends an exit notice."
  (unless (or (terminal-dead terminal)
              (process-exited-p terminal))
    ;; The I/O thread has stopped, so nothing else touches vterm now; the lock
    ;; is taken only to honour RENDER's contract.
    (bt2:with-lock-held ((terminal-io-lock terminal))
      (render terminal))
    (setf (terminal-exit-status terminal) status)
    (append-exit-notice terminal status)
    (redraw-display)))

(defmethod destroy ((terminal terminal))
  "Stop TERMINAL's I/O thread, kill its child process if it is still running
and free the native state.  Safe to call more than once."
  (unless (terminal-dead terminal)
    ;; Set DEAD while holding IO-LOCK: this waits out any in-flight parse and,
    ;; because the I/O loop re-checks DEAD under the same lock before parsing,
    ;; guarantees no parse can start afterward.
    (bt2:with-lock-held ((terminal-io-lock terminal))
      (setf (terminal-dead terminal) t))
    (unwind-protect
         (progn
           ;; Wake the I/O thread out of select and let it return on its own,
           ;; rather than killing it asynchronously (which could leave IO-LOCK
           ;; held, or error if the thread had already exited).
           (ffi::terminal-wake (terminal-viscus terminal))
           (let ((thread (terminal-thread terminal)))
             (when (and thread (not (eq thread (bt2:current-thread))))
               (ignore-errors (bt2:join-thread thread)))))
      (alexandria:when-let ((timer (terminal-render-timer terminal)))
        (ignore-errors (stop-timer timer)))
      (remove-terminal terminal)
      ;; Hangs up and reaps the child process, then frees vterm.
      (ffi::terminal-delete (terminal-viscus terminal)))))

(defmethod copy-mode-on ((terminal terminal))
  "Freeze rendering and show the scrollback above the screen, so the whole
history can be browsed and copied with ordinary Lem commands."
  (setf (terminal-copy-mode terminal) t)
  (activate-scrollback terminal))

(defmethod copy-mode-off ((terminal terminal))
  (setf (terminal-copy-mode terminal) nil)
  (deactivate-scrollback terminal)
  ;; Redraw the whole screen: output that arrived during copy mode was left
  ;; unrendered, and leaving copy mode may have left the buffer edited.
  (mark-all-rows-dirty terminal)
  (arm-render-timer terminal))

(defun fix-blue-color (color)
  (if (lem/common/color:color-equal color *blue-to-fix*)
      *fixed-blue*
      color))

(declaim (inline pack-attribute-key))
(defun pack-attribute-key (fg-r fg-g fg-b bg-r bg-g bg-b bold underline reverse-attr
                           default-fg default-bg)
  "Pack cell style into a single fixnum key (53 bits — fits in a 64-bit fixnum).
Attr flags are masked to one bit each: some vterm builds expose underline as
a multi-bit style enum, and an unmasked value would collide with the bg-b
field (bits 3..10) or with neighbouring flags."
  (logior (ash (logand default-bg 1) 52) (ash (logand default-fg 1) 51)
          (ash fg-r 43) (ash fg-g 35) (ash fg-b 27)
          (ash bg-r 19) (ash bg-g 11) (ash bg-b 3)
          (ash (logand bold 1) 2)
          (ash (logand underline 1) 1)
          (logand reverse-attr 1)))

(defun get-attribute-for-cell (terminal fg-r fg-g fg-b bg-r bg-g bg-b bold underline reverse-attr
                               default-fg default-bg)
  "Return a cached attribute for the given cell style components.  Cells in
the terminal's default colors get no color, so they follow the editor
theme (a light theme would otherwise show near-white text on white)."
  (let* ((key (pack-attribute-key fg-r fg-g fg-b bg-r bg-g bg-b bold underline reverse-attr
                                  default-fg default-bg))
         (cache (terminal-attribute-cache terminal)))
    (or (gethash key cache)
        (progn
          (when (>= (hash-table-count cache) *attribute-cache-limit*)
            (clrhash cache))
          (setf (gethash key cache)
                (make-attribute :foreground (if (= 1 default-fg)
                                                nil
                                                (fix-blue-color (make-color fg-r fg-g fg-b)))
                                :background (if (= 1 default-bg)
                                                nil
                                                (fix-blue-color (make-color bg-r bg-g bg-b)))
                                :reverse (= 1 (logand reverse-attr 1))
                                :bold (= 1 (logand bold 1))
                                :underline (= 1 (logand underline 1))))))))

;; Cached struct size — resolved once at load time so we never call
;; cffi:foreign-type-size on the render hot path and don't carry a
;; mutable lazy-init global.
(declaim (inline cell-data-size))
(defun cell-data-size ()
  (load-time-value (cffi:foreign-type-size '(:struct ffi::cell-data)) t))

(defun blank-attribute-p (attribute)
  "Return T when a space drawn with ATTRIBUTE is invisible: no background,
reverse video or underline."
  (not (or (attribute-background attribute)
           (attribute-reverse attribute)
           (attribute-underline attribute))))

(defun trim-trailing-blanks (chars attrs)
  "Drop the invisible spaces vterm pads every row with, so lines end where
their text ends: copying a line does not pick up the padding and C-e stops at
the text.  Spaces with a visible attribute (e.g. a coloured status bar) are
kept.  ATTRS are (start end attribute) runs in ascending order.  Returns the
trimmed CHARS and ATTRS."
  (let ((end (length chars)))
    (loop :for (start nil attribute) :in (reverse attrs)
          :while (blank-attribute-p attribute)
          :do (loop :while (and (> end start)
                                (char= #\Space (char chars (1- end))))
                    :do (decf end))
          :while (= end start))
    (if (= end (length chars))
        (values chars attrs)
        (values (subseq chars 0 end)
                (loop :for (start run-end attribute) :in attrs
                      :when (< start end)
                        :collect (list start (min run-end end) attribute))))))

(defun move-to-cursor (point row col)
  "Move POINT to screen ROW and COL, padding the line with spaces when the
cursor is past the end of its text (see TRIM-TRAILING-BLANKS)."
  (move-to-line point (1+ row))
  (move-to-column point col)
  (let ((missing (- col (point-column point))))
    (when (plusp missing)
      (insert-string point (make-string missing :initial-element #\Space)))))

(defun render-row-direct (terminal line-obj cols row buf cell-size)
  "Render a row directly into LINE-OBJ, bypassing the buffer edit API.

CONTRACT: this deliberately skips before/after-change hooks, undo
recording, and marker shifting.  It is safe ONLY for the terminal
buffer, whose content is owned exclusively by the vterm renderer:
nothing else writes to it, nothing observes its change hooks (the
terminal mode bypasses standard editing commands in EXECUTE), and
undo is irrelevant for a live shell.  Do not use this on any buffer
that may be edited by the user or watched by other features."
  (ffi::terminal-get-row (terminal-viscus terminal) row buf cols)
  ;; A wide character occupies two cells; vterm leaves the second one as a
  ;; placeholder.  Skip it, so the line holds one character per glyph and Lem
  ;; (which measures display width itself) draws it two columns wide.  INDEX
  ;; is the position in CHARS, which falls behind COL after a wide character.
  (let ((chars (make-string cols))
        (index 0)
        (skip-next nil)
        (attrs nil)
        (run-start 0)
        (previous-attribute nil))
    (loop :for col :from 0 :below cols
          :for ptr := (cffi:inc-pointer buf (* col cell-size))
          :do (if skip-next
                  (setf skip-next nil)
                  (let* ((ch (cffi:mem-ref ptr :uint32 +cell-off-ch+))
                         (fg-r (cffi:mem-ref ptr :uint8 +cell-off-fg-r+))
                         (fg-g (cffi:mem-ref ptr :uint8 +cell-off-fg-g+))
                         (fg-b (cffi:mem-ref ptr :uint8 +cell-off-fg-b+))
                         (bg-r (cffi:mem-ref ptr :uint8 +cell-off-bg-r+))
                         (bg-g (cffi:mem-ref ptr :uint8 +cell-off-bg-g+))
                         (bg-b (cffi:mem-ref ptr :uint8 +cell-off-bg-b+))
                         (bold (cffi:mem-ref ptr :uint8 +cell-off-bold+))
                         (underline (cffi:mem-ref ptr :uint8 +cell-off-underline+))
                         (reverse-attr (cffi:mem-ref ptr :uint8 +cell-off-reverse+))
                         (width (cffi:mem-ref ptr :uint8 +cell-off-width+))
                         (default-fg (cffi:mem-ref ptr :uint8 +cell-off-default-fg+))
                         (default-bg (cffi:mem-ref ptr :uint8 +cell-off-default-bg+))
                         (attribute (get-attribute-for-cell terminal
                                                           fg-r fg-g fg-b
                                                           bg-r bg-g bg-b
                                                           bold underline reverse-attr
                                                           default-fg default-bg))
                         ;; Fast validity check instead of ignore-errors + code-char
                         (char (if (and (plusp ch) (<= ch #x10FFFF))
                                   (code-char ch)
                                   #\Space)))
                    (unless (eq attribute previous-attribute)
                      (when previous-attribute
                        (push (list run-start index previous-attribute) attrs))
                      (setf run-start index)
                      (setf previous-attribute attribute))
                    (setf (schar chars index) char)
                    (incf index)
                    (when (and (= width 2) (char/= char #\Space))
                      (setf skip-next t))))
          :finally (when previous-attribute
                     (push (list run-start index previous-attribute) attrs)))
    (when (< index cols)
      (setf chars (subseq chars 0 index)))
    ;; Directly set line content — bypass all buffer edit machinery.
    ;; See CONTRACT above; this is permitted because the terminal buffer
    ;; is rendered-into, not edited.
    (multiple-value-bind (chars attrs)
        (trim-trailing-blanks chars (nreverse attrs))
      (line:set-line-string chars line-obj)
      (setf (getf (line:line-plist line-obj) :attribute) attrs))))

(defun buffer-view-points (buffer)
  "Return the buffer point and the view points of every window showing BUFFER."
  (cons (buffer-point buffer)
        (mapcar #'window-view-point (get-buffer-windows buffer))))

(defun point-positions (points)
  "Return the (line-number . charpos) of each of POINTS."
  (mapcar (lambda (point)
            (cons (line-number-at-point point) (point-charpos point)))
          points))

(defun shift-points-by-lines (points positions delta)
  "Move each of POINTS to its entry of POSITIONS, DELTA lines further down."
  (loop :for point :in points
        :for (line-number . charpos) :in positions
        :do (move-to-line point (+ line-number delta))
            (line-start point)
            (character-offset point (min charpos (length (line-string point))))))

(defun deactivate-scrollback (terminal)
  "Remove scrollback lines from the buffer, keeping only terminal rows.
Called when transitioning from scrollback-active to normal view."
  (when (terminal-scrollback-active terminal)
    (setf (terminal-scrollback-active terminal) nil)
    (let* ((buffer (terminal-buffer terminal))
           (rows (terminal-rows terminal))
           (nlines (buffer-nlines buffer)))
      (when (> nlines rows)
        (with-buffer-read-only buffer nil
          ;; Delete all lines before the terminal area
          (with-point ((start (buffer-point buffer))
                       (end (buffer-point buffer)))
            (buffer-start start)
            (move-to-line end (1+ (- nlines rows)))
            (line-start end)
            (delete-between-points start end)))))))

(defun activate-scrollback (terminal)
  "Insert scrollback lines from the ring buffer into the buffer above the terminal rows."
  (unless (terminal-scrollback-active terminal)
    ;; The scrollback ring is mutated by cb-sb-pushline/popline on the I/O
    ;; thread during parsing; snapshot it into a string under IO-LOCK, then
    ;; insert into the buffer (editor-thread-only) outside the lock.
    (let ((combined
            (bt2:with-lock-held ((terminal-io-lock terminal))
              (let ((count (scrollback-count terminal)))
                (when (plusp count)
                  (let* ((ring (terminal-scrollback-ring terminal))
                         (head (terminal-scrollback-head terminal))
                         (limit (length ring)))
                    (with-output-to-string (s)
                      (loop :for i :from 0 :below count
                            :for idx := (mod (- head count (- i)) limit)
                            :for line := (aref ring idx)
                            :do (when line (write-string line s))
                                (write-char #\newline s)))))))))
      (when combined
        (setf (terminal-scrollback-active terminal) t)
        (let* ((buffer (terminal-buffer terminal))
               (points (buffer-view-points buffer))
               (positions (point-positions points))
               (count (count #\Newline combined)))
          (with-buffer-read-only buffer nil
            (with-point ((start (buffer-point buffer)))
              (buffer-start start)
              (insert-string start combined)))
          ;; Keep the cursor and every window on the rows they showed, so the
          ;; history appears above the screen instead of replacing it.
          (shift-points-by-lines points positions count))))))

(defun snap-to-bottom (terminal)
  "Reset view of every window showing the terminal buffer to the terminal area."
  (let ((buffer (terminal-buffer terminal)))
    (dolist (window (get-buffer-windows buffer))
      (buffer-start (window-view-point window)))))

(defmethod render ((terminal terminal))
  ;; CONTRACT: callers must hold TERMINAL-IO-LOCK — RENDER reads vterm screen
  ;; state (terminal-get-row, cursor position), which the I/O thread mutates
  ;; while parsing.  RENDER-EVENT and UPDATE acquire the lock; do not call
  ;; this directly without holding it.
  (let* ((viscus (terminal-viscus terminal))
         (rows (terminal-rows terminal))
         (cols (terminal-cols terminal))
         (buffer (terminal-buffer terminal))
         (point (buffer-point buffer))
         (dirty (terminal-dirty-rows terminal))
         (cell-size (cell-data-size)))
    ;; If scrollback is displayed, remove it before rendering terminal rows
    (deactivate-scrollback terminal)
    (with-buffer-read-only buffer nil
      ;; Ensure the buffer has exactly ROWS lines
      (let ((nlines (buffer-nlines buffer)))
        (cond ((< nlines rows)
               (buffer-end point)
               (dotimes (i (- rows nlines))
                 (insert-character point #\newline)))
              ((> nlines rows)
               (move-to-line point (1+ rows))
               (line-start point)
               (with-point ((start point))
                 (character-offset start -1)
                 (buffer-end point)
                 (delete-between-points start point)))))
      ;; Get the first line object and walk the linked list directly.
      ;; This avoids all point manipulation during row rendering.
      (move-to-line point 1)
      (let ((first-line (point-line point)))
        (cffi:with-foreign-object (buf '(:struct ffi::cell-data) cols)
          (cond
            ;; Full redraw: render every row directly
            ((or (null dirty)
                 (/= (length dirty) rows))
             (loop :for row :from 0 :below rows
                   :for cur-line := first-line :then (line:line-next cur-line)
                   :while cur-line
                   :do (render-row-direct terminal cur-line cols row buf cell-size))
             (setf (terminal-dirty-rows terminal)
                   (make-array rows :element-type 'bit :initial-element 0))
             (setf (terminal-dirty-any-p terminal) nil))
            ;; Partial update: only dirty rows
            (t
             (loop :for row :from 0 :below rows
                   :for cur-line := first-line :then (line:line-next cur-line)
                   :while cur-line
                   :do (when (= 1 (aref dirty row))
                         (render-row-direct terminal cur-line cols row buf cell-size)))
             (fill dirty 0)
             (setf (terminal-dirty-any-p terminal) nil)))))
      (move-to-cursor point
                      (ffi::terminal-cursor-row viscus)
                      (ffi::terminal-cursor-col viscus)))))

(defmethod adjust-point ((terminal terminal))
  (let* ((buffer (terminal-buffer terminal))
         (point (buffer-point buffer))
         (viscus (terminal-viscus terminal)))
    ;; Cursor position is updated by cb-movecursor on the I/O thread.
    (multiple-value-bind (row col)
        (bt2:with-lock-held ((terminal-io-lock terminal))
          (values (ffi::terminal-cursor-row viscus)
                  (ffi::terminal-cursor-col viscus)))
      (with-buffer-read-only buffer nil
        (move-to-cursor point row col)))))

(defmethod process-input ((terminal terminal) &optional (budget-ms *write-timeout-ms*))
  "Drain and parse a slice of PTY data (up to BUDGET-MS).
CONTRACT: callers must hold TERMINAL-IO-LOCK — this runs vterm-input-write
and its damage callbacks, which race with RENDER and input otherwise."
  (ffi::terminal-process-input-timed (terminal-viscus terminal)
                                     budget-ms))

(defmethod input-character ((terminal terminal) character &key (mod 0))
  (unless (process-exited-p terminal)
    (bt2:with-lock-held ((terminal-io-lock terminal))
      (ffi::terminal-input-char (terminal-viscus terminal)
                                (char-code character)
                                mod))))

(defmethod input-key ((terminal terminal) key &key (mod 0))
  (unless (process-exited-p terminal)
    (bt2:with-lock-held ((terminal-io-lock terminal))
      (ffi::terminal-input-key (terminal-viscus terminal)
                               key
                               mod))))

(defun paste-text (string)
  "Return STRING with line breaks as a terminal sends them when pasting:
CR, like xterm, rather than LF."
  (with-output-to-string (out)
    (loop :with previous := nil
          :for char :across string
          :do (cond ((char= char #\Newline)
                     (unless (eql previous #\Return)
                       (write-char #\Return out)))
                    (t (write-char char out)))
              (setf previous char))))

(defmethod paste-string ((terminal terminal) string)
  "Send STRING to TERMINAL as pasted text.  When the program has enabled
bracketed paste mode it receives the text between ESC[200~ and ESC[201~, so a
shell inserts a multi-line paste instead of running its first line."
  (unless (process-exited-p terminal)
    (bt2:with-lock-held ((terminal-io-lock terminal))
      (let ((viscus (terminal-viscus terminal)))
        (ffi::terminal-paste-start viscus)
        (loop :for char :across (paste-text string)
              :do (ffi::terminal-input-char viscus (char-code char) 0))
        (ffi::terminal-paste-end viscus)))))

(defun same-size-p (terminal rows cols)
  (and (= (terminal-rows terminal) rows)
       (= (terminal-cols terminal) cols)))

(defmethod resize ((terminal terminal)
                   &key (rows (alexandria:required-argument :rows))
                        (cols (alexandria:required-argument :cols)))
  (unless (or (process-exited-p terminal)
              (same-size-p terminal rows cols))
    ;; terminal-resize mutates vterm (vterm-set-size) and triggers damage/
    ;; resize callbacks, so serialize against the I/O thread's parsing.
    (bt2:with-lock-held ((terminal-io-lock terminal))
      (setf (terminal-rows terminal) rows
            (terminal-cols terminal) cols)
      (ffi::terminal-resize (terminal-viscus terminal) rows cols)
      (mark-all-rows-dirty terminal)
      (clrhash (terminal-attribute-cache terminal)))
    ;; Reflect the new size even if no further PTY output follows.
    (arm-render-timer terminal)))

;;; callbacks
(defun cb-damage (rect id)
  "Mark damaged rows as dirty using the VTermRect region."
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (cffi:with-foreign-slots ((ffi::start-row ffi::end-row) rect (:struct ffi::vterm-rect))
        (mark-rows-dirty terminal ffi::start-row ffi::end-row)))))

(defun cb-moverect (dest src id)
  "Mark both source and destination regions as dirty."
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (cffi:with-foreign-slots ((ffi::start-row ffi::end-row) dest (:struct ffi::vterm-rect))
        (mark-rows-dirty terminal ffi::start-row ffi::end-row))
      (cffi:with-foreign-slots ((ffi::start-row ffi::end-row) src (:struct ffi::vterm-rect))
        (mark-rows-dirty terminal ffi::start-row ffi::end-row)))))

(defun cb-movecursor (pos oldpos visible id)
  "Mark both old and new cursor rows as dirty."
  (declare (ignore visible))
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (cffi:with-foreign-slots ((ffi::row) pos (:struct ffi::vterm-pos))
        (mark-rows-dirty terminal ffi::row (1+ ffi::row)))
      (cffi:with-foreign-slots ((ffi::row) oldpos (:struct ffi::vterm-pos))
        (mark-rows-dirty terminal ffi::row (1+ ffi::row))))))

(defun cb-settermprop (prop val id)
  (declare (ignore prop val id)))

(defun cb-bell (id)
  (declare (ignore id)))

(defun cb-resize (rows cols id)
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (setf (terminal-rows terminal) rows
            (terminal-cols terminal) cols)
      (mark-all-rows-dirty terminal))))

(defun cb-sb-pushline (cols cells id)
  "Store scrollback line in the ring buffer.  Uses zero-allocation C extraction
into the terminal's per-terminal buffer; trailing spaces are stripped C-side
so only the final Lisp string is allocated here."
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (let* ((ring (terminal-scrollback-ring terminal))
             (limit (length ring))
             (head (terminal-scrollback-head terminal)))
        ;; Extract text via the C extension's per-terminal buffer.
        (cffi:with-foreign-object (len-ptr :int)
          (let* ((c-str (ffi::terminal-sb-line-extract
                         (terminal-viscus terminal) cols cells len-ptr))
                 (len (cffi:mem-ref len-ptr :int))
                 ;; This runs on the I/O thread inside vterm-input-write, so the
                 ;; decode must never signal out of the callback (it would unwind
                 ;; through C and kill the thread).  A scrollback line can hold
                 ;; bytes that aren't valid UTF-8 — e.g. a non-UTF-8 filename
                 ;; from `find /' — so fall back to a lossless latin-1 decode,
                 ;; which never errors, instead of dropping the line.
                 (text (handler-case
                           (cffi:foreign-string-to-lisp c-str :count len :encoding :utf-8)
                         (error ()
                           (cffi:foreign-string-to-lisp c-str :count len :encoding :iso-8859-1)))))
            (setf (aref ring head) text)))
        (setf (terminal-scrollback-head terminal) (mod (1+ head) limit))
        (when (< (scrollback-count terminal) limit)
          (incf (scrollback-count terminal))))
      ;; Must mark all rows dirty because vterm scrolls content up internally
      ;; and cb-damage may only report the newly-written bottom row.
      (mark-all-rows-dirty terminal))))

(defun cb-sb-popline (cols cells id)
  "Remove the most recent scrollback line from the ring."
  (declare (ignore cols cells))
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (when (plusp (scrollback-count terminal))
        (let* ((ring (terminal-scrollback-ring terminal))
               (limit (length ring))
               (head (terminal-scrollback-head terminal))
               (new-head (mod (1- head) limit)))
          (setf (aref ring new-head) nil)
          (setf (terminal-scrollback-head terminal) new-head)
          (decf (scrollback-count terminal))))
      (mark-all-rows-dirty terminal))))

(defun cb-sb-clear (id)
  "Discard the stored scrollback (ESC[3J, sent by `clear').  Runs on the I/O
thread while parsing, under IO-LOCK like the other scrollback callbacks."
  (let ((terminal (find-terminal-by-id id)))
    (when terminal
      (fill (terminal-scrollback-ring terminal) nil)
      (setf (terminal-scrollback-head terminal) 0
            (scrollback-count terminal) 0))))

(ffi::set-callbacks :damage 'cb-damage
                    :moverect 'cb-moverect
                    :movecursor 'cb-movecursor
                    :settermprop 'cb-settermprop
                    :bell 'cb-bell
                    :resize 'cb-resize
                    :sb-pushline 'cb-sb-pushline
                    :sb-popline 'cb-sb-popline
                    :sb-clear 'cb-sb-clear)
