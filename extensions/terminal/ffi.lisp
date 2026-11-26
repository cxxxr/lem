(defpackage :lem-terminal/ffi
  (:use :cl)
  (:export :terminal-new
           :terminal-delete
           :vterm-rect
           :vterm-pos
           :vterm-prop
           :terminal-title
           :terminal-cwd
           :terminal-last-error-message))
(in-package :lem-terminal/ffi)

(pushnew (asdf:system-relative-pathname
          :lem-terminal (format nil
                                "~(lib/~A/~A/~)"
                                (uiop:operating-system)
                                (uiop:architecture)))
         cffi:*foreign-library-directories*
         :test #'uiop:pathname-equal)

(cffi:define-foreign-library terminal
  (:unix "terminal.so"))

(ignore-errors
  (cffi:use-foreign-library terminal))

(defconstant VTERM_KEY_NONE 0)
(defconstant VTERM_KEY_ENTER 1)
(defconstant VTERM_KEY_TAB 2)
(defconstant VTERM_KEY_BACKSPACE 3)
(defconstant VTERM_KEY_ESCAPE 4)
(defconstant VTERM_KEY_UP 5)
(defconstant VTERM_KEY_DOWN 6)
(defconstant VTERM_KEY_LEFT 7)
(defconstant VTERM_KEY_RIGHT 8)
(defconstant VTERM_KEY_INS 9)
(defconstant VTERM_KEY_DEL 10)
(defconstant VTERM_KEY_HOME 11)
(defconstant VTERM_KEY_END 12)
(defconstant VTERM_KEY_PAGEUP 13)
(defconstant VTERM_KEY_PAGEDOWN 14)
(defconstant VTERM_KEY_FUNCTION_0 256)
(defconstant VTERM_KEY_FUNCTION_MAX 511)
(defconstant VTERM_KEY_KP_0 512)
(defconstant VTERM_KEY_KP_1 513)
(defconstant VTERM_KEY_KP_2 514)
(defconstant VTERM_KEY_KP_3 515)
(defconstant VTERM_KEY_KP_4 516)
(defconstant VTERM_KEY_KP_5 517)
(defconstant VTERM_KEY_KP_6 518)
(defconstant VTERM_KEY_KP_7 519)
(defconstant VTERM_KEY_KP_8 520)
(defconstant VTERM_KEY_KP_9 521)
(defconstant VTERM_KEY_KP_MULT 522)
(defconstant VTERM_KEY_KP_PLUS 523)
(defconstant VTERM_KEY_KP_COMMA 524)
(defconstant VTERM_KEY_KP_MINUS 525)
(defconstant VTERM_KEY_KP_PERIOD 526)
(defconstant VTERM_KEY_KP_DIVIDE 527)
(defconstant VTERM_KEY_KP_ENTER 528)
(defconstant VTERM_KEY_KP_EQUAL 529)
(defconstant VTERM_KEY_MAX 530)
(defconstant VTERM_N_KEYS 530)

(defconstant VTERM_MOD_NONE #x00)
(defconstant VTERM_MOD_SHIFT #x01)
(defconstant VTERM_MOD_ALT #x02)
(defconstant VTERM_MOD_CTRL #x04)

;; Define a type corresponding to char*[], where the array is null-terminated
(cffi:define-foreign-type string-of-strings ()
  ()
  (:actual-type :pointer))
(cffi:define-parse-method string-of-strings ()
  (make-instance 'string-of-strings))
;; It's inefficient to store the array of string pointers in both the second return
;; value of translate-to-foreign and (necessarily) foreign memory- but I can't get
;; the "efficient" way to work.
;; Also this likely isn't performance sensitive. Like at all.
(defmethod cffi:translate-to-foreign (val (type string-of-strings))
  (let ((string-pointers (map 'vector  (lambda (str) (cffi:foreign-string-alloc str)) val)))
    (values (cffi:foreign-alloc :pointer :null-terminated-p t :initial-contents string-pointers)
            string-pointers)))
(defmethod cffi:free-translated-object (pointer (type string-of-strings) string-pointers)
  (map nil (lambda (p) (cffi:foreign-string-free p))
       string-pointers)
  (cffi:foreign-free pointer))

(cffi:defcfun ("terminal_new" %terminal-new) :pointer
  (id :int)
  (rows :int)
  (cols :int)
  (program :string)
  (argv (string-of-strings))
  (cb_damage :pointer)
  (cb_moverect :pointer)
  (cb_movecursor :pointer)
  (cb_settermprop :pointer)
  (cb_bell :pointer)
  (cb_resize :pointer)
  (cb_sb_pushline :pointer)
  (cb_sb_popline :pointer)
  (cb_sb_clear :pointer))

(defun terminal-new (directory id rows cols callbacks &key program argv)
  "Create a new terminal PTY running PROGRAM with ARGV.
When PROGRAM/ARGV are not provided, defaults to the user's shell started
in DIRECTORY (via 'cd <directory>; <shell>').
CALLBACKS is the list of the libvterm callback pointers, in the order of
terminal_new's arguments: damage, moverect, movecursor, settermprop, bell,
resize, sb_pushline, sb_popline and sb_clear.  Each is called with ID as its
last argument."
  (let* ((shell (or (uiop:getenv "SHELL") "/bin/bash"))
         (program (or program shell))
         (argv (or argv
                   (list shell "-c"
                         (concatenate 'string "cd " directory "; " shell)))))
    (apply #'%terminal-new id rows cols program argv callbacks)))

(cffi:defcfun ("terminal_delete" terminal-delete) :void
  "Free TERMINAL: hang up its child process, close the PTY and reap the child."
  (terminal :pointer))

(cffi:defcfun ("terminal_input_char" terminal-input-char) :void
  (terminal :pointer)
  (c :uint32)
  (mod :int))

(cffi:defcfun ("terminal_input_key" terminal-input-key) :void
  (terminal :pointer)
  (key :int)
  (mod :int))

(cffi:defcfun ("terminal_paste_start" terminal-paste-start) :void
  (terminal :pointer))

(cffi:defcfun ("terminal_paste_end" terminal-paste-end) :void
  (terminal :pointer))

(cffi:defcfun ("terminal_process_input_wait" terminal-process-input-wait) :int
  "Block until the PTY is readable (returns 1) or TERMINAL-WAKE was called /
the PTY already hung up (returns 0)."
  (terminal :pointer))

(cffi:defcfun ("terminal_wake" terminal-wake) :void
  "Interrupt a blocked TERMINAL-PROCESS-INPUT-WAIT from another thread."
  (terminal :pointer))

(cffi:defcfun ("terminal_last_error" %terminal-last-error) :int)

(defun terminal-last-error-message ()
  "Return why the last TERMINAL-NEW of this thread returned a null pointer, as
the message of its errno.  A terminal.so built before terminal_last_error
cannot say why: \"unknown error\" then."
  (if (cffi:foreign-symbol-pointer "terminal_last_error")
      (cffi:foreign-funcall "strerror" :int (%terminal-last-error) :string)
      "unknown error"))

(cffi:defcfun ("terminal_title" %terminal-title) :pointer
  (terminal :pointer))

(defun terminal-title (terminal)
  "Return the window title last set by the program in TERMINAL.  Bytes that
are not valid UTF-8 (e.g. a title truncated mid-character) fall back to
latin-1 rather than signalling, since this runs on the I/O thread."
  (let ((pointer (%terminal-title terminal)))
    (handler-case (cffi:foreign-string-to-lisp pointer :encoding :utf-8)
      (error ()
        (cffi:foreign-string-to-lisp pointer :encoding :iso-8859-1)))))

(cffi:defcfun ("terminal_cwd" %terminal-cwd) :int
  (terminal :pointer)
  (buf :pointer)
  (size :int))

(defun terminal-cwd (terminal)
  "Return the current working directory of TERMINAL's child process as a
string, or NIL when the OS cannot tell (see terminal_cwd in terminal.c)."
  (cffi:with-foreign-pointer (buf 4096 size)
    (let ((len (%terminal-cwd terminal buf size)))
      (when (plusp len)
        (handler-case (cffi:foreign-string-to-lisp buf :count len :encoding :utf-8)
          (error () nil))))))

(cffi:defcfun ("terminal_pid" terminal-pid) :int
  "Return the process id of the terminal's child process."
  (terminal :pointer))

(cffi:defcfun ("terminal_eof_p" %terminal-eof-p) :int
  (terminal :pointer))

(defun terminal-eof-p (terminal)
  "Return T once the PTY has hung up, i.e. the child process closed its side."
  (= 1 (%terminal-eof-p terminal)))

(cffi:defcfun ("terminal_collect_exit_status" terminal-collect-exit-status) :int
  "Reap the exited child and return its status: the exit code, 128+signal
when it was killed by a signal, or -1 when unknown."
  (terminal :pointer))

(cffi:defcfun ("terminal_process_input_nonblock" terminal-process-input-nonblock) :size
  (terminal :pointer))

;; Time-budgeted PTY drain: processes data for up to max-ms milliseconds
(cffi:defcfun ("terminal_process_input_timed" terminal-process-input-timed) :size
  (terminal :pointer)
  (max-ms :int))

(cffi:defcfun ("terminal_process_input" terminal-process-input) :void
  (terminal :pointer))

(cffi:defcfun ("terminal_query_cell" terminal-query-cell) :pointer
  (terminal :pointer)
  (x :int)
  (y :int))

;; Packed cell data struct for bulk row transfer (12 bytes per cell)
(cffi:defcstruct cell-data
  (ch :uint32)
  (fg-r :uint8)
  (fg-g :uint8)
  (fg-b :uint8)
  (bg-r :uint8)
  (bg-g :uint8)
  (bg-b :uint8)
  (bold :uint8)
  (underline :uint8)
  (reverse-attr :uint8)
  (width :uint8)
  (default-fg :uint8)
  (default-bg :uint8))

(cffi:defcfun ("terminal_get_row" terminal-get-row) :int
  (terminal :pointer)
  (row :int)
  (out :pointer)
  (cols :int))

(cffi:defcfun ("terminal_sb_line_to_string" terminal-sb-line-to-string) :int
  (terminal :pointer)
  (cols :int)
  (cells :pointer)
  (out :pointer)
  (out-size :int))

;; Zero-allocation: extracts into the terminal's per-terminal C buffer,
;; returns pointer. Trailing ASCII spaces are stripped during extraction.
(cffi:defcfun ("terminal_sb_line_extract" terminal-sb-line-extract) :pointer
  (terminal :pointer)
  (cols :int)
  (cells :pointer)
  (out-len (:pointer :int)))

(cffi:defcfun ("terminal_last_cell_chars" terminal-last-cell-chars) :pointer
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_width" terminal-last-cell-width) :int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_bold" terminal-last-cell-attrs-bold) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_underline" terminal-last-cell-attrs-underline) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_italic" terminal-last-cell-attrs-italic) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_blink" terminal-last-cell-attrs-blink) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_reverse" terminal-last-cell-attrs-reverse) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_conceal" terminal-last-cell-attrs-conceal) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_strike" terminal-last-cell-attrs-strike) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_font" terminal-last-cell-attrs-font) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_dwl" terminal-last-cell-attrs-dwl) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_dhl" terminal-last-cell-attrs-dhl) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_small" terminal-last-cell-attrs-small) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_baseline" terminal-last-cell-attrs-baseline) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_fg_red" terminal-last-cell-fg-red) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_fg_green" terminal-last-cell-fg-green) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_fg_blue" terminal-last-cell-fg-blue) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_bg_red" terminal-last-cell-bg-red) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_bg_green" terminal-last-cell-bg-green) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_bg_blue" terminal-last-cell-bg-blue) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_cursor_row" terminal-cursor-row) :int
  (terminal :pointer))

(cffi:defcfun ("terminal_cursor_col" terminal-cursor-col) :int
  (terminal :pointer))

(cffi:defcfun ("terminal_resize" terminal-resize) :void
  (terminal :pointer)
  (rows :int)
  (cols :int))

;; Field order must match libvterm's VTermRect exactly: start_row, end_row,
;; start_col, end_col.  A previous definition swapped end-row and start-col,
;; so cb-damage/cb-moverect read a column value as the end row and marked an
;; empty/garbage row range -- erases and scrolls the cursor didn't visit
;; (clear, completion listings) were never re-rendered, leaving ghosting.
(cffi:defcstruct vterm-rect
  "A rectangle of screen cells, as libvterm passes it to the damage and
moverect callbacks."
  (start-row :int)
  (end-row :int)
  (start-col :int)
  (end-col :int))

(cffi:defcstruct vterm-pos
  "A screen position, as libvterm passes it to the movecursor callback."
  (row :int)
  (col :int))

(cffi:defcenum vterm-prop
  "The terminal properties libvterm reports to the settermprop callback."
  (:VTERM_PROP_CURSORVISIBLE 1) ; bool
  :VTERM_PROP_CURSORBLINK       ; bool
  :VTERM_PROP_ALTSCREEN         ; bool
  :VTERM_PROP_TITLE             ; string
  :VTERM_PROP_ICONNAME          ; string
  :VTERM_PROP_REVERSE           ; bool
  :VTERM_PROP_CURSORSHAPE       ; number
  :VTERM_PROP_MOUSE             ; number
  :VTERM_PROP_FOCUSREPORT       ; bool
  )
