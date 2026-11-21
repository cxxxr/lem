(defpackage :lem-tests/terminal
  (:use :cl :rove)
  (:local-nicknames (:ffi :lem-terminal/ffi)))
(in-package :lem-tests/terminal)

;;; Process lifecycle of the native terminal helper (terminal.so).
;;; These tests drive the FFI directly, without an editor, and are skipped
;;; when terminal.so has not been built (`make terminal-lib').

(defvar *test-terminal-id* -1
  "Terminal id that matches no registered terminal, so the Lisp-side vterm
callbacks ignore events from these tests.")

(defparameter *wheel-test-script*
  "seq 1 100; while :; do printf x; sleep 0.02; done"
  "Prints 100 lines, then keeps adding to the last one.")

(defclass manual-timer-manager (lem/common/timer:timer-manager) ()
  (:documentation "Drops the renders the I/O thread asks for; the test renders
itself, as the timer would, with RENDER-TICK."))

;; send-timer-notification is not exported: it is what a timer manager
;; specializes to be told of timers, and there is no other way to keep the
;; renders of the I/O thread out of the tests.
(defmethod lem/common/timer::send-timer-notification
    ((timer-manager manual-timer-manager) continue)
  (declare (ignore continue)))

(lem:define-major-mode derived-terminal-mode lem-terminal/terminal-mode::terminal-mode
    (:name "Derived Terminal"))

(defun library-available-p ()
  (not (null (cffi:foreign-symbol-pointer "terminal_new"))))

(defun make-test-terminal (script)
  (ffi::terminal-new "" *test-terminal-id* 24 80
                     :program "/bin/sh"
                     :argv (list "/bin/sh" "-c" script)))

(defun drain-until-eof (viscus &key (timeout-seconds 5))
  "Parse PTY output like the I/O thread does until the PTY hangs up.
Returns T on EOF, NIL on timeout."
  (loop :with deadline := (+ (get-internal-real-time)
                             (* timeout-seconds internal-time-units-per-second))
        :until (ffi::terminal-eof-p viscus)
        :when (> (get-internal-real-time) deadline)
          :do (return-from drain-until-eof nil)
        :do (ffi::terminal-process-input-timed viscus 10)
            (sleep 0.01))
  t)

(defun process-alive-p (pid)
  (zerop (nth-value 2 (uiop:run-program (list "kill" "-0" (princ-to-string pid))
                                        :ignore-error-status t))))

(defun wait-for-pid-file (pathname &key (timeout-seconds 5))
  (loop :repeat (* timeout-seconds 100)
        :do (let ((content (ignore-errors (uiop:read-file-string pathname))))
              (when (and content (plusp (length (string-trim '(#\Newline) content))))
                (return (parse-integer content :junk-allowed t))))
            (sleep 0.01)))

(defmacro with-test-terminal ((var script) &body body)
  `(if (library-available-p)
       (let ((,var (make-test-terminal ,script)))
         (unwind-protect (progn ,@body)
           (ffi::terminal-delete ,var)))
       (skip "terminal.so is not built")))

(deftest exit-status-is-reported-after-eof
  (with-test-terminal (viscus "exit 3")
    (ok (drain-until-eof viscus))
    (ok (= 3 (ffi::terminal-collect-exit-status viscus)))
    (testing "waiting after EOF returns immediately instead of spinning"
      (ok (= 0 (ffi::terminal-process-input-wait viscus))))))

(deftest exec-failure-exits-with-127
  (if (library-available-p)
      (let ((viscus (ffi::terminal-new "" *test-terminal-id* 24 80
                                       :program "/nonexistent/lem-terminal-test"
                                       :argv (list "/nonexistent/lem-terminal-test"))))
        (unwind-protect
             (progn
               (ok (drain-until-eof viscus))
               (ok (= 127 (ffi::terminal-collect-exit-status viscus))))
          (ffi::terminal-delete viscus)))
      (skip "terminal.so is not built")))

(deftest wake-interrupts-wait
  (with-test-terminal (viscus "exec sleep 100")
    (let ((thread (bt2:make-thread
                   (lambda () (ffi::terminal-process-input-wait viscus))
                   :name "terminal wake test")))
      (sleep 0.2)
      (ffi::terminal-wake viscus)
      (ok (= 0 (bt2:join-thread thread))))))

(deftest delete-kills-running-process
  (if (library-available-p)
      (uiop:with-temporary-file (:pathname pid-file)
        (let* ((viscus (make-test-terminal
                        (format nil "echo $$ > '~A'; exec sleep 100"
                                (uiop:native-namestring pid-file))))
               (pid (wait-for-pid-file pid-file)))
          (ok pid)
          (ok (process-alive-p pid))
          (ffi::terminal-delete viscus)
          (ng (process-alive-p pid))))
      (skip "terminal.so is not built")))

(defun row-string (viscus row &key (cols 80))
  "Return the text of screen ROW, with trailing blanks removed."
  (let ((cell-size (cffi:foreign-type-size '(:struct ffi::cell-data))))
    (cffi:with-foreign-object (buf '(:struct ffi::cell-data) cols)
      (ffi::terminal-get-row viscus row buf cols)
      (string-right-trim
       " "
       (coerce (loop :for col :from 0 :below cols
                     :for ch := (cffi:mem-ref (cffi:inc-pointer buf (* col cell-size)) :uint32 0)
                     :collect (if (< 0 ch #x110000) (code-char ch) #\Space))
               'string)))))

(deftest alternate-screen-restores-main-screen
  (with-test-terminal (viscus "printf 'main'; printf '\\033[?1049h\\033[Halt'; printf '\\033[?1049l'")
    (ok (drain-until-eof viscus))
    (ok (equal "main" (row-string viscus 0)))))

(defun render-row-text (viscus row &key (cols 80))
  "Render screen ROW the way the terminal buffer does and return the line text."
  (let ((terminal (make-instance 'lem-terminal/terminal::terminal
                                 :id *test-terminal-id* :viscus viscus
                                 :rows 24 :cols cols :buffer nil))
        (buffer (lem:make-buffer "*terminal render test*" :temporary t)))
    (unwind-protect
         (let ((line (lem:point-line (lem:buffer-point buffer))))
           (cffi:with-foreign-object (buf '(:struct ffi::cell-data) cols)
             (lem-terminal/terminal::render-row-direct
              terminal line cols row buf
              (cffi:foreign-type-size '(:struct ffi::cell-data))))
           (lem:line-string (lem:buffer-point buffer)))
      (lem:delete-buffer buffer))))

(deftest wide-characters-are-not-followed-by-a-space
  (with-test-terminal (viscus "printf 'あい-abc'")
    (ok (drain-until-eof viscus))
    (ok (equal "あい-abc" (string-right-trim " " (render-row-text viscus 0))))))

(defun parse-for (viscus seconds)
  (loop :repeat (round seconds 0.01)
        :do (ffi::terminal-process-input-timed viscus 10)
            (sleep 0.01)))

(deftest paste-is-bracketed-and-uses-cr
  ;; The program enables bracketed paste, then dumps the 15 bytes it reads:
  ;; ESC[200~ a CR b ESC[201~
  (with-test-terminal (viscus "stty raw -echo; printf '\\033[?2004hready'; dd bs=1 count=15 2>/dev/null | od -An -c | tr -d ' \\n'; echo")
    (parse-for viscus 0.5)
    (let ((terminal (make-instance 'lem-terminal/terminal::terminal
                                   :id *test-terminal-id* :viscus viscus
                                   :rows 24 :cols 80 :buffer nil)))
      (lem-terminal/terminal:paste-string terminal (format nil "a~%b")))
    (ok (drain-until-eof viscus))
    (ok (search "033[200~a\\rb033[201~" (row-string viscus 0)))))

(deftest process-directory-follows-the-shell
  (with-test-terminal (viscus "cd /tmp && exec sleep 100")
    (let ((terminal (make-instance 'lem-terminal/terminal::terminal
                                   :id *test-terminal-id* :viscus viscus
                                   :rows 24 :cols 80 :buffer nil)))
      ;; truename: on macOS /tmp is a symlink to /private/tmp.
      (ok (loop :repeat 100
                :thereis (equal (uiop:native-namestring (truename "/tmp/"))
                                (lem-terminal/terminal:process-directory terminal))
                :do (sleep 0.02))))))

(deftest trailing-blanks-are-trimmed
  (let ((plain (lem:make-attribute :foreground "white"))
        (bar (lem:make-attribute :foreground "white" :background "blue")))
    (testing "invisible padding is dropped"
      (multiple-value-bind (chars attrs)
          (lem-terminal/terminal::trim-trailing-blanks
           "$ ls      " (list (list 0 10 plain)))
        (ok (equal "$ ls" chars))
        (ok (equal (list (list 0 4 plain)) attrs))))
    (testing "spaces with a background are kept"
      (multiple-value-bind (chars attrs)
          (lem-terminal/terminal::trim-trailing-blanks
           "ab  cd    " (list (list 0 2 plain) (list 2 6 bar) (list 6 10 plain)))
        (ok (equal "ab  cd" chars))
        (ok (equal (list (list 0 2 plain) (list 2 6 bar)) attrs)))
      (multiple-value-bind (chars attrs)
          (lem-terminal/terminal::trim-trailing-blanks
           "ab   " (list (list 0 2 plain) (list 2 5 bar)))
        (ok (equal "ab   " chars))
        (ok (= 2 (length attrs)))))
    (testing "a blank row becomes empty"
      (ok (equal "" (lem-terminal/terminal::trim-trailing-blanks
                     "    " (list (list 0 4 plain))))))))

(defun cell-slot (viscus row col slot &key (cols 80))
  (cffi:with-foreign-object (buf '(:struct ffi::cell-data) cols)
    (ffi::terminal-get-row viscus row buf cols)
    (cffi:foreign-slot-value (cffi:mem-aptr buf '(:struct ffi::cell-data) col)
                             '(:struct ffi::cell-data) slot)))

(deftest default-colors-are-reported
  ;; col 0: default colours, col 1: red fg, col 2: explicit black bg
  (with-test-terminal (viscus "printf 'a\\033[31mb\\033[0;40mc\\033[0m'")
    (ok (drain-until-eof viscus))
    (ok (= 1 (cell-slot viscus 0 0 'ffi::default-fg)))
    (ok (= 1 (cell-slot viscus 0 0 'ffi::default-bg)))
    (ok (= 0 (cell-slot viscus 0 1 'ffi::default-fg)))
    (ok (= 1 (cell-slot viscus 0 1 'ffi::default-bg)))
    (ok (= 1 (cell-slot viscus 0 2 'ffi::default-fg)))
    (ok (= 0 (cell-slot viscus 0 2 'ffi::default-bg)))))

(defun scrollback-count-after (script)
  "Run SCRIPT in a registered terminal (so the scrollback callbacks reach it)
and return how many scrollback lines it kept."
  (let* ((id 424242)
         (viscus (ffi::terminal-new "" id 24 80
                                    :program "/bin/sh"
                                    :argv (list "/bin/sh" "-c" script)))
         (terminal (make-instance 'lem-terminal/terminal::terminal
                                  :id id :viscus viscus
                                  :rows 24 :cols 80 :buffer nil)))
    (setf (lem-terminal/terminal::terminal-scrollback-ring terminal)
          (make-array 100 :initial-element nil))
    (lem-terminal/terminal::add-terminal terminal)
    (unwind-protect
         (progn (drain-until-eof viscus)
                (lem-terminal/terminal:scrollback-count terminal))
      (lem-terminal/terminal::remove-terminal terminal)
      (ffi::terminal-delete viscus))))

(deftest clear-discards-scrollback
  (if (library-available-p)
      (progn
        (ok (plusp (scrollback-count-after "seq 1 60")))
        (ok (zerop (scrollback-count-after "seq 1 60; printf '\\033[3J'"))))
      (skip "terminal.so is not built")))

(deftest window-title-is-recorded
  (with-test-terminal (viscus "printf '\\033]0;first\\007'; printf '\\033]2;日本語 title\\007'")
    (ok (drain-until-eof viscus))
    (ok (equal "日本語 title" (ffi::terminal-title viscus)))))

;;; Scrolling a terminal buffer with the mouse wheel, while output goes on.

(defun wait-until (predicate &key (timeout-seconds 5))
  (loop :repeat (* timeout-seconds 50)
        :until (funcall predicate)
        :do (sleep 0.02))
  (funcall predicate))

(defun first-line (buffer)
  (lem:with-point ((point (lem:buffer-start-point buffer)))
    (lem:line-string point)))

(defun buffer-has-p (buffer string)
  (lem:with-point ((point (lem:buffer-start-point buffer)))
    (lem:search-forward point string)))

(defun run-next-command ()
  "Read the next command and run it as the command loop does, with command
flags of its own.  For a wheel it is the bypass command <mouse-event>, which
scrolls from inside it."
  (lem:do-command-loop ()
    (lem:call-command (lem:read-command) nil)
    (return)))

(defun wheel (window wheel-y)
  "Turn the mouse wheel over WINDOW as a frontend does (up when WHEEL-Y is 1)."
  (lem:receive-mouse-wheel (lem:window-x window) (lem:window-y window) 0 0 0 wheel-y)
  (run-next-command))

(defun buffer-terminal (buffer)
  (lem-terminal/terminal-mode::buffer-terminal buffer))

(defun render (buffer)
  "Render BUFFER's terminal as the timer the I/O thread arms would (its
internal RENDER-TICK, which MANUAL-TIMER-MANAGER keeps from running)."
  (lem-terminal/terminal::render-tick (buffer-terminal buffer)))

(defun output-pending-p (buffer)
  "True when output not rendered yet has arrived for BUFFER's terminal."
  (lem-terminal/terminal::any-rows-dirty-p (buffer-terminal buffer)))

(defun copy-mode-p (buffer)
  (lem-terminal/terminal::terminal-copy-mode (buffer-terminal buffer)))

(defun wait-for-output (buffer &optional (text (format nil "100~%x")))
  (wait-until (lambda ()
                (render buffer)
                (buffer-has-p buffer text))))

(defun call-with-wheel-terminal (function &key (script *wheel-test-script*))
  "Call FUNCTION with a terminal buffer in the current window, running SCRIPT
(which starts by printing 100 lines), and the screen's rows."
  (if (library-available-p)
      (lem:with-current-buffers ()
        (lem-fake-interface:with-fake-interface ()
          (lem/common/timer:with-timer-manager (make-instance 'manual-timer-manager)
            (let* ((buffer (lem:make-buffer "*terminal wheel test*"))
                   (window (lem:current-window))
                   (rows (1- (lem:window-height window)))
                   (argv (list "/bin/sh" "-c" script)))
              (lem:switch-to-buffer buffer)
              (lem-terminal/terminal-mode::terminal-mode)
              (setf (lem:buffer-value buffer 'lem-terminal/terminal-mode::terminal-command)
                    (list "/bin/sh" argv)
                    (lem-terminal/terminal-mode::buffer-terminal buffer)
                    (lem-terminal/terminal:create
                     :rows rows :cols (lem:window-width window)
                     :buffer buffer :directory "" :program "/bin/sh" :argv argv))
              (unwind-protect (funcall function buffer window rows)
                (lem-terminal/terminal:destroy (buffer-terminal buffer)))))))
      (skip "terminal.so is not built")))

(deftest wheel-scrolls-the-scrollback-while-output-goes-on
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (ok (wait-for-output buffer))
     (wheel window 1)
     (ok (equal "1" (first-line buffer)) "the scrollback is shown")
     (let ((view (lem:line-number-at-point (lem:window-view-point window))))
       (ok (< view (- (lem:buffer-nlines buffer) rows)) "the window shows it")
       (ok (wait-until (lambda () (output-pending-p buffer))) "output arrives")
       (render buffer)
       (ok (equal "1" (first-line buffer)) "output does not take it away")
       (ok (= view (lem:line-number-at-point (lem:window-view-point window)))))
     (loop :repeat 50 :do (wheel window -1))
     (ok (= rows (lem:buffer-nlines buffer)) "back at the bottom, only the screen")
     (ok (= 1 (lem:line-number-at-point (lem:window-view-point window))))
     (ok (not (lem-terminal/terminal::terminal-copy-mode (buffer-terminal buffer)))
         "the terminal renders again"))))

(deftest wheel-down-at-the-bottom-leaves-the-buffer-alone
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore rows))
     (ok (wait-for-output buffer))
     (let ((tick (lem:buffer-modified-tick buffer)))
       (wheel window -1)
       (ok (= tick (lem:buffer-modified-tick buffer))
           "the scrollback is not put in and taken out again")))))

(deftest wheel-scrolls-after-terminal-restart
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore rows))
     (ok (wait-for-output buffer))
     (wheel window 1)
     ;; the process exits while the buffer is scrolled
     (lem-terminal/terminal::finish-process (buffer-terminal buffer) 0)
     (lem-terminal/terminal-mode::terminal-restart)
     (ok (wait-for-output buffer))
     (wheel window 1)
     (ok (equal "1" (first-line buffer)) "the new process's scrollback is shown"))))

(defun paste (text)
  "Paste TEXT as a frontend does."
  (lem:paste-using-mode (lem:ensure-mode-object
                         (lem:current-major-mode-at-point (lem:current-point)))
                        text))

(deftest wheel-shows-each-line-once-while-lines-scroll-off
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore rows))
     (ok (wait-for-output buffer (format nil "100~%L1~%")))
     ;; lines go on scrolling off the screen into the scrollback, unrendered
     (sleep 0.2)
     (wheel window 1)
     (let ((lines (uiop:split-string (lem:buffer-text buffer) :separator '(#\Newline))))
       (ok (= (length (remove "" lines :test #'equal))
              (length (remove-duplicates (remove "" lines :test #'equal) :test #'equal)))
           "no line is shown twice where the scrollback meets the screen")))
   :script "seq 1 100; i=1; while :; do echo L$i; i=$((i+1)); sleep 0.01; done"))

(deftest paste-while-scrolled-shows-the-screen
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (ok (wait-for-output buffer))
     (wheel window 1)
     (paste "y")
     (ok (= rows (lem:buffer-nlines buffer)) "the scrollback is gone")
     (ok (= 1 (lem:line-number-at-point (lem:window-view-point window))))
     (ok (not (lem-terminal/terminal::terminal-copy-mode (buffer-terminal buffer)))
         "the terminal renders again"))))

(deftest wheel-before-the-first-render-does-not-freeze
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore rows))
     ;; the buffer has one line yet, and nothing has scrolled off the screen
     (wheel window 1)
     (ok (not (lem-terminal/terminal::terminal-copy-mode (buffer-terminal buffer)))
         "nothing to show above the screen, so the terminal renders on")
     (ok (wait-for-output buffer (format nil "1~%2~%")) "output is rendered"))
   :script "sleep 0.3; seq 1 3; sleep 30"))

(deftest a-key-sent-from-m-x-while-scrolled-shows-the-screen
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (ok (wait-for-output buffer))
     (wheel window 1)
     ;; M-x runs the command through execute-command, a bypass command
     (let ((lem-terminal/terminal-mode::*running-bypass-command* t))
       (lem:call-command 'lem-terminal/terminal-mode::terminal-key-return nil))
     (ok (= rows (lem:buffer-nlines buffer)) "the scrollback is gone")
     (ok (not (lem-terminal/terminal::terminal-copy-mode (buffer-terminal buffer)))
         "the terminal renders again"))))

(deftest a-key-sent-from-m-x-in-copy-mode-keeps-the-history
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore window))
     (ok (wait-for-output buffer (format nil "100~%")))
     (lem-terminal/terminal-mode::terminal-copy-mode)
     (let ((nlines (lem:buffer-nlines buffer)))
       (ok (< rows nlines) "copy mode shows the history")
       ;; terminal-copy-mode does not inherit terminal-mode: the command runs
       ;; as in any other buffer.
       (lem:call-command 'lem-terminal/terminal-mode::terminal-key-return nil)
       (ok (= nlines (lem:buffer-nlines buffer)) "the history stays")
       (ok (copy-mode-p buffer) "the terminal stays frozen")
       (lem-terminal/terminal-mode::terminal-copy-mode-off)
       (ok (wait-for-output buffer "got Return") "the process got the key")))
   :script "seq 1 100; read line; echo got Return; sleep 30"))

(defun view-line (window)
  (lem:line-number-at-point (lem:window-view-point window)))

(defun shows-history-p (buffer window rows)
  "True when WINDOW starts above the screen, in the history."
  (<= (view-line window) (- (lem:buffer-nlines buffer) rows)))

(deftest wheel-up-from-a-view-below-the-first-row-shows-the-history
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (ok (wait-for-output buffer))
     ;; as moving down the screen in C-x [ and leaving it with Escape does
     (lem:move-to-line (lem:window-view-point window) 10)
     (wheel window 1)
     (ok (copy-mode-p buffer) "the terminal is frozen")
     (ok (shows-history-p buffer window rows) "the window shows the history"))))

(deftest an-error-in-the-scroll-does-not-leave-the-terminal-frozen
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (ok (wait-for-output buffer))
     (ok (signals (lem-terminal/terminal-mode::call-with-scrollback
                   (buffer-terminal buffer) window nil
                   (lambda () (error "the scroll failed")))))
     (ok (not (copy-mode-p buffer)) "the terminal renders again")
     (ok (= rows (lem:buffer-nlines buffer)) "the scrollback is gone"))))

(deftest wheel-down-on-the-screen-leaves-the-other-window-in-the-history
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore rows))
     (ok (wait-for-output buffer))
     (lem:split-window-vertically window)
     (let ((other (lem:get-next-window window))
           (terminal (buffer-terminal buffer)))
       (lem-terminal/terminal-mode::resize-terminal terminal window)
       (ok (wait-for-output buffer))
       (let ((rows (lem-terminal/terminal::terminal-rows terminal)))
         (wheel window 1)
         (ok (shows-history-p buffer window rows) "the first window shows the history")
         (wheel other -1)
         (ok (shows-history-p buffer window rows) "it still does")
         (ok (copy-mode-p buffer) "the terminal stays frozen")
         (ng (shows-history-p buffer other rows) "the other window stays on the screen")
         (loop :repeat 50 :do (wheel window -1))
         (ok (not (copy-mode-p buffer)) "both on the screen, the terminal renders again")
         (ok (= rows (lem:buffer-nlines buffer))))))))

(deftest the-process-exiting-while-the-history-is-read-shows-the-end
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (ok (wait-for-output buffer))
     (wheel window 1)
     (lem-terminal/terminal::finish-process (buffer-terminal buffer) 0)
     (ok (not (copy-mode-p buffer)) "scrolling ends")
     (ok (not (lem-terminal/terminal::terminal-scrollback-active (buffer-terminal buffer)))
         "the scrollback is gone")
     (ok (<= (lem:buffer-nlines buffer) (+ rows 2)) "the final screen and the notice")
     (ok (< (- (lem:buffer-nlines buffer) (view-line window)) (lem:window-height window))
         "the window shows them")
     (ok (buffer-has-p buffer "[Process exited with code 0]"))
     ;; C-x [ and Escape after the exit
     (lem-terminal/terminal-mode::terminal-copy-mode)
     (let ((text (lem:buffer-text buffer)))
       (ok (equal "1" (first-line buffer)) "copy mode shows the scrollback")
       (lem-terminal/terminal-mode::terminal-copy-mode-off)
       (ok (equal text (lem:buffer-text buffer))
           "leaving it edits nothing (the point is not put on the cursor row)")))))

(deftest wheel-up-without-history-redraws-nothing
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore rows))
     (ok (wait-for-output buffer (format nil "1~%2~%3")))
     (wheel window 1)
     (ok (not (copy-mode-p buffer)))
     (ok (not (output-pending-p buffer)) "no row is to be drawn again"))
   :script "printf '1\\n2\\n3'; sleep 30"))

(deftest a-scroll-of-zero-lines-leaves-the-buffer-alone
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore window rows))
     (ok (wait-for-output buffer))
     (let ((tick (lem:buffer-modified-tick buffer)))
       (lem:call-command 'lem:scroll-down 0)
       (ok (= tick (lem:buffer-modified-tick buffer))
           "the scrollback is not put in and taken out again")
       (ok (not (copy-mode-p buffer)))))))

(deftest paste-in-a-mode-made-from-terminal-mode-shows-the-screen
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (ok (wait-for-output buffer))
     (derived-terminal-mode)
     (wheel window 1)
     (ok (copy-mode-p buffer) "the wheel shows the history")
     (paste "y")
     (ok (= rows (lem:buffer-nlines buffer)) "the scrollback is gone")
     (ok (not (copy-mode-p buffer)) "the terminal renders again"))))

(deftest copy-mode-on-again-keeps-the-frozen-screen
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore window rows))
     (ok (wait-for-output buffer "ready"))
     (let ((terminal (buffer-terminal buffer)))
       (ok (= 0 (lem-terminal/terminal:copy-mode-on terminal))
           "nothing above the screen yet")
       (let ((screen (lem:buffer-text buffer)))
         (lem-terminal/terminal:input-key terminal ffi::vterm_key_enter)
         (ok (wait-until (lambda ()
                           (plusp (lem-terminal/terminal:scrollback-count terminal))))
             "lines go above the screen, unrendered")
         (lem-terminal/terminal:copy-mode-on terminal)
         (ok (uiop:string-suffix-p (lem:buffer-text buffer) screen)
             "the screen shown stays as it was"))))
   :script "printf ready; read line; seq 1 100; sleep 30"))

(deftest copy-mode-on-while-the-history-is-shown-counts-it
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (ok (wait-for-output buffer))
     (wheel window 1)
     (ok (= (- (lem:buffer-nlines buffer) rows)
            (lem-terminal/terminal:copy-mode-on (buffer-terminal buffer)))))))

(deftest wheel-up-after-the-exit-shows-the-scrollback
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore rows))
     (ok (wait-for-output buffer))
     (lem-terminal/terminal::finish-process (buffer-terminal buffer) 0)
     (ng (equal "1" (first-line buffer)) "the exit shows the final screen")
     ;; as M-v does: the wheel, from within <mouse-event>, the same
     (wheel window 1)
     (ok (equal "1" (first-line buffer)) "the scrollback is put above it")
     (let ((nlines (lem:buffer-nlines buffer)))
       (wheel window 1)
       (ok (= nlines (lem:buffer-nlines buffer)) "once")))))

(deftest wheel-up-in-a-side-window-shows-the-history
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore window))
     (ok (wait-for-output buffer))
     (lem:make-rightside-window buffer)
     (let ((side (lem:frame-rightside-window (lem:current-frame))))
       (unwind-protect
            (progn
              (wheel side 1)
              (ok (copy-mode-p buffer) "the terminal is frozen")
              (ok (shows-history-p buffer side rows) "the side window shows the history")
              (ng (= 1 (view-line side)) "a few lines above the screen, not its oldest line"))
         (lem:delete-rightside-window))))))

(defun call-with-frame-multiplexer (function)
  "Call FUNCTION with frame-multiplexer on, on a table of tabs of its own (as
tests/frame-multiplexer.lisp does)."
  (let ((lem/frame-multiplexer::*virtual-frame-map* (make-hash-table))
        (lem/frame-multiplexer::*recent-list* nil)
        (lem:*frame-buffer-list-function* nil))
    (lem/common/var:with-global-variable-value (lem/frame-multiplexer::frame-multiplexer nil)
      (lem/frame-multiplexer::enable-frame-multiplexer)
      ;; lay the windows out below the tab bar, as the next redisplay would
      (lem:redraw-display)
      (let ((first (lem:current-frame)))
        (unwind-protect (funcall function)
          ;; Turning the tabs off keeps the tab shown: the first one, which
          ;; has the window of CALL-WITH-WHEEL-TERMINAL, also when FUNCTION
          ;; failed in another tab.
          (when (and (lem/frame-multiplexer::enabled-frame-multiplexer-p)
                     (not (eq first (lem:current-frame))))
            (ignore-errors (lem/frame-multiplexer:frame-multiplexer-switch 0)))
          (lem/frame-multiplexer::disable-frame-multiplexer))))))

(deftest wheel-in-another-tab-leaves-this-tab-in-the-history
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore rows))
     (ok (wait-for-output buffer))
     (call-with-frame-multiplexer
      (lambda ()
        (let ((terminal (buffer-terminal buffer)))
          ;; the tab bar took a row
          (lem-terminal/terminal-mode::resize-terminal terminal window)
          (ok (wait-for-output buffer))
          (let ((rows (lem-terminal/terminal::terminal-rows terminal)))
            (wheel window 1)
            (let ((text (lem:line-string (lem:window-view-point window))))
              (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
              (lem:switch-to-buffer buffer)
              (let ((other (lem:current-window)))
                (wheel other 1)
                (loop :repeat 50 :do (wheel other -1))
                (ng (shows-history-p buffer other rows) "the other tab is on the screen")
                (ok (copy-mode-p buffer) "the terminal stays frozen")
                (ok (shows-history-p buffer window rows)
                    "the first tab's window still shows the history")
                (ok (equal text (lem:line-string (lem:window-view-point window)))
                    "the same line of it"))
              (lem/frame-multiplexer:frame-multiplexer-prev)
              (loop :repeat 50 :do (wheel window -1))
              (ok (not (copy-mode-p buffer))
                  "every window on the screen: the terminal runs on")))))))))

(deftest the-point-of-another-window-stays-on-its-line
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore rows))
     ;; (no output after the 100 lines: the screen rows keep their text)
     (ok (wait-for-output buffer (format nil "99~%100~%")))
     (lem:split-window-vertically window)
     ;; the terminal fits the smaller windows now
     (ok (wait-for-output buffer (format nil "99~%100~%")))
     (let* ((other (lem:get-next-window window))
            (point (lem:window-point other)))
       ;; at the start of the buffer, where the scrollback goes in
       (lem:buffer-start point)
       (let ((text (lem:line-string point)))
         (wheel window 1)
         (ok (copy-mode-p buffer) "the scrollback is put in")
         (ok (equal text (lem:line-string (lem:window-point other)))
             "the other window's point is still on its line"))))
   :script "seq 1 100; sleep 30"))

(defun call-with-history-read-in-another-tab (function)
  "Call FUNCTION with BUFFER, its terminal shown on the screen in the first
tab's WINDOW, while a second tab, the current one, reads its history."
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore rows))
     (ok (wait-for-output buffer))
     (call-with-frame-multiplexer
      (lambda ()
        (lem-terminal/terminal-mode::resize-terminal (buffer-terminal buffer) window)
        (ok (wait-for-output buffer))
        (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
        (lem:switch-to-buffer buffer)
        (let ((reader (lem:current-window)))
          (lem-terminal/terminal-mode::show-screen-top
           reader (- (lem:buffer-nlines buffer)
                     (lem-terminal/terminal::terminal-rows (buffer-terminal buffer))))
          (wheel reader 1)
          (ok (copy-mode-p buffer) "the second tab reads the history"))
        (funcall function buffer window))))))

(deftest deleting-the-tab-reading-the-history-lets-the-terminal-run-on
  (call-with-history-read-in-another-tab
   (lambda (buffer window)
     (declare (ignore window))
     (lem:call-command 'lem/frame-multiplexer:frame-multiplexer-delete nil)
     (ok (not (copy-mode-p buffer)) "the first tab shows the screen: the terminal runs on"))))

(deftest turning-the-tabs-off-lets-the-terminal-run-on
  (call-with-history-read-in-another-tab
   (lambda (buffer window)
     (declare (ignore window))
     (lem/frame-multiplexer:frame-multiplexer-prev)
     (ok (copy-mode-p buffer) "the second tab, hidden, still reads the history")
     ;; turning the tabs off keeps the first tab, the one shown
     (lem:call-command 'lem/frame-multiplexer:toggle-frame-multiplexer nil)
     (ok (not (copy-mode-p buffer)) "the terminal runs on"))))

(deftest the-tab-that-was-the-only-reader-going-away-lets-the-terminal-run-on
  (call-with-wheel-terminal
   (lambda (buffer window rows)
     (declare (ignore rows))
     (ok (wait-for-output buffer))
     (call-with-frame-multiplexer
      (lambda ()
        (lem-terminal/terminal-mode::resize-terminal (buffer-terminal buffer) window)
        (ok (wait-for-output buffer))
        ;; only the second tab shows the terminal
        (lem:switch-to-buffer (lem:make-buffer "*not the terminal*" :temporary t))
        (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
        (lem:switch-to-buffer buffer)
        (let ((reader (lem:current-window)))
          (lem-terminal/terminal-mode::show-screen-top
           reader (- (lem:buffer-nlines buffer)
                     (lem-terminal/terminal::terminal-rows (buffer-terminal buffer))))
          (wheel reader 1))
        (ok (copy-mode-p buffer) "the second tab reads the history")
        (lem:call-command 'lem/frame-multiplexer:frame-multiplexer-delete nil)
        (ok (null (lem:get-buffer-windows buffer)) "no window shows the terminal")
        (ok (not (copy-mode-p buffer)) "the terminal runs on"))))))
