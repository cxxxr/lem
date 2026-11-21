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

(defclass manual-timer-manager (lem/common/timer:timer-manager) ()
  (:documentation "Drops the renders the I/O thread asks for; the test renders
itself, as the timer would, with RENDER-TICK."))

(defmethod lem/common/timer::send-timer-notification
    ((timer-manager manual-timer-manager) continue)
  (declare (ignore continue)))

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

(defun wheel (window wheel-y)
  "Turn the mouse wheel over WINDOW as a frontend does (up when WHEEL-Y is 1),
as a command of its own: the flags of the previous one are not carried on."
  (let ((lem-core::*last-flags* nil)
        (lem-core::*curr-flags* nil))
    (lem-core::handle-mouse-event
     (make-instance 'lem-core::mouse-wheel
                    :x (lem:window-x window) :y (lem:window-y window)
                    :pixel-x 0 :pixel-y 0 :wheel-x 0 :wheel-y wheel-y))))

(defun buffer-terminal (buffer)
  (lem-terminal/terminal-mode::buffer-terminal buffer))

(defun render (buffer)
  "Render BUFFER's terminal as the timer the I/O thread arms would."
  (lem-terminal/terminal::render-tick (buffer-terminal buffer)))

(defun wait-for-output (buffer)
  (wait-until (lambda ()
                (render buffer)
                (buffer-has-p buffer (format nil "100~%x")))))

(defun call-with-wheel-terminal (function)
  "Call FUNCTION with a terminal buffer in the current window, running a
program that prints 100 lines and then keeps printing, and the screen's rows."
  (if (library-available-p)
      (lem:with-current-buffers ()
        (lem-fake-interface:with-fake-interface ()
          (lem/common/timer:with-timer-manager (make-instance 'manual-timer-manager)
            (let* ((buffer (lem:make-buffer "*terminal wheel test*"))
                   (window (lem:current-window))
                   (rows (1- (lem:window-height window)))
                   (argv (list "/bin/sh" "-c"
                               "seq 1 100; while :; do printf x; sleep 0.02; done")))
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
       (sleep 0.1)
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
