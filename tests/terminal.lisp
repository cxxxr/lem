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

(defun parse-until-row-contains (viscus string &key (row 0) (timeout-seconds 5))
  "Parse PTY output until screen ROW contains STRING.  Returns T when it does,
NIL on timeout."
  (loop :with deadline := (+ (get-internal-real-time)
                             (* timeout-seconds internal-time-units-per-second))
        :until (search string (row-string viscus row))
        :when (> (get-internal-real-time) deadline)
          :do (return-from parse-until-row-contains nil)
        :do (ffi::terminal-process-input-timed viscus 10)
            (sleep 0.01))
  t)

(deftest paste-is-bracketed-and-uses-cr
  ;; The program enables bracketed paste, then dumps the 15 bytes it reads:
  ;; ESC[200~ a CR b ESC[201~
  (with-test-terminal (viscus "stty raw -echo; printf '\\033[?2004hready'; dd bs=1 count=15 2>/dev/null | od -An -c | tr -d ' \\n'; echo")
    ;; "ready" comes after stty, so the PTY is raw before anything is pasted.
    (ok (parse-until-row-contains viscus "ready"))
    (let ((terminal (make-instance 'lem-terminal/terminal::terminal
                                   :id *test-terminal-id* :viscus viscus
                                   :rows 24 :cols 80 :buffer nil)))
      (lem-terminal/terminal:paste-string terminal (format nil "a~%b")))
    (ok (drain-until-eof viscus))
    (ok (search "033[200~a\\rb033[201~" (row-string viscus 0)))))
