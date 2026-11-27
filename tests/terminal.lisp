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
