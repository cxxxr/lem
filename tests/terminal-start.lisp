(defpackage :lem-tests/terminal-start
  (:use :cl :rove))
(in-package :lem-tests/terminal-start)

;;; A terminal that cannot be started (terminal_new returns NULL) must give
;;; an editor-error, not a terminal whose I/O thread reads a null pointer.
;;; It runs out of file descriptors with RLIMIT_NOFILE, whose number (7) and
;;; struct (two unsigned longs) are those of Linux on x86-64 and arm64.

(defconstant +rlimit-nofile+ 7)
(defconstant +emfile+ 24)

(defclass quiet-timer-manager (lem/common/timer:timer-manager) ()
  (:documentation "Drops the renders a started terminal's I/O thread asks for."))

(defmethod lem/common/timer::send-timer-notification
    ((timer-manager quiet-timer-manager) continue)
  (declare (ignore continue)))

(defun library-available-p ()
  "True when terminal.so is loaded and has this failure handling: an older
build aborts in forkpty when it is out of file descriptors."
  (not (null (cffi:foreign-symbol-pointer "terminal_last_error"))))

(defun strerror (errno)
  (cffi:foreign-funcall "strerror" :int errno :string))

(defun terminal-thread-p (id)
  "True when the I/O thread of the terminal with ID is running."
  (let ((name (format nil "Terminal ~D" id)))
    (some (lambda (thread) (equal name (bt2:thread-name thread)))
          (bt2:all-threads))))

(defun call-with-no-free-fd (function)
  "Call FUNCTION while every file descriptor below a lowered soft
RLIMIT_NOFILE is in use, so that any pipe or open fails with EMFILE.
The limit is the process's: other threads get EMFILE meanwhile too, so run
it in a test process, not in a Lem in use."
  (cffi:with-foreign-object (limit :unsigned-long 2)
    (assert (zerop (cffi:foreign-funcall "getrlimit" :int +rlimit-nofile+
                                                     :pointer limit :int)))
    (let ((soft (cffi:mem-aref limit :unsigned-long 0))
          (fillers '()))
      (unwind-protect
           (progn
             (setf (cffi:mem-aref limit :unsigned-long 0) (min soft 1024))
             (assert (zerop (cffi:foreign-funcall "setrlimit" :int +rlimit-nofile+
                                                              :pointer limit :int)))
             (loop :for fd := (cffi:foreign-funcall "open" :string "/dev/null"
                                                    :int 0 :int)
                   :repeat 2048
                   :while (<= 0 fd)
                   :do (push fd fillers))
             (funcall function))
        (dolist (fd fillers)
          (cffi:foreign-funcall "close" :int fd :int))
        (setf (cffi:mem-aref limit :unsigned-long 0) soft)
        (cffi:foreign-funcall "setrlimit" :int +rlimit-nofile+ :pointer limit :int)))))

(deftest a-terminal-that-cannot-start-is-an-editor-error
  #-(and linux (or x86-64 arm64)) (skip "needs Linux on x86-64 or arm64")
  #+(and linux (or x86-64 arm64))
  (if (library-available-p)
      (let ((buffer (lem:make-buffer "*terminal start test*" :temporary t))
            (terminals (length lem-terminal/terminal::*terminals*))
            (terminal nil)
            (condition nil))
        (unwind-protect
             (call-with-no-free-fd
              (lambda ()
                (handler-case
                    (setf terminal
                          (lem-terminal/terminal:create
                           :rows 24 :cols 80 :buffer buffer
                           :directory "" :program "/bin/sh"
                           :argv (list "/bin/sh" "-c" "exit 0")))
                  (lem:editor-error (c) (setf condition c)))))
          (when terminal
            (lem-terminal/terminal:destroy terminal)))
        (ok condition "create signals editor-error")
        (when condition
          (ok (search (strerror +emfile+) (princ-to-string condition))
              (format nil "the message gives the reason: ~A" condition)))
        (ok (= terminals (length lem-terminal/terminal::*terminals*))
            "no terminal is registered")
        (ok (not (terminal-thread-p lem-terminal/terminal::*terminal-id-counter*))
            "no I/O thread is started"))
      (skip "terminal.so is not built or is older than its failure handling")))

(deftest a-failed-terminal-restart-leaves-the-buffer-as-it-was
  #-(and linux (or x86-64 arm64)) (skip "needs Linux on x86-64 or arm64")
  #+(and linux (or x86-64 arm64))
  (if (library-available-p)
      (lem:with-current-buffers ()
        (lem-fake-interface:with-fake-interface ()
         (lem/common/timer:with-timer-manager (make-instance 'quiet-timer-manager)
          (let* ((buffer (lem:make-buffer "*terminal restart test*"))
                 (old (lem-terminal/terminal:create
                       :rows 24 :cols 80 :buffer buffer :directory ""
                       :program "/bin/sh" :argv (list "/bin/sh" "-c" "sleep 10")))
                 (condition nil)
                 (text nil))
            (lem:switch-to-buffer buffer)
            (lem-terminal/terminal-mode::terminal-mode)
            (setf (lem-terminal/terminal-mode::buffer-terminal buffer) old
                  ;; as when the process has exited, which terminal-restart needs
                  (lem-terminal/terminal::terminal-exit-status old) 0)
            (lem:with-buffer-read-only buffer nil
              (lem:insert-string (lem:buffer-point buffer) "the old output"))
            (setf text (lem:buffer-text buffer))
            ;; restarted from copy mode, as after C-x [
            (lem-terminal/terminal-mode::terminal-copy-mode)
            (unwind-protect
                 (progn
                   (call-with-no-free-fd
                    (lambda ()
                      (handler-case (lem-terminal/terminal-mode::terminal-restart)
                        (lem:editor-error (c) (setf condition c)))))
                   (ok condition "terminal-restart signals editor-error")
                   (ok (eq old (lem-terminal/terminal-mode::buffer-terminal buffer))
                       "the buffer keeps its terminal")
                   (ok (not (lem-terminal/terminal::terminal-dead old))
                       "which is not destroyed")
                   (ok (equal text (lem:buffer-text buffer)) "the old output stays")
                   (lem-terminal/terminal-mode::terminal-restart)
                   (let ((new (lem-terminal/terminal-mode::buffer-terminal buffer)))
                     (ok (and new (not (eq new old))
                              (not (lem-terminal/terminal:process-exited-p new)))
                         "terminal-restart starts it once descriptors are free")
                     (ok (lem-terminal/terminal::terminal-dead old)
                         "and destroys the old terminal")
                     (ok (eq 'lem-terminal/terminal-mode::terminal-mode
                             (lem:buffer-major-mode buffer))
                         "the buffer leaves copy mode, so keys go to the shell")))
              (lem-terminal/terminal:destroy old)
              (alexandria:when-let
                  ((new (lem-terminal/terminal-mode::buffer-terminal buffer)))
                (lem-terminal/terminal:destroy new)))))))
      (skip "terminal.so is not built or is older than its failure handling")))

(deftest a-terminal-that-cannot-start-leaves-no-buffer
  #-(and linux (or x86-64 arm64)) (skip "needs Linux on x86-64 or arm64")
  #+(and linux (or x86-64 arm64))
  (if (library-available-p)
      (lem:with-current-buffers ()
        (lem-fake-interface:with-fake-interface ()
         (lem/common/timer:with-timer-manager (make-instance 'quiet-timer-manager)
          (let ((buffers (lem:buffer-list))
                (terminals lem-terminal/terminal::*terminals*)
                (errors 0))
            (unwind-protect
                 (call-with-no-free-fd
                  (lambda ()
                    (handler-case (lem-terminal/terminal-mode::terminal t)
                      (lem:editor-error () (incf errors)))
                    (handler-case (lem-terminal/terminal-mode::create-terminal-with-command
                                   "/bin/sh" (list "/bin/sh"))
                      (lem:editor-error () (incf errors)))))
              ;; terminals that started after all are not left running
              (dolist (terminal (set-difference lem-terminal/terminal::*terminals*
                                                terminals))
                (lem-terminal/terminal:destroy terminal)))
            (ok (= 2 errors) "M-x terminal and a command terminal signal editor-error")
            (ok (equal buffers (lem:buffer-list)) "no *Terminal* buffer is left")))))
      (skip "terminal.so is not built or is older than its failure handling")))

(deftest terminal-restart-leaves-a-buffer-that-never-had-a-terminal
  (lem:with-current-buffers ()
    (lem-fake-interface:with-fake-interface ()
      (let ((buffer (lem:make-buffer "*terminal restart text*")))
        (lem:switch-to-buffer buffer)
        (lem:insert-string (lem:buffer-point buffer) "unsaved text")
        (lem-terminal/terminal-mode::terminal-mode)
        (ok (signals (lem-terminal/terminal-mode::terminal-restart) 'lem:editor-error))
        (ok (equal "unsaved text" (lem:buffer-text buffer)) "its text is kept")))))

(defun shell-running-p (marker)
  "True when a child of this process (a shell a terminal started, running or
not yet reaped) has MARKER in its command line.  Children only: another
process may carry the same text."
  (let ((self (cffi:foreign-funcall "getpid" :int)))
    (dolist (dir (directory #p"/proc/*/") nil)
      (let* ((pid (car (last (pathname-directory dir))))
             (stat (ignore-errors
                    (uiop:read-file-string (merge-pathnames "stat" dir))))
             (cmdline (ignore-errors
                       (uiop:read-file-string (merge-pathnames "cmdline" dir)))))
        (when (and stat cmdline (every #'digit-char-p pid))
          ;; stat is "pid (comm) state ppid ...": the comm may hold spaces
          (let* ((after-comm (subseq stat (1+ (position #\) stat :from-end t))))
                 (ppid (parse-integer (second (uiop:split-string
                                               (string-trim " " after-comm)))
                                      :junk-allowed t)))
            (when (and (eql ppid self)
                       (search marker (substitute #\Space (code-char 0) cmdline)))
              (return t))))))))

(defun wait-until-gone (marker)
  (loop :repeat 100
        :while (shell-running-p marker)
        :do (sleep 0.02))
  (not (shell-running-p marker)))

(deftest a-start-that-fails-after-the-fork-leaves-no-shell
  (if (library-available-p)
      (let* ((marker "sleep 31.4159")
             (buffer (lem:make-buffer "*terminal orphan test*" :temporary t))
             (terminals lem-terminal/terminal::*terminals*)
             (failed nil))
        ;; fails in create after terminal_new has started the shell
        (let ((lem-terminal/terminal::*scrollback-limit* -1))
          (handler-case
              (lem-terminal/terminal:create :rows 24 :cols 80 :buffer buffer
                                            :directory "" :program "/bin/sh"
                                            :argv (list "/bin/sh" "-c" marker))
            (error () (setf failed t))))
        (ok failed "create signals")
        (ok (wait-until-gone marker) "the shell is killed")
        (ok (eq terminals lem-terminal/terminal::*terminals*) "no terminal is registered"))
      (skip "terminal.so is not built or is older than its failure handling")))

(deftest a-terminal-buffer-whose-setup-fails-leaves-no-shell
  (if (library-available-p)
      (lem:with-current-buffers ()
        (lem-fake-interface:with-fake-interface ()
         (lem/common/timer:with-timer-manager (make-instance 'quiet-timer-manager)
          (let ((marker "sleep 27.1828")
                (buffers (lem:buffer-list))
                (terminals lem-terminal/terminal::*terminals*)
                (failed nil))
            (handler-case
                (lem-terminal/terminal-mode::call-with-new-terminal-buffer
                 "*Terminal*" (list :directory "" :program "/bin/sh"
                                    :argv (list "/bin/sh" "-c" marker))
                 (lambda (buffer)
                   (declare (ignore buffer))
                   (error "setting the buffer up failed")))
              (error () (setf failed t)))
            (ok failed "the error goes on")
            (ok (wait-until-gone marker) "the shell is killed")
            (ok (equal terminals lem-terminal/terminal::*terminals*)
                "the terminal is not registered")
            (ok (equal buffers (lem:buffer-list)) "the buffer is deleted")))))
      (skip "terminal.so is not built or is older than its failure handling")))
