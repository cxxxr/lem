(defpackage :lem-tests/incident-recorder
  (:use :cl :rove)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:local-nicknames (:ir :lem-incident-recorder)))
(in-package :lem-tests/incident-recorder)

(defun call-with-recorder (function)
  ;; Each test gets its own incident directory and recorder state, and only
  ;; the recorder on *error-report-hook*, so that running the tests inside a
  ;; Lem leaves its incidents and its hooks as they were.
  (let* ((directory (uiop:ensure-directory-pathname
                     (merge-pathnames (format nil "lem-incident-test-~36R/" (random (expt 36 8)))
                                      (uiop:temporary-directory))))
         (ir:*incident-directory* directory)
         (ir:*incident-recording-enabled* t)
         (lem:*error-report-hook* '())
         (ir::*recent-commands* '())
         (ir::*incident-count* 0)
         (ir::*last-written-times* (make-hash-table :test 'equal))
         (ir::*suppressed-counts* (make-hash-table :test 'equal))
         (lem-core::*last-reported-condition* nil))
    ;; registered with add-hook, as the extension does, not bound as a list
    (lem:add-hook lem:*error-report-hook* 'ir:record-incident)
    (unwind-protect (funcall function directory)
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defmacro with-recorder ((directory) &body body)
  `(call-with-recorder (lambda (,directory)
                         (declare (ignorable ,directory))
                         ,@body)))

(defun incident-files (directory)
  (directory (merge-pathnames "new/*.json" directory)))

(defun read-incident (pathname)
  (with-open-file (in pathname :external-format :utf-8)
    (yason:parse in)))

(defun signal-and-capture (function)
  "Call FUNCTION, which signals an error, and return the condition and the
backtrace taken where it was signalled, as the editor's handlers do."
  (let (condition backtrace)
    (block nil
      (handler-bind ((error (lambda (c)
                              (setf condition c
                                    backtrace (with-output-to-string (s)
                                                (uiop:print-backtrace :stream s :count 100)))
                              (return))))
        (funcall function)))
    (values condition backtrace)))

(defun fail-in-place-a (message) (error "a: ~A" message))
(defun fail-in-place-b (message) (error "b: ~A" message))

(deftest fingerprint-ignores-the-message-but-not-the-place
  (flet ((fingerprint (function message)
           (multiple-value-bind (condition backtrace)
               (signal-and-capture (lambda () (funcall function message)))
             (ir:incident-fingerprint condition backtrace))))
    (ok (equal (fingerprint 'fail-in-place-a "one")
               (fingerprint 'fail-in-place-a "two")))
    (ng (equal (fingerprint 'fail-in-place-a "one")
               (fingerprint 'fail-in-place-b "one")))))

(deftest frame-function-name
  (ok (equal "LEM-CORE::FOO" (ir::frame-function-name "3: (LEM-CORE::FOO #<BUFFER a> 1)")))
  (ok (equal "(LAMBDA (X) :IN LEM-CORE::BAR)"
             (ir::frame-function-name "12: ((LAMBDA (X) :IN LEM-CORE::BAR) 1)")))
  (ok (equal "LEM-CORE::BAZ" (ir::frame-function-name "0: (LEM-CORE::BAZ)")))
  (ok (null (ir::frame-function-name "Backtrace for: #<THREAD>"))))

(deftest long-strings-in-the-backtrace-are-cut
  (let* ((text (make-string 200 :initial-element #\x))
         (frame (format nil "1: (LEM::INSERT-STRING #<POINT> ~S)" text))
         (cut (ir::sanitize-backtrace frame)))
    (ok (search "(200 chars)" cut))
    (ng (search text cut)))
  (ok (search "\"short\"" (ir::sanitize-backtrace "1: (FOO \"short\")"))))

(deftest error-report-hook-errors-are-dropped
  (let ((called '()))
    (let ((lem:*error-report-hook* '()))
      (lem:add-hook lem:*error-report-hook*
                    (lambda (c b x) (declare (ignore c b x)) (error "broken hook"))
                    1)
      (lem:add-hook lem:*error-report-hook*
                    (lambda (c b x) (declare (ignore b)) (push (list c x) called)))
      (ok (progn (lem-core::report-error (make-condition 'simple-error) "" :command) t))
      (ok (= 1 (length called)))
      (ok (eq :command (second (first called)))))))

(deftest pop-up-backtrace-records-an-incident
  (with-fake-interface ()
    (with-recorder (directory)
      (block nil
        (handler-bind ((error (lambda (c)
                                (lem:pop-up-backtrace c)
                                (return))))
          (fail-in-place-a "from a command")))
      (let ((files (incident-files directory)))
        (ok (= 1 (length files)))
        (let ((incident (read-incident (first files))))
          (ok (equal "command" (gethash "context" incident)))
          (ok (search "FAIL-IN-PLACE-A" (gethash "backtrace" incident))))))))

(deftest an-incident-file-describes-the-error
  (with-fake-interface ()
    (with-recorder (directory)
      (lem:execute-key-sequence (list (lem:make-key :sym "s")
                                      (lem:make-key :sym "e")
                                      (lem:make-key :sym "c")))
      (lem:execute-key-sequence (list (lem:make-key :ctrl t :sym "a")))
      (multiple-value-bind (condition backtrace)
          (signal-and-capture (lambda () (fail-in-place-a "detail")))
        (let ((path (ir:record-incident condition backtrace :command)))
          (ok path)
          (ok (equal (list path) (incident-files directory)))
          (let* ((incident (read-incident path))
                 (commands (gethash "recent_commands" incident)))
            (ok (equal "command" (gethash "context" incident)))
            (ok (equal "SIMPLE-ERROR" (gethash "type" (gethash "condition" incident))))
            (ok (equal "a: detail" (gethash "message" (gethash "condition" incident))))
            (ok (equal (ir:incident-fingerprint condition backtrace)
                       (gethash "fingerprint" incident)))
            (ok (search "FAIL-IN-PLACE-A" (gethash "backtrace" incident)))
            ;; three self-inserts become one entry without the typed keys
            (ok (= 2 (length commands)))
            (let ((insert (first commands)))
              (ok (search "SELF-INSERT" (gethash "command" insert)))
              (ok (= 3 (gethash "count" insert)))
              (ok (null (gethash "keys" insert))))
            ;; the keys come from *input-hook*, which keys given to
            ;; execute-key-sequence do not run, so only the command is checked
            (ok (search "MOVE-TO-BEGINNING-OF-LINE" (gethash "command" (second commands))))
            (ok (stringp (gethash "major_mode" (gethash "buffer" incident))))))))))

(deftest the-same-error-is-written-once-per-interval
  (with-recorder (directory)
    (multiple-value-bind (condition backtrace)
        (signal-and-capture (lambda () (fail-in-place-a "again")))
      (ok (ir:record-incident condition backtrace :command))
      (ok (null (ir:record-incident condition backtrace :command)))
      (ok (= 1 (length (incident-files directory))))
      ;; once the interval has passed, the next one says how many were dropped
      (let ((fingerprint (ir:incident-fingerprint condition backtrace)))
        (decf (gethash fingerprint ir::*last-written-times*)
              (1+ ir::*same-fingerprint-interval*)))
      (let ((path (ir:record-incident condition backtrace :command)))
        (ok path)
        (ok (= 1 (gethash "suppressed_since_last" (read-incident path))))))))

(deftest recording-can-be-turned-off
  (with-recorder (directory)
    (let ((ir:*incident-recording-enabled* nil))
      (multiple-value-bind (condition backtrace)
          (signal-and-capture (lambda () (fail-in-place-a "off")))
        (ok (null (ir:record-incident condition backtrace :command)))
        (ok (null (incident-files directory)))))))

(deftest a-timer-error-keeps-the-backtrace-of-the-timer
  (let* ((timer (lem:make-idle-timer (lambda () (fail-in-place-b "in a timer"))
                                     :name "failing timer"))
         (condition (handler-case (progn (lem/common/timer::call-timer-function timer) nil)
                      (lem/common/timer:timer-error (e) e))))
    (ok condition)
    (ok (search "FAIL-IN-PLACE-B" (lem/common/timer:timer-error-backtrace condition)))))

(deftest a-crash-records-an-incident-before-the-editor-exits
  (with-recorder (directory)
    (let ((report
            (block nil
              (handler-bind ((lem-core::exit-editor
                               (lambda (c) (return (lem-core::exit-editor-report c)))))
                (handler-bind ((error #'lem-core::bailout))
                  (fail-in-place-a "crash"))))))
      (ok (search "a: crash" report))
      (ok (search "FAIL-IN-PLACE-A" report))
      (let ((files (incident-files directory)))
        (ok (= 1 (length files)))
        (ok (equal "crash" (gethash "context" (read-incident (first files)))))))))

(deftest typed-keys-are-not-recorded-whatever-the-command
  ;; terminal-input, isearch and completion run on typed text too
  (ok (null (ir::recorded-keys (list (lem:make-key :sym "p") (lem:make-key :sym "w")))))
  (ok (null (ir::recorded-keys (list (lem:make-key :shift t :sym "P")))))
  (ok (equal "C-a" (ir::recorded-keys (list (lem:make-key :ctrl t :sym "a")))))
  (ok (equal "Return" (ir::recorded-keys (list (lem:make-key :sym "Return")))))
  (ok (null (ir::recorded-keys '()))))

(deftest a-string-over-several-lines-is-cut
  (let* ((text (format nil "~{~A~%~}" (loop :for i :from 1 :to 50
                                            :collect (format nil "12: (line ~D" i))))
         (backtrace (format nil "0: (LEM::INSERT-STRING #<POINT> ~S)~%1: (LEM::CALLER)" text))
         (cut (ir::sanitize-backtrace backtrace)))
    (ok (search "chars)" cut))
    (ng (search "line 50" cut))
    ;; the lines of the string are not frames any more
    (ok (= 2 (count-if (lambda (line) (ir::frame-function-name line))
                       (uiop:split-string (string-right-trim '(#\Newline) cut)
                                          :separator '(#\Newline))))))
  (ok (search "#\\\" \"ok\""
              (ir::sanitize-backtrace (format nil "1: (FOO #\\\" \"ok\")~%2: (BAR \"x\")")))))

(define-condition test-error (error) ())

(deftest the-fingerprint-does-not-depend-on-the-current-package
  (multiple-value-bind (condition backtrace)
      (signal-and-capture (lambda () (error 'test-error)))
    (ok (equal (let ((*package* (find-package :cl-user)))
                 (ir:incident-fingerprint condition backtrace))
               (let ((*package* (find-package :lem-tests/incident-recorder)))
                 (ir:incident-fingerprint condition backtrace))))))

(deftest a-timer-error-shown-by-its-handle-function-is-recorded-once
  (with-fake-interface ()
    (with-recorder (directory)
      (let* ((timer (lem:make-idle-timer (lambda () (fail-in-place-b "handled"))
                                         :name "handled timer"
                                         :handle-function (lambda (c) (lem:pop-up-backtrace c))))
             (timer-error (handler-case (progn (lem/common/timer::call-timer-function timer) nil)
                            (lem/common/timer:timer-error (e) e))))
        ;; the command loop shows the timer-error too, as with-error-handler does
        (lem:pop-up-backtrace timer-error)
        (let ((files (incident-files directory)))
          (ok (= 1 (length files)))
          (let ((incident (read-incident (first files))))
            (ok (equal "timer" (gethash "context" incident)))
            (ok (search "FAIL-IN-PLACE-B" (gethash "backtrace" incident)))))))))
