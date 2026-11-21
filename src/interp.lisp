(in-package :lem-core)

(defvar *editor-abort-hook* '())
(defvar *exit-editor-hook* '())

(defvar *error-report-hook* '()
  "Hook (see add-hook) run when an error reaches the editor's own handlers.
Its functions get (CONDITION BACKTRACE CONTEXT): CONTEXT is :COMMAND (shown
in *EDITOR ERROR*), :CRASH (the editor exits), :TIMER or :BACKGROUND-JOB,
and BACKTRACE is a string from lem/common/backtrace:safe-backtrace (one
line per frame, arguments cut). For :BACKGROUND-JOB they run in the job's
thread, not in the editor's. They must not signal: report-error drops
what they signal.")

;; The condition report-error reported last, while a toplevel command loop
;; iteration runs (command-loop binds it to NIL for each); unbound outside of
;; one. A timer's error is reported by the timer's handle-function (as the
;; original error) and again when the timer-error wrapping it reaches the
;; command loop, in the same iteration; this lets the second one be skipped.
;; Bound per iteration so that the condition, and the buffers or strings it
;; refers to, is not kept until the next error.
(defvar *last-reported-condition*)

(defun backtrace-string (condition &key (count 100))
  "Return the backtrace printed by uiop, COUNT frames at most, all of them
if COUNT is NIL."
  (with-output-to-string (stream)
    (uiop:print-backtrace :stream stream
                          :condition condition
                          :count count)))

(defun already-reported-p (condition)
  "Return true if CONDITION, or the error a timer-error CONDITION wraps, is
*last-reported-condition*."
  (and (boundp '*last-reported-condition*)
       *last-reported-condition*
       (or (eq condition *last-reported-condition*)
           (and (typep condition 'timer-error)
                (eq (timer-error-condition condition)
                    *last-reported-condition*)))))

(defun report-error (condition backtrace context)
  "Run *error-report-hook* with CONDITION, BACKTRACE and CONTEXT, unless
CONDITION, or the error a timer-error CONDITION wraps, was reported last in
this command loop iteration. What a hook function signals is dropped, a
stack exhaustion included, so that reporting never makes the error it
reports worse."
  (unless (already-reported-p condition)
    (when (boundp '*last-reported-condition*)
      (setf *last-reported-condition* condition))
    ;; Not run-hooks: it stops at the first hook that signals.
    (dolist (hook *error-report-hook*)
      (handler-case (funcall (car hook) condition backtrace context)
        (serious-condition () nil)))))

(defun bailout (condition)
  ;; The whole backtrace: the report is what is left of a crash.
  (let ((backtrace (ignore-errors (backtrace-string condition :count nil))))
    (report-error condition (ignore-errors (safe-backtrace)) :crash)
    (signal 'exit-editor
            :report (with-output-to-string (stream)
                      (princ condition stream)
                      (fresh-line stream)
                      (when backtrace
                        (write-string backtrace stream))))))

(defun pop-up-backtrace (condition)
  (let* ((timer-backtrace (and (typep condition 'timer-error)
                               (timer-error-backtrace condition)))
         (o (with-output-to-string (stream)
              (princ condition stream)
              (fresh-line stream)
              (write-string (or timer-backtrace (backtrace-string condition))
                            stream))))
    ;; A timer's handle-function shows the original error, not a timer-error.
    ;; A command failing in a command loop the timer entered (a prompt it
    ;; opened, see command-loop) is :command: running-timer-p is false there,
    ;; and the timer-error that carries it out of the timer says so.
    (report-error condition
                  (or timer-backtrace (ignore-errors (safe-backtrace)))
                  (if (if (typep condition 'timer-error)
                          (not (timer-error-outside-timer-p condition))
                          (running-timer-p))
                      :timer
                      :command))
    (funcall 'pop-up-typeout-window
             (make-buffer "*EDITOR ERROR*")
             :function (lambda (stream)
                         (format stream "~A" o))
             :erase t)))

(defmacro with-error-handler (() &body body)
  `(handler-case
       (handler-bind ((error
                        (lambda (condition)
                          (handler-bind ((error #'bailout))
                            (pop-up-backtrace condition)
                            (redraw-display)))))
         ,@body)
     (error ())))

(defvar *interactive-p* nil)
(defun interactive-p () *interactive-p*)

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defmacro save-continue-flags (&body body)
  `(let ((*last-flags* *last-flags*)
         (*curr-flags* *curr-flags*))
     ,@body))

(defun continue-flag (flag)
  (prog1 (cdr (assoc flag *last-flags*))
    (push (cons flag t) *last-flags*)
    (push (cons flag t) *curr-flags*)))

(defun nullify-last-flags (flag &rest more-flags)
  "Set FLAG and MORE-FLAGS to nil in *LAST-FLAGS*."
  (push (cons flag nil) *last-flags*)
  (when more-flags
    (dotimes (i (length more-flags))
      (push (cons (nth i more-flags) nil) *last-flags*))))

(defmacro do-command-loop ((&key interactive) &body body)
  (alexandria:once-only (interactive)
    `(loop :for *last-flags* := nil :then *curr-flags*
           :for *curr-flags* := nil
           :do (let ((*interactive-p* ,interactive)) ,@body))))

(defun fix-current-buffer-if-broken ()
  (unless (eq (window-buffer (current-window))
              (current-buffer))
    (setf (current-buffer) (window-buffer (current-window)))))

(defun command-loop-body ()
  (flet ((redraw ()
           (when (= 0 (event-queue-length))
             (without-interrupts
               (handler-bind ((error #'bailout))
                 (redraw-display)))))

         (read-command-and-call ()
           (let ((cmd (with-idle-timers ()
                        (read-command))))
             (unless (or (eq cmd '<mouse-motion-event>)
                         (eq cmd '<mouse-event>))
               (message nil))
             (when cmd
               (call-command cmd nil))))

         (editor-abort-handler (c)
           (declare (ignore c))
           (buffer-mark-cancel (current-buffer)) ; TODO: define handler
           (run-hooks *editor-abort-hook*)
           )

         (editor-condition-handler (c)
           (declare (ignore c))
           (stop-record-key) ; TODO: define handler
           ))

    (redraw)

    (handler-case
        (handler-bind ((editor-abort
                         #'editor-abort-handler)
                       (editor-condition
                         #'editor-condition-handler))
          (let ((*this-command-keys* nil))
            (read-command-and-call)))
      (editor-condition (c)
        (restart-case (error c)
          (lem-restart:message ()
            (let ((message (princ-to-string c)))
              (unless (equal "" message)
                (message "~A" message))))
          (lem-restart:call-function (fn)
            (funcall fn)))))))

(defvar *toplevel-command-loop-p* t)

(defun toplevel-command-loop-p ()
  *toplevel-command-loop-p*)

(defvar *command-loop-counter* 0)

(defun command-loop-counter ()
  *command-loop-counter*)

(defun command-loop ()
  (flet ((run ()
           (do-command-loop (:interactive t)
             (incf *command-loop-counter*)
             (if (toplevel-command-loop-p)
                 (let ((*last-reported-condition* nil))
                   (with-error-handler ()
                     (let ((*toplevel-command-loop-p* nil))
                       (handler-bind ((editor-condition
                                        (lambda (c)
                                          (declare (ignore c))
                                          (invoke-restart 'lem-restart:message))))
                         (command-loop-body)))))
                 (command-loop-body))
             (fix-current-buffer-if-broken))))
    ;; Inline, so that the frames of a command loop stay as they were: the
    ;; incident fingerprint is made of the names of the topmost frames.
    (declare (inline run))
    (if (running-timer-p)
        ;; A command loop entered from a timer's function (a prompt it
        ;; opened) runs commands, not the timer's code (see pop-up-backtrace).
        (call-outside-timer #'run)
        (run))))

(defun toplevel-command-loop (initialize-function)
  (handler-bind ((exit-editor
                   (lambda (c)
                     (return-from toplevel-command-loop
                       (exit-editor-report c)))))
    (with-error-handler ()
      (funcall initialize-function))
    (with-editor-stream ()
      (command-loop))))

(defun exit-editor (&optional report)
  (run-hooks *exit-editor-hook*)
  (mapc #'disable-minor-mode (active-global-minor-modes))
  (signal 'exit-editor :report report))

(defun call-background-job (function cont)
  (bt2:make-thread
   (lambda ()
     (let ((backtrace)
           (printed-backtrace))
       (handler-case
           (handler-bind ((error (lambda (c)
                                   (declare (ignore c))
                                   (setf backtrace (ignore-errors (safe-backtrace))
                                         printed-backtrace
                                         (ignore-errors
                                          (with-output-to-string (stream)
                                            (uiop:print-backtrace :stream stream
                                                                  :count 100)))))))
             (let ((result (funcall function)))
               (send-event (lambda () (funcall cont result)))))
         (error (condition)
           ;; Reported here, in the job's thread, which the report is about.
           (report-error condition backtrace :background-job)
           (let ((error-text (with-output-to-string (stream)
                               ;; A condition's report may signal; the job's
                               ;; thread has no handler around this.
                               (write-string (or (ignore-errors (princ-to-string condition))
                                                 "#<error printing the condition>")
                                             stream)
                               (fresh-line stream)
                               (write-string (or printed-backtrace "") stream))))
             (send-event (lambda ()
                           (let ((buffer (make-buffer "*BACKGROUND JOB ERROR*")))
                             (erase-buffer buffer)
                             (insert-string (buffer-point buffer)
                                            error-text)
                             (pop-to-buffer buffer)
                             (buffer-start (buffer-point buffer))))))))))
   ;; Named, as the thread of its errors' incidents.
   :name "Lem background job"))
