(defpackage :lem-tests/interp
  (:use :cl :rove))
(in-package :lem-tests/interp)

(defvar *result*)

(lem:define-command $test-command-flag () ()
  (let ((value (lem:continue-flag :test-command-flag)))
    (alexandria:nconcf *result* (list value))))

(define-condition unprintable-error (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition stream))
             (error "This condition cannot be printed"))))

(lem:define-command $fail-in-a-prompt () ()
  (error "A command in a prompt failed"))

(defmacro with-reported-contexts ((contexts) &body body)
  `(let ((,contexts '())
         (lem:*error-report-hook* '()))
     (lem:add-hook lem:*error-report-hook*
                   (lambda (condition backtrace context)
                     (declare (ignore condition backtrace))
                     (push context ,contexts)))
     ,@body))

(defun execute-testing-command (n command-name &optional argument)
  (lem:do-command-loop ()
    (lem:call-command command-name argument)
    (unless (plusp (decf n)) (return))))

(deftest Execute-the-same-command-consecutively
  (lem-fake-interface:with-fake-interface ()
    (lem:with-current-buffers ()
      (let ((*result* '()))
        (execute-testing-command 3 '$test-command-flag)
        (ok (equal '(nil t t) *result*))))))

(defun recurse-then-fail (depth)
  (if (zerop depth)
      (error "Failed at the bottom")
      ;; 1+ keeps the call out of tail position, so that each level is a frame.
      (1+ (recurse-then-fail (1- depth)))))

(defun frame-line-count (text)
  (count-if (lambda (line) (ppcre:scan "^\\s*\\d+: " line))
            (uiop:split-string text :separator '(#\Newline))))

(deftest the-crash-report-has-the-whole-backtrace
  (let ((lem:*error-report-hook* '())
        (report nil))
    (handler-bind ((lem:exit-editor (lambda (c)
                                      (setf report (lem-core::exit-editor-report c)))))
      (handler-case
          (handler-bind ((error #'lem-core::bailout))
            (recurse-then-fail 150))
        (error () nil)))
    (ok (stringp report))
    (ok (< 150 (frame-line-count report)))))

(deftest the-last-reported-condition-is-not-kept-outside-a-command-loop
  ;; In a thread of its own, with no binding of *last-reported-condition*:
  ;; outside of a command loop also when the tests run in a Lem, whose
  ;; command loop binds it. (lem-core is locked, so progv cannot unbind it.)
  (let ((kept :not-run))
    (bt2:join-thread
     (bt2:make-thread
      (lambda ()
        (let ((lem:*error-report-hook* '()))
          (lem-core::report-error (make-condition 'simple-error :format-control "x")
                                  nil :command)
          (setf kept (and (boundp 'lem-core::*last-reported-condition*)
                          (symbol-value 'lem-core::*last-reported-condition*)))))))
    (ok (null kept))))

(deftest a-timer-error-is-reported-once-in-an-iteration
  (with-reported-contexts (contexts)
    (let ((lem-core::*last-reported-condition* nil)
          (original (make-condition 'simple-error :format-control "x")))
      (lem-core::report-error original nil :timer)
      (lem-core::report-error (make-condition 'lem/common/timer:timer-error
                                              :timer nil :condition original)
                              nil :timer)
      (ok (equal '(:timer) contexts)))))

(defun contexts-of-a-command-failing-in-a-prompt-of-a-timer (handle-function-p)
  (lem-fake-interface:with-fake-interface ()
    (lem:with-current-buffers ()
      (with-reported-contexts (contexts)
        (let* ((keymap (lem:make-keymap))
               (key (lem:make-key :ctrl t :sym "t"))
               (timer (lem:make-idle-timer
                       (lambda ()
                         (lem:prompt-for-string "in a timer: " :special-keymap keymap))
                       :name "a timer with a prompt"
                       :handle-function (when handle-function-p
                                          (lambda (condition)
                                            (lem:pop-up-backtrace condition))))))
          (lem:define-key keymap "C-t" '$fail-in-a-prompt)
          (lem:unread-key key)
          ;; As in the editor: a timer runs inside the toplevel command loop,
          ;; whose error handler is not around the prompt's command loop.
          (let ((lem-core::*last-reported-condition* nil)
                (lem-core::*toplevel-command-loop-p* nil)
                (lem/common/timer::*timer-manager*
                  (make-instance 'lem-core::lem-timer-manager)))
            (handler-case (lem/common/timer::call-timer-function timer)
              (lem/common/timer:timer-error (c)
                (lem:pop-up-backtrace c))))
          contexts)))))

(deftest a-command-failing-in-a-prompt-of-a-timer-is-a-command-error
  (testing "with a handle-function: reported once, by it"
    (ok (equal '(:command) (contexts-of-a-command-failing-in-a-prompt-of-a-timer t))))
  (testing "without one: reported once, as the timer-error"
    (ok (equal '(:command) (contexts-of-a-command-failing-in-a-prompt-of-a-timer nil)))))

(deftest a-background-job-error-that-cannot-be-printed-is-shown
  (lem-fake-interface:with-fake-interface ()
    (lem:with-current-buffers ()
      (with-reported-contexts (contexts)
        ;; The job's thread gets this test's event queue and hook.
        (let* ((queue (lem-core::make-concurrent-queue))
               (hook lem:*error-report-hook*)
               (lem-core::*editor-event-queue* queue)
               (bt2:*default-special-bindings*
                 (list* (cons 'lem-core::*editor-event-queue* queue)
                        (cons 'lem:*error-report-hook* (list 'quote hook))
                        bt2:*default-special-bindings*))
               (thread (lem-core::call-background-job
                        (lambda () (error 'unprintable-error))
                        (lambda (result) result))))
          (bt2:join-thread thread)
          (ok (equal '(:background-job) contexts))
          (lem-core::receive-event 0)
          (let ((buffer (lem:get-buffer "*BACKGROUND JOB ERROR*")))
            (ok buffer)
            (ok (and buffer
                     (search "#<error printing the condition>"
                             (lem:buffer-text buffer))))))))))

(deftest an-error-that-cannot-be-printed-is-shown
  (lem-fake-interface:with-fake-interface ()
    (lem:with-current-buffers ()
      (with-reported-contexts (contexts)
        (lem-core::with-error-handler ()
          (error 'unprintable-error))
        (ok (equal '(:command) contexts))
        (let ((buffer (lem:get-buffer "*EDITOR ERROR*")))
          (ok (and buffer
                   (search "#<error printing the condition>" (lem:buffer-text buffer)))))))))

(deftest a-timer-error-is-reported-once-by-with-error-handler
  ;; As around the editor's initialization: no command loop iteration.
  (lem-fake-interface:with-fake-interface ()
    (lem:with-current-buffers ()
      (with-reported-contexts (contexts)
        (let ((timer (lem:make-idle-timer (lambda () (error "A timer failed"))
                                          :name "a failing timer"
                                          :handle-function (lambda (condition)
                                                             (lem:pop-up-backtrace condition)))))
          (lem-core::with-error-handler ()
            (lem/common/timer::call-timer-function timer)))
        (ok (equal '(:timer) contexts))))))
