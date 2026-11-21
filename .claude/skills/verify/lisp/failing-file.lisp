;;; Loaded by compile-check.lisp and run-tests.lisp, not by build-ncurses.lisp:
;;; the method below must not be saved into the Lem that `verify build' makes.

(defvar *verify-condition-files* (make-hash-table :test 'eq :weakness :key)
  "The source file component whose compile or load signalled each error
condition.
Set by the :around method on `asdf:perform' below.")

(defmethod asdf:perform :around ((operation asdf:operation) (component asdf:cl-source-file))
  ;; Note the file of each error, so that a failed load can name it: a
  ;; top-level (error ...) says nothing of where it is.  Handlers run
  ;; innermost first, so the file that signalled the error is noted, not one
  ;; that loads another system at load time.
  (handler-bind ((error (lambda (condition)
                          (unless (gethash condition *verify-condition-files*)
                            (setf (gethash condition *verify-condition-files*)
                                  component)))))
    (call-next-method)))

(defun verify-failing-component (condition)
  "The source file component whose compile or load signalled CONDITION, or NIL."
  (gethash condition *verify-condition-files*))

(defun verify-print-failing-file (condition)
  "Print the file whose compile or load signalled CONDITION, if one is known."
  (let ((component (verify-failing-component condition)))
    (when component
      (format t "~&  in ~a~%"
              (enough-namestring (asdf:component-pathname component) (uiop:getcwd))))))
