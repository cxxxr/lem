(defpackage :lem-tests/keymap-test
  (:use :cl :rove)
  (:import-from :lem-core
                :make-keymap
                :make-prefix
                :keymap-add-prefix
                :make-key
                :traverse-keymap))
(in-package :lem-tests/keymap-test)

(defun make-test-keymap-with-closure-suffix ()
  "A keymap holding one normal symbol binding and one prefix whose
suffix is a closure -- the shape produced by transient toggle/choice
infixes."
  (let ((keymap (make-keymap :description 'test-keymap))
        (closure (lambda () 'transient-thunk)))
    (keymap-add-prefix keymap (make-prefix :key (make-key :sym "a")
                                           :suffix 'real-command))
    (keymap-add-prefix keymap (make-prefix :key (make-key :sym "b")
                                           :suffix closure))
    (values keymap closure)))

(deftest traverse-keymap-skips-non-symbol-suffixes
  "Regression test for #2100: transient closures (toggle/choice infix
suffixes) must not be yielded to traverse-keymap callbacks, since
callers like describe-bindings-internal call symbol-name on the
result and would signal a type-error on function objects."
  (multiple-value-bind (keymap closure) (make-test-keymap-with-closure-suffix)
    (let ((yielded '()))
      (traverse-keymap keymap
                       (lambda (kseq cmd)
                         (declare (ignore kseq))
                         (push cmd yielded)))
      (ok (every #'symbolp yielded))
      (ok (member 'real-command yielded))
      (ng (member closure yielded)))))

#+sbcl
(deftest describe-bindings-internal-terminates
  "Regression test for #2100: describe-bindings-internal used to loop
forever because the keymap-walk advance was removed but the
surrounding (loop :while keymap ...) was kept.  Wrap the call in a
timeout so a regression fails CI cleanly instead of hanging it."
  (let ((keymap (make-test-keymap-with-closure-suffix))
        (output nil))
    (let ((result
            (handler-case
                (bordeaux-threads:with-timeout (5)
                  (setf output
                        (with-output-to-string (s)
                          ;; Internal access: this helper is intentionally
                          ;; not exported (-internal suffix); the test pins
                          ;; down the regression at the unit level rather
                          ;; than committing to its stream-printing shape
                          ;; as a public contract.
                          (lem-core/commands/help::describe-bindings-internal
                           s "test" keymap t)))
                  :ok)
              (bordeaux-threads:timeout () :timed-out)
              (error (e) (cons :error e)))))
      (ok (eq result :ok))
      (ok (search "real-command" (or output ""))))))

(deftest multi-column-list-quits-with-c-g
  "C-g closes a multi-column-list (list-buffers...) also when it is not bound
to keyboard-quit (vi-mode): typed, it would filter the list."
  (let ((prefix (lem-core::keymap-find lem/multi-column-list::*multi-column-list-mode-keymap*
                                       (lem:parse-keyspec "C-g"))))
    (ok (and prefix
             (eq 'lem/multi-column-list::multi-column-list/quit
                 (lem-core::prefix-suffix prefix))))))
