;;; Run Rove test packages (VERIFY_SUITES) of lem-tests, or the whole system.
;;; Exits with 0 when every test passed, 1 otherwise.

(defun verify-load-or-fail (function)
  "Call FUNCTION, which loads systems.  When it signals an error (a file that
fails to compile, with SBCL's default *compile-file-failure-behaviour*
:error), print it and quit with a failure, so that the last line is still
VERIFY: tests failed."
  (handler-case (funcall function)
    (error (condition)
      (format t "~&VERIFY: could not load the tests:~%  ~a~%VERIFY: tests failed~%"
              condition)
      (uiop:quit 1))))

(verify-load-or-fail
 (lambda () (ql:quickload '(:lem-fake-interface :lem-tests) :silent t)))

(when (plusp (length (uiop:getenv "VERIFY_FRESH")))
  ;; Compile the files of lem-tests again even if their fasls look newer.
  (verify-load-or-fail
   (lambda () (asdf:load-system :lem-tests :force '(:lem-tests)))))

(defun verify-rove-function (name)
  "The function NAME of Rove's internal package rove/core/suite/package.
Rove exports no way to count the tests of a package, so this uses its
internals, as they are in the Rove that qlfile.lock pins.  Quits with a
message when that Rove has no such function."
  (let ((symbol (and (find-package :rove/core/suite/package)
                     (find-symbol (string name) :rove/core/suite/package))))
    (unless (and symbol (fboundp symbol))
      (format t "~&VERIFY: Rove has no function rove/core/suite/package:~(~a~); ~
                 update .claude/skills/verify/lisp/run-tests.lisp for the Rove ~
                 of qlfile.lock~%VERIFY: tests failed~%"
              name)
      (uiop:quit 1))
    (symbol-function symbol)))

(defun verify-suite-test-count (suite)
  "The number of tests in the Rove SUITE, or NIL if SUITE is NIL."
  (and suite (length (funcall (verify-rove-function '#:suite-tests) suite))))

(defun verify-package-test-count (name)
  "The number of tests in the Rove suite of package NAME, or NIL if there is none."
  (let ((package (find-package (string-upcase name))))
    (and package
         (verify-suite-test-count
          (funcall (verify-rove-function '#:find-suite) package)))))

(let* ((suites (verify-env-list "VERIFY_SUITES"))
       (counts (if suites
                   (mapcar #'verify-package-test-count suites)
                   ;; rove:run of the whole system finds its packages from the
                   ;; fasls; when it finds none it runs nothing and passes.
                   (mapcar #'verify-suite-test-count
                           (funcall (verify-rove-function '#:system-suites)
                                    (asdf:find-system :lem-tests))))))
  ;; Refuse a run that would execute no test: it would print "passed".
  (cond ((and suites (member-if (lambda (count) (not (and count (plusp count)))) counts))
         (loop :for name :in suites
               :for count :in counts
               :unless (and count (plusp count))
                 :do (format t "~&VERIFY: no test in package ~a~%" name))
         (format t "~&VERIFY: tests failed~%")
         (uiop:quit 1))
        ((notany (lambda (count) (and count (plusp count))) counts)
         ;; Also what a Rove that maps fasls to sources another way gives:
         ;; see the probe in common.lisp.
         (format t "~&VERIFY: no test package of lem-tests found (if lem-tests ~
                    has tests, Rove could not map its fasls back to their ~
                    sources: see the probe in .claude/skills/verify/lisp/common.lisp)~%~
                    VERIFY: tests failed~%")
         (uiop:quit 1)))
  (defparameter *verify-suites* suites))

(let* ((suites *verify-suites*)
       (passed (if suites
                   ;; Every package runs, also after one failed.
                   (every #'identity
                          (mapcar (lambda (name)
                                    (rove:run-suite (intern (string-upcase name) :keyword)))
                                  suites))
                   (rove:run :lem-tests))))
  ;; A line without colors, for grep.
  (format t "~&VERIFY: tests ~:[failed~;passed~]~%" passed)
  (uiop:quit (if passed 0 1)))
