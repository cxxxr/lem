;;; Run Rove test packages (VERIFY_SUITES) of lem-tests, or the whole system.
;;; Exits with 0 when every test passed, 1 otherwise.

(ql:quickload '(:lem-fake-interface :lem-tests) :silent t)

(when (plusp (length (uiop:getenv "VERIFY_FRESH")))
  ;; Compile the files of lem-tests again even if their fasls look newer.
  (asdf:load-system :lem-tests :force '(:lem-tests)))

(let ((suites (verify-env-list "VERIFY_SUITES")))
  ;; rove:run of the whole system finds its test packages from the fasls; if
  ;; it finds none it runs nothing and passes.  Refuse that.  (The function is
  ;; internal to Rove: it is what rove:run uses.)
  (unless (or suites
              (rove/core/suite/file::system-packages (asdf:find-system :lem-tests)))
    (format t "~&VERIFY: no test package of lem-tests found~%VERIFY: tests failed~%")
    (uiop:quit 1))
  (defparameter *verify-suites* suites))

(let* ((suites *verify-suites*)
       (passed (if suites
                   (every (lambda (name)
                            (rove:run-suite (intern (string-upcase name) :keyword)))
                          suites)
                   (rove:run :lem-tests))))
  ;; A line without colors, for grep.
  (format t "~&VERIFY: tests ~:[failed~;passed~]~%" passed)
  (uiop:quit (if passed 0 1)))
