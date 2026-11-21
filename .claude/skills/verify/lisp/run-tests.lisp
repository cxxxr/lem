;;; Run Rove test packages (VERIFY_SUITES) of lem-tests, or the whole system.
;;; Exits with 0 when every test passed, 1 otherwise.

(ql:quickload '(:lem-fake-interface :lem-tests) :silent t)

(when (plusp (length (uiop:getenv "VERIFY_FRESH")))
  ;; Compile the files of lem-tests again even if their fasls look newer.
  (asdf:load-system :lem-tests :force '(:lem-tests)))

(defun verify-suite-test-count (name)
  "The number of tests in the Rove suite of package NAME, or NIL if there is none."
  (let* ((package (find-package (string-upcase name)))
         (suite (and package (rove/core/suite/package:find-suite package))))
    (and suite (length (rove/core/suite/package:suite-tests suite)))))

(let* ((suites (verify-env-list "VERIFY_SUITES"))
       (counts (if suites
                   (mapcar #'verify-suite-test-count suites)
                   ;; rove:run of the whole system finds its packages from the
                   ;; fasls; when it finds none it runs nothing and passes.
                   (mapcar (lambda (suite)
                             (and suite (length (rove/core/suite/package:suite-tests suite))))
                           (rove/core/suite/package:system-suites (asdf:find-system :lem-tests))))))
  ;; Refuse a run that would execute no test: it would print "passed".
  (cond ((and suites (member-if (lambda (count) (not (and count (plusp count)))) counts))
         (loop :for name :in suites
               :for count :in counts
               :unless (and count (plusp count))
                 :do (format t "~&VERIFY: no test in package ~a~%" name))
         (format t "~&VERIFY: tests failed~%")
         (uiop:quit 1))
       ((notany (lambda (count) (and count (plusp count))) counts)
        (format t "~&VERIFY: no test package of lem-tests found~%VERIFY: tests failed~%")
        (uiop:quit 1)))
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
