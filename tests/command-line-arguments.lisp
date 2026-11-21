(defpackage :lem-tests/command-line-arguments
  (:use :cl
        :rove))
(in-package :lem-tests/command-line-arguments)

(defclass failing-interface (lem-fake-interface:fake-interface)
  ((failed :initform nil :accessor interface-failed-p))
  (:documentation "A fake interface whose views cannot be deleted, so that
tearing the editor down fails."))

(defmethod lem-if:delete-view ((implementation failing-interface) view)
  (declare (ignore view))
  (setf (interface-failed-p implementation) t)
  (error "This view cannot be deleted"))

(define-condition teardown-trouble (serious-condition) ()
  (:documentation "A serious condition that is not an error, as a
storage-condition."))

(defclass troubled-interface (failing-interface) ()
  (:documentation "A fake interface whose views cannot be deleted, with a
serious condition that is not an error."))

(defmethod lem-if:delete-view ((implementation troubled-interface) view)
  (declare (ignore view))
  (setf (interface-failed-p implementation) t)
  (error 'teardown-trouble))

(defun command-line-arguments-equal (arg1 arg2)
  (and (equal (lem-core::command-line-arguments-help arg2)
              (lem-core::command-line-arguments-help arg1))
       (equal (lem-core::command-line-arguments-debug arg2)
              (lem-core::command-line-arguments-debug arg1))
       (equal (lem-core::command-line-arguments-version arg2)
              (lem-core::command-line-arguments-version arg1))
       (equal (lem-core::command-line-arguments-without-init-file arg2)
              (lem-core::command-line-arguments-without-init-file arg1))
       (equal (lem-core::command-line-arguments-log-filename arg2)
              (lem-core::command-line-arguments-log-filename arg1))
       (equal (lem-core::command-line-arguments-interface arg2)
              (lem-core::command-line-arguments-interface arg1))
       (equal (lem-core::command-line-arguments-filenames arg2)
              (lem-core::command-line-arguments-filenames arg1))))

(deftest parse-args-test
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("foo"))
       (lem-core::make-command-line-arguments
        :filenames '("foo"))))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("foo" "bar"))
       (lem-core::make-command-line-arguments
        :filenames '("foo" "bar"))))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("foo" "bar" "-i" "ncurses"))
       (lem-core::make-command-line-arguments
        :filenames '("foo" "bar")
        :interface :ncurses)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-i" "ncurses" "foo" "bar"))
       (lem-core::make-command-line-arguments
        :filenames '("foo" "bar")
        :interface :ncurses)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("foo" "-i" "ncurses" "bar"))
       (lem-core::make-command-line-arguments
        :filenames '("foo" "bar")
        :interface :ncurses)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-i" "ncurses"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :interface :ncurses)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-h"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :help t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--help"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :help t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-h" "foo"))
       (lem-core::make-command-line-arguments
        :filenames '("foo")
        :help t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--debug"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :debug t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--debug" "foo"))
       (lem-core::make-command-line-arguments
        :filenames '("foo")
        :debug t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-v"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :version t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--version"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :version t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-q"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :without-init-file t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--without-init-file"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :without-init-file t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--log-filename" "test.log"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :log-filename "test.log")))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--log-filename" "test.log" "foo.txt"))
       (lem-core::make-command-line-arguments
        :filenames '("foo.txt")
        :log-filename "test.log")))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-i" "sdl2"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :interface :sdl2)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--interface" "sdl2"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :interface :sdl2)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("--debug" "--without-init-file" "foo.txt"))
       (lem-core::make-command-line-arguments
        :filenames '("foo.txt")
        :debug t
        :without-init-file t)))
  (ok (command-line-arguments-equal
       (lem-core::parse-args '("-h" "-v" "--debug"))
       (lem-core::make-command-line-arguments
        :filenames '()
        :help t
        :version t
        :debug t))))

(deftest parse-args-error-test
  (testing "missing argument for --log-filename"
    (ok (signals (lem-core::parse-args '("--log-filename")) 'lem-core::command-line-arguments-error)))
  (testing "missing argument for -i"
    (ok (signals (lem-core::parse-args '("-i")) 'lem-core::command-line-arguments-error)))
  (testing "missing argument for --interface"
    (ok (signals (lem-core::parse-args '("--interface")) 'lem-core::command-line-arguments-error))))

(deftest launch-after-the-editor-ends-starts-another
  ;; The frontend's finalize lets launch return: a launch called then (from
  ;; the REPL Lem was run from) must not find the editor still there.
  ;; (end-editor and *in-the-editor* are internal: run-editor-thread uses
  ;; them, and no exported function ends an editor without a frontend.)
  (let ((seen '()))
    (lem-fake-interface:with-fake-interface ()
      (let ((lem-core::*in-the-editor* t))
        (lem-core::end-editor :report
                              (lambda (report)
                                (push (list report lem-core::*in-the-editor*
                                            (lem-core:all-frames))
                                      seen)))))
    (ok (equal '((:report nil nil)) seen))))

(defun finalized-after-a-failing-teardown (interface-class)
  "End an editor whose teardown fails in an INTERFACE-CLASS, and return
whether the teardown failed and what the finalize saw."
  (let ((seen '())
        (interface (make-instance interface-class)))
    (lem-fake-interface:with-fake-interface ()
      (let ((lem-core::*in-the-editor* t))
        (lem:with-implementation interface
          (lem-core::end-editor :report
                                (lambda (report)
                                  (push (list report lem-core::*in-the-editor*)
                                        seen))))))
    (list (interface-failed-p interface) seen)))

(deftest the-frontend-ends-also-when-the-teardown-fails
  (testing "with an error"
    (ok (equal '(t ((:report nil)))
               (finalized-after-a-failing-teardown 'failing-interface))))
  (testing "with a serious condition that is not an error"
    (ok (equal '(t ((:report nil)))
               (finalized-after-a-failing-teardown 'troubled-interface)))))
