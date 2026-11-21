(defpackage :lem-tests/frame-multiplexer
  (:use :cl :lem :rove)
  (:import-from :lem
                :with-current-buffers)
  (:import-from :lem/common/var
                :with-global-variable-value)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-tests/frame-multiplexer)

(defun call-with-fresh-editor (function)
  ;; with-fake-interface gives the test its own frames and current buffer;
  ;; the tests here also compare whole buffer lists, so start from an empty one.
  (with-current-buffers ()
    (with-fake-interface ()
      (funcall function))))

(defmacro with-fresh-editor (() &body body)
  `(call-with-fresh-editor (lambda () ,@body)))

(defun call-with-frame-multiplexer (function)
  ;; Run frame-multiplexer on its own table of virtual frames and with the
  ;; variable off at the start, so that running these tests inside a Lem that
  ;; uses frame-multiplexer leaves its tabs and its setting as they were.
  ;; with-global-variable-value restores the previous value without running
  ;; the variable's hook.
  ;;
  ;; Unlike the let bindings, with-global-variable-value is not local to this
  ;; thread: it sets the value in the editor variable itself, which every
  ;; thread reads.  Run from a REPL thread of a running Lem, these tests turn
  ;; that Lem's frame-multiplexer variable (and, in
  ;; separation-can-be-turned-off, frame-multiplexer-separate-buffers) off and
  ;; on until they finish, and the editor thread sees it meanwhile.  Editor
  ;; variables have no per-thread value, so these tests assume that no other
  ;; thread uses the editor while they run: run them in a Lem that is idle, or
  ;; outside of one (make test, verify tests).
  (with-fresh-editor ()
    (let ((lem/frame-multiplexer::*virtual-frame-map* (make-hash-table))
          (lem/frame-multiplexer::*recent-list* nil)
          (*frame-buffer-list-function* nil))
      (with-global-variable-value (lem/frame-multiplexer::frame-multiplexer nil)
        (lem/frame-multiplexer::enable-frame-multiplexer)
        (unwind-protect (funcall function)
          (lem/frame-multiplexer::disable-frame-multiplexer))))))

(defmacro with-frame-multiplexer (() &body body)
  `(call-with-frame-multiplexer (lambda () ,@body)))

(defun offered-buffer-names ()
  (mapcar #'buffer-name (frame-buffer-list)))

(defun visit-new-buffer (name)
  (let ((buffer (make-buffer name)))
    (switch-to-buffer buffer)
    buffer))

(deftest each-frame-offers-its-own-buffers
  (with-frame-multiplexer ()
    (visit-new-buffer "a")
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (visit-new-buffer "b")
    (testing "the new frame does not offer the buffers of the first frame"
      (ok (member "b" (offered-buffer-names) :test #'string=))
      (ng (member "a" (offered-buffer-names) :test #'string=)))
    (testing "buffer completion is narrowed too"
      (ok (equal '("b")
                 (mapcar #'buffer-name (completion-buffer "b"))))
      (ok (null (completion-buffer "a"))))
    (lem/frame-multiplexer:frame-multiplexer-prev)
    (testing "the first frame does not offer the buffers of the new frame"
      (ok (member "a" (offered-buffer-names) :test #'string=))
      (ng (member "b" (offered-buffer-names) :test #'string=)))
    (testing "next-buffer and previous-buffer stay in the frame"
      (loop :repeat 4
            :do (next-buffer)
                (ng (string= "b" (buffer-name (current-buffer)))))
      (loop :repeat 4
            :do (previous-buffer)
                (ng (string= "b" (buffer-name (current-buffer))))))))

(deftest a-buffer-no-frame-has-shown-is-offered-everywhere
  (with-frame-multiplexer ()
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (make-buffer "background")
    (ok (member "background" (offered-buffer-names) :test #'string=))
    (lem/frame-multiplexer:frame-multiplexer-prev)
    (ok (member "background" (offered-buffer-names) :test #'string=))))

(deftest a-buffer-shown-in-two-frames-is-shared
  (with-frame-multiplexer ()
    (let ((shared (visit-new-buffer "shared")))
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      (switch-to-buffer shared)
      (ok (member "shared" (offered-buffer-names) :test #'string=))
      (lem/frame-multiplexer:frame-multiplexer-prev)
      (ok (member "shared" (offered-buffer-names) :test #'string=)))))

(deftest killing-a-buffer-shows-a-buffer-of-the-same-frame
  (with-frame-multiplexer ()
    (visit-new-buffer "a")
    (let ((a2 (visit-new-buffer "a2")))
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      (visit-new-buffer "b")
      (lem/frame-multiplexer:frame-multiplexer-prev)
      (kill-buffer a2)
      (ng (string= "b" (buffer-name (current-buffer))))
      (ng (member a2 (frame-buffer-list))))))

(deftest separation-can-be-turned-off
  (with-frame-multiplexer ()
    (visit-new-buffer "a")
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    ;; This sets the variable for every thread; see call-with-frame-multiplexer.
    (with-global-variable-value (lem/frame-multiplexer:frame-multiplexer-separate-buffers nil)
      (ok (member "a" (offered-buffer-names) :test #'string=)))))

(deftest quitting-a-window-shows-a-buffer-of-the-same-frame
  (with-frame-multiplexer ()
    (visit-new-buffer "a")
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (visit-new-buffer "b")
    (visit-new-buffer "help")
    (lem/frame-multiplexer:frame-multiplexer-prev)
    ;; Put "a" at the head of the whole buffer list.
    (visit-new-buffer "a-other")
    (lem/frame-multiplexer:frame-multiplexer-next)
    (ok (eq (get-buffer "a") (first (buffer-list))))
    (quit-window (current-window))
    (ok (string= "b" (buffer-name (current-buffer))))))

(deftest killing-the-last-buffer-of-a-frame-shows-tmp
  (with-frame-multiplexer ()
    (visit-new-buffer "a")
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (let ((b (visit-new-buffer "b")))
      (kill-buffer (get-buffer "*tmp*"))
      (ok (equal '("b") (offered-buffer-names)))
      (kill-buffer b)
      (ok (string= "*tmp*" (buffer-name (current-buffer)))))))

(deftest deleting-the-only-buffer-does-not-fail
  (with-fresh-editor ()
    (let ((x (visit-new-buffer "x")))
      (delete-buffer (get-buffer "*tmp*"))
      (ok (equal (list x) (buffer-list)))
      (ok (progn (delete-buffer x) t))
      (ok (string= "*tmp*" (buffer-name (current-buffer)))))))

(deftest killing-a-buffer-shown-in-two-windows-shows-the-same-buffer-in-both
  (with-fresh-editor ()
    (let ((a (make-buffer "a"))
          (k (make-buffer "k"))
          (b (make-buffer "b"))
          (c (make-buffer "c")))
      (switch-to-buffer k)
      (split-window-vertically (current-window))
      (ok (= 2 (length (get-buffer-windows k))))
      ;; The buffer list is (a k b c ...): the buffer before k is a.
      (dolist (buffer (list c b k a))
        (unbury-buffer buffer))
      (kill-buffer k)
      (ok (equal (list a a) (mapcar #'window-buffer (window-list)))))))

(deftest a-buffer-shown-in-a-side-window-of-a-hidden-frame-stays-in-that-frame
  (with-frame-multiplexer ()
    (visit-new-buffer "a")
    (let ((killed (make-buffer "killed")))
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      (let ((b (visit-new-buffer "b")))
        (make-leftside-window killed)
        ;; The buffer before "killed" in this frame's list is "b".
        (unbury-buffer killed)
        (unbury-buffer b)
        (let ((side-window (frame-leftside-window (current-frame))))
          (lem/frame-multiplexer:frame-multiplexer-prev)
          (kill-buffer killed)
          (ok (eq b (window-buffer side-window)))
          (testing "the displayed frame does not get the side window's new buffer"
            (ng (member "b" (offered-buffer-names) :test #'string=))))))))

(deftest a-frame-whose-buffers-are-all-gone-offers-no-other-frames-buffers
  (with-frame-multiplexer ()
    (let ((a (visit-new-buffer "a")))
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      ;; A temporary buffer is not in the buffer list, so once *tmp* is killed
      ;; this frame has no buffer of the list left.
      (let ((temporary (make-buffer "temporary" :temporary t)))
        (switch-to-buffer temporary)
        (kill-buffer (get-buffer "*tmp*"))
        (ok (equal (list a) (buffer-list)))
        (ok (null (frame-buffer-list)))
        (ok (null (completion-buffer "a")))
        (testing "commands that need another buffer still work"
          (ok (progn (next-buffer) (previous-buffer) t))
          (ok (eq temporary (current-buffer)))
          (testing "other-buffer falls back to the current buffer, not a buffer of the other frame"
            (ok (eq temporary (other-buffer)))))))))

(defun call-counting-calls (names function)
  "Call FUNCTION with the functions NAMES counting their calls, and return the
counts, in the order of NAMES."
  (let* ((counts (make-list (length names) :initial-element 0))
         (originals (mapcar #'fdefinition names)))
    ;; lem/frame-multiplexer is a locked package on SBCL.
    (#+sbcl sb-ext:without-package-locks #-sbcl progn
      (unwind-protect
           (progn
             (loop :for name :in names
                   :for original :in originals
                   :for cell :on counts
                   :do (let ((original original)
                             (cell cell))
                         (setf (fdefinition name)
                               (lambda (&rest args)
                                 (incf (car cell))
                                 (apply original args)))))
             (funcall function))
        (loop :for name :in names
              :for original :in originals
              :do (setf (fdefinition name) original))))
    counts))

(deftest the-workspace-buffer-list-reads-the-frame-table-once
  (with-frame-multiplexer ()
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (loop :for i :below 20
          :do (make-buffer (format nil "background-~D" i)))
    ;; claimed-buffers walks the whole frame table and virtual-frame-entries
    ;; copies it: once per call of frame-buffer-list, not once per buffer.
    ;; Expecting claimed-buffers exactly once also makes the test fail if the
    ;; counting misses the calls (the function inlined or no longer used).
    (destructuring-bind (claimed entries)
        (call-counting-calls '(lem/frame-multiplexer::claimed-buffers
                               lem/frame-multiplexer::virtual-frame-entries)
                             #'frame-buffer-list)
      (ok (= 1 claimed))
      (ok (<= entries 1)))))

(deftest a-frame-buffer-list-function-of-one-value-still-restricts
  (with-fresh-editor ()
    (let ((x (make-buffer "x")))
      (make-buffer "y")
      (let ((*frame-buffer-list-function*
              (lambda (frame) (declare (ignore frame)) (list x))))
        (ok (equal (list x) (frame-buffer-list))))
      (let ((*frame-buffer-list-function* (constantly nil)))
        (ok (equal (buffer-list) (frame-buffer-list))))
      (let ((*frame-buffer-list-function*
              (lambda (frame) (declare (ignore frame)) (values nil t))))
        (ok (null (frame-buffer-list))))
      (testing "only T as the second value restricts the frame to no buffer"
        (let ((*frame-buffer-list-function*
                (lambda (frame) (declare (ignore frame)) (values nil :unknown))))
          (ok (equal (buffer-list) (frame-buffer-list))))))))

(deftest killing-tmp-when-it-is-the-only-buffer-of-a-frame
  (with-frame-multiplexer ()
    (let ((a (visit-new-buffer "a")))
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      (let ((tmp (get-buffer "*tmp*")))
        (ok (eq tmp (current-buffer)))
        (kill-buffer tmp)
        (ok (deleted-buffer-p tmp))
        (testing "the frame shows a live buffer, not the killed *tmp* nor the other frame's buffer"
          (ng (deleted-buffer-p (current-buffer)))
          (ng (deleted-buffer-p (window-buffer (current-window))))
          (ng (eq a (current-buffer))))
        (testing "the other frame still has its buffer"
          (lem/frame-multiplexer:frame-multiplexer-prev)
          (ok (eq a (current-buffer))))))))

(deftest the-test-helper-leaves-a-running-frame-multiplexer-as-it-was
  ;; As in a Lem that uses frame-multiplexer: the variable is on and the
  ;; workspace functions are set.
  ;; The running Lem's table of virtual frames has an entry of its own; the
  ;; helper must not clear it (frame-multiplexer-init clears the table).
  (let ((list-function (lambda (frame) (declare (ignore frame)) nil))
        (recent-list (lem/common/ring:make-ring 1))
        (virtual-frame-map (make-hash-table)))
    (setf (gethash :running-lem virtual-frame-map) :its-virtual-frame)
    (with-global-variable-value (lem/frame-multiplexer::frame-multiplexer t)
      (let ((lem/frame-multiplexer::*virtual-frame-map* virtual-frame-map)
            (lem/frame-multiplexer::*recent-list* recent-list)
            (*frame-buffer-list-function* list-function))
        (with-frame-multiplexer ()
          (testing "the test gets a frame-multiplexer of its own"
            (ok (gethash (implementation) lem/frame-multiplexer::*virtual-frame-map*))
            (ok (null (gethash :running-lem lem/frame-multiplexer::*virtual-frame-map*)))))
        (testing "the running Lem's virtual frames are left as they were"
          (ok (eq virtual-frame-map lem/frame-multiplexer::*virtual-frame-map*))
          (ok (= 1 (hash-table-count virtual-frame-map)))
          (ok (eq :its-virtual-frame (gethash :running-lem virtual-frame-map))))
        (testing "the setting and the workspace functions are restored"
          (ok (eq t (variable-value 'lem/frame-multiplexer::frame-multiplexer :global)))
          (ok (eq list-function *frame-buffer-list-function*))
          (ok (eq recent-list lem/frame-multiplexer::*recent-list*)))))))

(deftest turning-it-off-frees-the-other-tabs
  (with-frame-multiplexer ()
    (let ((first (current-frame)))
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
      (lem/frame-multiplexer:frame-multiplexer-prev)
      (let ((shown (current-frame)))
        (ok (not (eq first shown)))
        (ok (= 3 (length (all-frames))))
        (lem/frame-multiplexer::disable-frame-multiplexer)
        (ok (equal (list shown) (all-frames))
            "the tabs that cannot be reached again are torn down")
        (ok (eq shown (current-frame)) "the frame shown stays")))))

(deftest turning-it-off-completes-when-a-tab-cannot-be-freed
  (with-frame-multiplexer ()
    (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)
    (let ((hidden (current-frame)))
      (lem/frame-multiplexer:frame-multiplexer-prev)
      (let ((shown (current-frame)))
        ;; freeing the hidden tab's windows signals
        (setf (frame-window-tree hidden) :not-a-window)
        (lem/frame-multiplexer::disable-frame-multiplexer)
        (ng (variable-value 'lem/frame-multiplexer::frame-multiplexer :global)
            "frame-multiplexer is off")
        (ok (equal (list shown) (all-frames)) "the hidden tab is forgotten")
        (ok (eq shown (current-frame)))
        (ng (some (lambda (window)
                    (typep window 'lem/frame-multiplexer::virtual-frame))
                  (frame-header-windows shown))
            "the tab bar is gone")))))
