(defpackage :lem-tests/cursors
  (:use :cl
        :rove
        :lem-tests/utilities)
  (:import-from :lem/common/killring
                :make-killring
                :push-killring-item
                :peek-killring-item)
  (:export :all-positions
           :positions-set-equal))
(in-package :lem-tests/cursors)

(defun make-testing-killring ()
  (let ((killring (lem/common/killring:make-killring 10)))
    (push-killring-item killring "aaa")
    (push-killring-item killring "bbb")
    (push-killring-item killring "ccc")
    killring))

(deftest simple-fake-cursor-test
  (with-testing-buffer (buffer (lem:make-buffer "cursor test"))
    (let* ((lem-core::*killring* (make-testing-killring))
           (cursor (lem-core::make-fake-cursor (lem:current-point))))
      (testing "Test the fake-cursor created"
        (testing "buffer-fake-cursors"
          (ok (alexandria:length= 1 (lem-core::buffer-fake-cursors buffer)))
          (ok (eq cursor (first (lem-core::buffer-fake-cursors buffer)))))
        (testing "killring"
          (let ((killring (lem-core::fake-cursor-killring cursor)))
            (ok (not (eq (lem-core::current-killring) killring)))
            (ok (equal "aaa" (peek-killring-item killring 2)))
            (ok (equal "bbb" (peek-killring-item killring 1)))
            (ok (equal "ccc" (peek-killring-item killring 0)))))
        (testing "point-kind"
          (ok (eq :left-inserting (lem:point-kind cursor)))))
      (testing "Delete cursor"
        (lem-core::delete-fake-cursor cursor)
        (ok (null (lem-core::buffer-fake-cursors buffer)))))))

(defun make-testing-fake-cursors (point n)
  (lem:with-point ((p point))
    (loop :repeat n
          :do (assert (not (null (lem:line-offset p 1))))
              (lem:make-fake-cursor p))))

(deftest test-to-execute-a-series-of-commands
  (lem-fake-interface:with-fake-interface ()
    (let ((lem-core::*killring* (lem/common/killring:make-killring 10)))
      (with-testing-buffer (buffer (make-text-buffer (lines "abcdefg" "hijklmn" "opqrstu")))
        (make-testing-fake-cursors (lem:buffer-point buffer) 2)
        (testing "execute self-insert command"
          (lem:execute-key-sequence (list (lem:make-key :sym " ")))
          (ok (string= (lines " abcdefg" " hijklmn" " opqrstu")
                       (lem:buffer-text buffer))))
        (testing "execute delete-previous-character command"
          (lem:execute-key-sequence (list (lem:make-key :ctrl t :sym "h")))
          (lem:buffer-text buffer)
          (ok (string= (lines "abcdefg" "hijklmn" "opqrstu")
                       (lem:buffer-text buffer))))
        (testing "multiple cursor killring"
          (lem:execute (lem:buffer-major-mode buffer)
                       (make-instance 'lem:delete-next-char)
                       4)
          (ok (equal '("abcd"
                       "opqr"
                       "hijk")
                     (mapcar (lambda (killring)
                               (peek-killring-item killring 0))
                             (cons lem-core::*killring*
                                   (mapcar (lambda (cursor)
                                             (lem-core::fake-cursor-killring cursor))
                                           (lem-core::buffer-fake-cursors buffer)))))))))))

(defun linum-and-column (point)
  (list (lem:line-number-at-point point)
        (lem:point-column point)))

(defun all-positions (buffer)
  (mapcar #'linum-and-column (lem-core::buffer-cursors buffer)))

(defun positions-set-equal (positions1 positions2)
  (alexandria:set-equal positions1 positions2 :test #'equal))

(deftest next-line/previous-line
  (lem-fake-interface:with-fake-interface ()
    (lem-core::save-continue-flags
      (with-testing-buffer (buffer (make-text-buffer (lines "abcdefghijklmn"
                                                            "opqrstuvwxyz"
                                                            "0123456789"
                                                            "------------------")))
        (lem-core::set-window-buffer buffer (lem:current-window))
        (make-testing-fake-cursors (lem:buffer-point buffer) 2)
        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance 'lem:move-to-end-of-line)
                     nil)

        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance 'lem:next-line)
                     nil)
        (ok (positions-set-equal '((2 12) (3 10) (4 10))
                                 (all-positions buffer)))

        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance 'lem:previous-line)
                     nil)
        (ok (positions-set-equal '((1 14) (2 12) (3 10))
                                 (all-positions buffer)))

        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance 'lem:next-line)
                     nil)
        (ok (positions-set-equal '((2 12) (3 10) (4 10))
                                 (all-positions buffer)))))))

(defun count-logged-messages (text)
  (let ((buffer (lem:get-buffer "*Messages*")))
    (if buffer
        (count text
               (uiop:split-string (lem:buffer-text buffer) :separator '(#\Newline))
               :test #'string=)
        0)))

(defun call-with-fake-cursors (command-class function)
  "Execute COMMAND-CLASS in a buffer with two fake cursors, then call
FUNCTION with the buffer while the buffer and *Messages* are still live."
  (lem-fake-interface:with-fake-interface ()
    (lem/common/timer:with-timer-manager (make-instance 'lem/common/timer:timer-manager)
      (with-testing-buffer (buffer (make-text-buffer (lines "abc" "def" "ghi")))
        (lem:switch-to-buffer buffer)
        (make-testing-fake-cursors (lem:buffer-point buffer) 2)
        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance command-class)
                     nil)
        (funcall function buffer)))))

(deftest multiple-cursors-message-once
  (testing "mark-set marks every cursor but reports once"
    (call-with-fake-cursors
     'lem:mark-set
     (lambda (buffer)
       (ok (every (lambda (cursor)
                    (lem:mark-active-p (lem-core:cursor-mark cursor)))
                  (lem-core:buffer-cursors buffer)))
       (ok (= 1 (count-logged-messages "Mark set"))))))
  (testing "yank-pop without a preceding yank reports once"
    (call-with-fake-cursors
     'lem:yank-pop
     (lambda (buffer)
       (declare (ignore buffer))
       (ok (= 1 (count-logged-messages "Previous command was not a yank")))))))

(defun call-in-buffer-with-fake-cursors (function)
  "Call FUNCTION with a buffer that has the real cursor and two fake cursors."
  (lem-fake-interface:with-fake-interface ()
    (lem/common/timer:with-timer-manager (make-instance 'lem/common/timer:timer-manager)
      (with-testing-buffer (buffer (make-text-buffer (lines "abc" "def" "ghi")))
        (lem:switch-to-buffer buffer)
        (make-testing-fake-cursors (lem:buffer-point buffer) 2)
        (funcall function buffer)))))

(deftest multiple-cursors-message-from-fake-cursors-only
  (testing "a message that only fake cursors report is still reported once"
    (call-in-buffer-with-fake-cursors
     (lambda (buffer)
       (declare (ignore buffer))
       (lem:process-each-cursors
        (lambda ()
          (when (lem:fake-cursor-p (lem:current-point))
            (lem:message "Only a fake cursor"))))
       (ok (= 1 (count-logged-messages "Only a fake cursor"))))))
  (testing "a message reported from another buffer is still reported once"
    (call-in-buffer-with-fake-cursors
     (lambda (buffer)
       (declare (ignore buffer))
       (let ((other (lem:make-buffer "message-once other buffer")))
         (unwind-protect
              (lem:process-each-cursors
               (lambda ()
                 (lem:with-current-buffer other
                   (lem:message "From another buffer"))))
           (lem:delete-buffer other)))
       (ok (= 1 (count-logged-messages "From another buffer"))))))
  (testing "a message outside a multiple-cursor command is reported"
    (call-in-buffer-with-fake-cursors
     (lambda (buffer)
       (declare (ignore buffer))
       (lem:message "Outside")
       (ok (= 1 (count-logged-messages "Outside")))))))

(deftest multiple-cursors-mark-set-with-prefix
  (testing "C-u mark-set moves each fake cursor to its own mark"
    (call-in-buffer-with-fake-cursors
     (lambda (buffer)
       (let ((command (make-instance 'lem:mark-set))
             (mode (lem:buffer-major-mode buffer)))
         (lem:execute mode command nil)
         (lem:execute mode (make-instance 'lem:forward-char) nil)
         (lem:execute mode command 4)
         (ok (every (lambda (cursor) (= 0 (lem:point-charpos cursor)))
                    (lem-core:buffer-cursors buffer))))))))
