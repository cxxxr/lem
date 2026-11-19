(defpackage :lem-tests/fold-region
  (:use :cl :rove :lem)
  (:import-from :lem-tests/utilities
                :with-testing-buffer
                :make-text-buffer
                :lines)
  (:import-from :lem/language-mode
                :fold-region-default))
(in-package :lem-tests/fold-region)

(defun point-at-line (buffer line-number)
  "Temporary point at the start of LINE-NUMBER (0-based) of BUFFER."
  (let ((point (copy-point (buffer-start-point buffer) :temporary)))
    (line-offset point line-number)
    point))

(defun fold-region-lines (buffer line-number)
  "The region that folds at LINE-NUMBER of BUFFER, as (start-line end-line
end-column), 0-based, or NIL."
  (multiple-value-bind (start end)
      (fold-region-default (point-at-line buffer line-number))
    (when start
      (list (1- (line-number-at-point start))
            (1- (line-number-at-point end))
            (point-charpos end)))))

(defun folded-lines (buffer)
  (loop :for overlay :in (buffer-overlays buffer)
        :when (overlay-get overlay :fold)
          :collect (1- (line-number-at-point (overlay-start overlay)))))

(deftest fold-a-brace-block
  (with-testing-buffer (buffer (make-text-buffer (lines "int f() {"
                                                        "  a();"
                                                        "  b();"
                                                        "}")))
    ;; the lines inside; the closing brace's line stays shown
    (ok (equal '(0 3 0) (fold-region-lines buffer 0)))
    (ok (null (fold-region-lines buffer 1)))))

(deftest fold-a-lisp-form
  (with-testing-buffer (buffer (make-text-buffer (lines "(defun f (x)"
                                                        "  (let ((y x))"
                                                        "    (g y)))"
                                                        "(h)")))
    ;; through the line that closes it
    (ok (equal '(0 2 11) (fold-region-lines buffer 0)))
    (ok (equal '(1 2 11) (fold-region-lines buffer 1)))
    (ok (null (fold-region-lines buffer 3)))))

(deftest fold-by-indentation
  (with-testing-buffer (buffer (make-text-buffer (lines "def f(x):"
                                                        "    a = x"
                                                        ""
                                                        "    return a"
                                                        "g()")))
    ;; the parentheses close on the same line: the indented lines under it
    (ok (equal '(0 3 12) (fold-region-lines buffer 0)))
    (ok (null (fold-region-lines buffer 4)))))

(deftest brackets-in-strings-do-not-fold
  (with-testing-buffer (buffer (make-text-buffer (lines "x = \"{\""
                                                        "y = 1")))
    (ok (null (fold-region-lines buffer 0)))))

(deftest fold-all-folds-the-outer-regions
  (with-testing-buffer (buffer (make-text-buffer (lines "(defun f ()"
                                                        "  (g"
                                                        "   1))"
                                                        ""
                                                        "(defun h ()"
                                                        "  2)")))
    (lem/language-mode::fold-all-regions buffer)
    (ok (equal '(0 4) (sort (folded-lines buffer) #'<)))
    (lem/language-mode:unfold-all)
    (ok (null (folded-lines buffer)))))

(deftest fold-all-folds-the-else-after-the-if
  (with-testing-buffer (buffer (make-text-buffer (lines "if (a) {"
                                                        "  b();"
                                                        "} else {"
                                                        "  c();"
                                                        "}")))
    (lem/language-mode::fold-all-regions buffer)
    (ok (equal '(0 2) (sort (folded-lines buffer) #'<)))))
