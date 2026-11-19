(defpackage :lem-tree-sitter
  (:use :cl)
  (:export #:enable-tree-sitter-for-mode #:tree-sitter-available-p))
(in-package :lem-tree-sitter)

(defun tree-sitter-available-p () nil)

(defun enable-tree-sitter-for-mode (syntax-table language query-path &key indent-query-path)
  "There is no tree-sitter: the mode keeps its own highlighting."
  (declare (ignore syntax-table language query-path indent-query-path))
  nil)
