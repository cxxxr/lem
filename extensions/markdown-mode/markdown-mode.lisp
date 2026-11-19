#| link : https://daringfireball.net/projects/markdown/syntax |#

(defpackage :lem-markdown-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*markdown-mode-hook*
           :markdown-mode))
(in-package :lem-markdown-mode)

(defvar *markdown-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :string-quote-chars '(#\`))))
    (set-syntax-parser table (lem-markdown-mode/syntax-parser:make-syntax-parser))
    table))

(defun tree-sitter-query-path ()
  "Return the path to the tree-sitter highlight query for Markdown."
  (asdf:system-relative-pathname :lem-markdown-mode "tree-sitter/highlights.scm"))

(define-major-mode markdown-mode language-mode
    (:name "Markdown"
     :keymap *markdown-mode-keymap*
     :syntax-table *markdown-syntax-table*
     :mode-hook *markdown-mode-hook*)
  (lem-tree-sitter:enable-tree-sitter-for-mode
   *markdown-syntax-table* "markdown" (tree-sitter-query-path))
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 4
        (variable-value 'calc-indent-function) 'markdown-calc-indent)
  (add-hook (variable-value 'after-save-hook :buffer (current-buffer))
            'lem-markdown-mode/internal:on-save-default)
  (add-hook (variable-value 'kill-buffer-hook :buffer (current-buffer))
            'lem-markdown-mode/internal:on-kill-default)
  (add-hook (variable-value 'after-change-functions :buffer (current-buffer))
            (lambda (start end old-len)
              (declare (ignore end old-len))
              (lem-markdown-mode/internal:on-change-default (point-buffer start)))))

(define-key *markdown-mode-keymap* "C-c C-l" 'markdown-insert-link)

(defun markdown-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (point-column point)))
      (+ column (- tab-width (rem column tab-width))))))

(defun call-without-lsp-mode (function)
  "Call FUNCTION with lsp-mode kept off, if lem-lsp-mode is loaded."
  (if (find-package :lem-lsp-mode)
      (uiop:symbol-call :lem-lsp-mode :call-without-lsp-mode function)
      (funcall function)))

(defmethod execute :around ((mode markdown-mode) command argument)
  "This method switches to the major mode corresponding to a language when a language name is specified in a Markdown code block."
  (call-without-lsp-mode
   (lambda ()
     (with-major-mode (current-major-mode-at-point (current-point))
       (call-next-method)))))

(defun %markdown-insert-link (&optional arg)
  "Insert a Markdown link.
Can be
- <https://lem-project.github.io>
- [Lem](https://lem-project.github.io), or
- [Lem](https://lem-project.github.io \"Lem Project website\").

With ARG (pressing C-u) will prompt for a title/tooltip text."
  (let ((url (prompt-for-string "URL: "
                                :history-symbol 'mh-markdown-url))
        (text
          (prompt-for-string "Link text (blank for plain URL): "))
        (title (if (not (uiop:emptyp arg))
                   (prompt-for-string "Title (tooltip text, optional): ")
                   nil)))
    (with-point ((p (current-point)))
      (cond
        ((and url
              (uiop:emptyp text)
              (uiop:emptyp title))
         (insert-string p (format nil "<~a>" url)))
        ((and url text (uiop:emptyp title))
         (insert-string p (format nil
                                  "[~a](~a)"
                                  text
                                  url)))
        ((and url text title)
         (insert-string p (format nil
                                  "[~a](~a \"~a\")"
                                  text
                                  url
                                  title)))))))

(defun %markdown-insert-link-with-region (start end &optional arg)
  "Insert a Markdown link using the text within a selected region.
Will use the text in the region as default.
Can be:
- [Lem](https://lem-project.github.io), or
- [Lem](https://lem-project.github.io \"Lem Project website\").

With ARG (pressing C-u) will prompt for a title/tooltip text."
  (let* ((url (prompt-for-string "URL: "
                                 :history-symbol 'mh-markdown-url))
         (text (points-to-string start end))
         (change-text (prompt-for-string "Link text: " :initial-value text))
         (title (if (not (uiop:emptyp arg))
                    (prompt-for-string "Title: ")
                  nil)))
    (delete-between-points start end)
    (if (not (null title))
        (insert-string start
                       (format nil
                               "[~a](~a \"~a\")"
                               change-text
                               url
                               title))
      (insert-string start
                     (format nil
                             "[~a](~a)"
                             change-text
                             url)))))

(define-command markdown-insert-link (&optional arg) (:universal-nil)
  "Insert a Markdown link at the current point."
  (let ((is-mark (lem/buffer/internal:mark-active-p (cursor-mark (current-point)))))
    (if (not is-mark)
        (%markdown-insert-link arg)
      (let ((start   (region-end (current-buffer)))
            (end     (region-beginning (current-buffer))))
        (%markdown-insert-link-with-region start end arg)))))

(define-file-type ("md" "markdown") markdown-mode)
