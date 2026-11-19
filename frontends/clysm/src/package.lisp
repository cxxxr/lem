(defpackage :lem-clysm
  (:use :cl)
  (:export #:clysm #:main #:*font-family* #:*font-size*
           ;; images (image.lisp)
           #:image #:image-p #:image-width #:image-height #:image-url
           #:load-image #:load-image-url #:image-attribute #:insert-image #:insert-image-file
           #:image-viewer-mode
           ;; text of other sizes
           #:scaled-attribute #:insert-text #:rich-text-sample
           ;; Markdown (markdown.lisp)
           #:markdown-live-mode #:*markdown-live-by-default* #:markdown-toggle-task
           #:markdown-fold-region #:markdown-fold-or-indent #:markdown-cycle-folds
           ;; Claude Code (claude.lisp)
           #:*claude-command* #:*claude-arguments* #:claude-code #:claude-code-continue
           #:claude-code-send #:claude-code-send-region #:claude-code-interrupt
           #:claude-code-cycle-mode #:claude-code-yes #:claude-code-always #:claude-code-no
           #:*claude-allow-bypass-permissions* #:*claude-permission-mode*
           #:claude-code-bypass-permissions #:claude-code-restart
           ;; looking at the editor from outside (remote.lisp)
           #:remote-start #:remote-stop))
