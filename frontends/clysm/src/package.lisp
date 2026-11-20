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
           #:*claude-command* #:*claude-arguments* #:claude #:claude-continue
           #:claude-send #:claude-send-region #:claude-interrupt
           #:claude-cycle-mode #:claude-yes #:claude-always #:claude-no
           #:*claude-allow-bypass-permissions* #:*claude-permission-mode*
           #:claude-bypass-permissions #:claude-restart
           ;; looking at the editor from outside (remote.lisp)
           #:remote-start #:remote-stop))
