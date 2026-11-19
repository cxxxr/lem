(defsystem "lem-markdown-mode"
  :depends-on ("lem/core"
               "lem-lisp-mode"
               "lem-posix-shell-mode"
               "lem-json-mode"
               "lem-tree-sitter")
  :serial t
  :components ((:file "internal")
               (:file "languages")
               (:file "syntax-parser")
               (:file "markdown-mode")
               (:file "interactive")))

(defsystem "lem-markdown-mode/preview"
  :description "Previews of Markdown buffers, in a browser or in an HTML buffer"
  :depends-on ("lem-markdown-mode"
               "3bmd"
               "3bmd-ext-code-blocks"
               "lisp-preprocessor"
               "trivial-ws"
               "trivial-open-browser")
  :pathname "preview"
  :serial t
  :components ((:file "preview")
               (:file "external-browser")
               (:file "html-buffer")))
