(defsystem "lem-go-mode"
  :depends-on ("lem/core" "yason")
  :serial t
  :components ((:file "go-mode")))

(defsystem "lem-go-mode/lsp"
  :description "The language server of lem-go-mode, for lem-lsp-mode"
  :depends-on ("lem-go-mode" "lem-lsp-mode")
  :components ((:file "lsp-config")))

(defsystem "lem-go-mode/call-graph"
  :description "Call graph provider for Go using tree-sitter"
  :depends-on ("lem-go-mode" "call-graph" "lem-tree-sitter")
  :components ((:file "call-graph-provider")))
