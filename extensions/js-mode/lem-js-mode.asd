(defsystem "lem-js-mode"
  :depends-on ("lem/core"
               "lem-xml-mode")
  :serial t
  :components ((:file "js-mode")
               (:file "eslint")))

(defsystem "lem-js-mode/lsp"
  :description "The language server of lem-js-mode, for lem-lsp-mode"
  :depends-on ("lem-js-mode" "lem-lsp-mode")
  :components ((:file "lsp-config")))

(defsystem "lem-js-mode/call-graph"
  :description "Call graph provider for JavaScript/TypeScript using tree-sitter"
  :depends-on ("lem-js-mode" "call-graph" "lem-tree-sitter")
  :components ((:file "call-graph-provider")))

