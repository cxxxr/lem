(defsystem "lem-yaml-mode"
  :depends-on ("lem/core" "lem-tree-sitter")
  :serial t
  :components ((:file "yaml-mode")))

(defsystem "lem-yaml-mode/lsp"
  :description "The language server of lem-yaml-mode, for lem-lsp-mode"
  :depends-on ("lem-yaml-mode" "lem-lsp-mode")
  :components ((:file "lsp-config")))
