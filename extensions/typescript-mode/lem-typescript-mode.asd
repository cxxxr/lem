(defsystem "lem-typescript-mode"
  :depends-on ("lem/core" "lem-js-mode")
  :serial t
  :components ((:file "typescript-mode")))

(defsystem "lem-typescript-mode/lsp"
  :description "The language server of lem-typescript-mode, for lem-lsp-mode"
  :depends-on ("lem-typescript-mode" "lem-lsp-mode")
  :components ((:file "lsp-config")))
