(defsystem "lem-ansible-mode"
  :description "Ansible major mode for Lem"
  :depends-on ("lem/core"  "lem-lsp-mode" "lem-tree-sitter" "lem-yaml-mode/lsp")
  :serial t
  :components ((:file "ansible-mode")
               (:file "lsp-config")))
