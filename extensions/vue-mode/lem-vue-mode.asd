(defsystem "lem-vue-mode"
  :depends-on ("lem/core"
               "lem-lsp-mode"
               "lem-js-mode/lsp")
  :serial t
  :components ((:file "vue-mode")
               (:file "lsp-config")))

