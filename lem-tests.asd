(defsystem "lem-tests"
  :depends-on ("lem/core"
               "lem-fake-interface"
               "lem-lisp-syntax"
               "lem-lisp-mode"
               "lem-legit"
               #+sbcl "lem-mcp-server"
               #+sbcl "lem-language-server"
               #+sbcl "lem-language-client"
               #+sbcl "lem-lsp-mode"
               "lem-tree-sitter"
               "lem-yaml-mode"
               "lem-wat-mode"
               "lem-nix-mode"
               "lem-clojure-mode"
               "lem-typst-mode"
               "cl-ansi-text"
               "trivial-package-local-nicknames"
               "rove"
               "yason")
  :pathname "tests"
  :components ((:file "utilities")
               (:module "buffer"
                :components ((:file "internal")))
               (:module "common"
                :components ((:file "ring")
                             (:file "killring")
                             (:file "history")
                             (:file "timer")))
               #+sbcl
               (:module "language-server"
                :components ((:file "utils")
                             (:file "test-utils")
                             (:file "micros-tests")
                             (:file "tests")
                             (:file "language-features-tests")))
               #+sbcl
               (:module "lsp-mode"
                :components ((:file "mock-client")
                             (:file "test-utils")
                             (:file "tests")
                             (:file "integration-tests")
                             (:file "spec-test")))
               #+sbcl
               (:module "mcp-server"
                :components ((:file "utils")
                             (:file "integration-tests")
                             (:file "edge-case-tests")
                             (:file "display-input-tests")))
               (:module "lisp-syntax"
                :components ((:file "indent-test")
                             (:file "defstruct-to-defclass")))
               (:module "lisp-mode"
                :components ((:file "package-inferred-system")
                             (:file "file-conversion")))
               (:module "tree-sitter"
                :components ((:file "main")))
               (:file "language-mode")
               (:file "killring")
               (:file "string-width-utils")
               (:file "char-type")
               (:file "syntax-test")
               (:file "syntax-scanner")
               (:file "buffer-list-test")
               (:file "keymap")
               (:file "popup-window")
               (:file "prompt")
               (:file "cursors")
               (:file "isearch")
               (:file "self-insert-command")
               (:file "interp")
               (:file "input")
               (:file "file")
               (:file "scala-mode")
               (:file "wat-mode")
               (:file "nix-mode")
               (:file "clojure-mode")
               (:file "typst-mode")
               (:file "completion")
               (:file "command-line-arguments")
               (:file "window")
               (:file "legit")
               (:file "filer")
               (:file "listener-mode")
               (:file "interface")
               (:file "attribute")
               (:file "display-cache")
               (:file "visual-line")
               (:file "proportional-text"))
  :perform (test-op (o c)
                    (symbol-call :rove :run c)))
