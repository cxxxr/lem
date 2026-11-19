#+ros.installing
(uiop:with-current-directory ((uiop:pathname-directory-pathname *load-truename*))
  (unless (uiop:directory-exists-p (merge-pathnames #P".qlot/"))
    (setf (uiop:getenv "SBCL_HOME") "")
    (uiop:run-program '("qlot" "install" "--no-deps")
                      :output t
                      :error-output t))
  #+quicklisp
  (setf ql:*quicklisp-home* (merge-pathnames #P".qlot/"))
  (let ((local-project-dir (or roswell:*local-project-directories*
                               #+quicklisp (copy-list ql:*local-project-directories*))))
    (load (merge-pathnames #P".qlot/setup.lisp"))
    ;; XXX: Not to modify the local project directories to install ros scripts in ~/.roswell/bin
    ;;   ref. https://github.com/roswell/roswell/blob/5b267381a66d36a514e2eee7283543f828541a63/lisp/util-install-quicklisp.lisp#L146
    (set (intern (string :*local-project-directories*) :ql) local-project-dir)))

(defsystem "lem/core"
  :depends-on ("iterate"
               "closer-mop"
               "trivia"
               "alexandria"
               "trivial-gray-streams"
               "trivial-types"
               "cl-ppcre"
               "cl-unicode"
               "inquisitor"
               "babel"
               "bordeaux-threads"
               "yason"
               "log4cl"
               "split-sequence"
               "str"
               "usocket"
               "quri"
               "cl-mustache"
               ;; "lem-encodings"
               #+sbcl
               sb-concurrency
               "lem-mailbox"
               #+sbcl
               "sb-sprof")
  :pathname "src"
  :serial t
  :components ((:module "common"
                :components ((:file "ring")
                             (:file "killring")
                             (:file "history")
                             (:file "timer")
                             (:file "command")
                             (:file "color")
                             (:file "queue")
                             (:file "hooks")
                             (:file "var")
                             (:file "socket")
                             (:file "utils")
                             (:module "character"
                              :serial t
                              :components ((:file "icon")
                                           (:file "eastasian")
                                           (:file "string-width-utils")
                                           (:file "package")))))
               (:module "buffer"
                :serial t
                :components ((:file "errors")
                             (:file "file-utils")
                             (:file "line")
                             (:file "buffer-list-manager")
                             (:file "syntax-table")
                             (:file "interrupt")
                             (:file "package")
                             (:module "internal"
                              :serial t
                              :components ((:file "var")
                                           (:file "editor-variables")
                                           (:file "buffer")
                                           (:file "point")
                                           (:file "edit")
                                           (:file "mark")
                                           (:file "undo")
                                           (:file "buffer-insert")
                                           (:file "basic")
                                           (:file "syntax-predicates")
                                           (:file "search")
                                           (:file "parse-partial-sexp")
                                           (:file "syntax-scan")
                                           (:file "syntax-parser")
                                           (:file "tmlanguage")
                                           (:file "check-corruption")))
                             (:file "encodings")
                             (:file "file")
                             (:file "indent")))
               (:file "internal-packages")
               (:file "system-utils")
               (:file "version")
               (:file "config")
               (:file "errors")
               (:file "system")
               (:file "key")
               (:file "macros")
               (:file "attribute")
               (:file "clipboard")
               (:file "save-excursion")
               (:file "killring")
               (:file "named-point")
               (:file "file")
               (:file "frame")
               (:file "echo")
               (:file "prompt")
               (:file "format")
               (:module "window"
                :serial t
                :components ((:file "window-tree")
                             (:file "window")
                             (:file "virtual-line")
                             (:file "floating-window")
                             (:file "header-window")
                             (:file "side-window")
                             (:file "attached-window")))
               (:file "buffer-ext") ; TODO
               (:file "popup")
               (:file "modeline")
               (:file "command")
               (:file "mode")
               (:file "keymap")
               (:file "defcommand")
               (:file "fundamental-mode")
               (:file "region")
               (:file "event-queue")
               (:file "interp")
               (:file "mouse")
               (:file "context-menu")
               (:file "input")
               (:file "overlay")
               (:file "streams")
               (:file "completion")
               (:file "typeout")
               (:file "cursors")
               (:file "command-advices")
               (:file "interface")
               (:file "highlight-line")
               (:file "syntax-scanner")
               (:file "html-buffer")
               (:file "site-init")
               (:file "command-line-arguments")
               (:file "tabbar-config")
               (:file "lem")

               (:file "color-theme")

               (:module "display"
                :serial t
                :components ((:file "base")
                             (:file "char-type")
                             (:file "logical-line")
                             (:file "physical-line")))

               (:module "commands"
                :serial t
                :components ((:file "move")
                             (:file "edit" :depends-on ("move"))
                             (:file "mark")
                             (:file "word" :depends-on ("edit"))
                             (:file "s-expression" :depends-on ("edit"))
                             (:file "file" :depends-on ("edit"))
                             (:file "project" :depends-on ("file"))
                             (:file "buffer")
                             (:file "window" :depends-on ("move"))
                             (:file "multiple-cursors")
                             (:file "process")
                             (:file "help")
                             (:file "font")
                             (:file "other" :depends-on ("file"))
                             (:file "frame")
                             #+sbcl (:file "sprof")))

               (:file "external-packages")

               (:module "ext"
                :serial t
                :components ((:file "popup-window")
                             (:file "popup-message")
                             (:file "popup-menu")
                             (:file "markdown-buffer")
                             (:file "multi-column-list")
                             (:file "context-menu")
                             (:file "list-buffers")
                             (:file "loading-spinner")
                             (:file "completion-mode")
                             (:file "prompt-window")
                             (:file "insert-char")
                             (:file "tmlanguage")
                             (:file "button")
                             (:file "listener-mode")
                             (:file "interactive-mode")
                             (:file "universal-argument")
                             (:file "kbdmacro")
                             (:file "isearch")
                             (:file "showparen")
                             (:file "line-numbers")
                             (:file "peek-source")
                             (:file "grep")
                             (:file "go-back")
                             (:file "hover")
                             (:file "language-mode")
                             (:file "language-mode-tools")
                             (:file "link")
                             (:file "thingatp")
                             (:file "gtags")
                             (:module "directory-mode"
                              :serial t
                              :components ((:file "file")
                                           (:file "attributes")
                                           (:file "mode")
                                           (:file "internal")
                                           (:file "commands")
                                           (:file "writable-directory")
                                           (:file "keybinds")
                                           (:file "main")))
                             (:file "abbrev")
                             (:file "rectangle")
                             (:file "auto-save")
                             (:file "frame-multiplexer")
                             (:file "filer")
                             (:file "themes")
                             (:file "detective")
                             (:file "read-only-sources")
                             (:file "image-buffer")))

               (:module "ui"
                :serial t
                :components ((:file "theme-list")))))

(defsystem "lem/extension-manager"
  :description "Lem's commands that install packages with Quicklisp"
  :depends-on ("lem/core" "lem-extension-manager")
  :pathname "src/ext"
  :components ((:file "extension-commands" :if-feature :quicklisp)))

(defsystem "lem/deepl"
  :description "Lem's commands that translate with DeepL"
  :depends-on ("lem/core" "dexador")
  :pathname "src/ext"
  :components ((:file "deepl")))

(defsystem "lem/extensions"
  :depends-on (;; Disabled for Nix build due to QL-DIST dependency
               #-nix-build
               "lem/extension-manager"
               "lem/deepl"
               #+sbcl
               "lem-template"
               "lem-welcome"
               "lem-lsp-mode"
               "lem-vi-mode"
               "lem-vi-mode/lisp-mode"
               "lem-vi-mode/dashboard"
               #+sbcl
               "lem-lisp-mode"
               #+sbcl
               "lem-go-mode/lsp"
               "lem-swift-mode"
               "lem-ansible-mode"
               "lem-c-mode"
               "lem-python-mode/lsp"
               "lem-posix-shell-mode"
               "lem-xml-mode"
               "lem-js-mode/lsp"
               "lem-css-mode"
               "lem-html-mode"
               "lem-vue-mode"
               "lem-typescript-mode/lsp"
               "lem-typst-mode"
               "lem-json-mode"
               "lem-rust-mode"
               "lem-zig-mode"
               "lem-kotlin-mode"
               "lem-paredit-mode"
               "lem-nim-mode"
               "lem-odin-mode"
               #-clasp
               "lem-scheme-mode"
               "lem-clojure-mode"

               "lem-patch-mode"
               "lem-toml-mode"

               "lem-yaml-mode/lsp"
               "lem-review-mode"
               "lem-asciidoc-mode"
               "lem-dart-mode"
               "lem-scala-mode"
               "lem-dot-mode"
               "lem-java-mode"
               "lem-haskell-mode"
               "lem-ocaml-mode"
               "lem-asm-mode"
               "lem-wat-mode"
               "lem-makefile-mode"
               "lem-shell-mode"
               "lem-sql-mode"
               "lem-base16-themes"
               #+sbcl
               "lem-elixir-mode"
               "lem-ruby-mode"
               "lem-perl-mode"
               "lem-erlang-mode"
               "lem-documentation-mode"
               "lem-elisp-mode"
               "lem-terraform-mode"
               "lem-nix-mode"
               "lem-markdown-mode/preview"
               "lem-color-preview"
               "lem-lua-mode"
               #-os-windows "lem-terminal"
               "lem-legit"
               "lem-tutor"
               "lem-dashboard"
               "lem-dockerfile-mode"
               "lem-copilot"
               "lem-claude-code"
               "lem-bookmark"
               "lem-mcp-server"
               "lem-transient"
               #+sbcl
               "lem-living-canvas"
               "lem-tree-sitter"
               "lem-git-gutter"
               "lem-skk-mode"
               "lem-emacs-help-mode"
               "lem-display-time-mode"
               "lem-spell-check"
               "lem-tramp"
               "lem-trans"
               "lem-winner-mode"))

(defsystem "lem"
  :version "2.3.0"
  :defsystem-depends-on ("deploy")
  :build-operation #+os-macosx "osx-app-deploy-op" #-os-macosx "deploy-op"
  :build-pathname "lem"
  :entry-point "lem:main"
  :depends-on ("lem-webview"
               "lem-server"
               #+(and os-unix (not os-macosx)) ; workaround: because (adf:make :lem) fails
               "lem-ncurses")
  :pathname "src"
  :components ((:file "macosx" :if-feature :os-macosx)
               (:file "linux" :if-feature (:and :os-unix (:not :os-macosx)))
               (:file "windows" :if-feature :os-windows)))
