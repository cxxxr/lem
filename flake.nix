{
  description = "A flake for lem";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];

      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      perSystem = {pkgs, ...}: let
        # --- Setup & Helpers ---
        lisp = pkgs.sbcl;

        # Helper to generate the Lisp build script used by all variants
        mkBuildScript = {entryPoint ? "lem:main"}:
          pkgs.writeText "build-lem.lisp" ''
            (defpackage :nix-cl-user (:use :cl))
            (in-package :nix-cl-user)

            ;; Load ASDF
            (load "${lem-base.asdfFasl}/asdf.${lem-base.faslExt}")
            (asdf:initialize-output-translations '(:output-translations :disable-cache :inherit-configuration))

            ;; Load Systems
            (mapcar #'asdf:load-system (uiop:split-string (uiop:getenv "systems")))

            ;; Dump Image
            (setf uiop:*image-entry-point* #'${entryPoint})
            (uiop:dump-image "lem" :executable t :compression t)
          '';

        sources = import ./_sources/generated.nix {
          inherit
            (pkgs)
            fetchgit
            fetchurl
            fetchFromGitHub
            dockerTools
            ;
        };

        # --- Core Lisp Dependencies ---

        micros = lisp.buildASDFSystem {
          inherit (sources.micros) pname src version;
          systems = ["micros"];
        };

        jsonrpc = lisp.buildASDFSystem {
          inherit (sources.jsonrpc) pname src version;
          systems = [
            "jsonrpc"
            "jsonrpc/transport/stdio"
            "jsonrpc/transport/tcp"
            "jsonrpc/transport/websocket"
            "jsonrpc/transport/local-domain-socket"
          ];
          lispLibs = with lisp.pkgs; [
            yason
            alexandria
            bordeaux-threads
            dissect
            chanl
            vom
            usocket
            trivial-timeout
            cl_plus_ssl
            quri
            fast-io
            trivial-utf-8
            websocket-driver
            clack
            clack-handler-hunchentoot
            event-emitter
            hunchentoot
          ];
        };

        async-process = let
          c-lib = pkgs.stdenv.mkDerivation {
            inherit (sources.async-process) pname src version;
            nativeBuildInputs = with pkgs; [
              libtool
              libffi.dev
              automake
              autoconf
              pkg-config
            ];
            buildPhase = "make PREFIX=$out";
          };
        in
          lisp.buildASDFSystem {
            inherit (sources.async-process) pname src version;
            systems = ["async-process"];
            lispLibs = [lisp.pkgs.cffi];
            nativeLibs = [c-lib];
            nativeBuildInputs = [pkgs.pkg-config];
          };

        lem-mailbox = lisp.buildASDFSystem {
          inherit (sources.lem-mailbox) pname src version;
          systems = ["lem-mailbox"];
          lispLibs = with lisp.pkgs; [
            bordeaux-threads
            bt-semaphore
            queues
            queues_dot_simple-cqueue
          ];
        };

        # --- Tree-sitter Support ---

        # Use local tree-sitter-cl if available, otherwise use nvfetcher source
        tree-sitter-cl-src = let
          localPath = /home/user/lem-project/tree-sitter-cl;
        in
          if builtins.pathExists localPath
          then localPath
          else sources.tree-sitter-cl.src;

        # C wrapper library for tree-sitter (handles by-value struct returns)
        ts-wrapper = pkgs.stdenv.mkDerivation {
          pname = "ts-wrapper";
          version = "0.1.0";
          src = "${tree-sitter-cl-src}/c-wrapper";
          buildInputs = [pkgs.tree-sitter];
          buildPhase = let
            ext =
              if pkgs.stdenv.hostPlatform.isDarwin
              then "dylib"
              else "so";
          in ''
            $CC -shared -fPIC -o libts-wrapper.${ext} ts-wrapper.c \
              -I${pkgs.tree-sitter}/include \
              -L${pkgs.tree-sitter}/lib \
              -ltree-sitter
          '';
          installPhase = let
            ext =
              if pkgs.stdenv.hostPlatform.isDarwin
              then "dylib"
              else "so";
          in ''
            mkdir -p $out/lib
            cp libts-wrapper.${ext} $out/lib/
          '';
        };

        # lem-terminal native helper (terminal.so), compiled from
        # extensions/terminal/terminal.c against libvterm. Dynamically linked:
        # the Nix stdenv records an RPATH to the libvterm store path, so it
        # resolves at runtime without any bundling or relinking. lem-terminal/
        # ffi.lisp loads it by the literal name "terminal.so" (even on macOS),
        # so the output keeps the .so suffix on both platforms.
        #
        # Uses libvterm-neovim, which is Leonerd's modern libvterm that
        # terminal.c targets. Plain pkgs.libvterm is a different, abandoned
        # glib/curses-based library with an incompatible API. libvterm-neovim
        # is Linux-only in nixpkgs, hence the Linux gate at the use sites, so
        # -lutil (forkpty) is unconditional here.
        terminal-so = pkgs.stdenv.mkDerivation {
          pname = "lem-terminal-so";
          version = "0.1.0";
          src = ./extensions/terminal;
          buildInputs = [pkgs.libvterm-neovim];
          buildPhase = ''
            $CC -shared -fPIC -o terminal.so terminal.c \
              -I${pkgs.libvterm-neovim}/include \
              -L${pkgs.libvterm-neovim}/lib -lvterm \
              -lutil
          '';
          installPhase = ''
            mkdir -p $out/lib
            cp terminal.so $out/lib/
          '';
        };

        # tree-sitter-cl Lisp bindings (from github.com/lem-project/tree-sitter-cl)
        tree-sitter-cl = lisp.buildASDFSystem {
          pname = "tree-sitter-cl";
          version = sources.tree-sitter-cl.version;
          src = tree-sitter-cl-src;
          systems = ["tree-sitter-cl"];
          lispLibs = with lisp.pkgs; [
            cffi
            alexandria
            trivial-garbage
          ];
          nativeLibs = [
            pkgs.tree-sitter
            ts-wrapper
          ];
        };

        # Tree-sitter language grammars
        tree-sitter-grammars = {
          json = pkgs.tree-sitter-grammars.tree-sitter-json;
          markdown = pkgs.tree-sitter-grammars.tree-sitter-markdown;
          yaml = pkgs.tree-sitter-grammars.tree-sitter-yaml;
          nix = pkgs.tree-sitter-grammars.tree-sitter-nix;
          # Languages for Living Canvas multi-language support
          python = pkgs.tree-sitter-grammars.tree-sitter-python;
          javascript = pkgs.tree-sitter-grammars.tree-sitter-javascript;
          typescript = pkgs.tree-sitter-grammars.tree-sitter-typescript;
          go = pkgs.tree-sitter-grammars.tree-sitter-go;
          perl = pkgs.tree-sitter-grammars.tree-sitter-perl;
          # Clojure support
          clojure = pkgs.tree-sitter-grammars.tree-sitter-clojure;
        };

        # --- Webview Specific Dependencies ---

        c-webview = pkgs.stdenv.mkDerivation {
          pname = "c-webview";
          version = "unstable";
          src = sources.webview.src;
          nativeBuildInputs = with pkgs; [
            cmake
            ninja
            pkg-config
          ];
          buildInputs =
            if pkgs.stdenv.hostPlatform.isLinux
            then [
              pkgs.webkitgtk_4_1
              pkgs.webkitgtk_6_0
              pkgs.gtk3
            ]
            else [];

          # Use FETCHCONTENT to use pre-fetched source instead of network
          configurePhase = ''
            runHook preConfigure
            cmake -G Ninja -B build -S c \
              -DCMAKE_BUILD_TYPE=Release \
              -DFETCHCONTENT_SOURCE_DIR_WEBVIEW=${sources.webview-upstream.src}
            runHook postConfigure
          '';
          buildPhase = "cmake --build build";
          installPhase = let
            suffix =
              if pkgs.stdenv.hostPlatform.isLinux
              then "so"
              else "dylib";
          in ''
            mkdir -p $out/lib
            cp build/lib/libexample.${suffix} $out/lib/libwebview.${suffix}
          '';
        };

        # Objective-C bridge used by lem-darwin, which loads it only on
        # Apple Silicon Macs.
        cl-objc = lisp.buildASDFSystem {
          inherit (sources.cl-objc) pname src version;
          systems = ["cl-objc"];
          lispLibs = with lisp.pkgs; [
            cffi
            cffi-grovel
            cffi-libffi
            yacc
            closer-mop
            org_dot_tfeb_dot_hax
            trivial-main-thread
          ];
        };

        # lem-darwin depends on cl-objc only on Apple Silicon Macs.
        darwinLispLibs = pkgs.lib.optionals (
          pkgs.stdenv.hostPlatform.isDarwin && pkgs.stdenv.hostPlatform.isAarch64
        ) [cl-objc];

        cl-webview = lisp.buildASDFSystem {
          inherit (sources.webview) pname src version;
          systems = ["webview"];
          lispLibs = with lisp.pkgs; [
            cffi
            float-features
          ];
          nativeLibs = [c-webview];
          postPatch = ''
            # Fix library loading path and name in the Lisp binding
            sed -i 's/(define-foreign-library (libwebview/(define-foreign-library libwebview/' webview.lisp
            sed -i '/:search-path/,/arm64"))))/d' webview.lisp
            sed -i 's/"libwebview\.so\.0\.12\.0"/"libwebview.so"/' webview.lisp
          '';
        };

        # --- Lem Core Definition ---

        # List of libraries common to all Lem frontends
        commonLispLibs = with lisp.pkgs; [
          micros
          async-process
          jsonrpc
          lem-mailbox
          tree-sitter-cl
          deploy
          iterate
          closer-mop
          trivia
          alexandria
          trivial-gray-streams
          trivial-types
          cl-ppcre
          cl-unicode
          inquisitor
          babel
          bordeaux-threads
          yason
          log4cl
          split-sequence
          str
          dexador
          cl-mustache
          esrap
          parse-number
          cl-package-locks
          trivial-utf-8
          swank
          _3bmd
          _3bmd-ext-code-blocks
          lisp-preprocessor
          trivial-ws
          trivial-open-browser
          frugal-uuid
          hunchentoot
          cl-template
          enchant
        ];

        makeLemDesktopItem = { terminal }: pkgs.makeDesktopItem {
          name = "lem";
          desktopName = "Lem";
          comment = "Common Lisp editor/IDE with high expansibility";
          exec = "lem %F";
          mimeTypes = [
            "text/english"
            "text/plain"
            "text/x-makefile"
            "text/x-c++hdr"
            "text/x-c++src"
            "application/x-shellscript"
            "text/x-c"
            "text/x-c++"
          ];
          icon = "lem";
          terminal = terminal;
          type = "Application";
          categories = [
            "Development"
            "TextEditor"
          ];
        };

        # The base derivation that other variants inherit from
        lem-base = lisp.buildASDFSystem {
          pname = "lem-base";
          version = "unstable";
          src = ./.;
          nativeBuildInputs = [pkgs.makeBinaryWrapper];
          lispLibs = commonLispLibs;

          postPatch = ''
            sed -i '1i(pushnew :nix-build *features*)' lem.asd
          '';

          buildScript = mkBuildScript {entryPoint = "lem:main";};

          installPhase = ''
            runHook preInstall
            mkdir -p $out/bin
            install lem $out/bin
            wrapProgram $out/bin/lem \
              --prefix LD_LIBRARY_PATH : "$LD_LIBRARY_PATH" \
              --prefix DYLD_LIBRARY_PATH : "$DYLD_LIBRARY_PATH"
            runHook postInstall
          '';
        };

        # 1. Ncurses Variant
        lem-ncurses = lem-base.overrideLispAttrs (o: {
          pname = "lem-ncurses";
          meta.mainProgram = "lem";
          systems = [
            "lem-ncurses"
            "tree-sitter-cl"
            "lem-tree-sitter"
          ];
          lispLibs =
            o.lispLibs
            ++ (with lisp.pkgs; [
              cl-charms
              cl-setlocale
            ]);
          nativeLibs =
            [
              pkgs.ncurses
              pkgs.enchant
              pkgs.tree-sitter
              ts-wrapper
            ]
            # libvterm is Linux-only in nixpkgs, so terminal-so (and thus the
            # terminal extension) is only available on Linux Nix builds.
            ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [terminal-so];
          # Add tree-sitter grammar paths (grammars don't have lib/ subdir)
          installPhase = ''
            runHook preInstall
            mkdir -p $out/bin
            install lem $out/bin
            wrapProgram $out/bin/lem \
              --prefix LD_LIBRARY_PATH : "$LD_LIBRARY_PATH:${tree-sitter-grammars.json}:${tree-sitter-grammars.markdown}:${tree-sitter-grammars.yaml}:${tree-sitter-grammars.nix}:${tree-sitter-grammars.python}:${tree-sitter-grammars.javascript}:${tree-sitter-grammars.typescript}:${tree-sitter-grammars.go}:${tree-sitter-grammars.perl}:${tree-sitter-grammars.clojure}" \
              --prefix DYLD_LIBRARY_PATH : "$DYLD_LIBRARY_PATH"

            mkdir -p $out/share/applications
            cp ${makeLemDesktopItem {terminal = true;}}/share/applications/* $out/share/applications
            mkdir -p $out/share/icons/hicolor/256x256/apps
            cp ${./resources/lem.png} $out/share/icons/hicolor/256x256/apps/lem.png

            runHook postInstall
          '';
        });

        # 2. SDL2 Variant
        lem-sdl2 = lem-base.overrideLispAttrs (o: {
          pname = "lem-sdl2";
          meta.mainProgram = "lem";
          systems = [
            "lem-sdl2"
            "tree-sitter-cl"
            "lem-tree-sitter"
          ];
          lispLibs =
            o.lispLibs
            ++ darwinLispLibs
            ++ (with lisp.pkgs; [
              sdl2
              sdl2-ttf
              sdl2-image
              trivial-main-thread
            ]);
          nativeLibs =
            (with pkgs; [
              SDL2
              SDL2_ttf
              SDL2_image
              enchant
              tree-sitter
              ts-wrapper
            ])
            # libvterm is Linux-only in nixpkgs (see lem-ncurses).
            ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [terminal-so];
          installPhase = ''
            runHook preInstall
            mkdir -p $out/bin
            install lem $out/bin
            wrapProgram $out/bin/lem \
              --prefix LD_LIBRARY_PATH : "$LD_LIBRARY_PATH:${tree-sitter-grammars.json}:${tree-sitter-grammars.markdown}:${tree-sitter-grammars.yaml}:${tree-sitter-grammars.nix}:${tree-sitter-grammars.python}:${tree-sitter-grammars.javascript}:${tree-sitter-grammars.typescript}:${tree-sitter-grammars.go}:${tree-sitter-grammars.perl}:${tree-sitter-grammars.clojure}" \
              --prefix DYLD_LIBRARY_PATH : "$DYLD_LIBRARY_PATH"

            mkdir -p $out/share/applications
            cp ${makeLemDesktopItem {terminal = false;}}/share/applications/* $out/share/applications
            mkdir -p $out/share/icons/hicolor/256x256/apps
            cp ${./resources/lem.png} $out/share/icons/hicolor/256x256/apps/lem.png

            runHook postInstall
          '';
        });

        # 3. Webview Variant
        lem-webview = lem-base.overrideLispAttrs (o: {
          pname = "lem-webview";
          meta.mainProgram = "lem";
          systems = [
            "lem-webview"
            "tree-sitter-cl"
            "lem-tree-sitter"
          ];

          # Use the specific webview entry point
          buildScript = mkBuildScript {entryPoint = "lem-webview:main";};

          lispLibs =
            o.lispLibs
            ++ [cl-webview]
            ++ darwinLispLibs
            ++ (with lisp.pkgs; [
              float-features
              command-line-arguments
            ]);

          nativeLibs =
            if pkgs.stdenv.hostPlatform.isLinux
            then [
              pkgs.webkitgtk_4_1
              pkgs.webkitgtk_6_0
              pkgs.gtk3
              pkgs.stdenv.cc.cc.lib
              c-webview
              pkgs.enchant
              pkgs.tree-sitter
              ts-wrapper
              terminal-so
            ]
            else [
              pkgs.stdenv.cc.cc.lib
              c-webview
              pkgs.enchant
              pkgs.tree-sitter
              ts-wrapper
            ];

          postPatch =
            (o.postPatch or "")
            + (
              if pkgs.stdenv.hostPlatform.isLinux
              then ''sed -i 's/fontName:"Monospace"/fontName:"DejaVu Sans Mono"/' frontends/server/frontend/dist/assets/index.js''
              else ''sed -i 's/fontName:"Monospace"/fontName:"Menlo"/' frontends/server/frontend/dist/assets/index.js''
            );

          postInstall =
            if pkgs.stdenv.hostPlatform.isLinux
            then ''
              wrapProgram $out/bin/lem \
                --set FONTCONFIG_FILE "${pkgs.makeFontsConf {fontDirectories = [pkgs.dejavu_fonts];}}" \
                --prefix XDG_DATA_DIRS : "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}" \
                --prefix XDG_DATA_DIRS : "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}" \
                --prefix LD_LIBRARY_PATH : "${tree-sitter-grammars.json}:${tree-sitter-grammars.markdown}:${tree-sitter-grammars.yaml}:${tree-sitter-grammars.nix}:${tree-sitter-grammars.python}:${tree-sitter-grammars.javascript}:${tree-sitter-grammars.typescript}:${tree-sitter-grammars.go}:${tree-sitter-grammars.perl}:${tree-sitter-grammars.clojure}"

              mkdir -p $out/share/applications
              cp ${makeLemDesktopItem {terminal = false;}}/share/applications/* $out/share/applications
              mkdir -p $out/share/icons/hicolor/256x256/apps
              cp ${./resources/lem.png} $out/share/icons/hicolor/256x256/apps/lem.png

            ''
            else ''
              wrapProgram $out/bin/lem \
                --prefix LD_LIBRARY_PATH : "${tree-sitter-grammars.json}:${tree-sitter-grammars.markdown}:${tree-sitter-grammars.yaml}:${tree-sitter-grammars.nix}:${tree-sitter-grammars.python}:${tree-sitter-grammars.javascript}:${tree-sitter-grammars.typescript}:${tree-sitter-grammars.go}:${tree-sitter-grammars.perl}:${tree-sitter-grammars.clojure}"
            '';
        });
      in {
        overlayAttrs = {
          inherit lem-ncurses lem-sdl2 lem-webview;
        };

        packages = {
          inherit lem-ncurses lem-sdl2 lem-webview;
          default = lem-ncurses;
        };

        apps = {
          lem-ncurses = {
            type = "app";
            program = lem-ncurses;
          };
          lem-sdl2 = {
            type = "app";
            program = lem-sdl2;
          };
          lem-webview = {
            type = "app";
            program = lem-webview;
          };
          default = {
            type = "app";
            program = lem-ncurses;
          };
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs;
            [
              # Lisp development
              sbcl
              sbclPackages.qlot-cli

              # Build tools
              gnumake
              pkg-config

              # Node.js for frontend development
              nodejs_22

              # Native libraries for frontends
              ncurses
              SDL2
              SDL2_ttf
              SDL2_image
              sdl3
              sdl3-ttf
              sdl3-image

              # Spell checking
              enchant

              # SSL/TLS support
              openssl

              # Tree-sitter support
              tree-sitter
              pkgs.tree-sitter-grammars.tree-sitter-json
              pkgs.tree-sitter-grammars.tree-sitter-markdown
              pkgs.tree-sitter-grammars.tree-sitter-yaml
              pkgs.tree-sitter-grammars.tree-sitter-nix
              pkgs.tree-sitter-grammars.tree-sitter-python
              pkgs.tree-sitter-grammars.tree-sitter-javascript
              pkgs.tree-sitter-grammars.tree-sitter-typescript
              pkgs.tree-sitter-grammars.tree-sitter-go
              pkgs.tree-sitter-grammars.tree-sitter-perl
              pkgs.tree-sitter-grammars.tree-sitter-clojure

              # Perl Language Server
              pkgs.perl5Packages.PLS # provides 'pls' command (used by lem-perl-mode)

              # Clojure development
              clojure
              clojure-lsp
              leiningen
              babashka

              # Code formatting
              nixfmt-rfc-style

              # Development tools
              direnv
            ]
            ++ lib.optionals stdenv.hostPlatform.isLinux [
			  # Terminal extension
			  pkgs.libvterm-neovim

              # Linux-specific dependencies for webview frontend
              webkitgtk_4_1
              gtk3
            ];

          # Set up library paths for native dependencies
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (
            [
			  pkgs.libffi
              pkgs.ncurses
              pkgs.SDL2
              pkgs.SDL2_ttf
              pkgs.SDL2_image
              pkgs.sdl3
              pkgs.sdl3-ttf
              pkgs.sdl3-image
              pkgs.enchant
              pkgs.openssl
              pkgs.tree-sitter
              ts-wrapper
              pkgs.tree-sitter-grammars.tree-sitter-json
              pkgs.tree-sitter-grammars.tree-sitter-markdown
              pkgs.tree-sitter-grammars.tree-sitter-yaml
              pkgs.tree-sitter-grammars.tree-sitter-nix
              pkgs.tree-sitter-grammars.tree-sitter-python
              pkgs.tree-sitter-grammars.tree-sitter-javascript
              pkgs.tree-sitter-grammars.tree-sitter-typescript
              pkgs.tree-sitter-grammars.tree-sitter-go
              pkgs.tree-sitter-grammars.tree-sitter-perl
              pkgs.tree-sitter-grammars.tree-sitter-clojure
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
              pkgs.webkitgtk_4_1
              pkgs.gtk3
              pkgs.glib
              pkgs.stdenv.cc.cc.lib
            ]
          );

          shellHook = ''
            # Prioritize local tree-sitter-cl c-wrapper for development
            if [ -f "$HOME/lem-project/tree-sitter-cl/c-wrapper/libts-wrapper.so" ]; then
              export LD_LIBRARY_PATH="$HOME/lem-project/tree-sitter-cl/c-wrapper:$LD_LIBRARY_PATH"
            fi
            echo "Lem development environment"
            echo "  SBCL: $(sbcl --version)"
            echo "  qlot: $(qlot --version 2>/dev/null || echo 'available')"
            echo "  clojure-lsp: $(clojure-lsp --version 2>/dev/null | head -1 || echo 'available')"
            echo ""
            echo "Quick start:"
            echo "  qlot install    # Install dependencies"
            echo "  make ncurses    # Build terminal version"
            echo "  make webview    # Build GUI (Webview) version"
            echo "  make sdl2       # Build GUI (SDL2) version"
            echo "  make sdl3       # Build GUI (SDL3) version"
            echo ""
            echo "Clojure tools:"
            echo "  clojure-lsp     # Clojure Language Server"
            echo "  clojure / clj   # Clojure CLI"
            echo "  lein            # Leiningen build tool"
            echo "  bb              # Babashka scripting"
          '';
        };
      };
    };
}
