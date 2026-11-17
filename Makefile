LISP ?= sbcl --dynamic-space-size 4GiB --noinform --no-sysinit --no-userinit
PREFIX ?= /usr/local
VARIANT ?= webview

ncurses:
	qlot install
	-$(MAKE) terminal-lib
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-ncurses.lisp

# The minimal-build does not include any of the extensions, and uses the ncurses frontend
minimal-build:
	qlot install
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-minimal-build.lisp

sdl2:
	qlot install
	-$(MAKE) terminal-lib
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-sdl2.lisp

# Experimental SDL3 frontend; needs SDL3, SDL3_ttf and SDL3_image.
sdl3:
	qlot install
	-$(MAKE) terminal-lib
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-sdl3.lisp

sdl2-ncurses:
	qlot install
	-$(MAKE) terminal-lib
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-sdl2-ncurses.lisp

server:
	qlot install
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-server.lisp

client:
	qlot install
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-client.lisp

webview:
	qlot install
	-$(MAKE) terminal-lib
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-webview.lisp

webview-ncurses:
	qlot install
	-$(MAKE) terminal-lib
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-webview-ncurses.lisp

lem: webview

.PHONY: legit terminal-lib

legit:
	qlot install
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-legit.lisp

# Build the lem-terminal native helper (terminal.so) for the current
# platform. Requires libvterm and a C toolchain. The editor build targets
# (ncurses, sdl2, webview, ...) invoke this best-effort via `-$(MAKE)
# terminal-lib`, so a missing libvterm/compiler is non-fatal: lem-terminal
# silently disables itself at runtime when the library is missing.
terminal-lib:
	$(LISP) --load scripts/build-terminal.lisp

install-bin:
	install -m 755 lem $(PREFIX)/bin

# TODO: on the fly edit lem.desktop depends on $(PREFIX)
install-desktop:
	install -m 644 scripts/install/lem.svg /usr/share/icons/hicolor/scalable/apps/
	gtk-update-icon-cache /usr/share/icons/hicolor
	install -m 644 scripts/install/lem-$(VARIANT).desktop /usr/share/applications/lem.desktop

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
install: install-bin
else
install: install-bin install-desktop
endif
	@echo "+--------------------------------+"
	@echo "|   Lem installation complete!   |"
	@echo "+--------------------------------+"

# Install targets
install-sdl2:
	$(MAKE) install VARIANT=sdl2

install-sdl3:
	$(MAKE) install VARIANT=sdl3

install-ncurses:
	$(MAKE) install VARIANT=ncurses

install-sdl2-ncurses:
	$(MAKE) install VARIANT=sdl2-ncurses

install-webview:
	$(MAKE) install VARIANT=webview

install-webview-ncurses:
	$(MAKE) install VARIANT=webview-ncurses

test:
	qlot install
	$(LISP) --load .qlot/setup.lisp \
		--eval '(asdf:test-system "lem-tests")' \
		--eval '(asdf:test-system "lem-vi-mode")' \
		--eval '(asdf:test-system "lem-transient")' \
		--eval '(asdf:test-system "lem-sdl2/tests")' \
		--quit

doc:
	qlot install
	$(LISP) --load .qlot/setup.lisp --load scripts/generate-documentation-tests.lisp --eval '(progn (lem-documentation-mode/tests::generate-markdown-file "test.md" :test) (quit))'

update:
	git pull
	qlot install

lint:
	.qlot/bin/sblint lem.asd
	.qlot/bin/sblint extensions/asciidoc-mode/lem-asciidoc-mode.asd
	.qlot/bin/sblint extensions/asm-mode/lem-asm-mode.asd
	.qlot/bin/sblint extensions/c-mode/lem-c-mode.asd
	.qlot/bin/sblint extensions/color-preview/lem-color-preview.asd
	.qlot/bin/sblint extensions/css-mode/lem-css-mode.asd
	.qlot/bin/sblint extensions/dart-mode/lem-dart-mode.asd
	.qlot/bin/sblint extensions/documentation-mode/lem-documentation-mode.asd
	.qlot/bin/sblint extensions/dot-mode/lem-dot-mode.asd
	.qlot/bin/sblint extensions/elisp-mode/lem-elisp-mode.asd
	.qlot/bin/sblint extensions/elixir-mode/lem-elixir-mode.asd
#	.qlot/bin/sblint extensions/encodings/lem-encodings.asd
	.qlot/bin/sblint extensions/erlang-mode/lem-erlang-mode.asd
	.qlot/bin/sblint extensions/go-mode/lem-go-mode.asd
	.qlot/bin/sblint extensions/haskell-mode/lem-haskell-mode.asd
	.qlot/bin/sblint extensions/html-mode/lem-html-mode.asd
	.qlot/bin/sblint extensions/java-mode/lem-java-mode.asd
	.qlot/bin/sblint extensions/js-mode/lem-js-mode.asd
	.qlot/bin/sblint extensions/json-mode/lem-json-mode.asd
	.qlot/bin/sblint extensions/language-client/lem-language-client.asd
	.qlot/bin/sblint extensions/language-server/lem-language-server.asd
	.qlot/bin/sblint extensions/legit/lem-legit.asd
	.qlot/bin/sblint extensions/lisp-mode/lem-lisp-mode.asd
	.qlot/bin/sblint extensions/lisp-syntax/lem-lisp-syntax.asd
	.qlot/bin/sblint extensions/lsp-base/lem-lsp-base.asd
	.qlot/bin/sblint extensions/lsp-mode/lem-lsp-mode.asd
	.qlot/bin/sblint extensions/lua-mode/lem-lua-mode.asd
	.qlot/bin/sblint extensions/makefile-mode/lem-makefile-mode.asd
	.qlot/bin/sblint extensions/markdown-mode/lem-markdown-mode.asd
	.qlot/bin/sblint extensions/nim-mode/lem-nim-mode.asd
	.qlot/bin/sblint extensions/ocaml-mode/lem-ocaml-mode.asd
	.qlot/bin/sblint extensions/odin-mode/lem-odin-mode.asd
	.qlot/bin/sblint extensions/paredit-mode/lem-paredit-mode.asd
	.qlot/bin/sblint extensions/patch-mode/lem-patch-mode.asd
	.qlot/bin/sblint extensions/posix-shell-mode/lem-posix-shell-mode.asd
	.qlot/bin/sblint extensions/process/lem-process.asd
	.qlot/bin/sblint extensions/python-mode/lem-python-mode.asd
	.qlot/bin/sblint extensions/review-mode/lem-review-mode.asd
	.qlot/bin/sblint extensions/rust-mode/lem-rust-mode.asd
	.qlot/bin/sblint extensions/scala-mode/lem-scala-mode.asd
	.qlot/bin/sblint extensions/scheme-mode/lem-scheme-mode.asd
	.qlot/bin/sblint extensions/shell-mode/lem-shell-mode.asd
	.qlot/bin/sblint extensions/spell-check/lem-spell-check.asd
	.qlot/bin/sblint extensions/sql-mode/lem-sql-mode.asd
	.qlot/bin/sblint extensions/swift-mode/lem-swift-mode.asd
	.qlot/bin/sblint extensions/terminal/lem-terminal.asd
	.qlot/bin/sblint extensions/typescript-mode/lem-typescript-mode.asd
	.qlot/bin/sblint extensions/vi-mode/lem-vi-mode.asd
	.qlot/bin/sblint extensions/welcome/lem-welcome.asd
	.qlot/bin/sblint extensions/xml-mode/lem-xml-mode.asd
	.qlot/bin/sblint extensions/yaml-mode/lem-yaml-mode.asd
	.qlot/bin/sblint extensions/ruby-mode/lem-ruby-mode.asd

# Generate macOS .icns from resources/lem.png. The source PNG is currently
# 256x256, which is the largest entry we can include without upscaling.
# Replace lem.png with a 1024x1024 source to also produce 256@2x/512/512@2x
# entries for Retina-quality dock and Finder icons.
resources/lem.icns: resources/lem.png
	mkdir -p resources/lem.iconset
	sips -z 16 16     $< --out resources/lem.iconset/icon_16x16.png
	sips -z 32 32     $< --out resources/lem.iconset/icon_16x16@2x.png
	sips -z 32 32     $< --out resources/lem.iconset/icon_32x32.png
	sips -z 64 64     $< --out resources/lem.iconset/icon_32x32@2x.png
	sips -z 128 128   $< --out resources/lem.iconset/icon_128x128.png
	sips -z 256 256   $< --out resources/lem.iconset/icon_128x128@2x.png
	sips -z 256 256   $< --out resources/lem.iconset/icon_256x256.png
	iconutil -c icns resources/lem.iconset -o $@
	rm -rf resources/lem.iconset

icns: resources/lem.icns

AppImage:
	docker buildx build -f docker/Dockerfile-AppImage --progress=plain --target artifact --output type=local,dest=./artifacts .
