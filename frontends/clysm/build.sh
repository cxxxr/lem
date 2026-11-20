#!/bin/sh
# Build Lem on Clysm as a desktop application in build/clysm/desktop/ and
# install Electron there:
#   frontends/clysm/build.sh && build/clysm/desktop/lem
# Clysm is the submodule frontends/clysm/clysm (or CLYSM_DIR names a
# checkout of https://github.com/cxxxr/clysm); Lem is this repository.
# lem-mailbox and micros are cloned into build/clysm/deps/ at the commits of
# qlfile.lock; the other libraries come from Quicklisp (QUICKLISP_HOME, by
# default ~/quicklisp/).  With LEM_NO_ELECTRON set, Electron is not
# installed (for build-web.sh).
set -e
here="$(cd "$(dirname "$0")" && pwd)"
lem="$(cd "$here/../.." && pwd)"
out=$lem/build/clysm/desktop
deps=$lem/build/clysm/deps
mkdir -p "$out" "$deps"
if [ -z "$CLYSM_DIR" ]; then
  [ -f "$here/clysm/tools/build-repl.lisp" ] ||
    git -C "$lem" submodule update --init frontends/clysm/clysm
  CLYSM_DIR=$here/clysm
fi
clysm="$(cd "$CLYSM_DIR" && pwd)"

clone() {   # clone REPOSITORY DIRECTORY [COMMIT]
  # (a checkout of an earlier COMMIT is moved to COMMIT)
  if [ -d "$2" ]; then
    [ -z "$3" ] || [ "$(git -C "$2" rev-parse HEAD)" = "$3" ] && return
  else
    git init -q "$2"
  fi
  git -C "$2" fetch -q --depth 1 "$1" "${3:-HEAD}"
  git -C "$2" checkout -q FETCH_HEAD
}
locked() {  # the commit of the git source NAME in qlfile.lock
  sed -n "/^(\"$1\" \\./,/:version/s/.*:version \"git-\\([0-9a-f]*\\)\".*/\\1/p" "$lem/qlfile.lock"
}
clone https://github.com/lem-project/lem-mailbox.git "$deps/lem-mailbox" "$(locked lem-mailbox)"
clone https://github.com/lem-project/micros.git "$deps/micros" "$(locked micros)"

cd "$clysm"
# the image: the compiler (ASDF compiles and loads Lem) and the UI framework
echo "building the image"
sbcl --script tools/build-repl.lisp "$out/lem.wasm" \
  ui/ui.lisp ui/command.lisp ui/present.lisp ui/pane.lisp ui/dialog.lisp ui/graphics.lisp \
  ui/stream.lisp ui/dom.lisp ui/desktop.lisp lib/repl-main.lisp 2>&1 | grep -v '^; ' || true
[ -s "$out/lem.wasm" ]

# Lem and its libraries, compiled by that image (the first time this takes
# a few minutes)
echo "compiling Lem"
node host/run.mjs "$out/lem.wasm" "$here/prepare.lisp" \
  "$lem" "$deps/lem-mailbox" "$deps/micros" "$out" > "$out/prepare.log" 2>&1 ||
  { tail -20 "$out/prepare.log"; exit 1; }

# the page and the shell
sed -e "s#@CLYSM_ROOT@#$clysm#" -e "s#@APP_DIR@#$out#" "$here/desktop/index.html" > "$out/index.html"
sed "s#'../common.mjs'#'./common.mjs'#" host/web/clysm.mjs > "$out/clysm.mjs"
cp host/common.mjs host/jsffi.mjs host/link.mjs ui/electron/main.js ui/electron/preload.js "$out/"
cp "$here/lem.css" "$here/load-lem.lisp" "$here/desktop/boot.lisp" "$here/desktop/package.json" \
   "$here/desktop/lem" "$here/desktop/train.js" "$here/desktop/hint-image.mjs" \
   "$here/desktop/link-image.mjs" "$out/"
cp "$lem/scripts/install/lem.svg" "$out/lem.svg"   # the icon, for lem --install
rm -rf "$out/fonts" && cp -r "$here/fonts" "$out/fonts"
# worlds (Clysm's contrib/world/): images in workers that the Lisp mode connects to
cp contrib/world/worker.js "$out/world-worker.js"
cp contrib/world/boot.lisp "$out/world-boot.lisp"
# the heap with Lem loaded, from which the page starts (runtime/image.lisp)
echo "saving the heap image"
echo "(defvar cl-user::*lem-save-image* \"$out/lem.image\") (load \"$out/boot.lisp\")" > "$out/save-image.lisp"
rm -f "$out/lem.image"
CLYSM_SAVE_IMAGE=1 node host/run.mjs "$out/lem.wasm" "$out/save-image.lisp" > "$out/save-image.log" 2>&1 ||
  { tail -20 "$out/save-image.log"; exit 1; }
# its modules linked into one (host/link.mjs)
node "$out/link-image.mjs" "$out/lem.image"
if [ -n "$LEM_NO_ELECTRON" ]; then echo "built $out (without Electron)"; exit 0; fi
[ -d "$out/node_modules/electron" ] || (cd "$out" && npm install --no-audit --no-fund)
# (npm may have left the download of Electron itself to its first start)
[ -d "$out/node_modules/electron/dist" ] || (cd "$out" && node node_modules/electron/install.js)
# compilation hints: the functions that run when Lem starts are compiled
# with the modules (host/common.mjs), learnt from a start of the page
echo "learning which functions run when Lem starts"
tmp=$(mktemp -d)
if (cd "$out" && CLYSM_IMAGE_TRAIN=$tmp/ran.json CLYSM_USER_DATA=$tmp/user-data CLYSM_ELECTRON_TEST=train.js \
      timeout 120 node_modules/electron/dist/electron . --ozone-platform=headless) > "$out/train.log" 2>&1 &&
   [ -s "$tmp/ran.json" ]; then
  node "$out/hint-image.mjs" "$out/lem.wasm" "$out/lem.image" "$tmp/ran.json"
else
  echo "(without compilation hints: see $out/train.log)"
fi
rm -rf "$tmp"
echo "built $out — run it with: $out/lem"
