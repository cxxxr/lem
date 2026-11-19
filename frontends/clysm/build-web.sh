#!/bin/sh
# Build Lem on Clysm for a browser, as one file that needs no server:
#   frontends/clysm/build-web.sh         # build/clysm/web/lem.html
# Open build/clysm/web/lem.html in Chrome or Edge (137 or later), from the
# disk or from any web server.  It contains the image and the files that
# loading Lem reads (the fasls that build.sh compiled); the files written
# in the home directory are kept in the browser's localStorage.
set -e
here="$(cd "$(dirname "$0")" && pwd)"
lem="$(cd "$here/../.." && pwd)"
desktop=$lem/build/clysm/desktop
deps=$lem/build/clysm/deps
out=$lem/build/clysm/web
[ -s "$desktop/lem.wasm" ] && [ -s "$desktop/config.lisp" ] && [ -s "$desktop/boot-list.lisp" ] ||
  LEM_NO_ELECTRON=1 "$here/build.sh"
clysm="$(cd "${CLYSM_DIR:-$here/clysm}" && pwd)"
mkdir -p "$out"
cp "$here/web/boot.lisp" "$here/web/welcome.lisp" "$here/load-lem.lisp" \
   "$desktop/config.lisp" "$desktop/boot-list.lisp" "$out/"
cd "$clysm"
# load Lem on Node.js as the page will, and note the files that it reads
echo "(defvar cl-user::*lem-load-only* t) (load \"$out/boot.lisp\")" > "$out/trace.lisp"
echo "loading Lem to see which files it reads"
CLYSM_TRACE_FILES=$out/trace.json node host/run.mjs "$desktop/lem.wasm" "$out/trace.lisp" > "$out/trace.log" 2>&1 ||
  { tail -20 "$out/trace.log"; exit 1; }
# the heap with Lem loaded, from which the page starts (runtime/image.lisp);
# its modules are those of the fasls in the pack, where it refers to them
echo "saving the heap image"
echo "(defvar cl-user::*lem-save-image* \"$out/lem.image\") (load \"$out/boot.lisp\")" > "$out/save-image.lisp"
rm -f "$out/lem.image"
CLYSM_SAVE_IMAGE=fasls node host/run.mjs "$desktop/lem.wasm" "$out/save-image.lisp" > "$out/save-image.log" 2>&1 ||
  { tail -20 "$out/save-image.log"; exit 1; }
# with the sources of Lem, micros, this frontend and Clysm's runtime (M-.)
# (sources.txt: the sources of what the page loads, written by boot.lisp)
node "$here/web/pack.mjs" "$out/trace.json" "$out/files.pack.gz" \
  "$lem" "$deps/micros" "$here" contrib/micros TREE:runtime LIST:"$out/sources.txt" \
  FILE:"$out/lem.image"
node "$here/web/make-html.mjs" "$clysm" "$desktop/lem.wasm" "$out/files.pack.gz" "$out/boot.lisp" "$HOME" \
  "$(cat "$out/load-count")" > "$out/lem.html"
echo "built $out/lem.html ($(du -h "$out/lem.html" | cut -f1)) — open it in Chrome or Edge"
