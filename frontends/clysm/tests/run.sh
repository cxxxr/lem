#!/bin/sh
# frontends/clysm/tests/run.sh — Lem on Clysm: build the desktop
# application and the page for a browser if needed, run each in Electron
# without a display (the page in a window without Node.js, opened from its
# file), and type into them (drive.js).
here="$(cd "$(dirname "$0")/.." && pwd)"
lem="$(cd "$here/../.." && pwd)"
desktop=$lem/build/clysm/desktop
web=$lem/build/clysm/web
[ -x "$desktop/node_modules/electron/dist/electron" ] || "$here/build.sh" > /dev/null || exit 1
[ -s "$web/lem.html" ] || "$here/build-web.sh" > /dev/null || exit 1
# (a short directory: the socket of a resident application is in it)
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
mkdir -p "$dir/home" "$dir/desktop-data" "$dir/web-data"
electron=$desktop/node_modules/electron/dist/electron
status=0
# each run ends with the line "lem: ok" or "lem (browser): ok"
passed() { tee "$dir/out" | grep -E '^(ok|FAIL|lem)'; grep -qE '^lem[^:]*: ok$' "$dir/out"; }
(cd "$desktop" &&
   LEM_HOME="$dir/home/" LEM_TEST_DIR="$dir" CLYSM_USER_DATA="$dir/desktop-data" \
   CLYSM_ELECTRON_TEST="$here/tests/drive.js" \
   timeout 180 "$electron" . --ozone-platform=headless 2>&1) | passed || status=1
LEM_WEB_PAGE="$web/lem.html" CLYSM_USER_DATA="$dir/web-data" \
  CLYSM_ELECTRON_TEST="$here/tests/drive.js" \
  timeout 180 "$electron" "$here/tests/web-shell.js" --ozone-platform=headless 2>&1 | passed || status=1
exit $status
