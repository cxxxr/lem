#!/usr/bin/env bash -ex
# scripts/macos-deploy.bash [webview|sdl3]
#
# Package Lem as a macOS application, Lem.app, zipped for distribution.
#   webview (default)  the `lem' system; produces lem-macos.zip
#   sdl3               the `lem-sdl3-app' system; produces lem-sdl3-macos.zip

set -o pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
parent_dir="$(dirname "$script_dir")"
cd "$parent_dir"

FRONTEND="${1:-webview}"
case "$FRONTEND" in
  webview)
    SYSTEM="lem"
    ZIP="lem-macos.zip"
    ;;
  sdl3)
    SYSTEM="lem-sdl3-app"
    ZIP="lem-sdl3-macos.zip"
    ;;
  *)
    echo "usage: $0 [webview|sdl3]" >&2
    exit 2
    ;;
esac
# deploy names the bundle and its executable after the system's
# :build-pathname, which each system sets to its own name.
DEPLOY_NAME="$SYSTEM"

if ! command -v brew >/dev/null 2>&1; then
  echo "error: Homebrew not found. Install brew before packaging." >&2
  exit 1
fi

# The SDL3 frontend loads these Homebrew libraries at build time, and deploy
# bundles the ones it loads; check up front so a missing one is named.
if [[ "$FRONTEND" == sdl3 ]]; then
  for formula in sdl3 sdl3_ttf sdl3_image freetype harfbuzz; do
    if ! brew --prefix --installed "$formula" >/dev/null 2>&1; then
      echo "error: Homebrew formula '$formula' is not installed (brew install $formula)." >&2
      exit 1
    fi
  done
fi

# ===== 1) ビルド =====
qlot install

# Build the lem-terminal native helper with libvterm statically linked so the
# bundled terminal.so has no external libvterm dependency. It must exist before
# `asdf:make :lem` so that lem-terminal/ffi.lisp loads it at build time and the
# deploy library bundles it into Lem.app/Contents/MacOS and reloads it at boot.
LEM_TERMINAL_STATIC=1 make terminal-lib

# Start from no bundle: deploy writes into an existing one, and on a
# case-insensitive volume bin/lem.app is the Lem.app an earlier build (of
# either frontend) left, so its files would end up in this one.
rm -rf bin/Lem.app "bin/$DEPLOY_NAME.app" bin/__lem_renaming__.app

qlot exec sbcl --eval "(ql:quickload :$SYSTEM)" --eval "(asdf:make :$SYSTEM)"

# Confirm the deploy library actually bundled terminal.so into the app; a
# missing helper means the terminal extension would silently disable itself.
if ! find "bin/$DEPLOY_NAME.app" -name 'terminal.so' -print -quit | grep -q .; then
  echo "error: terminal.so was not bundled into the app by deploy." >&2
  echo "       Ensure libvterm is installed so it loads at build time." >&2
  exit 1
fi

# Rename the deploy output ($DEPLOY_NAME.app: lem.app or lem-sdl3-app.app)
# to use the user-facing name (Lem.app). We rename the bundle, executable,
# and Info.plist fields here so Finder, the Dock, and the menu bar show "Lem".
#
# APFS (and HFS+) are case-insensitive but case-preserving by default, so
# `mv lem.app Lem.app` is a no-op (same path) and `rm -rf bin/Lem.app`
# would delete the bundle we just built. Force a case change by going
# through a uniquely-named intermediate.
mv "bin/$DEPLOY_NAME.app" bin/__lem_renaming__.app
mv bin/__lem_renaming__.app bin/Lem.app
mv "bin/Lem.app/Contents/MacOS/$DEPLOY_NAME" bin/Lem.app/Contents/MacOS/__lem_renaming__
mv bin/Lem.app/Contents/MacOS/__lem_renaming__ bin/Lem.app/Contents/MacOS/Lem
PLIST="bin/Lem.app/Contents/Info.plist"
/usr/libexec/PlistBuddy -c "Set :CFBundleName Lem"        "$PLIST"
/usr/libexec/PlistBuddy -c "Set :CFBundleDisplayName Lem" "$PLIST"
/usr/libexec/PlistBuddy -c "Set :CFBundleExecutable Lem"  "$PLIST"
/usr/libexec/PlistBuddy -c "Set :CFBundleIconFile Lem"    "$PLIST"

# アイコン（.icns for macOS dock/taskbar icon, .png as fallback）
# Generate the .icns first so a missing icon is a hard failure rather than
# silently shipping an iconless .app. The bundle is now Lem.app, and
# CFBundleIconFile is "Lem", so the icon must be named Lem.icns.
make icns
cp resources/lem.icns bin/Lem.app/Contents/Resources/Lem.icns
cp resources/lem.png  bin/Lem.app/Contents/Resources/Lem.png || true

# SDL3: the frontend's fonts and images. They go to Contents/Resources, where
# frontends/sdl3/deploy.lisp points the frontend at boot, because codesign
# rejects non-code files under Contents/MacOS. Only the files listed here
# ship: the Noto CJK, Noto Color Emoji and FreeMono fonts (about 50 MB) are
# left out, and macOS's own CJK, emoji and braille fonts are used instead
# (see *mac-default-font-files* in frontends/sdl3/font.lisp).
if [[ "$FRONTEND" == sdl3 ]]; then
  SDL3_RESOURCES="frontends/sdl3/resources"
  SDL3_RESOURCES_DEST="bin/Lem.app/Contents/Resources/sdl3/resources"
  mkdir -p "$SDL3_RESOURCES_DEST/fonts"
  for file in icon.png folder.png open-folder.png \
              fonts/NotoSansMono-Regular.ttf fonts/NotoSansMono-Bold.ttf \
              fonts/all-the-icons.ttf fonts/fontawesome.ttf fonts/file-icons.ttf \
              fonts/material-design-icons.ttf fonts/octicons.ttf; do
    cp "$SDL3_RESOURCES/$file" "$SDL3_RESOURCES_DEST/$file"
  done
fi

# ===== 2) OpenSSL を同梱し、参照先を @loader_path 化 =====
APP="bin/Lem.app"
EXE="$APP/Contents/MacOS/Lem"
LIBDIR="$APP/Contents/MacOS"

if [[ ! -x "$EXE" ]]; then
  echo "error: executable not found: $EXE" >&2
  exit 1
fi

OPENSSL_PREFIX="$(brew --prefix openssl@3)"
OPENSSL_LIB="$OPENSSL_PREFIX/lib"

# Cellar の実体（バージョン付きディレクトリ）を解決
LIBCRYPTO_REAL="$(realpath "$OPENSSL_LIB/libcrypto.3.dylib")"
LIBSSL_REAL="$(realpath "$OPENSSL_LIB/libssl.3.dylib")"

# deploy が同名を置いている場合もあるので上書き
cp -f "$OPENSSL_LIB/libssl.3.dylib"    "$LIBDIR/libssl.3.dylib"
cp -f "$OPENSSL_LIB/libcrypto.3.dylib" "$LIBDIR/libcrypto.3.dylib"

# dylib 自身の ID を @loader_path に揃える
install_name_tool -id @loader_path/libssl.3.dylib    "$LIBDIR/libssl.3.dylib"
install_name_tool -id @loader_path/libcrypto.3.dylib "$LIBDIR/libcrypto.3.dylib"

# libssl → libcrypto の参照を絶対パスから置換
# 1) Cellar 実体パス
install_name_tool -change "$LIBCRYPTO_REAL" @loader_path/libcrypto.3.dylib "$LIBDIR/libssl.3.dylib" || true
# 2) opt 経由のパス
install_name_tool -change "$OPENSSL_LIB/libcrypto.3.dylib" @loader_path/libcrypto.3.dylib "$LIBDIR/libssl.3.dylib" || true
# 3) @rpath の場合も @loader_path に寄せる（冪等）
install_name_tool -change @rpath/libcrypto.3.dylib @loader_path/libcrypto.3.dylib "$LIBDIR/libssl.3.dylib" || true

# 互換用シンボリックリンク（無印名で探すケースに備える）
( cd "$LIBDIR" && { ln -sf libssl.3.dylib libssl.dylib; ln -sf libcrypto.3.dylib libcrypto.dylib; } )

# 実行ファイル(lem)は SBCL のダンプ構造のため install_name_tool は当てない

# ===== 2b) Homebrew の依存ライブラリを同梱 =====
# deploy copies only the libraries CFFI loaded itself. Those link to other
# Homebrew libraries by absolute path (libSDL3_ttf -> harfbuzz -> glib,
# graphite2, ...), which a Mac without Homebrew lacks. Copy every such
# dependency into $LIBDIR and point each reference at @loader_path, until
# no library has one left.
#
# A dependency that deploy already copied is matched by its install ID, not
# by file name: deploy names its copies after the path CFFI opened (e.g.
# libSDL3.dylib) while other libraries refer to libSDL3.0.dylib, and loading
# two copies of SDL3 would give the process two separate SDL states.
#
# Homebrew libraries also refer to their siblings as @rpath/<name>, with an
# LC_RPATH of @loader_path/../lib that means nothing inside the app, so those
# references are resolved to Homebrew's copy and rewritten the same way.
#
# Written for the bash 3.2 that macOS ships: no associative arrays.

BREW_PATH_PATTERN='^(/opt/homebrew|/usr/local)/'
DEPENDENCY_PATTERN='^((/opt/homebrew|/usr/local)/|@rpath/)'
BREW_PREFIX="$(brew --prefix)"

# The regular (non-symlink) libraries in $LIBDIR, one per line.
bundled_libraries() {
  find "$LIBDIR" -maxdepth 1 -type f \( -name '*.dylib' -o -name '*.so' \) | sort
}

# A library's install ID, or nothing for a library without one.
install_id() {
  otool -D "$1" | tail -n +2
}

# The Homebrew (or @rpath) libraries $1 links to, one per line, without its
# own ID.
brew_dependencies() {
  local id
  id="$(install_id "$1")"
  otool -L "$1" | tail -n +2 | awk '{print $1}' \
    | grep -E "$DEPENDENCY_PATTERN" | grep -vxF "${id:-//no-id//}" || true
}

# The Homebrew file a dependency reference names: the path itself, or for
# @rpath/<name> the <name> Homebrew links into its lib directory.
dependency_source() {
  case "$1" in
    @rpath/*) echo "$BREW_PREFIX/lib/${1#@rpath/}" ;;
    *) echo "$1" ;;
  esac
}

# Lines "<file name><TAB><Homebrew path>" for every bundled library, by its
# original install ID and the real file that ID resolves to.
BUNDLE_INDEX="$(mktemp)"
trap 'rm -f "$BUNDLE_INDEX"' EXIT

index_library() {
  local name="$1" path="$2"
  printf '%s\t%s\n' "$name" "$path" >> "$BUNDLE_INDEX"
  if [[ -e "$path" ]]; then
    printf '%s\t%s\n' "$name" "$(realpath "$path")" >> "$BUNDLE_INDEX"
  fi
}

while IFS= read -r lib; do
  id="$(install_id "$lib")"
  if [[ "$id" =~ $BREW_PATH_PATTERN ]]; then
    index_library "$(basename "$lib")" "$id"
  fi
done < <(bundled_libraries)

# The bundled file name for the Homebrew library at $1, or nothing.
bundled_name() {
  local real
  real="$(realpath "$1" 2>/dev/null || true)"
  awk -F '\t' -v a="$1" -v b="$real" '$2 == a || (b != "" && $2 == b) { print $1; exit }' \
    "$BUNDLE_INDEX"
}

changed=1
while (( changed )); do
  changed=0
  while IFS= read -r lib; do
    while IFS= read -r dep; do
      [[ -n "$dep" ]] || continue
      source="$(dependency_source "$dep")"
      name="$(bundled_name "$source")"
      if [[ -z "$name" ]]; then
        if [[ ! -e "$source" ]]; then
          echo "error: $(basename "$lib") needs $dep, and $source does not exist." >&2
          exit 1
        fi
        name="$(basename "$source")"
        if [[ -e "$LIBDIR/$name" ]]; then
          echo "error: cannot bundle $source: $LIBDIR/$name is a different library." >&2
          exit 1
        fi
        echo "Bundling $source (needed by $(basename "$lib"))"
        cp "$(realpath "$source")" "$LIBDIR/$name"
        chmod u+w "$LIBDIR/$name"
        index_library "$name" "$source"
        changed=1
      fi
      install_name_tool -change "$dep" "@loader_path/$name" "$lib"
    done < <(brew_dependencies "$lib")
  done < <(bundled_libraries)
done

# Give each bundled Homebrew library an @loader_path ID like libssl's.
while IFS= read -r lib; do
  if [[ "$(install_id "$lib")" =~ $BREW_PATH_PATTERN ]]; then
    install_name_tool -id "@loader_path/$(basename "$lib")" "$lib"
  fi
done < <(bundled_libraries)

# ===== 3) 署名（必須ではないが念のため。失敗しても続行） =====
# install_name_tool invalidates a library's signature, and arm64 macOS will
# not load an unsigned library, so re-sign every one ad hoc.
while IFS= read -r lib; do
  codesign --force --sign - --timestamp=none "$lib" || true
done < <(bundled_libraries)
codesign --force --deep --sign - --timestamp=none "$APP" || true

# ===== 4) 検査（絶対パスが残っていないか） =====
echo "== otool -L libssl.3.dylib =="
otool -L "$LIBDIR/libssl.3.dylib" | sed 's/^/  /'
echo "== otool -L libcrypto.3.dylib =="
otool -L "$LIBDIR/libcrypto.3.dylib" | sed 's/^/  /'

remaining=0
while IFS= read -r lib; do
  # A library's own ID does not matter here (Lem opens it by path), so
  # check only what it links to.
  left="$(brew_dependencies "$lib")"
  if [[ -n "$left" ]]; then
    echo "error: Homebrew or @rpath references remain in $(basename "$lib"):" >&2
    echo "$left" | sed 's/^/  /' >&2
    remaining=1
  fi
done < <(bundled_libraries)
if (( remaining )); then
  exit 1
fi

# ===== 5) 配布用 ZIP =====
README_PATH="bin/README.md"
# A quoted heredoc: inside double quotes the backticks of the code fence
# would run the xattr command instead of writing it.
cat > "$README_PATH" <<'EOF'
The following command must be executed for Lem.app to start.
```
xattr -dr com.apple.quarantine Lem.app/
```
EOF

rm -f "$ZIP"
(
  cd bin
  zip -r "../$ZIP" "Lem.app" "README.md"
)
echo "Packaged: $(pwd)/$ZIP"
