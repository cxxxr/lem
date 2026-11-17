# lem-sdl3

Experimental SDL3 frontend for Lem, built on
[cl-sdl3](https://github.com/aiffc/cl-sdl3). It is a port of the SDL2 frontend
(`frontends/sdl2`), which remains the default graphical frontend.

## Install

You need SDL3, SDL3_ttf and SDL3_image (SDL3_mixer is not needed).

### macOS

```shell
$ brew install sdl3 sdl3_ttf sdl3_image
```

### Linux

Use your distribution's SDL3 packages where they exist (for example, on
Debian/Ubuntu releases that ship them: `libsdl3-dev`, `libsdl3-ttf-dev`,
`libsdl3-image-dev`). Otherwise build SDL3, SDL3_ttf and SDL3_image from
source as shared libraries.

### Windows (MSYS2)

```shell
$ pacman -S mingw-w64-x86_64-sdl3 mingw-w64-x86_64-sdl3-ttf mingw-w64-x86_64-sdl3-image
```

## Build

```shell
$ make sdl3
$ ./lem
```

## Launch from a REPL

```shell
$ qlot install
$ qlot exec sbcl
```

```common-lisp
* (ql:quickload :lem-sdl3)
* (lem:lem "-i" "sdl3")
```

## Tests

The tests need the SDL3 libraries but no window:

```common-lisp
* (asdf:test-system "lem-sdl3/tests")
```

## Configuration

The SDL3 frontend reads the same settings as the SDL2 frontend
(`:sdl2-font-size`, `:sdl2-normal-font`, `:sdl2-bold-font`,
`:sdl2-cjk-normal-font`, `:sdl2-cjk-bold-font`, `:sdl2-emoji-font`), so an
existing configuration applies unchanged.

### Keyboard layout (Windows / macOS)

If your keyboard is a JIS layout, put the following in `$HOME/.lem/init.lisp`:

```common-lisp
#+lem-sdl3
(lem-sdl3:set-keyboard-layout :jis)
```

## Notes

- cl-sdl3 always loads SDL3_mixer. Lem does not use audio, so
  `lem-sdl3.asd` lets loading continue when SDL3_mixer is missing (see the
  comment at the top of that file).
- The window's app ID (X11 `WM_CLASS`, Wayland `app_id`) is `lem`.
