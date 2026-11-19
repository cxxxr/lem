# Lem on Clysm

Lem running on [Clysm](https://github.com/cxxxr/clysm), a Common Lisp that
compiles Lisp to WebAssembly GC, in an Electron window or a browser page,
with a frontend written with Clysm's GUI framework (Clysm's `ui/`). Lem and
its libraries are compiled by Clysm; Clysm itself is the submodule
`frontends/clysm/clysm`.

```
$ make clysm                             # builds build/clysm/desktop/
$ build/clysm/desktop/lem [FILE...]      # starts it, or shows it at once
$ make test-clysm                        # types into it (and into the page below) without a display
```

For a browser, without a server:

```
$ make clysm-web                         # build/clysm/web/lem.html, one file (about 22 MB compressed)
```

Open `build/clysm/web/lem.html` in Chrome or Edge 137 or later (it needs
JavaScript Promise Integration, which Firefox and Safari do not have yet),
from the disk or from any web server. See [In a browser](#in-a-browser).

`build.sh` (`make clysm`) needs SBCL, Node.js 24, git, npm and Quicklisp
(`~/quicklisp/`, or `QUICKLISP_HOME`). It checks out the submodule
`frontends/clysm/clysm` if it is not (or uses the Clysm named by
`CLYSM_DIR`), clones lem-mailbox and micros at the commits of `qlfile.lock`
into `build/clysm/deps/`, builds the image, compiles this Lem and its
libraries with it (Quicklisp downloads what is missing; the first time this
takes a few minutes), and installs Electron in `build/clysm/desktop/`.

The submodule is checked out only to build Lem on Clysm: Clysm has system
definitions of its own (compatibility layers of some libraries), which a
Quicklisp local project or an ASDF `:tree` that contains this repository
would otherwise find. For the same reason the stubs below are
`stubs/NAME/NAME.system`, found by a search function of `setup.lisp`, not
`.asd` files.

**Starting.** Lem starts in under a second (from the `lem` command to the
editor on the screen, on Linux, while other programs keep the machine busy:
0.76 to 1.06 s; 0.6 s without a display): `build.sh` loads Lem on Node.js
and saves the heap in `lem.image` (heap images, Clysm's `docs/INTERNALS.md`), links
the 4655 modules compiled in it into one (`desktop/link-image.mjs`), and the
page starts from it instead of loading Lem: the image and that module are
instantiated without running their toplevel forms, and the 500,000 objects
are made again from the file. What remains is Electron (0.35 s to load the
page), V8 compiling the module (0.3 s: the functions that run when Lem
starts are compiled with it, in background threads, by compilation hints
that `build.sh` learns from a start, `desktop/train.js`), making the
objects (0.06 s) and Lem's first frame. `lem.image` is valid only for the image that saved it; otherwise,
or if it cannot be loaded, `boot.lisp` loads Lem as before: Lem's 359
fasls, while the page shows how far it is (about three seconds). They are
loaded without ASDF (`load-lem.lisp`): `prepare.lisp` notes them in
`boot-list.lisp` in their order with the systems, which are registered as
loaded and immutable, and Lem's Lisp mode with micros is an extension
(`lem-clysm-lisp.asd`) loaded when a Lisp file is first opened.
`CLYSM_STARTUP_LOG=FILE` makes the application append how long it took
(from `CLYSM_STARTUP_T0`, a time of `Date.now()`) to FILE.

**Starting at once.** Electron alone takes 0.1 to 0.3 s to show an empty
window. So the
application stays resident, as Emacs does with `emacs --daemon` and
`emacsclient`, and `build/clysm/desktop/lem` only tells it through a Unix
domain socket to open files and show its window:

```
$ build/clysm/desktop/lem --install   # start Lem hidden when you log in, and
                                    # add Lem (through lem) to the applications menu
$ build/clysm/desktop/lem FILE        # shows Lem: its first frame in about 20 ms
$ build/clysm/desktop/lem --daemon    # start it now, hidden
$ build/clysm/desktop/lem --stop      # end it
```

* Closing the window hides it; `C-x C-c` ends the editor, and the
  application starts again at once, hidden, so that the next `lem` is
  instant too. Only a `lem` before Lem is resident (the first after
  `--stop`, or before `--install`) waits for it to start.
* Measured on a Linux desktop (Wayland): from `lem` to the window shown 6
  to 8 ms, to its first frame 11 to 22 ms; `make test-lem` measures how long
  a client's file takes to be on the screen (8 to 10 ms).
* The socket is `resident.sock` in the application's data directory
  (`~/.config/Lem/`); its lines are `open FILE`, `show`, `hide` and `stop`
  ("resident" in `ui/electron/main.js`).

## How it works

**The image** (`build/clysm/desktop/lem.wasm`) contains Clysm's compiler and
the GUI framework. The page (`desktop/index.html`) runs it with the files of
the computer (Clysm's `host/web/clysm.mjs` with Node.js's `fs`; the Electron shell
gives the page Node.js when `package.json` says `"node": true`), and
`desktop/boot.lisp` loads Lem and the frontend with ASDF from the fasls that
`build.sh` compiled — `prepare.lisp` ran the same image on Node.js and wrote
where the systems are into `config.lisp`.

**One thread.** Lem runs its editor in a thread that waits for events in a
queue, which the frontend's input thread and timer threads fill, and its
Lisp mode has a thread that waits for micros's messages. A page has one
thread (`src/single-thread.lisp`):

* Lem's threads, and micros's, are Clysm's green threads
  (Clysm's `runtime/thread.lisp`): computations that take turns when they wait,
  suspended by JavaScript Promise Integration;
* where the editor waits for the queue, it waits with `js:await` for a
  promise that sending an event resolves (or a timeout);
* meanwhile the page runs: the listeners of the keyboard send keys, the GUI
  framework updates the screen;
* timers are JavaScript timers that send their functions to the editor, and
  a background job runs at once.

**The screen** (`src/display.lisp`). A view of Lem (a window) is a model of
the GUI framework; `render-line` turns Lem's drawing objects into segments
`(column width text style)` of the view's lines, and `update-display` gives
the lines of the views that changed to their observed slots. The components
that show them are rendered again; a line equal to the one before is not, so
typing a key changes one line of the page. Each kind of window has a
component of its own, in a layer of its own (from below): the windows of the
frame (`buffer-view`), each with its modeline apart (`modeline-bar`) and a
line between windows side by side; the tab bar (`tabbar-view`, a header
window); the minibuffer (the prompt and the message of the frame); the popups
(the other floating windows). The minibuffer and the popups
(`floating-view`) have their borders and are classes `lem-minibuffer` and
`lem-popup` of the page, so that `lem.css` gives each its own look. Text drawn
with some of Lem's attributes (the modeline's, frame-multiplexer's tabs, the
selected item of a menu, the prompt) has the class `lem-a-NAME` of the
attribute, and the cursor `lem-cursor`; `lem.css` makes the surfaces (bars,
cards, borders, the accent) from the colours of the theme, as Zed and VS
Code do: square tabs, the prompt and its completion as one card, a scroll
bar for each window.

**Images** (`src/image.lisp`). An image file (PNG, JPEG, GIF, WebP, SVG,
BMP, ICO, AVIF) opens in an image buffer, fitted to the window: `+` `-` `0`
zoom, `f` `w` `h` fit it to the window, its width, its height. `M-x
insert-image-file` puts an image into the text of a buffer, and Lisp code
does with `lem-clysm:load-image` and `lem-clysm:insert-image` (text whose
attribute has `:image`, which Lem draws as an image object: here an `<img>`
over the cells it takes). The page reads the file with its file system (the
computer's, or the files in memory in a browser, where `M-x open-local-file`
copies an image of the computer too) and decodes it before the editor gets it.
Text is drawn larger or smaller with an attribute that has `:scale` (2: twice
the size): `(lem-clysm:insert-text point "Title" :scale 2 :attribute
'lem:syntax-keyword-attribute)`, or `lem-clysm:scaled-attribute`. It takes
`(* scale width)` columns (not rounded, so that the text does not move where
Lem cuts it, at the cursor) and `(ceiling scale)` lines, and the text
and images of a line stand on its bottom, so that one buffer has text of
several sizes and images between them; `M-x rich-text-sample` shows one.

**Input** (`src/input.lisp`). Keys are typed into a text field that follows
the cursor, so that an input method shows its composition there: keys with a
modifier or without a character come from `keydown`, characters from
`input`, and a composed text from `compositionend`. The mouse clicks, drags
and scrolls in cells.

**Lisp** (`src/micros.lisp`). The Lisp mode is Lem's own: completion,
argument lists as you type, `M-.`, describe, the inspector, the debugger,
macroexpansion and the REPL are answers of micros (Lem's SWANK). Lem starts
a micros server in its own image and connects to it (its self-connect);
here, without sockets, the server listens and the Lisp mode connects with
the sockets of `stubs/usocket/`, which connect within the image: a
connection is two pipes, and a reader waits (a green thread) until the
other end writes. micros's backend for Clysm is Clysm's `contrib/micros/clysm.lisp`
(from that of SWANK). What micros evaluates is compiled to WebAssembly, in
a thread of its own: the editor goes on meanwhile, and code compiled once
the connection is made checks for interrupts, so that `C-c C-c` in the REPL
interrupts a computation (the debugger) and long ones take turns with the
editor. In a browser the page has the sources of Lem, micros, this frontend
and Clysm's runtime, so that `M-.` opens them.

**Worlds** (`src/world.lisp`, Clysm's
[`contrib/world/`](https://github.com/cxxxr/clysm/tree/main/contrib/world)). `M-x
world-start` starts another image in a worker, with a micros server of its
own, and connects the Lisp mode to it: the REPL, `C-M-x` and the rest then
work there, the editor goes on while a computation loops, and `M-x
world-kill` ends the world whatever it does (the Lisp mode is then back in
the editor's image). A world starts in about a second and a half. In the
desktop application only.

**Extensions** (`src/extensions.lisp`). Lem's language modes (C, CSS, Go,
HTML, JavaScript, JSON, Makefile, Markdown, shell, Python, Rust, SQL, TOML,
TypeScript, XML, YAML), vi-mode and the base16 color themes (about 190) are
compiled with the rest but loaded when first used, so that they do not make
Lem start later: the file types of a mode open their files with a function
that loads the mode's system with ASDF and then turns the mode on, a command
of the mode's name does the same, and `load-theme` loads the themes first
(also before Lem starts, when the theme saved last time is one of them).
What they cannot have here is in systems of their own in Lem, which are not
loaded: their language servers (`lem-go-mode/lsp`, ...) and the preview of
Markdown (`lem-markdown-mode/preview`); tree-sitter is a stub
(`stubs/lem-tree-sitter/`). Lem's system definitions are loaded as they
are, without fixups.

## In a browser

The demo at https://cxxxr.github.io/lem/ is this page, built by the
workflow `.github/workflows/clysm-demo.yml` (run by hand). It opens
`web/welcome.lisp`, a short tour, copied into the home directory (and
replaced by a newer one as long as it was not changed there).

The page starts from a heap image too (`build-web.sh` saves it, with
`CLYSM_SAVE_IMAGE=fasls`: its modules are those of the fasls in the pack,
where it refers to them, so that the page holds them once): opened from the
disk, the editor is there 1.2 to 1.4 s after the page is opened (0.4 s of
it unpacking the files). Browsers do not have V8's compilation hints, so
the code compiles as it is first run, and the modules are not linked (the
pack holds them once, in the fasls). Without the heap image (if it cannot
be loaded), `web/boot.lisp` loads Lem while the page shows how far it is:
it loads Lem as a computation that lets the page draw itself between files
(`js:await` of the next frame, at most every 50 ms), and calls
`lemBoot.progress` with the number of files loaded; `build-web.sh` counts
them when it loads Lem on Node.js. A browser without JavaScript Promise
Integration gets a page that says so.


## Files

| Path | Contents |
|---|---|
| `lem-clysm.asd`, `lem-clysm-lisp.asd`, `src/` | the frontend: `single-thread`, `files` (file times), `display`, `input`, `frontend` (the `lem-if` protocol), `browser` (files of the computer), `image` (images), `micros` (the server of the Lisp mode), `extensions` (loaded when first used) |
| `setup.lisp` | finds the stubs, Clysm's worlds and usocket for a page, and `lem-clysm`, for ASDF |
| `stubs/` | the libraries that Lem on Clysm does without: tree-sitter, subprocesses, an HTTP client, log4cl, deploy |
| `clysm/` | Clysm (submodule) |
| `prepare.lisp` | compiles Lem with the image (run by `build.sh` on Node.js), and notes what is loaded at start (`boot-list.lisp`) |
| `load-lem.lisp` | loads Lem without ASDF, showing how far it is in the page |
| `desktop/` | the page, `boot.lisp` (which also saves the heap image) and `package.json` of the application, `lem`, its client, `link-image.mjs` (one module), and `train.js` and `hint-image.mjs` (compilation hints) |
| `web/` | the page for a browser (`index.html`, `boot.lisp`, `welcome.lisp`), `pack.mjs`, `make-html.mjs` |
| `lem.css` | the look of both pages |
| `fonts/` | JetBrains Mono (the text) and Inter (the bars), under the SIL Open Font License: copied next to the desktop page, put into the page for a browser |
| `build.sh`, `build-web.sh` | build `build/clysm/desktop/`, `build/clysm/web/lem.html` |
| `tests/` | `make test-clysm`: `run.sh`, `drive.js` (types into the application), `web-shell.js` (the page in a plain window) |
