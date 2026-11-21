# Verification checklist

Each item has a **trigger** you can see in a diff, a **check** to run when the
trigger matches, and the **origin**: the change or review that taught it.
Read the diff, then run only the items whose trigger matches (usually a few).

Add an item when a bug got past verification or review and the diff that caused
it shows a pattern you could spot next time. Remove items that stop catching
anything. Keep the list short enough to read before each commit.

## Buffers and frames

### Code that offers buffers to choose from
- **Trigger:** a new or changed use of `(buffer-list)` to pick, list, cycle or
  complete buffers (not to look one up by name or file); `bury-buffer`'s return
  value used as the next buffer.
- **Check:** offer `(frame-buffer-list)` instead, so that each frame-multiplexer
  tab only offers its own buffers. With two tabs (`C-z c`), each visiting a
  different file, use the command in one tab and see that the other tab's
  buffer does not come up (and does not appear in that tab afterwards).
- **Origin:** cxxxr/lem#55 (workspace buffers); its review found `quit-window`
  still taking the next buffer from the whole list.

### Killing or deleting buffers
- **Trigger:** changes to `delete-buffer`, `kill-buffer`,
  `strip-buffer-from-frame-windows`, `kill-buffer-hook`, or to what a window
  shows after its buffer goes away.
- **Check:** the cases the review of #55 found: the only buffer of the editor
  (`delete-buffer` directly, the command guards it); the last buffer of a
  frame-multiplexer tab; a buffer shown in two windows (`C-x 2`); a buffer
  shown in a hidden tab (`C-z c`, open it, `C-z p`), in a side window
  (`M-x filer`) or in a floating window (a popup).
- **Origin:** cxxxr/lem#55.

## Tests

### Tests with the fake interface
- **Trigger:** a new or changed test using `with-fake-interface`, especially
  one that kills buffers or creates frames.
- **Check:** run the test package alone **and** inside the whole `lem-tests`
  run. `with-fake-interface` gives each use its own frames and current buffer
  (since cxxxr/lem#63), so the test need not bind them, but the buffer list
  is still shared: buffers made by earlier tests (and not deleted) are in
  `(buffer-list)`, and `other-buffer`, `bury-buffer` or a kill can pick one.
  A test that depends on which buffers exist wraps its body in
  `(with-current-buffers () ...)`, which starts from an empty list and puts
  the old one back afterwards, as `tests/listener-mode.lisp` does.
- **Origin:** cxxxr/lem#55 (a test passed alone and failed in the suite);
  cxxxr/lem#63 (frames and the current buffer per use).

### A test for a bug fix
- **Trigger:** a fix with a new regression test.
- **Check:** run the new test against the code before the fix
  (`verify worktree REV`, see SKILL.md) and see it fail for the reason of the
  bug, then pass with the fix.
- **Origin:** cxxxr/lem#55 (follow-up fix commit).

## Keeping Lem loadable everywhere

### Dependencies of the core and of the base modes
- **Trigger:** `:depends-on` of `lem/core`, `lem.asd`, or a base mode
  (lisp-mode, markdown-mode, vi-mode...) changes; a new dependency on LSP,
  micros, a C library or a network service.
- **Check:** what Lem on Clysm cannot load goes into a separate system
  (`lem-X-mode/lsp`, `lem-markdown-mode/preview`, ...) loaded by
  `lem/extensions`. Run `make test-clysm` (expect `lem: ok` and
  `lem (browser): ok`).
- **Origin:** cxxxr/lem#20–#25 (moving such dependencies out of lem/core).

### System definitions under frontends/clysm
- **Trigger:** a new `.asd` file anywhere under `frontends/clysm/`.
- **Check:** ASDF and Quicklisp find it for SBCL builds too and it hides the
  real system. Stubs are `stubs/NAME/NAME.system`, not `.asd`. `verify`
  excludes `clysm` directories, so it does not catch this. Load Lem without
  it and see that nothing under frontends/clysm is compiled:
  `sbcl --non-interactive --load .qlot/setup.lisp --eval '(ql:quickload :lem-fake-interface)' 2>&1 | grep -o 'frontends/clysm/[^ "]*\.lisp' | sort -u`
  (today this prints `frontends/clysm/clysm/contrib/closer-mop/closer-mop.lisp`, the known case
  below; anything else is new).
- **Origin:** Clysm's `contrib/closer-mop/closer-mop.asd` breaks loading Lem on
  SBCL (found while working on cxxxr/lem#55).

### JavaScript in Lem on Clysm
- **Trigger:** `js:eval` with a string of JavaScript source in
  `frontends/clysm/`.
- **Check:** drive JS packages from Lisp (`js:call`, `js:get`, `js:new`,
  `js:object`, Lisp lambdas as callbacks). For a slow path, find a bulk API
  instead of JS source.
- **Origin:** cxxxr/lem#54 (M-x terminal rewritten without JavaScript source).

## Display

### Window sizes
- **Trigger:** changes to window layout, `adjust-all-window-size`,
  `balance-windows`, side windows, or code run on a display resize.
- **Check:** split windows unevenly, then resize the terminal a few times
  (`tmux -L lem-verify resize-window -x 80 -y 24`, then back) and see that the
  windows keep their shares and side windows stay on the screen.
- **Origin:** cxxxr/lem#47, cxxxr/lem#48.

### Lines with hidden text
- **Trigger:** changes to line rendering, `:invisible` overlays, folding, or
  wrapped lines.
- **Check:** with long wrapped lines and hidden text (a fold with `C-c Tab`,
  Markdown with marks hidden), scroll so that a window begins in the middle of
  a wrapped line, and see that redisplay signals no error.
- **Origin:** cxxxr/lem#46 ("The value 108 is not of type (INTEGER 0 101)" at
  each redisplay), cxxxr/lem#36.

## The verify skill

### Loading code in the verify scripts
- **Trigger:** changes to how `.claude/skills/verify/lisp/*.lisp` load systems
  or files (`ql:quickload`, `asdf:load-system`, `asdf:load-asd`,
  `*compile-file-failure-behaviour*`, what is loaded after `compile-file`).
- **Check:** with throwaway edits, see that each run ends in its own last line
  (`N warnings`, then `N in scripts/ not checked` when scripts changed, or
  `VERIFY: tests ...`) with exit status 1, not a backtrace,
  and names the file: unbalanced parentheses in a `src/` file, a top-level
  `(error ...)`, a form the compiler catches as an error (`(let ((1 2)) nil)`)
  in a `src/` and in a `tests/` file. After `verify compile` of such a file,
  `verify tests` must fail too (no fasl of the failed file is left in the
  cache). A broken `.asd` (`:defsystem-depends-on` a missing system) in a
  directory between a file and its system's `.asd` is noted, and the file is
  still compiled in its system; but when the system above is a
  package-inferred-system (`contrib/ollama/sub/`), the file fails, as the
  broken `.asd` may be its own. With changed files of two systems that depend
  on a broken one (a new `contrib/ollama/a.lisp` with a top-level error, and
  `b.lisp` and `c.lisp` that use it), the error is printed once and the
  other files say `(X did not)`. With cl-migemo missing,
  `contrib/migemo/main.lisp` prints `System "cl-migemo" not found`; a new
  `contrib/migemo/x.lisp` and `y.lisp` that use `lem-migemo/main` (main.lisp
  as a system of its own, without cl-migemo): x prints
  `FAILED to load lem-migemo/x: COMPILE-FILE-ERROR ...` and
  `in contrib/migemo/main.lisp`, y says `(lem-migemo/main did not)`. A
  missing sibling of a component (`(:file "b" :depends-on ("a"))` in a
  throwaway `.asd`) is not taken as a failed system `a`. Revert the edits
  afterwards.
- **Origin:** the reviews of cxxxr/lem#62 (`verify compile` ended in a
  backtrace on a load error, and left fasls of failed files that later runs
  took as up to date); cxxxr/lem#70 and #75 (a failed dependency loaded again
  for each changed file); cxxxr/lem#77 (a missing sibling component taken as a
  failed system).
