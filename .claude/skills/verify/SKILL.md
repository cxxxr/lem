---
name: verify
description: Verify a change to Lem before committing it or saying it works - recompile the changed files, run the relevant Rove tests, and run the change in a real ncurses Lem driven through tmux. Use before every commit of Lisp code in this repository, after fixing a bug, and when asked to check, test or QA a change.
---

# Verify a change to Lem

Verification means running the change. Tests passing is not enough: use the
feature in a real Lem the way a user would, and say what you saw.

All commands go through `.claude/skills/verify/verify` (run it without
arguments for the list; exit status 0 = passed, 1 = failed, 2 = usage). It
works in `$LEM_VERIFY_DIR` (default `${TMPDIR:-/tmp}/lem-verify-$UID`), never in
the repository. Each `LEM_VERIFY_DIR` has its own compile cache, Lem binary and
tmux server, so give parallel runs different directories.

```sh
V=.claude/skills/verify/verify
```

## 1. Pick the checks from the diff

`$V diff` shows the change: the diff from `$V base` (the first of
`cxxxr/staging`, `origin/staging`, `staging`, `origin/main`, `main` that exists;
`git fetch` it if it is behind, or set `LEM_VERIFY_BASE`) plus uncommitted
changes. For a change already merged, use the PR's range instead.

Read [checklist.md](checklist.md) and run the items whose trigger matches the
diff, on top of the steps below.

## 2. Recompile the changed files

```sh
$V compile                # the .lisp files of $V diff
$V compile src/x.lisp
```

It loads Lem and `lem-tests`, then for each file the system it belongs to
(found from the `.asd` files above it, e.g. `lem-ncurses` for
`frontends/ncurses/`), compiles the files again and prints their warnings (not
redefinitions) and the files that failed to compile or load. Expect
`0 warnings` (followed by `N in scripts/ not checked` when scripts changed);
fix any undefined function or variable. A `.lisp` file with a
package definition in the directory of a package-inferred system such as
`lem-ollama`, or below it, that is not one of its components, is in the system
ASDF finds by its path: `lem-ollama/sub/foo` for `sub/foo.lisp`. Not
`Foo.lisp` on Linux: ASDF looks for the lowercase name, and a note says so.
A file in no system is compiled in what is loaded, so its own packages may be
missing, and is not loaded. `scripts/` is left out of the changed files, as
its files load their frontend before they run: a changed script is listed as
not checked, and is compiled only when you name it (`verify build` does not
run it).

## 3. Run the tests

```sh
$V tests lem-tests/frame-multiplexer   # Rove packages, seconds
$V tests                               # all of lem-tests, about 5 minutes
```

- Run the packages that cover the change while working, and all of
  `lem-tests` once before opening a PR. Every package given runs, also after
  one failed. The last line is `VERIFY: tests passed` or
  `VERIFY: tests failed`.
- A bug fix gets a regression test. See it fail on the code before the fix:
  `$V worktree <commit-before-the-fix>` makes a worktree of that commit; copy
  the new test file (and its `lem-tests.asd` line) there, run that worktree's
  own `verify tests` with another `LEM_VERIFY_DIR` (the command prints how),
  and remove the worktree afterwards. Do not check out old code in place.
- If a test fails but the code looks right after switching code versions in
  place, run `$V tests --fresh ...`: ASDF compares whole seconds and can keep a
  fasl of the other version.
- `lem-language-server/micros-tests` needs a micros server on localhost; it
  fails where local connections are blocked. Say so; do not hide it.
- Tests are in `lem-tests.asd`; a new test file must be added there.

## 4. Run the change in Lem

```sh
$V build                      # ncurses Lem; rebuild after every source change
$V start file.txt             # in tmux, cwd $LEM_VERIFY_DIR/work; prints the screen
$V keys C-x b Tab             # tmux send-keys words: C-x, M-x, Enter, Tab, Escape, "text"
$V screen                     # what the terminal shows now
$V tmux resize-window -x 80 -y 24
$V stop
```

- Put the files to open in `$LEM_VERIFY_DIR/work` (it exists after any
  `verify` command). `init.lisp` goes in `$LEM_VERIFY_DIR/home/`.
- The Lem runs without `DISPLAY`, so copy and paste do not touch the user's
  clipboard.
- frame-multiplexer is on: the first line is the tab bar (`C-z c` new tab,
  `C-z n` / `C-z p` switch). While a prompt or popup is open, the current tab's
  label shows its buffer (`*prompt*`, `*popup menu*`).
- `C-g` closes a prompt or a popup list. In a list such as `C-x C-b`
  (list-buffers), typed letters filter the list; `q` does not close it.
- Do what the change is for and the cases around it (another tab, a split
  window `C-x 2`, the filer side window `M-x filer`, an empty buffer, the prompt
  open), and read the screen each time.
- Read the whole screen, or `grep` it for the text you expect. Popups are not
  all at the top: `C-x C-b` (list-buffers) is centered, and its height grows
  with the number of buffers (up to the screen height less 4), so
  `$V screen | head` misses it. Right after starting with one file it is on
  lines 15-19 of the default 30; with 19 buffers, lines 6-27.

Other frontends, when the change is theirs:

- **Server (browser):** `sbcl --load .qlot/setup.lisp --load scripts/build-server.lisp`,
  run `./lem-server --port N --host 127.0.0.1`, drive it with Playwright.
  Delete `.playwright-mcp/` from the repository afterwards.
- **Lem on Clysm:** `frontends/clysm/build.sh` (about 2 minutes) after every
  source change, then `make test-clysm` (expect `lem: ok` and
  `lem (browser): ok`). `make test-clysm` builds the desktop application only
  when there is none: without `build.sh` it tests the old image. It runs
  `build-web.sh` itself when the page is older than the last `build.sh`; run
  `build-web.sh` after changing only `frontends/clysm/web/`.
  `frontends/clysm/tools/lem-remote` looks at a running one.
- **SDL2:** there is no tool to drive it here; say that it was not run.

## 5. When a check fails

- Fix the cause and run the check again. After three rounds that still fail,
  stop and bring the failure to the user with what you tried.
- A flaky test is a bug. Do not add sleeps, retries or skips to make it pass.
  Find why it depends on timing or on other tests (or on a stale fasl, see
  `--fresh`); if you cannot, tell the user.

## 6. Report

Say which checks ran and what they showed, including failures and the
checklist items that applied. Say what was not verified (a frontend you could
not run, a case you could not set up). If something got past this skill and
was found later, add an item to [checklist.md](checklist.md). If this skill
itself was wrong or unclear, fix it.
