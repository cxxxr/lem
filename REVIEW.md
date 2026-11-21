# Reviewing changes to Lem

What to look at when reviewing a change to this repository, by a person or by
an agent. Style rules are in `contract.yml`; checks to run for a kind of diff
are in `.claude/skills/verify/checklist.md`. Both apply here: read the
checklist items whose trigger matches the diff.

## Look for these first

- **Global state seen per frame or per buffer.** The buffer list, the current
  buffer and the frames are global; frame-multiplexer tabs, buffer-local
  variables and windows narrow them. A new `(buffer-list)` that chooses what to
  show, or a `(current-buffer)` / `(current-window)` used for a window of
  another frame, is likely wrong (`frame-buffer-list`, `with-current-window`).
- **Edge cases of buffers and windows.** The only buffer, a buffer shown in
  several windows, a buffer killed while shown in a hidden frame, a side or
  floating window, a temporary buffer (not in the buffer list), a window that
  is not switchable, the prompt window open.
- **What the change does to the other frontends.** Core code must not call a
  frontend; it goes through `lem-if:*`. Something added to `lem/core` or to a
  base mode must still load on Lem on Clysm (see the checklist).
- **Hooks and advices that run for every command or redisplay.** Cost per
  keystroke, and errors there break the whole editor.
- **Tests that pass for the wrong reason.** A test of a fix should fail without
  the fix. Tests with `with-fake-interface` share frames and the current buffer
  with earlier tests.

## How to report

- Tag each finding: **Important** (a bug this change adds or makes worse),
  **Pre-existing** (a bug that was already there; the author need not fix it
  here), or **Nit**.
- Give a concrete failure: the input or state, and the wrong result or error.
  A finding without one is a question, so ask it as one.
- Do not repeat what `contract.yml` checks unless the rule is broken in a way
  that matters for this change.
- When a later round of review runs, start from the earlier findings: check
  that they were fixed, then look at what the fixes changed.
