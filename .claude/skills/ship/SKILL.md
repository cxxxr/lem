---
name: ship
description: Ship a branch of Lem - verify it, open its PR to cxxxr/lem staging, run the code review, fix what must be fixed, wait for CI, and merge when every gate passed. Use when the user asks to ship a change, or to open a PR and merge it once it passes the checks.
---

# Ship a change

Take the current branch from "committed" to "merged into cxxxr/lem `staging`",
merging only when every gate below passed for the same commit. Invoking this
skill is the user's go-ahead to push, open the PR, comment on it and merge it;
stop and ask instead when a gate cannot be passed.

The fixed steps go through `.claude/skills/ship/ship` (run it without
arguments for the list). Checks come from the `verify` skill, and the review
from `/code-review`.

Each Bash call starts a new shell: no variable survives from one command to
the next. Write the script path, the PR number, the SHA and the scratchpad
path out in every command. Below, `$S` stands for `.claude/skills/ship/ship`,
`<PR>` for the PR number, `<SHA>` for the commit, and `<scratch>` for a
directory outside the repository (the session scratchpad).

## 1. Preflight

```sh
$S preflight
```

It refuses `staging` and `main`, uncommitted changes to tracked files, a branch
with no commits over `cxxxr/staging`, and one that conflicts with it. If the
change is not committed yet, commit it first (one commit per logical change,
messages like the existing history).

## 2. Verify

Follow the `verify` skill for the whole change (`verify diff` from
`cxxxr/staging`): compile, the test packages that cover it, all of `lem-tests`,
the checklist items that match, and the change used in a real Lem. Every
check must pass. The one allowed failure is
`lem-language-server/micros-tests` when local connections are blocked; say so.

Keep notes of what ran and what you saw: they become the PR's Verification
section.

## 3. Open the PR

Write the PR body to a file outside the repository (the scratchpad): a
Summary of the change, a Verification section (each check with its result, the
checklist items that applied, what was not verified), and the attribution line.
Then:

```sh
$S pr <scratch>/body.md     # prints <PR>; title: the first commit's subject, or give one
```

## 4. Review

Run `/code-review high https://github.com/cxxxr/lem/pull/<PR>` (the full URL: a
bare number resolves to lem-project/lem). It runs in the background; wait for
its findings. Then, by severity:

- **Important**: fix it, verify the fix (the checks it touches, plus a
  regression test that fails before the fix, see `verify worktree`), commit,
  push (`$S pr <scratch>/body.md` again pushes and finds the same PR), and
  review again. A
  later round starts from the earlier findings: check they were fixed, then
  look at what the fixes changed.
- **Pre-existing** and **Nit**: do not fix them in this PR. Add each to the
  Todoist `lem` project (`todoist add "..." -P lem --desc "..."`, details and
  the PR in the description) without asking.

When a finding is wrong, say why in the PR (`$S comment`) instead of changing
the code. After three review rounds that still have Important findings, stop
and bring them to the user.

## 5. Record the evidence

Post what the PR was judged on, for the reader of the PR:

```sh
$S comment <PR> <scratch>/evidence.md
```

The comment lists the commit SHA (`git rev-parse HEAD`), the checks run on it and
their results, the review rounds and what was fixed, and the Todoist tasks
added.

## 6. CI

```sh
git rev-parse HEAD          # <SHA>: the commit you verified and pushed, not $S head
$S wait <PR> <SHA>
```

It waits until `<SHA>` is the PR's head and reads the checks of that commit
(right after a push, the PR may still show the previous commit and its
checks). 0: passed (or only the slow macOS runners still queued at the timeout; say
so). 1: a check failed or was cancelled: read its log (`gh run view --repo cxxxr/lem --log-failed`),
fix it as an Important finding (step 4), and wait again. 3: timed out on other
checks: stop and tell the user.

## 7. Merge

Merge only the commit that was verified, reviewed and passed CI:

```sh
$S merge <PR> <SHA>         # the SHA that was verified, reviewed and waited for
```

It refuses if the PR's head moved since. Then report to the user: the PR
link, the merge commit, what was verified, what the review found and what was
fixed, the Todoist tasks added, and anything not verified.

## Stop and ask the user instead of merging when

- a check or review finding still fails after three rounds of fixes;
- a fix would change what the user asked for, or a finding needs their choice;
- preflight finds a conflict with `staging`;
- CI times out on checks other than the slow ones, or fails for a reason
  outside the change (a runner, the network).
