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
with no commits over `cxxxr/staging`, and one that conflicts with it (it lists
the files). If the change is not committed yet, commit it first (one commit per
logical change, messages like the existing history).

A conflict is for the user (see the end). Once the branch is rebased on
`cxxxr/staging`, verify it again: the code under the change is not the one
verified. `$S pr` pushes a rebased branch over the one already pushed
(`--force-with-lease`: only if nobody else pushed to it since); the commits
reviewed before the rebase are gone, so review the new ones too.

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

When the PR exists already (a later round), `$S pr` pushes and replaces the
PR's body with the file: update the Verification section in it first.

## 4. Review

Run `/code-review high https://github.com/cxxxr/lem/pull/<PR>` (the full URL: a
bare number resolves to lem-project/lem). It runs in the background; wait for
its findings. Then, by severity:

- **Important**: fix it, verify the fix (the checks it touches, plus a
  regression test that fails before the fix, see `verify worktree`), commit,
  update the Verification section of `<scratch>/body.md`, push
  (`$S pr <scratch>/body.md` again pushes, finds the same PR and sets its
  body), and review again. A later round starts from the earlier findings:
  check they were fixed, then look at what the fixes changed.
- **Pre-existing** and **Nit**: do not fix them in this PR. Add each to the
  Todoist `lem` project without asking, with the details and the PR in the
  description and one of two labels:
  `todoist add "..." -P lem -l ai --desc "..."` when the cause and the fix are
  clear and an agent can fix it and check it with `verify`;
  `todoist add "..." -P lem -l human --desc "..."` when it needs a choice of
  design, of feature or of process, or work that cannot be done here (a real
  macOS, the user's own setup). No `@` in the title: the CLI takes `@word` as a
  label.
  Do not add a finding that is already a task. The titles alone miss the same
  finding worded another way: read the descriptions too, e.g.
  `todoist list -f '#lem' --json | jq -r '.[] | "\(.id)\t\(.content)\t\(.description)"'`,
  and look for the same file, command or function. Note the ID of each task
  that matched instead.

When a finding is wrong, say why in the PR (`$S comment`) instead of changing
the code. After three review rounds that still have Important findings, stop
and bring them to the user.

## 5. CI

```sh
git rev-parse HEAD          # <SHA>: the commit you verified and pushed, not $S head
$S wait <PR> <SHA>
```

It waits until `<SHA>` is the PR's head and reads the checks, the commit
statuses and the workflow runs of that commit (right after a push, the PR may
still show the previous commit and its checks). Every workflow run must have
completed, so jobs created late (`needs:`) are waited for too.

| Exit | Meaning | Then |
|------|---------|------|
| 0 | passed, or only the slow macOS checks still pending at the timeout (it names them) | step 6; note the pending checks for step 8 |
| 1 | a check, status or workflow run failed or was cancelled | read its log (`gh run view --repo cxxxr/lem --log-failed`), fix it as an Important finding (step 4), and wait again on the new SHA |
| 3 | timed out on other checks, or `<SHA>` never became the PR's head | stop and tell the user |
| 4 | GitHub could not be read three polls in a row (network, `gh auth`, a wrong PR number); it prints gh's error | not a CI failure: fix the cause if it is yours (the PR number), else stop and tell the user |
| 5 | the PR's head moved past `<SHA>` to a newer commit | someone pushed to the branch: stop and tell the user |

## 6. Record the evidence

After CI passed and just before the merge, so that it describes the SHA that
is merged, post what the PR was judged on, for the reader of the PR:

```sh
$S comment <PR> <scratch>/evidence.md
```

The comment lists the commit SHA (the `<SHA>` of step 5), the checks run on it
and their results (with CI, and the slow checks still pending), the review
rounds and what was fixed, each Todoist task added with its label, and each
finding not added because a task had it already, with that task's ID.

## 7. Merge

Merge only the commit that was verified, reviewed and passed CI:

```sh
$S merge <PR> <SHA>         # the SHA that was verified, reviewed and waited for
```

It refuses if the PR's head moved since. Then report to the user: the PR
link, the merge commit, what was verified, what the review found and what was
fixed, the Todoist tasks added (with their labels) and the findings that were
tasks already (their IDs), the slow checks left pending (step 8), and anything
not verified.

## 8. Slow checks left pending

When step 5 passed with slow checks still pending, their result comes after
the merge; nobody would read it. Wait for them in the background (the PR's
head stays `<SHA>` after the merge):

```sh
SHIP_CI_SLOW= $S wait <PR> <SHA>     # run_in_background: none counts as slow now
```

- 0: say in the report that they passed.
- 1: the failure is on `staging` now. Add a task,
  `todoist add "..." -P lem -l ai --desc "..."` with the check, the SHA, the
  PR and the log (`gh run view --repo cxxxr/lem --log-failed`), and tell the
  user.
- 3 (still pending), or the session ends first: add a task to look at them
  later, `todoist add "..." -P lem -l ai --desc "..."` with the PR, the SHA and
  the command above.

## Stop and ask the user instead of merging when

- a check or review finding still fails after three rounds of fixes;
- a fix would change what the user asked for, or a finding needs their choice;
- preflight finds a conflict with `staging`;
- CI times out on checks other than the slow ones (`$S wait` exit 3), fails
  for a reason outside the change (a runner, the network), cannot be read
  (exit 4), or the PR's head moved past the commit (exit 5).
