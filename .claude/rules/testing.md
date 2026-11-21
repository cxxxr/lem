# Testing in Lem

## Test Framework

Lem uses Rove for testing.

## Running Tests

```bash
# All tests
make test

# Specific system
.qlot/bin/rove lem-tests.asd

# In REPL
(asdf:test-system "lem-tests")
```

## Writing Tests

```lisp
(defpackage :lem-my-mode/tests
  (:use :cl :rove :lem))
(in-package :lem-my-mode/tests)

(deftest test-feature
  (ok (= 1 1))
  (ng (= 1 2))
  (ok (equal "a" "a")))
```

## Test System Definition

```lisp
(defsystem "lem-my-mode/tests"
  :depends-on ("lem-my-mode" "rove")
  :components ((:file "tests/main"))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
```

## CI

Tests run via GitHub Actions (`.github/workflows/test.yml`; the Nix builds in
`ci.yml`):

- On a pull request, on Linux. A PR that changes only `.claude/`, `docs/` or
  `*.md` files runs no tests (`paths-ignore`).
- Not on a merge into `staging` (cxxxr/lem): its head is tested by the daily
  scheduled run, which also builds on macOS and skips a commit that already
  passed. `ship scheduled` (`.claude/skills/ship/`) shows the newest results.
- On a push to `main`: the tests and the Nix builds on every OS, macOS
  included (the same `paths-ignore`).
