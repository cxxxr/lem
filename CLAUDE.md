# CLAUDE.md - Lem Editor

Lem is a text editor written in Common Lisp with multiple frontend support (terminal, GUI, browser).

## Quick Reference

```bash
# Build and run
make sdl2          # GUI version
make ncurses       # Terminal version

# Development
qlot install       # Install dependencies
make test          # Run tests
```

## Project Structure

```
src/                    # Core editor
├── lem.lisp            # Entry point
├── buffer/             # Text buffer system
├── window/             # Window management
├── mode.lisp           # Mode system
├── keymap.lisp         # Key bindings
├── defcommand.lisp     # Command definition
└── interface.lisp      # Frontend abstraction

frontends/              # UI implementations
├── sdl2/               # Desktop GUI
├── ncurses/            # Terminal
└── webview/            # Browser-based

extensions/             # Language modes (50+)
├── lisp-mode/          # Common Lisp with REPL
├── lsp-mode/           # LSP client
└── vi-mode/            # Vim emulation
```

## Architecture

Four-layer design: Frontends → Interface (lem-if:*) → Core → Extensions

Key abstractions:
- **Buffer**: Text storage with points, marks, undo history
- **Window**: Buffer display, organized in tree (splits) or floating
- **Mode**: Major (one per buffer) + Minor (multiple, toggleable)
- **Command**: Interactive operation via `define-command`

## Coding Standards

This project uses code-contractor for automated PR review. code-contractor is a GitHub App that validates pull requests against rules defined in `contract.yml`.

**All coding standards are defined in `contract.yml` at the repository root.** Refer to it for:
- Package and file structure rules
- Documentation requirements
- Style conventions (loop syntax, error handling, etc.)
- Functional programming guidelines

## Key Files for Common Tasks

| Task | Files |
|------|-------|
| Add command | `src/defcommand.lisp` for macro, `src/commands/` for examples |
| Add mode | `extensions/` for examples, inherit `language-mode` |
| Add frontend | `src/interface.lisp` for protocol, `frontends/` for impl |
| Buffer ops | `src/buffer/internal/buffer.lisp`, `src/buffer/internal/edit.lisp` |
| Window ops | `src/window/window.lisp` |

## Development Notes

- Use `lem-core` package symbols, avoid `lem::` internal access
- Frontends implement `lem-if:*` generic functions
- Modes register via `define-major-mode` / `define-minor-mode`
- Commands register via `define-command` with argument descriptors

## Verification

- Verification means running the change: before committing Lisp code, use the
  `verify` skill (`.claude/skills/verify/`) to recompile the changed files, run
  the relevant tests, and use the change in a real Lem through tmux.
- Run the items of `.claude/skills/verify/checklist.md` whose trigger matches
  the diff. When a bug gets past verification or review, add an item there.
- A check that still fails after three rounds of fixes goes back to the user.
- Do not make a flaky test pass with sleeps, retries or skips; find out why it
  depends on timing or on other tests.
- Reviews follow `REVIEW.md`.
- To open a PR and merge it once it passes verify, the review and CI, use the
  `ship` skill (`.claude/skills/ship/`).

## Configuration

User config locations (in order):
1. `$LEM_HOME/init.lisp`
2. `~/.config/lem/init.lisp`
3. `~/.lem/init.lisp`
4. `~/.lemrc`

## Documentation

- Architecture details: `docs/ARCHITECTURE.md`
- Extension development: `docs/extension-development.md`
- Online docs: https://lem-project.github.io/

## Recent Changes
- 001-multi-language-canvas: Added Common Lisp (SBCL), tree-sitter-cl FFI bindings + lem/core, tree-sitter-cl, lem-call-graph (existing), lem-tree-sitter
- 001-multi-language-canvas: Added [if applicable, e.g., PostgreSQL, CoreData, files or N/A]

## Active Technologies
- Common Lisp (SBCL), tree-sitter-cl FFI bindings + lem/core, tree-sitter-cl, lem-call-graph (existing), lem-tree-sitter (001-multi-language-canvas)
- N/A (in-memory graph structures, optional position caching) (001-multi-language-canvas)
