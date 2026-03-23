---
name: magit-workflow
description: Use when the task is about Magit buffers, staged state, hunks, commits, branches, or git work being performed inside Emacs rather than only in the shell.
type: instruction
---

# Magit Workflow

Prefer Magit-aware reasoning when the user is working inside Magit or talking
about live git state as seen from Emacs.

## When To Prefer This

- `magit` features are loaded or the active buffer is a Magit buffer
- the user mentions Magit status, hunks, staging, unstaging, commits, stash, or rebase inside Emacs
- the task depends on what the current Magit buffer is showing

## Working Rules

1. Distinguish Magit UI state from repository state.
2. Prefer `emacs_eval` for inspecting live Magit buffers and commands.
3. Use shell git commands when the task is repo-wide, reproducible, or better expressed outside Emacs.
4. Treat destructive git actions conservatively and respect existing permission prompts.

## Good Patterns

- Read the current Magit buffer state before acting.
- Use Magit-aware inspection when the answer depends on staged hunks, sections, or transient UI state.
- Fall back to normal git commands only when Magit state is not needed.
