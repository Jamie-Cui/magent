---
name: emacs-runtime-inspection
description: Use when the task depends on inspecting live Emacs runtime state such as commands, variables, hooks, keymaps, advice, messages, warnings, or backtraces.
type: instruction
---

# Emacs Runtime Inspection

Use Emacs as the primary source of truth when the question is about live editor
state rather than static files.

## When To Prefer This

- command behavior is surprising
- a variable or mode seems to have the wrong value
- hook, advice, keymap, or buffer-local state matters
- `*Messages*`, `*Warnings*`, or `*Backtrace*` may contain evidence
- the user is asking what Emacs is doing right now

## Working Rules

1. Inspect before changing state.
2. Prefer `emacs_eval` over guessing from source files.
3. Read live runtime facts first, then form a hypothesis.
4. Separate read-only inspection from state-changing actions.

## Useful Inspection Patterns

- Check whether a function or variable exists before assuming it does.
- Inspect buffer-local values separately from default values.
- Inspect hooks, advices, and keymaps directly when behavior depends on them.
- Read `*Messages*`, `*Warnings*`, and `*Backtrace*` when debugging interactive failures.
- When you need an unfamiliar builtin, use runtime discovery first and only then call it.

## Runtime Discovery

Prefer small, targeted `emacs_eval` queries using helpers such as:

- `fboundp`, `boundp`
- `symbol-value`, `default-value`, `buffer-local-value`
- `apropos-internal`, `documentation`
- `where-is-internal`, `commandp`
- hook variables, keymaps, minor mode variables, and current buffer metadata

Do not preload large lists of builtins into the prompt. Discover specific
functions only when needed.
