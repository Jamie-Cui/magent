---
name: emacs-hook-debugging
description: Use when behavior depends on hooks, keymaps, remaps, or advice in the running Emacs session.
type: instruction
---

# Emacs Hook, Keymap, And Advice Debugging

Prefer runtime inspection over source-only guesses when command dispatch is the
problem.

## When To Prefer This

- a hook does not seem to run, or runs in the wrong order
- a key binding is shadowed, remapped, or unexpectedly local
- advice may be changing command behavior
- the user is debugging interactive Emacs behavior rather than file contents

## Working Rules

1. Inspect live hook values, keymaps, and advice before proposing fixes.
2. Distinguish global state from buffer-local state.
3. Explain which layer is responsible: hook list, active keymap, remap, advice, or command definition.
4. Keep inspection read-only until there is a concrete hypothesis.
