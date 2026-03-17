---
name: emacs-config-reload
description: Use when the task is about reloading init files, package state, or stale runtime configuration in the current Emacs.
type: instruction
---

# Emacs Config Reload Workflow

Treat config reload work as a runtime problem, not only a file-editing problem.

## When To Prefer This

- `init.el` or `early-init.el` changes did not take effect
- package or config reload order matters
- stale state remains after re-evaluating config
- the user is trying to reload one part of Emacs without restarting

## Working Rules

1. Separate file edits from runtime reload steps.
2. Check what is already loaded before reloading more code.
3. Prefer the smallest reload or re-evaluation that answers the question.
4. Call out when a full restart is cleaner than partial reload.
