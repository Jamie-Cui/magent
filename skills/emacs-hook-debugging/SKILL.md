---
name: emacs-hook-debugging
description: Diagnose Emacs hook ordering, key binding shadowing, remaps, and advice-related behavior.
type: instruction
tools: emacs_eval
---

# Emacs Hook Debugging

When behavior depends on hooks, keymaps, remaps, or advice, inspect the effective runtime objects directly. Check hook values, command remappings, active minor modes, local/global maps, and advice lists.

Explain which binding or hook actually wins and why.
