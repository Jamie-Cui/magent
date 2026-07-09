---
name: emacs-hook-debugging
title: Emacs Hook, Keymap, And Advice Debugging
description: Diagnose Emacs hook ordering, key binding shadowing, remaps, and advice-related behavior.
type: instruction
tools: emacs_eval
capability: true
family: emacs-debugging
source: builtin
feature: emacs
modes: emacs-lisp-mode, lisp-interaction-mode
keywords: hook, hooks, keymap, key binding, binding, advice, remap, shadowed, local-map
disclosure: active
risk: low
---

# Emacs Hook Debugging

When behavior depends on hooks, keymaps, remaps, or advice, inspect the effective runtime objects directly. Check hook values, command remappings, active minor modes, local/global maps, and advice lists.

Explain which binding or hook actually wins and why.
