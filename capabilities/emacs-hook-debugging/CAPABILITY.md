---
name: emacs-hook-debugging
title: Emacs Hook, Keymap, And Advice Debugging
description: Diagnose hook order, key binding shadowing, remaps, and advice-related behavior in a live Emacs session.
family: emacs-debugging
source: builtin
feature: emacs
skills: systematic-debugging, emacs-hook-debugging
modes: emacs-lisp-mode, lisp-interaction-mode
keywords: hook, hooks, keymap, key binding, binding, advice, remap, shadowed, local-map
disclosure: active
risk: low
---

Use this capability when behavior depends on interactive dispatch inside Emacs:
hooks firing, keymaps shadowing each other, or advice changing command flow.
