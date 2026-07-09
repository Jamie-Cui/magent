---
name: emacs-runtime-inspection
title: Emacs Runtime Inspection
description: Inspect live Emacs runtime state such as buffers, variables, hooks, keymaps, features, warnings, and backtraces.
type: instruction
tools: emacs_eval
capability: true
source: builtin
feature: emacs
modes: emacs-lisp-mode, lisp-interaction-mode
keywords: emacs, runtime, hook, keymap, advice, command, variable, warning, backtrace, diagnose, debug, messages
disclosure: active
risk: low
---

# Emacs Runtime Inspection

Use `emacs_eval` to inspect the running Emacs session when behavior depends on live state. Check relevant buffers, variables, active modes, loaded features, hooks, keymaps, messages, warnings, and backtraces before proposing a fix.

Prefer read-only inspection first. When changing state, make the mutation explicit and reversible.
