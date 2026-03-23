---
name: emacs-runtime-inspection
title: Emacs Runtime Inspection
description: Inspect live Emacs runtime state such as commands, variables, hooks, keymaps, warnings, and backtraces.
source: builtin
feature: emacs
skills: systematic-debugging, emacs-runtime-inspection
modes: emacs-lisp-mode, lisp-interaction-mode
keywords: emacs, runtime, hook, keymap, advice, command, variable, warning, backtrace, diagnose, debug, messages
disclosure: active
risk: low
---

Prefer this capability when the task is about understanding what live Emacs is
doing right now, especially for diagnosis and runtime inspection.
