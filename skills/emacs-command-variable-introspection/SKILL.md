---
name: emacs-command-variable-introspection
title: Emacs Command And Variable Introspection
description: Inspect Emacs commands, variables, bindings, defaults, and buffer-local values.
type: instruction
tools: emacs_eval
capability: true
family: emacs-introspection
source: builtin
feature: emacs
modes: emacs-lisp-mode, lisp-interaction-mode
keywords: command, commands, variable, variables, customize, symbol, binding, describe-function, describe-variable
disclosure: active
risk: low
---

# Emacs Command And Variable Introspection

Use Emacs introspection functions to inspect commands and variables. Distinguish symbol existence, function binding, interactive command status, default values, and buffer-local values.

When checking key bindings, inspect the active keymaps in the relevant buffer.
