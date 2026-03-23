---
name: emacs-command-variable-introspection
description: Use when the task is to inspect commands, variables, bindings, or live values inside Emacs.
type: instruction
---

# Emacs Command And Variable Introspection

Use runtime facts to answer questions about commands, variables, and bindings.

## When To Prefer This

- the user asks whether a command or variable exists
- current, default, or buffer-local values matter
- bindings and discoverability matter more than code editing
- the question is primarily descriptive rather than transformational

## Working Rules

1. Confirm symbols exist before reasoning about them.
2. Distinguish command bindings from function existence.
3. Distinguish default values from buffer-local overrides.
4. Give the shortest inspection path that answers the question.
