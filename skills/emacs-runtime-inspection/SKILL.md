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

For a single-snapshot question such as a count, current buffer/window/mode, variable value, or enabled state, use the first correctly scoped, read-only inspection that directly answers the requested quantity or state. Answer from that snapshot without exploring adjacent categories or repeating the inspection unless you discover that its scope or semantics were wrong, or the user asked for a distinction. Tool calls can themselves change live state, so do not replace a valid matching snapshot with a later self-caused observation.

Treat repeating the same or a nearly identical inspection as a loop signal: stop and use the evidence already collected. When several related values are genuinely needed, batch them into one expression after determining their required shape. If raw buffer or document text already answers a review or summary request, use it directly instead of spending calls on cosmetic conversion.
