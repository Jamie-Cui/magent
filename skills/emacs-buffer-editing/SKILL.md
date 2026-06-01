---
name: emacs-buffer-editing
description: Work with live Emacs buffers, regions, point, narrowing, and window-local editing state.
type: instruction
tools: emacs_eval
---

# Emacs Buffer Editing

When the task is about live buffer state, prefer Emacs-native buffer operations over shell text processing. Respect point, mark, narrowing, major mode, and buffer-local state.

Inspect before editing, and use `save-excursion` / `save-restriction` where appropriate.
