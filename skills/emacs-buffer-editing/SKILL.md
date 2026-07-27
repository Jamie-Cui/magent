---
name: emacs-buffer-editing
title: Emacs Buffer Editing
description: Work with live Emacs buffers, regions, point, narrowing, and window-local editing state.
type: instruction
tools: emacs_eval, read_file, read_buffer
capability: true
source: builtin
feature: emacs
keywords: buffer, region, point, mark, window, narrow, selected text
disclosure: suggested
risk: low
---

# Emacs Buffer Editing

When the task is about live buffer state, prefer Emacs-native buffer operations over shell text processing. Respect point, mark, narrowing, major mode, and buffer-local state.

Use `read_buffer` when a known file-visiting buffer's live text is needed,
including unsaved edits. Use `read_file` only when the saved disk version is
the intended source of truth. Use `emacs_eval` for non-file buffers and
editor state beyond text pagination.

Inspect before editing, and use `save-excursion` / `save-restriction` where appropriate.
