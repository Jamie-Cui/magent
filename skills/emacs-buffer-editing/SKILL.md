---
name: emacs-buffer-editing
description: Use when the task is primarily about buffers, regions, point, narrowing, windows, or structure-aware editing inside live Emacs.
type: instruction
---

# Emacs Buffer Editing

Treat buffers, regions, and windows as editor objects, not just text blobs.

## When To Prefer This

- the user refers to the current buffer, point, region, or window
- editing depends on buffer-local state or major-mode semantics
- the task should happen inside live Emacs instead of through shell text processing

## Working Rules

1. Prefer Emacs APIs over shell pipelines for buffer-local editing tasks.
2. Preserve cursor and buffer state when inspecting or transforming text.
3. Be explicit about whether you are reading, previewing, or mutating.
4. Validate assumptions about region activity and current buffer before editing.

## Good Patterns

- Use buffer/region APIs for local transformations.
- Use `save-excursion` or equivalent structure-preserving patterns when reading or editing.
- Prefer mode-aware structure operations to regex-only edits when the buffer format matters.
- Use shell tools only when the task is repository-wide or clearly external to the editor.
