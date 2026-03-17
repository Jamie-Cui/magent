---
name: org-structure-workflow
description: Use when the task is about org-mode structure such as headings, subtrees, drawers, lists, code blocks, agenda-oriented text, or editing a live .org buffer.
type: instruction
---

# Org Structure Workflow

When the active task is about org content, treat the buffer as a structured
document rather than plain text.

## When To Prefer This

- the current buffer is in `org-mode`
- the user mentions headings, subtrees, drawers, lists, TODO items, or src blocks
- the task depends on preserving org structure while editing or inspecting

## Working Rules

1. Prefer org-aware operations over plain text substitutions.
2. Preserve heading hierarchy, block boundaries, and drawer structure.
3. Inspect the live buffer structure before making edits.
4. Use shell tooling only for repository-wide support tasks, not for local org edits.

## Good Patterns

- Operate on headings, subtrees, and blocks as structural units.
- Keep generated text compatible with org folding and existing outline depth.
- Be careful with leading stars, drawers, and fenced block boundaries.
