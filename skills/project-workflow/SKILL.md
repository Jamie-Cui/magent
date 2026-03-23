---
name: project-workflow
description: Use when the task is about project roots, project switching, project-scoped file lookup, or project.el commands inside Emacs.
type: instruction
---

# Project Workflow

Treat the project as an editor-scoped workspace, not only as a directory tree.

## When To Prefer This

- the user refers to the current project or project root
- switching projects, project file search, or project compile is relevant
- the answer depends on project.el state, not only filesystem paths

## Working Rules

1. Inspect the active project root before acting on project-scoped commands.
2. Keep project.el workflow separate from generic filesystem traversal.
3. Use shell commands only when the task is repo-wide and editor project state is irrelevant.
