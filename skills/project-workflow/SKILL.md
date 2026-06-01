---
name: project-workflow
description: Use project.el-style project roots, scoped commands, project buffers, and project-aware execution.
type: instruction
tools: emacs_eval, grep, glob, bash
---

# Project Workflow

Resolve the active project root before running project-scoped operations. Keep file searches, shell commands, and session assumptions tied to that root.

When using Emacs project APIs, inspect the current project object instead of guessing from the current directory.
