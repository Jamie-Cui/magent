---
name: project-workflow
title: Project Navigation Workflow
description: Use project.el-style project roots, scoped commands, project buffers, and project-aware execution.
type: instruction
tools: emacs_eval, grep, glob, bash
capability: true
family: project
source: package
package: project
modes: prog-mode, text-mode, dired-mode
features: project
keywords: project root, current project, switch project, project file, project buffer, project compile
disclosure: active
risk: low
---

# Project Workflow

Resolve the active project root before running project-scoped operations. Keep file searches, shell commands, and session assumptions tied to that root.

When using Emacs project APIs, inspect the current project object instead of guessing from the current directory.
