---
name: magit-workflow
title: Magit Workflow
description: Work with Magit buffers and live Git state inside Emacs.
type: instruction
tools: emacs_eval, bash
capability: true
source: package
package: magit
modes: magit-status-mode, magit-diff-mode, magit-revision-mode
features: magit
keywords: magit, stage, unstage, hunk, commit, stash, rebase, status, branch
disclosure: active
risk: medium
---

# Magit Workflow

When the task depends on Magit UI state, inspect Magit buffers and sections directly. Distinguish Magit-visible state from plain `git` command output.

Use shell git only when it answers the question more directly or when no Magit buffer state is relevant.
