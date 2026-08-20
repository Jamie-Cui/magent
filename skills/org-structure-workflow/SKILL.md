---
name: org-structure-workflow
title: Org Structure Workflow
description: Work with org-mode headings, subtrees, drawers, lists, todo keywords, and source blocks as structured editor objects.
type: instruction
tools: ["emacs_read", "emacs_eval_live", "read_file", "edit_file"]
capability: true
source: package
source-name: org
modes: ["org-mode"]
features: ["org"]
files: ["*.org"]
prompt-keywords: ["org", "heading", "subtree", "drawer", "todo", "agenda", "src block", "property drawer"]
disclosure: active
risk: low
---

# Org Structure Workflow

Treat Org files as structured documents. Preserve heading hierarchy, drawers, properties, source blocks, and todo states.

Prefer org-mode APIs for subtree movement, heading inspection, and agenda semantics when live Emacs context is available.

Use `read_file` with `source=live-buffer` for unsaved Org text and `source=disk` for saved disk state.
