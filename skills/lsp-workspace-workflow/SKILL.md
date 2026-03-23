---
name: lsp-workspace-workflow
description: Use when the task depends on diagnostics, definitions, references, rename, or code actions from an LSP-backed editor workflow.
type: instruction
---

# LSP Workspace Workflow

Prefer editor-backed code intelligence when the user is asking about language
server operations rather than plain text search.

## When To Prefer This

- the user mentions diagnostics, definition lookup, references, rename, or code actions
- the active programming buffer is backed by `lsp-mode` or `eglot`
- the task depends on workspace-level symbol understanding

## Working Rules

1. Distinguish LSP workspace state from static repository state.
2. Prefer runtime/editor inspection when the answer depends on language server features.
3. Fall back to grep or file reading only when semantic navigation is unavailable.
