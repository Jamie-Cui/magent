---
name: lsp-workspace-workflow
title: LSP Workspace Workflow
description: Use editor-backed language server workflows such as diagnostics, definitions, references, rename, and code actions.
type: instruction
tools: emacs_eval, read_file, read_buffer, grep
capability: true
family: lsp
source: package
package: lsp-mode
features: lsp-mode, eglot
modes: prog-mode
keywords: lsp, eglot, diagnostics, definition, references, rename symbol, code action, workspace symbol
disclosure: active
risk: low
---

# LSP Workspace Workflow

When code intelligence matters, prefer the active LSP or Eglot workspace over plain text search. Inspect diagnostics, definitions, references, workspace symbols, and code actions through Emacs APIs when available.

Use `read_buffer` instead of `read_file` when unsaved source text matters.

Fall back to grep when the language server is absent or the answer is purely textual.
