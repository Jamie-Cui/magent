---
name: emacs-config-reload
description: Reload Emacs configuration safely and diagnose stale package or variable state after configuration changes.
type: instruction
tools: emacs_eval
---

# Emacs Config Reload Workflow

When reloading configuration, identify the affected file or feature first. Prefer loading the changed file, checking feature state, and inspecting messages or warnings afterward.

Avoid assuming a full Emacs restart unless the stale state cannot be safely refreshed in the running session.
