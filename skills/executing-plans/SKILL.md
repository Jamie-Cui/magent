---
name: executing-plans
description: Execute an existing plan while keeping status and recovery notes current.
type: instruction
tools: read_file, edit_file, bash, emacs_eval
---

# Executing Plans

Read the plan before editing. Keep progress status current, update recovery notes after meaningful checkpoints, and verify each completed slice before moving on.

If the plan becomes stale, update it instead of silently diverging.
