---
name: systematic-debugging
description: Use for structured diagnosis, reproducing failures, isolating causes, checking logs, and verifying fixes before declaring a problem solved.
type: instruction
tools: read_file, grep, emacs_eval, bash
---

# Systematic Debugging

Use a disciplined debugging loop:

1. Reproduce or inspect the failure before changing behavior.
2. Gather the smallest useful evidence from logs, buffers, tests, or traces.
3. Form one concrete hypothesis at a time.
4. Make the smallest fix that addresses the observed cause.
5. Re-run the focused check that would have failed before the fix.

Prefer targeted instrumentation and focused tests over broad rewrites.
