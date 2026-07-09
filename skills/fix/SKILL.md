---
name: fix
description: Diagnose and fix the current bug, failure, or regression. Use when the user types /fix or asks Magent to repair a failing behavior, test, command, or workflow.
type: instruction
tools: read_file, write_file, edit_file, grep, bash, emacs_eval
default-prompt: Diagnose and fix the current problem. Reconstruct the symptom from the user's context, logs, tests, buffers, or git diff; identify the smallest plausible root cause; make a focused change; add or update a focused test when practical; run the relevant verification; and report the root cause, fix, and validation.
---

# Fix Command

Use a disciplined repair loop:

1. Reconstruct the symptom and success criterion from the current context.
2. Inspect relevant code, logs, tests, buffers, and recent diffs before editing.
3. Form a concrete hypothesis and make the smallest change that addresses it.
4. Add or update a focused regression test when practical.
5. Run the verification that would have caught the problem.
6. Report the root cause, changed files, verification, and any remaining risk.

Avoid opportunistic refactors. Keep the fix scoped to the observed problem.
