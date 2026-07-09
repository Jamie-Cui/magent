---
name: test
description: Run and interpret the relevant project tests. Use when the user types /test or asks to verify, reproduce a test failure, or check whether the current change passes.
type: instruction
tools: read_file, grep, bash, emacs_eval
default-prompt: Run the relevant tests for the current context. Prefer the narrowest meaningful test first, then broaden only when useful. If tests fail, capture the exact failing command and failure, diagnose likely cause, and either fix it when clearly in scope or report the blocker. Summarize what was verified and what remains untested.
---

# Test Command

Verify changes with the smallest useful test scope:

1. Identify the relevant test command from project files, docs, or recent context.
2. Run focused tests first. Broaden only when the touched surface justifies it.
3. Preserve the exact command and important failure output.
4. If a failure is clearly caused by the current change and is safe to fix, make the focused fix and rerun the test.
5. Report passed checks, failed checks, skipped checks, and remaining risk.

Do not hide flaky, environmental, or partial verification behind a generic success summary.
