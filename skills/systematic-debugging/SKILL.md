---
name: systematic-debugging
description: Use when encountering any bug, test failure, or unexpected behavior, before proposing fixes
type: instruction
---

# Systematic Debugging

When you hit a bug, test failure, or unexpected behavior, follow this process instead of guessing.

## Process

1. **Reproduce** — confirm the bug is real and consistent. What exact input triggers it?
2. **Isolate** — narrow down where it happens. Binary search through the call stack.
3. **Hypothesize** — form one specific hypothesis about the root cause.
4. **Test hypothesis** — run a minimal experiment to confirm or refute it.
5. **Fix** — implement the minimal change that addresses the root cause.
6. **Verify** — confirm the fix works and didn't break anything else.

## Rules

- Never propose a fix before completing steps 1-4
- One hypothesis at a time — don't fix multiple things simultaneously
- Read the actual error message and stack trace before doing anything else
- Check the most recent changes first (git log, git diff)
- If stuck after 3 hypotheses, step back and re-read the error from scratch
- Distinguish symptoms from root causes — fix the root cause
