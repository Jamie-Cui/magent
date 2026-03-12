---
name: verification-before-completion
description: Use when about to claim work is complete, fixed, or passing — before committing or reporting done. Evidence before assertions.
type: instruction
---

# Verification Before Completion

Never claim work is done without running verification commands and showing the output.

## Required Before Claiming "Done"

1. **Run tests** — show the actual output, not "tests should pass"
2. **Run build/compile** — show success output
3. **Run linter** if applicable
4. **Show evidence** — paste the actual terminal output

## Forbidden Phrases Without Evidence

- "The tests should pass now"
- "This should fix the issue"
- "The build should succeed"
- "I believe this works"

## Required Phrases

- "I ran `make test` and got: [actual output]"
- "Tests pass: [paste output]"
- "Compilation succeeded: [paste output]"

## For Magent/Elisp

```bash
# Run ERT tests
make test

# Byte-compile to check for errors
make compile
```

Show the output before marking anything complete.
