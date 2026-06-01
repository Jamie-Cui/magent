---
name: test-driven-development
description: Use when adding or changing behavior that benefits from a failing test before implementation.
type: instruction
tools: read_file, edit_file, bash, emacs_eval
---

# Test Driven Development

Start with the narrowest test that captures the behavior change. Confirm it fails for the expected reason, implement the change, then rerun the focused test.

Keep tests close to the behavior boundary and avoid broad fixture churn.
