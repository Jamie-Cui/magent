---
name: test-driven-development
description: Use when implementing any feature or bugfix, before writing implementation code
type: instruction
---

# Test-Driven Development

Write tests before implementation. Red → Green → Refactor.

## Cycle

1. **Write a failing test** that describes the behavior you want
2. **Run it** — confirm it fails for the right reason
3. **Write minimal code** to make it pass (no more)
4. **Run tests** — confirm green
5. **Refactor** — clean up while keeping tests green
6. **Commit** — small, frequent commits at green

## Rules

- Never write implementation before a failing test exists
- Tests must fail before you write the implementation
- Write the simplest code that makes the test pass (YAGNI)
- One failing test at a time
- If the test is hard to write, the design needs work
- Test behavior, not implementation details

## For Elisp (magent context)

Use ERT for unit tests. Mock external dependencies with `cl-letf`.

```elisp
(ert-deftest test-specific-behavior ()
  "Test that X does Y given Z."
  (should (equal (function-under-test input) expected-output)))
```

Run tests:
```bash
emacs -Q --batch -L . -l ert -l test/magent-test.el \
  --eval '(ert-run-tests-batch "test-name-regexp")'
```
