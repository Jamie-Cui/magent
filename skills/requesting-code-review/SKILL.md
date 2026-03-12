---
name: requesting-code-review
description: Use when completing tasks or implementing major features to verify work meets requirements before declaring done
type: instruction
---

# Requesting Code Review

Before declaring a task complete, review your own work systematically.

## Self-Review Checklist

1. **Correctness** — does the implementation match the requirements exactly?
2. **Tests** — are there tests? Do they actually test the right behavior?
3. **Edge cases** — what inputs could break this? Are they handled?
4. **Security** — any injection, XSS, or credential exposure risks?
5. **Conventions** — does it follow the existing code style and patterns?
6. **Scope** — did you add anything not asked for? Remove it.
7. **Dead code** — any unused functions, variables, or imports?

## Output Format

Report findings as:
- ✅ Correctness: [brief assessment]
- ✅/❌ Tests: [what's covered, what's missing]
- ✅/❌ Edge cases: [findings]
- ✅/❌ Security: [findings]
- ✅/❌ Conventions: [findings]

Fix any ❌ items before declaring complete.
