---
name: receiving-code-review
description: Use when receiving code review feedback, before implementing suggestions — requires technical rigor, not blind agreement
type: instruction
---

# Receiving Code Review

When you receive code review feedback, apply technical rigor before implementing it.

## Process

1. **Read feedback carefully** — understand what is being asked and why
2. **Verify the claim** — is the feedback technically correct? Read the code yourself.
3. **Check for misunderstanding** — does the reviewer have full context?
4. **Assess impact** — what does this change affect?
5. **Implement or explain** — either implement the fix, or explain clearly why the current approach is correct

## Rules

- Do not implement feedback blindly — verify it first
- If feedback is wrong or based on misunderstanding, explain politely with evidence
- If feedback is ambiguous, ask a clarifying question before implementing
- Never implement a suggestion that introduces a bug or regression just to be agreeable
- Run tests after implementing any feedback change
