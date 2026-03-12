---
name: executing-plans
description: Use when you have a written implementation plan to execute with review checkpoints
type: instruction
---

# Executing Plans

When executing a written implementation plan, follow this discipline.

## Process

1. **Read the full plan** before starting any task
2. **Work task by task** — complete one fully before starting the next
3. **Mark steps** — check off `- [ ]` items as you complete them
4. **Checkpoint after each task** — run tests, confirm green, then commit
5. **Surface blockers** — if a step cannot be completed as written, stop and report; do not improvise around it

## Rules

- Follow the plan as written; do not skip steps
- If a step says "run tests", run them and show the output before proceeding
- Commit at the end of each task (or as specified in the plan)
- If the plan is wrong or incomplete, surface it rather than guessing
- Do not refactor or improve things not mentioned in the plan
