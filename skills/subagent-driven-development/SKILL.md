---
name: subagent-driven-development
description: Use when executing implementation plans with independent tasks — delegates each task to a fresh subagent via delegate tool
type: instruction
---

# Subagent-Driven Development

When executing a plan with independent tasks, delegate each task to a fresh subagent.

## Process

1. **Read the full plan** first
2. **Identify independent tasks** — tasks with no shared write-state dependencies
3. **Dispatch each task** via `delegate` with:
   - Full task description and steps
   - Relevant context (file paths, conventions, test commands)
   - Clear success criteria
4. **Review results** — verify each subagent's output before proceeding
5. **Handle failures** — if a subagent fails, debug the specific task

## Delegation Template

```
delegate(
  agent="general",
  prompt="""
  Task: [task name from plan]

  Steps:
  [paste exact steps from plan]

  Context:
  - Project root: [path]
  - Test command: [command]
  - Conventions: [key conventions]

  Success: [what done looks like]
  """
)
```

## Rules

- One subagent per independent task
- Provide complete context in each delegation (subagents have no prior context)
- Always verify subagent output before marking task complete
- Sequential tasks must run sequentially, even with subagents
