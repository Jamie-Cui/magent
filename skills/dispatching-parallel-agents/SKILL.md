---
name: dispatching-parallel-agents
description: Use when facing 2+ independent tasks that can be worked on without shared state or sequential dependencies
type: instruction
---

# Dispatching Parallel Agents

When you have multiple independent tasks, use the `delegate` tool to run them in parallel.

## When to Use

- 2+ tasks with no shared mutable state
- Tasks that don't depend on each other's output
- Research/exploration tasks (safe to parallelize always)

## When NOT to Use

- Tasks that write to the same files
- Tasks where task B needs task A's output
- Tasks that modify shared state (session, registry, etc.)

## Using delegate

```
delegate(
  agent="explore",
  prompt="Investigate X and return findings",
  context="relevant context here"
)
```

Available agents: `explore` (read-only research), `general` (full tool access).

## Pattern

1. Identify independent sub-tasks
2. Dispatch all in parallel via multiple `delegate` calls
3. Collect results
4. Synthesize and proceed
