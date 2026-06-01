---
name: subagent-driven-development
description: Coordinate work with child agents for exploration, parallel analysis, or independent review.
type: instruction
tools: spawn_agent, wait_agent, list_agents, close_agent
---

# Subagent Driven Development

Use child agents for bounded, independently checkable subtasks such as exploration, comparison, or review. Give each child a clear task, expected output, and context.

Wait for results, reconcile conflicts, and keep the root agent responsible for final decisions.
