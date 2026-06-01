---
name: dispatching-parallel-agents
description: Dispatch independent child agents for parallel analysis and merge their findings.
type: instruction
tools: spawn_agent, wait_agent, list_agents
---

# Dispatching Parallel Agents

Use parallel child agents only for independent work. Give each child a non-overlapping task and clear output contract.

After waiting, compare findings, resolve conflicts, and cite which result informed the final decision.
