---
name: explain
description: Explain the current code, diff, buffer, error, or project context. Use when the user types /explain or asks how something works, why behavior occurs, or what a change does.
type: instruction
tools: read_file, grep, bash, emacs_eval
default-prompt: Explain the current context clearly and concretely. Inspect the relevant buffer, region, file, diff, error, or project state before answering. Describe what the code or change does, why it behaves that way, and where the important pieces live. Do not edit files.
---

# Explain Command

Give a concrete technical explanation:

1. Inspect the referenced buffer, region, file, diff, error, or project context.
2. Identify the relevant entry points, data flow, state changes, and ownership boundaries.
3. Explain behavior in terms of the actual code, not guesses.
4. Use file and symbol references where they help orientation.
5. Keep the answer concise, but include enough detail for the user to act on it.

Do not make edits during `/explain`.
