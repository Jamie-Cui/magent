---
name: review
description: Review the current code changes like a senior code reviewer. Use when the user types /review or asks for bugs, regressions, risks, or missing tests.
type: instruction
tools: read_file, grep, bash
default-prompt: Review the current changes as a code reviewer. Inspect git status and the relevant diff, prioritize correctness bugs, regressions, missing tests, and operational risks. Lead with findings ordered by severity and include file/line references when possible. Then list open questions or assumptions, followed by a brief change summary. Do not edit files unless explicitly asked.
---

# Review Command

Take a code-review stance:

1. Inspect the current repository state before judging the change.
2. Focus on bugs, behavioral regressions, unsafe edge cases, missing tests, and mismatches with local conventions.
3. Lead with findings, ordered by severity. Include precise file and line references when possible.
4. Keep summaries secondary and concise.
5. If no issues are found, say that clearly and mention remaining test gaps or residual risk.

Do not make code changes during `/review` unless the user explicitly asks you to fix the findings.
