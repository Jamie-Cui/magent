---
name: magent-debug
description: Debug, reproduce, fix, and verify issues in the Magent Emacs package. Use when asked to debug magent, 排查或定位 Magent 问题, 复现或修复 *magent* / *magent-log* / *Messages* 报错, handle vague requests like “debug magent” with no prompt, investigate tool call hangs, diagnose task drift, UI rendering mistakes, performance regressions, runtime/live-Emacs bugs, compile or test failures, or carry a Magent bug through root-cause analysis, code changes, regression tests, live Emacs verification, and a concise Chinese summary.
---

# Magent Debug

Use live evidence to drive Magent debugging. Treat live Emacs reproduction as the main truth source, and use batch compile or test runs as supporting evidence.

## Operating Stance

- Report progress, questions, and conclusions in Chinese by default. Keep code, commands, file paths, buffer names, variables, and functions in their original spelling.
- Re-state the current problem and success criterion every time before editing, especially for vague requests or likely task drift.
- Prefer the smallest fix that addresses the observed root cause. Avoid opportunistic refactors unless the design problem is the bug.
- Add or update a focused regression test whenever practical. If a useful test is not practical, say why.
- Resolve repository paths dynamically. Do not hard-code one checkout path.

## Default Workflow

1. Reconstruct the problem first.
   If the user gives no clear symptom, inspect `*magent*`, `*magent-log*`, `*Messages*`, the current diff, and recent compile or test failures first. Then state the current understanding of the problem and the success criterion, and ask only the smallest clarifying question still needed.

2. Eliminate stale runtime state early.
   Check whether the current behavior could be caused by unloaded edits, stale `.elc`, old session state, old overlays, old request generation, or a dirty `*magent*` buffer. Prefer proving the runtime state before guessing about logic.

3. Build local context before searching wider.
   Read the repo-local `AGENTS.md` or `CLAUDE.md`, `Makefile`, and only the modules implied by the symptom. If the issue is still broad, start with `magent.el`, `magent-ui.el`, `magent-agent.el`, `magent-tools.el`, `magent-fsm*.el`, `magent-session.el`, and `test/magent-test.el`.

4. Reproduce in live Emacs first.
   Prefer live inspection and live reproduction when an Emacs server is available. Reload the smallest changed files, clear Magent session state when needed, reproduce with the smallest prompt that still shows the bug, and inspect `*magent*`, `*magent-log*`, and `*Messages*` after each attempt.

5. Fall back to batch only when needed.
   If no live Emacs is available, or if a batch-only failure is already evident, run `make compile`, `make test`, or a narrower batch reproduction. If live Emacs and batch disagree, trust the live Emacs reproduction and explain the mismatch.

6. Fix deliberately.
   Implement the narrowest code change that matches the observed root cause. Keep the write scope tight and separate root-cause fixes from cleanup.

7. Verify in layers.
   First verify that the live Emacs reproduction no longer fails. Then run targeted batch validation. Expand to broader compile or test coverage only when it adds confidence for the touched behavior.

8. Summarize in four parts.
   End with `症状与复现`, `根因`, `改动与验证`, and `剩余风险/下一步`.

## Debugging Priorities

- Prefer logs and runtime facts over speculation.
- Prefer minimal repros over long conversational scenarios.
- Prefer repo-local evidence over outside documentation.
- Prefer targeted tests over broad suites unless the touched area is large.
- Prefer continuing autonomously once the symptom is clear; only stop when the problem statement is still ambiguous or the next action is unusually risky.

## Reference Files

- Read [references/live-emacs-debug.md](references/live-emacs-debug.md) when you need the live-debug checklist, the standard repro prompts, or the stale-state checklist.
- Read [references/symptom-map.md](references/symptom-map.md) when you need a quick mapping from symptom to likely files, buffers, and subsystems.
