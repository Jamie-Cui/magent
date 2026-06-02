---
name: init
description: Initialize or refresh project instructions for Magent, similar to Codex /init. Use when the user types @init, asks to create AGENTS.md, or asks Magent to learn project build, test, architecture, and workflow conventions.
type: instruction
tools: read_file, write_file, edit_file, grep, glob, bash
default-prompt: Initialize this project for Magent. Inspect the repository and create or update AGENTS.md at the project root with concise, durable instructions for future agentic coding sessions. Include build/test commands, architecture overview, important workflows, conventions, and gotchas that are recoverable from the repo. Preserve useful existing AGENTS.md content and avoid unrelated churn.
---

# Project Initialization

Initialize or refresh the repository's `AGENTS.md` for future Magent sessions.

Follow this workflow:

1. Identify the project root and inspect the existing repository structure.
2. Read existing onboarding or contributor docs first, especially `README*`, `CONTRIBUTING*`, `docs/`, `Makefile`, package manifests, test configs, and any existing `AGENTS.md`.
3. Determine the commands a future agent should run for build, test, lint, format, and live verification. Prefer commands that are already documented or encoded in project files.
4. Write or update `AGENTS.md` at the project root with concise, durable guidance:
   - current architecture and module boundaries
   - build/test commands
   - workflow and verification notes
   - repository conventions
   - important gotchas
5. Preserve project-specific instructions already present unless they are clearly obsolete.
6. Avoid large copied excerpts from docs. Summarize and link paths instead.
7. Verify the edited file is readable and focused before reporting completion.

If the user provides extra text after `@init`, treat it as additional constraints
for the initialization pass.
