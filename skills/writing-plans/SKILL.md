---
name: writing-plans
description: Use when you have a spec or requirements for a multi-step task, before touching code
type: instruction
---

# Writing Implementation Plans

Write a comprehensive plan before touching code. The plan should give enough detail that a developer with no codebase context can execute it.

## Plan Structure

Save plans to `docs/plans/YYYY-MM-DD-<feature>.md`.

### Header (required)

```
# [Feature] Implementation Plan

**Goal:** One sentence.
**Architecture:** 2-3 sentences on approach.
**Tech Stack:** Key libraries/tools.
```

### File Map

List every file to create or modify before writing tasks. Each file has one clear responsibility.

### Tasks

Each task = one logical unit. Each step = one action (2-5 minutes).

```
### Task N: Name

**Files:**
- Create: `path/to/new-file.el`
- Modify: `path/to/existing.el`

- [ ] Step 1: Write failing test
- [ ] Step 2: Run test (confirm FAIL)
- [ ] Step 3: Write minimal implementation
- [ ] Step 4: Run test (confirm PASS)
- [ ] Step 5: Commit
```

## Rules

- Exact file paths always
- Include actual code in the plan, not "add validation"
- Include exact commands and expected output
- DRY, YAGNI, TDD, frequent commits
- Each task produces working, independently-testable software
