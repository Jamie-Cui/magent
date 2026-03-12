# Superpowers Migration Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Migrate the superpowers workflow skill system into magent by placing skill files under `~/.emacs.d/magent-skills/` and adding an on-demand skill index to `prompt.org` that instructs magent to load skills via `read_file` when applicable.

**Architecture:** Skills are stored as `~/.emacs.d/magent-skills/<name>/SKILL.md` files, each adapted from the original superpowers SKILL.md. The `prompt.org` system prompt gains a `* Skills` section that lists all skill names with one-line descriptions and their file paths, plus an instruction to `read_file` the relevant skill before acting. This mirrors the original superpowers mechanism (skill index → load on demand) without requiring always-on injection into every request.

**Tech Stack:** Elisp (magent SKILL.md format), org-mode (prompt.org), bash for file creation.

---

## Chunk 1: Skill Files

Create the 11 SKILL.md files adapted for magent's context (no Claude Code tool references, uses magent tool names: `read_file`, `write_file`, `edit_file`, `grep`, `glob`, `bash`, `emacs_eval`, `delegate`, `skill_invoke`).

### Task 1: brainstorming

**Files:**
- Create: `~/.emacs.d/magent-skills/brainstorming/SKILL.md`

- [ ] **Step 1: Create skill file**

```markdown
---
name: brainstorming
description: You MUST use this before any creative work - creating features, building components, adding functionality, or modifying behavior. Explores user intent, requirements, and design before implementation.
type: instruction
---

# Brainstorming Ideas Into Designs

Help turn ideas into fully formed designs through collaborative dialogue. Understand the project context first, then ask questions one at a time.

<HARD-GATE>
Do NOT write any code or take any implementation action until you have presented a design and the user has approved it.
</HARD-GATE>

## Process

1. **Explore context** — read relevant files, check recent git log
2. **Ask clarifying questions** — one at a time, understand purpose/constraints/success criteria
3. **Propose 2-3 approaches** — with trade-offs and your recommendation
4. **Present design** — in sections, get user approval after each section
5. **Transition** — use writing-plans skill (read_file ~/.emacs.d/magent-skills/writing-plans/SKILL.md)

## Rules

- One question per message, multiple-choice preferred
- Do not write code until design is approved
- Follow existing codebase patterns
- YAGNI: remove unnecessary features from all designs
- Design units with clear boundaries and single responsibilities
```

Run: `mkdir -p ~/.emacs.d/magent-skills/brainstorming && write_file the above content`

- [ ] **Step 2: Verify file exists and is readable**

Run: `glob ~/.emacs.d/magent-skills/brainstorming/SKILL.md`

---

### Task 2: systematic-debugging

**Files:**
- Create: `~/.emacs.d/magent-skills/systematic-debugging/SKILL.md`

- [ ] **Step 1: Create skill file**

```markdown
---
name: systematic-debugging
description: Use when encountering any bug, test failure, or unexpected behavior, before proposing fixes
type: instruction
---

# Systematic Debugging

When you hit a bug, test failure, or unexpected behavior, follow this process instead of guessing.

## Process

1. **Reproduce** — confirm the bug is real and consistent. What exact input triggers it?
2. **Isolate** — narrow down where it happens. Binary search through the call stack.
3. **Hypothesize** — form one specific hypothesis about the root cause.
4. **Test hypothesis** — run a minimal experiment to confirm or refute it.
5. **Fix** — implement the minimal change that addresses the root cause.
6. **Verify** — confirm the fix works and didn't break anything else.

## Rules

- Never propose a fix before completing steps 1-4
- One hypothesis at a time — don't fix multiple things simultaneously
- Read the actual error message and stack trace before doing anything else
- Check the most recent changes first (git log, git diff)
- If stuck after 3 hypotheses, step back and re-read the error from scratch
- Distinguish symptoms from root causes — fix the root cause
```

- [ ] **Step 2: Verify**

Run: `glob ~/.emacs.d/magent-skills/systematic-debugging/SKILL.md`

---

### Task 3: test-driven-development

**Files:**
- Create: `~/.emacs.d/magent-skills/test-driven-development/SKILL.md`

- [ ] **Step 1: Create skill file**

```markdown
---
name: test-driven-development
description: Use when implementing any feature or bugfix, before writing implementation code
type: instruction
---

# Test-Driven Development

Write tests before implementation. Red → Green → Refactor.

## Cycle

1. **Write a failing test** that describes the behavior you want
2. **Run it** — confirm it fails for the right reason
3. **Write minimal code** to make it pass (no more)
4. **Run tests** — confirm green
5. **Refactor** — clean up while keeping tests green
6. **Commit** — small, frequent commits at green

## Rules

- Never write implementation before a failing test exists
- Tests must fail before you write the implementation
- Write the simplest code that makes the test pass (YAGNI)
- One failing test at a time
- If the test is hard to write, the design needs work
- Test behavior, not implementation details

## For Elisp (magent context)

Use ERT for unit tests. Mock external dependencies with `cl-letf`.

```elisp
(ert-deftest test-specific-behavior ()
  "Test that X does Y given Z."
  (should (equal (function-under-test input) expected-output)))
```

Run tests:
```bash
emacs -Q --batch -L . -l ert -l test/magent-test.el \
  --eval '(ert-run-tests-batch "test-name-regexp")'
```
```

- [ ] **Step 2: Verify**

Run: `glob ~/.emacs.d/magent-skills/test-driven-development/SKILL.md`

---

### Task 4: writing-plans

**Files:**
- Create: `~/.emacs.d/magent-skills/writing-plans/SKILL.md`

- [ ] **Step 1: Create skill file**

```markdown
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
```

- [ ] **Step 2: Verify**

Run: `glob ~/.emacs.d/magent-skills/writing-plans/SKILL.md`

---

### Task 5: executing-plans

**Files:**
- Create: `~/.emacs.d/magent-skills/executing-plans/SKILL.md`

- [ ] **Step 1: Create skill file**

```markdown
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
```

- [ ] **Step 2: Verify**

Run: `glob ~/.emacs.d/magent-skills/executing-plans/SKILL.md`

---

### Task 6: verification-before-completion

**Files:**
- Create: `~/.emacs.d/magent-skills/verification-before-completion/SKILL.md`

- [ ] **Step 1: Create skill file**

```markdown
---
name: verification-before-completion
description: Use when about to claim work is complete, fixed, or passing — before committing or reporting done. Evidence before assertions.
type: instruction
---

# Verification Before Completion

Never claim work is done without running verification commands and showing the output.

## Required Before Claiming "Done"

1. **Run tests** — show the actual output, not "tests should pass"
2. **Run build/compile** — show success output
3. **Run linter** if applicable
4. **Show evidence** — paste the actual terminal output

## Forbidden Phrases Without Evidence

- "The tests should pass now"
- "This should fix the issue"
- "The build should succeed"
- "I believe this works"

## Required Phrases

- "I ran `make test` and got: [actual output]"
- "Tests pass: [paste output]"
- "Compilation succeeded: [paste output]"

## For Magent/Elisp

```bash
# Run ERT tests
make test

# Byte-compile to check for errors
make compile
```

Show the output before marking anything complete.
```

- [ ] **Step 2: Verify**

Run: `glob ~/.emacs.d/magent-skills/verification-before-completion/SKILL.md`

---

### Task 7: requesting-code-review

**Files:**
- Create: `~/.emacs.d/magent-skills/requesting-code-review/SKILL.md`

- [ ] **Step 1: Create skill file**

```markdown
---
name: requesting-code-review
description: Use when completing tasks or implementing major features to verify work meets requirements before declaring done
type: instruction
---

# Requesting Code Review

Before declaring a task complete, review your own work systematically.

## Self-Review Checklist

1. **Correctness** — does the implementation match the requirements exactly?
2. **Tests** — are there tests? Do they actually test the right behavior?
3. **Edge cases** — what inputs could break this? Are they handled?
4. **Security** — any injection, XSS, or credential exposure risks?
5. **Conventions** — does it follow the existing code style and patterns?
6. **Scope** — did you add anything not asked for? Remove it.
7. **Dead code** — any unused functions, variables, or imports?

## Output Format

Report findings as:
- ✅ Correctness: [brief assessment]
- ✅/❌ Tests: [what's covered, what's missing]
- ✅/❌ Edge cases: [findings]
- ✅/❌ Security: [findings]
- ✅/❌ Conventions: [findings]

Fix any ❌ items before declaring complete.
```

- [ ] **Step 2: Verify**

Run: `glob ~/.emacs.d/magent-skills/requesting-code-review/SKILL.md`

---

### Task 8: receiving-code-review

**Files:**
- Create: `~/.emacs.d/magent-skills/receiving-code-review/SKILL.md`

- [ ] **Step 1: Create skill file**

```markdown
---
name: receiving-code-review
description: Use when receiving code review feedback, before implementing suggestions — requires technical rigor, not blind agreement
type: instruction
---

# Receiving Code Review

When you receive code review feedback, apply technical rigor before implementing it.

## Process

1. **Read feedback carefully** — understand what is being asked and why
2. **Verify the claim** — is the feedback technically correct? Read the code yourself.
3. **Check for misunderstanding** — does the reviewer have full context?
4. **Assess impact** — what does this change affect?
5. **Implement or explain** — either implement the fix, or explain clearly why the current approach is correct

## Rules

- Do not implement feedback blindly — verify it first
- If feedback is wrong or based on misunderstanding, explain politely with evidence
- If feedback is ambiguous, ask a clarifying question before implementing
- Never implement a suggestion that introduces a bug or regression just to be agreeable
- Run tests after implementing any feedback change
```

- [ ] **Step 2: Verify**

Run: `glob ~/.emacs.d/magent-skills/receiving-code-review/SKILL.md`

---

### Task 9: finishing-a-development-branch

**Files:**
- Create: `~/.emacs.d/magent-skills/finishing-a-development-branch/SKILL.md`

- [ ] **Step 1: Create skill file**

```markdown
---
name: finishing-a-development-branch
description: Use when implementation is complete and all tests pass — guides integration decision (merge, PR, or cleanup)
type: instruction
---

# Finishing a Development Branch

When implementation is done and tests pass, follow this checklist before integrating.

## Pre-Integration Checklist

- [ ] All tests pass (`make test`)
- [ ] No byte-compile warnings (`make compile`)
- [ ] No unintended files changed (`git status`)
- [ ] Commit messages follow conventions (conventional commits)
- [ ] No debug code or temporary changes left in

## Integration Options

**Option A: Direct merge to main**
- Appropriate for small, self-contained changes
- `git checkout main && git merge --no-ff feature-branch`

**Option B: Squash merge**
- Appropriate when branch has many WIP commits
- `git checkout main && git merge --squash feature-branch && git commit`

**Option C: Leave for PR review**
- Appropriate when changes need human review
- Push branch, open PR, request review

## After Integration

- Delete the feature branch: `git branch -d feature-branch`
- Verify main is in good state: run tests on main
```

- [ ] **Step 2: Verify**

Run: `glob ~/.emacs.d/magent-skills/finishing-a-development-branch/SKILL.md`

---

### Task 10: dispatching-parallel-agents

**Files:**
- Create: `~/.emacs.d/magent-skills/dispatching-parallel-agents/SKILL.md`

- [ ] **Step 1: Create skill file**

```markdown
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
```

- [ ] **Step 2: Verify**

Run: `glob ~/.emacs.d/magent-skills/dispatching-parallel-agents/SKILL.md`

---

### Task 11: subagent-driven-development

**Files:**
- Create: `~/.emacs.d/magent-skills/subagent-driven-development/SKILL.md`

- [ ] **Step 1: Create skill file**

```markdown
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
```

- [ ] **Step 2: Verify**

Run: `glob ~/.emacs.d/magent-skills/subagent-driven-development/SKILL.md`

---

## Chunk 2: prompt.org Skills Index

Add a `* Skills` section to `prompt.org` that instructs magent to check and load skills on demand.

### Task 12: Add skills index to prompt.org

**Files:**
- Modify: `/home/jamie/opt/emacs.d/site-lisp/magent/prompt.org`

- [ ] **Step 1: Read current prompt.org**

Read the file to find the right insertion point (end of file).

- [ ] **Step 2: Append Skills section**

Append to `prompt.org`:

```org
* Skills

You have workflow skills available as files. Before responding to any non-trivial request, check if a skill applies. If it does, read the skill file and follow it.

** When to check for skills

Check for an applicable skill when the user asks you to:
- Build, create, or add a feature → ~brainstorming~
- Fix a bug or unexpected behavior → ~systematic-debugging~
- Implement a feature or bugfix → ~test-driven-development~
- Plan a multi-step task → ~writing-plans~
- Execute a written plan → ~executing-plans~
- Verify or claim work is done → ~verification-before-completion~
- Review completed work → ~requesting-code-review~
- Act on review feedback → ~receiving-code-review~
- Finish and integrate a branch → ~finishing-a-development-branch~
- Run multiple independent tasks → ~dispatching-parallel-agents~
- Execute a plan with independent tasks → ~subagent-driven-development~

** Skill files

| Skill | File |
|---|---|
| brainstorming | ~/.emacs.d/magent-skills/brainstorming/SKILL.md |
| systematic-debugging | ~/.emacs.d/magent-skills/systematic-debugging/SKILL.md |
| test-driven-development | ~/.emacs.d/magent-skills/test-driven-development/SKILL.md |
| writing-plans | ~/.emacs.d/magent-skills/writing-plans/SKILL.md |
| executing-plans | ~/.emacs.d/magent-skills/executing-plans/SKILL.md |
| verification-before-completion | ~/.emacs.d/magent-skills/verification-before-completion/SKILL.md |
| requesting-code-review | ~/.emacs.d/magent-skills/requesting-code-review/SKILL.md |
| receiving-code-review | ~/.emacs.d/magent-skills/receiving-code-review/SKILL.md |
| finishing-a-development-branch | ~/.emacs.d/magent-skills/finishing-a-development-branch/SKILL.md |
| dispatching-parallel-agents | ~/.emacs.d/magent-skills/dispatching-parallel-agents/SKILL.md |
| subagent-driven-development | ~/.emacs.d/magent-skills/subagent-driven-development/SKILL.md |

To load a skill, use =read_file= on its path, then follow the instructions in it.
```

- [ ] **Step 3: Verify the section was added correctly**

Run: `grep -n "Skills" ~/.../prompt.org` — confirm the heading appears.

- [ ] **Step 4: Commit**

```bash
cd /home/jamie/opt/emacs.d/site-lisp/magent
git add prompt.org
git add -A  # to catch any new skill files if stored in repo
git commit -m "feat(skills): migrate superpowers workflow skills to magent"
```

---

## Chunk 3: Verification

### Task 13: End-to-end smoke test

- [ ] **Step 1: Reload magent in running Emacs**

```elisp
(load "/home/jamie/opt/emacs.d/site-lisp/magent/magent-config.el" nil t)
```

Check that `prompt.org` is picked up correctly (the system prompt should include the Skills section).

- [ ] **Step 2: Verify skill files exist**

```bash
ls ~/.emacs.d/magent-skills/
# Expected: brainstorming  dispatching-parallel-agents  executing-plans
#           finishing-a-development-branch  receiving-code-review
#           requesting-code-review  subagent-driven-development
#           systematic-debugging  test-driven-development
#           verification-before-completion  writing-plans
```

- [ ] **Step 3: Test trigger in magent**

In the magent buffer, send: `"帮我给 magent 加一个新功能"` (should trigger brainstorming)

Observe: magent should recognize it needs to load the brainstorming skill and use `read_file` on the skill path before responding.

- [ ] **Step 4: Final commit if needed**

Confirm all files are committed.
