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
