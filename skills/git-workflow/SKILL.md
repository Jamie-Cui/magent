---
name: git-workflow
description: Assist with Git workflow operations like status, diff, branch management, and commits.
tools: bash, read
type: instruction
---

# Git Workflow Skill

When the user asks about Git operations, follow these guidelines:

## Basic Operations

1. **Status Check**: Run `git status` to understand the current state
2. **Branch Info**: Use `git branch -a` to list all branches
3. **Commit History**: Run `git log --oneline -10` for recent commits
4. **Diff Review**: Use `git diff` to show uncommitted changes

## Safety Rules

- Always explain what you're doing before running Git commands
- Warn the user about destructive operations:
  - `git push --force`
  - `git reset --hard`
  - `git clean -fd`
  - Deleting branches
- Ask for confirmation before running destructive commands

## Commit Messages

Follow conventional commits format:
- `feat:` for new features
- `fix:` for bug fixes
- `docs:` for documentation
- `chore:` for maintenance tasks
- `refactor:` for code refactoring

## Example Workflow

```bash
# Check status
git status

# Stage specific files
git add file1.el file2.el

# Commit with message
git commit -m "feat: add new feature"

# Push to remote
git push origin branch-name
```