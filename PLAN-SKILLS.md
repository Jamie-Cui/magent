# Plan: Support Claude Code/OpenCode Style Skills

## Current State

### Claude Code Skills Format
- **File**: `~/.claude/skills/<name>/SKILL.md`
- **Structure**: YAML frontmatter + Markdown body
- **Frontmatter fields**:
  - `name`: Skill identifier
  - `description`: Brief description for skill selection
  - `tools`: Comma-separated list of tools the skill uses (e.g., "Bash")
  - `disable-model-invocation`: If true, skill provides instructions for LLM to execute
- **Companion files**: Optional `.el` or other implementation files alongside SKILL.md
- **Invocation**: LLM reads the skill markdown and follows instructions to use tools

### Magent Skills Format (Current)
- **Location**: Hardcoded in `magent-skills.el`
- **Structure**: `magent-skill` struct with `name`, `description`
- **Invocation**: Via `skill_invoke` tool with `skill_name`, `operation`, `args`
- **Operations**: Predefined functions (list-functions, describe-function, etc.)

## Key Differences

| Aspect | Claude Code | Magent (Current) |
|--------|-------------|------------------|
| Definition | Markdown files | Elisp code |
| Extensibility | Drop-in files | Code changes |
| Invocation | Instruction-based | Tool-call-based |
| Implementation | External scripts/tools | Internal Elisp |
| Tool access | Declared in frontmatter | Inherited from agent |

## Proposed Architecture

### 1. Skill File Format

```markdown
---
name: my-skill
description: Brief description of what this skill does
tools: read, write, bash  # Tools the skill needs access to
type: instruction | tool  # New field
---

# Skill Instructions

The markdown body becomes part of the system prompt when this skill is active.
It tells the LLM how to accomplish the skill's purpose.
```

### 2. Skill Types

#### Type: `instruction` (Claude Code style)
- Markdown body is injected into the system prompt
- LLM follows instructions and uses available tools
- No `skill_invoke` needed - LLM uses tools directly

#### Type: `tool` (Current Magent style)
- Skill is invoked via `skill_invoke` tool
- Has predefined operations
- Implementation in Elisp

### 3. Skill Directory Structure

```
~/.emacs.d/magent-skills/           # Global skills
  emacs/
    SKILL.md
    agent-skills-emacs.el           # Optional implementation
  git/
    SKILL.md

.magent/skills/                     # Project-local skills
  project-specific/
    SKILL.md
```

### 4. New Components

#### `magent-skill-file.el`
- Load skills from directories
- Parse YAML frontmatter + markdown body
- Register skills in the skill registry
- Support both skill types

#### Enhanced `magent-skill` struct
```elisp
(cl-defstruct magent-skill
  name                ; Skill name (string)
  description         ; Brief description
  type                ; 'instruction or 'tool
  tools               ; List of required tools
  prompt              ; Markdown body (for instruction type)
  invoke-function     ; Function for tool type
  file-path)          ; Source file path
```

### 5. Skill Invocation

#### For `instruction` type skills:
- When skill is active, inject the markdown body into system prompt
- LLM reads instructions and uses tools directly
- No explicit `skill_invoke` call needed

#### For `tool` type skills:
- Keep current `skill_invoke` mechanism
- LLM calls `skill_invoke` with skill_name, operation, args

### 6. Integration with Agents

- Agents can enable/disable skills via permission system
- Skills are listed in agent's available capabilities
- System prompt includes active skill instructions

## Implementation Plan

### Phase 1: Core Infrastructure
1. **Create `magent-skill-file.el`**
   - Directory scanning
   - YAML frontmatter parsing (reuse from `magent-agent-file.el`)
   - Skill registration

2. **Enhance `magent-skill` struct**
   - Add new fields: `type`, `tools`, `prompt`, `invoke-function`, `file-path`
   - Update `magent-skills.el` to use new struct

### Phase 2: Instruction-Type Skills
3. **Modify system prompt injection**
   - When instruction-type skills are active, append their prompts
   - Update `magent-agent.el` to include skill prompts

4. **Create sample instruction-type skills**
   - Convert existing emacs skill to instruction type
   - Create new example skills

### Phase 3: Tool-Type Skills Enhancement
5. **Support companion implementation files**
   - Auto-load `.el` files alongside SKILL.md
   - Register invoke-function from loaded file

6. **Update `skill_invoke` tool**
   - Better error messages
   - Support for skill-specific argument validation

### Phase 4: Configuration & UX
7. **Customization options**
   - `magent-skill-directories`: List of directories to scan
   - `magent-default-skills`: Skills enabled by default

8. **Interactive commands**
   - `magent-list-skills`: Show all available skills
   - `magent-enable-skill`: Enable a skill for current session
   - `magent-reload-skills`: Reload all skills from disk

## File Changes Summary

| File | Change |
|------|--------|
| `magent-skills.el` | Enhanced struct, registry, loader |
| `magent-skill-file.el` | **New** - File loading, parsing |
| `magent-tools.el` | Update `skill_invoke` tool |
| `magent-agent.el` | Include skill prompts in system message |
| `magent-config.el` | New customization variables |
| `magent.el` | Initialize skills on load |

## Backward Compatibility

- Existing `skill_invoke` tool continues to work
- Built-in `emacs` skill remains available
- No breaking changes to existing agent configurations

## Example: Instruction-Type Skill

```markdown
---
name: git-workflow
description: Assist with Git workflow operations
tools: bash, read
type: instruction
---

# Git Workflow Skill

When the user asks about Git operations:

1. **Status Check**: Run `git status` to understand the current state
2. **Branch Info**: Use `git branch -a` to list all branches
3. **Commit History**: Run `git log --oneline -10` for recent commits
4. **Diff Review**: Use `git diff` to show uncommitted changes

Always explain what you're doing before running Git commands.
Warn the user about destructive operations (force push, reset --hard, etc.).
```

## Example: Tool-Type Skill

```markdown
---
name: database
description: Query and manage databases
tools: bash
type: tool
---

# Database Skill

This skill provides database operations through the skill_invoke tool.

Available operations:
- `query`: Execute a SQL query
- `list-tables`: List all tables in the database
- `describe-table`: Show table schema
```

Implementation in `magent-skill-database.el`:
```elisp
(defun magent-skill-database-invoke (operation args)
  (pcase operation
    ("query" (magent-skill-database--query (car args)))
    ("list-tables" (magent-skill-database--list-tables))
    ...))
```

## Timeline

- **Phase 1**: Core infrastructure ✅ DONE
- **Phase 2**: Instruction-type skills ✅ DONE
- **Phase 3**: Tool-type enhancement ✅ DONE
- **Phase 4**: Configuration & UX ✅ DONE

Total: 1 session (completed ahead of schedule)

## Implementation Status

### Completed

1. **Enhanced `magent-skill` struct** (`magent-skills.el`)
   - New fields: `type`, `tools`, `prompt`, `invoke-function`, `file-path`
   - `magent-skill-create` constructor
   - Registry functions: `magent-skills-register`, `magent-skills-list-by-type`
   - `magent-skills-get-instruction-prompts` for system prompt injection
   - `magent-skills-invoke` for tool-type skill invocation

2. **Created `magent-skill-file.el`**
   - YAML frontmatter parsing (reused from `magent-agent-file.el`)
   - Directory scanning from `magent-skill-directories`
   - Support for both `instruction` and `tool` type skills
   - Companion `.el` file loading for tool-type skills
   - `magent-skill-file-load-all` to load from disk

3. **Updated `magent-agent.el`**
   - Instruction-type skill prompts injected into system message
   - Appears under "# Active Skills" section

4. **Updated `magent.el`**
   - Loads `magent-skill-file` module
   - Initializes skills on mode activation

5. **Created sample instruction-type skill**
   - `~/.emacs.d/magent-skills/git-workflow/SKILL.md`

6. **Interactive commands**
   - `magent-list-skills`: Display all registered skills
   - `magent-reload-skills`: Reload skills from disk
   - `magent-describe-skill`: Show detailed skill information

### Test Results

```
(:all-skills ("git-workflow" "emacs")
 :by-type (:instruction ("git-workflow") :tool ("emacs"))
 :emacs-info (:type tool :has-invoke-fn t)
 :git-info (:type instruction :prompt-length 1068)
 :invoke-test "magent-ask-at-point\nmagent-clear-log...")
```