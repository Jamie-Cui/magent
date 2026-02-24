# Agent System Design

This document details the multi-agent architecture in Magent, including agent types, permissions, lifecycle, and customization.

## Overview

Magent uses a multi-agent system where different agents have specialized capabilities, prompts, and tool access permissions. This allows for:

- **Task specialization**: Different agents optimized for different workflows
- **Security**: Restrict dangerous operations per agent
- **Flexibility**: Users create custom agents for their needs
- **Efficiency**: Specialized prompts reduce token usage

## Agent Types

Agents are categorized by their mode, which determines how they can be used:

### Primary Agents

User-facing agents that can be selected for interactive sessions.

**Built-in Primary Agents:**

1. **build** (default)
   - General-purpose coding agent
   - Full tool access (read, write, grep, glob, bash)
   - Default agent for new sessions
   - Temperature: 0.7

2. **plan**
   - Planning and architecture design
   - Read-only access to most files
   - Can only edit `.magent/plan/*.md` files
   - Focused on organizing work before implementation
   - Temperature: 0.7

### Subagents

Internal agents called by primary agents for specialized subtasks. Not directly selectable by users.

**Built-in Subagents:**

1. **explore**
   - Fast codebase exploration
   - Tools: read, grep, glob, bash (read-only operations)
   - Specialized prompt for efficient file searching
   - No write or edit permissions
   - Thoroughness levels: quick, medium, very thorough

2. **general**
   - Multi-step task execution
   - Similar permissions to build agent
   - Used for delegated complex tasks
   - Cannot access todo tools (avoid circular calls)

### Internal Agents

Hidden agents for system operations. Mode is set to `all` but hidden from user selection.

**Built-in Internal Agents:**

1. **compaction**
   - Session summarization
   - No tool access (text generation only)
   - Creates detailed conversation summaries
   - Used when sessions grow too long

2. **title**
   - Generate conversation titles
   - No tool access
   - Creates brief titles (≤50 chars)
   - Used for session management UI

3. **summary**
   - PR-style summaries
   - No tool access
   - 2-3 sentence summaries of changes
   - Used for commit messages, PR descriptions

## Agent Data Structure

Agents are defined using the `magent-agent-info` struct:

```elisp
(cl-defstruct magent-agent-info
  name           ; Unique identifier (string)
  description    ; Human-readable description
  mode           ; 'primary, 'subagent, or 'all
  native         ; t if built-in, nil if custom
  hidden         ; Hide from user selection
  prompt         ; System prompt (string or nil)
  permission     ; Permission rules (see Permission System)
  temperature    ; Override default temperature
  top-p          ; Override default top-p
  model          ; Override default model
  color)         ; UI color hint
```

## Permission System

Each agent has a permission ruleset controlling tool access.

### Permission Actions

- **allow**: Tool is permitted unconditionally
- **deny**: Tool is blocked
- **ask**: Prompt user for confirmation before executing

### Permission Rule Format

Permissions are defined as alists with optional nesting:

```elisp
;; Simple rules
((bash . allow)
 (write_file . deny))

;; File-specific rules
((read_file . ((\"*.env\" . deny)
               (\"*.env.*\" . deny)
               (\"*.env.example\" . allow)
               (\"*\" . allow))))

;; Wildcard default
((* . allow))
```

### Permission Resolution

When a tool is requested, permissions are resolved in this order:

1. **Exact tool match**: Check for tool-specific rule
2. **File pattern match**: If file provided, check pattern rules
3. **Wildcard rule**: Check for `*` default
4. **Default**: Return `allow` if no rules match

**Example Resolution:**

Agent permission rules:
```elisp
((read_file . ((\"*.env\" . deny)
               (\"*\" . allow)))
 (write_file . ask)
 (bash . deny))
```

Resolutions:
- `read_file("/foo/bar.el")` → **allow** (matches `*` pattern)
- `read_file("/foo/.env")` → **deny** (matches `*.env` pattern)
- `write_file("/foo/bar.el")` → **ask** (prompts user)
- `bash("ls")` → **deny** (explicit denial)
- `grep("pattern", "/")` → **allow** (default, no rule specified)

### Default Permissions

The default permission set (used by build agent):

```elisp
(
 ;; Allow most tools
 (* . allow)

 ;; Special cases requiring confirmation
 (doom_loop . ask)
 (external_directory . ask)

 ;; Protect sensitive files
 (read_file . ((\"*.env\" . deny)
               (\"*.env.*\" . deny)
               (\"*.env.example\" . allow)
               (\"*\" . allow)))
)
```

### Example Agent Permissions

**Plan Agent:**
```elisp
(
 (* . allow)  ; Allow most tools
 (edit . ((\"*\" . deny)  ; Deny all edits except...
          (\".magent/plan/*.md\" . allow)))  ; ...plan files
)
```

**Explore Agent:**
```elisp
(
 (* . deny)  ; Deny by default
 (grep . allow)
 (glob . allow)
 (list . allow)
 (bash . allow)  ; Read-only bash commands
 (read . allow)
)
```

**Review Agent (custom example):**
```elisp
(
 (read_file . allow)
 (grep . allow)
 (glob . allow)
 (write_file . deny)  ; No modifications
 (bash . deny)
)
```

## Agent Lifecycle

### 1. Registration

Agents are registered during initialization:

**Built-in agents:**
```elisp
(defun magent-agent-registry-init ()
  "Initialize the agent registry with built-in agents."
  (setq magent--agent-registry (make-hash-table :test 'equal))
  (dolist (agent (magent-agent-types-initialize))
    (magent-agent-registry-register agent)))
```

**Custom agents:**
```elisp
(when magent-load-custom-agents
  (magent-agent-file-load-all))
```

### 2. Selection

Agents are assigned to sessions:

**Default assignment:**
```elisp
(magent-session-set-agent session
  (magent-agent-registry-get magent-default-agent))
```

**Manual selection:**
```elisp
M-x magent-select-agent
```

**Programmatic:**
```elisp
(magent-agent-process "prompt" #'callback
  (magent-agent-registry-get "explore"))
```

### 3. Execution

Agent processes user prompts through gptel:

1. Get agent's system prompt
2. Filter tools by agent permissions into `gptel-tool` list
3. Apply per-agent gptel overrides (model, temperature)
4. Call `gptel-request` — gptel manages the tool-calling loop
5. Callback receives final response or error

### 4. Session Persistence

Agent assignment persists with the session:

```elisp
(magent-session-save session)  ; Saves agent association
```

## Custom Agents

Users can create custom agents by adding markdown files to `.magent/agent/`.

### File Format

**Path:** `.magent/agent/myagent.md`

```markdown
---
description: Short description of what this agent does
mode: primary
hidden: false
temperature: 0.5
top_p: 0.9
model: claude-sonnet-4-20250514
tools:
  bash: false
  read: true
  write: true
  grep: true
  glob: true
---

You are a specialized agent for [purpose].

Your strengths:
- [Capability 1]
- [Capability 2]

Guidelines:
- [Guideline 1]
- [Guideline 2]

Complete the user's request following these guidelines.
```

### Frontmatter Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `description` | string | Yes | Brief agent description |
| `mode` | string | No | `primary`, `subagent`, or `all` (default: `all`) |
| `hidden` | boolean | No | Hide from user selection (default: `false`) |
| `temperature` | float | No | Override default temperature (0.0-1.0) |
| `top_p` | float | No | Override default top-p (0.0-1.0) |
| `model` | string | No | Override default model |
| `color` | string | No | UI color hint |
| `tools` | object | No | Tool permissions (key: tool, value: boolean) |

### Tool Configuration

The `tools` field specifies which tools are enabled:

```yaml
tools:
  bash: true       # Allow bash
  read: true       # Allow read_file
  write: false     # Deny write_file
  grep: true       # Allow grep
  glob: true       # Allow glob
```

**Note:** If a tool is not mentioned, it defaults to `allow`. To deny a tool, explicitly set it to `false`.

### Custom Agent Examples

#### Code Review Agent

```markdown
---
description: Code review specialist
mode: primary
hidden: false
temperature: 0.3
tools:
  bash: false
  write: false
---

You are a code review specialist. Analyze code for:
- Bugs and potential issues
- Code style and best practices
- Performance optimizations
- Security vulnerabilities

Provide constructive feedback with specific examples.
Use grep and glob to explore the codebase thoroughly.
```

#### Documentation Agent

```markdown
---
description: Documentation writer
mode: primary
temperature: 0.7
tools:
  bash: false
---

You are a documentation specialist. Create clear, comprehensive documentation including:
- API documentation
- User guides
- Code comments
- README files

Focus on clarity and completeness.
Use examples where appropriate.
```

#### Test Generator Agent

```markdown
---
description: Test case generator
mode: primary
temperature: 0.5
---

You are a test generation specialist. Create comprehensive test suites including:
- Unit tests
- Integration tests
- Edge cases
- Error conditions

Follow the project's testing conventions.
Ensure good code coverage.
```

## Agent Communication

### Primary to Subagent

Primary agents can delegate tasks to subagents (future enhancement):

```elisp
;; Future API design
(magent-agent-delegate "explore"
  "Find all API endpoint definitions")
```

### Agent Context Sharing

Agents share context through the session:

- All agents see the full conversation history
- Tool results are visible to subsequent iterations
- File contents read are part of the context

## Agent Registry API

### Registration

```elisp
(magent-agent-registry-register agent-info)
```

Adds agent to the global registry. Overwrites if agent with same name exists.

### Lookup

```elisp
(magent-agent-registry-get "build")  ; Get by name
(magent-agent-registry-get-default)  ; Get default agent
```

### Listing

```elisp
(magent-agent-registry-list)          ; All agents
(magent-agent-registry-primary-agents) ; Only primary agents
(magent-agent-registry-subagents)     ; Only subagents
(magent-agent-registry-list-names)    ; Just the names
```

### Management

```elisp
(magent-agent-registry-remove "myagent")  ; Remove agent
(magent-agent-registry-clear)             ; Clear all
(magent-agent-registry-init)              ; Reinitialize
```

## Best Practices

### Creating Custom Agents

1. **Clear Purpose**: Give agents a specific, focused purpose
2. **Minimal Permissions**: Only grant necessary tool access
3. **Detailed Prompts**: Provide clear guidelines and examples
4. **Appropriate Temperature**: Lower for deterministic tasks, higher for creative tasks
5. **Test Thoroughly**: Verify agent behavior with various inputs

### Using Agents

1. **Choose Wisely**: Select agent appropriate for the task
2. **Understand Limits**: Know what each agent can/cannot do
3. **Session Scope**: Keep related work in one session
4. **Monitor Iterations**: Watch for excessive API calls
5. **Custom Over Generic**: Create specialized agents for repeated tasks

### Permission Design

1. **Least Privilege**: Start with minimal permissions
2. **File Patterns**: Use patterns to protect sensitive files
3. **Ask for Dangerous**: Use `ask` for potentially harmful operations
4. **Test Patterns**: Verify file patterns match as expected
5. **Document Rules**: Comment complex permission logic

## Troubleshooting

### Agent Not Found

```elisp
(magent-agent-registry-list-names)  ; Check available agents
(magent-load-agent-files)           ; Reload custom agents
```

### Permission Denied

Check agent permissions:
```elisp
(let ((agent (magent-agent-registry-get "myagent")))
  (magent-agent-info-permission agent))
```

Test permission resolution:
```elisp
(magent-permission-resolve rules 'write_file "/path/to/file")
```

### Custom Agent Not Loading

1. Check file location: `.magent/agent/*.md`
2. Verify YAML syntax in frontmatter
3. Check `magent-load-custom-agents` is `t`
4. Look for errors in `*Messages*` buffer
5. Try loading manually: `M-x magent-load-agent-files`

### Agent Using Wrong Model

Priority order:
1. Agent-specific model override
2. gptel default (`gptel-model`)

Check agent's model override:
```elisp
(magent-agent-info-model agent-info)
```
