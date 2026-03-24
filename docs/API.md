# Magent API Reference

## Interactive Commands

### Primary Commands

#### `magent-dwim`
**Keybinding:** `C-c m p`

Smart prompt command. Opens output buffer if not visible, or prompts for input with auto-context attachment.

#### `magent-prompt-region`
**Keybinding:** `C-c m r`

Send selected region to AI with optional prompt.

#### `magent-ask-at-point`
**Keybinding:** `C-c m a`

Ask about symbol at point.

### Session Management

#### `magent-clear-session`
**Keybinding:** `C-c m c`

Clear current session (messages, permissions, queue).

#### `magent-resume-session`
**Keybinding:** None

Resume a saved session from history.

#### `magent-select-agent`
**Keybinding:** `C-c m A`

Select an agent for the current session.

#### `magent-show-current-agent`
**Keybinding:** `C-c m i`

Display current session's agent info.

#### `magent-list-agents`
**Keybinding:** `C-c m v`

List all available agents.

### Diagnostics

#### `magent-diagnose-emacs`
**Keybinding:** `C-c m d`

Start structured diagnosis of current Emacs session.

#### `magent-doctor`
**Keybinding:** `C-c m D`

Run magent self-check and diagnose issues.

#### `magent-show-log`
**Keybinding:** `C-c m l`

View API request/response log.

#### `magent-clear-log`
**Keybinding:** `C-c m L`

Clear the log buffer.

### UI Commands

#### `magent-ui-toggle-section`
**Keybinding:** `C-c m t`

Toggle fold/unfold of section at point.

#### `magent-toggle-by-pass-permission`
**Keybinding:** None

Toggle permission bypass for tool filtering and approval prompts.

## Programmatic API

### Session Functions

#### `magent-session-reset`
Clear session state, permission overrides, and queue.

```elisp
(magent-session-reset)
```

#### `magent-session-add-message`
Add a message to the session history.

```elisp
(magent-session-add-message role content &optional tool-calls tool-results)
```

- `role`: "user" or "assistant"
- `content`: Message text
- `tool-calls`: Optional list of tool call plists
- `tool-results`: Optional list of tool result plists

#### `magent-session-save`
Save current session to disk.

```elisp
(magent-session-save)
```

#### `magent-session-load`
Load session from file.

```elisp
(magent-session-load file)
```

### Agent Functions

#### `magent-agent-process`
Process a prompt with the current agent.

```elisp
(magent-agent-process prompt &optional callback)
```

- `prompt`: User prompt string
- `callback`: Optional completion callback

#### `magent-agent-get`
Get agent info by name.

```elisp
(magent-agent-get name)
```

Returns `magent-agent-info` struct or nil.

### Permission Functions

#### `magent-permission-resolve`
Resolve permission for a tool and optional file path.

```elisp
(magent-permission-resolve tool-name &optional file-path permissions)
```

Returns: `allow`, `deny`, or `ask`

#### `magent-permission-set-override`
Set a temporary permission override for the session.

```elisp
(magent-permission-set-override tool-name action &optional file-pattern)
```

### Tool Functions

#### `magent-tools-get-enabled`
Get list of enabled tool structs filtered by permissions.

```elisp
(magent-tools-get-enabled &optional permissions)
```

### Skill Functions

#### `magent-skills-get`
Get skill by name.

```elisp
(magent-skills-get name)
```

#### `magent-skills-list`
List all loaded skills.

```elisp
(magent-skills-list &optional type)
```

- `type`: Optional filter by "instruction" or "tool"

## Data Structures

### `magent-agent-info`
Agent configuration struct.

**Fields:**
- `name` — Agent name (string)
- `description` — Brief description
- `mode` — `primary`, `subagent`, or `all`
- `hidden` — Hide from UI (boolean)
- `temperature` — Override temperature (number or nil)
- `model` — Override model (string or nil)
- `prompt` — System prompt (string)
- `permission` — Permission rules (alist)

### `magent-session`
Session state struct.

**Fields:**
- `messages` — Message history (list)
- `agent` — Current agent name (string)
- `project-root` — Project root path (string or nil)
- `buffer-content` — Raw buffer text for restore (string)

### `magent-skill`
Skill definition struct.

**Fields:**
- `name` — Skill name (string)
- `description` — Brief description (string)
- `type` — "instruction" or "tool"
- `tools` — Required tools (list of symbols)
- `body` — Skill content (string)
- `dir` — Source directory (string)

## Customization Variables

### Core Settings

- `magent-system-prompt` — Default system prompt
- `magent-buffer-name` — Output buffer name (default: `"*magent*"`)
- `magent-default-agent` — Default agent (default: `"build"`)
- `magent-enable-tools` — Globally enabled tools (list of symbols)

### Session & History

- `magent-session-directory` — Session storage directory
- `magent-max-history` — Max messages in history (default: 100)
- `magent-audit-directory` — Audit log directory

### Timeouts

- `magent-request-timeout` — LLM request timeout (default: 120s)
- `magent-bash-timeout` — Bash command timeout (default: 30s)
- `magent-emacs-eval-timeout` — Emacs eval timeout (default: 10s)

### UI Settings

- `magent-auto-scroll` — Auto-scroll output buffer (default: t)
- `magent-include-reasoning` — Display reasoning blocks (default: t)
- `magent-ui-batch-insert-delay` — Streaming batch delay (default: 0.05s)
- `magent-ui-fontify-threshold` — Async fontification threshold (default: 500 chars)

### Tool Settings

- `magent-grep-program` — Ripgrep binary path (default: `"rg"`)
- `magent-grep-max-matches` — Max grep matches (default: 100)

### Permissions

- `magent-by-pass-permission` — Bypass permission prompts (default: nil)
- `magent-enable-audit-log` — Enable audit logging (default: t)

## Hooks

### `magent-mode-hook`
Run when `magent-mode` is enabled.

### `magent-output-mode-hook`
Run when output buffer is created.

## Faces

- `magent-user-face` — User message headers
- `magent-assistant-face` — Assistant message headers
- `magent-tool-call-face` — Tool call markers
- `magent-error-face` — Error message headers
