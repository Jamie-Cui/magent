# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
make compile       # Byte-compile all Elisp files
make clean         # Remove compiled .elc files
```

Single-file compilation:
```bash
emacs -Q --batch -L . -L ~/proj/gptel -f batch-byte-compile magent-foo.el
```

The Makefile expects gptel source at `$HOME/proj/gptel` by default. Override with `GPTEL_DIR`:
```bash
make compile GPTEL_DIR=/path/to/gptel
```

## Testing

After any elisp code change, **always** test magent end-to-end in the running Emacs instance (via `emacsclient --eval`):

1. Reload changed files: `(load "/path/to/changed-file.el" nil t)`
2. Clear session: `(magent-clear-session)`
3. Run test prompts to verify both streaming and tool-calling:
   - **Non-tool prompt**: `"你好"` - verifies streaming text rendering and assistant section creation
   - **Tool-use prompt**: `"帮我看下 emacs 里面有多少 buffer"` - verifies tool calling loop (emacs_eval), UI rendering of `[tool: ...]` sections, and FSM state transitions
   - **Complex tool prompt**: `"帮我在 emacs 里面打开 magent 的 magit buffer"` - verifies multi-step tool execution

4. Check for errors in:
   - `*magent*` buffer - main output and tool results
   - `*magent-log*` buffer - FSM state transitions and debug logs
   - `*Messages*` buffer - Emacs errors (e.g., `progn: Beginning of buffer`, `Wrong type argument`)

5. Verify FSM reaches `DONE` state in `*magent-log*`

## Architecture

Magent is an Emacs Lisp AI coding agent with a multi-agent architecture and permission-based tool access. It delegates all LLM communication to **gptel** (the sole external dependency beyond Emacs 27.1+).

### Core Flow

1. **Entry point** (`magent.el`): `magent-mode` minor mode with `C-c m` keybinding prefix. Activating the mode initializes the agent registry, loads custom agents, and loads Claude Code skills.

2. **UI** (`magent-ui.el`): Minibuffer input, output buffer rendering, and logging. The `*magent*` buffer uses overlay-based collapsible sections:
   - Each message block (user, assistant, tool, error) gets a `[type]` header with a dot-dash line extending to window width, followed by a collapsible body.
   - Tool calls render as `[tool: tool_name]` sections with args and result in the body.
   - Sections fold/unfold with `TAB`; `S-TAB` toggles all sections. `magent-output-mode` provides these keybindings.
   - Streaming responses create the section overlay on completion; tool-only rounds (no text streamed) clean up the orphaned header.
   - Assistant message bodies are fontified via a temporary `org-mode` buffer.
   - Commands: `magent-prompt`, `magent-prompt-region`, `magent-ask-at-point`.

3. **Agent processing** (`magent-agent.el`): `magent-agent-process` builds a gptel prompt list from the session, applies per-agent overrides (model, temperature), filters tools by permissions, then calls `gptel-request`. gptel handles the LLM communication and tool-calling loop. The callback receives either a final string response or an error.

4. **Agent definitions**: Two files compose the agent system:
   - `magent-agent-registry.el`: Consolidated file containing `cl-defstruct` (`magent-agent-info`) with fields (name, description, mode, native, hidden, temperature, top-p, color, model, prompt, options, steps, permission), 7 built-in agents (`build`, `plan`, `explore`, `general`, `compaction`, `title`, `summary`), and hash-table registry with lookup, filtering by mode/visibility, and interactive selection. Provides feature aliases for `magent-agent-info` and `magent-agent-types`.
   - `magent-agent-file.el`: Loads custom agents from `.magent/agent/*.md` (YAML frontmatter + markdown body as system prompt)

5. **Permission system** (`magent-permission.el`): Rule-based access control per agent. Rules map tool names to `allow`/`deny`/`ask`, with optional nested file-pattern rules (glob syntax). Resolution order: exact tool match → nested file rules → wildcard (`*`) fallback → default deny.

6. **Tools** (`magent-tools.el`): Implements `read_file`, `write_file`, `edit_file`, `grep`, `glob`, `bash`, `emacs_eval`, `delegate`, `skill_invoke` as `gptel-tool` structs (9 total). Tools are registered globally but filtered per-agent through the permission system. `delegate` spawns a nested `gptel-request` using a named subagent. `skill_invoke` calls Claude Code skills (currently: `emacs` skill).

7. **FSM** (`magent-fsm.el` + backends): Unified entry point for the tool-calling loop with two switchable backends controlled by `magent-fsm-backend`:
   - `magent-fsm-backend-native.el`: Custom FSM with states INIT → SEND → WAIT → PROCESS → TOOL → DONE/ERROR. Handles HTTP via gptel, tool execution with mutex-based synchronization, and permission-aware confirmation.
   - `magent-fsm-backend-gptel.el`: Lightweight wrapper around `gptel-request` that delegates the entire tool-calling loop to gptel. Uses a hidden request buffer; tools auto-execute without interactive confirmation (gptel's confirm UI requires a visible buffer).

   The unified API (`magent-fsm-create`, `magent-fsm-start`, `magent-fsm-abort`, `magent-fsm-destroy`) dispatches to the configured backend. Default backend is `gptel`.

8. **Skills** (`magent-skills.el` + `magent-skill-file.el` + `magent-skill-emacs.el`): Claude Code/OpenCode style skill system with two types:
   - **instruction type**: Markdown body is injected into the system prompt. LLM follows instructions and uses available tools directly.
   - **tool type**: Skill is invoked via `skill_invoke` tool with predefined operations.

   Skills are loaded from:
   - Global: `~/.emacs.d/magent-skills/<name>/SKILL.md`
   - Project: `.magent/skills/<name>/SKILL.md`

   Tool-type skills can have companion `.el` files defining `magent-skill-<name>-invoke` function.

   Built-in `emacs` skill (tool-type) provides: list-functions, describe-function, eval-expression, execute-keys, minibuffer-prompt, current-buffer-state.

9. **Session** (`magent-session.el`): Conversation state with messages list, assigned agent, and history trimming. Persists to `~/.emacs.d/magent-sessions/` as JSON. Converts to gptel prompt list format for API calls.

### Key Design Decisions

- **No custom HTTP client**: All LLM communication goes through gptel. Provider, model, and API key configuration is managed by gptel (`gptel-backend`, `gptel-model`, `gptel-api-key`).
- **Switchable FSM backends**: `magent-fsm-backend` (default `gptel`) selects between the native FSM and gptel's built-in tool loop. The gptel backend is simpler (no custom state machine) but skips interactive tool confirmation. The native backend provides full FSM control with permission-based confirmation prompts.
- **Per-agent gptel overrides**: `magent-agent-info-apply-gptel-overrides` temporarily sets gptel variables (model, temperature, backend) for the duration of a request.
- **Agent modes**: `primary` (user-facing), `subagent` (called internally), `all` (either role).
- **Tool filtering**: Tools are defined once globally but each agent only sees tools allowed by its permission rules.
- **Skill types**: `instruction` (Claude Code style - prompts injected to system message) vs `tool` (traditional - invoked via `skill_invoke`).

### Skill File Format

```markdown
---
name: skill-name
description: Brief description
tools: bash, read        # Required tools (optional)
type: instruction        # 'instruction' or 'tool'
---

# Skill Instructions

The markdown body becomes part of the system prompt for instruction-type skills.
For tool-type skills, this describes available operations.
```

### Configuration

Magent-specific settings via `customize-group RET magent` (17 defcustom variables): `magent-system-prompt`, `magent-buffer-name`, `magent-auto-scroll`, `magent-enable-tools`, `magent-project-root-function`, `magent-max-history`, `magent-default-agent`, `magent-load-custom-agents`, `magent-enable-logging`, `magent-assistant-prompt` (tag text for `[assistant]` headers), `magent-user-prompt` (tag text for `[user]` headers), `magent-tool-call-prompt` (tag text for `[tool: ...]` headers), `magent-error-prompt` (tag text for `[error]` headers), `magent-agent-directory`, `magent-session-directory`, `magent-grep-program`, `magent-fsm-backend` (FSM backend: `gptel` or `magent`).

Skill-specific settings:
- `magent-skill-directories`: List of directories to scan for skill files (default: `~/.emacs.d/magent-skills`)
- `magent-skill-file-name`: Skill definition file name (default: `SKILL.md`)

LLM provider/model/key settings are managed entirely by gptel — configure via `gptel-backend`, `gptel-model`, and `gptel-api-key` (or env vars `ANTHROPIC_API_KEY`/`OPENAI_API_KEY`).

### Interactive Commands

| Command | Description |
|---------|-------------|
| `magent-list-skills` | Display all registered skills |
| `magent-describe-skill` | Show detailed skill information |
| `magent-reload-skills` | Reload skills from disk |

