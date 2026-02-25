# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
make compile       # Byte-compile all Elisp files
make clean         # Remove compiled .elc files
```

Single-file compilation:
```bash
emacs -Q --batch -L lisp -L ~/proj/gptel -f batch-byte-compile lisp/magent-foo.el
```

The Makefile expects gptel source at `$HOME/proj/gptel` by default. Override with `GPTEL_DIR`:
```bash
make compile GPTEL_DIR=/path/to/gptel
```

## Architecture

Magent is an Emacs Lisp AI coding agent with a multi-agent architecture and permission-based tool access. It delegates all LLM communication to **gptel** (the sole external dependency beyond Emacs 27.1+).

### Core Flow

1. **Entry point** (`magent.el`): `magent-mode` minor mode with `C-c o` keybinding prefix. Activating the mode initializes the agent registry and loads custom agents.

2. **UI** (`magent-ui.el`): Minibuffer input, output buffer rendering (markdown, tool calls, errors), and logging. Commands like `magent-prompt`, `magent-prompt-region`, `magent-ask-at-point` feed into the agent system.

3. **Agent processing** (`magent-agent.el`): `magent-agent-process` builds a gptel prompt list from the session, applies per-agent overrides (model, temperature), filters tools by permissions, then calls `gptel-request`. gptel handles the LLM communication and tool-calling loop. The callback receives either a final string response or an error.

4. **Agent definitions**: Five files compose the agent system:
   - `magent-agent-info.el`: `cl-defstruct` with fields: name, description, mode, native, hidden, temperature, top-p, color, model, prompt, options, steps, permission
   - `magent-agent-types.el`: 7 built-in agents — `build` (default primary), `plan` (primary), `explore` (subagent), `general` (subagent), `compaction`, `title`, `summary` (all internal)
   - `magent-agent-registry.el`: Hash-table registry with lookup, filtering by mode/visibility, and interactive selection
   - `magent-agent-file.el`: Loads custom agents from `.magent/agent/*.md` (YAML frontmatter + markdown body as system prompt)

5. **Permission system** (`magent-permission.el`): Rule-based access control per agent. Rules map tool names to `allow`/`deny`/`ask`, with optional nested file-pattern rules (glob syntax). Resolution order: exact tool match → nested file rules → wildcard (`*`) fallback → default deny.

6. **Tools** (`magent-tools.el`): Implements `read_file`, `write_file`, `edit_file`, `grep`, `glob`, `bash`, `emacs_eval`, `delegate` as `gptel-tool` structs. Tools are registered globally but filtered per-agent through the permission system. `delegate` spawns a nested `gptel-request` using a named subagent.

7. **Session** (`magent-session.el`): Conversation state with messages list, assigned agent, and history trimming. Persists to `~/.emacs.d/magent-sessions/` as JSON. Converts to gptel prompt list format for API calls.

### Key Design Decisions

- **No custom HTTP client**: All LLM communication goes through gptel. Provider, model, and API key configuration is managed by gptel (`gptel-backend`, `gptel-model`, `gptel-api-key`).
- **Per-agent gptel overrides**: `magent-agent-info-apply-gptel-overrides` temporarily sets gptel variables (model, temperature, backend) for the duration of a request.
- **Agent modes**: `primary` (user-facing), `subagent` (called internally), `all` (either role).
- **Tool filtering**: Tools are defined once globally but each agent only sees tools allowed by its permission rules.

### Configuration

Magent-specific settings via `customize-group RET magent`: `magent-system-prompt`, `magent-buffer-name`, `magent-enable-tools`, `magent-max-history`, `magent-default-agent`, `magent-load-custom-agents`, `magent-enable-logging`.

LLM provider/model/key settings are managed entirely by gptel — configure via `gptel-backend`, `gptel-model`, and `gptel-api-key` (or env vars `ANTHROPIC_API_KEY`/`OPENAI_API_KEY`).
