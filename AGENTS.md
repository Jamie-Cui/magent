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

1. **Entry point** (`magent.el`): `magent-mode` minor mode with `C-c m` keybinding prefix. Activating the mode initializes the agent registry, loads custom agents, and loads Claude Code skills.

2. **UI** (`magent-ui.el`): Minibuffer input, output buffer rendering (markdown, tool calls, errors), and logging. Commands like `magent-prompt`, `magent-prompt-region`, `magent-ask-at-point` feed into the agent system.

3. **Agent processing** (`magent-agent.el`): `magent-agent-process` builds a gptel prompt list from the session, applies per-agent overrides (model, temperature), filters tools by permissions, then calls `gptel-request`. gptel handles the LLM communication and tool-calling loop. The callback receives either a final string response or an error.

4. **Agent definitions**: Two files compose the agent system:
   - `magent-agent-registry.el`: Consolidated file containing `cl-defstruct` (`magent-agent-info`) with fields (name, description, mode, native, hidden, temperature, top-p, color, model, prompt, options, steps, permission), 7 built-in agents (`build`, `plan`, `explore`, `general`, `compaction`, `title`, `summary`), and hash-table registry with lookup, filtering by mode/visibility, and interactive selection. Provides feature aliases for `magent-agent-info` and `magent-agent-types`.
   - `magent-agent-file.el`: Loads custom agents from `.magent/agent/*.md` (YAML frontmatter + markdown body as system prompt)

5. **Permission system** (`magent-permission.el`): Rule-based access control per agent. Rules map tool names to `allow`/`deny`/`ask`, with optional nested file-pattern rules (glob syntax). Resolution order: exact tool match → nested file rules → wildcard (`*`) fallback → default deny.

6. **Tools** (`magent-tools.el`): Implements `read_file`, `write_file`, `edit_file`, `grep`, `glob`, `bash`, `emacs_eval`, `delegate`, `skill_invoke` as `gptel-tool` structs (9 total). Tools are registered globally but filtered per-agent through the permission system. `delegate` spawns a nested `gptel-request` using a named subagent. `skill_invoke` calls Claude Code skills (currently: `emacs` skill).

7. **FSM** (`magent-fsm.el`): Finite state machine for tool-calling loop (INIT → SEND → WAIT → PROCESS → TOOL → DONE/ERROR). Currently delegates HTTP to gptel via `gptel-request`.

8. **Skills** (`magent-skills.el` + `magent-skill-emacs.el`): Skill registry with built-in emacs skill for interacting with the running Emacs instance (list-functions, describe-function, eval-expression, execute-keys, minibuffer-prompt, current-buffer-state). Skills are invoked directly in-process via the `skill_invoke` tool.

9. **Session** (`magent-session.el`): Conversation state with messages list, assigned agent, and history trimming. Persists to `~/.emacs.d/magent-sessions/` as JSON. Converts to gptel prompt list format for API calls.

### Key Design Decisions

- **No custom HTTP client**: All LLM communication goes through gptel. Provider, model, and API key configuration is managed by gptel (`gptel-backend`, `gptel-model`, `gptel-api-key`).
- **Per-agent gptel overrides**: `magent-agent-info-apply-gptel-overrides` temporarily sets gptel variables (model, temperature, backend) for the duration of a request.
- **Agent modes**: `primary` (user-facing), `subagent` (called internally), `all` (either role).
- **Tool filtering**: Tools are defined once globally but each agent only sees tools allowed by its permission rules.

### Configuration

Magent-specific settings via `customize-group RET magent` (17 defcustom variables): `magent-system-prompt`, `magent-buffer-name`, `magent-auto-scroll`, `magent-enable-streaming`, `magent-enable-tools`, `magent-project-root-function`, `magent-max-history`, `magent-default-agent`, `magent-load-custom-agents`, `magent-enable-logging`, `magent-assistant-prompt`, `magent-user-prompt`, `magent-tool-call-prompt`, `magent-error-prompt`, `magent-agent-directory`, `magent-session-directory`, `magent-grep-program`.

LLM provider/model/key settings are managed entirely by gptel — configure via `gptel-backend`, `gptel-model`, and `gptel-api-key` (or env vars `ANTHROPIC_API_KEY`/`OPENAI_API_KEY`).
