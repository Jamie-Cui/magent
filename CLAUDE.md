# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
make compile    # Byte-compile all Elisp files
make test       # Run tests (requires magent-tests.el)
make clean      # Remove compiled .elc files
```

For single-file compilation:
```bash
emacs -Q --batch -L lisp -f batch-byte-compile lisp/magent-foo.el
```

## Architecture

Magent is an Emacs Lisp AI coding agent that integrates with Anthropic Claude and OpenAI models. The package uses a modular architecture with clear separation of concerns.

### Core Flow

1. **Entry Point** (`magent.el`): Defines `magent-mode` minor mode with keybindings (`C-c o` prefix). Initializes the agent registry and loads custom agents on mode activation.

2. **Agent System**: Multi-agent architecture with permission-based tool access:
   - `magent-agent.el`: Main agent loop that processes prompts, handles tool calling, and manages iterations (max 10 per request)
   - `magent-agent-registry.el`: Central registry for agent lookup/registration
   - `magent-agent-info.el`: `cl-defstruct` defining agent properties (name, mode, permissions, temperature, model, prompt)
   - `magent-agent-types.el`: Built-in agents: `build` (default), `plan`, `explore`, `general`, `compaction`, `title`, `summary`
   - `magent-agent-file.el`: Loads custom agents from `.opencode/agent/*.md` files (YAML frontmatter + markdown)

3. **Permission System** (`magent-permission.el`): Rule-based tool access control per agent. Supports `allow`, `deny`, `ask` with file-pattern matching (e.g., deny `*.env` files).

4. **API Layer** (`magent-api.el`): HTTP client supporting Anthropic and OpenAI formats. Handles message conversion, tool definitions, and streaming. Uses `url-retrieve` for async requests.

5. **Tools** (`magent-tools.el`): Implements `read_file`, `write_file`, `grep`, `glob`, `bash`. Tools are filtered per-agent based on permission rules.

6. **Session** (`magent-session.el`): Manages conversation history with per-session agent assignment. Supports persistence to `~/.emacs.d/magent-sessions/`.

### Agent Modes

- `primary`: User-facing agents (build, plan)
- `subagent`: Internal agents called by primary agents (explore, general)
- `all`: Can act as either

### Key Data Structures

- `magent-agent-info`: Agent configuration struct with name, mode, permissions, prompt
- `magent-session`: Conversation state with messages list and assigned agent
- `magent-permission`: Rule-based access control (`(tool . allow/deny/ask)` or nested file patterns)

### Configuration

All config via `customize-group RET magent`:
- `magent-provider`: `'anthropic`, `'openai`, or `'openai-compatible`
- `magent-api-key`: Direct key or use env vars `ANTHROPIC_API_KEY`/`OPENAI_API_KEY`
- `magent-enable-tools`: List of enabled tool symbols
- `magent-load-custom-agents`: Load from `.opencode/agent/*.md`

### Custom Agents

Create `.opencode/agent/myagent.md`:
```markdown
---
description: My custom agent
mode: primary
hidden: false
temperature: 0.5
---

Your custom system prompt here.
```
