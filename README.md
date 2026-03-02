# Magent - AI Coding Agent for Emacs

An Emacs Lisp AI coding agent with multi-agent architecture, permission-based tool access, and LLM integration via [gptel](https://github.com/karthink/gptel).

## Features

- **Multi-agent system** with specialized agents (build, plan, explore, general)
- **Permission-based tool access** with fine-grained control per agent
- **Custom agent support** via `.magent/agent/*.md` files
- **LLM integration via gptel** supporting Anthropic Claude, OpenAI GPT, and compatible APIs
- **File operations**: read, write, edit, grep, glob
- **Shell command execution** via bash tool
- **Emacs Lisp evaluation** via emacs_eval tool
- **Agent delegation** with explore and general subagents
- **Claude Code skills** for interacting with the running Emacs instance
- **Session management** with conversation history and persistence
- **Minibuffer interface** for quick prompts

## Installation

### Manual Installation

Add the project to your Emacs load path:

```elisp
(add-to-list 'load-path "/path/to/magent/lisp")
(require 'magent)
```

### Using `use-package`

```elisp
(use-package magent
  :load-path "/path/to/magent/lisp"
  :config
  (global-magent-mode 1))
```

## Configuration

### Quick Start

Magent delegates all LLM communication to [gptel](https://github.com/karthink/gptel). Configure your provider, model, and API key through gptel:

```elisp
;; gptel handles provider/model/key configuration
(setq gptel-model 'claude-sonnet-4-20250514)
(setq gptel-api-key "sk-ant-...")  ; or use ANTHROPIC_API_KEY env var
```

See [gptel documentation](https://github.com/karthink/gptel#configuration) for full provider setup (Anthropic, OpenAI, Ollama, etc.).

### Magent-Specific Options

Customize with `M-x customize-group RET magent RET`:

| Option | Default | Description |
|--------|---------|-------------|
| `magent-system-prompt` | (built-in) | Default system prompt for agents |
| `magent-buffer-name` | `"*magent*"` | Output buffer name |
| `magent-auto-scroll` | `t` | Auto-scroll output buffer |
| `magent-enable-streaming` | `t` | Enable streaming responses |
| `magent-enable-tools` | all 9 tools | Globally enabled tools |
| `magent-project-root-function` | `nil` | Custom project root finder |
| `magent-max-history` | `100` | Max messages in history |
| `magent-default-agent` | `"build"` | Default agent for new sessions |
| `magent-load-custom-agents` | `t` | Load custom agents from `.magent/agent/*.md` |
| `magent-enable-logging` | `t` | Enable logging to `*magent-log*` buffer |
| `magent-assistant-prompt` | `"[AI  ] "` | Prefix for AI messages |
| `magent-user-prompt` | `"[USER] "` | Prefix for user messages |
| `magent-tool-call-prompt` | `"[TOOL] "` | Prefix for tool call notifications |
| `magent-error-prompt` | `"[ERR ] "` | Prefix for error messages |
| `magent-agent-directory` | `".magent/agent"` | Relative path to custom agent dir |
| `magent-session-directory` | `~/.emacs.d/magent-sessions/` | Session persistence directory |
| `magent-grep-program` | `"rg"` | Path to ripgrep binary |

## Usage

### Interactive Commands

| Command | Keybinding | Description |
|---------|-----------|-------------|
| `magent-prompt` | `C-c m p` | Prompt for input and send to AI |
| `magent-prompt-region` | `C-c m r` | Send selected region to AI |
| `magent-ask-at-point` | `C-c m a` | Ask about symbol at point |
| `magent-clear-session` | `C-c m c` | Clear current session |
| `magent-show-session` | `C-c m s` | Show session summary |
| `magent-show-log` | `C-c m l` | View API request/response log |
| `magent-clear-log` | `C-c m L` | Clear the log buffer |
| `magent-select-agent` | `C-c m A` | Select an agent for this session |
| `magent-show-current-agent` | `C-c m i` | Show current session's agent |
| `magent-list-agents` | `C-c m v` | List all available agents |

### Quick Example

```elisp
;; Enable the mode
(magent-mode 1)

;; Or globally
(global-magent-mode 1)

;; Send a prompt
M-x magent-prompt

;; Or use keybinding
C-c m p
```

## Agent System

Magent uses a multi-agent architecture where different agents have different capabilities and permissions.

### Built-in Agents

| Agent | Mode | Description |
|-------|------|-------------|
| `build` | primary | Default agent for general coding tasks with full tool access |
| `plan` | primary | Planning agent with restricted file edits (only `.magent/plan/*.md`) |
| `explore` | subagent | Fast codebase exploration (read/grep/glob/bash only) |
| `general` | subagent | General-purpose subagent for delegated tasks (no delegation) |
| `compaction` | primary (hidden) | Session summarization / conversation compaction |
| `title` | primary (hidden) | Conversation title generation |
| `summary` | primary (hidden) | Pull-request style summary generation |

### Agent Modes

- **primary**: User-facing agents that can be selected for sessions
- **subagent**: Internal agents called by primary agents for subtasks
- **all**: Can act as either primary or subagent

### Permission System

Each agent has permission rules controlling tool access:

```elisp
;; Example permission rules in an agent definition
(
 (read_file . allow)           ; Allow all file reads
 (write_file . ((deny "*.env")  ; Deny writing to .env files
                (deny "*.key")  ; Deny writing to .key files
                (allow "*")))   ; Allow writing to other files
 (bash . ask)                  ; Ask user before running bash commands
)
```

Permission actions:
- `allow`: Tool is allowed
- `deny`: Tool is blocked
- `ask`: Prompt user for confirmation

### Custom Agents

Create custom agents by adding markdown files to `.magent/agent/`:

**Example: `.magent/agent/reviewer.md`**

```markdown
---
description: Code review specialist
mode: primary
hidden: false
temperature: 0.3
permissions:
  - (read_file . allow)
  - (write_file . deny)
  - (bash . deny)
  - (grep . allow)
  - (glob . allow)
---

You are a code review specialist. Analyze code for:
- Bugs and potential issues
- Code style and best practices
- Performance optimizations
- Security vulnerabilities

Provide constructive feedback with specific examples.
```

The YAML frontmatter supports:
- `description`: Short description of the agent
- `mode`: `primary`, `subagent`, or `all`
- `hidden`: Hide from agent selection UI
- `temperature`: Override default temperature
- `model`: Override default model
- `permissions`: List of permission rules

## Available Tools

The AI agent has access to these tools (can be customized per agent):

| Tool | Side-effect | Description |
|------|-------------|-------------|
| `read_file` | no | Read file contents from the filesystem |
| `write_file` | yes | Write/create file (auto-creates parent dirs) |
| `edit_file` | yes | Replace exact text in file (must match once) |
| `grep` | no | Regex search via ripgrep; returns `file:line:content` |
| `glob` | no | Find files matching a glob pattern |
| `bash` | yes | Execute shell commands (default timeout 30s) |
| `emacs_eval` | yes | Evaluate Emacs Lisp expressions (default timeout 10s) |
| `delegate` | yes | Spawn a nested request using a named subagent |
| `skill_invoke` | no | Invoke Claude Code skills (e.g., Emacs interaction) |

Tools with side effects prompt the user for confirmation before execution.

Tool availability is controlled by:
1. Global `magent-enable-tools` setting
2. Per-agent permission rules

## Architecture

### Core Flow

1. **Entry Point** (`magent.el`): Defines `magent-mode` minor mode with `C-c m` keybinding prefix. Initializes the agent registry, loads custom agents, and loads Claude Code skills.

2. **Agent System**: Multi-agent architecture with permission-based tool access:
   - `magent-agent.el`: Builds gptel prompts, applies per-agent overrides, calls `gptel-request`
   - `magent-agent-registry.el`: Agent info struct, built-in agent definitions, and central registry (consolidated from previously separate files)
   - `magent-agent-file.el`: Custom agent loader (`.magent/agent/*.md`)

3. **Permission System** (`magent-permission.el`): Rule-based tool access control per agent with file-pattern matching (glob syntax).

4. **FSM** (`magent-fsm.el`): Finite state machine for the tool-calling loop (INIT → SEND → WAIT → PROCESS → TOOL → DONE/ERROR). Currently delegates HTTP to gptel.

5. **LLM Integration** (via gptel): All LLM communication is handled by [gptel](https://github.com/karthink/gptel). Provider, model, and API key configuration is managed entirely by gptel.

6. **Tools** (`magent-tools.el`): Tool implementations registered as `gptel-tool` structs, filtered per agent based on permission rules.

7. **Skills** (`magent-skills.el` + `magent-skill-emacs.el`): Skill registry with built-in emacs skill for Emacs interaction. Skills are invoked directly in-process via the `skill_invoke` tool.

8. **Session** (`magent-session.el`): Conversation history management with per-session agent assignment and JSON persistence.

### File Structure

```
lisp/
├── magent.el                  # Main entry point and mode definition
├── magent-config.el           # Configuration (customize group, 17 defcustom vars)
├── magent-session.el          # Session & message history (JSON persistence)
├── magent-tools.el            # Tool implementations (9 gptel-tool structs)
├── magent-agent.el            # Agent logic (gptel integration)
├── magent-agent-registry.el   # Agent struct, built-in definitions, and registry
├── magent-agent-file.el       # Custom agent file loader (.magent/agent/*.md)
├── magent-permission.el       # Permission system (allow/deny/ask with glob patterns)
├── magent-fsm.el              # FSM for tool-calling loop
├── magent-skill-emacs.el      # Built-in Emacs interaction skill
├── magent-skills.el           # Skill registry
├── magent-ui.el               # Minibuffer UI & output buffer rendering
└── magent-pkg.el              # Package descriptor
```

### Key Data Structures

- `magent-agent-info`: Agent configuration struct (name, description, mode, permissions, prompt, temperature, model, etc.)
- `magent-session`: Conversation state with messages list, assigned agent, and history trimming
- `magent-fsm`: FSM state for tool-calling loop (state, session, agent, tools, pending-tools, etc.)
- `magent-skill`: Loaded Claude Code skill (name, description, tools, body, dir)

## Development

### Build Commands

```bash
make compile    # Byte-compile all Elisp files
make test       # Run tests (requires magent-tests.el)
make clean      # Remove compiled .elc files
```

### Single File Compilation

```bash
emacs -Q --batch -L lisp -f batch-byte-compile lisp/magent-foo.el
```

## Session Persistence

Sessions are automatically saved to `~/.emacs.d/magent-sessions/` (configurable). Each session maintains:
- Conversation history
- Assigned agent
- Message metadata

## Troubleshooting

### Enable Logging

View request/response logs:

```elisp
(setq magent-enable-logging t)
M-x magent-show-log    ; C-c m l
```

### Check Agent Configuration

```elisp
M-x magent-list-agents        ; List all agents
M-x magent-show-current-agent ; Show current session's agent
```

### Verify API Key

API keys are managed by gptel. Check with:

```elisp
gptel-api-key  ; Should return your API key
```

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

## Credits

- **Inspired by**: [OpenCode](https://github.com/anomalyco/opencode)

## Contributing

Contributions are welcome! Please ensure:
- Code follows existing style conventions
- All files byte-compile without warnings
- Documentation is updated for new features

## Links

- **Project Repository**: https://github.com/jamie-cui/magent
