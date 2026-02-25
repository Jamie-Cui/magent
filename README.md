# Magent - AI Coding Agent for Emacs

An Emacs Lisp AI coding agent with multi-agent architecture, permission-based tool access, and LLM integration via [gptel](https://github.com/karthink/gptel).

## Features

- **Multi-agent system** with specialized agents (build, plan, explore, general)
- **Permission-based tool access** with fine-grained control per agent
- **Custom agent support** via `.magent/agent/*.md` files
- **LLM integration via gptel** supporting Anthropic Claude, OpenAI GPT, and compatible APIs
- **File operations**: read, write, grep, glob
- **Shell command execution** via bash tool
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
| `magent-enable-tools` | `(read write grep glob bash)` | Enabled tools |
| `magent-max-history` | `100` | Max messages in history |
| `magent-default-agent` | `"build"` | Default agent for new sessions |
| `magent-load-custom-agents` | `t` | Load custom agents from `.magent/agent/*.md` |
| `magent-enable-logging` | `t` | Enable logging to `*magent-log*` buffer |

## Usage

### Interactive Commands

| Command | Keybinding | Description |
|---------|-----------|-------------|
| `magent-prompt` | `C-c o p` | Prompt for input and send to AI |
| `magent-prompt-region` | `C-c o r` | Send selected region to AI |
| `magent-ask-at-point` | `C-c o a` | Ask about symbol at point |
| `magent-clear-session` | `C-c o c` | Clear current session |
| `magent-show-session` | `C-c o s` | Show session summary |
| `magent-view-log` | `C-c o l` | View API request/response log |
| `magent-clear-log` | `C-c o L` | Clear the log buffer |
| `magent-select-agent` | `C-c o A` | Select an agent for this session |
| `magent-show-current-agent` | `C-c o i` | Show current session's agent |
| `magent-list-agents` | `C-c o v` | List all available agents |

### Quick Example

```elisp
;; Enable the mode
(magent-mode 1)

;; Or globally
(global-magent-mode 1)

;; Send a prompt
M-x magent-prompt

;; Or use keybinding
C-c o p
```

## Agent System

Magent uses a multi-agent architecture where different agents have different capabilities and permissions.

### Built-in Agents

| Agent | Mode | Description |
|-------|------|-------------|
| `build` | primary | Default agent for general coding tasks with full tool access |
| `plan` | primary | Planning agent for design and architecture tasks |
| `explore` | subagent | Fast exploration agent for codebase navigation |
| `general` | subagent | General-purpose subagent for delegated tasks |
| `compaction` | all | Conversation compaction agent |
| `title` | all | Session title generation agent |
| `summary` | all | Summarization agent |

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

| Tool | Description |
|------|-------------|
| `read_file` | Read file contents from the filesystem |
| `write_file` | Write content to a file |
| `grep` | Search for patterns in files using regular expressions |
| `glob` | Find files matching a glob pattern |
| `bash` | Execute shell commands |

Tool availability is controlled by:
1. Global `magent-enable-tools` setting
2. Per-agent permission rules

## Architecture

### Core Flow

1. **Entry Point** (`magent.el`): Defines `magent-mode` minor mode with keybindings. Initializes the agent registry and loads custom agents.

2. **Agent System**: Multi-agent architecture with permission-based tool access:
   - `magent-agent.el`: Builds gptel prompts, applies per-agent overrides, calls `gptel-request`
   - `magent-agent-registry.el`: Central registry for agent lookup/registration
   - `magent-agent-info.el`: Agent configuration data structures
   - `magent-agent-types.el`: Built-in agent definitions
   - `magent-agent-file.el`: Custom agent loader

3. **Permission System** (`magent-permission.el`): Rule-based tool access control per agent with file-pattern matching.

4. **LLM Integration** (via gptel): All LLM communication, message formatting, and tool-calling loops are handled by [gptel](https://github.com/karthink/gptel).

5. **Tools** (`magent-tools.el`): Tool implementations registered as `gptel-tool` structs, filtered per agent based on permission rules.

6. **Session** (`magent-session.el`): Conversation history management with per-session agent assignment and persistence.

### File Structure

```
lisp/
├── magent.el                  # Main entry point and mode definition
├── magent-config.el           # Configuration (customize group)
├── magent-session.el          # Session & message history
├── magent-tools.el            # Tool implementations (gptel-tool structs)
├── magent-agent.el            # Agent logic (gptel integration)
├── magent-agent-registry.el   # Agent registration system
├── magent-agent-info.el       # Agent data structures
├── magent-agent-types.el      # Built-in agent definitions
├── magent-agent-file.el       # Custom agent file loader
├── magent-permission.el       # Permission system
├── magent-ui.el               # Minibuffer UI & display
└── magent-pkg.el              # Package descriptor
```

### Key Data Structures

- `magent-agent-info`: Agent configuration struct with name, mode, permissions, prompt
- `magent-session`: Conversation state with messages list and assigned agent
- `magent-permission`: Rule-based access control with file pattern matching

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
M-x magent-view-log
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
