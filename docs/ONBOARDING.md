# Magent Onboarding Guide

**Generated:** 2026-03-24
**Git Commit:** 6f45f490a2a34f064a052e9844731388ce70eecb

## Project Overview

**Magent** is an Emacs Lisp AI coding agent with multi-agent architecture and permission-based tool access.

- **Languages:** Emacs Lisp
- **Primary Dependency:** [gptel](https://github.com/karthink/gptel) (handles all LLM communication)
- **Requirements:** Emacs 27.1+, spinner, transient, ripgrep
- **Purpose:** Provide AI-assisted coding capabilities within Emacs with fine-grained control over agent permissions and tool access

## Architecture Layers

### Layer 1: Entry Point & Configuration

The foundation layer that initializes the system and manages settings.

**Key Files:**
- `magent.el` — Main entry point, defines `magent-mode` minor mode with `C-c m` prefix
- `magent-config.el` — All `defcustom` variables, `defface` definitions, shared utilities
- `magent-pkg.el` — Package metadata

**What it does:** Lazy initialization triggered on first command via `magent--ensure-initialized`. Mode enable only adds modeline construct; full setup (agent registry, skills) happens on demand.

### Layer 2: Session & State Management

Manages conversation history and request serialization.

**Key Files:**
- `magent-session.el` — Conversation state with message list, JSON persistence, per-project sessions
- `magent-queue.el` — Single-request serialization (rejects new prompts when busy)
- `magent-audit.el` — Persistent JSONL audit logging for permissions and sensitive actions

**What it does:** Maintains conversation history scoped by project, persists to `~/.emacs.d/magent-sessions/`, stores raw buffer content for lossless restore. Queue ensures only one LLM request runs at a time.

### Layer 3: Agent System

Multi-agent architecture with specialized agents for different tasks.

**Key Files:**
- `magent-agent.el` — Core agent processing: builds gptel prompts, applies overrides, calls `gptel-request`
- `magent-agent-registry.el` — Agent struct definitions, 8 built-in agents, hash-table registry
- `magent-agent-file.el` — Loads custom agents from `.magent/agent/*.md` files
- `magent-permission.el` — Rule-based tool access control (allow/deny/ask with glob patterns)

**What it does:** Provides specialized agents (build, plan, explore, general, etc.) with different capabilities. Permission system filters tools per agent. Custom agents extend functionality via markdown files with YAML frontmatter.

### Layer 4: Tools & Capabilities

The action layer that executes operations requested by agents.

**Key Files:**
- `magent-tools.el` — 10 `gptel-tool` structs: read_file, write_file, edit_file, grep, glob, bash, emacs_eval, delegate, skill_invoke, web_search
- `magent-skills.el` — Skill registry, built-in skills, file loading, inspection commands
- `magent-capability.el` — Capability definitions and management
- `magent-capability-file.el` — File-based capability loading
- `magent-approval.el` — User approval prompts for sensitive operations

**What it does:** Tools provide concrete actions (file I/O, shell commands, web search). Skills extend agent behavior (instruction-type injected into prompts, tool-type invoked via skill_invoke). Approval system gates dangerous operations.

### Layer 5: FSM & LLM Integration

Orchestrates the tool-calling loop and LLM communication.

**Key Files:**
- `magent-fsm.el` — Unified FSM API (INIT → SEND → WAIT → PROCESS → TOOL → DONE/ERROR)
- `magent-fsm-shared.el` — FSM creation, tool conversion to gptel format
- `magent-fsm-backend-gptel.el` — Active backend: gptel callback, streaming state, unknown-tool advice
- `magent-fsm-backend-native.el` — Struct definitions (native execution disabled)

**What it does:** FSM manages the request lifecycle. Currently only gptel backend is active. Handles streaming responses, tool execution, and error recovery. `magent--handle-unknown-tools-a` advice prevents hangs from hallucinated tool names.

### Layer 6: User Interface

Org-mode derived buffer for interaction and output rendering.

**Key Files:**
- `magent-ui.el` — In-buffer input/output, org-mode derived, streaming sections, transient menu
- `magent-md2org.el` — Markdown → org-mode conversion for assistant output
- `magent-file-loader.el` — Shared frontmatter parser for agent/skill/capability files

**What it does:** `*magent*` buffer uses org-mode with custom faces. In-buffer input via `* [USER]` sections. Tool calls render as `#+begin_tool`/`#+end_tool` blocks (auto-folded). Streaming uses chunk batching and async fontification.

### Layer 7: Integrations

Optional integrations with other Emacs packages.

**Key Files:**
- `magent-magit.el` — Magit integration for AI-assisted commit messages and diff explanations
- `magent-events.el` — Event system for extensibility

**What it does:** Provides hooks into other Emacs workflows. Magit integration drafts commit messages and explains diffs through isolated `gptel-request` calls.

## Key Concepts

### Multi-Agent Architecture

Magent uses specialized agents with different capabilities:
- **build** (default) — Full tool access for general coding
- **plan** — Restricted file edits (only `.magent/plan/*.md`)
- **explore** — Fast codebase exploration (read/grep/glob/bash only)
- **general** — Subagent for delegated tasks (no delegation)
- **compaction**, **title**, **summary** — Hidden utility agents

Agents have modes: `primary` (user-facing), `subagent` (internal), `all` (either).

### Permission System

Fine-grained control over tool access per agent:
```elisp
((read_file . allow)              ; Allow all reads
 (write_file . ((deny "*.env")    ; Deny .env files
                (deny "*.key")    ; Deny .key files
                (allow "*")))     ; Allow others
 (bash . ask))                    ; Prompt user
```

Resolution order: exact tool match → file-pattern rules → wildcard (`*`) → default allow.

### Skill System

Two skill types:
- **instruction** — Markdown injected into system prompt
- **tool** — Invoked via `skill_invoke` tool

Skills load from: (1) built-in `skills/`, (2) user `~/.emacs.d/magent-skills/`, (3) project `.magent/skills/`.

### Session Scoping

Sessions are project-aware:
- In a project: state scoped to that project
- Outside projects: global session fallback
- Persists to `~/.emacs.d/magent-sessions/projects/<sha1>/`

### Lazy Initialization

Mode enable is lightweight (modeline only). Full initialization (registry, skills) happens on first command via `magent--ensure-initialized`.

## Guided Tour

### Step 1: Start with the Entry Point

Begin at `magent.el` to understand how the mode is activated and what commands are available. The `C-c m` prefix map shows all interactive entry points.

### Step 2: Explore the UI

Read `magent-ui.el` to see how the `*magent*` buffer works. Key insight: it derives from org-mode, so all org features (folding, fontification) apply. The `magent-ui--with-insert` macro is critical for understanding how streaming works.

### Step 3: Follow a Request Lifecycle

Trace a request through these files in order:
1. `magent-agent.el` — `magent-agent-process` builds the prompt
2. `magent-fsm.el` — FSM dispatches to backend
3. `magent-fsm-backend-gptel.el` — Handles streaming and tool execution
4. `magent-tools.el` — Tool implementations execute
5. `magent-ui.el` — Results render in buffer

### Step 4: Understand Permissions

Read `magent-permission.el` to see how tool access is controlled. The `magent-permission-resolve` function shows the resolution order. Then look at `magent-agent-registry.el` to see how built-in agents define their permissions.

### Step 5: Explore Extensibility

Check out:
- `magent-agent-file.el` — How custom agents load from `.magent/agent/*.md`
- `magent-skills.el` — How skills extend agent capabilities
- `magent-file-loader.el` — The shared frontmatter parser used by both

### Step 6: Study the Tools

Read `magent-tools.el` to understand the 10 available tools. Pay attention to:
- `emacs_eval` — Executes in the request buffer context (uses `magent-tools--request-buffer-name`)
- `delegate` — Spawns nested requests with subagents
- `web_search` — DuckDuckGo integration via `url-retrieve`

### Step 7: Review Testing

Look at `test/magent-test.el` to see how the codebase is tested. Tests mock `gptel-request` and use `cl-letf` to isolate state. This shows you the public API surface.

## File Map

### Core Entry & Configuration
- **magent.el** — Mode definition, keybindings, lazy initialization
- **magent-config.el** — All customization variables, faces, shared utilities, logging stub

### State Management
- **magent-session.el** — Conversation history, per-project sessions, JSON persistence
- **magent-queue.el** — Single-request serialization (busy rejection)
- **magent-audit.el** — JSONL audit logs for permissions and sensitive actions

### Agent System
- **magent-agent.el** — Core processing: builds prompts, filters tools, calls gptel
- **magent-agent-registry.el** — Agent struct, 8 built-in agents, registry
- **magent-agent-file.el** — Custom agent loader from `.magent/agent/*.md`
- **magent-permission.el** — Tool access control with glob patterns

### Tools & Skills
- **magent-tools.el** — 10 tool implementations as gptel-tool structs
- **magent-skills.el** — Skill registry, built-in skills, file loading, commands
- **magent-capability.el** — Capability definitions
- **magent-capability-file.el** — File-based capability loading
- **magent-approval.el** — User approval prompts for sensitive operations

### FSM & LLM Integration
- **magent-fsm.el** — FSM API, dispatches to backend
- **magent-fsm-shared.el** — FSM creation, tool conversion
- **magent-fsm-backend-gptel.el** — Active backend: streaming, callbacks, unknown-tool handling
- **magent-fsm-backend-native.el** — Struct definitions (execution disabled)

### User Interface
- **magent-ui.el** — Org-mode derived buffer, streaming, sections, transient menu
- **magent-md2org.el** — Markdown to org-mode converter
- **magent-file-loader.el** — Shared frontmatter parser

### Integrations
- **magent-magit.el** — Magit integration for commit messages and diff explanations
- **magent-events.el** — Event system for extensibility

## Complexity Hotspots

These areas require careful attention when modifying:

### 1. magent-fsm-backend-gptel.el (High Complexity)
**Why it's complex:** Manages streaming state, tool execution callbacks, and handles edge cases like qwen3-max splitting tool calls with empty names. Contains `magent--handle-unknown-tools-a` advice that merges split tool entries.

**Approach carefully:** Any changes to streaming logic or tool-use handling can break the FSM loop. Test thoroughly with multiple LLM providers.

### 2. magent-ui.el (High Complexity)
**Why it's complex:** Org-mode derived buffer with custom insertion logic, async fontification, chunk batching, and read-only region management. The `magent-ui--with-insert` macro suppresses buffer-boundary signals to prevent evil-mode errors.

**Approach carefully:** Insertions must use `inhibit-read-only`. Org fontification can trigger re-entrancy. Request generation counter prevents stale callbacks.

### 3. magent-permission.el (Medium Complexity)
**Why it's complex:** Order-dependent file-pattern matching with glob syntax. Resolution order matters: exact match → file patterns → wildcard → default allow.

**Approach carefully:** More specific patterns must come before less specific ones. Test with various glob patterns.

### 4. magent-tools.el (Medium Complexity)
**Why it's complex:** 10 different tool implementations with varying side effects, timeouts, and error handling. `emacs_eval` uses `magent-tools--request-buffer-name` to execute in correct buffer context.

**Approach carefully:** Tools run in process filters. Timeout handling must be robust. Context capture for `emacs_eval` is critical.

### 5. magent-session.el (Medium Complexity)
**Why it's complex:** Per-project session scoping with global fallback, JSON persistence, buffer-content restoration, and history trimming.

**Approach carefully:** Session directory calculation uses SHA1 of project root. Buffer content must be preserved for lossless restore.

## Development Workflow

### Building & Testing

```bash
# Byte-compile all files
make compile

# Run full test suite (~92 tests)
make test

# Clean compiled files
make clean
```

### Live Development

Test changes in running Emacs:

```bash
# Reload a changed file
emacsclient --eval '(load "/path/to/magent-foo.el" nil t)'

# Clear session state
emacsclient --eval '(magent-clear-session)'

# Check logs
emacsclient --eval '(with-current-buffer "*magent-log*" (buffer-string))'
```

### Test Prompts

After changes, verify with:
- **Non-tool:** `"你好"` — Tests streaming and assistant sections
- **Tool-use:** `"帮我看下 emacs 里面有多少 buffer"` — Tests `emacs_eval` tool
- **Multi-step:** `"帮我在 emacs 里面打开 magent 的 magit buffer"` — Tests chained execution

Check `*magent*`, `*magent-log*`, and `*Messages*` buffers for errors.

## Common Patterns

### Adding a New Tool

1. Define in `magent-tools.el`:
   ```elisp
   (defun magent-tools--my-tool (args)
     ;; Implementation
     )

   (defvar magent-tools--my-tool-tool
     (gptel-make-tool :function #'magent-tools--my-tool
                      :name "my_tool"
                      :description "What it does"))
   ```

2. Add to `magent-enable-tools` default in `magent-config.el`
3. Update agent permissions in `magent-agent-registry.el`

### Creating a Custom Agent

Create `.magent/agent/my-agent.md`:

```markdown
---
description: Agent purpose
mode: primary
temperature: 0.7
permissions:
  - (read_file . allow)
  - (write_file . ask)
  - (bash . deny)
---

System prompt goes here.
```

### Adding a Skill

Create `skills/my-skill/SKILL.md`:

```markdown
---
name: my-skill
description: Brief description
type: instruction
tools: read_file, grep
---

Skill instructions for the agent.
```

## Getting Help

- **Documentation:** `README.org`, `CLAUDE.md` in repo root
- **Interactive help:** `M-x magent-doctor` for self-diagnostics
- **Logs:** `C-c m l` to view request/response log
- **Agent info:** `C-c m v` to list agents, `C-c m i` for current agent

## Next Steps

1. **Run the tests** — `make test` to verify your environment
2. **Try the examples** — Enable `magent-mode` and run `C-c m p`
3. **Read the code** — Follow the guided tour above
4. **Experiment** — Create a custom agent or skill
5. **Contribute** — Check open issues and submit PRs

---

**Welcome to magent!** This guide should help you get oriented. The codebase follows clear separation of concerns with each module handling a specific responsibility. Start with the guided tour and don't hesitate to dive into the code—it's well-structured and documented.


