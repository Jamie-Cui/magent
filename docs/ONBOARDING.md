---
title: Magent Onboarding Guide
lang: en
alt_url: /ONBOARDING.zh.html
---

# Magent Onboarding Guide

**Updated:** 2026-07-07

## Project Overview

**Magent** is an Emacs Lisp AI coding agent with multi-agent architecture, a durable turn ledger, and permission-based tool access.

- **Languages:** Emacs Lisp
- **Primary Dependency:** [gptel](https://github.com/karthink/gptel) (handles all LLM communication)
- **Requirements:** Emacs 29.1+, gptel, transient, compat, yaml, acp, agent-shell, ripgrep
- **Purpose:** Provide AI-assisted coding capabilities inside the live Emacs runtime, while keeping provider transport in gptel and keeping tool access explicit and auditable

## Current Agent Workflow

Magent has a Magent-owned agent loop and a durable child-agent lifecycle for collaborative work. Root agents can spawn, message, wait for, list, inspect/resume, and close child-agent jobs.

Read `docs/AGENT_JOBS.md` before changing child-agent behavior, session persistence, or agent-related tools. Codex sandbox behavior is explicitly out of scope.

## Architecture Layers

### Layer 1: Entry Point & Configuration

The foundation layer that initializes the system and manages settings.

**Key Files:**
- `magent.el` — Main entry point, defines `magent-mode` minor mode with `C-c m` prefix
- `magent-config.el` — All `defcustom` variables and `defface` definitions
- `magent-pkg.el` — Package metadata

**What it does:** Lazy initialization triggered on first command via `magent--ensure-initialized`. Mode enable only adds modeline construct; full setup (agent registry, skills) happens on demand.

### Layer 2: Session & Runtime State Management

Manages conversation history, scoped overlays, and runtime state.

**Key Files:**
- `magent-ledger.el` — Thread/turn/item ledger, status transitions, snapshot shape
- `magent-session.el` — Conversation projections, JSON persistence, per-project sessions
- `magent-agent-job.el` — Durable child-agent job records and JSON shape
- `magent-runtime.el` — Static initialization plus project-local overlay activation for agents, skills, and capabilities
- `magent-runtime-api.el` — UI/backend-facing runtime session and prompt API
- `magent-runtime-queue.el` — Global single-execution runtime queue with session-scoped cancellation
- `magent-audit.el` — Persistent JSONL audit logging for permissions and sensitive actions

**What it does:** Maintains ledger-backed conversation history scoped by project, persists to `~/.emacs.d/magent/sessions/` by default, stores durable child-agent jobs under `agent-jobs`, and activates or unloads project-local overlays as scope changes. Runtime UI backends submit work through `magent-runtime-api.el`; `magent-runtime-queue.el` owns queued/active turn state and session-scoped cancellation.

### Layer 3: Agent System

Multi-agent architecture with specialized agents for different tasks.

**Key Files:**
- `magent-agent.el` — Core agent processing: builds gptel prompts, applies overrides, calls `gptel-request`
- `magent-agent-info.el` — Agent metadata struct and helpers
- `magent-agent-builtins.el` — 7 built-in agent definitions
- `magent-agent-registry.el` — Hash-table registry and project overlays
- `magent-agent-file.el` — Loads custom agents from `.magent/agent/*.md` files
- `magent-permission.el` — Rule-based tool access control (allow/deny/ask with glob patterns)

**What it does:** Provides specialized agents (`build`, `plan`, `explore`, `general`, and hidden utility agents) with different capabilities. Permission system filters tools per agent. Custom agents extend functionality via markdown files with YAML frontmatter.

**Current behavior:** Magent replaced the old one-shot `delegate` surface with durable child-agent jobs that have stable ids, status, transcript/result storage, and parent/child session relationships.

### Layer 4: Tools & Capabilities

The action layer that executes operations requested by agents.

**Key Files:**
- `magent-tools.el` — 14 `gptel-tool` structs: read_file, write_file, edit_file, grep, glob, bash, emacs_eval, spawn_agent, send_agent_message, wait_agent, list_agents, close_agent, skill_invoke, web_search
- `magent-skills.el` — Skill registry, built-in skills, file loading, inspection commands
- `magent-capability.el` — Capability definitions, resolution, and file-backed loading
- `magent-approval.el` — User approval prompts for sensitive operations

**What it does:** Tools provide concrete actions (file I/O, shell commands, web search) and child-agent coordination (`spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, `close_agent`). Skills extend agent behavior (instruction-type injected into prompts, tool-type invoked via skill_invoke). Approval system gates dangerous operations.

### Layer 5: Agent Loop & LLM Integration

Orchestrates the tool-calling loop and LLM communication.

**Key Files:**
- `magent-agent-loop.el` — Active Magent-owned loop, tool dispatch, serial queueing, abort helpers, continuation
- `magent-llm.el` — Provider-neutral request/event protocol
- `magent-llm-gptel.el` — Thin `gptel-request` sampling adapter

**What it does:** `magent-agent-loop.el` consumes normalized LLM events, records assistant/tool state into the session, dispatches tools through `magent-tool-orchestrator`, handles visible tool rendering, abort cleanup, and Codex-style continuation. Tool results are fed back to the model rather than being stopped by an `emacs_eval` call-count guard. `magent-llm-gptel.el` still calls `gptel-request`; Magent does not rewrite provider transport.

### Layer 6: UI Backends

Default agent-shell UI plus an isolated legacy workspace/compose backend.

**Key Files:**
- `magent-ui.el` — Thin public command router, logger, backend switch, and compatibility shims
- `magent-agent-shell.el` — Magent agent-shell config and prompt routing
- `magent-acp.el` — In-process ACP adapter for agent-shell
- `magent-ui-legacy.el` — Legacy workspace/compose buffers, ledger projection, streaming summaries, transient menu
- `magent-evil.el` — Optional Evil integration loaded explicitly by Evil users
- `magent-markdown-to-org.el` — Legacy markdown → org converter, no longer used by the live workspace path
- `magent-file-loader.el` — Shared frontmatter parser for agent/skill/capability files

**What it does:** Plain prompts route to agent-shell by default through
`magent-agent-shell.el` and the in-process ACP adapter. ACP session/prompt
requests submit to `magent-runtime-api.el` and remain pending until the
corresponding Magent turn completes. The legacy UI remains available through
`magent-ui-legacy.el`; it derives from `special-mode`, renders an oldest-first
ledger timeline, and keeps prompt text in a scope-specific
`magent-compose-mode` buffer. See `docs/UI_BACKENDS.md` before changing this
boundary.

### Layer 7: Events

**Key Files:**
- `magent-lifecycle-events.el` — Structured lifecycle hooks for turns, subagents, and tool calls

**What it does:** Provides structured lifecycle hooks for turns, subagents, and tool calls.

## Key Concepts

### Multi-Agent Architecture

Magent uses specialized agents with different capabilities:
- **build** (default) — Full tool access for general coding
- **plan** — Restricted file edits (only `.magent/plan/*.md`)
- **explore** — Fast codebase exploration (read/grep/glob/bash only)
- **general** — General-purpose subagent for child-agent tasks
- **compaction**, **title**, **summary** — Hidden utility agents

Agents have modes: `primary` (user-facing), `subagent` (internal), `all` (either).

Current child-agent architecture is documented in `docs/AGENT_JOBS.md`. The Codex workflow alignment plan remains useful as implementation history.

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

Instruction skills can be selected as one-shot context for the next request.
The agent-shell backend exposes native commands such as
`magent-agent-shell-toggle-skill-for-next-request` and
`magent-agent-shell-run-skill-command`; the legacy transient Skills submenu
continues to expose the same workflow for the legacy workspace. Skills that
define `default-prompt` can also be run as command-like actions; built-in
`init` initializes or refreshes the project root `AGENTS.md`, similar to Codex
`/init`, and accepts optional extra instructions. Compose buffers are plain
prompt text and no longer parse `@skill` or `@clear` control syntax.

Skills load from: (1) built-in `skills/`, (2) user `~/.emacs.d/magent-skills/`, (3) project `.magent/skills/`.

### Session Scoping

Sessions are project-aware:
- In a project: state scoped to that project
- Outside projects: global session fallback
- Persists to `~/.emacs.d/magent/sessions/projects/<sha1>/`
- Stores durable child-agent job metadata, result/error state, and transcripts in `agent-jobs`

### Lazy Initialization

Mode enable is lightweight (modeline only). Full initialization (registry, skills) happens on first command via `magent--ensure-initialized`.

## Guided Tour

### Step 0: Read the Architecture Boundary

Start with `docs/ARCHITECTURE.md` for the product boundary: Magent keeps provider plumbing in gptel, runs the agent loop in Emacs Lisp, exposes Emacs runtime context through tools, and does not implement Codex-style OS sandboxing.

### Step 1: Start with the Entry Point

Begin at `magent.el` to understand how the mode is activated and what commands are available. The `C-c m` prefix map shows all interactive entry points.

### Step 2: Explore the UI Boundary

Read `docs/UI_BACKENDS.md`, then `magent-ui.el`, `magent-agent-shell.el`, and
`magent-acp.el`. Key insight: `magent-ui.el` is a thin router; the default
interactive experience is agent-shell. Legacy workspace rendering and compose
submission live in `magent-ui-legacy.el`.

### Step 3: Follow a Request Lifecycle

Trace a request through these files in order:
1. `magent-agent.el` — `magent-agent-process` builds the prompt
2. `magent-agent-loop.el` — Owns normalized events, tool dispatch, queueing, abort, and continuation
3. `magent-llm-gptel.el` — Calls `gptel-request` for one sampling request
4. `magent-tool-orchestrator.el` / `magent-tools.el` — Resolve permissions and execute tool implementations
5. `magent-runtime-api.el` / `magent-acp.el` — Backend submissions and UI-neutral events for agent-shell
6. `magent-ui-legacy.el` — Legacy buffer rendering when the legacy backend is active

### Step 4: Understand Permissions

Read `magent-permission.el` to see how tool access is controlled. The `magent-permission-resolve` function shows the resolution order. Then look at `magent-agent-builtins.el` to see how built-in agents define their permissions.

### Step 5: Explore Extensibility

Check out:
- `magent-agent-file.el` — How custom agents load from `.magent/agent/*.md`
- `magent-skills.el` — How skills extend agent capabilities
- `magent-file-loader.el` — The shared frontmatter parser used by both

### Step 6: Study the Tools

Read `magent-tools.el` to understand the available tools. Pay attention to:
- `emacs_eval` — Executes in the request buffer context (uses `magent-tools--request-buffer-name`)
- `spawn_agent` / `send_agent_message` / `wait_agent` / `list_agents` / `close_agent` — Coordinate durable child-agent jobs
- `web_search` — DuckDuckGo integration via `url-retrieve`

Then read `docs/AGENT_JOBS.md` for the lifecycle contract and persistence boundaries.

### Step 7: Review Testing

Look at `test/magent-test.el` to see how the codebase is tested. Tests mock `gptel-request` and use `cl-letf` to isolate state. This shows you the public API surface.

## File Map

### Core Entry & Configuration
- **magent.el** — Mode definition, keybindings, lazy initialization
- **magent-config.el** — All customization variables, faces, logging stub
- **magent-json.el** — JSON-safe serialization helpers

### State Management
- **magent-session.el** — Conversation history, per-project sessions, JSON persistence
- **magent-runtime.el** — Static initialization and project-local overlay activation
- **magent-audit.el** — JSONL audit logs for permissions and sensitive actions

### Agent System
- **magent-agent.el** — Core processing: builds prompts, filters tools, calls gptel
- **magent-agent-info.el** — Agent metadata struct and helpers
- **magent-agent-builtins.el** — 7 built-in agent definitions
- **magent-agent-registry.el** — Agent registry and project overlays
- **magent-agent-file.el** — Custom agent loader from `.magent/agent/*.md`
- **magent-permission.el** — Tool access control with glob patterns

### Tools & Skills
- **magent-tools.el** — 14 tool implementations as gptel-tool structs
- **magent-skills.el** — Skill registry, built-in skills, file loading, commands
- **magent-capability.el** — Capability definitions, resolution, and file-backed loading
- **magent-approval.el** — User approval prompts for sensitive operations

### Agent Loop & LLM Integration
- **magent-agent-loop.el** — Active normalized event loop, tool dispatch, queueing, guards, abort, continuation
- **magent-llm.el** — Provider-neutral request/event protocol
- **magent-llm-gptel.el** — `gptel-request` adapter

### User Interface
- **magent-ui.el** — Thin backend router, logger, and compatibility shims
- **magent-agent-shell.el** — agent-shell backend config and prompt routing
- **magent-acp.el** — In-process ACP adapter
- **magent-runtime-api.el** — UI/backend-facing runtime API
- **magent-runtime-queue.el** — Runtime queue and session-scoped cancellation
- **magent-ui-legacy.el** — Legacy workspace/compose buffers, streaming summaries, transcript/detail views, transient menu
- **magent-markdown-to-org.el** — Legacy markdown to org-mode converter outside the live workspace path
- **magent-file-loader.el** — Shared frontmatter parser

### Events
- **magent-lifecycle-events.el** — Structured lifecycle hooks for turns, subagents, and tool calls

### CI And Package Data
- **.github/workflows/test.yml** — Byte-compilation, unit tests, and deterministic live smoke tests
- **.github/workflows/coverage.yml** — Batch `testcover` run and coverage artifact upload
- **.github/workflows/melpazoid.yml** — MELPA-style package checks
- **test/coverage.el** — Coverage runner used by `make coverage`

Magent packages runtime data as well as Elisp. `prompt.org`, `skills/`, and
`capabilities/` must remain included in the melpazoid/MELPA recipe because
`magent-config.el`, `magent-skills.el`, and `magent-capability.el` resolve
those paths at runtime.

## Complexity Hotspots

These areas require careful attention when modifying:

### 1. magent-agent-loop.el (High Complexity)
**Why it's complex:** Owns the active request/tool loop: normalized event accumulation, provider-neutral tool-call batch completion, serial execution, permission orchestration, UI tool rendering, abort cleanup, tool-result session recording, and continuation.

**Approach carefully:** Any changes to loop state, tool callback ordering, or abort handling can hang a turn or corrupt session history. Add focused ERT coverage first, then verify live with tool-use prompts when Emacs is available.

### 2. magent-llm-gptel.el (Medium Complexity)
**Why it's complex:** It is intentionally the only place that may touch gptel callback/FSM details. It converts provider callback shapes into normalized Magent events without letting gptel's tool-loop semantics leak into the main loop.

**Approach carefully:** Keep provider transport concerns here and loop behavior in `magent-agent-loop.el`. Do not add new main-loop dependencies on gptel private FSM handlers.

### 3. UI backend boundary (High Complexity)
**Why it's complex:** The default UI now spans `magent-ui.el`, `magent-agent-shell.el`, `magent-acp.el`, `magent-runtime-api.el`, and `magent-runtime-queue.el`. The legacy workspace/compose implementation remains in `magent-ui-legacy.el` and still has custom insertion logic, async fontification, chunk batching, and read-only region management.

**Approach carefully:** Keep default agent-shell behavior out of `magent-ui-legacy.el`. Keep backend-neutral behavior in `magent-runtime-api.el`, ACP conversion in `magent-acp.el`, and agent-shell-specific behavior in `magent-agent-shell.el`. In legacy code, insertions must use `inhibit-read-only`.

### 4. magent-permission.el (Medium Complexity)
**Why it's complex:** Order-dependent file-pattern matching with glob syntax. Resolution order matters: exact match → file patterns → wildcard → default allow.

**Approach carefully:** More specific patterns must come before less specific ones. Test with various glob patterns.

### 5. magent-tools.el (Medium Complexity)
**Why it's complex:** 14 different tool implementations with varying side effects, timeouts, child-agent runtime state, and error handling. `emacs_eval` uses `magent-tools--request-buffer-name` to execute in correct buffer context.

**Approach carefully:** Tools run in process filters. Timeout handling must be robust. Context capture for `emacs_eval`, child-agent status persistence, and parent/child request-context inheritance are critical.

### 6. magent-session.el (Medium Complexity)
**Why it's complex:** Per-project session scoping with global fallback, JSON persistence, ledger-driven UI restoration, legacy buffer-content migration, child-agent job persistence, and history trimming.

**Approach carefully:** Session directory calculation uses SHA1 of project root. Buffer content and `agent-jobs` must be preserved for lossless restore and child transcript inspection.

## Development Workflow

### Building & Testing

```bash
# Byte-compile all files
make compile

# Run full test suite
make test

# Run unit tests only
make test-unit

# Run coverage and write coverage/testcover-summary.tsv
make coverage

# Clean compiled files
make clean
```

`make test-live-smoke` requires an Emacs server and uses stubbed gptel
transport. `make test-live` uses the real configured provider and may consume
tokens, so run it only against an isolated daemon.

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
3. Update built-in agent permissions in `magent-agent-builtins.el`

For agent lifecycle tools such as `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, or `close_agent`, update `docs/AGENT_JOBS.md` and related tests. These tools should remain one coherent job lifecycle rather than unrelated standalone tools.

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

- **Documentation:** `README.org`, `AGENTS.md` in repo root
- **Child-agent architecture:** `docs/AGENT_JOBS.md`
- **Interactive help:** `M-x magent-doctor` for self-diagnostics
- **Command menu:** `C-c m ?` from any Magent-enabled buffer, or `?` in `*magent*`
- **Skills:** transient `s s` to toggle a one-shot instruction skill, `s K` to clear selected skills
- **Logs:** transient `l l` to view request/response log
- **Agent info:** transient `A v` to list agents, transient `A m` for current agent

## Next Steps

1. **Run the tests** — Start with `make test-unit`; use an isolated Emacs server before running the full `make test`
2. **Try the examples** — Enable `magent-mode` and run `C-c m p`
3. **Read the code** — Follow the guided tour above
4. **Experiment** — Create a custom agent or skill
5. **Contribute** — Check open issues and submit PRs

---

**Welcome to magent!** This guide should help you get oriented. The codebase follows clear separation of concerns with each module handling a specific responsibility. Start with the guided tour and don't hesitate to dive into the code—it's well-structured and documented.
