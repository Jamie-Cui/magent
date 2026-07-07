---
title: Magent Architecture
lang: en
alt_url: /ARCHITECTURE.zh.html
---

# Magent Architecture

Magent is an Emacs-native AI coding agent. Its core design choice is that a serious Emacs workflow already has rich live state: buffers, modes, projects, Magit, Org, xref, LSP, process buffers, and years of user-specific interaction habits. Magent therefore runs as an Emacs Lisp agent runtime instead of wrapping a terminal-only coding agent.

The boundary is intentionally narrow:

- Provider transport, HTTP/SSE handling, model selection, and API keys stay in `gptel` through `gptel-request`.
- User interaction defaults to `agent-shell` through an in-process ACP adapter, while the old special-mode workspace remains an isolated legacy backend.
- Agent execution, tool orchestration, permission decisions, sessions, skills, capabilities, and child-agent jobs are owned by Magent.
- Codex-style seatbelt, bubblewrap, sandbox, and shell isolation are out of scope. Magent permissions are workflow controls and audit records, not an OS security boundary.

## System Boundary

```text
User
  |
  v
agent-shell / legacy UI
  |
  v
magent-ui.el -> magent-agent-shell.el -> magent-acp.el
  |
  v
magent-runtime-api.el -> magent-runtime-queue.el
  |
  v
thread/turn/item ledger in magent-ledger.el and magent-session.el
  |
  v
magent-agent.el -> magent-agent-loop.el
  |                     |
  |                     v
  |              magent-tool-orchestrator.el -> magent-tools.el
  v
magent-llm-gptel.el -> gptel-request -> provider
```

This shape keeps replacement points visible. A provider change should stay behind `magent-llm-gptel.el` and gptel configuration. A UI change should stay around `magent-ui.el`, `magent-acp.el`, and `magent-runtime-api.el`. A persistence or resume change should go through the ledger and session modules. A tool policy change should go through the permission and orchestrator modules.

## Dependency Layers

### Emacs Runtime

Emacs is not just a host process. Magent depends on live editor state through buffers, major modes, project roots, timers, processes, URL retrieval, JSON parsing, `special-mode`, and user-installed packages. The `emacs_eval` tool is important because it lets the agent inspect the active editor instead of only reading files from disk.

### Provider Plumbing

`magent-llm.el` defines provider-neutral request and event shapes. `magent-llm-gptel.el` performs one sampling request by calling `gptel-request` and translating gptel callbacks into normalized Magent events. Magent may hide gptel callback/FSM details inside this adapter, but the rest of the loop consumes only normalized events.

### UI And Runtime API

The default UI backend is `agent-shell`. `magent-agent-shell.el` registers the Magent agent-shell configuration, `magent-acp.el` implements the in-process ACP adapter, and `magent-runtime-api.el` receives prompt submissions from UI backends. `magent-runtime-queue.el` currently runs one active turn globally, while preserving session-scoped cancellation and queue bookkeeping.

The old workspace/compose UI lives in `magent-ui-legacy.el`. It is loaded lazily and should not become a dependency of the default agent-shell path.

### Ledger And Persistence

The durable source of truth is a `thread -> turn -> item` ledger. `magent-ledger.el` defines the state objects, transitions, and journal events. `magent-runtime-api.el` creates user submissions, while `magent-runtime-queue.el` schedules active execution. `magent-session.el` stores a materialized `snapshot` plus an append-only `journal`.

Legacy `messages`, `context-items`, and `buffer-content` are projections or migration data. They are not the canonical UI source of truth.

### Tools, Permissions, And Audit

`magent-tools.el` exposes 14 `gptel-tool` structs:

- `read_file`, `write_file`, `edit_file`
- `grep`, `glob`, `bash`
- `emacs_eval`
- `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, `close_agent`
- `skill_invoke`, `web_search`

`magent-tool-orchestrator.el` resolves permissions, asks for approval when needed, dispatches the tool call, writes audit records, and reports results back to the loop. `magent-permission.el` resolves rules in this order: exact tool match, file-pattern rules, wildcard fallback, then default allow.

## Request Lifecycle

1. A user prompt enters through agent-shell or the legacy UI.
2. The UI backend submits to `magent-runtime-api.el`.
3. `magent-runtime-api.el` records a queued turn and a completed user item.
4. `magent-runtime-queue.el` starts the turn when the global execution slot is free.
5. `magent-agent.el` chooses the session agent, active skills, capability instructions, and allowed tools.
6. `magent-llm-gptel.el` calls `gptel-request` for one sampling request.
7. `magent-agent-loop.el` receives normalized text, reasoning, tool-call, and completion events.
8. Tool calls are accumulated until `tool-call-batch-end`, then dispatched serially through the orchestrator.
9. Tool results update the same ledger item that started with the tool call.
10. If tool output should be shown to the model, `magent-agent.el` rebuilds the prompt from the ledger and starts the next sampling request.
11. Completion, failure, abort, or queue drop transitions the turn to its terminal state and notifies the UI backend.

This continuation model is Codex-style in the sense that tool results feed a follow-up sampling request, but it remains Emacs-native and gptel-backed.

## Extension Model

Magent has three file-backed extension layers:

- Custom agents under `.magent/agent/*.md`.
- Skills under bundled `skills/`, user `~/.emacs.d/magent-skills/`, and project `.magent/skills/`.
- Capabilities under bundled `capabilities/` and project `.magent/capabilities/`.

Instruction skills are injected into the system prompt. Tool skills are invoked through `skill_invoke`. Capabilities score the current context and activate a small number of instruction skills. This progressive disclosure model keeps the base prompt smaller while still giving the agent Emacs-specific workflow knowledge when the current request needs it.

## Product Tradeoffs

Magent is closest to gptel plus a stateful agent runtime, not a replacement for gptel. Compared with wrapping a terminal agent inside Emacs, Magent can inspect and use live editor context. Compared with Codex or Claude Code, Magent does not aim for broad multi-surface coverage or strong OS isolation; its advantage is low-friction access to a long-lived Emacs environment.

The practical consequence is that new work should preserve these boundaries:

- Keep provider work in gptel and `magent-llm-gptel.el`.
- Keep default UI work out of `magent-ui-legacy.el`.
- Keep durable workflow state in the ledger.
- Keep child-agent behavior aligned with `docs/AGENT_JOBS.md`.
- Treat permissions as confirmation/audit workflow, not as sandbox security.
