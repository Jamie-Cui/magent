# Child-Agent Jobs

Magent uses durable child-agent jobs for collaborative agent work. This replaces the old one-shot `delegate` tool with a small lifecycle that the root agent can coordinate explicitly.

## Tool Surface

The public child-agent tool surface is:

| Tool | Purpose |
| --- | --- |
| `spawn_agent` | Start a child-agent job and return a stable job id. |
| `send_agent_message` | Send follow-up input to an existing child job. |
| `wait_agent` | Wait for one or more jobs and return status/result data. |
| `list_agents` | List child jobs for the current parent session. |
| `close_agent` | Close a job and abort its live request when present. |

These tools share the `agent` permission key. The old `delegate` tool is not kept as a compatibility wrapper.

## Data Model

`magent-agent-job.el` defines `magent-agent-job`, the durable record for a child job. Each job stores:

- `id`
- `parent-session-id`
- `agent-name`
- `task-name`
- `status`
- `prompt`
- `created-at`
- `updated-at`
- `transcript`
- `result`
- `error`
- `metadata`

Valid statuses are `queued`, `running`, `waiting`, `completed`, `failed`, `closed`, and `cancelled`.

The parent `magent-session` persists child jobs in its `agent-jobs` slot. Runtime-only state for active loops lives in `magent-tools--agent-job-runtimes`, keyed by job id. This keeps parent session JSON sufficient for resume/inspection while avoiding serializing live request handles.

## Runtime Flow

1. `spawn_agent` creates a `magent-agent-job` in the parent session.
2. The child request starts through the Magent-owned loop in `magent-agent-loop.el`.
3. The child runs with summary-only UI so it does not write its full transcript into the parent conversation body.
4. Completion, failure, close, and wait events update the durable job record.
5. Tool results return model-visible JSON summaries to the root agent.

The provider boundary remains `gptel-request` through `magent-llm-gptel.el`. Magent owns orchestration, tool dispatch, persistence, abort behavior, and child-job coordination.

## Inheritance

Child jobs inherit the parent request context where practical:

- project root and session scope
- backend/model names
- temperature and top-p
- active skill names and capability context
- effective permission profile bounded by the parent and child agent
- request depth for recursive spawn guards

`magent-child-agent-max-depth` controls recursive spawning. The default allows direct children and blocks recursive child spawning.

## UI And Resume

The parent Magent UI renders compact child-agent lifecycle updates.
Full child prompt, metadata, result/error, and transcript state remain
inspectable with:

- `M-x magent-show-agent-transcript`
- transient `S j` from `C-c m ?` or `?` in `*magent*`

`magent-resume-session` restores persisted `agent-jobs` with the parent session. Active request handles are not restored after Emacs restart, but saved job metadata, result/error state, and transcripts remain available for inspection.

## Boundaries

This lifecycle intentionally preserves Magent's Emacs-native workflow:

- live Emacs buffers
- `emacs_eval`
- agent-shell UI plus the isolated legacy workspace/compose backend
- project-scoped sessions
- gptel transport

Codex sandbox, seatbelt, bubblewrap, and shell-isolation parity are out of scope.
