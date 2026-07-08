---
title: Agent Workflow State Machine
lang: en
alt_url: /AGENT_WORKFLOW.zh.html
---

# Agent Workflow State Machine

Magent now models the agent loop as an explicit `thread -> turn -> item`
ledger. This is the source of truth for agent workflow state. Legacy
session `messages` and `context-items` remain as derived projections for
gptel prompt construction, migration, and older tests.

## Codex Alignment

Codex exposes a thread event stream at the SDK/app-server boundary:

- `thread.started`
- `turn.queued`
- `turn.started`
- `item.started`
- `item.updated`
- `item.completed`
- `turn.completed` or `turn.failed`

Magent keeps the same workflow boundary, adapted to Emacs:

- Provider transport still goes through `gptel-request`.
- The Magent loop owns tool dispatch, continuation outcomes, abort, and
  Emacs UI rendering. The turn layer owns final-response policy, including
  one no-tool retry when a post-tool continuation produces empty assistant
  text.
- No Codex sandbox, seatbelt, bubblewrap, or shell isolation parity is
  introduced.

The closest Codex references are:

- `sdk/typescript/src/events.ts`: SDK event names and turn/item stream
  contract.
- `codex-rs/core/src/session/turn.rs`: core sampling loop, tool
  continuation, history recording, and follow-up decisions.
- `codex-rs/app-server/src/bespoke_event_handling.rs`: app-server
  conversion from core events into thread/turn notifications.
- `codex-rs/app-server-protocol/src/protocol/v2/thread.rs`: thread
  status shape.

## State Objects

`magent-ledger.el` defines the canonical ledger:

- `magent-thread`
  - statuses: `not-loaded`, `idle`, `active`, `system-error`, `closed`
- `magent-thread-turn`
  - statuses: `queued`, `in-progress`, `completed`, `interrupted`,
    `failed`, `dropped`
- `magent-thread-item`
  - statuses: `pending`, `in-progress`, `completed`, `failed`,
    `cancelled`

Each user prompt creates one turn. Assistant messages, reasoning blocks,
tool invocations, and tool outputs are items under that turn. Reasoning
items are never promoted to assistant message text, even when a provider
finishes with no visible content.

The transition table is explicit:

| Object | Runtime statuses | Terminal statuses |
| --- | --- | --- |
| thread | `not-loaded`, `idle`, `active`, `system-error`, `closed` | `closed` |
| turn | `queued`, `in-progress` | `completed`, `interrupted`, `failed`, `dropped` |
| item | `pending`, `in-progress` | `completed`, `failed`, `cancelled` |

Turn lifecycle:

```text
queued -> in-progress -> completed
                      -> interrupted
                      -> failed
                      -> dropped
```

Item lifecycle:

```text
pending -> in-progress -> completed
                       -> failed
                       -> cancelled
```

## Tool Items

Tool calls and tool results are one item lifecycle, not two persisted
records.

The item starts when the model asks for the tool:

```elisp
(:type tool :status in-progress :name "grep" :input (:pattern "..."))
```

It completes or fails when the tool result is available:

```elisp
(:type tool :status completed :output "...")
```

This differs from the older `tool-call` plus `tool-output` split. For
prompt reuse, Magent still projects a completed tool item into gptel's
historical `(tool . PLIST)` shape.

If the model asks for a tool and the result arrives later, Magent updates
the same item by `call-id`. If an older code path records only a tool
result, the loop creates a synthetic turn so the item still has a durable
turn parent.

## Persistence

Persistence is `snapshot + journal`.

- `journal`: append-only event log. It records lifecycle transitions such
  as `turn-queued`, `turn-started`, `item-started`, `item-completed`, and
  `turn-completed`. It is the audit log and is preserved even when its
  events are already reflected in the current snapshot.
- `snapshot`: materialized full thread state. It stores the current
  thread, turns, and items so resume does not need to replay from the
  beginning every time.

Session JSON now writes both:

```json
{
  "snapshot": { "id": "...", "turns": [...] },
  "journal": [{ "type": "turn-started", "...": "..." }]
}
```

Older `messages` and `context-items` are kept as projections for gptel
prompt reuse and migration. On load, Magent prefers replaying
`snapshot + journal`; if
those fields are absent, it migrates legacy `messages` into a thread
ledger.

Replay semantics:

1. Load `snapshot` into materialized thread state.
2. Attach all `journal` events for audit/history visibility.
3. Apply only events with `seq > snapshot.last-event-seq`.

This means `snapshot` is the fast restore point and `journal` is the
append-only record. The two are not interchangeable.

## Loop Flow

1. UI submission enters `magent-runtime-api.el`.
2. `magent-runtime-api.el` creates a queued ledger turn and records the completed
   user message item immediately.
3. When the submission actually starts, `magent-runtime-api.el` transitions the
   turn to `in-progress`.
4. `magent-agent-process` reuses that turn/user item idempotently instead
   of duplicating the user message.
5. `magent-agent-loop` consumes normalized LLM events.
6. Text and reasoning deltas update materialized in-progress items in
   the snapshot without appending one journal event per chunk. Terminal
   item events carry the final content.
7. Tool-call events are accumulated until the provider-neutral
   `tool-call-batch-end` event closes the sampling batch.
8. Tool dispatch starts `tool` items, records approval metadata when
   available, and updates those same items to `completed` or `failed`.
9. Tool output returns a continuation outcome such as `tool-output`;
   `magent-agent-process` owns the decision to rebuild the prompt from
   session history and start the next sampling request.  This keeps tool
   execution separate from turn continuation policy.
10. If a post-tool continuation completes with empty assistant text,
    `magent-agent-process` attempts one no-tool final-response request
    before completing the turn.  Sampling-limit finalization uses the same
    no-tool request shape with separate metadata.
11. Assistant completion records an assistant message item and completes
   the turn.
12. Abort, failure, and dropped queued submissions transition the turn to
   `interrupted`, `failed`, or `dropped`; in-progress items under an
   aborted turn are marked `cancelled`.
13. `magent-max-sampling-requests` is a lifecycle guard for one user
    turn.  It limits the number of model sampling requests, including
    tool-output continuations, without inspecting or deduplicating tool
    command contents.

Prompt reconstruction is ledger-driven.  When a current turn id is known,
Magent includes completed history plus that turn only, preventing later
queued user submissions from leaking into the active model request.

## UI Projection

The default UI backend is agent-shell.  `magent-ui.el` is a thin public
command router; plain prompts route to `magent-agent-shell.el`, which creates
an in-process ACP client implemented by `magent-acp.el`.  ACP session/prompt
requests submit to `magent-runtime-api.el` and stay pending until the
corresponding runtime turn completes, fails, or is cancelled.  The runtime
emits Magent-native observer events; `magent-acp.el` converts those events to
ACP `session/update` messages.

`magent-runtime-queue.el` owns queued/active turn state.  The first
implementation uses one global active turn at a time, but submissions are
tagged by runtime session id so cancellation is session-scoped: cancelling one
ACP session removes that session's queued work and aborts its active turn
without dropping other sessions' queued turns.

The legacy workspace/compose UI is isolated in `magent-ui-legacy.el`.  It
derives from `special-mode`, shows an oldest-first timeline of retained
non-dropped ledger turns, and uses an independent `magent-compose-mode` buffer
keyed by session scope.  Reasoning is stored as ledger items; the legacy
workspace shows status and character count, while transcript/detail commands
can inspect full content.  See `docs/UI_BACKENDS.md` for the current backend
boundary.

## Codex Differences Still Preserved

Magent intentionally still differs from Codex in these loop-adjacent
areas:

- UI is Emacs-native but now backendized: default interaction is agent-shell
  through an in-process ACP client, while the old workspace/compose UI is a
  legacy backend.
- Provider streaming is normalized behind `magent-llm-gptel.el`, but
  transport remains gptel.
- Codex core runs a turn as a multi-sampling loop where pending user
  input, mailbox items, auto-compaction, and tool follow-up can all
  extend one active turn. Magent keeps request serialization in the
  runtime queue and uses Codex-style continuation for tool results: tool
  execution records model-visible output, then the turn layer decides
  whether to continue sampling or ask once for a no-tool final answer when
  the post-tool response is empty.  Magent does not implement Codex's
  app-server mailbox or steering queue.
- Codex thread status is app-server-facing
  `notLoaded/idle/systemError/active{activeFlags}`. Magent keeps the
  same top-level status idea but also persists explicit local turn and
  item statuses because the Emacs session file is the durable source of
  truth.
- Codex rollout history stores response items and turn context items.
  Magent stores a ledger `snapshot + journal`; `messages` and
  `context-items` are projections used for gptel prompt reuse and
  migration.
- Codex provider/model plumbing is native to codex-core. Magent keeps
  `gptel-request` as the provider transport and normalizes gptel events
  before the Magent-owned loop consumes them.
- Tool execution is serialized by Magent's tool queue unless individual
  tool/runtime support is later added. Codex has richer per-tool runtime,
  approval, MCP, and process execution machinery; that is outside this
  workflow goal.
- Child agents are durable Magent jobs, documented in `AGENT_JOBS.md`,
  not Codex app-server threads.

## Backlog / TODO

No open TODO remains from the agent-workflow/UI refactor backlog.  Future
hardening candidates:

- Add per-tool renderer plugins for additional structured outputs beyond
  the generic file/path and grep-style link extraction.
- Add live visual smoke coverage for very large transcripts and long
  streaming sessions.

## Design Review Summary

After this change, the main agent-loop design gap versus Codex is no
longer the lack of explicit thread/turn/item lifecycle. Magent now has
that lifecycle and persists it.

The remaining differences are deliberate product/runtime boundaries:

- Magent does not have Codex's app-server lifecycle manager, subscriber
  model, unloaded thread cache, or active flags beyond the local ledger
  status.
- Magent does not have Codex's same intra-turn input queue/mailbox
  semantics.
- Magent does not use Codex rollout files directly; it keeps its own
  session JSON with `snapshot + journal`.
- Magent does not split tool call and tool output as separate source-of-
  truth records. A tool is one item whose status and output are updated.
