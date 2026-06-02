# Agent Workflow State Machine

Magent now models the agent loop as an explicit `thread -> turn -> item`
ledger. This is the source of truth for agent workflow state. Legacy
session `messages` and `context-items` remain as derived projections for
gptel prompt construction, UI restore, and older tests.

## Codex Alignment

Codex exposes a thread event stream at the SDK/app-server boundary:

- `thread.started`
- `turn.started`
- `item.started`
- `item.updated`
- `item.completed`
- `turn.completed` or `turn.failed`

Magent keeps the same workflow boundary, adapted to Emacs:

- Provider transport still goes through `gptel-request`.
- The Magent loop owns tool dispatch, continuation, abort, and Emacs UI
  rendering.
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

`magent-thread.el` defines the canonical ledger:

- `magent-thread`
  - statuses: `not-loaded`, `idle`, `active`, `system-error`, `closed`
- `magent-thread-turn`
  - statuses: `queued`, `in-progress`, `completed`, `interrupted`,
    `failed`, `dropped`
- `magent-thread-item`
  - statuses: `pending`, `in-progress`, `completed`, `failed`,
    `cancelled`

Each user prompt creates one turn. Assistant messages, reasoning blocks,
tool invocations, and tool outputs are items under that turn.

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
  as `turn-started`, `item-started`, `item-completed`, and
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

Older `messages` and `context-items` are kept as projections for display
and migration. On load, Magent prefers replaying `snapshot + journal`; if
those fields are absent, it migrates legacy `messages` into a thread
ledger.

Replay semantics:

1. Load `snapshot` into materialized thread state.
2. Attach all `journal` events for audit/history visibility.
3. Apply only events with `seq > snapshot.last-event-seq`.

This means `snapshot` is the fast restore point and `journal` is the
append-only record. The two are not interchangeable.

## Loop Flow

1. UI submission enters `magent-turn`.
2. `magent-turn` creates a ledger turn and starts it.
3. `magent-agent-process` records the user message item in that turn.
4. `magent-agent-loop` consumes normalized LLM events.
5. Tool-call events start `tool` items.
6. Tool results update the same `tool` items to `completed` or `failed`.
7. Tool output returns a continuation outcome such as `tool-output`;
   `magent-agent-process` owns the decision to rebuild the prompt from
   session history and start the next sampling request.  This keeps tool
   execution separate from turn continuation policy.
8. Assistant completion records an assistant message item and completes
   the turn.
9. Abort, failure, and dropped queued submissions transition the turn to
   `interrupted`, `failed`, or `dropped`.
10. `magent-max-sampling-requests` is a lifecycle guard for one user
    turn.  It limits the number of model sampling requests, including
    tool-output continuations, without inspecting or deduplicating tool
    command contents.

## Codex Differences Still Preserved

Magent intentionally still differs from Codex in these loop-adjacent
areas:

- UI is an Emacs org buffer, not a TUI/app-server client.
- Provider streaming is normalized behind `magent-llm-gptel.el`, but
  transport remains gptel.
- Codex core runs a turn as a multi-sampling loop where pending user
  input, mailbox items, auto-compaction, and tool follow-up can all
  extend one active turn. Magent keeps request serialization in the
  Emacs UI and uses Codex-style continuation for tool results: tool
  execution records model-visible output, then the turn layer decides
  whether to continue sampling.  Magent does not implement Codex's
  app-server mailbox or steering queue.
- Codex thread status is app-server-facing
  `notLoaded/idle/systemError/active{activeFlags}`. Magent keeps the
  same top-level status idea but also persists explicit local turn and
  item statuses because the Emacs session file is the durable source of
  truth.
- Codex rollout history stores response items and turn context items.
  Magent stores a ledger `snapshot + journal`; `messages` and
  `context-items` are projections used for gptel prompt reuse and UI
  restoration.
- Codex provider/model plumbing is native to codex-core. Magent keeps
  `gptel-request` as the provider transport and normalizes gptel events
  before the Magent-owned loop consumes them.
- Tool execution is serialized by Magent's tool queue unless individual
  tool/runtime support is later added. Codex has richer per-tool runtime,
  approval, MCP, and process execution machinery; that is outside this
  workflow goal.
- Child agents are durable Magent jobs, documented in `AGENT_JOBS.md`,
  not Codex app-server threads.

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
