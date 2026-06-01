# Codex Agent Workflow Alignment Plan

**Date:** 2026-05-30
**Status:** Implemented; documentation/migration closure complete
**Goal:** Analyze the agent workflow differences between Magent and the local Codex checkout at `~/proj/codex`, then move Magent toward Codex-like agent behavior where it makes sense inside Emacs. Do not implement Codex-style sandboxing.
**Compatibility:** Breaking changes were allowed for this plan. Magent now uses a clean Magent-owned loop and child-agent lifecycle instead of preserving the old `delegate` surface. The old `magent-fsm*` modules have been removed.

## Latest Checkpoint

2026-05-30:

- Added `magent-agent-job.el` as the durable child-agent job data layer.
- Added `agent-jobs` to `magent-session` and persisted it through session JSON.
- Added session helpers for adding, looking up, and updating child-agent jobs.
- Added focused ERT tests for job creation, lookup, status transition, and session save/load round-trip.
- Added `magent-agent-job.el` to `Makefile` compilation sources and root documentation.
- Verification blocked locally because neither `emacs` nor `emacsclient` is available in the current shell.

Superseded next step: the first child-agent tool-surface slice can replace `delegate` directly because breaking changes are allowed.

2026-05-30 follow-up:

- Reviewed current `magent-fsm.el`, `magent-fsm-tools.el`, `magent-agent.el`, `magent-tool-registry.el`, `magent-tool-orchestrator.el`, and `magent-turn.el`.
- Decision direction: stop treating gptel's FSM as the Magent agent loop. Magent should own the loop, transcript, tool dispatch, child-agent coordination, continuation policy, aborts, and UI events.
- Keep using `gptel-request` and gptel's provider/request/stream plumbing. The target is not to rewrite request transport; it is to avoid gptel's FSM/tool-loop semantics as the Magent agent loop.
- User decision: breaking changes are acceptable. Do not spend effort preserving old `delegate` behavior or old `magent-fsm.el` APIs once replacement code is ready.

2026-05-30 implementation checkpoint 2:

- Added `magent-llm.el` with provider-neutral LLM request and normalized event structs.
- Added constructors for text delta, reasoning delta/end, tool-call, completed, and error events.
- Added plist round-trip helpers for normalized events.
- Added `magent-llm.el` to `magent.el` and `Makefile`.
- Added focused ERT tests for event constructors, plist round-trip, and request validation.

Superseded next step: `magent-llm-gptel.el` now exists as the only module allowed to touch gptel callback/info/FSM details while still calling `gptel-request`.

2026-05-30 implementation checkpoint 3:

- Added `magent-llm-gptel.el`, a thin adapter that calls `gptel-request` for one sampling request.
- Added an adapter-local single-sampling gptel FSM so gptel owns provider transport/parsing but does not execute tools or continue the tool loop.
- Mapped gptel callback shapes into normalized `magent-llm-event` values: text deltas, reasoning deltas/end, tool calls, completed, and error/abort.
- Added `magent-llm-gptel.el` to `magent.el` and `Makefile`.
- Added focused ERT tests for the gptel request boundary, reasoning/error callback mapping, and tool-call normalization.

Superseded next step: `magent-agent-loop.el` now consumes normalized events, accumulates text/reasoning, records tool calls/results into session state, and replaced the old loop.

2026-05-30 implementation checkpoint 4:

- Added `magent-agent-loop.el` as the first Magent-owned loop state module.
- The loop consumes normalized `magent-llm-event` values and accumulates text, reasoning, tool-call, completion, usage, stop-reason, and error state.
- Added `magent-agent-loop-start`, which wraps a `magent-llm-request` callback so events are recorded before being forwarded to callers.
- Added `magent-agent-loop.el` to `magent.el`, `Makefile`, `AGENTS.md`, and `README.org`.
- Added focused ERT tests for event accumulation, terminal completed/error state, and sampler callback wrapping.

Superseded next step: tool-call batching, round counting, assistant/tool result recording, and agent process wiring now live on the Magent-owned loop path.

2026-05-31 implementation checkpoint 5:

- Moved tool round counting helpers into `magent-agent-loop.el`.
- Added `magent-agent-loop-tool-round-limit-message` with the existing user-facing limit wording.
- Added `magent-agent-loop-record-tool-result`, which records model-visible tool outputs through `magent-session-add-tool-message`.
- Added focused ERT tests for loop-owned tool round limits and session recording of tool results.
- Verification is still blocked in this shell because `emacs` and `emacsclient` are unavailable.

Next recommended step: add a loop-level tool dispatch function that converts pending normalized tool-call events to the existing orchestrator shape, calls `magent-tool-orchestrator`, records each tool result through `magent-agent-loop-record-tool-result`, and starts the next `gptel-request` sampling round through `magent-llm-gptel-sample`.

2026-05-31 implementation checkpoint 6:

- Added `magent-agent-loop-dispatch-tool-calls`.
- The loop now converts pending normalized tool-call events to the existing orchestrator call shape.
- Known tools are dispatched through `magent-tool-orchestrator`; each result is recorded through `magent-agent-loop-record-tool-result`.
- Unknown tools are recorded as model-visible tool-result errors instead of depending on gptel unknown-tool advice.
- Tool round limits now block dispatch at the loop layer.
- Added focused ERT tests for known tool dispatch, unknown tool error recording, and tool round limit blocking.
- Historical note: at this checkpoint the main flow was not switched yet; checkpoint 8 switches it to the Magent-owned loop.

Next recommended step: implement continuation in `magent-agent-loop.el`: after tool dispatch completes, rebuild the prompt from the session and call the sampler again. Then change `magent-agent-process` to construct a `magent-llm-request` and start the loop directly.

2026-05-31 implementation checkpoint 7:

- Added loop continuation helpers: `magent-agent-loop-request-for-current-session` and `magent-agent-loop-continue`.
- Continuation rebuilds the prompt from `magent-session-to-gptel-prompt-list`, preserving the request's system prompt, tools, backend, model, stream setting, callback, and metadata.
- Tool-call dispatch now converts provider plist args to positional arg values for execution, then converts positional arg values back to plist args for model-visible session history.
- Added focused ERT test for continuation prompt rebuilding after a tool result.
- Historical note: at this checkpoint the main flow was still not switched; checkpoint 8 switches it to the Magent-owned loop.

Next recommended step: replace `magent-agent-process`'s FSM construction with `magent-llm-request` + `magent-agent-loop-start`, and wire loop tool completion to `magent-agent-loop-continue`.

2026-05-31 implementation checkpoint 8:

- Switched `magent-agent-process` from `magent-fsm-create`/`magent-fsm-start` to `magent-llm-request` + `magent-agent-loop-start`.
- `magent-agent-process` now converts runtime tool plists to gptel-tool schema objects for provider exposure, while Magent still owns execution through `magent-tool-orchestrator`.
- Loop event handling now owns text streaming, reasoning UI events, final assistant session recording, request lifecycle events, tool dispatch, and continuation.
- `magent-llm-gptel.el` marks the final tool-call event in a batch with `:last t`, allowing the loop to dispatch all pending tool calls together.
- Updated tests that previously asserted FSM construction to assert loop construction instead.
- Updated root docs to state that the Magent-owned loop is now active.
- Verification remains blocked in this shell because `emacs` and `emacsclient` are unavailable.

Superseded next step: repeated `emacs_eval` guard, tool UI rendering, queue behavior, and abort cleanup now live in `magent-agent-loop.el`.

2026-05-31 implementation checkpoint 9:

- Adjusted `magent-agent-process` request lifecycle events so each sampling round emits `llm-request-start` before calling the sampler and emits `llm-request-end` from final/error handling.
- Tool completion now rebuilds the loop request from the current session and uses the same local sample helper, so continuation request events happen in the correct order.
- The active main path remains `magent-agent-process` -> `magent-agent-loop-start` -> `magent-llm-gptel-sample`.

Superseded next step: repeated `emacs_eval` guard and visible tool rendering have moved into the loop/tool runner path.

2026-05-31 implementation checkpoint 10:

- Migrated active-path tool runner behavior from `magent-fsm-tools.el` into `magent-agent-loop.el`.
- Added loop-owned abort controller and serial tool queue.
- `magent-agent-loop-run-tool` now emits `tool-call-start` / `tool-call-end`, renders visible tool calls/results when `magent-request-context` allows full UI, suppresses direct UI for `summary-only` child contexts, strips display-only `reason` args before execution, tracks large raw tool results, and intercepts duplicate or excessive `emacs_eval` calls.
- `magent-agent-loop-create-orchestrator` now wires permission audit callbacks through the active loop path.
- `magent-agent-loop-start` stores the sampler request handle, installs the loop abort controller on the request context, and drops callbacks after loop abort.
- Added `magent-agent-loop-abort`; UI interrupt and delegate cleanup now abort a Magent-owned loop directly.
- Added focused ERT coverage for new loop guards, tool UI visibility, tool events, serial async queueing, abort cleanup/late callback dropping, request-context abort-controller installation, and UI interrupt against a loop.
- Updated `AGENTS.md`, `README.org`, `docs/ONBOARDING.md`, and `docs/CONTRIBUTING.md` so the active architecture points at `magent-agent-loop.el` instead of saying the FSM is active.
- Verification performed: `git diff --check` passed. ERT/byte-compile still could not run because `emacs` and `emacsclient` are not available in this shell or common macOS install paths checked.

Next recommended step: once an Emacs binary is available, run focused checks first:

```bash
emacs -Q --batch -L . -L /Users/jamiecui/.emacs.d/elpa/gptel-20260503.538 -l ert -l test/magent-test.el --eval '(ert-run-tests-batch "magent-test-agent-loop-")'
emacs -Q --batch -L . -L /Users/jamiecui/.emacs.d/elpa/gptel-20260503.538 -l ert -l test/magent-test.el --eval '(ert-run-tests-batch "magent-test-\\(simple-prompt\\|session-recording\\|agent-process\\|ui-interrupt\\)")'
make compile
```

Superseded next step: the old `magent-fsm*` files have been deleted. Continue by replacing `delegate` with Codex-like child-agent lifecycle tools using `magent-agent-job.el`.

2026-05-31 implementation checkpoint 11:

- Deleted `magent-fsm.el`, `magent-fsm-tools.el`, `magent-fsm-backend-gptel.el`, and `magent-fsm-shared.el`.
- Removed runtime/config/build/live-test references to the deleted FSM modules, including `magent-fsm-backend`.
- Migrated remaining permission prompt and audit tests from FSM helpers to `magent-tool-orchestrator` plus `magent-agent-loop` audit hooks.
- Removed old tests that only asserted deleted FSM internals, gptel backend advice, legacy queue wrappers, and compatibility feature shims; equivalent active-path coverage now lives under `magent-test-agent-loop-*`, permission prompt, audit, and UI interrupt tests.
- Changed `magent-llm-gptel-sample` to return the request buffer as the abort handle so the main loop no longer needs to inspect gptel FSM handles.
- Updated `AGENTS.md`, `README.org`, `docs/ONBOARDING.md`, and `docs/CONTRIBUTING.md` to state that the `magent-fsm*` modules are removed and the active loop is `magent-agent-loop.el`.
- Removed stale references after the user deleted the old performance-measurement directory.
- Verification performed: deleted FSM module names and old performance-measurement references are gone from runtime code, tests, Makefile, README, AGENTS, and stable docs; `git diff --check` passed.
- Verification still blocked: ERT and byte-compile could not run because `emacs` and `emacsclient` are not available in the current shell.

Next recommended step: implement the child-agent tool surface on top of `magent-agent-job.el`: `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, and `close_agent`. Replace `delegate` directly after the new tools have focused tests.

2026-06-01 implementation checkpoint 12:

- Replaced the public `delegate` tool surface with durable child-agent lifecycle tools: `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, and `close_agent`.
- Added runtime child-job coordination state in `magent-tools.el` while keeping durable job metadata in the parent session's `agent-jobs` slot.
- `spawn_agent` now creates a persistent `magent-agent-job`, starts the child through the Magent-owned loop with `summary-only` UI, records result/error/transcript state, and returns model-visible JSON with the stable job id.
- `send_agent_message`, `wait_agent`, `list_agents`, and `close_agent` operate on existing job ids and return structured model-visible JSON.
- Moved the lifecycle tools under the shared `agent` permission key and updated defaults, built-in general-agent recursion denial, audit redaction, tool-call summaries, tests, and docs.
- Fixed a pre-existing missing close paren in `magent-agent.el` that blocked `load`, byte compilation, and batch ERT.
- Verification performed: focused ERT for the child-agent tool surface passed 4/4; byte-compilation of `magent-agent.el`, `magent-tools.el`, `magent-agent-loop.el`, `magent-permission.el`, `magent-agent-registry.el`, `magent-audit.el`, and `magent-config.el` passed; `git diff --check HEAD` passed. Emacs 31 emitted existing obsolete `when-let`/`if-let` warnings and the existing `magent-tools--bash` free-variable warning.

Next recommended step: define and test deeper child config inheritance rules: project root, model/temperature/top-p, capability context, permission profile, max-depth/recursive spawn guards, and separation from the parent visible buffer.

2026-06-01 implementation checkpoint 13:

- Added request-context fields for child-agent inheritance: agent depth, project root, backend/model, temperature/top-p, skill names, capability context, and effective permission profile.
- `magent-agent-process` now records the effective runtime sampling/context settings on the active request context and passes temperature/top-p through normalized request metadata.
- `magent-llm-gptel-sample` applies request metadata for `gptel-temperature` in the request buffer, keeping inherited sampling settings across child requests and continuations.
- Child-agent jobs now persist inheritance metadata, including project root, model/backend names, sampling values, active skill names, UI visibility, depth, and a compact permission profile.
- Child tools now resolve relative file paths and bash working directories from the inherited request project root.
- Added `magent-child-agent-max-depth`, defaulting to 1, so root agents can spawn direct children while recursive child spawning is blocked by default.
- Child agents now receive an effective permission profile bounded by both the parent request permissions and the child agent's own permissions.
- Verification performed: focused ERT for runtime inheritance, gptel temperature metadata, child-agent spawn/depth, and inherited project-root file resolution passed 5/5; byte-compilation of `magent-runtime.el`, `magent-config.el`, `magent-agent.el`, `magent-tools.el`, and `magent-llm-gptel.el` passed. Emacs 31 emitted existing obsolete `when-let`/`if-let` warnings and the existing `magent-tools--bash` free-variable warning.
- Full `make test` currently reaches 248/249 passing and fails on the pre-existing `magent-test-skills-load-all-includes-systematic-debugging` check because `skills/systematic-debugging/SKILL.md` was removed in `b8eee97` while tests/capabilities still reference that skill.

Next recommended step: render child-agent lifecycle activity in the main Magent buffer as compact event blocks and add an inspection command for child transcripts.

2026-06-01 implementation checkpoint 14:

- Added compact child-agent lifecycle UI blocks rendered as `#+begin_agent` / `#+end_agent` entries in the parent Magent buffer.
- Child job lifecycle rendering is scoped to the parent session buffer and deferred for parent tool calls so it does not nest inside the visible `#+begin_tool` block.
- Added `magent-show-agent-transcript` for inspecting persisted child-agent prompt, metadata, result/error, and transcript state.
- Added `C-c m j` and transient menu access for child-agent transcript inspection.
- Child-agent job status changes now snapshot and save the parent session when there is parent conversation history, preserving active/completed job state for resume.
- Added focused ERT coverage for compact lifecycle UI rendering, transcript inspection, child-agent completion persistence, resume restoration of `agent-jobs`, and the new keybinding.
- Verification performed: focused Task 6 ERT passed 14/14; byte-compilation of `magent-ui.el`, `magent-tools.el`, `magent.el`, and `test/magent-test.el` passed; `git diff --check` passed. Emacs 31 emitted existing obsolete `when-let`/`if-let` warnings, existing test warning noise, and the existing `magent-tools--bash` free-variable warning.

Next recommended step: complete Task 7 documentation and migration notes for the final child-agent architecture and `delegate` replacement.

2026-06-01 implementation checkpoint 15:

- Added `docs/AGENT_JOBS.md` as the stable child-agent job architecture reference.
- Updated `README.org`, `AGENTS.md`, `docs/README.md`, `docs/ONBOARDING.md`, `docs/CONTRIBUTING.md`, and `docs/TROUBLESHOOTING.md` to describe the implemented child-agent lifecycle.
- Documented the public replacement for `delegate`: `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, and `close_agent`.
- Updated command documentation for `magent-resume-session` (`C-c m R`) and `magent-show-agent-transcript` (`C-c m j`).
- Marked Task 7 complete.

Next recommended step: treat `docs/AGENT_JOBS.md` as the stable reference for future child-agent lifecycle changes; keep this plan as implementation history.

## Scope

This plan is about agent workflow, not provider setup or UI polish in isolation.

In scope:

- Compare Magent's former `delegate`-based subagent flow with Codex's collaborative agent lifecycle.
- Design and implement a persistent subagent/job model for Magent that can spawn, message, wait for, list, resume/inspect, and close child agents.
- Keep Magent's Emacs-native strengths: live buffers, `emacs_eval`, org output, project-scoped sessions, and gptel transport.
- Preserve existing permission prompts and audit logging while improving the workflow model.
- Replace Magent's dependency on gptel's request FSM with a Magent-owned agent loop while continuing to use `gptel-request` for provider requests.
- Document each implementation step so interrupted work can resume from git.

Out of scope:

- Codex sandbox, seatbelt, bubblewrap, or shell isolation parity.
- Replacing `gptel-request`, gptel provider configuration, or gptel's HTTP/SSE request plumbing.
- Reusing gptel's tool FSM semantics as the long-term Magent loop.
- Preserving old `delegate` or `magent-fsm.el` compatibility after the replacement loop is ready.
- Rewriting the whole UI before the agent lifecycle is understood.
- Feature-for-feature parity with Codex when the feature is terminal-specific.

## Current Magent Workflow

Magent has a multi-agent registry, per-agent permissions, a Magent-owned tool loop, and durable child-agent jobs:

- `build`, `plan`, `explore`, `general`, `compaction`, `title`, and `summary` are registered agent profiles.
- The primary session owns the visible `*magent*` buffer and message history.
- Tool calls run through `magent-agent-loop.el` and `magent-tool-orchestrator.el`.
- The public child-agent tool surface is `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, and `close_agent`.
- Each child job has a stable id, status, prompt, metadata, result/error state, and transcript stored in the parent session's `agent-jobs` slot.
- The parent `*magent*` buffer renders compact `#+begin_agent` lifecycle blocks; full child job details are inspected with `magent-show-agent-transcript` (`C-c m j`).
- `magent-resume-session` restores persisted child job metadata with the parent session.

## Agent Loop Inventory

Current call chain after checkpoint 15:

1. `magent-agent-process` builds session prompt, system prompt, skill/capability prompt, tool list, and per-agent gptel overrides.
2. `magent-agent-process` constructs a `magent-llm-request` and starts `magent-agent-loop-start`.
3. `magent-agent-loop.el` owns turn state, streaming/reasoning accumulation, tool-call batching, tool round limits, repeated `emacs_eval` guards, tool-result recording, continuation, and abort cleanup.
4. `magent-llm-gptel.el` calls `gptel-request` for one sampling request and maps gptel callback/info shapes into normalized `magent-llm-event` values.
5. `magent-tool-orchestrator.el` handles allow/deny/ask permissions, approval prompts, session overrides, file rules, and audit hooks.
6. Tool results are appended to `magent-session` as model-visible tool messages before `magent-agent-loop-continue` starts the next sampling round.

Current `magent-turn.el` responsibilities:

- A small submission queue around the active request.
- Tracks active submission and current request handle for interrupt. Some internal names still say `fsm`; those are naming debt, not a dependency on deleted `magent-fsm*` modules.
- Does not currently own the model/tool loop.

Implication: Magent keeps the `gptel-request` boundary, but the main loop no longer depends on the deleted `magent-fsm*` modules. Any unavoidable gptel FSM usage must stay inside `magent-llm-gptel.el` as provider adapter detail.

## Codex Workflow Notes

The local Codex checkout exists at `/Users/jamiecui/proj/codex`.

Relevant files inspected on 2026-05-30:

- `/Users/jamiecui/proj/codex/codex-rs/core/src/tools/handlers/multi_agents.rs`
- `/Users/jamiecui/proj/codex/codex-rs/core/src/tools/handlers/multi_agents_v2.rs`
- `/Users/jamiecui/proj/codex/codex-rs/core/src/tools/handlers/multi_agents_common.rs`
- `/Users/jamiecui/proj/codex/codex-rs/core/src/session/multi_agents.rs`
- `/Users/jamiecui/proj/codex/codex-rs/core/src/agent/agent_resolver.rs`
- `/Users/jamiecui/proj/codex/codex-rs/core/hierarchical_agents_message.md`
- `/Users/jamiecui/proj/codex/docs/agents_md.md`

Observed workflow traits to consider:

- Codex exposes a collaboration tool surface rather than a single delegate call.
- V1-style tools include spawn, send input, wait, resume, and close.
- V2-style tools add list agents, assign task, send message, wait, and close.
- Child agents have stable thread identity and can also be referenced through an agent path or task-oriented name.
- A spawned child derives from the parent's effective turn config, including live runtime choices such as model/provider, reasoning settings, approval policy, cwd, and developer instructions.
- Full-history forked agents intentionally inherit some parent choices instead of accepting independent model/reasoning overrides.
- The model-visible workflow includes guidance for when the root agent should use subagents and how subagents report back.
- Codex has explicit status/wait behavior so the root agent can coordinate multiple child agents without blocking all reasoning on one synchronous nested call.

Additional Codex loop traits relevant to replacing gptel FSM:

- Codex owns turn state separately from provider transport.
- Provider responses are normalized into model-visible items/events.
- Tool calls are dispatched by Codex's tool router/orchestrator, not by the provider client.
- Tool outputs are appended to the transcript as first-class response input items before the next sampling request.
- Child-agent work is part of the same orchestration layer as ordinary tools.
- Abort, timeout, status, and wait behavior are turn-owned state transitions.

## Implemented Target Behavior For Magent

Magent moved from one-shot delegation to a small collaborative agent lifecycle:

- A primary agent can spawn one or more child agents with a task name, role, prompt, and inherited context.
- Each child has a stable id, status, transcript, parent session id, optional task name, and agent profile.
- The primary agent can send follow-up messages to a child.
- The primary agent can wait for one or more children and receive structured status/result summaries.
- The primary agent can list active children for the current session.
- The primary agent can close a child and record the final result.
- Child agents inherit the parent's live Magent context where practical: project root, current session, model/backend, temperature/top-p, capability/skill activation context, request depth, and effective permission profile.
- The UI makes child activity visible without forcing every child transcript into the main conversation body.

## Comparison Table

| Concept | Codex observed shape | Previous Magent shape | Implemented Magent shape |
| --- | --- | --- | --- |
| Root interaction | Session turn drives tool calls and can coordinate child agents | `*magent*` session owned visible turn and FSM | Visible Emacs session remains root coordinator |
| Child identity | Stable thread id plus path/task-oriented references | Old subagent surface had no durable child id | Stable job id plus optional task name and parent session key |
| Spawn | Collaboration tool starts a child from live turn config | `delegate` started a nested one-shot request | `spawn_agent` creates durable job and starts child request |
| Message follow-up | Send input/message to an existing child | Not available after `delegate` returned | `send_agent_message` appends input to a child job |
| Wait/status | Wait tools return structured status/result data | Parent waited synchronously for nested delegate result | `wait_agent` can poll one or more jobs with timeout |
| List | V2 tool can list collaborative agents | `magent-list-agents` lists agent definitions, not running children | `list_agents` reports active child jobs for current session |
| Close | Explicit close operation for child agent | Not available | `close_agent` marks child closed and records final state |
| Inheritance | Child derives effective live turn config | Agent profile overrides were applied per request | Child inherits project/session/model/capability/permission context where practical |
| Persistence | Threads/jobs have durable metadata in Codex state | Magent persisted parent session messages and buffer content | Child job metadata and inspectable transcript/result state persist in parent session |
| Sandbox | Codex carries approval/sandbox runtime state | Magent has permission prompts and audit logs, not sandboxing | Preserve permissions/audit; do not copy sandbox behavior |

## Implemented Architecture Direction

Magent introduced a Magent-side agent job layer instead of expanding `delegate`.

Implemented modules:

- `magent-agent-job.el`: data structures, registry, status transitions, parent/child relationship, result storage.
- A focused section in `magent-tools.el`: tool wrappers for `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, and `close_agent`.
- `magent-session.el`: persist child job metadata and enough transcript state to resume after Emacs restart.
- `magent-ui.el`: render compact child-agent events and provide inspection commands.
- `magent-agent.el`: derive child prompt/config from parent session and selected child agent profile.
- `magent-agent-registry.el`: existing `subagent` mode remains the profile boundary for child agents.

Because breaking changes were allowed, `delegate` was replaced directly and does not have a long-term compatibility wrapper.

## gptel-request / Loop Decoupling Direction

The completed loop replacement split the old FSM responsibilities into two conceptual layers:

1. Magent-owned agent loop.
2. gptel-request sampling adapter.

The agent loop should be independent of gptel FSM internals. It owns:

- turn state and lifecycle;
- transcript/context item list;
- model-visible tool call/output items;
- text and reasoning accumulation;
- tool dispatch, permission, audit, and result recording;
- child-agent job coordination;
- continuation after tool results;
- max tool rounds and repeated-tool guards;
- abort/cancel state;
- request/session events;
- UI callbacks.

The gptel sampling adapter should own only:

- reading selected `gptel-backend`, `gptel-model`, and provider variables;
- calling `gptel-request` for a single model sampling request;
- streaming normalized deltas back to Magent;
- returning normalized final/error metadata from gptel callback/info.

Avoid long-term dependencies on:

- `gptel-make-fsm`;
- `gptel-fsm-info`;
- `gptel--handle-wait`;
- `gptel--handle-post`;
- `gptel--handle-tool-use`;
- `gptel--fsm-transition`;
- global advice around gptel private FSM functions.

Temporary gptel private dependencies are acceptable only behind `magent-llm-gptel.el`. The request boundary should remain `gptel-request`.

### Proposed New Modules

- `magent-agent-loop.el`: Magent-owned turn loop. Drives model sampling, handles stream events, dispatches tools, appends tool outputs, repeats until final answer/error/abort.
- `magent-llm.el`: provider-neutral request/response protocol for Magent. Defines request plist/struct and normalized events such as `:text-delta`, `:reasoning-delta`, `:tool-call`, `:completed`, `:error`, `:usage`.
- `magent-llm-gptel.el`: adapter that calls `gptel-request` for one sampling request and maps gptel callback/info data back into `magent-llm` events.
- `magent-transcript.el` or extension of `magent-session.el`: owns model-visible context item conversion, replacing gptel prompt-list as the primary transcript shape.

### Normalized Loop Event Shape

Start with a small internal event protocol:

- `(:type text-delta :text TEXT)`
- `(:type reasoning-delta :text TEXT)`
- `(:type reasoning-end)`
- `(:type tool-call :id ID :name NAME :arguments PLIST-OR-ALIST :raw RAW)`
- `(:type completed :text TEXT :usage USAGE :stop-reason STOP)`
- `(:type error :message MESSAGE :metadata META)`

The loop should not receive raw gptel `info` except inside `magent-llm-gptel.el`.

### Migration Strategy

Do not rewrite everything in one patch, but do not design for old-surface compatibility. Use a direct replacement path:

1. Add `magent-llm.el` event/request definitions and tests.
2. Add `magent-llm-gptel.el` adapter that calls `gptel-request`, but hides all gptel callback/info details behind normalized events.
3. Add `magent-agent-loop.el` that consumes normalized events and reuses existing `magent-tool-orchestrator` for permissions and tool execution.
4. Move text/reasoning accumulation, tool round guard, tool result recording, continuation policy, and abort state into `magent-agent-loop.el`.
5. Change `magent-agent-process` to call `magent-agent-loop-start`.
6. Delete the old `magent-fsm*` modules.
7. Keep gptel private FSM usage restricted to `magent-llm-gptel.el`; remove or narrow it later if gptel exposes a cleaner one-shot sampling boundary.

### Resolved Loop Boundary Decisions

- `magent-llm-gptel.el` uses a tiny local gptel FSM only inside the adapter to obtain one sampling response without letting gptel execute tools or continue the loop.
- `magent-llm-gptel-sample` returns the request buffer as the abort handle, so `magent-agent-loop.el` does not inspect gptel FSM handles.
- Magent still exposes tools to gptel as `gptel-tool` structs for provider schema compatibility; native tool metadata can be introduced later at the provider boundary if useful.
- Transcript persistence can move toward Codex-like response items after the child-agent tool surface lands; the loop replacement does not require that migration first.
- Provider-specific reasoning/tool-call quirks are represented as normalized `magent-llm-event` metadata and raw tool-call payloads inside the adapter boundary.

## Implementation Tasks

### Task 1: Baseline Comparison

- [x] Read the current Magent `delegate` implementation and FSM tool queue in detail.
- [x] Read Codex spawn/send/wait/list/close handlers enough to map request and result shapes.
- [x] Produce a concise comparison table in this plan: concept, Codex implementation, current Magent equivalent, target Magent behavior.
- [x] Identify which Codex concepts should be ignored because they are sandbox-specific or terminal-specific.

### Task 2: Data Model

- [x] Define a `magent-agent-job` struct with id, parent session key, agent name, task name, status, prompt, created/updated time, transcript, result, and error fields.
- [x] Add deterministic status values such as `queued`, `running`, `waiting`, `completed`, `failed`, `closed`, and `cancelled`.
- [x] Decide how much child transcript belongs in `magent-session` persistence versus separate JSON files.
- [x] Add focused ERT tests for job creation, lookup, status transitions, and persistence shape.

Decision: store compact child-agent job metadata and transcript/result state in the parent session JSON under `agent-jobs` for now. Split child transcripts into separate files later only if size or UI inspection requirements make the parent session too heavy.

### Task 3: Tool Surface

- [x] Add `spawn_agent` for starting a durable child job.
- [x] Add `send_agent_message` for follow-up input.
- [x] Add `wait_agent` for polling one or more child jobs with timeout.
- [x] Add `list_agents` for model-visible coordination state.
- [x] Add `close_agent` for explicit cleanup.
- [x] Replace `delegate` with the new tool surface once direct tools pass tests; no long-term compatibility wrapper is required.

### Task 4: Keep gptel-request, Replace gptel FSM Loop Semantics

- [x] Add `magent-llm.el` with provider-neutral request and normalized event definitions.
- [x] Add tests for normalized text, reasoning, tool-call, completed, and error event handling.
- [x] Add `magent-llm-gptel.el` adapter that calls `gptel-request` and hides gptel callback/info details behind normalized events.
- [x] Add `magent-agent-loop.el` with loop state, sampler callback wrapping, and normalized event accumulation.
- [x] Move max tool round state and tool round limit messaging out of `magent-fsm.el`.
- [x] Move tool result session recording out of `magent-fsm.el`.
- [x] Move repeated `emacs_eval` guard state into `magent-agent-loop.el`.
- [x] Move continuation prompt rebuilding out of `magent-fsm.el`.
- [x] Wire continuation policy to run automatically after loop-level tool dispatch.
- [x] Dispatch tools from `magent-agent-loop.el` through `magent-tool-orchestrator`.
- [x] Change `magent-agent-process` to start the new loop directly once the adapter and loop pass focused tests.
- [x] Remove direct dependency on `gptel--handle-*` and gptel FSM advice from the main loop. Any unavoidable gptel FSM usage stays inside `magent-llm-gptel.el`.
- [x] Delete the old `magent-fsm*` modules.

### Task 5: Runtime And Prompt Inheritance

- [x] Define child config inheritance rules for Magent: project root, gptel model override, temperature/top-p, active capability context, permission profile, and current session metadata.
- [x] Ensure child agents do not mutate the parent visible buffer directly.
- [x] Add safeguards for max depth and recursive spawn loops.
- [x] Add tests that verify child agents inherit expected settings without sharing mutable loop state.

### Task 6: UI And Resume

- [x] Render child-agent lifecycle events in the main `*magent*` buffer as compact tool/event blocks.
- [x] Add an inspection command for child transcripts.
- [x] Persist active/completed child jobs with the parent session.
- [x] Verify `magent-resume-session` restores enough job state to continue or inspect interrupted work.

### Task 7: Documentation And Migration

- [x] Update `README.org` tool and architecture sections when the new tools exist.
- [x] Update the current docs index and any restored API/design reference docs after public commands or functions are introduced.
- [x] Document the final agent-job architecture in the active docs set before closing this plan.
- [x] Document the `delegate` removal/replacement direction once the new tools supersede it.
- [x] Document the final `gptel-request` sampling boundary after deleting the old FSM modules.

## Acceptance Criteria

- Magent has a documented, test-covered child-agent lifecycle instead of only one-shot delegation.
- Magent owns the agent loop and no longer relies on gptel's FSM/tool-loop internals for turn continuation.
- The root agent can coordinate at least two child jobs and wait for their results.
- Existing one-shot `delegate` behavior can break once replacement tools and docs are in place.
- Session persistence is sufficient to recover progress after changing machines and syncing git state plus session files where applicable.
- No Codex sandbox behavior is copied into Magent.

## Verification Commands

Use these after implementation tasks, not necessarily during this planning-only cleanup:

```bash
make compile
make test
emacsclient --eval '(load "/Users/jamiecui/proj/magent/magent-agent-job.el" nil t)'
emacsclient --eval '(magent-clear-session)'
```

Current verification status:

- Static checks passed in the current shell: `git diff --check HEAD`.
- Documentation closure static checks passed: stale public docs were scanned for old one-shot `delegate`/10-tool migration wording, and `git diff --check` passed.
- Focused ERT passed for the child-agent tool surface:

```bash
emacs -Q --batch -L . -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'gptel-[0-9]*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'spinner-*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'transient-*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'cond-let-*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'evil-*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'yaml-[0-9]*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'llama-*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'with-editor-*' -type d | head -1) -l ert -l test/magent-test.el --eval '(ert-run-tests-batch "magent-test-tools-\\(permission-key\\|spawn-agent\\|list-wait\\|all-registered\\)")'
```

- Focused byte-compilation passed for the changed runtime files:

```bash
emacs -Q --batch -L . -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'gptel-[0-9]*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'spinner-*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'transient-*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'cond-let-*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'evil-*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'yaml-[0-9]*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'llama-*' -type d | head -1) -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'with-editor-*' -type d | head -1) --eval '(setq byte-compile-error-on-warn nil)' --eval '(setq byte-compile-warnings '\''(not cl-functions obsolete))' -f batch-byte-compile magent-agent.el magent-tools.el magent-agent-loop.el magent-permission.el magent-agent-registry.el magent-audit.el magent-config.el
```

- Existing warnings remain on Emacs 31: obsolete `when-let`/`if-let` warnings and `magent-tools--bash`'s `--cl-block-magent-tools--bash--` free-variable warning.
- Broader regression commands still useful before closing the plan:

```bash
emacs -Q --batch -L . -l ert -l test/magent-test.el --eval '(ert-run-tests-batch "magent-test-\\(agent-job\\|session-agent-job\\|session-save-load-preserves-agent-jobs\\)")'
emacs -Q --batch -L . -l ert -l test/magent-test.el --eval '(ert-run-tests-batch "magent-test-llm-")'
emacs -Q --batch -L . -l ert -l test/magent-test.el --eval '(ert-run-tests-batch "magent-test-llm-gptel-")'
emacs -Q --batch -L . -l ert -l test/magent-test.el --eval '(ert-run-tests-batch "magent-test-agent-loop-")'
emacs -Q --batch -L . -l ert -l test/magent-test.el --eval '(ert-run-tests-batch "magent-test-\\(simple-prompt\\|session-recording\\|agent-process\\)")'
```

Live prompts to verify later:

- `你好`
- `帮我看下 emacs 里面有多少 buffer`
- `把这个任务拆给两个子 agent 分别分析，然后汇总结果`
