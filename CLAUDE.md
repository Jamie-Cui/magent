
This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Magent is an Emacs Lisp AI coding agent (~31 `magent-*.el` files) with a multi-agent
architecture, permission-based tool access, and a durable child-agent/job lifecycle.
All LLM communication is delegated to **gptel** — the only external runtime dependency
beyond Emacs 29.1+, `transient`, `compat`, `yaml`, `acp`, and `agent-shell`. Do not rewrite gptel
provider/HTTP/SSE integration; the Magent loop consumes only normalized events.

## Build, test, lint

```bash
make compile          # Byte-compile all Elisp files (this is the lint gate — must be warning-clean)
make test-unit        # Run the ERT unit suite in batch mode
make test             # Unit tests + deterministic live smoke tests (stubbed gptel transport)
make coverage         # Run ERT under testcover → coverage/testcover-summary.tsv
make clean            # Remove compiled .elc files
```

The Makefile auto-detects dependency paths by scanning `~/.emacs.d/elpa/`. Override with
e.g. `GPTEL_DIR=/path/to/gptel`. `.elc` files are gitignored; run `make clean` before
live/batch verification if any may be stale.

Run a **single test** by regexp:

```bash
emacs -Q --batch -L . -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'gptel-*' -type d | head -1) \
  -l ert -l test/magent-test.el --eval '(ert-run-tests-batch "test-name-regexp")'
```

`make test-live` / `test-live-smoke` need an Emacs server (`EMACSCLIENT`); `test-live`
uses the real provider and consumes tokens. For real provider debugging use an isolated
daemon (`emacs --daemon=magent-live-test`) — never risk the user's main Emacs. See
`docs/TROUBLESHOOTING.md#live-emacs-tests-fail-or-hang`.

## Architecture (big picture)

Data and control flow through five layers. Understanding the boundaries between them
matters more than any single file:

1. **Entry + runtime** (`magent.el`, `magent-runtime.el`): `magent-mode` (`C-c m` prefix)
   only adds a modeline construct on enable. All static init (agents, skills, capabilities)
   is **lazy** via `magent--ensure-initialized`. Runtime owns the ordered init pipeline and
   activates/unloads project-local `.magent/` overlays as session scope changes.

2. **Ledger** (`magent-thread.el`, `magent-session.el`): The canonical workflow state is an
   explicit **thread → turn → item** state machine, NOT the message list. Session JSON persists
   an append-only `journal` (audit log) plus a materialized `snapshot`; restore replays only
   events after `snapshot.last-event-seq`. Legacy `messages`/`context-items`/`buffer-content`
   are derived projections kept for gptel prompt reuse — the UI renders from the ledger.

3. **Agent processing + loop** (`magent-agent.el` → `magent-agent-loop.el`, with
   `magent-llm.el` / `magent-llm-gptel.el`): `magent-agent-process` builds the gptel prompt,
   applies per-agent overrides (model/temperature via `default-value`, intentionally avoiding
   buffer-local gptel state), exposes permission-filtered tools, then starts the Magent-owned
   loop. The loop owns tool dispatch (through `magent-tool-orchestrator`), serial tool queueing,
   permission/audit hooks, visible tool rendering, abort cleanup, and continuation policy.
   `magent-llm-gptel.el` may touch gptel's private FSM internally for one sampling request, but
   nothing above it sees gptel details.

4. **Tools + permissions + capabilities** (`magent-tools.el`, `magent-permission.el`,
   `magent-capability.el`): 14 `gptel-tool` structs registered globally, filtered per-agent.
   Permission resolution: exact tool match → file-pattern rules (glob, first-match-wins) →
   wildcard `*` → **default allow**. Capabilities score request context and attach matching
   instruction skills.

5. **UI** (`magent-ui.el`): The `*magent*` workspace derives from `special-mode`
   (`magent-output-mode`) and is read-only + ledger-rendered. Prompt editing lives in a separate
   `magent-compose-mode` buffer (`C-c C-c` submits). Tool calls, reasoning, and child-agent
   events render as compact rows — do not reintroduce org folding or markdown-to-org in the live path.

**Agents** (`magent-agent-registry.el`): 7 built-ins — `build` (default), `plan`, `explore`,
`general`, `compaction`, `title`, `summary` — via `cl-defstruct magent-agent-info`. Modes:
`primary` (user-facing), `subagent` (internal), `all`. Custom agents load from `.magent/agent/*.md`
(YAML frontmatter + markdown body).

**Child-agent jobs** (`magent-agent-job.el`): Primary agents coordinate durable child jobs via
`spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, `close_agent` (these replaced the
old `delegate` tool — do not reintroduce a wrapper). Stored under session `agent-jobs`. See
`docs/AGENT_JOBS.md`.

## Conventions and gotchas

- Every source file starts with `;;; -*- lexical-binding: t; -*-` and carries an SPDX license header.
- Tool pattern: `magent-tools--<name>` (internal fn) + `magent-tools--<name>-tool` (gptel-tool var).
- Byte-compile warnings suppressed only for `cl-functions` and `obsolete`; everything else must be clean.
- `magent-log` is a **stub** in `magent-config.el` until `magent-ui` loads — batch-test logs go nowhere unless you load `magent-ui`.
- Keep Evil-specific code in `magent-evil.el` (optional, not loaded by default); no `evil-*` calls in core UI.
- Package metadata stays centralized in `magent.el` / `magent-pkg.el`; non-main modules must NOT carry `Package-Requires` headers (package-lint treats them as ineffective).
- **Out of scope:** Codex sandbox/seatbelt/bubblewrap/shell isolation. Preserve the Emacs-native workflow (live buffers, `emacs_eval`, compose/workspace buffers, project-scoped sessions, gptel transport).

## Deeper reference

`AGENTS.md` is the authoritative, detailed agent guide (full module dependency graph, testing
patterns, keybindings). Also see `README.org`, `docs/AGENT_JOBS.md`, `docs/AGENT_WORKFLOW.md`,
and `docs/TROUBLESHOOTING.md`.

@AGENTS.md
