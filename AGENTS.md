# AGENTS.md

Guidance for agentic coding tools working with Magent.

## Boundaries

- **Do not** implement Codex sandbox/seatbelt/bubblewrap/shell isolation. Preserve Emacs-native workflow (live buffers, `emacs_eval`, agent-shell/legacy UI, project-scoped sessions, gptel transport).
- **Keep** `gptel-request` for provider/HTTP/SSE plumbing. Do not rewrite gptel integration.
- **Child-agent tools**: `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, `close_agent` replaced the old `delegate` tool. Do not reintroduce a compatibility wrapper unless explicitly requested.
- **Interrupted work**: Update relevant docs or task notes before stopping so progress is recoverable from git.

See `docs/AGENT_JOBS.md` and `docs/AGENT_WORKFLOW.md` for child-agent lifecycle details.

## Build Commands

```bash
make compile    # Byte-compile all Elisp files
make test-unit  # Run ERT unit tests in batch mode
make test       # Run unit tests plus deterministic live smoke tests
make coverage   # Run ERT under testcover and write coverage/testcover-summary.tsv
make clean      # Remove compiled .elc files
```

The Makefile auto-detects dependency paths (`gptel`, `spinner`, `transient`, `cond-let`, `compat`, `evil`, `yaml`, `llama`, `with-editor`) by scanning `~/.emacs.d/elpa/`. Override any with e.g. `GPTEL_DIR=/path/to/gptel`.

Single-file compilation:
```bash
emacs -Q --batch -L . -L ~/path/to/gptel -f batch-byte-compile magent-foo.el
```

Run a single test by regexp:
```bash
emacs -Q --batch -L . -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'gptel-*' -type d | head -1) \
  -l ert -l test/magent-test.el --eval '(ert-run-tests-batch "test-name-regexp")'
```

## Testing

### Unit Tests

`test/magent-test.el` contains the main ERT suite. Tests mock `gptel-request` and UI functions via `cl-letf`. Key patterns:
- Registry tests bind `magent-agent-registry--agents` to a fresh hash table
- Skills tests bind `magent-skills--registry` to nil
- Session tests call `magent-session-reset` to clear global state
- Loop/tool permission tests should use `magent-agent-loop.el` and `magent-tool-orchestrator.el`

### Coverage

`test/coverage.el` is the batch `testcover` runner used by `make coverage` and GitHub Actions. It instruments Magent sources, reloads built-in skill/capability directories from this checkout, runs `test/magent-test.el`, and writes `coverage/testcover-summary.tsv`.

### GitHub Actions

CI is defined under `.github/workflows/`:
- `test.yml`: installs Emacs 29.4 via Nix, installs package dependencies, runs `make compile`, `make test-unit`, and `make test-live-smoke` in a temporary daemon.
- `coverage.yml`: runs `make coverage` and uploads `coverage/testcover-summary.tsv`.
- `melpazoid.yml`: runs MELPA-style package checks. Its recipe must include `prompt.org`, `skills/`, and `capabilities/`, because `magent-config.el`, skills, and capabilities depend on bundled package data at runtime.

Package metadata should stay centralized in `magent.el` and `magent-pkg.el`; non-main modules should not carry `Package-Requires` headers. Keep SPDX license identifiers in every Elisp source file so melpazoid can detect licensing consistently.

### End-to-End Testing

After elisp code changes, test in the running Emacs via `emacsclient --eval`:

1. Reload: `(load "/path/to/changed-file.el" nil t)`
2. Clear: `(magent-clear-session)`
3. Test prompts:
   - Non-tool: `"你好"` — verifies streaming text and assistant section
   - Tool-use: `"帮我看下 emacs 里面有多少 buffer"` — verifies `emacs_eval` tool calling, UI rendering, Magent-owned loop
   - Multi-step: `"帮我在 emacs 里面打开 magent 的 magit buffer"` — verifies chained tool execution
4. Check `*magent*`, `*magent-log*`, and `*Messages*` for errors

For real gptel/tool debugging, prefer an isolated server and the playbook in
`docs/TROUBLESHOOTING.md#live-emacs-tests-fail-or-hang`. Key rules:
- Use `emacs --daemon=magent-live-test`; do not risk hanging or killing the main Emacs.
- Invoke make targets with `EMACSCLIENT="emacsclient -s magent-live-test"` when testing against that daemon.
- Load `test/magent-live-test.el`, run `magent-live-test-reload-source`, and verify `magent-live-test--repo-source-summary` points at this checkout rather than `~/.emacs.d/elpa/magent`.
- Run `make clean` before live/batch verification if any `.elc` files may be stale.
- For long real provider tests, use `magent-live-test-run-async` plus `/tmp/magent-live-*.el` status files and redacted gptel traces.

## Architecture

Magent is an Emacs Lisp AI coding agent with a multi-agent architecture and permission-based tool access. All LLM communication is delegated to **gptel**. UI integration depends on `agent-shell` and `acp`; package requirements currently target Emacs 29.1+.

### Module Dependency Graph

```
magent.el (entry point: magent-mode, global-magent-mode)
  ├─ magent-config.el            (defcustoms, deffaces, shared utilities, magent-log stub)
  ├─ magent-runtime.el           (static init + project overlay activation)
  ├─ magent-protocol.el          (wire protocol types and event normalization)
  ├─ magent-events.el            (event construction helpers)
  ├─ magent-thread.el            (thread/turn/item state machine, journal, snapshot)
  ├─ magent-turn.el              (request serialization, turn lifecycle)
  ├─ magent-session.el           (thread ledger projections, JSON persistence)
  ├─ magent-context.el           (context-item collection and prompt assembly helpers)
  ├─ magent-agent-job.el         (durable child-agent job state and JSON shape)
  ├─ magent-runtime-queue.el     (UI-neutral global turn queue and session-scoped cancellation)
  ├─ magent-runtime-api.el       (UI/backend-facing runtime session and prompt API)
  ├─ magent-llm.el               (provider-neutral request/event protocol)
  ├─ magent-llm-gptel.el         (gptel-request sampling adapter; hides gptel callback/FSM details)
  ├─ magent-agent-loop.el        (Magent-owned normalized event loop, tool dispatch, queueing, abort)
  ├─ magent-tool-orchestrator.el (permission, approval, audit, and tool-call orchestration)
  ├─ magent-tool-registry.el     (tool registration helpers)
  ├─ magent-approval.el          (user approval prompts for permission=ask)
  ├─ magent-audit.el             (audit trail for tool invocations and decisions)
  ├─ magent-capability.el        (capability registry and prompt-time resolution)
  ├─ magent-tools.el             (14 gptel-tool structs)
  ├─ magent-agent.el             (magent-agent-process: builds gptel prompt, calls gptel-request)
  ├─ magent-agent-registry.el    (cl-defstruct, 7 built-in agents, hash-table registry)
  ├─ magent-agent-types.el       (legacy feature-name compatibility shim)
  ├─ magent-agent-file.el        (loads custom agents from .magent/agent/*.md)
  ├─ magent-permission.el        (rule-based tool access control per agent)
  ├─ magent-acp.el               (in-process ACP adapter for agent-shell)
  ├─ magent-agent-shell.el       (agent-shell backend registration and routing)
  ├─ magent-ui.el                (thin UI backend router, logger, compatibility shims)
  ├─ magent-ui-legacy.el         (legacy special-mode workspace, compose buffer, ledger projection)
  ├─ magent-file-loader.el       (shared file-backed definition loader and frontmatter parser)
  ├─ magent-md2org.el            (legacy markdown → org-mode compatibility helpers)
  ├─ magent-evil.el              (optional Evil integration; not loaded by magent by default)
  └─ magent-skills.el            (skill registry, built-in skill definitions, file loading, and interactive commands)
```

### Core Flow

1. **Entry point** (`magent.el`): `magent-mode` minor mode with `C-c m` prefix. Mode enable only adds a modeline construct. Static initialization is **lazy** — triggered on first command via `magent--ensure-initialized`.

2. **Runtime** (`magent-runtime.el`): Owns the ordered initialization pipeline for agents, skills, and capabilities. Static bundled definitions load once; project-local overlays under `.magent/` are activated and unloaded as session scope changes.

3. **UI backend boundary** (`magent-ui.el`, `magent-agent-shell.el`, `magent-acp.el`, `magent-runtime-api.el`): `magent-ui.el` is now a thin backend router and compatibility layer. The default `magent-ui-backend` is `agent-shell`; plain prompts route to `magent-agent-shell`, which uses an in-process ACP client implemented by `magent-acp.el`. ACP submits prompts through `magent-runtime-api.el`, and runtime observer events are converted to ACP `session/update` messages. ACP prompt requests remain pending until the corresponding Magent turn completes, fails, or is cancelled.
   - `magent-runtime-queue.el` owns the global single-execution queue and session-scoped cancellation
   - Runtime emits Magent-native observer events; ACP conversion is isolated in `magent-acp.el`
   - `magent-agent-run-turn` is the low-level backend entry point; `magent-agent-process` remains the compatibility wrapper
   - `magent-ui-legacy.el` contains the old special-mode workspace, compose buffer, timeline rendering, transient menu, and legacy prompt dispatch
   - Plain agent-shell usage must not load `magent-ui-legacy.el`; legacy-only commands load it lazily
   - Per-request legacy skill/agent overrides still use the legacy dispatch path until agent-shell has equivalent UI controls
   - See `docs/UI_BACKENDS.md` for the boundary contract

4. **Legacy UI** (`magent-ui-legacy.el`): The retained workspace derives from `special-mode` (`magent-output-mode`) and renders a read-only **Magit-style chronological timeline** from the ledger. Prompt composition lives in a scope-specific `magent-compose-mode` buffer.
   - Workspace restore is ledger-driven; `buffer-content` is legacy fallback data and is no longer the UI source of truth
   - The workspace `header-line` is the single status surface for scope, agent, thread status, request state, queue length, session id, and selected one-shot skills
   - Compose buffers are plain prompt text and do not parse `@clear`, `@init`, `@skill`, `$command`, or slash-style control syntax
   - Streaming uses chunk batching (`magent-ui-batch-insert-delay`) and does not convert markdown to org in the live path
   - Request serialization is owned by `magent-turn`; `magent-ui--processing` remains a compatibility flag

5. **Agent processing** (`magent-agent.el`): `magent-agent-run-turn` is the UI-neutral low-level entry point for runtime backends. `magent-agent-process` builds a gptel prompt list from the session, applies per-agent overrides (model, temperature via `default-value` — intentionally avoids buffer-local gptel settings), exposes filtered tools to the provider, then starts `magent-agent-loop`.

6. **Tools** (`magent-tools.el`): 14 `gptel-tool` structs — `read_file`, `write_file`, `edit_file`, `grep`, `glob`, `bash`, `emacs_eval`, `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, `close_agent`, `skill_invoke`, `web_search`. Tools are registered globally but filtered per-agent through permissions. The child-agent tools share the `agent` permission key. `web_search` uses DuckDuckGo via `url-retrieve` + `libxml-parse-html-region` (requires Emacs built with `--with-xml2`).

7. **Agent Loop**: `magent-agent.el` starts the Magent-owned loop through `magent-agent-loop.el`. `magent-llm.el` defines normalized request/events, including `tool-call-batch-end`, and `magent-llm-gptel.el` calls `gptel-request` while hiding gptel callback/FSM details. The loop owns tool dispatch through `magent-tool-orchestrator`, serial tool queueing, permission audit hooks, visible tool rendering, request abort cleanup, tool-result session recording, and Codex-style continuation. Tool results are fed back to the model; the loop does not impose an `emacs_eval` per-turn call cap.

8. **Permissions** (`magent-permission.el`): Rules map tool names to `allow`/`deny`/`ask`, with optional file-pattern sub-rules (glob syntax). Resolution: exact tool match → file-pattern rules → wildcard (`*`) fallback → **default allow**. File-pattern rules are order-dependent (first match wins); more specific patterns must come before less specific ones.

9. **Capabilities** (`magent-capability.el`): File-backed capability definitions score the current request context and attach matching instruction skills. Bundled, user, and project-local capability overlays all feed the same resolver.

10. **Session and workflow ledger** (`magent-thread.el`, `magent-session.el`): The canonical agent workflow state is an explicit thread/turn/item ledger. Thread statuses are `not-loaded`, `idle`, `active`, `system-error`, and `closed`; turn statuses are `queued`, `in-progress`, `completed`, `interrupted`, `failed`, and `dropped`; item statuses are `pending`, `in-progress`, `completed`, `failed`, and `cancelled`. Tool call/result is one `tool` item lifecycle keyed by call id. Session JSON persists both an append-only `journal` and a materialized `snapshot`; snapshot restores materialized state, while journal remains the audit log and only events after `snapshot.last-event-seq` are replayed. Legacy `messages` and `context-items` are derived projections used for gptel prompt reuse and migration. `buffer-content` remains only legacy data; UI restore comes from the ledger. `agent-jobs` stores durable child-agent metadata.

11. **Skills** (`magent-skills.el`): Two types — `instruction` (markdown injected into system prompt) and `tool` (invoked via `skill_invoke`). The module now contains the registry, built-in `skill-creator`, file-based skill loading, and interactive inspection commands. Skills load in priority order from (1) built-in `skills/`, (2) user directory `~/.emacs.d/magent-skills/<name>/SKILL.md`, and (3) project-local `.magent/skills/<name>/SKILL.md`.

### Gotchas

- **Default UI is agent-shell**: plain interactive use should go through `magent-agent-shell.el` and `magent-acp.el`. Do not add new default UI behavior to `magent-ui-legacy.el`.
- **Legacy UI is isolated**: `magent-ui-legacy.el` owns `magent-output-mode`, compose buffers, legacy rendering, and the old transient menu. Keep new UI-independent behavior in `magent-runtime-api.el`.
- **`magent-output-mode` derives from `special-mode`**: In the legacy UI, the workspace is read-only and ledger-rendered. Use `inhibit-read-only` for insertions; do not reintroduce org folding or markdown-to-org conversion in the live path.
- **`magent-ui--with-insert` suppresses buffer-boundary signals**: In the legacy UI, catches `beginning-of-buffer`, `end-of-buffer`, etc. to suppress cursor-adjustment errors from process filters and active minor modes.
- **Evil integration is optional**: Keep Evil-specific behavior in `magent-evil.el`. Do not add `evil-*` calls, variables, or `with-eval-after-load 'evil` forms to core UI files.
- **`magent-log` is a stub in `magent-config.el`**: No-op until `magent-ui` is loaded. In batch tests, logs go nowhere unless you explicitly load `magent-ui`.
- **Tool execution helpers live in `magent-agent-loop.el`**: serial queueing, abort cleanup, visible tool rendering, and tool-result recording are all loop-owned. As with Codex, repeated tool use is steered by prompt/context rather than a hard `emacs_eval` call-count guard.
- **Provider transport stays in gptel**: `magent-llm-gptel.el` may use gptel private FSM details internally for one sampling request, but the Magent loop consumes only normalized events.
- **Legacy request generation counter**: `magent-ui--request-generation` increments on legacy dispatch and interrupt. Stale callbacks compare their captured generation and discard themselves if mismatched.
- **`revert-buffer` in legacy output buffer**: Bound to `magent-ui--revert-buffer` → `magent-ui-render-history`.

### Agent Definitions

Built-in agents: `build` (default), `plan`, `explore`, `general`, `compaction`, `title`, `summary`. Defined in `magent-agent-registry.el` with `cl-defstruct magent-agent-info` (fields: name, description, mode, native, hidden, temperature, top-p, color, model, prompt, options, steps, permission). `magent-agent-types.el` is a compatibility shim for older `require` forms. Agent modes: `primary` (user-facing), `subagent` (internal), `all` (either).

Custom agents: `.magent/agent/*.md` files with YAML frontmatter + markdown body (system prompt). Frontmatter is parsed by `magent-file-loader.el` (supports booleans, numbers, quoted strings, comma-separated lists; converts underscores to hyphens in keys).

### Skill File Format

```markdown
---
name: skill-name
description: Brief description
tools: bash, read
type: instruction        # 'instruction' or 'tool'
---

Markdown body: system prompt for instruction-type, operation docs for tool-type.
```

Tool-type skills can have companion `.el` files defining `magent-skill-<name>-invoke`.

### Configuration

All `defcustom` variables are in `magent-config.el` under `customize-group magent`. LLM provider/model/key settings are managed entirely by gptel.

Key settings: `magent-default-agent` (`"build"`), `magent-enable-tools` (list of enabled tool symbols), `magent-include-reasoning` (`t`/`ignore`/`nil`), `magent-request-timeout` (120s), `magent-bash-timeout` (30s), `magent-emacs-eval-timeout` (10s), `magent-max-history` (100).

### Keybindings

`C-c m` prefix in `magent-mode`. Plain prompt entry opens or reuses the
agent-shell backend by default; the transient menu remains available as a
legacy/compatibility command surface:

| Key | Command |
|-----|---------|
| `C-c m p` | `magent-dwim` |
| `C-c m r` | `magent-prompt-region` |
| `C-c m a` | `magent-ask-at-point` |
| `C-c m ?` | `magent-transient-menu` |

Former direct bindings (`C-c m d`, `C-c m c`, `C-c m A`, etc.) remain obsolete.
Lower-frequency legacy commands are still reachable from `magent-transient-menu`.

Transient top-level keys:

| Key | Action |
|-----|--------|
| `o` | Open compose |
| `i` | Run the built-in `init` skill command |
| `!` | Run a command-like skill |
| `A` | Agent submenu |
| `s` | Skills submenu |
| `x` | Capabilities submenu |
| `c` | Clear current session |
| `S` | Session/transcript submenu |
| `l` | Logs submenu |
| `h` | Health/diagnostics submenu |
| `b` | Buffer submenu |

Common nested paths: `? s s` toggles a one-shot instruction skill, `? s K` clears selected skills, `? A A` selects an agent, `? A v` lists agents, `? S j` opens a child-agent transcript, and `? l l` shows the log.

In legacy `magent-output-mode`: `TAB` fold/unfold turn sections, `?` transient menu, `C-c C-c` opens compose when idle or confirms interrupt while a request is running, `C-c C-o` opens compose. In legacy `magent-compose-mode`: `C-c C-c` submits and `C-c C-k` clears compose.

## Conventions

- All files use `;;; -*- lexical-binding: t; -*-`
- Use Conventional Commits for commit messages.
- System prompt loaded from `prompt.org` adjacent to `magent-config.el`
- Tool implementations follow pattern: `magent-tools--<name>` (internal fn) + `magent-tools--<name>-tool` (gptel-tool var)
- Byte-compile warnings suppressed: `cl-functions`, `obsolete`
