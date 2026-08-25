# AGENTS.md

Guidance for agentic coding tools working with Magent.

## Boundaries

- **Do not** implement Codex sandbox/seatbelt/bubblewrap/shell isolation. Preserve Emacs-native workflow (live buffers, isolated `emacs_eval`, explicit `emacs_eval_live`, agent-shell, project-scoped sessions, gptel transport).
- **Keep** `gptel-request` for provider/HTTP/SSE plumbing. Do not rewrite gptel integration.
- **Child-agent tools**: `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, `close_agent` replaced the old `delegate` tool. Do not reintroduce a compatibility wrapper unless explicitly requested.
- **Interrupted work**: Update relevant docs or task notes before stopping so progress is recoverable from git.

See `docs/AGENT_JOBS.org` and `docs/AGENT_WORKFLOW.org` for child-agent lifecycle details.

## Build Commands

```bash
make compile    # Byte-compile all Elisp files
make lint       # Validate declarations and package metadata
make test-unit  # Run ERT unit tests in batch mode
make test       # Run unit tests plus deterministic live smoke tests
make coverage   # Run ERT under testcover and write coverage/testcover-summary.tsv
make clean      # Remove build files and Harbor benchmark containers/networks
make purge      # Also remove benchmark Docker images and Harbor task cache
```

The Makefile auto-detects dependency paths (`gptel`, `acp`, `shell-maker`, `agent-shell`, `cond-let`, `compat`, `yaml`, `llama`, `with-editor`, `package-lint`) by scanning `~/.emacs.d/elpa/`. Override any with e.g. `GPTEL_DIR=/path/to/gptel`.

Single-file compilation:
```bash
emacs -Q --batch -L lisp -L ~/path/to/gptel -f batch-byte-compile lisp/magent-foo.el
```

Run a single test by regexp:
```bash
emacs -Q --batch -L lisp -L $(find ~/.emacs.d/elpa -maxdepth 1 -name 'gptel-*' -type d | head -1) \
  -l ert -l test/magent-test.el --eval '(ert-run-tests-batch "test-name-regexp")'
```

## Testing

### Unit Tests

`test/magent-test.el` contains the main ERT suite. Tests mock `gptel-request` and frontend/runtime functions via `cl-letf`. Key patterns:
- Registry tests bind `magent-agent-registry--agents` to a fresh hash table
- Skills tests bind `magent-skills--registry` to nil
- Session tests call `magent-session-reset` to clear global state
- Loop/tool permission tests should use `magent-agent-loop.el` and `magent-tool-orchestrator.el`

### Coverage

`test/coverage.el` is the batch `testcover` runner used by `make coverage` and GitHub Actions. It instruments Magent sources, reloads built-in skill/capability directories from this checkout, runs `test/magent-test.el`, and writes `coverage/testcover-summary.tsv`.

### GitHub Actions

CI is defined under `.github/workflows/`:
- `test.yml`: tests Emacs 29.4 and 30.1 via Nix, installs package dependencies, runs `make compile`, `make lint`, `make test-unit`, and `make test-live-smoke` in a temporary daemon.
- `coverage.yml`: runs `make coverage` and uploads `coverage/testcover-summary.tsv`.
- `melpazoid.yml`: runs MELPA-style package checks. Its recipe explicitly includes `lisp/magent*.el`, `prompts/`, and `skills/`, because Magent depends on those production libraries and bundled data at runtime. Keep the explicit library and runtime-data entries aligned with the package source manifest.

Package metadata should stay centralized in `magent.el` and `magent-pkg.el`; non-main modules should not carry `Package-Requires` headers. Keep SPDX license identifiers in every Elisp source file so melpazoid can detect licensing consistently.

### Local melpazoid Reproduction

When reproducing the melpazoid job locally:

1. Use the exact recipe from `.github/workflows/melpazoid.yml` and set `LOCAL_REPO` to this checkout. Verify the staged `pkg/` contains the complete flattened production Elisp set, excludes ERT sources under `test/`, and includes the bundled runtime-data directories before trusting later checks.
2. Check for an existing `melpazoid:latest` image before rebuilding. It may be reused only when its installed dependency set matches the newly generated `_requirements.el`; never treat the package sources embedded in an old image as current.
3. To reuse a compatible image, bind-mount the newly staged `pkg/` over `/workspace/pkg` and separately mount the current `melpazoid.el`, because the package mount hides the checker bundled in the image. The staged package root must be writable by the container user because byte compilation creates `.elc` files.
4. Start each rerun from a fresh staged package, or remove checker-generated `.elc` and autoload files first. Otherwise a second run can lint artifacts that were not part of the MELPA recipe and produce misleading extra warnings.
5. Run the container with `--network=none`, matching melpazoid's normal test target. Success for a packaging regression requires a zero exit status and a clean packaged `#'load` check; source-tree compilation alone does not prove that the recipe copied every runtime file.

Keep `magent-test-melpazoid-recipe-packages-production-libraries` aligned with the workflow recipe so unit tests catch missing production libraries or runtime-data entries. When adding a production module, also keep `magent-test-source-manifest-covers-production-elisp` green.

### End-to-End Testing

After elisp code changes, test in the running Emacs via `emacsclient --eval`:

1. Reload: `(load "/path/to/changed-file.el" nil t)`
2. Clear: `(magent-runtime-session-clear (magent-runtime-session-current))`
3. Test prompts:
   - Non-tool: `"你好"` — verifies streaming text and assistant section
   - Tool-use: `"帮我看下 emacs 里面有多少 buffer"` — verifies `emacs_read` tool calling, UI rendering, Magent-owned loop
   - Multi-step: `"帮我在 emacs 里面打开 magent 的 magit buffer"` — verifies chained tool execution
4. Check the active Magent agent-shell buffer, `*magent-log*`, and `*Messages*`
   for errors

For real gptel/tool debugging, prefer an isolated server and the playbook in
`docs/TROUBLESHOOTING.org#live-emacs-tests-fail-or-hang`. Key rules:
- Use `emacs --daemon=magent-live-test`; do not risk hanging or killing the main Emacs.
- Invoke make targets with `EMACSCLIENT="emacsclient -s magent-live-test"` when testing against that daemon.
- Load `test/magent-live-test.el`, run `magent-live-test-reload-source`, and verify `magent-live-test--repo-source-summary` points at this checkout rather than `~/.emacs.d/elpa/magent`.
- Run `make clean` before live/batch verification if any `.elc` files may be stale.
- For long real provider tests, use `magent-live-test-run-async` plus `/tmp/magent-live-*.el` status files and redacted gptel traces.

## Architecture

Magent is an Emacs Lisp AI coding agent with a multi-agent architecture and permission-based tool access. All LLM communication is delegated to **gptel**. UI integration depends on `agent-shell` and `acp`; package requirements currently target Emacs 29.1+.

### Module Dependency Graph

```
magent.el (package entry point and lazy runtime bootstrap)
  ├─ magent-log.el               (UI-neutral logging sinks and headless fallback)
  ├─ magent-config.el            (UI-neutral runtime and feature defcustoms)
  ├─ magent-json.el              (JSON-safe serialization helpers)
  ├─ magent-redaction.el         (fail-closed Magent-owned outbound redaction)
  ├─ magent-runtime.el           (static init + project overlay activation)
  ├─ magent-protocol.el          (wire protocol types and event normalization)
  ├─ magent-lifecycle-events.el  (lifecycle event sinks and context helpers)
  ├─ magent-ledger.el            (thread/turn/item state machine, journal, snapshot)
  ├─ magent-session.el           (thread ledger projections, JSON persistence)
  ├─ magent-agent-job.el         (durable child-agent job state and JSON shape)
  ├─ magent-runtime-queue.el     (UI-neutral global turn queue and session-scoped cancellation)
  ├─ magent-runtime-api.el       (UI/backend-facing runtime session and prompt API)
  ├─ magent-project-instructions.el (bounded scoped AGENTS.md discovery and prompt injection)
  ├─ magent-action.el            (Workflow DSL, Step runtime, layered Action registry, Invocation lifecycle)
  ├─ magent-action-builtins.el   (data-defined prompt Actions and built-in registration)
  ├─ magent-action-session.el    (isolated Action persistence, ledger, and cancellation)
  ├─ magent-action-session-view.el (isolated Action session listing and inspection)
  ├─ magent-action-builtin-doctor.el            (trusted probes + one sanitized tool-free analysis)
  ├─ magent-llm.el               (provider-neutral request/event protocol)
  ├─ magent-llm-gptel.el         (gptel-request sampling adapter; hides gptel callback/FSM details)
  ├─ magent-action-builtin-memory.el            (Emacs profile memory scan, persistence, and prompt injection)
  ├─ magent-agent-loop.el        (Magent-owned normalized event loop, tool dispatch, queueing, abort)
  ├─ magent-tool-orchestrator.el (permission, approval, audit, and tool-call orchestration)
  ├─ magent-approval.el          (user approval prompts for permission=ask)
  ├─ magent-audit.el             (audit trail for tool invocations and decisions)
  ├─ magent-capability.el        (capability registry and prompt-time resolution)
  ├─ magent-tools.el             (canonical catalog of 16 gptel-tool structs)
  ├─ magent-agent.el             (magent-agent-run-turn: builds prompts and starts the Magent loop)
  ├─ magent-agent-info.el        (agent metadata struct and helpers)
  ├─ magent-agent-builtins.el    (7 built-in agent definitions)
  ├─ magent-agent-registry.el    (hash-table registry and project overlays)
  ├─ magent-agent-file.el        (loads custom agents from .magent/agent/*.md)
  ├─ magent-permission.el        (rule-based tool access control per agent)
  ├─ magent-acp.el               (in-process ACP adapter for agent-shell)
  ├─ magent-agent-shell.el       (agent-shell backend registration and routing)
  ├─ magent-file-loader.el       (shared file-backed definition loader and frontmatter parser)
  └─ magent-skills.el            (skill registry, built-in skill definitions, file loading, and interactive commands)
```

### Core Flow

1. **Entry point** (`magent.el`, `magent-agent-shell.el`): `magent.el` loads the package; supported interaction starts with `magent-start`, `magent-agent-shell-start`, or Magent selected from `agent-shell`. Static initialization is **lazy** — triggered on the first supported command via `magent--ensure-initialized`.

2. **Runtime** (`magent-runtime.el`): Owns the ordered initialization pipeline for agents, skills, slash commands, and capabilities. Static bundled definitions load once; project-local overlays under `.magent/` are activated and unloaded as session scope changes.

3. **Actions** (`magent-action.el`, `magent-action-builtins.el`, `magent-action-session.el`, `magent-action-session-view.el`): Elisp-native Actions are registered through `magent-action-register`; slash commands and interactive commands are frontend projections. `:exposure` selects the projections, while `:session-policy` selects the current conversation or an isolated durable Action session. One deep Action module owns the generator Workflow DSL, managed Step runtime, layered registry, and Invocation lifecycle. Trusted Elisp owns control flow while managed agent, Answer, argv process, and callback Steps provide asynchronous suspension, exact-submission cancellation, and ledger activity. Elisp feature dependencies are declared with `:requires`; an agent Step's `:tools` is its exact provider allowlist, and Actions have no project-workspace requirement. Definitions resolve by `core > project > user > package > builtin`, and core names are reserved. ACP resolves slash discovery and dispatch against each runtime session's exact scope. Terminal results are claimed before cleanup, and finalization errors remain terminal failures.

4. **Isolated Actions** (`magent-action-session.el`, `magent-action-session-view.el`, `magent-action-builtin-memory.el`, `magent-action-builtin-doctor.el`): `/doctor` and `/memory-*` are unified Action specs exposed through both agent-shell and `M-x magent-action-run-*`. `magent-action-enabled-builtins` controls their registration as `doctor` and `memory` groups and refreshes frontend discovery after Custom changes. They create isolated sessions under `magent-session-directory/actions`, preserve the current conversation, and can be inspected with `magent-action-list-sessions` or cancelled with `magent-action-cancel`. The old `commands/` format is not read or migrated. Memory Actions respect `magent-bypass-permission`. Doctor uses trusted read-only probes, Magent-owned redaction, and one tool-free direct request outside the runtime queue. Custom probes are trusted Elisp, not sandboxed code. See `docs/DOCTOR.org`.

5. **Supported frontend boundary** (`magent-agent-shell.el`, `magent-acp.el`, `magent-runtime-api.el`): `magent-agent-shell.el` is the only supported frontend integration and uses an in-process ACP client implemented by `magent-acp.el`. ACP routes registered slash input through `magent-action.el`, submits model turns through `magent-runtime-api.el`, and converts runtime observer events to ACP `session/update` messages. ACP prompt requests remain pending until the corresponding command invocation or ordinary Magent turn completes, fails, or is cancelled.
   - `magent-runtime-queue.el` owns the global single-execution queue and session-scoped cancellation
   - Runtime emits Magent-native observer events; ACP conversion is isolated in `magent-acp.el`
   - ACP text/resource blocks are stored as structured turn metadata and reconstructed as user-role prompt context; local `file://` resources also provide scoped request paths
   - ACP session config exposes a per-session `Automatic capabilities` switch
   - `magent-agent-run-turn` is the backend entry point
   - See `docs/UI_BACKENDS.org` for the boundary contract

6. **Agent processing** (`magent-agent.el`): `magent-agent-run-turn` is the UI-neutral entry point for runtime backends. Its internal execution path builds a gptel prompt list from the session, discovers applicable `AGENTS.md` files from project root toward request-local resources within a bounded project scope, applies per-agent overrides (model, temperature via `default-value` — intentionally avoids buffer-local gptel settings), resolves the request's exact tool names through the canonical catalog and agent permissions, then starts `magent-agent-loop`.

7. **Tools** (`magent-tools.el`): one canonical catalog owns 16 `gptel-tool` structs, their names, permission keys, and approval policies — `read_file`, `write_file`, `edit_file`, `grep`, `glob`, `bash`, `emacs_read`, `emacs_eval`, `emacs_eval_live`, `read_tool_output`, `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, `close_agent`, `web_search`. `read_file` requires `source=disk|live-buffer` and emits a revision; writes require the expected revision. `emacs_eval` uses a fresh child Emacs, `emacs_read` provides fixed live queries, and `emacs_eval_live` is the explicit dangerous live escape hatch. Both arbitrary eval tools require once-only approval. Oversized results spill to session-private storage and are paged through `read_tool_output`. All implementations and persisted tool items use `magent-tool-result`; unstructured strings are rejected. `glob` traverses in bounded event-loop slices without following directory symlinks. Child-agent tools share `agent`; their runtime registry and event-driven wait observers live in `magent-agent-job.el`, and parent persistence is deferred. `web_search` uses DuckDuckGo via `url-retrieve` + `libxml-parse-html-region` (requires Emacs built with `--with-xml2`).

8. **Agent Loop**: `magent-agent.el` starts the Magent-owned loop through `magent-agent-loop.el`. `magent-llm.el` defines normalized request/events, including `tool-call-batch-end`, and `magent-llm-gptel.el` calls `gptel-request` while hiding gptel callback/FSM details. The loop owns tool dispatch through `magent-tool-orchestrator`, serial tool queueing, permission audit hooks, structured lifecycle event emission, request abort cleanup, and tool-result session recording. Runtime observers project visible tool events to supported frontends. The agent execution path feeds tool results back only through provider-native continuation, completes empty provider responses with explicit metadata, and fails directly when an optional sampling limit is reached. Reasoning events stay separate from assistant text and are not used as final-answer fallback.

9. **Permissions** (`magent-permission.el`): Rules map tool names to `allow`/`deny`/`ask`, with optional file-pattern sub-rules (glob syntax). Resolution: exact tool match → file-pattern rules → wildcard (`*`) fallback → **default allow**. File-pattern rules are order-dependent (first match wins); more specific patterns must come before less specific ones.

10. **Capabilities** (`magent-capability.el`): File-backed capability definitions score the current request context and attach matching instruction skills. Automatic activation requires a word-bounded prompt-keyword intent match in addition to context score; context-only matches remain suggested, and linked skills are filtered against the selected agent's exposed tools. Bundled, user, and project-local capability overlays all feed the same resolver.

11. **Session and workflow ledger** (`magent-ledger.el`, `magent-session.el`): The canonical agent workflow state is an explicit thread/turn/item ledger. Thread statuses are `not-loaded`, `idle`, `active`, `system-error`, and `closed`; turn statuses are `queued`, `in-progress`, `completed`, `interrupted`, `failed`, and `dropped`; item statuses are `pending`, `in-progress`, `completed`, `failed`, and `cancelled`. Tool call/result is one `tool` item lifecycle keyed by call id. Session JSON schema v6 is atomically replaced with a materialized `snapshot` and a bounded journal tail; other schemas and unknown fields are rejected. Consumers select explicit `ledger`, `transcript`, `provider`, `compaction`, or `audit` views. `agent-jobs` stores durable child-agent metadata. Action turns retain Action metadata. Isolated Action sessions are persisted under `actions/` and excluded from normal conversation listings.

12. **Skills** (`magent-skills.el`): Skills are instruction-only Markdown injected into the system prompt. Executable extensions belong in trusted Elisp Actions or first-class catalog tools; skills are never converted into Action adapters, and companion Elisp next to `SKILL.md` is not loaded. The module contains the registry, built-in `skill-creator`, file-based loading, and interactive inspection commands. Skills load in priority order from (1) built-in `skills/`, (2) user directory `~/.emacs.d/magent/skills/<name>/SKILL.md`, and (3) project-local `.magent/skills/<name>/SKILL.md`.

### Gotchas

- **Only supported frontend is agent-shell**: interactive use goes through `magent-agent-shell.el` and `magent-acp.el`.
- **Action projections are registry-driven**: keep invocation and precedence in `magent-action.el`; frontends discover and dispatch command projections through that public layer rather than private command tables.
- **Actions own both projections**: keep slash discovery, interactive wrappers, isolated sessions, and lifecycle ownership in `magent-action`; do not add `/magent-*` local command tables to `magent-acp.el`.
- **Doctor never receives general tools**: keep `magent-action-run-doctor` on the trusted probe plus one tool-free request path. Do not expose `emacs_eval`, shell, file tools, backend objects, credentials, environment variables, or raw provider logs through Doctor probes.
- **Frontend code stays on the supported path**: keep UI-independent behavior in `magent-runtime-api.el`, ACP conversion in `magent-acp.el`, and agent-shell behavior in `magent-agent-shell.el`.
- **Core logging is UI-neutral**: `magent-log.el` dispatches formatted messages to sinks and falls back to `message` for warnings/errors when headless.
- **Tool execution helpers live in `magent-agent-loop.el`**: serial queueing, abort cleanup, lifecycle event emission, and tool-result recording are all loop-owned; UI sinks own visible rendering. As with Codex, repeated tool use is steered by prompt/context rather than a hard `emacs_eval` call-count guard. Do not add hidden final-response recovery requests.
- **Provider transport stays in gptel**: `magent-llm-gptel.el` may use gptel private FSM details internally for one sampling request, but the Magent loop consumes only normalized events.

### Agent Definitions

Built-in agents: `build` (default), `plan`, `explore`, `general`, `compaction`, `title`, `summary`. Defined in `magent-agent-builtins.el` with `cl-defstruct magent-agent-info` in `magent-agent-info.el` (fields: name, description, mode, native, hidden, temperature, top-p, effort, color, model, prompt, permission, source-layer, source-scope). Agent modes: `primary` (user-facing), `subagent` (internal), `all` (either).

Custom agents: `.magent/agent/*.md` files with YAML frontmatter + markdown body (system prompt). Frontmatter is parsed by `magent-file-loader.el`; keys use their exact kebab-case spelling, unknown keys are rejected, and list-valued fields use YAML sequences.

### Skill File Format

```markdown
---
name: skill-name
description: Brief description
tools: [bash, read_file]
type: instruction        # the only supported type
requires-project: true   # optional: reject use from a global session
---

Markdown body: system-prompt instructions. `type` must be `instruction`.
```

### Configuration

UI-neutral `defcustom` variables live in `magent-config.el` under `customize-group magent`. LLM provider/model/key settings are managed entirely by gptel.

Key settings: `magent-default-agent` (`"build"`), `magent-enable-tools` (list of enabled tool symbols), `magent-action-enabled-builtins` (`(doctor memory)`), `magent-include-reasoning` (`t`/`ignore`/`nil`), `magent-request-timeout` (120s), `magent-bash-timeout` (300s), `magent-emacs-eval-timeout` (10s), `magent-action-process-timeout` (300s), `magent-action-step-output-max-chars` (24000), `magent-max-history` (100).

### Supported Frontend Commands

| Command | Action |
|---------|--------|
| `magent-start` | Open or reuse the project-local Magent agent-shell buffer |
| `magent-agent-shell-start` | Start a fresh Magent agent-shell buffer |
| `magent-agent-shell-prompt-region` | Send the active region through agent-shell |
| `magent-agent-shell-ask-at-point` | Ask about the symbol at point through agent-shell |
| `magent-agent-shell-interrupt` | Interrupt the active request |
| `magent-agent-shell-toggle-skill-for-next-request` | Toggle a one-shot instruction skill |
| `magent-agent-shell-run-command` | Select and run an Elisp-native Action through its command projection |

Use agent-shell's own bindings, session options, mode selector, and slash
commands for interaction.

## Conventions

- All files use `;;; -*- lexical-binding: t; -*-`
- Use Conventional Commits for commit messages.
- Keep the canonical package version in the `Package-Version` header of
  `lisp/magent.el`; Git tags and GitHub Releases use the matching `vX.Y.Z`.
  Record user-visible changes in `CHANGELOG.org` and follow
  `docs/RELEASING.org` for compatibility and release decisions.
- Core, built-in agent, slash-command, and internal runtime prompts are editable Org files under `prompts/`; every bundled Org prompt must also appear exactly once in `prompts/manifest.txt`; skill prompts remain self-contained in `skills/*/SKILL.md`
- Tool implementations follow pattern: `magent-tools--<name>` (internal fn) + `magent-tools--<name>-tool` (gptel-tool var)
- Byte-compile warnings suppressed: `cl-functions`
