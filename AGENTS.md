# AGENTS.md

Guidance for agentic coding tools working with Magent.

## Boundaries

- **Do not** implement Codex sandbox/seatbelt/bubblewrap/shell isolation. Preserve Emacs-native workflow (live buffers, `emacs_eval`, agent-shell, project-scoped sessions, gptel transport).
- **Keep** `gptel-request` for provider/HTTP/SSE plumbing. Do not rewrite gptel integration.
- **Child-agent tools**: `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, `close_agent` replaced the old `delegate` tool. Do not reintroduce a compatibility wrapper unless explicitly requested.
- **Interrupted work**: Update relevant docs or task notes before stopping so progress is recoverable from git.

See `docs/AGENT_JOBS.org` and `docs/AGENT_WORKFLOW.org` for child-agent lifecycle details.

## Build Commands

```bash
make compile    # Byte-compile all Elisp files
make test-unit  # Run ERT unit tests in batch mode
make test       # Run unit tests plus deterministic live smoke tests
make coverage   # Run ERT under testcover and write coverage/testcover-summary.tsv
make clean      # Remove compiled .elc files
```

The Makefile auto-detects dependency paths (`gptel`, `acp`, `shell-maker`, `agent-shell`, `cond-let`, `compat`, `yaml`, `llama`, `with-editor`) by scanning `~/.emacs.d/elpa/`. Override any with e.g. `GPTEL_DIR=/path/to/gptel`.

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
- `test.yml`: installs Emacs 29.4 via Nix, installs package dependencies, runs `make compile`, `make test-unit`, and `make test-live-smoke` in a temporary daemon.
- `coverage.yml`: runs `make coverage` and uploads `coverage/testcover-summary.tsv`.
- `melpazoid.yml`: runs MELPA-style package checks. Its recipe must include `prompt/`, `skills/`, and `capabilities/`, because `magent-config.el`, prompts, skills, and capabilities depend on bundled package data at runtime.

Package metadata should stay centralized in `magent.el` and `magent-pkg.el`; non-main modules should not carry `Package-Requires` headers. Keep SPDX license identifiers in every Elisp source file so melpazoid can detect licensing consistently.

### End-to-End Testing

After elisp code changes, test in the running Emacs via `emacsclient --eval`:

1. Reload: `(load "/path/to/changed-file.el" nil t)`
2. Clear: `(magent-runtime-session-clear (magent-runtime-session-current))`
3. Test prompts:
   - Non-tool: `"你好"` — verifies streaming text and assistant section
   - Tool-use: `"帮我看下 emacs 里面有多少 buffer"` — verifies `emacs_eval` tool calling, UI rendering, Magent-owned loop
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
magent.el (entry point: magent-mode, global-magent-mode)
  ├─ magent-log.el               (UI-neutral logging sinks and headless fallback)
  ├─ magent-config.el            (UI-neutral runtime and feature defcustoms)
  ├─ magent-json.el              (JSON-safe serialization helpers)
  ├─ magent-redaction.el         (fail-closed Magent-owned outbound redaction)
  ├─ magent-runtime.el           (static init + project overlay activation)
  ├─ magent-protocol.el          (wire protocol types and event normalization)
  ├─ magent-lifecycle-events.el  (lifecycle event sinks and context helpers)
  ├─ magent-ledger.el            (thread/turn/item state machine, journal, snapshot)
  ├─ magent-session.el           (thread ledger projections, JSON persistence)
  ├─ magent-transcript-context.el (structured transcript context helpers)
  ├─ magent-agent-job.el         (durable child-agent job state and JSON shape)
  ├─ magent-runtime-queue.el     (UI-neutral global turn queue and session-scoped cancellation)
  ├─ magent-runtime-api.el       (UI/backend-facing runtime session and prompt API)
  ├─ magent-project-instructions.el (bounded scoped AGENTS.md discovery and prompt injection)
  ├─ magent-command.el           (session-backed internal command workflows)
  ├─ magent-doctor.el            (trusted probes + one sanitized tool-free analysis)
  ├─ magent-llm.el               (provider-neutral request/event protocol)
  ├─ magent-llm-gptel.el         (gptel-request sampling adapter; hides gptel callback/FSM details)
  ├─ magent-memory.el            (Emacs profile memory scan, persistence, and prompt injection)
  ├─ magent-agent-loop.el        (Magent-owned normalized event loop, tool dispatch, queueing, abort)
  ├─ magent-tool-orchestrator.el (permission, approval, audit, and tool-call orchestration)
  ├─ magent-tool-runtime.el      (runtime-facing tool metadata adapter)
  ├─ magent-approval.el          (user approval prompts for permission=ask)
  ├─ magent-audit.el             (audit trail for tool invocations and decisions)
  ├─ magent-capability.el        (capability registry and prompt-time resolution)
  ├─ magent-repo-summary.el      (deterministic single-file Org summary writer)
  ├─ magent-tools.el             (15 gptel-tool structs)
  ├─ magent-agent.el             (magent-agent-process: builds gptel prompt, calls gptel-request)
  ├─ magent-agent-info.el        (agent metadata struct and helpers)
  ├─ magent-agent-builtins.el    (7 built-in agent definitions)
  ├─ magent-agent-registry.el    (hash-table registry and project overlays)
  ├─ magent-agent-file.el        (loads custom agents from .magent/agent/*.md)
  ├─ magent-permission.el        (rule-based tool access control per agent)
  ├─ magent-acp.el               (in-process ACP adapter for agent-shell)
  ├─ magent-agent-shell.el       (agent-shell backend registration and routing)
  ├─ magent-modeline.el          (UI-neutral mode-line formatting)
  ├─ magent-file-loader.el       (shared file-backed definition loader and frontmatter parser)
  └─ magent-skills.el            (skill registry, built-in skill definitions, file loading, and interactive commands)
```

### Core Flow

1. **Entry point** (`magent.el`, `magent-agent-shell.el`): `magent.el` loads the package; supported interaction starts with `magent-agent-shell-dwim`, `magent-agent-shell-start`, or Magent selected from `agent-shell`. Static initialization is **lazy** — triggered on the first supported command via `magent--ensure-initialized`.

2. **Runtime** (`magent-runtime.el`): Owns the ordered initialization pipeline for agents, skills, and capabilities. Static bundled definitions load once; project-local overlays under `.magent/` are activated and unloaded as session scope changes.

3. **Internal commands** (`magent-command.el`, `magent-memory.el`, `magent-doctor.el`): Magent-owned maintenance workflows are invoked as explicit `M-x magent-run-*` commands and create isolated internal sessions under `magent-session-directory/internal`. They are not exposed as agent-shell slash commands. `magent-command-register` supports direct pipelines and, for trusted extensions that need it, normal agent-loop runners. Agent-loop runners retain session-scoped cancellation handles exposed through `M-x magent-cancel-internal-command`. Memory commands use the same session-backed command layer and respect `magent-bypass-permission` for scan/clear confirmation. Doctor is deliberately different at the execution layer: trusted read-only probes collect bounded data, `magent-redaction.el` sanitizes it, and one direct request runs with no tools outside the normal runtime queue. Custom probes are trusted Elisp, not sandboxed code. See `docs/DOCTOR.org`.

4. **Supported frontend boundary** (`magent-agent-shell.el`, `magent-acp.el`, `magent-runtime-api.el`): `magent-agent-shell.el` is the only supported frontend integration and uses an in-process ACP client implemented by `magent-acp.el`. ACP submits prompts through `magent-runtime-api.el`, and runtime observer events are converted to ACP `session/update` messages. ACP prompt requests remain pending until the corresponding Magent turn completes, fails, or is cancelled.
   - `magent-runtime-queue.el` owns the global single-execution queue and session-scoped cancellation
   - Runtime emits Magent-native observer events; ACP conversion is isolated in `magent-acp.el`
   - ACP text/resource blocks are stored as structured turn metadata and reconstructed as user-role prompt context; local `file://` resources also provide scoped request paths
   - ACP session config exposes a per-session `Automatic capabilities` switch
   - `magent-agent-run-turn` is the low-level backend entry point; `magent-agent-process` remains the compatibility wrapper
   - See `docs/UI_BACKENDS.org` for the boundary contract

5. **Agent processing** (`magent-agent.el`): `magent-agent-run-turn` is the UI-neutral low-level entry point for runtime backends. `magent-agent-process` builds a gptel prompt list from the session, discovers applicable `AGENTS.md` files from project root toward request-local resources within a bounded project scope, applies per-agent overrides (model, temperature via `default-value` — intentionally avoids buffer-local gptel settings), exposes filtered tools to the provider, then starts `magent-agent-loop`.

6. **Tools** (`magent-tools.el`): 15 `gptel-tool` structs — `read_file`, `write_file`, `write_repo_summary`, `edit_file`, `grep`, `glob`, `bash`, `emacs_eval`, `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, `close_agent`, `skill_invoke`, `web_search`. Tools are registered globally but filtered per-agent through permissions. `write_repo_summary` delegates canonical single-file Org updates to `magent-repo-summary.el` and shares the `write` permission key. The child-agent tools share the `agent` permission key. `web_search` uses DuckDuckGo via `url-retrieve` + `libxml-parse-html-region` (requires Emacs built with `--with-xml2`).

7. **Agent Loop**: `magent-agent.el` starts the Magent-owned loop through `magent-agent-loop.el`. `magent-llm.el` defines normalized request/events, including `tool-call-batch-end`, and `magent-llm-gptel.el` calls `gptel-request` while hiding gptel callback/FSM details. The loop owns tool dispatch through `magent-tool-orchestrator`, serial tool queueing, permission audit hooks, structured lifecycle event emission, request abort cleanup, and tool-result session recording. Runtime observers project visible tool events to supported frontends. `magent-agent-process` owns Codex-style continuation policy: tool results are fed back to the model, sampling limits force a no-tool final request, and a post-tool empty assistant completion gets one no-tool final-response retry. Reasoning events stay separate from assistant text and are not used as final-answer fallback.

8. **Permissions** (`magent-permission.el`): Rules map tool names to `allow`/`deny`/`ask`, with optional file-pattern sub-rules (glob syntax). Resolution: exact tool match → file-pattern rules → wildcard (`*`) fallback → **default allow**. File-pattern rules are order-dependent (first match wins); more specific patterns must come before less specific ones.

9. **Capabilities** (`magent-capability.el`): File-backed capability definitions score the current request context and attach matching instruction skills. Automatic activation requires a word-bounded prompt-keyword intent match in addition to context score; context-only matches remain suggested, and linked skills are filtered against the selected agent's exposed tools. Bundled, user, and project-local capability overlays all feed the same resolver.

10. **Session and workflow ledger** (`magent-ledger.el`, `magent-session.el`): The canonical agent workflow state is an explicit thread/turn/item ledger. Thread statuses are `not-loaded`, `idle`, `active`, `system-error`, and `closed`; turn statuses are `queued`, `in-progress`, `completed`, `interrupted`, `failed`, and `dropped`; item statuses are `pending`, `in-progress`, `completed`, `failed`, and `cancelled`. Tool call/result is one `tool` item lifecycle keyed by call id. Session JSON is atomically replaced with a materialized `snapshot` and a bounded tail of the in-memory append-only `journal`; only events after `snapshot.last-event-seq` are replayed. The separate JSONL audit subsystem retains tool/permission audit records. Legacy `messages` and `context-items` are derived projections used for gptel prompt reuse and migration. `buffer-content` remains only legacy data; frontend restore comes from the ledger. `agent-jobs` stores durable child-agent metadata. Internal command session metadata is persisted alongside ordinary session JSON but stored outside normal session listing paths.

11. **Skills** (`magent-skills.el`): Two types — `instruction` (markdown injected into system prompt) and `tool` (invoked via `skill_invoke`). The module now contains the registry, built-in `skill-creator`, file-based skill loading, and interactive inspection commands. Skills load in priority order from (1) built-in `skills/`, (2) user directory `~/.emacs.d/magent-skills/<name>/SKILL.md`, and (3) project-local `.magent/skills/<name>/SKILL.md`.

### Gotchas

- **Only supported frontend is agent-shell**: interactive use goes through `magent-agent-shell.el` and `magent-acp.el`.
- **Internal commands are not slash commands**: keep Magent maintenance workflows behind explicit `M-x magent-run-*` commands and `magent-command-register`; do not add `/magent-*` local command handling back to `magent-acp.el`.
- **Doctor never receives general tools**: keep `magent-run-doctor` on the trusted probe plus one tool-free request path. Do not expose `emacs_eval`, shell, file tools, backend objects, credentials, environment variables, or raw provider logs through Doctor probes.
- **Frontend code stays on the supported path**: keep UI-independent behavior in `magent-runtime-api.el`, ACP conversion in `magent-acp.el`, and agent-shell behavior in `magent-agent-shell.el`.
- **Core logging is UI-neutral**: `magent-log.el` dispatches formatted messages to sinks and falls back to `message` for warnings/errors when headless.
- **Tool execution helpers live in `magent-agent-loop.el`**: serial queueing, abort cleanup, lifecycle event emission, and tool-result recording are all loop-owned; UI sinks own visible rendering. As with Codex, repeated tool use is steered by prompt/context rather than a hard `emacs_eval` call-count guard. Keep final-response retry policy in `magent-agent.el`, not in tool dispatch.
- **Provider transport stays in gptel**: `magent-llm-gptel.el` may use gptel private FSM details internally for one sampling request, but the Magent loop consumes only normalized events.

### Agent Definitions

Built-in agents: `build` (default), `plan`, `explore`, `general`, `compaction`, `title`, `summary`. Defined in `magent-agent-builtins.el` with `cl-defstruct magent-agent-info` in `magent-agent-info.el` (fields: name, description, mode, native, hidden, temperature, top-p, color, model, prompt, options, steps, permission). Agent modes: `primary` (user-facing), `subagent` (internal), `all` (either).

Custom agents: `.magent/agent/*.md` files with YAML frontmatter + markdown body (system prompt). Frontmatter is parsed by `magent-file-loader.el` (supports booleans, numbers, quoted strings, comma-separated lists; converts underscores to hyphens in keys).

### Skill File Format

```markdown
---
name: skill-name
description: Brief description
tools: bash, read
type: instruction        # 'instruction' or 'tool'
requires-project: true   # optional: reject use from a global session
---

Markdown body: system prompt for instruction-type, operation docs for tool-type.
```

Tool-type skills can have companion `.el` files defining `magent-skill-<name>-invoke`.

### Configuration

UI-neutral `defcustom` variables live in `magent-config.el` under `customize-group magent`. LLM provider/model/key settings are managed entirely by gptel.

Key settings: `magent-default-agent` (`"build"`), `magent-enable-tools` (list of enabled tool symbols), `magent-org-roam-directory` (repository summary destination; nil falls back to `org-roam-directory`), `magent-include-reasoning` (`t`/`ignore`/`nil`), `magent-request-timeout` (120s), `magent-bash-timeout` (30s), `magent-emacs-eval-timeout` (10s), `magent-max-history` (100).

### Supported Frontend Commands

| Command | Action |
|---------|--------|
| `magent-agent-shell-dwim` | Open or reuse the project-local Magent agent-shell buffer |
| `magent-agent-shell-start` | Start a fresh Magent agent-shell buffer |
| `magent-agent-shell-prompt-region` | Send the active region through agent-shell |
| `magent-agent-shell-ask-at-point` | Ask about the symbol at point through agent-shell |
| `magent-agent-shell-interrupt` | Interrupt the active request |
| `magent-agent-shell-toggle-skill-for-next-request` | Toggle a one-shot instruction skill |
| `magent-agent-shell-run-skill-command` | Run a command-like skill |

Use agent-shell's own bindings, session options, mode selector, and slash
commands for interaction.

## Conventions

- All files use `;;; -*- lexical-binding: t; -*-`
- Use Conventional Commits for commit messages.
- Core, built-in agent, and internal runtime prompts are editable Org files under `prompt/`; every bundled Org prompt must also appear exactly once in `prompt/manifest.txt`; skill prompts remain self-contained in `skills/*/SKILL.md`
- Tool implementations follow pattern: `magent-tools--<name>` (internal fn) + `magent-tools--<name>-tool` (gptel-tool var)
- Byte-compile warnings suppressed: `cl-functions`
