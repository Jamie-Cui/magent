# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
make compile    # Byte-compile all Elisp files
make test       # Run ERT tests in batch mode
make clean      # Remove compiled .elc files
```

The Makefile auto-detects dependency paths (`gptel`, `spinner`, `transient`, `cond-let`) by scanning `~/.emacs.d/elpa/`. Override any with e.g. `GPTEL_DIR=/path/to/gptel`.

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

`test/magent-test.el` contains ~92 ERT tests across 10 suites. Tests mock `gptel-request` and UI functions via `cl-letf`. Key patterns:
- Registry tests bind `magent-agent-registry--agents` to a fresh hash table
- Skills tests bind `magent-skills--registry` to nil
- Session tests call `magent-session-reset` to clear global state
- FSM tool/permission tests should load `magent-fsm-tools`; compatibility tests may still load `magent-fsm-shared`

### End-to-End Testing

After elisp code changes, test in the running Emacs via `emacsclient --eval`:

1. Reload: `(load "/path/to/changed-file.el" nil t)`
2. Clear: `(magent-clear-session)`
3. Test prompts:
   - Non-tool: `"你好"` — verifies streaming text and assistant section
   - Tool-use: `"帮我看下 emacs 里面有多少 buffer"` — verifies `emacs_eval` tool calling, UI rendering, FSM
   - Multi-step: `"帮我在 emacs 里面打开 magent 的 magit buffer"` — verifies chained tool execution
4. Check `*magent*`, `*magent-log*`, and `*Messages*` for errors

## Architecture

Magent is an Emacs Lisp AI coding agent with a multi-agent architecture and permission-based tool access. All LLM communication is delegated to **gptel** (the sole external dependency beyond Emacs 27.1+, `spinner`, and `transient`).

### Module Dependency Graph

```
magent.el (entry point: magent-mode, global-magent-mode)
  ├─ magent-config.el     (defcustoms, deffaces, shared utilities, magent-log stub)
  ├─ magent-runtime.el    (static init + project overlay activation)
  ├─ magent-session.el    (conversation state, JSON persistence)
  ├─ magent-capability.el (capability registry and prompt-time resolution)
  ├─ magent-tools.el      (10 gptel-tool structs)
  ├─ magent-agent.el      (magent-agent-process: builds gptel prompt, calls gptel-request)
  ├─ magent-agent-registry.el  (cl-defstruct, 7 built-in agents, hash-table registry)
  ├─ magent-agent-info.el / magent-agent-types.el  (legacy feature-name compatibility shims)
  ├─ magent-agent-file.el      (loads custom agents from .magent/agent/*.md)
  ├─ magent-permission.el      (rule-based tool access control per agent)
  ├─ magent-ui.el              (in-buffer input/output, org-mode derived, overlay sections)
  ├─ magent-fsm.el             (active gptel FSM implementation + public API)
  ├─ magent-fsm-tools.el       (tool wrapping, queueing, permission flow, abort helpers)
  ├─ magent-file-loader.el     (shared file-backed definition loader and frontmatter parser)
  ├─ magent-md2org.el          (markdown → org-mode conversion for assistant output)
  └─ magent-skills.el          (skill registry, built-in skill definitions, file loading, and interactive commands)
```

### Core Flow

1. **Entry point** (`magent.el`): `magent-mode` minor mode with `C-c m` prefix. Mode enable only adds a modeline construct. Static initialization is **lazy** — triggered on first command via `magent--ensure-initialized`.

2. **Runtime** (`magent-runtime.el`): Owns the ordered initialization pipeline for agents, skills, and capabilities. Static bundled definitions load once; project-local overlays under `.magent/` are activated and unloaded as session scope changes.

3. **UI** (`magent-ui.el`): The `*magent*` buffer derives from `org-mode` (`magent-output-mode`). Uses **in-buffer input**: `magent-prompt` inserts an editable `* [USER]` section at buffer end; `C-c C-c` submits. Past content is read-only.
   - Message sections use level-1 org headings with custom faces
   - Tool calls render as `#+begin_tool`/`#+end_tool` blocks (auto-folded via deferred `org-cycle`)
   - Reasoning blocks render as `#+begin_think`/`#+end_think` (auto-folded)
   - Streaming uses chunk batching (`magent-ui-batch-insert-delay`) and async fontification above `magent-ui-fontify-threshold`
   - Request serialization is owned here via `magent-ui--processing` and `magent-ui--enqueue`; concurrent prompts are rejected with a busy message instead of buffered
   - `?` opens a transient menu; `TAB`/`S-TAB` fold sections; `C-g` interrupts

4. **Agent processing** (`magent-agent.el`): `magent-agent-process` builds a gptel prompt list from the session, applies per-agent overrides (model, temperature via `default-value` — intentionally avoids buffer-local gptel settings), filters tools by permissions, then calls `gptel-request`.

5. **Tools** (`magent-tools.el`): 10 `gptel-tool` structs — `read_file`, `write_file`, `edit_file`, `grep`, `glob`, `bash`, `emacs_eval`, `delegate`, `skill_invoke`, `web_search`. Tools are registered globally but filtered per-agent through permissions. `web_search` uses DuckDuckGo via `url-retrieve` + `libxml-parse-html-region` (requires Emacs built with `--with-xml2`).

6. **FSM** (`magent-fsm.el`): Only the **gptel backend** is active. The full request lifecycle, streaming callback, and gptel advice now live in `magent-fsm.el`. Tool wrapping, permission prompting, queueing, and abort helpers live in `magent-fsm-tools.el`. The legacy features `magent-fsm-backend-gptel` and `magent-fsm-shared` remain as compatibility shims.

7. **Permissions** (`magent-permission.el`): Rules map tool names to `allow`/`deny`/`ask`, with optional file-pattern sub-rules (glob syntax). Resolution: exact tool match → file-pattern rules → wildcard (`*`) fallback → **default allow**. File-pattern rules are order-dependent (first match wins); more specific patterns must come before less specific ones.

8. **Capabilities** (`magent-capability.el`): File-backed capability definitions score the current request context and attach matching instruction skills. Bundled, user, and project-local capability overlays all feed the same resolver.

9. **Session** (`magent-session.el`): Conversation state with messages list and history trimming. Persists to `magent-session-directory` as JSON. The `buffer-content` slot stores raw buffer text for lossless restore (preserving tool/reasoning blocks not in the message list). `magent-session-reset` clears the scoped session plus approval and capability overrides.

10. **Skills** (`magent-skills.el`): Two types — `instruction` (markdown injected into system prompt) and `tool` (invoked via `skill_invoke`). The module now contains the registry, built-in `skill-creator`, file-based skill loading, and interactive inspection commands. Skills load in priority order from (1) built-in `skills/`, (2) user directory `~/.emacs.d/magent-skills/<name>/SKILL.md`, and (3) project-local `.magent/skills/<name>/SKILL.md`.

### Gotchas

- **`magent-output-mode` derives from `org-mode`**: All org keybindings, font-lock, and folding apply. Use `inhibit-read-only` for insertions; org fontification can trigger re-entrancy.
- **`magent-ui--with-insert` suppresses buffer-boundary signals**: Catches `beginning-of-buffer`, `end-of-buffer`, etc. to suppress evil-mode cursor adjustment errors from gptel process filters.
- **`magent-log` is a stub in `magent-config.el`**: No-op until `magent-ui` is loaded. In batch tests, logs go nowhere unless you explicitly load `magent-ui`.
- **Tool execution helpers live in `magent-fsm-tools.el`**: `magent-fsm-backend-gptel.el` and `magent-fsm-shared.el` are compatibility shims only.
- **`magent--handle-unknown-tools-a`**: An `around` advice on `gptel--handle-tool-use` that pre-fills error results for hallucinated tool names, preventing FSM hangs.
- **Request generation counter**: `magent-ui--request-generation` increments on dispatch and interrupt. Stale callbacks compare their captured generation and discard themselves if mismatched.
- **`revert-buffer` in output buffer**: Bound to `magent-ui--revert-buffer` → `magent-ui-render-history`. Safe to use `g` in evil mode.

### Agent Definitions

Built-in agents: `build` (default), `plan`, `explore`, `general`, `compaction`, `title`, `summary`. Defined in `magent-agent-registry.el` with `cl-defstruct magent-agent-info` (fields: name, description, mode, native, hidden, temperature, top-p, color, model, prompt, options, steps, permission). `magent-agent-info.el` and `magent-agent-types.el` are now only compatibility shims for older `require` forms. Agent modes: `primary` (user-facing), `subagent` (internal), `all` (either).

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

Key settings: `magent-fsm-backend` (only `gptel` currently), `magent-default-agent` (`"build"`), `magent-enable-tools` (list of enabled tool symbols), `magent-include-reasoning` (`t`/`ignore`/`nil`), `magent-request-timeout` (120s), `magent-bash-timeout` (30s), `magent-emacs-eval-timeout` (10s), `magent-max-history` (100).

### Keybindings

`C-c m` prefix in `magent-mode`:

| Key | Command |
|-----|---------|
| `C-c m p` | `magent-dwim` |
| `C-c m d` | `magent-diagnose-emacs` |
| `C-c m D` | `magent-doctor` |
| `C-c m r` | `magent-prompt-region` |
| `C-c m a` | `magent-ask-at-point` |
| `C-c m c` | `magent-clear-session` |
| `C-c m l` | `magent-show-log` |
| `C-c m L` | `magent-clear-log` |
| `C-c m t` | `magent-ui-toggle-section` |
| `C-c m A` | `magent-select-agent` |
| `C-c m i` | `magent-show-current-agent` |
| `C-c m v` | `magent-list-agents` |

In `magent-output-mode`: `TAB` fold/unfold, `S-TAB` toggle all, `?` transient menu, `C-g` interrupt.

## Conventions

- All files use `;;; -*- lexical-binding: t; -*-`
- System prompt loaded from `prompt.org` adjacent to `magent-config.el`
- Tool implementations follow pattern: `magent-tools--<name>` (internal fn) + `magent-tools--<name>-tool` (gptel-tool var)
- Byte-compile warnings suppressed: `cl-functions`, `obsolete`
