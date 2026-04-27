# Magent API Reference

This file documents the current public surface of the in-repo Elisp modules.
It is intentionally concise and tracks the cleaned architecture: one gptel FSM
backend, project-scoped runtime overlays, file-backed skills/capabilities, and
an org-mode output buffer.

## Interactive Commands

### Primary Commands

#### `magent-dwim`
**Keybinding:** `C-c m p`

Open the Magent buffer and insert a prompt, optionally attaching structured
buffer context when `magent-auto-context` is enabled.

#### `magent-prompt-region`
**Keybinding:** `C-c m r`

Send the active region plus an optional prompt.

#### `magent-ask-at-point`
**Keybinding:** `C-c m a`

Ask about the symbol or form at point.

### Session And Agent Commands

#### `magent-clear-session`
**Keybinding:** `C-c m c`

Clear the active scoped session and local capability overrides.

#### `magent-resume-session`
**Keybinding:** `C-c m R`

Resume a saved session from disk.  Completion candidates include the saved
date and time.

#### `magent-select-agent`
**Keybinding:** `C-c m A`

Select the active session agent.

#### `magent-show-current-agent`
**Keybinding:** `C-c m i`

Show the current session agent.

#### `magent-list-agents`
**Keybinding:** `C-c m v`

List built-in and project-loaded agents.

### Capability Commands

#### `magent-list-capabilities-for-current-context`
**Keybinding:** `C-c m x`

Resolve and display capabilities for the current buffer context.

#### `magent-explain-last-capability-resolution`
**Keybinding:** `C-c m e`

Show the most recent capability resolution with scores and reasons.

#### `magent-toggle-capability-locally`
**Keybinding:** `C-c m k`

Toggle a capability on or off for the current Emacs session.

#### `magent-reload-capabilities`
**Keybinding:** None

Reload capability definitions from bundled, user, and project directories.

### Diagnostics And UI Commands

#### `magent-diagnose-emacs`
**Keybinding:** `C-c m d`

Start a structured diagnosis flow for the running Emacs session.

#### `magent-doctor`
**Keybinding:** `C-c m D`

Run Magent self-checks and emit a diagnosis prompt.

#### `magent-show-log`
**Keybinding:** `C-c m l`

Show the Magent log buffer.

#### `magent-clear-log`
**Keybinding:** `C-c m L`

Clear the Magent log buffer.

#### `magent-ui-toggle-section`
**Keybinding:** `C-c m t`

Fold or unfold the org section at point in `*magent*`.

### Skill Inspection Commands

#### `magent-list-skills`
**Keybinding:** None

List currently registered skills.

#### `magent-reload-skills`
**Keybinding:** None

Reload bundled, user, and project-local skills from disk.

#### `magent-describe-skill`
**Keybinding:** None

Show the full description, tools, prompt, and source path for one skill.

## Programmatic API

### Session Functions

#### `magent-session-reset`

```elisp
(magent-session-reset)
```

Reset the active scoped session.

#### `magent-session-add-message`

```elisp
(magent-session-add-message session role content)
```

Append one message to `session`.

- `session`: a `magent-session` struct
- `role`: one of `user`, `assistant`, or `tool`
- `content`: a string or a list of content blocks

#### `magent-session-save`

```elisp
(magent-session-save)
```

Persist the current scoped session to disk.

#### `magent-session-load`

```elisp
(magent-session-load filepath)
```

Load a saved session JSON file and activate it.

### Agent Functions

#### `magent-agent-process`

```elisp
(magent-agent-process prompt &optional callback)
```

Build the prompt, resolve skills/capabilities, and dispatch one request through
the gptel-backed FSM.

### Skill Functions

#### `magent-skills-get`

```elisp
(magent-skills-get name)
```

Return one skill struct by name.

#### `magent-skills-list`

```elisp
(magent-skills-list)
```

Return all skill names.

#### `magent-skills-load-file`

```elisp
(magent-skills-load-file filepath)
```

Load one `SKILL.md` definition.

#### `magent-skills-load-all`

```elisp
(magent-skills-load-all &optional directories)
```

Load all skill definitions from the given directories, or from the configured
skill search roots when `directories` is nil.

#### `magent-skills-reload`

```elisp
(magent-skills-reload)
```

Drop file-backed skills and reload them while preserving built-ins.

### Capability Functions

#### `magent-capability-load-file`

```elisp
(magent-capability-load-file filepath)
```

Load one `CAPABILITY.md` definition.

#### `magent-capability-load-all`

```elisp
(magent-capability-load-all &optional directories)
```

Load all capability definitions from bundled, user, or project paths.

#### `magent-capability-reload`

```elisp
(magent-capability-reload)
```

Reload file-backed capabilities in place.

#### `magent-capability-resolve`

```elisp
(magent-capability-resolve prompt context explicit-skills)
```

Score capabilities against prompt text plus structured context and return a
`magent-capability-resolution`.

### Permission And Tool Functions

#### `magent-permission-resolve`

```elisp
(magent-permission-resolve tool-name &optional file-path permissions)
```

Resolve `allow`, `deny`, or `ask` for a tool call.

#### `magent-tools-get-enabled`

```elisp
(magent-tools-get-enabled &optional permissions)
```

Return globally registered tools filtered by permissions.

## Data Structures

### `magent-agent-info`

Agent configuration struct defined in `magent-agent-registry.el`.

Key fields:
- `name`
- `description`
- `mode`
- `native`
- `hidden`
- `temperature`
- `top-p`
- `color`
- `model`
- `prompt`
- `options`
- `steps`
- `permission`
- `file-path`
- `source-layer`
- `source-scope`

### `magent-session`

Scoped conversation state defined in `magent-session.el`.

Key fields:
- `messages`
- `max-history`
- `id`
- `agent`
- `buffer-content`
- `approval-overrides`

### `magent-skill`

Skill definition struct defined in `magent-skills.el`.

Key fields:
- `name`
- `description`
- `type`
- `tools`
- `prompt`
- `invoke-function`
- `file-path`
- `source-layer`
- `source-scope`

### `magent-capability`

Capability definition struct defined in `magent-capability.el`.

Key fields:
- `name`
- `title`
- `description`
- `family`
- `source-kind`
- `source-layer`
- `source-scope`
- `source-name`
- `skills`
- `modes`
- `features`
- `files`
- `prompt-keywords`
- `disclosure`
- `risk`
- `notes`
- `file-path`

### `magent-capability-resolution`

Resolver output struct defined in `magent-capability.el`.

Key fields:
- `prompt`
- `context`
- `explicit-skills`
- `matches`
- `active-capabilities`
- `suggested-capabilities`
- `skill-names`

## Key Customization Variables

### Core Settings

- `magent-buffer-name`
- `magent-default-agent`
- `magent-enable-tools`
- `magent-fsm-backend` (single supported value: `gptel`)

### Session, Audit, And History

- `magent-session-directory`
- `magent-audit-directory`
- `magent-enable-audit-log`
- `magent-max-history`

### Timeouts

- `magent-request-timeout`
- `magent-bash-timeout`
- `magent-emacs-eval-timeout`

### UI

- `magent-auto-scroll`
- `magent-ui-fontify-threshold`
- `magent-ui-batch-insert-delay`
- `magent-include-reasoning`

### Search

- `magent-grep-program`
- `magent-grep-max-matches`

### `magent-mode-hook`
Run when `magent-mode` is enabled.

### `magent-output-mode-hook`
Run when output buffer is created.

## Faces

- `magent-user-face` — User message headers
- `magent-assistant-face` — Assistant message headers
- `magent-tool-call-face` — Tool call markers
- `magent-error-face` — Error message headers
