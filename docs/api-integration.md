# LLM Integration Guide

This document describes how Magent integrates with LLM providers through [gptel](https://github.com/karthink/gptel).

## Overview

Magent delegates all LLM communication to gptel, an Emacs package for interacting with large language models. gptel handles provider-specific APIs, message formatting, tool calling loops, and streaming. Magent builds on top of gptel by adding per-agent configuration overrides, permission-based tool filtering, and session management.

## Supported Providers

Any provider supported by gptel works with Magent. Common options:

### Anthropic Claude

**Environment Variable:** `ANTHROPIC_API_KEY`

**Models:**
- `claude-opus-4-20250514` (most capable)
- `claude-sonnet-4-20250514` (balanced, default)
- `claude-haiku-4-20250514` (fastest)

### OpenAI

**Environment Variable:** `OPENAI_API_KEY`

**Models:**
- `gpt-4-turbo`
- `gpt-4`
- `gpt-3.5-turbo`

### Other Providers

gptel supports many additional providers including Ollama (local models), Azure OpenAI, Google Gemini, and more. See [gptel's README](https://github.com/karthink/gptel#supported-llm-backends) for the full list.

## Configuration

### Basic Setup

All provider, model, and API key configuration is done through gptel:

```elisp
;; Anthropic (default gptel backend)
(setq gptel-api-key "sk-ant-...")
(setq gptel-model 'claude-sonnet-4-20250514)

;; Or use environment variables
;; export ANTHROPIC_API_KEY="sk-ant-..."
```

For non-default providers, configure gptel backends:

```elisp
;; OpenAI
(setq gptel-backend
      (gptel-make-openai "OpenAI"
        :key "sk-..."
        :models '(gpt-4-turbo gpt-4 gpt-3.5-turbo)))

;; Local model via Ollama
(setq gptel-backend
      (gptel-make-ollama "Ollama"
        :host "localhost:11434"
        :models '(llama3 codellama)))
```

See [gptel configuration docs](https://github.com/karthink/gptel#configuration) for complete setup instructions.

### Per-Agent Overrides

Individual agents can override the default model and temperature:

```elisp
;; In a custom agent file (.magent/agent/myagent.md)
---
model: claude-haiku-4-20250514
temperature: 0.3
---
```

Or programmatically:

```elisp
(setf (magent-agent-info-model agent-info) "claude-haiku-4-20250514")
(setf (magent-agent-info-temperature agent-info) 0.3)
```

These overrides are applied temporarily during the request via `magent-agent-info-apply-gptel-overrides`, which sets the relevant gptel variables for the duration of the `gptel-request` call.

## How Magent Uses gptel

### Request Flow

```
magent-agent-process
    |
Build prompt list from session (magent-session-to-gptel-prompt-list)
    |
Get system prompt from agent (or magent-system-prompt)
    |
Filter tools by agent permissions (magent-tools-get-gptel-tools)
    |
Apply per-agent overrides (magent-agent-info-apply-gptel-overrides)
    |
gptel-request (prompt-list, :system, :stream nil, :callback)
    |
gptel handles LLM communication and tool-calling loop
    |
Callback receives: string response, tool results, or error
```

### Tool Registration

Tools are registered as `gptel-tool` structs using `gptel-make-tool`:

```elisp
(gptel-make-tool
 :name "read_file"
 :description "Read the contents of a file at the given path."
 :args (list '(:name "path"
               :type string
               :description "Absolute or relative path to the file"))
 :function #'magent-tools--read-file
 :category "magent")
```

Tools that modify the system (`write_file`, `bash`) include `:confirm t` so gptel can prompt for user confirmation.

### Tool Filtering

Not all tools are available to all agents. `magent-tools-get-gptel-tools` filters the global tool list by:

1. **Global config**: Only tools in `magent-enable-tools` are considered
2. **Agent permissions**: Only tools the agent's permission rules allow

### Callback Handling

`magent-agent--make-callback` creates a gptel callback that handles:

- **String response**: Final text -- added to session, passed to UI
- **Tool results**: Tools have executed, gptel continues the loop -- displayed in UI
- **Tool calls pending**: Awaiting user confirmation -- logged
- **Abort/error**: Request failed -- displayed as error

## Logging

### Enable Logging

```elisp
(setq magent-enable-logging t)
```

### View Logs

```elisp
M-x magent-view-log
```

### Clear Logs

```elisp
M-x magent-clear-log
```

Magent logs agent activity to the `*magent-log*` buffer. For lower-level HTTP debugging, enable Emacs URL library debug mode:

```elisp
(setq url-debug t)
```

## Cost Optimization

### Token Usage

Monitor approximate token usage through session context size:

```elisp
(magent-session-get-context-size session)
;; => Approximate token count
```

### Model Selection

Choose appropriate models per agent:
- **Simple tasks**: Use faster/cheaper models (Haiku, GPT-3.5)
- **Complex tasks**: Use more capable models (Opus, GPT-4)
- **Code generation**: Balanced models work well (Sonnet, GPT-4-turbo)

Per-agent model overrides in custom agent files:

```yaml
---
model: claude-haiku-4-20250514
---
```

## Troubleshooting

### API Key Not Found

Ensure gptel has the correct API key:

```elisp
gptel-api-key  ; Check current key
(setq gptel-api-key "your-key")
;; or set ANTHROPIC_API_KEY / OPENAI_API_KEY environment variable
```

### Wrong Model or Provider

Check gptel configuration:

```elisp
gptel-model    ; Current model
gptel-backend  ; Current backend/provider
```

### Agent Using Wrong Model

Per-agent model overrides take precedence. Check the agent's model field:

```elisp
(magent-agent-info-model (magent-agent-registry-get "myagent"))
```

Priority order:
1. Agent-specific model override
2. gptel default (`gptel-model`)

### Connection Issues

For HTTP-level debugging:

```elisp
(setq url-debug t)
```

Check gptel's own troubleshooting: `M-x gptel` should work independently of Magent.
