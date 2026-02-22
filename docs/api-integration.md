# API Integration Guide

This document describes how Magent integrates with LLM providers (Anthropic Claude, OpenAI GPT, and OpenAI-compatible APIs).

## Overview

The API layer (`magent-api.el`) provides a unified interface to multiple LLM providers while handling provider-specific message formats, tool definitions, and response parsing.

## Supported Providers

### Anthropic Claude

**Provider ID:** `'anthropic`

**API Endpoint:** `https://api.anthropic.com/v1/messages`

**Authentication:** API key via `x-api-key` header

**Environment Variable:** `ANTHROPIC_API_KEY`

**Models:**
- `claude-opus-4-20250514` (most capable)
- `claude-sonnet-4-20250514` (balanced, default)
- `claude-haiku-4-20250514` (fastest)

### OpenAI

**Provider ID:** `'openai`

**API Endpoint:** `https://api.openai.com/v1/chat/completions`

**Authentication:** Bearer token via `Authorization` header

**Environment Variable:** `OPENAI_API_KEY`

**Models:**
- `gpt-4-turbo`
- `gpt-4`
- `gpt-3.5-turbo`

### OpenAI-Compatible

**Provider ID:** `'openai-compatible`

**API Endpoint:** Configured via `magent-base-url`

**Authentication:** Bearer token via `Authorization` header

**Use Cases:**
- Local LLMs (LM Studio, Ollama with OpenAI compatibility)
- Alternative providers (Azure OpenAI, etc.)
- Self-hosted models

## Configuration

### Basic Setup

```elisp
;; For Anthropic
(setq magent-provider 'anthropic)
(setq magent-api-key "sk-ant-...")
(setq magent-model "claude-sonnet-4-20250514")

;; For OpenAI
(setq magent-provider 'openai)
(setq magent-api-key "sk-...")
(setq magent-model "gpt-4-turbo")

;; For OpenAI-compatible
(setq magent-provider 'openai-compatible)
(setq magent-base-url "http://localhost:1234/v1")
(setq magent-api-key "not-needed-for-local")
(setq magent-model "local-model-name")
```

### Environment Variables

Instead of setting `magent-api-key` directly, you can use environment variables:

```bash
# In your shell profile
export ANTHROPIC_API_KEY="sk-ant-..."
export OPENAI_API_KEY="sk-..."
```

Magent will automatically detect and use these variables.

### Validation

Check if credentials are configured:

```elisp
M-x magent-api-set-credentials
```

Or programmatically:

```elisp
(magent-get-api-key)  ; Returns key or nil
```

## Message Format

### Internal Format

Magent uses a consistent internal message format:

```elisp
;; Simple text message
((role . "user")
 (content . "Hello, world!"))

;; Structured content (Anthropic-style)
((role . "assistant")
 (content . (((type . "text")
              (text . "Response text"))
             ((type . "tool_use")
              (id . "call_123")
              (name . "read_file")
              (input . ((path . "/foo/bar.el")))))))

;; Tool result
((role . "tool")
 (content . ((type . "tool_result")
             (tool_use_id . "call_123")
             (content . "File contents..."))))
```

### Provider-Specific Conversion

#### Anthropic Messages API

**Request format:**
```json
{
  "model": "claude-sonnet-4-20250514",
  "max_tokens": 8192,
  "temperature": 0.7,
  "messages": [
    {
      "role": "user",
      "content": "Hello"
    }
  ],
  "tools": [
    {
      "name": "read_file",
      "description": "Read file contents",
      "input_schema": {
        "type": "object",
        "properties": {
          "path": {
            "type": "string",
            "description": "File path"
          }
        },
        "required": ["path"]
      }
    }
  ]
}
```

**Response format:**
```json
{
  "id": "msg_123",
  "type": "message",
  "role": "assistant",
  "content": [
    {
      "type": "text",
      "text": "I'll read that file."
    },
    {
      "type": "tool_use",
      "id": "call_123",
      "name": "read_file",
      "input": {
        "path": "/foo/bar.el"
      }
    }
  ]
}
```

#### OpenAI Chat Completions API

**Request format:**
```json
{
  "model": "gpt-4-turbo",
  "max_tokens": 8192,
  "temperature": 0.7,
  "messages": [
    {
      "role": "user",
      "content": "Hello"
    }
  ],
  "tools": [
    {
      "type": "function",
      "function": {
        "name": "read_file",
        "description": "Read file contents",
        "parameters": {
          "type": "object",
          "properties": {
            "path": {
              "type": "string",
              "description": "File path"
            }
          },
          "required": ["path"]
        }
      }
    }
  ]
}
```

**Response format:**
```json
{
  "id": "chatcmpl-123",
  "object": "chat.completion",
  "choices": [
    {
      "message": {
        "role": "assistant",
        "content": "I'll read that file.",
        "tool_calls": [
          {
            "id": "call_123",
            "type": "function",
            "function": {
              "name": "read_file",
              "arguments": "{\"path\": \"/foo/bar.el\"}"
            }
          }
        ]
      }
    }
  ]
}
```

## Tool Definitions

### Internal Tool Schema

Tools are defined with this structure:

```elisp
((name . "read_file")
 (description . "Read the contents of a file")
 (input_schema
   (type . "object")
   (properties . ((path . ((type . "string")
                           (description . "File path")))))
   (required . (path))
   (additionalProperties . :json-false)))
```

### Provider Conversion

#### Anthropic

Uses `input_schema` directly (native format).

#### OpenAI

Converts to function calling format:

```elisp
(defun magent-api--convert-tools (tools)
  "Convert internal tool definitions to OpenAI format."
  (mapcar (lambda (tool)
            `((type . "function")
              (function . ((name . ,(cdr (assq 'name tool)))
                          (description . ,(cdr (assq 'description tool)))
                          (parameters . ,(cdr (assq 'input_schema tool)))))))
          tools))
```

## API Calls

### Chat Completion

Main API function:

```elisp
(cl-defun magent-api-chat (messages
                           &key tools stream model
                                max-tokens temperature
                                callback)
  "Send chat completion request to LLM provider.")
```

**Parameters:**
- `messages`: List of message objects
- `tools`: List of available tool definitions (optional)
- `stream`: Enable streaming (not fully implemented)
- `model`: Override default model
- `max-tokens`: Maximum response tokens
- `temperature`: Sampling temperature (0.0-1.0)
- `callback`: Function called with response

**Example:**

```elisp
(magent-api-chat
 '(((role . "user") (content . "Hello")))
 :tools (magent-tools-get-definitions)
 :callback (lambda (response)
             (message "Got: %s"
                     (magent-api--extract-content response))))
```

### Response Handling

Responses are parsed and normalized:

```elisp
;; Extract text content
(magent-api--extract-content response)
;; => "Response text"

;; Extract tool uses
(magent-api--extract-tool-uses response)
;; => (((id . "call_123")
;;      (name . "read_file")
;;      (input . ((path . "/foo/bar.el")))))
```

### Error Handling

API errors are detected and thrown:

```elisp
(when (assq 'error response)
  (error "API error: %s" (cdr (assq 'error response))))
```

Common error scenarios:
- Invalid API key → Authentication error
- Rate limiting → 429 status code
- Model not found → Invalid model error
- Malformed request → 400 Bad Request

## Logging

### Enable Logging

```elisp
(setq magent-enable-logging t)
```

### View Logs

```elisp
M-x magent-view-log
```

### Log Contents

Logs include:
- Request timestamps
- API endpoints
- Message counts
- Request bodies (JSON)
- Response bodies (JSON)
- Error messages

**Example log:**

```
[14:32:15] [API] Sending request to: https://api.anthropic.com/v1/messages
[14:32:15] [API] Messages count: 3
[14:32:15] [API] Request body: {"model":"claude-sonnet-4-20250514",...}
[14:32:17] [API] Response received: {"id":"msg_123",...}
```

### Clear Logs

```elisp
M-x magent-clear-log
```

## Streaming Support

### Current Status

Basic streaming infrastructure exists but is not fully integrated with the agent loop.

### Streaming API

```elisp
(magent-api-chat messages
  :stream t
  :callback (lambda (chunk)
              (insert chunk)))
```

### How Streaming Works

1. **Server-Sent Events (SSE)**: Both providers use SSE for streaming
2. **Incremental Deltas**: Receive partial responses as they're generated
3. **Event Parsing**: Parse `data:` lines from SSE stream
4. **Delta Extraction**: Extract text deltas from events

### Future Enhancement

Full streaming support would require:
1. Buffer management for incremental updates
2. Tool use detection in streaming mode
3. Partial JSON parsing for tool arguments
4. UI updates during generation

## Rate Limiting

### Provider Limits

**Anthropic:**
- Rate limits vary by plan tier
- Includes request/minute and token/minute limits
- Returns 429 with retry-after header

**OpenAI:**
- Rate limits vary by model and plan
- Measured in requests/minute and tokens/minute
- Returns 429 with retry-after header

### Handling Rate Limits

Currently, Magent does not implement automatic retry logic. Rate limit errors propagate to the user.

**Future enhancement:**
```elisp
;; Proposed retry logic
(when (eq status 429)
  (let ((retry-after (cdr (assq 'retry-after headers))))
    (sleep-for (string-to-number retry-after))
    (magent-api-chat ...)))  ; Retry request
```

## Cost Optimization

### Token Usage

Monitor token usage through logging:

```elisp
(magent-session-get-context-size session)
;; => Approximate token count
```

### Model Selection

Choose appropriate models:
- **Simple tasks**: Use faster/cheaper models (Haiku, GPT-3.5)
- **Complex tasks**: Use more capable models (Opus, GPT-4)
- **Code generation**: Balanced models work well (Sonnet, GPT-4-turbo)

### Agent-Specific Models

Override model per agent:

```elisp
;; In custom agent file
---
model: claude-haiku-4-20250514
---
```

Or programmatically:

```elisp
(setf (magent-agent-info-model agent-info) "claude-haiku-4-20250514")
```

## Testing

### Mock Responses

For testing without API calls:

```elisp
;; Mock API function
(cl-defun magent-api-chat (messages &rest _)
  (funcall callback '((content . "Mock response"))))
```

### Local Models

Test with local models:

```elisp
(setq magent-provider 'openai-compatible)
(setq magent-base-url "http://localhost:1234/v1")
(setq magent-model "local-model")
```

### Debug Mode

Enable verbose logging:

```elisp
(setq magent-enable-logging t)
(setq url-debug t)  ; Emacs url library debugging
```

## Troubleshooting

### API Key Not Found

```
Error: API key not set
```

**Solution:**
```elisp
(setq magent-api-key "your-key")
;; or
(setenv "ANTHROPIC_API_KEY" "your-key")
```

### Connection Timeout

```
Error: Connection timeout
```

**Solution:**
```elisp
(setq magent-api--request-timeout 180)  ; Increase timeout
```

### Invalid Model

```
Error: Model not found
```

**Solution:** Verify model name matches provider's models:
```elisp
;; Check current model
magent-model

;; Update to valid model
(setq magent-model "claude-sonnet-4-20250514")
```

### SSL Certificate Issues

```
Error: Certificate verification failed
```

**Solution:**
```elisp
;; Temporary workaround (not recommended for production)
(setq tls-checktrust nil)
```

### Rate Limit Exceeded

```
Error: Rate limit exceeded (429)
```

**Solution:** Wait and retry, or upgrade API plan tier.

## Advanced Usage

### Custom Headers

Add custom headers to requests:

```elisp
;; Modify magent-api--get-headers
(defun magent-api--get-headers ()
  `(,@(default-headers)
    ("x-custom-header" . "value")))
```

### Request Interceptors

Modify requests before sending:

```elisp
;; Hook into url-request-data
(advice-add 'magent-api-chat :before
  (lambda (&rest _)
    (message "Sending request...")))
```

### Response Transformers

Transform responses after receiving:

```elisp
(advice-add 'magent-api--parse-response :filter-return
  (lambda (response)
    ;; Add custom processing
    response))
```

## Provider Comparison

| Feature | Anthropic | OpenAI | OpenAI-Compatible |
|---------|-----------|--------|-------------------|
| Tool Calling | Native | Native | Varies by provider |
| Streaming | Yes (SSE) | Yes (SSE) | Varies |
| Context Window | Up to 200K tokens | Up to 128K tokens | Varies |
| Response Format | Structured blocks | Simple content | Usually OpenAI format |
| Image Support | Yes | Yes | Varies |
| Function Calling | tool_use blocks | tool_calls array | Usually tool_calls |

## Best Practices

1. **Use Environment Variables**: Keep API keys out of code
2. **Enable Logging**: Monitor requests during development
3. **Handle Errors**: Wrap API calls in error handlers
4. **Choose Appropriate Models**: Balance cost and capability
5. **Monitor Token Usage**: Track costs through logging
6. **Test Locally First**: Use local models for development
7. **Implement Retries**: Handle transient failures gracefully
8. **Cache When Possible**: Reduce redundant API calls
