# Session Management

This document describes how Magent manages conversation state, message history, and session persistence.

## Overview

Sessions in Magent maintain the context and history of conversations with AI agents. Each session:

- Stores conversation messages (user, assistant, tool results)
- Tracks the assigned agent
- Maintains a unique identifier
- Can be persisted to disk and resumed
- Automatically manages history size

## Session Structure

### Data Structure

Sessions are defined using `cl-defstruct`:

```elisp
(cl-defstruct magent-session
  messages        ; List of message objects (newest first)
  max-history     ; Maximum messages to retain
  id              ; Unique session identifier
  agent)          ; Currently assigned agent info
```

### Message Format

Messages follow a consistent internal format:

```elisp
;; User message
((role . user)
 (content . "Please read main.el"))

;; Assistant message with text
((role . assistant)
 (content . "I'll read that file for you."))

;; Assistant message with tool use
((role . assistant)
 (content . (((type . "text")
              (text . "I'll read that file."))
             ((type . "tool_use")
              (id . "call_123")
              (name . "read_file")
              (input . ((path . "main.el")))))))

;; Tool result
((role . tool)
 (content . ((type . "tool_result")
             (tool_use_id . "call_123")
             (content . ";;; main.el contents..."))))
```

## Session Lifecycle

### 1. Creation

Sessions are created lazily when first needed:

```elisp
(defun magent-session-get ()
  "Get the current session, creating one if needed."
  (unless magent--current-session
    (setq magent--current-session (magent-session-create)))
  magent--current-session)
```

**Properties of new sessions:**
- Empty message list
- Default max-history: `magent-max-history` (100)
- No ID until first save
- No agent assignment (will use default)

### 2. Agent Assignment

Agents are assigned when first processing a prompt:

```elisp
;; Automatic assignment to default agent
(magent-agent-process "prompt")  ; Uses default agent

;; Explicit agent selection
(magent-select-agent)  ; Interactive selection
```

**Agent Persistence:**
- Agent stays assigned for session lifetime
- Can be changed with `magent-select-agent`
- Saved with session for resumption

### 3. Message Accumulation

Messages are added during conversation:

```elisp
;; User message
(magent-session-add-message session 'user "Hello")

;; Assistant message
(magent-session-add-message session 'assistant "Hi there!")

;; Tool result
(magent-session-add-tool-result session "call_123" "File contents...")
```

**Message ordering:**
- Internal storage: newest first (prepend)
- API usage: oldest first (reversed)
- Automatic trimming to max-history

### 4. Persistence

Sessions can be saved to disk:

```elisp
;; Save current session
(magent-session-save session)
;; → ~/.emacs.d/magent-sessions/session-20250221-143015.json

;; Load saved session
(magent-session-load "/path/to/session.json")
```

### 5. Reset/Clear

Sessions can be cleared:

```elisp
;; Interactive
M-x magent-clear-session

;; Programmatic
(magent-session-reset)
```

## Session Operations

### Creating Sessions

**Automatic (recommended):**
```elisp
(let ((session (magent-session-get)))
  ;; Use session
  )
```

**Manual:**
```elisp
(let ((session (magent-session-create
                :max-history 50
                :agent (magent-agent-registry-get "explore"))))
  ;; Use session
  )
```

### Adding Messages

**User messages:**
```elisp
(magent-session-add-message session 'user "Find all .el files")
```

**Assistant messages:**
```elisp
(magent-session-add-message session 'assistant
  "I'll search for .el files using glob.")
```

**Tool results:**
```elisp
(magent-session-add-tool-result session "call_123"
  "file1.el\nfile2.el\nfile3.el")
```

### Retrieving Messages

**All messages (chronological order):**
```elisp
(magent-session-get-messages session)
;; Returns messages oldest to newest
```

**Message count:**
```elisp
(length (magent-session-messages session))
```

**Latest message:**
```elisp
(car (magent-session-messages session))
```

### Agent Management

**Get assigned agent:**
```elisp
(magent-session-get-agent session)
;; Returns magent-agent-info or nil
```

**Set agent:**
```elisp
(magent-session-set-agent session
  (magent-agent-registry-get "plan"))
```

**Show current agent:**
```elisp
M-x magent-show-current-agent
```

### Session Information

**Get/generate session ID:**
```elisp
(magent-session-get-id session)
;; Returns existing ID or generates new one
;; Format: "session-YYYYMMDD-HHMMSS"
```

**Estimate context size:**
```elisp
(magent-session-get-context-size session)
;; Returns approximate token count (~4 chars per token)
```

**Summarize session:**
```elisp
(magent-session-summarize session)
;; Returns text summary of recent messages
```

## History Management

### Automatic Trimming

Sessions automatically trim to `max-history` messages:

```elisp
(defun magent-session-add-message (session role content)
  "Add a message to SESSION."
  (let ((messages (magent-session-messages session)))
    (push (list (cons 'role role)
                (cons 'content content))
          messages)
    ;; Trim to max history
    (when (> (length messages) (magent-session-max-history session))
      (setf messages (butlast messages
                      (- (length messages)
                         (magent-session-max-history session)))))
    (setf (magent-session-messages session) messages))
  session)
```

**Behavior:**
- Oldest messages removed first
- Trimming happens on every message add
- No orphaned tool uses/results (limitation)

### Configuring History Size

**Global default:**
```elisp
(setq magent-max-history 200)  ; Default: 100
```

**Per-session:**
```elisp
(let ((session (magent-session-create :max-history 50)))
  ;; This session limited to 50 messages
  )
```

**Disable trimming:**
```elisp
(setf (magent-session-max-history session) most-positive-fixnum)
```

### Context Size Management

**Monitor context size:**
```elisp
(let ((size (magent-session-get-context-size session)))
  (message "Approximate tokens: %d" size))
```

**When to compact:**
- Context size approaching model limit (e.g., 8K for Sonnet)
- Many tool calls accumulating
- Session feels "slow" (large context)

**Manual compaction** (future feature):
```elisp
;; Use compaction agent to summarize
(magent-agent-process-with-agent "Summarize this conversation"
                                 "compaction")
```

## Session Persistence

### Save Location

**Default directory:**
```elisp
~/.emacs.d/magent-sessions/
```

**Filename format:**
```
session-YYYYMMDD-HHMMSS.json
```

### Saved Content

**JSON structure:**
```json
{
  "id": "session-20250221-143015",
  "messages": [
    {
      "role": "user",
      "content": "Hello"
    },
    {
      "role": "assistant",
      "content": "Hi there!"
    }
  ],
  "timestamp": "2025-02-21T14:30:15-0800"
}
```

**Note:** Agent assignment is NOT currently persisted (limitation).

### Saving Sessions

**Manual save:**
```elisp
(magent-session-save session)
;; Returns saved filepath

;; Save to specific location
(magent-session-save session "/custom/path/session.json")
```

**Automatic save** (not implemented):
Future enhancement to auto-save after each interaction.

### Loading Sessions

**Load from file:**
```elisp
(let ((session (magent-session-load "/path/to/session.json")))
  (setq magent--current-session session))
```

**List saved sessions:**
```elisp
(magent-session-list-saved)
;; Returns list of .json files in sessions directory
```

**Interactive load** (not implemented):
Future enhancement for session browser UI.

## Multi-Session Management

### Current Limitations

Magent currently uses a single global session:

```elisp
(defvar magent--current-session nil
  "The current active session.")
```

**Implications:**
- Only one conversation active at a time
- Clearing session affects all buffers
- No per-buffer or per-project sessions

### Future Enhancements

Potential multi-session support:

```elisp
;; Per-buffer sessions
(defvar-local magent-buffer-session nil)

;; Per-project sessions
(defvar magent-project-sessions (make-hash-table :test 'equal))

;; Session manager UI
(magent-session-manager)  ; Browse/switch sessions
```

## Session Display

### Show Session Summary

**Interactive:**
```elisp
M-x magent-show-session
```

**Display format:**
```
Session Summary:

[USER] Find all .el files
[ASSISTANT] I'll search for .el files using glob.
[TOOL] tool_use: glob
[TOOL] tool_result: file1.el\nfile2.el\nfile3.el
[ASSISTANT] I found 3 Emacs Lisp files.
```

### Session Statistics

**Message counts by role:**
```elisp
(let ((messages (magent-session-get-messages session)))
  (cl-loop for msg in messages
           for role = (cdr (assq 'role msg))
           count (eq role 'user) into users
           count (eq role 'assistant) into assistants
           count (eq role 'tool) into tools
           finally return (list :user users
                               :assistant assistants
                               :tool tools)))
```

**Tool usage statistics:**
```elisp
;; Count tool calls
(cl-loop for msg in (magent-session-get-messages session)
         when (eq (cdr (assq 'role msg)) 'tool)
         count t)
```

## Best Practices

### Session Management

1. **One task per session**: Start fresh for unrelated tasks
2. **Clear when done**: Reset session after completing work
3. **Monitor size**: Check context size for long conversations
4. **Save important sessions**: Persist sessions you might need later
5. **Appropriate agent**: Select right agent before starting

### Message Management

1. **Concise prompts**: Keep user messages focused
2. **Context awareness**: Remember session maintains history
3. **Tool results**: Don't manually add; handled by agent loop
4. **Cleanup old sessions**: Periodically delete saved sessions

### History Size

1. **Balance context vs. cost**: Larger history = more tokens
2. **Task-appropriate size**: Simple tasks need less history
3. **Monitor trimming**: Know when old messages are lost
4. **Compact when needed**: Use summarization for very long sessions

### Agent Consistency

1. **Same agent per session**: Don't switch mid-conversation
2. **Agent-appropriate tasks**: Use right agent from start
3. **Re-assign sparingly**: Only switch agent if really needed

## Troubleshooting

### Session State Issues

**Session appears lost:**
```elisp
;; Check if session exists
magent--current-session

;; Verify messages
(magent-session-messages (magent-session-get))
```

**Agent not assigned:**
```elisp
;; Check agent
(magent-session-get-agent (magent-session-get))

;; Manually assign
(magent-session-set-agent (magent-session-get)
  (magent-agent-registry-get "build"))
```

### Persistence Issues

**Save fails:**
```elisp
;; Check directory exists and is writable
(file-writable-p (expand-file-name "magent-sessions" user-emacs-directory))

;; Create directory
(make-directory (expand-file-name "magent-sessions" user-emacs-directory) t)
```

**Load fails:**
```elisp
;; Verify file exists
(file-exists-p filepath)

;; Check JSON validity
;; Use external tool: jq . < session.json
```

### Context Size Issues

**Responses getting slow:**
- Too many messages in history
- Very long tool results
- Large file contents in context

**Solutions:**
```elisp
;; Reduce max history
(setq magent-max-history 50)

;; Clear and restart session
M-x magent-clear-session

;; Use summarization (future)
(magent-compact-session)
```

### Message Ordering Confusion

**Remember:**
- Internal storage: newest first (performance)
- API calls: oldest first (correct conversation order)
- `magent-session-get-messages` returns oldest first

## Advanced Usage

### Custom Session Implementations

**Per-project sessions:**
```elisp
(defvar-local my-project-session nil)

(defun my-get-project-session ()
  "Get or create session for current project."
  (or my-project-session
      (setq my-project-session
            (magent-session-create
             :id (concat "project-" (projectile-project-name))))))
```

**Session with pre-loaded context:**
```elisp
(defun my-create-contextual-session (files)
  "Create session with FILES pre-loaded as context."
  (let ((session (magent-session-create)))
    (dolist (file files)
      (magent-session-add-message session 'user
        (format "File %s:\n%s" file (magent-tools--read-file file))))
    session))
```

### Session Analysis

**Extract conversation text:**
```elisp
(defun my-session-text (session)
  "Extract plain text from SESSION messages."
  (mapconcat
   (lambda (msg)
     (let ((role (cdr (assq 'role msg)))
           (content (cdr (assq 'content msg))))
       (format "[%s] %s" role content)))
   (magent-session-get-messages session)
   "\n\n"))
```

**Find tool usage patterns:**
```elisp
(defun my-session-tools-used (session)
  "List all tools used in SESSION."
  (cl-loop for msg in (magent-session-get-messages session)
           when (eq (cdr (assq 'role msg)) 'assistant)
           append (cl-loop for block in (cdr (assq 'content msg))
                          when (equal (cdr (assq 'type block)) "tool_use")
                          collect (cdr (assq 'name block)))))
```

## API Reference

### Functions

| Function | Purpose |
|----------|---------|
| `magent-session-create` | Create new session struct |
| `magent-session-get` | Get current session (create if needed) |
| `magent-session-reset` | Clear current session |
| `magent-session-add-message` | Add user/assistant message |
| `magent-session-add-tool-result` | Add tool execution result |
| `magent-session-get-messages` | Get all messages (chronological) |
| `magent-session-get-agent` | Get assigned agent |
| `magent-session-set-agent` | Set assigned agent |
| `magent-session-get-id` | Get/generate session ID |
| `magent-session-get-context-size` | Estimate token count |
| `magent-session-summarize` | Generate text summary |
| `magent-session-save` | Persist session to disk |
| `magent-session-load` | Load session from file |
| `magent-session-list-saved` | List saved session files |

### Interactive Commands

| Command | Keybinding | Description |
|---------|-----------|-------------|
| `magent-clear-session` | `C-c o c` | Clear current session |
| `magent-show-session` | `C-c o s` | Display session summary |
| `magent-show-current-agent` | `C-c o i` | Show assigned agent |
| `magent-select-agent` | `C-c o A` | Change session agent |
