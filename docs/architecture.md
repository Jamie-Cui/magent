# Magent Architecture

This document describes the high-level architecture of Magent, an Emacs Lisp implementation of OpenCode.

## Overview

Magent is a multi-agent AI coding assistant that integrates with LLM providers (Anthropic Claude, OpenAI GPT) to provide intelligent code assistance within Emacs. The system uses a modular architecture with clear separation of concerns across six main layers.

## System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      User Interface Layer                    │
│                       (magent-ui.el)                         │
│  - Minibuffer prompts                                        │
│  - Output buffer display                                     │
│  - Interactive commands                                      │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      Session Layer                           │
│                    (magent-session.el)                       │
│  - Conversation history management                           │
│  - Message persistence                                       │
│  - Agent assignment per session                              │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      Agent Layer                             │
│           (magent-agent.el, magent-agent-*.el)              │
│  - Agent orchestration and loop (max 10 iterations)         │
│  - Tool calling coordination                                │
│  - Permission-based access control                          │
│  - Built-in and custom agent management                     │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      Permission Layer                        │
│                  (magent-permission.el)                      │
│  - Rule-based tool access control                           │
│  - File pattern matching                                    │
│  - allow/deny/ask resolution                                │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      Tools Layer                             │
│                    (magent-tools.el)                         │
│  - read_file, write_file                                    │
│  - grep, glob                                               │
│  - bash command execution                                   │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      API Layer                               │
│                    (magent-api.el)                           │
│  - HTTP client for LLM providers                            │
│  - Message format conversion                                │
│  - Streaming support                                        │
│  - Request/response logging                                 │
└─────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. Entry Point (`magent.el`)

The main entry point defines the `magent-mode` minor mode and provides the user-facing interface.

**Responsibilities:**
- Mode initialization and keybindings (`C-c o` prefix)
- Agent registry initialization
- Custom agent loading on startup
- Global mode support

**Key Functions:**
- `magent-mode`: Enable/disable the minor mode
- `global-magent-mode`: Enable globally across all buffers

### 2. Agent System

The agent system implements a multi-agent architecture where different agents have specialized capabilities and permission rules.

#### Agent Registry (`magent-agent-registry.el`)

Central registry for agent lookup and management.

**Responsibilities:**
- Store and retrieve agent definitions
- Distinguish between primary and subagents
- Provide agent listing and filtering

**Key Functions:**
- `magent-agent-registry-register`: Add agent to registry
- `magent-agent-registry-get`: Retrieve agent by name
- `magent-agent-registry-primary-agents`: List user-facing agents

#### Agent Info (`magent-agent-info.el`)

Defines the data structure for agent configuration using `cl-defstruct`.

**Structure Fields:**
- `name`: Unique agent identifier
- `description`: Human-readable description
- `mode`: `primary`, `subagent`, or `all`
- `native`: Whether built-in or custom
- `hidden`: Hide from user selection
- `prompt`: System prompt for the agent
- `permission`: Permission rules for tool access
- `temperature`, `top-p`: LLM sampling parameters
- `model`: Override default model

#### Agent Types (`magent-agent-types.el`)

Defines the seven built-in agents:

1. **build** (primary): Default agent with full tool access
2. **plan** (primary): Planning mode with restricted file edits
3. **explore** (subagent): Fast codebase exploration specialist
4. **general** (subagent): Multi-step task execution
5. **compaction** (internal): Session summarization
6. **title** (internal): Conversation title generation
7. **summary** (internal): PR-style summaries

#### Agent Loop (`magent-agent.el`)

Implements the core agent execution loop with tool calling.

**Agent Loop Flow:**
1. Accept user prompt
2. Add user message to session
3. Call LLM API with available tools
4. If response contains tool uses:
   - Execute each tool with permission check
   - Add tool results to session
   - Loop back to step 3 (max 10 iterations)
5. If response contains only text:
   - Add assistant message to session
   - Call completion callback

**Key Functions:**
- `magent-agent-process`: Entry point for agent execution
- `magent-agent--loop`: Main iteration loop
- `magent-agent--get-tools`: Filter tools by agent permissions
- `magent-agent--execute-tools`: Execute tool calls with checks

#### Custom Agents (`magent-agent-file.el`)

Loads custom agents from `.opencode/agent/*.md` files.

**File Format:**
```markdown
---
description: My custom agent
mode: primary
hidden: false
temperature: 0.5
tools:
  bash: false
  read: true
  write: true
---

Your custom system prompt here.
```

**Responsibilities:**
- Parse YAML frontmatter
- Convert to agent info structures
- Register with agent registry
- Support saving agents back to files

### 3. Permission System (`magent-permission.el`)

Rule-based access control for tools with file pattern matching.

**Permission Types:**
- `allow`: Tool is permitted
- `deny`: Tool is blocked
- `ask`: Prompt user for confirmation

**Rule Structure:**
Rules are alists that can be nested for fine-grained control:

```elisp
;; Simple tool permission
((bash . deny))

;; File-specific permissions
((read . ((\"*.env\" . deny)
          (\"*.el\" . allow)
          (\"*\" . allow))))

;; Wildcard default
((* . allow))
```

**Resolution Algorithm:**
1. Check for exact tool match
2. If tool has nested rules and file provided, check file patterns
3. Check wildcard `*` rule
4. Default to `allow`

**File Pattern Matching:**
- Glob patterns: `*.el`, `*.env.*`
- Exact paths: `/path/to/file`
- Wildcard default: `*`

### 4. Session Management (`magent-session.el`)

Manages conversation state and history.

**Session Structure:**
- `messages`: List of conversation messages
- `max-history`: Maximum messages to retain
- `id`: Unique session identifier
- `agent`: Currently assigned agent

**Message Types:**
- `user`: User prompts
- `assistant`: AI responses (text or tool uses)
- `tool`: Tool execution results

**Features:**
- Automatic history trimming
- Session persistence to `~/.emacs.d/magent-sessions/`
- Context size estimation (~4 chars per token)
- Session summarization

### 5. Tools Layer (`magent-tools.el`)

Implements the tools that agents can use to interact with the system.

**Available Tools:**

1. **read_file**: Read file contents
   - Returns file text or error message

2. **write_file**: Write content to file
   - Creates parent directories if needed
   - Returns success/error message

3. **grep**: Search files with regex
   - Recursive directory search
   - Skips `.git`, `node_modules`, etc.
   - Returns `file:line:content` format

4. **glob**: Find files by pattern
   - Supports `*` and `**` wildcards
   - Returns newline-separated paths

5. **bash**: Execute shell commands
   - Configurable timeout (default 30s)
   - Returns stdout + stderr
   - Timeout protection

**Tool Definitions:**

Each tool provides a schema for the LLM:
- `name`: Tool identifier
- `description`: What the tool does
- `input_schema`: JSON schema for parameters

### 6. API Layer (`magent-api.el`)

HTTP client for communicating with LLM providers.

**Supported Providers:**
- Anthropic Claude (Messages API)
- OpenAI GPT (Chat Completions API)
- OpenAI-compatible APIs (custom base URL)

**Key Responsibilities:**
- Convert between internal and provider-specific message formats
- Handle tool definitions in provider format
- Parse responses and extract content/tool uses
- Support streaming responses
- Request/response logging

**Message Conversion:**

Internal format uses consistent structure:
```elisp
((role . "user")
 (content . "text"))
```

Provider formats differ:
- **Anthropic**: Structured content blocks with types
- **OpenAI**: Simple string content

**Tool Use Extraction:**

Different providers have different tool calling formats:
- **Anthropic**: `tool_use` content blocks
- **OpenAI**: `tool_calls` array in message

The API layer normalizes these to internal format:
```elisp
((id . "call_123")
 (name . "read_file")
 (input . ((path . "/foo/bar.el"))))
```

## Data Flow

### Typical Request Flow

1. **User Input**
   - User invokes `magent-prompt` (C-c o p)
   - Minibuffer prompts for input

2. **Session Update**
   - User message added to session
   - Session retrieves or assigns agent

3. **Agent Processing**
   - Agent loop begins
   - Tools filtered by agent permissions
   - API request created with messages + tools

4. **LLM Response**
   - API returns response (text or tool uses)
   - Response parsed and normalized

5. **Tool Execution** (if applicable)
   - Each tool use checked against permissions
   - Tools executed if allowed
   - Results added to session
   - Loop continues (max 10 iterations)

6. **Completion**
   - Final text response added to session
   - Output displayed to user
   - Session persisted

### Custom Agent Loading Flow

1. **Mode Activation**
   - `magent-mode` enabled
   - Agent registry initialized with built-in agents

2. **Custom Agent Discovery**
   - Scan `.opencode/agent/*.md` files
   - Parse YAML frontmatter
   - Extract agent configuration

3. **Agent Registration**
   - Create `magent-agent-info` structure
   - Register with agent registry
   - Available for session assignment

## Design Decisions

### 1. Multi-Agent Architecture

**Decision:** Support multiple specialized agents rather than a single general-purpose agent.

**Rationale:**
- Different tasks benefit from different permissions (e.g., planning doesn't need file edits)
- Specialized prompts improve task-specific performance
- Users can create custom agents for their workflows
- Matches OpenCode's proven architecture

### 2. Permission-Based Access Control

**Decision:** Implement fine-grained permission system with file pattern matching.

**Rationale:**
- Security: Prevent accidental writes to sensitive files (`.env`)
- Flexibility: Different agents have different needs
- User control: `ask` permission allows user confirmation
- Extensibility: Custom agents can specify tool restrictions

### 3. Session-Based Conversations

**Decision:** Maintain conversation history in sessions rather than stateless requests.

**Rationale:**
- Context awareness: Agent can reference previous messages
- Continuity: Multi-turn tool calling requires state
- Persistence: Save and resume conversations
- Agent consistency: Keep same agent throughout session

### 4. Maximum Iteration Limit

**Decision:** Limit agent loop to 10 iterations.

**Rationale:**
- Prevent infinite loops in tool calling
- Control API costs
- Force more efficient tool use
- Match OpenCode behavior

### 5. Synchronous Tool Execution

**Decision:** Execute tools synchronously within agent loop.

**Rationale:**
- Simplicity: Easier to reason about execution order
- Reliability: Guaranteed completion before next iteration
- Emacs threading: Limited async support in Emacs
- Future: Can add async support if needed

### 6. Provider Abstraction

**Decision:** Support multiple LLM providers with unified interface.

**Rationale:**
- Flexibility: Users choose their preferred provider
- Future-proofing: Easy to add new providers
- Testing: Can switch providers for different tasks
- Cost optimization: Use cheaper models when appropriate

## Extension Points

The architecture provides several extension points:

1. **Custom Agents**: Add `.opencode/agent/*.md` files
2. **New Tools**: Extend `magent-tools.el` with new capabilities
3. **New Providers**: Add provider support in `magent-api.el`
4. **Permission Rules**: Define custom permission schemes
5. **UI Extensions**: Build on `magent-ui.el` for rich displays

## Performance Considerations

### Token Usage

- Sessions automatically trim to `magent-max-history` messages
- Context size estimation helps monitor token usage
- Consider using compaction agent for long conversations

### API Calls

- Each agent loop iteration = 1 API call
- Tool-heavy tasks may use multiple iterations
- Streaming reduces perceived latency

### File Operations

- Large file reads can impact performance
- Grep scans entire directory trees
- Glob uses Emacs built-in wildcards (efficient)

## Future Enhancements

Potential areas for improvement:

1. **Async Tool Execution**: Parallel tool calls
2. **Tool Caching**: Cache file reads within session
3. **Rich UI**: Dedicated buffer with formatting
4. **Token Counting**: Accurate token usage tracking
5. **Streaming Tool Results**: Stream grep/glob output
6. **Agent Delegation**: Primary agents calling subagents
7. **Plugin System**: External tool definitions
