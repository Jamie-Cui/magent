# Magent Documentation

This directory contains comprehensive design documentation for Magent, an Emacs Lisp implementation of OpenCode.

## Documentation Index

### [Architecture](architecture.md)
High-level system architecture and design decisions.

**Contents:**
- System architecture overview with layer diagrams
- Core components and their responsibilities
- Data flow through the system
- Design decisions and rationale
- Extension points and future enhancements
- Performance considerations

**Read this if:** You want to understand how Magent works at a high level, or you're planning to extend or modify the system.

### [Agent System](agent-system.md)
Deep dive into the multi-agent architecture.

**Contents:**
- Agent types (primary, subagent, internal)
- Built-in agent descriptions
- Agent data structures
- Permission system details
- Custom agent creation guide
- Agent lifecycle and communication
- Best practices and troubleshooting

**Read this if:** You want to create custom agents, understand how permissions work, or modify agent behavior.

### [API Integration](api-integration.md)
Guide to LLM provider integration.

**Contents:**
- Supported providers (Anthropic, OpenAI, compatible APIs)
- Configuration and authentication
- Message format conversion
- Tool definition formats
- Streaming support
- Error handling and rate limiting
- Cost optimization strategies
- Testing and debugging

**Read this if:** You're working with API integration, troubleshooting connection issues, or adding support for new providers.

### [Tools](tools.md)
Complete reference for available tools.

**Contents:**
- Tool descriptions and parameters
- Usage examples and patterns
- Implementation details
- Security considerations
- Performance characteristics
- Tool combinations and best practices
- Extending the tool system

**Read this if:** You want to understand what tools agents can use, how to use them effectively, or how to create new tools.

### [Session Management](session-management.md)
Guide to conversation state and history management.

**Contents:**
- Session structure and lifecycle
- Message format and storage
- History management and trimming
- Session persistence
- Context size management
- Multi-session considerations
- Best practices and troubleshooting

**Read this if:** You're working with conversation state, implementing session features, or debugging history issues.

## Quick Reference

### For Users

**Getting Started:**
1. Read the main [README.md](../README.md) for installation and basic usage
2. Review [Agent System](agent-system.md) to understand available agents
3. Check [Tools](tools.md) to see what operations are available

**Common Tasks:**
- Creating custom agents → [Agent System - Custom Agents](agent-system.md#custom-agents)
- Configuring API access → [API Integration - Configuration](api-integration.md#configuration)
- Managing conversation history → [Session Management](session-management.md)

### For Developers

**Architecture Overview:**
1. Start with [Architecture](architecture.md) for the big picture
2. Read [Agent System](agent-system.md) for the agent loop
3. Review [API Integration](api-integration.md) for provider details

**Common Development Tasks:**
- Adding a new tool → [Tools - Extending Tools](tools.md#extending-tools)
- Adding a built-in agent → [Agent System - Agent Types](agent-system.md#agent-types)
- Supporting a new provider → [API Integration - Provider Comparison](api-integration.md#provider-comparison)

## Document Conventions

### Code Examples

Emacs Lisp code examples:
```elisp
(magent-session-get)
```

Shell commands:
```bash
make compile
```

JSON structures:
```json
{
  "key": "value"
}
```

### Cross-References

Links to sections within documents:
- Same document: `[Section Name](#section-name)`
- Other document: `[Document - Section](document.md#section)`

### Terminology

- **Agent**: An AI assistant with specific capabilities and permissions
- **Session**: A conversation context with message history
- **Tool**: A function agents can call to interact with the system
- **Permission**: Access control rules for tools
- **Provider**: LLM API service (Anthropic, OpenAI, etc.)

## Contributing to Documentation

When adding or updating documentation:

1. **Accuracy**: Ensure information matches current implementation
2. **Examples**: Include practical code examples
3. **Clarity**: Write for both novice and experienced users
4. **Completeness**: Cover common use cases and edge cases
5. **Maintenance**: Update when making code changes

### Documentation Structure

Each document should include:
- **Overview**: Brief summary of the topic
- **Core Concepts**: Key ideas and terminology
- **Detailed Reference**: Complete API or feature documentation
- **Examples**: Practical usage examples
- **Best Practices**: Recommended patterns
- **Troubleshooting**: Common issues and solutions

## Getting Help

If the documentation doesn't answer your question:

1. **Check source code**: The implementation is authoritative
2. **Search issues**: Look for related GitHub issues
3. **Ask questions**: Create a GitHub issue
4. **Contribute**: Improve the docs for others

## Documentation TODO

Future documentation improvements:

- [ ] UI/UX guide for interactive commands
- [ ] Configuration reference with all options
- [ ] Testing guide for running/writing tests
- [ ] Contribution guide for developers
- [ ] Troubleshooting guide with common issues
- [ ] Migration guide from other tools
- [ ] Performance tuning guide
- [ ] Security best practices
- [ ] Example workflows and use cases
- [ ] Video tutorials or screencasts

## License

This documentation is part of the Magent project and licensed under the GNU General Public License v3.0.
