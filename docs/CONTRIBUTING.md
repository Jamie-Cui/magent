# Contributing to Magent

Thank you for your interest in contributing to magent! This guide will help you get started.

## Current Focus

Magent's Codex-style agent workflow alignment is implemented as a durable child-agent/job lifecycle. Before changing agent lifecycle behavior, read and update `docs/AGENT_JOBS.md`.

The lifecycle surface is `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, `close_agent`, plus `magent-show-agent-transcript` for inspection. Codex sandboxing, seatbelt, bubblewrap, and shell isolation parity are out of scope.

## Development Setup

### Prerequisites
- Emacs 27.1+
- gptel 0.9.8+
- spinner 1.7.4+
- transient 0.4+
- ripgrep (for grep tool)

### Building

```bash
make compile    # Byte-compile all files
make test       # Run test suite
make clean      # Remove compiled files
```

## Code Style

### Naming Conventions
- Custom functions/variables: `+module/name` prefix
- Advice functions: `-a` suffix (e.g., `+evil-join-a`)
- Hook functions: `-h` suffix (e.g., `+enable-delete-trailing-whitespace-h`)
- Internal functions: `magent-module--function` (double dash)
- Public API: `magent-module-function` (single dash)

### File Headers
All files must include:
```elisp
;;; filename.el --- Brief description  -*- lexical-binding: t; -*-
```

### Documentation
- All public functions need docstrings
- Use `;;;` for section headers
- Use `;;` for inline comments
- Update README.org for user-facing changes
- Update AGENTS.md for developer-facing changes
- Update `docs/AGENT_JOBS.md` for child-agent lifecycle changes
- Update the relevant plan before stopping interrupted plan-driven work

## Testing

### Running Tests
```bash
# Full suite
make test

# Single test by regexp
emacs -Q --batch -L . -L ~/.emacs.d/elpa/gptel-* \
  -l ert -l test/magent-test.el \
  --eval '(ert-run-tests-batch "test-name-regexp")'
```

### Writing Tests
- Mock `gptel-request` with `cl-letf`
- Bind registries to fresh state
- Call `magent-session-reset` to clear global state
- Test both success and error paths

### Live Testing
```bash
# Reload changed file
emacsclient --eval '(load "/path/to/file.el" nil t)'

# Clear session
emacsclient --eval '(magent-clear-session)'

# Test prompts
# - Non-tool: "你好"
# - Tool-use: "帮我看下 emacs 里面有多少 buffer"
# - Multi-step: "帮我在 emacs 里面打开 magent 的 magit buffer"
```

## Pull Request Process

1. **Fork and branch** — Create a feature branch from `dev`
2. **Make changes** — Follow code style and add tests
3. **Test** — Run `make test` and verify manually
4. **Document** — Update relevant docs
5. **Commit** — Use conventional commits: `feat:`, `fix:`, `docs:`, `test:`
6. **Submit PR** — Target the `dev` branch

## Areas for Contribution

### High Priority
- Child-agent lifecycle polish and reliability, excluding sandbox parity
- Additional tool implementations
- Performance optimizations
- Test coverage improvements
- Documentation enhancements

### Medium Priority
- New built-in agents
- Skill system extensions
- UI improvements
- Error handling refinements

### Low Priority
- Code cleanup
- Refactoring
- Style improvements

## Architecture Guidelines

### Adding a New Tool
1. Implement in `magent-tools.el`
2. Add to `magent-enable-tools` default
3. Update agent permissions
4. Add tests
5. Document in README.org

Agent lifecycle tools are a special case. If the tool participates in spawning, messaging, waiting for, listing, resuming, inspecting, or closing child agents, keep the implementation aligned with `docs/AGENT_JOBS.md` and update the related tests.

### Creating a New Module
1. Follow dependency graph (see AGENTS.md)
2. Use `;;; -*- lexical-binding: t; -*-`
3. Require dependencies explicitly
4. Add to `magent.el` if needed
5. Write tests in `test/magent-test.el`

For a new child-agent/job module, prefer a focused module such as `magent-agent-job.el` over expanding `magent-tools.el` until the lifecycle becomes hard to isolate.

### Modifying the Agent Loop
- Active request/tool-loop behavior belongs in `magent-agent-loop.el`
- Keep using `magent-llm-gptel.el` and `gptel-request` for provider transport
- Add focused tests for normalized events, tool queueing, permission decisions, abort behavior, and session recording
- Update request generation and live-request checks when callbacks can arrive after interruption
- Do not reintroduce removed agent-loop modules; add provider-specific transport handling only in `magent-llm-gptel.el`

## Getting Help

- **Questions:** Open a GitHub discussion
- **Bugs:** Open an issue with reproduction steps
- **Features:** Open an issue to discuss before implementing
- **Chat:** Join discussions in issues/PRs

## License

By contributing, you agree that your contributions will be licensed under GPL-3.0.
