---
title: Contributing to Magent
lang: en
alt_url: /CONTRIBUTING.zh.html
---

# Contributing to Magent

Thank you for your interest in contributing to magent! This guide will help you get started.

## Current Focus

Magent's Codex-style agent workflow alignment is implemented as a durable child-agent/job lifecycle. Before changing agent lifecycle behavior, read and update `docs/AGENT_JOBS.md`.

The lifecycle surface is `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, `close_agent`, plus `magent-show-agent-transcript` for inspection. Codex sandboxing, seatbelt, bubblewrap, and shell isolation parity are out of scope.

## Development Setup

### Prerequisites
- Emacs 29.1+
- gptel 0.9.8+
- transient 0.7.8+
- compat 30.1+
- yaml 1.0+
- acp 0.12.2+
- agent-shell 0.57.1+
- ripgrep (for grep tool)

### Building

```bash
make compile    # Byte-compile all files
make test-unit  # Run batch ERT unit tests
make test       # Run unit tests plus deterministic live smoke tests
make coverage   # Run ERT under built-in testcover
make clean      # Remove compiled files
```

The Makefile auto-detects ELPA dependency directories under
`~/.emacs.d/elpa/`. Override variables such as `GPTEL_DIR`, `TRANSIENT_DIR`,
`COMPAT_DIR`, or `YAML_DIR` when testing against nonstandard dependency
checkouts.

## Code Style

### Naming Conventions
- Internal functions and variables use `magent-module--name` with a double dash.
- Public functions, commands, and variables use `magent-module-name` with a single dash.
- Tool implementations follow `magent-tools--<name>` for the internal function and `magent-tools--<name>-tool` for the `gptel-tool` variable.
- Optional integration code stays in its own module, for example Evil-specific behavior belongs in `magent-evil.el`.
- Do not use personal-config prefixes such as `+module/name` in Magent source files.

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

# Unit tests only
make test-unit

# Coverage summary
make coverage

# Single test by regexp
emacs -Q --batch -L lisp -L ~/.emacs.d/elpa/gptel-* \
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

When running real live tests against the configured gptel provider, enable
`debug-on-error` in the running Emacs first:

```bash
emacsclient --eval '(setq debug-on-error t)'
```

During and after the run, inspect `*Messages*`, `*Backtrace*`,
`*magent-log*` or `*magent-live-test-log*`, and `*gptel-log*`.  Do not paste
raw `*gptel-log*` output into issues or commits without checking for provider
headers, request bodies, or API-key material.

### Continuous Integration

GitHub Actions runs:

- `test.yml`: Emacs 29.4 installation, dependency installation, byte-compile,
  unit tests, and deterministic live smoke tests in a temporary daemon.
- `coverage.yml`: `make coverage`, uploading `coverage/testcover-summary.tsv`.
- `melpazoid.yml`: MELPA-style package checks.

The melpazoid recipe includes `prompt.org`, `skills/`, and `capabilities/`.
Those are bundled runtime data and must stay in the package recipe whenever
their paths are used by `magent-config.el`, `magent-skills.el`, or
`magent-capability.el`.

Keep package metadata centralized in `magent.el` and `magent-pkg.el`. Do not
add `Package-Requires` headers to secondary modules; package-lint treats those
as ineffective outside the main file. Every Elisp file should include an SPDX
license identifier.

## Pull Request Process

1. **Fork and branch** — Create a feature branch from `dev`
2. **Make changes** — Follow code style and add tests
3. **Test** — Run `make test` and verify manually
4. **Document** — Update relevant docs, including CI/package docs when build metadata changes
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
- When changing continuation or final-response behavior, test post-tool empty completions and reasoning-only completions; reasoning must not become assistant text
- Update request generation and live-request checks when callbacks can arrive after interruption
- Do not reintroduce removed agent-loop modules; add provider-specific transport handling only in `magent-llm-gptel.el`

## Getting Help

- **Questions:** Open a GitHub discussion
- **Bugs:** Open an issue with reproduction steps
- **Features:** Open an issue to discuss before implementing
- **Chat:** Join discussions in issues/PRs

## License

By contributing, you agree that your contributions will be licensed under GPL-3.0.
