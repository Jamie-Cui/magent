# Contributing to Magent

Thank you for your interest in contributing to magent! This guide will help you get started.

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
make test       # Run test suite (~92 tests)
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
- Update CLAUDE.md for developer-facing changes

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

### Creating a New Module
1. Follow dependency graph (see CLAUDE.md)
2. Use `;;; -*- lexical-binding: t; -*-`
3. Require dependencies explicitly
4. Add to `magent.el` if needed
5. Write tests in `test/magent-test.el`

### Modifying the FSM
- Only touch `magent-fsm-backend-gptel.el` (native backend disabled)
- Test with multiple LLM providers
- Handle streaming edge cases
- Update request generation counter if needed

## Getting Help

- **Questions:** Open a GitHub discussion
- **Bugs:** Open an issue with reproduction steps
- **Features:** Open an issue to discuss before implementing
- **Chat:** Join discussions in issues/PRs

## License

By contributing, you agree that your contributions will be licensed under GPL-3.0.
