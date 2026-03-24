# Magent Documentation

Welcome to the magent documentation. This directory contains comprehensive guides and design documents for understanding and contributing to magent.

## Documentation Index

### Getting Started
- **[ONBOARDING.md](ONBOARDING.md)** — New developer onboarding guide with architecture overview, guided tour, and complexity hotspots
- **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)** — Common issues and solutions

### Reference
- **[API.md](API.md)** — Complete API reference with commands, functions, data structures, and customization variables
- **[CONTRIBUTING.md](CONTRIBUTING.md)** — Contribution guidelines, code style, testing, and PR process

### Design & Architecture
- **[design.org](design.org)** — Comprehensive design document covering architecture, data structures, and implementation details
- **[live-in-emacs-positioning.org](live-in-emacs-positioning.org)** — Product positioning and the "live-in-emacs" philosophy

### Planning Documents
- **[plans/](plans/)** — Implementation plans and design proposals
  - `2026-03-17-capability-followups.md` — Capability system follow-up tasks
  - `2026-03-19-streaming-performance.md` — Streaming performance improvements

### Project Root Documentation
- **[../README.org](../README.org)** — Main project README with features, installation, and usage
- **[../CLAUDE.md](../CLAUDE.md)** — Development guide for Claude Code with build commands and architecture notes

## Quick Links

### For New Contributors
1. Start with [ONBOARDING.md](ONBOARDING.md)
2. Read [CONTRIBUTING.md](CONTRIBUTING.md) for development workflow
3. Review [API.md](API.md) for public API surface
4. Explore [design.org](design.org) for deep architecture understanding

### For Users
1. [../README.org](../README.org) — Installation and configuration
2. [TROUBLESHOOTING.md](TROUBLESHOOTING.md) — Common issues and solutions
3. Run `M-x magent-doctor` for self-diagnostics
4. Use `C-c m ?` for the transient menu

### For Developers
1. [CONTRIBUTING.md](CONTRIBUTING.md) — Code style and PR process
2. [API.md](API.md) — Complete API reference
3. [../CLAUDE.md](../CLAUDE.md) — Build commands and testing
4. [design.org](design.org) — Architecture deep dive

### For Researchers
- [live-in-emacs-positioning.org](live-in-emacs-positioning.org) — Design philosophy and trade-offs

## Documentation Standards

When adding new documentation:
- Place user-facing docs in project root (README.org)
- Place developer docs in `docs/`
- Place implementation plans in `docs/plans/`
- Use org-mode for design docs, markdown for guides
- Update this index when adding new files
