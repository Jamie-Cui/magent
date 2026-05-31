# Magent Documentation

Welcome to the Magent documentation. This directory is intentionally small right now: the active project focus is captured in the Codex agent workflow alignment plan, while stable user-facing material remains in the root README.

## Documentation Index

### Getting Started
- **[ONBOARDING.md](ONBOARDING.md)** — New developer onboarding guide with architecture overview, guided tour, and complexity hotspots
- **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)** — Common issues and solutions

### Contribution
- **[CONTRIBUTING.md](CONTRIBUTING.md)** — Contribution guidelines, code style, testing, and PR process

### Active Planning
- **[plans/](plans/)** — Implementation plans and design proposals
  - `2026-05-30-codex-agent-workflow-alignment.md` — Active plan for aligning Magent's agent workflow with Codex-style collaborative agents, excluding sandbox parity

### Project Root Documentation
- **[../README.org](../README.org)** — Main project README with features, installation, and usage
- **[../AGENTS.md](../AGENTS.md)** — Development guide for agentic coding tools with build commands, architecture notes, and active project goals

## Quick Links

### For New Contributors
1. Start with [ONBOARDING.md](ONBOARDING.md)
2. Read [CONTRIBUTING.md](CONTRIBUTING.md) for development workflow
3. Read [../AGENTS.md](../AGENTS.md) for current architecture notes and active goals
4. Read [plans/2026-05-30-codex-agent-workflow-alignment.md](plans/2026-05-30-codex-agent-workflow-alignment.md) before changing agent workflow behavior

### For Users
1. [../README.org](../README.org) — Installation and configuration
2. [TROUBLESHOOTING.md](TROUBLESHOOTING.md) — Common issues and solutions
3. Run `M-x magent-doctor` for self-diagnostics
4. Use `C-c m ?` for the transient menu

### For Developers
1. [CONTRIBUTING.md](CONTRIBUTING.md) — Code style and PR process
2. [../AGENTS.md](../AGENTS.md) — Build commands, testing, architecture notes, and active goals
3. [ONBOARDING.md](ONBOARDING.md) — Architecture overview and guided code tour
4. [plans/2026-05-30-codex-agent-workflow-alignment.md](plans/2026-05-30-codex-agent-workflow-alignment.md) — Current Codex agent workflow comparison and migration plan

## Documentation Standards

When adding new documentation:
- Place user-facing docs in project root (README.org)
- Place developer docs in `docs/`
- Place implementation plans in `docs/plans/`
- Prefer markdown for docs under `docs/` unless there is a clear reason to use org-mode
- Update this index when adding new files
- Keep the active plan updated before stopping work so another machine can resume from git
