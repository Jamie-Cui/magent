# Magent Documentation

Welcome to the Magent documentation. Stable user-facing material remains in the root README, while this directory holds developer guides, architecture notes, and troubleshooting docs.

## Documentation Index

### Getting Started
- **[ONBOARDING.md](ONBOARDING.md)** — New developer onboarding guide with architecture overview, guided tour, and complexity hotspots
- **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)** — Common issues and solutions

### Architecture
- **[AGENT_JOBS.md](AGENT_JOBS.md)** — Durable child-agent job lifecycle, tool surface, persistence, UI, and boundaries

### Contribution
- **[CONTRIBUTING.md](CONTRIBUTING.md)** — Contribution guidelines, code style, testing, and PR process
- **[TROUBLESHOOTING.md](TROUBLESHOOTING.md#github-actions-failures)** — CI, live smoke, and melpazoid failure notes

### Project Root Documentation
- **[../README.org](../README.org)** — Main project README with features, installation, and usage
- **[../AGENTS.md](../AGENTS.md)** — Development guide for agentic coding tools with build commands, architecture notes, and repository conventions

## Quick Links

### For New Contributors
1. Start with [ONBOARDING.md](ONBOARDING.md)
2. Read [CONTRIBUTING.md](CONTRIBUTING.md) for development workflow
3. Read [../AGENTS.md](../AGENTS.md) for current architecture notes and development guidance
4. Read [AGENT_JOBS.md](AGENT_JOBS.md) before changing child-agent lifecycle behavior

### For Users
1. [../README.org](../README.org) — Installation and configuration
2. [TROUBLESHOOTING.md](TROUBLESHOOTING.md) — Common issues and solutions
3. Run `M-x magent-doctor` for self-diagnostics
4. Use `C-c m ?` for the transient menu

### For Developers
1. [CONTRIBUTING.md](CONTRIBUTING.md) — Code style and PR process
2. [../AGENTS.md](../AGENTS.md) — Build commands, testing, architecture notes, and development guidance
3. [ONBOARDING.md](ONBOARDING.md) — Architecture overview and guided code tour
4. [AGENT_JOBS.md](AGENT_JOBS.md) — Current child-agent job architecture

### CI And Packaging
1. [../README.org](../README.org) — Public workflow badges and development commands
2. [CONTRIBUTING.md](CONTRIBUTING.md#continuous-integration) — Local and CI verification sequence
3. [TROUBLESHOOTING.md](TROUBLESHOOTING.md#github-actions-failures) — Known GitHub Actions failure signatures
4. [../AGENTS.md](../AGENTS.md#testing) — Agent-facing test, coverage, live smoke, and melpazoid notes

## Documentation Standards

When adding new documentation:
- Place user-facing docs in project root (README.org)
- Place developer docs in `docs/`
- Prefer markdown for docs under `docs/` unless there is a clear reason to use org-mode
- Update this index when adding new files
- Keep stable docs or active task notes updated before stopping work so another machine can resume from git
