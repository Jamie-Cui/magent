---
title: Documentation Overview
lang: en
alt_url: /zh/
permalink: /
---

# Magent Documentation

Welcome to the Magent documentation. Stable user-facing material remains in the root README, while this directory holds developer guides, architecture notes, and troubleshooting docs.

## Local Preview

From the repository root:

```bash
scripts/serve-docs-local.sh
```

The script serves the site at `http://127.0.0.1:8099/` by default. If the port is busy, it picks the next available port and prints the URL. Override the bind address or preferred port with:

```bash
HOST=0.0.0.0 PORT=4000 scripts/serve-docs-local.sh
```

## Documentation Index

### Getting Started
- **[ONBOARDING.md](ONBOARDING.html)** — New developer onboarding guide with architecture overview, guided tour, and complexity hotspots
- **[TROUBLESHOOTING.md](TROUBLESHOOTING.html)** — Common issues and solutions

### Architecture
- **[ARCHITECTURE.md](ARCHITECTURE.html)** — Product positioning, system boundaries, module layers, request flow, and capability model
- **[AGENT_WORKFLOW.md](AGENT_WORKFLOW.html)** — Thread/turn/item state machine, loop flow, persistence (snapshot + journal), UI projection, and Codex alignment
- **[AGENT_JOBS.md](AGENT_JOBS.html)** — Durable child-agent job lifecycle, tool surface, persistence, UI, and boundaries
- **[UI_BACKENDS.md](UI_BACKENDS.html)** — UI backend boundary, agent-shell + ACP flow, runtime API, and legacy UI isolation

### Contribution
- **[CONTRIBUTING.md](CONTRIBUTING.html)** — Contribution guidelines, code style, testing, and PR process
- **[TROUBLESHOOTING.md](TROUBLESHOOTING.html#github-actions-failures)** — CI, live smoke, and melpazoid failure notes

### Project Root Documentation
- **[../README.org](https://github.com/jamie-cui/magent/blob/master/README.org)** — Main project README with features, installation, and usage
- **[../AGENTS.md](https://github.com/jamie-cui/magent/blob/master/AGENTS.md)** — Development guide for agentic coding tools with build commands, architecture notes, and repository conventions

## Quick Links

### For New Contributors
1. Start with [ONBOARDING.md](ONBOARDING.html)
2. Read [CONTRIBUTING.md](CONTRIBUTING.html) for development workflow
3. Read [../AGENTS.md](https://github.com/jamie-cui/magent/blob/master/AGENTS.md) for current architecture notes and development guidance
4. Read [AGENT_JOBS.md](AGENT_JOBS.html) before changing child-agent lifecycle behavior

### For Users
1. [../README.org](https://github.com/jamie-cui/magent/blob/master/README.org) — Installation and configuration
2. [TROUBLESHOOTING.md](TROUBLESHOOTING.html) — Common issues and solutions
3. Run `M-x magent-doctor` for self-diagnostics
4. Use `C-c m ?` for the transient menu

### For Developers
1. [CONTRIBUTING.md](CONTRIBUTING.html) — Code style and PR process
2. [../AGENTS.md](https://github.com/jamie-cui/magent/blob/master/AGENTS.md) — Build commands, testing, architecture notes, and development guidance
3. [ARCHITECTURE.md](ARCHITECTURE.html) — Current architecture and system boundaries
4. [ONBOARDING.md](ONBOARDING.html) — Guided code tour and complexity hotspots
5. [AGENT_JOBS.md](AGENT_JOBS.html) — Current child-agent job architecture
6. [UI_BACKENDS.md](UI_BACKENDS.html) — Current UI backend architecture

### CI And Packaging
1. [../README.org](https://github.com/jamie-cui/magent/blob/master/README.org) — Public workflow badges and development commands
2. [CONTRIBUTING.md](CONTRIBUTING.html#continuous-integration) — Local and CI verification sequence
3. [TROUBLESHOOTING.md](TROUBLESHOOTING.html#github-actions-failures) — Known GitHub Actions failure signatures
4. [../AGENTS.md](https://github.com/jamie-cui/magent/blob/master/AGENTS.md#testing) — Agent-facing test, coverage, live smoke, and melpazoid notes

## Documentation Standards

When adding new documentation:
- Place user-facing docs in project root (README.org)
- Place developer docs in `docs/`
- Prefer markdown for docs under `docs/` unless there is a clear reason to use org-mode
- Update this index when adding new files
- Keep stable docs or active task notes updated before stopping work so another machine can resume from git
