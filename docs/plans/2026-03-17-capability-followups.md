# Capability Progressive Disclosure Follow-up Plan

**Goal:** Turn the new capability layer from a minimal internal selector into a stable, explainable, user-tunable product surface for Magent.
**Architecture:** Keep capabilities as a layer above skills and below tools. Continue using instruction skills as the behavior-shaping substrate, but add stronger matching, user controls, richer package capability coverage, and better debug/inspection surfaces around the resolver.
**Tech Stack:** Emacs Lisp, YAML frontmatter capability files, existing Magent skill loader, current UI/agent/session pipeline, ERT, live Emacs verification via `emacsclient`.

## File Map

- Modify: `magent-capability.el`
- Modify: `magent-capability.el`
- Modify: `magent-agent.el`
- Modify: `magent-ui.el`
- Modify: `magent-config.el`
- Modify: `magent.el`
- Modify: `magent-skills.el`
- Modify: `test/magent-test.el`
- Modify: `README.org`
- Modify: `docs/design.org`
- Create: `capabilities/*/CAPABILITY.md`
- Create: `skills/*/SKILL.md`
- Create: `docs/plans/2026-03-17-capability-followups.md`

## Task 1: Stabilize The Capability Model

**Files:**
- Modify: `magent-capability.el`
- Modify: `magent-capability.el`
- Modify: `test/magent-test.el`
- Modify: `docs/design.org`

- [ ] Add a clear public comment block describing capability lifecycle: discovery -> scoring -> suggestion -> activation.
- [ ] Normalize capability metadata parsing so single strings, comma-separated strings, and YAML lists behave consistently across `skills`, `modes`, `features`, `files`, and `keywords`.
- [ ] Add a resolver output shape that is explicitly documented and easy to inspect in tests.
- [ ] Add tests for tie-breaking, disabled capabilities, zero-active-limit behavior, and prompt-context extraction.
- [ ] Document the capability model in `docs/design.org` so the new abstraction is part of the architecture doc instead of living only in code.

## Task 2: Improve Matching Without Making It Magical

**Files:**
- Modify: `magent-capability.el`
- Modify: `magent-ui.el`
- Modify: `test/magent-test.el`

- [ ] Expand context capture to include project-relevant facts such as current file extension, buffer major mode family, and whether the current buffer is modified.
- [ ] Add a small set of explicit match helpers for common cases: mode family match, feature-loaded match, file glob match, prompt keyword match.
- [ ] Keep scoring deterministic and additive; do not add opaque LLM-based matching.
- [ ] Add tests covering mixed-context turns such as `org-mode` plus explicit git wording, and Magit buffers plus generic commit wording.
- [ ] Add a debug string or plist field that makes each score contribution inspectable from logs and tests.

## Task 3: Add User Controls And Turn-Level Explainability

**Files:**
- Modify: `magent-config.el`
- Modify: `magent-capability.el`
- Modify: `magent-ui.el`
- Modify: `magent.el`
- Modify: `README.org`
- Modify: `test/magent-test.el`

- [ ] Add interactive commands to list capabilities for the current context, explain the last resolution, and toggle a capability on/off locally.
- [ ] Add a small user-visible summary in the `*magent*` buffer or log when capabilities were auto-activated for the current turn.
- [ ] Add customization for disabling individual capabilities and entire capability families without editing files on disk.
- [ ] Ensure explicit slash-selected skills still win over capability auto-selection when there is tension.
- [ ] Document the user controls in `README.org`.

## Task 4: Expand Builtin Capability Families

**Files:**
- Create: `capabilities/*/CAPABILITY.md`
- Create: `skills/*/SKILL.md`
- Modify: `test/magent-test.el`
- Modify: `README.org`

- [ ] Add builtin capability families for hook/keymap/advice debugging, package/config reload workflow, and command/variable introspection.
- [ ] Keep builtins grouped by problem domain, not by raw Elisp function list.
- [ ] Add tests that verify builtin capabilities activate from `emacs-lisp-mode`, `lisp-interaction-mode`, and diagnosis prompts.
- [ ] Add README examples showing when builtin capabilities should auto-activate and when they should stay silent.

## Task 5: Curate Third-Party Package Capabilities

**Files:**
- Create: `capabilities/*/CAPABILITY.md`
- Create: `skills/*/SKILL.md`
- Modify: `magent-capability.el`
- Modify: `README.org`
- Modify: `test/magent-test.el`

- [ ] Start with a small curated package set: `magit`, `org`, `project`, and one LSP package family.
- [ ] Define each package capability around a user-facing workflow, not around package names alone.
- [ ] Add tests proving irrelevant installed features do not automatically leak into prompt activation.
- [ ] Decide and document whether package capabilities are gated by `featurep`, `major-mode`, file patterns, or a combination.
- [ ] Add authoring examples for maintainers so new package capabilities follow one consistent pattern.

## Task 6: Separate Metadata Contribution From Activation Policy

**Files:**
- Modify: `magent-capability.el`
- Modify: `magent-capability.el`
- Modify: `docs/design.org`
- Modify: `README.org`

- [ ] Define which fields are considered maintainer-owned policy versus package-author or user-provided metadata.
- [ ] Keep activation policy, risk level, and disclosure defaults under Magent control.
- [ ] Allow future metadata ingestion from package-local capability files, but keep that path disabled by default until governance is clear.
- [ ] Document the governance model: maintainer defines projection, package authors contribute facts, users decide local enablement.

## Task 7: Tighten Integration With Existing Workflows

**Files:**
- Modify: `magent-agent.el`
- Modify: `magent-ui.el`
- Modify: `magent-skills.el`
- Modify: `test/magent-test.el`

- [ ] Make diagnosis flows such as `magent-diagnose-emacs` and `magent-doctor` compose cleanly with auto-capabilities instead of duplicating too much guidance.
- [ ] Ensure capability-derived skills do not bloat the system prompt when a specialized command already hard-selects a debugging skill.
- [ ] Add tests for merging explicit skills, capability skills, and diagnosis commands in the same turn.
- [ ] Audit the final prompt composition path for duplicate skills and redundant prompt sections.

## Task 8: Verification And Regression Discipline

**Files:**
- Modify: `test/magent-test.el`
- Modify: `Makefile`
- Modify: `README.org`

- [ ] Add regression tests for capability loading order and startup initialization.
- [ ] Add tests for capability reload behavior in a running session.
- [ ] Add one or two end-to-end live Emacs verification recipes to `README.org` for maintainers:
  - Org buffer -> org capability activates
  - Magit buffer -> Magit capability activates
  - Plain code buffer with no matching context -> no auto capability
- [ ] Keep `make compile` and `make test` green after each capability family is added.

## Acceptance Criteria

- Capability selection stays deterministic and explainable.
- Explicit user-selected skills continue to work unchanged.
- Builtin capabilities remain grouped by task domain, not raw function catalogs.
- Third-party package capabilities remain curated and small in scope.
- Users can inspect and disable capability activation without patching Magent internals.
- Documentation explains both the mental model and the authoring model.

## Commands

```bash
make compile
make test
emacsclient --eval '(magent-reload-skills)'
emacsclient --eval '(magent-reload-capabilities)'
```
