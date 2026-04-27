# Symptom Map

Use this file to jump from a user-visible symptom to the most likely Magent subsystems and files.

## Tool Call Hangs Or FSM Stalls

Start with:

- `magent-fsm-backend-gptel.el`
- `magent-tools.el`
- `magent-agent.el`
- `magent-permission.el`
- `magent-ui.el`

Look for:

- tool result handling that does not complete the loop
- confirmation or permission paths that never resolve
- hallucinated or unknown tools
- stale request generation or interrupted callbacks
- queue or in-flight request state that was never cleared

## `*magent*` Buffer Rendering Problems

Start with:

- `magent-ui.el`
- `magent-session.el`
- `magent-md2org.el` if present in the checkout
- `magent-config.el`

Look for:

- org-mode folding and render timing issues
- overlay or read-only boundary mistakes
- deferred fontification or batching behavior
- stale buffer content restored from session state

## Task Drift

Treat task drift as one of:

- the assistant starts solving a different problem than the user asked
- the tool chain wanders away from the intended goal
- session or history state causes Magent to lose the current objective

Start with:

- `prompt.org`
- `magent-agent.el`
- `magent-session.el`
- `magent-agent-registry.el`
- `magent-tools.el`

Look for:

- prompt assembly that over-weights stale context
- history trimming or restore behavior that changes intent
- agent selection or defaults that change the task framing
- tool results that add noise or redirect the conversation

## Performance Regressions Or Perceived Slowness

Start with:

- `magent-ui.el`
- `magent-session.el`
- `magent-fsm-backend-gptel.el`
- `magent-agent.el`
- `magent-audit.el` when auditing is active

Look for:

- streaming batch size and fontification cost
- large history payloads or expensive session restore paths
- excessive logging or audit overhead
- repeated prompt rebuilding or redundant tool loops

## Compile Or Test Failures

Start with:

- `Makefile`
- the edited `.el` files
- `test/magent-test.el`
- backend files required by structs or shared definitions

Look for:

- missing load paths or dependency discovery issues
- byte-compile warnings that reveal stale APIs
- tests that need registry or session reset setup
- native or shared struct definitions that batch code still requires

## First Read Set For Broad Failures

When the symptom is still broad after the first pass, default to:

- `AGENTS.md` or `CLAUDE.md`
- `Makefile`
- `magent.el`
- `magent-ui.el`
- `magent-agent.el`
- `magent-tools.el`
- `magent-fsm*.el`
- `magent-session.el`
- `test/magent-test.el`
