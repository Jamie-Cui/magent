# Make Action the executable extension abstraction

- Status: Accepted
- Date: 2026-07-28

## Context

The earlier `Command` model mixed two meanings: the trusted Elisp extension
that owns execution, and the slash/interactive protocol entry used to invoke
it. Skills could also grow `default-prompt` adapters that appeared in the same
registry. The result obscured ownership: Command, Workflow, Invocation, Skill
adapter, and frontend command could all look like competing abstractions.

The Workflow runtime also lived in a separate module despite sharing all of its
state and lifecycle invariants with registration and Invocation execution.
Conversely, interactive isolated-session viewing was coupled to persistence and
cancellation.

## Decision

`Action` is the sole executable extension abstraction. A command is only a
frontend or protocol projection of an Action.

- `magent-action.el` owns the Workflow DSL, managed Step runtime, layered Action
  registry, and Invocation lifecycle as one deep module.
- `magent-action-session.el` owns isolated Action persistence, ledger activity,
  and cancellation; `magent-action-session-view.el` owns interactive viewing.
- Bundled one-turn prompt Actions are data entries whose Workflows are generated
  uniformly.
- Skills remain instruction data selected for ordinary turns. They are never
  converted into Action adapters.
- Runtime submission resolves request-local agent and exact tools before
  creating ledger state. Action cancellation targets the exact owned
  submission, and finalization errors are terminal failures.
- `magent-execution-result` is the shared execution result type across runtime
  submissions and Action Steps.

This is a breaking rename. Old `magent-command-*`, `magent-agent-result`, and
Workflow-module compatibility aliases are not provided.

New isolated Action sessions are stored under `actions/`. The former
`commands/` data is not migrated or read; existing files are left untouched.

## Consequences

Extension authors have one executable abstraction and one lifecycle owner.
Frontend code can continue to call entries “commands” where ACP, slash menus,
or interactive commands require that vocabulary, without leaking it into the
domain model. Skill selection and Action invocation no longer share a
compatibility registry.

Callers must update names and registrations in one change. Existing isolated
Command session history is intentionally absent from Action viewers unless a
user handles it outside Magent.
