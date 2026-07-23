# Use one Elisp Workflow model for commands

- Status: Accepted
- Date: 2026-07-23

## Context

Commands previously split extensions between declarative turns and imperative
handlers. That duplicated completion and cancellation contracts, while complex
handlers rebuilt sequencing, process ownership, progress, and cleanup by hand.
A declarative step graph would add another control-flow language even though
trusted Elisp already supplies branching, loops, validation, local state, and
direct integration with Emacs objects.

## Decision

Every Command registration owns one generator-backed Elisp Workflow and an
explicit session policy. A Command Invocation is the sole runtime lifecycle
owner.

Workflow Elisp remains ordinary, brief, and non-blocking. It yields managed
Steps only at asynchronous boundaries:

- Agent Steps run intermediate model turns.
- Answer Steps run one terminal streaming model turn.
- Process Steps run a string argv list without a shell-command DSL.
- Callback Steps adapt existing asynchronous Elisp APIs.

Process and callback Steps may overlap between Invocations. Agent and Answer
Steps use the existing runtime FIFO. Every Step receives start/finish activity
recording and Invocation-owned cancellation.

A Workflow that reaches its end returns a user-visible string or nil. Process,
agent, and callback failures signal typed, recoverable conditions inside the
generator. Cancellation is terminal and never resumes Workflow Elisp.

Command registration uses `:requires` for Emacs feature symbols loaded during
invocation preflight. Tool requirements belong to the agent Step that uses
them. Commands have no project-workspace requirement.

## Consequences

This is a breaking change. `:turn`, `:handler`, `:requires-project`, and the
public handler lifecycle functions are removed without compatibility wrappers.
Built-in commands, skill adapters, isolated maintenance commands, live tests,
and third-party examples must use the Workflow API.

The extension surface is smaller and lifecycle behavior is uniform. Extension
authors can express complex orchestration in Elisp without manually owning
process sentinels, runtime submission callbacks, or terminal cleanup.
