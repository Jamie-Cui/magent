# Magent

Magent is an Emacs-native agent runtime whose explicit Actions combine
trusted Elisp control flow with managed asynchronous agent capabilities.

## Language

**Action**:
An explicitly registered executable user action that owns metadata and one
Workflow. It may be projected as a slash command, an interactive command, or
both.
_Avoid_: Handler, prompt command, Command as a domain object

**Workflow**:
The non-blocking Elisp control program executed for an Action.
_Avoid_: Handler lifecycle, workflow run

**Action Invocation**:
One execution of an Action and the sole owner of its runtime state.
_Avoid_: Workflow run, command context, Action context

**Command**:
A frontend or protocol projection used to invoke an Action, such as an ACP
slash command or an Emacs interactive command.
_Avoid_: A second executable extension abstraction

**Step**:
An asynchronous boundary that a Workflow yields to Magent for waiting,
cancellation, progress, and activity recording. It is not a security or
side-effect boundary.
_Avoid_: Stage, task node

**Answer Step**:
A terminal agent Step that streams the final user-visible response and ends
the Action Invocation.
_Avoid_: Final visibility mode

**Requirement**:
An Emacs feature that must load successfully before an Action Invocation can
start. It does not install packages or require a project workspace.
_Avoid_: Project requirement, package dependency installer
