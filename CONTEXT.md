# Magent

Magent is an Emacs-native agent runtime whose explicit commands combine
trusted Elisp control flow with managed asynchronous agent capabilities.

## Language

**Command**:
An explicitly registered user action that owns metadata and one Workflow.
_Avoid_: Handler, prompt command

**Workflow**:
The non-blocking Elisp control program executed for a Command.
_Avoid_: Handler lifecycle, workflow run

**Command Invocation**:
One execution of a Command and the sole owner of its runtime state.
_Avoid_: Workflow run, command context

**Step**:
An asynchronous boundary that a Workflow yields to Magent for waiting,
cancellation, progress, and activity recording. It is not a security or
side-effect boundary.
_Avoid_: Stage, task node

**Answer Step**:
A terminal agent Step that streams the final user-visible response and ends
the Command Invocation.
_Avoid_: Final visibility mode

**Requirement**:
An Emacs feature that must load successfully before a Command Invocation can
start. It does not install packages or require a project workspace.
_Avoid_: Project requirement, package dependency installer
