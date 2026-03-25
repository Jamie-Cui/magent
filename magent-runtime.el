;;; magent-runtime.el --- Runtime state for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; Central runtime orchestration for Magent.  This module owns:
;; - request-scoped execution context
;; - project-local overlay activation for agents, skills, and capabilities

;;; Code:

(require 'cl-lib)
(require 'magent-audit)
(require 'magent-session)

(defvar magent-load-custom-agents)

(declare-function magent-agent-file-load-project-scope "magent-agent-file")
(declare-function magent-agent-initialize-static "magent-agent-registry")
(declare-function magent-agent-registry-remove-project-scope "magent-agent-registry")
(declare-function magent-capability-initialize-static "magent-capability")
(declare-function magent-capability-load-project-scope "magent-capability")
(declare-function magent-capability-remove-project-scope "magent-capability")
(declare-function magent-audit-enable "magent-audit")
(declare-function magent-log "magent-ui")
(declare-function magent-session-refresh-agent "magent-session")
(declare-function magent-skills-initialize-static "magent-skills")
(declare-function magent-skills-load-project-scope "magent-skills")
(declare-function magent-skills-remove-project-scope "magent-skills")

(defvar magent--initialized nil
  "Non-nil when Magent static definitions have been initialized.")

(defconst magent-runtime--overlay-specs
  '((:name agents
     :static magent-agent-initialize-static
     :load-project magent-agent-file-load-project-scope
     :unload-project magent-agent-registry-remove-project-scope
     :project-enabled magent-load-custom-agents)
    (:name skills
     :static magent-skills-initialize-static
     :load-project magent-skills-load-project-scope
     :unload-project magent-skills-remove-project-scope)
    (:name capabilities
     :static magent-capability-initialize-static
     :load-project magent-capability-load-project-scope
     :unload-project magent-capability-remove-project-scope))
  "Ordered overlay pipeline for Magent runtime definitions.")

(cl-defstruct (magent-request-context
               (:constructor magent-request-context-create)
               (:copier nil))
  "Request-scoped runtime state for one Magent execution."
  id
  scope
  session
  approval-session
  origin-buffer-name
  origin-context
  (ui-visibility 'full)
  parent-request-id
  live-p
  event-context
  abort-controller)

(defun magent-request-context-ui-visible-p (context)
  "Return non-nil when CONTEXT should render UI details."
  (or (null context)
      (eq (magent-request-context-ui-visibility context) 'full)))

(defvar magent-runtime--active-project-scope nil
  "Current project scope whose overlay definitions are active.
Nil means only static definitions are loaded.")

(defun magent-runtime-active-project-scope ()
  "Return the currently active project overlay scope, or nil."
  magent-runtime--active-project-scope)

(defun magent-runtime--project-overlay-enabled-p (spec)
  "Return non-nil when project overlay SPEC should load."
  (let ((enabled (plist-get spec :project-enabled)))
    (cond
     ((null enabled) t)
     ((symbolp enabled) (symbol-value enabled))
     ((functionp enabled) (funcall enabled))
     (t enabled))))

(defun magent-runtime--run-static-initializers ()
  "Run all static definition initializers in dependency order."
  (dolist (spec magent-runtime--overlay-specs)
    (when-let ((fn (plist-get spec :static)))
      (funcall fn))))

(defun magent-runtime--run-project-overlay-phase (phase scope)
  "Run project overlay PHASE for SCOPE across all registered specs."
  (dolist (spec magent-runtime--overlay-specs)
    (when (or (eq phase :unload-project)
              (magent-runtime--project-overlay-enabled-p spec))
      (when-let ((fn (plist-get spec phase)))
        (funcall fn scope)))))

(defun magent-runtime-initialize-static ()
  "Load Magent definitions that are independent of project scope."
  (magent-runtime--run-static-initializers))

(defun magent-runtime-ensure-initialized ()
  "Ensure Magent static runtime definitions are initialized."
  (unless magent--initialized
    (magent-log "INFO Initializing magent agents and skills...")
    (magent-audit-enable)
    (magent-runtime-initialize-static)
    (setq magent--initialized t)
    (magent-log "INFO magent initialization complete"))
  magent--initialized)

(defun magent-runtime-command-scope ()
  "Return the scope implied by the current interactive command context."
  (if (derived-mode-p 'magent-output-mode)
      (or (and (boundp 'magent-ui--buffer-scope)
               magent-ui--buffer-scope)
          (magent-session-current-scope))
    (magent-session-scope-from-directory default-directory)))

(defun magent-runtime-prepare-command-context (&optional scope)
  "Ensure Magent is initialized and activate the command SCOPE.
When SCOPE is nil, derive it from the current buffer context."
  (magent-runtime-ensure-initialized)
  (magent-runtime-activate-scope
   (or scope (magent-runtime-command-scope))))

(defun magent-runtime--unload-project-overlay (scope)
  "Unload project-local overlay definitions for SCOPE."
  (when scope
    (magent-runtime--run-project-overlay-phase :unload-project scope)
    (magent-log "INFO unloaded project overlay for %s" scope)))

(defun magent-runtime--load-project-overlay (scope)
  "Load project-local overlay definitions for SCOPE."
  (when scope
    (magent-runtime--run-project-overlay-phase :load-project scope)
    (magent-log "INFO loaded project overlay for %s" scope)))

(defun magent-runtime-activate-scope (scope &optional force)
  "Activate project-local overlay definitions for SCOPE.
SCOPE must be either the symbol `global' or a normalized project root.
When FORCE is non-nil, reload the overlay even if SCOPE is unchanged."
  (let ((target-project-scope (unless (eq scope 'global) scope)))
    (when (or force
              (not (equal target-project-scope magent-runtime--active-project-scope)))
      (magent-runtime--unload-project-overlay
       magent-runtime--active-project-scope)
      (setq magent-runtime--active-project-scope nil)
      (when target-project-scope
        (magent-runtime--load-project-overlay target-project-scope)
        (setq magent-runtime--active-project-scope target-project-scope))))
  (when-let ((session (and (not (eq scope 'global))
                           (magent-session-get-if-present scope))))
    (magent-session-refresh-agent session))
  (when (eq scope 'global)
    (when-let ((session (magent-session-get-if-present 'global)))
      (magent-session-refresh-agent session)))
  scope)

(provide 'magent-runtime)
;;; magent-runtime.el ends here
