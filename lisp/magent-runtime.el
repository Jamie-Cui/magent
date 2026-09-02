;;; magent-runtime.el --- Runtime state for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Central runtime orchestration for Magent.  This module owns:
;; - request-scoped execution context
;; - project-local definition loading for agents, skills, and capabilities

;;; Code:

(require 'cl-lib)
(require 'magent-agent-info)
(require 'magent-audit)
(require 'magent-lifecycle-events)
(require 'magent-log)
(require 'magent-session)

(defvar magent-load-custom-agents)

(declare-function magent-agent-file-load-project-scope "magent-agent-file")
(declare-function magent-agent-initialize-static "magent-agent-registry")
(declare-function magent-agent-registry-remove-project-scope "magent-agent-registry")
(declare-function magent-capability-initialize-static "magent-capability")
(declare-function magent-capability-load-project-scope "magent-capability")
(declare-function magent-capability-remove-project-scope "magent-capability")
(declare-function magent-action-initialize-static "magent-action")
(declare-function magent-skills-initialize-static "magent-skills")
(declare-function magent-skills-load-project-scope "magent-skills")
(declare-function magent-skills-remove-project-scope "magent-skills")

(defvar magent--initialized nil
  "Non-nil when Magent static definitions have been initialized.")

(defvar-local magent-runtime-context-buffer-p nil
  "Non-nil when the current buffer is a Magent-owned UI context buffer.")

(defvar magent-runtime-context-scope-functions nil
  "Functions that may resolve the scope for the current interactive context.
Each function is called without arguments.  The first non-nil return value
wins; when all functions return nil, scope is derived from
`default-directory'.")

(defconst magent-runtime--definition-specs
  '((:name agents
           :static-feature magent-agent-registry
           :static magent-agent-initialize-static
           :load-project-feature magent-agent-file
           :load-project magent-agent-file-load-project-scope
           :remove-project-feature magent-agent-registry
           :remove-project magent-agent-registry-remove-project-scope
           :project-enabled magent-load-custom-agents)
    (:name skills
           :static-feature magent-skills
           :static magent-skills-initialize-static
           :load-project-feature magent-skills
           :load-project magent-skills-load-project-scope
           :remove-project-feature magent-skills
           :remove-project magent-skills-remove-project-scope)
    (:name actions
           :static-feature magent-action
           :static magent-action-initialize-static)
    (:name capabilities
           :static-feature magent-capability
           :static magent-capability-initialize-static
           :load-project-feature magent-capability
           :load-project magent-capability-load-project-scope
           :remove-project-feature magent-capability
           :remove-project magent-capability-remove-project-scope))
  "Ordered initialization and project-loading pipeline for definitions.")

(cl-defstruct (magent-request-context
               (:constructor magent-request-context-create)
               (:copier nil))
  "Request-scoped runtime state for one Magent execution."
  id
  scope
  session
  prompt
  agent
  turn-id
  approval-session
  origin-buffer-name
  origin-context
  (ui-visibility 'full)
  parent-request-id
  (agent-depth 0)
  project-root
  model-route
  parent-model-route
  model
  backend
  temperature
  top-p
  effort
  (tool-names :all)
  skill-names
  capability-context
  permission-profile
  approval-provider
  observer
  (observer-seq 0)
  submission-id
  live-p
  event-context
  audit-context
  abort-controller)

(defun magent-request-context--copy-audit-snapshot (snapshot)
  "Return an independent scalar copy of audit SNAPSHOT."
  (when snapshot
    (cl-loop for (key value) on snapshot by #'cddr
             append (list key (if (stringp value)
                                  (copy-sequence value)
                                value)))))

(defun magent-request-context-ui-visible-p (context)
  "Return non-nil when CONTEXT should render UI details."
  (or (null context)
      (eq (magent-request-context-ui-visibility context) 'full)))

(defun magent-request-context-session-id (context)
  "Return CONTEXT's session id, if available."
  (when-let* ((session (and context
                            (magent-request-context-session context))))
    (magent-session-get-id session)))

(defun magent-request-context-audit-snapshot (context)
  "Return immutable scalar audit attribution captured from CONTEXT.
The returned plist deliberately excludes sessions, callbacks, provider
objects, and other live runtime state so lifecycle sinks and completed
  approval records cannot retain an entire request graph."
  (when (magent-request-context-p context)
    (let ((snapshot (magent-request-context-audit-context context)))
      (unless snapshot
        (let* ((session (magent-request-context-session context))
               (valid-session (and (magent-session-p session) session))
               (scope (magent-request-context-scope context))
               (project-root
                (or (and (stringp (magent-request-context-project-root context))
                         (magent-request-context-project-root context))
                    (and (stringp scope) scope)))
               (canonical-root
                (and project-root
                     (condition-case nil
                         (magent-session-canonical-scope project-root)
                       (error (expand-file-name project-root)))))
               (agent (or (magent-request-context-agent context)
                          (and valid-session
                               (magent-session-agent valid-session))))
               (candidate-event-context
                (magent-request-context-event-context context))
               (event-context
                (and (magent-lifecycle-events-context-p
                      candidate-event-context)
                     candidate-event-context)))
          (setq snapshot
                (list :attribution-source 'request-snapshot
                      :session-id (and valid-session
                                       (magent-session-get-id valid-session))
                      :scope scope
                      :project-root canonical-root
                      :project-id (and canonical-root
                                       (substring
                                        (secure-hash 'sha256 canonical-root)
                                        0 16))
                      :agent (cond
                              ((magent-agent-info-p agent)
                               (magent-agent-info-name agent))
                              ((symbolp agent) (symbol-name agent))
                              ((stringp agent) agent))
                      :turn-id
                      (and event-context
                           (magent-lifecycle-events-context-turn-id
                            event-context))
                      :subagent-id
                      (and event-context
                           (magent-lifecycle-events-context-subagent-id
                            event-context))))
          (setf (magent-request-context-audit-context context)
                (magent-request-context--copy-audit-snapshot snapshot))))
      ;; Consumers receive their own plist so provider hooks cannot mutate the
      ;; request-owned attribution captured for later lifecycle events.
      (magent-request-context--copy-audit-snapshot
       (magent-request-context-audit-context context)))))

(defun magent-request-context-notify (context type &rest props)
  "Notify CONTEXT's request-local observer of TYPE with PROPS.
The observer receives a Magent-native plist event.  Observer errors are
isolated so UI/backend rendering cannot break the active agent turn."
  (when-let* ((observer (and context
                             (magent-request-context-observer context))))
    (setf (magent-request-context-observer-seq context)
          (1+ (or (magent-request-context-observer-seq context) 0)))
    (let ((event (append
                  (list :type type
                        :seq (magent-request-context-observer-seq context)
                        :time (float-time)
                        :session-id (magent-request-context-session-id context)
                        :submission-id
                        (magent-request-context-submission-id context)
                        :turn-id (magent-request-context-turn-id context))
                  props)))
      (condition-case err
          (funcall observer event)
        (error
         (magent-log "ERROR request observer failed: %s (type=%S)"
                     (error-message-string err) type))))
    t))

(defvar magent-runtime--active-project-scope nil
  "Most recently prepared interactive project scope.
Project definitions for other scopes remain registered.  Nil means the
interactive context is global, not that project definitions were unloaded.")

(defun magent-runtime-active-project-scope ()
  "Return the most recently prepared interactive project scope, or nil."
  magent-runtime--active-project-scope)

(defun magent-runtime--project-definitions-enabled-p (spec)
  "Return non-nil when project definitions for SPEC should load."
  (let ((enabled (plist-get spec :project-enabled)))
    (cond
     ((null enabled) t)
     ((symbolp enabled) (symbol-value enabled))
     ((functionp enabled) (funcall enabled))
     (t enabled))))

(defun magent-runtime--run-static-initializers ()
  "Run all static definition initializers in dependency order."
  (dolist (spec magent-runtime--definition-specs)
    (when-let* ((feature (plist-get spec :static-feature)))
      (require feature))
    (when-let* ((fn (plist-get spec :static)))
      (funcall fn))))

(defun magent-runtime--phase-feature-key (phase)
  "Return the feature key associated with definition PHASE."
  (pcase phase
    (:load-project :load-project-feature)
    (:remove-project :remove-project-feature)))

(defun magent-runtime--run-project-definition-phase (phase scope)
  "Run project definition PHASE for SCOPE across all registered specs."
  (dolist (spec magent-runtime--definition-specs)
    (when (or (eq phase :remove-project)
              (magent-runtime--project-definitions-enabled-p spec))
      (when-let* ((feature-key (magent-runtime--phase-feature-key phase))
                  (feature (plist-get spec feature-key)))
        (require feature))
      (when-let* ((fn (plist-get spec phase)))
        (funcall fn scope)))))

(defun magent-runtime-initialize-static ()
  "Load Magent definitions that are independent of project scope."
  (magent-runtime--run-static-initializers))

(defun magent-runtime-ensure-initialized ()
  "Ensure Magent static runtime definitions are initialized."
  (unless magent--initialized
    (magent-log "INFO Initializing Magent runtime definitions...")
    (magent-audit-enable)
    (magent-runtime-initialize-static)
    (setq magent--initialized t)
    (magent-log "INFO magent initialization complete"))
  magent--initialized)

(defun magent-runtime-context-scope ()
  "Return the scope implied by the current interactive context."
  (or (run-hook-with-args-until-success
       'magent-runtime-context-scope-functions)
      (magent-session-scope-from-directory default-directory)))

(defun magent-runtime-prepare-context (&optional scope)
  "Ensure Magent is initialized and activate SCOPE.
When SCOPE is nil, derive it from the current buffer context."
  (magent-runtime-ensure-initialized)
  (let ((target (or scope (magent-runtime-context-scope))))
    (magent-runtime-activate-scope target)))

(defun magent-runtime--remove-project-definitions (scope)
  "Remove retained project definitions for SCOPE."
  (when scope
    (magent-runtime--run-project-definition-phase :remove-project scope)))

(defun magent-runtime--load-project-definitions (scope)
  "Load or refresh project definitions for SCOPE."
  (when scope
    (magent-runtime--run-project-definition-phase :load-project scope)
    (magent-log "INFO loaded project definitions for %s" scope)))

(defun magent-runtime-activate-scope (scope &optional force)
  "Prepare SCOPE and load or refresh its project-local definitions.
SCOPE must be either the symbol `global' or a normalized project root.
Definitions from other project scopes remain registered and are filtered at
resolution time.  When FORCE is non-nil, reload SCOPE even if it is unchanged."
  (let ((target-project-scope (unless (eq scope 'global) scope)))
    (when (or force
              (not (equal target-project-scope magent-runtime--active-project-scope)))
      (let ((previous-scope magent-runtime--active-project-scope))
        (condition-case err
            (progn
              (when target-project-scope
                (magent-runtime--load-project-definitions target-project-scope))
              (setq magent-runtime--active-project-scope target-project-scope))
          (error
           ;; Project definitions remain retained across ordinary scope
           ;; switches.  A failed target load is removed fail-closed without
           ;; disturbing definitions owned by another scope.
           (when target-project-scope
             (condition-case cleanup-error
                 (magent-runtime--remove-project-definitions
                  target-project-scope)
               (error
                (magent-log
                 "WARN failed to remove partial project definitions: %s"
                 (error-message-string cleanup-error)))))
           (setq magent-runtime--active-project-scope
                 (unless (equal previous-scope target-project-scope)
                   previous-scope))
           (signal (car err) (cdr err)))))))
  (when-let* ((session (and (not (eq scope 'global))
                            (magent-session-get-if-present scope))))
    (magent-session-refresh-agent session scope))
  (when (eq scope 'global)
    (when-let* ((session (magent-session-get-if-present 'global)))
      (magent-session-refresh-agent session 'global)))
  scope)

(provide 'magent-runtime)
;;; magent-runtime.el ends here
