;;; magent-runtime.el --- Runtime state for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Central runtime orchestration for Magent.  This module owns:
;; - request-scoped execution context
;; - project-local overlay activation for agents, skills, commands, and
;;   capabilities

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
(declare-function magent-command-initialize-static "magent-command")
(declare-function magent-command-load-project-scope "magent-command")
(declare-function magent-command-remove-project-scope "magent-command")
(declare-function magent-runtime-queue-active-scope "magent-runtime-queue")
(declare-function magent-runtime-queue-execution-active-p "magent-runtime-queue")
(declare-function magent-skills-initialize-static "magent-skills")
(declare-function magent-skills-load-project-scope "magent-skills")
(declare-function magent-skills-remove-project-scope "magent-skills")

(defvar magent--initialized nil
  "Non-nil when Magent static definitions have been initialized.")

(defvar-local magent-runtime-context-buffer-p nil
  "Non-nil when the current buffer is a Magent-owned UI context buffer.")

(defvar magent-runtime-command-scope-functions nil
  "Functions that may resolve the scope for the current command buffer.
Each function is called without arguments.  The first non-nil return value
wins; when all functions return nil, scope is derived from
`default-directory'.")

(defconst magent-runtime--overlay-specs
  '((:name agents
     :state-variable magent-agent-registry--agents
     :static-feature magent-agent-registry
     :static magent-agent-initialize-static
     :load-project-feature magent-agent-file
     :load-project magent-agent-file-load-project-scope
     :unload-project-feature magent-agent-registry
     :unload-project magent-agent-registry-remove-project-scope
     :project-enabled magent-load-custom-agents)
    (:name skills
     :state-variable magent-skills--registry
     :static-feature magent-skills
     :static magent-skills-initialize-static
     :load-project-feature magent-skills
     :load-project magent-skills-load-project-scope
     :unload-project-feature magent-skills
     :unload-project magent-skills-remove-project-scope)
    (:name commands
     :state-variable magent-command--registry
     :static-feature magent-command
     :static magent-command-initialize-static
     :load-project-feature magent-command
     :load-project magent-command-load-project-scope
     :unload-project-feature magent-command
     :unload-project magent-command-remove-project-scope)
    (:name capabilities
     :state-variable magent-capability--registry
     :static-feature magent-capability
     :static magent-capability-initialize-static
     :load-project-feature magent-capability
     :load-project magent-capability-load-project-scope
     :unload-project-feature magent-capability
     :unload-project magent-capability-remove-project-scope))
  "Ordered overlay pipeline for Magent runtime definitions.")

(cl-defstruct (magent-request-context
               (:constructor magent-request-context-create)
               (:copier nil))
  "Request-scoped runtime state for one Magent execution."
  id
  scope
  session
  turn-id
  approval-session
  origin-buffer-name
  origin-context
  (ui-visibility 'full)
  parent-request-id
  (agent-depth 0)
  project-root
  model
  backend
  temperature
  top-p
  effort
  skill-names
  capability-context
  permission-profile
  approval-provider
  observer
  (observer-seq 0)
  submission-id
  live-p
  event-context
  abort-controller)

(defvar magent-request-context--audit-contexts
  (make-hash-table :test #'eq :weakness 'key)
  "Frozen scalar audit snapshots keyed by request-context identity.
Keeping this outside the struct preserves active request objects across a
source reload from older Magent versions.")

(defun magent-request-context--copy-audit-snapshot (snapshot)
  "Return an independent scalar copy of audit SNAPSHOT."
  (when snapshot
    (cl-loop for (key value) on snapshot by #'cddr
             append (list key (if (stringp value)
                                  (copy-sequence value)
                                value)))))

(defun magent-request-context--set-audit-context (context snapshot)
  "Freeze scalar audit SNAPSHOT for CONTEXT outside the struct layout."
  (puthash context
           (magent-request-context--copy-audit-snapshot snapshot)
           magent-request-context--audit-contexts)
  context)

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
    (let ((snapshot (gethash context magent-request-context--audit-contexts)))
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
                         (file-truename (expand-file-name project-root))
                       (error (expand-file-name project-root)))))
               (agent (and valid-session
                           (magent-session-agent valid-session)))
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
                              ((and agent (fboundp 'magent-agent-info-name))
                               (ignore-errors (magent-agent-info-name agent)))
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
          (magent-request-context--set-audit-context context snapshot)))
      ;; Consumers receive their own plist so provider hooks cannot mutate the
      ;; request-owned attribution captured for later lifecycle events.
      (magent-request-context--copy-audit-snapshot
       (gethash context magent-request-context--audit-contexts)))))

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
    (when-let* ((feature (plist-get spec :static-feature)))
      (require feature))
    (when-let* ((fn (plist-get spec :static)))
      (funcall fn))))

(defun magent-runtime--phase-feature-key (phase)
  "Return the feature key associated with overlay PHASE."
  (pcase phase
    (:load-project :load-project-feature)
    (:unload-project :unload-project-feature)))

(defun magent-runtime--run-project-overlay-phase (phase scope)
  "Run project overlay PHASE for SCOPE across all registered specs."
  (dolist (spec magent-runtime--overlay-specs)
    (when (or (eq phase :unload-project)
              (magent-runtime--project-overlay-enabled-p spec))
      (when-let* ((feature-key (magent-runtime--phase-feature-key phase))
                  (feature (plist-get spec feature-key)))
        (require feature))
      (when-let* ((fn (plist-get spec phase)))
        (funcall fn scope)))))

(defun magent-runtime--copy-overlay-state (value)
  "Return a transaction snapshot of overlay registry VALUE."
  (cond
   ((hash-table-p value) (copy-hash-table value))
   ((consp value) (copy-tree value))
   ((sequencep value) (copy-sequence value))
   (t value)))

(defun magent-runtime--snapshot-overlay-state ()
  "Capture the registry state owned by every project overlay spec."
  (cl-loop for spec in magent-runtime--overlay-specs
           for variable = (plist-get spec :state-variable)
           when (and variable (boundp variable))
           collect (cons variable
                         (magent-runtime--copy-overlay-state
                          (symbol-value variable)))))

(defun magent-runtime--restore-overlay-state (snapshot)
  "Restore project overlay registries from SNAPSHOT."
  (dolist (entry snapshot)
    (set (car entry) (cdr entry))))

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

(defun magent-runtime-command-scope ()
  "Return the scope implied by the current interactive command context."
  (or (run-hook-with-args-until-success
       'magent-runtime-command-scope-functions)
      (magent-session-scope-from-directory default-directory)))

(defun magent-runtime-prepare-command-context (&optional scope)
  "Ensure Magent is initialized and activate the command SCOPE.
When SCOPE is nil, derive it from the current buffer context."
  (magent-runtime-ensure-initialized)
  (let ((target (or scope (magent-runtime-command-scope))))
    (when (and (fboundp 'magent-runtime-queue-execution-active-p)
               (magent-runtime-queue-execution-active-p)
               (not (equal target
                           (magent-runtime-queue-active-scope))))
      (user-error
       "Magent: cannot switch project definitions while a turn is active"))
    (magent-runtime-activate-scope target)))

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
      (let ((previous-scope magent-runtime--active-project-scope)
            (snapshot (magent-runtime--snapshot-overlay-state)))
        (condition-case err
            (progn
              (magent-runtime--unload-project-overlay previous-scope)
              (setq magent-runtime--active-project-scope nil)
              (when target-project-scope
                (magent-runtime--load-project-overlay target-project-scope))
              (setq magent-runtime--active-project-scope target-project-scope))
          (error
           ;; A loader may fail after registering only part of an overlay.
           ;; Give every owner a chance to release non-registry state, then
           ;; restore the exact definitions that were active before the
           ;; switch.  The original activation error remains authoritative.
           (when target-project-scope
             (ignore-errors
               (magent-runtime--unload-project-overlay target-project-scope)))
           (magent-runtime--restore-overlay-state snapshot)
           (setq magent-runtime--active-project-scope previous-scope)
           (signal (car err) (cdr err)))))))
  (when-let* ((session (and (not (eq scope 'global))
                           (magent-session-get-if-present scope))))
    (magent-session-refresh-agent session))
  (when (eq scope 'global)
    (when-let* ((session (magent-session-get-if-present 'global)))
      (magent-session-refresh-agent session)))
  scope)

(provide 'magent-runtime)
;;; magent-runtime.el ends here
