;;; magent-runtime-api.el --- UI-neutral Magent runtime API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Stable UI/backend-facing runtime API.  UI backends submit prompts to a
;; runtime session and receive request-local Magent-native observer events.
;; Submission freezes one request context before queueing; the queue does not
;; reconstruct execution policy when the turn starts.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magent-agent)
(require 'magent-agent-info)
(require 'magent-agent-loop)
(require 'magent-agent-registry)
(require 'magent-config)
(require 'magent-protocol)
(require 'magent-prompt)
(require 'magent-runtime)
(require 'magent-runtime-queue)
(require 'magent-session)
(require 'magent-ledger)

(declare-function magent-skills-get "magent-skills")
(declare-function magent-skill-requires-project "magent-skills" t t)
(declare-function magent-tools-get-gptel-tools-for-permission "magent-tools")
(cl-defstruct (magent-runtime-session
               (:constructor magent-runtime-session-create)
               (:copier nil))
  id
  scope
  magent-session
  model-route
  effort
  pending-skills
  metadata)

(defvar magent-runtime-api--sessions (make-hash-table :test #'equal)
  "Runtime session wrappers keyed by (SCOPE SESSION-ID).")

(defvar magent-runtime-api--clearing-sessions
  (make-hash-table :test #'eq :weakness 'key)
  "Exact runtime sessions currently inside a clear transaction.")

(defun magent-runtime-api--session-key (scope session-id)
  "Return registry key for SCOPE and SESSION-ID."
  (list scope session-id))

(defun magent-runtime-api--session-id (session)
  "Return SESSION's stable id."
  (magent-session-get-id session))

(defun magent-runtime-api--scope-lease-conflict-p (scope session)
  "Return non-nil when installing SESSION at SCOPE would steal a lease."
  (when-let* ((active-scope (magent-runtime-queue-active-scope)))
    (and (equal (magent-session-scope-origin scope) active-scope)
         (not (eq (magent-runtime-queue-active-session-object) session)))))

(defun magent-runtime-session-ensure-registerable
    (scope session &optional wrapper-only)
  "Signal when registering SESSION at SCOPE would violate a queue lease.
When WRAPPER-ONLY is non-nil, validate only the runtime wrapper registration;
the currently installed session is not replaced."
  (let* ((id (magent-runtime-api--session-id session))
         (existing
          (gethash (magent-runtime-api--session-key scope id)
                   magent-runtime-api--sessions))
         (existing-session
          (and existing (magent-runtime-session-magent-session existing)))
         (installed (and (not wrapper-only)
                         (magent-session-get-if-present scope))))
    (when (and (not wrapper-only)
               (magent-runtime-api--scope-lease-conflict-p scope session))
      (user-error
       "Magent: cannot replace a session while its scope owns the execution lease"))
    (dolist (candidate (delq nil (list existing-session installed)))
      (when (and (not (eq candidate session))
                 (magent-runtime-queue-session-busy-p candidate))
        (user-error
         "Magent: cannot replace a session with active or queued work")))
    t))

(defun magent-runtime-api--session-clearing-p (runtime-session)
  "Return non-nil during RUNTIME-SESSION's exact clear transaction."
  (gethash runtime-session magent-runtime-api--clearing-sessions))

(defun magent-runtime-api--assert-session-available (runtime-session)
  "Signal when RUNTIME-SESSION cannot accept a new state mutation."
  (when (magent-runtime-api--session-clearing-p runtime-session)
    (user-error "Magent: session is being cleared"))
  t)

(defun magent-runtime-api--wrap-session (session scope)
  "Return runtime wrapper for Magent SESSION at SCOPE."
  (magent-runtime-session-ensure-registerable scope session t)
  (let* ((id (magent-runtime-api--session-id session))
         (key (magent-runtime-api--session-key scope id))
         (existing (gethash key magent-runtime-api--sessions)))
    (if existing
        (progn
          (setf (magent-runtime-session-scope existing) scope
                (magent-runtime-session-magent-session existing) session)
          existing)
      (let ((runtime-session
             (magent-runtime-session-create
              :id id
              :scope scope
              :magent-session session)))
        (puthash key runtime-session magent-runtime-api--sessions)
        runtime-session))))

(defun magent-runtime-session-current (&optional scope)
  "Return the current runtime session for SCOPE."
  (magent-runtime-ensure-initialized)
  (let ((target-scope (or scope (magent-session-current-scope))))
    (if-let* ((session (magent-session-get-if-present target-scope)))
        (progn
          (magent-session-activate target-scope)
          (magent-runtime-api--wrap-session session target-scope))
      ;; Preflight an absent scope before `magent-session-activate' can create
      ;; and install a replacement underneath an exact active-session lease.
      (magent-runtime-session-register
       target-scope (magent-session-create)))))

(defun magent-runtime-session-new (&optional scope)
  "Create a new runtime session for SCOPE.
Activate it immediately unless another session from the same scope owns the
execution lease.  In that case, keep the new session detached until its first
queued submission starts."
  (magent-runtime-ensure-initialized)
  (let* ((target-scope (or scope (magent-session-current-scope)))
         (session (magent-session-create)))
    (if (magent-runtime-api--scope-lease-conflict-p target-scope session)
        (magent-runtime-api--wrap-session session target-scope)
      (magent-runtime-session-register target-scope session))))

(defun magent-runtime-session-fork (source-runtime-session)
  "Return a detached, durable fork of SOURCE-RUNTIME-SESSION.
The source must have no active or queued work.  Conversation state and stable
session options are copied, while approvals, child jobs, and one-shot skills
start empty.  The fork remains detached from the ambient scope until its first
submission so the source frontend stays current."
  (unless (magent-runtime-session-p source-runtime-session)
    (error "Expected a runtime session, got: %S" source-runtime-session))
  (magent-runtime-api--assert-session-available source-runtime-session)
  (let* ((source-session
          (magent-runtime-session-magent-session source-runtime-session))
         (scope (magent-runtime-session-scope source-runtime-session)))
    (unless scope
      (error "Magent: cannot fork a runtime session without an explicit scope"))
    (when-let* ((thread (magent-session-thread source-session)))
      (unless (equal (magent-thread-scope thread) scope)
        (error "Magent: source thread scope does not match its runtime scope")))
    (when (magent-runtime-queue-session-busy-p source-session)
      (user-error "Magent: cannot fork a session with active or queued work"))
    (let* ((fork-session (magent-session-fork source-session scope))
           (fork-id (magent-session-get-id fork-session))
           (source-id (magent-runtime-session-id source-runtime-session))
           (key (magent-runtime-api--session-key scope fork-id))
           (spill-ids
            (magent-thread-spill-result-ids
             (magent-session-thread source-session)))
           runtime-session
           completed)
      (unless (equal source-id (magent-session-id source-session))
        (error "Magent: runtime and durable session ids do not match"))
      (unwind-protect
          (progn
            ;; Detached registration cannot steal the source's ambient scope
            ;; slot or execution lease.  Preflight before creating artifacts.
            (magent-runtime-session-ensure-registerable
             scope fork-session t)
            (magent-tool-output-spill-fork-session
             scope source-id fork-id spill-ids)
            (magent-session-save-for-session fork-session scope)
            (setq runtime-session
                  (magent-runtime-api--wrap-session fork-session scope))
            (setf (magent-runtime-session-effort runtime-session)
                  (magent-runtime-session-effort source-runtime-session)
                  (magent-runtime-session-model-route runtime-session)
                  (magent-runtime-session-model-route source-runtime-session)
                  (magent-runtime-session-pending-skills runtime-session) nil
                  (magent-runtime-session-metadata runtime-session)
                  (list :capabilities-enabled
                        (magent-runtime-session-capabilities-enabled-p
                         source-runtime-session)))
            (setq completed t)
            (magent-log "INFO runtime session forked: %s -> %s scope=%s"
                        source-id fork-id scope)
            runtime-session)
        (unless completed
          (when (eq (gethash key magent-runtime-api--sessions)
                    runtime-session)
            (remhash key magent-runtime-api--sessions))
          (condition-case err
              (magent-session-clear fork-session scope)
            (error
             (magent-log "WARN failed rolling back fork session %s: %s"
                         fork-id (error-message-string err))))
          (condition-case err
              (magent-tool-output-spill-delete-session scope fork-id)
            (error
             (magent-log "WARN failed rolling back fork spills %s: %s"
                         fork-id (error-message-string err)))))))))

(defun magent-runtime-session-from-id (session-id &optional scope)
  "Return runtime SESSION-ID, optionally restricted to exact SCOPE."
  (if scope
      (gethash (magent-runtime-api--session-key scope session-id)
               magent-runtime-api--sessions)
    (let (found ambiguous)
      (maphash
       (lambda (_key runtime-session)
         (when (equal (magent-runtime-session-id runtime-session) session-id)
           (if (or (null found) (eq found runtime-session))
               (setq found runtime-session)
             (setq ambiguous t))))
       magent-runtime-api--sessions)
      (unless ambiguous found))))

(defun magent-runtime-session-register (scope session)
  "Install SESSION at SCOPE and return its runtime wrapper."
  (magent-runtime-session-ensure-registerable scope session)
  (magent-session-install scope session)
  (magent-runtime-api--wrap-session session scope))

(defun magent-runtime-api--resolve-agent (runtime-session &optional agent-or-name)
  "Resolve AGENT-OR-NAME for RUNTIME-SESSION without changing session state."
  (let* ((session (magent-runtime-session-magent-session runtime-session))
         (scope (magent-runtime-session-scope runtime-session))
         (agent
          (cond
           ((null agent-or-name)
            (or (and (magent-session-agent session)
                     (magent-agent-registry-get
                      (magent-agent-info-name (magent-session-agent session))
                      scope))
                (magent-agent-registry-get-default scope)))
           ((magent-agent-info-p agent-or-name) agent-or-name)
           ((stringp agent-or-name)
            (magent-agent-registry-get agent-or-name scope))
           ((symbolp agent-or-name)
            (magent-agent-registry-get
             (symbol-name agent-or-name) scope)))))
    (unless agent
      (error "Unknown Magent agent: %S" agent-or-name))
    agent))

(defun magent-runtime-session-set-agent (runtime-session agent-or-name)
  "Set RUNTIME-SESSION's agent to AGENT-OR-NAME."
  (magent-runtime-api--assert-session-available runtime-session)
  (let* ((agent (magent-runtime-api--resolve-agent
                 runtime-session agent-or-name))
         (session (magent-runtime-session-magent-session runtime-session)))
    (magent-session-set-agent session agent)
    agent))

(defun magent-runtime-session-agent-name (runtime-session)
  "Return RUNTIME-SESSION's active agent name."
  (let ((agent (magent-runtime-api--resolve-agent runtime-session)))
    (and agent (magent-agent-info-name agent))))

(defun magent-runtime-session-model-route-option (runtime-session)
  "Return RUNTIME-SESSION's explicit model route, or nil for automatic routing."
  (unless (magent-runtime-session-p runtime-session)
    (error "Expected a runtime session, got: %S" runtime-session))
  (magent-runtime-session-model-route runtime-session))

(defun magent-runtime-session-set-model-route (runtime-session route)
  "Set RUNTIME-SESSION's explicit model ROUTE and return it.
Pass nil to restore automatic routing."
  (unless (magent-runtime-session-p runtime-session)
    (error "Expected a runtime session, got: %S" runtime-session))
  (magent-runtime-api--assert-session-available runtime-session)
  (let ((stored-route
         (when route
           (magent-model-route-relabel
            (magent-sampling-gptel-validate-route route) 'session))))
    (setf (magent-runtime-session-model-route runtime-session) stored-route)
    stored-route))

(defun magent-runtime-session-clear-model-route (runtime-session)
  "Restore automatic model routing for RUNTIME-SESSION."
  (magent-runtime-session-set-model-route runtime-session nil))

(defun magent-runtime-session-effective-model-route
    (runtime-session &optional agent-or-name phase)
  "Resolve RUNTIME-SESSION's model route for AGENT-OR-NAME and PHASE.
The returned route is an immutable request-ready snapshot."
  (unless (magent-runtime-session-p runtime-session)
    (error "Expected a runtime session, got: %S" runtime-session))
  (let ((agent (magent-runtime-api--resolve-agent
                runtime-session agent-or-name)))
    (magent-agent-resolve-model-route
     agent
     :explicit-route (magent-runtime-session-model-route runtime-session)
     :phase phase)))

(defun magent-runtime-session-model-routing-configured-p
    (runtime-session &optional agent-or-name)
  "Return non-nil when RUNTIME-SESSION has enough policy to attempt routing.
AGENT-OR-NAME selects a request-local agent without changing session state.
This distinguishes a clean, not-yet-configured gptel installation from an
invalid explicit route, which must still fail loudly during resolution."
  (unless (magent-runtime-session-p runtime-session)
    (error "Expected a runtime session, got: %S" runtime-session))
  (let ((agent (magent-runtime-api--resolve-agent
                runtime-session agent-or-name)))
    (or (magent-runtime-session-model-route runtime-session)
        (magent-agent-info-model agent)
        (default-value 'gptel-backend)
        (default-value 'gptel-model))))

(defun magent-runtime-session-title (runtime-session)
  "Return RUNTIME-SESSION's canonical display title, or nil."
  (unless (magent-runtime-session-p runtime-session)
    (error "Expected a runtime session, got: %S" runtime-session))
  (magent-session-summary-title
   (magent-runtime-session-magent-session runtime-session)))

(defun magent-runtime-session-available-tool-names
    (runtime-session &optional agent-or-name)
  "Return tool symbols available to RUNTIME-SESSION's effective agent.
When AGENT-OR-NAME is non-nil, inspect that agent instead of the session's
current selection.  This is a read-only public preflight API for extensions."
  (require 'magent-agent-registry)
  (require 'magent-tools)
  (let* ((agent
          (magent-runtime-api--resolve-agent runtime-session agent-or-name))
         (permission (and agent (magent-agent-info-permission agent))))
    (mapcar (lambda (tool)
              (intern (gptel-tool-name tool)))
            (magent-tools-get-gptel-tools-for-permission permission :all))))

(defun magent-runtime-session-effort-option (runtime-session)
  "Return RUNTIME-SESSION's current effort option."
  (magent-effort-option-or-auto
   (or (magent-runtime-session-effort runtime-session)
       magent-default-effort)))

(defun magent-runtime-session-set-effort (runtime-session effort)
  "Set RUNTIME-SESSION effort option to EFFORT and return it."
  (magent-runtime-api--assert-session-available runtime-session)
  (let ((option (magent-effort-option-or-auto effort)))
    (setf (magent-runtime-session-effort runtime-session) option)
    option))

(defun magent-runtime-session-capabilities-enabled-p (runtime-session)
  "Return whether RUNTIME-SESSION should auto-resolve capabilities."
  (let ((metadata (magent-runtime-session-metadata runtime-session)))
    (if (and (proper-list-p metadata)
             (plist-member metadata :capabilities-enabled))
        (eq (plist-get metadata :capabilities-enabled) t)
      magent-enable-capabilities)))

(defun magent-runtime-session-set-capabilities-enabled
    (runtime-session enabled)
  "Set capability auto-resolution for RUNTIME-SESSION to ENABLED."
  (magent-runtime-api--assert-session-available runtime-session)
  (let ((metadata (magent-runtime-session-metadata runtime-session)))
    (unless (proper-list-p metadata)
      (setq metadata nil))
    (setf (magent-runtime-session-metadata runtime-session)
          (plist-put metadata :capabilities-enabled (and enabled t))))
  (magent-runtime-session-capabilities-enabled-p runtime-session))

(defun magent-runtime-session-clear-pending-skills (runtime-session)
  "Clear one-shot skills for RUNTIME-SESSION."
  (magent-runtime-api--assert-session-available runtime-session)
  (setf (magent-runtime-session-pending-skills runtime-session) nil))

(defun magent-runtime-session-clear (runtime-session)
  "Clear RUNTIME-SESSION while preserving its ACP-visible identity.
Any active or queued work for the session is cancelled first."
  (unless (magent-runtime-session-p runtime-session)
    (error "Expected runtime session, got: %S" runtime-session))
  (magent-runtime-api--assert-session-available runtime-session)
  (let* ((session (magent-runtime-session-magent-session runtime-session))
         (scope (magent-runtime-session-scope runtime-session))
         (installed (magent-session-get-if-present scope)))
    ;; A stale wrapper with the same persisted id must never delete or replace
    ;; the registered session's file.  Different ids have independent files
    ;; and may be cleared without stealing the scope's current-session slot.
    (when (and installed
               (not (eq installed session))
               (equal (magent-session-id installed)
                      (magent-session-id session)))
      (user-error
       "Magent: refusing to clear a stale session with a reused id"))
    (puthash runtime-session t magent-runtime-api--clearing-sessions)
    (unwind-protect
        (progn
          (magent-runtime-cancel runtime-session)
          (when (magent-runtime-queue-session-busy-p session)
            (error "Magent: session cancellation did not release all work"))
          (magent-session-clear session scope)
          (setf (magent-runtime-session-pending-skills runtime-session) nil)
          ;; Preserve another session currently selected in the same scope.
          ;; A manually constructed runtime wrapper still becomes discoverable
          ;; when no session has ever been installed there.
          (unless (magent-session-get-if-present scope)
            (magent-session-install scope session))
          (when (fboundp 'magent-clear-capability-overrides)
            (magent-clear-capability-overrides))
          (magent-log "INFO runtime session cleared: %s"
                      (magent-runtime-session-id runtime-session))
          runtime-session)
      (remhash runtime-session magent-runtime-api--clearing-sessions))))

(defun magent-runtime-api--notify-submission (submission type &rest props)
  "Notify SUBMISSION's observer of TYPE with PROPS."
  (when-let* ((request-context
               (magent-runtime-submission-request-context submission)))
    (apply #'magent-request-context-notify request-context type props)))

(defun magent-runtime-api--prepare-turn
    (runtime-session prompt &optional metadata)
  "Create a queued ledger turn for PROMPT in RUNTIME-SESSION."
  (let* ((session (magent-runtime-session-magent-session runtime-session))
         (scope (or (magent-runtime-session-scope runtime-session)
                    (magent-session-current-scope)))
         (thread (let ((magent--current-session session)
                       (magent-session--current-scope scope))
                   (magent-session-thread-ledger session)))
         (turn-metadata (append (list :source 'runtime-queue) metadata))
         (turn (magent-thread-queue-turn
                thread prompt nil turn-metadata)))
    (magent-thread-record-user-message-if-needed
     thread (magent-thread-turn-id turn) prompt nil
     turn-metadata)
    (magent-session-save-deferred-for-session session scope)
    (magent-thread-turn-id turn)))

(defun magent-runtime-api--submission-live-p (submission)
  "Return non-nil while SUBMISSION still owns the active runtime slot."
  (and (not (magent-runtime-submission-finalized submission))
       (eq (magent-runtime-submission-status submission) 'running)
       (eq submission (magent-runtime-queue-active-submission))))

(defun magent-runtime-api--call-completion (submission status result)
  "Safely call SUBMISSION's completion callback with STATUS and RESULT."
  (when-let* ((fn (magent-runtime-submission-on-complete submission)))
    (condition-case err
        (funcall fn status result)
      (error
       (magent-log "ERROR runtime completion callback failed: %s"
                   (error-message-string err))))))

(defun magent-runtime-api--finish-submission (submission status result)
  "Finish SUBMISSION with STATUS and RESULT."
  (unless (magent-runtime-submission-finalized submission)
    (setf (magent-runtime-submission-finalized submission) t
          (magent-runtime-submission-status submission) status
          (magent-runtime-submission-finished-at submission) (float-time)
          (magent-runtime-submission-detail submission) result)
    (magent-runtime-api--notify-submission
     submission
     (pcase status
       ('completed 'turn-complete)
       ('cancelled 'turn-cancelled)
       (_ 'turn-failed))
     :status status
     :result result)
    (if (eq submission (magent-runtime-queue-active-submission))
      (magent-runtime-queue-finish-active
       status result
       (lambda ()
         (magent-runtime-api--call-completion submission status result)))
      (magent-runtime-api--call-completion submission status result))))

(defun magent-runtime-api--submission-execution-scope (submission)
  "Return the project/global definition scope for SUBMISSION."
  (let* ((runtime-session
          (magent-runtime-submission-runtime-session submission))
         (scope (or (and runtime-session
                         (magent-runtime-session-scope runtime-session))
                    'global)))
    (magent-session-scope-origin scope)))

(defun magent-runtime-api--activate-submission-session (submission)
  "Prepare SUBMISSION's definition scope and install its session."
  (let ((runtime-session
         (magent-runtime-submission-runtime-session submission)))
    (magent-runtime-activate-scope
     (magent-runtime-api--submission-execution-scope submission))
    (magent-session-install
     (magent-runtime-session-scope runtime-session)
     (magent-runtime-session-magent-session runtime-session))
    ;; Scope preparation refreshes the session already registered for its
    ;; ordinary scope.  Internal and captured sessions installed only after
    ;; preparation still need an explicit refresh.
    (magent-session-refresh-agent
     (magent-runtime-session-magent-session runtime-session)
     (magent-runtime-session-scope runtime-session))))

(defun magent-runtime-api--mark-submission-turn-started (submission)
  "Mark SUBMISSION's ledger turn in progress, when it has one."
  (when-let* ((runtime-session
               (magent-runtime-submission-runtime-session submission))
              (session (magent-runtime-session-magent-session runtime-session))
              (thread (magent-session-thread-ledger session))
              (request-context
               (magent-runtime-submission-request-context submission))
              (turn-id (magent-request-context-turn-id request-context))
              (turn (magent-thread-find-turn thread turn-id)))
    (unless (magent-thread-terminal-turn-p turn)
      (magent-thread-start-turn thread turn-id)
      (magent-session-save-deferred-for-session
       session (magent-runtime-session-scope runtime-session)))))

(defun magent-runtime-api--mark-submission-turn-dropped (submission detail)
  "Mark queued SUBMISSION's ledger turn dropped with DETAIL."
  (when-let* ((runtime-session
               (magent-runtime-submission-runtime-session submission))
              (session (magent-runtime-session-magent-session runtime-session))
              (thread (magent-session-thread-ledger session))
              (request-context
               (magent-runtime-submission-request-context submission))
              (turn-id (magent-request-context-turn-id request-context))
              (turn (magent-thread-find-turn thread turn-id)))
    (unless (magent-thread-terminal-turn-p turn)
      (magent-thread-drop-turn thread turn-id detail)
      (magent-session-save-deferred-for-session
       session (magent-runtime-session-scope runtime-session)))))

(defun magent-runtime-api--mark-submission-turn-interrupted (submission detail)
  "Mark active SUBMISSION's ledger turn interrupted with DETAIL."
  (when-let* ((runtime-session
               (magent-runtime-submission-runtime-session submission))
              (session (magent-runtime-session-magent-session runtime-session))
              (thread (magent-session-thread-ledger session))
              (request-context
               (magent-runtime-submission-request-context submission))
              (turn-id (magent-request-context-turn-id request-context))
              (turn (magent-thread-find-turn thread turn-id)))
    (unless (magent-thread-terminal-turn-p turn)
      (magent-thread-interrupt-turn thread turn-id detail)
      (magent-session-save-deferred-for-session
       session (magent-runtime-session-scope runtime-session)))))

(defun magent-runtime-api--mark-submission-turn-failed (submission detail)
  "Mark SUBMISSION's ledger turn failed with DETAIL."
  (when-let* ((runtime-session
               (magent-runtime-submission-runtime-session submission))
              (session (magent-runtime-session-magent-session runtime-session))
              (thread (magent-session-thread-ledger session))
              (request-context
               (magent-runtime-submission-request-context submission))
              (turn-id (magent-request-context-turn-id request-context))
              (turn (magent-thread-find-turn thread turn-id)))
    (unless (magent-thread-terminal-turn-p turn)
      (magent-thread-fail-turn thread turn-id detail)
      (magent-session-save-deferred-for-session
       session (magent-runtime-session-scope runtime-session)))))

(defun magent-runtime-api--start-submission (submission)
  "Start executing SUBMISSION."
  (condition-case err
      (progn
        (magent-runtime-api--activate-submission-session submission)
        (magent-runtime-api--mark-submission-turn-started submission)
        (let* ((request-context
                (magent-runtime-submission-request-context submission))
               (prompt (magent-request-context-prompt request-context)))
          (when (magent-runtime-api--submission-live-p submission)
            (magent-runtime-api--notify-submission submission 'turn-start))
          (when (magent-runtime-api--submission-live-p submission)
            (magent-runtime-api--notify-submission
             submission 'user-message
             :text prompt))
          (when (magent-runtime-api--submission-live-p submission)
            (let ((handle
                   (magent-agent-run-turn
                    request-context
                    :on-complete
                    (lambda (result)
                      (let ((status
                             (if (magent-execution-result-success-p result)
                                 'completed
                               'failed)))
                        (magent-runtime-api--finish-submission
                         submission status result))))))
              (setf (magent-runtime-submission-handle submission) handle)
              ;; A synchronous observer may cancel while the sampler is still
              ;; on the stack, before HANDLE can be stored.  The starter lease
              ;; prevents the next ticket from advancing until this point.
              (when (and (eq (magent-runtime-submission-status submission)
                             'cancelled)
                         (magent-agent-loop-p handle))
                (magent-agent-loop-abort handle))))))
    (error
     (let* ((startup-message (format "Runtime startup failed: %s"
                                     (error-message-string err)))
            (result (magent-execution-result-failed
                     startup-message (list :status 'startup-error))))
       (magent-runtime-api--mark-submission-turn-failed
        submission startup-message)
       (magent-runtime-api--finish-submission submission 'failed result)))))

(defun magent-runtime-api--validate-skill-scope (runtime-session skill-names)
  "Validate that SKILL-NAMES may run in RUNTIME-SESSION's scope."
  (when skill-names
    (require 'magent-skills)
    (let ((origin (magent-session-scope-origin
                   (magent-runtime-session-scope runtime-session))))
      (dolist (name skill-names)
        (let* ((skill-name (if (symbolp name) (symbol-name name) name))
               (skill (and (stringp skill-name)
                           (magent-skills-get
                            skill-name
                            (magent-runtime-session-scope runtime-session)))))
          (when (and skill
                     (magent-skill-requires-project skill)
                     (not (stringp origin)))
            (user-error
             "Skill /%s requires a project workspace; global sessions are unsupported"
             skill-name)))))))

(defun magent-runtime-api--resolve-tools (agent tools)
  "Return validated exact tool names for AGENT and requested TOOLS.
TOOLS may be `:all' or a proper list.  Exact selections fail before a ledger
turn is created when any name is unknown or unavailable."
  (require 'magent-tools)
  (let* ((permission (magent-agent-info-permission agent))
         (requested
          (unless (eq tools :all)
            (delete-dups
             (mapcar (lambda (tool)
                       (if (symbolp tool)
                           tool
                         (intern (format "%s" tool))))
                     tools))))
         (resolved
          (magent-tools-get-gptel-tools-for-permission
           permission (if (eq tools :all) :all requested)))
         (available
          (mapcar (lambda (tool)
                    (intern (gptel-tool-name tool)))
                  resolved)))
    (when requested
      (when-let* ((missing (cl-set-difference requested available)))
        (user-error "Magent tools unavailable to agent %s: %s"
                    (magent-agent-info-name agent)
                    (mapconcat #'symbol-name missing ", "))))
    (if (eq tools :all) :all requested)))

(cl-defun magent-runtime-submit
    (runtime-session prompt &key context (tools :all) skills agent observer
                     approval-provider effort turn-metadata on-complete)
  "Submit PROMPT to RUNTIME-SESSION.
TOOLS is `:all' or an exact list of tool names.  OBSERVER receives
request-local Magent-native events."
  (unless (magent-runtime-session-p runtime-session)
    (error "Expected runtime session, got: %S" runtime-session))
  (magent-runtime-api--assert-session-available runtime-session)
  (unless (and (stringp prompt)
               (not (string-empty-p (string-trim prompt))))
    (error "Prompt is empty"))
  (unless (or (eq tools :all) (proper-list-p tools))
    (error "Expected :tools to be :all or a proper list, got: %S" tools))
  (let* ((effective-agent
          (magent-runtime-api--resolve-agent runtime-session agent))
         (effective-tools
          (magent-runtime-api--resolve-tools effective-agent tools))
         (effective-skills
          (or skills (magent-runtime-session-pending-skills runtime-session)))
         (model-routing-configured-p
          (magent-runtime-session-model-routing-configured-p
           runtime-session effective-agent)))
    (magent-runtime-api--validate-skill-scope
     runtime-session effective-skills)
    (let* ((submission-id (magent-protocol-generate-id "submission"))
           (session (magent-runtime-session-magent-session runtime-session))
           (turn-id (magent-runtime-api--prepare-turn
                     runtime-session prompt turn-metadata))
           (request-context
            (magent-request-context-create
             :id submission-id
             :scope (magent-runtime-session-scope runtime-session)
             :session session
             :prompt prompt
             :agent effective-agent
             :turn-id turn-id
             :approval-session session
             :ui-visibility 'none
             :origin-context context
             :tool-names effective-tools
             :model-route
             (and model-routing-configured-p
                  (magent-runtime-session-effective-model-route
                   runtime-session effective-agent))
             :effort (or (magent-effort-normalize-option effort)
                         (magent-effort-normalize-option
                          (magent-runtime-session-effort runtime-session)))
             :skill-names effective-skills
             :approval-provider approval-provider
             :observer observer
             :submission-id submission-id))
           (submission
            (magent-runtime-submission-create
             :id submission-id
             :runtime-session runtime-session
             :request-context request-context
             :on-complete on-complete)))
      (setf (magent-request-context-live-p request-context)
            (lambda ()
              (magent-runtime-api--submission-live-p submission)))
      (magent-runtime-session-clear-pending-skills runtime-session)
      (magent-runtime-queue-submit
       submission #'magent-runtime-api--start-submission))))

(cl-defun magent-runtime-session-compact
    (runtime-session &key instruction observer approval-provider turn-metadata
                     on-complete)
  "Compact RUNTIME-SESSION with its hidden compaction agent.
INSTRUCTION optionally refines the summary.  OBSERVER, APPROVAL-PROVIDER,
TURN-METADATA, and ON-COMPLETE have the same roles as in
`magent-runtime-submit'.  The compaction turn is marked as a future
prompt-history boundary.  Its request-local agent does not change the
session's selected user-facing agent."
  (unless (magent-runtime-session-p runtime-session)
    (error "Expected runtime session, got: %S" runtime-session))
  (magent-runtime-api--assert-session-available runtime-session)
  (let* ((compaction-agent
          (magent-agent-registry-get
           "compaction" (magent-runtime-session-scope runtime-session)))
         (pending-skills
          (magent-runtime-session-pending-skills runtime-session))
         (extra (string-trim (or instruction "")))
         (base-prompt (magent-prompt-read "internal/session-compaction.org"))
         (prompt (if (string-empty-p extra)
                     base-prompt
                   (concat
                    base-prompt "\n\n"
                    (magent-prompt-render
                     "internal/additional-instruction.org"
                     `((instruction . ,extra)))))))
    (unless compaction-agent
      (error "Magent compaction agent is unavailable"))
    (setf (magent-runtime-session-pending-skills runtime-session) nil)
    (unwind-protect
        (magent-runtime-submit
         runtime-session prompt
         :agent compaction-agent
         :observer observer
         :approval-provider approval-provider
         :turn-metadata (append (list :compaction t) turn-metadata)
         :on-complete
         (lambda (status result)
           (when on-complete
             (funcall on-complete status result))))
      (setf (magent-runtime-session-pending-skills runtime-session)
            pending-skills))))

(defun magent-runtime-processing-p ()
  "Return non-nil when any runtime turn is active."
  (magent-runtime-queue-processing-p))

(defun magent-runtime-pending-count (&optional runtime-session)
  "Return queued turn count, optionally for RUNTIME-SESSION."
  (magent-runtime-queue-length runtime-session))

(defun magent-runtime-cancel-submission (runtime-session submission-id)
  "Cancel exact SUBMISSION-ID owned by RUNTIME-SESSION.
Return non-nil when an active or queued submission was cancelled."
  (let ((queued
         (magent-runtime-queue-remove-submission
          runtime-session submission-id))
        (active (magent-runtime-queue-active-submission))
        (reason "Submission cancelled"))
    (cond
     (queued
      (magent-runtime-api--mark-submission-turn-dropped queued reason)
      (magent-runtime-api--finish-submission
       queued 'cancelled
       (magent-execution-result-cancelled reason (list :reason 'cancelled)))
      t)
     ((and active
           (eq (magent-runtime-submission-runtime-session active)
               runtime-session)
           (equal (magent-runtime-submission-id active) submission-id))
      (setf (magent-runtime-submission-status active) 'cancelled)
      (when-let* ((handle (magent-runtime-submission-handle active)))
        (when (magent-agent-loop-p handle)
          (magent-agent-loop-abort handle)))
      (magent-runtime-api--mark-submission-turn-interrupted active reason)
      (magent-runtime-api--finish-submission
       active 'cancelled
       (magent-execution-result-cancelled reason (list :reason 'cancelled)))
      t)
     (t nil))))

(defun magent-runtime-cancel (runtime-session)
  "Cancel RUNTIME-SESSION active and queued submissions."
  (let* ((removed (magent-runtime-queue-remove-session runtime-session))
         (active (magent-runtime-queue-active-submission)))
    (dolist (submission removed)
      (magent-runtime-api--mark-submission-turn-dropped
       submission "Queued turn cancelled")
      (magent-runtime-api--finish-submission
       submission 'cancelled
       (magent-execution-result-cancelled
        "Queued turn cancelled" (list :reason 'cancelled))))
    (when (and active
               (eq (magent-runtime-submission-runtime-session active)
                   runtime-session))
      (setf (magent-runtime-submission-status active) 'cancelled)
      (when-let* ((handle (magent-runtime-submission-handle active)))
        (when (magent-agent-loop-p handle)
          (magent-agent-loop-abort handle)))
      (magent-runtime-api--mark-submission-turn-interrupted
       active "Active turn cancelled")
      (magent-runtime-api--finish-submission
       active 'cancelled
       (magent-execution-result-cancelled
        "Active turn cancelled" (list :reason 'cancelled))))
    (+ (length removed)
       (if (and active
                (eq (magent-runtime-submission-runtime-session active)
                    runtime-session))
           1
         0))))

(defun magent-runtime-api--session-id-from-file (file)
  "Return persisted session id for FILE."
  (file-name-sans-extension (file-name-nondirectory file)))

(defun magent-runtime-api--list-session-files (files)
  "Return saved session display plists represented by FILES."
  (delq
   nil
   (mapcar
    (lambda (file)
      (let* ((metadata (magent-session--read-file-metadata-cached file))
             (valid (or (not (plist-member metadata :valid))
                        (plist-get metadata :valid)))
             (id (or (plist-get metadata :id)
                     (and valid
                          (magent-runtime-api--session-id-from-file file))))
             (scope (if (eq (plist-get metadata :scope) 'global)
                        'global
                      (plist-get metadata :project-root))))
        (when (and valid (magent-session-valid-id-p id))
          (list :id id
                :file file
                :scope scope
                :project-root (plist-get metadata :project-root)
                :title (plist-get metadata :summary-title)
                :updated-at
                (float-time (magent-session--file-display-time file))))))
    files)))

(defun magent-runtime-list-sessions ()
  "Return all saved sessions as plists for UI display."
  (magent-runtime-api--list-session-files
   (magent-session-list-files)))

(defun magent-runtime-list-sessions-for-scope (scope)
  "Return saved sessions for exact SCOPE as UI/ACP display plists."
  (magent-runtime-api--list-session-files
   (magent-session-list-files-for-scope scope)))

(defun magent-runtime-load-session-file (file)
  "Load session FILE and return a runtime session."
  (when-let* ((loaded (magent-session-read-file file))
              (scope (plist-get loaded :scope))
              (session (plist-get loaded :session)))
    (magent-runtime-session-register scope session)))

(provide 'magent-runtime-api)
;;; magent-runtime-api.el ends here
