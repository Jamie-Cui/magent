;;; magent-runtime-api.el --- UI-neutral Magent runtime API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Stable UI/backend-facing runtime API.  UI backends submit prompts to a
;; runtime session and receive request-local Magent-native observer events.

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
(declare-function magent-tool-runtime-for-permission "magent-tool-runtime")
(declare-function magent-tool-runtime-name "magent-tool-runtime" t t)

(cl-defstruct (magent-runtime-session
               (:constructor magent-runtime-session-create)
               (:copier nil))
  id
  scope
  magent-session
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

(defun magent-runtime-session-set-agent (runtime-session agent-or-name)
  "Set RUNTIME-SESSION's agent to AGENT-OR-NAME."
  (magent-runtime-api--assert-session-available runtime-session)
  (let* ((agent (cond
                 ((magent-agent-info-p agent-or-name) agent-or-name)
                 ((stringp agent-or-name)
                  (magent-agent-registry-get agent-or-name))
                 ((symbolp agent-or-name)
                  (magent-agent-registry-get (symbol-name agent-or-name)))))
         (session (magent-runtime-session-magent-session runtime-session)))
    (unless agent
      (error "Unknown Magent agent: %S" agent-or-name))
    (magent-session-set-agent session agent)
    agent))

(defun magent-runtime-session-agent-name (runtime-session)
  "Return RUNTIME-SESSION's active agent name."
  (let* ((session (magent-runtime-session-magent-session runtime-session))
         (agent (or (magent-session-agent session)
                    (magent-agent-registry-get-default))))
    (and agent (magent-agent-info-name agent))))

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
  (require 'magent-tool-runtime)
  (let* ((session (magent-runtime-session-magent-session runtime-session))
         (agent
          (cond
           ((null agent-or-name)
            (or (magent-session-agent session)
                (magent-agent-registry-get-default)))
           ((magent-agent-info-p agent-or-name) agent-or-name)
           ((symbolp agent-or-name)
            (magent-agent-registry-get (symbol-name agent-or-name)))
           ((stringp agent-or-name)
            (magent-agent-registry-get agent-or-name))))
         (permission (and agent (magent-agent-info-permission agent))))
    (unless agent
      (error "Unknown Magent agent: %S" agent-or-name))
    (mapcar (lambda (runtime)
              (let ((name (magent-tool-runtime-name runtime)))
                (if (symbolp name) name (intern (format "%s" name)))))
            (magent-tool-runtime-for-permission permission))))

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
  (when-let* ((observer (magent-runtime-submission-observer submission)))
    (let ((event (append
                  (list :type type
                        :time (float-time)
                        :session-id
                        (magent-runtime-submission-session-id submission)
                        :submission-id
                        (magent-runtime-submission-id submission)
                        :turn-id
                        (magent-runtime-submission-turn-id submission))
                  props)))
      (condition-case err
          (funcall observer event)
        (error
         (magent-log "ERROR runtime observer failed: %s"
                     (error-message-string err)))))))

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
    (magent-session-refresh-projections session)
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
  "Return the project/global overlay scope for SUBMISSION."
  (let* ((runtime-session (magent-runtime-submission-session submission))
         (scope (or (magent-runtime-submission-scope submission)
                    (and runtime-session
                         (magent-runtime-session-scope runtime-session))
                    'global)))
    (magent-session-scope-origin scope)))

(defun magent-runtime-api--activate-submission-session (submission)
  "Activate SUBMISSION's project overlay and install its session."
  (let ((runtime-session (magent-runtime-submission-session submission)))
    (magent-runtime-activate-scope
     (magent-runtime-api--submission-execution-scope submission))
    (magent-session-install
     (magent-runtime-session-scope runtime-session)
     (magent-runtime-session-magent-session runtime-session))
    ;; Overlay activation refreshes the session currently registered for its
    ;; ordinary scope.  Internal sessions and captured sessions installed only
    ;; after that activation still need an explicit refresh.
    (magent-session-refresh-agent
     (magent-runtime-session-magent-session runtime-session))))

(defun magent-runtime-api--mark-submission-turn-started (submission)
  "Mark SUBMISSION's ledger turn in progress, when it has one."
  (when-let* ((runtime-session (magent-runtime-submission-session submission))
              (session (magent-runtime-session-magent-session runtime-session))
              (thread (magent-session-thread-ledger session))
              (turn-id (magent-runtime-submission-turn-id submission))
              (turn (magent-thread-find-turn thread turn-id)))
    (unless (magent-thread-terminal-turn-p turn)
      (magent-thread-start-turn thread turn-id)
      (magent-session-refresh-projections session)
      (magent-session-save-deferred-for-session
       session (magent-runtime-session-scope runtime-session)))))

(defun magent-runtime-api--mark-submission-turn-dropped (submission detail)
  "Mark queued SUBMISSION's ledger turn dropped with DETAIL."
  (when-let* ((runtime-session (magent-runtime-submission-session submission))
              (session (magent-runtime-session-magent-session runtime-session))
              (thread (magent-session-thread-ledger session))
              (turn-id (magent-runtime-submission-turn-id submission))
              (turn (magent-thread-find-turn thread turn-id)))
    (unless (magent-thread-terminal-turn-p turn)
      (magent-thread-drop-turn thread turn-id detail)
      (magent-session-refresh-projections session)
      (magent-session-save-deferred-for-session
       session (magent-runtime-session-scope runtime-session)))))

(defun magent-runtime-api--mark-submission-turn-interrupted (submission detail)
  "Mark active SUBMISSION's ledger turn interrupted with DETAIL."
  (when-let* ((runtime-session (magent-runtime-submission-session submission))
              (session (magent-runtime-session-magent-session runtime-session))
              (thread (magent-session-thread-ledger session))
              (turn-id (magent-runtime-submission-turn-id submission))
              (turn (magent-thread-find-turn thread turn-id)))
    (unless (magent-thread-terminal-turn-p turn)
      (magent-thread-interrupt-turn thread turn-id detail)
      (magent-session-refresh-projections session)
      (magent-session-save-deferred-for-session
       session (magent-runtime-session-scope runtime-session)))))

(defun magent-runtime-api--mark-submission-turn-failed (submission detail)
  "Mark SUBMISSION's ledger turn failed with DETAIL."
  (when-let* ((runtime-session (magent-runtime-submission-session submission))
              (session (magent-runtime-session-magent-session runtime-session))
              (thread (magent-session-thread-ledger session))
              (turn-id (magent-runtime-submission-turn-id submission))
              (turn (magent-thread-find-turn thread turn-id)))
    (unless (magent-thread-terminal-turn-p turn)
      (magent-thread-fail-turn thread turn-id detail)
      (magent-session-refresh-projections session)
      (magent-session-save-deferred-for-session
       session (magent-runtime-session-scope runtime-session)))))

(defun magent-runtime-api--start-submission (submission)
  "Start executing SUBMISSION."
  (condition-case err
      (progn
        (magent-runtime-api--activate-submission-session submission)
        (magent-runtime-api--mark-submission-turn-started submission)
        (let* ((runtime-session (magent-runtime-submission-session submission))
               (session (magent-runtime-session-magent-session runtime-session))
               (request-context
                (magent-request-context-create
                 :id (magent-runtime-submission-id submission)
                 :scope (magent-runtime-session-scope runtime-session)
                 :session session
                 :turn-id (magent-runtime-submission-turn-id submission)
                 :approval-session session
                 :ui-visibility 'none
                 :origin-context (magent-runtime-submission-context submission)
                 :effort (magent-runtime-submission-effort submission)
                 :skill-names (magent-runtime-submission-skills submission)
                 :approval-provider
                 (magent-runtime-submission-approval-provider submission)
                 :observer (magent-runtime-submission-observer submission)
                 :submission-id (magent-runtime-submission-id submission)
                 :live-p (lambda ()
                           (magent-runtime-api--submission-live-p submission)))))
          (when (magent-runtime-api--submission-live-p submission)
            (magent-runtime-api--notify-submission submission 'turn-start))
          (when (magent-runtime-api--submission-live-p submission)
            (magent-runtime-api--notify-submission
             submission 'user-message
             :text (magent-runtime-submission-prompt submission)))
          (when (magent-runtime-api--submission-live-p submission)
            (let ((handle
                   (magent-agent-run-turn
                    :session session
                    :prompt (magent-runtime-submission-prompt submission)
                    :agent (magent-runtime-submission-agent submission)
                    :skills (magent-runtime-submission-skills submission)
                    :context (magent-runtime-submission-context submission)
                    :request-context request-context
                    :on-complete
                    (lambda (result)
                      (let ((status
                             (if (magent-agent-result-success-p result)
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
            (result (magent-agent-result-failed
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
                           (magent-skills-get skill-name))))
          (when (and skill
                     (magent-skill-requires-project skill)
                     (not (stringp origin)))
            (user-error
             "Skill /%s requires a project workspace; global sessions are unsupported"
             skill-name)))))))

(cl-defun magent-runtime-submit
    (runtime-session prompt &key context skills agent observer approval-provider
                     effort turn-metadata on-complete)
  "Submit PROMPT to RUNTIME-SESSION.
OBSERVER receives request-local Magent-native events."
  (unless (magent-runtime-session-p runtime-session)
    (error "Expected runtime session, got: %S" runtime-session))
  (magent-runtime-api--assert-session-available runtime-session)
  (unless (and (stringp prompt)
               (not (string-empty-p (string-trim prompt))))
    (error "Prompt is empty"))
  (let* ((effective-skills
          (or skills (magent-runtime-session-pending-skills runtime-session))))
    (magent-runtime-api--validate-skill-scope
     runtime-session effective-skills)
    (let* ((turn-id (magent-runtime-api--prepare-turn
                     runtime-session prompt turn-metadata))
           (submission
            (magent-runtime-submission-create
             :id (magent-protocol-generate-id "submission")
             :session runtime-session
             :session-id (magent-runtime-session-id runtime-session)
             :scope (magent-runtime-session-scope runtime-session)
             :prompt prompt
             :context context
             :skills effective-skills
             :agent agent
             :effort (or (magent-effort-normalize-option effort)
                         (magent-effort-normalize-option
                          (magent-runtime-session-effort runtime-session)))
             :observer observer
             :approval-provider approval-provider
             :on-complete on-complete
             :turn-id turn-id)))
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
prompt-history boundary while the session's selected user-facing agent is
restored after the request finishes."
  (unless (magent-runtime-session-p runtime-session)
    (error "Expected runtime session, got: %S" runtime-session))
  (magent-runtime-api--assert-session-available runtime-session)
  (let* ((session (magent-runtime-session-magent-session runtime-session))
         (selected-agent (magent-session-agent session))
         (compaction-agent (magent-agent-registry-get "compaction"))
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
           (magent-session-set-agent session selected-agent)
           (magent-session-save-deferred-for-session
            session (magent-runtime-session-scope runtime-session))
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

(defun magent-runtime-cancel (runtime-session)
  "Cancel RUNTIME-SESSION active and queued submissions."
  (let* ((removed (magent-runtime-queue-remove-session runtime-session))
         (active (magent-runtime-queue-active-submission)))
    (dolist (submission removed)
      (magent-runtime-api--mark-submission-turn-dropped
       submission "Queued turn cancelled")
      (magent-runtime-api--finish-submission
       submission 'cancelled
       (magent-agent-result-cancelled
        "Queued turn cancelled" (list :reason 'cancelled))))
    (when (and active
               (eq (magent-runtime-submission-session active)
                   runtime-session))
      (setf (magent-runtime-submission-status active) 'cancelled)
      (when-let* ((handle (magent-runtime-submission-handle active)))
        (when (magent-agent-loop-p handle)
          (magent-agent-loop-abort handle)))
      (magent-runtime-api--mark-submission-turn-interrupted
       active "Active turn cancelled")
      (magent-runtime-api--finish-submission
       active 'cancelled
       (magent-agent-result-cancelled
        "Active turn cancelled" (list :reason 'cancelled))))
    (+ (length removed)
       (if (and active
                (eq (magent-runtime-submission-session active)
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
