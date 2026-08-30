;;; magent-agent.el --- Agent processing for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Agent processing for Magent.  Builds prompt lists from session history,
;; applies per-agent gptel overrides, and starts the Magent-owned loop.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'magent-agent-loop)
(require 'magent-lifecycle-events)
(require 'magent-sampling)
(require 'magent-sampling-gptel)
(require 'magent-project-instructions)
(require 'magent-protocol)
(require 'magent-runtime)
(require 'magent-tools)
(require 'magent-session)
(require 'magent-ledger)
(require 'magent-agent-registry)
(require 'magent-permission)
(require 'magent-prompt)

(declare-function magent-capability-resolution-skill-names
                  "magent-capability" t t)
(declare-function magent-capability-resolution-to-plist "magent-capability")
(declare-function magent-capability-resolve "magent-capability")
(declare-function magent-skills-get-instruction-prompts "magent-skills")
(declare-function magent-skills-missing-tools "magent-skills")
(declare-function magent-skills-dedupe-names "magent-skills")

;;; Agent execution

(defun magent-agent--request-live-p (request-live-p)
  "Return non-nil when REQUEST-LIVE-P allows callbacks to run."
  (or (null request-live-p)
      (funcall request-live-p)))

(defun magent-agent--ui-visible-p (request-state)
  "Return non-nil when REQUEST-STATE allows UI writes."
  (or (null request-state)
      (magent-request-context-ui-visible-p request-state)))

(defun magent-agent--completion-callback-text (loop event streaming-started)
  "Return assistant text that still needs a callback for EVENT.
LOOP has already accumulated EVENT by the time this helper is called.
When streaming has not started, the current sample result should be rendered.
When streaming has started, render only the completed event's text that
was not already emitted as text deltas."
  (let ((result (magent-agent-loop-result loop))
        (event-text (magent-sampling-event-text event))
        (streamed (magent-agent-loop-sample-text loop)))
    (cond
     ((not streaming-started)
      result)
     ((or (null event-text)
          (string-empty-p event-text))
      nil)
     ((or (string= streamed event-text)
          (string-prefix-p event-text streamed)
          (string-suffix-p event-text streamed))
      nil)
     ((string-prefix-p streamed event-text)
      (substring event-text (length streamed)))
     (t
      event-text))))

(defun magent-agent--request-project-root (request-context request-state)
  "Return the project root associated with REQUEST-CONTEXT or REQUEST-STATE."
  (or (and request-state
           (magent-request-context-project-root request-state))
      (and request-context
           (plist-get request-context :project-root))
      (and request-state
           (let ((scope (magent-request-context-scope request-state)))
             (and (stringp scope) scope)))
      (magent-project-root nil t)))

(defun magent-agent--capabilities-enabled-p (request-context)
  "Return whether capability resolution is enabled for REQUEST-CONTEXT."
  (if (and (proper-list-p request-context)
           (plist-member request-context :capabilities-enabled))
      (eq (plist-get request-context :capabilities-enabled) t)
    magent-enable-capabilities))

(defun magent-agent--validate-explicit-skill-tools
    (skill-names available-tools)
  "Signal when explicit SKILL-NAMES require tools outside AVAILABLE-TOOLS."
  (when (and skill-names (require 'magent-skills nil t))
    (dolist (skill-name skill-names)
      (when-let* ((missing
                   (magent-skills-missing-tools skill-name available-tools)))
        (error "Skill '%s' requires unavailable tool(s): %s"
               skill-name
               (mapconcat #'symbol-name missing ", "))))))

(cl-defun magent-agent-resolve-model-route
    (agent &key explicit-route parent-route phase)
  "Resolve and validate the model route for AGENT.
EXPLICIT-ROUTE is a request or session selection and has highest priority.
An explicit model on AGENT takes precedence over PARENT-ROUTE, which lets a
child agent override its inherited parent route.  Otherwise the current gptel
defaults are used.  PHASE is retained as a policy seam for future request
builders without adding a phase router today."
  (unless (magent-agent-info-p agent)
    (error "Expected Magent agent info, got: %S" agent))
  (let* ((agent-route (magent-agent-info-model-route agent))
         (selected (or explicit-route
                       agent-route
                       parent-route
                       (magent-sampling-gptel-default-route)))
         (source (cond
                  (explicit-route
                   (or (magent-model-route-source explicit-route) 'request))
                  (agent-route 'agent)
                  (parent-route 'parent)
                  (t 'gptel)))
         (route
          (magent-model-route-relabel
           selected source (magent-agent-info-name agent) phase)))
    (magent-sampling-gptel-validate-route route)))

(defun magent-agent--fail-request-turn
    (session turn-id request-scope detail)
  "Fail SESSION's TURN-ID with DETAIL.
REQUEST-SCOPE is used when scheduling persistence.  This helper is
idempotent so higher-level runtime error handling may safely repeat it."
  (when-let* ((thread (magent-session-thread-ledger session))
              (turn (and turn-id
                         (magent-thread-find-turn thread turn-id))))
    (unless (magent-thread-terminal-turn-p turn)
      (dolist (item (magent-thread-turn-items turn))
        (when (eq (magent-thread-item-status item) 'in-progress)
          (magent-thread-fail-item thread item detail)))
      (magent-thread-fail-turn thread turn-id detail)
      (condition-case err
          (magent-session-save-deferred-for-session
           session request-scope)
        (error
         (magent-log "ERROR startup failure session save failed: %s"
                     (error-message-string err)))))))

(defun magent-agent--context-system-message (project-root)
  "Return prompt context for PROJECT-ROOT."
  (when (and (stringp project-root)
             (not (string-empty-p project-root)))
    (magent-prompt-render "internal/project-context.org"
                          `((project-root . ,project-root)))))

(defun magent-agent--context-provider-messages
    (user-prompt request-context project-root)
  "Return trusted provider context for USER-PROMPT.
REQUEST-CONTEXT and PROJECT-ROOT are passed unchanged to every function in
`magent-context-provider-functions'.  Provider failures and invalid values are
logged without aborting the current turn."
  (let (messages)
    (dolist (provider magent-context-provider-functions (nreverse messages))
      (condition-case err
          (let ((message
                 (funcall provider user-prompt request-context project-root)))
            (cond
             ((null message))
             ((not (stringp message))
              (magent-log
               "WARN context provider %S returned a non-string value"
               provider))
             ((not (string-empty-p message))
              (push message messages))))
        (error
         (magent-log "WARN context provider %S failed: %s"
                     provider (error-message-string err)))))))

(defun magent-agent--compose-system-message
    (global-system-message agent-role-message
                           project-root context-provider-messages skill-prompts
                           &optional project-instructions)
  "Return system prompt from GLOBAL-SYSTEM-MESSAGE and prompt context.
AGENT-ROLE-MESSAGE supplements the universal contract instead of replacing it.
PROJECT-ROOT contributes workspace context, CONTEXT-PROVIDER-MESSAGES are
trusted request-local extension blocks, and SKILL-PROMPTS are active skills.
PROJECT-INSTRUCTIONS contains scoped repository instructions discovered by
Magent.
The runtime trust policy is appended last so every built-in or custom agent
receives the same instruction-provenance and permission invariants."
  (let ((context-message
         (magent-agent--context-system-message project-root))
        (skills-message
         (when skill-prompts
           (magent-prompt-render
            "internal/active-skills.org"
            `((skills . ,(mapconcat #'identity skill-prompts "\n\n"))))))
        (runtime-policy
         (magent-prompt-read "internal/runtime-policy.org")))
    (mapconcat
     #'identity
     (delq nil
           (append (list global-system-message
                         agent-role-message
                         context-message
                         project-instructions)
                   context-provider-messages
                   (list skills-message runtime-policy)))
     "\n\n")))

(defun magent-agent--execute-turn
    (user-prompt &optional callback agent-info skill-names event-context
                 request-context capability-resolution text-callback request-live-p
                 request-state)
  "Execute USER-PROMPT through the Magent-owned agent loop.
CALLBACK is called with one final `magent-execution-result'.
AGENT-INFO is the agent to use (defaults to session agent or registry default).
SKILL-NAMES is a list of skill name strings to activate for this request.
EVENT-CONTEXT is an optional existing event context to reuse.
REQUEST-CONTEXT is an optional structured context plist captured at
dispatch time.
CAPABILITY-RESOLUTION is an optional precomputed capability resolver
result for this turn.
TEXT-CALLBACK receives streaming text chunks for this request.
REQUEST-LIVE-P is an optional predicate used to discard stale backend
callbacks after the UI has moved on to a newer request.
REQUEST-STATE is an optional `magent-request-context' carrying runtime
state for the request.
When SKILL-NAMES is nil, the capability resolver may still auto-activate
instruction skills for the turn.  Explicit skills are merged with and
deduplicated against capability-selected skills.

The tool calling loop is managed by `magent-agent-loop'.  This function:
  1. Builds the prompt list from session history
  2. Retrieves per-agent overrides (backend, model, temperature, tools)
  3. Creates a normalized LLM request and starts loop execution
  4. Records the final response in the session via the callback"
  (let* ((session (or (and request-state
                           (magent-request-context-session request-state))
                      (magent-session-get)))
         (agent (or agent-info
                    (magent-session-agent session)
                    (magent-agent-registry-get-default)))
         (effective-permission
          (or (and request-state
                   (magent-request-context-permission-profile request-state))
              (magent-agent-info-permission agent)))
         (request-scope
          (or (and request-state
                   (magent-request-context-scope request-state))
              (magent-session-current-scope)))
         (inherited-context
          (or event-context
              (and request-state
                   (magent-request-context-event-context request-state))))
         (owns-context (null inherited-context))
         (context (or inherited-context
                      (magent-lifecycle-events-begin-turn
                       (format "Agent %s" (magent-agent-info-name agent)))))
         (process-turn-id
          (and request-state
               (magent-request-context-turn-id request-state))))
    (condition-case err
        (progn
          (when request-state
            (setf (magent-request-context-session request-state) session
                  (magent-request-context-live-p request-state)
                  (or (magent-request-context-live-p request-state)
                      request-live-p)
                  (magent-request-context-event-context request-state) context))
          ;; Freeze scalar audit attribution after the request-local agent is
          ;; selected.  Later UI/session mutations must not relabel this turn.
          (when request-state
            (magent-request-context-audit-snapshot request-state))
          (let* ((thread (magent-session-thread-ledger session))
                 (state-turn-id process-turn-id)
                 (state-turn
                  (and state-turn-id
                       (magent-thread-find-turn thread state-turn-id))))
            (unless state-turn
              (setq state-turn
                    (magent-thread-create-turn
                     thread user-prompt nil (list :source 'agent-run-turn))
                    state-turn-id (magent-thread-turn-id state-turn)))
            (magent-thread-record-user-message-if-needed
             thread state-turn-id user-prompt nil
             (list :source 'agent-run-turn))
            (let* ((active-turn (magent-thread-active-turn
                                 (magent-session-thread-ledger session)))
                   (turn-id
                    (or (and state-turn state-turn-id)
                        (and active-turn
                             (magent-thread-turn-id active-turn)))))
              (setq process-turn-id turn-id)
              (when (and request-state turn-id)
                (setf (magent-request-context-turn-id request-state)
                      turn-id))))
    (let* ((current-turn-id (and request-state
                                 (magent-request-context-turn-id request-state)))
           (prompt-list (magent-session-context-view
                         session 'provider current-turn-id))
           (agent-role-msg (magent-agent-info-prompt agent))
           (request-project-root
            (magent-agent--request-project-root request-context request-state))
           (tools
            (magent-tools-get-gptel-tools-for-permission
             effective-permission
             (if request-state
                 (magent-request-context-tool-names request-state)
               :all)))
           (available-tool-names
            (mapcar (lambda (tool) (intern (gptel-tool-name tool))) tools))
           (explicit-skill-names
            (magent-skills-dedupe-names
             (append skill-names
                     (and request-state
                          (magent-request-context-skill-names
                           request-state)))))
           (_skill-tool-validation
            (magent-agent--validate-explicit-skill-tools
             explicit-skill-names available-tool-names))
           (capability-resolution
            (or capability-resolution
                (when (and (magent-agent--capabilities-enabled-p request-context)
                           (require 'magent-capability nil t))
                  (magent-capability-resolve
                   user-prompt request-context explicit-skill-names
                   available-tool-names))))
           (resolved-skill-names
            (magent-skills-dedupe-names
             (append
              explicit-skill-names
              (and capability-resolution
                   (magent-capability-resolution-skill-names
                    capability-resolution)))))
           (skill-prompts (when (and (require 'magent-skills nil t)
                                     resolved-skill-names)
                            (magent-skills-get-instruction-prompts
                             resolved-skill-names)))
           (context-provider-messages
            (magent-agent--context-provider-messages
             user-prompt request-context request-project-root))
           (project-instructions
            (magent-project-instructions-system-message
             request-project-root request-context))
           (system-msg
            (magent-agent--compose-system-message
             magent-system-prompt agent-role-msg
             request-project-root context-provider-messages skill-prompts
             project-instructions)))
      (when capability-resolution
        (magent-lifecycle-events-emit
         'capability-resolution
         :context context
         :resolution
         (magent-capability-resolution-to-plist capability-resolution)))
      (magent-agent-info-apply-gptel-overrides
       agent
       (lambda ()
         (let* ((route
                 (magent-agent-resolve-model-route
                  agent
                  :explicit-route
                  (and request-state
                       (magent-request-context-model-route request-state))
                  :parent-route
                  (and request-state
                       (magent-request-context-parent-model-route
                        request-state))))
                (inherited-temperature
                 (and request-state
                      (magent-request-context-temperature request-state)))
                (inherited-top-p
                 (and request-state
                      (magent-request-context-top-p request-state)))
                (inherited-effort
                 (and request-state
                      (magent-request-context-effort request-state)))
                (backend (magent-model-route-backend route))
                (model (magent-model-route-model route))
                (temperature (or inherited-temperature
                                 (magent-agent-info-temperature agent)
                                 (and (boundp 'gptel-temperature)
                                      (default-value 'gptel-temperature))))
                (top-p (or inherited-top-p
                           (magent-agent-info-top-p agent)))
                (effort-option (or inherited-effort
                                   (magent-agent-info-effort agent)
                                   (magent-effort-option-or-auto
                                    magent-default-effort)))
                (effort (magent-effort-effective effort-option)))
           (when request-state
             (setf (magent-request-context-project-root request-state)
                   (or (magent-request-context-project-root request-state)
                       request-project-root)
                   (magent-request-context-model-route request-state)
                   route
                   (magent-request-context-model request-state)
                   model
                   (magent-request-context-backend request-state)
                   backend
                   (magent-request-context-temperature request-state)
                   (or (magent-request-context-temperature request-state)
                       temperature)
                   (magent-request-context-top-p request-state)
                   (or (magent-request-context-top-p request-state)
                       top-p)
                   (magent-request-context-effort request-state)
                   (or (magent-request-context-effort request-state)
                       effort-option)
                   (magent-request-context-skill-names request-state)
                   (copy-sequence resolved-skill-names)
                   (magent-request-context-capability-context request-state)
                   (or (magent-request-context-capability-context request-state)
                       (and capability-resolution
                            (magent-capability-resolution-to-plist
                             capability-resolution))
                       request-context)
                   (magent-request-context-permission-profile request-state)
                   effective-permission))
           (magent-log "INFO agent=%s backend=%s model=%s route-source=%s tools=[%s]"
                       (magent-agent-info-name agent)
                       (gptel-backend-name backend)
                       model
                       (magent-model-route-source route)
                       (mapconcat #'gptel-tool-name tools ", "))
           (when resolved-skill-names
             (magent-log "INFO active skills=[%s]"
                         (mapconcat #'identity resolved-skill-names ", ")))
           (let* ((gptel-backend backend)
                  (gptel-model model)
                  (gptel-temperature temperature)
                  (request-tools
                   (magent-agent-loop-tools-for-provider tools))
                  (_tool-capability-preflight
                   (when (and request-tools
                              (eq (magent-sampling-gptel-route-tool-capability route)
                                  'unsupported))
                     (error
                      "Magent model %s on backend %s does not support tools"
                      model (gptel-backend-name backend))))
                  (gptel-tools request-tools)
                  (live-p (or request-live-p
                              (and request-state
                                   (magent-request-context-live-p
                                    request-state))))
                  (streaming-started nil)
                  (text-delta-seen nil)
                  (assistant-item nil)
                  (reasoning-item nil)
                  (sampling-count 0)
                  (sample-assistant-content-before nil)
                  loop)
             (cl-labels
                 ((current-turn-id
                   ()
                   (or (and request-state
                            (magent-request-context-turn-id request-state))
                       (and loop
                            (magent-agent-loop-turn-id loop))
                       (and (magent-thread-active-turn
                             (magent-session-thread-ledger session))
                            (magent-thread-turn-id
                             (magent-thread-active-turn
                              (magent-session-thread-ledger session))))))
                  (ensure-assistant-item
                   ()
                   (or assistant-item
                       (when-let* ((thread (magent-session-thread-ledger
                                            session))
                                   (turn-id (current-turn-id)))
                         (setq assistant-item
                               (magent-thread-ensure-message-item
                                thread turn-id 'assistant nil
                                (list :source 'streaming)))
                         assistant-item)))
                  (ensure-reasoning-item
                   ()
                   (or reasoning-item
                       (when-let* ((thread (magent-session-thread-ledger
                                            session))
                                   (turn-id (current-turn-id)))
                         (setq reasoning-item
                               (magent-thread-start-item
                                thread turn-id 'reasoning
                                :metadata
                                (list :source 'streaming
                                      :include-reasoning
                                      magent-include-reasoning)))
                         reasoning-item)))
                  (record-text-delta
                   (text)
                   (when-let* ((chunk (and (stringp text) text))
                               (thread (magent-session-thread-ledger session))
                               (item (ensure-assistant-item)))
                     (magent-thread-append-item-content thread item chunk)
                     (magent-session-save-deferred-for-session
                      session request-scope)))
                  (record-reasoning-delta
                   (text)
                   (when magent-include-reasoning
                     (when-let* ((chunk (and (stringp text) text))
                                 (thread (magent-session-thread-ledger session))
                                 (item (ensure-reasoning-item)))
                       (magent-thread-append-item-content thread item chunk)
                       (magent-session-save-deferred-for-session
                        session request-scope))))
                  (finish-reasoning-item
                   ()
                   (when-let* ((thread (magent-session-thread-ledger session))
                               (item reasoning-item))
                     (unless (magent-thread-terminal-item-p item)
                       (magent-thread-complete-item
                        thread item
                        :content (magent-thread-item-content item)
                        :metadata (magent-thread-item-metadata item))
                       (magent-session-save-deferred-for-session
                        session request-scope))
                     (setq reasoning-item nil)))
                  (record-assistant-terminal
                   (status response)
                   (let* ((thread (magent-session-thread-ledger session))
                          (turn-id (current-turn-id))
                          (text (cond
                                 ((stringp response) response)
                                 ((null response) "")
                                 (t (format "%S" response))))
                          (transcript
                           (if (eq status 'completed)
                               (magent-agent-loop-transcript loop)
                             text)))
                     (if (and thread turn-id)
                         (let ((item (or assistant-item
                                         (magent-thread-ensure-message-item
                                          thread turn-id 'assistant nil
                                          (list :source 'terminal)))))
                           (pcase status
                             ('completed
                              (magent-thread-complete-item
                               thread item
                               :role 'assistant
                               :content transcript)
                              (magent-thread-complete-turn
                               thread turn-id))
                             (_
                              (let ((message
                                     (if (string-prefix-p "Error:" text)
                                         text
                                       (concat "Error: " text))))
                                (magent-thread-fail-item
                                 thread item message
                                 :role 'assistant
                                 :content message)
                                (magent-thread-fail-turn
                                 thread turn-id message))))
                           (setq assistant-item item)
                           (condition-case err
                               (magent-session-save-for-session
                                session request-scope)
                             (error
                              (magent-log
                               "ERROR immediate session save failed: %s"
                               (error-message-string err)))))
                       (error "Turn %s is missing from its session ledger"
                              turn-id))))
                  (emit-request-start
                   ()
                   (magent-lifecycle-events-emit
                    'llm-request-start
                    :context context
                    :backend (and backend (gptel-backend-name backend))
                    :model (format "%s" model)
                    :prompt-count
                    (length
                     (magent-sampling-request-prompt
                      (magent-agent-loop-request loop)))
                    :tool-count
                    (length
                     (magent-sampling-request-tools
                      (magent-agent-loop-request loop)))
                    :system-prompt-length (length (or system-msg ""))))
                  (close-reasoning
                   ()
                   (finish-reasoning-item))
                  (prepare-sample
                   ()
                   (setq sample-assistant-content-before
                         (and assistant-item
                              (copy-tree
                               (magent-thread-item-content assistant-item)))
                         streaming-started nil
                         text-delta-seen nil)
                   (magent-agent-loop-begin-sample loop))
                  (sample
                   ()
                   (prepare-sample)
                   (cl-incf sampling-count)
                   (emit-request-start)
                   (magent-agent-loop-start loop))
                  (request-for-current-session
                   ()
                   (let ((request
                          (magent-agent-loop-request-for-current-session
                           loop)))
                     (magent-sampling-request-create
                      :prompt (magent-sampling-request-prompt request)
                      :system (magent-sampling-request-system request)
                      :tools request-tools
                      :model (magent-sampling-request-model request)
                      :backend (magent-sampling-request-backend request)
                      :stream t
                      :callback (magent-sampling-request-callback request)
                      :metadata (magent-sampling-request-metadata request))))
                  (rollback-current-sample-text
                   ()
                   (magent-agent-loop-discard-sample-text loop)
                   (when assistant-item
                     (setf (magent-thread-item-content assistant-item)
                           (copy-tree sample-assistant-content-before))
                     (magent-session-save-deferred-for-session
                      session request-scope)))
                  (continue-turn
                   (outcome)
                   (if (and (numberp magent-max-sampling-requests)
                            (> magent-max-sampling-requests 0)
                            (>= sampling-count
                                magent-max-sampling-requests))
                       (let ((failure-message
                              (format
                               "Maximum sampling requests reached for this turn (%d)"
                               magent-max-sampling-requests)))
                         (magent-agent-loop-set-tool-continuation loop nil)
                         (finish-turn
                          'failed failure-message
                          (list :reason 'sampling-limit
                                :sampling-count sampling-count
                                :continuation-reason
                                (plist-get outcome :reason))))
                     (let ((continuation
                            (magent-agent-loop-tool-continuation loop)))
                       (magent-log
                        "INFO continuing model response: reason=%s count=%d"
                        (plist-get outcome :reason)
                        (1+ sampling-count))
                       (if (functionp continuation)
                           (progn
                             (magent-agent-loop-set-tool-continuation loop nil)
                             (prepare-sample)
                             (cl-incf sampling-count)
                             (emit-request-start)
                             (condition-case err
                                 (funcall continuation)
                               (error
                                (finish-turn
                                 'failed
                                 (format "Provider continuation failed: %s"
                                         (error-message-string err))))))
                         (setf (magent-agent-loop-request loop)
                               (request-for-current-session))
                         (sample)))))
                  (finish-streaming
                   ()
                   (close-reasoning))
                  (finish-turn
                   (status response &optional metadata)
                   (finish-streaming)
                   (magent-lifecycle-events-emit
                    'llm-request-end
                    :context context
                    :status status
                    :backend (and backend (gptel-backend-name backend))
                    :model (format "%s" model))
                   (when owns-context
                     (magent-lifecycle-events-end-turn context status))
                   (record-assistant-terminal status response)
                   (when callback
                     (funcall callback
                              (if (eq status 'completed)
                                  (magent-execution-result-completed
                                   response metadata)
                                (magent-execution-result-failed
                                 response metadata)))))
                  (start-streaming
                   ()
                   (close-reasoning)
                   (setq streaming-started t))
                  (handle-completed-event
                   (event)
                   (let ((observer-text
                          (magent-agent--completion-callback-text
                           loop event text-delta-seen))
                         (callback-text
                          (magent-agent--completion-callback-text
                           loop event streaming-started)))
                     (when (and (stringp observer-text)
                                (not (string-empty-p observer-text)))
                       (magent-request-context-notify
                        request-state 'assistant-delta
                        :text observer-text))
                     (when (and (stringp callback-text)
                                (not (string-empty-p callback-text)))
                       (start-streaming)
                       (when (and text-callback
                                  (magent-agent--ui-visible-p request-state))
                         (funcall text-callback callback-text))))
                   (let* ((result (or (magent-agent-loop-result loop) ""))
                          (empty-p (string-empty-p result))
                          (metadata (and empty-p
                                         (list :reason 'empty-completion))))
                     (when empty-p
                       (magent-log "WARN provider completed without assistant text"))
                     (magent-request-context-notify
                      request-state 'assistant-complete
                      :text result
                      :empty empty-p)
                     (finish-turn 'completed result metadata)))
                  (handle-event
                   (event)
                   (when (magent-agent--request-live-p live-p)
                     (let ((event-type (magent-sampling-event-type event)))
                       (pcase event-type
                       ('text-delta
                        (start-streaming)
                        (magent-lifecycle-events-emit
                         'text-delta
                         :context context
                         :text (magent-sampling-event-text event))
                        (unless (string-empty-p
                                 (or (magent-sampling-event-text event) ""))
                          (setq text-delta-seen t))
                        (magent-request-context-notify
                         request-state 'assistant-delta
                         :text (magent-sampling-event-text event))
                        (record-text-delta (magent-sampling-event-text event))
                        (when (and text-callback
                                   (magent-agent--ui-visible-p request-state))
                          (funcall text-callback
                                   (magent-sampling-event-text event))))
                       ('reasoning-delta
                        (magent-request-context-notify
                         request-state 'reasoning-delta
                         :text (magent-sampling-event-text event))
                        (record-reasoning-delta
                         (magent-sampling-event-text event)))
                       ('reasoning-end
                        (magent-request-context-notify
                         request-state 'reasoning-complete)
                        (close-reasoning))
                       ('tool-call
                        (close-reasoning)
                        (magent-request-context-notify
                         request-state 'tool-call-detected
                         :tool-id (magent-sampling-event-id event)
                         :name (magent-sampling-event-name event)
                         :arguments (magent-sampling-event-arguments event)))
                       ('tool-call-batch-end
                        (close-reasoning)
                        (when (eq (plist-get (magent-sampling-event-metadata event)
                                             :source)
                                  'textual-dsml)
                          (rollback-current-sample-text))
                        (magent-lifecycle-events-emit
                         'llm-request-end
                         :context context
                         :status 'tool-calls
                         :backend (and backend (gptel-backend-name backend))
                         :model (format "%s" model))
                        (magent-agent-loop-dispatch-tool-calls
                         loop
                         (magent-agent-loop-create-orchestrator
                          loop
                          effective-permission
                          request-state)
                         (lambda (outcome)
                           (if (and (eq (magent-agent-loop-status loop)
                                        'failed)
                                    (stringp (plist-get outcome :result)))
                               (finish-turn 'failed
                                            (plist-get outcome :result))
                             (continue-turn outcome)))))
                       ('completed
                        (handle-completed-event event))
                      ('error
                       (magent-request-context-notify
                        request-state 'turn-error
                        :message (magent-sampling-event-message event)
                        :metadata (magent-sampling-event-metadata event))
                       (finish-turn 'failed
                                    (magent-sampling-event-message event)
                                    (magent-sampling-event-metadata event))))))))
               (setq loop
                     (magent-agent-loop-create
                      :session session
                      :request
                     (magent-sampling-request-create
                      :prompt prompt-list
                      :system system-msg
                      :tools request-tools
                      :model model
                      :backend backend
                      :stream t
                      :metadata (append
                                 (list :temperature temperature
                                       :top-p top-p)
                                 (when effort
                                   (list :effort effort)))
                      :callback #'handle-event)
                      :request-context request-state
                      :event-context context
                      :owns-event-context-p owns-context
                      :turn-id (and request-state
                                    (magent-request-context-turn-id
                                     request-state))
                      :sampler #'magent-sampling-gptel-sample))
               (sample)
               loop)))))))
      (error
       (let ((message (error-message-string err)))
         (magent-agent--fail-request-turn
          session process-turn-id request-scope message)
         (when owns-context
           (magent-lifecycle-events-end-turn context 'failed message)))
       (signal (car err) (cdr err))))))

(cl-defun magent-agent-run-turn
    (&key session prompt agent skills context observer request-context
          approval-provider capability-resolution on-complete request-live-p)
  "Run one Magent turn for SESSION with PROMPT.
This is the UI-neutral execution entry point.  OBSERVER receives
Magent-native request events through REQUEST-CONTEXT.  ON-COMPLETE is
called with one final `magent-execution-result'."
  (unless session
    (error ":session is required"))
  (unless prompt
    (error ":prompt is required"))
  (let* ((request-context
          (or request-context
              (magent-request-context-create
               :session session
               :ui-visibility 'none)))
         (live-p (or request-live-p
                     (magent-request-context-live-p request-context))))
    (setf (magent-request-context-session request-context) session
          (magent-request-context-origin-context request-context)
          (or (magent-request-context-origin-context request-context)
              context)
          (magent-request-context-observer request-context)
          (or (magent-request-context-observer request-context)
              observer)
          (magent-request-context-approval-provider request-context)
          (or (magent-request-context-approval-provider request-context)
              approval-provider)
          (magent-request-context-ui-visibility request-context)
          (or (magent-request-context-ui-visibility request-context)
              'none)
          (magent-request-context-live-p request-context) live-p)
    (magent-agent--execute-turn
     prompt
     on-complete
     agent
     skills
     nil
     context
     capability-resolution
     nil
     live-p
     request-context)))

(provide 'magent-agent)
;;; magent-agent.el ends here
