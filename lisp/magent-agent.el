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
(require 'magent-llm)
(require 'magent-llm-gptel)
(require 'magent-memory)
(require 'magent-project-instructions)
(require 'magent-protocol)
(require 'magent-runtime)
(require 'magent-tools)
(require 'magent-tool-runtime)
(require 'magent-session)
(require 'magent-ledger)
(require 'magent-agent-registry)
(require 'magent-permission)
(require 'magent-prompt)

(declare-function magent-capability-resolution-skill-names
                  "magent-capability" t t)
(declare-function magent-capability-resolution-to-plist "magent-capability")
(declare-function magent-capability-resolve "magent-capability")
(declare-function magent-capability-resolve-for-turn "magent-capability")
(declare-function magent-skills-get-instruction-prompts "magent-skills")
(declare-function magent-skills-missing-tools "magent-skills")
(declare-function magent-skills-dedupe-names "magent-skills")

;;; Agent execution

(defun magent-agent--empty-final-response-retry-prompt ()
  "Return the prompt for retrying an empty post-tool final response."
  (magent-prompt-read "internal/empty-final-response-retry.org"))

(defun magent-agent--strict-final-response-retry-prompt ()
  "Return the prompt for strict final-response recovery."
  (magent-prompt-read "internal/strict-final-response-retry.org"))

(defcustom magent-post-tool-reasoning-idle-retry-delay 5
  "Seconds to wait after post-tool reasoning ends with no assistant text.
When a provider emits only reasoning after tool output and then stalls,
Magent aborts that sampling request and performs the normal final-response
retry.  Set to nil or 0 to disable this guard."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'magent)

(defcustom magent-post-tool-reasoning-max-duration 30
  "Maximum seconds to allow post-tool reasoning with no assistant text.
Some providers can keep streaming reasoning after tool output without ever
emitting assistant text, a tool call, or a terminal completion.  When this
limit is reached Magent aborts that sampling request and performs the
normal final-response retry.  Set to nil or 0 to disable this guard."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'magent)

(defun magent-agent--metadata-without-retry-flags (metadata)
  "Return METADATA without request-local retry flags.
Improper or circular metadata is dropped instead of scanned, because this
helper runs from async process callbacks where an unbounded plist walk can
lock the Emacs main thread."
  (when (proper-list-p metadata)
    (let (result)
      (while (consp metadata)
        (let ((key (pop metadata))
              (value (pop metadata)))
          (unless (memq key '(:final-response-retry
                              :strict-final-response-retry
                              :include-reasoning
                              :disable-provider-tools))
            (push key result)
            (push value result))))
      (nreverse result))))

(defun magent-agent--metadata-status-p (metadata status)
  "Return non-nil when METADATA contains a `:status' equal to STATUS.
Provider error metadata can contain duplicate `:status' keys, so callers must
not rely on `plist-get' returning the meaningful one."
  (when (proper-list-p metadata)
    (let ((found nil))
      (while (consp metadata)
        (let ((key (pop metadata))
              (value (pop metadata)))
          (when (and (eq key :status)
                     (equal value status))
            (setq found t))))
      found)))

(defun magent-agent--abort-error-event-p (event)
  "Return non-nil when EVENT represents an expected provider abort."
  (let ((metadata (magent-llm-event-metadata event)))
    (or (magent-agent--metadata-status-p metadata 'abort)
        (equal (magent-llm-event-message event) "Request aborted"))))

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
When streaming has not started, the full loop result should be rendered.
When streaming has started, render only the completed event's text that
was not already emitted as text deltas."
  (let ((result (magent-agent-loop-result loop))
        (event-text (magent-llm-event-text event))
        (streamed (magent-agent-loop-text loop)))
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
      (ignore-errors (magent-project-root nil t))))

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
      (magent-session-refresh-projections session)
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

(defun magent-agent--compose-system-message
    (base-system-message project-root memory-message skill-prompts
                         &optional project-instructions)
  "Return system prompt from BASE-SYSTEM-MESSAGE and prompt context.
PROJECT-ROOT contributes workspace context, MEMORY-MESSAGE is a selected
Emacs profile memory block, and SKILL-PROMPTS are active skill prompts.
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
    (mapconcat #'identity
               (delq nil
                      (list base-system-message
                            context-message
                            project-instructions
                            memory-message
                           skills-message
                           runtime-policy))
               "\n\n")))

(defun magent-agent-process
    (user-prompt &optional callback agent-info skill-names event-context
                 request-context capability-resolution text-callback request-live-p
                 request-state)
  "Process USER-PROMPT through the AI agent using the Magent loop.
CALLBACK is called with the final string response when complete.
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
          (magent-session-set-agent session agent)
          ;; Freeze scalar audit attribution after the request-local agent is
          ;; selected.  Later UI/session mutations must not relabel this turn.
          (when request-state
            (magent-request-context-audit-snapshot request-state))
          (let* ((thread (magent-session-thread-ledger session))
                 (state-turn-id process-turn-id)
                 (state-turn
                  (and state-turn-id
                       (magent-thread-find-turn thread state-turn-id))))
            (if state-turn
                (progn
                  (magent-thread-record-user-message-if-needed
                   thread state-turn-id user-prompt nil
                   (list :source 'agent-process))
                  (magent-session-refresh-projections session))
              (magent-session-add-message session 'user user-prompt))
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
           (prompt-list (magent-session-to-gptel-prompt-list
                         session current-turn-id))
           (base-system-msg (or (magent-agent-info-prompt agent)
                                magent-system-prompt))
           (request-project-root
            (magent-agent--request-project-root request-context request-state))
           (tools (mapcar #'magent-tool-runtime-to-plist
                          (magent-tool-runtime-for-permission
                           effective-permission)))
           (available-tool-names
            (mapcar (lambda (tool)
                      (intern (plist-get tool :name)))
                    tools))
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
           (memory-message
            (magent-memory-system-message
             user-prompt request-context request-project-root))
           (project-instructions
            (magent-project-instructions-system-message
             request-project-root request-context))
           (system-msg
            (magent-agent--compose-system-message
             base-system-msg request-project-root memory-message skill-prompts
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
         (let* ((inherited-backend
                 (and request-state
                      (magent-request-context-backend request-state)))
                (inherited-model
                 (and request-state
                      (magent-request-context-model request-state)))
                (inherited-temperature
                 (and request-state
                      (magent-request-context-temperature request-state)))
                (inherited-top-p
                 (and request-state
                      (magent-request-context-top-p request-state)))
                (inherited-effort
                 (and request-state
                      (magent-request-context-effort request-state)))
                (backend (or inherited-backend gptel-backend))
                (model (or inherited-model gptel-model))
                (temperature (or inherited-temperature
                                 (and (boundp 'gptel-temperature)
                                      gptel-temperature)))
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
                   (magent-request-context-model request-state)
                   (or (magent-request-context-model request-state) model)
                   (magent-request-context-backend request-state)
                   (or (magent-request-context-backend request-state) backend)
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
           (magent-log "INFO agent=%s backend=%s model=%s tools=[%s]"
                       (magent-agent-info-name agent)
                       (gptel-backend-name backend)
                       model
                       (mapconcat (lambda (tool) (plist-get tool :name)) tools ", "))
           (when resolved-skill-names
             (magent-log "INFO active skills=[%s]"
                         (mapconcat #'identity resolved-skill-names ", ")))
           (let* ((gptel-backend backend)
                  (gptel-model model)
                  (gptel-temperature temperature)
                  (request-tools (magent-agent-loop-tools-to-gptel tools))
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
                  (sample-text-delta-seen nil)
                  (sample-final-response-retry nil)
                  (sample-strict-final-response-retry nil)
                  (last-continuation-reason nil)
                  (tool-output-seq 0)
                  (empty-final-response-retried-for-tool-output-seq nil)
                  (strict-final-response-retried-for-tool-output-seq nil)
                  (post-tool-reasoning-idle-timer nil)
                  (post-tool-reasoning-deadline-timer nil)
                  (post-tool-reasoning-deadline-start-time nil)
                  (suppress-next-abort-error nil)
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
                       (magent-session-refresh-projections session)
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
                                 (t (format "%S" response)))))
                     (if (and thread turn-id)
                         (let ((item (or assistant-item
                                         (magent-thread-ensure-message-item
                                          thread turn-id 'assistant nil
                                          (list :source 'terminal)))))
                           (pcase status
                             ('completed
                              (let ((final-text
                                     (if (string-empty-p text)
                                         (or (magent-thread-item-content item)
                                             "")
                                       text)))
                                (magent-thread-complete-item
                                 thread item
                                 :role 'assistant
                                 :content final-text)
                                (magent-thread-complete-turn
                                 thread turn-id)))
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
                           (magent-session-refresh-projections session)
                           (condition-case err
                               (magent-session-save-for-session
                                session request-scope)
                             (error
                              (magent-log
                               "ERROR immediate session save failed: %s"
                               (error-message-string err)))))
                       (when (eq status 'completed)
                         (magent-session-add-message
                          session 'assistant text)))))
                  (emit-request-start
                   ()
                   (magent-lifecycle-events-emit
                    'llm-request-start
                    :context context
                    :backend (and backend (gptel-backend-name backend))
                    :model (format "%s" model)
                    :prompt-count
                    (length
                     (magent-llm-request-prompt
                      (magent-agent-loop-request loop)))
                    :tool-count
                    (length
                     (magent-llm-request-tools
                      (magent-agent-loop-request loop)))
                    :system-prompt-length (length (or system-msg ""))))
                  (close-reasoning
                   ()
                   (finish-reasoning-item))
                  (sample
                   ()
                   (when post-tool-reasoning-idle-timer
                     (cancel-timer post-tool-reasoning-idle-timer)
                     (setq post-tool-reasoning-idle-timer nil))
                   (when post-tool-reasoning-deadline-timer
                     (cancel-timer post-tool-reasoning-deadline-timer)
                     (setq post-tool-reasoning-deadline-timer nil))
                   (setq post-tool-reasoning-deadline-start-time nil)
                   (setq sample-text-delta-seen nil
                         sample-final-response-retry
                         (and loop
                              (plist-get
                               (magent-llm-request-metadata
                                (magent-agent-loop-request loop))
                               :final-response-retry))
                         sample-strict-final-response-retry
                         (and loop
                              (plist-get
                               (magent-llm-request-metadata
                                (magent-agent-loop-request loop))
                               :strict-final-response-retry)))
                   (cl-incf sampling-count)
                   (emit-request-start)
                   (magent-agent-loop-start loop))
                  (request-for-current-session
                   ()
                   (let ((request
                          (magent-agent-loop-request-for-current-session
                           loop)))
                     (magent-llm-request-create
                      :prompt (magent-llm-request-prompt request)
                      :system (magent-llm-request-system request)
                      :tools request-tools
                      :model (magent-llm-request-model request)
                      :backend (magent-llm-request-backend request)
                      :stream t
                      :callback (magent-llm-request-callback request)
                      :metadata
                      (magent-agent--metadata-without-retry-flags
                       (magent-llm-request-metadata request)))))
                  (request-for-current-session-final-response-retry
                   ()
                   (let ((request (request-for-current-session)))
                     (magent-llm-request-create
                      :prompt
                      (append
                       (magent-llm-request-prompt request)
                       (list
                        (cons
                         'prompt
                         (magent-agent--empty-final-response-retry-prompt))))
                      :system (magent-llm-request-system request)
                      :tools (magent-llm-request-tools request)
                      :model (magent-llm-request-model request)
                      :backend (magent-llm-request-backend request)
                      :stream nil
                      :callback (magent-llm-request-callback request)
                      :metadata
                      (append
                       (magent-agent--metadata-without-retry-flags
                        (magent-llm-request-metadata request))
                       (list :final-response-retry t
                             :disable-provider-tools t)))))
                  (request-for-current-session-strict-final-response-retry
                   ()
                   (let ((request (request-for-current-session)))
                     (magent-llm-request-create
                      :prompt
                      (append
                       (magent-llm-request-prompt request)
                       (list
                        (cons
                         'prompt
                         (magent-agent--strict-final-response-retry-prompt))))
                      :system (magent-llm-request-system request)
                      :tools (magent-llm-request-tools request)
                      :model (magent-llm-request-model request)
                      :backend (magent-llm-request-backend request)
                      :stream nil
                      :callback (magent-llm-request-callback request)
                      :metadata
                      (append
                       (magent-agent--metadata-without-retry-flags
                        (magent-llm-request-metadata request))
                       (list :final-response-retry t
                             :strict-final-response-retry t
                             :effort 'minimal
                             :include-reasoning nil
                             :disable-provider-tools t)))))
                  (cancel-post-tool-reasoning-idle-timer
                   ()
                   (when post-tool-reasoning-idle-timer
                     (cancel-timer post-tool-reasoning-idle-timer)
                     (setq post-tool-reasoning-idle-timer nil)))
                  (cancel-post-tool-reasoning-deadline-timer
                   ()
                   (when post-tool-reasoning-deadline-timer
                     (cancel-timer post-tool-reasoning-deadline-timer)
                     (setq post-tool-reasoning-deadline-timer nil))
                   (setq post-tool-reasoning-deadline-start-time nil))
                  (empty-post-tool-final-response-retry-p
                   ()
                   (and (eq last-continuation-reason 'tool-output)
                        (not sample-final-response-retry)
                        (not
                         (equal
                          empty-final-response-retried-for-tool-output-seq
                          tool-output-seq))
                        (not sample-text-delta-seen)))
                  (final-response-retry-reasoning-retry-p
                   ()
                   (and sample-final-response-retry
                        (not sample-strict-final-response-retry)
                        (not
                         (equal
                          strict-final-response-retried-for-tool-output-seq
                          tool-output-seq))
                        (not sample-text-delta-seen)))
                  (strict-final-response-retry-failure-p
                   ()
                   (and sample-strict-final-response-retry
                        (not sample-text-delta-seen)))
                  (retry-empty-final-response
                   (reason)
                   (when (empty-post-tool-final-response-retry-p)
                     (cancel-post-tool-reasoning-idle-timer)
                     (cancel-post-tool-reasoning-deadline-timer)
                     (when (memq reason '(reasoning-idle
                                          reasoning-timeout))
                       (magent-agent-loop--cancel-request-timeout loop)
                       (setq suppress-next-abort-error t)
                       (magent-agent-loop--abort-request-handle
                        (magent-agent-loop-request-handle loop)))
                     (setq empty-final-response-retried-for-tool-output-seq
                           tool-output-seq
                           last-continuation-reason
                           'empty-final-response-retry)
                     (setf (magent-agent-loop-request loop)
                           (request-for-current-session-final-response-retry))
                     (magent-log
                      "INFO retrying empty final response after tool output: %s"
                      reason)
                     (sample)
                     t))
                  (retry-strict-final-response
                   (reason)
                   (when (final-response-retry-reasoning-retry-p)
                     (cancel-post-tool-reasoning-idle-timer)
                     (cancel-post-tool-reasoning-deadline-timer)
                     (when (memq reason '(reasoning-idle
                                          reasoning-timeout))
                       (magent-agent-loop--cancel-request-timeout loop)
                       (setq suppress-next-abort-error t)
                       (magent-agent-loop--abort-request-handle
                        (magent-agent-loop-request-handle loop)))
                     (setq strict-final-response-retried-for-tool-output-seq
                           tool-output-seq
                           last-continuation-reason
                           'strict-final-response-retry)
                     (setf (magent-agent-loop-request loop)
                           (request-for-current-session-strict-final-response-retry))
                     (magent-log
                      "INFO retrying strict final response after retry reasoning: %s"
                      reason)
                     (sample)
                     t))
                  (fail-strict-final-response
                   (reason)
                   (when (strict-final-response-retry-failure-p)
                     (cancel-post-tool-reasoning-idle-timer)
                     (cancel-post-tool-reasoning-deadline-timer)
                     (when (memq reason '(reasoning-idle
                                          reasoning-timeout))
                       (magent-agent-loop--cancel-request-timeout loop)
                       (setq suppress-next-abort-error t)
                       (magent-agent-loop--abort-request-handle
                        (magent-agent-loop-request-handle loop)))
                     (let ((message
                            "Error: Model returned no assistant text after strict final-response retry."))
                       (magent-request-context-notify
                        request-state 'turn-error
                        :message message
                        :metadata
                        (list :status 'strict-final-response-retry))
                       (finish-turn
                        'failed message
                        (list :status 'strict-final-response-retry
                              :reason reason)))
                     t))
                  (handle-post-tool-reasoning-timeout
                   (reason)
                   (or (retry-empty-final-response reason)
                       (retry-strict-final-response reason)
                       (fail-strict-final-response reason)))
                  (post-tool-reasoning-guard-active-p
                   ()
                   (or (empty-post-tool-final-response-retry-p)
                       (final-response-retry-reasoning-retry-p)
                       (strict-final-response-retry-failure-p)))
                  (schedule-post-tool-reasoning-idle-retry
                   ()
                   (when (and (numberp
                              magent-post-tool-reasoning-idle-retry-delay)
                              (> magent-post-tool-reasoning-idle-retry-delay
                                 0)
                              (post-tool-reasoning-guard-active-p))
                     (cancel-post-tool-reasoning-idle-timer)
                     (setq post-tool-reasoning-idle-timer
                           (run-at-time
                            magent-post-tool-reasoning-idle-retry-delay
                            nil
                            (lambda ()
                              (setq post-tool-reasoning-idle-timer nil)
                              (handle-post-tool-reasoning-timeout
                               'reasoning-idle))))))
                  (schedule-post-tool-reasoning-deadline-retry
                   ()
                   (when (and (numberp
                              magent-post-tool-reasoning-max-duration)
                              (> magent-post-tool-reasoning-max-duration 0)
                              (post-tool-reasoning-guard-active-p))
                     (unless post-tool-reasoning-deadline-start-time
                       (setq post-tool-reasoning-deadline-start-time
                             (float-time)))
                     (unless post-tool-reasoning-deadline-timer
                       (setq post-tool-reasoning-deadline-timer
                             (run-at-time
                              magent-post-tool-reasoning-max-duration
                              nil
                              (lambda ()
                                (setq post-tool-reasoning-deadline-timer nil)
                                (handle-post-tool-reasoning-timeout
                                 'reasoning-timeout)))))))
                  (post-tool-reasoning-deadline-expired-p
                   ()
                   (and (numberp magent-post-tool-reasoning-max-duration)
                        (> magent-post-tool-reasoning-max-duration 0)
                        post-tool-reasoning-deadline-start-time
                        (>= (- (float-time)
                               post-tool-reasoning-deadline-start-time)
                            magent-post-tool-reasoning-max-duration)
                        (post-tool-reasoning-guard-active-p)))
                  (continue-turn
                   (outcome)
                   (setq last-continuation-reason
                         (plist-get outcome :reason))
                   (when (eq last-continuation-reason 'tool-output)
                     (cl-incf tool-output-seq))
                   (if (and (numberp magent-max-sampling-requests)
                            (> magent-max-sampling-requests 0)
                            (>= sampling-count
                                magent-max-sampling-requests))
                       (if sample-strict-final-response-retry
                           (let ((failure-message
                                  (format
                                   "Maximum sampling requests reached for this turn (%d); the forced final request did not complete"
                                   magent-max-sampling-requests)))
                             (finish-turn
                              'failed failure-message
                              (list :status 'sampling-limit
                                    :sampling-count sampling-count
                                    :continuation-reason
                                    last-continuation-reason)))
                         (setf (magent-agent-loop-request loop)
                               (request-for-current-session-strict-final-response-retry))
                         (magent-log
                          "WARN sampling budget reached after %d request(s); forcing one provider-tools-disabled final request: %s"
                          sampling-count last-continuation-reason)
                         (sample))
                     (setf (magent-agent-loop-request loop)
                           (request-for-current-session))
                     (magent-log
                      "INFO continuing model response: reason=%s count=%d"
                      (plist-get outcome :reason)
                      (1+ sampling-count))
                     (sample)))
                  (finish-streaming
                   ()
                   (cancel-post-tool-reasoning-idle-timer)
                   (cancel-post-tool-reasoning-deadline-timer)
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
                                  response
                                (magent-agent-result-failed
                                 response metadata)))))
                  (start-streaming
                   ()
                   (close-reasoning)
                   (setq streaming-started t))
                  (handle-event
                   (event)
                   (when (magent-agent--request-live-p live-p)
                     (let ((event-type (magent-llm-event-type event)))
                       (unless (eq event-type 'reasoning-end)
                         (cancel-post-tool-reasoning-idle-timer))
                       (unless (memq event-type '(reasoning-delta
                                                  reasoning-end))
                         (cancel-post-tool-reasoning-deadline-timer))
                     (pcase event-type
                       ('text-delta
                        (start-streaming)
                       (magent-lifecycle-events-emit
                        'text-delta
                        :context context
                        :text (magent-llm-event-text event))
                       (unless (string-empty-p
                                (or (magent-llm-event-text event) ""))
                         (setq text-delta-seen t
                               sample-text-delta-seen t))
                       (magent-request-context-notify
                        request-state 'assistant-delta
                        :text (magent-llm-event-text event))
                        (record-text-delta (magent-llm-event-text event))
                        (when (and text-callback
                                   (magent-agent--ui-visible-p request-state))
                          (funcall text-callback
                                   (magent-llm-event-text event))))
                       ('reasoning-delta
                        (magent-request-context-notify
                         request-state 'reasoning-delta
                         :text (magent-llm-event-text event))
                        (schedule-post-tool-reasoning-deadline-retry)
                        (if (post-tool-reasoning-deadline-expired-p)
                            (handle-post-tool-reasoning-timeout
                             'reasoning-timeout)
                          (record-reasoning-delta
                           (magent-llm-event-text event))))
                       ('reasoning-end
                        (magent-request-context-notify
                         request-state 'reasoning-complete)
                        (close-reasoning)
                        (schedule-post-tool-reasoning-idle-retry))
                       ('tool-call
                        (close-reasoning)
                        (magent-request-context-notify
                         request-state 'tool-call-detected
                         :tool-id (magent-llm-event-id event)
                         :name (magent-llm-event-name event)
                         :arguments (magent-llm-event-arguments event)))
                       ('tool-call-batch-end
                        (close-reasoning)
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
                        (cond
                         ((and (empty-post-tool-final-response-retry-p)
                               (string-empty-p
                                (or (magent-llm-event-text event) "")))
                          (retry-empty-final-response 'completed-empty))
                         ((and sample-strict-final-response-retry
                               (not sample-text-delta-seen)
                               (string-empty-p
                                (or (magent-llm-event-text event) "")))
                          (fail-strict-final-response 'completed-empty))
                         ((and sample-final-response-retry
                               (not sample-strict-final-response-retry)
                               (not sample-text-delta-seen)
                               (string-empty-p
                                (or (magent-llm-event-text event) "")))
                          (let ((message
                                 "Error: Model returned an empty final response after tool output retry."))
                            (magent-log
                             "WARN empty final response retry returned no text")
                            (magent-request-context-notify
                             request-state 'turn-error
                             :message message
                             :metadata
                             (list :status 'empty-final-response-retry))
                            (finish-turn
                             'failed message
                             (list :status
                                   'empty-final-response-retry))))
                         (t
                          (magent-request-context-notify
                           request-state 'assistant-complete
                           :text (magent-agent-loop-result loop))
                          (finish-turn 'completed
                                       (magent-agent-loop-result loop)))))
                      ('error
                       (if (and suppress-next-abort-error
                                (magent-agent--abort-error-event-p event))
                           (setq suppress-next-abort-error nil)
                         (magent-request-context-notify
                          request-state 'turn-error
                          :message (magent-llm-event-message event)
                          :metadata (magent-llm-event-metadata event))
                         (finish-turn 'failed
                                      (magent-llm-event-message event)
                                      (magent-llm-event-metadata event)))))))))
               (setq loop
                     (magent-agent-loop-create
                      :session session
                      :request
                     (magent-llm-request-create
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
                      :sampler #'magent-llm-gptel-sample))
               (magent-agent-loop-abort-controller-register
                (magent-agent-loop-abort-controller loop)
                (lambda ()
                  (cancel-post-tool-reasoning-idle-timer)
                  (cancel-post-tool-reasoning-deadline-timer)))
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
          approval-provider on-complete request-live-p)
  "Run one Magent turn for SESSION with PROMPT.
This is the UI-neutral execution entry point.  OBSERVER receives
Magent-native request events through REQUEST-CONTEXT.  ON-COMPLETE is
called with a string on success or a `magent-agent-result' on failure."
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
    (magent-agent-process
     prompt
     on-complete
     agent
     skills
     nil
     context
     nil
     nil
     live-p
     request-context)))

(provide 'magent-agent)
;;; magent-agent.el ends here
