;;; magent-agent.el --- Agent processing for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

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
(require 'magent-protocol)
(require 'magent-runtime)
(require 'magent-tools)
(require 'magent-tool-runtime)
(require 'magent-session)
(require 'magent-ledger)
(require 'magent-agent-registry)
(require 'magent-permission)

;; Forward declarations for UI functions
(declare-function magent-ui-insert-streaming "magent-ui")
(declare-function magent-ui-start-streaming "magent-ui")
(declare-function magent-ui-continue-streaming "magent-ui")
(declare-function magent-ui-finish-streaming-fontify "magent-ui")
(declare-function magent-ui-insert-reasoning-start "magent-ui")
(declare-function magent-ui-insert-reasoning-text "magent-ui")
(declare-function magent-ui-insert-reasoning-end "magent-ui")
(declare-function magent-capability-resolution-skill-names "magent-capability")
(declare-function magent-capability-resolution-to-plist "magent-capability")
(declare-function magent-capability-resolve-for-turn "magent-capability")
(declare-function magent-skills-get-instruction-prompts "magent-skills")

;;; Agent execution

(defconst magent-agent--empty-final-response-retry-prompt
  "The previous assistant response after tool execution was empty. Continue the task now. Use tools if additional inspection is required; otherwise write the final answer."
  "User prompt appended when retrying an empty post-tool final response.")

(defconst magent-agent--strict-final-response-retry-prompt
  "The previous final-response retry still produced no assistant text. Do not call tools or emit tool-call markup. Write the final answer now based only on the context already available."
  "User prompt appended when strict final-response recovery is required.")

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

(defun magent-agent--dedupe-skill-names (skill-names)
  "Return SKILL-NAMES without duplicates, preserving order."
  (let (seen result)
    (dolist (name skill-names (nreverse result))
      (when (and name (not (member name seen)))
        (push name seen)
        (push name result)))))

(defun magent-agent--request-live-p (request-live-p)
  "Return non-nil when REQUEST-LIVE-P allows callbacks to run."
  (or (null request-live-p)
      (funcall request-live-p)))

(defun magent-agent--ui-visible-p (request-state)
  "Return non-nil when REQUEST-STATE allows UI writes."
  (or (null request-state)
      (magent-request-context-ui-visible-p request-state)))

(defun magent-agent--completion-ui-text (loop event streaming-started)
  "Return assistant text that still needs UI rendering for EVENT.
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

(defun magent-agent--context-system-message (project-root)
  "Return prompt context for PROJECT-ROOT."
  (when (and (stringp project-root)
             (not (string-empty-p project-root)))
    (format
     "Current project root: %s\nUse this as the current repository/workspace. When the user says \"this repo\", \"this repository\", or \"this project\", inspect this path. Resolve relative file tool paths against this root and do not invent unrelated absolute paths."
     project-root)))

(defun magent-agent--compose-system-message
    (base-system-message project-root skill-prompts)
  "Return system prompt from BASE-SYSTEM-MESSAGE, PROJECT-ROOT, and SKILL-PROMPTS."
  (let ((context-message
         (magent-agent--context-system-message project-root))
        (skills-message
         (when skill-prompts
           (concat "# Active Skills\n\n"
                   (mapconcat #'identity skill-prompts "\n\n")))))
    (mapconcat #'identity
               (delq nil
                     (list base-system-message
                           context-message
                           skills-message))
               "\n\n")))

(defun magent-agent-process
    (user-prompt &optional callback agent-info skill-names event-context
                 request-context capability-resolution ui-callback request-live-p
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
UI-CALLBACK receives streaming text chunks for this request and defaults
to `magent-ui-insert-streaming'.
REQUEST-LIVE-P is an optional predicate used to discard stale backend
callbacks after the UI has moved on to a newer request.
REQUEST-STATE is an optional `magent-request-context' carrying runtime
state for the request.
When nil, no skills are injected (skills must be explicitly selected
via slash commands in the prompt).

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
         (context (or event-context
                      (and request-state
                           (magent-request-context-event-context request-state))
                      (magent-lifecycle-events-begin-turn
                       (format "Agent %s" (magent-agent-info-name agent))))))
    (when request-state
      (setf (magent-request-context-session request-state) session
            (magent-request-context-live-p request-state)
            (or (magent-request-context-live-p request-state)
                request-live-p)
            (magent-request-context-event-context request-state) context))
    (magent-session-set-agent session agent)
    (let* ((thread (magent-session-thread-ledger session))
           (state-turn-id (and request-state
                               (magent-request-context-turn-id request-state)))
           (state-turn (and state-turn-id
                            (magent-thread-find-turn thread state-turn-id))))
      (if state-turn
          (progn
            (magent-thread-record-user-message-if-needed
             thread state-turn-id user-prompt nil
             (list :source 'agent-process))
            (magent-session-refresh-projections session))
        (magent-session-add-message session 'user user-prompt))
      (when request-state
        (let* ((active-turn (magent-thread-active-turn
                             (magent-session-thread-ledger session)))
               (turn-id (or (and state-turn state-turn-id)
                            (and active-turn
                                 (magent-thread-turn-id active-turn)))))
          (when turn-id
            (setf (magent-request-context-turn-id request-state) turn-id)))))
    (let* ((current-turn-id (and request-state
                                 (magent-request-context-turn-id request-state)))
           (prompt-list (magent-session-to-gptel-prompt-list
                         session current-turn-id))
           (base-system-msg (or (magent-agent-info-prompt agent)
                                magent-system-prompt))
           (request-project-root
            (magent-agent--request-project-root request-context request-state))
           (capability-resolution
            (or capability-resolution
                (when (require 'magent-capability nil t)
                  (magent-capability-resolve-for-turn
                   user-prompt request-context skill-names))))
           (resolved-skill-names
            (magent-agent--dedupe-skill-names
             (or (and capability-resolution
                      (magent-capability-resolution-skill-names
                       capability-resolution))
                 skill-names)))
           (skill-prompts (when (and (require 'magent-skills nil t)
                                     resolved-skill-names)
                            (magent-skills-get-instruction-prompts
                             resolved-skill-names)))
           (system-msg
            (magent-agent--compose-system-message
             base-system-msg request-project-root skill-prompts))
           (tools (mapcar #'magent-tool-runtime-to-plist
                          (magent-tool-runtime-for-agent agent))))
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
                   (or (magent-request-context-skill-names request-state)
                       resolved-skill-names)
                   (magent-request-context-capability-context request-state)
                   (or (magent-request-context-capability-context request-state)
                       (and capability-resolution
                            (magent-capability-resolution-to-plist
                             capability-resolution))
                       request-context)
                   (magent-request-context-permission-profile request-state)
                   (or (magent-request-context-permission-profile
                        request-state)
                       (magent-agent-info-permission agent))))
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
                  (reasoning-open nil)
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
                     (magent-session-save-deferred)))
                  (record-reasoning-delta
                   (text)
                   (when (not (null magent-include-reasoning))
                     (when-let* ((chunk (and (stringp text) text))
                                 (thread (magent-session-thread-ledger session))
                                 (item (ensure-reasoning-item)))
                       (magent-thread-append-item-content thread item chunk)
                       (magent-session-save-deferred))))
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
                       (magent-session-save-deferred))
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
                               (magent-session-save)
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
                   (when reasoning-open
                     (setq reasoning-open nil)
                     (when (magent-agent--ui-visible-p request-state)
                       (magent-ui-insert-reasoning-end)))
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
                         magent-agent--empty-final-response-retry-prompt)))
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
                         magent-agent--strict-final-response-retry-prompt)))
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
                   (if (and (numberp magent-max-sampling-requests)
                            (> magent-max-sampling-requests 0)
                            (>= sampling-count
                                magent-max-sampling-requests))
                       (let ((message
                              (format
                               "Maximum sampling requests reached for this turn (%d)"
                               magent-max-sampling-requests)))
                         (magent-log
                          "WARN stopping turn continuation after %d sampling request(s): %s"
                          sampling-count
                          (plist-get outcome :reason))
                         (finish-turn 'failed message
                                      (list :status 'sampling-limit
                                           :sampling-count sampling-count
                                           :continuation-reason
                                           (plist-get outcome :reason))))
                     (setq last-continuation-reason
                           (plist-get outcome :reason))
                     (when (eq last-continuation-reason 'tool-output)
                       (cl-incf tool-output-seq))
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
                   (close-reasoning)
                   (when (and streaming-started
                              (magent-agent--ui-visible-p request-state))
                     (magent-ui-finish-streaming-fontify)))
                  (finish-turn
                   (status response &optional metadata)
                   (finish-streaming)
                   (magent-lifecycle-events-emit
                    'llm-request-end
                    :context context
                    :status status
                    :backend (and backend (gptel-backend-name backend))
                    :model (format "%s" model))
                   (unless event-context
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
                   (when (and (not streaming-started)
                              (magent-agent--ui-visible-p request-state))
                     (setq streaming-started t)
                     (magent-ui-start-streaming)))
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
                        (when (magent-agent--ui-visible-p request-state)
                          (funcall (or ui-callback
                                       #'magent-ui-insert-streaming)
                                   (magent-llm-event-text event))))
                       ('reasoning-delta
                        (when (and (eq magent-include-reasoning t)
                                   (magent-agent--ui-visible-p request-state))
                          (unless reasoning-open
                            (setq reasoning-open t)
                            (magent-ui-insert-reasoning-start))
                          (magent-ui-insert-reasoning-text
                           (magent-llm-event-text event)))
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
                         :arguments (magent-llm-event-arguments event))
                        (when (and streaming-started
                                   (magent-agent--ui-visible-p request-state))
                          (magent-ui-continue-streaming)))
                       ('tool-call-batch-end
                        (close-reasoning)
                        (when (and streaming-started
                                   (magent-agent--ui-visible-p request-state))
                          (magent-ui-continue-streaming))
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
                          (magent-agent-info-permission agent)
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
                               (magent-agent--completion-ui-text
                                loop event text-delta-seen))
                              (ui-text
                               (magent-agent--completion-ui-text
                                loop event streaming-started)))
                          (when (and (stringp observer-text)
                                     (not (string-empty-p observer-text)))
                            (magent-request-context-notify
                             request-state 'assistant-delta
                             :text observer-text))
                          (when (and (stringp ui-text)
                                     (not (string-empty-p ui-text)))
                            (start-streaming)
                            (when (magent-agent--ui-visible-p request-state)
                              (funcall (or ui-callback
                                           #'magent-ui-insert-streaming)
                                       ui-text))))
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
                      :turn-id (and request-state
                                    (magent-request-context-turn-id
                                     request-state))
                      :sampler #'magent-llm-gptel-sample))
               (sample)
               loop))))))))

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
