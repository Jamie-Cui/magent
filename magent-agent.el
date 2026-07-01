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
(require 'magent-events)
(require 'magent-llm)
(require 'magent-llm-gptel)
(require 'magent-protocol)
(require 'magent-runtime)
(require 'magent-tools)
(require 'magent-tool-registry)
(require 'magent-session)
(require 'magent-thread)
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

(defconst magent-agent--sampling-limit-final-prompt
  "Magent has reached the per-turn tool budget. Stop using tools and answer the original user request now using only the tool results already available. Be concise.")

(defun magent-agent--sampling-limit-final-request (loop)
  "Return a no-tool final response request for LOOP's current session."
  (let* ((request (magent-agent-loop-request-for-current-session loop))
         (metadata (magent-llm-request-metadata request)))
    (magent-llm-request-create
     :prompt (append (magent-llm-request-prompt request)
                     (list (cons 'prompt
                                 magent-agent--sampling-limit-final-prompt)))
     :system (magent-llm-request-system request)
     :tools nil
     :model (magent-llm-request-model request)
     :backend (magent-llm-request-backend request)
     :stream t
     :callback (magent-llm-request-callback request)
     :metadata (append metadata '(:sampling-limit-final t)))))

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
                      (magent-events-begin-turn
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
           (system-msg (if skill-prompts
                           (concat base-system-msg
                                   "\n\n# Active Skills\n\n"
                                   (mapconcat #'identity skill-prompts "\n\n"))
                         base-system-msg))
           (tools (mapcar #'magent-tool-registry-runtime-to-plist
                          (magent-tool-registry-for-agent agent))))
      (when capability-resolution
        (magent-events-emit
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
                (backend (or inherited-backend gptel-backend))
                (model (or inherited-model gptel-model))
                (temperature (or inherited-temperature
                                 (and (boundp 'gptel-temperature)
                                      gptel-temperature)))
                (top-p (or inherited-top-p
                           (magent-agent-info-top-p agent))))
           (when request-state
             (setf (magent-request-context-project-root request-state)
                   (or (magent-request-context-project-root request-state)
                       (and request-context
                            (plist-get request-context :project-root))
                       (let ((scope (magent-request-context-scope request-state)))
                         (and (stringp scope) scope))
                       (ignore-errors (magent-project-root nil t)))
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
                  (gptel-tools (magent-agent-loop-tools-to-gptel tools))
                  (live-p (or request-live-p
                              (and request-state
                                   (magent-request-context-live-p
                                    request-state))))
                  (streaming-started nil)
                  (reasoning-open nil)
                  (assistant-item nil)
                  (reasoning-item nil)
                  (sampling-count 0)
                  (sampling-limit-final-attempted nil)
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
                                (if (magent-session--assistant-response-reusable-p
                                     final-text)
                                    (magent-thread-complete-turn
                                     thread turn-id)
                                  (magent-thread-fail-turn
                                   thread turn-id final-text))))
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
                   (magent-events-emit
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
                   (cl-incf sampling-count)
                   (emit-request-start)
                   (magent-agent-loop-start loop))
                  (continue-turn
                   (outcome)
                   (if (and (numberp magent-max-sampling-requests)
                            (> magent-max-sampling-requests 0)
                            (>= sampling-count
                                magent-max-sampling-requests))
                       (if sampling-limit-final-attempted
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
                         (setq sampling-limit-final-attempted t)
                         (setf (magent-agent-loop-request loop)
                               (magent-agent--sampling-limit-final-request
                                loop))
                         (magent-log
                          "WARN forcing final response after %d sampling request(s): %s"
                          sampling-count
                          (plist-get outcome :reason))
                         (sample))
                     (setf (magent-agent-loop-request loop)
                           (magent-agent-loop-request-for-current-session
                            loop))
                     (magent-log
                      "INFO continuing model response: reason=%s count=%d"
                      (plist-get outcome :reason)
                      (1+ sampling-count))
                     (sample)))
                  (finish-streaming
                   ()
                   (close-reasoning)
                   (when (and streaming-started
                              (magent-agent--ui-visible-p request-state))
                     (magent-ui-finish-streaming-fontify)))
                  (finish-turn
                   (status response &optional metadata)
                   (finish-streaming)
                   (magent-events-emit
                    'llm-request-end
                    :context context
                    :status status
                    :backend (and backend (gptel-backend-name backend))
                    :model (format "%s" model))
                   (unless event-context
                     (magent-events-end-turn context status))
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
                     (pcase (magent-llm-event-type event)
                       ('text-delta
                        (start-streaming)
                       (magent-events-emit
                        'text-delta
                        :context context
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
                        (record-reasoning-delta
                         (magent-llm-event-text event)))
                       ('reasoning-end
                        (close-reasoning))
                       ('tool-call
                        (close-reasoning)
                        (when (and streaming-started
                                   (magent-agent--ui-visible-p request-state))
                          (magent-ui-continue-streaming)))
                       ('tool-call-batch-end
                        (close-reasoning)
                        (when (and streaming-started
                                   (magent-agent--ui-visible-p request-state))
                          (magent-ui-continue-streaming))
                        (magent-events-emit
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
                        (let ((ui-text
                               (magent-agent--completion-ui-text
                                loop event streaming-started)))
                          (when (and (stringp ui-text)
                                     (not (string-empty-p ui-text)))
                            (start-streaming)
                            (when (magent-agent--ui-visible-p request-state)
                              (funcall (or ui-callback
                                           #'magent-ui-insert-streaming)
                                       ui-text))))
                        (finish-turn 'completed
                                     (magent-agent-loop-result loop)))
                      ('error
                        (finish-turn 'failed
                                     (magent-llm-event-message event)
                                     (magent-llm-event-metadata event)))))))
               (setq loop
                     (magent-agent-loop-create
                      :session session
                      :request
                     (magent-llm-request-create
                      :prompt prompt-list
                      :system system-msg
                      :tools gptel-tools
                      :model model
                      :backend backend
                      :stream t
                      :metadata (list :temperature temperature
                                      :top-p top-p)
                      :callback #'handle-event)
                      :request-context request-state
                      :turn-id (and request-state
                                    (magent-request-context-turn-id
                                     request-state))
                      :sampler #'magent-llm-gptel-sample))
               (sample)
               loop))))))))

(provide 'magent-agent)
;;; magent-agent.el ends here
