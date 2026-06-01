;;; magent-agent.el --- Agent processing for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

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
(require 'magent-runtime)
(require 'magent-tools)
(require 'magent-tool-registry)
(require 'magent-session)
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
    (magent-session-add-message session 'user user-prompt)
    (let* ((prompt-list (magent-session-to-gptel-prompt-list session))
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
         (let ((backend gptel-backend)
               (model gptel-model))
           (magent-log "INFO agent=%s backend=%s model=%s tools=[%s]"
                       (magent-agent-info-name agent)
                       (gptel-backend-name backend)
                       model
                       (mapconcat (lambda (tool) (plist-get tool :name)) tools ", "))
           (when resolved-skill-names
             (magent-log "INFO active skills=[%s]"
                         (mapconcat #'identity resolved-skill-names ", ")))
           (let* ((gptel-tools (magent-agent-loop-tools-to-gptel tools))
                  (live-p (or request-live-p
                              (and request-state
                                   (magent-request-context-live-p
                                    request-state))))
                  (streaming-started nil)
                  (reasoning-open nil)
                  loop)
             (cl-labels
                 ((emit-request-start
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
                    :tool-count (length gptel-tools)
                    :system-prompt-length (length (or system-msg ""))))
                  (sample
                   ()
                   (emit-request-start)
                   (magent-agent-loop-start loop))
                  (finish-streaming
                   ()
                   (when (and reasoning-open
                              (magent-agent--ui-visible-p request-state))
                     (setq reasoning-open nil)
                     (magent-ui-insert-reasoning-end))
                   (when (and streaming-started
                              (magent-agent--ui-visible-p request-state))
                     (magent-ui-finish-streaming-fontify)))
                  (finish-turn
                   (status response)
                   (finish-streaming)
                   (magent-events-emit
                    'llm-request-end
                    :context context
                    :status status
                    :backend (and backend (gptel-backend-name backend))
                    :model (format "%s" model))
                   (unless event-context
                     (magent-events-end-turn context status))
                   (when (and (eq status 'completed)
                              (stringp response))
                     (magent-session-add-message session 'assistant response))
                   (when callback
                     (funcall callback response)))
                  (start-streaming
                   ()
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
                           (magent-llm-event-text event))))
                       ('reasoning-end
                        (when (and reasoning-open
                                   (magent-agent--ui-visible-p request-state))
                          (setq reasoning-open nil)
                          (magent-ui-insert-reasoning-end)))
                       ('tool-call
                        (when (and streaming-started
                                   (magent-agent--ui-visible-p request-state))
                          (magent-ui-continue-streaming))
                        (when (plist-get (magent-llm-event-metadata event)
                                         :last)
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
                           (lambda (&optional result)
                             (if (and (eq (magent-agent-loop-status loop)
                                          'failed)
                                      (stringp result))
                                 (finish-turn 'failed result)
                               (setf (magent-agent-loop-request loop)
                                     (magent-agent-loop-request-for-current-session
                                      loop))
                               (sample))))))
                       ('completed
                        (finish-turn 'completed
                                     (magent-llm-event-text event)))
                       ('error
                        (finish-turn 'failed
                                     (magent-llm-event-message event)))))))
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
                      :callback #'handle-event)
                      :request-context request-state
                      :sampler #'magent-llm-gptel-sample
                      :max-tool-rounds (magent-agent-info-steps agent)))
               (sample)
               loop))))))))

(provide 'magent-agent)
;;; magent-agent.el ends here
