;;; magent-agent.el --- Agent logic with internal FSM  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Agent logic for Magent.  Uses internal FSM (magent-fsm.el) for managing
;; the tool calling loop.  gptel is used as an HTTP backend provider.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'magent-fsm)
(require 'magent-tools)
(require 'magent-session)
(require 'magent-agent-registry)
(require 'magent-permission)

;; Forward declarations for UI functions
(declare-function magent-ui-insert-streaming "magent-ui")

;;; Agent execution

(defun magent-agent-process (user-prompt &optional callback agent-info)
  "Process USER-PROMPT through the AI agent using magent FSM.
CALLBACK is called with the final string response when complete.
AGENT-INFO is the agent to use (defaults to session agent or registry default).

The tool calling loop is managed by magent-fsm.  This function:
  1. Builds the prompt list from session history
  2. Retrieves per-agent overrides (backend, model, temperature, tools)
  3. Creates FSM and starts execution
  4. Records the final response in the session via the callback"
  (let* ((session (magent-session-get))
         (agent (or agent-info
                    (magent-session-get-agent session)
                    (magent-agent-registry-get-default))))
    ;; Store agent in session
    (magent-session-set-agent session agent)
    ;; Add user message to session
    (magent-session-add-message session 'user user-prompt)
    ;; Build prompt list from full session history
    (let* ((prompt-list (magent-session-to-gptel-prompt-list session))
           (system-msg (or (magent-agent-info-prompt agent)
                           magent-system-prompt))
           (tools (magent-tools-get-magent-tools agent)))

      ;; Apply per-agent gptel variable overrides to get backend/model
      (magent-agent-info-apply-gptel-overrides
       agent
       (lambda ()
         (let ((backend gptel-backend)
               (model gptel-model))

           (magent-log "INFO agent=%s backend=%s model=%s tools=[%s] streaming=%s"
                       (magent-agent-info-name agent)
                       (gptel-backend-name backend)
                       model
                       (mapconcat (lambda (tool) (plist-get tool :name)) tools ", ")
                       magent-enable-streaming)

           ;; Create FSM
           (let ((fsm (magent-fsm-create
                       :session session
                       :agent agent
                       :backend backend
                       :model model
                       :prompt-list prompt-list
                       :system-prompt system-msg
                       :tools tools
                       :streaming-p magent-enable-streaming
                       :callback callback
                       :ui-callback #'magent-ui-insert-streaming)))

             ;; Start FSM execution
             (magent-fsm-start fsm))))))))

;;; Agent selection helpers

(defun magent-agent-get-by-name (name)
  "Get agent by NAME from registry."
  (magent-agent-registry-get name))

(defun magent-agent-list-primary ()
  "List all primary agents."
  (magent-agent-registry-primary-agents))

(defun magent-agent-list-subagents ()
  "List all subagents."
  (magent-agent-registry-subagents))

(provide 'magent-agent)
;;; magent-agent.el ends here
