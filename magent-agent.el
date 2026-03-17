;;; magent-agent.el --- Agent processing for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Agent processing for Magent.  Builds prompt lists from session history,
;; applies per-agent gptel overrides, and starts FSM execution.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'magent-events)
(require 'magent-fsm)
(require 'magent-tools)
(require 'magent-session)
(require 'magent-agent-registry)
(require 'magent-permission)

;; Forward declarations for UI functions
(declare-function magent-ui-insert-streaming "magent-ui")
(declare-function magent-capability-resolution-skill-names "magent-capability")
(declare-function magent-capability-resolve-for-turn "magent-capability")
(declare-function magent-skills-get-instruction-prompts "magent-skills")

;;; Agent execution

(defun magent-agent-process
    (user-prompt &optional callback agent-info skill-names event-context request-context)
  "Process USER-PROMPT through the AI agent using magent FSM.
CALLBACK is called with the final string response when complete.
AGENT-INFO is the agent to use (defaults to session agent or registry default).
SKILL-NAMES is a list of skill name strings to activate for this request.
EVENT-CONTEXT is an optional existing event context to reuse.
REQUEST-CONTEXT is an optional structured context plist captured at
dispatch time.
When nil, no skills are injected (skills must be explicitly selected
via slash commands in the prompt).

The tool calling loop is managed by magent-fsm.  This function:
  1. Builds the prompt list from session history
  2. Retrieves per-agent overrides (backend, model, temperature, tools)
  3. Creates FSM and starts execution
  4. Records the final response in the session via the callback"
  (let* ((session (magent-session-get))
         (agent (or agent-info
                    (magent-session-agent session)
                    (magent-agent-registry-get-default)))
         (context (or event-context
                      (magent-events-begin-turn
                       (format "Agent %s" (magent-agent-info-name agent))))))
    (magent-session-set-agent session agent)
    (magent-session-add-message session 'user user-prompt)
    (let* ((prompt-list (magent-session-to-gptel-prompt-list session))
           (base-system-msg (or (magent-agent-info-prompt agent)
                                magent-system-prompt))
           (capability-resolution
            (when (require 'magent-capability nil t)
              (magent-capability-resolve-for-turn
               user-prompt request-context skill-names)))
           (resolved-skill-names
            (or (and capability-resolution
                     (magent-capability-resolution-skill-names
                      capability-resolution))
                skill-names))
           (skill-prompts (when (and (require 'magent-skills nil t)
                                     resolved-skill-names)
                            (magent-skills-get-instruction-prompts
                             resolved-skill-names)))
           (system-msg (if skill-prompts
                           (concat base-system-msg
                                   "\n\n# Active Skills\n\n"
                                   (mapconcat #'identity skill-prompts "\n\n"))
                         base-system-msg))
           (tools (magent-tools-get-magent-tools agent)))
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
           (let ((fsm (magent-fsm-create
                       :session session
                       :agent agent
                       :backend backend
                       :model model
                       :prompt-list prompt-list
                       :system-prompt system-msg
                       :tools tools
                       :event-context context
                       :permission (magent-agent-info-permission agent)
                       :callback (lambda (response)
                                   (unless event-context
                                     (magent-events-end-turn
                                      context
                                      (if (stringp response) 'completed 'failed)))
                                   (when (stringp response)
                                     (magent-session-add-message session 'assistant response))
                                   (when callback (funcall callback response)))
                       :ui-callback #'magent-ui-insert-streaming)))
             (magent-fsm-start fsm)
             fsm)))))))

(provide 'magent-agent)
;;; magent-agent.el ends here
