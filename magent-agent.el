;;; magent-agent.el --- Agent processing for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Agent processing for Magent.  Builds prompt lists from session history,
;; applies per-agent gptel overrides, and dispatches to the configured backend.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'magent-backend)
(require 'magent-tools)
(require 'magent-session)
(require 'magent-agent-registry)
(require 'magent-permission)

;; Forward declarations for UI functions
(declare-function magent-ui-insert-streaming "magent-ui")
(declare-function magent-skills-get-instruction-prompts "magent-skills")
(declare-function magent-agent-info-resolve-backend "magent-agent-registry")
(declare-function magent-agent-info-resolve-model "magent-agent-registry")
(defvar magent-backend-type)

;;; Agent execution

(defun magent-agent-process (user-prompt &optional callback agent-info)
  "Process USER-PROMPT through the AI agent using configured backend.
CALLBACK is called with the final string response when complete.
AGENT-INFO is the agent to use (defaults to session agent or registry default).

The tool calling loop is managed by the configured backend (see `magent-backend-type').
This function:
  1. Builds the prompt list from session history
  2. Retrieves per-agent overrides (backend, model, temperature, tools)
  3. Creates backend request and starts execution
  4. Records the final response in the session via the callback"
  (let* ((session (magent-session-get))
         (agent (or agent-info
                    (magent-session-get-agent session)
                    (magent-agent-registry-get-default))))
    (magent-session-set-agent session agent)
    (magent-session-add-message session 'user user-prompt)
    (let* ((prompt-list (magent-session-to-gptel-prompt-list session))
           (base-system-msg (or (magent-agent-info-prompt agent)
                                magent-system-prompt))
           (skill-prompts (when (require 'magent-skills nil t)
                            (magent-skills-get-instruction-prompts)))
           (system-msg (if skill-prompts
                           (concat base-system-msg
                                   "\n\n# Active Skills\n\n"
                                   (mapconcat #'identity skill-prompts "\n\n"))
                         base-system-msg))
           (backend (magent-agent-info-resolve-backend agent))
           (model (magent-agent-info-resolve-model agent))
           (_ (magent-log "DEBUG resolved model=%s gptel-model=%s buffer=%s" model gptel-model (buffer-name)))
           (backend-type (or magent-backend-type 'gptel))
           (backend-impl (magent-backend-get backend-type))
           ;; Use gptel-tools for gptel backend, magent-tools for FSM backend
           (tools (if (eq backend-type 'gptel)
                      (magent-tools-get-gptel-tools agent)
                    (magent-tools-get-magent-tools agent))))
      (unless backend-impl
        (error "Backend not found: %s" backend-type))
      (magent-log "INFO agent=%s backend-type=%s gptel-backend=%s model=%s tools=[%s]"
                  (magent-agent-info-name agent)
                  backend-type
                  (gptel-backend-name backend)
                  model
                  (if (eq backend-type 'gptel)
                      (mapconcat (lambda (tool) (gptel-tool-name tool)) tools ", ")
                    (mapconcat (lambda (tool) (plist-get tool :name)) tools ", ")))
      (let* ((request (magent-backend-request-create
                       :session session
                       :agent agent
                       :backend backend
                       :model model
                       :prompt-list prompt-list
                       :system-prompt system-msg
                       :tools tools
                       :streaming-p t
                       :permission (magent-agent-info-permission agent)
                       :callback callback
                       :ui-callback #'magent-ui-insert-streaming))
             (handle (magent-backend-start backend-impl request)))
        handle))))

(provide 'magent-agent)
;;; magent-agent.el ends here
