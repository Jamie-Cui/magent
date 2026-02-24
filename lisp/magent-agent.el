;;; magent-agent.el --- Agent logic using gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Agent logic for Magent.  Uses gptel for LLM communication and tool calling.
;; The agent loop (tool calls, re-requests) is managed entirely by gptel's FSM.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'magent-tools)
(require 'magent-session)
(require 'magent-agent-registry)
(require 'magent-agent-info)
(require 'magent-permission)

;; Forward declarations for UI functions used in the callback
(declare-function magent-ui-insert-tool-call "magent-ui")
(declare-function magent-ui-insert-error "magent-ui")
(declare-function magent-log "magent-ui")

;;; Agent execution

(defun magent-agent-process (user-prompt &optional callback agent-info)
  "Process USER-PROMPT through the AI agent using gptel.
CALLBACK is called with the final string response when complete.
AGENT-INFO is the agent to use (defaults to session agent or registry default).

The tool calling loop is managed by gptel's FSM.  Magent's role is to:
  1. Build the prompt list from session history
  2. Set up per-agent gptel overrides (backend, model, temperature, tools)
  3. Call `gptel-request' with the correct system prompt and tools
  4. Record the final response in the session via the callback"
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
           (tools (magent-tools-get-gptel-tools agent)))
      ;; Apply per-agent gptel variable overrides, then fire the request
      (magent-agent-info-apply-gptel-overrides
       agent
       (lambda ()
         (let ((gptel-tools tools)
               (gptel-use-tools (if tools t nil)))
           (gptel-request
             prompt-list
             :system system-msg
             :stream nil
             :callback (magent-agent--make-callback session callback))))))))

(defun magent-agent--make-callback (session final-callback)
  "Return a gptel callback that handles all response types.
SESSION is the magent session to record the final response.
FINAL-CALLBACK is called with the response string (or nil on error)."
  (lambda (response info)
    (cond
     ;; Final string response — the normal completion path
     ((stringp response)
      (magent-session-add-message session 'assistant response)
      (when final-callback
        (funcall final-callback response)))

     ;; Tool results (tools have run, FSM is continuing)
     ((and (consp response) (eq (car response) 'tool-result))
      (dolist (entry (cdr response))
        (let* ((tool (car entry))
               (args (cadr entry)))
          (when (fboundp 'magent-ui-insert-tool-call)
            (magent-ui-insert-tool-call (gptel-tool-name tool) args)))))

     ;; Tool calls pending user confirmation
     ((and (consp response) (eq (car response) 'tool-call))
      ;; gptel handles the confirmation UI; we just log
      (when (fboundp 'magent-log)
        (dolist (pending (cdr response))
          (magent-log "Awaiting confirmation for tool: %s"
                      (gptel-tool-name (car pending))))))

     ;; Reasoning block (extended thinking) — ignore
     ((and (consp response) (eq (car response) 'reasoning))
      nil)

     ;; Abort
     ((eq response 'abort)
      (when final-callback
        (funcall final-callback nil)))

     ;; nil / error
     ((null response)
      (let ((status (plist-get info :status)))
        (when (fboundp 'magent-ui-insert-error)
          (magent-ui-insert-error (or status "Request failed")))
        (when final-callback
          (funcall final-callback nil)))))))

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
