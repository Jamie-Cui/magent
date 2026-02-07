;;; magent-agent.el --- Agent logic and tool calling for OpenCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Code:

(require 'cl-lib)
(require 'magent-tools)
(require 'magent-session)
(require 'magent-api)
(require 'magent-agent-registry)
(require 'magent-agent-info)
(require 'magent-permission)

;;; Agent execution

(defun magent-agent-process (user-prompt &optional callback agent-info)
  "Process USER-PROMPT through the AI agent with tool calling.
Calls CALLBACK with the final response when complete.
AGENT-INFO is the agent to use (defaults to current session's agent)."
  (let ((session (magent-session-get))
        ;; Get agent from session or use default
        (agent (or agent-info
                   (magent-session-get-agent session)
                   (magent-agent-registry-get-default))))
    ;; Store agent in session
    (magent-session-set-agent session agent)
    ;; Add user message to session
    (magent-session-add-message session 'user user-prompt)

    ;; Start the agent loop with agent context
    (magent-agent--loop session agent callback)))

(defun magent-agent--loop (session agent callback &optional iteration)
  "Main agent loop for handling tool calls.
SESSION is the current session.
AGENT is the agent info structure.
CALLBACK is called with final response.
ITERATION tracks loop depth to prevent infinite loops."
  (let ((iter (or iteration 0))
        (max-iterations 10))
    (when (>= iter max-iterations)
      (error "Agent exceeded maximum iterations (%d)" max-iterations))

    (let ((messages (magent-session-get-messages session))
          ;; Get tools filtered by agent permissions
          (tools (magent-agent--get-tools agent)))
      ;; Make API request with agent's model and temperature
      (magent-api-chat
       messages
       :tools tools
       :stream nil
       :model (magent-agent-info-model-string agent)
       :temperature (or (magent-agent-info-temperature agent)
                        magent-temperature)
       :callback (lambda (response)
                   (magent-agent--handle-response
                    session agent response callback iter))))))

(defun magent-agent--get-tools (agent-info)
  "Get tool definitions for AGENT-INFO, filtered by permissions."
  (let* ((permission (magent-agent-info-permission agent-info))
         (all-tools (magent-tools-get-definitions))
         (filtered-tools nil))
    (dolist (tool all-tools)
      (let ((tool-name (cdr (assq 'name tool))))
        (when (magent-permission-allow-p permission tool-name)
          (push tool filtered-tools))))
    (nreverse filtered-tools)))

(defun magent-agent--handle-response (session agent response callback iteration)
  "Handle API RESPONSE, executing tools or calling CALLBACK."
  (let ((tool-uses (magent-api--extract-tool-uses response)))
    (if tool-uses
        ;; Execute tools and continue loop
        (magent-agent--execute-tools session agent tool-uses response callback iteration)
      ;; No tools, done - add assistant message and call callback
      (let ((content (magent-api--extract-content response)))
        (magent-session-add-message session 'assistant content)
        (when callback
          (funcall callback content))))))

(defun magent-agent--execute-tools (session agent tool-uses response callback iteration)
  "Execute TOOL-USES and continue agent loop."
  (let ((assistant-msg `((role . "assistant")
                         (content . ,(cdr (assq 'content response))))))
    ;; Add assistant message with tool uses to session
    (magent-session-add-message session 'assistant
                                 (cdr (assq 'content response)))

    ;; Execute each tool with permission check
    (dolist (tool-use tool-uses)
      (let* ((tool-id (cdr (assq 'id tool-use)))
             (tool-name (cdr (assq 'name tool-use)))
             (tool-input (cdr (assq 'input tool-use)))
             (permission (magent-agent-info-permission agent)))
        ;; Check permission before executing
        (if (magent-permission-allow-p permission tool-name)
            (let ((result (magent-tools-execute tool-name tool-input)))
              (magent-session-add-tool-result session tool-id result))
          ;; Permission denied
          (magent-session-add-tool-result session tool-id
                                          (format "Permission denied: tool '%s' not allowed" tool-name))))))

    ;; Continue the loop
    (magent-agent--loop session agent callback (1+ iteration))))

;;; Streaming support

(defun magent-agent-process-stream (user-prompt callback &optional agent-info)
  "Process USER-PROMPT with streaming response.
CALLBACK is called with each content chunk.
AGENT-INFO is the agent to use (defaults to default agent)."
  (let ((session (magent-session-get))
        (agent (or agent-info
                   (magent-agent-registry-get-default))))
    (magent-session-set-agent session agent)
    (magent-session-add-message session 'user user-prompt)

    (let ((messages (magent-session-get-messages session))
          (tools (magent-agent--get-tools agent)))
      (magent-api-chat
       messages
       :tools tools
       :stream t
       :model (magent-agent-info-model-string agent)
       :callback (lambda (chunk)
                   (funcall callback chunk))))))

;;; Tool use formatting

(defun magent-agent--format-tool-use (tool-use)
  "Format a tool use for display."
  (let ((name (cdr (assq 'name tool-use)))
        (input (cdr (assq 'input tool-use))))
    (format "🔧 Using: %s\n   Args: %s"
            name
            (if (stringp input)
                input
              (json-encode input)))))

(defun magent-agent--format-tool-result (result)
  "Format a tool result for display."
  (let ((truncated (if (> (length result) 500)
                      (concat (substring result 0 500) "...")
                    result)))
    (format "   Result: %s" truncated)))

;;; Context injection

(defun magent-agent--inject-context (session)
  "Inject relevant context into the session.
This adds information about the current project."
  (when magent-project-root-function
    (let* ((root (funcall magent-project-root-function))
           (files (magent-session-get-project-files session root)))
      (when files
        (let ((context-msg
               (format "Current project files:\n%s"
                       (string-join (take 20 files) "\n"))))
          ;; This could be added as a system message or context
          context-msg)))))

;;; Error handling

(defun magent-agent--handle-error (err session callback)
  "Handle an error during agent execution."
  (let ((error-msg (format "Error: %s" (error-message-string err))))
    (magent-session-add-message session 'assistant error-msg)
    (when callback
      (funcall callback error-msg))))

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
