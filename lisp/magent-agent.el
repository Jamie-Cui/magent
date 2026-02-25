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
(declare-function magent-ui-insert-streaming "magent-ui")
(declare-function magent-ui-start-streaming "magent-ui")
(declare-function gptel--accept-tool-calls "gptel")

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
      (magent-log "INFO agent=%s model=%s tools=[%s] streaming=%s"
                  (magent-agent-info-name agent)
                  gptel-model
                  (mapconcat #'gptel-tool-name tools ", ")
                  magent-enable-streaming)
      ;; Apply per-agent gptel variable overrides, then fire the request.
      ;;
      ;; gptel reads gptel-tools and gptel-use-tools via `buffer-local-value'
      ;; from the :buffer argument (defaults to current-buffer).  Dynamic `let'
      ;; bindings are invisible to `buffer-local-value', so we must set them
      ;; buffer-locally in the buffer that gptel will read from.
      (magent-agent-info-apply-gptel-overrides
       agent
       (lambda ()
         (let ((request-buffer (get-buffer-create " *magent-request*")))
           (with-current-buffer request-buffer
             (setq-local gptel-tools tools)
             (setq-local gptel-use-tools (if tools t nil)))
           (gptel-request
             prompt-list
             :buffer request-buffer
             :system system-msg
             :stream magent-enable-streaming
             :callback (magent-agent--make-callback session callback))))))))

(defun magent-agent--make-callback (session final-callback)
  "Return a gptel callback that handles all response types.
SESSION is the magent session to record the final response.
FINAL-CALLBACK is called with the response string (or nil on error).

Handles both streaming and non-streaming modes.  When streaming,
gptel calls this callback repeatedly with string chunks, then
with t to signal completion."
  (let ((streamed-text "")
        (streaming-started nil))
    (lambda (response info)
      (cond
       ;; Streaming: text chunk (string while streaming is enabled)
       ((and magent-enable-streaming (stringp response))
        (unless streaming-started
          (setq streaming-started t)
          (magent-log "INFO streaming started")
          (magent-ui-start-streaming))
        (setq streamed-text (concat streamed-text response))
        (magent-ui-insert-streaming response))

       ;; Streaming: completion signal (response is t)
       ((and magent-enable-streaming (eq response t))
        (magent-log "INFO streaming complete (%d chars)" (length streamed-text))
        (magent-session-add-message session 'assistant streamed-text)
        (when final-callback
          (funcall final-callback streamed-text)))

       ;; Non-streaming: final string response
       ((stringp response)
        (magent-log "INFO response received (%d chars)" (length response))
        (magent-session-add-message session 'assistant response)
        (when final-callback
          (funcall final-callback response)))

       ;; Tool results (tools have run, FSM is continuing)
       ((and (consp response) (eq (car response) 'tool-result))
        ;; Reset streaming state for the next text segment
        (setq streamed-text "")
        (setq streaming-started nil)
        (dolist (entry (cdr response))
          (let* ((tool (car entry))
                 (args (cadr entry))
                 (result (caddr entry)))
            (magent-log "INFO tool result: %s args=%s result=%s"
                        (gptel-tool-name tool)
                        (truncate-string-to-width (format "%s" args) 120 nil nil "...")
                        (truncate-string-to-width (format "%s" result) 120 nil nil "..."))
            (magent-ui-insert-tool-call (gptel-tool-name tool) args))))

       ;; Tool calls pending user confirmation — auto-accept since magent
       ;; manages access control via its permission system.  gptel's
       ;; :confirm mechanism is not wired to magent's UI, so pending calls
       ;; would silently stall the FSM if left unhandled.
       ((and (consp response) (eq (car response) 'tool-call))
        (let ((pending (cdr response)))
          (magent-log "INFO auto-accepting %d pending tool call(s)" (length pending))
          (dolist (p pending)
            (magent-log "INFO  -> %s arg-values=%s raw-tool-use=%s"
                        (gptel-tool-name (car p))
                        (truncate-string-to-width (format "%s" (cadr p)) 120 nil nil "...")
                        (truncate-string-to-width
                         (format "%s" (plist-get info :tool-use)) 200 nil nil "...")))
          (gptel--accept-tool-calls pending nil)))

       ;; Reasoning block (extended thinking) — ignore
       ((and (consp response) (eq (car response) 'reasoning))
        (magent-log "INFO reasoning block received"))

       ;; Abort
       ((eq response 'abort)
        (magent-log "INFO request aborted")
        (when final-callback
          (funcall final-callback nil)))

       ;; nil / error
       ((null response)
        (let ((status (plist-get info :status)))
          (magent-log "ERROR request failed: %s" (or status "unknown"))
          (magent-ui-insert-error (or status "Request failed"))
          (when final-callback
            (funcall final-callback nil))))))))

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
