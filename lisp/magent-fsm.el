;;; magent-fsm.el --- FSM for tool calling loop  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Finite state machine for managing LLM request/response cycles with tool calling.
;; This FSM handles the complete loop: send request → receive response → execute tools
;; → inject results → send new request → ... until done.
;;
;; States:
;;   INIT    - Initial state, ready to start
;;   SEND    - Preparing HTTP request
;;   WAIT    - HTTP request in flight
;;   PROCESS - Processing LLM response
;;   TOOL    - Executing tool calls
;;   DONE    - Terminal state (success)
;;   ERROR   - Terminal state (failure)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

;; Forward declarations
(declare-function magent-session-add-message "magent-session")
(declare-function magent-log "magent-config")

;;; FSM Data Structure

(cl-defstruct (magent-fsm (:constructor magent-fsm-create)
                          (:copier magent-fsm-copy))
  "Finite state machine for magent LLM requests."
  (state 'INIT)              ; Current FSM state
  (session nil)              ; magent-session struct
  (agent nil)                ; magent-agent-info struct
  (backend nil)              ; gptel-backend or custom backend
  (model nil)                ; Model symbol (e.g., 'claude-sonnet-4)
  (prompt-list nil)          ; Conversation history as prompt list
  (system-prompt nil)        ; System prompt string
  (tools nil)                ; List of available tools
  (streaming-p nil)          ; Whether to stream responses
  (pending-tools nil)        ; Tool calls awaiting execution
  (accumulated-text "")      ; Accumulated text response
  (streamed-chunks "")       ; Streaming chunks buffer
  (http-status nil)          ; Last HTTP status code
  (http-message nil)         ; Last HTTP status message
  (error nil)                ; Error message if any
  (callback nil)             ; User callback (called on DONE/ERROR)
  (ui-callback nil)          ; UI callback for streaming chunks
  (request-buffer nil)       ; Buffer for HTTP request
  (process nil))             ; HTTP process object

;;; State Transition

(defun magent-fsm-transition (fsm new-state)
  "Transition FSM to NEW-STATE and execute state handler."
  (magent-log "FSM transition: %s -> %s" (magent-fsm-state fsm) new-state)
  (setf (magent-fsm-state fsm) new-state)
  (magent-fsm--handle-state fsm))

(defun magent-fsm--handle-state (fsm)
  "Execute handler for current FSM state."
  (pcase (magent-fsm-state fsm)
    ('INIT (magent-fsm--handle-init fsm))
    ('SEND (magent-fsm--handle-send fsm))
    ('WAIT nil)  ; Waiting for HTTP response, nothing to do
    ('PROCESS (magent-fsm--handle-process fsm))
    ('TOOL (magent-fsm--handle-tool fsm))
    ('DONE (magent-fsm--handle-done fsm))
    ('ERROR (magent-fsm--handle-error fsm))))

;;; State Handlers

(defun magent-fsm--handle-init (fsm)
  "Handle INIT state: validate inputs and prepare for first request."
  (let ((session (magent-fsm-session fsm))
        (agent (magent-fsm-agent fsm))
        (backend (magent-fsm-backend fsm))
        (model (magent-fsm-model fsm)))
    (unless session
      (setf (magent-fsm-error fsm) "No session provided")
      (magent-fsm-transition fsm 'ERROR)
      (cl-return-from magent-fsm--handle-init))
    (unless backend
      (setf (magent-fsm-error fsm) "No backend provided")
      (magent-fsm-transition fsm 'ERROR)
      (cl-return-from magent-fsm--handle-init))
    (unless model
      (setf (magent-fsm-error fsm) "No model provided")
      (magent-fsm-transition fsm 'ERROR)
      (cl-return-from magent-fsm--handle-init))
    ;; Ready to send first request
    (magent-fsm-transition fsm 'SEND)))

(defun magent-fsm--handle-send (fsm)
  "Handle SEND state: prepare and fire HTTP request."
  ;; TODO: Implement HTTP request using gptel's backend interface
  ;; For now, use gptel-request as a fallback
  (magent-log "SEND state: preparing HTTP request")

  ;; This will be replaced with direct HTTP implementation
  ;; For MVP, we delegate to gptel but capture the FSM flow
  (magent-fsm--send-via-gptel fsm))

(defun magent-fsm--handle-process (fsm)
  "Handle PROCESS state: parse response and determine next state."
  ;; This is called after HTTP response arrives
  ;; Response has been parsed and stored in FSM
  (cond
   ((magent-fsm-error fsm)
    (magent-fsm-transition fsm 'ERROR))
   ((magent-fsm-pending-tools fsm)
    (magent-fsm-transition fsm 'TOOL))
   (t
    ;; No tools, we're done
    (magent-fsm-transition fsm 'DONE))))

(defun magent-fsm--handle-tool (fsm)
  "Handle TOOL state: execute all pending tool calls."
  (let* ((tool-calls (magent-fsm-pending-tools fsm))
         (total (length tool-calls))
         (completed 0)
         (results '()))

    (magent-log "TOOL state: executing %d tool call(s)" total)

    ;; Execute each tool call
    (dolist (tool-call tool-calls)
      (let* ((name (plist-get tool-call :name))
             (args (plist-get tool-call :args))
             (id (plist-get tool-call :id))
             (tool-spec (magent-fsm--find-tool (magent-fsm-tools fsm) name))
             (process-result
              (lambda (result)
                ;; Store result
                (push (list :id id
                            :name name
                            :args args
                            :result result)
                      results)
                (cl-incf completed)

                ;; When all tools complete, inject results and loop back
                (when (= completed total)
                  (magent-log "All %d tool(s) completed, injecting results" total)
                  (magent-fsm--inject-tool-results fsm (nreverse results))
                  ;; Clear pending tools and accumulated text for next round
                  (setf (magent-fsm-pending-tools fsm) nil)
                  (setf (magent-fsm-accumulated-text fsm) "")
                  (setf (magent-fsm-streamed-chunks fsm) "")
                  ;; Loop back to SEND
                  (magent-fsm-transition fsm 'SEND)))))

        (unless tool-spec
          (funcall process-result (format "Error: tool '%s' not found" name))
          (cl-return-from nil))

        ;; Execute tool (sync or async)
        (condition-case err
            (let ((tool-fn (plist-get tool-spec :function))
                  (tool-async (plist-get tool-spec :async)))
              (if tool-async
                  ;; Async tool: pass callback as first argument
                  (apply tool-fn process-result args)
                ;; Sync tool: call directly and invoke callback
                (let ((result (apply tool-fn args)))
                  (funcall process-result result))))
          (error
           (funcall process-result
                    (format "Error executing tool: %s" (error-message-string err)))))))))

(defun magent-fsm--handle-done (fsm)
  "Handle DONE state: finalize and call user callback."
  (magent-log "FSM DONE: final response length=%d"
              (length (magent-fsm-accumulated-text fsm)))

  ;; Add final response to session
  (when (> (length (magent-fsm-accumulated-text fsm)) 0)
    (magent-session-add-message (magent-fsm-session fsm)
                                'assistant
                                (magent-fsm-accumulated-text fsm)))

  ;; Call user callback
  (when (magent-fsm-callback fsm)
    (funcall (magent-fsm-callback fsm)
             (magent-fsm-accumulated-text fsm))))

(defun magent-fsm--handle-error (fsm)
  "Handle ERROR state: log error and call user callback with nil."
  (let ((error-msg (or (magent-fsm-error fsm) "Unknown error")))
    (magent-log "ERROR FSM error: %s" error-msg)

    ;; Call user callback with nil to signal error
    (when (magent-fsm-callback fsm)
      (funcall (magent-fsm-callback fsm) nil))))

;;; Tool Utilities

(defun magent-fsm--find-tool (tools name)
  "Find tool with NAME in TOOLS list."
  (cl-find-if (lambda (tool)
                (string= (plist-get tool :name) name))
              tools))

(defun magent-fsm--inject-tool-results (fsm results)
  "Inject tool RESULTS into FSM's prompt-list for next request.
RESULTS is a list of plists with :id, :name, :args, :result."
  ;; Format depends on backend type
  ;; For Anthropic: append tool_result content blocks
  ;; For OpenAI: append assistant message with tool_calls, then tool messages

  (let ((backend (magent-fsm-backend fsm))
        (prompt-list (magent-fsm-prompt-list fsm)))

    ;; For now, use a simplified approach that works with gptel
    ;; We'll enhance this when we implement direct HTTP

    ;; Append tool results as a special marker for gptel to handle
    ;; This is a temporary implementation
    (setf (magent-fsm-prompt-list fsm)
          (append prompt-list
                  (list (cons 'tool-results results))))))

;;; HTTP Layer (Temporary gptel delegation)

(defun magent-fsm--send-via-gptel (fsm)
  "Temporary implementation: delegate to gptel-request.
This will be replaced with direct HTTP implementation."
  (require 'gptel)
  (require 'magent-session)

  (let* ((prompt-list (magent-fsm-prompt-list fsm))
         (system-prompt (magent-fsm-system-prompt fsm))
         (tools (magent-fsm-tools fsm))
         (streaming (magent-fsm-streaming-p fsm))
         (backend (magent-fsm-backend fsm))
         (model (magent-fsm-model fsm))
         (request-buffer (or (magent-fsm-request-buffer fsm)
                             (generate-new-buffer " *magent-fsm-request*"))))

    (setf (magent-fsm-request-buffer fsm) request-buffer)

    ;; Convert magent tools to gptel tools format
    (let ((gptel-tools (magent-fsm--convert-tools-to-gptel tools)))

      (with-current-buffer request-buffer
        (setq-local gptel-tools gptel-tools)
        (setq-local gptel-use-tools (if gptel-tools t nil)))

      ;; Call gptel-request with FSM callback
      (let ((gptel-backend backend)
            (gptel-model model))
        (gptel-request
            prompt-list
          :buffer request-buffer
          :system system-prompt
          :stream streaming
          :callback (magent-fsm--make-gptel-callback fsm))))))

(defun magent-fsm--make-gptel-callback (fsm)
  "Create a gptel callback that feeds into FSM state machine."
  (lambda (response info)
    ;; Transition to WAIT state when request is sent
    (when (eq (magent-fsm-state fsm) 'SEND)
      (setf (magent-fsm-state fsm) 'WAIT))

    (cond
     ;; Streaming text chunk
     ((and (magent-fsm-streaming-p fsm) (stringp response))
      (setf (magent-fsm-streamed-chunks fsm)
            (concat (magent-fsm-streamed-chunks fsm) response))
      ;; Call UI callback
      (when (magent-fsm-ui-callback fsm)
        (funcall (magent-fsm-ui-callback fsm) response)))

     ;; Streaming completion (t signal)
     ((and (magent-fsm-streaming-p fsm) (eq response t))
      (setf (magent-fsm-accumulated-text fsm)
            (concat (magent-fsm-accumulated-text fsm)
                    (magent-fsm-streamed-chunks fsm)))
      ;; Don't transition yet if tools are pending
      (unless (plist-get info :tool-pending)
        (magent-fsm-transition fsm 'PROCESS)))

     ;; Non-streaming final response
     ((stringp response)
      (setf (magent-fsm-accumulated-text fsm) response)
      (magent-fsm-transition fsm 'PROCESS))

     ;; Tool results (after execution)
     ((and (consp response) (eq (car response) 'tool-result))
      ;; gptel has executed tools for us, continue
      (setf (magent-fsm-streamed-chunks fsm) ""))

     ;; Pending tool calls (need confirmation)
     ((and (consp response) (eq (car response) 'tool-call))
      ;; Auto-accept tool calls (magent uses permission system instead)
      (require 'gptel)
      (gptel--accept-tool-calls (cdr response) nil))

     ;; Error
     ((null response)
      (setf (magent-fsm-error fsm)
            (or (plist-get info :status) "Request failed"))
      (magent-fsm-transition fsm 'ERROR))

     ;; Abort
     ((eq response 'abort)
      (setf (magent-fsm-error fsm) "Request aborted")
      (magent-fsm-transition fsm 'ERROR)))))

(defun magent-fsm--convert-tools-to-gptel (tools)
  "Convert magent tool specs to gptel-tool structs.
TOOLS is a list of plists with :name, :description, :args, :function, :async."
  (mapcar (lambda (tool)
            (require 'gptel)
            (gptel-make-tool
             :name (plist-get tool :name)
             :description (plist-get tool :description)
             :args (plist-get tool :args)
             :function (plist-get tool :function)
             :async (plist-get tool :async)))
          tools))

;;; Public API

(defun magent-fsm-start (fsm)
  "Start FSM execution from INIT state."
  (unless (eq (magent-fsm-state fsm) 'INIT)
    (error "FSM must be in INIT state to start"))
  (magent-fsm-transition fsm 'INIT))

(defun magent-fsm-abort (fsm)
  "Abort FSM execution."
  (setf (magent-fsm-error fsm) "User aborted")
  (magent-fsm-transition fsm 'ERROR))

(provide 'magent-fsm)

;;; magent-fsm.el ends here
