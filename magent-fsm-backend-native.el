;;; magent-fsm-backend-native.el --- Native FSM backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;; FIXME: This native FSM backend is not ready for use.
;; Currently only the gptel backend (magent-fsm-backend-gptel.el) is supported.
;; See magent-fsm.el and magent-config.el.

;;; Commentary:

;; Native magent FSM backend implementation.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'gptel-request)
(require 'magent-events)
(require 'magent-fsm-shared)

;; Forward declarations
(declare-function magent-session-add-message "magent-session")
(declare-function magent-log "magent-config")
(declare-function magent-ui-start-streaming "magent-ui")
(declare-function magent-ui-continue-streaming "magent-ui")
(declare-function magent-ui-finish-streaming-fontify "magent-ui")
(declare-function magent-ui-insert-reasoning-start "magent-ui")
(declare-function magent-ui-insert-reasoning-text "magent-ui")
(declare-function magent-ui-insert-reasoning-end "magent-ui")
(declare-function gptel-request "gptel")
(declare-function gptel-tool-confirm "gptel-request")
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-confirm-tool-calls)
(defvar magent-request-timeout)
(defvar magent-include-reasoning)

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
        (backend (magent-fsm-backend fsm))
        (model (magent-fsm-model fsm)))
    (cond
     ((not session)
      (setf (magent-fsm-error fsm) "No session provided")
      (magent-fsm-transition fsm 'ERROR))
     ((not backend)
      (setf (magent-fsm-error fsm) "No backend provided")
      (magent-fsm-transition fsm 'ERROR))
     ((not model)
      (setf (magent-fsm-error fsm) "No model provided")
      (magent-fsm-transition fsm 'ERROR))
     (t
      ;; Ready to send first request
      (magent-fsm-transition fsm 'SEND)))))

(defvar magent-fsm--timers (make-hash-table :test 'eq :weakness 'key)
  "Hash table mapping FSM instances to their timeout timers.")

(defun magent-fsm--start-timeout (fsm)
  "Start or restart the timeout timer for FSM."
  (magent-fsm--cancel-timeout fsm)
  (when (and (boundp 'magent-request-timeout)
             (> magent-request-timeout 0))
    (puthash fsm
             (run-at-time magent-request-timeout nil
                          #'magent-fsm--timeout-handler fsm)
             magent-fsm--timers)))

(defun magent-fsm--immediately-finish (fsm error-msg)
  "Immediately finish FSM with ERROR-MSG, resetting processing state."
  (setf (magent-fsm-error fsm) error-msg)
  (magent-fsm-transition fsm 'ERROR))

(defun magent-fsm--cancel-timeout (fsm)
  "Cancel the timeout timer for FSM."
  (let ((timer (gethash fsm magent-fsm--timers)))
    (when timer
      (cancel-timer timer)
      (remhash fsm magent-fsm--timers))))

(defun magent-fsm--timeout-handler (fsm)
  "Handle timeout for FSM stuck in WAIT state."
  (remhash fsm magent-fsm--timers)
  (when (eq (magent-fsm-state fsm) 'WAIT)
    (magent-log "ERROR FSM timeout: no response in %d seconds"
                magent-request-timeout)
    (magent-fsm--immediately-finish fsm
      (format "Request timed out after %d seconds" magent-request-timeout))))

(defun magent-fsm--handle-send (fsm)
  "Handle SEND state: prepare and fire HTTP request via gptel."
  (magent-log "SEND state: preparing HTTP request")
  (magent-fsm--start-timeout fsm)
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
         (completed-count (make-vector 1 0))
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
                ;; Show result in UI
                (magent-fsm--show-tool-result name result)
                ;; Store result
                (push (list :id id
                            :name name
                            :args args
                            :result result)
                      results)
                (aset completed-count 0 (1+ (aref completed-count 0)))

                ;; When all tools complete, inject results and loop back
                (when (= (aref completed-count 0) total)
                  (magent-log "All %d tool(s) completed, injecting results" total)
                  (magent-fsm--inject-tool-results fsm (nreverse results))
                  ;; Clear pending tools and accumulated text for next round
                  (setf (magent-fsm-pending-tools fsm) nil)
                  (setf (magent-fsm-accumulated-text fsm) "")
                  (setf (magent-fsm-streamed-chunks fsm) "")
                  ;; Loop back to SEND
                  (magent-fsm-transition fsm 'SEND)))))

        ;; Show tool call in UI
        (magent-fsm--show-tool-call name args)

        (if (not tool-spec)
            (funcall process-result (format "Error: tool '%s' not found" name))
          ;; Execute tool (sync or async)
          (condition-case err
              (let ((tool-fn (plist-get tool-spec :function))
                    (tool-async (plist-get tool-spec :async))
                    ;; Convert args plist to positional arguments based on tool spec
                    (arg-values (magent-fsm--args-to-positional args (plist-get tool-spec :args))))
                (if tool-async
                    ;; Async tool: pass callback as first argument
                    (apply tool-fn process-result arg-values)
                  ;; Sync tool: call directly and invoke callback
                  (let ((result (apply tool-fn arg-values)))
                    (funcall process-result result))))
            (error
             (funcall process-result
                      (format "Error executing tool: %s" (error-message-string err))))))))))

(defun magent-fsm--args-to-positional (args-plist args-spec)
  "Convert ARGS-PLIST to positional arguments based on ARGS-SPEC.
ARGS-PLIST is a plist like (:path \"foo\" :content \"bar\").
ARGS-SPEC is a list of argument specifications from the tool definition.
Returns a list of positional arguments in the order defined by ARGS-SPEC."
  (mapcar
   (lambda (arg-spec)
     (let ((key (intern (concat ":" (plist-get arg-spec :name)))))
       (plist-get args-plist key)))
   args-spec))

(defun magent-fsm--handle-done (fsm)
  "Handle DONE state: finalize and call user callback."
  (magent-fsm--cancel-timeout fsm)
  (magent-log "FSM DONE: final response length=%d"
              (length (magent-fsm-accumulated-text fsm)))

  ;; Call user callback — wrap in condition-case so cleanup always runs
  (condition-case err
      (when (magent-fsm-callback fsm)
        (funcall (magent-fsm-callback fsm)
                 (magent-fsm-accumulated-text fsm)))
    (error
     (magent-log "ERROR in DONE callback: %s" (error-message-string err))))

  ;; Clean up resources (always runs even if callback threw)
  (magent-fsm-destroy fsm))

(defun magent-fsm--handle-error (fsm)
  "Handle ERROR state: log error and call user callback with nil."
  (magent-fsm--cancel-timeout fsm)
  (let ((error-msg (or (magent-fsm-error fsm) "Unknown error")))
    (magent-log "ERROR FSM error: %s" error-msg)

    ;; Call user callback with nil — wrap in condition-case so cleanup always runs
    (condition-case err
        (when (magent-fsm-callback fsm)
          (funcall (magent-fsm-callback fsm) nil))
      (error
       (magent-log "ERROR in ERROR callback: %s" (error-message-string err))))

    ;; Clean up resources (always runs even if callback threw)
    (magent-fsm-destroy fsm)))

;;; Tool Utilities

(defun magent-fsm--find-tool (tools name)
  "Find tool with NAME in TOOLS list."
  (cl-find-if (lambda (tool)
                (string= (plist-get tool :name) name))
              tools))

(defun magent-fsm--inject-tool-results (fsm results)
  "Inject tool RESULTS into FSM's prompt-list for next request.
RESULTS is a list of plists with :id, :name, :args, :result."
  (let ((prompt-list (magent-fsm-prompt-list fsm)))
    (setf (magent-fsm-prompt-list fsm)
          (append prompt-list
                  (list (cons 'tool-results results))))))

;;; HTTP Layer (gptel delegation)

(defun magent-fsm--send-via-gptel (fsm)
  "Send LLM request via gptel-request."
  (require 'gptel)
  (require 'magent-session)
  (require 'magent-ui)
  (require 'magent-config)

  (let* ((prompt-list (magent-fsm-prompt-list fsm))
         (system-prompt (magent-fsm-system-prompt fsm))
         (tools (magent-fsm-tools fsm))
         (streaming (magent-fsm-streaming-p fsm))
         (backend (magent-fsm-backend fsm))
         (model (magent-fsm-model fsm))
         (request-buffer (or (magent-fsm-request-buffer fsm)
                             (generate-new-buffer " *magent-fsm-request*"))))

    (setf (magent-fsm-request-buffer fsm) request-buffer)

    ;; For streaming, prepare the UI with assistant prompt prefix
    (when streaming
      (magent-ui-start-streaming))

    ;; Convert magent tools to gptel tools format (with permission-aware :confirm)
    (let ((converted-tools (magent-fsm--convert-tools-to-gptel
                            tools (magent-fsm-permission fsm))))

      (with-current-buffer request-buffer
        (setq-local gptel-backend backend)
        (setq-local gptel-model model)
        (setq-local gptel-tools converted-tools)
        (setq-local gptel-use-tools (and converted-tools t))
        (setq-local gptel-include-reasoning magent-include-reasoning)
        (setq-local gptel-confirm-tool-calls 'auto)
        ;; Call gptel-request from within request-buffer so
        ;; gptel--sanitize-model reads correct buffer-locals.
        (gptel-request
            prompt-list
          :buffer (current-buffer)
          :system system-prompt
          :stream streaming
          :callback (magent-fsm--make-gptel-callback fsm))))))

(defun magent-fsm--make-gptel-callback (fsm)
  "Create a gptel callback that feeds into FSM state machine.
In streaming mode, gptel manages the tool calling loop: it parses
tool calls, confirms them, executes them, and sends tool results
back to the LLM.  The FSM stays in WAIT during the entire loop
and only transitions to PROCESS/DONE when the final response
(with no tool calls) arrives."
  (lambda (response info)
    ;; Ignore callbacks if FSM already completed or aborted
    (unless (or (memq (magent-fsm-state fsm) '(DONE ERROR))
                (magent-fsm-aborted fsm))
      ;; Suppress beginning-of-buffer / end-of-buffer signals that can
      ;; leak from UI operations called inside gptel's process filter.
      ;; These are benign cursor-adjustment errors (often from evil-mode)
      ;; that would otherwise appear in *Messages* as "progn: ...".
      (condition-case nil
          (progn
            ;; Reset timeout on every callback activity
            (magent-fsm--start-timeout fsm)
            ;; Transition to WAIT state when request is sent
            (when (eq (magent-fsm-state fsm) 'SEND)
              (setf (magent-fsm-state fsm) 'WAIT))

            (cond
             ;; Reasoning block chunk
             ((and (consp response) (eq (car response) 'reasoning)
                   (magent-fsm-streaming-p fsm))
              (let ((reasoning-text (cdr response)))
                (when (and magent-include-reasoning
                           (stringp reasoning-text))
                  (setf (magent-fsm-reasoning-text fsm)
                        (concat (magent-fsm-reasoning-text fsm) reasoning-text)))
                (if (eq reasoning-text t)
                    (when (magent-fsm-in-reasoning-block fsm)
                      (magent-ui-insert-reasoning-end)
                      (setf (magent-fsm-in-reasoning-block fsm) nil))
                  (when (eq magent-include-reasoning t)
                    (unless (magent-fsm-in-reasoning-block fsm)
                      (magent-ui-insert-reasoning-start)
                      (setf (magent-fsm-in-reasoning-block fsm) t))
                    (magent-ui-insert-reasoning-text reasoning-text)))))

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
              (setf (magent-fsm-streamed-chunks fsm) "")
              ;; Close reasoning block if still open (some backends
              ;; don't send (reasoning . t) before streaming completion)
              (when (magent-fsm-in-reasoning-block fsm)
                (magent-ui-insert-reasoning-end)
                (setf (magent-fsm-in-reasoning-block fsm) nil))
              (if (or (plist-get info :tool-use)
                      (plist-get info :tool-pending))
                  ;; Tool use round: stay in the same section
                  (magent-ui-continue-streaming)
                ;; Final round: finalize and transition
                (magent-ui-finish-streaming-fontify)
                (magent-fsm-transition fsm 'PROCESS)))

             ;; gptel tool-call confirmation — permission-aware handling.
             ;; Fires when a tool's :confirm function returns t.
             ;; Format: (tool-call . ((gptel-tool arg-values callback) ...))
             ((and (consp response) (eq (car response) 'tool-call))
              ;; Close reasoning block if open before tool call
              (when (magent-fsm-in-reasoning-block fsm)
                (magent-ui-insert-reasoning-end)
                (setf (magent-fsm-in-reasoning-block fsm) nil))
              (magent-fsm--handle-tool-call-confirmation fsm (cdr response)))

             ;; gptel tool-result — tool call/result already shown via wrapped function.
             ;; Continue in the same section (streaming mode only).
             ;; Format: (tool-result . ((gptel-tool args result) ...))
             ((and (consp response) (eq (car response) 'tool-result))
              ;; Reset reasoning state for new response
              (setf (magent-fsm-in-reasoning-block fsm) nil)
              ;; Continue streaming in the same section, no new heading
              (when (magent-fsm-streaming-p fsm)
                (magent-ui-continue-streaming)
                (setf (magent-fsm-streamed-chunks fsm) "")))

             ;; Error
             ((null response)
              (setf (magent-fsm-error fsm)
                    (or (plist-get info :status) "Request failed"))
              (magent-fsm-transition fsm 'ERROR))

;; Abort
              ((eq response 'abort)
               (setf (magent-fsm-error fsm) "Request aborted")
               (magent-fsm-transition fsm 'ERROR))))
         ((beginning-of-buffer end-of-buffer beginning-of-line end-of-line)
          (magent-log "DEBUG Suppressed cursor error in FSM callback")
          nil)))))


;;; Public API

(defun magent-fsm-native-start (fsm)
  "Start FSM execution from INIT state."
  (unless (eq (magent-fsm-state fsm) 'INIT)
    (error "FSM must be in INIT state to start"))
  (magent-fsm-transition fsm 'INIT))

(defun magent-fsm-native-abort (fsm)
  "Abort FSM execution immediately and clean up all resources."
  (setf (magent-fsm-aborted fsm) t)
  (magent-fsm--cancel-timeout fsm)
  (setf (magent-fsm-error fsm) "User aborted")

  ;; Call user callback to reset UI state
  (condition-case err
      (when (magent-fsm-callback fsm)
        (funcall (magent-fsm-callback fsm) nil))
    (error
     (magent-log "ERROR in abort callback: %s" (error-message-string err))))

  ;; Immediately destroy FSM to kill HTTP process
  (magent-fsm-destroy fsm))

(defun magent-fsm-native-destroy (fsm)
  "Clean up all FSM resources explicitly.
Cancels timeout timers, kills request buffers, and deletes processes.
Call this when a FSM is no longer needed to prevent resource leaks."
  (when fsm
    ;; Cancel timeout timer
    (magent-fsm--cancel-timeout fsm)

    ;; Kill request buffer
    (when-let ((buf (magent-fsm-request-buffer fsm)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))

    ;; Delete process if still running
    (when-let ((proc (magent-fsm-process fsm)))
      (when (process-live-p proc)
        (delete-process proc)))

    (magent-log "INFO FSM destroyed and resources cleaned up")))

(provide 'magent-fsm-backend-native)

;;; magent-fsm-backend-native.el ends here
