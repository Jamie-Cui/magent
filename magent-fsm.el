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
(declare-function magent-log "magent-ui")
(declare-function magent-ui-insert-tool-call "magent-ui")
(declare-function magent-ui-insert-tool-result "magent-ui")
(declare-function magent-ui-start-streaming "magent-ui")
(declare-function magent-ui-finish-streaming-fontify "magent-ui")
(declare-function gptel-request "gptel")
(declare-function gptel-make-tool "gptel")
(declare-function gptel-tool-async "gptel-request")
(declare-function gptel-tool-function "gptel-request")
(declare-function gptel-tool-confirm "gptel-request")
(declare-function gptel-tool-name "gptel-request")
(declare-function gptel-tool-args "gptel-request")
(defvar gptel-backend)
(defvar gptel-model)
(defvar magent-request-timeout)

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
    (setf (magent-fsm-error fsm)
          (format "Request timed out after %d seconds" magent-request-timeout))
    (magent-fsm-transition fsm 'ERROR)))

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
                ;; Show result in UI
                (magent-fsm--show-tool-result name result)
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

  ;; Add final response to session
  (when (> (length (magent-fsm-accumulated-text fsm)) 0)
    (magent-session-add-message (magent-fsm-session fsm)
                                'assistant
                                (magent-fsm-accumulated-text fsm)))

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

(defun magent-fsm--show-tool-call (name args)
  "Show tool call NAME with ARGS in the UI."
  (require 'magent-ui)
  (let ((input (cond
                ;; skill_invoke: show skill_name and operation
                ((string= name "skill_invoke")
                 (let ((skill-name (plist-get args :skill_name))
                       (operation (plist-get args :operation)))
                   (format "%s/%s" (or skill-name "?") (or operation "?"))))
                ;; delegate: show agent name
                ((string= name "delegate")
                 (let ((agent (plist-get args :agent)))
                   (format "agent: %s" (or agent "?"))))
                ;; bash: show command (truncated)
                ((string= name "bash")
                 (let ((cmd (plist-get args :command)))
                   (if (and cmd (> (length cmd) magent-ui-tool-input-max-length))
                       (format "%s..." (substring cmd 0 (- magent-ui-tool-input-max-length 3)))
                     (or cmd "?"))))
                ;; read_file/write_file/edit_file: show path
                ((member name '("read_file" "write_file" "edit_file"))
                 (let ((path (plist-get args :path)))
                   (or path "?")))
                ;; grep: show pattern
                ((string= name "grep")
                 (let ((pattern (plist-get args :pattern))
                       (path (plist-get args :path)))
                   (format "%s in %s" (or pattern "?") (or path "?"))))
                ;; glob: show pattern
                ((string= name "glob")
                 (let ((pattern (plist-get args :pattern)))
                   (or pattern "?")))
                ;; emacs_eval: show sexp (truncated)
                ((string= name "emacs_eval")
                 (let ((sexp (plist-get args :sexp)))
                   (if (and sexp (> (length sexp) magent-ui-tool-input-max-length))
                       (format "%s..." (substring sexp 0 (- magent-ui-tool-input-max-length 3)))
                     (or sexp "?"))))
                ;; Default: show all args
                (t
                 (if args
                     (truncate-string-to-width
                      (format "%s" args) 60 nil nil "...")
                   "")))))
    (magent-ui-insert-tool-call name input)))

(defun magent-fsm--show-tool-result (name result)
  "Show tool RESULT for tool NAME in the UI."
  (require 'magent-ui)
  (require 'magent-config)
  (let* ((result-str (if (stringp result) result (format "%s" result)))
         (truncated (if (> (length result-str) magent-ui-result-max-length)
                        (format "%s... [%d bytes]"
                                (substring result-str 0 magent-ui-result-preview-length)
                                (length result-str))
                      result-str)))
    (magent-ui-insert-tool-result name truncated)))

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

    ;; Convert magent tools to gptel tools format
    (let ((converted-tools (magent-fsm--convert-tools-to-gptel tools)))

      (with-current-buffer request-buffer
        (setq-local gptel-tools converted-tools)
        (setq-local gptel-use-tools (if converted-tools t nil)))

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
  "Create a gptel callback that feeds into FSM state machine.
In streaming mode, gptel manages the tool calling loop: it parses
tool calls, confirms them, executes them, and sends tool results
back to the LLM.  The FSM stays in WAIT during the entire loop
and only transitions to PROCESS/DONE when the final response
(with no tool calls) arrives."
  (lambda (response info)
    ;; Ignore callbacks if FSM already completed
    (unless (memq (magent-fsm-state fsm) '(DONE ERROR))
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
              ;; Apply markdown fontification to completed streaming text
              (magent-ui-finish-streaming-fontify)
              ;; If no tool calls pending, we are done
              (unless (or (plist-get info :tool-use)
                          (plist-get info :tool-pending))
                (magent-fsm-transition fsm 'PROCESS)))

             ;; Non-streaming final response
             ((stringp response)
              (setf (magent-fsm-accumulated-text fsm) response)
              (magent-fsm-transition fsm 'PROCESS))

             ;; gptel tool-call confirmation — show in UI, then auto-accept.
             ;; Only fires when gptel-confirm-tool-calls is t or tool has :confirm.
             ;; Format: (tool-call . ((gptel-tool arg-values callback) ...))
             ((and (consp response) (eq (car response) 'tool-call))
              (dolist (tc (cdr response))
                (let* ((tool-spec (car tc))
                       (arg-values (cadr tc))
                       (cb (caddr tc))
                       (name (gptel-tool-name tool-spec))
                       (args-plist
                        (cl-loop for spec in (gptel-tool-args tool-spec)
                                 for val in arg-values
                                 append (list (intern (concat ":" (plist-get spec :name))) val))))
                  (magent-fsm--show-tool-call name args-plist)
                  ;; Auto-accept: run the tool and call the callback
                  (if (gptel-tool-async tool-spec)
                      (apply (gptel-tool-function tool-spec) cb arg-values)
                    (funcall cb (apply (gptel-tool-function tool-spec) arg-values))))))

             ;; gptel tool-result — show tool call + result, prepare for next round.
             ;; Format: (tool-result . ((gptel-tool args result) ...))
             ;; In auto-confirm mode (default), this is the primary path.
             ;; args here is a plist (:key val ...) from gptel.
             ((and (consp response) (eq (car response) 'tool-result))
              (dolist (tr (cdr response))
                (let ((name (gptel-tool-name (car tr)))
                      (args (cadr tr))
                      (result (caddr tr)))
                  (magent-fsm--show-tool-call name args)
                  (magent-fsm--show-tool-result name result)))
              ;; Prepare UI for next streaming response
              (magent-ui-start-streaming)
              (setf (magent-fsm-streamed-chunks fsm) ""))

             ;; Error
             ((null response)
              (setf (magent-fsm-error fsm)
                    (or (plist-get info :status) "Request failed"))
              (magent-fsm-transition fsm 'ERROR))

             ;; Abort
             ((eq response 'abort)
              (setf (magent-fsm-error fsm) "Request aborted")
              (magent-fsm-transition fsm 'ERROR))))
        ((beginning-of-buffer end-of-buffer)
         (magent-log "DEBUG Suppressed cursor error in FSM callback")
         nil)))))

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

(defun magent-fsm-destroy (fsm)
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

(provide 'magent-fsm)

;;; magent-fsm.el ends here
