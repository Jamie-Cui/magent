;;; magent-agent-loop.el --- Magent-owned agent turn loop  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Magent-owned turn loop state.  This module consumes normalized
;; `magent-llm-event' values instead of raw gptel callback/FSM details,
;; and owns tool execution, continuation, and abort handling.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel-request)
(require 'magent-config)
(require 'magent-json)
(require 'magent-lifecycle-events)
(require 'magent-llm)
(require 'magent-protocol)
(require 'magent-runtime)
(require 'magent-session)
(require 'magent-ledger)
(require 'magent-tool-orchestrator)
(require 'magent-tools)

(declare-function magent-audit-record-permission-decision "magent-audit")

(defvar magent-tools--register-cancel)
(defvar magent-tools--request-context)

(defconst magent-agent-loop--tool-input-summary-max-length 60
  "Maximum width of tool input summaries emitted by the runtime.")

(defconst magent-agent-loop--tool-result-summary-max-length 200
  "Maximum tool result length emitted without a summary preview.")

(defconst magent-agent-loop--tool-result-summary-preview-length 150
  "Tool result preview length used by runtime lifecycle events.")

(define-error 'magent-agent-loop-invalid-tool-arguments
  "Invalid textual tool arguments")

(cl-defstruct
    (magent-agent-loop-abort-controller
     (:constructor magent-agent-loop-abort-controller-create))
  "Request-scoped abort state for Magent-owned tool execution."
  cleanups
  aborted)

(defun magent-agent-loop-abort-controller-register (controller cleanup)
  "Register CLEANUP with abort CONTROLLER.
If CONTROLLER is already aborted, run CLEANUP immediately."
  (when (functionp cleanup)
    (if (and controller
             (magent-agent-loop-abort-controller-aborted controller))
        (with-demoted-errors "Magent abort cleanup error: %S"
          (funcall cleanup))
      (when controller
        (push cleanup
              (magent-agent-loop-abort-controller-cleanups controller)))))
  cleanup)

(defun magent-agent-loop-abort-controller-abort (controller)
  "Abort CONTROLLER and run its cleanup closures once."
  (when (and controller
             (not (magent-agent-loop-abort-controller-aborted controller)))
    (setf (magent-agent-loop-abort-controller-aborted controller) t)
    (let ((cleanups (nreverse
                     (magent-agent-loop-abort-controller-cleanups
                      controller))))
      (setf (magent-agent-loop-abort-controller-cleanups controller) nil)
      (dolist (cleanup cleanups)
        (with-demoted-errors "Magent abort cleanup error: %S"
          (funcall cleanup))))))

(cl-defstruct (magent-agent-loop-tool-queue
               (:constructor magent-agent-loop-tool-queue-create))
  "Serial queue for Magent-owned tool execution."
  items
  busy
  aborted)

(cl-defstruct (magent-agent-loop
               (:constructor magent-agent-loop--create)
               (:copier nil))
  request
  sampler
  session
  request-context
  turn-id
  abort-controller
  tool-queue
  request-handle
  request-timeout-timer
  status
  text-chunks
  reasoning-chunks
  tool-calls
  usage
  stop-reason
  error
  result
  metadata)

(defvar magent-agent-loop--event-contexts
  (make-hash-table :test #'eq :weakness 'key)
  "Lifecycle contexts keyed by loop identity without changing struct layout.")

(defvar magent-agent-loop--context-ownership
  (make-hash-table :test #'eq :weakness 'key)
  "Lifecycle ownership markers keyed by loop identity.")

(defvar magent-agent-loop--tool-continuations
  (make-hash-table :test #'eq :weakness 'key)
  "Provider continuation callbacks keyed by loop identity.")

(defvar magent-agent-loop--sample-text-chunks
  (make-hash-table :test #'eq :weakness 'key)
  "Current provider sample text chunks keyed by loop identity.")

(defun magent-agent-loop-tool-continuation (loop)
  "Return LOOP's pending provider continuation, if any."
  (gethash loop magent-agent-loop--tool-continuations))

(defun magent-agent-loop-set-tool-continuation (loop continuation)
  "Set LOOP's pending provider CONTINUATION and return it."
  (if continuation
      (puthash loop continuation magent-agent-loop--tool-continuations)
    (remhash loop magent-agent-loop--tool-continuations))
  continuation)

(defun magent-agent-loop-event-context (loop)
  "Return LOOP's captured lifecycle context, if any."
  (gethash loop magent-agent-loop--event-contexts))

(defun magent-agent-loop-owns-event-context-p (loop)
  "Return non-nil when LOOP owns its lifecycle context.
Loops created before this metadata existed retain the historical owned
behavior, which is safe because lifecycle completion is idempotent."
  (let ((marker (gethash loop magent-agent-loop--context-ownership 'missing)))
    (if (eq marker 'missing) t (eq marker 'owned))))

(defun magent-agent-loop-create (&rest args)
  "Create a `magent-agent-loop' from keyword ARGS.
Recognized keys include `:request', `:sampler', `:request-context',
`:event-context', `:owns-event-context-p', `:status', and `:metadata'.
SAMPLER is a function called with REQUEST by `magent-agent-loop-start'."
  (let ((sampler (plist-get args :sampler)))
    (when (and sampler (not (functionp sampler)))
      (error "Agent loop sampler is not callable: %S" sampler))
    (let ((loop
           (magent-agent-loop--create
            :request (plist-get args :request)
            :sampler sampler
            :session (plist-get args :session)
            :request-context (plist-get args :request-context)
            :turn-id (plist-get args :turn-id)
            :abort-controller (or (plist-get args :abort-controller)
                                  (magent-agent-loop-abort-controller-create))
            :tool-queue (or (plist-get args :tool-queue)
                            (magent-agent-loop-tool-queue-create))
            :status (or (plist-get args :status) 'created)
            :text-chunks nil
            :reasoning-chunks nil
            :tool-calls nil
            :metadata (plist-get args :metadata))))
      (puthash loop (plist-get args :event-context)
               magent-agent-loop--event-contexts)
      (puthash loop
               (if (plist-get args :owns-event-context-p)
                   'owned
                 'inherited)
               magent-agent-loop--context-ownership)
      loop)))

(defun magent-agent-loop-text (loop)
  "Return accumulated assistant text for LOOP."
  (apply #'concat (nreverse (copy-sequence
                             (magent-agent-loop-text-chunks loop)))))

(defun magent-agent-loop-sample-text (loop)
  "Return assistant text accumulated for LOOP's current provider sample."
  (apply #'concat
         (nreverse
          (copy-sequence
           (gethash loop magent-agent-loop--sample-text-chunks)))))

(defun magent-agent-loop-begin-sample (loop)
  "Start a new provider sample for LOOP.
Turn-wide text remains available through \`magent-agent-loop-text', while the
terminal result is reset to the text produced by the new sample."
  (remhash loop magent-agent-loop--sample-text-chunks)
  (setf (magent-agent-loop-result loop) nil)
  loop)

(defun magent-agent-loop-discard-sample-text (loop)
  "Discard LOOP's current sample text from its turn transcript.
This is used when textual tool syntax was streamed as assistant text before
being normalized into structured tool calls."
  (let ((count (length
                (gethash loop magent-agent-loop--sample-text-chunks))))
    (when (> count 0)
      (setf (magent-agent-loop-text-chunks loop)
            (nthcdr count (magent-agent-loop-text-chunks loop)))))
  (remhash loop magent-agent-loop--sample-text-chunks)
  (setf (magent-agent-loop-result loop) nil)
  loop)

(defun magent-agent-loop-reasoning (loop)
  "Return accumulated reasoning text for LOOP."
  (apply #'concat (nreverse (copy-sequence
                             (magent-agent-loop-reasoning-chunks loop)))))

(defun magent-agent-loop--combined-result (streamed final)
  "Return a final assistant result from STREAMED chunks and FINAL text."
  (cond
   ((or (null streamed) (string-empty-p streamed))
    (or final ""))
   ((or (null final) (string-empty-p final))
    streamed)
   ((string= streamed final)
    streamed)
   ((string-prefix-p streamed final)
    final)
   ((string-suffix-p final streamed)
    streamed)
   (t
    (concat streamed final))))

(defun magent-agent-loop-apply-event (loop event)
  "Apply normalized LLM EVENT to LOOP and return LOOP."
  (unless (magent-agent-loop-p loop)
    (error "Expected magent-agent-loop, got: %S" loop))
  (unless (magent-llm-event-p event)
    (error "Expected magent-llm-event, got: %S" event))
  (pcase (magent-llm-event-type event)
    ('text-delta
     (let ((text (or (magent-llm-event-text event) "")))
       (push text (magent-agent-loop-text-chunks loop))
       (puthash loop
                (cons text
                      (gethash loop magent-agent-loop--sample-text-chunks))
                magent-agent-loop--sample-text-chunks))
     (setf (magent-agent-loop-status loop) 'streaming))
    ('reasoning-delta
     (push (or (magent-llm-event-text event) "")
           (magent-agent-loop-reasoning-chunks loop))
     (setf (magent-agent-loop-status loop) 'reasoning))
    ('reasoning-end
     (setf (magent-agent-loop-status loop) 'streaming))
    ('tool-call
     (push event (magent-agent-loop-tool-calls loop))
     (setf (magent-agent-loop-status loop) 'tool-pending))
    ('tool-call-batch-end
     (setf (magent-agent-loop-status loop) 'tool-pending)
     (magent-agent-loop-set-tool-continuation
      loop (magent-llm-event-continuation event)))
    ('completed
     (setf (magent-agent-loop-status loop) 'completed
           (magent-agent-loop-result loop)
           (let ((result (magent-agent-loop--combined-result
                          (magent-agent-loop-sample-text loop)
                          (magent-llm-event-text event))))
             result)
           (magent-agent-loop-usage loop)
           (magent-llm-event-usage event)
           (magent-agent-loop-stop-reason loop)
           (magent-llm-event-stop-reason event)))
    ('error
     (setf (magent-agent-loop-status loop) 'failed
           (magent-agent-loop-error loop)
           (magent-llm-event-message event)))
    ('usage
     (setf (magent-agent-loop-usage loop)
           (magent-llm-event-usage event))))
  loop)

(defun magent-agent-loop-transcript (loop)
  "Return LOOP's complete assistant transcript, including terminal text."
  (magent-agent-loop--combined-result
   (magent-agent-loop-text loop)
   (magent-agent-loop-result loop)))

(defun magent-agent-loop--tool-name (tool-spec raw-call)
  "Return tool name from TOOL-SPEC or RAW-CALL."
  (or (and tool-spec
           (fboundp 'gptel-tool-name)
           (ignore-errors (gptel-tool-name tool-spec)))
      (plist-get raw-call :name)
      "unknown"))

(defun magent-agent-loop-tool-call-summary (name args)
  "Format tool call NAME with ARGS into a concise display summary."
  (let* ((reason (plist-get args :reason))
         (base
          (cond
           ((string= name "skill_invoke")
            (format "%s/%s"
                    (or (plist-get args :skill_name) "?")
                    (or (plist-get args :operation) "?")))
           ((string= name "spawn_agent")
            (format "%s: %s"
                    (or (plist-get args :agent) "?")
                    (or (plist-get args :task_name)
                        (plist-get args :prompt)
                        "?")))
           ((member name '("send_agent_message" "wait_agent" "close_agent"))
            (or (plist-get args :job_id) "?"))
           ((string= name "list_agents")
            "child jobs")
           ((string= name "bash")
            (if-let* ((cmd (plist-get args :command)))
                (truncate-string-to-width
                 cmd magent-agent-loop--tool-input-summary-max-length
                 nil nil "...")
              "?"))
           ((member name '("read_file" "write_file" "edit_file"))
            (or (plist-get args :path) "?"))
           ((string= name "write_repo_summary")
            (or (plist-get args :scope)
                (plist-get args :mode)
                "summary"))
           ((string= name "grep")
            (format "%s in %s"
                    (or (plist-get args :pattern) "?")
                    (or (plist-get args :path) "?")))
           ((string= name "glob")
            (or (plist-get args :pattern) "?"))
           ((string= name "emacs_eval")
            (if-let* ((sexp (plist-get args :sexp)))
                (truncate-string-to-width
                 sexp magent-agent-loop--tool-input-summary-max-length
                 nil nil "...")
              "?"))
           (t
            (if args
                (truncate-string-to-width
                 (format "%s" args)
                 magent-agent-loop--tool-input-summary-max-length
                 nil nil "...")
              "")))))
    (if (and reason (not (string-empty-p reason)))
        (format "[%s] %s" reason base)
      base)))

(defun magent-agent-loop-tool-result-summary (result)
  "Return display summary for tool RESULT."
  (let ((result-str (magent-tool-result-output-string result)))
    (if (> (length result-str)
           magent-agent-loop--tool-result-summary-max-length)
        (format "%s... [%d bytes]"
                (substring
                 result-str 0
                 magent-agent-loop--tool-result-summary-preview-length)
                (length result-str))
      result-str)))

(defun magent-agent-loop-filter-display-args (args-spec arg-values)
  "Remove display-only args from ARG-VALUES using ARGS-SPEC."
  (if (cl-find "reason" args-spec
               :key (lambda (spec) (plist-get spec :name))
               :test #'string=)
      (cl-loop for spec in args-spec
               for value in arg-values
               unless (string= (plist-get spec :name) "reason")
               collect value)
    arg-values))

(defun magent-agent-loop--ui-visible-p (request-context)
  "Return non-nil when REQUEST-CONTEXT should render tool UI."
  (or (null request-context)
      (magent-request-context-ui-visible-p request-context)))

(defun magent-agent-loop--event-context (request-context)
  "Return event context from REQUEST-CONTEXT, if present."
  (and request-context
       (magent-request-context-event-context request-context)))

(defun magent-agent-loop--aborted-p (loop)
  "Return non-nil when LOOP has been aborted."
  (let ((controller (and (magent-agent-loop-p loop)
                         (magent-agent-loop-abort-controller loop))))
    (and controller
         (magent-agent-loop-abort-controller-aborted controller))))

(defun magent-agent-loop--resource-identity-error
    (identity request-context fn-args)
  "Return a failed tool result when frozen resource IDENTITY changed.
The check runs at the actual dequeue point.  It closes deterministic races
across Magent's own serial tool queue without claiming OS-level isolation."
  (when identity
    (let* ((index (plist-get identity :file-arg-index))
           (expected (plist-get identity :canonical-resource))
           (value (and (integerp index) (nth index fn-args)))
           (project-root
            (or (and request-context
                     (magent-request-context-project-root request-context))
                (and request-context
                     (let ((scope (magent-request-context-scope request-context)))
                       (and (stringp scope) scope)))))
           (actual
            (condition-case nil
                (magent-tools-canonical-resource-path value project-root)
              (error nil))))
      (unless (and (stringp expected) (equal actual expected))
        (magent-tool-result-create
         :status 'failed
         :success nil
         :output "Error: resource identity changed after permission evaluation"
         :error "Resource identity changed after permission evaluation"
         :metadata (list :resource-identity-changed t))))))

(defun magent-agent-loop-tool-queue-abort (queue)
  "Abort QUEUE, discarding pending tool items."
  (when queue
    (setf (magent-agent-loop-tool-queue-aborted queue) t
          (magent-agent-loop-tool-queue-items queue) nil
          (magent-agent-loop-tool-queue-busy queue) nil)))

(defun magent-agent-loop-tool-queue-push (queue item)
  "Push ITEM onto QUEUE and start processing if idle."
  (unless (magent-agent-loop-tool-queue-aborted queue)
    (setf (magent-agent-loop-tool-queue-items queue)
          (nconc (magent-agent-loop-tool-queue-items queue) (list item)))
    (magent-agent-loop-tool-queue-run queue)))

(defun magent-agent-loop--execute-tool-item (queue item)
  "Execute one queued tool ITEM from QUEUE."
  (let ((abort-controller (plist-get item :abort-controller)))
    (unless (and abort-controller
                 (magent-agent-loop-abort-controller-aborted
                  abort-controller))
      (setf (magent-agent-loop-tool-queue-busy queue) t)
      (let ((request-context (plist-get item :request-context))
            (name (plist-get item :name))
            (call-id (plist-get item :call-id))
            (summary (plist-get item :summary))
            (args-plist (plist-get item :args-plist))
            (fn (plist-get item :fn))
            (async-p (plist-get item :async))
            (structured-result-p (plist-get item :structured-result-p))
            (resource-identity (plist-get item :resource-identity))
            (fn-args (plist-get item :args))
            (callback (plist-get item :callback)))
        (magent-lifecycle-events-emit 'tool-call-start
                            :context
                            (magent-agent-loop--event-context request-context)
                            :audit-context
                            (magent-request-context-audit-snapshot request-context)
                            :call-id call-id
                            :tool-name name
                            :title name
                            :summary summary
                            :description summary
                            :args args-plist
                            :ui-visible
                            (magent-agent-loop--ui-visible-p request-context))
        (magent-request-context-notify
         request-context 'tool-call-start
         :tool-id call-id
         :name name
         :kind (magent-tools-permission-key name)
         :title name
         :summary summary
         :input args-plist
         :raw-input args-plist)
        (cl-labels
            ((complete
              (result)
              (let* ((result (magent-tool-result-normalize result name call-id))
                     (status (magent-tool-result-status-value result))
                     (output (magent-tool-result-output-string result)))
                (setf (magent-agent-loop-tool-queue-busy queue) nil)
                (unless (or (magent-agent-loop-tool-queue-aborted queue)
                            (and abort-controller
                                 (magent-agent-loop-abort-controller-aborted
                                  abort-controller)))
                  (magent-lifecycle-events-emit
                   'tool-call-end
                   :context
                   (magent-agent-loop--event-context request-context)
                   :audit-context
                   (magent-request-context-audit-snapshot request-context)
                   :call-id call-id
                   :tool-name name
                   :status status
                   :exit-code (magent-tool-result-exit-code result)
                   :result output
                   :result-summary
                   (magent-agent-loop-tool-result-summary result)
                   :ui-visible
                   (magent-agent-loop--ui-visible-p request-context))
                  (magent-request-context-notify
                   request-context 'tool-call-complete
                   :tool-id call-id
                   :name name
                   :status (if (eq status 'completed) 'completed 'failed)
                   :output output
                   :exit-code (magent-tool-result-exit-code result)
                   :output-preview
                   (magent-agent-loop-tool-result-summary result))
                  (funcall callback (if structured-result-p result output))
                  (magent-agent-loop-tool-queue-run queue)))))
          (let ((magent-tools--register-cancel
                 (lambda (cleanup)
                   (magent-agent-loop-abort-controller-register
                    abort-controller cleanup)))
                (magent-tools--request-context request-context))
            (if-let* ((identity-error
                       (magent-agent-loop--resource-identity-error
                        resource-identity request-context fn-args)))
                (complete identity-error)
              (if async-p
                  (condition-case err
                      (apply fn #'complete fn-args)
                    (quit
                     (complete "Error: Tool execution interrupted"))
                    (error
                     (complete
                      (format "Error: Tool execution failed: %s"
                              (error-message-string err)))))
                (complete
                 (condition-case err
                     (apply fn fn-args)
                   (quit "Error: Tool execution interrupted")
                   (error
                    (concat "Error: Tool execution failed: "
                            (error-message-string err)))))))))))))

(defun magent-agent-loop-tool-queue-run (queue)
  "Process the next queued tool in QUEUE when idle."
  (unless (or (magent-agent-loop-tool-queue-aborted queue)
              (magent-agent-loop-tool-queue-busy queue))
    (when-let* ((item (pop (magent-agent-loop-tool-queue-items queue))))
      (magent-agent-loop--execute-tool-item queue item))))

(defun magent-agent-loop-run-tool
    (loop request-context tool-spec callback arg-values
          &optional structured-result-p resource-identity)
  "Run TOOL-SPEC with ARG-VALUES and call CALLBACK with the result.
When STRUCTURED-RESULT-P is non-nil, pass a `magent-tool-result'; otherwise
preserve the historical string callback contract."
  (let* ((args-spec (and (fboundp 'gptel-tool-args)
                         (gptel-tool-args tool-spec)))
         (name (magent-agent-loop--tool-name tool-spec nil))
         (args-plist (magent-agent-loop-args-to-plist args-spec arg-values))
         (call-id (magent-lifecycle-events-generate-id))
         (summary (magent-agent-loop-tool-call-summary name args-plist))
         (fn-args (magent-agent-loop-filter-display-args
                   args-spec arg-values))
         (fn (gptel-tool-function tool-spec))
         (async-p (and (fboundp 'gptel-tool-async)
                       (gptel-tool-async tool-spec))))
    (unless (magent-agent-loop--aborted-p loop)
      (magent-agent-loop-tool-queue-push
       (magent-agent-loop-tool-queue loop)
       (list :loop loop
             :request-context request-context
             :abort-controller (magent-agent-loop-abort-controller loop)
             :name name
             :call-id call-id
             :summary summary
             :args-plist args-plist
             :fn fn
             :async async-p
             :structured-result-p structured-result-p
             :resource-identity resource-identity
             :callback callback
             :args fn-args)))))

(defun magent-agent-loop--record-tool-start (loop call-id name args-plist)
  "Record a started tool item for LOOP."
  (when-let* ((session (magent-agent-loop-session loop))
              (thread (magent-session-thread-ledger session))
              (turn-id (magent-agent-loop--ensure-turn-id loop thread)))
    (magent-thread-start-item
     thread turn-id 'tool
     :id call-id
     :call-id call-id
     :name name
     :input args-plist
     :metadata (list :source 'tool-dispatch))
    (magent-session-refresh-projections session)))

(defun magent-agent-loop--tool-result-metadata (raw-call)
  "Return ledger metadata extracted from RAW-CALL."
  (let ((metadata (list :source 'tool-result)))
    (dolist (key '(:approval-decision :approval-source))
      (when (plist-member raw-call key)
        (setq metadata
              (append metadata (list key (plist-get raw-call key))))))
    metadata))

(defun magent-agent-loop--ensure-turn-id (loop thread)
  "Return LOOP's current turn id, creating a synthetic turn in THREAD if needed."
  (let ((turn-id (or (magent-agent-loop-turn-id loop)
                     (and (magent-agent-loop-request-context loop)
                          (magent-request-context-turn-id
                           (magent-agent-loop-request-context loop)))
                     (and (magent-thread-active-turn thread)
                          (magent-thread-turn-id
                           (magent-thread-active-turn thread))))))
    (unless turn-id
      (let ((turn (magent-thread-create-turn
                   thread nil nil (list :synthetic t
                                         :source 'agent-loop-tool))))
        (setq turn-id (magent-thread-turn-id turn))))
    (setf (magent-agent-loop-turn-id loop) turn-id)
    (when (magent-agent-loop-request-context loop)
      (setf (magent-request-context-turn-id
             (magent-agent-loop-request-context loop))
            turn-id))
    turn-id))

(defun magent-agent-loop-find-file-arg-index (args-spec)
  "Return the 0-based file argument index from ARGS-SPEC, if any."
  (cl-loop for spec in args-spec
           for index from 0
           when (member (plist-get spec :name) '("path" "file"))
           return index))

(defun magent-agent-loop-args-to-plist (args-spec arg-values)
  "Return plist args from ARGS-SPEC and positional ARG-VALUES."
  (if (magent-agent-loop--plist-args-p arg-values)
      arg-values
    (cl-loop for spec in args-spec
             for value in arg-values
             append (list (intern (concat ":" (plist-get spec :name)))
                          value))))

(defun magent-agent-loop-summarize-args (arg-values args-spec)
  "Return a concise summary for ARG-VALUES and ARGS-SPEC."
  (let* ((first-value (car arg-values))
         (first-name (when args-spec
                       (plist-get (car args-spec) :name)))
         (display (cond
                   ((null first-value) "")
                   ((stringp first-value)
                    (truncate-string-to-width
                     first-value 60 nil nil "..."))
                   (t (format "%s" first-value)))))
    (if (and first-name (not (string-empty-p display)))
        display
      "")))

(defun magent-agent-loop-audit-permission-decision
    (tool-spec arg-values decision source &optional request-context)
  "Record permission DECISION for TOOL-SPEC with ARG-VALUES."
  (when (fboundp 'magent-audit-record-permission-decision)
    (let* ((tool-name (magent-agent-loop--tool-name tool-spec nil))
           (perm-key (magent-tools-permission-key tool-name))
           (args-spec (and (fboundp 'gptel-tool-args)
                           (gptel-tool-args tool-spec)))
           (args-plist (magent-agent-loop-args-to-plist
                        args-spec arg-values))
           (summary (magent-agent-loop-tool-call-summary
                     tool-name args-plist)))
      (magent-audit-record-permission-decision
       tool-name perm-key decision source
       :context (magent-agent-loop--event-context request-context)
       :audit-context
       (magent-request-context-audit-snapshot request-context)
       :summary summary
       :args args-plist))))

(defun magent-agent-loop-tools-to-gptel (tools)
  "Convert Magent tool plist TOOLS to gptel-tool structs for provider schema."
  (mapcar
   (lambda (tool)
     (gptel-make-tool
      :name (plist-get tool :name)
      :description (plist-get tool :description)
      :args (magent-agent-loop--json-safe-tool-args
             (plist-get tool :args))
      :function (plist-get tool :function)
      :async (plist-get tool :async)
      :category "magent"))
   tools))

(defun magent-agent-loop--json-safe-tool-args (args)
  "Return ARGS with schema values acceptable to `json-serialize'."
  (mapcar (lambda (spec)
            (if (listp spec)
                (magent-json-safe-value (copy-sequence spec))
              spec))
          args))

(defun magent-agent-loop-create-orchestrator
    (loop permission request-context)
  "Create a tool orchestrator for LOOP, PERMISSION, and REQUEST-CONTEXT."
  (magent-tool-orchestrator-create
   :permission permission
   :request-context request-context
   :run-tool-function (lambda (tool-spec callback arg-values
                               &optional resource-identity)
                        (magent-agent-loop-run-tool
                         loop request-context tool-spec callback arg-values t
                         resource-identity))
   :audit-function #'magent-agent-loop-audit-permission-decision
   :file-arg-index-function #'magent-agent-loop-find-file-arg-index
   :args-to-plist-function #'magent-agent-loop-args-to-plist
   :summarize-function #'magent-agent-loop-summarize-args))

(defun magent-agent-loop-record-tool-result
    (loop tool-spec arg-values raw-call result)
  "Record TOOL-SPEC RESULT for LOOP's session and return LOOP.
ARG-VALUES are stored as the model-visible tool arguments.  RAW-CALL can
carry provider-specific ids and names."
  (when-let* ((session (magent-agent-loop-session loop)))
    (let* ((thread (magent-session-thread-ledger session))
           (turn-id (magent-agent-loop--ensure-turn-id loop thread))
           (call-id (or (plist-get raw-call :id)
                        (plist-get raw-call :call-id)
                        (and (fboundp 'magent-lifecycle-events-generate-id)
                             (magent-lifecycle-events-generate-id))
                        "tool-call")))
      (magent-thread-record-tool-result
       thread
       turn-id
       call-id
       (magent-agent-loop--tool-name tool-spec raw-call)
       (magent-agent-loop--tool-args-plist tool-spec arg-values raw-call)
       result
       (magent-agent-loop--tool-result-metadata raw-call))
      (magent-session-refresh-projections session)))
  loop)

(defun magent-agent-loop--tool-args (event)
  "Return tool arguments from normalized tool-call EVENT."
  (or (magent-llm-event-arguments event)
      (plist-get (magent-llm-event-raw event) :args)))

(defun magent-agent-loop--tool-raw-call (event)
  "Return raw provider call from normalized tool-call EVENT."
  (or (magent-llm-event-raw event)
      (list :id (magent-llm-event-id event)
            :name (magent-llm-event-name event)
            :args (magent-agent-loop--tool-args event))))

(defun magent-agent-loop--find-tool (loop name)
  "Return tool named NAME from LOOP's request tools."
  (cl-find-if
   (lambda (tool)
     (equal (magent-agent-loop--tool-name tool nil) name))
   (magent-llm-request-tools (magent-agent-loop-request loop))))

(defun magent-agent-loop--plist-args-p (args)
  "Return non-nil when ARGS looks like an Elisp plist."
  (or (null args)
      (and (listp args)
           (keywordp (car args)))))

(defun magent-agent-loop--signal-invalid-tool-arguments
    (format-string &rest args)
  "Signal invalid textual tool arguments using FORMAT-STRING and ARGS."
  (signal 'magent-agent-loop-invalid-tool-arguments
          (list (apply #'format format-string args))))

(defun magent-agent-loop--tool-arg-key (spec)
  "Return the keyword argument key described by tool argument SPEC."
  (intern (concat ":" (magent-json-safe-name (plist-get spec :name)))))

(defun magent-agent-loop--parse-textual-json-argument (name type value)
  "Parse textual JSON VALUE for argument NAME with schema TYPE."
  (unless (stringp value)
    (magent-agent-loop--signal-invalid-tool-arguments
     "argument '%s' must be a JSON %s (got %S)" name type value))
  (condition-case err
      (json-parse-string value
                         :object-type 'plist
                         :array-type 'array
                         :null-object :null
                         :false-object :json-false)
    (error
     (magent-agent-loop--signal-invalid-tool-arguments
      "argument '%s' must be a JSON %s (got %S): %s"
      name type value (error-message-string err)))))

(defun magent-agent-loop--coerce-textual-tool-value (spec value)
  "Coerce textual tool VALUE according to argument SPEC."
  (let* ((name (magent-json-safe-name (plist-get spec :name)))
         (type (magent-json-safe-name (plist-get spec :type))))
    (pcase type
      ("string"
       (unless (stringp value)
         (magent-agent-loop--signal-invalid-tool-arguments
          "argument '%s' must be a string (got %S)" name value))
       value)
      ("integer"
       (cond
        ((integerp value) value)
        ((and (stringp value)
              (string-match-p "\\`[-+]?[0-9]+\\'" value))
         (string-to-number value))
        (t
         (magent-agent-loop--signal-invalid-tool-arguments
          "argument '%s' must be an integer (got %S)" name value))))
      ("number"
       (cond
        ((numberp value) value)
        ((and (stringp value)
              (string-match-p
               "\\`-?\\(?:0\\|[1-9][0-9]*\\)\\(?:\\.[0-9]+\\)?\\(?:[eE][-+]?[0-9]+\\)?\\'"
               value))
         (string-to-number value))
        (t
         (magent-agent-loop--signal-invalid-tool-arguments
          "argument '%s' must be a number (got %S)" name value))))
      ("boolean"
       (cond
        ((eq value t) t)
        ((or (null value) (eq value :json-false)) :json-false)
        ((and (stringp value) (string-equal (downcase value) "true")) t)
        ((and (stringp value) (string-equal (downcase value) "false"))
         :json-false)
        (t
         (magent-agent-loop--signal-invalid-tool-arguments
          "argument '%s' must be a boolean (got %S)" name value))))
      ("array"
       (let ((parsed
              (if (vectorp value)
                  value
                (magent-agent-loop--parse-textual-json-argument
                 name type value))))
         (unless (vectorp parsed)
           (magent-agent-loop--signal-invalid-tool-arguments
            "argument '%s' must be a JSON array (got %S)" name value))
         parsed))
      ("object"
       (let ((parsed
              (if (and (listp value) (magent-json--plist-p value))
                  value
                (magent-agent-loop--parse-textual-json-argument
                 name type value))))
         (unless (and (listp parsed) (magent-json--plist-p parsed))
           (magent-agent-loop--signal-invalid-tool-arguments
            "argument '%s' must be a JSON object (got %S)" name value))
         parsed))
      ("null"
       (if (or (eq value :null)
               (and (stringp value) (string-equal value "null")))
           :null
         (magent-agent-loop--signal-invalid-tool-arguments
          "argument '%s' must be null (got %S)" name value)))
      (_ value))))

(defun magent-agent-loop--normalize-textual-tool-args (tool-spec args)
  "Normalize textual DSML ARGS according to TOOL-SPEC's argument schema."
  (let* ((args-spec (and (fboundp 'gptel-tool-args)
                         (gptel-tool-args tool-spec)))
         (tool-name (magent-agent-loop--tool-name tool-spec nil))
         (proper-plist-p
          (and (listp args)
               (zerop (% (length args) 2))
               (cl-loop for (key _value) on args by #'cddr
                        always (keywordp key))))
         (known-keys (mapcar #'magent-agent-loop--tool-arg-key args-spec)))
    (unless proper-plist-p
      (magent-agent-loop--signal-invalid-tool-arguments
       "tool '%s' returned malformed textual arguments: %S" tool-name args))
    (let ((unknown
           (cl-loop for (key _value) on args by #'cddr
                    unless (memq key known-keys)
                    collect (substring (symbol-name key) 1))))
      (when unknown
        (magent-agent-loop--signal-invalid-tool-arguments
         "tool '%s' received unknown argument%s: %s; available arguments: %s"
         tool-name
         (if (= (length unknown) 1) "" "s")
         (mapconcat #'identity unknown ", ")
         (mapconcat (lambda (key) (substring (symbol-name key) 1))
                    known-keys ", "))))
    (let (normalized)
      (dolist (spec args-spec)
        (let ((key (magent-agent-loop--tool-arg-key spec)))
          (if (plist-member args key)
              (setq normalized
                    (append normalized
                            (list key
                                  (magent-agent-loop--coerce-textual-tool-value
                                   spec (plist-get args key)))))
            (unless (plist-get spec :optional)
              (magent-agent-loop--signal-invalid-tool-arguments
               "tool '%s' is missing required argument '%s'"
               tool-name (substring (symbol-name key) 1))))))
      normalized)))

(defun magent-agent-loop--tool-arg-values
    (tool-spec args &optional textual-dsml-p)
  "Return positional tool arg values for TOOL-SPEC from ARGS.
When TEXTUAL-DSML-P is non-nil, convert the JSON false sentinel to nil for the
Elisp tool function after preserving it in the normalized raw arguments."
  (if (and tool-spec
           (magent-agent-loop--plist-args-p args)
           (fboundp 'gptel-tool-args))
      (mapcar
       (lambda (spec)
         (let ((value
                (plist-get args
                           (intern (concat ":" (plist-get spec :name))))))
           (unless (or (eq value :null)
                       (and textual-dsml-p (eq value :json-false)))
             value)))
       (gptel-tool-args tool-spec))
    args))

(defun magent-agent-loop--tool-args-plist (tool-spec arg-values raw-call)
  "Return model-visible plist args for TOOL-SPEC and ARG-VALUES."
  (cond
   ((magent-agent-loop--plist-args-p arg-values)
    (magent-json-safe-tool-args arg-values))
   ((and tool-spec
         (listp arg-values)
         (fboundp 'gptel-tool-args))
    (let ((raw-args (plist-get raw-call :args)))
      (magent-json-safe-tool-args
       (if (and (plist-member raw-call :args)
                (magent-agent-loop--plist-args-p raw-args))
           raw-args
         (cl-loop for spec in (gptel-tool-args tool-spec)
                  for value in arg-values
                  unless (or (null value)
                             (eq value :null))
                  append (list (intern (concat ":" (plist-get spec :name)))
                               value))))))
   (t
    (magent-json-safe-tool-args
     (or (plist-get raw-call :args)
         arg-values)))))

(defun magent-agent-loop-tool-event-to-call (loop event)
  "Convert normalized tool-call EVENT to orchestrator call shape.
Return `(TOOL-SPEC ARG-VALUES nil RAW-CALL)' or nil when the tool is not
available in LOOP's request tools."
  (let* ((name (magent-llm-event-name event))
         (tool-spec (magent-agent-loop--find-tool loop name)))
    (when tool-spec
      (let* ((raw-call (magent-agent-loop--tool-raw-call event))
             (textual-dsml-p
              (member (plist-get raw-call :source)
                      '(textual-dsml "textual-dsml")))
             (args (magent-agent-loop--tool-args event)))
        (unless (or (plist-get raw-call :id)
                    (plist-get raw-call :call-id))
          (plist-put raw-call :id (magent-lifecycle-events-generate-id)))
        (when textual-dsml-p
          (setq args
                (magent-agent-loop--normalize-textual-tool-args
                 tool-spec args))
          (plist-put raw-call :args args))
        (list tool-spec
              (magent-agent-loop--tool-arg-values
               tool-spec args textual-dsml-p)
              (magent-llm-event-result-callback event)
              raw-call)))))

(defun magent-agent-loop--invalid-tool-arguments-result (loop event err)
  "Record and return a model-visible invalid argument result for EVENT and ERR."
  (let* ((raw-call (magent-agent-loop--tool-raw-call event))
         (error-message (format "Error: %s" (error-message-string err))))
    (magent-agent-loop-record-tool-result
     loop nil (magent-agent-loop--tool-args event) raw-call error-message)
    (when-let* ((result-callback (magent-llm-event-result-callback event)))
      (funcall result-callback error-message))
    error-message))

(defun magent-agent-loop--unknown-tool-result (loop event known-tool-names)
  "Record and return an unknown-tool result for EVENT."
  (let* ((raw-call (magent-agent-loop--tool-raw-call event))
         (name (or (magent-llm-event-name event)
                   (plist-get raw-call :name)
                   "unknown"))
         (error-message
          (format "Error: tool '%s' not found. Available: %s"
                  name
                  (mapconcat #'identity known-tool-names ", "))))
    (magent-agent-loop-record-tool-result
     loop nil (magent-agent-loop--tool-args event) raw-call error-message)
    (when-let* ((result-callback (magent-llm-event-result-callback event)))
      (funcall result-callback error-message))
    error-message))

(defun magent-agent-loop--tool-dispatch-outcome
    (reason status &optional result)
  "Return a plist describing a tool dispatch continuation outcome."
  (list :needs-follow-up t
        :reason reason
        :status status
        :result result))

(defun magent-agent-loop-dispatch-tool-calls
    (loop orchestrator &optional done-callback)
  "Dispatch LOOP's pending tool calls through ORCHESTRATOR.
Unknown tools are recorded as tool-result errors.  Known tool calls are
passed to `magent-tool-orchestrator-handle-tool-calls'.  DONE-CALLBACK
receives a plist describing why the turn needs a follow-up sampling
request after model-visible tool output has been recorded."
  (unless (magent-agent-loop-p loop)
    (error "Expected magent-agent-loop, got: %S" loop))
  (unless (magent-tool-orchestrator-p orchestrator)
    (error "Expected magent-tool-orchestrator, got: %S" orchestrator))
  (let* ((events (nreverse (copy-sequence
                            (magent-agent-loop-tool-calls loop))))
         (tools (magent-llm-request-tools (magent-agent-loop-request loop)))
         (known-tool-names
          (delq nil
                (mapcar (lambda (tool)
                          (magent-agent-loop--tool-name tool nil))
                        tools)))
         known-calls
         failed-results)
    (setf (magent-agent-loop-tool-calls loop) nil)
    (dolist (event events)
      (condition-case err
          (if-let* ((call (magent-agent-loop-tool-event-to-call loop event)))
              (progn
                (magent-agent-loop--record-tool-start
                 loop
                 (or (plist-get (nth 3 call) :id)
                     (plist-get (nth 3 call) :call-id))
                 (magent-agent-loop--tool-name (car call) (nth 3 call))
                 (magent-agent-loop--tool-args-plist
                  (car call) (cadr call) (nth 3 call)))
                (push call known-calls))
            (push (magent-agent-loop--unknown-tool-result
                   loop event known-tool-names)
                  failed-results))
        (magent-agent-loop-invalid-tool-arguments
         (push (magent-agent-loop--invalid-tool-arguments-result
                loop event err)
               failed-results))))
    (setq known-calls (nreverse known-calls))
    (if known-calls
        (let ((base-result-callback
               (magent-tool-orchestrator-result-callback orchestrator))
              (base-done-callback
               (magent-tool-orchestrator-done-callback orchestrator)))
          (setf (magent-tool-orchestrator-result-callback orchestrator)
                (lambda (tool-spec arg-values raw-call result)
                  (magent-agent-loop-record-tool-result
                   loop tool-spec arg-values raw-call result)
                  (when base-result-callback
                    (funcall base-result-callback
                             tool-spec arg-values raw-call result))))
          (setf (magent-tool-orchestrator-done-callback orchestrator)
                (lambda ()
                  (when base-done-callback
                    (funcall base-done-callback))
                  (when done-callback
                    (funcall
                     done-callback
                     (magent-agent-loop--tool-dispatch-outcome
                      'tool-output
                      (if failed-results 'failed 'completed)
                      (car failed-results))))))
          (magent-tool-orchestrator-handle-tool-calls
           orchestrator known-calls))
      (when done-callback
        (funcall
         done-callback
         (magent-agent-loop--tool-dispatch-outcome
          'tool-output
          (if failed-results 'failed 'completed)
          (car failed-results))))))
  loop)

(defun magent-agent-loop-request-for-current-session (loop)
  "Return a request for LOOP using the latest session transcript.
The returned request preserves the existing request's system, tools,
model, backend, stream flag, callback, and metadata.  When LOOP has no
session, the existing request prompt is reused."
  (unless (magent-agent-loop-p loop)
    (error "Expected magent-agent-loop, got: %S" loop))
  (let ((request (magent-agent-loop-request loop)))
    (unless (magent-llm-request-p request)
      (error "Agent loop requires a magent-llm-request"))
    (magent-llm-request-create
     :prompt (if-let* ((session (magent-agent-loop-session loop)))
                 (magent-session-to-gptel-prompt-list
                  session
                  (magent-agent-loop-turn-id loop))
               (magent-llm-request-prompt request))
     :system (magent-llm-request-system request)
     :tools (magent-llm-request-tools request)
     :model (magent-llm-request-model request)
     :backend (magent-llm-request-backend request)
     :stream (magent-llm-request-stream request)
     :callback (magent-llm-request-callback request)
     :metadata (magent-llm-request-metadata request))))

(defun magent-agent-loop-continue (loop)
  "Continue LOOP with a new sampling request from current session state."
  (setf (magent-agent-loop-request loop)
        (magent-agent-loop-request-for-current-session loop))
  (magent-agent-loop-start loop))

(defun magent-agent-loop--abort-request-handle (handle)
  "Abort provider request HANDLE returned by the sampler."
  (when (and (fboundp 'gptel-abort)
             (bufferp handle)
             (buffer-live-p handle))
    (with-demoted-errors "Magent gptel abort error: %S"
      (gptel-abort handle))
    (when (buffer-live-p handle)
      (kill-buffer handle))))

(defun magent-agent-loop--cancel-request-timeout (loop)
  "Cancel LOOP's active provider request timeout timer."
  (when-let* ((timer (magent-agent-loop-request-timeout-timer loop)))
    (cancel-timer timer)
    (setf (magent-agent-loop-request-timeout-timer loop) nil)))

(defun magent-agent-loop--request-active-p (loop)
  "Return non-nil when LOOP is still waiting on a provider response."
  (memq (magent-agent-loop-status loop)
        '(running streaming reasoning tool-pending)))

(defun magent-agent-loop--schedule-request-timeout
    (loop original-callback)
  "Schedule provider inactivity timeout for LOOP.
ORIGINAL-CALLBACK receives a normalized timeout error event when the
provider request has emitted no event for `magent-request-timeout' seconds."
  (magent-agent-loop--cancel-request-timeout loop)
  (when (and (numberp magent-request-timeout)
             (> magent-request-timeout 0)
             (magent-agent-loop--request-active-p loop))
    (let ((timer
           (run-at-time
            magent-request-timeout nil
            (lambda ()
              (when (and (magent-agent-loop-p loop)
                         (magent-agent-loop--request-active-p loop)
                         (not (magent-agent-loop--aborted-p loop)))
                (let ((timeout-message
                       (format "Request timed out after %s seconds"
                               magent-request-timeout)))
                  (setf (magent-agent-loop-request-timeout-timer loop)
                        nil)
                  (magent-agent-loop-abort-controller-abort
                   (magent-agent-loop-abort-controller loop))
                  (magent-agent-loop--abort-request-handle
                   (magent-agent-loop-request-handle loop))
                  (let ((event (magent-llm-error-event
                                timeout-message
                                (list :status 'timeout
                                      :timeout magent-request-timeout))))
                    (magent-agent-loop-apply-event loop event)
                    (when original-callback
                      (funcall original-callback event)))))))))
      (setf (magent-agent-loop-request-timeout-timer loop) timer))))

(defun magent-agent-loop-abort (loop)
  "Abort LOOP, its pending tools, and its active provider request."
  (when (magent-agent-loop-p loop)
    (let ((already-aborted (magent-agent-loop--aborted-p loop)))
      (magent-agent-loop--cancel-request-timeout loop)
      (setf (magent-agent-loop-status loop) 'cancelled)
      (magent-agent-loop-tool-queue-abort
       (magent-agent-loop-tool-queue loop))
      (magent-agent-loop-abort-controller-abort
       (magent-agent-loop-abort-controller loop))
      (magent-agent-loop--abort-request-handle
       (magent-agent-loop-request-handle loop))
      (when-let* ((session (magent-agent-loop-session loop))
                  (thread (magent-session-thread-ledger session))
                  (turn-id (magent-agent-loop-turn-id loop)))
        (let ((changed nil))
          (when (> (magent-thread-cancel-in-progress-items
                    thread turn-id "User aborted")
                   0)
            (setq changed t))
          (when-let* ((turn (magent-thread-find-turn thread turn-id)))
            (unless (magent-thread-terminal-turn-p turn)
              (magent-thread-interrupt-turn thread turn-id "User aborted")
              (setq changed t)))
          (when changed
            (magent-session-refresh-projections session)
            (magent-session-save-deferred-for-session
             session
             (when-let* ((request-context
                          (magent-agent-loop-request-context loop)))
               (magent-request-context-scope request-context))))))
      (when-let* ((request-context (magent-agent-loop-request-context loop)))
        (when (eq (magent-request-context-abort-controller request-context)
                  (magent-agent-loop-abort-controller loop))
          (setf (magent-request-context-abort-controller request-context)
                nil)))
      (unless already-aborted
        (when (magent-agent-loop-owns-event-context-p loop)
          (when-let* ((context
                       (or (magent-agent-loop-event-context loop)
                           (magent-agent-loop--event-context
                            (magent-agent-loop-request-context loop)))))
            (magent-lifecycle-events-end-turn
             context 'cancelled "User aborted"))))))
  loop)

(defun magent-agent-loop-start (loop)
  "Start LOOP by invoking its sampler.
The request callback is wrapped so normalized events are accumulated in
LOOP before being forwarded to the original request callback.  Return
the sampler return value."
  (unless (magent-agent-loop-p loop)
    (error "Expected magent-agent-loop, got: %S" loop))
  (let* ((request (magent-agent-loop-request loop))
         (sampler (magent-agent-loop-sampler loop))
         (original-callback (and request
                                 (magent-llm-request-callback request)))
         (terminal-event-seen nil))
    (unless (magent-llm-request-p request)
      (error "Agent loop requires a magent-llm-request"))
    (unless sampler
      (error "Agent loop requires a sampler"))
    (setf (magent-agent-loop-status loop) 'running)
    (when-let* ((request-context (magent-agent-loop-request-context loop)))
      (setf (magent-request-context-abort-controller request-context)
            (magent-agent-loop-abort-controller loop))
      (unless (magent-agent-loop-turn-id loop)
        (setf (magent-agent-loop-turn-id loop)
              (magent-request-context-turn-id request-context))))
    (let ((loop-request
           (magent-llm-request-create
            :prompt (magent-llm-request-prompt request)
            :system (magent-llm-request-system request)
            :tools (magent-llm-request-tools request)
            :model (magent-llm-request-model request)
            :backend (magent-llm-request-backend request)
            :stream (magent-llm-request-stream request)
            :metadata (magent-llm-request-metadata request)
            :callback (lambda (event)
                        (unless (magent-agent-loop--aborted-p loop)
                          (when-let* (((eq (magent-llm-event-type event)
                                           'tool-call-batch-end))
                                      (continuation
                                       (magent-llm-event-continuation event)))
                            (magent-llm-event-set-continuation
                             event
                             (lambda (&rest args)
                               (setq terminal-event-seen nil)
                               (setf (magent-agent-loop-status loop) 'running)
                               (magent-agent-loop--schedule-request-timeout
                                loop original-callback)
                               (apply continuation args))))
                          (if (memq (magent-llm-event-type event)
                                    '(tool-call-batch-end completed error))
                              (progn
                                (setq terminal-event-seen t)
                                (magent-agent-loop--cancel-request-timeout loop))
                            ;; Every normalized provider event proves forward
                            ;; progress.  Restart the inactivity window before
                            ;; applying the event, while LOOP is still active.
                            (unless terminal-event-seen
                              (magent-agent-loop--schedule-request-timeout
                               loop original-callback)))
                          (magent-agent-loop-apply-event loop event)
                          (when original-callback
                            (funcall original-callback event)))))))
      (let ((handle (funcall sampler loop-request)))
        (setf (magent-agent-loop-request-handle loop) handle)
        (unless terminal-event-seen
          (magent-agent-loop--schedule-request-timeout
           loop original-callback))
        handle))))

(provide 'magent-agent-loop)
;;; magent-agent-loop.el ends here
