;;; magent-agent-loop.el --- Magent-owned agent turn loop  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Magent-owned turn loop state.  This module consumes normalized
;; `magent-llm-event' values instead of raw gptel callback/FSM details,
;; and owns tool execution, continuation, and abort handling.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magent-config)
(require 'magent-events)
(require 'magent-llm)
(require 'magent-runtime)
(require 'magent-session)
(require 'magent-tool-orchestrator)
(require 'magent-tools)

(declare-function gptel-abort "gptel-request")
(declare-function gptel-make-tool "gptel")
(declare-function gptel-tool-args "gptel-request")
(declare-function gptel-tool-async "gptel-request")
(declare-function gptel-tool-function "gptel-request")
(declare-function gptel-tool-name "gptel-request")
(declare-function magent-audit-record-permission-decision "magent-audit")
(declare-function magent-ui-insert-tool-call "magent-ui")
(declare-function magent-ui-insert-tool-result "magent-ui")
(declare-function magent-tools-permission-key "magent-tools")

(defvar magent-tools--register-cancel)
(defvar magent-tools--request-context)

(defconst magent-agent-loop-large-raw-content-threshold-bytes 65536
  "Minimum raw text size usually sufficient for direct model answers.")

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
  tool-guard-state
  abort-controller
  tool-queue
  request-handle
  status
  max-tool-rounds
  tool-round-count
  text-chunks
  reasoning-chunks
  tool-calls
  usage
  stop-reason
  error
  result
  metadata)

(defun magent-agent-loop-create (&rest args)
  "Create a `magent-agent-loop' from keyword ARGS.
Recognized keys are `:request', `:sampler', `:status', and
`:metadata'.  SAMPLER is a function called with REQUEST by
`magent-agent-loop-start'."
  (let ((sampler (plist-get args :sampler)))
    (when (and sampler (not (functionp sampler)))
      (error "Agent loop sampler is not callable: %S" sampler))
    (magent-agent-loop--create
     :request (plist-get args :request)
     :sampler sampler
     :session (plist-get args :session)
     :request-context (plist-get args :request-context)
     :tool-guard-state (or (plist-get args :tool-guard-state)
                           (magent-agent-loop-tool-guard-state-create))
     :abort-controller (or (plist-get args :abort-controller)
                           (magent-agent-loop-abort-controller-create))
     :tool-queue (or (plist-get args :tool-queue)
                     (magent-agent-loop-tool-queue-create))
     :status (or (plist-get args :status) 'created)
     :max-tool-rounds (plist-get args :max-tool-rounds)
     :tool-round-count (or (plist-get args :tool-round-count) 0)
     :text-chunks nil
     :reasoning-chunks nil
     :tool-calls nil
     :metadata (plist-get args :metadata))))

(defun magent-agent-loop-text (loop)
  "Return accumulated assistant text for LOOP."
  (apply #'concat (nreverse (copy-sequence
                             (magent-agent-loop-text-chunks loop)))))

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
   ((or (string= streamed final)
        (string-suffix-p final streamed))
    streamed)
   ((string-prefix-p streamed final)
    final)
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
     (push (or (magent-llm-event-text event) "")
           (magent-agent-loop-text-chunks loop))
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
    ('completed
     (setf (magent-agent-loop-status loop) 'completed
           (magent-agent-loop-result loop)
           (magent-agent-loop--combined-result
            (magent-agent-loop-text loop)
            (magent-llm-event-text event))
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

(defun magent-agent-loop-note-tool-round (loop)
  "Increment and return LOOP's tool round count."
  (let ((count (1+ (or (magent-agent-loop-tool-round-count loop) 0))))
    (setf (magent-agent-loop-tool-round-count loop) count)
    count))

(defun magent-agent-loop-tool-round-limit-exceeded-p (loop)
  "Return non-nil when LOOP has exceeded its configured tool round limit.
This function increments the loop's round count."
  (let ((limit (magent-agent-loop-max-tool-rounds loop)))
    (and (numberp limit)
         (> (magent-agent-loop-note-tool-round loop) limit))))

(defun magent-agent-loop-tool-round-limit-message (loop)
  "Return LOOP's user-facing tool round limit message."
  (let ((limit (magent-agent-loop-max-tool-rounds loop)))
    (format "Error: tool loop exceeded %d round%s. The model kept requesting more tools instead of answering."
            limit
            (if (= limit 1) "" "s"))))

(defun magent-agent-loop--tool-name (tool-spec raw-call)
  "Return tool name from TOOL-SPEC or RAW-CALL."
  (or (and tool-spec
           (fboundp 'gptel-tool-name)
           (ignore-errors (gptel-tool-name tool-spec)))
      (plist-get raw-call :name)
      "unknown"))

(defun magent-agent-loop-tool-guard-state-create ()
  "Create per-turn state used by tool-call guards."
  (let ((state (make-hash-table :test 'eq)))
    (puthash :emacs-eval-count 0 state)
    (puthash :seen-emacs-eval-sexps (make-hash-table :test 'equal) state)
    (puthash :last-raw-content-tool nil state)
    (puthash :last-raw-content-bytes nil state)
    state))

(defun magent-agent-loop-tool-guard-track-result (name result guard-state)
  "Record request-local metadata for tool NAME and RESULT."
  (when (and guard-state
             (member name '("emacs_eval" "read_file"))
             (stringp result)
             (not (string-prefix-p "Error:" result)))
    (let ((bytes (string-bytes result)))
      (when (>= bytes magent-agent-loop-large-raw-content-threshold-bytes)
        (puthash :last-raw-content-tool name guard-state)
        (puthash :last-raw-content-bytes bytes guard-state)))))

(defun magent-agent-loop-emacs-eval-direct-answer-guidance (&optional guard-state)
  "Return guidance steering the model toward direct answers."
  (let ((tool (and guard-state (gethash :last-raw-content-tool guard-state)))
        (bytes (and guard-state (gethash :last-raw-content-bytes guard-state))))
    (if (and (stringp tool)
             (integerp bytes)
             (>= bytes magent-agent-loop-large-raw-content-threshold-bytes))
        (format "A prior %s call already returned %d bytes of raw content. That is enough context for summarization or general analysis. Answer directly from that content instead of calling formatting or filtering tools."
                tool bytes)
      "If you already retrieved the file or buffer content, use that raw content directly instead of calling formatting or filtering tools.")))

(defun magent-agent-loop-emacs-eval-limit-message (limit &optional guard-state)
  "Return a constructive repeated `emacs_eval' limit message."
  (format "Error: tool_use_limit_reached. emacs_eval exceeded %d call%s in this turn. Stop exploring and answer the user directly. %s"
          limit
          (if (= limit 1) "" "s")
          (magent-agent-loop-emacs-eval-direct-answer-guidance guard-state)))

(defun magent-agent-loop-maybe-intercept-tool-call
    (name args-plist guard-state)
  "Return a synthetic tool result when NAME/ARGS-PLIST should be intercepted."
  (when (and guard-state
             (string= name "emacs_eval"))
    (let* ((sexp (and-let* ((raw (plist-get args-plist :sexp))
                            ((stringp raw)))
                   (string-trim raw)))
           (seen (gethash :seen-emacs-eval-sexps guard-state))
           (count (gethash :emacs-eval-count guard-state 0))
           (limit magent-emacs-eval-max-calls-per-turn))
      (cond
       ((and sexp (gethash sexp seen))
        (format "[system-intercept] You already executed this exact emacs_eval query in this turn. Stop calling tools and answer the user directly. %s"
                (magent-agent-loop-emacs-eval-direct-answer-guidance
                 guard-state)))
       ((and (natnump limit)
             (>= count limit))
        (concat "[system-intercept] "
                (magent-agent-loop-emacs-eval-limit-message
                 limit guard-state)))
       (t
        (puthash :emacs-eval-count (1+ count) guard-state)
        (when sexp
          (puthash sexp t seen))
        nil)))))

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
            (if-let ((cmd (plist-get args :command)))
                (truncate-string-to-width
                 cmd magent-ui-tool-input-max-length nil nil "...")
              "?"))
           ((member name '("read_file" "write_file" "edit_file"))
            (or (plist-get args :path) "?"))
           ((string= name "grep")
            (format "%s in %s"
                    (or (plist-get args :pattern) "?")
                    (or (plist-get args :path) "?")))
           ((string= name "glob")
            (or (plist-get args :pattern) "?"))
           ((string= name "emacs_eval")
            (if-let ((sexp (plist-get args :sexp)))
                (truncate-string-to-width
                 sexp magent-ui-tool-input-max-length nil nil "...")
              "?"))
           (t
            (if args
                (truncate-string-to-width (format "%s" args) 60 nil nil "...")
              "")))))
    (if (and reason (not (string-empty-p reason)))
        (format "[%s] %s" reason base)
      base)))

(defun magent-agent-loop-tool-result-summary (result)
  "Return display summary for tool RESULT."
  (let ((result-str (if (stringp result) result (format "%s" result))))
    (if (> (length result-str) magent-ui-result-max-length)
        (format "%s... [%d bytes]"
                (substring result-str 0 magent-ui-result-preview-length)
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
            (guard-state (plist-get item :guard-state))
            (fn (plist-get item :fn))
            (async-p (plist-get item :async))
            (fn-args (plist-get item :args))
            (callback (plist-get item :callback)))
        (magent-events-emit 'tool-call-start
                            :context
                            (magent-agent-loop--event-context request-context)
                            :call-id call-id
                            :tool-name name
                            :title name
                            :summary summary
                            :description summary
                            :args args-plist)
        (when (magent-agent-loop--ui-visible-p request-context)
          (require 'magent-ui)
          (magent-ui-insert-tool-call name summary))
        (cl-labels
            ((complete
              (result)
              (setf (magent-agent-loop-tool-queue-busy queue) nil)
              (unless (or (magent-agent-loop-tool-queue-aborted queue)
                          (and abort-controller
                               (magent-agent-loop-abort-controller-aborted
                                abort-controller)))
                (magent-agent-loop-tool-guard-track-result
                 name result guard-state)
                (when (magent-agent-loop--ui-visible-p request-context)
                  (require 'magent-ui)
                  (magent-ui-insert-tool-result
                   name
                   (magent-agent-loop-tool-result-summary result)))
                (magent-events-emit 'tool-call-end
                                    :context
                                    (magent-agent-loop--event-context
                                     request-context)
                                    :call-id call-id
                                    :tool-name name
                                    :result result)
                (funcall callback result)
                (magent-agent-loop-tool-queue-run queue))))
          (let ((magent-tools--register-cancel
                 (lambda (cleanup)
                   (magent-agent-loop-abort-controller-register
                    abort-controller cleanup)))
                (magent-tools--request-context request-context))
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
                  (format "Error: Tool execution failed: %s"
                          (error-message-string err))))))))))))

(defun magent-agent-loop-tool-queue-run (queue)
  "Process the next queued tool in QUEUE when idle."
  (unless (or (magent-agent-loop-tool-queue-aborted queue)
              (magent-agent-loop-tool-queue-busy queue))
    (when-let ((item (pop (magent-agent-loop-tool-queue-items queue))))
      (magent-agent-loop--execute-tool-item queue item))))

(defun magent-agent-loop-run-tool
    (loop request-context tool-spec callback arg-values)
  "Run TOOL-SPEC with ARG-VALUES and call CALLBACK with the result."
  (let* ((args-spec (and (fboundp 'gptel-tool-args)
                         (gptel-tool-args tool-spec)))
         (name (magent-agent-loop--tool-name tool-spec nil))
         (args-plist (magent-agent-loop-args-to-plist args-spec arg-values))
         (call-id (magent-events-generate-id))
         (summary (magent-agent-loop-tool-call-summary name args-plist))
         (guard-state (magent-agent-loop-tool-guard-state loop))
         (intercept-result
          (magent-agent-loop-maybe-intercept-tool-call
           name args-plist guard-state))
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
             :guard-state guard-state
             :fn (if intercept-result
                     (lambda () intercept-result)
                   fn)
             :async (and async-p (not intercept-result))
             :callback callback
             :args (if intercept-result nil fn-args))))))

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
       :summary summary
       :args args-plist))))

(defun magent-agent-loop-tools-to-gptel (tools)
  "Convert Magent tool plist TOOLS to gptel-tool structs for provider schema."
  (mapcar
   (lambda (tool)
     (gptel-make-tool
      :name (plist-get tool :name)
      :description (plist-get tool :description)
      :args (plist-get tool :args)
      :function (plist-get tool :function)
      :async (plist-get tool :async)
      :category "magent"))
   tools))

(defun magent-agent-loop-create-orchestrator
    (loop permission request-context)
  "Create a tool orchestrator for LOOP, PERMISSION, and REQUEST-CONTEXT."
  (magent-tool-orchestrator-create
   :permission permission
   :request-context request-context
   :run-tool-function (lambda (tool-spec callback arg-values)
                        (magent-agent-loop-run-tool
                         loop request-context tool-spec callback arg-values))
   :audit-function #'magent-agent-loop-audit-permission-decision
   :file-arg-index-function #'magent-agent-loop-find-file-arg-index
   :args-to-plist-function #'magent-agent-loop-args-to-plist
   :summarize-function #'magent-agent-loop-summarize-args))

(defun magent-agent-loop-record-tool-result
    (loop tool-spec arg-values raw-call result)
  "Record TOOL-SPEC RESULT for LOOP's session and return LOOP.
ARG-VALUES are stored as the model-visible tool arguments.  RAW-CALL can
carry provider-specific ids and names."
  (when-let ((session (magent-agent-loop-session loop)))
    (magent-session-add-tool-message
     session
     (or (plist-get raw-call :id)
         (plist-get raw-call :call-id)
         (and (fboundp 'magent-events-generate-id)
              (magent-events-generate-id))
         "tool-call")
     (magent-agent-loop--tool-name tool-spec raw-call)
     (magent-agent-loop--tool-args-plist tool-spec arg-values raw-call)
     result))
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

(defun magent-agent-loop--tool-arg-values (tool-spec args)
  "Return positional tool arg values for TOOL-SPEC from ARGS."
  (if (and tool-spec
           (magent-agent-loop--plist-args-p args)
           (fboundp 'gptel-tool-args))
      (mapcar
       (lambda (spec)
         (plist-get args (intern (concat ":" (plist-get spec :name)))))
       (gptel-tool-args tool-spec))
    args))

(defun magent-agent-loop--tool-args-plist (tool-spec arg-values raw-call)
  "Return model-visible plist args for TOOL-SPEC and ARG-VALUES."
  (cond
   ((magent-agent-loop--plist-args-p arg-values)
    arg-values)
   ((and tool-spec
         (listp arg-values)
         (fboundp 'gptel-tool-args))
    (cl-loop for spec in (gptel-tool-args tool-spec)
             for value in arg-values
             append (list (intern (concat ":" (plist-get spec :name)))
                          value)))
   (t
    (or (plist-get raw-call :args)
        arg-values))))

(defun magent-agent-loop-tool-event-to-call (loop event)
  "Convert normalized tool-call EVENT to orchestrator call shape.
Return `(TOOL-SPEC ARG-VALUES nil RAW-CALL)' or nil when the tool is not
available in LOOP's request tools."
  (let* ((name (magent-llm-event-name event))
         (tool-spec (magent-agent-loop--find-tool loop name)))
    (when tool-spec
      (list tool-spec
            (magent-agent-loop--tool-arg-values
             tool-spec
             (magent-agent-loop--tool-args event))
            nil
            (magent-agent-loop--tool-raw-call event)))))

(defun magent-agent-loop--unknown-tool-result (loop event known-tool-names)
  "Record and return an unknown-tool result for EVENT."
  (let* ((raw-call (magent-agent-loop--tool-raw-call event))
         (name (or (magent-llm-event-name event)
                   (plist-get raw-call :name)
                   "unknown"))
         (message (format "Error: tool '%s' not found. Available: %s"
                          name
                          (mapconcat #'identity known-tool-names ", "))))
    (magent-agent-loop-record-tool-result
     loop nil (magent-agent-loop--tool-args event) raw-call message)
    message))

(defun magent-agent-loop-dispatch-tool-calls
    (loop orchestrator &optional done-callback)
  "Dispatch LOOP's pending tool calls through ORCHESTRATOR.
Unknown tools are recorded as tool-result errors.  Known tool calls are
passed to `magent-tool-orchestrator-handle-tool-calls'.  DONE-CALLBACK is
called after all known tool calls complete, or immediately when there are
no known calls."
  (unless (magent-agent-loop-p loop)
    (error "Expected magent-agent-loop, got: %S" loop))
  (unless (magent-tool-orchestrator-p orchestrator)
    (error "Expected magent-tool-orchestrator, got: %S" orchestrator))
  (if (magent-agent-loop-tool-round-limit-exceeded-p loop)
      (let ((message (magent-agent-loop-tool-round-limit-message loop)))
        (setf (magent-agent-loop-status loop) 'failed
              (magent-agent-loop-error loop) message)
        (when done-callback
          (funcall done-callback message)))
    (let* ((events (nreverse (copy-sequence
                              (magent-agent-loop-tool-calls loop))))
           (tools (magent-llm-request-tools (magent-agent-loop-request loop)))
           (known-tool-names
            (delq nil
                  (mapcar (lambda (tool)
                            (magent-agent-loop--tool-name tool nil))
                          tools)))
           known-calls)
      (setf (magent-agent-loop-tool-calls loop) nil)
      (dolist (event events)
        (if-let ((call (magent-agent-loop-tool-event-to-call loop event)))
            (push call known-calls)
          (magent-agent-loop--unknown-tool-result loop event known-tool-names)))
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
                      (funcall done-callback))))
            (magent-tool-orchestrator-handle-tool-calls
             orchestrator known-calls))
        (when done-callback
          (funcall done-callback)))))
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
     :prompt (if-let ((session (magent-agent-loop-session loop)))
                 (magent-session-to-gptel-prompt-list session)
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

(defun magent-agent-loop-abort (loop)
  "Abort LOOP, its pending tools, and its active provider request."
  (when (magent-agent-loop-p loop)
    (let ((already-aborted (magent-agent-loop--aborted-p loop)))
      (setf (magent-agent-loop-status loop) 'cancelled)
      (magent-agent-loop-tool-queue-abort
       (magent-agent-loop-tool-queue loop))
      (magent-agent-loop-abort-controller-abort
       (magent-agent-loop-abort-controller loop))
      (magent-agent-loop--abort-request-handle
       (magent-agent-loop-request-handle loop))
      (when-let ((request-context (magent-agent-loop-request-context loop)))
        (when (eq (magent-request-context-abort-controller request-context)
                  (magent-agent-loop-abort-controller loop))
          (setf (magent-request-context-abort-controller request-context)
                nil)))
      (unless already-aborted
        (when-let ((context (magent-agent-loop--event-context
                             (magent-agent-loop-request-context loop))))
          (magent-events-end-turn context 'cancelled "User aborted")))))
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
                                 (magent-llm-request-callback request))))
    (unless (magent-llm-request-p request)
      (error "Agent loop requires a magent-llm-request"))
    (unless sampler
      (error "Agent loop requires a sampler"))
    (setf (magent-agent-loop-status loop) 'running)
    (when-let ((request-context (magent-agent-loop-request-context loop)))
      (setf (magent-request-context-abort-controller request-context)
            (magent-agent-loop-abort-controller loop)))
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
                          (magent-agent-loop-apply-event loop event)
                          (when original-callback
                            (funcall original-callback event)))))))
      (let ((handle (funcall sampler loop-request)))
        (setf (magent-agent-loop-request-handle loop) handle)
        handle))))

(provide 'magent-agent-loop)
;;; magent-agent-loop.el ends here
