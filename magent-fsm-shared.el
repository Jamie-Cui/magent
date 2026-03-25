;;; magent-fsm-shared.el --- Shared FSM helpers for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Shared data structures and helper functions used by the active gptel
;; backend and related tests.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magent-config)
(require 'magent-approval)
(require 'magent-events)
(require 'magent-runtime)

(declare-function gptel-request "gptel")
(declare-function gptel-make-tool "gptel")
(declare-function gptel-tool-async "gptel-request")
(declare-function gptel-tool-function "gptel-request")
(declare-function gptel-tool-args "gptel-request")
(declare-function gptel-tool-name "gptel-request")
(declare-function magent-ui-insert-tool-call "magent-ui")
(declare-function magent-ui-insert-tool-result "magent-ui")
(declare-function magent-audit-record-permission-decision "magent-audit")
(declare-function magent-tools-permission-key "magent-tools")
(declare-function magent-permission-bypass-p "magent-permission")
(declare-function magent-permission-resolve "magent-permission")
(declare-function magent-permission-session-override "magent-permission")
(declare-function magent-permission-set-session-override "magent-permission")

(defvar magent-tools--register-cancel nil
  "Dynamically bound async-tool cancellation registrar.
When non-nil, async Magent tools call this function with a cleanup
closure that should run if the current request is aborted.")

(defvar magent-tools--request-context nil
  "Dynamically bound request context for the active tool call.")

(defvar magent-fsm--tool-guard-state nil
  "Dynamically bound per-request tool guard state.")

(cl-defstruct (magent-fsm (:constructor magent-fsm-struct-create)
                          (:copier magent-fsm-copy))
  "Finite state machine for Magent LLM requests."
  (state 'INIT)
  session
  agent
  backend
  model
  prompt-list
  system-prompt
  tools
  streaming-p
  pending-tools
  (accumulated-text "")
  (streamed-chunks "")
  (reasoning-text "")
  in-reasoning-block
  http-status
  http-message
  error
  callback
  ui-callback
  request-buffer
  process
  permission
  aborted)

(cl-defstruct
    (magent-fsm--abort-controller
     (:constructor magent-fsm--abort-controller-create))
  "Request-scoped abort state for async tool execution."
  cleanups
  aborted)

(defun magent-fsm-release-resources (fsm)
  "Release transient process resources attached to FSM."
  (when fsm
    (when-let ((proc (magent-fsm-process fsm)))
      (setf (magent-fsm-process fsm) nil)
      (when (process-live-p proc)
        (delete-process proc)))
    (when-let ((buffer (magent-fsm-request-buffer fsm)))
      (setf (magent-fsm-request-buffer fsm) nil)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun magent-fsm--abort-controller-register (controller cleanup)
  "Register CLEANUP with abort CONTROLLER.
If CONTROLLER is already aborted, run CLEANUP immediately."
  (when (functionp cleanup)
    (if (and controller
             (magent-fsm--abort-controller-aborted controller))
        (with-demoted-errors "Magent abort cleanup error: %S"
          (funcall cleanup))
      (when controller
        (push cleanup (magent-fsm--abort-controller-cleanups controller)))))
  cleanup)

(defun magent-fsm--abort-controller-abort (controller)
  "Abort CONTROLLER and run its cleanup closures once."
  (when (and controller
             (not (magent-fsm--abort-controller-aborted controller)))
    (setf (magent-fsm--abort-controller-aborted controller) t)
    (let ((cleanups (nreverse
                     (magent-fsm--abort-controller-cleanups controller))))
      (setf (magent-fsm--abort-controller-cleanups controller) nil)
      (dolist (cleanup cleanups)
        (with-demoted-errors "Magent abort cleanup error: %S"
          (funcall cleanup))))))

(defun magent-fsm--show-tool-call-format (name args)
  "Format tool call NAME with ARGS into a display string.
When ARGS contains a :reason key, prepend it as [reason] to the summary."
  ;; FIXME: add `magent-tool-show-reason' defcustom (default t) to suppress
  ;; reason display without removing the arg from tool definitions.
  (let* ((reason (plist-get args :reason))
         (base
          (cond
           ((string= name "skill_invoke")
            (let ((skill-name (plist-get args :skill_name))
                  (operation (plist-get args :operation)))
              (format "%s/%s" (or skill-name "?") (or operation "?"))))
           ((string= name "delegate")
            (let ((agent (plist-get args :agent)))
              (format "agent: %s" (or agent "?"))))
           ((string= name "bash")
            (let ((cmd (plist-get args :command)))
              (if cmd
                  (truncate-string-to-width cmd magent-ui-tool-input-max-length nil nil "...")
                "?")))
           ((member name '("read_file" "write_file" "edit_file"))
            (or (plist-get args :path) "?"))
           ((string= name "grep")
            (let ((pattern (plist-get args :pattern))
                  (path (plist-get args :path)))
              (format "%s in %s" (or pattern "?") (or path "?"))))
           ((string= name "glob")
            (or (plist-get args :pattern) "?"))
           ((string= name "emacs_eval")
            (let ((sexp (plist-get args :sexp)))
              (if sexp
                  (truncate-string-to-width sexp magent-ui-tool-input-max-length nil nil "...")
                "?")))
           (t
            (if args
                (truncate-string-to-width (format "%s" args) 60 nil nil "...")
              "")))))
    (if (and reason (not (string-empty-p reason)))
        (format "[%s] %s" reason base)
      base)))

(defun magent-fsm--show-tool-call (name args)
  "Show tool call NAME with ARGS in the UI."
  (require 'magent-ui)
  (magent-ui-insert-tool-call name (magent-fsm--show-tool-call-format name args)))

(defun magent-fsm--show-tool-result (name result)
  "Show tool RESULT for tool NAME in the UI."
  (require 'magent-ui)
  (let* ((result-str (if (stringp result) result (format "%s" result)))
         (truncated (if (> (length result-str) magent-ui-result-max-length)
                        (format "%s... [%d bytes]"
                                (substring result-str 0 magent-ui-result-preview-length)
                                (length result-str))
                      result-str)))
    (magent-ui-insert-tool-result name truncated)))

(defun magent-fsm--args-to-plist (args-spec arg-values)
  "Build a keyword plist from ARGS-SPEC names and ARG-VALUES."
  (cl-loop for spec in args-spec
           for val in arg-values
           append (list (intern (concat ":" (plist-get spec :name))) val)))

(defconst magent-fsm--large-raw-content-threshold-bytes 65536
  "Minimum raw text size that usually provides enough context to answer.
When an `emacs_eval' or `read_file' result reaches this size, Magent
records it so later guard messages can steer the model toward answering
directly instead of continuing exploratory tool use.")

(defun magent-fsm--tool-guard-state-create ()
  "Create per-request state used for Magent tool-call guards."
  (let ((state (make-hash-table :test 'eq)))
    (puthash :emacs-eval-count 0 state)
    (puthash :seen-emacs-eval-sexps (make-hash-table :test 'equal) state)
    (puthash :last-raw-content-tool nil state)
    (puthash :last-raw-content-bytes nil state)
    state))

(defun magent-fsm--tool-guard-track-result (name result guard-state)
  "Record useful request-local metadata for tool NAME and RESULT.
Large raw text returned by `emacs_eval' or `read_file' is remembered so
future guard messages can remind the model that it already has enough
material to summarize or analyze directly."
  (when (and guard-state
             (member name '("emacs_eval" "read_file"))
             (stringp result)
             (not (string-prefix-p "Error:" result)))
    (let ((bytes (string-bytes result)))
      (when (>= bytes magent-fsm--large-raw-content-threshold-bytes)
        (puthash :last-raw-content-tool name guard-state)
        (puthash :last-raw-content-bytes bytes guard-state)))))

(defun magent-fsm--emacs-eval-direct-answer-guidance (&optional guard-state)
  "Return guidance that steers the model toward answering directly.
When GUARD-STATE records a previous large raw-content read, include its
size so the model sees that it already has enough context."
  (let ((tool (and guard-state (gethash :last-raw-content-tool guard-state)))
        (bytes (and guard-state (gethash :last-raw-content-bytes guard-state))))
    (if (and (stringp tool)
             (integerp bytes)
             (>= bytes magent-fsm--large-raw-content-threshold-bytes))
        (format "A prior %s call already returned %d bytes of raw content. That is enough context for summarization or general analysis. Answer directly from that content instead of calling formatting or filtering tools."
                tool bytes)
      "If you already retrieved the file or buffer content, use that raw content directly instead of calling formatting or filtering tools.")))

(defun magent-fsm--emacs-eval-limit-message (limit &optional guard-state)
  "Return a constructive limit message for repeated `emacs_eval' calls.
LIMIT is the configured per-turn cap.  GUARD-STATE adds large-result
context when available."
  (format "Error: tool_use_limit_reached. emacs_eval exceeded %d call%s in this turn. Stop exploring and answer the user directly. %s"
          limit
          (if (= limit 1) "" "s")
          (magent-fsm--emacs-eval-direct-answer-guidance guard-state)))

(defun magent-fsm--maybe-intercept-tool-call (name args-plist guard-state)
  "Return a synthetic tool result when NAME/ARGS-PLIST should be intercepted.
GUARD-STATE tracks request-local tool guard state."
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
                (magent-fsm--emacs-eval-direct-answer-guidance guard-state)))
       ((and (natnump limit)
             (>= count limit))
        (concat "[system-intercept] "
                (magent-fsm--emacs-eval-limit-message limit guard-state)))
       (t
        (puthash :emacs-eval-count (1+ count) guard-state)
        (when sexp
          (puthash sexp t seen))
        nil)))))

;;; Tool render queue

(cl-defstruct (magent-fsm--tool-queue
               (:constructor magent-fsm--tool-queue-create))
  items
  busy
  aborted)

(defun magent-fsm--tool-queue-push (queue item)
  "Push ITEM onto QUEUE and start processing if idle."
  (unless (magent-fsm--tool-queue-aborted queue)
    (setf (magent-fsm--tool-queue-items queue)
          (nconc (magent-fsm--tool-queue-items queue) (list item)))
    (magent-fsm--tool-queue-run queue)))

(defun magent-fsm--tool-queue-abort (queue)
  "Abort QUEUE, discarding pending tool items."
  (when queue
    (setf (magent-fsm--tool-queue-aborted queue) t
          (magent-fsm--tool-queue-items queue) nil
          (magent-fsm--tool-queue-busy queue) nil)))

(defun magent-fsm--tool-queue-run (queue)
  "Process the next queued tool if the queue is idle."
  (unless (or (magent-fsm--tool-queue-aborted queue)
              (magent-fsm--tool-queue-busy queue))
    (when-let ((item (pop (magent-fsm--tool-queue-items queue))))
      (let ((abort-controller (plist-get item :abort-controller)))
        (unless (and abort-controller
                     (magent-fsm--abort-controller-aborted abort-controller))
          (setf (magent-fsm--tool-queue-busy queue) t)
          (let ((name (plist-get item :name))
                (input (plist-get item :input))
                (call-id (plist-get item :call-id))
                (args-plist (plist-get item :args-plist))
                (tool-guard-state (plist-get item :guard-state))
                (request-context (plist-get item :request-context))
                (event-context (plist-get item :event-context))
                (fn (plist-get item :fn))
                (async-p (plist-get item :async))
                (callback (plist-get item :callback))
                (args (plist-get item :args)))
            (when (magent-request-context-ui-visible-p request-context)
              (require 'magent-ui))
            (magent-events-emit 'tool-call-start
                                :context event-context
                                :call-id call-id
                                :tool-name name
                                :title name
                                :summary input
                                :description input
                                :args args-plist)
            (when (magent-request-context-ui-visible-p request-context)
              (magent-ui-insert-tool-call name input))
            (let ((completion
                   (lambda (result)
                     (setf (magent-fsm--tool-queue-busy queue) nil)
                     (unless (or (magent-fsm--tool-queue-aborted queue)
                                 (and abort-controller
                                      (magent-fsm--abort-controller-aborted
                                       abort-controller)))
                       (magent-fsm--tool-guard-track-result
                        name result tool-guard-state)
                       (when (magent-request-context-ui-visible-p request-context)
                         (magent-fsm--show-tool-result name result))
                       (magent-events-emit 'tool-call-end
                                           :context event-context
                                           :call-id call-id
                                           :tool-name name
                                           :result result)
                       (funcall callback result)
                       (magent-fsm--tool-queue-run queue)))))
              (if async-p
                  (let ((magent-tools--register-cancel
                         (lambda (cleanup)
                           (magent-fsm--abort-controller-register
                            abort-controller cleanup)))
                        (magent-tools--request-context request-context))
                    (condition-case err
                        (apply fn completion args)
                      (quit
                       (funcall completion "Error: Tool execution interrupted"))
                      (error
                        (funcall completion
                                 (format "Error: Tool execution failed: %s"
                                         (error-message-string err))))))
                (funcall completion
                         (condition-case err
                             (let ((magent-tools--register-cancel
                                    (lambda (cleanup)
                                      (magent-fsm--abort-controller-register
                                       abort-controller cleanup)))
                                   (magent-tools--request-context request-context))
                               (apply fn args))
                           (quit "Error: Tool execution interrupted")
                           (error
                            (format "Error: Tool execution failed: %s"
                                    (error-message-string err)))))))))))))

(defun magent-fsm--filter-display-args (args-spec arg-values)
  "Remove display-only args from ARG-VALUES before invoking the tool function.
Currently strips the \\='reason\\=' arg which is used for UI display only and
must not be forwarded to the actual tool implementation."
  ;; FIXME: when `magent-tool-show-reason' defcustom is added, this function
  ;; can be removed when the feature is disabled (no reason arg → no filtering).
  (if (cl-find "reason" args-spec
               :key (lambda (s) (plist-get s :name))
               :test #'string=)
      (cl-loop for spec in args-spec
               for val in arg-values
               unless (string= (plist-get spec :name) "reason")
               collect val)
    arg-values))

(defun magent-fsm--wrap-tool-function (name args-spec original-fn async-p
                                            &optional queue abort-controller
                                            guard-state request-context
                                            event-context)
  "Wrap ORIGINAL-FN to render tool UI and events before/after execution."
  (if queue
      (lambda (callback &rest arg-values)
        (let* ((args-plist (magent-fsm--args-to-plist args-spec arg-values))
               (summary (magent-fsm--show-tool-call-format name args-plist))
               (fn-args (magent-fsm--filter-display-args args-spec arg-values))
               (intercept-result (magent-fsm--maybe-intercept-tool-call
                                  name args-plist guard-state)))
          (magent-fsm--tool-queue-push
           queue
           (list :name name
                 :call-id (magent-events-generate-id)
                 :input summary
                 :args-plist args-plist
                 :guard-state guard-state
                 :request-context request-context
                 :event-context event-context
                 :fn (if intercept-result
                         (lambda () intercept-result)
                       original-fn)
                 :async (and async-p (not intercept-result))
                 :callback callback
                 :abort-controller abort-controller
                 :args (if intercept-result nil fn-args)))))
    (if async-p
        (lambda (callback &rest arg-values)
          (unless (and abort-controller
                       (magent-fsm--abort-controller-aborted abort-controller))
            (let* ((args-plist (magent-fsm--args-to-plist args-spec arg-values))
                   (call-id (magent-events-generate-id))
                   (summary (magent-fsm--show-tool-call-format name args-plist))
                   (fn-args (magent-fsm--filter-display-args args-spec arg-values))
                   (intercept-result (magent-fsm--maybe-intercept-tool-call
                                      name args-plist guard-state)))
              (magent-events-emit 'tool-call-start
                                  :context event-context
                                  :call-id call-id
                                  :tool-name name
                                  :title name
                                  :summary summary
                                  :description summary
                                  :args args-plist)
              (when (magent-request-context-ui-visible-p request-context)
                (magent-fsm--show-tool-call name args-plist))
              (if intercept-result
                  (progn
                    (when (magent-request-context-ui-visible-p request-context)
                      (magent-fsm--show-tool-result name intercept-result))
                    (magent-events-emit 'tool-call-end
                                        :context event-context
                                        :call-id call-id
                                        :tool-name name
                                        :result intercept-result)
                    (funcall callback intercept-result))
                (let ((magent-tools--register-cancel
                       (lambda (cleanup)
                         (magent-fsm--abort-controller-register
                          abort-controller cleanup)))
                      (magent-tools--request-context request-context))
                  (apply original-fn
                         (lambda (result)
                           (unless (and abort-controller
                                        (magent-fsm--abort-controller-aborted
                                         abort-controller))
                             (magent-fsm--tool-guard-track-result name result guard-state)
                             (when (magent-request-context-ui-visible-p request-context)
                               (magent-fsm--show-tool-result name result))
                             (magent-events-emit 'tool-call-end
                                                 :context event-context
                                                 :call-id call-id
                                                 :tool-name name
                                                 :result result)
                             (funcall callback result)))
                         fn-args))))))
      (lambda (&rest arg-values)
        (unless (and abort-controller
                     (magent-fsm--abort-controller-aborted abort-controller))
          (let* ((args-plist (magent-fsm--args-to-plist args-spec arg-values))
                 (call-id (magent-events-generate-id))
                 (summary (magent-fsm--show-tool-call-format name args-plist))
                 (fn-args (magent-fsm--filter-display-args args-spec arg-values))
                 (intercept-result (magent-fsm--maybe-intercept-tool-call
                                    name args-plist guard-state))
                 result)
            (magent-events-emit 'tool-call-start
                                :context event-context
                                :call-id call-id
                                :tool-name name
                                :title name
                                :summary summary
                                :description summary
                                :args args-plist)
            (when (magent-request-context-ui-visible-p request-context)
              (magent-fsm--show-tool-call name args-plist))
            (if intercept-result
                (setq result intercept-result)
              (let ((magent-tools--register-cancel
                     (lambda (cleanup)
                       (magent-fsm--abort-controller-register
                        abort-controller cleanup)))
                    (magent-tools--request-context request-context))
                (setq result (apply original-fn fn-args))))
            (unless (and abort-controller
                         (magent-fsm--abort-controller-aborted abort-controller))
              (magent-fsm--tool-guard-track-result name result guard-state)
              (when (magent-request-context-ui-visible-p request-context)
                (magent-fsm--show-tool-result name result))
              (magent-events-emit 'tool-call-end
                                  :context event-context
                                  :call-id call-id
                                  :tool-name name
                                  :result result)
              result)))))))

(defun magent-fsm--convert-tools-to-gptel
    (tools &optional permission request-context event-context
           abort-controller tool-queue)
  "Convert Magent TOOLS to gptel-tool structs with UI wrappers."
  (require 'gptel)
  (let ((queue (or tool-queue (magent-fsm--tool-queue-create))))
    (mapcar (lambda (tool)
              (let* ((name (plist-get tool :name))
                     (args-spec (plist-get tool :args))
                     (original-fn (plist-get tool :function))
                     (async-p (plist-get tool :async))
                     (perm-key (plist-get tool :perm-key))
                     (wrapped-fn (magent-fsm--wrap-tool-function
                                  name args-spec original-fn async-p
                                  queue abort-controller
                                  magent-fsm--tool-guard-state
                                  request-context event-context))
                     (confirm-fn (when permission
                                   (magent-fsm--make-confirm-function
                                    perm-key permission args-spec
                                    request-context))))
                (gptel-make-tool
                 :name name
                 :description (plist-get tool :description)
                 :args args-spec
                 :function wrapped-fn
                 :async t
                 :confirm confirm-fn)))
            tools)))

;;; Permission-aware tool confirmation

(defun magent-fsm--make-confirm-function (perm-key permission args-spec
                                                   &optional request-context)
  "Create a gptel :confirm function for PERM-KEY under PERMISSION."
  (require 'magent-permission)
  (let ((approval-session (or (and request-context
                                   (magent-request-context-approval-session
                                    request-context))
                              (and request-context
                                   (magent-request-context-session request-context)))))
    (unless (magent-permission-bypass-p)
      (let* ((base-perm (magent-permission-resolve permission perm-key))
           (file-arg-index (magent-fsm--find-file-arg-index args-spec)))
        (cond
         ((eq base-perm 'ask)
          (if file-arg-index
              (lambda (&rest arg-values)
                (let ((override (magent-permission-session-override perm-key approval-session)))
                  (if (eq override 'allow)
                      (let ((file-path (nth file-arg-index arg-values)))
                        (when file-path
                          (eq (magent-permission-resolve permission perm-key file-path)
                              'deny)))
                    t)))
            (lambda (&rest _arg-values)
              (not (eq (magent-permission-session-override perm-key approval-session)
                       'allow)))))
         ((eq base-perm 'allow)
          (if file-arg-index
              (lambda (&rest arg-values)
                (let ((file-path (nth file-arg-index arg-values)))
                  (when file-path
                    (not (eq (magent-permission-resolve permission perm-key file-path)
                             'allow)))))
            nil))
         ((eq base-perm 'deny)
          (lambda (&rest _arg-values) t))
         (t nil))))))

(defun magent-fsm--find-file-arg-index (args-spec)
  "Return the 0-based index of a file path arg in ARGS-SPEC, if any."
  (cl-loop for spec in args-spec
           for i from 0
           when (member (plist-get spec :name) '("path" "file"))
           return i))

(defun magent-fsm--audit-permission-decision
    (tool-spec arg-values decision source &optional request-context)
  "Record a permission DECISION for TOOL-SPEC with ARG-VALUES."
  (when (fboundp 'magent-audit-record-permission-decision)
    (let* ((tool-name (gptel-tool-name tool-spec))
           (perm-key (magent-tools-permission-key tool-name))
           (args-plist (magent-fsm--args-to-plist (gptel-tool-args tool-spec)
                                                  arg-values))
           (summary (magent-fsm--show-tool-call-format tool-name args-plist)))
      (magent-audit-record-permission-decision
       tool-name perm-key decision source
       :context (and request-context
                     (magent-request-context-event-context request-context))
       :summary summary
       :args args-plist))))

(defun magent-fsm--handle-tool-call-confirmation-with-permission
    (permission tool-calls &optional request-context)
  "Handle TOOL-CALLS using PERMISSION rules."
  (require 'magent-permission)
  (let ((approval-session (or (and request-context
                                   (magent-request-context-approval-session
                                    request-context))
                              (and request-context
                                   (magent-request-context-session request-context)))))
    (if (magent-permission-bypass-p)
      (dolist (tc tool-calls)
        (let* ((tool-spec (car tc))
               (arg-values (cadr tc))
               (cb (caddr tc))
               (tool-name (gptel-tool-name tool-spec)))
          (magent-log "PERM bypass allow: %s" tool-name)
          (magent-fsm--audit-permission-decision
           tool-spec arg-values 'allow 'bypass request-context)
          (magent-fsm--run-tool tool-spec cb arg-values)))
      (let (pending)
        (dolist (tc tool-calls)
          (let* ((tool-spec (car tc))
                 (arg-values (cadr tc))
                 (cb (caddr tc))
                 (tool-name (gptel-tool-name tool-spec))
                 (perm-key (magent-tools-permission-key tool-name))
                 (file-path (when perm-key
                              (let ((idx (magent-fsm--find-file-arg-index
                                          (gptel-tool-args tool-spec))))
                                (when idx
                                  (nth idx arg-values)))))
                 (resolved (when (and permission perm-key)
                             (magent-permission-resolve permission perm-key file-path)))
                 (override (when perm-key
                             (magent-permission-session-override
                              perm-key approval-session))))
            (cond
             ((eq override 'allow)
              (magent-log "PERM auto-allow (session override): %s" tool-name)
              (magent-fsm--audit-permission-decision
               tool-spec arg-values 'allow 'session-override-allow request-context)
              (magent-fsm--run-tool tool-spec cb arg-values))
             ((eq override 'deny)
              (magent-log "PERM auto-deny (session override): %s" tool-name)
              (magent-fsm--audit-permission-decision
               tool-spec arg-values 'deny 'session-override-deny request-context)
              (funcall cb (format "Error: tool '%s' denied by session policy" tool-name)))
             ((eq resolved 'deny)
              (magent-log "PERM auto-deny (file rule): %s %s" tool-name (or file-path ""))
              (magent-fsm--audit-permission-decision
               tool-spec arg-values 'deny 'file-rule-deny request-context)
              (funcall cb (format "Error: access denied for %s on %s"
                                  tool-name (or file-path "this resource"))))
             ((and file-path (eq resolved 'allow))
              (magent-log "PERM auto-allow (file rule): %s %s" tool-name file-path)
              (magent-fsm--audit-permission-decision
               tool-spec arg-values 'allow 'file-rule-allow request-context)
              (magent-fsm--run-tool tool-spec cb arg-values))
             (t
              (push tc pending)))))
        (when pending
          (magent-fsm--prompt-next-tool-call
           (nreverse pending) request-context))))))

(defun magent-fsm--handle-tool-call-confirmation (fsm tool-calls)
  "Handle TOOL-CALLS for FSM using its attached permission rules."
  (magent-fsm--handle-tool-call-confirmation-with-permission
   (magent-fsm-permission fsm)
   tool-calls))

(defun magent-fsm--prompt-next-tool-call (tool-calls &optional request-context)
  "Prompt for the next tool call in TOOL-CALLS."
  (when tool-calls
    (let* ((tc (car tool-calls))
           (rest (cdr tool-calls))
           (tool-spec (car tc))
           (arg-values (cadr tc))
           (cb (caddr tc))
           (tool-name (gptel-tool-name tool-spec))
           (perm-key (magent-tools-permission-key tool-name))
           (approval-session (or (and request-context
                                      (magent-request-context-approval-session
                                       request-context))
                                 (and request-context
                                      (magent-request-context-session request-context))))
           (summary (magent-fsm--summarize-args
                     arg-values (gptel-tool-args tool-spec))))
      (magent-approval-request
       (list :request-id (magent-events-generate-id)
             :context (and request-context
                           (magent-request-context-event-context request-context))
             :tool-name tool-name
             :perm-key perm-key
             :summary summary
             :args (magent-fsm--args-to-plist (gptel-tool-args tool-spec) arg-values))
       (lambda (decision)
         (pcase decision
           ('allow-once
            (magent-log "PERM user allowed (once): %s" tool-name)
            (magent-fsm--run-tool tool-spec cb arg-values))
           ('deny-once
            (magent-log "PERM user denied (once): %s" tool-name)
           (funcall cb (format "Error: tool '%s' denied by user" tool-name)))
           ('allow-session
            (magent-log "PERM user always-allow: %s" tool-name)
            (when perm-key
              (magent-permission-set-session-override
               perm-key 'allow approval-session))
            (magent-fsm--run-tool tool-spec cb arg-values))
           ('deny-session
            (magent-log "PERM user always-deny: %s" tool-name)
            (when perm-key
              (magent-permission-set-session-override
               perm-key 'deny approval-session))
            (funcall cb (format "Error: tool '%s' denied by user" tool-name))))
         (magent-fsm--prompt-next-tool-call rest request-context))))))

(defun magent-fsm--run-tool (tool-spec cb arg-values)
  "Execute TOOL-SPEC with ARG-VALUES and call CB with the result."
  (condition-case err
      (if (gptel-tool-async tool-spec)
          (apply (gptel-tool-function tool-spec) cb arg-values)
        (funcall cb (apply (gptel-tool-function tool-spec) arg-values)))
    (error
     (funcall cb (format "Error executing tool: %s"
                         (error-message-string err))))))

(defun magent-fsm--summarize-args (arg-values args-spec)
  "Create a short prompt summary from ARG-VALUES and ARGS-SPEC."
  (let* ((first-val (car arg-values))
         (first-name (when args-spec
                       (plist-get (car args-spec) :name)))
         (display (cond
                   ((null first-val) "")
                   ((stringp first-val)
                    (if (> (length first-val) 60)
                        (concat (substring first-val 0 57) "...")
                      first-val))
                   (t (format "%s" first-val)))))
    (if (and first-name (not (string-empty-p display)))
        display
      "")))

(provide 'magent-fsm-shared)
;;; magent-fsm-shared.el ends here
