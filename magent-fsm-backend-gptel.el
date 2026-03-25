;;; magent-fsm-backend-gptel.el --- Gptel FSM backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Gptel-based FSM backend implementation.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'json)

(require 'magent-events)
(require 'magent-fsm-shared)
(require 'magent-runtime)
(declare-function magent-ui-start-streaming "magent-ui")
(declare-function magent-ui-continue-streaming "magent-ui")
(declare-function magent-ui-insert-streaming "magent-ui")
(declare-function magent-ui-insert-reasoning-start "magent-ui")
(declare-function magent-ui-insert-reasoning-text "magent-ui")
(declare-function magent-ui-insert-reasoning-end "magent-ui")
(declare-function magent-ui-finish-streaming-fontify "magent-ui")
(declare-function magent-log "magent-config")
(declare-function gptel-abort "gptel")

(defvar magent-include-reasoning)

(defun magent-fsm-backend-gptel--make-stream-state ()
  "Create mutable per-request streaming state for the gptel backend."
  (let ((state (make-hash-table :test 'eq)))
    (puthash :text-chunks nil state)
    (puthash :reasoning-chunks nil state)
    (puthash :in-reasoning-block nil state)
    (puthash :saw-reasoning nil state)
    state))

(defsubst magent-fsm-backend-gptel--render-reasoning-p ()
  "Return non-nil when reasoning blocks should be shown in the UI.
Only literal t renders reasoning.  `ignore' still retains reasoning
internally, while nil drops it."
  (eq magent-include-reasoning t))

(defun magent-fsm-backend-gptel--note-reasoning-seen
    (state request-id backend model)
  "Record that reasoning was seen in STATE for REQUEST-ID.
BACKEND and MODEL are used for diagnostic logging."
  (unless (gethash :saw-reasoning state)
    (puthash :saw-reasoning t state)
    (magent-log "DEBUG reasoning received request=%s backend=%s model=%s ui=%S"
                request-id
                (and backend (gptel-backend-name backend))
                (format "%s" model)
                magent-include-reasoning)))

(defun magent-fsm-backend-gptel--remember-reasoning (state reasoning-text)
  "Append REASONING-TEXT to STATE when Magent retains reasoning.
Both t and `ignore' keep reasoning in STATE.  Only nil discards it."
  (when (and magent-include-reasoning
             (stringp reasoning-text))
    (push reasoning-text (gethash :reasoning-chunks state))
    (puthash :reasoning-chunks
             (gethash :reasoning-chunks state)
             state)))

(defun magent-fsm-backend-gptel--close-reasoning-block (state &optional request-state)
  "Close the current reasoning block tracked in STATE when needed."
  (when (gethash :in-reasoning-block state)
    (when (magent-fsm-backend-gptel--ui-visible-p request-state)
      (magent-ui-insert-reasoning-end))
    (puthash :in-reasoning-block nil state)))

(defun magent-fsm-backend-gptel--request-bytes (data)
  "Return encoded byte size for DATA, or nil when unavailable."
  (when data
    (length
     (encode-coding-string
      (condition-case nil
          (json-encode data)
        (error (format "%s" data)))
      'utf-8-unix t))))

(defun magent-fsm-backend-gptel--request-live-p (live-p)
  "Return non-nil when LIVE-P indicates the request is still current."
  (if (functionp live-p)
      (condition-case err
          (funcall live-p)
        (error
         (magent-log "WARN request liveness predicate failed: %s"
                     (error-message-string err))
         nil))
    t))

(defun magent-fsm-backend-gptel--ui-visible-p (request-state)
  "Return non-nil when REQUEST-STATE should render tool/text UI."
  (or (null request-state)
      (not (magent-request-context-p request-state))
      (magent-request-context-ui-visible-p request-state)))

(defun magent-fsm-backend-gptel--managed-info-p (info)
  "Return non-nil when INFO belongs to a Magent-managed request."
  (and info (plist-get info :magent-managed)))

(defun magent-fsm-backend-gptel--final-text (state info)
  "Return the final assistant text assembled from STATE and INFO."
  (or (and (gethash :text-chunks state)
           (apply #'concat
                  (reverse (gethash :text-chunks state))))
      (plist-get info :content)
      ""))

(defun magent-fsm-backend-gptel--usage-plist (info final-text)
  "Return a normalized usage plist from INFO and FINAL-TEXT."
  (let* ((input-tokens (or (plist-get info :input-tokens)
                           (plist-get info :prompt-tokens)))
         (output-tokens (or (plist-get info :output-tokens)
                            (plist-get info :completion-tokens)))
         (total-tokens (or (plist-get info :total-tokens)
                           (and (numberp input-tokens)
                                (numberp output-tokens)
                                (+ input-tokens output-tokens))))
         (tool-use (plist-get info :tool-use))
         (request-bytes (magent-fsm-backend-gptel--request-bytes
                         (plist-get info :data))))
    (list :input-tokens input-tokens
          :output-tokens output-tokens
          :total-tokens total-tokens
          :usage-available (or (numberp input-tokens)
                               (numberp output-tokens)
                               (numberp total-tokens))
          :request-bytes request-bytes
          :response-chars (length final-text)
          :tool-use-count (length tool-use)
          :stop-reason (plist-get info :stop-reason)
          :http-status (plist-get info :http-status)
          :error (plist-get info :error))))

(defun magent-fsm-backend-gptel--note-tool-round (info)
  "Increment and return the current tool round count for INFO."
  (let ((count (1+ (or (plist-get info :magent-tool-round-count) 0))))
    (plist-put info :magent-tool-round-count count)
    count))

(defun magent-fsm-backend-gptel--tool-round-limit-reached-p (info)
  "Return non-nil when INFO has exceeded its configured tool round limit."
  (when-let ((limit (plist-get info :magent-tool-round-limit)))
    (> (magent-fsm-backend-gptel--note-tool-round info) limit)))

(defun magent-fsm-backend-gptel--tool-round-limit-message (info)
  "Return a user-facing error message for INFO's tool round limit."
  (let ((limit (plist-get info :magent-tool-round-limit)))
    (format "Error: tool loop exceeded %d round%s. The model kept requesting more tools instead of answering."
            limit
            (if (= limit 1) "" "s"))))

(defun magent-fsm-backend-gptel--abort-tool-loop (fsm info &optional message)
  "Abort FSM because INFO exceeded a Magent loop guard."
  (let ((message (or message
                     (magent-fsm-backend-gptel--tool-round-limit-message info))))
    (magent-log "WARN aborting tool loop request=%s count=%s limit=%s message=%s"
                (plist-get info :magent-request-id)
                (plist-get info :magent-tool-round-count)
                (plist-get info :magent-tool-round-limit)
                message)
    (plist-put info :tool-use nil)
    (plist-put info :tool-pending nil)
    (plist-put info :tool-success nil)
    (plist-put info :error message)
    (when-let ((callback (plist-get info :callback)))
      (funcall callback nil info))
    (gptel--fsm-transition fsm 'ERRS)))

(defun magent-fsm-backend-gptel--assistant-tool-call-count (info tool-name)
  "Return the number of assistant tool calls to TOOL-NAME in INFO history."
  (let* ((data (plist-get info :data))
         (messages (and data (plist-get data :messages)))
         (count 0))
    (when (vectorp messages)
      (cl-loop for msg across messages
               when (and (equal (plist-get msg :role) "assistant")
                         (plist-get msg :tool_calls))
               do (cl-loop for tc across (plist-get msg :tool_calls)
                           for func = (plist-get tc :function)
                           for name = (and func (plist-get func :name))
                           when (equal name tool-name)
                           do (cl-incf count))))
    count))

(defun magent-fsm-backend-gptel--emacs-eval-limit-message ()
  "Return the hard-stop message for repeated emacs_eval exploration."
  (let ((limit magent-emacs-eval-max-calls-per-turn))
    (if (natnump limit)
        (magent-fsm--emacs-eval-limit-message limit)
      (concat "Error: tool_use_limit_reached. emacs_eval exceeded the configured "
              "per-turn limit. Stop exploring and answer the user directly. "
              (magent-fsm--emacs-eval-direct-answer-guidance)))))

(defun magent-fsm-backend-gptel-create (&rest args)
  "Create gptel FSM from keyword ARGS (returns plist with parameters).
Accepts the same keyword arguments as `magent-fsm-create'."
  (list :backend 'gptel
        :session (plist-get args :session)
        :agent (plist-get args :agent)
        :gptel-backend (plist-get args :backend)
        :model (plist-get args :model)
        :prompt-list (plist-get args :prompt-list)
        :system-prompt (plist-get args :system-prompt)
        :tools (plist-get args :tools)
        :max-tool-rounds (plist-get args :max-tool-rounds)
        :event-context (plist-get args :event-context)
        :permission (plist-get args :permission)
        :request-state (plist-get args :request-state)
        :abort-controller (magent-fsm--abort-controller-create)
        :tool-queue nil
        :request-buffer nil
        :live-p (plist-get args :live-p)
        :callback (plist-get args :callback)
        :ui-callback (plist-get args :ui-callback)))

(defun magent-fsm-backend-gptel-start (params)
  "Start gptel request with PARAMS plist."
  (let* ((prompt-list (plist-get params :prompt-list))
         (system-prompt (plist-get params :system-prompt))
         (tools (plist-get params :tools))
         (event-context (plist-get params :event-context))
         (permission (plist-get params :permission))
         (request-state (plist-get params :request-state))
         (abort-controller (plist-get params :abort-controller))
         (callback (plist-get params :callback))
         (ui-callback (plist-get params :ui-callback))
         (live-p (plist-get params :live-p))
         (backend (plist-get params :gptel-backend))
         (model (plist-get params :model))
         (request-id (magent-events-generate-id))
         (request-buffer (generate-new-buffer " *magent-gptel-request*"))
         (tool-queue (or (plist-get params :tool-queue)
                         (magent-fsm--tool-queue-create)))
         (tool-guard-state (magent-fsm--tool-guard-state-create))
         (max-tool-rounds (plist-get params :max-tool-rounds))
         ;; Install permission-aware :confirm functions and handle
         ;; `(tool-call . ...)' callbacks ourselves instead of relying on
         ;; gptel's default in-buffer confirmation UI.
         (gptel-tools (let ((magent-fsm--tool-guard-state tool-guard-state))
                        (magent-fsm--convert-tools-to-gptel
                         tools permission request-state event-context
                         abort-controller tool-queue)))
         (stream-state (magent-fsm-backend-gptel--make-stream-state)))

    (plist-put params :request-buffer request-buffer)
    (plist-put params :tool-queue tool-queue)
    (when request-state
      (setf (magent-request-context-abort-controller request-state)
            abort-controller))

    (when (magent-fsm-backend-gptel--ui-visible-p request-state)
      (magent-ui-start-streaming))
    (magent-events-emit
     'llm-request-start
     :context event-context
     :request-id request-id
     :backend (and backend (gptel-backend-name backend))
     :model (format "%s" model)
     :prompt-count (length prompt-list)
     :tool-count (length tools)
     :system-prompt-length (length (or system-prompt "")))

    (with-current-buffer request-buffer
      (setq-local gptel-backend backend)
      (setq-local gptel-model model)
      (setq-local gptel-tools gptel-tools)
      (setq-local gptel-use-tools (and gptel-tools t))
      (setq-local gptel-confirm-tool-calls 'auto)
      (setq-local gptel-include-reasoning magent-include-reasoning)
      (let* ((gptel-fsm
              (gptel-request
                  prompt-list
                :buffer (current-buffer)
                :system system-prompt
                :stream t
                :callback (lambda (response info)
                            (magent-fsm-backend-gptel--callback
                             response info callback ui-callback request-buffer
                             permission event-context request-state stream-state
                             request-id backend model live-p))))
             (info (and (gptel-fsm-p gptel-fsm)
                        (gptel-fsm-info gptel-fsm))))
        (when info
          (plist-put info :magent-managed t)
          (plist-put info :magent-request-id request-id)
          (plist-put info :magent-tool-round-count 0)
          (when (numberp max-tool-rounds)
            (plist-put info :magent-tool-round-limit max-tool-rounds)))))))

(defun magent-fsm-backend-gptel--callback (response info callback ui-callback buffer
                                                    permission event-context
                                                    &optional request-state
                                                    stream-state
                                                    request-id backend model
                                                    live-p)
  "Handle gptel response.
Wraps all UI operations in `condition-case' to suppress benign
cursor-boundary signals that can leak from evil-mode adjustments
inside gptel's process filter."
  (if (not (magent-fsm-backend-gptel--request-live-p live-p))
      (progn
        (magent-log "DEBUG discarding stale gptel callback request=%s response=%S"
                    request-id response)
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
    (let ((state (or stream-state
                     (magent-fsm-backend-gptel--make-stream-state))))
      (condition-case nil
          (cond
           ((and (consp response) (eq (car response) 'reasoning))
            (let ((reasoning-text (cdr response)))
              (magent-fsm-backend-gptel--note-reasoning-seen
               state request-id backend model)
              (magent-fsm-backend-gptel--remember-reasoning state reasoning-text)
              (if (eq reasoning-text t)
                  (magent-fsm-backend-gptel--close-reasoning-block
                   state request-state)
                ;; Only t renders reasoning in the buffer.
                ;; `ignore' keeps the chunks but suppresses UI output.
                (when (and (magent-fsm-backend-gptel--render-reasoning-p)
                           (magent-fsm-backend-gptel--ui-visible-p request-state))
                  (unless (gethash :in-reasoning-block state)
                    (magent-ui-insert-reasoning-start)
                    (puthash :in-reasoning-block t state))
                  (magent-ui-insert-reasoning-text reasoning-text)))))
           ((stringp response)
            (push response (gethash :text-chunks state))
            (puthash :text-chunks (gethash :text-chunks state) state)
            (magent-log "DEBUG text-chunk len=%d total-chunks=%d"
                        (length response)
                        (length (gethash :text-chunks state)))
            (magent-events-emit 'text-delta :context event-context :text response)
            (when (and ui-callback
                       (magent-fsm-backend-gptel--ui-visible-p request-state))
              (funcall ui-callback response)))
           ((and (consp response) (eq (car response) 'tool-call))
            (magent-fsm-backend-gptel--close-reasoning-block state request-state)
            (magent-fsm--handle-tool-call-confirmation-with-permission
             permission (cdr response) request-state))
           ((and (consp response) (eq (car response) 'tool-result))
            (magent-fsm-backend-gptel--close-reasoning-block state request-state)
            (magent-log "DEBUG tool-result received stop-reason=%S"
                        (plist-get info :stop-reason))
            (when (magent-fsm-backend-gptel--ui-visible-p request-state)
              (magent-ui-continue-streaming)))
           ((eq response t)
            (magent-fsm-backend-gptel--close-reasoning-block state request-state)
            (when (and magent-include-reasoning
                       (not (gethash :saw-reasoning state)))
              (magent-log "DEBUG no reasoning received request=%s backend=%s model=%s ui=%S"
                          request-id
                          (and backend (gptel-backend-name backend))
                          (format "%s" model)
                          magent-include-reasoning))
            (let ((final-text (magent-fsm-backend-gptel--final-text state info)))
              (magent-log "DEBUG response=t tool-use=%S chunks=%d content-len=%d final-len=%d"
                          (and (plist-get info :tool-use) t)
                          (length (gethash :text-chunks state))
                          (length (or (plist-get info :content) ""))
                          (length final-text))
              (apply
               #'magent-events-emit
               'llm-request-end
               :context event-context
               :request-id request-id
               :status 'completed
               :backend (and backend (gptel-backend-name backend))
               :model (format "%s" model)
               (magent-fsm-backend-gptel--usage-plist info final-text))
              (if (plist-get info :tool-use)
                  ;; Tool use round: gptel will continue the loop.
                  ;; Stay in the same section — don't finalize or create a new heading.
                  (when (magent-fsm-backend-gptel--ui-visible-p request-state)
                    (magent-ui-continue-streaming))
                ;; Final response: finalize the streaming section and finish.
                (when (magent-fsm-backend-gptel--ui-visible-p request-state)
                  (magent-ui-finish-streaming-fontify))
                (when callback
                  (funcall callback final-text))
                (when (buffer-live-p buffer)
                  (kill-buffer buffer)))))
           ((null response)
            (magent-log "DEBUG null response (failure) error=%S http=%S"
                        (plist-get info :error)
                        (plist-get info :http-status))
            (magent-fsm-backend-gptel--close-reasoning-block state request-state)
            (when (and magent-include-reasoning
                       (not (gethash :saw-reasoning state)))
              (magent-log "DEBUG no reasoning received request=%s backend=%s model=%s ui=%S"
                          request-id
                          (and backend (gptel-backend-name backend))
                          (format "%s" model)
                          magent-include-reasoning))
            (apply
             #'magent-events-emit
             'llm-request-end
             :context event-context
             :request-id request-id
             :status 'failed
             :backend (and backend (gptel-backend-name backend))
             :model (format "%s" model)
             (magent-fsm-backend-gptel--usage-plist info ""))
            (let ((error-text (plist-get info :error)))
              (if (and (stringp error-text)
                       (> (length error-text) 0))
                  (progn
                    (when (and ui-callback
                               (magent-fsm-backend-gptel--ui-visible-p request-state))
                      (funcall ui-callback error-text))
                    (when (magent-fsm-backend-gptel--ui-visible-p request-state)
                      (magent-ui-finish-streaming-fontify))
                    (when callback
                      (funcall callback error-text)))
                (when callback
                  (funcall callback nil))))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))
        ((beginning-of-buffer end-of-buffer beginning-of-line end-of-line)
         nil)))))

(defun magent-fsm-backend-gptel-abort (params)
  "Abort gptel request."
  (let ((context (plist-get params :event-context))
        (abort-controller (plist-get params :abort-controller))
        (tool-queue (plist-get params :tool-queue))
        (request-buffer (plist-get params :request-buffer)))
    (magent-fsm--tool-queue-abort tool-queue)
    (magent-fsm--abort-controller-abort abort-controller)
    (when (buffer-live-p request-buffer)
      (with-demoted-errors "Magent gptel abort error: %S"
        (gptel-abort request-buffer))
      (when (buffer-live-p request-buffer)
        (kill-buffer request-buffer)))
    (magent-events-end-turn context 'cancelled "User aborted")))

(defun magent-fsm-backend-gptel-destroy (params)
  "Destroy gptel FSM (no-op)."
  (ignore params))

;;; Advice to reset :reasoning-block between tool-use turns

(defun magent--reset-reasoning-block-a (fsm)
  "Reset :reasoning-block in FSM info before firing a new HTTP request.

gptel's `gptel--handle-wait' clears :tool-success, :tool-use, :error,
:http-status, and :reasoning between turns but does NOT clear
:reasoning-block.  For models that stream reasoning via a separate JSON
field (e.g. qwen3-max with :enable_thinking t), when turn 1 contains
reasoning followed by a tool call with no text content, :reasoning-block
is left as \\='in.  On the second turn the stream-filter's
`((not (eq reasoning-block t)) ...)' branch then mis-classifies incoming
text content as reasoning, leaving :text-chunks empty and producing a
silent empty final response (Variant A of the qwen3-max empty-response
  bug documented in project.org)."
  (let ((info (gptel-fsm-info fsm)))
    (when (and (magent-fsm-backend-gptel--managed-info-p info)
               (plist-get info :reasoning-block))
      (magent-log "DEBUG reset :reasoning-block (was %S) before new HTTP request"
                  (plist-get info :reasoning-block))
      (plist-put info :reasoning-block nil))))

(advice-add 'gptel--handle-wait :before #'magent--reset-reasoning-block-a)

;;; Unknown-tool advice for gptel FSM

(defun magent--handle-unknown-tools-a (orig-fn fsm)
  "Around advice for `gptel--handle-tool-use'.
Pre-fill :result for unknown tool calls so gptel filters them out,
preventing the FSM from hanging when the model hallucinates tool names."
  (let* ((info (gptel-fsm-info fsm))
         (patched nil))
    (if (not (magent-fsm-backend-gptel--managed-info-p info))
        (funcall orig-fn fsm)
      (if (magent-fsm-backend-gptel--tool-round-limit-reached-p info)
        (magent-fsm-backend-gptel--abort-tool-loop fsm info)
        (when-let* ((all-tool-use (plist-get info :tool-use))
                    (tools (plist-get info :tools)))
          ;; Debug: log all tool calls with name, id, and args to diagnose splitting issues
          (magent-log "DEBUG tool-use entries: %s"
                      (mapconcat (lambda (tc)
                                   (format "'%s'(id=%S args=%s)"
                                           (plist-get tc :name)
                                           (plist-get tc :id)
                                           (if (plist-get tc :args) "present" "nil")))
                                 all-tool-use ", "))
          ;; Fix: when an empty-named tool call precedes a known tool with nil args,
          ;; merge the empty tool's args and id into the known tool, then remove
          ;; the empty entry from the tool-use list entirely.  This works around a
          ;; qwen3-max streaming quirk where the first chunk arrives with name=""
          ;; causing gptel to split one tool call into two entries with the same id.
          ;; Previously we pre-filled the empty entry with an error result, but both
          ;; entries share the same call_id, so the model received an error for its
          ;; only tool call and terminated the loop prematurely.
          (let ((merged-from nil)
                (all-list all-tool-use))
            (while (cdr all-list)
              (let* ((tc (car all-list))
                     (next (cadr all-list)))
                (when (and (string= (plist-get tc :name) "")
                           (null (plist-get tc :result))
                           (plist-get tc :args)
                           (null (plist-get next :args))
                           (cl-find-if (lambda (ts)
                                         (equal (gptel-tool-name ts)
                                                (plist-get next :name)))
                                       tools))
                  (plist-put next :args (plist-get tc :args))
                  (when (and (plist-get tc :id) (not (plist-get next :id)))
                    (plist-put next :id (plist-get tc :id)))
                  (push tc merged-from)
                  (magent-log "INFO merged args+id(%S) from empty-named tool into '%s'"
                              (plist-get tc :id)
                              (plist-get next :name))))
              (setq all-list (cdr all-list)))
            (when merged-from
              (plist-put info :tool-use
                         (cl-remove-if (lambda (tc) (memq tc merged-from))
                                       all-tool-use))
              (setq all-tool-use (plist-get info :tool-use))
              (let* ((data (plist-get info :data))
                     (messages (and data (plist-get data :messages)))
                     (n (and messages (length messages))))
                (when (and n (> n 0))
                  (let ((idx (1- n)))
                    (while (and (>= idx 0)
                                (not (and (equal (plist-get (aref messages idx) :role)
                                                 "assistant")
                                          (plist-get (aref messages idx) :tool_calls))))
                      (cl-decf idx))
                    (when (>= idx 0)
                      (let* ((msg (aref messages idx))
                             (tool-calls (plist-get msg :tool_calls))
                             (id-to-args (make-hash-table :test 'equal)))
                        (cl-loop for tc across tool-calls
                                 do (let* ((func (plist-get tc :function))
                                           (name (and func (plist-get func :name)))
                                           (args (and func (plist-get func :arguments))))
                                      (when (and (stringp name) (string-empty-p name)
                                                 (stringp args) (not (string-empty-p args)))
                                        (puthash (plist-get tc :id) args id-to-args))))
                        (cl-loop for tc across tool-calls
                                 do (let* ((func (plist-get tc :function))
                                           (name (and func (plist-get func :name)))
                                           (id (plist-get tc :id))
                                           (args-from-empty (gethash id id-to-args))
                                           (cur-args (and func (plist-get func :arguments))))
                                      (when (and (stringp name) (not (string-empty-p name))
                                                 args-from-empty
                                                 (or (null cur-args) (string-empty-p cur-args)))
                                        (plist-put func :arguments args-from-empty)
                                        (magent-log "INFO fixed '%s' arguments in assistant message" name))))
                        (let ((filtered
                               (cl-remove-if
                                (lambda (tc)
                                  (let* ((func (plist-get tc :function))
                                         (name (and func (plist-get func :name))))
                                    (and (stringp name) (string-empty-p name))))
                                (append tool-calls nil))))
                          (when (< (length filtered) (length tool-calls))
                            (plist-put msg :tool_calls (vconcat filtered))
                            (magent-log "INFO patched assistant message: removed %d empty tool_call(s)"
                                        (- (length tool-calls) (length filtered))))))))))))
          (let ((avail (mapconcat #'gptel-tool-name tools ", ")))
            (dolist (tc all-tool-use)
              (unless (plist-get tc :result)
                (let ((name (plist-get tc :name)))
                  (unless (cl-find-if
                           (lambda (ts) (equal (gptel-tool-name ts) name))
                           tools)
                    (plist-put tc :result
                               (format "Error: tool '%s' not found. Available: %s"
                                       name avail))
                    (setq patched t)
                    (magent-log "WARN unknown tool '%s'(id=%S) — returned error to model"
                                name (plist-get tc :id))))))))
        (when (and (natnump magent-emacs-eval-max-calls-per-turn)
                   (cl-find-if (lambda (tc)
                                 (equal (plist-get tc :name) "emacs_eval"))
                               (plist-get info :tool-use))
                   (>= (magent-fsm-backend-gptel--assistant-tool-call-count
                        info "emacs_eval")
                       magent-emacs-eval-max-calls-per-turn))
          (magent-fsm-backend-gptel--abort-tool-loop
           fsm info
           (magent-fsm-backend-gptel--emacs-eval-limit-message)))
        (unless (plist-get info :error)
          (funcall orig-fn fsm)
          (when (and patched (eq (gptel-fsm-state fsm) 'TOOL))
            (let ((remaining (cl-remove-if
                              (lambda (tc) (plist-get tc :result))
                              (plist-get info :tool-use))))
              (when (null remaining)
                (plist-put info :tool-success t)
                (let ((backend (plist-get info :backend)))
                  (gptel--inject-prompt
                   backend (plist-get info :data)
                   (gptel--parse-tool-results backend (plist-get info :tool-use))))
                (funcall (plist-get info :callback)
                         (cons 'tool-result nil) info)
                (gptel--fsm-transition fsm)))))))))

(advice-add 'gptel--handle-tool-use :around #'magent--handle-unknown-tools-a)

(provide 'magent-fsm-backend-gptel)
;;; magent-fsm-backend-gptel.el ends here
