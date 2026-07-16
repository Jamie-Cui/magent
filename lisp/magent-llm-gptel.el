;;; magent-llm-gptel.el --- gptel-request adapter for Magent LLM events  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Thin adapter from `gptel-request' callbacks to Magent's normalized
;; `magent-llm-event' protocol.  This module is the only intended place
;; for gptel FSM/callback details; the Magent-owned agent loop consumes
;; normalized events instead of gptel internals.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'gptel-request)
(require 'magent-config)
(require 'magent-json)
(require 'magent-llm)

(declare-function gptel--error-p "gptel-request")
(declare-function gptel--handle-post "gptel-request")
(declare-function gptel--handle-wait "gptel-request")
(declare-function gptel--tool-use-p "gptel-request")
(declare-function gptel-fsm-info "gptel-request")
(declare-function gptel-make-fsm "gptel-request")
(declare-function gptel-tool-name "gptel-request")
(declare-function gptel-openai-p "gptel-openai")
(declare-function gptel-openai-responses-p "gptel-openai")
(declare-function gptel-anthropic-p "gptel-anthropic")
(declare-function gptel-bedrock-p "gptel-bedrock")
(declare-function gptel-gemini-p "gptel-gemini")
(declare-function gptel-ollama-p "gptel-ollama")
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-tools)
(defvar gptel-use-tools)
(defvar gptel-confirm-tool-calls)
(defvar gptel-include-reasoning)
(defvar gptel-temperature)
(defvar gptel--request-params)
(defvar magent-include-reasoning)

(defun magent-llm-gptel--managed-context-p (context)
  "Return non-nil when gptel CONTEXT belongs to this adapter."
  (and (listp context)
       (or (plist-get context :magent-llm-gptel)
           (plist-get context :magent-managed))))

(defun magent-llm-gptel--managed-info-p (info)
  "Return non-nil when gptel INFO belongs to this adapter."
  (and (listp info)
       (or (plist-get info :magent-llm-gptel)
           (plist-get info :magent-managed)
           (magent-llm-gptel--managed-context-p
            (plist-get info :context)))))

(defun magent-llm-gptel--sanitize-provider-tool-args (args)
  "Return JSON-safe provider tool ARGS with null-like plist values omitted.
This boundary receives provider/gptel tool metadata where nil means missing or
JSON null, unlike Magent's internal tool args where Lisp nil can be meaningful."
  (cond
   ((null args) nil)
   ((magent-json--plist-p args)
    (let (out)
      (while args
        (let ((key (pop args))
              (val (pop args)))
          (unless (or (null val)
                      (eq val :null))
            (setq out (append out
                              (list key (magent-json-safe-value val)))))))
      out))
   (t
    (magent-json-safe-value args))))

(defun magent-llm-gptel--sanitize-tool-call (tool-call)
  "Sanitize one gptel TOOL-CALL plist in place and return it."
  (when (listp tool-call)
    (when (plist-member tool-call :name)
      (plist-put tool-call
                 :name
                 (magent-json-safe-name (plist-get tool-call :name))))
    (when (plist-member tool-call :args)
      (plist-put tool-call
                 :args
                 (magent-llm-gptel--sanitize-provider-tool-args
                  (plist-get tool-call :args)))))
  tool-call)

(defun magent-llm-gptel--sanitize-tool-use (info)
  "Sanitize gptel INFO's `:tool-use' values in place."
  (when-let* ((tool-use (and (listp info) (plist-get info :tool-use))))
    (dolist (tool-call tool-use)
      (magent-llm-gptel--sanitize-tool-call tool-call))))

(defun magent-llm-gptel--sanitize-assistant-tool-calls (info)
  "Sanitize assistant tool call history in gptel INFO's request data.
Some gptel parsing paths preserve Lisp symbols in tool-call names after
reading Magent's structured tool result blocks.  Emacs' native JSON
serializer rejects those symbols when gptel logs or sends continuation
requests, so Magent normalizes this boundary before curl serializes it."
  (let* ((data (and (listp info) (plist-get info :data)))
         (messages (and (listp data) (plist-get data :messages))))
    (when (vectorp messages)
      (cl-loop for msg across messages
               when (and (listp msg)
                         (equal (plist-get msg :role) "assistant")
                         (vectorp (plist-get msg :tool_calls)))
               do (cl-loop for tc across (plist-get msg :tool_calls)
                           for func = (and (listp tc)
                                           (plist-get tc :function))
                           when (listp func)
                           do (progn
                                (when (plist-member func :name)
                                  (plist-put
                                   func
                                   :name
                                   (magent-json-safe-name
                                    (plist-get func :name))))
                                (when (plist-member func :arguments)
                                  (let ((arguments
                                         (plist-get func :arguments)))
                                    (unless (stringp arguments)
                                      (plist-put
                                       func
                                       :arguments
                                       (magent-json-encode
                                        (magent-llm-gptel--sanitize-provider-tool-args
                                         arguments))))))))))))

(defun magent-llm-gptel--sanitize-info (info)
  "Sanitize gptel INFO structures that may be serialized as JSON."
  (magent-llm-gptel--sanitize-tool-use info)
  (magent-llm-gptel--sanitize-assistant-tool-calls info)
  info)

(defun magent-llm-gptel--put-nested-param
    (data section key value)
  "Set SECTION's KEY to VALUE in provider request DATA."
  (let ((params (copy-sequence (plist-get data section))))
    (plist-put data section (plist-put params key value))))

(defun magent-llm-gptel--apply-top-p-to-info (info)
  "Apply Magent top-p context to gptel request INFO in wire format."
  (let* ((context (plist-get info :context))
         (top-p (and (listp context)
                     (plist-member context :top-p)
                     (plist-get context :top-p)))
         (backend (plist-get info :backend))
         (data (plist-get info :data)))
    (when (and top-p (listp data))
      (cond
       ((or (magent-llm-gptel--backend-openai-responses-p backend)
            (magent-llm-gptel--backend-openai-chat-p backend)
            (and (fboundp 'gptel-anthropic-p)
                 (gptel-anthropic-p backend)))
        (plist-put info :data (plist-put data :top_p top-p)))
       ((and (fboundp 'gptel-ollama-p)
             (gptel-ollama-p backend))
        (plist-put info :data
                   (magent-llm-gptel--put-nested-param
                    data :options :top_p top-p)))
       ((and (fboundp 'gptel-gemini-p)
             (gptel-gemini-p backend))
        (plist-put info :data
                   (magent-llm-gptel--put-nested-param
                    data :generationConfig :topP top-p)))
       ((and (fboundp 'gptel-bedrock-p)
             (gptel-bedrock-p backend))
        (plist-put info :data
                   (magent-llm-gptel--put-nested-param
                    data :inferenceConfig :topP top-p))))))
  info)

(defun magent-llm-gptel--reset-reasoning-block-a (fsm)
  "Reset managed gptel FSM reasoning state before a request starts."
  (when-let* ((info (and (fboundp 'gptel-fsm-info)
                        (gptel-fsm-info fsm))))
    (when (and (magent-llm-gptel--managed-info-p info)
               (plist-get info :reasoning-block))
      (plist-put info :reasoning-block nil))))

(defun magent-llm-gptel--sanitize-before-curl-a (orig-fn info &rest args)
  "Sanitize Magent-managed INFO before gptel serializes request data."
  (when (magent-llm-gptel--managed-info-p info)
    (magent-llm-gptel--apply-top-p-to-info info)
    (magent-llm-gptel--sanitize-info info))
  (apply orig-fn info args))

(defun magent-llm-gptel--sanitize-after-parse-response-a
    (orig-fn backend response info)
  "Sanitize Magent-managed INFO after gptel parses a response."
  (prog1 (funcall orig-fn backend response info)
    (when (magent-llm-gptel--managed-info-p info)
      (magent-llm-gptel--sanitize-info info))))

(defun magent-llm-gptel--sanitize-after-parse-stream-a
    (orig-fn backend info)
  "Sanitize Magent-managed INFO after gptel parses a stream chunk."
  (prog1 (funcall orig-fn backend info)
    (when (magent-llm-gptel--managed-info-p info)
      (magent-llm-gptel--sanitize-info info))))

(defun magent-llm-gptel--install-boundary-advice ()
  "Install adapter-local gptel boundary sanitization advice."
  (unless (advice-member-p #'magent-llm-gptel--reset-reasoning-block-a
                           'gptel--handle-wait)
    (advice-add 'gptel--handle-wait
                :before #'magent-llm-gptel--reset-reasoning-block-a))
  (unless (advice-member-p #'magent-llm-gptel--sanitize-before-curl-a
                           'gptel-curl--get-args)
    (advice-add 'gptel-curl--get-args
                :around #'magent-llm-gptel--sanitize-before-curl-a))
  (unless (advice-member-p #'magent-llm-gptel--sanitize-after-parse-response-a
                           'gptel--parse-response)
    (advice-add 'gptel--parse-response
                :around
                #'magent-llm-gptel--sanitize-after-parse-response-a))
  (when (fboundp 'gptel-curl--parse-stream)
    (unless (advice-member-p #'magent-llm-gptel--sanitize-after-parse-stream-a
                             'gptel-curl--parse-stream)
      (advice-add 'gptel-curl--parse-stream
                  :around
                  #'magent-llm-gptel--sanitize-after-parse-stream-a))))

(defun magent-llm-gptel--make-state ()
  "Create adapter-local streaming state."
  (let ((state (make-hash-table :test #'eq)))
    (puthash :text-chunks nil state)
    (puthash :reasoning-chunks nil state)
    (puthash :reasoning-emitted nil state)
    (puthash :reasoning-ended nil state)
    (puthash :terminal-emitted nil state)
    state))

(defun magent-llm-gptel--emit (request event)
  "Emit EVENT through REQUEST's callback."
  (when-let* ((callback (magent-llm-request-callback request)))
    (funcall callback event)))

(defun magent-llm-gptel--emit-terminal (request state event)
  "Emit terminal EVENT for REQUEST once, tracking it in STATE."
  (unless (gethash :terminal-emitted state)
    (puthash :terminal-emitted t state)
    (magent-llm-gptel--emit request event)))

(defun magent-llm-gptel--streamed-text (state)
  "Return accumulated streamed text from STATE."
  (apply #'concat (nreverse (copy-sequence
                             (gethash :text-chunks state)))))

(defun magent-llm-gptel--flush-reasoning (request state info)
  "Emit cached non-streaming reasoning chunks for REQUEST.
Streaming reasoning is emitted as it arrives.  Non-streaming reasoning is
held until the adapter can distinguish actual reasoning from providers
that put the final answer only in a reasoning field."
  (unless (gethash :reasoning-emitted state)
    (let ((metadata (magent-llm-gptel--metadata info)))
      (dolist (text (nreverse (copy-sequence
                               (gethash :reasoning-chunks state))))
        (magent-llm-gptel--emit
         request
         (magent-llm-reasoning-delta-event text metadata)))
      (puthash :reasoning-emitted t state)
      (when (gethash :reasoning-ended state)
        (magent-llm-gptel--emit
         request
         (magent-llm-reasoning-end-event metadata))))))

(defun magent-llm-gptel--pending-tool-use-p (info)
  "Return non-nil when INFO contains unfinished gptel tool calls."
  (let ((tool-use (and (listp info) (plist-get info :tool-use))))
    (cond
     ((null tool-use) nil)
     ((listp tool-use)
      (cl-some (lambda (tool-call)
                 (not (plist-get tool-call :result)))
               tool-use))
     (t t))))

(defun magent-llm-gptel--final-text (state info)
  "Return final response text from STATE and gptel INFO."
  (let ((content (and (listp info) (plist-get info :content)))
        (streamed (magent-llm-gptel--streamed-text state)))
    (cond
     ((and (stringp content) (not (string-empty-p content))) content)
     ((not (string-empty-p streamed)) streamed)
     (t ""))))

(defconst magent-llm-gptel--dsml-tool-calls-open
  "<｜｜DSML｜｜tool_calls>"
  "Opening marker for textual DSML tool-call envelopes.")

(defconst magent-llm-gptel--dsml-tool-calls-close
  "</｜｜DSML｜｜tool_calls>"
  "Closing marker for textual DSML tool-call envelopes.")

(defconst magent-llm-gptel--textual-tool-call-max-length 200000
  "Maximum text length considered for textual DSML tool-call parsing.")

(defun magent-llm-gptel--parse-dsml-tag-attr
    (text pos tag attr)
  "Return (VALUE . BODY-START) for TAG ATTR in TEXT at POS, or nil."
  (let* ((prefix (format "<｜｜DSML｜｜%s " tag))
         (attr-prefix (format "%s=\"" attr))
         (tag-start (string-search prefix text pos)))
    (when (and tag-start (= tag-start pos))
      (let* ((attr-start (string-search attr-prefix text
                                         (+ tag-start (length prefix))))
             (value-start (and attr-start
                               (+ attr-start (length attr-prefix))))
             (value-end (and value-start
                             (string-search "\"" text value-start)))
             (tag-end (and value-end
                           (string-search ">" text value-end))))
        (when (and value-start value-end tag-end)
          (cons (substring text value-start value-end)
                (1+ tag-end)))))))

(defun magent-llm-gptel--parse-dsml-tool-call-params (body)
  "Return a plist of textual DSML parameter values from BODY."
  (let ((pos 0)
        (close "</｜｜DSML｜｜parameter>")
        args)
    (while (< pos (length body))
      (let ((start (string-search "<｜｜DSML｜｜parameter " body pos)))
        (if (null start)
            (setq pos (length body))
          (let* ((parsed (magent-llm-gptel--parse-dsml-tag-attr
                          body start "parameter" "name"))
                 (name (car-safe parsed))
                 (content-start (cdr-safe parsed))
                 (content-end (and content-start
                                   (string-search close body content-start))))
            (if (not (and name content-start content-end))
                (setq pos (length body))
              (setq args
                    (plist-put
                     args
                     (intern (concat ":" name))
                     (string-trim
                      (substring body content-start content-end)))
                    pos (+ content-end (length close))))))))
    args))

(defun magent-llm-gptel--parse-dsml-tool-calls (text &optional metadata)
  "Return normalized tool-call events parsed from textual DSML TEXT.
Tool-call blocks may appear as a pure envelope or embedded in surrounding
assistant prose."
  (when (and (stringp text)
             (<= (length text)
                 magent-llm-gptel--textual-tool-call-max-length))
    (let ((pos 0)
          (invoke-close "</｜｜DSML｜｜invoke>")
          events
          (index 0)
          (metadata (if (plist-member metadata :provider)
                        metadata
                      (append (list :provider 'gptel) metadata))))
      (while (< pos (length text))
        (let ((block-start
               (string-search
                magent-llm-gptel--dsml-tool-calls-open text pos)))
          (if (null block-start)
              (setq pos (length text))
            (let* ((block-body-start
                    (+ block-start
                       (length magent-llm-gptel--dsml-tool-calls-open)))
                   (block-end
                    (string-search
                     magent-llm-gptel--dsml-tool-calls-close
                     text
                     block-body-start)))
              (if (null block-end)
                  (setq pos (length text))
                (let* ((block-body (substring text block-body-start block-end))
                       (block-pos 0))
                  (while (< block-pos (length block-body))
                    (let ((start (string-search
                                  "<｜｜DSML｜｜invoke "
                                  block-body
                                  block-pos)))
                      (if (null start)
                          (setq block-pos (length block-body))
                        (let* ((parsed
                                (magent-llm-gptel--parse-dsml-tag-attr
                                 block-body start "invoke" "name"))
                               (name (car-safe parsed))
                               (body-start (cdr-safe parsed))
                               (body-end
                                (and body-start
                                     (string-search
                                      invoke-close
                                      block-body
                                      body-start))))
                          (if (not (and name body-start body-end))
                              (setq block-pos (length block-body))
                            (cl-incf index)
                            (let* ((raw-text
                                    (substring
                                     block-body
                                     start
                                     (+ body-end (length invoke-close))))
                                   (body
                                    (substring block-body
                                               body-start
                                               body-end))
                                   (args
                                    (magent-llm-gptel--parse-dsml-tool-call-params
                                     body))
                                   (id (format
                                        "textual-dsml-%d-%s"
                                        index
                                        (substring
                                         (secure-hash 'sha1 raw-text)
                                         0 10)))
                                   (raw-call (list :id id
                                                   :name name
                                                   :args args
                                                   :source 'textual-dsml)))
                              (push (magent-llm-tool-call-event
                                     id name args raw-call metadata)
                                    events)
                              (setq block-pos
                                    (+ body-end
                                       (length invoke-close)))))))))
                  (setq pos
                        (+ block-end
                           (length
                            magent-llm-gptel--dsml-tool-calls-close)))))))))
      (nreverse events))))

(defun magent-llm-gptel--emit-tool-call-batch
    (request state events metadata)
  "Emit normalized tool-call EVENTS followed by a batch-end event."
  (puthash :terminal-emitted t state)
  (dolist (event events)
    (magent-llm-gptel--emit request event))
  (magent-llm-gptel--emit
   request
   (magent-llm-tool-call-batch-end-event metadata)))

(defun magent-llm-gptel--emit-completed-or-textual-tool-calls
    (request state info text)
  "Emit completion TEXT, or convert textual DSML tool calls into tool events.
Return `tool-call' when textual tool calls were emitted, otherwise
`completed'."
  (let* ((metadata (magent-llm-gptel--metadata info))
         (strict-final
          (plist-get (magent-llm-request-metadata request)
                     :strict-final-response-retry))
         (events (unless strict-final
                   (magent-llm-gptel--parse-dsml-tool-calls
                    text metadata))))
    (if events
      (progn
        (magent-llm-gptel--flush-reasoning request state info)
        (magent-llm-gptel--emit-tool-call-batch
         request state events metadata)
        'tool-call)
      (unless (string-empty-p (or text ""))
        (magent-llm-gptel--flush-reasoning request state info))
      (magent-llm-gptel--emit-terminal
       request
       state
       (magent-llm-completed-event
        text
        (and (listp info) (plist-get info :tokens))
        (and (listp info) (plist-get info :stop-reason))
        metadata))
      'completed)))

(defun magent-llm-gptel--metadata (info)
  "Return adapter metadata extracted from gptel INFO."
  (let ((metadata (list :provider 'gptel)))
    (dolist (key '(:status :http-status :error :tokens :stop-reason))
      (when-let* ((value (plist-get info key)))
        (setq metadata (append metadata (list key value)))))
    metadata))

(defun magent-llm-gptel--tool-name (tool-spec raw-call)
  "Return the tool name from TOOL-SPEC or RAW-CALL."
  (or (and (fboundp 'gptel-tool-name)
           tool-spec
           (ignore-errors (gptel-tool-name tool-spec)))
      (plist-get raw-call :name)))

(defun magent-llm-gptel--tool-id (raw-call)
  "Return a stable id from RAW-CALL, when present."
  (or (plist-get raw-call :id)
      (plist-get raw-call :call-id)
      (plist-get raw-call :tool-call-id)))

(defun magent-llm-gptel--normalize-tool-call (call &optional metadata)
  "Convert one gptel CALL into a normalized tool-call event.
METADATA is merged into the event metadata."
  (let* ((tool-spec (nth 0 call))
         (args (nth 1 call))
         (raw-call (or (nth 3 call)
                       (list :name (magent-llm-gptel--tool-name tool-spec nil)
                             :args args)))
         (name (magent-llm-gptel--tool-name tool-spec raw-call)))
    (magent-llm-tool-call-event
     (magent-llm-gptel--tool-id raw-call)
     name
     args
     raw-call
     (if (plist-member metadata :provider)
         metadata
       (append (list :provider 'gptel) metadata)))))

(defun magent-llm-gptel--handle-tool-use (fsm)
  "Report pending gptel tool calls without executing them."
  (when-let* ((info (gptel-fsm-info fsm))
              (callback (plist-get info :callback))
              (tools (plist-get info :tools))
              (tool-use (cl-remove-if (lambda (tc) (plist-get tc :result))
                                      (plist-get info :tool-use))))
    (magent-llm-gptel--sanitize-info info)
    (let (pending-calls)
      (dolist (tool-call tool-use)
        (let* ((name (plist-get tool-call :name))
               (tool-spec (cl-find-if
                           (lambda (ts)
                             (equal (gptel-tool-name ts) name))
                           tools))
               (args (plist-get tool-call :args)))
          (push (list tool-spec args nil tool-call) pending-calls)))
      (funcall callback (cons 'tool-call (nreverse pending-calls)) info))))

(defun magent-llm-gptel--handle-done (request state buffer fsm)
  "Emit a completion if gptel reaches DONE without a final callback.
Some providers can return reasoning-only non-streaming responses.  gptel
emits the reasoning callback, then reaches DONE without invoking the
normal response callback because there is no content field.  Magent still
needs a terminal event to cancel request timeout handling and release UI
input."
  (when (and request state)
    (let ((info (gptel-fsm-info fsm)))
      (unless (or (gethash :terminal-emitted state)
                  (magent-llm-gptel--pending-tool-use-p info)
                  (and (listp info) (plist-get info :error)))
        (let ((content (and (listp info) (plist-get info :content))))
          (when (or (and (stringp content)
                         (not (string-empty-p content)))
                    (not (string-empty-p
                          (magent-llm-gptel--streamed-text state))))
            (magent-llm-gptel--flush-reasoning request state info)))
        (magent-llm-gptel--emit-completed-or-textual-tool-calls
         request
         state
         info
         (magent-llm-gptel--final-text state info))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun magent-llm-gptel--make-sampling-fsm (&optional request state buffer)
  "Create a gptel FSM for one model sampling request.
It lets gptel own transport/parsing while preventing gptel from
executing tools or continuing the tool loop."
  (gptel-make-fsm
   :table `((INIT . ((t . WAIT)))
            (WAIT . ((t . TYPE)))
            (TYPE . ((,#'gptel--error-p . ERRS)
                     (,#'gptel--tool-use-p . TOOL)
                     (t . DONE)))
            (TOOL . ((t . DONE))))
   :handlers `((WAIT ,#'gptel--handle-wait)
               (TOOL ,#'magent-llm-gptel--handle-tool-use)
               (DONE ,(apply-partially #'magent-llm-gptel--handle-done
                                        request state buffer)
                     ,#'gptel--handle-post)
               (ERRS ,#'gptel--handle-post)
               (ABRT ,#'gptel--handle-post))))

(defun magent-llm-gptel--callback (request state buffer response info)
  "Map gptel RESPONSE and INFO to normalized events for REQUEST."
  (cond
   ((stringp response)
    (if (and (not (plist-get info :stream))
             (not (magent-llm-gptel--pending-tool-use-p info)))
        (progn
          (magent-llm-gptel--emit-completed-or-textual-tool-calls
           request state info response)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
      (push response (gethash :text-chunks state))
      (unless (plist-get (magent-llm-request-metadata request)
                         :final-response-retry)
        (magent-llm-gptel--emit
         request
         (magent-llm-text-delta-event
          response
          (magent-llm-gptel--metadata info))))))
   ((and (consp response) (eq (car response) 'reasoning))
    (if (eq (cdr response) t)
        (progn
          (puthash :reasoning-ended t state)
          (when (gethash :reasoning-emitted state)
            (magent-llm-gptel--emit
             request
             (magent-llm-reasoning-end-event
              (magent-llm-gptel--metadata info)))))
      (let ((text (or (cdr response) "")))
        (push text (gethash :reasoning-chunks state))
        (when (plist-get info :stream)
          (puthash :reasoning-emitted t state)
          (magent-llm-gptel--emit
           request
           (magent-llm-reasoning-delta-event
            text
            (magent-llm-gptel--metadata info)))))))
   ((and (consp response) (eq (car response) 'tool-call))
    (magent-llm-gptel--flush-reasoning request state info)
    (let ((calls (cdr response))
          (metadata (magent-llm-gptel--metadata info)))
      (dolist (call calls)
        (magent-llm-gptel--emit
         request
         (magent-llm-gptel--normalize-tool-call call metadata)))
      (magent-llm-gptel--emit
       request
       (magent-llm-tool-call-batch-end-event metadata))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
   ((eq response t)
    (if (magent-llm-gptel--pending-tool-use-p info)
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
      (magent-llm-gptel--emit-completed-or-textual-tool-calls
       request state info (magent-llm-gptel--final-text state info))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
   ((eq response 'abort)
    (magent-llm-gptel--emit-terminal
     request
     state
     (magent-llm-error-event
      "Request aborted"
      (append (magent-llm-gptel--metadata info) '(:status abort))))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
   ((null response)
    (magent-llm-gptel--emit-terminal
     request
     state
     (magent-llm-error-event
      (or (plist-get info :status)
          (plist-get info :error)
          "gptel request failed")
      (magent-llm-gptel--metadata info)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))))

(defun magent-llm-gptel--backend-openai-responses-p (backend)
  "Return non-nil when BACKEND uses OpenAI Responses wire format."
  (and backend
       (fboundp 'gptel-openai-responses-p)
       (gptel-openai-responses-p backend)))

(defun magent-llm-gptel--backend-openai-chat-p (backend)
  "Return non-nil when BACKEND uses OpenAI-compatible chat wire format."
  (and backend
       (not (magent-llm-gptel--backend-openai-responses-p backend))
       (fboundp 'gptel-openai-p)
       (gptel-openai-p backend)))

(defun magent-llm-gptel--unsupported-effort
    (effort reason &optional fallback)
  "Handle unsupported EFFORT for REASON, optionally returning FALLBACK."
  (pcase magent-effort-unsupported-policy
    ('error
     (error "Magent effort %s is unsupported: %s" effort reason))
    ('warn-and-downgrade
     (magent-log
      "WARN effort=%s unsupported (%s)%s"
      effort reason
      (if fallback
          (format ", using %s" fallback)
        ", ignoring"))
     fallback)
    (_ nil)))

(defun magent-llm-gptel--chat-effort (effort)
  "Return OpenAI-compatible chat EFFORT, applying xhigh policy."
  (if (eq effort 'xhigh)
      (magent-llm-gptel--unsupported-effort
       effort
       "OpenAI-compatible chat requests do not advertise xhigh"
       'high)
    effort))

(defun magent-llm-gptel--effort-request-params (backend effort)
  "Return provider request params for BACKEND and EFFORT, or nil."
  (let ((normalized (magent-effort-effective effort)))
    (when normalized
      (cond
       ((magent-llm-gptel--backend-openai-responses-p backend)
        `(:reasoning (:effort ,(symbol-name normalized))))
       ((magent-llm-gptel--backend-openai-chat-p backend)
        (when-let* ((chat-effort
                    (magent-llm-gptel--chat-effort normalized)))
          `(:reasoning_effort ,(symbol-name chat-effort))))
       (t
        (magent-llm-gptel--unsupported-effort
         normalized
         "backend does not advertise a Magent effort mapping")
        nil)))))

(defun magent-llm-gptel--top-p-request-params (backend top-p)
  "Return provider request params for BACKEND and TOP-P, or nil."
  (when top-p
    (cond
     ((or (magent-llm-gptel--backend-openai-responses-p backend)
          (magent-llm-gptel--backend-openai-chat-p backend)
          (and (fboundp 'gptel-anthropic-p)
               (gptel-anthropic-p backend)))
      `(:top_p ,top-p))
     ((and (fboundp 'gptel-ollama-p)
           (gptel-ollama-p backend))
      `(:options (:top_p ,top-p)))
     ((or (and (fboundp 'gptel-gemini-p)
               (gptel-gemini-p backend))
          (and (fboundp 'gptel-bedrock-p)
               (gptel-bedrock-p backend)))
      ;; These providers nest top-p beside values that gptel constructs
      ;; later.  The managed curl boundary merges it into the final data.
      nil)
     (t
      (magent-log
       "WARN top-p=%s ignored: backend has no safe Magent mapping"
       top-p)
      nil))))

(defun magent-llm-gptel--merge-request-params (base extra)
  "Return BASE request params with EXTRA taking precedence."
  (let ((merged (copy-sequence base)))
    (cl-loop for (key value) on extra by #'cddr
             for existing = (plist-get merged key)
             do (setq merged
                      (plist-put
                       merged key
                       (if (and (consp existing)
                                (consp value)
                                (magent-json--plist-p existing)
                                (magent-json--plist-p value))
                           (magent-llm-gptel--merge-request-params
                            existing value)
                         value))))
    merged))

(defun magent-llm-gptel-sample (request)
  "Start one gptel sampling request for REQUEST.
Return the request buffer as the abort handle.  REQUEST must be a
`magent-llm-request'."
  (unless (magent-llm-request-p request)
    (error "Expected magent-llm-request, got: %S" request))
  (magent-llm-gptel--install-boundary-advice)
  (let ((buffer (generate-new-buffer " *magent-llm-gptel-request*"))
        (state (magent-llm-gptel--make-state))
        (metadata (magent-llm-request-metadata request)))
    (with-current-buffer buffer
      (when (magent-llm-request-backend request)
        (setq-local gptel-backend (magent-llm-request-backend request)))
      (when (magent-llm-request-model request)
        (setq-local gptel-model (magent-llm-request-model request)))
      (when (and (plist-member metadata :temperature)
                 (boundp 'gptel-temperature))
        (setq-local gptel-temperature (plist-get metadata :temperature)))
      (let ((sampling-params
             (magent-llm-gptel--top-p-request-params
              gptel-backend (plist-get metadata :top-p))))
        (when-let* ((effort-params
                    (magent-llm-gptel--effort-request-params
                     gptel-backend
                     (plist-get metadata :effort))))
          (setq sampling-params
                (magent-llm-gptel--merge-request-params
                 sampling-params effort-params)))
        (when sampling-params
          (setq-local gptel--request-params
                      (magent-llm-gptel--merge-request-params
                       gptel--request-params
                       sampling-params))))
      (setq-local gptel-tools (magent-llm-request-tools request))
      (setq-local gptel-use-tools
                  (and gptel-tools
                       (not (plist-get metadata :disable-provider-tools))))
      (setq-local gptel-confirm-tool-calls t)
      (when (boundp 'magent-include-reasoning)
        (setq-local gptel-include-reasoning
                    (if (plist-member metadata :include-reasoning)
                        (plist-get metadata :include-reasoning)
                      magent-include-reasoning)))
      (gptel-request
          (magent-llm-request-prompt request)
        :buffer buffer
        :context (append
                  (list :magent-llm-gptel t)
                  (when (plist-member metadata :top-p)
                    (list :top-p (plist-get metadata :top-p))))
        :system (magent-llm-request-system request)
        :stream (magent-llm-request-stream request)
        :fsm (magent-llm-gptel--make-sampling-fsm request state buffer)
        :callback (lambda (response info)
                    (magent-llm-gptel--callback
                     request state buffer response info))))
    buffer))

(provide 'magent-llm-gptel)
;;; magent-llm-gptel.el ends here
