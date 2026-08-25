;;; magent-sampling-gptel.el --- gptel-request adapter for Magent sampling events  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Thin adapter from `gptel-request' callbacks to Magent's normalized
;; `magent-sampling-event' protocol.  This module is the only intended place
;; for gptel FSM/callback details; the Magent-owned agent loop consumes
;; normalized events instead of gptel internals.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-util)
(require 'gptel)
(require 'gptel-request)
(require 'magent-config)
(require 'magent-json)
(require 'magent-sampling)
(require 'magent-protocol)

(declare-function gptel-openai-p "gptel-openai" t t)
(declare-function gptel-openai-responses-p "gptel-openai" t t)
(declare-function gptel-anthropic-p "gptel-anthropic" t t)
(declare-function gptel-bedrock-p "gptel-bedrock" t t)
(declare-function gptel-gemini-p "gptel-gemini" t t)
(declare-function gptel-ollama-p "gptel-ollama" t t)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-tools)
(defvar gptel-use-tools)
(defvar gptel-confirm-tool-calls)
(defvar gptel-include-reasoning)
(defvar gptel-temperature)
(defvar gptel--known-backends)
(defvar gptel-proxy)
(defvar magent-include-reasoning)

(defconst magent-sampling-gptel-auto-model-id "auto"
  "ACP model id that clears an explicit Magent session route.")

(defun magent-sampling-gptel--model-name (model)
  "Return gptel MODEL's provider-facing name as a string."
  (if (fboundp 'gptel--model-name)
      (gptel--model-name model)
    (format "%s" model)))

(defun magent-sampling-gptel--route-id (backend-name model)
  "Return an opaque model id for BACKEND-NAME and MODEL."
  (format "gptel/%s/%s"
          (url-hexify-string (format "%s" backend-name))
          (url-hexify-string (magent-sampling-gptel--model-name model))))

(defun magent-sampling-gptel--model-description (backend-name model)
  "Return a concise safe description for MODEL on BACKEND-NAME."
  (let* ((description (get model :description))
         (capabilities (get model :capabilities))
         (context-window (get model :context-window))
         (details
          (delq nil
                (list
                 (and capabilities
                      (format "capabilities: %s"
                              (mapconcat (lambda (value) (format "%s" value))
                                         capabilities ", ")))
                 (and context-window
                      (format "context: %sk" context-window))))))
    (string-join
     (delq nil
           (list (and (stringp description) description)
                 (and details (string-join details "; "))
                 (unless (or description details)
                   (format "gptel model from %s" backend-name))))
     " — ")))

(defun magent-sampling-gptel--descriptor (backend-name backend model)
  "Return a safe model descriptor for BACKEND-NAME, BACKEND, and MODEL."
  (magent-model-descriptor-create
   :id (magent-sampling-gptel--route-id backend-name model)
   :name (format "%s:%s" backend-name
                 (magent-sampling-gptel--model-name model))
   :description (magent-sampling-gptel--model-description backend-name model)
   :backend-name (format "%s" backend-name)
   :backend backend
   :model model
   :capabilities (copy-sequence (get model :capabilities))))

(defun magent-sampling-gptel-model-catalog ()
  "Return descriptors for every model registered with gptel.
gptel currently exposes backend lookup and model accessors publicly but uses
the private `gptel--known-backends' registry for its own cross-provider model
picker.  Keep that private dependency isolated and fail clearly if its shape
changes."
  (unless (boundp 'gptel--known-backends)
    (error "Installed gptel does not expose its backend registry"))
  (unless (and (proper-list-p gptel--known-backends)
               (cl-every #'consp gptel--known-backends))
    (error "Unsupported gptel backend registry shape"))
  (let ((seen (make-hash-table :test #'equal))
        descriptors)
    (dolist (entry gptel--known-backends)
      (let ((backend-name (car entry))
            (backend (cdr entry)))
        (when (gptel-backend-p backend)
          (dolist (model (gptel-backend-models backend))
            (let* ((descriptor
                    (magent-sampling-gptel--descriptor
                     backend-name backend model))
                   (id (magent-model-descriptor-id descriptor)))
              (unless (gethash id seen)
                (puthash id t seen)
                (push descriptor descriptors)))))))
    (nreverse descriptors)))

(defun magent-sampling-gptel-model-descriptor (model-id)
  "Return the gptel model descriptor for MODEL-ID, or nil."
  (cl-find model-id (magent-sampling-gptel-model-catalog)
           :key #'magent-model-descriptor-id :test #'equal))

(defun magent-sampling-gptel-descriptor-route (descriptor &optional source)
  "Return a model route for DESCRIPTOR attributed to SOURCE."
  (unless (magent-model-descriptor-p descriptor)
    (error "Expected Magent model descriptor, got: %S" descriptor))
  (magent-model-route-create
   :backend (magent-model-descriptor-backend descriptor)
   :model (magent-model-descriptor-model descriptor)
   :source (or source 'session)))

(defun magent-sampling-gptel-descriptor-for-route (route)
  "Return the catalog descriptor matching ROUTE, or nil."
  (when (magent-model-route-p route)
    (cl-find-if
     (lambda (descriptor)
       (and (eq (magent-model-descriptor-backend descriptor)
                (magent-model-route-backend route))
            (equal (magent-model-descriptor-model descriptor)
                   (magent-model-route-model route))))
     (magent-sampling-gptel-model-catalog))))

(defun magent-sampling-gptel-default-route ()
  "Return the current global gptel route."
  (magent-model-route-create
   :backend (default-value 'gptel-backend)
   :model (default-value 'gptel-model)
   :source 'gptel))

(defun magent-sampling-gptel-validate-route (route)
  "Return ROUTE when its backend and model remain registered and compatible."
  (unless (magent-model-route-p route)
    (error "Expected Magent model route, got: %S" route))
  (let ((backend (magent-model-route-backend route))
        (model (magent-model-route-model route)))
    (unless (gptel-backend-p backend)
      (error "Magent model route has no valid gptel backend"))
    (unless model
      (error "Magent model route has no model"))
    (unless (member model (gptel-backend-models backend))
      (error "Magent model %s is unavailable on gptel backend %s"
             model (gptel-backend-name backend)))
    (unless (magent-sampling-gptel-descriptor-for-route route)
      (error "Magent backend/model route is no longer registered with gptel: %s:%s"
             (gptel-backend-name backend) model)))
  route)

(defun magent-sampling-gptel-route-tool-capability (route)
  "Return `supported', `unsupported', or `unknown' for ROUTE tool use."
  (magent-sampling-gptel-validate-route route)
  (let* ((model (magent-model-route-model route))
         (properties (and (symbolp model) (symbol-plist model))))
    (if (and (symbolp model)
             (plist-member properties :capabilities))
        (if (cl-some (lambda (capability)
                       (member (format "%s" capability)
                               '("tool-use" "tool" "tools"
                                 "function-calling")))
                     (get model :capabilities))
            'supported
          'unsupported)
      'unknown)))

(defun magent-sampling-gptel--managed-context-p (context)
  "Return non-nil when gptel CONTEXT belongs to this adapter."
  (and (listp context)
       (or (plist-get context :magent-sampling-gptel)
           (plist-get context :magent-managed))))

(defun magent-sampling-gptel--managed-info-p (info)
  "Return non-nil when gptel INFO belongs to this adapter."
  (and (listp info)
       (or (plist-get info :magent-sampling-gptel)
           (plist-get info :magent-managed)
           (magent-sampling-gptel--managed-context-p
            (plist-get info :context)))))

(defun magent-sampling-gptel--sanitize-provider-tool-args (args)
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

(defun magent-sampling-gptel--sanitize-tool-call (tool-call)
  "Sanitize one gptel TOOL-CALL plist in place and return it."
  (when (listp tool-call)
    (when (plist-member tool-call :name)
      (plist-put tool-call
                 :name
                 (magent-json-safe-name (plist-get tool-call :name))))
    (when (plist-member tool-call :args)
      (plist-put tool-call
                 :args
                 (magent-sampling-gptel--sanitize-provider-tool-args
                  (plist-get tool-call :args)))))
  tool-call)

(defun magent-sampling-gptel--sanitize-tool-use (info)
  "Sanitize gptel INFO's `:tool-use' values in place."
  (when-let* ((tool-use (and (listp info) (plist-get info :tool-use))))
    (dolist (tool-call tool-use)
      (magent-sampling-gptel--sanitize-tool-call tool-call))))

(defun magent-sampling-gptel--sanitize-assistant-tool-calls (info)
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
                                        (magent-sampling-gptel--sanitize-provider-tool-args
                                         arguments))))))))))))

(defun magent-sampling-gptel--sanitize-info (info)
  "Sanitize gptel INFO structures that may be serialized as JSON."
  (magent-sampling-gptel--sanitize-tool-use info)
  (magent-sampling-gptel--sanitize-assistant-tool-calls info)
  info)

(defun magent-sampling-gptel--put-nested-param
    (data section key value)
  "Set SECTION's KEY to VALUE in provider request DATA."
  (let ((params (copy-sequence (plist-get data section))))
    (plist-put data section (plist-put params key value))))

(defun magent-sampling-gptel--apply-top-p-to-info (info)
  "Apply Magent top-p context to gptel request INFO in wire format."
  (let* ((context (plist-get info :context))
         (top-p (and (listp context)
                     (plist-member context :top-p)
                     (plist-get context :top-p)))
         (backend (plist-get info :backend))
         (data (plist-get info :data)))
    (when (and top-p (listp data))
      (cond
       ((or (magent-sampling-gptel--backend-openai-responses-p backend)
            (magent-sampling-gptel--backend-openai-chat-p backend)
            (and (fboundp 'gptel-anthropic-p)
                 (gptel-anthropic-p backend)))
        (plist-put info :data (plist-put data :top_p top-p)))
       ((and (fboundp 'gptel-ollama-p)
             (gptel-ollama-p backend))
        (plist-put info :data
                   (magent-sampling-gptel--put-nested-param
                    data :options :top_p top-p)))
       ((and (fboundp 'gptel-gemini-p)
             (gptel-gemini-p backend))
        (plist-put info :data
                   (magent-sampling-gptel--put-nested-param
                    data :generationConfig :topP top-p)))
       ((and (fboundp 'gptel-bedrock-p)
             (gptel-bedrock-p backend))
        (plist-put info :data
                   (magent-sampling-gptel--put-nested-param
                    data :inferenceConfig :topP top-p))))))
  info)

(defun magent-sampling-gptel--reset-reasoning-block-a (fsm)
  "Reset managed gptel FSM reasoning state before a request starts."
  (when-let* ((info (and (fboundp 'gptel-fsm-info)
                        (gptel-fsm-info fsm))))
    (when (and (magent-sampling-gptel--managed-info-p info)
               (plist-get info :reasoning-block))
      (plist-put info :reasoning-block nil))))

(defun magent-sampling-gptel--sanitize-before-curl-a (orig-fn info &rest args)
  "Sanitize Magent-managed INFO before gptel serializes request data."
  (when (magent-sampling-gptel--managed-info-p info)
    (magent-sampling-gptel--apply-top-p-to-info info)
    (magent-sampling-gptel--sanitize-info info))
  (apply orig-fn info args))

(defun magent-sampling-gptel--suppress-connect-headers-a
    (orig-fn info &rest args)
  "Suppress proxy CONNECT headers for Magent-managed curl requests."
  (let ((curl-args (apply orig-fn info args)))
    (if (and (magent-sampling-gptel--managed-info-p info)
             (stringp gptel-proxy)
             (not (string-empty-p gptel-proxy))
             (not (member "--suppress-connect-headers" curl-args)))
        (append curl-args '("--suppress-connect-headers"))
      curl-args)))

(defun magent-sampling-gptel--sanitize-after-parse-response-a
    (orig-fn backend response info)
  "Sanitize Magent-managed INFO after gptel parses a response."
  (prog1 (funcall orig-fn backend response info)
    (when (magent-sampling-gptel--managed-info-p info)
      (magent-sampling-gptel--sanitize-info info))))

(defun magent-sampling-gptel--sanitize-after-parse-stream-a
    (orig-fn backend info)
  "Sanitize Magent-managed INFO after gptel parses a stream chunk."
  (prog1 (funcall orig-fn backend info)
    (when (magent-sampling-gptel--managed-info-p info)
      (magent-sampling-gptel--sanitize-info info))))

(defun magent-sampling-gptel--curl-provider-error (buffer info)
  "Return a structured provider error from curl response BUFFER.
INFO is gptel request metadata containing the curl write-out UUID.  Try each
HTTP header boundary before the write-out marker so proxy and redirect header
blocks do not hide an otherwise valid JSON error response."
  (when-let* (((buffer-live-p buffer))
              (uuid (plist-get info :uuid)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (when (search-backward uuid nil t)
          (let ((response-end (max (point-min) (1- (point)))))
            (goto-char (point-min))
            (catch 'provider-error
              (while (re-search-forward "\r?\n\r?\n" response-end t)
                (let* ((body (string-trim
                              (buffer-substring-no-properties
                               (point) response-end)))
                       (response
                        (and (not (string-empty-p body))
                             (ignore-errors
                               (gptel--json-read-string body))))
                       (provider-error
                        (and response
                             (gptel--parse-response-error response))))
                  (when provider-error
                    (throw 'provider-error provider-error)))))))))))

(defun magent-sampling-gptel--capture-curl-provider-error (buffer info)
  "Capture a structured provider error from curl BUFFER into gptel INFO."
  (when-let* ((provider-error
               (magent-sampling-gptel--curl-provider-error buffer info)))
    (if (plist-member info :magent-provider-error)
        (plist-put info :magent-provider-error provider-error)
      (nconc info (list :magent-provider-error provider-error))))
  info)

(defun magent-sampling-gptel--capture-curl-error-before-cleanup-a
    (process _status)
  "Capture a Magent provider error before gptel destroys PROCESS state."
  (when-let* ((requests
               (and (boundp 'gptel--request-alist)
                    (symbol-value 'gptel--request-alist)))
              (entry (alist-get process requests))
              (fsm (car entry))
              (info (gptel-fsm-info fsm))
              ((magent-sampling-gptel--managed-info-p info))
              (http-status (plist-get info :http-status))
              ((not (member http-status '("100" "200")))))
    (magent-sampling-gptel--capture-curl-provider-error
     (process-buffer process) info)))

(defun magent-sampling-gptel--install-boundary-advice ()
  "Install adapter-local gptel boundary sanitization advice."
  (unless (advice-member-p #'magent-sampling-gptel--reset-reasoning-block-a
                           'gptel--handle-wait)
    (advice-add 'gptel--handle-wait
                :before #'magent-sampling-gptel--reset-reasoning-block-a))
  (unless (advice-member-p #'magent-sampling-gptel--sanitize-before-curl-a
                           'gptel-curl--get-args)
    (advice-add 'gptel-curl--get-args
                :around #'magent-sampling-gptel--sanitize-before-curl-a))
  (unless (advice-member-p #'magent-sampling-gptel--suppress-connect-headers-a
                           'gptel-curl--get-config-args)
    (advice-add 'gptel-curl--get-config-args
                :around
                #'magent-sampling-gptel--suppress-connect-headers-a))
  (unless (advice-member-p #'magent-sampling-gptel--sanitize-after-parse-response-a
                           'gptel--parse-response)
    (advice-add 'gptel--parse-response
                :around
                #'magent-sampling-gptel--sanitize-after-parse-response-a))
  (when (fboundp 'gptel-curl--parse-stream)
    (unless (advice-member-p #'magent-sampling-gptel--sanitize-after-parse-stream-a
                             'gptel-curl--parse-stream)
      (advice-add 'gptel-curl--parse-stream
                  :around
                  #'magent-sampling-gptel--sanitize-after-parse-stream-a)))
  (when (fboundp 'gptel-curl--stream-cleanup)
    (unless
        (advice-member-p
         #'magent-sampling-gptel--capture-curl-error-before-cleanup-a
         'gptel-curl--stream-cleanup)
      (advice-add
       'gptel-curl--stream-cleanup
       :before #'magent-sampling-gptel--capture-curl-error-before-cleanup-a))))

(defun magent-sampling-gptel--make-state ()
  "Create adapter-local streaming state."
  (let ((state (make-hash-table :test #'eq)))
    (puthash :text-chunks nil state)
    (puthash :reasoning-chunks nil state)
    (puthash :reasoning-emitted nil state)
    (puthash :reasoning-ended nil state)
    (puthash :terminal-emitted nil state)
    state))

(defun magent-sampling-gptel--emit (request event)
  "Emit EVENT through REQUEST's callback."
  (when-let* ((callback (magent-sampling-request-callback request)))
    (funcall callback event)))

(defun magent-sampling-gptel--emit-terminal (request state event)
  "Emit terminal EVENT for REQUEST once, tracking it in STATE."
  (unless (gethash :terminal-emitted state)
    (puthash :terminal-emitted t state)
    (magent-sampling-gptel--emit request event)))

(defun magent-sampling-gptel--streamed-text (state)
  "Return accumulated streamed text from STATE."
  (apply #'concat (nreverse (copy-sequence
                             (gethash :text-chunks state)))))

(defun magent-sampling-gptel--flush-reasoning (request state info)
  "Emit cached non-streaming reasoning chunks for REQUEST.
Streaming reasoning is emitted as it arrives.  Non-streaming reasoning is
held until the adapter can distinguish actual reasoning from providers
that put the final answer only in a reasoning field."
  (unless (gethash :reasoning-emitted state)
    (let ((metadata (magent-sampling-gptel--metadata info)))
      (dolist (text (nreverse (copy-sequence
                               (gethash :reasoning-chunks state))))
        (magent-sampling-gptel--emit
         request
         (magent-sampling-reasoning-delta-event text metadata)))
      (puthash :reasoning-emitted t state)
      (when (gethash :reasoning-ended state)
        (magent-sampling-gptel--emit
         request
         (magent-sampling-reasoning-end-event metadata))))))

(defun magent-sampling-gptel--pending-tool-use-p (info)
  "Return non-nil when INFO contains unfinished gptel tool calls."
  (let ((tool-use (and (listp info) (plist-get info :tool-use))))
    (cond
     ((null tool-use) nil)
     ((listp tool-use)
      (cl-some (lambda (tool-call)
                 (not (plist-get tool-call :result)))
               tool-use))
     (t t))))

(defun magent-sampling-gptel--final-text (state info)
  "Return final response text from STATE and gptel INFO."
  (let ((content (and (listp info) (plist-get info :content)))
        (streamed (magent-sampling-gptel--streamed-text state)))
    (cond
     ((and (stringp content) (not (string-empty-p content))) content)
     ((not (string-empty-p streamed)) streamed)
     (t ""))))

(defconst magent-sampling-gptel--dsml-tool-calls-open
  "<｜｜DSML｜｜tool_calls>"
  "Opening marker for textual DSML tool-call envelopes.")

(defconst magent-sampling-gptel--dsml-tool-calls-close
  "</｜｜DSML｜｜tool_calls>"
  "Closing marker for textual DSML tool-call envelopes.")

(defconst magent-sampling-gptel--textual-tool-call-max-length 200000
  "Maximum text length considered for textual DSML tool-call parsing.")

(defun magent-sampling-gptel--parse-dsml-tag-attr
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

(defun magent-sampling-gptel--parse-dsml-tool-call-params (body)
  "Return a plist of textual DSML parameter values from BODY."
  (let ((pos 0)
        (close "</｜｜DSML｜｜parameter>")
        args)
    (while (< pos (length body))
      (let ((start (string-search "<｜｜DSML｜｜parameter " body pos)))
        (if (null start)
            (setq pos (length body))
          (let* ((parsed (magent-sampling-gptel--parse-dsml-tag-attr
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

(defun magent-sampling-gptel--parse-dsml-tool-calls (text &optional metadata)
  "Return normalized tool-call events parsed from textual DSML TEXT.
Tool-call blocks may appear as a pure envelope or embedded in surrounding
assistant prose."
  (when (and (stringp text)
             (<= (length text)
                 magent-sampling-gptel--textual-tool-call-max-length))
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
                magent-sampling-gptel--dsml-tool-calls-open text pos)))
          (if (null block-start)
              (setq pos (length text))
            (let* ((block-body-start
                    (+ block-start
                       (length magent-sampling-gptel--dsml-tool-calls-open)))
                   (block-end
                    (string-search
                     magent-sampling-gptel--dsml-tool-calls-close
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
                                (magent-sampling-gptel--parse-dsml-tag-attr
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
                                    (magent-sampling-gptel--parse-dsml-tool-call-params
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
                              (push (magent-sampling-tool-call-event
                                     id name args raw-call metadata)
                                    events)
                              (setq block-pos
                                    (+ body-end
                                       (length invoke-close)))))))))
                  (setq pos
                        (+ block-end
                           (length
                            magent-sampling-gptel--dsml-tool-calls-close)))))))))
      (nreverse events))))

(defun magent-sampling-gptel--emit-tool-call-batch
    (request state events metadata &optional continuation)
  "Emit normalized tool-call EVENTS followed by a batch-end event."
  (puthash :terminal-emitted t state)
  (dolist (event events)
    (magent-sampling-gptel--emit request event))
  (magent-sampling-gptel--emit
   request
   (magent-sampling-tool-call-batch-end-event metadata continuation)))

(defun magent-sampling-gptel--record-textual-tool-result (record result)
  "Store model-visible RESULT in textual tool RECORD."
  (plist-put record :output
             (if (magent-tool-result-p result)
                 (magent-tool-result-output-string result)
               (gptel--to-string result)))
  (plist-put record :done t))

(defun magent-sampling-gptel--textual-tool-result-message (records)
  "Return an OpenAI chat user message containing textual tool RECORDS."
  (list
   :role "user"
   :content
   (concat
    "The previous model response encoded tool requests as assistant text "
    "instead of using the provider's tool-call protocol.  Magent recovered "
    "and executed those requests.  Continue from these results, and use the "
    "native tool-call protocol for any further tools.\n\n"
    (magent-json-encode
     (list
      :recovered_textual_tool_results
      (vconcat
       (mapcar
        (lambda (record)
          (list :id (plist-get record :id)
                :name (plist-get record :name)
                :arguments
                (magent-json-safe-value (plist-get record :arguments))
                :output (plist-get record :output)))
        records)))))))

(defun magent-sampling-gptel--openai-chat-continuation-supported-p (fsm info)
  "Return non-nil when FSM and INFO support native OpenAI chat continuation."
  (let* ((backend (and (listp info) (plist-get info :backend)))
         (data (and (listp info) (plist-get info :data)))
         (messages (and (listp data) (plist-get data :messages))))
    (and fsm
         backend
         (fboundp 'gptel-openai-p)
         (gptel-openai-p backend)
         (vectorp messages))))

(defun magent-sampling-gptel--continue-with-user-message (fsm state message)
  "Append OpenAI chat MESSAGE to FSM's context and continue it."
  (let ((info (gptel-fsm-info fsm)))
    (gptel--inject-prompt
     (plist-get info :backend)
     (plist-get info :data)
     message)
    (plist-put info :magent-after-tool-output t)
    (magent-sampling-gptel--reset-sample-state state info)
    (gptel--fsm-transition fsm 'WAIT)))

(defun magent-sampling-gptel--continue-textual-tool-use
    (fsm state records)
  "Append textual tool RECORDS to FSM's native context and continue it."
  (unless (cl-every (lambda (record) (plist-get record :done)) records)
    (error "Cannot continue gptel request before all textual tools finish"))
  (magent-sampling-gptel--continue-with-user-message
   fsm state (magent-sampling-gptel--textual-tool-result-message records)))

(defun magent-sampling-gptel--prepare-textual-continuation
    (fsm state info events)
  "Attach result callbacks to textual EVENTS and return their continuation."
  (when (magent-sampling-gptel--openai-chat-continuation-supported-p fsm info)
    (let ((records
           (mapcar
            (lambda (event)
              (list :id (magent-sampling-event-id event)
                    :name (magent-sampling-event-name event)
                    :arguments (magent-sampling-event-arguments event)
                    :output nil
                    :done nil))
            events))
          resumed)
      (cl-mapc
       (lambda (event record)
         (magent-sampling-event-set-result-callback
          event
          (apply-partially
           #'magent-sampling-gptel--record-textual-tool-result record)))
       events records)
      (lambda ()
        (unless resumed
          (unless (cl-every (lambda (record) (plist-get record :done))
                            records)
            (error
             "Cannot continue gptel request before all textual tools finish"))
          (setq resumed t)
          (magent-sampling-gptel--continue-textual-tool-use
           fsm state records))))))

(defun magent-sampling-gptel--emit-completed-or-textual-tool-calls
    (request state info text &optional fsm)
  "Emit completion TEXT, or convert textual DSML tool calls into tool events.
Return a symbol describing completion, including whether the native provider
context remains paused for Magent recovery."
  (let* ((metadata (magent-sampling-gptel--metadata info))
         (events (magent-sampling-gptel--parse-dsml-tool-calls text metadata)))
    (if events
        (let ((continuation
               (magent-sampling-gptel--prepare-textual-continuation
                fsm state info events)))
          (magent-sampling-gptel--flush-reasoning request state info)
          (magent-sampling-gptel--emit-tool-call-batch
           request state events
           (append metadata '(:source textual-dsml))
           continuation)
          (if continuation 'tool-call-paused 'tool-call))
      (unless (string-empty-p (or text ""))
        (magent-sampling-gptel--flush-reasoning request state info))
      (magent-sampling-gptel--emit-terminal
       request
       state
       (magent-sampling-completed-event
        text
        (and (listp info) (plist-get info :tokens))
        (and (listp info) (plist-get info :stop-reason))
        metadata))
      'completed)))

(defun magent-sampling-gptel--metadata (info)
  "Return adapter metadata extracted from gptel INFO."
  (let ((metadata (list :provider 'gptel)))
    (dolist (key '(:status :http-status :error :tokens :stop-reason))
      (when-let* ((value (plist-get info key)))
        (setq metadata (append metadata (list key value)))))
    (when-let* ((provider-error (plist-get info :magent-provider-error)))
      (setq metadata
            (append metadata
                    (list :provider-error
                          (magent-json-safe-value provider-error)))))
    metadata))

(defun magent-sampling-gptel--provider-error-message (provider-error)
  "Return a concise message for structured PROVIDER-ERROR."
  (cond
   ((stringp provider-error) provider-error)
   ((listp provider-error)
    (let ((message (or (plist-get provider-error :message)
                       (plist-get provider-error :detail))))
      (if (stringp message)
          message
        (format "%S" provider-error))))
   (provider-error (format "%S" provider-error))))

(defun magent-sampling-gptel--error-message (info)
  "Return the most useful provider error message from gptel INFO."
  (let ((captured-error (plist-get info :magent-provider-error))
        (provider-error (plist-get info :error))
        (status (plist-get info :status)))
    (cond
     (captured-error
      (magent-sampling-gptel--provider-error-message captured-error))
     (provider-error
      (magent-sampling-gptel--provider-error-message provider-error))
     (status (format "%s" status))
     (t "gptel request failed"))))

(defun magent-sampling-gptel--tool-name (tool-spec raw-call)
  "Return the tool name from TOOL-SPEC or RAW-CALL."
  (or (and (fboundp 'gptel-tool-name)
           tool-spec
           (ignore-errors (gptel-tool-name tool-spec)))
      (plist-get raw-call :name)))

(defun magent-sampling-gptel--tool-id (raw-call)
  "Return a stable id from RAW-CALL, when present."
  (or (plist-get raw-call :id)
      (plist-get raw-call :call-id)
      (plist-get raw-call :tool-call-id)))

(defun magent-sampling-gptel--normalize-tool-call (call &optional metadata)
  "Convert one gptel CALL into a normalized tool-call event.
METADATA is merged into the event metadata."
  (let* ((tool-spec (nth 0 call))
         (args (nth 1 call))
         (raw-call (or (nth 3 call)
                       (list :name (magent-sampling-gptel--tool-name tool-spec nil)
                             :args args)))
         (name (magent-sampling-gptel--tool-name tool-spec raw-call)))
    (magent-sampling-tool-call-event
     (magent-sampling-gptel--tool-id raw-call)
     name
     args
     raw-call
     (if (plist-member metadata :provider)
         metadata
       (append (list :provider 'gptel) metadata))
     (nth 2 call))))

(defun magent-sampling-gptel--record-tool-result
    (info tool-spec tool-call result)
  "Record RESULT for TOOL-CALL in gptel INFO without resuming the request."
  (let ((result (if (magent-tool-result-p result)
                    (magent-tool-result-output-string result)
                  (gptel--to-string result))))
    (push (list tool-spec (plist-get tool-call :args) result)
          (plist-get info :tool-result))
    (plist-put tool-call :result result)))

(defun magent-sampling-gptel--reset-sample-state (state &optional info)
  "Reset adapter STATE and optional gptel INFO before continuation."
  (puthash :text-chunks nil state)
  (puthash :reasoning-chunks nil state)
  (puthash :reasoning-emitted nil state)
  (puthash :reasoning-ended nil state)
  (puthash :terminal-emitted nil state)
  (when (listp info)
    (plist-put info :content nil)
    (plist-put info :stop-reason nil)))

(defun magent-sampling-gptel--continue-tool-use (fsm state)
  "Inject completed tool results into FSM and continue its provider request."
  (let* ((info (gptel-fsm-info fsm))
         (tool-use (plist-get info :tool-use)))
    (unless tool-use
      (error "No gptel tool calls are available to continue"))
    (unless (cl-every (lambda (tool-call)
                        (plist-member tool-call :result))
                      tool-use)
      (error "Cannot continue gptel request before all tools finish"))
    (gptel--inject-prompt
     (plist-get info :backend)
     (plist-get info :data)
     (gptel--parse-tool-results (plist-get info :backend) tool-use))
    (plist-put info :tool-pending nil)
    (plist-put info :magent-tool-continuation nil)
    (plist-put info :magent-after-tool-output t)
    (magent-sampling-gptel--reset-sample-state state info)
    (gptel--fsm-transition fsm 'WAIT)))

(defun magent-sampling-gptel--handle-tool-use (state fsm)
  "Report pending gptel tool calls and pause FSM for Magent execution."
  (when-let* ((info (gptel-fsm-info fsm))
              (callback (plist-get info :callback))
              (tools (plist-get info :tools))
              (tool-use (cl-remove-if (lambda (tc) (plist-get tc :result))
                                      (plist-get info :tool-use))))
    (magent-sampling-gptel--sanitize-info info)
    (let (pending-calls
          resumed)
      (dolist (tool-call tool-use)
        (let* ((name (plist-get tool-call :name))
               (tool-spec (cl-find-if
                           (lambda (ts)
                             (equal (gptel-tool-name ts) name))
                           tools))
               (args (plist-get tool-call :args))
               (result-callback
                (apply-partially #'magent-sampling-gptel--record-tool-result
                                 info tool-spec tool-call)))
          (push (list tool-spec args result-callback tool-call)
                pending-calls)))
      (plist-put info :tool-pending t)
      (plist-put
       info :magent-tool-continuation
       (lambda ()
         (unless resumed
           (setq resumed t)
           (magent-sampling-gptel--continue-tool-use fsm state))))
      (funcall callback (cons 'tool-call (nreverse pending-calls)) info))))

(defun magent-sampling-gptel--handle-done (request state buffer fsm)
  "Emit a completion if gptel reaches DONE without a final callback.
Some providers can return reasoning-only non-streaming responses.  gptel
emits the reasoning callback, then reaches DONE without invoking the
normal response callback because there is no content field.  Magent still
needs a terminal event to cancel request timeout handling and release UI
input."
  (when (and request state)
    (let ((info (gptel-fsm-info fsm)))
      (unless (or (gethash :terminal-emitted state)
                  (magent-sampling-gptel--pending-tool-use-p info)
                  (and (listp info) (plist-get info :error)))
        (let ((content (and (listp info) (plist-get info :content))))
          (when (or (and (stringp content)
                         (not (string-empty-p content)))
                    (not (string-empty-p
                          (magent-sampling-gptel--streamed-text state))))
            (magent-sampling-gptel--flush-reasoning request state info)))
        (unless
            (memq
             (magent-sampling-gptel--emit-completed-or-textual-tool-calls
              request
              state
              info
              (magent-sampling-gptel--final-text state info)
              fsm)
             '(tool-call-paused completed-paused))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(defun magent-sampling-gptel--make-sampling-fsm (&optional request state buffer)
  "Create a gptel FSM for one model sampling request.
gptel owns transport and its native request data.  Magent pauses the FSM at
tool calls, executes them itself, and explicitly resumes the same context."
  (gptel-make-fsm
   :table `((INIT . ((t . WAIT)))
            (WAIT . ((t . TYPE)))
            (TYPE . ((,#'gptel--error-p . ERRS)
                     (,#'gptel--tool-use-p . TOOL)
                     (t . DONE)))
            (TOOL . ((t . DONE))))
   :handlers `((WAIT ,#'gptel--handle-wait)
               (TOOL ,(apply-partially
                       #'magent-sampling-gptel--handle-tool-use state))
               (DONE ,(apply-partially #'magent-sampling-gptel--handle-done
                                        request state buffer)
                     ,#'gptel--handle-post)
               (ERRS ,#'gptel--handle-post)
               (ABRT ,#'gptel--handle-post))))

(defun magent-sampling-gptel--callback
    (request state buffer response info &optional fsm)
  "Map gptel RESPONSE and INFO to normalized events for REQUEST."
  (cond
   ((stringp response)
    (if (and (not (plist-get info :stream))
             (not (magent-sampling-gptel--pending-tool-use-p info)))
        (unless
            (memq
             (magent-sampling-gptel--emit-completed-or-textual-tool-calls
              request state info response fsm)
             '(tool-call-paused completed-paused))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
      (push response (gethash :text-chunks state))
      (magent-sampling-gptel--emit
       request
       (magent-sampling-text-delta-event
        response
        (magent-sampling-gptel--metadata info)))))
   ((and (consp response) (eq (car response) 'reasoning))
    (if (eq (cdr response) t)
        (progn
          (puthash :reasoning-ended t state)
          (when (gethash :reasoning-emitted state)
            (magent-sampling-gptel--emit
             request
             (magent-sampling-reasoning-end-event
              (magent-sampling-gptel--metadata info)))))
      (let ((text (or (cdr response) "")))
        (push text (gethash :reasoning-chunks state))
        (when (plist-get info :stream)
          (puthash :reasoning-emitted t state)
          (magent-sampling-gptel--emit
           request
           (magent-sampling-reasoning-delta-event
            text
            (magent-sampling-gptel--metadata info)))))))
   ((and (consp response) (eq (car response) 'tool-call))
    (magent-sampling-gptel--flush-reasoning request state info)
    (let ((calls (cdr response))
          (metadata (magent-sampling-gptel--metadata info)))
      (dolist (call calls)
        (magent-sampling-gptel--emit
         request
         (magent-sampling-gptel--normalize-tool-call call metadata)))
      (magent-sampling-gptel--emit
       request
       (magent-sampling-tool-call-batch-end-event
        metadata
        (plist-get info :magent-tool-continuation)))))
   ((eq response t)
    (if (magent-sampling-gptel--pending-tool-use-p info)
        nil
      (unless
          (memq
           (magent-sampling-gptel--emit-completed-or-textual-tool-calls
            request state info (magent-sampling-gptel--final-text state info) fsm)
           '(tool-call-paused completed-paused))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))))
   ((eq response 'abort)
    (magent-sampling-gptel--emit-terminal
     request
     state
     (magent-sampling-error-event
      "Request aborted"
      (append (magent-sampling-gptel--metadata info) '(:status abort))))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
   ((null response)
    (magent-sampling-gptel--emit-terminal
     request
     state
     (magent-sampling-error-event
      (magent-sampling-gptel--error-message info)
      (magent-sampling-gptel--metadata info)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))))

(defun magent-sampling-gptel--backend-openai-responses-p (backend)
  "Return non-nil when BACKEND uses OpenAI Responses wire format."
  (and backend
       (fboundp 'gptel-openai-responses-p)
       (gptel-openai-responses-p backend)))

(defun magent-sampling-gptel--backend-openai-chat-p (backend)
  "Return non-nil when BACKEND uses OpenAI-compatible chat wire format."
  (and backend
       (not (magent-sampling-gptel--backend-openai-responses-p backend))
       (fboundp 'gptel-openai-p)
       (gptel-openai-p backend)))

(defun magent-sampling-gptel--unsupported-effort
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

(defun magent-sampling-gptel--chat-effort (effort)
  "Return OpenAI-compatible chat EFFORT, applying xhigh policy."
  (if (eq effort 'xhigh)
      (magent-sampling-gptel--unsupported-effort
       effort
       "OpenAI-compatible chat requests do not advertise xhigh"
       'high)
    effort))

(defun magent-sampling-gptel--effort-request-params (backend effort)
  "Return provider request params for BACKEND and EFFORT, or nil."
  (let ((normalized (magent-effort-effective effort)))
    (when normalized
      (cond
       ((magent-sampling-gptel--backend-openai-responses-p backend)
        `(:reasoning (:effort ,(symbol-name normalized))))
       ((magent-sampling-gptel--backend-openai-chat-p backend)
        (when-let* ((chat-effort
                    (magent-sampling-gptel--chat-effort normalized)))
          `(:reasoning_effort ,(symbol-name chat-effort))))
       (t
        (magent-sampling-gptel--unsupported-effort
         normalized
         "backend does not advertise a Magent effort mapping")
        nil)))))

(defun magent-sampling-gptel--top-p-request-params (backend top-p)
  "Return provider request params for BACKEND and TOP-P, or nil."
  (when top-p
    (cond
     ((or (magent-sampling-gptel--backend-openai-responses-p backend)
          (magent-sampling-gptel--backend-openai-chat-p backend)
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

(defun magent-sampling-gptel--merge-request-params (base extra)
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
                           (magent-sampling-gptel--merge-request-params
                            existing value)
                         value))))
    merged))

(defun magent-sampling-gptel-sample (request)
  "Start one gptel sampling request for REQUEST.
Return the request buffer as the abort handle.  REQUEST must be a
`magent-sampling-request'."
  (unless (magent-sampling-request-p request)
    (error "Expected magent-sampling-request, got: %S" request))
  (magent-sampling-gptel--install-boundary-advice)
  (let ((buffer (generate-new-buffer " *magent-sampling-gptel-request*"))
        (state (magent-sampling-gptel--make-state))
        (metadata (magent-sampling-request-metadata request)))
    (with-current-buffer buffer
      (when (magent-sampling-request-backend request)
        (setq-local gptel-backend (magent-sampling-request-backend request)))
      (when (magent-sampling-request-model request)
        (setq-local gptel-model (magent-sampling-request-model request)))
      (when (and (plist-member metadata :temperature)
                 (boundp 'gptel-temperature))
        (setq-local gptel-temperature (plist-get metadata :temperature)))
      (let ((sampling-params
             (magent-sampling-gptel--top-p-request-params
              gptel-backend (plist-get metadata :top-p))))
        (when-let* ((effort-params
                    (magent-sampling-gptel--effort-request-params
                     gptel-backend
                     (plist-get metadata :effort))))
          (setq sampling-params
                (magent-sampling-gptel--merge-request-params
                 sampling-params effort-params)))
        (when sampling-params
          (setq-local gptel--request-params
                      (magent-sampling-gptel--merge-request-params
                       gptel--request-params
                       sampling-params))))
      (setq-local gptel-tools (magent-sampling-request-tools request))
      (setq-local gptel-use-tools
                  (and gptel-tools
                       (not (plist-get metadata :disable-provider-tools))))
      (setq-local gptel-confirm-tool-calls t)
      (when (boundp 'magent-include-reasoning)
        (setq-local gptel-include-reasoning
                    (if (plist-member metadata :include-reasoning)
                        (plist-get metadata :include-reasoning)
                      magent-include-reasoning)))
      (let ((fsm (magent-sampling-gptel--make-sampling-fsm
                  request state buffer)))
        (gptel-request
            (magent-sampling-request-prompt request)
          :buffer buffer
          :context (append
                    (list :magent-sampling-gptel t)
                    (when (plist-member metadata :top-p)
                      (list :top-p (plist-get metadata :top-p))))
          :system (magent-sampling-request-system request)
          :stream (magent-sampling-request-stream request)
          :fsm fsm
          :callback (lambda (response info)
                      (magent-sampling-gptel--callback
                       request state buffer response info fsm)))))
    buffer))

(provide 'magent-sampling-gptel)
;;; magent-sampling-gptel.el ends here
