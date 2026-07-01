;;; magent-llm-gptel.el --- gptel-request adapter for Magent LLM events  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

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
(require 'magent-llm)

(declare-function gptel--error-p "gptel-request")
(declare-function gptel--handle-post "gptel-request")
(declare-function gptel--handle-wait "gptel-request")
(declare-function gptel--tool-use-p "gptel-request")
(declare-function gptel-fsm-info "gptel-request")
(declare-function gptel-make-fsm "gptel-request")
(declare-function gptel-tool-name "gptel-request")
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-tools)
(defvar gptel-use-tools)
(defvar gptel-confirm-tool-calls)
(defvar gptel-include-reasoning)
(defvar gptel-temperature)
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
                 (magent-json-safe-tool-args
                  (plist-get tool-call :args)))))
  tool-call)

(defun magent-llm-gptel--sanitize-tool-use (info)
  "Sanitize gptel INFO's `:tool-use' values in place."
  (when-let ((tool-use (and (listp info) (plist-get info :tool-use))))
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
                                        (magent-json-safe-tool-args
                                         arguments))))))))))))

(defun magent-llm-gptel--sanitize-info (info)
  "Sanitize gptel INFO structures that may be serialized as JSON."
  (magent-llm-gptel--sanitize-tool-use info)
  (magent-llm-gptel--sanitize-assistant-tool-calls info)
  info)

(defun magent-llm-gptel--reset-reasoning-block-a (fsm)
  "Reset managed gptel FSM reasoning state before a request starts."
  (when-let ((info (and (fboundp 'gptel-fsm-info)
                        (gptel-fsm-info fsm))))
    (when (and (magent-llm-gptel--managed-info-p info)
               (plist-get info :reasoning-block))
      (plist-put info :reasoning-block nil))))

(defun magent-llm-gptel--sanitize-before-curl-a (orig-fn info &rest args)
  "Sanitize Magent-managed INFO before gptel serializes request data."
  (when (magent-llm-gptel--managed-info-p info)
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
  (when-let ((callback (magent-llm-request-callback request)))
    (funcall callback event)))

(defun magent-llm-gptel--emit-terminal (request state event)
  "Emit terminal EVENT for REQUEST once, tracking it in STATE."
  (unless (gethash :terminal-emitted state)
    (puthash :terminal-emitted t state)
    (magent-llm-gptel--emit request event)))

(defun magent-llm-gptel--reasoning-text (state)
  "Return accumulated reasoning text from STATE."
  (apply #'concat (nreverse (copy-sequence
                             (gethash :reasoning-chunks state)))))

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
        (streamed (magent-llm-gptel--streamed-text state))
        (reasoning (magent-llm-gptel--reasoning-text state)))
    (cond
     ((and (stringp content) (not (string-empty-p content))) content)
     ((not (string-empty-p streamed)) streamed)
     ((not (string-empty-p reasoning)) reasoning)
     (t ""))))

(defun magent-llm-gptel--metadata (info)
  "Return adapter metadata extracted from gptel INFO."
  (let ((metadata (list :provider 'gptel)))
    (dolist (key '(:status :http-status :error :tokens :stop-reason))
      (when-let ((value (plist-get info key)))
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
        (magent-llm-gptel--emit-terminal
         request
         state
         (magent-llm-completed-event
          (magent-llm-gptel--final-text state info)
          (and (listp info) (plist-get info :tokens))
          (and (listp info) (plist-get info :stop-reason))
          (magent-llm-gptel--metadata info)))
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
          (magent-llm-gptel--flush-reasoning request state info)
          (magent-llm-gptel--emit-terminal
           request
           state
           (magent-llm-completed-event
            response
            (plist-get info :tokens)
            (plist-get info :stop-reason)
            (magent-llm-gptel--metadata info)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
      (push response (gethash :text-chunks state))
      (magent-llm-gptel--emit
       request
       (magent-llm-text-delta-event
        response
        (magent-llm-gptel--metadata info)))))
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
      (magent-llm-gptel--flush-reasoning request state info)
      (magent-llm-gptel--emit-terminal
       request
       state
       (magent-llm-completed-event
        (magent-llm-gptel--final-text state info)
        (plist-get info :tokens)
        (plist-get info :stop-reason)
        (magent-llm-gptel--metadata info)))
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
      (setq-local gptel-tools (magent-llm-request-tools request))
      (setq-local gptel-use-tools (and gptel-tools t))
      (setq-local gptel-confirm-tool-calls t)
      (when (boundp 'magent-include-reasoning)
        (setq-local gptel-include-reasoning magent-include-reasoning))
      (gptel-request
          (magent-llm-request-prompt request)
        :buffer buffer
        :context (list :magent-llm-gptel t)
        :system (magent-llm-request-system request)
        :stream (magent-llm-request-stream request)
        :fsm (magent-llm-gptel--make-sampling-fsm request state buffer)
        :callback (lambda (response info)
                    (magent-llm-gptel--callback
                     request state buffer response info))))
    buffer))

(provide 'magent-llm-gptel)
;;; magent-llm-gptel.el ends here
