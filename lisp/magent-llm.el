;;; magent-llm.el --- Provider-neutral LLM request events  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Internal request and event shapes used by the Magent-owned agent
;; loop.  Provider adapters, such as a gptel-request adapter, should map
;; their backend-specific callbacks into these values so the loop does
;; not depend on provider FSM internals.

;;; Code:

(require 'cl-lib)

(defconst magent-llm-event-types
  '(text-delta
    reasoning-delta
    reasoning-end
    tool-call
    tool-call-batch-end
    completed
    error
    usage)
  "Valid normalized LLM event types.")

(cl-defstruct (magent-llm-request
               (:constructor magent-llm-request--create)
               (:copier nil))
  prompt
  system
  tools
  model
  backend
  stream
  callback
  metadata)

(cl-defstruct (magent-llm-event
               (:constructor magent-llm-event--create)
               (:copier nil))
  type
  text
  id
  name
  arguments
  raw
  usage
  stop-reason
  message
  metadata)

(defvar magent-llm--event-result-callbacks
  (make-hash-table :test #'eq :weakness 'key)
  "Runtime result callbacks keyed by tool-call event identity.")

(defvar magent-llm--event-continuations
  (make-hash-table :test #'eq :weakness 'key)
  "Runtime provider continuations keyed by batch-end event identity.")

(defun magent-llm-event-result-callback (event)
  "Return EVENT's runtime tool-result callback, if any."
  (gethash event magent-llm--event-result-callbacks))

(defun magent-llm-event-set-result-callback (event callback)
  "Set EVENT's runtime tool-result CALLBACK and return it."
  (if callback
      (puthash event callback magent-llm--event-result-callbacks)
    (remhash event magent-llm--event-result-callbacks))
  callback)

(defun magent-llm-event-continuation (event)
  "Return EVENT's runtime provider continuation, if any."
  (gethash event magent-llm--event-continuations))

(defun magent-llm-event-set-continuation (event continuation)
  "Set EVENT's runtime provider CONTINUATION and return it."
  (if continuation
      (puthash event continuation magent-llm--event-continuations)
    (remhash event magent-llm--event-continuations))
  continuation)

(defun magent-llm-event-type-p (type)
  "Return non-nil when TYPE is a valid normalized LLM event type."
  (memq type magent-llm-event-types))

(defun magent-llm--coerce-event-type (type)
  "Return TYPE as a valid event type symbol, or signal an error."
  (let ((symbol (cond
                 ((symbolp type) type)
                 ((stringp type) (intern type))
                 (t type))))
    (unless (magent-llm-event-type-p symbol)
      (error "Invalid LLM event type: %S" type))
    symbol))

(defun magent-llm-request-create (&rest args)
  "Create a `magent-llm-request' from keyword ARGS.
Recognized keys are `:prompt', `:system', `:tools', `:model',
`:backend', `:stream', `:callback', and `:metadata'."
  (let ((callback (plist-get args :callback)))
    (when (and callback (not (functionp callback)))
      (error "LLM request callback is not callable: %S" callback))
    (magent-llm-request--create
     :prompt (plist-get args :prompt)
     :system (plist-get args :system)
     :tools (plist-get args :tools)
     :model (plist-get args :model)
     :backend (plist-get args :backend)
     :stream (plist-get args :stream)
     :callback callback
     :metadata (plist-get args :metadata))))

(defun magent-llm-event-create (type &rest props)
  "Create a normalized `magent-llm-event' of TYPE with PROPS."
  (let ((event
         (magent-llm-event--create
          :type (magent-llm--coerce-event-type type)
          :text (plist-get props :text)
          :id (plist-get props :id)
          :name (plist-get props :name)
          :arguments (plist-get props :arguments)
          :raw (plist-get props :raw)
          :usage (plist-get props :usage)
          :stop-reason (plist-get props :stop-reason)
          :message (plist-get props :message)
          :metadata (plist-get props :metadata))))
    (magent-llm-event-set-result-callback
     event (plist-get props :result-callback))
    (magent-llm-event-set-continuation
     event (plist-get props :continuation))
    event))

(defun magent-llm-text-delta-event (text &optional metadata)
  "Create a text delta event for TEXT and optional METADATA."
  (magent-llm-event-create 'text-delta :text text :metadata metadata))

(defun magent-llm-reasoning-delta-event (text &optional metadata)
  "Create a reasoning delta event for TEXT and optional METADATA."
  (magent-llm-event-create 'reasoning-delta :text text :metadata metadata))

(defun magent-llm-reasoning-end-event (&optional metadata)
  "Create a reasoning end event with optional METADATA."
  (magent-llm-event-create 'reasoning-end :metadata metadata))

(defun magent-llm-tool-call-event
    (id name arguments &optional raw metadata result-callback)
  "Create a tool call event with ID, NAME, ARGUMENTS, RAW, and METADATA.
RESULT-CALLBACK, when non-nil, accepts the model-visible tool result so the
provider adapter can retain its native continuation context."
  (magent-llm-event-create 'tool-call
                           :id id
                           :name name
                           :arguments arguments
                           :raw raw
                           :metadata metadata
                           :result-callback result-callback))

(defun magent-llm-tool-call-batch-end-event (&optional metadata continuation)
  "Create a tool-call batch-end event with METADATA and CONTINUATION.
CONTINUATION, when non-nil, resumes the provider request after every tool result
callback in the batch has been called."
  (magent-llm-event-create 'tool-call-batch-end
                           :metadata metadata
                           :continuation continuation))

(defun magent-llm-completed-event
    (&optional text usage stop-reason metadata continuation)
  "Create a completed event with TEXT, USAGE, STOP-REASON, and METADATA."
  (magent-llm-event-create 'completed
                           :text text
                           :usage usage
                           :stop-reason stop-reason
                           :metadata metadata
                           :continuation continuation))

(defun magent-llm-error-event (message &optional metadata)
  "Create an error event with MESSAGE and optional METADATA."
  (magent-llm-event-create 'error :message message :metadata metadata))

(defun magent-llm-event-to-plist (event)
  "Convert EVENT to a plist without nil-valued optional keys."
  (let ((plist (list :type (magent-llm-event-type event))))
    (dolist (slot '((:text . magent-llm-event-text)
                    (:id . magent-llm-event-id)
                    (:name . magent-llm-event-name)
                    (:arguments . magent-llm-event-arguments)
                    (:raw . magent-llm-event-raw)
                    (:usage . magent-llm-event-usage)
                    (:stop-reason . magent-llm-event-stop-reason)
                    (:message . magent-llm-event-message)
                    (:metadata . magent-llm-event-metadata)
                    (:result-callback . magent-llm-event-result-callback)
                    (:continuation . magent-llm-event-continuation)))
      (let ((value (funcall (cdr slot) event)))
        (when value
          (setq plist (append plist (list (car slot) value))))))
    plist))

(defun magent-llm-event-from-plist (plist)
  "Create a `magent-llm-event' from PLIST."
  (let ((type (plist-get plist :type))
        (copy (copy-sequence plist))
        result)
    (while copy
      (let ((key (pop copy))
            (value (pop copy)))
        (unless (eq key :type)
          (setq result (append result (list key value))))))
    (apply #'magent-llm-event-create type result)))

(provide 'magent-llm)
;;; magent-llm.el ends here
