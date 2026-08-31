;;; magent-sampling.el --- Provider-neutral sampling request/event protocol  -*- lexical-binding: t; -*-

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

(defconst magent-sampling-event-types
  '(text-delta
    reasoning-delta
    reasoning-end
    tool-call
    tool-call-batch-end
    completed
    error
    usage)
  "Valid normalized sampling event types.")

(cl-defstruct (magent-model-route
               (:constructor magent-model-route-create)
               (:copier nil))
  "One immutable provider/model selection for a sampling request.
BACKEND is a provider adapter object and MODEL is its provider-facing model
identifier.  SOURCE records which policy layer selected the route.  The
optional PROFILE-AGENT and PHASE fields make the same contract reusable by
future per-agent and per-phase request builders."
  backend
  model
  source
  profile-agent
  phase)

(cl-defstruct (magent-model-descriptor
               (:constructor magent-model-descriptor-create)
               (:copier nil))
  "Frontend-safe metadata for one selectable model route.
ID is an opaque stable identifier.  NAME and DESCRIPTION are display values.
BACKEND-NAME is safe durable metadata, while BACKEND is the live adapter
object used only inside the runtime.  CAPABILITIES is provider metadata and
must never contain credentials or backend connection settings."
  id
  name
  description
  backend-name
  backend
  model
  capabilities)

(defun magent-model-route-relabel (route source &optional profile-agent phase)
  "Return a new ROUTE attributed to SOURCE, PROFILE-AGENT, and PHASE."
  (unless (magent-model-route-p route)
    (error "Expected Magent model route, got: %S" route))
  (magent-model-route-create
   :backend (magent-model-route-backend route)
   :model (magent-model-route-model route)
   :source source
   :profile-agent profile-agent
   :phase phase))

(cl-defstruct (magent-sampling-request
               (:constructor magent-sampling-request--create)
               (:copier nil))
  prompt
  system
  tools
  model
  backend
  stream
  callback
  metadata)

(cl-defstruct (magent-sampling-event
               (:constructor magent-sampling-event--create)
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

(defvar magent-sampling--event-result-callbacks
  (make-hash-table :test #'eq :weakness 'key)
  "Runtime result callbacks keyed by tool-call event identity.")

(defvar magent-sampling--event-continuations
  (make-hash-table :test #'eq :weakness 'key)
  "Runtime provider continuations keyed by batch-end event identity.")

(defun magent-sampling-event-result-callback (event)
  "Return EVENT's runtime tool-result callback, if any."
  (gethash event magent-sampling--event-result-callbacks))

(defun magent-sampling-event-set-result-callback (event callback)
  "Set EVENT's runtime tool-result CALLBACK and return it."
  (if callback
      (puthash event callback magent-sampling--event-result-callbacks)
    (remhash event magent-sampling--event-result-callbacks))
  callback)

(defun magent-sampling-event-continuation (event)
  "Return EVENT's runtime provider continuation, if any."
  (gethash event magent-sampling--event-continuations))

(defun magent-sampling-event-set-continuation (event continuation)
  "Set EVENT's runtime provider CONTINUATION and return it."
  (if continuation
      (puthash event continuation magent-sampling--event-continuations)
    (remhash event magent-sampling--event-continuations))
  continuation)

(defun magent-sampling-event-type-p (type)
  "Return non-nil when TYPE is a valid normalized sampling event type."
  (memq type magent-sampling-event-types))

(defun magent-sampling--coerce-event-type (type)
  "Return TYPE as a valid event type symbol, or signal an error."
  (let ((symbol (cond
                 ((symbolp type) type)
                 ((stringp type) (intern type))
                 (t type))))
    (unless (magent-sampling-event-type-p symbol)
      (error "Invalid sampling event type: %S" type))
    symbol))

(defun magent-sampling-request-create (&rest args)
  "Create a `magent-sampling-request' from keyword ARGS.
Recognized keys are `:prompt', `:system', `:tools', `:model',
`:backend', `:stream', `:callback', and `:metadata'."
  (let ((callback (plist-get args :callback)))
    (when (and callback (not (functionp callback)))
      (error "Sampling request callback is not callable: %S" callback))
    (magent-sampling-request--create
     :prompt (plist-get args :prompt)
     :system (plist-get args :system)
     :tools (plist-get args :tools)
     :model (plist-get args :model)
     :backend (plist-get args :backend)
     :stream (plist-get args :stream)
     :callback callback
     :metadata (plist-get args :metadata))))

(defun magent-sampling-event-create (type &rest props)
  "Create a normalized `magent-sampling-event' of TYPE with PROPS."
  (let ((event
         (magent-sampling-event--create
          :type (magent-sampling--coerce-event-type type)
          :text (plist-get props :text)
          :id (plist-get props :id)
          :name (plist-get props :name)
          :arguments (plist-get props :arguments)
          :raw (plist-get props :raw)
          :usage (plist-get props :usage)
          :stop-reason (plist-get props :stop-reason)
          :message (plist-get props :message)
          :metadata (plist-get props :metadata))))
    (magent-sampling-event-set-result-callback
     event (plist-get props :result-callback))
    (magent-sampling-event-set-continuation
     event (plist-get props :continuation))
    event))

(defun magent-sampling-text-delta-event (text &optional metadata)
  "Create a text delta event for TEXT and optional METADATA."
  (magent-sampling-event-create 'text-delta :text text :metadata metadata))

(defun magent-sampling-reasoning-delta-event (text &optional metadata)
  "Create a reasoning delta event for TEXT and optional METADATA."
  (magent-sampling-event-create 'reasoning-delta :text text :metadata metadata))

(defun magent-sampling-reasoning-end-event (&optional metadata)
  "Create a reasoning end event with optional METADATA."
  (magent-sampling-event-create 'reasoning-end :metadata metadata))

(defun magent-sampling-tool-call-event
    (id name arguments &optional raw metadata result-callback)
  "Create a tool call event with ID, NAME, ARGUMENTS, RAW, and METADATA.
RESULT-CALLBACK, when non-nil, accepts the model-visible tool result so the
provider adapter can retain its native continuation context."
  (magent-sampling-event-create 'tool-call
                           :id id
                           :name name
                           :arguments arguments
                           :raw raw
                           :metadata metadata
                           :result-callback result-callback))

(defun magent-sampling-tool-call-batch-end-event (&optional metadata continuation)
  "Create a tool-call batch-end event with METADATA and CONTINUATION.
CONTINUATION, when non-nil, resumes the provider request after every tool result
callback in the batch has been called."
  (magent-sampling-event-create 'tool-call-batch-end
                           :metadata metadata
                           :continuation continuation))

(defun magent-sampling-completed-event (&optional text usage stop-reason metadata)
  "Create a completed event with TEXT, USAGE, STOP-REASON, and METADATA."
  (magent-sampling-event-create 'completed
                           :text text
                           :usage usage
                           :stop-reason stop-reason
                           :metadata metadata))

(defun magent-sampling-error-event (message &optional metadata)
  "Create an error event with MESSAGE and optional METADATA."
  (magent-sampling-event-create 'error :message message :metadata metadata))

(defun magent-sampling-event-to-plist (event)
  "Convert EVENT to a plist without nil-valued optional keys."
  (let ((plist (list :type (magent-sampling-event-type event))))
    (dolist (slot '((:text . magent-sampling-event-text)
                    (:id . magent-sampling-event-id)
                    (:name . magent-sampling-event-name)
                    (:arguments . magent-sampling-event-arguments)
                    (:raw . magent-sampling-event-raw)
                    (:usage . magent-sampling-event-usage)
                    (:stop-reason . magent-sampling-event-stop-reason)
                    (:message . magent-sampling-event-message)
                    (:metadata . magent-sampling-event-metadata)
                    (:result-callback . magent-sampling-event-result-callback)
                    (:continuation . magent-sampling-event-continuation)))
      (let ((value (funcall (cdr slot) event)))
        (when value
          (setq plist (append plist (list (car slot) value))))))
    plist))

(defun magent-sampling-event-from-plist (plist)
  "Create a `magent-sampling-event' from PLIST."
  (let ((type (plist-get plist :type))
        (copy (copy-sequence plist))
        result)
    (while copy
      (let ((key (pop copy))
            (value (pop copy)))
        (unless (eq key :type)
          (setq result (append result (list key value))))))
    (apply #'magent-sampling-event-create type result)))

(provide 'magent-sampling)
;;; magent-sampling.el ends here
