;;; magent-llm.el --- Provider-neutral LLM request events  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

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
   :metadata (plist-get props :metadata)))

(defun magent-llm-text-delta-event (text &optional metadata)
  "Create a text delta event for TEXT and optional METADATA."
  (magent-llm-event-create 'text-delta :text text :metadata metadata))

(defun magent-llm-reasoning-delta-event (text &optional metadata)
  "Create a reasoning delta event for TEXT and optional METADATA."
  (magent-llm-event-create 'reasoning-delta :text text :metadata metadata))

(defun magent-llm-reasoning-end-event (&optional metadata)
  "Create a reasoning end event with optional METADATA."
  (magent-llm-event-create 'reasoning-end :metadata metadata))

(defun magent-llm-tool-call-event (id name arguments &optional raw metadata)
  "Create a tool call event with ID, NAME, ARGUMENTS, RAW, and METADATA."
  (magent-llm-event-create 'tool-call
                           :id id
                           :name name
                           :arguments arguments
                           :raw raw
                           :metadata metadata))

(defun magent-llm-tool-call-batch-end-event (&optional metadata)
  "Create a tool-call batch-end event with optional METADATA."
  (magent-llm-event-create 'tool-call-batch-end :metadata metadata))

(defun magent-llm-completed-event (&optional text usage stop-reason metadata)
  "Create a completed event with TEXT, USAGE, STOP-REASON, and METADATA."
  (magent-llm-event-create 'completed
                           :text text
                           :usage usage
                           :stop-reason stop-reason
                           :metadata metadata))

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
                    (:metadata . magent-llm-event-metadata)))
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
