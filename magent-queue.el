;;; magent-queue.el --- Processing lock for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Processing lock for serializing concurrent requests.  When a prompt
;; arrives while an LLM request is in flight it is immediately rejected
;; with a message rather than silently buffered.
;;
;; The lock is a global singleton because magent has a single output
;; buffer and single processing slot.  Queue items carry the prompt
;; text, the entry-point symbol (for display), and a timestamp.

;;; Code:

(require 'cl-lib)
(require 'magent-config)

;;; Queue item structure

(cl-defstruct (magent-queue-item
               (:constructor magent-queue-item-create)
               (:copier nil))
  "A prompt being dispatched or about to be dispatched."
  (prompt nil :type string)
  (display nil :type (or string null))
  (source 'prompt :type symbol)
  (skills nil :type list)
  agent
  request-context
  capability-resolution
  (timestamp 0.0 :type float))

;;; Module state

(defvar magent-queue--processing nil
  "Non-nil while an LLM request is in flight.")

;; Forward declaration: magent-ui.el provides this function.
(declare-function magent-ui--run-item "magent-ui")

;;; Public predicates

(defun magent-queue-processing-p ()
  "Return non-nil if a request is currently being processed."
  magent-queue--processing)

;;; Public mutators

(defun magent-queue-enqueue
    (prompt source &optional display skills agent request-context capability-resolution)
  "Dispatch PROMPT immediately, or reject with a message if busy.
SOURCE is a symbol identifying the calling command.
DISPLAY is the text to show in the user message heading; defaults to PROMPT.
SKILLS is a list of skill name strings selected via slash commands.
AGENT is an optional `magent-agent-info' override for this request.
REQUEST-CONTEXT is an optional structured context plist captured from
the originating buffer.
CAPABILITY-RESOLUTION is an optional precomputed resolver result for
this turn.
Returns nil always (never queued)."
  (if magent-queue--processing
      (message "Magent: busy — wait for the current request to finish")
    (magent-queue--dispatch (magent-queue-item-create
                             :prompt prompt
                             :display display
                             :source source
                             :skills skills
                             :agent agent
                             :request-context request-context
                             :capability-resolution capability-resolution
                             :timestamp (float-time))))
  nil)

(defun magent-queue-dequeue-and-run ()
  "Release the processing lock.
Called by `magent-ui--finish-processing' and `magent-interrupt'."
  (setq magent-queue--processing nil))

(defun magent-queue-clear ()
  "No-op: kept for API compatibility (queue no longer buffers items)."
  nil)

;;; Private dispatch

(defun magent-queue--dispatch (item)
  "Set the processing lock and hand ITEM off to the UI layer.
Must only be called when `magent-queue--processing' is nil.
Uses `run-at-time' to defer dispatch so callers finish their
stack frame before UI mutations happen."
  (setq magent-queue--processing t)
  (run-at-time 0 nil #'magent-ui--run-item item))

(provide 'magent-queue)
;;; magent-queue.el ends here
