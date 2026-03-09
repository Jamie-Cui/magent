;;; magent-queue.el --- Prompt queue for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Prompt queue for serializing concurrent requests.  When a prompt
;; arrives while an LLM request is in flight the item is pushed onto
;; a FIFO queue.  After each request completes (or is interrupted) the
;; next queued item is dispatched automatically.
;;
;; The queue is a global singleton because magent has a single output
;; buffer and single processing slot.  Queue items carry the prompt
;; text, the entry-point symbol (for display), and a timestamp.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'magent-config)

;;; Queue item structure

(cl-defstruct (magent-queue-item
               (:constructor magent-queue-item-create)
               (:copier nil))
  "A pending prompt waiting to be dispatched."
  (prompt nil :type string)
  (source 'prompt :type symbol)
  (timestamp 0.0 :type float))

;;; Module state

(defvar magent-queue--items nil
  "FIFO list of `magent-queue-item' structs.  Car is next to dispatch.")

(defvar magent-queue--processing nil
  "Non-nil while an LLM request is in flight.")

;; Forward declaration: magent-ui.el provides this function.
(declare-function magent-ui--run-item "magent-ui")

;;; Public predicates and accessors

(defun magent-queue-processing-p ()
  "Return non-nil if a request is currently being processed."
  magent-queue--processing)

(defun magent-queue-length ()
  "Return the number of items currently queued (not counting active)."
  (length magent-queue--items))

(defun magent-queue-items ()
  "Return a snapshot list of pending `magent-queue-item' structs."
  (copy-sequence magent-queue--items))

;;; Public mutators

(defun magent-queue-enqueue (prompt source)
  "Enqueue PROMPT from SOURCE or dispatch immediately if idle.
SOURCE is a symbol identifying the calling command.
Returns t if the item was queued (caller should notify user).
Returns nil if dispatched immediately (no notification needed).
Signals an error when the queue is full."
  (cond
   ((not magent-queue--processing)
    (magent-queue--dispatch (magent-queue-item-create
                             :prompt prompt
                             :source source
                             :timestamp (float-time)))
    nil)
   ((>= (length magent-queue--items) magent-queue-max-size)
    (error "Magent queue is full (%d items); try again later"
           magent-queue-max-size))
   (t
    (setq magent-queue--items
          (nconc magent-queue--items
                 (list (magent-queue-item-create
                        :prompt prompt
                        :source source
                        :timestamp (float-time)))))
    (magent-log "INFO queue: enqueued [%s] depth=%d"
                source (length magent-queue--items))
    t)))

(defun magent-queue-dequeue-and-run ()
  "Release the processing lock and dispatch the next item if any.
Called by `magent-ui--finish-processing' and `magent-interrupt'."
  (setq magent-queue--processing nil)
  (when magent-queue--items
    (let ((next (car magent-queue--items)))
      (setq magent-queue--items (cdr magent-queue--items))
      (magent-log "INFO queue: dequeuing [%s] remaining=%d"
                  (magent-queue-item-source next)
                  (length magent-queue--items))
      (magent-queue--dispatch next))))

(defun magent-queue-clear ()
  "Discard all queued items.  Does not abort the current request."
  (let ((n (length magent-queue--items)))
    (setq magent-queue--items nil)
    (when (> n 0)
      (magent-log "INFO queue: cleared %d pending item(s)" n))))

(defun magent-queue-remove-nth (n)
  "Remove the item at 0-based index N from the queue.
Signals an error if N is out of range."
  (let ((len (length magent-queue--items)))
    (when (or (< n 0) (>= n len))
      (error "Magent queue index %d out of range (queue has %d items)" n len))
    (setq magent-queue--items
          (append (seq-take magent-queue--items n)
                  (seq-drop magent-queue--items (1+ n))))
    (magent-log "INFO queue: removed item at index %d, remaining=%d"
                n (length magent-queue--items))))

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
