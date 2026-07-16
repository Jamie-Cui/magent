;;; magent-lifecycle-events.el --- Lifecycle event sinks for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Event sink support for emitting machine-readable Magent lifecycle
;; events alongside the normal UI flow.

;;; Code:

(require 'cl-lib)

(cl-defstruct (magent-lifecycle-events-context
               (:constructor magent-lifecycle-events-context-create)
               (:copier nil))
  "Context associated with a Magent turn or subagent."
  turn-id
  subagent-id
  title
  ended)

(defvar magent-lifecycle-events--sinks nil
  "Registered event sink functions.
Each sink receives a plist describing one event.")

(defvar magent-lifecycle-events--current-context nil
  "Current top-level turn context.")

(defvar magent-lifecycle-events--subagent-audit-context nil
  "Dynamically captured scalar attribution for subagent creation.
This keeps the historical two-argument creation call reload-compatible.")

(defvar magent-lifecycle-events--audit-contexts
  (make-hash-table :test #'eq :weakness 'key)
  "Scalar audit attribution keyed by lifecycle context identity.
This state deliberately lives outside the struct so source reloads preserve
the historical `magent-lifecycle-events-context' layout.")

(defun magent-lifecycle-events--copy-audit-context (audit-context)
  "Return an independent scalar copy of AUDIT-CONTEXT."
  (when audit-context
    (cl-loop for (key value) on audit-context by #'cddr
             append (list key (if (stringp value)
                                  (copy-sequence value)
                                value)))))

(defun magent-lifecycle-events-context-audit-context (context)
  "Return an independent audit attribution copy for CONTEXT."
  (magent-lifecycle-events--copy-audit-context
   (gethash context magent-lifecycle-events--audit-contexts)))

(defun magent-lifecycle-events--set-audit-context (context audit-context)
  "Store an independent AUDIT-CONTEXT for CONTEXT."
  (if audit-context
      (puthash context
               (magent-lifecycle-events--copy-audit-context audit-context)
               magent-lifecycle-events--audit-contexts)
    (remhash context magent-lifecycle-events--audit-contexts))
  context)

(defun magent-lifecycle-events-generate-id ()
  "Generate a lowercase identifier suitable for Magent events."
  (let ((alphabet "abcdefghijklmnopqrstuvwxyz0123456789")
        (result (make-string 24 ?a))
        (index 0))
    (while (< index 24)
      (aset result index (aref alphabet (random (length alphabet))))
      (setq index (1+ index)))
    result))

(defun magent-lifecycle-events-add-sink (sink)
  "Register SINK as an event sink."
  (cl-pushnew sink magent-lifecycle-events--sinks :test #'eq)
  sink)

(defun magent-lifecycle-events-remove-sink (sink)
  "Unregister SINK from the event sink list."
  (setq magent-lifecycle-events--sinks (delq sink magent-lifecycle-events--sinks)))

(defun magent-lifecycle-events-clear-sinks ()
  "Remove all registered sinks."
  (setq magent-lifecycle-events--sinks nil))

(defun magent-lifecycle-events--dispatch (event)
  "Dispatch EVENT plist to all registered sinks."
  (dolist (sink magent-lifecycle-events--sinks)
    (condition-case err
        (funcall sink event)
      (error
       (message "magent-lifecycle-events sink error: %s"
                (error-message-string err))))))

(defun magent-lifecycle-events-emit (type &rest props)
  "Emit an event of TYPE with PROPS.
TYPE is stored as `:type'.  PROPS should be a plist.  When no explicit
`:context' is provided, the current turn context is used."
  (let* ((context (or (plist-get props :context)
                      magent-lifecycle-events--current-context))
         (event (append (list :type type
                              :time (float-time))
                        (when context
                          (list :context context
                                :turn-id (magent-lifecycle-events-context-turn-id context)
                                :subagent-id (magent-lifecycle-events-context-subagent-id context)))
                        props)))
    (magent-lifecycle-events--dispatch event)))

(defun magent-lifecycle-events-begin-turn (&optional title)
  "Create and activate a new top-level turn context with TITLE."
  (let ((context (magent-lifecycle-events-context-create
                  :turn-id (magent-lifecycle-events-generate-id)
                  :title title)))
    (setq magent-lifecycle-events--current-context context)
    (magent-lifecycle-events-emit 'turn-start :context context :title title)
    context))

(defun magent-lifecycle-events-end-turn (context status &optional detail)
  "End CONTEXT with STATUS and optional DETAIL.
STATUS should be one of `completed', `failed', or `cancelled'."
  (unless (or (null context)
              (magent-lifecycle-events-context-ended context))
    (setf (magent-lifecycle-events-context-ended context) t)
    (magent-lifecycle-events-emit 'turn-end
                        :context context
                        :status status
                        :detail detail)
    (when (eq context magent-lifecycle-events--current-context)
      (setq magent-lifecycle-events--current-context nil))))

(defun magent-lifecycle-events-create-subagent-context
    (&optional title parent-context audit-context)
  "Create and emit a subagent context with TITLE.
PARENT-CONTEXT defaults to the current context.  AUDIT-CONTEXT is an
optional immutable scalar attribution plist carried to the later stop event."
  (let* ((audit-context
          (or audit-context magent-lifecycle-events--subagent-audit-context))
         (parent (or parent-context magent-lifecycle-events--current-context))
         (context (magent-lifecycle-events-context-create
                   :turn-id (and parent (magent-lifecycle-events-context-turn-id parent))
                   :subagent-id (magent-lifecycle-events-generate-id)
                   :title title)))
    (when audit-context
      (let ((snapshot
             (magent-lifecycle-events--copy-audit-context audit-context)))
        (setq snapshot
              (plist-put snapshot :turn-id
                         (magent-lifecycle-events-context-turn-id context))
              snapshot
              (plist-put snapshot :subagent-id
                         (magent-lifecycle-events-context-subagent-id context)))
        (magent-lifecycle-events--set-audit-context context snapshot)))
    (magent-lifecycle-events-emit
     'subagent-start :context context :title title
     :audit-context (magent-lifecycle-events-context-audit-context context))
    context))

(defun magent-lifecycle-events-stop-subagent (context)
  "Emit a stop event for subagent CONTEXT."
  (unless (or (null context)
              (magent-lifecycle-events-context-ended context))
    (setf (magent-lifecycle-events-context-ended context) t)
    (magent-lifecycle-events-emit
     'subagent-stop :context context
     :audit-context (magent-lifecycle-events-context-audit-context context))))

(provide 'magent-lifecycle-events)
;;; magent-lifecycle-events.el ends here
