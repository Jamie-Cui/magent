;;; magent-events.el --- Event sink support for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; Event sink support for emitting machine-readable Magent lifecycle
;; events alongside the normal UI flow.

;;; Code:

(require 'cl-lib)

(cl-defstruct (magent-events-context
               (:constructor magent-events-context-create)
               (:copier nil))
  "Context associated with a Magent turn or subagent."
  turn-id
  subagent-id
  title
  ended)

(defvar magent-events--sinks nil
  "Registered event sink functions.
Each sink receives a plist describing one event.")

(defvar magent-events--current-context nil
  "Current top-level turn context.")

(defun magent-events-generate-id ()
  "Generate a lowercase identifier suitable for Magent events."
  (let ((alphabet "abcdefghijklmnopqrstuvwxyz0123456789")
        (result (make-string 24 ?a))
        (index 0))
    (while (< index 24)
      (aset result index (aref alphabet (random (length alphabet))))
      (setq index (1+ index)))
    result))

(defun magent-events-current-context ()
  "Return the current turn context, or nil."
  magent-events--current-context)

(defun magent-events-add-sink (sink)
  "Register SINK as an event sink."
  (cl-pushnew sink magent-events--sinks :test #'eq)
  sink)

(defun magent-events-remove-sink (sink)
  "Unregister SINK from the event sink list."
  (setq magent-events--sinks (delq sink magent-events--sinks)))

(defun magent-events-clear-sinks ()
  "Remove all registered sinks."
  (setq magent-events--sinks nil))

(defun magent-events--dispatch (event)
  "Dispatch EVENT plist to all registered sinks."
  (dolist (sink magent-events--sinks)
    (condition-case err
        (funcall sink event)
      (error
       (message "magent-events sink error: %s" (error-message-string err))))))

(defun magent-events-emit (type &rest props)
  "Emit an event of TYPE with PROPS.
TYPE is stored as `:type'.  PROPS should be a plist.  When no explicit
`:context' is provided, the current turn context is used."
  (let* ((context (or (plist-get props :context)
                      magent-events--current-context))
         (event (append (list :type type
                              :time (float-time))
                        (when context
                          (list :context context
                                :turn-id (magent-events-context-turn-id context)
                                :subagent-id (magent-events-context-subagent-id context)))
                        props)))
    (magent-events--dispatch event)))

(defun magent-events-begin-turn (&optional title)
  "Create and activate a new top-level turn context with TITLE."
  (let ((context (magent-events-context-create
                  :turn-id (magent-events-generate-id)
                  :title title)))
    (setq magent-events--current-context context)
    (magent-events-emit 'turn-start :context context :title title)
    context))

(defun magent-events-end-turn (context status &optional detail)
  "End CONTEXT with STATUS and optional DETAIL.
STATUS should be one of `completed', `failed', or `cancelled'."
  (unless (or (null context)
              (magent-events-context-ended context))
    (setf (magent-events-context-ended context) t)
    (magent-events-emit 'turn-end
                        :context context
                        :status status
                        :detail detail)
    (when (eq context magent-events--current-context)
      (setq magent-events--current-context nil))))

(defun magent-events-create-subagent-context (&optional title parent-context)
  "Create and emit a subagent context with TITLE.
PARENT-CONTEXT defaults to the current context."
  (let* ((parent (or parent-context magent-events--current-context))
         (context (magent-events-context-create
                   :turn-id (and parent (magent-events-context-turn-id parent))
                   :subagent-id (magent-events-generate-id)
                   :title title)))
    (magent-events-emit 'subagent-start :context context :title title)
    context))

(defun magent-events-stop-subagent (context)
  "Emit a stop event for subagent CONTEXT."
  (unless (or (null context)
              (magent-events-context-ended context))
    (setf (magent-events-context-ended context) t)
    (magent-events-emit 'subagent-stop :context context)))

(provide 'magent-events)
;;; magent-events.el ends here
