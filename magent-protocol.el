;;; magent-protocol.el --- Codex-like protocol data for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; Lightweight protocol structures shared by Magent's UI, turn runtime,
;; context history, and tool orchestration.  This is intentionally small:
;; it models the logical agent runtime without implementing Codex sandboxing.

;;; Code:

(require 'cl-lib)
(require 'magent-config)

(defun magent-protocol-generate-id (&optional prefix)
  "Return a lowercase identifier with optional PREFIX."
  (let ((alphabet "abcdefghijklmnopqrstuvwxyz0123456789")
        (result (make-string 24 ?a))
        (index 0))
    (while (< index 24)
      (aset result index (aref alphabet (random (length alphabet))))
      (setq index (1+ index)))
    (if prefix
        (format "%s-%s" prefix result)
      result)))

(cl-defstruct (magent-op
               (:constructor magent-op-create)
               (:copier nil))
  "Submission operation accepted by the Magent runtime."
  id
  type
  payload
  (created-at 0.0))

(cl-defstruct (magent-event
               (:constructor magent-event-create)
               (:copier nil))
  "Runtime event emitted by Magent."
  id
  type
  turn-id
  payload
  (created-at 0.0))

(cl-defstruct (magent-response-item
               (:constructor magent-response-item-create)
               (:copier nil))
  "Structured model-visible transcript item.
TYPE is a symbol such as `message', `tool', `reasoning', or
`compaction'.  Tool calls and tool results are represented as one item
whose status moves from `in-progress' to `completed' or `failed'."
  id
  type
  role
  content
  name
  call-id
  output
  status
  phase
  metadata)

(cl-defstruct (magent-tool-call
               (:constructor magent-tool-call-create)
               (:copier nil))
  "Structured tool invocation."
  id
  name
  arguments
  perm-key
  reason)

(cl-defstruct (magent-tool-result
               (:constructor magent-tool-result-create)
               (:copier nil))
  "Structured tool result."
  call-id
  name
  output
  (success t)
  metadata)

(cl-defstruct (magent-agent-result
               (:constructor magent-agent-result-create)
               (:copier nil))
  "Final status returned from an agent request.
Successful requests may still use plain strings for compatibility.  This
structure is used when callers need to distinguish failed requests from a
normal assistant string."
  status
  content
  error
  metadata)

(defun magent-agent-result-success-p (result)
  "Return non-nil when RESULT represents a successful agent response."
  (or (stringp result)
      (and (magent-agent-result-p result)
           (eq (magent-agent-result-status result) 'completed))))

(defun magent-agent-result-content-string (result)
  "Return user-visible content for RESULT."
  (cond
   ((stringp result) result)
   ((magent-agent-result-p result)
    (or (magent-agent-result-content result)
        (magent-agent-result-error result)
        ""))
   ((null result) "")
   (t (format "%s" result))))

(defun magent-agent-result-failed (error &optional metadata)
  "Return a failed `magent-agent-result' with ERROR and METADATA."
  (magent-agent-result-create
   :status 'failed
   :error (if (stringp error) error (format "%s" error))
   :metadata metadata))

(defun magent-protocol-user-input-op (payload)
  "Create a user-input operation carrying PAYLOAD."
  (magent-op-create
   :id (magent-protocol-generate-id "op")
   :type 'user-input
   :payload payload
   :created-at (float-time)))

(defun magent-protocol-interrupt-op ()
  "Create an interrupt operation."
  (magent-op-create
   :id (magent-protocol-generate-id "op")
   :type 'interrupt
   :created-at (float-time)))

(defun magent-protocol-message-item (role content &optional phase)
  "Create a message response item with ROLE, CONTENT, and optional PHASE."
  (magent-response-item-create
   :id (magent-protocol-generate-id "item")
   :type 'message
   :role role
   :content content
   :phase phase))

(defun magent-protocol-tool-call-item (call)
  "Create a response item from tool CALL."
  (magent-response-item-create
   :id (or (magent-tool-call-id call)
           (magent-protocol-generate-id "item"))
   :type 'tool
   :name (magent-tool-call-name call)
   :call-id (magent-tool-call-id call)
   :content (magent-tool-call-arguments call)
   :status 'in-progress
   :metadata (list :perm-key (magent-tool-call-perm-key call)
                   :reason (magent-tool-call-reason call))))

(defun magent-protocol-tool-result-item (result)
  "Create a completed tool response item from RESULT."
  (magent-response-item-create
   :id (or (magent-tool-result-call-id result)
           (magent-protocol-generate-id "item"))
   :type 'tool
   :name (magent-tool-result-name result)
   :call-id (magent-tool-result-call-id result)
   :output (magent-tool-result-output result)
   :status (if (magent-tool-result-success result) 'completed 'failed)
   :metadata (magent-tool-result-metadata result)))

(defun magent-protocol--symbol-name-or-nil (value)
  "Return VALUE as a symbol name string, or nil."
  (cond
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   (t nil)))

(defun magent-protocol--intern-or-nil (value)
  "Return VALUE interned when it is a non-empty string."
  (cond
   ((symbolp value) value)
   ((and (stringp value) (> (length value) 0)) (intern value))
   (t nil)))

(defun magent-protocol-response-item-to-alist (item)
  "Convert response ITEM to a JSON-serializable alist."
  `((id . ,(magent-response-item-id item))
    (type . ,(magent-protocol--symbol-name-or-nil
              (magent-response-item-type item)))
    (role . ,(magent-protocol--symbol-name-or-nil
              (magent-response-item-role item)))
    (content . ,(let ((content (magent-response-item-content item)))
                  (and content (magent-json-safe-value content))))
    (name . ,(magent-json-safe-name
              (magent-response-item-name item) nil))
    (call-id . ,(magent-response-item-call-id item))
    (output . ,(let ((output (magent-response-item-output item)))
                 (and output (magent-json-safe-value output))))
    (status . ,(magent-protocol--symbol-name-or-nil
                (magent-response-item-status item)))
    (phase . ,(magent-protocol--symbol-name-or-nil
               (magent-response-item-phase item)))
    (metadata . ,(let ((metadata (magent-response-item-metadata item)))
                   (and metadata (magent-json-safe-value metadata))))))

(defun magent-protocol-response-item-from-alist (alist)
  "Reconstruct a response item from JSON-decoded ALIST."
  (magent-response-item-create
   :id (cdr (assq 'id alist))
   :type (magent-protocol--intern-or-nil (cdr (assq 'type alist)))
   :role (magent-protocol--intern-or-nil (cdr (assq 'role alist)))
   :content (cdr (assq 'content alist))
   :name (cdr (assq 'name alist))
   :call-id (cdr (assq 'call-id alist))
   :output (cdr (assq 'output alist))
   :status (magent-protocol--intern-or-nil (cdr (assq 'status alist)))
   :phase (magent-protocol--intern-or-nil (cdr (assq 'phase alist)))
   :metadata (cdr (assq 'metadata alist))))

(provide 'magent-protocol)
;;; magent-protocol.el ends here
