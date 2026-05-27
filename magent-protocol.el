;;; magent-protocol.el --- Codex-like protocol data for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; Lightweight protocol structures shared by Magent's UI, turn runtime,
;; context history, and tool orchestration.  This is intentionally small:
;; it models the logical agent runtime without implementing Codex sandboxing.

;;; Code:

(require 'cl-lib)

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
TYPE is a symbol such as `message', `tool-call', `tool-output',
`reasoning', or `compaction'."
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
   :type 'tool-call
   :name (magent-tool-call-name call)
   :call-id (magent-tool-call-id call)
   :content (magent-tool-call-arguments call)
   :metadata (list :perm-key (magent-tool-call-perm-key call)
                   :reason (magent-tool-call-reason call))))

(defun magent-protocol-tool-result-item (result)
  "Create a response item from tool RESULT."
  (magent-response-item-create
   :id (magent-protocol-generate-id "item")
   :type 'tool-output
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
    (content . ,(magent-response-item-content item))
    (name . ,(magent-response-item-name item))
    (call-id . ,(magent-response-item-call-id item))
    (output . ,(magent-response-item-output item))
    (status . ,(magent-protocol--symbol-name-or-nil
                (magent-response-item-status item)))
    (phase . ,(magent-protocol--symbol-name-or-nil
               (magent-response-item-phase item)))
    (metadata . ,(magent-response-item-metadata item))))

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
