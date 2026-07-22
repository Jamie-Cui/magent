;;; magent-protocol.el --- Codex-like protocol data for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Lightweight protocol structures shared by Magent's UI, turn runtime,
;; context history, and tool orchestration.  This is intentionally small:
;; it models the logical agent runtime without implementing Codex sandboxing.

;;; Code:

(require 'cl-lib)
(require 'magent-json)

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

(cl-defstruct (magent-tool-result
               (:constructor magent-tool-result-create)
               (:copier nil))
  "Structured tool result."
  call-id
  name
  output
  (success t)
  status
  error
  exit-code
  metadata)

(defun magent-tool-result--legacy-status (value)
  "Infer a tool status from legacy string VALUE."
  (if (and (stringp value)
           (or (string-match-p "\\`Error\\b" value)
               (string-match-p "\\`HTTP error\\b" value)
               (string-match-p "\\`Command timed out\\b" value)))
      'failed
    'completed))

(defun magent-tool-result-status-value (result)
  "Return normalized status for tool RESULT."
  (cond
   ((magent-tool-result-p result)
    (or (magent-tool-result-status result)
        (if (magent-tool-result-success result) 'completed 'failed)))
   (t (magent-tool-result--legacy-status result))))

(defun magent-tool-result-success-p (result)
  "Return non-nil when tool RESULT represents successful execution."
  (eq (magent-tool-result-status-value result) 'completed))

(defun magent-tool-result-output-string (result)
  "Return model/UI-visible output string for tool RESULT."
  (let ((value (if (magent-tool-result-p result)
                   (or (magent-tool-result-output result)
                       (magent-tool-result-error result)
                       "")
                 result)))
    (cond
     ((stringp value) value)
     ((null value) "")
     (t (format "%s" value)))))

(defun magent-tool-result-normalize (value &optional name call-id)
  "Return VALUE as a structured tool result with optional NAME and CALL-ID."
  (if (magent-tool-result-p value)
      (progn
        (unless (magent-tool-result-name value)
          (setf (magent-tool-result-name value) name))
        (unless (magent-tool-result-call-id value)
          (setf (magent-tool-result-call-id value) call-id))
        (unless (magent-tool-result-status value)
          (setf (magent-tool-result-status value)
                (if (magent-tool-result-success value) 'completed 'failed)))
        value)
    (let ((status (magent-tool-result--legacy-status value)))
      (magent-tool-result-create
       :call-id call-id
       :name name
       :output (if (stringp value) value (magent-json-safe-value value))
       :success (eq status 'completed)
       :status status
       :error (and (eq status 'failed)
                   (magent-tool-result-output-string value))))))

(cl-defstruct (magent-agent-result
               (:constructor magent-agent-result-create)
               (:copier nil))
  "Final status returned from an agent request."
  status
  content
  error
  metadata)

(defun magent-agent-result-success-p (result)
  "Return non-nil when RESULT represents a successful agent response."
  (unless (magent-agent-result-p result)
    (signal 'wrong-type-argument (list 'magent-agent-result-p result)))
  (eq (magent-agent-result-status result) 'completed))

(defun magent-agent-result-content-string (result)
  "Return user-visible content for RESULT."
  (unless (magent-agent-result-p result)
    (signal 'wrong-type-argument (list 'magent-agent-result-p result)))
  (or (magent-agent-result-content result)
      (magent-agent-result-error result)
      ""))

(defun magent-agent-result-completed (content &optional metadata)
  "Return a completed `magent-agent-result' with CONTENT and METADATA."
  (magent-agent-result-create
   :status 'completed
   :content (or content "")
   :metadata metadata))

(defun magent-agent-result-failed (error &optional metadata)
  "Return a failed `magent-agent-result' with ERROR and METADATA."
  (magent-agent-result-create
   :status 'failed
   :error (if (stringp error) error (format "%s" error))
   :metadata metadata))

(defun magent-agent-result-cancelled (error &optional metadata)
  "Return a cancelled `magent-agent-result' with ERROR and METADATA."
  (magent-agent-result-create
   :status 'cancelled
   :error (if (stringp error) error (format "%s" error))
   :metadata metadata))

(provide 'magent-protocol)
;;; magent-protocol.el ends here
