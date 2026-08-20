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

(defun magent-tool-result-require (value &optional name call-id)
  "Validate and complete structured tool result VALUE.
NAME and CALL-ID fill missing request identity fields."
  (unless (magent-tool-result-p value)
    (signal 'wrong-type-argument (list 'magent-tool-result-p value)))
  (unless (magent-tool-result-name value)
    (setf (magent-tool-result-name value) name))
  (unless (magent-tool-result-call-id value)
    (setf (magent-tool-result-call-id value) call-id))
  (unless (magent-tool-result-status value)
    (setf (magent-tool-result-status value)
          (if (magent-tool-result-success value) 'completed 'failed)))
  value)

(defun magent-tool-result-status-value (result)
  "Return normalized status for structured tool RESULT."
  (magent-tool-result-status
   (magent-tool-result-require result)))

(defun magent-tool-result-success-p (result)
  "Return non-nil when structured tool RESULT completed."
  (eq (magent-tool-result-status-value result) 'completed))

(defun magent-tool-result-output-string (result)
  "Return model/UI-visible output string for structured tool RESULT."
  (setq result (magent-tool-result-require result))
  (let ((value (or (magent-tool-result-output result)
                   (magent-tool-result-error result)
                   "")))
    (cond
     ((stringp value) value)
     ((null value) "")
     (t (format "%s" value)))))

(cl-defstruct (magent-execution-result
               (:constructor magent-execution-result-create)
               (:copier nil))
  "Final status returned from one Magent execution."
  status
  content
  error
  metadata)

(defun magent-execution-result-success-p (result)
  "Return non-nil when RESULT represents a successful execution."
  (unless (magent-execution-result-p result)
    (signal 'wrong-type-argument (list 'magent-execution-result-p result)))
  (eq (magent-execution-result-status result) 'completed))

(defun magent-execution-result-content-string (result)
  "Return user-visible content for RESULT."
  (unless (magent-execution-result-p result)
    (signal 'wrong-type-argument (list 'magent-execution-result-p result)))
  (or (magent-execution-result-content result)
      (magent-execution-result-error result)
      ""))

(defun magent-execution-result-completed (content &optional metadata)
  "Return a completed `magent-execution-result' with CONTENT and METADATA."
  (magent-execution-result-create
   :status 'completed
   :content (or content "")
   :metadata metadata))

(defun magent-execution-result-failed (error &optional metadata)
  "Return a failed `magent-execution-result' with ERROR and METADATA."
  (magent-execution-result-create
   :status 'failed
   :error (if (stringp error) error (format "%s" error))
   :metadata metadata))

(defun magent-execution-result-cancelled (error &optional metadata)
  "Return a cancelled `magent-execution-result' with ERROR and METADATA."
  (magent-execution-result-create
   :status 'cancelled
   :error (if (stringp error) error (format "%s" error))
   :metadata metadata))

(provide 'magent-protocol)
;;; magent-protocol.el ends here
