;;; magent-agent-job.el --- Durable child-agent job state  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Data structures for persistent child-agent jobs.  This is the
;; foundation for a Codex-style collaborative agent lifecycle; tool
;; handlers and UI integration are layered on top.

;;; Code:

(require 'cl-lib)
(require 'magent-config)
(require 'magent-json)

(defconst magent-agent-job-statuses
  '(queued running waiting completed failed closed cancelled)
  "Valid lifecycle statuses for a Magent child-agent job.")

(defvar magent-agent-job--last-id-stem nil
  "Timestamp stem used for the most recently generated agent job id.")

(defvar magent-agent-job--last-id-seq 0
  "Sequence number used when multiple agent jobs are created in one second.")

(cl-defstruct (magent-agent-job
               (:constructor magent-agent-job--create)
               (:copier nil))
  id
  parent-session-id
  agent-name
  task-name
  status
  prompt
  created-at
  updated-at
  transcript
  result
  error
  metadata)

(defun magent-agent-job-status-p (status)
  "Return non-nil when STATUS is a valid agent job status."
  (memq status magent-agent-job-statuses))

(defun magent-agent-job--coerce-status (status)
  "Return STATUS as a valid status symbol, or signal an error."
  (let ((symbol (cond
                 ((null status) 'queued)
                 ((symbolp status) status)
                 ((stringp status) (intern status))
                 (t status))))
    (unless (magent-agent-job-status-p symbol)
      (error "Invalid agent job status: %S" status))
    symbol))

(defun magent-agent-job-generate-id ()
  "Generate a stable, human-readable child-agent job id."
  (let ((stem (format-time-string "agent-%Y%m%d-%H%M%S")))
    (if (equal stem magent-agent-job--last-id-stem)
        (cl-incf magent-agent-job--last-id-seq)
      (setq magent-agent-job--last-id-stem stem
            magent-agent-job--last-id-seq 0))
    (if (> magent-agent-job--last-id-seq 0)
        (format "%s-%d" stem magent-agent-job--last-id-seq)
      stem)))

(defun magent-agent-job-create (&rest args)
  "Create a `magent-agent-job' from keyword ARGS.
Recognized keys are `:id', `:parent-session-id', `:agent-name',
`:task-name', `:status', `:prompt', `:created-at', `:updated-at',
`:transcript', `:result', `:error', and `:metadata'."
  (let* ((now (float-time))
         (status (magent-agent-job--coerce-status
                  (plist-get args :status)))
         (created-at (or (plist-get args :created-at) now)))
    (magent-agent-job--create
     :id (or (plist-get args :id)
             (magent-agent-job-generate-id))
     :parent-session-id (plist-get args :parent-session-id)
     :agent-name (plist-get args :agent-name)
     :task-name (plist-get args :task-name)
     :status status
     :prompt (plist-get args :prompt)
     :created-at created-at
     :updated-at (or (plist-get args :updated-at) created-at)
     :transcript (plist-get args :transcript)
     :result (plist-get args :result)
     :error (plist-get args :error)
     :metadata (plist-get args :metadata))))

(defun magent-agent-job-set-status (job status &optional result error)
  "Set JOB to STATUS and optionally record RESULT or ERROR.
Return JOB."
  (let ((status (magent-agent-job--coerce-status status)))
    (setf (magent-agent-job-status job) status)
    (when (memq status '(queued running waiting))
      (setf (magent-agent-job-result job) nil
            (magent-agent-job-error job) nil)))
  (setf (magent-agent-job-updated-at job) (float-time))
  (when result
    (setf (magent-agent-job-result job) result))
  (when error
    (setf (magent-agent-job-error job) error))
  job)

(defun magent-agent-job-find (jobs id)
  "Return the job with ID from JOBS, or nil."
  (cl-find id jobs :key #'magent-agent-job-id :test #'equal))

(defun magent-agent-job--alist-get (key alist)
  "Return KEY from ALIST."
  (cdr (assq key alist)))

(defun magent-agent-job-to-alist (job)
  "Convert JOB to a JSON-serializable alist."
  `((id . ,(magent-agent-job-id job))
    (parent-session-id . ,(magent-agent-job-parent-session-id job))
    (agent-name . ,(magent-agent-job-agent-name job))
    (task-name . ,(magent-agent-job-task-name job))
    (status . ,(symbol-name (magent-agent-job-status job)))
    (prompt . ,(magent-agent-job-prompt job))
    (created-at . ,(magent-agent-job-created-at job))
    (updated-at . ,(magent-agent-job-updated-at job))
    (transcript . ,(vconcat
                    (mapcar #'magent-json-safe-value
                            (or (magent-agent-job-transcript job) nil))))
    (result . ,(let ((result (magent-agent-job-result job)))
                 (and result (magent-json-safe-value result))))
    (error . ,(let ((error (magent-agent-job-error job)))
                (and error (magent-json-safe-value error))))
    (metadata . ,(let ((metadata (magent-agent-job-metadata job)))
                   (and metadata (magent-json-safe-value metadata))))))

(defun magent-agent-job-from-alist (alist)
  "Create a `magent-agent-job' from JSON-decoded ALIST."
  (magent-agent-job-create
   :id (magent-agent-job--alist-get 'id alist)
   :parent-session-id (magent-agent-job--alist-get 'parent-session-id alist)
   :agent-name (magent-agent-job--alist-get 'agent-name alist)
   :task-name (magent-agent-job--alist-get 'task-name alist)
   :status (magent-agent-job--alist-get 'status alist)
   :prompt (magent-agent-job--alist-get 'prompt alist)
   :created-at (magent-agent-job--alist-get 'created-at alist)
   :updated-at (magent-agent-job--alist-get 'updated-at alist)
   :transcript (magent-agent-job--alist-get 'transcript alist)
   :result (magent-agent-job--alist-get 'result alist)
   :error (magent-agent-job--alist-get 'error alist)
   :metadata (magent-agent-job--alist-get 'metadata alist)))

(provide 'magent-agent-job)
;;; magent-agent-job.el ends here
