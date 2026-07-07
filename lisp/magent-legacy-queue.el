;;; magent-legacy-queue.el --- Legacy UI submission queue for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A small Codex-like submission queue for Magent.  It separates user
;; submission ordering from the UI buffer while preserving the existing
;; gptel-backed execution path.

;;; Code:

(require 'cl-lib)
(require 'magent-lifecycle-events)
(require 'magent-protocol)
(require 'magent-session)
(require 'magent-ledger)

(declare-function magent-ui--request-prompt "magent-ui")

(cl-defstruct (magent-legacy-queue-submission
               (:constructor magent-legacy-queue-submission-create)
               (:copier nil))
  id
  op
  payload
  dispatcher
  status
  submitted-at
  started-at
  finished-at
  turn-id)

(defvar magent-legacy-queue--active nil
  "Currently running `magent-legacy-queue-submission', if any.")

(defvar magent-legacy-queue--pending nil
  "Queued `magent-legacy-queue-submission' objects.")

(defvar magent-legacy-queue--current-request-handle nil
  "Request handle associated with the active submission, if known.")

(defun magent-legacy-queue-active-submission ()
  "Return the active submission, or nil."
  magent-legacy-queue--active)

(defun magent-legacy-queue-processing-p ()
  "Return non-nil when a submission is active."
  (and magent-legacy-queue--active t))

(defun magent-legacy-queue-pending-p ()
  "Return non-nil when submissions are queued."
  (and magent-legacy-queue--pending t))

(defun magent-legacy-queue-length ()
  "Return the number of queued submissions."
  (length magent-legacy-queue--pending))

(defun magent-legacy-queue-current-request-handle ()
  "Return the request handle associated with the active submission."
  magent-legacy-queue--current-request-handle)

(defun magent-legacy-queue-set-current-request-handle (request-handle)
  "Record REQUEST-HANDLE for the active submission."
  (setq magent-legacy-queue--current-request-handle request-handle))

(defun magent-legacy-queue-active-id-p (submission-id)
  "Return non-nil when SUBMISSION-ID is the active submission."
  (and magent-legacy-queue--active
       (equal (magent-legacy-queue-submission-id magent-legacy-queue--active)
              submission-id)))

(defun magent-legacy-queue--emit (type submission &rest props)
  "Emit TYPE for SUBMISSION with PROPS."
  (apply #'magent-lifecycle-events-emit
         type
         :submission-id (magent-legacy-queue-submission-id submission)
         :op-type (and (magent-legacy-queue-submission-op submission)
                       (magent-op-type (magent-legacy-queue-submission-op submission)))
         props))

(defun magent-legacy-queue--session-thread ()
  "Return the current session thread ledger, or nil."
  (when-let* ((session (magent-session-get))
              (thread (magent-session-thread-ledger session)))
    thread))

(defun magent-legacy-queue--submission-turn (submission)
  "Return SUBMISSION's ledger turn, or nil."
  (when-let* ((thread (magent-legacy-queue--session-thread))
              (turn-id (magent-legacy-queue-submission-turn-id submission)))
    (cl-find turn-id (magent-thread-turns thread)
             :key #'magent-thread-turn-id
             :test #'equal)))

(defun magent-legacy-queue--payload-prompt (payload)
  "Return a best-effort prompt string from SUBMISSION PAYLOAD."
  (cond
   ((stringp payload) payload)
   ((and (recordp payload)
         (fboundp 'magent-ui--request-prompt))
    (ignore-errors (magent-ui--request-prompt payload)))
   ((and (listp payload) (plist-member payload :prompt))
    (plist-get payload :prompt))
   (t nil)))

(defun magent-legacy-queue--start (submission)
  "Start SUBMISSION asynchronously."
  (setq magent-legacy-queue--active submission
        magent-legacy-queue--current-request-handle nil)
  (setf (magent-legacy-queue-submission-status submission) 'running
        (magent-legacy-queue-submission-started-at submission) (float-time))
  (when-let* ((thread (magent-legacy-queue--session-thread))
              (turn-id (magent-legacy-queue-submission-turn-id submission)))
    (magent-thread-start-turn thread turn-id)
    (magent-session-refresh-projections (magent-session-get)))
  (magent-legacy-queue--emit 'submission-start submission)
  (run-at-time 0 nil
               (lambda (submission)
                 (condition-case err
                     (funcall (magent-legacy-queue-submission-dispatcher submission)
                              submission)
                   (error
                    (magent-legacy-queue--emit 'submission-error submission
                                       :error (error-message-string err))
                    (magent-legacy-queue-finish 'failed
                                        (error-message-string err)))))
               submission))

(defun magent-legacy-queue-submit (op payload dispatcher)
  "Submit OP with PAYLOAD to DISPATCHER.
When a turn is already active, the submission is queued and will run
after the active submission finishes.  Return the submission id."
  (let* ((thread (magent-legacy-queue--session-thread))
         (prompt (magent-legacy-queue--payload-prompt payload))
         (turn (when thread
                 (magent-thread-queue-turn
                  thread
                  prompt
                  (and op (magent-op-id op))
                  (list :source 'submission-queue))))
         (submission (magent-legacy-queue-submission-create
                      :id (or (and turn (magent-thread-turn-id turn))
                              (and op (magent-op-id op))
                              (magent-protocol-generate-id "sub"))
                      :op op
                      :payload payload
                      :dispatcher dispatcher
                      :status 'queued
                      :submitted-at (float-time)
                      :turn-id (and turn (magent-thread-turn-id turn)))))
    (when (and thread turn prompt)
      (magent-thread-record-user-message-if-needed
       thread (magent-thread-turn-id turn) prompt nil
       (list :source 'submission-queue))
      (magent-session-refresh-projections (magent-session-get)))
    (magent-legacy-queue--emit 'submission-received submission)
    (if magent-legacy-queue--active
        (progn
          (setq magent-legacy-queue--pending
                (nconc magent-legacy-queue--pending (list submission)))
          (magent-legacy-queue--emit 'submission-queued submission
                             :queue-length (length magent-legacy-queue--pending)))
      (magent-legacy-queue--start submission))
    (magent-legacy-queue-submission-id submission)))

(defun magent-legacy-queue-finish (&optional status detail)
  "Finish the active submission with STATUS and optional DETAIL.
Return the next queued submission id when one is started."
  (let ((finished magent-legacy-queue--active))
    (when finished
      (setf (magent-legacy-queue-submission-status finished)
            (or status 'completed)
            (magent-legacy-queue-submission-finished-at finished)
            (float-time))
      (when-let* ((thread (magent-legacy-queue--session-thread))
                  (turn-id (magent-legacy-queue-submission-turn-id finished)))
        (let ((turn (cl-find turn-id (magent-thread-turns thread)
                             :key #'magent-thread-turn-id
                             :test #'equal)))
          (unless (and turn (magent-thread-terminal-turn-p turn))
            (pcase (or status 'completed)
              ('completed (magent-thread-complete-turn thread turn-id))
              ('failed (magent-thread-fail-turn thread turn-id detail))
              ('interrupted (magent-thread-interrupt-turn thread turn-id detail))
              ('dropped (magent-thread-drop-turn thread turn-id detail))
              (_ nil))))
        (magent-session-refresh-projections (magent-session-get)))
      (magent-legacy-queue--emit 'submission-finished finished
                         :status (magent-legacy-queue-submission-status finished)
                         :detail detail)))
  (setq magent-legacy-queue--active nil
        magent-legacy-queue--current-request-handle nil)
  (when-let ((next (pop magent-legacy-queue--pending)))
    (magent-legacy-queue--start next)
    (magent-legacy-queue-submission-id next)))

(defun magent-legacy-queue-clear ()
  "Drop all queued submissions and return the number dropped."
  (let ((count (length magent-legacy-queue--pending)))
    (dolist (submission magent-legacy-queue--pending)
      (setf (magent-legacy-queue-submission-status submission) 'dropped
            (magent-legacy-queue-submission-finished-at submission) (float-time))
      (when-let* ((thread (magent-legacy-queue--session-thread))
                  (turn-id (magent-legacy-queue-submission-turn-id submission)))
        (magent-thread-drop-turn thread turn-id "Queued submission dropped")
        (magent-session-refresh-projections (magent-session-get)))
      (magent-legacy-queue--emit 'submission-dropped submission))
    (setq magent-legacy-queue--pending nil)
    count))

(defun magent-legacy-queue-interrupt (&optional abort-function)
  "Interrupt the active submission and clear queued submissions.
ABORT-FUNCTION, when non-nil, is called with the active request handle."
  (let ((request-handle magent-legacy-queue--current-request-handle)
        (active magent-legacy-queue--active))
    (when (and request-handle abort-function)
      (funcall abort-function request-handle))
    (magent-legacy-queue-clear)
    (when active
      (setf (magent-legacy-queue-submission-status active) 'interrupted
            (magent-legacy-queue-submission-finished-at active) (float-time))
      (when-let* ((thread (magent-legacy-queue--session-thread))
                  (turn-id (magent-legacy-queue-submission-turn-id active)))
        (magent-thread-interrupt-turn thread turn-id "User interrupted")
        (magent-session-refresh-projections (magent-session-get)))
      (magent-legacy-queue--emit 'submission-interrupted active))
    (setq magent-legacy-queue--active nil
          magent-legacy-queue--current-request-handle nil)))

(provide 'magent-legacy-queue)
;;; magent-legacy-queue.el ends here
