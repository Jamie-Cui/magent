;;; magent-runtime-queue.el --- Session-scoped runtime queue  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Global single-execution queue for Magent runtime submissions.  Each
;; submission is tagged with a runtime session id so cancellation remains
;; session-scoped even though the first implementation runs one turn at a time.

;;; Code:

(require 'cl-lib)
(require 'magent-protocol)

(cl-defstruct (magent-runtime-submission
               (:constructor magent-runtime-submission-create)
               (:copier nil))
  id
  session
  session-id
  prompt
  context
  skills
  agent
  effort
  observer
  approval-provider
  on-complete
  status
  submitted-at
  started-at
  finished-at
  turn-id
  handle
  detail)

(defvar magent-runtime-queue--active nil
  "Currently running `magent-runtime-submission'.")

(defvar magent-runtime-queue--pending nil
  "Queued `magent-runtime-submission' objects.")

(defun magent-runtime-queue-active-submission ()
  "Return the active runtime submission, or nil."
  magent-runtime-queue--active)

(defun magent-runtime-queue-processing-p ()
  "Return non-nil when a runtime submission is active."
  (and magent-runtime-queue--active t))

(defun magent-runtime-queue-pending-p ()
  "Return non-nil when runtime submissions are queued."
  (and magent-runtime-queue--pending t))

(defun magent-runtime-queue-length (&optional session-id)
  "Return queued submission count.
When SESSION-ID is non-nil, count only queued submissions for that session."
  (length
   (if session-id
       (cl-remove-if-not
        (lambda (submission)
          (equal (magent-runtime-submission-session-id submission)
                 session-id))
        magent-runtime-queue--pending)
     magent-runtime-queue--pending)))

(defun magent-runtime-queue--start (submission starter)
  "Mark SUBMISSION active and call STARTER with it."
  (setq magent-runtime-queue--active submission)
  (setf (magent-runtime-submission-status submission) 'running
        (magent-runtime-submission-started-at submission) (float-time))
  (funcall starter submission))

(defun magent-runtime-queue-submit (submission starter)
  "Submit SUBMISSION, using STARTER when it becomes active.
Return SUBMISSION's id."
  (unless (magent-runtime-submission-id submission)
    (setf (magent-runtime-submission-id submission)
          (magent-protocol-generate-id "submission")))
  (setf (magent-runtime-submission-status submission) 'queued
        (magent-runtime-submission-submitted-at submission) (float-time))
  (if magent-runtime-queue--active
      (setq magent-runtime-queue--pending
            (nconc magent-runtime-queue--pending (list submission)))
    (magent-runtime-queue--start submission starter))
  (magent-runtime-submission-id submission))

(defun magent-runtime-queue-finish-active (&optional status detail starter)
  "Finish the active submission with STATUS and DETAIL.
When STARTER is non-nil, start the next queued submission.  Return the
next submission id when one is started."
  (when magent-runtime-queue--active
    (setf (magent-runtime-submission-status magent-runtime-queue--active)
          (or status 'completed)
          (magent-runtime-submission-finished-at magent-runtime-queue--active)
          (float-time)
          (magent-runtime-submission-detail magent-runtime-queue--active)
          detail))
  (setq magent-runtime-queue--active nil)
  (when (and starter magent-runtime-queue--pending)
    (let ((next (pop magent-runtime-queue--pending)))
      (magent-runtime-queue--start next starter)
      (magent-runtime-submission-id next))))

(defun magent-runtime-queue-remove-session (session-id)
  "Remove queued submissions for SESSION-ID and return them."
  (let (removed kept)
    (dolist (submission magent-runtime-queue--pending)
      (if (equal (magent-runtime-submission-session-id submission) session-id)
          (progn
            (setf (magent-runtime-submission-status submission) 'cancelled
                  (magent-runtime-submission-finished-at submission) (float-time))
            (push submission removed))
        (push submission kept)))
    (setq magent-runtime-queue--pending (nreverse kept))
    (nreverse removed)))

(defun magent-runtime-queue-active-session-p (session-id)
  "Return non-nil when SESSION-ID owns the active submission."
  (and magent-runtime-queue--active
       (equal (magent-runtime-submission-session-id
               magent-runtime-queue--active)
              session-id)))

(provide 'magent-runtime-queue)
;;; magent-runtime-queue.el ends here
