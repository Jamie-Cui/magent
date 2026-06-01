;;; magent-turn.el --- Turn queue runtime for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; A small Codex-like submission queue for Magent.  It separates user
;; submission ordering from the UI buffer while preserving the existing
;; gptel-backed execution path.

;;; Code:

(require 'cl-lib)
(require 'magent-events)
(require 'magent-protocol)

(cl-defstruct (magent-turn-submission
               (:constructor magent-turn-submission-create)
               (:copier nil))
  id
  op
  payload
  dispatcher
  status
  submitted-at
  started-at
  finished-at)

(defvar magent-turn--active nil
  "Currently running `magent-turn-submission', if any.")

(defvar magent-turn--queue nil
  "Queued `magent-turn-submission' objects.")

(defvar magent-turn--current-request-handle nil
  "Request handle associated with the active submission, if known.")

(defun magent-turn-active-submission ()
  "Return the active submission, or nil."
  magent-turn--active)

(defun magent-turn-processing-p ()
  "Return non-nil when a submission is active."
  (and magent-turn--active t))

(defun magent-turn-pending-p ()
  "Return non-nil when submissions are queued."
  (and magent-turn--queue t))

(defun magent-turn-current-request-handle ()
  "Return the request handle associated with the active turn."
  magent-turn--current-request-handle)

(defun magent-turn-set-current-request-handle (request-handle)
  "Record REQUEST-HANDLE for the active turn."
  (setq magent-turn--current-request-handle request-handle))

(defun magent-turn-active-id-p (submission-id)
  "Return non-nil when SUBMISSION-ID is the active submission."
  (and magent-turn--active
       (equal (magent-turn-submission-id magent-turn--active)
              submission-id)))

(defun magent-turn--emit (type submission &rest props)
  "Emit TYPE for SUBMISSION with PROPS."
  (apply #'magent-events-emit
         type
         :submission-id (magent-turn-submission-id submission)
         :op-type (and (magent-turn-submission-op submission)
                       (magent-op-type (magent-turn-submission-op submission)))
         props))

(defun magent-turn--start (submission)
  "Start SUBMISSION asynchronously."
  (setq magent-turn--active submission
        magent-turn--current-request-handle nil)
  (setf (magent-turn-submission-status submission) 'running
        (magent-turn-submission-started-at submission) (float-time))
  (magent-turn--emit 'submission-start submission)
  (run-at-time 0 nil
               (lambda (submission)
                 (condition-case err
                     (funcall (magent-turn-submission-dispatcher submission)
                              submission)
                   (error
                    (magent-turn--emit 'submission-error submission
                                       :error (error-message-string err))
                    (magent-turn-finish 'failed
                                        (error-message-string err)))))
               submission))

(defun magent-turn-submit (op payload dispatcher)
  "Submit OP with PAYLOAD to DISPATCHER.
When a turn is already active, the submission is queued and will run
after the active turn finishes.  Return the submission id."
  (let* ((submission (magent-turn-submission-create
                      :id (or (and op (magent-op-id op))
                              (magent-protocol-generate-id "sub"))
                      :op op
                      :payload payload
                      :dispatcher dispatcher
                      :status 'queued
                      :submitted-at (float-time))))
    (magent-turn--emit 'submission-received submission)
    (if magent-turn--active
        (progn
          (setq magent-turn--queue
                (nconc magent-turn--queue (list submission)))
          (magent-turn--emit 'submission-queued submission
                             :queue-length (length magent-turn--queue)))
      (magent-turn--start submission))
    (magent-turn-submission-id submission)))

(defun magent-turn-finish (&optional status detail)
  "Finish the active submission with STATUS and optional DETAIL.
Return the next queued submission id when one is started."
  (let ((finished magent-turn--active))
    (when finished
      (setf (magent-turn-submission-status finished)
            (or status 'completed)
            (magent-turn-submission-finished-at finished)
            (float-time))
      (magent-turn--emit 'submission-finished finished
                         :status (magent-turn-submission-status finished)
                         :detail detail)))
  (setq magent-turn--active nil
        magent-turn--current-request-handle nil)
  (when-let ((next (pop magent-turn--queue)))
    (magent-turn--start next)
    (magent-turn-submission-id next)))

(defun magent-turn-clear-queue ()
  "Drop all queued submissions and return the number dropped."
  (let ((count (length magent-turn--queue)))
    (dolist (submission magent-turn--queue)
      (setf (magent-turn-submission-status submission) 'dropped
            (magent-turn-submission-finished-at submission) (float-time))
      (magent-turn--emit 'submission-dropped submission))
    (setq magent-turn--queue nil)
    count))

(defun magent-turn-interrupt (&optional abort-function)
  "Interrupt the active turn and clear queued submissions.
ABORT-FUNCTION, when non-nil, is called with the active request handle."
  (let ((request-handle magent-turn--current-request-handle)
        (active magent-turn--active))
    (when (and request-handle abort-function)
      (funcall abort-function request-handle))
    (magent-turn-clear-queue)
    (when active
      (setf (magent-turn-submission-status active) 'interrupted
            (magent-turn-submission-finished-at active) (float-time))
      (magent-turn--emit 'submission-interrupted active))
    (setq magent-turn--active nil
          magent-turn--current-request-handle nil)))

(provide 'magent-turn)
;;; magent-turn.el ends here
