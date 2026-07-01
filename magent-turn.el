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
(require 'magent-session)
(require 'magent-thread)

(declare-function magent-ui--request-prompt "magent-ui")

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
  finished-at
  turn-id)

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

(defun magent-turn-queue-length ()
  "Return the number of queued submissions."
  (length magent-turn--queue))

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

(defun magent-turn--session-thread ()
  "Return the current session thread ledger, or nil."
  (when-let* ((session (magent-session-get))
              (thread (magent-session-thread-ledger session)))
    thread))

(defun magent-turn--submission-turn (submission)
  "Return SUBMISSION's ledger turn, or nil."
  (when-let* ((thread (magent-turn--session-thread))
              (turn-id (magent-turn-submission-turn-id submission)))
    (cl-find turn-id (magent-thread-turns thread)
             :key #'magent-thread-turn-id
             :test #'equal)))

(defun magent-turn--payload-prompt (payload)
  "Return a best-effort prompt string from SUBMISSION PAYLOAD."
  (cond
   ((stringp payload) payload)
   ((and (recordp payload)
         (fboundp 'magent-ui--request-prompt))
    (ignore-errors (magent-ui--request-prompt payload)))
   ((and (listp payload) (plist-member payload :prompt))
    (plist-get payload :prompt))
   (t nil)))

(defun magent-turn--start (submission)
  "Start SUBMISSION asynchronously."
  (setq magent-turn--active submission
        magent-turn--current-request-handle nil)
  (setf (magent-turn-submission-status submission) 'running
        (magent-turn-submission-started-at submission) (float-time))
  (when-let* ((thread (magent-turn--session-thread))
              (turn-id (magent-turn-submission-turn-id submission)))
    (magent-thread-start-turn thread turn-id)
    (magent-session-refresh-projections (magent-session-get)))
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
  (let* ((thread (magent-turn--session-thread))
         (prompt (magent-turn--payload-prompt payload))
         (turn (when thread
                 (magent-thread-queue-turn
                  thread
                  prompt
                  (and op (magent-op-id op))
                  (list :source 'submission-queue))))
         (submission (magent-turn-submission-create
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
      (when-let* ((thread (magent-turn--session-thread))
                  (turn-id (magent-turn-submission-turn-id finished)))
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
      (when-let* ((thread (magent-turn--session-thread))
                  (turn-id (magent-turn-submission-turn-id submission)))
        (magent-thread-drop-turn thread turn-id "Queued submission dropped")
        (magent-session-refresh-projections (magent-session-get)))
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
      (when-let* ((thread (magent-turn--session-thread))
                  (turn-id (magent-turn-submission-turn-id active)))
        (magent-thread-interrupt-turn thread turn-id "User interrupted")
        (magent-session-refresh-projections (magent-session-get)))
      (magent-turn--emit 'submission-interrupted active))
    (setq magent-turn--active nil
          magent-turn--current-request-handle nil)))

(provide 'magent-turn)
;;; magent-turn.el ends here
