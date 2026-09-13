;;; magent-runtime-queue.el --- Session-scoped runtime queue  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Session-scoped FIFO execution for Magent runtime submissions.  Independent
;; sessions run concurrently, including sessions in the same project.  Each
;; submission retains its exact runtime session wrapper and request context;
;; completion and cancellation always address the exact submission.

;;; Code:

(require 'cl-lib)
(require 'magent-protocol)

(cl-defstruct (magent-runtime-submission
               (:constructor magent-runtime-submission-create)
               (:copier nil))
  id
  runtime-session
  request-context
  on-complete
  starter
  status
  submitted-at
  started-at
  finished-at
  handle
  detail
  finalized)

(defvar magent-runtime-queue--active nil
  "List of running `magent-runtime-submission' objects.")

(defvar magent-runtime-queue--pending nil
  "Queued `magent-runtime-submission' objects.")

(cl-defstruct (magent-runtime-arbiter-ticket
               (:constructor magent-runtime-arbiter-ticket-create)
               (:copier nil))
  "One backend-neutral ticket in a session execution FIFO."
  owner
  token
  id
  starter
  rollback
  session
  starting
  finishing
  finish-requested)

(defvar magent-runtime-queue--arbiter-active nil
  "List of active `magent-runtime-arbiter-ticket' objects, one per session.")

(defvar magent-runtime-queue--arbiter-pending nil
  "Pending backend-neutral tickets, ordered by submission time.")

(defvar magent-runtime-queue--arbiter-ticket-adapters
  (make-hash-table :test #'eq :weakness 'key)
  "Backend callbacks keyed by arbiter ticket.
Callbacks keep the runtime queue independent of backend-specific token types.")

(declare-function magent-runtime-session-magent-session
                  "magent-runtime-api" t t)
(declare-function magent-runtime-session-scope
                  "magent-runtime-api" t t)

(defun magent-runtime-queue--set-ticket-adapters
    (ticket live-p scope session)
  "Associate backend callbacks LIVE-P, SCOPE, and SESSION with TICKET."
  (when (or live-p scope session)
    (puthash ticket
             (list :live-p live-p :scope scope :session session)
             magent-runtime-queue--arbiter-ticket-adapters))
  ticket)

(defun magent-runtime-queue--ticket-adapter-call (ticket key)
  "Call TICKET's backend adapter at KEY, returning nil when absent."
  (when-let* ((function
               (plist-get
                (gethash ticket magent-runtime-queue--arbiter-ticket-adapters)
                key)))
    (funcall function)))

(defun magent-runtime-queue--arbiter-ticket-live-p (ticket)
  "Return non-nil when TICKET still belongs to its backend queue."
  (let ((token (magent-runtime-arbiter-ticket-token ticket)))
    (or
     ;; A backend may remove its token from its pending queue before its
     ;; starter commits the backend-specific active state.  Keep that small
     ;; transaction window owned by this ticket.
     (or (magent-runtime-arbiter-ticket-starting ticket)
         (magent-runtime-arbiter-ticket-finishing ticket))
     (pcase (magent-runtime-arbiter-ticket-owner ticket)
       ('runtime
        (or (memq token magent-runtime-queue--active)
            (memq token magent-runtime-queue--pending)))
       (_
        (let ((live-p
               (plist-get
                (gethash ticket
                         magent-runtime-queue--arbiter-ticket-adapters)
                :live-p)))
          (and live-p (funcall live-p))))))))

(defun magent-runtime-queue--arbiter-rollback-ticket (ticket err)
  "Roll back TICKET after its starter signalled ERR.
Rollback failures never mask the original starter error."
  (when-let* ((rollback (magent-runtime-arbiter-ticket-rollback ticket)))
    (condition-case rollback-error
        (funcall rollback err)
      (t
       (display-warning
        'magent
        (format "Queue rollback failed for %s: %s"
                (magent-runtime-arbiter-ticket-id ticket)
                (error-message-string rollback-error))
        :warning)))))

(defun magent-runtime-queue--arbiter-start-ticket (ticket)
  "Start TICKET transactionally.
Return `active' when it remains active, `finished' when its starter
synchronously finalized it, or (error . ERR) after rolling it back."
  (push ticket magent-runtime-queue--arbiter-active)
  (setf (magent-runtime-arbiter-ticket-starting ticket) t
        (magent-runtime-arbiter-ticket-finish-requested ticket) nil)
  (condition-case err
      (progn
        (funcall (magent-runtime-arbiter-ticket-starter ticket))
        (setf (magent-runtime-arbiter-ticket-starting ticket) nil)
        (if (magent-runtime-arbiter-ticket-finish-requested ticket)
            (progn
              (magent-runtime-queue--arbiter-release-ticket ticket)
              'finished)
          'active))
    (t
     ;; Keep STARTING true while the backend rolls back.  If rollback or an
     ;; error hook calls `magent-runtime-queue-arbiter-finish', advancement is
     ;; deferred until the whole starter transaction has unwound.
     (magent-runtime-queue--arbiter-rollback-ticket ticket err)
     (setf (magent-runtime-arbiter-ticket-starting ticket) nil
           (magent-runtime-arbiter-ticket-finish-requested ticket) t)
     (magent-runtime-queue--arbiter-release-ticket ticket)
     (cons 'error err))))

(defun magent-runtime-queue--arbiter-release-ticket (ticket)
  "Release exact TICKET without affecting other sessions."
  (setq magent-runtime-queue--arbiter-active
        (delq ticket magent-runtime-queue--arbiter-active))
  (remhash ticket magent-runtime-queue--arbiter-ticket-adapters))

(defun magent-runtime-queue--arbiter-blocked-p (ticket)
  "Return non-nil when TICKET's captured session already has an active turn."
  (cl-some (lambda (active)
             (eq (magent-runtime-arbiter-ticket-session active)
                 (magent-runtime-arbiter-ticket-session ticket)))
           magent-runtime-queue--arbiter-active))

(defun magent-runtime-queue--arbiter-start-next ()
  "Start runnable session FIFO heads and return the first started ticket id.
A busy session never blocks another session.  Starting and finishing tickets
retain their session until all synchronous callbacks have returned."
  (let (first-id ticket)
    (while (setq ticket
                 (cl-find-if
                  (lambda (candidate)
                    (not (magent-runtime-queue--arbiter-blocked-p candidate)))
                  magent-runtime-queue--arbiter-pending))
      (setq magent-runtime-queue--arbiter-pending
            (delq ticket magent-runtime-queue--arbiter-pending))
      (when (magent-runtime-queue--arbiter-ticket-live-p ticket)
        (pcase (magent-runtime-queue--arbiter-start-ticket ticket)
          ('active
           (unless first-id
             (setq first-id (magent-runtime-arbiter-ticket-id ticket))))
          ('finished nil)
          (`(error . ,err)
           ;; Accepted asynchronous work reports failure through its backend
           ;; rollback.  It must not fail an unrelated completion callback.
           (display-warning
            'magent
            (format "Queued starter failed for %s: %s"
                    (magent-runtime-arbiter-ticket-id ticket)
                    (error-message-string err))
            :warning)))))
    first-id))

(defun magent-runtime-queue--arbiter-reconcile ()
  "Discard inactive tickets and resume runnable session FIFOs."
  (dolist (ticket (copy-sequence magent-runtime-queue--arbiter-active))
    (unless (magent-runtime-queue--arbiter-ticket-live-p ticket)
      (magent-runtime-queue--arbiter-release-ticket ticket)))
  (setq magent-runtime-queue--arbiter-pending
        (cl-remove-if-not #'magent-runtime-queue--arbiter-ticket-live-p
                          magent-runtime-queue--arbiter-pending))
  (magent-runtime-queue--arbiter-start-next))

(defun magent-runtime-queue-arbitrate
    (owner token id starter &optional rollback live-p scope session)
  "Submit OWNER's TOKEN and STARTER to its session execution FIFO.
ID is the stable backend submission id.  ROLLBACK receives a starter error
and must undo backend state established by STARTER.  LIVE-P, SCOPE, and
SESSION are optional zero-argument callbacks used to inspect non-runtime
tokens without coupling the arbiter to their representation.  Return
`started' or `queued'.  Session identity is captured at submission time;
separate sessions may execute concurrently even within one project."
  (magent-runtime-queue--arbiter-reconcile)
  (let ((ticket (magent-runtime-arbiter-ticket-create
                 :owner owner :token token :id id :starter starter
                 :rollback rollback)))
    (magent-runtime-queue--set-ticket-adapters
     ticket live-p scope session)
    (setf (magent-runtime-arbiter-ticket-session ticket)
          (magent-runtime-queue--ticket-session-object ticket))
    (if (magent-runtime-queue--arbiter-blocked-p ticket)
        (progn
          (setq magent-runtime-queue--arbiter-pending
                (nconc magent-runtime-queue--arbiter-pending (list ticket)))
          'queued)
      (pcase (magent-runtime-queue--arbiter-start-ticket ticket)
        ((or 'active 'finished)
         (when (eq (magent-runtime-arbiter-ticket-finish-requested ticket) t)
           (magent-runtime-queue--arbiter-start-next))
         'started)
        (`(error . ,err)
         (magent-runtime-queue--arbiter-start-next)
         (signal (car err) (cdr err)))))))

(defun magent-runtime-queue-arbiter-cancel (owner token)
  "Remove OWNER's queued TOKEN from its session execution FIFO."
  (setq magent-runtime-queue--arbiter-pending
        (cl-remove-if
         (lambda (ticket)
           (when (and (eq (magent-runtime-arbiter-ticket-owner ticket) owner)
                      (eq (magent-runtime-arbiter-ticket-token ticket) token))
             (remhash ticket magent-runtime-queue--arbiter-ticket-adapters)
             t))
         magent-runtime-queue--arbiter-pending)))

(defun magent-runtime-queue-arbiter-finish (owner token &optional before-advance)
  "Finish exact active OWNER TOKEN and resume runnable session FIFO heads.
Call BEFORE-ADVANCE after releasing the backend token but while the ticket
still owns its session.  Return a successor id, `handled' without a successor,
or nil when TOKEN is not active."
  (when-let* ((ticket
               (cl-find-if
                (lambda (candidate)
                  (and (eq (magent-runtime-arbiter-ticket-owner candidate) owner)
                       (eq (magent-runtime-arbiter-ticket-token candidate) token)))
                magent-runtime-queue--arbiter-active)))
    (cond
     ((magent-runtime-arbiter-ticket-finishing ticket)
      (setf (magent-runtime-arbiter-ticket-finish-requested ticket) t)
      'handled)
     ((magent-runtime-arbiter-ticket-starting ticket)
      (unwind-protect
          (when before-advance (funcall before-advance))
        (setf (magent-runtime-arbiter-ticket-finish-requested ticket) t))
      'handled)
     (t
      (let (next-id)
        (setf (magent-runtime-arbiter-ticket-finishing ticket) t)
        (unwind-protect
            (when before-advance (funcall before-advance))
          (setf (magent-runtime-arbiter-ticket-finishing ticket) nil)
          (magent-runtime-queue--arbiter-release-ticket ticket)
          (setq next-id (magent-runtime-queue--arbiter-start-next)))
        (or next-id 'handled))))))

(defun magent-runtime-queue-arbiter-owner (&optional session)
  "Return an active backend owner, optionally for exact SESSION.
Without SESSION this is an aggregate busy query, not a submission selector."
  (magent-runtime-queue--arbiter-reconcile)
  (when-let* ((ticket (if session
                         (cl-find session magent-runtime-queue--arbiter-active
                                  :key #'magent-runtime-arbiter-ticket-session
                                  :test #'eq)
                       (car magent-runtime-queue--arbiter-active))))
    (magent-runtime-arbiter-ticket-owner ticket)))

(defun magent-runtime-queue-execution-active-p ()
  "Return non-nil while any backend owns a session execution lease."
  (and (magent-runtime-queue-arbiter-owner) t))

(defun magent-runtime-queue-active-submissions ()
  "Return a fresh list of all active runtime submissions."
  (copy-sequence magent-runtime-queue--active))

(defun magent-runtime-queue-active-submission (&optional runtime-session)
  "Return the active submission for exact RUNTIME-SESSION.
Without RUNTIME-SESSION, return the sole active submission, or signal when
multiple sessions are running.  Use `magent-runtime-queue-active-submissions'
for aggregate inspection."
  (if runtime-session
      (cl-find runtime-session magent-runtime-queue--active
               :key #'magent-runtime-submission-runtime-session :test #'eq)
    (when (cdr magent-runtime-queue--active)
      (error "Multiple Magent sessions are active; specify a runtime session"))
    (car magent-runtime-queue--active)))

(defun magent-runtime-queue-processing-p ()
  "Return non-nil when a runtime submission is active."
  (and magent-runtime-queue--active t))

(defun magent-runtime-queue-pending-p ()
  "Return non-nil when runtime submissions are queued."
  (and magent-runtime-queue--pending t))

(defun magent-runtime-queue-length (&optional runtime-session)
  "Return queued submission count.
When RUNTIME-SESSION is non-nil, count only submissions owned by that exact
runtime session wrapper."
  (length
   (if runtime-session
       (cl-remove-if-not
        (lambda (submission)
          (eq (magent-runtime-submission-runtime-session submission)
              runtime-session))
        magent-runtime-queue--pending)
     magent-runtime-queue--pending)))

(defun magent-runtime-queue--start (submission starter)
  "Mark SUBMISSION active and call STARTER with it."
  (let ((effective-starter
         (or starter (magent-runtime-submission-starter submission))))
    (unless effective-starter
      (error "Runtime submission has no starter: %s"
             (magent-runtime-submission-id submission)))
    (push submission magent-runtime-queue--active)
    (setf (magent-runtime-submission-starter submission) effective-starter)
    (setf (magent-runtime-submission-status submission) 'running
          (magent-runtime-submission-started-at submission) (float-time))
    (funcall effective-starter submission)))

(defun magent-runtime-queue--rollback-start (submission err)
  "Undo partial runtime startup for SUBMISSION after ERR."
  (setq magent-runtime-queue--active
        (delq submission magent-runtime-queue--active))
  (setf (magent-runtime-submission-status submission) 'failed
        (magent-runtime-submission-finished-at submission) (float-time)
        (magent-runtime-submission-detail submission)
        (error-message-string err)))

(defun magent-runtime-queue-submit (submission starter)
  "Submit SUBMISSION, using STARTER when it becomes active.
Return SUBMISSION's id."
  (unless (magent-runtime-submission-id submission)
    (setf (magent-runtime-submission-id submission)
          (magent-protocol-generate-id "submission")))
  (setf (magent-runtime-submission-status submission) 'queued
        (magent-runtime-submission-submitted-at submission) (float-time))
  (setf (magent-runtime-submission-starter submission) starter)
  (setq magent-runtime-queue--pending
        (nconc magent-runtime-queue--pending (list submission)))
  (magent-runtime-queue-arbitrate
   'runtime submission (magent-runtime-submission-id submission)
   (lambda ()
     (setq magent-runtime-queue--pending
           (delq submission magent-runtime-queue--pending))
     (magent-runtime-queue--start
      submission (magent-runtime-submission-starter submission)))
   (lambda (err)
     (setq magent-runtime-queue--pending
           (delq submission magent-runtime-queue--pending))
     (magent-runtime-queue--rollback-start submission err)))
  (magent-runtime-submission-id submission))

(defun magent-runtime-queue-finish
    (submission &optional status detail before-advance)
  "Finish exact active SUBMISSION with STATUS and DETAIL.
Call BEFORE-ADVANCE after releasing the backend slot but before the next
turn of this session starts.  Other sessions keep their active submissions."
  (when (memq submission magent-runtime-queue--active)
    (setf (magent-runtime-submission-status submission) (or status 'completed)
          (magent-runtime-submission-finished-at submission) (float-time)
          (magent-runtime-submission-detail submission) detail)
    (setq magent-runtime-queue--active
          (delq submission magent-runtime-queue--active))
    (let ((disposition
           (magent-runtime-queue-arbiter-finish
            'runtime submission before-advance)))
      (unless (eq disposition 'handled) disposition))))

(defun magent-runtime-queue-remove-session (runtime-session)
  "Remove queued submissions for exact RUNTIME-SESSION and return them."
  (let (removed kept)
    (dolist (submission magent-runtime-queue--pending)
      (if (eq (magent-runtime-submission-runtime-session submission)
              runtime-session)
          (progn
            (setf (magent-runtime-submission-status submission) 'cancelled
                  (magent-runtime-submission-finished-at submission) (float-time))
            (push submission removed))
        (push submission kept)))
    (setq magent-runtime-queue--pending (nreverse kept))
    (dolist (submission removed)
      (magent-runtime-queue-arbiter-cancel 'runtime submission))
    (nreverse removed)))

(defun magent-runtime-queue-remove-submission (runtime-session submission-id)
  "Remove queued SUBMISSION-ID owned by exact RUNTIME-SESSION.
Return the removed submission, or nil when it is not queued."
  (let (removed kept)
    (dolist (submission magent-runtime-queue--pending)
      (if (and (null removed)
               (eq (magent-runtime-submission-runtime-session submission)
                   runtime-session)
               (equal (magent-runtime-submission-id submission)
                      submission-id))
          (setq removed submission)
        (push submission kept)))
    (setq magent-runtime-queue--pending (nreverse kept))
    (when removed
      (setf (magent-runtime-submission-status removed) 'cancelled
            (magent-runtime-submission-finished-at removed) (float-time))
      (magent-runtime-queue-arbiter-cancel 'runtime removed))
    removed))

(defun magent-runtime-queue--submission-session-object (submission)
  "Return the Magent session captured by runtime SUBMISSION, or nil."
  (when-let* ((runtime-session
               (magent-runtime-submission-runtime-session submission)))
    (when (fboundp 'magent-runtime-session-magent-session)
      (magent-runtime-session-magent-session runtime-session))))

(defun magent-runtime-queue--ticket-session-object (ticket)
  "Return the exact Magent session captured by arbiter TICKET, or nil."
  (when ticket
    (let ((token (magent-runtime-arbiter-ticket-token ticket)))
      (pcase (magent-runtime-arbiter-ticket-owner ticket)
        ('runtime
         (magent-runtime-queue--submission-session-object token))
        (_
         (magent-runtime-queue--ticket-adapter-call ticket :session))))))

(defun magent-runtime-queue-session-busy-owners (session)
  "Return backend owners whose active or queued work captures SESSION.
The comparison is by exact session object identity.  This is used by session
  replacement and clear transactions; equal ids are deliberately insufficient."
  (let (owners)
    (dolist (ticket
             (append magent-runtime-queue--arbiter-active
                     magent-runtime-queue--arbiter-pending))
      (when (eq session
                (magent-runtime-queue--ticket-session-object ticket))
        (cl-pushnew (magent-runtime-arbiter-ticket-owner ticket)
                    owners :test #'eq)))
    (nreverse owners)))

(defun magent-runtime-queue-session-busy-p (session)
  "Return non-nil when active or queued work captures exact SESSION."
  (and (magent-runtime-queue-session-busy-owners session) t))

(provide 'magent-runtime-queue)
;;; magent-runtime-queue.el ends here
