;;; magent-runtime-queue.el --- Session-scoped runtime queue  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Global single-execution queue for Magent runtime submissions.  Each
;; submission retains its runtime session object so cancellation remains
;; identity-scoped even though the first implementation runs one turn at a time.

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
  scope
  status
  submitted-at
  started-at
  finished-at
  turn-id
  handle
  detail
  finalized)

(defvar magent-runtime-queue--submission-starters
  (make-hash-table :test #'eq :weakness 'key)
  "Per-submission starters kept outside the stable submission struct.")

(defun magent-runtime-submission-starter (submission)
  "Return the request-local starter captured for SUBMISSION."
  (gethash submission magent-runtime-queue--submission-starters))

(defun magent-runtime-queue--set-submission-starter (submission starter)
  "Associate STARTER with SUBMISSION without changing its struct layout."
  (if starter
      (puthash submission starter magent-runtime-queue--submission-starters)
    (remhash submission magent-runtime-queue--submission-starters))
  starter)

(defvar magent-runtime-queue--active nil
  "Currently running `magent-runtime-submission'.")

(defvar magent-runtime-queue--pending nil
  "Queued `magent-runtime-submission' objects.")

(cl-defstruct (magent-runtime-arbiter-ticket
               (:constructor magent-runtime-arbiter-ticket-create)
               (:copier nil))
  "One backend-neutral ticket in the global execution FIFO."
  owner
  token
  id
  starter
  rollback
  starting
  finishing
  finish-requested)

(defvar magent-runtime-queue--arbiter-active nil
  "Active `magent-runtime-arbiter-ticket', regardless of backend.")

(defvar magent-runtime-queue--arbiter-pending nil
  "Global FIFO of backend-neutral execution tickets.")

(defvar magent-runtime-queue--arbiter-bootstrap-complete nil
  "Non-nil after preserved pre-arbiter backend state has been adopted.")

(defvar magent-runtime-queue--arbiter-ticket-adapters
  (make-hash-table :test #'eq :weakness 'key)
  "Backend callbacks keyed by arbiter ticket.
Callbacks keep the runtime queue independent of backend-specific token types.")

(declare-function magent-runtime-api--start-submission "magent-runtime-api")
(declare-function magent-runtime-session-magent-session "magent-runtime-api")

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

(defun magent-runtime-queue--runtime-bootstrap-ticket (submission active-p)
  "Return an arbiter ticket for preserved runtime SUBMISSION.
ACTIVE-P means the backend request is already running and must not restart."
  (magent-runtime-arbiter-ticket-create
   :owner 'runtime
   :token submission
   :id (magent-runtime-submission-id submission)
   :starter
   (if active-p
       #'ignore
     (lambda ()
       (setq magent-runtime-queue--pending
             (delq submission magent-runtime-queue--pending))
       (magent-runtime-queue--start
        submission
        (or (magent-runtime-submission-starter submission)
            (and (fboundp 'magent-runtime-api--start-submission)
                 #'magent-runtime-api--start-submission)))))
   :rollback
   (unless active-p
     (lambda (err)
       (setq magent-runtime-queue--pending
             (delq submission magent-runtime-queue--pending))
       (magent-runtime-queue--rollback-start submission err)))))

(defun magent-runtime-queue--ticket-time (ticket active-p)
  "Return a sortable timestamp for preserved TICKET.
Use started time for ACTIVE-P tickets and submitted time otherwise."
  (let ((token (magent-runtime-arbiter-ticket-token ticket)))
    (or (if active-p
            (magent-runtime-submission-started-at token)
          (magent-runtime-submission-submitted-at token))
        most-positive-fixnum)))

(defun magent-runtime-queue--bootstrap-preserved-backends ()
  "Create global lease tickets for runtime state preserved across reload."
  (when (and (null magent-runtime-queue--arbiter-active)
             (null magent-runtime-queue--arbiter-pending))
    (let (active-tickets pending-tickets)
      (when magent-runtime-queue--active
        (push (magent-runtime-queue--runtime-bootstrap-ticket
               magent-runtime-queue--active t)
              active-tickets))
      (dolist (submission magent-runtime-queue--pending)
        (push (magent-runtime-queue--runtime-bootstrap-ticket submission nil)
              pending-tickets))
      (setq active-tickets
            (sort active-tickets
                  (lambda (left right)
                    (< (magent-runtime-queue--ticket-time left t)
                       (magent-runtime-queue--ticket-time right t))))
            pending-tickets
            (sort pending-tickets
                  (lambda (left right)
                    (< (magent-runtime-queue--ticket-time left nil)
                       (magent-runtime-queue--ticket-time right nil)))))
      (when active-tickets
        (setq magent-runtime-queue--arbiter-active (pop active-tickets)))
      (setq magent-runtime-queue--arbiter-pending
            (append active-tickets pending-tickets)))))

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
        (or (eq token magent-runtime-queue--active)
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
  (setq magent-runtime-queue--arbiter-active ticket)
  (setf (magent-runtime-arbiter-ticket-starting ticket) t
        (magent-runtime-arbiter-ticket-finish-requested ticket) nil)
  (condition-case err
      (progn
        (funcall (magent-runtime-arbiter-ticket-starter ticket))
        (setf (magent-runtime-arbiter-ticket-starting ticket) nil)
        (if (magent-runtime-arbiter-ticket-finish-requested ticket)
            (progn
              (when (eq magent-runtime-queue--arbiter-active ticket)
                (setq magent-runtime-queue--arbiter-active nil))
              'finished)
          'active))
    (t
     ;; Keep STARTING true while the backend rolls back.  If rollback or an
     ;; error hook calls `magent-runtime-queue-arbiter-finish', advancement is
     ;; deferred until the whole starter transaction has unwound.
     (magent-runtime-queue--arbiter-rollback-ticket ticket err)
     (setf (magent-runtime-arbiter-ticket-starting ticket) nil
           (magent-runtime-arbiter-ticket-finish-requested ticket) t)
     (when (eq magent-runtime-queue--arbiter-active ticket)
       (setq magent-runtime-queue--arbiter-active nil))
     (cons 'error err))))

(defun magent-runtime-queue--arbiter-start-next ()
  "Start the first live ticket in the global FIFO and return its id."
  (let (started)
    (while (and (not magent-runtime-queue--arbiter-active)
                magent-runtime-queue--arbiter-pending
                (not started))
      (let ((ticket (pop magent-runtime-queue--arbiter-pending)))
        (when (magent-runtime-queue--arbiter-ticket-live-p ticket)
          (pcase (magent-runtime-queue--arbiter-start-ticket ticket)
            ('active (setq started ticket))
            ('finished nil)
            (`(error . ,err)
             ;; This ticket was already accepted asynchronously, so its
             ;; backend rollback is the durable failure report.  Do not make
             ;; the preceding ticket's finish path fail as collateral damage.
             (display-warning
              'magent
              (format "Queued starter failed for %s: %s"
                      (magent-runtime-arbiter-ticket-id ticket)
                      (error-message-string err))
              :warning))))))
    (and started (magent-runtime-arbiter-ticket-id started))))

(defun magent-runtime-queue--arbiter-reconcile ()
  "Discard stale tickets left by reloads/tests and resume the global FIFO."
  (unless (and magent-runtime-queue--arbiter-active
               (magent-runtime-queue--arbiter-ticket-live-p
                magent-runtime-queue--arbiter-active))
    (setq magent-runtime-queue--arbiter-active nil))
  (setq magent-runtime-queue--arbiter-pending
        (cl-remove-if-not #'magent-runtime-queue--arbiter-ticket-live-p
                          magent-runtime-queue--arbiter-pending))
  (unless magent-runtime-queue--arbiter-active
    (magent-runtime-queue--arbiter-start-next)))

(defun magent-runtime-queue-arbitrate
    (owner token id starter &optional rollback live-p scope session)
  "Submit OWNER's TOKEN and STARTER to the global execution FIFO.
ID is the stable backend submission id.  ROLLBACK receives a starter error
and must undo backend state established by STARTER.  LIVE-P, SCOPE, and
SESSION are optional zero-argument callbacks used to inspect non-runtime
tokens without coupling the arbiter to their representation.  Return
`started' or `queued'."
  (magent-runtime-queue--arbiter-reconcile)
  (let ((ticket (magent-runtime-arbiter-ticket-create
                 :owner owner :token token :id id :starter starter
                 :rollback rollback)))
    (magent-runtime-queue--set-ticket-adapters
     ticket live-p scope session)
    (if magent-runtime-queue--arbiter-active
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
         (condition-case nil
             (magent-runtime-queue--arbiter-start-next)
           (error nil))
         (signal (car err) (cdr err)))))))

(defun magent-runtime-queue-arbiter-cancel (owner token)
  "Remove OWNER's queued TOKEN from the global execution FIFO."
  (setq magent-runtime-queue--arbiter-pending
        (cl-remove-if
         (lambda (ticket)
           (when (and (eq (magent-runtime-arbiter-ticket-owner ticket) owner)
                      (eq (magent-runtime-arbiter-ticket-token ticket) token))
             (remhash ticket magent-runtime-queue--arbiter-ticket-adapters)
             t))
         magent-runtime-queue--arbiter-pending)))

(defun magent-runtime-queue-arbiter-finish (owner token &optional before-advance)
  "Finish active OWNER TOKEN and start the next global FIFO ticket.
When BEFORE-ADVANCE is non-nil, call it after the backend has released its
active token but while this arbiter ticket still owns the execution lease.
Return the id of the next ticket when one starts, `handled' when the token
was finished without a successor, or nil when TOKEN did not own the lease."
  (when (and magent-runtime-queue--arbiter-active
             (eq (magent-runtime-arbiter-ticket-owner
                  magent-runtime-queue--arbiter-active)
                 owner)
             (eq (magent-runtime-arbiter-ticket-token
                  magent-runtime-queue--arbiter-active)
                 token))
    (let ((ticket magent-runtime-queue--arbiter-active))
      (cond
       ((magent-runtime-arbiter-ticket-finishing ticket)
        ;; A completion hook re-entered the finish path.  The outer finish
        ;; transaction still owns advancement.
        (setf (magent-runtime-arbiter-ticket-finish-requested ticket) t)
        'handled)
       ((magent-runtime-arbiter-ticket-starting ticket)
        ;; Synchronous completion from inside a starter must run its cleanup
        ;; before the starter returns, but advancement remains deferred.
        (unwind-protect
            (when before-advance
              (funcall before-advance))
          (setf (magent-runtime-arbiter-ticket-finish-requested ticket) t))
        'handled)
       (t
        (let (next-id)
          (setf (magent-runtime-arbiter-ticket-finishing ticket) t)
          (unwind-protect
              (when before-advance
                (funcall before-advance))
            (setf (magent-runtime-arbiter-ticket-finishing ticket) nil)
            (when (eq magent-runtime-queue--arbiter-active ticket)
              (setq magent-runtime-queue--arbiter-active nil
                    next-id (magent-runtime-queue--arbiter-start-next))))
          (or next-id 'handled)))))))

(defun magent-runtime-queue-arbiter-owner ()
  "Return the backend tag owning global execution, or nil."
  (magent-runtime-queue--arbiter-reconcile)
  (and magent-runtime-queue--arbiter-active
       (magent-runtime-arbiter-ticket-owner
        magent-runtime-queue--arbiter-active)))

(defun magent-runtime-queue-execution-active-p ()
  "Return non-nil while any backend owns the global execution lease."
  (and (magent-runtime-queue-arbiter-owner) t))

(declare-function magent-session-scope-origin "magent-session")

(defun magent-runtime-queue-active-scope ()
  "Return the project/global scope owning the execution lease, or nil."
  (magent-runtime-queue--arbiter-reconcile)
  (when-let* ((ticket magent-runtime-queue--arbiter-active)
              (token (magent-runtime-arbiter-ticket-token ticket)))
    (let ((scope
           (pcase (magent-runtime-arbiter-ticket-owner ticket)
             ('runtime (magent-runtime-submission-scope token))
             (_ (magent-runtime-queue--ticket-adapter-call
                 ticket :scope)))))
      (if (and scope (fboundp 'magent-session-scope-origin))
          (magent-session-scope-origin scope)
        scope))))

(defun magent-runtime-queue-active-submission ()
  "Return the active runtime submission, or nil."
  magent-runtime-queue--active)

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
          (eq (magent-runtime-submission-session submission)
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
    (setq magent-runtime-queue--active submission)
    (magent-runtime-queue--set-submission-starter submission effective-starter)
    (setf (magent-runtime-submission-status submission) 'running
          (magent-runtime-submission-started-at submission) (float-time))
    (funcall effective-starter submission)))

(defun magent-runtime-queue--rollback-start (submission err)
  "Undo partial runtime startup for SUBMISSION after ERR."
  (when (eq magent-runtime-queue--active submission)
    (setq magent-runtime-queue--active nil))
  (setf (magent-runtime-submission-status submission) 'failed
        (magent-runtime-submission-finished-at submission) (float-time)
        (magent-runtime-submission-detail submission)
        (error-message-string err)))

(defun magent-runtime-queue-kick ()
  "Reconcile and resume the backend-neutral global FIFO."
  (magent-runtime-queue--arbiter-reconcile))

(defun magent-runtime-queue-submit (submission starter)
  "Submit SUBMISSION, using STARTER when it becomes active.
Return SUBMISSION's id."
  (unless (magent-runtime-submission-id submission)
    (setf (magent-runtime-submission-id submission)
          (magent-protocol-generate-id "submission")))
  (setf (magent-runtime-submission-status submission) 'queued
        (magent-runtime-submission-submitted-at submission) (float-time))
  (magent-runtime-queue--set-submission-starter submission starter)
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

(defun magent-runtime-queue-finish-active
    (&optional status detail before-advance)
  "Finish the active submission with STATUS and DETAIL.
Call BEFORE-ADVANCE after releasing the backend-active slot but before the
next global FIFO ticket starts.  Return the next submission id when one is
started."
  (let ((finished magent-runtime-queue--active))
    (when finished
      (setf (magent-runtime-submission-status finished)
          (or status 'completed)
          (magent-runtime-submission-finished-at finished)
          (float-time)
          (magent-runtime-submission-detail finished) detail)
      (setq magent-runtime-queue--active nil)
      (let ((disposition
             (magent-runtime-queue-arbiter-finish
              'runtime finished before-advance)))
        ;; Preserve completion semantics across live reloads from versions that
        ;; predate the backend-neutral arbiter.
        (unless disposition
          (when before-advance
            (funcall before-advance))
          (setq disposition (magent-runtime-queue-kick)))
        (unless (eq disposition 'handled)
          disposition)))))

(defun magent-runtime-queue-remove-session (runtime-session)
  "Remove queued submissions for exact RUNTIME-SESSION and return them."
  (let (removed kept)
    (dolist (submission magent-runtime-queue--pending)
      (if (eq (magent-runtime-submission-session submission) runtime-session)
          (progn
            (setf (magent-runtime-submission-status submission) 'cancelled
                  (magent-runtime-submission-finished-at submission) (float-time))
            (push submission removed))
        (push submission kept)))
    (setq magent-runtime-queue--pending (nreverse kept))
    (dolist (submission removed)
      (magent-runtime-queue-arbiter-cancel 'runtime submission))
    (nreverse removed)))

(defun magent-runtime-queue--submission-session-object (submission)
  "Return the Magent session captured by runtime SUBMISSION, or nil."
  (when-let* ((runtime-session
               (magent-runtime-submission-session submission)))
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
             (append (and magent-runtime-queue--arbiter-active
                          (list magent-runtime-queue--arbiter-active))
                     magent-runtime-queue--arbiter-pending))
      (when (eq session
                (magent-runtime-queue--ticket-session-object ticket))
        (cl-pushnew (magent-runtime-arbiter-ticket-owner ticket)
                    owners :test #'eq)))
    ;; Preserve session-busy behavior across a live reload from a version
    ;; predating arbiter tickets.  Only runtime state can exist without a
    ;; backend adapter now.
    (when (or (and magent-runtime-queue--active
                   (eq session
                       (magent-runtime-queue--submission-session-object
                        magent-runtime-queue--active)))
              (cl-some
               (lambda (submission)
                 (eq session
                     (magent-runtime-queue--submission-session-object
                      submission)))
               magent-runtime-queue--pending))
      (cl-pushnew 'runtime owners :test #'eq))
    (nreverse owners)))

(defun magent-runtime-queue-session-busy-p (session)
  "Return non-nil when active or queued work captures exact SESSION."
  (and (magent-runtime-queue-session-busy-owners session) t))

(defun magent-runtime-queue-active-session-object ()
  "Return the exact Magent session owning the global execution lease."
  (magent-runtime-queue--arbiter-reconcile)
  (magent-runtime-queue--ticket-session-object
   magent-runtime-queue--arbiter-active))

;; `defvar' preserves old backend tokens during a source reload.  Adopt them
;; exactly once after all queue helpers are defined, before any new submission
;; can mistake the absence of an arbiter ticket for an idle executor.
(unless magent-runtime-queue--arbiter-bootstrap-complete
  (setq magent-runtime-queue--arbiter-bootstrap-complete t)
  (magent-runtime-queue--bootstrap-preserved-backends))

(provide 'magent-runtime-queue)
;;; magent-runtime-queue.el ends here
