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
(require 'magent-runtime-queue)

(declare-function magent-ui--request-prompt "magent-ui")

(cl-defstruct (magent-legacy-queue-submission
               (:constructor magent-legacy-queue-submission--create)
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

(defvar magent-legacy-queue--submission-metadata
  (make-hash-table :test #'eq :weakness 'key)
  "Captured session identity and lifecycle flags keyed by submission.
The metadata stays outside the historical struct layout for live reloads.")

(defun magent-legacy-queue-submission-create (&rest args)
  "Create a legacy submission from ARGS while storing extension metadata."
  (unless (zerop (% (length args) 2))
    (error "Legacy submission arguments must be keyword/value pairs"))
  (let (base-args metadata)
    (while args
      (let ((key (pop args))
            (value (pop args)))
        (if (memq key '(:session :scope :received :finalized))
            (setq metadata (plist-put metadata key value))
          (setq base-args (append base-args (list key value))))))
    (let ((submission (apply #'magent-legacy-queue-submission--create
                             base-args)))
      (when metadata
        (puthash submission metadata magent-legacy-queue--submission-metadata))
      submission)))

(defun magent-legacy-queue--metadata (submission key)
  "Return SUBMISSION metadata KEY."
  (plist-get (gethash submission magent-legacy-queue--submission-metadata) key))

(defun magent-legacy-queue--set-metadata (submission key value)
  "Set SUBMISSION metadata KEY to VALUE."
  (puthash submission
           (plist-put (gethash submission
                               magent-legacy-queue--submission-metadata)
                      key value)
           magent-legacy-queue--submission-metadata)
  value)

(defun magent-legacy-queue-submission-session (submission)
  "Return the session captured for SUBMISSION."
  (magent-legacy-queue--metadata submission :session))

(defun magent-legacy-queue-submission-scope (submission)
  "Return the scope captured for SUBMISSION."
  (magent-legacy-queue--metadata submission :scope))

(defun magent-legacy-queue-submission-received (submission)
  "Return non-nil when SUBMISSION emitted its receipt event."
  (magent-legacy-queue--metadata submission :received))

(defun magent-legacy-queue-submission-finalized (submission)
  "Return non-nil when SUBMISSION has terminalized."
  (magent-legacy-queue--metadata submission :finalized))

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

(defun magent-legacy-queue-set-current-request-handle
    (request-handle &optional submission)
  "Record REQUEST-HANDLE for the active SUBMISSION.
When SUBMISSION is non-nil, ignore a stale handle for any other active token."
  (when (or (null submission)
            (magent-legacy-queue-active-submission-p submission))
    (setq magent-legacy-queue--current-request-handle request-handle)))

(defun magent-legacy-queue-active-id-p (submission-id)
  "Return non-nil when SUBMISSION-ID is the active submission."
  (and magent-legacy-queue--active
       (equal (magent-legacy-queue-submission-id magent-legacy-queue--active)
              submission-id)))

(defun magent-legacy-queue-active-submission-p (submission)
  "Return non-nil when SUBMISSION is the exact active submission object."
  (eq magent-legacy-queue--active submission))

(defun magent-legacy-queue--emit (type submission &rest props)
  "Emit TYPE for SUBMISSION with PROPS."
  (apply #'magent-lifecycle-events-emit
         type
         :submission-id (magent-legacy-queue-submission-id submission)
         :op-type (and (magent-legacy-queue-submission-op submission)
                       (magent-op-type (magent-legacy-queue-submission-op submission)))
         props))

(declare-function magent-runtime-activate-scope "magent-runtime")

(defun magent-legacy-queue--submission-session (&optional submission)
  "Return the captured session for SUBMISSION or the current session."
  (or (and submission
           (magent-legacy-queue-submission-session submission))
      (magent-session-get)))

(defun magent-legacy-queue--session-thread (&optional submission)
  "Return SUBMISSION's captured session thread ledger, or nil."
  (when-let* ((session (magent-legacy-queue--submission-session submission))
              (thread (magent-session-thread-ledger session)))
    thread))

(defun magent-legacy-queue--submission-turn (submission)
  "Return SUBMISSION's ledger turn, or nil."
  (when-let* ((turn-id (magent-legacy-queue-submission-turn-id submission))
              (thread (magent-legacy-queue--session-thread submission)))
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

(defun magent-legacy-queue--activate-submission (submission)
  "Activate SUBMISSION's captured scope and session."
  (let ((session (magent-legacy-queue-submission-session submission))
        (scope (magent-legacy-queue-submission-scope submission)))
    (when scope
      (when (fboundp 'magent-runtime-activate-scope)
        (magent-runtime-activate-scope scope))
      (when session
        (magent-session-install scope session)
        (magent-session-refresh-agent session)))))

(defun magent-legacy-queue--save-submission (submission)
  "Persist SUBMISSION's captured session and scope asynchronously."
  (when-let* ((session (magent-legacy-queue-submission-session submission)))
    (magent-session-save-deferred-for-session
     session (magent-legacy-queue-submission-scope submission))))

(defun magent-legacy-queue--emit-received-once (submission)
  "Emit `submission-received' once for committed SUBMISSION."
  (unless (magent-legacy-queue-submission-received submission)
    (magent-legacy-queue--set-metadata submission :received t)
    (magent-legacy-queue--emit 'submission-received submission)))

(defun magent-legacy-queue--terminalize (submission status detail)
  "Commit terminal STATUS and DETAIL for SUBMISSION exactly once.
Return non-nil when this call performed the transition."
  (unless (magent-legacy-queue-submission-finalized submission)
    (magent-legacy-queue--set-metadata submission :finalized t)
    (setf (magent-legacy-queue-submission-status submission)
          (or status 'completed)
          (magent-legacy-queue-submission-finished-at submission)
          (float-time))
    (when-let* ((turn-id (magent-legacy-queue-submission-turn-id submission))
                (thread (magent-legacy-queue--session-thread submission))
                (turn (cl-find turn-id (magent-thread-turns thread)
                               :key #'magent-thread-turn-id
                               :test #'equal)))
      (unless (magent-thread-terminal-turn-p turn)
        (pcase (or status 'completed)
          ('completed (magent-thread-complete-turn thread turn-id))
          ('failed (magent-thread-fail-turn thread turn-id detail))
          ('interrupted (magent-thread-interrupt-turn thread turn-id detail))
          ('dropped (magent-thread-drop-turn thread turn-id detail))
          (_ nil)))
      (magent-session-refresh-projections
       (magent-legacy-queue--submission-session submission)))
    (magent-legacy-queue--save-submission submission)
    t))

(defun magent-legacy-queue--rollback-start (submission err)
  "Undo partial legacy startup for SUBMISSION after ERR."
  (let ((detail (error-message-string err)))
    (when (magent-legacy-queue-active-submission-p submission)
      (setq magent-legacy-queue--active nil
            magent-legacy-queue--current-request-handle nil))
    (when (magent-legacy-queue--terminalize submission 'failed detail)
      (magent-legacy-queue--emit 'submission-error submission :error detail)
      (magent-legacy-queue--emit
       'submission-finished submission :status 'failed :detail detail))))

(defun magent-legacy-queue--start (submission)
  "Start SUBMISSION asynchronously."
  (magent-legacy-queue--activate-submission submission)
  (setq magent-legacy-queue--active submission
        magent-legacy-queue--current-request-handle nil)
  (setf (magent-legacy-queue-submission-status submission) 'running
        (magent-legacy-queue-submission-started-at submission) (float-time))
  (magent-legacy-queue--emit-received-once submission)
  ;; A received sink may synchronously interrupt or finish this exact token.
  (when (and (magent-legacy-queue-active-submission-p submission)
             (not (magent-legacy-queue-submission-finalized submission)))
    (when-let* ((thread (magent-legacy-queue--session-thread submission))
                (turn-id (magent-legacy-queue-submission-turn-id submission)))
      (magent-thread-start-turn thread turn-id)
      (magent-session-refresh-projections
       (magent-legacy-queue--submission-session submission))
      (magent-legacy-queue--save-submission submission))
    (magent-legacy-queue--emit 'submission-start submission)
    (run-at-time 0 nil
                 (lambda (submission)
                   (when (and (magent-legacy-queue-active-submission-p submission)
                              (eq (magent-legacy-queue-submission-status submission)
                                  'running))
                     (condition-case err
                         (progn
                           ;; A user may have switched buffers/scopes after the
                           ;; zero-delay timer was scheduled.  Dispatch against
                           ;; the submission's captured identity, not ambient
                           ;; session globals.
                           (magent-legacy-queue--activate-submission submission)
                           (funcall
                            (magent-legacy-queue-submission-dispatcher submission)
                            submission))
                       (error
                        (magent-legacy-queue--emit
                         'submission-error submission
                         :error (error-message-string err))
                        (magent-legacy-queue-finish-submission
                         submission 'failed (error-message-string err))))))
                 submission)))

(defun magent-legacy-queue-kick ()
  "Reconcile and resume the backend-neutral global FIFO."
  (magent-runtime-queue-kick))

(defun magent-legacy-queue-submit (op payload dispatcher)
  "Submit OP with PAYLOAD to DISPATCHER.
When a turn is already active, the submission is queued and will run
after the active submission finishes.  Return the submission id."
  (let* ((session (magent-session-get))
         (scope (magent-session-current-scope))
         (thread (and session (magent-session-thread-ledger session)))
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
                      :session session
                      :scope scope
                      :status 'queued
                      :submitted-at (float-time)
                      :turn-id (and turn (magent-thread-turn-id turn)))))
    (when (and thread turn prompt)
      (magent-thread-record-user-message-if-needed
       thread (magent-thread-turn-id turn) prompt nil
       (list :source 'submission-queue))
      (magent-session-refresh-projections session))
    (setq magent-legacy-queue--pending
          (nconc magent-legacy-queue--pending (list submission)))
    (let ((disposition
           (magent-runtime-queue-arbitrate
            'legacy submission (magent-legacy-queue-submission-id submission)
            (lambda ()
              (setq magent-legacy-queue--pending
                    (delq submission magent-legacy-queue--pending))
              (magent-legacy-queue--start submission))
            (lambda (err)
              (setq magent-legacy-queue--pending
                    (delq submission magent-legacy-queue--pending))
              (magent-legacy-queue--rollback-start submission err)))))
      (when (eq disposition 'queued)
        (magent-legacy-queue--emit-received-once submission)
        (when (and (not (magent-legacy-queue-submission-finalized submission))
                   (memq submission magent-legacy-queue--pending))
          (magent-legacy-queue--emit 'submission-queued submission
                                     :queue-length
                                     (length magent-legacy-queue--pending)))))
    (magent-legacy-queue--save-submission submission)
    (magent-legacy-queue-submission-id submission)))

(defun magent-legacy-queue-finish-submission (submission &optional status detail)
  "Finish exact active SUBMISSION with STATUS and optional DETAIL.
Return the next queued submission id when one is started.  A stale callback
for an older submission is a no-op, even when ids have been reused."
  (when (and (magent-legacy-queue-active-submission-p submission)
             (not (magent-legacy-queue-submission-finalized submission)))
    (setq magent-legacy-queue--active nil
          magent-legacy-queue--current-request-handle nil)
    (let ((finalize
           (lambda ()
             (when (magent-legacy-queue--terminalize submission status detail)
               (magent-legacy-queue--emit
                'submission-finished submission
                :status (magent-legacy-queue-submission-status submission)
                :detail detail))))
          disposition)
      (setq disposition
            (magent-runtime-queue-arbiter-finish
             'legacy submission finalize))
      (unless disposition
        ;; A live reload may preserve the backend token from a version that had
        ;; no global arbiter ticket.  Completion must still be durable.
        (funcall finalize)
        (setq disposition (magent-runtime-queue-kick)))
      (unless (eq disposition 'handled)
        disposition))))

(defun magent-legacy-queue-finish (&optional status detail)
  "Finish the active submission with STATUS and optional DETAIL.
Return the next queued submission id when one is started."
  (when-let* ((submission magent-legacy-queue--active))
    (magent-legacy-queue-finish-submission submission status detail)))

(defun magent-legacy-queue-clear ()
  "Drop all queued submissions and return the number dropped."
  (let* ((submissions magent-legacy-queue--pending)
         (count (length submissions)))
    ;; Detach every token before invoking lifecycle sinks.  New submissions
    ;; created by a sink belong to a later transaction and must survive clear.
    (setq magent-legacy-queue--pending nil)
    (dolist (submission submissions)
      (magent-runtime-queue-arbiter-cancel 'legacy submission))
    ;; Commit every terminal state before invoking any external sink.  A sink
    ;; observing the first drop must never see later detached tokens still
    ;; looking queued, even when it re-enters clear/submit/interrupt paths.
    (let (terminalized)
      (dolist (submission submissions)
        (when (magent-legacy-queue--terminalize
               submission 'dropped "Queued submission dropped")
          (push submission terminalized)))
      (dolist (submission (nreverse terminalized))
        (magent-legacy-queue--emit 'submission-dropped submission)))
    count))

(defun magent-legacy-queue-interrupt (&optional abort-function)
  "Interrupt the active submission and clear queued submissions.
ABORT-FUNCTION, when non-nil, is called with the active request handle."
  (let ((request-handle magent-legacy-queue--current-request-handle)
        (active magent-legacy-queue--active))
    (magent-legacy-queue-clear)
    (setq magent-legacy-queue--active nil
          magent-legacy-queue--current-request-handle nil)
    (when active
      (let ((interrupt
             (lambda ()
               (when (magent-legacy-queue--terminalize
                      active 'interrupted "User interrupted")
                 (magent-legacy-queue--emit 'submission-interrupted active))
               ;; Invalidate queue identity before aborting.  A synchronous
               ;; provider callback now observes a stale submission and cannot
               ;; advance twice.
               (when (and request-handle abort-function)
                 (condition-case err
                     (funcall abort-function request-handle)
                   (error
                    (display-warning
                     'magent
                     (format "Legacy request abort failed: %s"
                             (error-message-string err))
                     :warning)))))))
        (unless (magent-runtime-queue-arbiter-finish
                 'legacy active interrupt)
          (funcall interrupt)
          (magent-runtime-queue-kick))))))

(provide 'magent-legacy-queue)
;;; magent-legacy-queue.el ends here
