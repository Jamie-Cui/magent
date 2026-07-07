;;; magent-runtime-api.el --- UI-neutral Magent runtime API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stable UI/backend-facing runtime API.  UI backends submit prompts to a
;; runtime session and receive request-local Magent-native observer events.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magent-agent)
(require 'magent-agent-loop)
(require 'magent-agent-registry)
(require 'magent-protocol)
(require 'magent-runtime)
(require 'magent-runtime-queue)
(require 'magent-session)
(require 'magent-ledger)

(cl-defstruct (magent-runtime-session
               (:constructor magent-runtime-session-create)
               (:copier nil))
  id
  scope
  magent-session
  pending-skills
  metadata)

(defvar magent-runtime-api--sessions (make-hash-table :test #'equal)
  "Runtime session wrappers keyed by session id.")

(defun magent-runtime-api--session-id (session)
  "Return SESSION's stable id."
  (magent-session-get-id session))

(defun magent-runtime-api--wrap-session (session scope)
  "Return runtime wrapper for Magent SESSION at SCOPE."
  (let* ((id (magent-runtime-api--session-id session))
         (existing (gethash id magent-runtime-api--sessions)))
    (if existing
        (progn
          (setf (magent-runtime-session-scope existing) scope
                (magent-runtime-session-magent-session existing) session)
          existing)
      (let ((runtime-session
             (magent-runtime-session-create
              :id id
              :scope scope
              :magent-session session)))
        (puthash id runtime-session magent-runtime-api--sessions)
        runtime-session))))

(defun magent-runtime-session-current (&optional scope)
  "Return the current runtime session for SCOPE."
  (magent-runtime-ensure-initialized)
  (let* ((target-scope (or scope (magent-session-current-scope)))
         (session (magent-session-activate target-scope)))
    (magent-runtime-api--wrap-session session target-scope)))

(defun magent-runtime-session-new (&optional scope)
  "Create and activate a new runtime session for SCOPE."
  (magent-runtime-ensure-initialized)
  (let* ((target-scope (or scope (magent-session-current-scope)))
         (session (magent-session-create)))
    (magent-session-install target-scope session)
    (magent-runtime-api--wrap-session session target-scope)))

(defun magent-runtime-session-from-id (session-id)
  "Return runtime session SESSION-ID, or nil."
  (gethash session-id magent-runtime-api--sessions))

(defun magent-runtime-session-register (scope session)
  "Install SESSION at SCOPE and return its runtime wrapper."
  (magent-session-install scope session)
  (magent-runtime-api--wrap-session session scope))

(defun magent-runtime-session-set-agent (runtime-session agent-or-name)
  "Set RUNTIME-SESSION's agent to AGENT-OR-NAME."
  (let* ((agent (cond
                 ((magent-agent-info-p agent-or-name) agent-or-name)
                 ((stringp agent-or-name)
                  (magent-agent-registry-get agent-or-name))
                 ((symbolp agent-or-name)
                  (magent-agent-registry-get (symbol-name agent-or-name)))))
         (session (magent-runtime-session-magent-session runtime-session)))
    (unless agent
      (error "Unknown Magent agent: %S" agent-or-name))
    (magent-session-set-agent session agent)
    agent))

(defun magent-runtime-session-agent-name (runtime-session)
  "Return RUNTIME-SESSION's active agent name."
  (let* ((session (magent-runtime-session-magent-session runtime-session))
         (agent (or (magent-session-agent session)
                    (magent-agent-registry-get-default))))
    (and agent (magent-agent-info-name agent))))

(defun magent-runtime-session-toggle-pending-skill (runtime-session skill-name)
  "Toggle one-shot SKILL-NAME for RUNTIME-SESSION."
  (let ((skills (magent-runtime-session-pending-skills runtime-session)))
    (setf (magent-runtime-session-pending-skills runtime-session)
          (if (member skill-name skills)
              (remove skill-name skills)
            (append skills (list skill-name))))))

(defun magent-runtime-session-clear-pending-skills (runtime-session)
  "Clear one-shot skills for RUNTIME-SESSION."
  (setf (magent-runtime-session-pending-skills runtime-session) nil))

(defun magent-runtime-api--notify-submission (submission type &rest props)
  "Notify SUBMISSION's observer of TYPE with PROPS."
  (when-let ((observer (magent-runtime-submission-observer submission)))
    (let ((event (append
                  (list :type type
                        :time (float-time)
                        :session-id
                        (magent-runtime-submission-session-id submission)
                        :submission-id
                        (magent-runtime-submission-id submission)
                        :turn-id
                        (magent-runtime-submission-turn-id submission))
                  props)))
      (condition-case err
          (funcall observer event)
        (error
         (magent-log "ERROR runtime observer failed: %s"
                     (error-message-string err)))))))

(defun magent-runtime-api--prepare-turn (runtime-session prompt)
  "Create a queued ledger turn for PROMPT in RUNTIME-SESSION."
  (let* ((session (magent-runtime-session-magent-session runtime-session))
         (thread (magent-session-thread-ledger session))
         (turn (magent-thread-queue-turn
                thread prompt nil (list :source 'runtime-queue))))
    (magent-thread-record-user-message-if-needed
     thread (magent-thread-turn-id turn) prompt nil
     (list :source 'runtime-queue))
    (magent-session-refresh-projections session)
    (magent-thread-turn-id turn)))

(defun magent-runtime-api--submission-live-p (submission)
  "Return non-nil while SUBMISSION should still accept callbacks."
  (memq (magent-runtime-submission-status submission) '(running queued)))

(defun magent-runtime-api--finish-submission (submission status result)
  "Finish SUBMISSION with STATUS and RESULT."
  (setf (magent-runtime-submission-status submission) status
        (magent-runtime-submission-detail submission) result)
  (magent-runtime-api--notify-submission
   submission
   (pcase status
     ('completed 'turn-complete)
     ('cancelled 'turn-cancelled)
     (_ 'turn-failed))
   :status status
   :result result)
  (when (equal (magent-runtime-submission-id submission)
               (and (magent-runtime-queue-active-submission)
                    (magent-runtime-submission-id
                     (magent-runtime-queue-active-submission))))
    (magent-runtime-queue-finish-active
     status result #'magent-runtime-api--start-submission))
  (when-let ((fn (magent-runtime-submission-on-complete submission)))
    (condition-case err
        (funcall fn status result)
      (error
       (magent-log "ERROR runtime completion callback failed: %s"
                   (error-message-string err))))))

(defun magent-runtime-api--activate-submission-session (submission)
  "Make SUBMISSION's session active in the legacy session layer."
  (let ((runtime-session (magent-runtime-submission-session submission)))
    (magent-session-install
     (magent-runtime-session-scope runtime-session)
     (magent-runtime-session-magent-session runtime-session))))

(defun magent-runtime-api--start-submission (submission)
  "Start executing SUBMISSION."
  (magent-runtime-api--activate-submission-session submission)
  (magent-runtime-api--notify-submission submission 'turn-start)
  (magent-runtime-api--notify-submission
   submission 'user-message
   :text (magent-runtime-submission-prompt submission))
  (let* ((runtime-session (magent-runtime-submission-session submission))
         (session (magent-runtime-session-magent-session runtime-session))
         (request-context
          (magent-request-context-create
           :id (magent-runtime-submission-id submission)
           :scope (magent-runtime-session-scope runtime-session)
           :session session
           :turn-id (magent-runtime-submission-turn-id submission)
           :approval-session session
           :ui-visibility 'none
           :origin-context (magent-runtime-submission-context submission)
           :skill-names (magent-runtime-submission-skills submission)
           :approval-provider
           (magent-runtime-submission-approval-provider submission)
           :observer (magent-runtime-submission-observer submission)
           :submission-id (magent-runtime-submission-id submission)
           :live-p (lambda ()
                     (magent-runtime-api--submission-live-p submission)))))
    (setf (magent-runtime-submission-handle submission)
          (magent-agent-run-turn
           :session session
           :prompt (magent-runtime-submission-prompt submission)
           :agent (magent-runtime-submission-agent submission)
           :skills (magent-runtime-submission-skills submission)
           :context (magent-runtime-submission-context submission)
           :request-context request-context
           :on-complete
           (lambda (result)
             (let ((status (if (magent-agent-result-success-p result)
                               'completed
                             'failed)))
               (magent-runtime-api--finish-submission submission status result)))))))

(cl-defun magent-runtime-submit
    (runtime-session prompt &key context skills agent observer approval-provider
                     on-complete)
  "Submit PROMPT to RUNTIME-SESSION.
OBSERVER receives request-local Magent-native events."
  (unless (magent-runtime-session-p runtime-session)
    (error "Expected runtime session, got: %S" runtime-session))
  (unless (and (stringp prompt)
               (not (string-empty-p (string-trim prompt))))
    (error "Prompt is empty"))
  (let* ((effective-skills
          (or skills (magent-runtime-session-pending-skills runtime-session)))
         (turn-id (magent-runtime-api--prepare-turn runtime-session prompt))
         (submission
          (magent-runtime-submission-create
           :id (magent-protocol-generate-id "submission")
           :session runtime-session
           :session-id (magent-runtime-session-id runtime-session)
           :prompt prompt
           :context context
           :skills effective-skills
           :agent agent
           :observer observer
           :approval-provider approval-provider
           :on-complete on-complete
           :turn-id turn-id)))
    (magent-runtime-session-clear-pending-skills runtime-session)
    (magent-runtime-queue-submit submission #'magent-runtime-api--start-submission)))

(defun magent-runtime-processing-p ()
  "Return non-nil when any runtime turn is active."
  (magent-runtime-queue-processing-p))

(defun magent-runtime-pending-count (&optional runtime-session)
  "Return queued turn count, optionally for RUNTIME-SESSION."
  (magent-runtime-queue-length
   (and runtime-session
        (magent-runtime-session-id runtime-session))))

(defun magent-runtime-cancel (runtime-session)
  "Cancel RUNTIME-SESSION active and queued submissions."
  (let* ((session-id (magent-runtime-session-id runtime-session))
         (removed (magent-runtime-queue-remove-session session-id))
         (active (magent-runtime-queue-active-submission)))
    (dolist (submission removed)
      (magent-runtime-api--notify-submission submission 'turn-cancelled
                                        :status 'cancelled
                                        :result "Queued turn cancelled")
      (when-let ((fn (magent-runtime-submission-on-complete submission)))
        (funcall fn 'cancelled "Queued turn cancelled")))
    (when (and active
               (equal (magent-runtime-submission-session-id active)
                      session-id))
      (setf (magent-runtime-submission-status active) 'cancelled)
      (when-let ((handle (magent-runtime-submission-handle active)))
        (when (magent-agent-loop-p handle)
          (magent-agent-loop-abort handle)))
      (magent-runtime-api--finish-submission
       active 'cancelled "Active turn cancelled"))
    (+ (length removed)
       (if (and active
                (equal (magent-runtime-submission-session-id active)
                       session-id))
           1
         0))))

(defun magent-runtime-api--session-id-from-file (file)
  "Return persisted session id for FILE."
  (file-name-sans-extension (file-name-nondirectory file)))

(defun magent-runtime-list-sessions ()
  "Return saved sessions as plists for UI/ACP display."
  (mapcar
   (lambda (file)
     (let* ((metadata (magent-session--read-file-metadata-cached file))
            (scope (if (eq (plist-get metadata :scope) 'global)
                       'global
                     (plist-get metadata :project-root))))
       (list :id (magent-runtime-api--session-id-from-file file)
             :file file
             :scope scope
             :project-root (plist-get metadata :project-root)
             :title (plist-get metadata :summary-title)
             :updated-at (float-time (magent-session--file-display-time file)))))
   (magent-session-list-files)))

(defun magent-runtime-load-session-file (file)
  "Load session FILE and return a runtime session."
  (when-let* ((loaded (magent-session-read-file file))
              (scope (plist-get loaded :scope))
              (session (plist-get loaded :session)))
    (magent-runtime-session-register scope session)))

(provide 'magent-runtime-api)
;;; magent-runtime-api.el ends here
