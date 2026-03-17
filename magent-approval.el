;;; magent-approval.el --- Approval providers for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; Pluggable approval providers for Magent tool confirmations.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar magent-approval-provider-function #'magent-approval-local-request
  "Function used to request tool approval.
The function is called with a REQUEST plist that always includes
`:request-id'.  Providers must eventually resolve the request via
`magent-approval-resolve-request'.")

(defvar magent-approval-state-change-functions nil
  "Hook run after approval state changes.
Each function receives EVENT, REQUEST-ID, and ENTRY.")

(defvar magent-approval--pending-requests (make-hash-table :test 'equal)
  "Pending approval requests keyed by request id.
Each value is a plist with keys `:request', `:callback', and `:provider'.")

(defvar magent-approval--completed-requests (make-hash-table :test 'equal)
  "Completed approval requests keyed by request id.
Each value is a plist with keys `:request', `:provider', `:decision',
and `:completed-at'.")

(defvar magent-approval--local-prompt-timers (make-hash-table :test 'equal)
  "Pending local approval prompt timers keyed by request id.")

(defun magent-approval--normalize-request (request)
  "Return REQUEST with a stable `:request-id'."
  (let ((copy (copy-sequence request)))
    (unless (plist-get copy :request-id)
      (setq copy (plist-put copy :request-id
                            (format "approval-%s-%06x"
                                    (format-time-string "%Y%m%d%H%M%S")
                                    (random #xFFFFFF)))))
    copy))

(defun magent-approval--notify-state-change (event request-id entry)
  "Notify listeners that approval state changed.
EVENT is a symbol, REQUEST-ID identifies the changed request, and ENTRY
is the current pending/completed entry when applicable."
  (run-hook-with-args 'magent-approval-state-change-functions
                      event request-id entry))

(defun magent-approval--local-clear-prompt-timer (request-id)
  "Cancel and forget the local prompt timer for REQUEST-ID."
  (when request-id
    (when-let ((timer (gethash request-id magent-approval--local-prompt-timers)))
      (cancel-timer timer))
    (remhash request-id magent-approval--local-prompt-timers)))

(defun magent-approval--local-forget-prompt-timer (request-id)
  "Forget the local prompt timer for REQUEST-ID without cancelling it."
  (when request-id
    (remhash request-id magent-approval--local-prompt-timers)))

(defun magent-approval--local-state-changed (event request-id entry)
  "Clean up local prompt timers after approval state EVENT.
REQUEST-ID and ENTRY follow `magent-approval-state-change-functions'."
  (pcase event
    ((or 'resolved 'dropped 'cleared)
     (when (or (null entry)
               (eq (plist-get entry :provider) #'magent-approval-local-request))
       (magent-approval--local-clear-prompt-timer request-id)))))

(defun magent-approval-pending-request (request-id)
  "Return the pending request plist for REQUEST-ID, or nil."
  (when-let ((entry (gethash request-id magent-approval--pending-requests)))
    (plist-get entry :request)))

(defun magent-approval-request-provider (request-id)
  "Return the provider function recorded for REQUEST-ID, or nil."
  (when-let ((entry (gethash request-id magent-approval--pending-requests)))
    (plist-get entry :provider)))

(defun magent-approval-completed-request (request-id)
  "Return the completed request entry for REQUEST-ID, or nil."
  (gethash request-id magent-approval--completed-requests))

(defun magent-approval-pending-count (&optional predicate)
  "Return the number of pending approval requests.
When PREDICATE is non-nil, count only requests for which
`(funcall PREDICATE REQUEST-ID ENTRY)' returns non-nil."
  (let ((count 0))
    (maphash (lambda (request-id entry)
               (when (or (null predicate)
                         (funcall predicate request-id entry))
                 (setq count (1+ count))))
             magent-approval--pending-requests)
    count))

(defun magent-approval-completed-count (&optional predicate)
  "Return the number of completed approval requests.
When PREDICATE is non-nil, count only requests for which
`(funcall PREDICATE REQUEST-ID ENTRY)' returns non-nil."
  (let ((count 0))
    (maphash (lambda (request-id entry)
               (when (or (null predicate)
                         (funcall predicate request-id entry))
                 (setq count (1+ count))))
             magent-approval--completed-requests)
    count))

(defun magent-approval-resolve-request (request-id decision)
  "Resolve REQUEST-ID with DECISION and invoke its callback.
Return non-nil when a pending request was found."
  (when-let ((entry (gethash request-id magent-approval--pending-requests)))
    (let* ((callback (plist-get entry :callback))
           (completed-entry (list :request (plist-get entry :request)
                                  :provider (plist-get entry :provider)
                                  :decision decision
                                  :completed-at (float-time))))
      (remhash request-id magent-approval--pending-requests)
      (puthash request-id completed-entry magent-approval--completed-requests)
      (when callback
        (condition-case err
            (funcall callback decision)
          (error
           (message "Magent approval callback failed: %s"
                    (error-message-string err)))))
      (magent-approval--notify-state-change 'resolved request-id completed-entry))
    t))

(defun magent-approval-clear-completed (&optional predicate)
  "Remove completed requests matching PREDICATE.
When PREDICATE is nil, clear all completed approvals."
  (if (null predicate)
      (progn
        (clrhash magent-approval--completed-requests)
        (magent-approval--notify-state-change 'cleared nil nil))
    (let (request-ids)
      (maphash (lambda (request-id entry)
                 (when (funcall predicate request-id entry)
                   (push request-id request-ids)))
               magent-approval--completed-requests)
      (dolist (request-id request-ids)
        (when-let ((entry (gethash request-id magent-approval--completed-requests)))
          (remhash request-id magent-approval--completed-requests)
          (magent-approval--notify-state-change 'cleared request-id entry))))))

(defun magent-approval-cancel-requests (&optional predicate decision)
  "Resolve pending requests matching PREDICATE with DECISION.
PREDICATE follows the same calling convention as
`magent-approval-pending-count'.  DECISION defaults to `deny-once'.
Return the number of cancelled requests."
  (let ((cancelled 0)
        (decision (or decision 'deny-once))
        request-ids)
    (maphash (lambda (request-id entry)
               (when (or (null predicate)
                         (funcall predicate request-id entry))
                 (push request-id request-ids)))
             magent-approval--pending-requests)
    (dolist (request-id request-ids)
      (when (magent-approval-resolve-request request-id decision)
        (setq cancelled (1+ cancelled))))
    cancelled))

(defun magent-approval-drop-requests (&optional predicate)
  "Discard pending requests matching PREDICATE without invoking callbacks.
Return the number of dropped requests."
  (let ((dropped 0)
        request-ids)
    (maphash (lambda (request-id entry)
               (when (or (null predicate)
                         (funcall predicate request-id entry))
                 (push request-id request-ids)))
             magent-approval--pending-requests)
    (dolist (request-id request-ids)
      (when-let ((entry (gethash request-id magent-approval--pending-requests)))
        (remhash request-id magent-approval--pending-requests)
        (setq dropped (1+ dropped))
        (magent-approval--notify-state-change 'dropped request-id entry)))
    dropped))

(defun magent-approval-request (request callback)
  "Request approval for REQUEST and deliver the decision to CALLBACK.
Returns the assigned request id."
  (let* ((provider magent-approval-provider-function)
         (normalized (magent-approval--normalize-request request))
         (request-id (plist-get normalized :request-id)))
    (puthash request-id
             (list :request normalized
                   :callback callback
                   :provider provider)
             magent-approval--pending-requests)
    (condition-case err
        (progn
          (funcall provider normalized)
          (magent-approval--notify-state-change
           'requested request-id (gethash request-id magent-approval--pending-requests))
          request-id)
      (error
       (remhash request-id magent-approval--pending-requests)
       (signal (car err) (cdr err))))))

(defun magent-approval-local-request (request)
  "Request local approval for REQUEST, then resolve it."
  (let* ((request-id (plist-get request :request-id))
         (tool-name (plist-get request :tool-name))
         (summary (plist-get request :summary))
         (timer
          (run-at-time
           0 nil
           (lambda (request-id tool-name summary)
             (magent-approval--local-forget-prompt-timer request-id)
             (when (magent-approval-pending-request request-id)
               (let* ((summary (or summary ""))
                      (prompt (format "magent: allow %s%s? [y]es/[n]o/[A]lways/[D]eny always: "
                                      tool-name
                                      (if (string-empty-p summary)
                                          ""
                                        (format " (%s)" summary))))
                      (choice (read-char-choice prompt '(?y ?n ?A ?D))))
                 (magent-approval-resolve-request
                  request-id
                  (pcase choice
                    (?y 'allow-once)
                    (?n 'deny-once)
                    (?A 'allow-session)
                    (?D 'deny-session))))))
           request-id tool-name summary)))
    (when timer
      (puthash request-id timer magent-approval--local-prompt-timers))))

(add-hook 'magent-approval-state-change-functions
          #'magent-approval--local-state-changed)

(provide 'magent-approval)
;;; magent-approval.el ends here
