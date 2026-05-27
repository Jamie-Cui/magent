;;; magent-tool-orchestrator.el --- Permissioned tool orchestration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; Central tool approval and execution flow for Magent.  This intentionally
;; omits OS sandboxing; it only coordinates Magent permissions, user
;; approvals, audit hooks, execution callbacks, and event emission.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'magent-approval)
(require 'magent-config)
(require 'magent-events)
(require 'magent-permission)
(require 'magent-protocol)
(require 'magent-tools)

(cl-defstruct (magent-tool-orchestrator
               (:constructor magent-tool-orchestrator-create)
               (:copier nil))
  permission
  request-context
  run-tool-function
  audit-function
  file-arg-index-function
  args-to-plist-function
  summarize-function
  result-callback
  done-callback)

(defun magent-tool-orchestrator--approval-session (request-context)
  "Return approval session for REQUEST-CONTEXT."
  (or (and request-context
           (magent-request-context-approval-session request-context))
      (and request-context
           (magent-request-context-session request-context))))

(defun magent-tool-orchestrator--call-audit
    (orchestrator tool-spec arg-values decision source)
  "Call ORCHESTRATOR audit hook."
  (when-let ((fn (magent-tool-orchestrator-audit-function orchestrator)))
    (funcall fn tool-spec arg-values decision source
             (magent-tool-orchestrator-request-context orchestrator))))

(defun magent-tool-orchestrator--run (orchestrator tool-spec cb arg-values)
  "Run TOOL-SPEC through ORCHESTRATOR."
  (funcall (magent-tool-orchestrator-run-tool-function orchestrator)
           tool-spec cb arg-values))

(defun magent-tool-orchestrator--finish-one
    (orchestrator tool-spec arg-values raw-call result)
  "Record one tool RESULT through ORCHESTRATOR callbacks."
  (when-let ((fn (magent-tool-orchestrator-result-callback orchestrator)))
    (funcall fn tool-spec arg-values raw-call result)))

(defun magent-tool-orchestrator--file-arg-index (orchestrator args-spec)
  "Return file arg index for ARGS-SPEC using ORCHESTRATOR."
  (when-let ((fn (magent-tool-orchestrator-file-arg-index-function orchestrator)))
    (funcall fn args-spec)))

(defun magent-tool-orchestrator--args-plist (orchestrator args-spec arg-values)
  "Return plist args via ORCHESTRATOR."
  (if-let ((fn (magent-tool-orchestrator-args-to-plist-function orchestrator)))
      (funcall fn args-spec arg-values)
    arg-values))

(defun magent-tool-orchestrator--summary (orchestrator arg-values args-spec)
  "Return approval summary via ORCHESTRATOR."
  (if-let ((fn (magent-tool-orchestrator-summarize-function orchestrator)))
      (funcall fn arg-values args-spec)
    (format "%S" arg-values)))

(defun magent-tool-orchestrator-handle-tool-calls (orchestrator tool-calls)
  "Handle TOOL-CALLS using ORCHESTRATOR.
TOOL-CALLS follows gptel's `(TOOL-SPEC ARG-VALUES CALLBACK RAW-CALL)' shape."
  (let* ((permission (magent-tool-orchestrator-permission orchestrator))
         (request-context (magent-tool-orchestrator-request-context orchestrator))
         (approval-session
          (magent-tool-orchestrator--approval-session request-context))
         (remaining (length tool-calls)))
    (cl-labels ((complete-one
                 (tool-spec arg-values raw-call result)
                 (magent-tool-orchestrator--finish-one
                  orchestrator tool-spec arg-values raw-call result)
                 (cl-decf remaining)
                 (when (and (<= remaining 0)
                            (magent-tool-orchestrator-done-callback
                             orchestrator))
                   (funcall (magent-tool-orchestrator-done-callback
                             orchestrator)))))
    (if (magent-permission-bypass-p)
        (dolist (tc tool-calls)
          (let* ((tool-spec (car tc))
                 (arg-values (cadr tc))
                 (cb (caddr tc))
                 (raw-call (nth 3 tc))
                 (tool-name (gptel-tool-name tool-spec)))
            (magent-log "PERM bypass allow: %s" tool-name)
            (magent-tool-orchestrator--call-audit
             orchestrator tool-spec arg-values 'allow 'bypass)
            (magent-tool-orchestrator--run
             orchestrator tool-spec
             (lambda (result)
               (when cb
                 (funcall cb result))
               (complete-one tool-spec arg-values raw-call result))
             arg-values)))
      (let (pending)
        (dolist (tc tool-calls)
          (let* ((tool-spec (car tc))
                 (arg-values (cadr tc))
                 (cb (caddr tc))
                 (raw-call (nth 3 tc))
                 (tool-name (gptel-tool-name tool-spec))
                 (perm-key (magent-tools-permission-key tool-name))
                 (file-path
                  (when perm-key
                    (let ((idx (magent-tool-orchestrator--file-arg-index
                                orchestrator
                                (gptel-tool-args tool-spec))))
                      (when idx
                        (nth idx arg-values)))))
                 (resolved (when perm-key
                             (magent-permission-resolve
                              permission perm-key file-path)))
                 (override (when perm-key
                             (magent-permission-session-override
                              perm-key approval-session))))
            (magent-events-emit
             'tool-approval-evaluated
             :context (and request-context
                           (magent-request-context-event-context request-context))
             :tool-name tool-name
             :perm-key perm-key
             :file file-path
             :decision (or override resolved 'ask))
            (cond
             ((eq override 'allow)
              (magent-log "PERM auto-allow (session override): %s" tool-name)
              (magent-tool-orchestrator--call-audit
               orchestrator tool-spec arg-values
               'allow 'session-override-allow)
              (magent-tool-orchestrator--run
               orchestrator tool-spec
               (lambda (result)
                 (when cb
                   (funcall cb result))
                 (complete-one tool-spec arg-values raw-call result))
               arg-values))
             ((eq override 'deny)
              (magent-log "PERM auto-deny (session override): %s" tool-name)
              (magent-tool-orchestrator--call-audit
               orchestrator tool-spec arg-values
               'deny 'session-override-deny)
              (let ((result (format "Error: tool '%s' denied by session policy"
                                    tool-name)))
                (when cb
                  (funcall cb result))
                (complete-one tool-spec arg-values raw-call result)))
             ((eq resolved 'deny)
              (magent-log "PERM auto-deny (file rule): %s %s"
                          tool-name (or file-path ""))
              (magent-tool-orchestrator--call-audit
               orchestrator tool-spec arg-values 'deny 'file-rule-deny)
              (let ((result (format "Error: access denied for %s on %s"
                                    tool-name
                                    (or file-path "this resource"))))
                (when cb
                  (funcall cb result))
                (complete-one tool-spec arg-values raw-call result)))
             ((and file-path (eq resolved 'allow))
              (magent-log "PERM auto-allow (file rule): %s %s"
                          tool-name file-path)
              (magent-tool-orchestrator--call-audit
               orchestrator tool-spec arg-values 'allow 'file-rule-allow)
              (magent-tool-orchestrator--run
               orchestrator tool-spec
               (lambda (result)
                 (when cb
                   (funcall cb result))
                 (complete-one tool-spec arg-values raw-call result))
               arg-values))
             ((eq resolved 'allow)
              (magent-log "PERM auto-allow: %s" tool-name)
              (magent-tool-orchestrator--call-audit
               orchestrator tool-spec arg-values 'allow 'rule-allow)
              (magent-tool-orchestrator--run
               orchestrator tool-spec
               (lambda (result)
                 (when cb
                   (funcall cb result))
                 (complete-one tool-spec arg-values raw-call result))
               arg-values))
             (t
              (push tc pending)))))
        (when pending
          (magent-tool-orchestrator-prompt-next
           orchestrator (nreverse pending) #'complete-one)))))))

(defun magent-tool-orchestrator-prompt-next
    (orchestrator tool-calls &optional complete-one)
  "Prompt for TOOL-CALLS one by one through ORCHESTRATOR."
  (when tool-calls
    (let* ((tc (car tool-calls))
           (rest (cdr tool-calls))
           (tool-spec (car tc))
           (arg-values (cadr tc))
           (cb (caddr tc))
           (raw-call (nth 3 tc))
           (tool-name (gptel-tool-name tool-spec))
           (perm-key (magent-tools-permission-key tool-name))
           (request-context
            (magent-tool-orchestrator-request-context orchestrator))
           (approval-session
            (magent-tool-orchestrator--approval-session request-context))
           (summary
            (magent-tool-orchestrator--summary
             orchestrator arg-values (gptel-tool-args tool-spec))))
      (magent-approval-request
       (list :request-id (magent-events-generate-id)
             :context (and request-context
                           (magent-request-context-event-context request-context))
             :tool-name tool-name
             :perm-key perm-key
             :summary summary
             :args (magent-tool-orchestrator--args-plist
                    orchestrator (gptel-tool-args tool-spec) arg-values))
       (lambda (decision)
         (pcase decision
           ('allow-once
            (magent-log "PERM user allowed (once): %s" tool-name)
            (magent-tool-orchestrator--call-audit
             orchestrator tool-spec arg-values 'allow 'user-allow-once)
            (magent-tool-orchestrator--run
             orchestrator tool-spec
             (lambda (result)
               (when cb
                 (funcall cb result))
               (when complete-one
                 (funcall complete-one tool-spec arg-values raw-call result)))
             arg-values))
           ('deny-once
            (magent-log "PERM user denied (once): %s" tool-name)
            (magent-tool-orchestrator--call-audit
             orchestrator tool-spec arg-values 'deny 'user-deny-once)
            (let ((result (format "Error: tool '%s' denied by user" tool-name)))
              (when cb
                (funcall cb result))
              (when complete-one
                (funcall complete-one tool-spec arg-values raw-call result))))
           ('allow-session
            (magent-log "PERM user always-allow: %s" tool-name)
            (when perm-key
              (magent-permission-set-session-override
               perm-key 'allow approval-session))
            (magent-tool-orchestrator--call-audit
             orchestrator tool-spec arg-values 'allow 'user-allow-session)
            (magent-tool-orchestrator--run
             orchestrator tool-spec
             (lambda (result)
               (when cb
                 (funcall cb result))
               (when complete-one
                 (funcall complete-one tool-spec arg-values raw-call result)))
             arg-values))
           ('deny-session
            (magent-log "PERM user always-deny: %s" tool-name)
            (when perm-key
              (magent-permission-set-session-override
               perm-key 'deny approval-session))
            (magent-tool-orchestrator--call-audit
             orchestrator tool-spec arg-values 'deny 'user-deny-session)
            (let ((result (format "Error: tool '%s' denied by user" tool-name)))
              (when cb
                (funcall cb result))
              (when complete-one
                (funcall complete-one tool-spec arg-values raw-call result)))))
         (magent-tool-orchestrator-prompt-next
          orchestrator rest complete-one))))))

(provide 'magent-tool-orchestrator)
;;; magent-tool-orchestrator.el ends here
