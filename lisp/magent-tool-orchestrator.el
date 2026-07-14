;;; magent-tool-orchestrator.el --- Permissioned tool orchestration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Central tool approval and execution flow for Magent.  This intentionally
;; omits OS sandboxing; it only coordinates Magent permissions, user
;; approvals, audit hooks, execution callbacks, and event emission.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'magent-approval)
(require 'magent-config)
(require 'magent-lifecycle-events)
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

(defun magent-tool-orchestrator--project-root (request-context)
  "Return the canonical project root carried by REQUEST-CONTEXT."
  (when-let* ((root
               (or (and request-context
                        (magent-request-context-project-root request-context))
                   (and request-context
                        (let ((scope (magent-request-context-scope
                                      request-context)))
                          (and (stringp scope) scope))))))
    (condition-case nil
        (file-truename (expand-file-name root))
      (error (expand-file-name root)))))

(defun magent-tool-orchestrator--canonical-resource
    (file-path request-context)
  "Return canonical FILE-PATH for permission and tool I/O identity."
  (unless (stringp file-path)
    (error "Resource path must be a string"))
  (magent-tools-canonical-resource-path
   file-path
   (magent-tool-orchestrator--project-root request-context)))

(defun magent-tool-orchestrator--call-audit
    (orchestrator tool-spec arg-values decision source)
  "Call ORCHESTRATOR audit hook."
  (when-let* ((fn (magent-tool-orchestrator-audit-function orchestrator)))
    (funcall fn tool-spec arg-values decision source
             (magent-tool-orchestrator-request-context orchestrator))))

(defun magent-tool-orchestrator--accepts-arity-p (function arity)
  "Return non-nil when FUNCTION accepts ARITY arguments."
  (pcase-let ((`(,minimum . ,maximum) (func-arity function)))
    (and (<= minimum arity)
         (or (eq maximum 'many)
             (null maximum)
             (and (numberp maximum) (>= maximum arity))))))

(defun magent-tool-orchestrator--run
    (orchestrator tool-spec cb arg-values &optional resource-identity)
  "Run TOOL-SPEC through ORCHESTRATOR."
  (let ((function (magent-tool-orchestrator-run-tool-function orchestrator)))
    ;; The fourth argument is a compatible extension: legacy/test runners that
    ;; accept the historical three arguments continue to work, while the agent
    ;; loop carries the frozen identity to the actual dequeue point.
    (if (and resource-identity
             (magent-tool-orchestrator--accepts-arity-p function 4))
        (funcall function tool-spec cb arg-values resource-identity)
      (funcall function tool-spec cb arg-values))))

(defun magent-tool-orchestrator--finish-one
    (orchestrator tool-spec arg-values raw-call result)
  "Record one tool RESULT through ORCHESTRATOR callbacks."
  (when-let* ((fn (magent-tool-orchestrator-result-callback orchestrator)))
    (funcall fn tool-spec arg-values raw-call result)))

(defun magent-tool-orchestrator--annotate-approval
    (raw-call decision source)
  "Return RAW-CALL annotated with approval DECISION and SOURCE."
  (let ((call (or raw-call nil)))
    (setq call (plist-put call :approval-decision decision))
    (setq call (plist-put call :approval-source source))
    call))

(defun magent-tool-orchestrator--file-arg-index (orchestrator args-spec)
  "Return file arg index for ARGS-SPEC using ORCHESTRATOR."
  (when-let* ((fn (magent-tool-orchestrator-file-arg-index-function orchestrator)))
    (funcall fn args-spec)))

(defun magent-tool-orchestrator--args-plist (orchestrator args-spec arg-values)
  "Return plist args via ORCHESTRATOR."
  (if-let* ((fn (magent-tool-orchestrator-args-to-plist-function orchestrator)))
      (funcall fn args-spec arg-values)
    arg-values))

(defun magent-tool-orchestrator--summary (orchestrator arg-values args-spec)
  "Return approval summary via ORCHESTRATOR."
  (if-let* ((fn (magent-tool-orchestrator-summarize-function orchestrator)))
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
            (setq raw-call
                  (magent-tool-orchestrator--annotate-approval
                   raw-call 'allow 'bypass))
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
                 (file-arg-index
                  (when perm-key
                    (magent-tool-orchestrator--file-arg-index
                     orchestrator
                     (gptel-tool-args tool-spec))))
                 (raw-file-path
                  (and file-arg-index
                       (nth file-arg-index arg-values)))
                 (project-root nil)
                 (canonicalization-error nil)
                 (file-path
                  (when file-arg-index
                    (condition-case err
                        (progn
                          (setq project-root
                                (magent-tool-orchestrator--project-root
                                 request-context))
                          (magent-tool-orchestrator--canonical-resource
                           raw-file-path request-context))
                      (error
                       (setq canonicalization-error err)
                       nil))))
                 (resolved
                  (when perm-key
                    (if canonicalization-error
                        'deny
                      (magent-permission-resolve
                       permission perm-key file-path project-root))))
                 (override (when perm-key
                             (magent-permission-session-override
                              perm-key approval-session)))
                 (resource-identity
                  (and file-arg-index
                       file-path
                       (not canonicalization-error)
                       (list :file-arg-index file-arg-index
                             :canonical-resource file-path))))
            ;; Freeze the same canonical resource identity that permission
            ;; resolution inspected into the eventual tool invocation.  In
            ;; particular, an approval delay must not allow a symlinked model
            ;; argument to resolve to a different target at execution time.
            (when (and file-arg-index file-path)
              (setq arg-values (copy-sequence arg-values))
              (setf (nth file-arg-index arg-values) file-path))
            (magent-lifecycle-events-emit
             'tool-approval-evaluated
             :context (and request-context
                           (magent-request-context-event-context request-context))
             :audit-context
             (magent-request-context-audit-snapshot request-context)
             :tool-name tool-name
             :perm-key perm-key
             :file file-path
             :decision (if (eq resolved 'deny)
                           'deny
                         (or override resolved 'ask)))
            (cond
             ;; A persisted session choice only resolves an `ask'.  It must
             ;; never relax an explicit tool or resource deny.
             ((eq resolved 'deny)
              (let ((source (cond
                             (canonicalization-error
                              'canonicalization-deny)
                             (file-path 'file-rule-deny)
                             (t 'rule-deny))))
                (magent-log "PERM auto-deny (%s): %s %s"
                            source tool-name (or file-path ""))
                (magent-tool-orchestrator--call-audit
                 orchestrator tool-spec arg-values 'deny source)
                (setq raw-call
                      (magent-tool-orchestrator--annotate-approval
                       raw-call 'deny source))
                (let ((result
                       (if canonicalization-error
                           (format "Error: invalid or unstable resource path for %s"
                                   tool-name)
                         (format "Error: access denied for %s on %s"
                                 tool-name
                                 (or file-path "this resource")))))
                  (when cb
                    (funcall cb result))
                  (complete-one tool-spec arg-values raw-call result))))
             ((eq override 'allow)
              (magent-log "PERM auto-allow (session override): %s" tool-name)
              (magent-tool-orchestrator--call-audit
               orchestrator tool-spec arg-values
               'allow 'session-override-allow)
              (setq raw-call
                    (magent-tool-orchestrator--annotate-approval
                     raw-call 'allow 'session-override-allow))
              (magent-tool-orchestrator--run
               orchestrator tool-spec
               (lambda (result)
                 (when cb
                   (funcall cb result))
                 (complete-one tool-spec arg-values raw-call result))
               arg-values resource-identity))
             ((eq override 'deny)
              (magent-log "PERM auto-deny (session override): %s" tool-name)
              (magent-tool-orchestrator--call-audit
               orchestrator tool-spec arg-values
               'deny 'session-override-deny)
              (setq raw-call
                    (magent-tool-orchestrator--annotate-approval
                     raw-call 'deny 'session-override-deny))
              (let ((result (format "Error: tool '%s' denied by session policy"
                                    tool-name)))
                (when cb
                  (funcall cb result))
                (complete-one tool-spec arg-values raw-call result)))
             ((and file-path (eq resolved 'allow))
              (magent-log "PERM auto-allow (file rule): %s %s"
                          tool-name file-path)
              (magent-tool-orchestrator--call-audit
               orchestrator tool-spec arg-values 'allow 'file-rule-allow)
              (setq raw-call
                    (magent-tool-orchestrator--annotate-approval
                     raw-call 'allow 'file-rule-allow))
              (magent-tool-orchestrator--run
               orchestrator tool-spec
               (lambda (result)
                 (when cb
                   (funcall cb result))
                 (complete-one tool-spec arg-values raw-call result))
               arg-values resource-identity))
             ((eq resolved 'allow)
              (magent-log "PERM auto-allow: %s" tool-name)
              (magent-tool-orchestrator--call-audit
               orchestrator tool-spec arg-values 'allow 'rule-allow)
              (setq raw-call
                    (magent-tool-orchestrator--annotate-approval
                     raw-call 'allow 'rule-allow))
              (magent-tool-orchestrator--run
               orchestrator tool-spec
               (lambda (result)
                 (when cb
                   (funcall cb result))
                 (complete-one tool-spec arg-values raw-call result))
               arg-values resource-identity))
             (t
              (push (list tool-spec arg-values cb raw-call resource-identity)
                    pending)))))
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
           (resource-identity (nth 4 tc))
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
       (list :request-id (magent-lifecycle-events-generate-id)
             :provider (and request-context
                            (magent-request-context-approval-provider
                             request-context))
             :context (and request-context
                           (magent-request-context-event-context request-context))
             :audit-context
             (magent-request-context-audit-snapshot request-context)
             :tool-name tool-name
             :perm-key perm-key
             :summary summary
             :args (magent-tool-orchestrator--args-plist
                    orchestrator (gptel-tool-args tool-spec) arg-values))
       (lambda (decision)
         (magent-request-context-notify
          request-context 'approval-resolved
          :tool-name tool-name
          :perm-key perm-key
          :decision decision
          :args (magent-tool-orchestrator--args-plist
                 orchestrator (gptel-tool-args tool-spec) arg-values))
         (pcase decision
           ('allow-once
            (magent-log "PERM user allowed (once): %s" tool-name)
            (magent-tool-orchestrator--call-audit
             orchestrator tool-spec arg-values 'allow 'user-allow-once)
            (setq raw-call
                  (magent-tool-orchestrator--annotate-approval
                   raw-call 'allow 'user-allow-once))
            (magent-tool-orchestrator--run
             orchestrator tool-spec
             (lambda (result)
               (when cb
                 (funcall cb result))
               (when complete-one
                 (funcall complete-one tool-spec arg-values raw-call result)))
             arg-values resource-identity))
           ('deny-once
            (magent-log "PERM user denied (once): %s" tool-name)
            (magent-tool-orchestrator--call-audit
             orchestrator tool-spec arg-values 'deny 'user-deny-once)
            (setq raw-call
                  (magent-tool-orchestrator--annotate-approval
                   raw-call 'deny 'user-deny-once))
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
            (setq raw-call
                  (magent-tool-orchestrator--annotate-approval
                   raw-call 'allow 'user-allow-session))
            (magent-tool-orchestrator--run
             orchestrator tool-spec
             (lambda (result)
               (when cb
                 (funcall cb result))
               (when complete-one
                 (funcall complete-one tool-spec arg-values raw-call result)))
             arg-values resource-identity))
           ('deny-session
            (magent-log "PERM user always-deny: %s" tool-name)
            (when perm-key
              (magent-permission-set-session-override
               perm-key 'deny approval-session))
            (magent-tool-orchestrator--call-audit
             orchestrator tool-spec arg-values 'deny 'user-deny-session)
            (setq raw-call
                  (magent-tool-orchestrator--annotate-approval
                   raw-call 'deny 'user-deny-session))
            (let ((result (format "Error: tool '%s' denied by user" tool-name)))
              (when cb
                (funcall cb result))
               (when complete-one
                 (funcall complete-one tool-spec arg-values raw-call result)))))
         (magent-tool-orchestrator-prompt-next
          orchestrator rest complete-one))))))

(provide 'magent-tool-orchestrator)
;;; magent-tool-orchestrator.el ends here
