;;; magent-tool-orchestrator.el --- Permissioned tool orchestration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

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
  prepare-result-function
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

(defun magent-tool-orchestrator--run
    (orchestrator tool-spec cb arg-values &optional resource-identity)
  "Run TOOL-SPEC through ORCHESTRATOR."
  (let ((function (magent-tool-orchestrator-run-tool-function orchestrator)))
    (funcall function tool-spec cb arg-values resource-identity)))

(defun magent-tool-orchestrator--finish-one
    (orchestrator tool-spec arg-values raw-call result)
  "Record one tool RESULT through ORCHESTRATOR callbacks."
  (when-let* ((fn (magent-tool-orchestrator-result-callback orchestrator)))
    (funcall fn tool-spec arg-values raw-call result)))

(defun magent-tool-orchestrator--deliver-provider-result (callback result)
  "Deliver structured RESULT to provider CALLBACK as model-visible text."
  (when callback
    (funcall callback (magent-tool-result-output-string result))))

(defun magent-tool-orchestrator--prepare-result (orchestrator result)
  "Return RESULT prepared once for provider and persistence."
  (if-let* ((function
             (magent-tool-orchestrator-prepare-result-function orchestrator)))
      (funcall function result)
    result))

(defun magent-tool-orchestrator--deliver-and-finish
    (orchestrator tool-spec arg-values raw-call provider-callback
                  complete-one result &optional prepared-p)
  "Deliver and record RESULT through callbacks.
Unless PREPARED-P is non-nil, prepare RESULT exactly once first."
  (let ((prepared (if prepared-p
                      result
                    (magent-tool-orchestrator--prepare-result
                     orchestrator result))))
    (magent-tool-orchestrator--deliver-provider-result
     provider-callback prepared)
    (when complete-one
      (funcall complete-one tool-spec arg-values raw-call prepared))
    prepared))

(defun magent-tool-orchestrator--failed-result (message)
  "Return a structured orchestration failure for MESSAGE."
  (magent-tool-result-create
   :status 'failed :success nil :output message :error message))

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
    (cl-labels
        ((complete-one
          (tool-spec arg-values raw-call result)
          (magent-tool-orchestrator--finish-one
           orchestrator tool-spec arg-values raw-call result)
          (cl-decf remaining)
          (when (and (<= remaining 0)
                     (magent-tool-orchestrator-done-callback orchestrator))
            (funcall (magent-tool-orchestrator-done-callback orchestrator)))))
      (let (pending)
        (dolist (tc tool-calls)
          (let* ((tool-spec (car tc))
                 (arg-values (cadr tc))
                 (cb (caddr tc))
                 (raw-call (nth 3 tc))
                 (tool-name (gptel-tool-name tool-spec))
                 (perm-key (magent-tools-permission-key tool-name))
                 (approval-policy (magent-tools-approval-policy tool-name))
                 (file-arg-index
                  (when perm-key
                    (magent-tool-orchestrator--file-arg-index
                     orchestrator (gptel-tool-args tool-spec))))
                 (raw-file-path
                  (and file-arg-index (nth file-arg-index arg-values)))
                 project-root
                 canonicalization-error
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
                 (rule-decision
                  (and perm-key
                       (not canonicalization-error)
                       (magent-permission-resolve
                        permission perm-key file-path project-root)))
                 (raw-override
                  (and perm-key
                       (magent-permission-session-override
                        perm-key approval-session)))
                 (effective
                  (if canonicalization-error
                      (list :decision 'deny :source 'canonicalization-deny)
                    (magent-permission-effective-decision
                     rule-decision approval-policy raw-override)))
                 (decision (plist-get effective :decision))
                 (policy-source (plist-get effective :source))
                 (source
                  (pcase policy-source
                    ('rule
                     (if file-path
                         (if (eq decision 'allow)
                             'file-rule-allow
                           'file-rule-deny)
                       (if (eq decision 'allow) 'rule-allow 'rule-deny)))
                    ('session-override
                     (if (eq decision 'allow)
                         'session-override-allow
                       'session-override-deny))
                    (_ policy-source)))
                 (resource-identity
                  (and file-arg-index file-path
                       (list :file-arg-index file-arg-index
                             :canonical-resource file-path))))
            ;; Freeze the same canonical resource identity that permission
            ;; resolution inspected into the eventual tool invocation.
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
             :decision decision)
            (pcase decision
              ('deny
               (magent-log "PERM auto-deny (%s): %s %s"
                           source tool-name (or file-path ""))
               (magent-tool-orchestrator--call-audit
                orchestrator tool-spec arg-values 'deny source)
               (setq raw-call
                     (magent-tool-orchestrator--annotate-approval
                      raw-call 'deny source))
               (let ((result
                      (magent-tool-orchestrator--failed-result
                       (cond
                        (canonicalization-error
                         (format
                          "Error: invalid or unstable resource path for %s"
                          tool-name))
                        ((eq source 'session-override-deny)
                         (format "Error: tool '%s' denied by session policy"
                                 tool-name))
                        (t
                         (format "Error: access denied for %s on %s"
                                 tool-name
                                 (or file-path "this resource")))))))
                 (magent-tool-orchestrator--deliver-and-finish
                  orchestrator tool-spec arg-values raw-call cb
                  #'complete-one result)))
              ('allow
               (magent-log "PERM auto-allow (%s): %s" source tool-name)
               (magent-tool-orchestrator--call-audit
                orchestrator tool-spec arg-values 'allow source)
               (setq raw-call
                     (magent-tool-orchestrator--annotate-approval
                      raw-call 'allow source))
               (magent-tool-orchestrator--run
                orchestrator tool-spec
                (lambda (result)
                  (magent-tool-orchestrator--deliver-and-finish
                   orchestrator tool-spec arg-values raw-call cb
                   #'complete-one result t))
                arg-values resource-identity))
              (_
               (push (list tool-spec arg-values cb raw-call resource-identity)
                     pending)))))
        (when pending
          (magent-tool-orchestrator-prompt-next
           orchestrator (nreverse pending) #'complete-one))))))

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
           (approval-policy (magent-tools-approval-policy tool-name))
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
             :approval-policy approval-policy
             :summary summary
             :args (magent-tool-orchestrator--args-plist
                    orchestrator (gptel-tool-args tool-spec) arg-values))
       (lambda (decision)
         (setq decision
               (magent-approval-normalize-decision
                (list :approval-policy approval-policy) decision))
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
               (magent-tool-orchestrator--deliver-and-finish
                orchestrator tool-spec arg-values raw-call cb complete-one
                result t))
             arg-values resource-identity))
           ('deny-once
            (magent-log "PERM user denied (once): %s" tool-name)
            (magent-tool-orchestrator--call-audit
             orchestrator tool-spec arg-values 'deny 'user-deny-once)
            (setq raw-call
                  (magent-tool-orchestrator--annotate-approval
                   raw-call 'deny 'user-deny-once))
            (let ((result
                   (magent-tool-orchestrator--failed-result
                    (format "Error: tool '%s' denied by user" tool-name))))
              (magent-tool-orchestrator--deliver-and-finish
               orchestrator tool-spec arg-values raw-call cb complete-one
               result)))
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
               (magent-tool-orchestrator--deliver-and-finish
                orchestrator tool-spec arg-values raw-call cb complete-one
                result t))
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
            (let ((result
                   (magent-tool-orchestrator--failed-result
                    (format "Error: tool '%s' denied by user" tool-name))))
              (magent-tool-orchestrator--deliver-and-finish
               orchestrator tool-spec arg-values raw-call cb complete-one
               result))))
         (magent-tool-orchestrator-prompt-next
          orchestrator rest complete-one))))))

(provide 'magent-tool-orchestrator)
;;; magent-tool-orchestrator.el ends here
