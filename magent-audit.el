;;; magent-audit.el --- Persistent audit logging for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Compact JSONL audit logging for permission decisions and sensitive
;; agent actions.  This is intentionally separate from *magent-log* so
;; audit data survives Emacs restarts and remains machine-readable.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magent-config)
(require 'magent-approval)
(require 'magent-events)
(require 'magent-session)

(declare-function magent-agent-info-name "magent-agent-registry")

(defconst magent-audit--sensitive-tools
  '("bash" "emacs_eval" "write_file" "edit_file" "delegate")
  "Tool names that are always persisted to the audit log.")

(defvar magent-audit--enabled nil
  "Non-nil when Magent audit hooks are installed.")

(defun magent-audit-enable ()
  "Enable persistent audit logging hooks."
  (unless magent-audit--enabled
    (magent-events-add-sink #'magent-audit--event-sink)
    (add-hook 'magent-approval-state-change-functions
              #'magent-audit--approval-state-changed)
    (setq magent-audit--enabled t))
  magent-audit--enabled)

(defun magent-audit-disable ()
  "Disable persistent audit logging hooks."
  (when magent-audit--enabled
    (magent-events-remove-sink #'magent-audit--event-sink)
    (remove-hook 'magent-approval-state-change-functions
                 #'magent-audit--approval-state-changed)
    (setq magent-audit--enabled nil))
  magent-audit--enabled)

(defun magent-audit-record (event &rest props)
  "Persist an audit EVENT with PROPS as one JSONL record."
  (when magent-enable-audit-log
    (condition-case err
        (let ((record (magent-audit--build-record event props)))
          (when record
            (make-directory (magent-audit--directory) t)
            (with-temp-buffer
              (insert (json-encode record))
              (insert "\n")
              (append-to-file (point-min) (point-max)
                              (magent-audit--daily-file-path)))))
      (error
       (magent-log "WARN audit write failed: %s"
                   (error-message-string err))))))

(defun magent-audit-record-permission-decision (tool-name perm-key decision source
                                                         &rest props)
  "Persist a structured permission decision for TOOL-NAME.
PERM-KEY is the tool permission symbol.  DECISION is the final
decision symbol and SOURCE identifies how that decision was made.
PROPS accepts the same plist keys as `magent-audit-record'."
  (apply #'magent-audit-record
         'permission-decision
         :tool-name tool-name
         :perm-key perm-key
         :decision decision
         :decision-source source
         props))

(defun magent-audit--directory ()
  "Return the directory used for persisted audit logs."
  (expand-file-name
   (or magent-audit-directory
       (expand-file-name "audit" magent-session-directory))))

(defun magent-audit--daily-file-path ()
  "Return today's audit log file path."
  (expand-file-name
   (format "audit-%s.jsonl" (format-time-string "%Y%m%d"))
   (magent-audit--directory)))

(defun magent-audit--build-record (event props)
  "Build one audit record for EVENT from PROPS."
  (let* ((context (or (plist-get props :context)
                      (plist-get props :event-context)
                      (magent-events-current-context)))
         (scope (magent-session-current-scope))
         (project-root (unless (eq scope 'global) scope))
         (session (and (boundp 'magent--current-session) magent--current-session))
         (session-id (when session (magent-session-get-id session)))
         (tool-name (plist-get props :tool-name))
         (args (plist-get props :args)))
    `((timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
      (event . ,(magent-audit--stringify event))
      (agent . ,(magent-audit--current-agent-name session))
      (turn_id . ,(or (plist-get props :turn-id)
                      (and context (magent-events-context-turn-id context))))
      (subagent_id . ,(or (plist-get props :subagent-id)
                          (and context (magent-events-context-subagent-id context))))
      (session_id . ,session-id)
      (scope . ,(magent-audit--scope-name scope))
      (project_root . ,project-root)
      (tool_name . ,tool-name)
      (perm_key . ,(magent-audit--stringify (plist-get props :perm-key)))
      (decision . ,(magent-audit--stringify (plist-get props :decision)))
      (decision_source . ,(magent-audit--stringify (plist-get props :decision-source)))
      (status . ,(magent-audit--stringify (plist-get props :status)))
      (summary . ,(magent-audit--preview (plist-get props :summary)))
      (args_preview . ,(magent-audit--sanitize-args tool-name args))
      (result_preview . ,(magent-audit--preview (plist-get props :result)))
      (call_id . ,(plist-get props :call-id))
      (request_id . ,(plist-get props :request-id))
      (title . ,(magent-audit--preview (plist-get props :title)))
      (detail . ,(magent-audit--preview (plist-get props :detail))))))

(defun magent-audit--scope-name (scope)
  "Return a stable scope name string for SCOPE."
  (cond
   ((eq scope 'global) "global")
   ((stringp scope) "project")
   (t nil)))

(defun magent-audit--stringify (value)
  "Convert VALUE into a stable string or nil."
  (cond
   ((null value) nil)
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   (t (format "%s" value))))

(defun magent-audit--current-agent-name (session)
  "Return the current agent name for SESSION, or nil."
  (when-let ((agent (and session (magent-session-agent session))))
    (if (fboundp 'magent-agent-info-name)
        (magent-agent-info-name agent)
      (format "%s" agent))))

(defun magent-audit--preview (value)
  "Return a compact single-line preview string for VALUE."
  (when value
    (let* ((text (replace-regexp-in-string "[ \t\n\r]+" " "
                                           (string-trim (format "%s" value))))
           (clean (unless (string-empty-p text) text)))
      (when clean
        (truncate-string-to-width clean
                                  magent-audit-preview-length
                                  nil nil "...")))))

(defun magent-audit--sanitize-args (tool-name args)
  "Return a redacted JSON-safe preview for TOOL-NAME ARGS."
  (when args
    (pcase tool-name
      ("write_file"
       (magent-audit--compact-alist
        (cons 'path (plist-get args :path))
        (cons 'content_length (magent-audit--string-length (plist-get args :content)))))
      ("edit_file"
       (magent-audit--compact-alist
        (cons 'path (plist-get args :path))
        (cons 'old_text_length (magent-audit--string-length (plist-get args :old_text)))
        (cons 'new_text_length (magent-audit--string-length (plist-get args :new_text)))))
      ("bash"
       (magent-audit--compact-alist
        (cons 'command (magent-audit--preview (plist-get args :command)))
        (cons 'timeout (plist-get args :timeout))))
      ("emacs_eval"
       (magent-audit--compact-alist
        (cons 'sexp (magent-audit--preview (plist-get args :sexp)))
        (cons 'timeout (plist-get args :timeout))))
      ("delegate"
       (magent-audit--compact-alist
        (cons 'agent (plist-get args :agent))
        (cons 'prompt_preview (magent-audit--preview (plist-get args :prompt)))
        (cons 'prompt_length (magent-audit--string-length (plist-get args :prompt)))))
      ("read_file"
       (magent-audit--compact-alist
        (cons 'path (plist-get args :path))))
      (_
       (let (pairs)
         (while args
           (let ((key (intern (substring (symbol-name (pop args)) 1)))
                 (value (pop args)))
             (push (cons key (magent-audit--preview value)) pairs)))
         (nreverse pairs))))))

(defun magent-audit--compact-alist (&rest pairs)
  "Return PAIRS without null values."
  (delq nil
        (mapcar (lambda (pair)
                  (when (cdr pair) pair))
                pairs)))

(defun magent-audit--string-length (value)
  "Return the string length of VALUE, or nil."
  (when (stringp value)
    (length value)))

(defun magent-audit--tool-status (result)
  "Return a compact status string for RESULT."
  (let ((text (magent-audit--stringify result)))
    (cond
     ((null text) nil)
     ((string-match-p "\\`Error\\b" text) "error")
     ((string-match-p "\\`Command timed out\\b" text) "timeout")
     (t "ok"))))

(defun magent-audit--approval-request-data (entry)
  "Extract the request plist from approval ENTRY."
  (or (plist-get entry :request) entry))

(defun magent-audit--approval-decision-info (event entry)
  "Return a `(DECISION . SOURCE)' pair for approval EVENT and ENTRY."
  (let ((decision (and entry (plist-get entry :decision))))
    (pcase event
      ('requested '(ask . approval-requested))
      ('resolved
       (pcase decision
         ('allow-once '(allow . user-allow-once))
         ('deny-once '(deny . user-deny-once))
         ('allow-session '(allow . user-allow-session))
         ('deny-session '(deny . user-deny-session))
         (_ (cons decision 'approval-resolved))))
      ('dropped '(dropped . approval-dropped))
      ('cleared '(cleared . approval-cleared))
      (_ (cons event event)))))

(defun magent-audit--approval-state-changed (event request-id entry)
  "Persist approval lifecycle EVENT for REQUEST-ID and ENTRY."
  (when (or request-id entry)
    (let* ((request (magent-audit--approval-request-data entry))
           (context (plist-get request :context))
           (decision-info (magent-audit--approval-decision-info event entry)))
      (magent-audit-record
       (intern (format "approval-%s" event))
       :context context
       :request-id request-id
       :tool-name (plist-get request :tool-name)
       :perm-key (plist-get request :perm-key)
       :decision (car decision-info)
       :decision-source (cdr decision-info)
       :status (pcase event
                 ('requested 'pending)
                 ('resolved 'completed)
                 (_ event))
       :summary (plist-get request :summary)
       :args (plist-get request :args)
       :detail (and (eq event 'resolved)
                    (magent-audit--stringify (plist-get entry :decision)))))))

(defun magent-audit--event-sink (event)
  "Persist supported EVENT plists to the audit log."
  (pcase (plist-get event :type)
    ((or 'tool-call-start 'tool-call-end)
     (when (member (plist-get event :tool-name) magent-audit--sensitive-tools)
       (magent-audit-record
        (plist-get event :type)
        :context (plist-get event :context)
        :turn-id (plist-get event :turn-id)
        :subagent-id (plist-get event :subagent-id)
        :tool-name (plist-get event :tool-name)
        :call-id (plist-get event :call-id)
        :status (and (eq (plist-get event :type) 'tool-call-end)
                     (magent-audit--tool-status (plist-get event :result)))
        :summary (or (plist-get event :summary)
                     (plist-get event :description))
        :args (plist-get event :args)
        :result (plist-get event :result)
        :title (plist-get event :title))))
    ('subagent-start
     (magent-audit-record
      'subagent-start
      :context (plist-get event :context)
      :turn-id (plist-get event :turn-id)
      :subagent-id (plist-get event :subagent-id)
      :status 'started
      :title (plist-get event :title)
      :summary (plist-get event :title)))
    ('subagent-stop
     (magent-audit-record
      'subagent-stop
      :context (plist-get event :context)
      :turn-id (plist-get event :turn-id)
      :subagent-id (plist-get event :subagent-id)
      :status 'stopped))))

(provide 'magent-audit)
;;; magent-audit.el ends here
