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
(require 'pp)
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

(defvar magent-audit--pending-writes nil
  "Queued audit payloads waiting to be flushed to disk.
Each entry is a cons cell of the form (FILE . JSONL-LINE).")

(defvar magent-audit--flush-timer nil
  "Idle timer used to flush queued audit records to disk.")

(defvar magent-audit-entry-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'magent-audit-toggle-entry)
    map)
  "Keymap used on audit entry summaries.")

(defvar-local magent-audit--all-records nil
  "All audit records loaded into the current audit buffer.")

(defvar-local magent-audit--visible-records nil
  "Audit records currently rendered in the buffer.")

(defvar-local magent-audit--filters nil
  "Current audit browser filters as a plist.")

(defvar-local magent-audit--expanded-ids nil
  "Record ids expanded in the current audit buffer.")

(defvar-local magent-audit--load-errors 0
  "Number of audit lines skipped during the last refresh.")

(defvar-local magent-audit--truncated-count 0
  "Number of matching records omitted from the current render.")

(defvar magent-audit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'magent-audit-refresh)
    (define-key map (kbd "c") #'magent-audit-clear-filters)
    (define-key map (kbd "e") #'magent-audit-filter-by-event)
    (define-key map (kbd "t") #'magent-audit-filter-by-tool)
    (define-key map (kbd "d") #'magent-audit-filter-by-decision)
    (define-key map (kbd "s") #'magent-audit-filter-by-status)
    (define-key map (kbd "a") #'magent-audit-filter-by-agent)
    (define-key map (kbd "RET") #'magent-audit-toggle-entry)
    (define-key map (kbd "TAB") #'magent-audit-toggle-entry)
    map)
  "Keymap for `magent-audit-mode'.")

(define-derived-mode magent-audit-mode special-mode "MagentAudit"
  "Major mode for browsing Magent audit records."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'magent-audit-refresh)
  (setq-local header-line-format '(:eval (magent-audit--header-line))))

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
    (magent-audit--flush-pending)
    (setq magent-audit--enabled nil))
  magent-audit--enabled)

(defun magent-audit-record (event &rest props)
  "Persist an audit EVENT with PROPS as one JSONL record."
  (when magent-enable-audit-log
    (condition-case err
        (let ((record (magent-audit--build-record event props)))
          (when record
            (magent-audit--enqueue-record
             (magent-audit--daily-file-path)
             (concat (json-encode record) "\n"))))
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

(defun magent-audit--enqueue-record (file line)
  "Queue audit LINE for FILE and schedule an idle flush."
  (push (cons file line) magent-audit--pending-writes)
  (magent-audit--schedule-flush))

(defun magent-audit--schedule-flush ()
  "Schedule an idle flush for queued audit records."
  (unless (timerp magent-audit--flush-timer)
    (setq magent-audit--flush-timer
          (run-with-idle-timer
           (max 0.0 (or magent-audit-flush-delay 0.0))
           nil
           #'magent-audit--flush-pending))))

(defun magent-audit--flush-pending ()
  "Write queued audit records to disk in batched appends."
  (when (timerp magent-audit--flush-timer)
    (cancel-timer magent-audit--flush-timer))
  (setq magent-audit--flush-timer nil)
  (when magent-audit--pending-writes
    (condition-case err
        (let ((writes (prog1 (nreverse magent-audit--pending-writes)
                        (setq magent-audit--pending-writes nil)))
              grouped)
          (dolist (entry writes)
            (let* ((file (car entry))
                   (line (cdr entry))
                   (existing (assoc file grouped)))
              (if existing
                  (setcdr existing (concat (cdr existing) line))
                (push (cons file line) grouped))))
          (dolist (entry (nreverse grouped))
            (make-directory (file-name-directory (car entry)) t)
            (with-temp-buffer
              (insert (cdr entry))
              (append-to-file (point-min) (point-max) (car entry)))))
      (error
       (magent-log "WARN audit write failed: %s"
                   (error-message-string err))))))

(defun magent-audit-get-buffer ()
  "Return the Magent audit browser buffer."
  (let ((buffer (get-buffer-create magent-audit-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magent-audit-mode)
        (magent-audit-mode)))
    buffer))

(defun magent-show-audit ()
  "Display the Magent audit browser."
  (interactive)
  (magent-audit--flush-pending)
  (let ((buffer (magent-audit-get-buffer)))
    (with-current-buffer buffer
      (unless magent-audit--filters
        (setq magent-audit--filters nil))
      (magent-audit-refresh))
    (display-buffer buffer)
    buffer))

(defun magent-audit-refresh (&optional _ignore-auto _noconfirm preserve-entry-id)
  "Reload and redraw the Magent audit browser.
PRESERVE-ENTRY-ID is the record id to keep point on after
refresh.  The first two arguments follow `revert-buffer'."
  (interactive)
  (magent-audit--flush-pending)
  (let ((buffer (magent-audit-get-buffer)))
    (with-current-buffer buffer
      (setq preserve-entry-id (or preserve-entry-id
                                  (magent-audit--entry-id-at-point)))
      (pcase-let* ((`(,records . ,errors) (magent-audit--load-records))
                   (filtered (magent-audit--apply-filters records magent-audit--filters))
                   (limit (and (integerp magent-audit-max-records)
                               (> magent-audit-max-records 0)
                               magent-audit-max-records))
                   (visible (if (and limit (> (length filtered) limit))
                                (cl-subseq filtered 0 limit)
                              filtered)))
        (setq magent-audit--all-records records
              magent-audit--visible-records visible
              magent-audit--load-errors errors
              magent-audit--truncated-count (max 0 (- (length filtered)
                                                      (length visible))))
        (magent-audit--render preserve-entry-id)))
    buffer))

(defun magent-audit-clear-filters ()
  "Clear all active audit browser filters."
  (interactive)
  (unless (derived-mode-p 'magent-audit-mode)
    (user-error "Current buffer is not a Magent audit buffer"))
  (setq magent-audit--filters nil
        magent-audit--expanded-ids nil)
  (magent-audit-refresh)
  (message "Magent audit filters cleared"))

(defun magent-audit-filter-by-event ()
  "Set the current audit event filter."
  (interactive)
  (magent-audit--prompt-and-set-filter :event "Event"))

(defun magent-audit-filter-by-tool ()
  "Set the current audit tool filter."
  (interactive)
  (magent-audit--prompt-and-set-filter :tool-name "Tool"))

(defun magent-audit-filter-by-decision ()
  "Set the current audit decision filter."
  (interactive)
  (magent-audit--prompt-and-set-filter :decision "Decision"))

(defun magent-audit-filter-by-status ()
  "Set the current audit status filter."
  (interactive)
  (magent-audit--prompt-and-set-filter :status "Status"))

(defun magent-audit-filter-by-agent ()
  "Set the current audit agent filter."
  (interactive)
  (magent-audit--prompt-and-set-filter :agent "Agent"))

(defun magent-audit-toggle-entry ()
  "Toggle details for the audit entry at point."
  (interactive)
  (unless (derived-mode-p 'magent-audit-mode)
    (user-error "Current buffer is not a Magent audit buffer"))
  (let ((entry-id (magent-audit--entry-id-at-point)))
    (unless entry-id
      (user-error "No Magent audit entry at point"))
    (if (member entry-id magent-audit--expanded-ids)
        (setq magent-audit--expanded-ids
              (delete entry-id (copy-sequence magent-audit--expanded-ids)))
      (push entry-id magent-audit--expanded-ids))
    (magent-audit-refresh nil nil entry-id)))

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

(defun magent-audit--load-records ()
  "Return `(RECORDS . LOAD-ERRORS)' for the current audit window."
  (let ((files (magent-audit--list-files))
        (cutoff (magent-audit--cutoff-time))
        records
        (load-errors 0))
    (dolist (file files)
      (pcase-let ((`(,file-records . ,file-errors)
                   (magent-audit--read-file file)))
        (setq load-errors (+ load-errors file-errors))
        (dolist (record file-records)
          (when (magent-audit--record-in-window-p record cutoff)
            (push record records)))))
    (setq records (sort records #'magent-audit--record-newer-p))
    (cons records load-errors)))

(defun magent-audit--list-files ()
  "Return available audit JSONL files."
  (let ((directory (magent-audit--directory)))
    (if (file-directory-p directory)
        (sort (directory-files directory t "\\.jsonl\\'")
              #'string>)
      nil)))

(defun magent-audit--cutoff-time ()
  "Return the oldest time included by default, or nil for all data."
  (when (and (integerp magent-audit-default-days)
             (> magent-audit-default-days 0))
    (time-subtract (current-time)
                   (days-to-time magent-audit-default-days))))

(defun magent-audit--read-file (file)
  "Return `(RECORDS . LOAD-ERRORS)' loaded from audit FILE."
  (let (records
        (load-errors 0))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (line (split-string (buffer-string) "\n" t))
        (condition-case err
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'symbol))
              (push (json-read-from-string line) records))
          (error
           (setq load-errors (1+ load-errors))
           (magent-log "WARN audit ui skipped malformed record in %s: %s"
                       file
                       (error-message-string err))))))
    (cons (nreverse records) load-errors)))

(defun magent-audit--record-newer-p (left right)
  "Return non-nil when LEFT should sort before RIGHT."
  (let ((left-time (magent-audit--record-time left))
        (right-time (magent-audit--record-time right)))
    (time-less-p right-time left-time)))

(defun magent-audit--record-time (record)
  "Return the parsed timestamp of RECORD."
  (or (alist-get '_parsed_time record)
      (let ((parsed (condition-case nil
                        (date-to-time (or (alist-get 'timestamp record) ""))
                      (error (seconds-to-time 0)))))
        (setf (alist-get '_parsed_time record) parsed)
        parsed)))

(defun magent-audit--record-in-window-p (record cutoff)
  "Return non-nil when RECORD is newer than CUTOFF or CUTOFF is nil."
  (or (null cutoff)
      (not (time-less-p (magent-audit--record-time record) cutoff))))

(defun magent-audit--apply-filters (records filters)
  "Return RECORDS that satisfy FILTERS."
  (if (null filters)
      records
    (cl-remove-if-not
     (lambda (record)
       (magent-audit--record-matches-p record filters))
     records)))

(defun magent-audit--record-matches-p (record filters)
  "Return non-nil when RECORD matches FILTERS."
  (let ((matches t))
    (while (and filters matches)
      (let ((key (pop filters))
            (value (pop filters)))
        (when value
          (setq matches
                (equal (magent-audit--filter-value record key) value)))))
    matches))

(defun magent-audit--filter-value (record key)
  "Return the comparable filter value from RECORD for KEY."
  (alist-get (magent-audit--filter-field key) record))

(defun magent-audit--filter-field (key)
  "Return the record field symbol corresponding to filter KEY."
  (pcase key
    (:tool-name 'tool_name)
    (_ (intern (substring (symbol-name key) 1)))))

(defun magent-audit--prompt-and-set-filter (key prompt)
  "Prompt for PROMPT and set KEY in the active audit buffer."
  (unless (derived-mode-p 'magent-audit-mode)
    (user-error "Current buffer is not a Magent audit buffer"))
  (let* ((current (plist-get magent-audit--filters key))
         (choices (magent-audit--filter-choices key))
         (value (completing-read
                 (if current
                     (format "%s (RET to clear, current %s): " prompt current)
                   (format "%s (RET to clear): " prompt))
                 choices nil nil nil nil current)))
    (magent-audit--set-filter-value key (unless (string-empty-p value) value))))

(defun magent-audit--set-filter-value (key value)
  "Set audit filter KEY to VALUE in the current buffer."
  (setq magent-audit--filters (plist-put magent-audit--filters key value)
        magent-audit--expanded-ids nil)
  (magent-audit-refresh)
  (message (if value
               "Magent audit %s filter: %s"
             "Magent audit %s filter cleared")
           (substring (symbol-name key) 1)
           (or value "")))

(defun magent-audit--filter-choices (key)
  "Return sorted distinct choices for filter KEY."
  (let ((field (magent-audit--filter-field key))
        values)
    (dolist (record magent-audit--all-records)
      (when-let ((value (alist-get field record)))
        (push value values)))
    (sort (delete-dups values) #'string-lessp)))

(defun magent-audit--render (&optional preserve-entry-id)
  "Render the current audit buffer.
When PRESERVE-ENTRY-ID is non-nil, restore point to that entry."
  (let ((inhibit-read-only t)
        (buffer (current-buffer)))
    (erase-buffer)
    (cond
     ((null magent-audit--all-records)
      (insert "No audit records found.\n"))
     ((null magent-audit--visible-records)
      (insert "No audit records matched the current filters.\n"))
     (t
      (when (> magent-audit--load-errors 0)
        (insert (format "%d malformed audit line(s) were skipped.\n\n"
                        magent-audit--load-errors)))
      (dolist (record magent-audit--visible-records)
        (magent-audit--insert-record record))))
    (goto-char (point-min))
    (when preserve-entry-id
      (magent-audit--goto-entry preserve-entry-id))
    (set-buffer-modified-p nil)
    buffer))

(defun magent-audit--insert-record (record)
  "Insert one audit RECORD into the current buffer."
  (let* ((entry-id (magent-audit--record-id record))
         (expanded (member entry-id magent-audit--expanded-ids))
         (start (point)))
    (insert (magent-audit--summary-line record expanded))
    (put-text-property start (point) 'magent-audit-entry-id entry-id)
    (put-text-property start (point) 'mouse-face 'highlight)
    (put-text-property start (point) 'keymap magent-audit-entry-map)
    (when expanded
      (let ((detail-start (point)))
        (insert (magent-audit--detail-block record))
        (put-text-property detail-start (point) 'magent-audit-entry-id entry-id)))
    (insert "\n")))

(defun magent-audit--summary-line (record expanded)
  "Return the one-line summary for audit RECORD.
When EXPANDED is non-nil, include the expanded marker."
  (let* ((timestamp (magent-audit--display-time (alist-get 'timestamp record)))
         (decision (alist-get 'decision record))
         (status (alist-get 'status record))
         (event (alist-get 'event record))
         (tool (alist-get 'tool_name record))
         (summary (or (alist-get 'summary record)
                      (alist-get 'title record)
                      (alist-get 'detail record)
                      (alist-get 'result_preview record)
                      ""))
         (marker (if expanded "[-]" "[+]"))
         (primary (or decision status event "record")))
    (concat
     (propertize marker 'face 'font-lock-comment-face)
     " "
     (propertize timestamp 'face 'font-lock-comment-face)
     " "
     (magent-audit--propertize-badge primary)
     (if event
         (concat " " (propertize event 'face 'font-lock-function-name-face))
       "")
     (if tool
         (concat " " (propertize tool 'face 'font-lock-variable-name-face))
       "")
     (if (string-empty-p summary)
         ""
       (concat " " summary))
     "\n")))

(defun magent-audit--detail-block (record)
  "Return the multi-line detail block for RECORD."
  (let ((lines nil))
    (dolist (field '(timestamp event agent scope project_root turn_id subagent_id
                               session_id request_id call_id tool_name perm_key
                               decision decision_source status title summary detail
                               args_preview result_preview))
      (when-let ((value (alist-get field record)))
        (push (magent-audit--detail-line field value) lines)))
    (concat
     (mapconcat #'identity (nreverse lines) "\n")
     "\n")))

(defun magent-audit--detail-line (field value)
  "Return a formatted detail line for FIELD VALUE."
  (format "    %s: %s"
          (replace-regexp-in-string "_" "-" (symbol-name field))
          (magent-audit--format-value value)))

(defun magent-audit--format-value (value)
  "Return a printable value string for VALUE."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((or (listp value) (vectorp value))
    (string-trim-right
     (replace-regexp-in-string
      "\n"
      "\n      "
      (pp-to-string value))))
   (t (format "%s" value))))

(defun magent-audit--display-time (timestamp)
  "Return a compact display string for TIMESTAMP."
  (if (and timestamp (>= (length timestamp) 19))
      (replace-regexp-in-string "T" " " (substring timestamp 0 19))
    (or timestamp "<no time>")))

(defun magent-audit--propertize-badge (text)
  "Return TEXT propertized as an audit badge."
  (let* ((upper (upcase text))
         (face (cond
                ((member text '("allow" "ok" "completed" "started"))
                 'success)
                ((member text '("deny" "error" "timeout" "dropped"))
                 'error)
                ((member text '("pending" "ask"))
                 'warning)
                (t 'font-lock-keyword-face))))
    (propertize (format "[%s]" upper) 'face face)))

(defun magent-audit--record-id (record)
  "Return a stable identifier for RECORD."
  (secure-hash 'sha1 (format "%S"
                             (list (alist-get 'timestamp record)
                                   (alist-get 'event record)
                                   (alist-get 'request_id record)
                                   (alist-get 'call_id record)
                                   (alist-get 'tool_name record)
                                   (alist-get 'summary record)
                                   (alist-get 'detail record)))))

(defun magent-audit--entry-id-at-point ()
  "Return the audit entry id at point, or nil."
  (or (get-text-property (point) 'magent-audit-entry-id)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'magent-audit-entry-id))))

(defun magent-audit--goto-entry (entry-id)
  "Move point to ENTRY-ID if present."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (< (point) (point-max)))
      (if (equal (get-text-property (point) 'magent-audit-entry-id) entry-id)
          (setq found t)
        (goto-char (or (next-single-property-change
                        (point) 'magent-audit-entry-id nil (point-max))
                       (point-max)))))
    found))

(defun magent-audit--header-line ()
  "Return the dynamic header line for the current audit buffer."
  (let* ((filters (magent-audit--active-filter-labels))
         (window (if (and (integerp magent-audit-default-days)
                          (> magent-audit-default-days 0))
                     (format "days=%d" magent-audit-default-days)
                   "days=all"))
         (counts (format "records %d/%d"
                         (length magent-audit--visible-records)
                         (length magent-audit--all-records)))
         (truncated (when (> magent-audit--truncated-count 0)
                      (format " truncated=%d" magent-audit--truncated-count)))
         (errors (when (> magent-audit--load-errors 0)
                   (format " parse-errors=%d" magent-audit--load-errors))))
    (string-join
     (delq nil
           (list "Audit"
                 counts
                 truncated
                 errors
                 window
                 (and filters (concat "filters " filters))
                 "keys g refresh c clear e event t tool d decision s status a agent RET details"))
     " | ")))

(defun magent-audit--active-filter-labels ()
  "Return a compact string describing active audit filters."
  (let ((filters magent-audit--filters)
        labels)
    (while filters
      (let ((key (pop filters))
            (value (pop filters)))
        (when value
          (push (format "%s=%s"
                        (substring (symbol-name key) 1)
                        value)
                labels))))
    (when labels
      (string-join (nreverse labels) " "))))

(provide 'magent-audit)
;;; magent-audit.el ends here
