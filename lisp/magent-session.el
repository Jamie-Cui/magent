;;; magent-session.el --- Session management for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Session management for storing conversation history and state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magent-config)
(require 'magent-json)
(require 'magent-protocol)
(require 'magent-ledger)
(require 'magent-agent-job)

(declare-function magent-agent-info-name "magent-agent-registry")
(declare-function magent-agent-registry-get "magent-agent-registry")

;;; Session state structure

(cl-defstruct (magent-session
               (:constructor magent-session-create)
               (:copier nil))
  (messages nil)             ; List of messages in chronological order
  (max-history magent-max-history)
  (id nil)
  (agent nil)
  (buffer-content nil)       ; Saved buffer text for lossless restore
  (approval-overrides nil)   ; Session-scoped approval memory
  (context-items nil)        ; Structured Codex-like transcript items
  (agent-jobs nil)           ; Durable child-agent job state
  (thread nil)               ; Canonical thread/turn/item ledger
  (metadata nil))            ; Top-level session metadata alist

;;; Message helpers

(defsubst magent-msg-role (msg)
  "Return the role symbol of message MSG."
  (cdr (assq 'role msg)))

(defsubst magent-msg-content (msg)
  "Return the content of message MSG (string or content-block list)."
  (cdr (assq 'content msg)))

(defun magent-session--tool-content-p (content)
  "Return non-nil when CONTENT is a structured tool-call result."
  (and (listp content)
       (plist-member content :name)
       (plist-member content :result)))

(defsubst magent-session--content-to-string (content)
  "Coerce CONTENT to a plain string.
If CONTENT is a string, return it unchanged.
If CONTENT is a list of content blocks, concatenate their text fields."
  (cond
   ((stringp content) content)
   ((magent-session--tool-content-p content)
    (or (plist-get content :result) ""))
   ((listp content)
    (mapconcat (lambda (b) (or (cdr (assq 'text b)) "")) content ""))
   (t "")))

(defun magent-session--assistant-response-error-p (content)
  "Return non-nil when assistant CONTENT is synthetic failure text."
  (string-prefix-p "Error:"
                   (string-trim
                    (magent-session--content-to-string content))))

(defun magent-session--assistant-response-reusable-p (content)
  "Return non-nil when assistant CONTENT should be reused in prompts.
Empty assistant replies and synthetic failure text are preserved in the saved
transcript, but should not be fed back into later requests."
  (let ((text (string-trim (magent-session--content-to-string content))))
    (and (not (string-empty-p text))
         (not (magent-session--assistant-response-error-p content)))))

(defconst magent-session-summary-title-max-width 48
  "Maximum display width for saved session summary titles.")

(defvar magent-session--metadata-cache (make-hash-table :test #'equal)
  "Cached lightweight metadata for saved session files.")

(defvar magent-session--current-scope)

(defun magent-session-metadata-value (session key)
  "Return SESSION metadata value for KEY, or nil."
  (cdr (assq key (and session
                      (magent-session-metadata session)))))

(defun magent-session-set-metadata-value (session key value)
  "Set SESSION metadata KEY to VALUE.
When VALUE is nil, remove KEY.  Return SESSION metadata."
  (when session
    (let ((metadata (assq-delete-all
                     key (copy-sequence
                          (magent-session-metadata session)))))
      (when value
        (push (cons key value) metadata))
      (setf (magent-session-metadata session) metadata)
      metadata)))

(defun magent-session--metadata-string (session key)
  "Return SESSION metadata KEY as a string, or nil."
  (when-let* ((value (magent-session-metadata-value session key)))
    (cond
     ((stringp value) value)
     ((symbolp value) (symbol-name value))
     (t (format "%s" value)))))

(defun magent-session--internal-kind-p (kind)
  "Return non-nil when KIND denotes an internal command session."
  (or (eq kind 'internal-command)
      (equal kind "internal-command")))

(defun magent-session-internal-scope-p (scope)
  "Return non-nil when SCOPE is an internal command scope."
  (and (listp scope)
       (magent-session--internal-kind-p (plist-get scope :kind))))

(defun magent-session-internal-scope
    (session-id command origin-scope)
  "Return an internal command scope for SESSION-ID, COMMAND, and ORIGIN-SCOPE."
  (list :kind 'internal-command
        :id session-id
        :command command
        :origin-scope origin-scope))

(defun magent-session--scope-origin (scope)
  "Return ordinary project/global origin for SCOPE."
  (if (magent-session-internal-scope-p scope)
      (or (plist-get scope :origin-scope) 'global)
    scope))

(defun magent-session-scope-origin (scope)
  "Return the public project/global origin represented by SCOPE."
  (magent-session--scope-origin scope))

(defun magent-session--origin-scope-for-session (session scope)
  "Return ordinary project/global origin for SESSION saved under SCOPE."
  (or (magent-session-metadata-value session 'origin-scope)
      (magent-session--scope-origin scope)
      'global))

(defun magent-session--command-name-for-storage (name)
  "Return safe internal command NAME for storage paths."
  (let ((raw (cond
              ((stringp name) name)
              ((symbolp name) (symbol-name name))
              ((null name) "unknown")
              (t (format "%s" name)))))
    (replace-regexp-in-string
     "[^[:alnum:]_.-]+" "-"
     (string-trim raw))))

(defun magent-session-internal-directory (&optional command)
  "Return internal command session directory, optionally for COMMAND."
  (let ((root (or magent-command-session-directory
                  (expand-file-name "internal" magent-session-directory))))
    (if command
        (expand-file-name
         (magent-session--command-name-for-storage command)
         root)
      root)))

(defun magent-session--clean-summary-title (text)
  "Normalize TEXT into a single-line summary title."
  (when (stringp text)
    (let ((clean (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text))))
      (unless (string-empty-p clean)
        (truncate-string-to-width
         clean
         magent-session-summary-title-max-width
         nil nil "...")))))

(defun magent-session--summary-title-from-messages (messages)
  "Derive a brief summary title from session MESSAGES."
  (catch 'title
    (dolist (msg messages)
      (let ((role (cdr (assq 'role msg)))
            (content (cdr (assq 'content msg))))
        (when (member role '(user assistant "user" "assistant"))
          (when-let* ((title (magent-session--clean-summary-title
                             (magent-session--content-to-string content))))
            (throw 'title title)))))
    nil))

;;; Thread ledger projection

(defun magent-session--scope-for-thread (session)
  "Return the current scope to store in SESSION's thread ledger."
  (or (and session
           (magent-thread-p (magent-session-thread session))
           (magent-thread-scope (magent-session-thread session)))
      (and session
           (magent-session-metadata-value session 'origin-scope))
      (magent-session--scope-origin magent-session--current-scope)
      magent-session--current-scope))

(defun magent-session--ensure-thread (session)
  "Return SESSION's canonical thread ledger, creating it when needed."
  (when session
    (or (magent-session-thread session)
        (let ((thread
               (magent-thread-create
                :id (or (magent-session-id session)
                        (magent-session-get-id session))
                :session-id (or (magent-session-id session)
                                (magent-session-get-id session))
                :scope (magent-session--scope-for-thread session)
                :status 'idle
                :metadata (append (list :source 'magent)
                                  (and (magent-session-metadata session)
                                       (list :session-metadata
                                             (magent-session-metadata
                                              session)))))))
          (setf (magent-session-thread session) thread)
          thread))))

(defun magent-session--message-item-from-legacy (role content turn-id)
  "Return a completed ledger message item from legacy ROLE CONTENT."
  (magent-thread-item-create
   :turn-id turn-id
   :type 'message
   :status 'completed
   :role role
   :content content
   :completed-at (float-time)))

(defun magent-session--tool-item-from-legacy (content turn-id)
  "Return a completed ledger tool item from legacy CONTENT."
  (let* ((safe-name (magent-json-safe-name (plist-get content :name)))
         (safe-args (magent-json-safe-tool-args (plist-get content :args)))
         (safe-result (magent-session--content-to-string content))
         (call-id (or (plist-get content :id)
                      (magent-protocol-generate-id "tool"))))
    (magent-thread-item-create
     :id call-id
     :turn-id turn-id
     :type 'tool
     :status (if (magent-tool-result-success-p safe-result) 'completed 'failed)
     :name safe-name
     :call-id call-id
     :input safe-args
     :output safe-result
     :error (and (not (magent-tool-result-success-p safe-result)) safe-result)
     :completed-at (float-time)
     :metadata (list :legacy t))))

(defun magent-session--thread-from-messages
    (messages id scope &optional metadata)
  "Build a thread ledger from legacy MESSAGES for ID and SCOPE."
  (let ((thread (magent-thread-create
                 :id id
                 :session-id id
                 :scope scope
                 :status 'idle
                 :metadata (append (list :migrated-from 'messages)
                                   metadata)))
        current-turn)
    (dolist (msg messages)
      (let ((role (magent-msg-role msg))
            (content (magent-msg-content msg)))
        (cond
         ((eq role 'user)
          (let ((turn (magent-thread-turn-create
                       :thread-id id
                       :status 'completed
                       :input (magent-session--content-to-string content)
                       :started-at (float-time)
                       :completed-at (float-time)
                       :duration-ms 0
                       :metadata (list :legacy t))))
            (setq current-turn turn)
            (setf (magent-thread-turn-items turn)
                  (list (magent-session--message-item-from-legacy
                         role content
                         (magent-thread-turn-id turn))))
            (setf (magent-thread-turns thread)
                  (nconc (magent-thread-turns thread) (list turn)))
            (unless (magent-thread-preview thread)
              (setf (magent-thread-preview thread)
                    (magent-session--content-to-string content)))))
         ((eq role 'tool)
          (when (and current-turn
                     (magent-session--tool-content-p content))
            (setf (magent-thread-turn-items current-turn)
                  (nconc (magent-thread-turn-items current-turn)
                         (list (magent-session--tool-item-from-legacy
                                content
                                (magent-thread-turn-id current-turn)))))))
         ((eq role 'assistant)
          (unless current-turn
            (setq current-turn
                  (magent-thread-turn-create
                   :thread-id id
                   :status 'completed
                   :started-at (float-time)
                   :completed-at (float-time)
                   :duration-ms 0
                   :metadata (list :legacy t)))
            (setf (magent-thread-turns thread)
                  (nconc (magent-thread-turns thread)
                         (list current-turn))))
          (setf (magent-thread-turn-items current-turn)
                (nconc (magent-thread-turn-items current-turn)
                       (list (magent-session--message-item-from-legacy
                              role content
                              (magent-thread-turn-id current-turn)))))
	          (setf (magent-thread-turn-status current-turn)
	                (if (magent-session--assistant-response-error-p content)
	                    'failed
	                  'completed)
	                (magent-thread-turn-completed-at current-turn) (float-time)
	                (magent-thread-turn-duration-ms current-turn) 0)))))
    thread))

(defun magent-session-refresh-projections (session)
  "Refresh SESSION legacy message/context projections from its thread ledger."
  (when (and session (magent-session-thread session))
    (setf (magent-session-messages session)
          (magent-thread-messages (magent-session-thread session)))
    (setf (magent-session-context-items session)
          (magent-thread-response-items (magent-session-thread session)))))

(defun magent-session-thread-ledger (session)
  "Return SESSION's canonical thread ledger and refresh projections."
  (let ((thread (magent-session--ensure-thread session)))
    (magent-session-refresh-projections session)
    thread))

;;; Session management

(defvar magent--current-session nil
  "The current active session.")

(defvar magent-session--current-scope 'global
  "The currently active session scope.
This is either the symbol `global' or a normalized project root path.")

(defvar magent-session--scoped-sessions (make-hash-table :test #'equal)
  "Hash table of session objects keyed by scope.")

(defvar magent-session--last-id-stem nil
  "Timestamp stem used for the most recently generated session id.")

(defvar magent-session--last-id-seq 0
  "Sequence number used when multiple sessions are created in one second.")

(defvar magent-session--save-timer nil
  "Idle timer used for deferred UI session saves.")

(defconst magent-session-schema-version 5
  "Current schema version written to session JSON files.")

(defun magent-session--persisted-journal (thread)
  "Return the bounded journal tail persisted for THREAD."
  (let ((journal (and thread (magent-thread-journal thread)))
        (limit magent-session-journal-max-events))
    (if (and (integerp limit)
             (>= limit 0)
             (> (length journal) limit))
        (last journal limit)
      journal)))

(defun magent-session--write-json-atomic (filepath data)
  "Atomically encode DATA as JSON and replace FILEPATH."
  (let* ((directory (file-name-directory filepath))
         (prefix (expand-file-name ".magent-session-" directory))
         (tempfile (make-temp-file prefix nil ".json.tmp")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (let ((json-null :null)
                  (json-false :json-false)
                  (coding-system-for-write 'utf-8-unix))
              (insert (json-encode data))
              (write-region (point-min) (point-max) tempfile nil 'silent)))
          (rename-file tempfile filepath t)
          (setq tempfile nil))
      (when (and tempfile (file-exists-p tempfile))
        (delete-file tempfile)))))

(defun magent-session--normalize-project-root (root)
  "Normalize project ROOT for use as a stable scope key."
  (when root
    (file-truename (directory-file-name root))))

(defun magent-session-scope-from-directory (&optional directory)
  "Return the session scope derived from DIRECTORY.
Returns a normalized project root string or the symbol `global'."
  (or (magent-session--normalize-project-root
       (magent-project-root directory t))
      'global))

(defun magent-session-current-scope ()
  "Return the currently active session scope."
  magent-session--current-scope)

(defun magent-session--session-for-scope (scope)
  "Return the session object for SCOPE, creating one if needed."
  (or (gethash scope magent-session--scoped-sessions)
      (let ((session (magent-session-create)))
        (puthash scope session magent-session--scoped-sessions)
        session)))

(defun magent-session-get-if-present (scope)
  "Return the live session object for SCOPE, or nil if none is installed."
  (gethash scope magent-session--scoped-sessions))

(defun magent-session-approval-override (session perm-key)
  "Return SESSION's persisted approval override for PERM-KEY, or nil."
  (cdr (assq perm-key
             (and session
                  (magent-session-approval-overrides session)))))

(defun magent-session-set-approval-override (session perm-key decision)
  "Persist DECISION for PERM-KEY in SESSION and return the override alist."
  (when session
    (let ((overrides (assq-delete-all perm-key
                                      (copy-sequence
                                       (magent-session-approval-overrides session)))))
      (push (cons perm-key decision) overrides)
      (setf (magent-session-approval-overrides session) overrides)
      overrides)))

(defun magent-session-clear-approval-overrides (session)
  "Clear all persisted approval overrides from SESSION."
  (when session
    (setf (magent-session-approval-overrides session) nil)))

(defun magent-session-add-agent-job (session job)
  "Add JOB to SESSION, replacing any existing job with the same id.
Return JOB."
  (when session
    (setf (magent-session-agent-jobs session)
          (cons job
                (cl-remove (magent-agent-job-id job)
                           (magent-session-agent-jobs session)
                           :key #'magent-agent-job-id
                           :test #'equal))))
  job)

(defun magent-session-agent-job (session id)
  "Return SESSION's child-agent job with ID, or nil."
  (and session
       (magent-agent-job-find (magent-session-agent-jobs session) id)))

(defun magent-session-set-agent-job-status
    (session id status &optional result error)
  "Set SESSION child-agent job ID to STATUS.
Optionally record RESULT or ERROR.  Return the updated job, or nil."
  (when-let* ((job (magent-session-agent-job session id)))
    (magent-agent-job-set-status job status result error)))

(defun magent-session-activate (&optional scope)
  "Activate SCOPE and return its session.
SCOPE must be either `global' or a normalized project root string."
  (let ((target-scope (or scope 'global)))
    (setq magent-session--current-scope target-scope
          magent--current-session (magent-session--session-for-scope target-scope))))

(defun magent-session-get ()
  "Get the current session, creating one if needed."
  (or magent--current-session
      (magent-session-activate magent-session--current-scope)))

(defun magent-session-clear (session &optional scope)
  "Clear SESSION in place and remove its persisted transcript.
SCOPE defaults to the active session scope.  SESSION keeps its identity,
selected agent, and history limit so runtime UI handles remain valid."
  (when session
    (let* ((target-scope (or scope magent-session--current-scope))
           (id (magent-session-id session))
           (filepath
            (and id
                 (expand-file-name
                  (concat id ".json")
                  (magent-session--scope-storage-directory target-scope)))))
      (setf (magent-session-messages session) nil
            (magent-session-buffer-content session) nil
            (magent-session-approval-overrides session) nil
            (magent-session-context-items session) nil
            (magent-session-agent-jobs session) nil
            (magent-session-thread session) nil
            (magent-session-metadata session) nil)
      (when (and filepath (file-exists-p filepath))
        (condition-case err
            (progn
              (delete-file filepath)
              (remhash filepath magent-session--metadata-cache))
          (error
           (magent-log "WARN failed deleting cleared session %s: %s"
                       filepath (error-message-string err)))))))
  session)

(defun magent-session-reset ()
  "Reset the current session, clearing all messages and permission overrides."
  (interactive)
  (let ((session magent--current-session))
    (when session
      (magent-session-clear-approval-overrides session)))
  (remhash magent-session--current-scope magent-session--scoped-sessions)
  (setq magent--current-session nil)
  (when (fboundp 'magent-capability-clear-local-overrides)
    (magent-capability-clear-local-overrides))
  (magent-log "INFO session cleared for scope %s" magent-session--current-scope))

(defun magent-session--scope-storage-directory (scope)
  "Return the storage directory for SCOPE."
  (cond
   ((magent-session-internal-scope-p scope)
    (magent-session-internal-directory (plist-get scope :command)))
   ((eq scope 'global)
    magent-session-directory)
   (t
    (expand-file-name
     (concat "projects/" (secure-hash 'sha1 scope))
     magent-session-directory))))

(defun magent-session--infer-file-scope (filepath)
  "Infer the session scope for FILEPATH."
  (let ((path (file-truename filepath))
        (project-prefix (file-name-as-directory
                         (expand-file-name "projects" magent-session-directory))))
    (if (string-prefix-p project-prefix path)
        (file-name-directory (directory-file-name path))
      'global)))

(defun magent-session--file-scope-kind (filepath)
  "Return the symbolic scope kind for FILEPATH."
  (if (eq (magent-session--infer-file-scope filepath) 'global)
      'global
    'project))

(defun magent-session--file-display-time (filepath)
  "Return FILEPATH's logical session time.
Prefer the timestamp embedded in `session-YYYYMMDD-HHMMSS' filenames.
Fall back to the file modification time for legacy filenames."
  (let ((name (file-name-sans-extension (file-name-nondirectory filepath))))
    (if (string-match
         "session-\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)"
         name)
        (encode-time
         (string-to-number (match-string 6 name))
         (string-to-number (match-string 5 name))
         (string-to-number (match-string 4 name))
         (string-to-number (match-string 3 name))
         (string-to-number (match-string 2 name))
         (string-to-number (match-string 1 name)))
      (file-attribute-modification-time
       (file-attributes filepath)))))

(defun magent-session--sort-files-by-time (files)
  "Return FILES sorted by logical session time, newest first."
  (sort files
        (lambda (a b)
          (time-less-p
           (magent-session--file-display-time b)
           (magent-session--file-display-time a)))))

(defun magent-session--list-files-in-directory (directory)
  "Return session JSON files in DIRECTORY, newest first."
  (when (file-directory-p directory)
    (magent-session--sort-files-by-time
     (directory-files directory t "\\.json$"))))

(defun magent-session--project-files ()
  "Return all project-scoped session files under `magent-session-directory'."
  (let ((projects-dir (expand-file-name "projects" magent-session-directory)))
    (when (file-directory-p projects-dir)
      (magent-session--sort-files-by-time
       (directory-files-recursively projects-dir "\\.json$")))))

(defun magent-session-list-internal-files (&optional command)
  "Return internal command session JSON files, optionally limited to COMMAND."
  (let ((directory (magent-session-internal-directory command)))
    (when (file-directory-p directory)
      (magent-session--sort-files-by-time
       (directory-files-recursively directory "\\.json$")))))

(defun magent-session--read-file-metadata (filepath)
  "Read lightweight metadata from session FILEPATH."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents filepath)
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read))
               (kind (cdr (assq 'kind data)))
               (command (cdr (assq 'command data)))
               (status (cdr (assq 'status data)))
               (title (cdr (assq 'title data)))
               (parent-session-id (cdr (assq 'parent-session-id data)))
               (metadata (cdr (assq 'metadata data)))
               (scope-name (cdr (assq 'scope data)))
               (project-root (cdr (assq 'project-root data)))
               (summary-title (or (magent-session--clean-summary-title
                                   title)
                                  (magent-session--clean-summary-title
                                   (cdr (assq 'summary-title data)))
                                  (magent-session--summary-title-from-messages
                                   (cdr (assq 'messages data))))))
          (list :scope (if (equal scope-name "project") 'project 'global)
                :project-root (magent-session--normalize-project-root project-root)
                :summary-title summary-title
                :kind kind
                :command command
                :status status
                :title title
                :parent-session-id parent-session-id
                :metadata metadata)))
    (error
     (list :scope (magent-session--file-scope-kind filepath)
           :project-root nil
           :summary-title nil
           :kind nil
           :command nil
           :status nil
           :title nil
           :parent-session-id nil
           :metadata nil))))

(defun magent-session--metadata-cache-key (filepath)
  "Return a cache key for FILEPATH based on current file attributes."
  (let ((attrs (file-attributes filepath)))
    (when attrs
      (list (file-attribute-size attrs)
            (file-attribute-modification-time attrs)))))

(defun magent-session--read-file-metadata-cached (filepath)
  "Read lightweight metadata from FILEPATH using an attribute-validated cache."
  (let ((key (magent-session--metadata-cache-key filepath)))
    (if key
        (let ((entry (gethash filepath magent-session--metadata-cache)))
          (if (equal (plist-get entry :key) key)
              (plist-get entry :metadata)
            (let ((metadata (magent-session--read-file-metadata filepath)))
              (puthash filepath
                       (list :key key :metadata metadata)
                       magent-session--metadata-cache)
              metadata)))
      (magent-session--read-file-metadata filepath))))

(defun magent-session--project-label (root)
  "Return a human-readable label for ROOT."
  (if root
      (abbreviate-file-name root)
    "Unknown project"))

(defun magent-session--file-group (filepath)
  "Return the completion group label for FILEPATH."
  (let* ((meta (magent-session--read-file-metadata-cached filepath))
         (scope (plist-get meta :scope))
         (project-root (plist-get meta :project-root)))
    (cond
     ((eq scope 'global) "Global")
     ((and (stringp project-root)
           (equal project-root magent-session--current-scope))
      (format "Current Project: %s"
              (magent-session--project-label project-root)))
     (t
      (format "Project: %s"
              (magent-session--project-label project-root))))))

(defun magent-session--file-rank (filepath)
  "Return the sort rank for FILEPATH."
  (let* ((meta (magent-session--read-file-metadata-cached filepath))
         (scope (plist-get meta :scope))
         (project-root (plist-get meta :project-root)))
    (cond
     ((and (eq scope 'project)
           (stringp project-root)
           (equal project-root magent-session--current-scope))
      0)
     ((eq scope 'project) 1)
     (t 2))))

(defun magent-session--all-files ()
  "Return all saved session files."
  (append (magent-session--project-files)
          (magent-session--list-files-in-directory
           (magent-session--scope-storage-directory 'global))))

(defun magent-session--sort-files-for-display (files)
  "Sort FILES by project grouping, current project first."
  (sort (copy-sequence files)
        (lambda (a b)
          (let ((rank-a (magent-session--file-rank a))
                (rank-b (magent-session--file-rank b))
                (group-a (magent-session--file-group a))
                (group-b (magent-session--file-group b)))
            (cond
             ((/= rank-a rank-b) (< rank-a rank-b))
             ((not (string-equal group-a group-b))
              (string-lessp group-a group-b))
             (t
              (time-less-p
               (magent-session--file-display-time b)
               (magent-session--file-display-time a))))))))

(defun magent-session--format-display-timestamp (filepath)
  "Return a display timestamp for session FILEPATH.
Prefer the timestamp embedded in `session-YYYYMMDD-HHMMSS' filenames.
Fall back to the file modification time for legacy filenames."
  (format-time-string "%Y-%m-%d %H:%M:%S"
                      (magent-session--file-display-time filepath)))

(defun magent-session--format-display-time (filepath)
  "Return the time-of-day portion of FILEPATH's display timestamp."
  (let ((timestamp (magent-session--format-display-timestamp filepath)))
    (if (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\([0-9:]+\\)\\'" timestamp)
        (match-string 1 timestamp)
      timestamp)))

;;; Session persistence

(defun magent-session--msg-to-alist (msg)
  "Convert MSG to a JSON-serializable alist with string role."
  (let ((role (magent-msg-role msg))
        (content (magent-msg-content msg)))
    `((role . ,(symbol-name role))
      (content . ,(if (and (eq role 'tool)
                           (magent-session--tool-content-p content))
                      `((id . ,(plist-get content :id))
                        (name . ,(magent-json-safe-name
                                  (plist-get content :name)))
                        (args-json . ,(magent-json-encode
                                       (magent-json-safe-tool-args
                                        (plist-get content :args))))
                        (result . ,(plist-get content :result)))
                    (magent-session--content-to-string content))))))

(defun magent-session--context-item-to-alist (item)
  "Convert structured context ITEM to a JSON-serializable alist."
  (magent-protocol-response-item-to-alist item))

(defun magent-session--alist-to-context-item (alist)
  "Reconstruct a structured context item from JSON-decoded ALIST."
  (magent-protocol-response-item-from-alist alist))

(defun magent-session--alist-to-msg (alist)
  "Reconstruct a session message from JSON-decoded ALIST."
  (let ((role (intern (cdr (assq 'role alist))))
        (content (cdr (assq 'content alist))))
    `((role . ,role)
      (content . ,(if (and (eq role 'tool)
                           (listp content))
                      (let* ((args-json (cdr (assq 'args-json content)))
                             (args (when (and (stringp args-json)
                                              (> (length args-json) 0))
                                     (let ((json-object-type 'plist)
                                           (json-array-type 'list))
                                       (ignore-errors
                                         (json-read-from-string args-json))))))
                        (list :id (cdr (assq 'id content))
                              :name (magent-json-safe-name
                                     (cdr (assq 'name content)))
                              :args args
                              :result (cdr (assq 'result content))))
                    content)))))

(defun magent-session-save ()
  "Save the current session to disk as <session-id>.json.
Called automatically after each successful LLM response.
The caller is responsible for updating `magent-session-buffer-content'
before calling this function."
  (let* ((scope magent-session--current-scope)
         (session magent--current-session))
    (when session
      (magent-session-thread-ledger session)
      (when (or (magent-session-messages session)
                (magent-session-agent-jobs session))
      (let ((storage-dir (magent-session--scope-storage-directory scope)))
        (make-directory storage-dir t)
        (let* ((messages (magent-session-messages session))
               (thread (magent-session-thread session))
               (id (magent-session-get-id session))
               (filepath (expand-file-name (concat id ".json") storage-dir))
               (origin-scope (magent-session--origin-scope-for-session
                              session scope))
               (kind (magent-session--metadata-string session 'kind))
               (command (magent-session--metadata-string session 'command))
               (status (magent-session--metadata-string session 'status))
               (title (magent-session--metadata-string session 'title))
               (parent-session-id
                (magent-session--metadata-string session 'parent-session-id))
               (summary-title (or (magent-session--clean-summary-title title)
                                  (magent-session--summary-title-from-messages
                                   messages)))
               (approval-overrides
                (mapcar (lambda (entry)
                          `((tool . ,(symbol-name (car entry)))
                            (decision . ,(symbol-name (cdr entry)))))
                        (magent-session-approval-overrides session)))
               (data `((id . ,id)
                       (schema-version . ,magent-session-schema-version)
                       ,@(when kind
                           `((kind . ,kind)))
                       ,@(when command
                           `((command . ,command)))
                       ,@(when status
                           `((status . ,status)))
                       ,@(when title
                           `((title . ,title)))
                       ,@(when parent-session-id
                           `((parent-session-id . ,parent-session-id)))
                       ,@(when (magent-session-metadata session)
                           `((metadata . ,(magent-json-safe-value
                                           (magent-session-metadata session)))))
                       (scope . ,(if (eq origin-scope 'global)
                                     "global"
                                   "project"))
                       ,@(unless (eq origin-scope 'global)
                           `((project-root . ,origin-scope)))
                       ,@(when summary-title
                           `((summary-title . ,summary-title)))
                       (messages . ,(vconcat (mapcar #'magent-session--msg-to-alist messages)))
                       (context-items . ,(vconcat
                                          (mapcar
                                           #'magent-session--context-item-to-alist
                                           (magent-session-context-items session))))
                       (snapshot . ,(and thread
                                         (magent-thread-snapshot-to-alist thread)))
                       (journal . ,(vconcat
                                    (mapcar #'magent-thread-event-to-alist
                                            (magent-session--persisted-journal
                                             thread))))
                       (agent-jobs . ,(vconcat
                                       (mapcar
                                        #'magent-agent-job-to-alist
                                        (magent-session-agent-jobs session))))
                       (approval-overrides . ,(vconcat approval-overrides))
                       (buffer-content . ,(or (magent-session-buffer-content session) "")))))
          (magent-session--write-json-atomic filepath data)
          (remhash filepath magent-session--metadata-cache)
          (magent-log "INFO session saved to %s (%d messages) scope=%s"
                      id (length messages) scope)))))))

(defun magent-session-save-deferred (&optional delay)
  "Schedule a session save to run after Emacs is idle.
DELAY defaults to `magent-session-save-idle-delay'.  The active session
and scope at scheduling time are saved even if the user switches scopes
before the timer fires."
  (let ((session magent--current-session)
        (scope magent-session--current-scope)
        timer)
    (setq timer
          (run-with-idle-timer
           (or delay magent-session-save-idle-delay) nil
           (lambda ()
             (when (eq magent-session--save-timer timer)
               (setq magent-session--save-timer nil))
             (let ((previous-session magent--current-session)
                   (previous-scope magent-session--current-scope))
               (unwind-protect
                   (progn
                     (setq magent--current-session session
                           magent-session--current-scope scope)
                     (magent-session-save))
                 (setq magent--current-session previous-session
                       magent-session--current-scope previous-scope))))))
    (setq magent-session--save-timer timer)
    magent-session--save-timer))

(defun magent-session-save-deferred-for-session (session &optional scope delay)
  "Schedule SESSION to be saved for SCOPE after Emacs is idle.
SCOPE defaults to SESSION's ledger scope, falling back to the active scope.
The active session and scope are restored before returning."
  (let ((previous-session magent--current-session)
        (previous-scope magent-session--current-scope)
        (target-scope (or scope
                          (magent-session--scope-for-thread session)
                          magent-session--current-scope)))
    (unwind-protect
        (progn
          (setq magent--current-session session
                magent-session--current-scope target-scope)
          (if delay
              (magent-session-save-deferred delay)
            (magent-session-save-deferred)))
      (setq magent--current-session previous-session
            magent-session--current-scope previous-scope))))

(defun magent-session-read-file (filepath)
  "Read session data from FILEPATH without changing active session state.
Return a plist with keys `:scope', `:session', and `:id', or nil on error."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents filepath)
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read))
               (id (cdr (assq 'id data)))
               (kind (cdr (assq 'kind data)))
               (command (cdr (assq 'command data)))
               (status (cdr (assq 'status data)))
               (title (cdr (assq 'title data)))
               (parent-session-id (cdr (assq 'parent-session-id data)))
               (metadata-raw (cdr (assq 'metadata data)))
               (scope-name (cdr (assq 'scope data)))
               (project-root (cdr (assq 'project-root data)))
               (msgs-raw (cdr (assq 'messages data)))
               (context-raw (cdr (assq 'context-items data)))
               (snapshot-raw (cdr (assq 'snapshot data)))
               (journal-raw (cdr (assq 'journal data)))
               (jobs-raw (cdr (assq 'agent-jobs data)))
               (approval-raw (cdr (assq 'approval-overrides data)))
               (bc (or (cdr (assq 'buffer-content data)) ""))
               (scope (pcase scope-name
                        ("project"
                         (or (magent-session--normalize-project-root project-root)
                             (magent-session--infer-file-scope filepath)))
                        ("global" 'global)
                        (_ (magent-session--infer-file-scope filepath))))
               (messages (mapcar #'magent-session--alist-to-msg msgs-raw))
               (context-items (mapcar #'magent-session--alist-to-context-item
                                      context-raw))
               (thread
                (if snapshot-raw
                    (magent-thread-replay
                     snapshot-raw
                     (mapcar #'magent-thread-event-from-alist journal-raw))
                  (magent-session--thread-from-messages
                   messages id scope (list :loaded-from filepath))))
               (agent-jobs (mapcar #'magent-agent-job-from-alist jobs-raw))
               (approval-overrides
                (mapcar
                 (lambda (entry)
                   (cons (intern (cdr (assq 'tool entry)))
                         (intern (cdr (assq 'decision entry)))))
                 approval-raw))
               (metadata (append metadata-raw
                                 (delq nil
                                       `((kind . ,kind)
                                         (command . ,command)
                                         (status . ,status)
                                         (title . ,title)
                                         (parent-session-id
                                          . ,parent-session-id)
                                         (origin-scope . ,scope)))))
               (session (magent-session-create
                         :id id
                         :metadata metadata
                         :messages messages
                         :context-items context-items
                         :agent-jobs agent-jobs
                         :approval-overrides approval-overrides
                         :thread thread
                         :buffer-content (when (> (length bc) 0) bc))))
          (unless context-raw
            (magent-session-refresh-projections session))
          (list :scope scope
                :session session
                :id id)))
    (error
     (magent-log "ERROR loading session %s: %s" filepath (error-message-string err))
     nil)))

(defun magent-session-install (scope session)
  "Install SESSION for SCOPE and make it active."
  (puthash scope session magent-session--scoped-sessions)
  (magent-session-activate scope)
  session)

(defun magent-session-refresh-agent (session)
  "Refresh SESSION's agent pointer from the current registry.
When the session references a custom agent that is no longer active for the
current scope, clear it so Magent falls back to the default agent."
  (when-let* ((agent (magent-session-agent session)))
    (when (fboundp 'magent-agent-registry-get)
      (setf (magent-session-agent session)
            (magent-agent-registry-get (magent-agent-info-name agent)))))
  session)

(defun magent-session-load (filepath)
  "Load the session from FILEPATH.
Restores `magent--current-session'.  Returns the session or nil."
  (when-let* ((loaded (magent-session-read-file filepath))
              (scope (plist-get loaded :scope))
              (session (plist-get loaded :session)))
    (magent-session-install scope session)
    (magent-log "INFO session loaded from %s (%d messages) scope=%s"
                (plist-get loaded :id)
                (length (magent-session-messages session))
                scope)
    session))

(defun magent-session-list-files ()
  "Return all session JSON files grouped by project for resume display."
  (magent-session--sort-files-for-display
   (delq nil (magent-session--all-files))))

(defun magent-session--format-file (filepath)
  "Return a human-readable label for session FILEPATH.
Parses the session-YYYYMMDD-HHMMSS filename pattern into a date/time string."
  (let* ((meta (magent-session--read-file-metadata-cached filepath))
         (scope (plist-get meta :scope))
         (project-root (plist-get meta :project-root))
         (summary-title (plist-get meta :summary-title))
         (timestamp (magent-session--format-display-timestamp filepath)))
    (concat
     (if (eq scope 'global)
         (format "%s  (global)" timestamp)
       (format "%s  (%s)" timestamp
               (magent-session--project-label project-root)))
     (when summary-title
       (format "  %s" summary-title)))))

(defun magent-session-get-id (session)
  "Get or generate a unique ID for SESSION."
  (or (magent-session-id session)
      (let* ((stem (format-time-string "%Y%m%d-%H%M%S"))
             (seq (if (equal stem magent-session--last-id-stem)
                      (cl-incf magent-session--last-id-seq)
                    (setq magent-session--last-id-stem stem
                          magent-session--last-id-seq 0)))
             (id (if (zerop seq)
                     (format "session-%s" stem)
                   (format "session-%s-%02d" stem seq))))
        (setf (magent-session-id session) id)
        id)))

(defun magent-session-set-agent (session agent)
  "Set the agent for SESSION to AGENT."
  (setf (magent-session-agent session) agent))

;;; Message management

(defun magent-session-add-message (session role content)
  "Add a message to SESSION.
ROLE is either \\='user, \\='assistant, or \\='tool.
CONTENT can be a string or a list of content blocks."
  (let* ((thread (magent-session--ensure-thread session))
         (turn (magent-thread-active-turn thread)))
    (pcase role
      ('user
       (unless turn
         (setq turn
               (magent-thread-create-turn
                thread
                (magent-session--content-to-string content))))
       (unless (magent-thread-turn-input turn)
         (setf (magent-thread-turn-input turn)
               (magent-session--content-to-string content)))
       (magent-thread-record-message
        thread (magent-thread-turn-id turn) 'user content))
      ('assistant
       (unless turn
         (setq turn
               (magent-thread-create-turn
                thread nil nil (list :synthetic t))))
       (magent-thread-record-message
        thread (magent-thread-turn-id turn) 'assistant content)
       (if (magent-session--assistant-response-error-p content)
           (magent-thread-fail-turn
            thread (magent-thread-turn-id turn)
            (magent-session--content-to-string content))
         (magent-thread-complete-turn
          thread (magent-thread-turn-id turn))))
      ('tool
       (unless turn
         (setq turn
               (or (car (last (magent-thread-turns thread)))
                   (magent-thread-create-turn
                    thread nil nil (list :synthetic t)))))
       (when (magent-session--tool-content-p content)
         (magent-thread-record-tool-result
          thread
          (magent-thread-turn-id turn)
          (or (plist-get content :id)
              (magent-protocol-generate-id "tool"))
          (plist-get content :name)
          (plist-get content :args)
          (plist-get content :result)
          (list :legacy-message t)))))
    (magent-session-refresh-projections session)
    (when (> (length (magent-session-messages session))
             (+ (magent-session-max-history session) 10))
      (magent-session--trim-history session)))
  session)

(defun magent-session--trim-history (session)
  "Trim SESSION messages to max-history limit."
  (let* ((messages (magent-session-messages session))
         (count (length messages))
         (max (magent-session-max-history session))
         (to-remove (- count max)))
    (when (> to-remove 0)
      (when-let* ((thread (magent-session-thread session)))
        (setf (magent-thread-turns thread)
              (magent-session--trim-thread-turns
               (magent-thread-turns thread)
               max))
        (magent-session-refresh-projections session))
      (magent-log "INFO Trimmed session history: removed %d old messages" to-remove))))

(defun magent-session--trim-thread-turns (turns max-messages)
  "Trim TURNS so the last MAX-MESSAGES message items remain.
Non-message items are retained only when they occur after the retained
message boundary."
  (let* ((flat (cl-loop for turn in turns append
                        (mapcar (lambda (item) (cons turn item))
                                (magent-thread-turn-items turn))))
         (flat-length (length flat))
         (message-count 0)
         boundary)
    (cl-loop for pair in (reverse flat)
             for reverse-index from 0
             for index = (- flat-length reverse-index 1)
             for item = (cdr pair)
             when (eq (magent-thread-item-type item) 'message)
             do (progn
                  (cl-incf message-count)
                  (when (<= message-count max-messages)
                    (setq boundary index))))
    (if (or (null boundary)
            (zerop boundary))
        turns
      (let ((index -1)
            trimmed)
        (dolist (turn turns (nreverse trimmed))
          (let (kept)
            (dolist (item (magent-thread-turn-items turn))
              (cl-incf index)
              (when (>= index boundary)
                (push item kept)))
            (when kept
              (setf (magent-thread-turn-items turn) (nreverse kept))
              (push turn trimmed))))))))

(defun magent-session--trim-context-items-for-messages (items max-messages)
  "Trim structured ITEMS to roughly MAX-MESSAGES message items.
Non-message items are retained only after the retained message boundary."
  (let ((message-count 0)
        start)
    (cl-loop for item in (reverse items)
             for index from (1- (length items)) downto 0
             do (when (eq (magent-response-item-type item) 'message)
                  (cl-incf message-count)
                  (when (<= message-count max-messages)
                    (setq start index))))
    (if start
        (nthcdr start items)
      items)))

(defun magent-session-add-tool-message (session id name args result)
  "Add a structured tool result message to SESSION.
ID is the provider tool-call id, NAME is the tool name, ARGS is the
tool argument plist, and RESULT is the model-visible tool result."
  (magent-session-add-message
   session 'tool
   (list :id id
         :name (magent-json-safe-name name)
         :args (magent-json-safe-tool-args args)
         :result (if (stringp result) result (format "%s" result)))))

(defun magent-session-get-messages (session)
  "Get all messages from SESSION in chronological order."
  (magent-session-refresh-projections session)
  (magent-session-messages session))

;;; Session display

(defun magent-session-summarize (session)
  "Create a summary of SESSION messages.
Returns a condensed version of the conversation."
  (let ((messages (magent-session-get-messages session)))
    (when messages
      (with-temp-buffer
        (insert "Session Summary:\n\n")
        (dolist (msg (last messages 20))
          (let ((role (magent-msg-role msg))
                (content (magent-msg-content msg)))
            (insert (format "[%s] " (upcase (symbol-name role))))
            (insert (truncate-string-to-width
                     (magent-session--content-to-string content) 80 nil nil "..."))
            (insert "\n\n")))
        (buffer-string)))))

;;; gptel prompt list conversion

(defun magent-session--turn-message-content (turn role)
  "Return TURN's last message content for ROLE, or nil."
  (catch 'content
    (dolist (item (reverse (magent-thread-turn-items turn)))
      (when (and (eq (magent-thread-item-type item) 'message)
                 (eq (magent-thread-item-role item) role)
                 (magent-thread-terminal-item-p item))
        (throw 'content (magent-thread-item-content item))))
    nil))

(defun magent-session--turn-user-content (turn)
  "Return TURN's prompt-visible user content."
  (or (magent-session--turn-message-content turn 'user)
      (magent-thread-turn-input turn)))

(defun magent-session--tool-prompt-entry (item)
  "Return a gptel prompt-list tool plist for ledger ITEM."
  (let ((output (magent-thread-item-output item)))
    (list :id (or (magent-thread-item-call-id item)
                  (magent-thread-item-id item))
          :name (magent-json-safe-name
                 (magent-thread-item-name item))
          :args (magent-json-safe-tool-args
                 (magent-thread--tool-input-plist
                  (magent-thread-item-input item)))
          :result (if (stringp output)
                      output
                    (format "%s" output)))))

(defun magent-session--turn-tool-prompt-entries (turn)
  "Return prompt-visible tool entries for TURN."
  (let (tools)
    (dolist (item (magent-thread-turn-items turn) (nreverse tools))
      (when (and (eq (magent-thread-item-type item) 'tool)
                 (magent-thread-terminal-item-p item))
        (push (magent-session--tool-prompt-entry item) tools)))))

(defun magent-session--turn-include-p (turn current-turn-id)
  "Return non-nil when TURN should be included in prompt generation."
  (let ((status (magent-thread-turn-status turn)))
    (or (eq status 'completed)
        (and current-turn-id
             (equal (magent-thread-turn-id turn) current-turn-id)
             (memq status '(queued in-progress))))))

(defun magent-session--compaction-turn-p (turn)
  "Return non-nil when TURN is a reusable completed compaction boundary."
  (let ((metadata (magent-thread-turn-metadata turn))
        (assistant-content
         (magent-session--turn-message-content turn 'assistant)))
    (and (eq (magent-thread-turn-status turn) 'completed)
         (if (magent-json--plist-p metadata)
             (plist-get metadata :compaction)
           (cdr (assq 'compaction metadata)))
         (magent-session--assistant-response-reusable-p assistant-content))))

(defun magent-session--turns-from-last-compaction (turns)
  "Return the tail of TURNS beginning with its last compaction boundary."
  (let ((cursor turns)
        (result turns))
    (while cursor
      (when (magent-session--compaction-turn-p (car cursor))
        (setq result cursor))
      (setq cursor (cdr cursor)))
    result))

(defun magent-session-to-gptel-prompt-list (session &optional current-turn-id)
  "Convert SESSION messages to a gptel-request prompt list.
Returns a list in gptel's advanced format:
  ((prompt . \"user msg\") (response . \"assistant msg\") ...)
Structured tool result messages are emitted as `(tool . PLIST)' entries so
gptel can serialize historical tool calls/results for the active backend.

Only completed turns are reused.  When an assistant reply is empty or a
synthetic error string, Magent drops both that reply and its paired user
prompt from future prompt reuse.  The final pending user prompt is still
included so the current turn is preserved.

When CURRENT-TURN-ID is non-nil, prompt generation stops after that turn.
This prevents later queued user submissions from leaking into the active
sampling request."
  (let* ((thread (magent-session-thread-ledger session))
         (turns (magent-session--turns-from-last-compaction
                 (and thread (magent-thread-turns thread))))
         (effective-current-turn-id
          (or current-turn-id
              (and (cl-find-if
                    (lambda (turn)
                      (memq (magent-thread-turn-status turn)
                            '(queued in-progress)))
                    (reverse turns))
                   (magent-thread-turn-id
                    (cl-find-if
                     (lambda (turn)
                       (memq (magent-thread-turn-status turn)
                             '(queued in-progress)))
                     (reverse turns))))))
         prompt-list
         stop)
    (dolist (turn turns)
      (unless stop
        (when (magent-session--turn-include-p
               turn effective-current-turn-id)
          (let* ((user-content (magent-session--turn-user-content turn))
                 (user-text (magent-session--content-to-string user-content))
                 (assistant-content
                  (magent-session--turn-message-content turn 'assistant))
                 (completed (eq (magent-thread-turn-status turn)
                                'completed)))
            (when (and user-text (not (string-empty-p user-text)))
              (cond
               ((and completed
                     (magent-session--assistant-response-reusable-p
                      assistant-content))
                (push (cons 'prompt user-text) prompt-list)
                (dolist (tool (magent-session--turn-tool-prompt-entries turn))
                  (push (cons 'tool tool) prompt-list))
                (push (cons 'response
                            (magent-session--content-to-string
                             assistant-content))
                      prompt-list))
               ((and completed assistant-content)
                (magent-log
                 "INFO dropping non-reusable session turn from prompt reuse"))
               ((or effective-current-turn-id
                    (not completed))
                (push (cons 'prompt user-text) prompt-list)
                (dolist (tool (magent-session--turn-tool-prompt-entries turn))
                  (push (cons 'tool tool) prompt-list)))))))
        (when (and effective-current-turn-id
                   (equal (magent-thread-turn-id turn)
                          effective-current-turn-id))
          (setq stop t))))
    (nreverse prompt-list)))

(provide 'magent-session)
;;; magent-session.el ends here
