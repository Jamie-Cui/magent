;;; magent-session.el --- Session management for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Session management for storing conversation history and state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'subr-x)
(require 'magent-agent-info)
(require 'magent-config)
(require 'magent-json)
(require 'magent-protocol)
(require 'magent-ledger)
(require 'magent-agent-job)

(declare-function magent-agent-registry-get "magent-agent-registry")

;;; Session state structure

(cl-defstruct (magent-session
  (:constructor magent-session-create)
               (:copier nil))
  (max-history magent-max-history)
  (id nil)
  (agent nil)
  (approval-overrides nil)   ; Session-scoped approval memory
  (agent-jobs nil)           ; Durable child-agent job state
  (thread nil)               ; Canonical thread/turn/item ledger
  (metadata nil))            ; Top-level session metadata alist

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

(defvar magent-session--loaded-sessions
  (make-hash-table :test #'eq :weakness 'key)
  "Runtime-only map of sessions awaiting restart reconciliation to source files.")

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

(defun magent-session--action-kind-p (kind)
  "Return non-nil when KIND denotes an isolated action session."
  (or (eq kind 'action)
      (equal kind "action")))

(defun magent-session-action-scope-p (scope)
  "Return non-nil when SCOPE is an isolated action scope."
  (and (listp scope)
       (magent-session--action-kind-p (plist-get scope :kind))))

(defun magent-session-action-scope
    (session-id action origin-scope)
  "Return an isolated action scope for SESSION-ID, ACTION, and ORIGIN-SCOPE."
  (list :kind 'action
        :id session-id
        :action action
        :origin-scope origin-scope))

(defun magent-session--scope-origin (scope)
  "Return ordinary project/global origin for SCOPE."
  (if (magent-session-action-scope-p scope)
      (or (plist-get scope :origin-scope) 'global)
    scope))

(defun magent-session-scope-origin (scope)
  "Return the public project/global origin represented by SCOPE."
  (magent-session--scope-origin scope))

(defun magent-session-canonical-scope (scope)
  "Return canonical project origin for SCOPE, or nil for global scope."
  (let ((origin (magent-session-scope-origin scope)))
    (cond
     ((or (null origin) (eq origin 'global)) nil)
     ((stringp origin)
      (condition-case nil
          (file-truename (directory-file-name origin))
        (error (directory-file-name (expand-file-name origin)))))
     (t origin))))

(defun magent-session--origin-scope-for-session (session scope)
  "Return ordinary project/global origin for SESSION saved under SCOPE."
  (or (magent-session-metadata-value session 'origin-scope)
      (magent-session--scope-origin scope)
      'global))

(defun magent-session--action-name-for-storage (name)
  "Return safe action NAME for storage paths."
  (let ((raw (cond
              ((stringp name) name)
              ((symbolp name) (symbol-name name))
              ((null name) "unknown")
              (t (format "%s" name)))))
    (replace-regexp-in-string
     "[^[:alnum:]_.-]+" "-"
     (string-trim raw))))

(defun magent-session-action-directory (&optional action)
  "Return isolated action session directory, optionally for ACTION."
  (let ((root (or magent-action-session-directory
                  (expand-file-name "actions" magent-session-directory))))
    (if action
        (expand-file-name
         (magent-session--action-name-for-storage action)
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

(defun magent-session--summary-title-from-thread (thread)
  "Derive a brief summary title from THREAD."
  (catch 'title
    (dolist (item (and thread (magent-thread-all-items thread)))
      (let ((role (magent-thread-item-role item))
            (content (magent-thread-item-content item)))
        (when (and (eq (magent-thread-item-type item) 'message)
                   (memq role '(user assistant)))
          (when-let* ((title (magent-session--clean-summary-title
                             (magent-session--content-to-string content))))
            (throw 'title title)))))
    nil))

(defun magent-session-summary-title (session)
  "Return SESSION's canonical display title, or nil when it has none."
  (unless (magent-session-p session)
    (error "Expected a Magent session, got: %S" session))
  (or (magent-session--clean-summary-title
       (magent-session--metadata-string session 'title))
      (magent-session--summary-title-from-thread
       (magent-session-thread-ledger session))))

;;; Thread ledger projection

(defun magent-session--scope-for-thread (session)
  "Return the current scope to store in SESSION's thread ledger."
  (or (and session
           (magent-thread-p (magent-session-thread session))
           (magent-thread-scope (magent-session-thread session)))
      (and (magent-session-action-scope-p magent-session--current-scope)
           magent-session--current-scope)
      (and session
           (magent-session-metadata-value session 'origin-scope))
      (magent-session--scope-origin magent-session--current-scope)
      magent-session--current-scope))

(defun magent-session--ensure-thread (session)
  "Return SESSION's canonical thread ledger, creating it when needed."
  (when session
    (or (magent-session-thread session)
        (let* ((id (or (magent-session-id session)
                       (magent-session-get-id session)))
               (scope (magent-session--scope-for-thread session))
               (thread
                (magent-thread-create
                 :id id
                 :session-id id
                 :scope scope
                 :status 'idle
                 :metadata (append (list :source 'magent)
                                   (and (magent-session-metadata session)
                                        (list :session-metadata
                                              (magent-session-metadata
                                               session)))))))
          (setf (magent-session-thread session) thread)
          thread))))

(defun magent-session-thread-ledger (session)
  "Return SESSION's canonical thread ledger."
  (magent-session--ensure-thread session))

(defun magent-session--forkable-p (session)
  "Return non-nil when SESSION has a stable ledger snapshot to fork."
  (let ((thread (magent-session-thread session)))
    (or (null thread)
        (and (not (eq (magent-thread-status thread) 'active))
             (cl-every #'magent-thread-terminal-turn-p
                       (magent-thread-turns thread))
             (cl-every #'magent-thread-terminal-item-p
                       (magent-thread-all-items thread))))))

(defun magent-session--deep-copy-data (value)
  "Return a recursive copy of JSON-like VALUE, including strings."
  (cond
   ((stringp value) (copy-sequence value))
   ((vectorp value)
    (vconcat (mapcar #'magent-session--deep-copy-data
                     (append value nil))))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash
       (lambda (key item)
         (puthash (magent-session--deep-copy-data key)
                  (magent-session--deep-copy-data item)
                  copy))
       value)
      copy))
   ((consp value)
    (cons (magent-session--deep-copy-data (car value))
          (magent-session--deep-copy-data (cdr value))))
   (t value)))

(defun magent-session--fork-thread
    (source-thread source-session-id session-id scope session-metadata)
  "Deep-copy SOURCE-THREAD for SESSION-ID in SCOPE.
SOURCE-SESSION-ID records the branch parent.  SESSION-METADATA is embedded in
the new thread metadata.  Historical turn, item, and call ids remain stable,
while the mutable thread identity and journal start a new branch."
  (let* ((now (float-time))
         (thread
          (if source-thread
              (magent-thread-snapshot-from-alist
               (magent-session--deep-copy-data
                (magent-thread-snapshot-to-alist source-thread)))
            (magent-thread-create
             :id session-id :session-id session-id :scope scope))))
    (setf (magent-thread-id thread) session-id
          (magent-thread-session-id thread) session-id
          (magent-thread-scope thread) scope
          (magent-thread-status thread) 'idle
          (magent-thread-created-at thread) now
          (magent-thread-updated-at thread) now
          (magent-thread-metadata thread)
          (list :source 'magent
                :forked-from-session-id source-session-id
                :session-metadata session-metadata)
          (magent-thread-journal thread) nil
          (magent-thread-snapshot-created-at thread) now
          (magent-thread-last-event-seq thread) 0)
    (dolist (turn (magent-thread-turns thread))
      (setf (magent-thread-turn-thread-id turn) session-id))
    thread))

(defun magent-session-fork (source scope)
  "Return an independent fork of SOURCE in exact SCOPE.
The fork retains conversation history, the selected agent, and the history
limit.  Session-scoped approvals, child jobs, and unrelated metadata are not
inherited.  SOURCE is never modified."
  (unless (magent-session-p source)
    (error "Expected a Magent session, got: %S" source))
  (unless scope
    (error "An explicit session scope is required"))
  (unless (magent-session--forkable-p source)
    (user-error "Magent: cannot fork a session with non-terminal work"))
  (let* ((source-id
          (or (magent-session-id source)
              (error "Magent: cannot fork a session without an id")))
         (source-title (magent-session-metadata-value source 'title))
         (fork (magent-session-create
                :max-history (magent-session-max-history source)
                :agent (magent-session-agent source)))
         (fork-id (magent-session-get-id fork))
         (metadata
          (append
           `((parent-session-id . ,source-id)
             (forked-at . ,(float-time)))
           (and source-title `((title . ,source-title))))))
    (setf (magent-session-metadata fork) metadata
          (magent-session-thread fork)
          (magent-session--fork-thread
           (magent-session-thread source) source-id fork-id scope metadata))
    fork))

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
  "Shared idle timer used to flush deferred session saves.")

(defvar magent-session--pending-saves nil
  "Deferred saves as (SESSION . SCOPE) pairs awaiting the shared idle timer.")

(defconst magent-session-schema-version 6
  "Current schema version written to session JSON files.")

(defconst magent-session--json-fields
  '(id schema-version kind action status title parent-session-id metadata
    scope project-root summary-title snapshot journal agent-jobs
    approval-overrides)
  "Fields accepted by the current session JSON schema.")

(defconst magent-session--required-json-fields
  '(id schema-version scope snapshot journal agent-jobs approval-overrides)
  "Fields required by the current session JSON schema.")

(define-error 'magent-session-schema-error
  "Unsupported or invalid Magent session schema")

(defconst magent-session-id-max-length 200
  "Maximum accepted length of a persisted Magent session id.")

(defun magent-session-valid-id-p (id)
  "Return non-nil when ID is safe as a single session filename stem."
  (and (stringp id)
       (> (length id) 0)
       (<= (length id) magent-session-id-max-length)
       (string-match-p
        "\\`[[:alnum:]][[:alnum:]_.-]*\\'" id)
       (not (member id '("." "..")))))

(defun magent-session-validate-id (id)
  "Return safe session ID or signal `magent-session-schema-error'."
  (unless (magent-session-valid-id-p id)
    (signal 'magent-session-schema-error
            (list (format "Invalid Magent session id: %S" id))))
  id)

(defun magent-session--file-id (filepath)
  "Return validated session id encoded by FILEPATH's filename."
  (magent-session-validate-id
   (file-name-sans-extension (file-name-nondirectory filepath))))

(defun magent-session--validate-schema-version (value)
  "Return VALUE when it is the current session schema version."
  (unless (equal value magent-session-schema-version)
    (signal 'magent-session-schema-error
            (list (format "Unsupported session schema version: %S (expected %d)"
                          value magent-session-schema-version))))
  value)

(defun magent-session--validate-json-fields (data)
  "Reject unknown or missing fields in session JSON DATA."
  (unless (and (listp data) (cl-every #'consp data))
    (signal 'magent-session-schema-error
            (list "Session JSON root must be an object")))
  (let ((keys (mapcar #'car data)))
    (when (/= (length keys) (length (delete-dups (copy-sequence keys))))
      (signal 'magent-session-schema-error
              (list "Session JSON contains duplicate fields"))))
  (dolist (entry data)
    (unless (memq (car entry) magent-session--json-fields)
      (signal 'magent-session-schema-error
              (list (format "Unsupported session field: %S" (car entry))))))
  (dolist (field magent-session--required-json-fields)
    (unless (assq field data)
      (signal 'magent-session-schema-error
              (list (format "Session is missing required field: %s" field)))))
  data)

(defun magent-session--approval-override-from-alist (entry)
  "Return one current-format approval override from ENTRY."
  (unless (and (listp entry)
               (= (length entry) 2)
               (assq 'tool entry)
               (assq 'decision entry)
               (cl-every (lambda (field)
                           (memq (car field) '(tool decision)))
                         entry))
    (signal 'magent-session-schema-error
            (list (format "Invalid approval override fields: %S" entry))))
  (let ((tool (cdr (assq 'tool entry)))
        (decision (cdr (assq 'decision entry))))
    (unless (and (stringp tool) (not (string-empty-p tool)))
      (signal 'magent-session-schema-error
              (list (format "Invalid approval override tool: %S" tool))))
    (unless (member decision '("allow" "deny"))
      (signal 'magent-session-schema-error
              (list (format "Invalid approval override decision: %S"
                            decision))))
    (cons (intern tool) (intern decision))))

(defun magent-session--decode-json-state (data)
  "Validate and decode nested current-format persistence objects in DATA."
  (let ((snapshot (cdr (assq 'snapshot data))))
    (unless snapshot
      (signal 'magent-session-schema-error
              (list "Session is missing its ledger snapshot")))
    (list :snapshot (magent-thread-snapshot-from-alist snapshot)
          :events (mapcar #'magent-thread-event-from-alist
                          (cdr (assq 'journal data)))
          :agent-jobs (mapcar #'magent-agent-job-from-alist
                              (cdr (assq 'agent-jobs data)))
          :approval-overrides
          (mapcar #'magent-session--approval-override-from-alist
                  (cdr (assq 'approval-overrides data))))))

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
  (unless (symbolp perm-key)
    (error "Approval override key must be a symbol: %S" perm-key))
  (unless (memq decision '(allow deny))
    (error "Approval override decision must be allow or deny: %S" decision))
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

(defun magent-session-activate (&optional scope)
  "Activate SCOPE and return its session.
SCOPE must be either `global' or a normalized project root string."
  (let ((target-scope (or scope 'global)))
    (magent-tool-output-spill-cleanup-all)
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
           (id (and (magent-session-id session)
                    (magent-session-validate-id
                     (magent-session-id session))))
           (filepath
            (and id
                 (expand-file-name
                  (concat id ".json")
                  (magent-session--scope-storage-directory target-scope)))))
      (magent-session--cancel-deferred-save-for-session session target-scope)
      (setf (magent-session-approval-overrides session) nil
            (magent-session-agent-jobs session) nil
            (magent-session-thread session) nil
            (magent-session-metadata session) nil)
      (remhash session magent-session--loaded-sessions)
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
  "Reset the current session, clearing its ledger and permission overrides."
  (let ((session magent--current-session))
    (when session
      (magent-session-clear-approval-overrides session)))
  (remhash magent-session--current-scope magent-session--scoped-sessions)
  (setq magent--current-session nil)
  (when (fboundp 'magent-clear-capability-overrides)
    (magent-clear-capability-overrides))
  (magent-log "INFO session cleared for scope %s" magent-session--current-scope))

(defun magent-session--scope-storage-directory (scope)
  "Return the storage directory for SCOPE."
  (cond
   ((magent-session-action-scope-p scope)
    (magent-session-action-directory (plist-get scope :action)))
   ((eq scope 'global)
    (expand-file-name "global" magent-session-directory))
   (t
    (expand-file-name
     (concat "projects/" (secure-hash 'sha1 scope))
     magent-session-directory))))

(defun magent-session--infer-file-scope (filepath)
  "Infer the session scope for FILEPATH."
  (let ((path (file-truename filepath))
        (project-prefix (file-name-as-directory
                         (expand-file-name "projects" magent-session-directory))))
    (cond
     ((string-prefix-p project-prefix path)
      (file-name-directory (directory-file-name path)))
     ((string-prefix-p
       (file-name-as-directory
        (expand-file-name "global" magent-session-directory))
       path)
      'global)
     (t
      (signal 'magent-session-schema-error
              (list (format "Session file is outside a current storage scope: %s"
                            filepath)))))))

(defun magent-session--file-scope-kind (filepath)
  "Return the symbolic scope kind for FILEPATH."
  (if (eq (magent-session--infer-file-scope filepath) 'global)
      'global
    'project))

(defun magent-session--file-display-time (filepath)
  "Return FILEPATH's modification time."
  (file-attribute-modification-time (file-attributes filepath)))

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

(defun magent-session-list-action-files (&optional action)
  "Return isolated action session files, optionally limited to ACTION."
  (let ((directory (magent-session-action-directory action)))
    (when (file-directory-p directory)
      (magent-session--sort-files-by-time
       (directory-files-recursively directory "\\.json$")))))

(defun magent-session--read-validated-data (filepath)
  "Read and validate current session data from FILEPATH."
  (with-temp-buffer
    (insert-file-contents filepath)
    (let* ((data (json-parse-buffer
                  :object-type 'alist
                  :array-type 'list
                  :null-object nil
                  :false-object :json-false))
           (_fields (magent-session--validate-json-fields data))
           (_schema-version
            (magent-session--validate-schema-version
             (cdr (assq 'schema-version data))))
           (state (magent-session--decode-json-state data))
           (file-id (magent-session--file-id filepath))
           (raw-id (cdr (assq 'id data)))
           (_required-id
            (unless raw-id
              (signal 'magent-session-schema-error
                      (list "Session is missing its id"))))
           (id (magent-session-validate-id raw-id))
           (_matching-id
            (unless (equal id file-id)
              (signal
               'magent-session-schema-error
               (list (format "Session id %S does not match filename %S"
                             id file-id)))))
           (scope-name (cdr (assq 'scope data)))
           (project-root (cdr (assq 'project-root data)))
           (scope
            (pcase scope-name
              ("project"
               (or (and (stringp project-root)
                        (magent-session--normalize-project-root project-root))
                   (signal 'magent-session-schema-error
                           (list "Project session is missing project-root"))))
              ("global" 'global)
              (_
               (signal 'magent-session-schema-error
                       (list (format "Invalid session scope: %S"
                                     scope-name)))))))
      (list :data data :id id :scope scope :state state))))

(defun magent-session--read-file-metadata (filepath)
  "Read lightweight metadata from session FILEPATH."
  (condition-case nil
      (let* ((validated (magent-session--read-validated-data filepath))
             (data (plist-get validated :data))
             (id (plist-get validated :id))
             (scope (plist-get validated :scope))
             (kind (cdr (assq 'kind data)))
             (action (cdr (assq 'action data)))
             (status (cdr (assq 'status data)))
             (title (cdr (assq 'title data)))
             (parent-session-id (cdr (assq 'parent-session-id data)))
             (metadata (cdr (assq 'metadata data)))
             (summary-title (or (magent-session--clean-summary-title title)
                                (magent-session--clean-summary-title
                                 (cdr (assq 'summary-title data))))))
        (list :valid t
              :id id
              :scope (if (eq scope 'global) 'global 'project)
              :project-root (and (stringp scope) scope)
              :summary-title summary-title
              :kind kind
              :action action
              :status status
              :title title
              :parent-session-id parent-session-id
              :metadata metadata))
    (error
     (list :valid nil
           :id nil
           :scope (magent-session--file-scope-kind filepath)
           :project-root nil
           :summary-title nil
           :kind nil
           :action nil
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
  "Return a display timestamp for session FILEPATH."
  (format-time-string "%Y-%m-%d %H:%M:%S"
                      (magent-session--file-display-time filepath)))

;;; Session persistence

(defun magent-session-save-for-session (session scope)
  "Synchronously save SESSION for explicit SCOPE as <session-id>.json.
This is the persistence primitive for asynchronous callers: it never reads or
temporarily rebinds the ambient current session or scope."
  (unless (magent-session-p session)
    (error "Expected a Magent session, got: %S" session))
  (unless scope
    (error "An explicit session scope is required"))
  (let ((thread (magent-session-thread-ledger session)))
    (when (or (magent-thread-turns thread)
              (magent-session-agent-jobs session))
      (let ((storage-dir (magent-session--scope-storage-directory scope)))
        (make-directory storage-dir t)
        (let* ((id (magent-session-get-id session))
               (filepath (expand-file-name (concat id ".json") storage-dir))
               (origin-scope (magent-session--origin-scope-for-session
                              session scope))
               (kind (magent-session--metadata-string session 'kind))
               (action (magent-session--metadata-string session 'action))
               (status (magent-session--metadata-string session 'status))
               (title (magent-session--metadata-string session 'title))
               (parent-session-id
                (magent-session--metadata-string session 'parent-session-id))
               (summary-title (magent-session-summary-title session))
               (approval-overrides
                (mapcar (lambda (entry)
                          `((tool . ,(symbol-name (car entry)))
                            (decision . ,(symbol-name (cdr entry)))))
                        (magent-session-approval-overrides session)))
               (data `((id . ,id)
                       (schema-version . ,magent-session-schema-version)
                       ,@(when kind
                           `((kind . ,kind)))
                       ,@(when action
                           `((action . ,action)))
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
                       (snapshot . ,(magent-thread-snapshot-to-alist thread))
                       (journal . ,(vconcat
                                    (mapcar #'magent-thread-event-to-alist
                                            (magent-session--persisted-journal
                                             thread))))
                       (agent-jobs . ,(vconcat
                                       (mapcar
                                        #'magent-agent-job-to-alist
                                        (magent-session-agent-jobs session))))
                       (approval-overrides . ,(vconcat approval-overrides)))))
          (magent-session--write-json-atomic filepath data)
          (remhash filepath magent-session--metadata-cache)
          (magent-log "INFO session saved to %s (%d turns) scope=%s"
                      id (length (magent-thread-turns thread)) scope)
          filepath)))))

(defun magent-session-save-deferred-for-session (session &optional scope delay)
  "Schedule SESSION to be saved for SCOPE after Emacs is idle.
SCOPE defaults to SESSION's ledger scope, falling back to the active scope.
Repeated requests for the same SESSION and SCOPE coalesce behind one shared
idle timer.  Different sessions remain distinct and no ambient session state
is consulted when the timer fires."
  (unless (magent-session-p session)
    (error "Expected a Magent session, got: %S" session))
  (let ((target-scope (or scope
                          (magent-session--scope-for-thread session)
                          magent-session--current-scope)))
    (unless (cl-find-if
             (lambda (entry)
               (and (eq (car entry) session)
                    (equal (cdr entry) target-scope)))
             magent-session--pending-saves)
      (push (cons session target-scope) magent-session--pending-saves))
    (unless magent-session--save-timer
      (setq magent-session--save-timer
            (run-with-idle-timer
             (or delay magent-session-save-idle-delay) nil
             #'magent-session--flush-deferred-saves)))
    magent-session--save-timer))

(defun magent-session--flush-deferred-saves ()
  "Flush all coalesced deferred session saves independently."
  (let ((pending (nreverse magent-session--pending-saves)))
    (setq magent-session--pending-saves nil
          magent-session--save-timer nil)
    (dolist (entry pending)
      (condition-case err
          (magent-session-save-for-session (car entry) (cdr entry))
        (error
         (magent-log "WARN deferred session save failed for %s: %s"
                     (or (magent-session-id (car entry)) "<new-session>")
                     (error-message-string err)))))))

(defun magent-session--cancel-deferred-save-for-session (session &optional scope)
  "Remove pending saves for SESSION, restricted to SCOPE when non-nil."
  (setq magent-session--pending-saves
        (cl-delete-if
         (lambda (entry)
           (and (eq (car entry) session)
                (or (null scope) (equal (cdr entry) scope))))
         magent-session--pending-saves))
  (when (and (null magent-session--pending-saves)
             magent-session--save-timer)
    (cancel-timer magent-session--save-timer)
    (setq magent-session--save-timer nil)))

(defun magent-session-read-file (filepath)
  "Read session data from FILEPATH without changing active session state.
Return a plist with keys `:scope', `:session', and `:id', or nil on error."
  (condition-case err
      (let* ((validated (magent-session--read-validated-data filepath))
             (data (plist-get validated :data))
             (id (plist-get validated :id))
             (scope (plist-get validated :scope))
             (state (plist-get validated :state))
             (kind (cdr (assq 'kind data)))
             (action (cdr (assq 'action data)))
             (status (cdr (assq 'status data)))
             (title (cdr (assq 'title data)))
             (parent-session-id (cdr (assq 'parent-session-id data)))
             (metadata-raw (cdr (assq 'metadata data)))
             (thread
              (magent-thread-replay
               (plist-get state :snapshot)
               (plist-get state :events)))
             (agent-jobs (plist-get state :agent-jobs))
             (approval-overrides (plist-get state :approval-overrides))
             (metadata (append metadata-raw
                               (delq nil
                                     `((kind . ,kind)
                                       (action . ,action)
                                       (status . ,status)
                                       (title . ,title)
                                       (parent-session-id
                                        . ,parent-session-id)
                                       (origin-scope . ,scope)))))
             (session (magent-session-create
                       :id id
                       :metadata metadata
                       :agent-jobs agent-jobs
                       :approval-overrides approval-overrides
                       :thread thread)))
          (puthash session filepath magent-session--loaded-sessions)
          (list :scope scope
                :session session
                :id id))
    (error
     (magent-log "ERROR loading session %s: %s" filepath (error-message-string err))
     nil)))

(defun magent-session-reconcile-after-restart (session)
  "Terminalize SESSION state that cannot survive an Emacs restart.
Return the number of thread, item, and child-job lifecycle objects changed."
  (let ((changed 0)
        (reason "Interrupted by Emacs restart"))
    (when-let* ((thread (magent-session-thread session)))
      (cl-incf changed
               (magent-thread-reconcile-after-restart thread reason)))
    (dolist (job (magent-session-agent-jobs session))
      (when (magent-agent-job-reconcile-after-restart job reason)
        (cl-incf changed)))
    changed))

(defun magent-session-install (scope session)
  "Install SESSION for SCOPE and make it active.
Persisted non-terminal work is reconciled once before the session becomes
available, then saved atomically through the explicit session/scope API."
  (let ((recovered
         (when (gethash session magent-session--loaded-sessions)
           (prog1 (magent-session-reconcile-after-restart session)
             (remhash session magent-session--loaded-sessions)))))
    (puthash scope session magent-session--scoped-sessions)
    (magent-session-activate scope)
    (when (> (or recovered 0) 0)
      (condition-case err
          (magent-session-save-for-session session scope)
        (error
         (magent-log
          "ERROR saving reconciled session %s: %s"
          (or (magent-session-id session) "unknown")
          (error-message-string err)))))
    session))

(defun magent-session-refresh-agent (session)
  "Refresh SESSION's agent pointer from the current registry.
When the session references a custom agent that is no longer active for the
current scope, clear it so Magent falls back to the default agent."
  (when-let* ((agent (magent-session-agent session)))
    (when (fboundp 'magent-agent-registry-get)
      (setf (magent-session-agent session)
            (magent-agent-registry-get (magent-agent-info-name agent)))))
  session)

(defun magent-session-list-files ()
  "Return all session JSON files grouped by project for resume display."
  (magent-session--sort-files-for-display
   (cl-remove-if-not
    (lambda (file)
      (let ((metadata (magent-session--read-file-metadata-cached file)))
        (or (not (plist-member metadata :valid))
            (plist-get metadata :valid))))
    (delq nil (magent-session--all-files)))))

(defun magent-session-list-files-for-scope (scope)
  "Return valid saved session files stored for exact SCOPE, newest first."
  (unless scope
    (error "An explicit session scope is required"))
  (cl-remove-if-not
   (lambda (file)
     (let ((metadata (magent-session--read-file-metadata-cached file)))
       (or (not (plist-member metadata :valid))
           (plist-get metadata :valid))))
   (magent-session--list-files-in-directory
    (magent-session--scope-storage-directory scope))))

(defun magent-session-get-id (session)
  "Get or generate a unique ID for SESSION."
  (or (and (magent-session-id session)
           (magent-session-validate-id (magent-session-id session)))
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

(defun magent-session--trim-history (session)
  "Trim SESSION's ledger to its message-item history limit."
  (let* ((thread (magent-session-thread-ledger session))
         (count (cl-count 'message (magent-thread-all-items thread)
                          :key #'magent-thread-item-type))
         (max (magent-session-max-history session))
         (to-remove (- count max)))
    (when (> to-remove 0)
      (setf (magent-thread-turns thread)
            (magent-session--trim-thread-turns
             (magent-thread-turns thread) max))
      (magent-log "INFO trimmed %d old ledger messages" to-remove))))

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

(defun magent-session--metadata-value (metadata key)
  "Return KEY from plist or alist METADATA."
  (cond
   ((magent-json--plist-p metadata) (plist-get metadata key))
   ((listp metadata)
    (or (map-elt metadata key)
        (map-elt metadata (intern (substring (symbol-name key) 1)))
        (map-elt metadata (substring (symbol-name key) 1))))))

(defun magent-session--content-block-value (block key)
  "Return KEY from ACP-style content BLOCK."
  (or (map-elt block key)
      (map-elt block (intern (concat ":" (symbol-name key))))
      (map-elt block (symbol-name key))))

(defun magent-session--resource-block-text (block)
  "Render one normalized ACP resource BLOCK for model input."
  (let* ((type (magent-session--content-block-value block 'type))
         (resource (magent-session--content-block-value block 'resource))
         (uri (or (magent-session--content-block-value block 'uri)
                  (and resource
                       (magent-session--content-block-value resource 'uri))))
         (name (or (magent-session--content-block-value block 'name)
                   (and resource
                        (magent-session--content-block-value resource 'name))))
         (mime-type
          (or (magent-session--content-block-value block 'mimeType)
              (and resource
                   (magent-session--content-block-value resource 'mimeType))))
         (text (or (magent-session--content-block-value block 'text)
                   (and resource
                        (magent-session--content-block-value resource 'text))))
         (label (or name uri type "resource"))
         (metadata
          (string-join
           (delq nil
                 (list (format "Name: %s" label)
                       (and uri (format "URI: %s" uri))
                       (and mime-type (format "MIME type: %s" mime-type))))
           "\n")))
    (if (and (stringp text) (not (string-empty-p text)))
        (format "[Attached context resource]\n%s\nContent:\n%s\n[End attached context resource]"
                metadata text)
      (format "[Attached context resource link]\n%s\n[End attached context resource link]"
              metadata))))

(defun magent-session-content-blocks-to-prompt (content-blocks)
  "Render normalized CONTENT-BLOCKS as one user-role model prompt."
  (let (parts)
    (dolist (block (append content-blocks nil))
      (let ((type (magent-session--content-block-value block 'type)))
        (push
         (if (or (null type) (equal type "text"))
             (or (magent-session--content-block-value block 'text) "")
           (magent-session--resource-block-text block))
         parts)))
    (string-trim (mapconcat #'identity (nreverse parts) "\n"))))

(defun magent-session--turn-content-blocks (turn)
  "Return structured user content blocks recorded for TURN, or nil."
  (let* ((item (magent-thread-turn-message-item turn 'user))
         (item-metadata (and item (magent-thread-item-metadata item))))
    (or (magent-session--metadata-value item-metadata :content-blocks)
        (magent-session--metadata-value
         (magent-thread-turn-metadata turn) :content-blocks))))

(defun magent-session--turn-user-content (turn)
  "Return TURN's prompt-visible user content."
  (or (when-let* ((content-blocks
                   (magent-session--turn-content-blocks turn)))
        (magent-session-content-blocks-to-prompt content-blocks))
      (magent-session--turn-message-content turn 'user)
      (magent-thread-turn-input turn)))

(defun magent-session--tool-prompt-entry (item)
  "Return a gptel prompt-list tool plist for ledger ITEM."
  (let ((output (magent-thread-item-output item)))
    (list :id (or (magent-thread-item-call-id item)
                  (magent-thread-item-id item))
          :name (magent-json-safe-name
                 (magent-thread-item-name item))
          :args (magent-thread-tool-input-plist
                 (magent-thread-item-input item))
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
  (let* ((status (magent-thread-turn-status turn))
         (metadata (magent-thread-turn-metadata turn))
         (workflow-control
          (magent-session--metadata-value metadata :workflow-control))
         (workflow-activity
          (magent-session--metadata-value metadata :workflow-activity))
         (current-p
          (and current-turn-id
               (equal (magent-thread-turn-id turn) current-turn-id))))
    (and (not workflow-control)
         (or (not workflow-activity) current-p)
         (or (eq status 'completed)
             (and current-p (memq status '(queued in-progress)))))))

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

(defun magent-session--provider-context-view (session &optional current-turn-id)
  "Build the explicit provider replay context view for SESSION.
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

(defconst magent-session-context-view-kinds
  '(ledger transcript provider compaction audit)
  "Explicit durable and derived session context views.")

(defun magent-session-context-view
    (session kind &optional current-turn-id)
  "Return explicit context view KIND for SESSION.
LEDGER is the complete materialized snapshot, TRANSCRIPT is the UI-facing
ledger projection, PROVIDER is provider-shaped replay, COMPACTION is the
bounded model replay used as summarizer input, and AUDIT contains the durable
snapshot plus the bounded journal tail."
  (unless (memq kind magent-session-context-view-kinds)
    (error "Unknown Magent context view: %S" kind))
  (let ((thread (magent-session-thread-ledger session)))
    (pcase kind
      ('ledger (and thread (magent-thread-snapshot-to-alist thread)))
      ('transcript (and thread (magent-thread-transcript thread)))
      ((or 'provider 'compaction)
       (magent-session--provider-context-view session current-turn-id))
      ('audit
       (and thread
            `((snapshot . ,(magent-thread-snapshot-to-alist thread))
              (journal . ,(vconcat
                            (mapcar #'magent-thread-event-to-alist
                                    (magent-thread-journal thread))))))))))

(provide 'magent-session)
;;; magent-session.el ends here
