;;; magent-session.el --- Session management for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Session management for storing conversation history and state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magent-config)

;;; Session state structure

(cl-defstruct (magent-session
               (:constructor magent-session-create)
               (:copier nil))
  (messages nil)             ; List of messages in chronological order
  (max-history magent-max-history)
  (id nil)
  (agent nil)
  (buffer-content nil))      ; Saved buffer text for lossless restore

;;; Message helpers

(defsubst magent-msg-role (msg)
  "Return the role symbol of message MSG."
  (cdr (assq 'role msg)))

(defsubst magent-msg-content (msg)
  "Return the content of message MSG (string or content-block list)."
  (cdr (assq 'content msg)))

(defsubst magent-session--content-to-string (content)
  "Coerce CONTENT to a plain string.
If CONTENT is a string, return it unchanged.
If CONTENT is a list of content blocks, concatenate their text fields."
  (if (stringp content) content
    (mapconcat (lambda (b) (or (cdr (assq 'text b)) "")) content "")))

(defconst magent-session-summary-title-max-width 48
  "Maximum display width for saved session summary titles.")

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
          (when-let ((title (magent-session--clean-summary-title
                             (magent-session--content-to-string content))))
            (throw 'title title)))))
    nil))

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

(defun magent-session--detect-project-root (&optional directory)
  "Return the project root for DIRECTORY, or nil when no project is found."
  (let ((default-directory (or directory default-directory)))
    (or (when (bound-and-true-p magent-project-root-function)
          (funcall magent-project-root-function))
        (when (fboundp 'projectile-project-root)
          (ignore-errors (projectile-project-root)))
        (when (fboundp 'project-current)
          (ignore-errors
            (when-let ((proj (project-current nil)))
              (if (fboundp 'project-root)
                  (project-root proj)
                (car (with-no-warnings (project-roots proj))))))))))

(defun magent-session--normalize-project-root (root)
  "Normalize project ROOT for use as a stable scope key."
  (when root
    (file-truename (directory-file-name root))))

(defun magent-session-scope-from-directory (&optional directory)
  "Return the session scope derived from DIRECTORY.
Returns a normalized project root string or the symbol `global'."
  (or (magent-session--normalize-project-root
       (magent-session--detect-project-root directory))
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

(defun magent-session-reset ()
  "Reset the current session, clearing all messages and permission overrides."
  (interactive)
  (remhash magent-session--current-scope magent-session--scoped-sessions)
  (setq magent--current-session nil)
  (when (fboundp 'magent-permission-clear-session-overrides)
    (magent-permission-clear-session-overrides))
  (when (fboundp 'magent-queue-clear)
    (magent-queue-clear))
  (magent-log "INFO session cleared for scope %s" magent-session--current-scope))

(defun magent-session--scope-storage-directory (scope)
  "Return the storage directory for SCOPE."
  (if (eq scope 'global)
      magent-session-directory
    (expand-file-name
     (concat "projects/" (secure-hash 'sha1 scope))
     magent-session-directory)))

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

(defun magent-session--sort-files-by-mtime (files)
  "Return FILES sorted by modification time, newest first."
  (sort files
        (lambda (a b)
          (time-less-p
           (file-attribute-modification-time (file-attributes b))
           (file-attribute-modification-time (file-attributes a))))))

(defun magent-session--list-files-in-directory (directory)
  "Return session JSON files in DIRECTORY, newest first."
  (when (file-directory-p directory)
    (magent-session--sort-files-by-mtime
     (directory-files directory t "\\.json$"))))

(defun magent-session--project-files ()
  "Return all project-scoped session files under `magent-session-directory'."
  (let ((projects-dir (expand-file-name "projects" magent-session-directory)))
    (when (file-directory-p projects-dir)
      (magent-session--sort-files-by-mtime
       (directory-files-recursively projects-dir "\\.json$")))))

(defun magent-session--read-file-metadata (filepath)
  "Read lightweight metadata from session FILEPATH."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents filepath)
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read))
               (scope-name (cdr (assq 'scope data)))
               (project-root (cdr (assq 'project-root data)))
               (summary-title (or (magent-session--clean-summary-title
                                   (cdr (assq 'summary-title data)))
                                  (magent-session--summary-title-from-messages
                                   (cdr (assq 'messages data))))))
          (list :scope (if (equal scope-name "project") 'project 'global)
                :project-root (magent-session--normalize-project-root project-root)
                :summary-title summary-title)))
    (error
     (list :scope (magent-session--file-scope-kind filepath)
           :project-root nil
           :summary-title nil))))

(defun magent-session--project-label (project-root)
  "Return a human-readable label for PROJECT-ROOT."
  (if project-root
      (abbreviate-file-name project-root)
    "Unknown project"))

(defun magent-session--file-group (filepath)
  "Return the completion group label for FILEPATH."
  (let* ((meta (magent-session--read-file-metadata filepath))
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
  (let* ((meta (magent-session--read-file-metadata filepath))
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
               (file-attribute-modification-time (file-attributes b))
               (file-attribute-modification-time (file-attributes a)))))))))

;;; Session persistence

(defun magent-session--msg-to-alist (msg)
  "Convert MSG to a JSON-serializable alist with string role."
  (let ((role (magent-msg-role msg))
        (content (magent-msg-content msg)))
    `((role . ,(symbol-name role))
      (content . ,(magent-session--content-to-string content)))))

(defun magent-session--alist-to-msg (alist)
  "Reconstruct a session message from JSON-decoded ALIST."
  (let ((role (intern (cdr (assq 'role alist))))
        (content (cdr (assq 'content alist))))
    `((role . ,role) (content . ,content))))

(defun magent-session-save ()
  "Save the current session to disk as <session-id>.json.
Called automatically after each successful LLM response.
The caller is responsible for updating `magent-session-buffer-content'
before calling this function."
  (let* ((scope magent-session--current-scope)
         (session magent--current-session))
    (when (and session (magent-session-messages session))
      (let ((storage-dir (magent-session--scope-storage-directory scope)))
        (make-directory storage-dir t)
        (let* ((messages (magent-session-messages session))
               (id (magent-session-get-id session))
               (filepath (expand-file-name (concat id ".json") storage-dir))
               (summary-title (magent-session--summary-title-from-messages messages))
               (data `((id . ,id)
                       (scope . ,(if (eq scope 'global) "global" "project"))
                       ,@(unless (eq scope 'global)
                           `((project-root . ,scope)))
                       ,@(when summary-title
                           `((summary-title . ,summary-title)))
                       (messages . ,(vconcat (mapcar #'magent-session--msg-to-alist messages)))
                       (buffer-content . ,(or (magent-session-buffer-content session) "")))))
          (with-temp-file filepath
            (insert (json-encode data)))
          (magent-log "INFO session saved to %s (%d messages) scope=%s"
                      id (length messages) scope))))))

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
               (scope-name (cdr (assq 'scope data)))
               (project-root (cdr (assq 'project-root data)))
               (msgs-raw (cdr (assq 'messages data)))
               (bc (or (cdr (assq 'buffer-content data)) ""))
               (scope (pcase scope-name
                        ("project"
                         (or (magent-session--normalize-project-root project-root)
                             (magent-session--infer-file-scope filepath)))
                        ("global" 'global)
                        (_ (magent-session--infer-file-scope filepath))))
               (messages (mapcar #'magent-session--alist-to-msg msgs-raw))
               (session (magent-session-create
                         :id id
                         :messages messages
                         :buffer-content (when (> (length bc) 0) bc))))
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
Parses the session-YYYYMMDD-HHMMSS filename pattern into a date string."
  (let* ((name (file-name-sans-extension (file-name-nondirectory filepath)))
         (meta (magent-session--read-file-metadata filepath))
         (scope (plist-get meta :scope))
         (project-root (plist-get meta :project-root))
         (summary-title (plist-get meta :summary-title))
         (timestamp
          (if (string-match
               "session-\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)"
               name)
              (format "%s-%s-%s %s:%s:%s"
                      (match-string 1 name) (match-string 2 name) (match-string 3 name)
                      (match-string 4 name) (match-string 5 name) (match-string 6 name))
            name)))
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
  (let* ((msg (list (cons 'role role) (cons 'content content)))
         (messages (magent-session-messages session))
         (new-messages (nconc messages (list msg))))
    (setf (magent-session-messages session) new-messages)
    (when (> (length new-messages) (+ (magent-session-max-history session) 10))
      (magent-session--trim-history session)))
  session)

(defun magent-session--trim-history (session)
  "Trim SESSION messages to max-history limit."
  (let* ((messages (magent-session-messages session))
         (count (length messages))
         (max (magent-session-max-history session))
         (to-remove (- count max)))
    (when (> to-remove 0)
      (setf (magent-session-messages session) (nthcdr to-remove messages))
      (magent-log "INFO Trimmed session history: removed %d old messages" to-remove))))

(defun magent-session-get-messages (session)
  "Get all messages from SESSION in chronological order."
  ;; Messages are now stored in chronological order, no reverse needed
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

(defun magent-session-to-gptel-prompt-list (session)
  "Convert SESSION messages to a gptel-request prompt list.
Returns a list in gptel's advanced format:
  ((prompt . \"user msg\") (response . \"assistant msg\") ...)
Tool result messages are skipped; gptel handles tool round-trips
internally within each request."
  (let ((messages (magent-session-get-messages session)))
    (delq nil
          (mapcar
           (lambda (msg)
             (let ((role (magent-msg-role msg))
                   (content (magent-msg-content msg)))
               (pcase role
                 ('user
                  (cons 'prompt (magent-session--content-to-string content)))
                 ('assistant
                  (cons 'response (magent-session--content-to-string content)))
                 (_ nil))))
           messages))))

(provide 'magent-session)
;;; magent-session.el ends here
