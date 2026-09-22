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

(defun magent-session--assistant-response-reusable-p (content)
  "Return non-nil when assistant CONTENT contains visible text.
Failure attribution belongs to ledger status and metadata, not a text prefix."
  (not (string-blank-p (magent-session--content-to-string content))))

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

(defun magent-session--normalize-project-root (root)
  "Normalize project ROOT for use as a stable scope key.
Remote roots are normalized textually so session routing does not contact the
TRAMP host.  Local roots retain symlink-aware canonicalization."
  (when root
    (let ((expanded (directory-file-name (expand-file-name root))))
      (if (file-remote-p expanded)
          expanded
        (file-truename expanded)))))

(defun magent-session-canonical-scope (scope)
  "Return canonical project origin for SCOPE, or nil for global scope."
  (let ((origin (magent-session-scope-origin scope)))
    (cond
     ((or (null origin) (eq origin 'global)) nil)
     ((stringp origin)
      (condition-case nil
          (magent-session--normalize-project-root origin)
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

(defconst magent-session--summary-title-scan-limit 4096
  "Characters of a message considered when deriving a summary title.
A title is at most `magent-session-summary-title-max-width' characters and
only depends on the earliest non-blank text, so cleaning a whole message
body would cost O(message) for no visible difference.")

(defun magent-session--summary-title-from-thread (thread)
  "Derive a brief summary title from THREAD.
Walk turns directly and stop at the first usable message: materializing every
item first would make this O(conversation) on a path that only needs the
earliest one."
  (catch 'title
    (dolist (turn (and thread (magent-thread-turns thread)))
      (dolist (item (magent-thread-turn-items turn))
        (when (and (eq (magent-thread-item-type item) 'message)
                   (memq (magent-thread-item-role item) '(user assistant)))
          (let ((content (magent-session--content-to-string
                          (magent-thread-item-content item))))
            (when (and (stringp content) (not (string-empty-p content)))
              (when-let* ((title (magent-session--clean-summary-title
                                  (substring content 0 (min (length content)
                                                            magent-session--summary-title-scan-limit)))))
                (throw 'title title)))))))
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
          (magent-thread-journal-tail thread) nil
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
  "Shared elapsed-time timer used to flush deferred session saves.")

(defvar magent-session--pending-saves nil
  "Deferred saves as (SESSION . SCOPE) pairs awaiting the shared timer.")

(defconst magent-session-schema-version 7
  "Current schema version written to session JSON files.")

(defconst magent-session--legacy-schema-version 6
  "Schema version whose session JOURNAL was stored inline and is migrated.")

(defconst magent-session--json-fields
  '(id schema-version kind action status title parent-session-id metadata
    scope project-root summary-title agent-jobs approval-overrides)
  "Fields accepted by the current session header schema.
The materialized ledger snapshot and the event log are stored in sibling
files, so neither bulk artifact is re-encoded when a header field changes.")

(defconst magent-session--required-json-fields
  '(id schema-version scope agent-jobs approval-overrides)
  "Fields required by the current session header schema.")

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

(defun magent-session--decode-json-state (snapshot events header)
  "Decode persistence objects from SNAPSHOT, EVENTS, and HEADER.
SNAPSHOT is the decoded ledger snapshot alist, EVENTS the already-validated
events that replay after it, and HEADER the session header alist."
  (unless snapshot
    (signal 'magent-session-schema-error
            (list "Session is missing its ledger snapshot")))
  (list :snapshot (magent-thread-snapshot-from-alist snapshot)
        :events events
        :agent-jobs (mapcar #'magent-agent-job-from-alist
                            (cdr (assq 'agent-jobs header)))
        :approval-overrides
        (mapcar #'magent-session--approval-override-from-alist
                (cdr (assq 'approval-overrides header)))))

;;; Session persistence layout
;;
;; A session is stored as three files whose sizes and rates of change differ
;; by orders of magnitude, so each pays only for itself:
;;
;;   <id>.json      header plus small mutable session state; listing reads it
;;   <id>.jsonl     append-only ledger events, one JSON object per line
;;   <id>.snapshot  materialized ledger state, rewritten only on compaction
;;
;; Streaming appends to the log, so persistence costs what changed rather than
;; what the conversation has accumulated.  Between checkpoints the log is
;; authoritative: `magent-thread-replay' applies the snapshot first and skips
;; any logged event its `last-event-seq' already covers, which is what makes
;; snapshot-then-truncate crash safe.

(defconst magent-session--log-extension ".jsonl"
  "Suffix of the append-only session event log.")

(defconst magent-session--snapshot-extension ".snapshot"
  "Suffix of a session's materialized ledger snapshot file.")

(defvar magent-session--persisted-cursor (make-hash-table :test #'eq)
  "Last journal cons already written per session.
Holding the cons rather than a sequence number keeps \"which events are new\"
a constant-time question.")

(defvar magent-session--log-count (make-hash-table :test #'eq)
  "Number of events appended to each session's log since its checkpoint.")

(defvar magent-session--written-header (make-hash-table :test #'eq)
  "JSON text last written for each session's header file.
The header is rewritten on every flush, but most flushes change nothing in
it; skipping an identical rewrite removes a file replace per save.")

(defun magent-session--log-filepath (filepath)
  "Return the event-log path paired with session FILEPATH."
  (magent-session--sibling-filepath filepath magent-session--log-extension))

(defun magent-session--snapshot-filepath (filepath)
  "Return the ledger-snapshot path paired with session FILEPATH."
  (magent-session--sibling-filepath filepath magent-session--snapshot-extension))

(defun magent-session--sibling-filepath (filepath extension)
  "Return FILEPATH's sibling sharing its id and using EXTENSION."
  (expand-file-name
   (concat (file-name-nondirectory (file-name-sans-extension filepath))
           extension)
   (file-name-directory filepath)))

(defun magent-session--json-text (data)
  "Return DATA encoded as JSON text with Magent's null and false sentinels.
`json-serialize' is an order of magnitude faster than `json-encode' but
rejects Lisp symbols, so values pass through `magent-json-safe-value' first.
That is what `json-encode' used to coerce silently; doing it explicitly keeps
the previous on-disk shape while surfacing genuinely unsupported values."
  (json-serialize (magent-json-safe-value data)
                  :null-object :null
                  :false-object :json-false))

(defun magent-session--event-line (event)
  "Return EVENT encoded as one single-line JSON object.
Any newline inside the payload is escaped by the encoder, so a line always
corresponds to exactly one event."
  (magent-session--json-text (magent-thread-event-to-alist event)))

(defun magent-session--log-text (lines)
  "Return LINES joined as log text, or the empty string when LINES is nil."
  (if lines (concat (mapconcat #'identity lines "\n") "\n") ""))

(defun magent-session--write-log-events (filepath events)
  "Atomically replace the log at FILEPATH with EVENTS."
  (make-directory (file-name-directory filepath) t)
  (let* ((directory (file-name-directory filepath))
         (tempfile (make-temp-file (expand-file-name ".magent-log-" directory)
                                   nil ".jsonl.tmp")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-buffer
              (insert (magent-session--log-text
                       (mapcar #'magent-session--event-line events)))
              (write-region (point-min) (point-max) tempfile nil 'silent)))
          (rename-file tempfile filepath t)
          (setq tempfile nil))
      (when (and tempfile (file-exists-p tempfile))
        (delete-file tempfile)))))

(defun magent-session--append-events-to-log (filepath events)
  "Append EVENTS to the log at FILEPATH, one JSON object per line."
  (when events
    (make-directory (file-name-directory filepath) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-buffer
        (insert (magent-session--log-text (mapcar #'magent-session--event-line
                                                  events)))
        (write-region (point-min) (point-max) filepath t 'silent)))))

(defun magent-session--parse-log-line (line filepath index)
  "Return the event encoded by LINE from FILEPATH at 1-based INDEX.
Signal `magent-session-schema-error' instead of guessing at malformed data."
  (condition-case err
      (magent-thread-event-from-alist
       (json-parse-string line
                          :object-type 'alist
                          :array-type 'array
                          :null-object nil
                          :false-object :json-false))
    (error
     (signal 'magent-session-schema-error
             (list (format "Invalid session event at %s:%d: %s"
                           (file-name-nondirectory filepath) index
                           (error-message-string err)))))))

(defun magent-session--load-log-events (filepath)
  "Return the validated events logged at FILEPATH.
A log whose final line is incomplete was interrupted mid-append; that torn
fragment is dropped and the file is repaired, because it cannot be
interpreted unambiguously and leaving it would corrupt the next append.  A
malformed earlier line means real corruption and signals instead."
  (if (not (file-exists-p filepath))
      nil
    (let ((text (with-temp-buffer
                  (let ((coding-system-for-read 'utf-8-unix))
                    (insert-file-contents filepath))
                  (buffer-string))))
      (let* ((complete (string-suffix-p "\n" text))
             (lines (butlast (split-string text "\n")))
             (events (cl-loop for line in lines
                              for index from 1
                              collect (magent-session--parse-log-line
                                       line filepath index))))
        (unless complete
          (magent-log "WARN session log %s ends mid-line; dropping the torn tail"
                      (file-name-nondirectory filepath))
          (magent-session--write-log-events filepath events))
        events))))

(defun magent-session--read-json-object (filepath)
  "Return FILEPATH parsed as a JSON object alist."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents filepath))
    (json-parse-buffer
     :object-type 'alist
     :array-type 'array
     :null-object nil
     :false-object :json-false)))

(defun magent-session--migrate-v6-file (filepath data)
  "Rewrite schema-6 session DATA at FILEPATH as the current layout.
Return the equivalent current-version session header.  The log and snapshot
are written before the v6 file is replaced, so an interrupted migration leaves
the original file valid and the migration simply runs again."
  (let* ((logfile (magent-session--log-filepath filepath))
         (snapshot (cdr (assq 'snapshot data)))
         (events (mapcar #'magent-thread-event-from-alist
                         (cdr (assq 'journal data))))
         (header (delq nil
                       (mapcar (lambda (entry)
                                 (pcase (car entry)
                                   ((or 'journal 'snapshot) nil)
                                   ('schema-version
                                    (cons 'schema-version
                                          magent-session-schema-version))
                                   (_ entry)))
                               data))))
    (magent-session--write-log-events logfile events)
    (magent-session--write-json-atomic
     (magent-session--snapshot-filepath filepath) snapshot)
    (magent-session--write-json-atomic filepath header)
    (magent-log "INFO migrated session %s from schema %d to %d"
                (file-name-nondirectory filepath)
                magent-session--legacy-schema-version
                magent-session-schema-version)
    header))

(defun magent-session--write-json-atomic (filepath data)
  "Atomically encode DATA as JSON and replace FILEPATH."
  (let* ((directory (file-name-directory filepath))
         (prefix (expand-file-name ".magent-session-" directory))
         (tempfile (make-temp-file prefix nil ".json.tmp")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (let ((coding-system-for-write 'utf-8-unix))
              ;; `magent-session--json-text' returns unibyte UTF-8 bytes.
              (insert (decode-coding-string (magent-session--json-text data)
                                            'utf-8))
              (write-region (point-min) (point-max) tempfile nil 'silent)))
          (rename-file tempfile filepath t)
          (setq tempfile nil))
      (when (and tempfile (file-exists-p tempfile))
        (delete-file tempfile)))))

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
      (remhash session magent-session--persisted-cursor)
      (remhash session magent-session--log-count)
      (remhash session magent-session--written-header)
      (dolist (path (and filepath
                         (list filepath
                               (magent-session--log-filepath filepath)
                               (magent-session--snapshot-filepath filepath))))
        (when (and path (file-exists-p path))
          (condition-case err
              (progn
                (delete-file path)
                (remhash filepath magent-session--metadata-cache))
            (error
             (magent-log "WARN failed deleting cleared session %s: %s"
                         path (error-message-string err))))))))
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

(defun magent-session--read-validated-data (filepath &optional metadata-only)
  "Read and validate current session data from FILEPATH.
When METADATA-ONLY is non-nil the snapshot and the event log are not decoded,
which is what session listing needs and all it should pay for."
  (let* ((raw (magent-session--read-json-object filepath))
         (data (if (equal (cdr (assq 'schema-version raw))
                          magent-session--legacy-schema-version)
                   (magent-session--migrate-v6-file filepath raw)
                 raw))
         (_fields (magent-session--validate-json-fields data))
         (_schema-version
          (magent-session--validate-schema-version
           (cdr (assq 'schema-version data))))
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
                                   scope-name))))))
         ;; Decode the bulk artifacts last: a cheap header problem should not
         ;; cost a snapshot and log read, and its diagnostic must win.
         (state
          (unless metadata-only
            (let ((snapshot-file (magent-session--snapshot-filepath filepath)))
              (magent-session--decode-json-state
               (if (file-exists-p snapshot-file)
                   (magent-session--read-json-object snapshot-file)
                 (signal 'magent-session-schema-error
                         (list "Session is missing its ledger snapshot")))
               (magent-session--load-log-events
                (magent-session--log-filepath filepath))
               data)))))
    (list :data data :id id :scope scope :state state)))

(defun magent-session--read-file-metadata (filepath)
  "Read lightweight metadata from session FILEPATH."
  (condition-case nil
      (let* ((validated (magent-session--read-validated-data filepath t))
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

(defun magent-session--unpersisted-events (session thread)
  "Return THREAD's journal events not yet appended for SESSION."
  (let ((cursor (gethash session magent-session--persisted-cursor)))
    (if cursor
        (cdr cursor)
      (magent-thread-journal thread))))

(defun magent-session--header-data-for-session (session scope)
  "Return the session header JSON DATA for SESSION persisted under SCOPE.
Only header and small mutable state belong here, so rewriting it stays cheap
no matter how large the conversation grew."
  (let* ((origin-scope (magent-session--origin-scope-for-session session scope))
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
                  (magent-session-approval-overrides session))))
    `((id . ,(magent-session-get-id session))
      (schema-version . ,magent-session-schema-version)
      ,@(when kind `((kind . ,kind)))
      ,@(when action `((action . ,action)))
      ,@(when status `((status . ,status)))
      ,@(when title `((title . ,title)))
      ,@(when parent-session-id `((parent-session-id . ,parent-session-id)))
      ,@(when (magent-session-metadata session)
          `((metadata . ,(magent-json-safe-value
                          (magent-session-metadata session)))))
      (scope . ,(if (eq origin-scope 'global) "global" "project"))
      ,@(unless (eq origin-scope 'global)
          `((project-root . ,origin-scope)))
      ,@(when summary-title `((summary-title . ,summary-title)))
      (agent-jobs . ,(vconcat
                      (mapcar #'magent-agent-job-to-alist
                              (magent-session-agent-jobs session))))
      (approval-overrides . ,(vconcat approval-overrides)))))

(defun magent-session--write-header-for-session (session scope filepath)
  "Write SESSION's header to FILEPATH unless it already holds that content."
  (let* ((data (magent-session--header-data-for-session session scope))
         (text (magent-session--json-text data)))
    (unless (equal text (gethash session magent-session--written-header))
      (magent-session--write-json-atomic filepath data)
      (puthash session text magent-session--written-header))
    filepath))

(defun magent-session--compaction-due-p (session filepath)
  "Return non-nil when SESSION needs its ledger snapshot rewritten."
  (or (not (file-exists-p (magent-session--snapshot-filepath filepath)))
      (and magent-session-log-max-events
           (<= magent-session-log-max-events
               (gethash session magent-session--log-count 0)))))

(defun magent-session--bounded-journal (thread)
  "Return the journal tail retained beside THREAD's snapshot.
The snapshot already covers everything through its `last-event-seq', so the
tail is history for inspection rather than replay input."
  (let ((journal (and thread (magent-thread-journal thread)))
        (limit magent-session-log-max-events))
    (if (and (integerp limit)
             (>= limit 0)
             (> (length journal) limit))
        (last journal limit)
      journal)))

(defun magent-session--write-snapshot-for-session (session filepath)
  "Write SESSION's materialized ledger snapshot and reset its event log.
The snapshot is replaced first, so a crash before the log is rewritten leaves
a superset of events that replay simply skips by `last-event-seq'."
  (let ((thread (magent-session-thread-ledger session)))
    (magent-session--write-json-atomic
     (magent-session--snapshot-filepath filepath)
     (magent-thread-snapshot-to-alist thread))
    (magent-session--write-log-events
     (magent-session--log-filepath filepath)
     (magent-session--bounded-journal thread)))
  (puthash session 0 magent-session--log-count)
  filepath)

(defun magent-session-save-for-session (session scope)
  "Synchronously persist SESSION for explicit SCOPE.
The header <session-id>.json is rewritten (it is small), new ledger events are
appended to <session-id>.jsonl, and the materialized snapshot is rewritten only
when it is missing or the log has grown past `magent-session-log-max-events'.
This is the persistence primitive for asynchronous callers: it never reads or
temporarily rebinds the ambient current session or scope."
  (unless (magent-session-p session)
    (error "Expected a Magent session, got: %S" session))
  (unless scope
    (error "An explicit session scope is required"))
  (let ((thread (magent-session-thread-ledger session)))
    (when (or (magent-thread-turns thread)
              (magent-session-agent-jobs session))
      (let* ((storage-dir (magent-session--scope-storage-directory scope))
             (_ (make-directory storage-dir t))
             (id (magent-session-get-id session))
             (filepath (expand-file-name (concat id ".json") storage-dir))
             (new-events (magent-session--unpersisted-events session thread)))
        (when new-events
          (magent-session--append-events-to-log
           (magent-session--log-filepath filepath) new-events)
          (puthash session (magent-thread-journal-tail thread)
                   magent-session--persisted-cursor)
          (puthash session (+ (gethash session magent-session--log-count 0)
                              (length new-events))
                   magent-session--log-count))
        (magent-session--write-header-for-session session scope filepath)
        (when (magent-session--compaction-due-p session filepath)
          (magent-session--write-snapshot-for-session session filepath))
        (remhash filepath magent-session--metadata-cache)
        (magent-log "INFO session saved to %s (%d turns) scope=%s"
                    id (length (magent-thread-turns thread)) scope)
        filepath))))

(defun magent-session-save-deferred-for-session (session &optional scope delay)
  "Schedule SESSION to be saved for SCOPE after DELAY seconds.
DELAY defaults to `magent-session-save-idle-delay' and is measured from
scheduling, independently of how long Emacs has been idle.
SCOPE defaults to SESSION's ledger scope, falling back to the active scope.
Repeated requests for the same SESSION and SCOPE coalesce behind one shared
timer without postponing it.  Different sessions remain distinct and no
ambient session state is consulted when the timer fires."
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
            ;; Process output does not reset Emacs idleness.  An idle timer
            ;; could therefore fire immediately for every streaming chunk.
            (run-at-time
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
          ;; Everything on disk is already persisted; only events recorded
          ;; after this load should ever be appended.
          (puthash session (magent-thread-journal-tail thread)
                   magent-session--persisted-cursor)
          (puthash session 0 magent-session--log-count)
          (puthash session (magent-session--json-text data)
                   magent-session--written-header)
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

(defun magent-session-refresh-agent (session &optional scope)
  "Refresh SESSION's selected agent from the registry for SCOPE."
  (when-let* ((agent (magent-session-agent session)))
    (when (fboundp 'magent-agent-registry-get)
      (setf (magent-session-agent session)
            (magent-agent-registry-get
             (magent-agent-info-name agent) scope))))
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
      (let ((removed (- count (cl-count 'message (magent-thread-all-items thread)
                                       :key #'magent-thread-item-type))))
        (when (> removed 0)
          (magent-log "INFO trimmed %d old ledger messages" removed))))))

(defun magent-session--trim-thread-turns (turns max-messages)
  "Keep the newest whole TURNS covering MAX-MESSAGES messages.
Never sever a user goal from its commentary or tool results.  Retain at least
one turn, including an oversized or currently active turn."
  (let ((count 0) kept)
    (dolist (turn (reverse turns))
      (when (or (null kept) (< count max-messages)
                (memq (magent-thread-turn-status turn) '(queued in-progress)))
        (cl-incf count (cl-count 'message (magent-thread-turn-items turn)
                                :key #'magent-thread-item-type))
        (push turn kept)))
    kept))

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
         (or (not (magent-session--metadata-value metadata :compaction))
             (eq status 'completed))
         (or (not workflow-activity) current-p)
         (or (memq status '(completed interrupted failed))
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
  "Build ordered provider replay for SESSION through CURRENT-TURN-ID.
Retain user goals and terminal tool results even when a turn failed,
was interrupted, or ended without an assistant answer.  Replay completed
assistant messages at their original item positions; runtime diagnostics
and unfinished assistant fragments are not model-authored answers.
Later queued submissions are excluded from the active request."
  (let* ((thread (magent-session-thread-ledger session))
         (turns (magent-session--turns-from-last-compaction
                 (and thread (magent-thread-turns thread))))
         (current (or current-turn-id
                      (when-let* ((turn (cl-find-if
                                        (lambda (turn)
                                          (memq (magent-thread-turn-status turn)
                                                '(queued in-progress)))
                                        turns)))
                        (magent-thread-turn-id turn))))
         entries stop)
    (dolist (turn turns)
      (unless stop
        (when (magent-session--turn-include-p turn current)
          (let ((user-text (magent-session--content-to-string
                            (magent-session--turn-user-content turn))))
            (unless (string-empty-p user-text)
              (push (cons 'prompt user-text) entries))
            (dolist (item (magent-thread-turn-items turn))
              (pcase (magent-thread-item-type item)
                ('message
                 (when (and (eq (magent-thread-item-role item) 'assistant)
                            (eq (magent-thread-item-status item) 'completed)
                            (not (eq (magent-session--metadata-value
                                      (magent-thread-item-metadata item) :source)
                                     'runtime-error))
                            (magent-session--assistant-response-reusable-p
                             (magent-thread-item-content item)))
                   (push (cons 'response
                               (if-let* ((native-id (magent-session--metadata-value
                                                    (magent-thread-item-metadata item)
                                                    :native-id)))
                                   (list (magent-thread-item-content item) :native-id native-id)
                                 (magent-thread-item-content item))) entries)))
                ('provider
                 (when (eq (magent-thread-item-status item) 'completed)
                   (push (cons 'provider
                               (magent-thread--alist-to-keyword-plist
                                (magent-thread-item-metadata item))) entries)))
                ('tool
                 (when (magent-thread-terminal-item-p item)
                   (push (cons 'tool (magent-session--tool-prompt-entry item))
                         entries)))))))
        (when (equal (magent-thread-turn-id turn) current)
          (setq stop t))))
    (nreverse entries)))

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
      ('provider
       (magent-session--provider-context-view session current-turn-id))
      ('compaction
       (cl-loop for entry in (magent-session--provider-context-view session current-turn-id)
                unless (eq (car entry) 'provider)
                collect (if (and (eq (car entry) 'response) (consp (cdr entry)))
                            (cons 'response (cadr entry)) entry)))
      ('audit
       (and thread
            `((snapshot . ,(magent-thread-snapshot-to-alist thread))
              (journal . ,(vconcat
                            (mapcar #'magent-thread-event-to-alist
                                    (magent-thread-journal thread))))))))))

(provide 'magent-session)
;;; magent-session.el ends here
