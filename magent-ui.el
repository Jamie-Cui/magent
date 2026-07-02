;;; magent-ui.el --- User interface for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; User interface for Magent with a read-only workspace buffer and an
;; independent compose buffer.  The workspace renders a compact projection of
;; the thread ledger; the durable ledger, not buffer text, is the source of
;; truth for restore and transcript views.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'spinner)
(require 'subr-x)
(require 'transient)
(require 'magent-approval)
(require 'magent-events)
(require 'magent-runtime)
(require 'magent-session)
(require 'magent-agent-job)
(require 'magent-agent)
(require 'magent-agent-registry)
(require 'magent-agent-loop)
(require 'magent-protocol)
(require 'magent-turn)

(defvar magent--spinner)
(defvar magent-compose-close-after-submit)
(defvar magent-compose-window-height)
(defvar magent-enable-logging)
(defvar magent-log-level)

(defvar magent--current-request-handle nil
  "Current active request handle, if any.")

;; Forward declaration for buffer-local scope state (defined in
;; "Compose input" section below).
(defvar magent-ui--buffer-scope nil)

(defvar-local magent-ui--fold-state nil
  "Hash table of persisted fragment fold state for the current workspace.")

(defvar magent-ui--section-depth 0
  "Dynamic indentation depth for Magent workspace sections.")

(defvar magent-ui-after-input-submit-hook nil
  "Hook run in the Magent buffer after successful in-buffer input submission.
Functions are called with no arguments before the prompt is dispatched.")

(defvar magent-dwim-hook nil
  "Hook run after `magent-dwim' displays the Magent buffer and positions point.")

(defvar magent-ui-region-active-functions nil
  "Special hook for extra region-active predicates.
Functions are called with no arguments by `magent-ui--capture-buffer-context'.
They should return non-nil only when `region-beginning' and `region-end'
are valid for the current buffer.")

;; Forward declarations for magent-skills (loaded lazily via require)
(declare-function magent-capability-capture-context "magent-capability")
(declare-function magent-explain-last-capability-resolution "magent-capability")
(declare-function magent-list-capabilities-for-current-context "magent-capability")
(declare-function magent-capability-resolution-summary "magent-capability")
(declare-function magent-capability-resolve-for-turn "magent-capability")
(declare-function magent-toggle-capability-locally "magent-capability")
(declare-function magent-skills-get "magent-skills")
(declare-function magent-skills-default-prompt "magent-skills")
(declare-function magent-skills-list "magent-skills")
(declare-function magent-skills-list-by-type "magent-skills")
(declare-function magent-list-skills "magent-skills")
(declare-function magent-describe-skill "magent-skills")
(declare-function magent-reload-skills "magent-skills")
(declare-function magent-skill-type "magent-skills")
(declare-function magent-skill-description "magent-skills")
(declare-function magent-show-audit "magent-audit")
(declare-function magent-session--session-for-scope "magent-session")
(declare-function magent-toggle-by-pass-permission "magent-permission")

;; Forward declaration for magent entry point (magent.el loaded first)
(declare-function magent--ensure-initialized "magent")

(defun magent-ui--abort-active-request (request)
  "Abort active REQUEST when it is a Magent-owned loop."
  (when (magent-agent-loop-p request)
    (magent-agent-loop-abort request)))

;;; Request dispatch

(cl-defstruct (magent-ui--request
               (:constructor magent-ui--request-create)
               (:copier nil))
  "A prompt being dispatched or about to be dispatched."
  (prompt nil :type string)
  (display nil :type (or string null))
  (source 'prompt :type symbol)
  (skills nil :type list)
  agent
  request-context
  capability-resolution
  (timestamp 0.0 :type float))

(defvar magent-ui--processing nil
  "Legacy UI processing flag.
Kept for compatibility with older tests and external callers.  The
active runtime state is owned by `magent-turn'.")

(defvar magent-ui--pending-skills-by-scope (make-hash-table :test #'equal)
  "One-shot instruction skills selected for the next request by scope.")

(defun magent-ui-processing-p ()
  "Return non-nil if a request is currently being processed."
  (or magent-ui--processing
      (magent-turn-processing-p)
      (magent-turn-pending-p)))

(defun magent-ui--enqueue
    (prompt source &optional display skills agent request-context capability-resolution)
  "Submit PROMPT to the runtime queue.
SOURCE is a symbol identifying the calling command.
DISPLAY is the text to show in the user message heading; defaults to PROMPT.
SKILLS is a list of explicit instruction skill names for this request.
AGENT is an optional `magent-agent-info' override for this request.
REQUEST-CONTEXT is an optional structured context plist captured from
the originating buffer.
CAPABILITY-RESOLUTION is an optional precomputed resolver result for
this turn.
Returns the submitted operation id."
  (let ((item (magent-ui--request-create
               :prompt prompt
               :display display
               :source source
               :skills skills
               :agent agent
               :request-context request-context
               :capability-resolution capability-resolution
               :timestamp (float-time))))
    (magent-turn-submit
     (magent-protocol-user-input-op item)
     item
     #'magent-ui--dispatch-submission)))

(defun magent-ui--clear-processing ()
  "Release the processing lock.
Called by `magent-ui--finish-processing' and `magent-interrupt'."
  (setq magent-ui--processing nil)
  (magent-ui--refresh-header-line (magent-session-current-scope)))

(defun magent-ui--dispatch (item)
  "Set the processing lock and hand ITEM off to the UI layer.
Must only be called when `magent-ui--processing' is nil.
Uses `run-at-time' to defer dispatch so callers finish their
stack frame before UI mutations happen."
  (setq magent-ui--processing t)
  (magent-ui--refresh-header-line (magent-session-current-scope))
  (run-at-time 0 nil #'magent-ui--run-item item))

(defun magent-ui--dispatch-submission (submission)
  "Dispatch a queued runtime SUBMISSION to the UI runner."
  (setq magent-ui--processing t)
  (magent-ui--refresh-header-line (magent-session-current-scope))
  (magent-ui--run-item (magent-turn-submission-payload submission)
                       (magent-turn-submission-id submission)))

(defvar magent-ui--request-generation 0
  "Monotonically increasing counter for request dispatch cycles.
Incremented on each new dispatch and on interrupt.  Callbacks
capture this value and compare on completion to detect and
discard stale callbacks from interrupted requests.")

;;; Buffer management

(defvar magent-log-buffer-name "*magent-log*"
  "Name of the buffer used for Magent logging.")

(defun magent-ui--buffer-name-base ()
  "Return the configured base name used for Magent output buffers."
  (let ((base (string-trim magent-buffer-name "\\`\\*+" "\\*+\\'")))
    (if (string-empty-p base) "magent" base)))

(defun magent-ui--scope-buffer-label (scope)
  "Return the display label used in a Magent buffer name for SCOPE."
  (if (eq scope 'global)
      "global"
    (let ((name (file-name-nondirectory (directory-file-name scope))))
      (if (string-empty-p name) "project" name))))

(defun magent-ui--buffer-has-scope-p (buffer scope)
  "Return non-nil when BUFFER is a Magent output buffer for SCOPE."
  (with-current-buffer buffer
    (and (derived-mode-p 'magent-output-mode)
         (equal magent-ui--buffer-scope scope))))

(defun magent-ui--scope-buffer-name (scope)
  "Return a non-conflicting buffer name for SCOPE."
  (let* ((base (magent-ui--buffer-name-base))
         (label (magent-ui--scope-buffer-label scope))
         (hash (substring (secure-hash 'sha1 (format "%s" scope)) 0 6))
         (attempt 0)
         name)
    (while
        (let* ((suffix (pcase attempt
                         (0 "")
                         (1 (format "#%s" hash))
                         (_ (format "#%s-%d" hash attempt))))
               (candidate (format "*%s:%s%s*" base label suffix))
               (existing (get-buffer candidate)))
          (setq name candidate)
          (and existing
               (not (magent-ui--buffer-has-scope-p existing scope))
               (progn
                 (cl-incf attempt)
                 t))))
    name))

(defun magent-ui--scope-buffer (&optional scope)
  "Return the Magent output buffer for SCOPE.
When SCOPE is nil, use the current session scope."
  (let* ((target-scope (or scope (magent-session-current-scope)))
         (buffer (get-buffer-create
                  (magent-ui--scope-buffer-name target-scope))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magent-output-mode)
        (magent-output-mode))
      (setq-local magent-ui--buffer-scope target-scope))
    buffer))

(defun magent-ui--compose-buffer-name (&optional scope)
  "Return the compose buffer name for SCOPE."
  (let* ((target-scope (or scope (magent-session-current-scope)))
         (base (magent-ui--buffer-name-base))
         (label (magent-ui--scope-buffer-label target-scope))
         (hash (substring (secure-hash 'sha1 (format "%s" target-scope)) 0 6)))
    (format "*%s-compose:%s#%s*" base label hash)))

(defun magent-ui-compose-buffer (&optional scope)
  "Return the Magent compose buffer for SCOPE."
  (let* ((target-scope (or scope (magent-session-current-scope)))
         (buffer (get-buffer-create
                  (magent-ui--compose-buffer-name target-scope))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magent-compose-mode)
        (magent-compose-mode))
      (setq-local magent-ui--buffer-scope target-scope))
    buffer))

(defun magent-ui--display-compose-buffer (buffer)
  "Display BUFFER as the Magent compose popup and select it."
  (let ((window
         (display-buffer
          buffer
          `((display-buffer-in-side-window)
            (side . bottom)
            (slot . 1)
            (window-height . ,magent-compose-window-height)))))
    (when (and (windowp window)
               (window-live-p window))
      (select-window window))
    window))

(defun magent-ui-open-compose (&optional scope initial-text)
  "Display the compose buffer for SCOPE and optionally append INITIAL-TEXT."
  (interactive)
  (let ((buffer (magent-ui-compose-buffer scope)))
    (when (and initial-text (not (string-empty-p initial-text)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (unless (or (bobp) (eq (char-before) ?\n))
          (insert "\n"))
        (insert initial-text)
        (unless (eq (char-before) ?\n)
          (insert "\n"))))
    (with-current-buffer buffer
      (goto-char (point-max)))
    (magent-ui--display-compose-buffer buffer)
    buffer))

(defun magent-ui--select-workspace (&optional scope)
  "Display and select the Magent workspace for SCOPE."
  (let ((window (magent-ui-display-buffer scope)))
    (when (and (windowp window)
               (window-live-p window))
      (select-window window))
    window))

(defun magent-ui--maybe-close-compose-window (buffer)
  "Close BUFFER's compose window when configured to do so."
  (when magent-compose-close-after-submit
    (when-let* ((window (get-buffer-window buffer t)))
      (when (and (windowp window)
                 (window-live-p window))
        (quit-window nil window)))))

(defun magent-compose-cancel ()
  "Clear the current Magent compose buffer."
  (interactive)
  (unless (derived-mode-p 'magent-compose-mode)
    (user-error "Not in a Magent compose buffer"))
  (erase-buffer)
  (message "Magent compose cleared"))

(defun magent-ui-get-log-buffer ()
  "Get or create the Magent log buffer."
  (let ((buffer (get-buffer-create magent-log-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magent-log-mode)
        (magent-log-mode)))
    buffer))

(defconst magent-ui--log-level-order
  '((debug . 10)
    (info . 20)
    (warn . 30)
    (error . 40))
  "Priority order for `magent-log' filtering.")

(defun magent-ui--log-message-level (message)
  "Return normalized severity symbol for log MESSAGE."
  (let ((prefix (and (string-match "\\`\\([A-Z]+\\)\\(?:\\s-\\|:\\|$\\)" message)
                     (match-string 1 message))))
    (pcase prefix
      ("DEBUG" 'debug)
      ((or "INFO" "PERM") 'info)
      ("WARN" 'warn)
      ("ERROR" 'error)
      (_ 'info))))

(defun magent-ui--loggable-message-p (message)
  "Return non-nil when MESSAGE should be written to `*magent-log*'."
  (and magent-enable-logging
       (>= (alist-get (magent-ui--log-message-level message)
                      magent-ui--log-level-order)
           (alist-get magent-log-level
                      magent-ui--log-level-order))))

(defmacro magent-ui--with-insert (buffer &rest body)
  "Execute BODY at end of BUFFER with `inhibit-read-only' set.
After BODY, marks the newly inserted region as read-only and
auto-scrolls if `magent-auto-scroll' is non-nil.
Buffer-boundary signals are suppressed because callbacks from
gptel process filters can trigger cursor adjustments from active
minor modes that hit buffer edges."
  (declare (indent 1))
  (let ((ro-start (make-symbol "ro-start")))
    `(with-current-buffer ,buffer
       (let ((inhibit-read-only t)
             (,ro-start (point-max)))
         (condition-case nil
             (progn
               (goto-char (point-max))
               ,@body
               (when (> (point-max) ,ro-start)
                 (add-text-properties ,ro-start (point-max) '(read-only t)))
               (when magent-auto-scroll
                 (goto-char (point-max))
                 (let ((win (get-buffer-window (current-buffer))))
                   (when win
                     (set-window-point win (point-max))))))
           ((beginning-of-buffer end-of-buffer beginning-of-line end-of-line)
            nil))))))

(defun magent-log (format-string &rest args)
  "Log a message to the Magent log buffer.
FORMAT-STRING and ARGS are passed to `format'.
Uses a simple insert rather than `magent-ui--with-insert' because
the log buffer already has `buffer-read-only' and should not get
per-character `read-only' text properties (they would travel with
yanked text)."
  (let ((message (apply #'format format-string args)))
    (when (magent-ui--loggable-message-p message)
      (let ((buf (magent-ui-get-log-buffer)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
              (insert (format "[%s] %s\n"
                              timestamp
                              message)))))))))

(defun magent-ui-get-buffer (&optional scope)
  "Get or create the Magent output buffer for SCOPE."
  (magent-ui--scope-buffer scope))

(defun magent-ui-display-buffer (&optional scope)
  "Display the Magent output buffer for SCOPE.
If the buffer is empty but the session has history, render all
past messages so the user can see the full conversation."
  (interactive)
  (display-buffer
   (magent-ui--ensure-scope-buffer-rendered
    (or scope
        (and (derived-mode-p 'magent-output-mode)
             (magent-ui--context-scope))))))

(defun magent-ui--snapshot-buffer-content (session &optional scope)
  "Stop persisting workspace text into SESSION.
The ledger is the restore source.  This function remains as a harmless
compatibility hook for older callers."
  (ignore scope)
  (when session
    (setf (magent-session-buffer-content session) nil))
  nil)

(defun magent-ui--context-scope ()
  "Return the session scope implied by the current command context."
  (if (or (derived-mode-p 'magent-output-mode)
          (derived-mode-p 'magent-compose-mode))
      (or magent-ui--buffer-scope
          (magent-session-current-scope))
    (magent-session-scope-from-directory default-directory)))

(defun magent-ui--ensure-scope-buffer-rendered (scope &optional force-render)
  "Return SCOPE's Magent buffer, rendering session history when needed."
  (let ((buffer (magent-ui-get-buffer scope)))
    (when (or force-render
              (zerop (buffer-size buffer)))
      (magent-ui-render-history t scope))
    buffer))

(defun magent-ui--activate-scope
    (scope &optional session force-render preserve-current-session)
  "Activate SCOPE with optional SESSION, then prepare its UI buffer."
  (cond
   (session
    (magent-session-install scope session))
   ((or (not preserve-current-session)
        (not (equal scope (magent-session-current-scope)))
        (null magent--current-session))
    (magent-session-activate scope)))
  (magent-runtime-activate-scope scope)
  (magent-ui--ensure-scope-buffer-rendered scope force-render)
  (magent-session-get))

(defun magent-ui--activate-context-session ()
  "Activate the session scope for the current command context.
When switching scopes, snapshot the outgoing buffer before activating the
new session.  Ensures the target scope has its own Magent buffer and
renders history into it on first use.  Returns the active session."
  (let ((target-scope (magent-ui--context-scope))
        (current-scope (magent-session-current-scope)))
    (unless (equal target-scope current-scope)
      (when (magent-ui-processing-p)
        (user-error "Magent: cannot switch project while a request is in progress"))
      (magent-ui--snapshot-buffer-content magent--current-session current-scope))
    (magent-ui--activate-scope target-scope nil nil t)))

(defun magent-ui--one-line (value &optional width)
  "Return VALUE as a single display line truncated to WIDTH."
  (truncate-string-to-width
   (replace-regexp-in-string "[ \t\n\r]+" " " (format "%s" (or value "")))
   (or width 96) nil nil "..."))

(defun magent-ui--dedupe-string-list (strings)
  "Return STRINGS without duplicates, preserving first occurrence."
  (let (result)
    (dolist (string strings)
      (when (and (stringp string)
                 (not (string-blank-p string))
                 (not (member string result)))
        (push string result)))
    (nreverse result)))

(defun magent-ui--scope-key (&optional scope)
  "Return the normalized pending-state key for SCOPE."
  (or scope (magent-session-current-scope)))

(defun magent-ui--pending-skills (&optional scope)
  "Return one-shot skills selected for SCOPE's next request."
  (copy-sequence
   (gethash (magent-ui--scope-key scope) magent-ui--pending-skills-by-scope)))

(defun magent-ui--set-pending-skills (scope skills)
  "Set SCOPE's one-shot pending SKILLS and refresh its workspace."
  (let ((key (magent-ui--scope-key scope))
        (normalized (magent-ui--dedupe-string-list skills)))
    (if normalized
        (puthash key normalized magent-ui--pending-skills-by-scope)
      (remhash key magent-ui--pending-skills-by-scope))
    (magent-ui--refresh-header-line key)
    normalized))

(defun magent-ui--clear-pending-skills (&optional scope)
  "Clear one-shot pending skills for SCOPE."
  (magent-ui--set-pending-skills scope nil))

(defun magent-ui--toggle-pending-skill (skill-name &optional scope)
  "Toggle SKILL-NAME for SCOPE's next request."
  (let* ((key (magent-ui--scope-key scope))
         (skills (magent-ui--pending-skills key))
         (enabled (member skill-name skills)))
    (magent-ui--set-pending-skills
     key
     (if enabled
         (remove skill-name skills)
       (append skills (list skill-name))))
    (not enabled)))

(defun magent-ui--header-skills-text (skills)
  "Return compact header-line text for pending SKILLS."
  (cond
   ((null skills) nil)
   ((<= (length skills) 3)
    (concat "skills: " (magent-ui--one-line
                        (mapconcat #'identity skills ", ") 72)))
   (t
    (format "skills: %d selected" (length skills)))))

(defun magent-ui--request-state-text ()
  "Return compact text for the current request queue state."
  (cond
   ((magent-turn-processing-p) "active")
   ((magent-turn-pending-p) "queued")
   (t "idle")))

(defun magent-ui--header-line ()
  "Return the Magent workspace header-line for the current buffer."
  (let* ((scope (magent-ui--scope-key magent-ui--buffer-scope))
         (session (magent-session--session-for-scope scope))
         (thread (and session (magent-session-thread-ledger session)))
         (agent (magent-session-agent session))
         (agent-name (if agent
                         (magent-agent-info-name agent)
                       magent-default-agent))
         (thread-state (or (and thread
                                (symbol-name (magent-thread-status thread)))
                           "idle"))
         (thread-id (or (and thread (magent-thread-id thread)) "none"))
         (request-state (magent-ui--request-state-text))
         (skills-text
          (magent-ui--header-skills-text
           (magent-ui--pending-skills scope))))
    (string-join
     (delq nil
           (list "Magent"
                 (format "scope: %s"
                         (magent-ui--one-line
                          (magent-ui--scope-buffer-label scope) 24))
                 (format "agent: %s" agent-name)
                 (format "thread: %s" thread-state)
                 (format "request: %s" request-state)
                 (format "queue: %s" (magent-turn-queue-length))
                 (format "session: %s" (magent-ui--one-line thread-id 36))
                 skills-text))
     "  ")))

(defun magent-ui--refresh-header-line (&optional scope)
  "Refresh the workspace header-line for SCOPE if its buffer exists."
  (let* ((key (magent-ui--scope-key scope))
         (buffer (get-buffer (magent-ui--scope-buffer-name key))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local header-line-format '(:eval (magent-ui--header-line)))
        (force-mode-line-update)))))

(defun magent-ui--item-text (item)
  "Return ITEM text content."
  (or (magent-thread-item-content item)
      (magent-thread-item-output item)
      ""))

(defun magent-ui--metadata-get (metadata key)
  "Return KEY from plist or alist METADATA."
  (cond
   ((plist-member metadata key)
    (plist-get metadata key))
   ((assq key metadata)
    (cdr (assq key metadata)))
   ((and (keywordp key)
         (assq (intern (substring (symbol-name key) 1)) metadata))
    (cdr (assq (intern (substring (symbol-name key) 1)) metadata)))
   ((and (keywordp key)
         (assoc (substring (symbol-name key) 1) metadata))
    (cdr (assoc (substring (symbol-name key) 1) metadata)))))

(defun magent-ui--arg-get (args key)
  "Return KEY from plist or alist ARGS."
  (magent-ui--metadata-get args key))

(defun magent-ui--apply-assistant-text-properties (start end)
  "Apply lightweight markdown text properties between START and END."
  (when (< start end)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^[ \t]*#+[ \t]+\\(.+\\)$" end t)
        (add-text-properties
         (match-beginning 1) (match-end 1)
         '(face font-lock-function-name-face)))
      (goto-char start)
      (while (re-search-forward "`\\([^`\n]+\\)`" end t)
        (add-text-properties
         (match-beginning 1) (match-end 1)
         '(face font-lock-constant-face)))
      (goto-char start)
      (while (re-search-forward "\\*\\*\\([^*\n]+\\)\\*\\*" end t)
        (add-text-properties
         (match-beginning 1) (match-end 1)
         '(face bold))))))

(defun magent-ui--insert-rendered-text (text)
  "Insert TEXT and apply lightweight assistant text properties."
  (let ((start (point)))
    (insert text)
    (magent-ui--apply-assistant-text-properties start (point))))

(defun magent-ui--insert-labelled-block (label text &optional face render-text)
  "Insert LABEL and TEXT into the current buffer."
  (insert (propertize label 'face (or face 'font-lock-keyword-face)) "\n")
  (when (and (stringp text) (not (string-empty-p text)))
    (if render-text
        (magent-ui--insert-rendered-text text)
      (insert text))
    (unless (eq (char-before) ?\n)
      (insert "\n"))))

(defun magent-ui--fold-state-table ()
  "Return the current buffer's fragment fold state table."
  (or (and (hash-table-p magent-ui--fold-state)
           magent-ui--fold-state)
      (setq-local magent-ui--fold-state
                  (make-hash-table :test #'equal))))

(defconst magent-ui--fold-state-missing
  (make-symbol "magent-ui-fold-state-missing")
  "Sentinel for missing fragment fold state.")

(defun magent-ui--fragment-folded-p (key default-folded)
  "Return non-nil when fragment KEY should render folded.
DEFAULT-FOLDED is used only when KEY has no persisted state."
  (let ((state (gethash key (magent-ui--fold-state-table)
                        magent-ui--fold-state-missing)))
    (cond
     ((eq state magent-ui--fold-state-missing)
      default-folded)
     ((eq state 'expanded)
      nil)
     ((or (eq state 'folded) state)
      t)
     (t nil))))

(defun magent-ui--apply-fold-overlay (start end)
  "Fold the fragment body from START to END."
  (when (< start end)
    (let ((ov (make-overlay start end)))
      (overlay-put ov 'invisible 'magent-ui)
      (overlay-put ov 'isearch-open-invisible #'delete-overlay)
      (overlay-put ov 'magent-ui-overlay t)
      (overlay-put ov 'magent-ui-fold t)
      ov)))

(defun magent-ui--fragment-marker (folded)
  "Return the Magit-like marker for FOLDED."
  (if folded "+" "-"))

(defun magent-ui--section-prefix ()
  "Return indentation prefix for the current section depth."
  (make-string (* 2 (max 0 magent-ui--section-depth)) ?\s))

(defun magent-ui--insert-workspace-prefix ()
  "Insert indentation for content at the current workspace section depth."
  (insert (magent-ui--section-prefix)))

(defun magent-ui--insert-workspace-line (text &optional face)
  "Insert TEXT as one workspace line at the current section depth."
  (magent-ui--insert-workspace-prefix)
  (insert (if face
              (propertize text 'face face)
            text)
          "\n"))

(defun magent-ui--update-fragment-marker (pos folded)
  "Update the fold marker visible at fragment POS to FOLDED."
  (let ((marker-pos (get-text-property pos 'magent-ui-fragment-marker-position)))
    (when (and marker-pos
               (<= (point-min) marker-pos)
               (< marker-pos (point-max)))
      (let ((inhibit-read-only t)
            (new-char (aref (magent-ui--fragment-marker folded) 0))
            (old-char (char-after marker-pos)))
        (when old-char
          (subst-char-in-region marker-pos (1+ marker-pos)
                                old-char new-char t))))))

(defun magent-ui--insert-fragment
    (title face body-fn &optional key default-folded magit-style)
  "Insert a foldable fragment with TITLE, FACE, and BODY-FN.
When MAGIT-STYLE is non-nil, prefix the header with a Magit-like
fold marker and indentation.  DEFAULT-FOLDED controls first render
state when KEY has no persisted fold state."
  (let* ((fragment-key (or key title))
         (folded (magent-ui--fragment-folded-p fragment-key default-folded))
         (header-start (point))
         (marker-position nil))
    (when magit-style
      (insert (magent-ui--section-prefix))
      (setq marker-position (point))
      (insert (magent-ui--fragment-marker folded) " "))
    (insert (propertize title 'face face) "\n")
    (let ((header-end (1- (point)))
          (body-start (point)))
      (let ((magent-ui--section-depth
             (if magit-style
                 (1+ magent-ui--section-depth)
               magent-ui--section-depth)))
        (funcall body-fn))
      (let ((body-end (point)))
        (add-text-properties
         header-start header-end
         (list 'magent-ui-fragment t
               'magent-ui-fragment-key fragment-key
               'magent-ui-fragment-body-start body-start
               'magent-ui-fragment-body-end body-end
               'magent-ui-fragment-marker-position marker-position))
        (if (< body-start body-end)
            (when folded
              (magent-ui--apply-fold-overlay body-start body-end))
          ;; Empty body: no overlay is created, so never leave a folded
          ;; marker that misrepresents a fold the user cannot toggle.
          (when (and magit-style folded)
            (magent-ui--update-fragment-marker header-start nil)))))))

(defun magent-ui--turn-title (turn)
  "Return workspace title for TURN."
  (format "Turn %s  [%s]"
          (magent-ui--one-line
           (or (magent-thread-turn-input turn)
               (magent-thread-turn-id turn))
           72)
          (symbol-name (magent-thread-turn-status turn))))

(defun magent-ui--turn-items (turn &optional type role)
  "Return TURN items filtered by TYPE and ROLE."
  (cl-remove-if-not
   (lambda (item)
     (and (or (null type)
              (eq (magent-thread-item-type item) type))
          (or (null role)
              (eq (magent-thread-item-role item) role))))
   (or (and turn (magent-thread-turn-items turn)) nil)))

(defun magent-ui--parse-context-fields (raw)
  "Parse auto-context RAW into an alist of string fields."
  (let ((pos 0)
        fields)
    (while (and (stringp raw)
                (string-match
                 "\\([[:alnum:]_-]+\\)=\\(\"\\([^\"]*\\)\"\\|[^[:space:]]+\\)"
                 raw pos))
      (let ((key (match-string 1 raw))
            (quoted-value (match-string 3 raw))
            (value (match-string 2 raw)))
        (push (cons key (or quoted-value value)) fields)
        (setq pos (match-end 0))))
    (nreverse fields)))

(defun magent-ui--split-user-context (text)
  "Split auto-context prefix from user TEXT for workspace display."
  (let ((body (or text ""))
        raw)
    (when (and (stringp text)
               (string-prefix-p "[Context: " text))
      (let* ((line-end (or (string-match "\n" text) (length text)))
             (first-line (substring text 0 line-end)))
        (when (string-match "\\`\\[Context: \\(.*\\)\\]\\'" first-line)
          (setq raw (match-string 1 first-line))
          (setq body (string-trim-left (substring text line-end))))))
    (list :body body
          :context-raw raw
          :context-fields (magent-ui--parse-context-fields raw))))

(defun magent-ui--item-content-string (item)
  "Return ITEM content as a display string, or nil."
  (let ((content (and item (magent-thread-item-content item))))
    (when content
      (format "%s" content))))

(defun magent-ui--turn-display-data (turn)
  "Return display data derived from TURN without mutating ledger state."
  (let* ((user-items (magent-ui--turn-items turn 'message 'user))
         (first-user (car user-items))
         (split (magent-ui--split-user-context
                 (magent-ui--item-content-string first-user)))
         (first-body (plist-get split :body))
         (rest-bodies (mapcar
                       #'magent-ui--item-content-string
                       (cdr user-items)))
         (prompt (string-trim-right
                  (string-join
                   (cl-remove-if
                    (lambda (value)
                      (or (null value)
                          (and (stringp value)
                               (string-empty-p value))))
                    (cons first-body rest-bodies))
                   "\n\n"))))
    (list :prompt prompt
          :context-raw (plist-get split :context-raw)
          :context-fields (plist-get split :context-fields)
          :assistant-items (magent-ui--turn-items turn 'message 'assistant)
          :tool-items (magent-ui--turn-items turn 'tool)
          :reasoning-items (magent-ui--turn-items turn 'reasoning))))

(defun magent-ui--turn-preview (turn)
  "Return a compact prompt preview for TURN.
Extracts only the first user message so the title path avoids the
full item scan performed by `magent-ui--turn-display-data'."
  (let* ((first-user (magent-thread-turn-message-item turn 'user))
         (split (magent-ui--split-user-context
                 (magent-ui--item-content-string first-user)))
         (display-prompt (plist-get split :body))
         (prompt (or (and (stringp display-prompt)
                          (not (string-empty-p display-prompt))
                          display-prompt)
                     (magent-thread-turn-input turn)
                     (magent-thread-turn-id turn))))
    (magent-ui--one-line prompt 58)))

(defun magent-ui--turn-status-label (status)
  "Return a fixed-width workspace label for turn STATUS."
  (pcase status
    ('queued "WAIT")
    ('in-progress "RUN ")
    ('completed "DONE")
    ('failed "FAIL")
    ('interrupted "STOP")
    ('dropped "DROP")
    (_ "????")))

(defun magent-ui--turn-start-time-label (turn)
  "Return TURN's `started-at' timestamp as HH:MM, or --:--."
  (let ((started-at (and turn (magent-thread-turn-started-at turn))))
    (if (numberp started-at)
        (condition-case nil
            (format-time-string "%H:%M" (seconds-to-time started-at))
          (error "--:--"))
      "--:--")))

(defun magent-ui--turn-section-title (turn index)
  "Return timeline section title for TURN at one-based INDEX."
  (let ((preview (magent-ui--turn-preview turn)))
    (format "[%s] Turn %d  %s%s%s"
            (magent-ui--turn-status-label
             (magent-thread-turn-status turn))
            index
            (magent-ui--turn-start-time-label turn)
            (or (magent-ui--turn-alert-summary turn) "")
            (if (string-empty-p preview)
                ""
              (format "  %s" preview)))))

(defun magent-ui--tool-approval-attention-p (item)
  "Return non-nil when ITEM approval metadata deserves header attention."
  (let* ((metadata (magent-thread-item-metadata item))
         (decision (magent-ui--metadata-get metadata :approval-decision))
         (name (and decision (format "%s" decision))))
    (member name '("ask" "deny" "denied"))))

(defun magent-ui--turn-alert-summary (turn)
  "Return a compact abnormal-state summary for TURN."
  (let* ((tools (magent-ui--turn-items turn 'tool))
         (failed (cl-count-if
                  (lambda (item)
                    (memq (magent-thread-item-status item)
                          '(failed cancelled)))
                  tools))
         (running (cl-count-if
                   (lambda (item)
                     (memq (magent-thread-item-status item)
                           '(pending in-progress)))
                   tools))
         (approvals (cl-count-if
                     #'magent-ui--tool-approval-attention-p
                     tools))
         (parts (delq
                 nil
                 (list
                  (when (magent-thread-turn-error turn) "error")
                  (when (> failed 0) (format "failed tools:%d" failed))
                  (when (> running 0) (format "running tools:%d" running))
                  (when (> approvals 0) (format "approval:%d" approvals))))))
    (when parts
      (format "  ! %s" (string-join parts ", ")))))

(defun magent-ui--insert-workspace-text (text &optional render-text face)
  "Insert TEXT as indented workspace content.
When RENDER-TEXT is non-nil, apply lightweight assistant text
properties to the inserted region.  FACE applies to the full region."
  (when (and (stringp text)
             (not (string-empty-p text)))
    (let ((start (point))
          (lines (split-string text "\n")))
      (while (and lines
                  (string-empty-p (car (last lines))))
        (setq lines (butlast lines)))
      (dolist (line lines)
        (magent-ui--insert-workspace-line line))
      (when face
        (add-text-properties start (point) (list 'face face)))
      (when render-text
        (magent-ui--apply-assistant-text-properties start (point))))))

(defun magent-ui--insert-context-details (fields raw)
  "Insert parsed context FIELDS, falling back to RAW."
  (magent-ui--insert-workspace-line "Context" 'font-lock-comment-face)
  (if fields
      (dolist (field fields)
        (magent-ui--insert-workspace-line
         (format "  %s: %s" (car field) (cdr field))
         'font-lock-comment-face))
    (when raw
      (magent-ui--insert-workspace-line
       (format "  context: %s" raw)
       'font-lock-comment-face))))

(defun magent-ui--insert-workspace-meta-section (turn data)
  "Insert folded Meta section for TURN using DATA."
  (let ((context-raw (plist-get data :context-raw))
        (context-fields (plist-get data :context-fields))
        (reasoning-items (plist-get data :reasoning-items))
        (error (magent-thread-turn-error turn)))
    (when (or context-raw reasoning-items error)
      (magent-ui--insert-fragment
       "Meta"
       'font-lock-comment-face
       (lambda ()
         (when context-raw
           (magent-ui--insert-context-details context-fields context-raw))
         (dolist (item reasoning-items)
           (magent-ui--insert-reasoning-item item))
         (when error
           (magent-ui--insert-workspace-line
            (format "Error: %s" error)
            'magent-error-body)))
       (format "turn:%s:meta" (magent-thread-turn-id turn))
       t
       t))))

(defun magent-ui--insert-workspace-tools-section (turn data)
  "Insert folded Tools section for TURN using DATA."
  (let ((tool-items (plist-get data :tool-items)))
    (when tool-items
      (magent-ui--insert-fragment
       (format "Tools (%d)" (length tool-items))
       'magent-tool-header
       (lambda ()
         (dolist (item tool-items)
           (magent-ui--insert-tool-item item)))
       (format "turn:%s:tools" (magent-thread-turn-id turn))
       t
       t))))

(defun magent-ui--insert-workspace-turn-body (turn)
  "Insert Magit-like workspace body for TURN."
  (let* ((data (magent-ui--turn-display-data turn))
         (prompt (plist-get data :prompt))
         (assistant-items (plist-get data :assistant-items)))
    (when (and prompt (not (string-empty-p prompt)))
      (magent-ui--insert-fragment
       "Prompt"
       'magent-user-header
       (lambda ()
         (magent-ui--insert-workspace-text prompt nil))
       (format "turn:%s:prompt" (magent-thread-turn-id turn))
       nil
       t))
    (cl-loop with multiple = (cdr assistant-items)
             for item in assistant-items
             for index from 1
             for content = (or (magent-thread-item-content item) "")
             unless (string-empty-p content)
             do
             (magent-ui--insert-fragment
              (if multiple
                  (format "Response %d" index)
                "Response")
              'magent-assistant-header
              (lambda ()
                (magent-ui--insert-workspace-text content t))
              (format "turn:%s:response:%s"
                      (magent-thread-turn-id turn)
                      (magent-thread-item-id item))
              nil
              t))
    (magent-ui--insert-workspace-tools-section turn data)
    (magent-ui--insert-workspace-meta-section turn data)))

(defun magent-ui--tool-result-preview (item)
  "Return a compact display string for tool ITEM."
  (let* ((output (or (magent-thread-item-output item)
                     (magent-thread-item-error item)
                     ""))
         (text (format "%s" output)))
    (if (> (length text) magent-ui-result-max-length)
        (format "[result %d chars; use transcript/detail view]" (length text))
      (magent-ui--one-line text magent-ui-result-max-length))))

(defconst magent-ui--tool-path-keys
  '(:path :file :filepath :filename :target-file :old-file :new-file)
  "Tool argument keys that should render as file links.")

(defun magent-ui--key-label (key)
  "Return a compact display label for KEY."
  (if (keywordp key)
      (substring (symbol-name key) 1)
    (format "%s" key)))

(defun magent-ui--keywordize-key (key)
  "Return KEY as a keyword symbol when possible."
  (cond
   ((keywordp key) key)
   ((symbolp key) (intern (concat ":" (symbol-name key))))
   ((stringp key) (intern (concat ":" key)))
   (t key)))

(defun magent-ui--line-number (value)
  "Return VALUE as a positive line number, or nil."
  (cond
   ((and (integerp value) (> value 0)) value)
   ((and (stringp value)
         (string-match-p "\\`[0-9]+\\'" value)
         (> (string-to-number value) 0))
    (string-to-number value))))

(defun magent-ui--tool-path-entries (args)
  "Return path entries from tool ARGS.
Each entry is `(KEY PATH LINE)'."
  (let ((line (magent-ui--line-number
               (or (magent-ui--arg-get args :line)
                   (magent-ui--arg-get args :line-number))))
        entries)
    (cond
     ((and (listp args)
           (keywordp (car args)))
      (let ((copy args))
        (while copy
          (let ((key (pop copy))
                (value (pop copy)))
            (when (and (memq key magent-ui--tool-path-keys)
                       (stringp value)
                       (not (string-empty-p value)))
              (push (list key value line) entries))))))
     ((listp args)
      (dolist (cell args)
        (let ((key (magent-ui--keywordize-key (car-safe cell)))
              (value (cdr-safe cell)))
          (when (and (memq key magent-ui--tool-path-keys)
                     (stringp value)
                     (not (string-empty-p value)))
            (push (list key value line) entries))))))
    (nreverse entries)))

(defun magent-ui--resolve-file-path (path)
  "Return PATH resolved relative to the current Magent scope."
  (if (file-name-absolute-p path)
      path
    (let ((scope (magent-ui--context-scope)))
      (expand-file-name
       path
       (cond
        ((stringp scope)
         (file-name-as-directory scope))
        ((stringp default-directory)
         default-directory)
        (t
         (file-name-as-directory (expand-file-name "."))))))))

(defun magent-ui--file-button-action (button)
  "Open the file represented by BUTTON."
  (let ((file (button-get button 'magent-ui-file))
        (line (button-get button 'magent-ui-line)))
    (find-file file)
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun magent-ui--insert-file-button (path &optional line)
  "Insert a clickable file PATH with optional LINE."
  (let ((file (magent-ui--resolve-file-path path)))
    (insert-text-button
     path
     'follow-link t
     'help-echo (if line
                    (format "Open %s:%s" file line)
                  (format "Open %s" file))
     'magent-ui-file file
     'magent-ui-line line
     'action #'magent-ui--file-button-action))
  (when line
    (insert (format ":%d" line))))

(defun magent-ui--insert-tool-path-details (item)
  "Insert file/path detail rows for tool ITEM."
  (dolist (entry (magent-ui--tool-path-entries
                  (magent-thread-item-input item)))
    (pcase-let ((`(,key ,path ,line) entry))
      (magent-ui--insert-workspace-prefix)
      (insert "  " (magent-ui--key-label key) ": ")
      (magent-ui--insert-file-button path line)
      (insert "\n"))))

(defun magent-ui--insert-tool-approval-detail (item)
  "Insert approval detail row for tool ITEM when present."
  (let* ((metadata (magent-thread-item-metadata item))
         (decision (magent-ui--metadata-get metadata :approval-decision))
         (source (magent-ui--metadata-get metadata :approval-source)))
    (when decision
      (magent-ui--insert-workspace-line
       (format "  approval: %s%s"
               decision
               (if source
                   (format " (%s)" source)
                 ""))))))

(defun magent-ui--grep-result-entries (text)
  "Return up to three `(PATH LINE)' entries parsed from TEXT."
  (let (entries)
    (when (stringp text)
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (and (< (length entries) 3)
                    (re-search-forward
                     "^\\([^:\n]+\\):\\([0-9]+\\):" nil t))
          (push (list (match-string 1)
                      (magent-ui--line-number (match-string 2)))
                entries))))
    (nreverse entries)))

(defun magent-ui--insert-tool-output-details (item)
  "Insert tool-specific output detail rows for ITEM."
  (let ((name (magent-thread-item-name item))
        (output (or (magent-thread-item-output item)
                    (magent-thread-item-error item))))
    (when (and output
               (member name '("grep" "glob")))
      (dolist (entry (magent-ui--grep-result-entries output))
        (pcase-let ((`(,path ,line) entry))
          (magent-ui--insert-workspace-prefix)
          (insert "  match: ")
          (magent-ui--insert-file-button path line)
          (insert "\n"))))))

(defun magent-ui--insert-tool-item (item)
  "Insert a compact tool ITEM row."
  (magent-ui--insert-workspace-line
   (format "Tool %s [%s] %s"
           (or (magent-thread-item-name item) "tool")
           (symbol-name (magent-thread-item-status item))
           (magent-ui--one-line
            (magent-thread-item-input item)
            magent-ui-tool-input-max-length))
   'magent-tool-header)
  (magent-ui--insert-tool-path-details item)
  (magent-ui--insert-tool-approval-detail item)
  (magent-ui--insert-tool-output-details item)
  (when (magent-thread-terminal-item-p item)
    (magent-ui--insert-workspace-line
     (concat "  -> " (magent-ui--tool-result-preview item))
     'magent-tool-result)))

(defun magent-ui--insert-reasoning-item (item)
  "Insert a compact reasoning ITEM row without exposing chain text."
  (let* ((text (or (magent-thread-item-content item) ""))
         (visibility (magent-ui--metadata-get
                      (magent-thread-item-metadata item)
                      :include-reasoning)))
    (magent-ui--insert-workspace-line
     (format "Reasoning [%s] %d chars%s"
             (symbol-name (magent-thread-item-status item))
             (length text)
             (if (eq visibility 'ignore) " hidden" ""))
     'magent-reasoning-header)))

(defun magent-ui--insert-turn-body (turn)
  "Insert workspace body for TURN."
  (dolist (item (magent-thread-turn-items turn))
    (pcase (magent-thread-item-type item)
      ('message
       (pcase (magent-thread-item-role item)
         ('user
          (magent-ui--insert-labelled-block
           "User" (magent-thread-item-content item) 'magent-user-header))
         ('assistant
          (magent-ui--insert-labelled-block
           (format "Assistant [%s]"
                   (symbol-name (magent-thread-item-status item)))
           (magent-thread-item-content item)
           'magent-assistant-header
           t))))
      ('tool
       (magent-ui--insert-tool-item item))
      ('reasoning
       (magent-ui--insert-reasoning-item item))))
  (when (magent-thread-turn-error turn)
    (insert (propertize
             (format "Error: %s\n" (magent-thread-turn-error turn))
             'face 'magent-error-body)))
  (insert "\n"))

(defun magent-ui--workspace-turns (thread)
  "Return non-dropped turns to show for THREAD in chronological order."
  (cl-remove-if
   (lambda (turn)
     (eq (magent-thread-turn-status turn) 'dropped))
   (or (and thread (magent-thread-turns thread)) nil)))

(defun magent-ui-render-history (&optional _skip-snapshot scope)
  "Render the chronological ledger workspace for SCOPE."
  (interactive)
  (let* ((target-scope (or scope
                           (and (or (derived-mode-p 'magent-output-mode)
                                    (derived-mode-p 'magent-compose-mode))
                                (magent-ui--context-scope))
                           (magent-session-current-scope)))
         (session (magent-session--session-for-scope target-scope))
         (thread (and session (magent-session-thread-ledger session)))
         (buf (magent-ui-get-buffer target-scope)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (remove-overlays (point-min) (point-max) 'magent-ui-overlay t)
        (erase-buffer)
        (setq-local buffer-invisibility-spec '(magent-ui))
        (let ((turns (magent-ui--workspace-turns thread))
              (magent-ui--section-depth 0))
          (if turns
              (let ((latest-index (length turns)))
                (cl-loop for turn in turns
                         for index from 1
                         do
                         (magent-ui--insert-fragment
                          (magent-ui--turn-section-title turn index)
                          'font-lock-function-name-face
                          (lambda ()
                            (magent-ui--insert-workspace-turn-body turn))
                          (format "turn:%s"
                                  (magent-thread-turn-id turn))
                          (/= index latest-index)
                          t)))
            (insert "No turns yet. Use `C-c m p` to compose a prompt.\n")))
        (add-text-properties (point-min) (point-max) '(read-only t))
        (goto-char (point-min))))))

(defun magent-ui-clear-buffer (&optional scope)
  "Clear the Magent output buffer for SCOPE."
  (interactive)
  (with-current-buffer (magent-ui-get-buffer scope)
    (let ((inhibit-read-only t))
      (remove-overlays (point-min) (point-max) 'magent-ui-overlay t)
      (erase-buffer))))

;;; Interrupt command

(defun magent-interrupt ()
  "Interrupt the current request.
Bumps the generation counter so stale callbacks from the aborted
request are discarded."
  (interactive)
  (let ((turn-request-handle (magent-turn-current-request-handle)))
    (magent-turn-interrupt #'magent-ui--abort-active-request)
    (when magent--current-request-handle
      (unless (eq magent--current-request-handle turn-request-handle)
        (magent-ui--abort-active-request magent--current-request-handle))
      (setq magent--current-request-handle nil)))
  (magent-approval-drop-requests)
  (cl-incf magent-ui--request-generation)
  (when (and (boundp 'magent--spinner) magent--spinner)
    (spinner-stop magent--spinner))
  (magent-ui-insert-status-line "[Interrupted by user]")
  (magent-log "INFO Request interrupted by user (gen now %d)"
              magent-ui--request-generation)
  (magent-ui--clear-processing)
  (magent-ui--maybe-show-input-prompt))

(defun magent-ui-submit-or-interrupt ()
  "Open compose when idle, or confirm interrupt when a request is running."
  (interactive)
  (if (magent-ui-processing-p)
      (when (y-or-n-p "Interrupt current Magent request? ")
        (magent-interrupt))
    (magent-ui-open-compose (magent-ui--context-scope))))

(defun magent-ui-compose-from-output ()
  "Open the compose buffer for the current Magent output scope."
  (interactive)
  (magent-ui-open-compose (magent-ui--context-scope)))

;;; Transient menu

(defun magent-ui--transient-source-buffer ()
  "Return the buffer that opened the current transient, when available."
  (when (boundp 'transient--source-buffer)
    (let ((buffer (symbol-value 'transient--source-buffer)))
      (and (buffer-live-p buffer) buffer))))

(defun magent-ui--call-in-command-context (function)
  "Call FUNCTION from the transient source buffer when there is one."
  (if-let* ((buffer (magent-ui--transient-source-buffer)))
      (with-current-buffer buffer
        (funcall function))
    (funcall function)))

(defun magent-ui--activate-command-context ()
  "Activate and return the scope for a UI command invocation."
  (magent-ui--call-in-command-context
   (lambda ()
     (magent-ui--activate-context-session)
     (magent-ui--context-scope))))

(defun magent-ui--dispatch-from-command-context
    (prompt source display skills agent)
  "Dispatch PROMPT using the original UI command context."
  (magent-ui--call-in-command-context
   (lambda ()
     (magent-ui-dispatch-prompt prompt source display skills t agent))))

(defun magent-ui-open-compose-command ()
  "Open compose for the current command context."
  (interactive)
  (magent-ui-open-compose (magent-ui--activate-command-context)))

(defun magent-ui-clear-session-command ()
  "Clear the session for the current command context."
  (interactive)
  (magent-ui--call-in-command-context #'magent-clear-session))

(defun magent-ui--instruction-skill-names ()
  "Return sorted instruction skill names."
  (sort (copy-sequence (magent-skills-list-by-type 'instruction)) #'string<))

(defun magent-ui--command-skill-names ()
  "Return sorted instruction skill names that have a default prompt."
  (cl-remove-if-not #'magent-skills-default-prompt
                    (magent-ui--instruction-skill-names)))

(defun magent-ui--read-instruction-skill (prompt)
  "Read an instruction skill name with PROMPT."
  (let ((names (magent-ui--instruction-skill-names)))
    (unless names
      (user-error "Magent: no instruction skills are registered"))
    (completing-read prompt names nil t)))

(defun magent-ui-toggle-skill-for-next-request (&optional skill-name)
  "Toggle an instruction skill for the current scope's next request."
  (interactive)
  (magent--ensure-initialized)
  (let* ((scope (magent-ui--activate-command-context))
         (name (or skill-name
                   (magent-ui--read-instruction-skill
                    "Toggle skill for next request: "))))
    (unless (member name (magent-ui--instruction-skill-names))
      (user-error "Magent: '%s' is not an instruction skill" name))
    (message "Magent: skill %s %s for next request"
             name
             (if (magent-ui--toggle-pending-skill name scope)
                 "selected"
               "cleared"))))

(defun magent-ui-clear-skills-for-next-request ()
  "Clear pending instruction skills for the current scope's next request."
  (interactive)
  (let ((scope (magent-ui--activate-command-context)))
    (magent-ui--clear-pending-skills scope)
    (message "Magent: selected skills cleared")))

(defun magent-ui--skill-command-text (skill-name extra-instruction)
  "Return default prompt text for SKILL-NAME plus EXTRA-INSTRUCTION."
  (let ((prompt (magent-skills-default-prompt skill-name))
        (extra (string-trim (or extra-instruction ""))))
    (unless prompt
      (user-error "Magent: skill '%s' has no default prompt" skill-name))
    (if (string-blank-p extra)
        prompt
      (concat prompt "\n\nAdditional instruction:\n" extra))))

(defun magent-ui-run-skill-command (&optional skill-name extra-instruction)
  "Run a command-like skill with its default prompt.
When EXTRA-INSTRUCTION is non-nil, append it to the default prompt."
  (interactive)
  (magent--ensure-initialized)
  (let* ((scope (magent-ui--activate-command-context))
         (name (or skill-name
                   (let ((names (magent-ui--command-skill-names)))
                     (unless names
                       (user-error "Magent: no command-like skills are registered"))
                     (completing-read "Run skill command: " names nil t))))
         (extra (if extra-instruction
                    extra-instruction
                  (read-string
                   (format "Extra instruction for %s (optional): " name))))
         (text (magent-ui--skill-command-text name extra))
         (skills (magent-ui--dedupe-string-list
                  (append (magent-ui--pending-skills scope)
                          (list name)))))
    (magent-ui--dispatch-from-command-context text 'skill-command nil skills nil)
    (magent-ui--clear-pending-skills scope)))

(defun magent-ui-run-init-command (&optional extra-instruction)
  "Run the built-in init skill command."
  (interactive)
  (magent-ui-run-skill-command "init" extra-instruction))

(defun magent-transient-menu--assign-agent-keys (agents)
  "Return alist of (KEY . AGENT-INFO) for AGENTS with unique single-char keys.
Skips keys already reserved by `magent-transient-agent-menu'."
  (let ((used (make-hash-table :test #'equal))
        result)
    ;; Reserve keys used by static agent menu entries.
    (dolist (k '("A" "m" "v"))
      (puthash k t used))
    (dolist (agent agents)
      (let* ((name (magent-agent-info-name agent))
             (key (cl-loop for ch across name
                           for k = (string ch)
                           unless (gethash k used)
                           return k
                           finally return nil)))
        (unless key
          (let ((i 1))
            (while (gethash (number-to-string i) used)
              (cl-incf i))
            (setq key (number-to-string i))))
        (puthash key t used)
        (push (cons key agent) result)))
    (nreverse result)))

(defun magent-transient-menu--agent-suffixes (_)
  "Return transient suffix objects for all primary agents."
  (magent--ensure-initialized)
  (mapcar (lambda (pair)
            (let* ((key   (car pair))
                   (agent (cdr pair))
                   (name  (magent-agent-info-name agent)))
              (transient-parse-suffix
               'magent-transient-agent-menu
               (list key name
                     (let ((agent-name name))
                       (lambda ()
                         (interactive)
                         (let* ((scope   (magent-ui--activate-command-context))
                                (info    (magent-agent-registry-get agent-name))
                                (session (magent-session-get)))
                           (magent-session-set-agent session info)
                           (magent-ui--refresh-header-line scope)
                           (magent-log "INFO agent selected: %s" agent-name))))))))
          (magent-transient-menu--assign-agent-keys
           (magent-agent-registry-primary-agents))))

(transient-define-prefix magent-transient-agent-menu ()
  "Magent agent menu."
  ["Agent"
   [("A" "select" magent-select-agent)
    ("m" "current" magent-show-current-agent)
    ("v" "list" magent-list-agents)]]
  ["Primary agents"
   [:class transient-column
           :setup-children magent-transient-menu--agent-suffixes]])

(transient-define-prefix magent-transient-skill-menu ()
  "Magent skill menu."
  ["Next Request"
   [("s" "toggle skill" magent-ui-toggle-skill-for-next-request)
    ("K" "clear selected" magent-ui-clear-skills-for-next-request)]]
  ["Run"
   [("i" "init project" magent-ui-run-init-command)
    ("!" "skill command" magent-ui-run-skill-command)]]
  ["Inspect"
   [("Y" "list" magent-list-skills)
    ("H" "describe" magent-describe-skill)
    ("U" "reload" magent-reload-skills)]])

(transient-define-prefix magent-transient-capability-menu ()
  "Magent capability menu."
  ["Capabilities"
   [("x" "current context" magent-list-capabilities-for-current-context)
    ("e" "last resolution" magent-explain-last-capability-resolution)
    ("k" "toggle local" magent-toggle-capability-locally)]])

(transient-define-prefix magent-transient-session-menu ()
  "Magent session menu."
  ["Session"
   [("c" "clear" magent-ui-clear-session-command)
    ("R" "resume" magent-resume-session)]]
  ["Transcripts"
   [("T" "parent transcript" magent-show-transcript)
    ("j" "child agent" magent-show-agent-transcript)]])

(transient-define-prefix magent-transient-log-menu ()
  "Magent log menu."
  ["Logs"
   [("l" "show log" magent-show-log)
    ("a" "show audit" magent-show-audit)
    ("L" "clear log" magent-clear-log)]])

(transient-define-prefix magent-transient-health-menu ()
  "Magent health menu."
  ["Diagnostics"
   [("d" "diagnose Emacs" magent-diagnose-emacs)
    ("D" "Magent doctor" magent-doctor)]])

(transient-define-prefix magent-transient-buffer-menu ()
  "Magent buffer menu."
  ["Buffer"
   [("r" "toggle read-only" magent-toggle-read-only)
    ("P" "permission bypass" magent-toggle-by-pass-permission)]])

(transient-define-prefix magent-transient-menu ()
  "Magent command menu."
  ["Request"
   [("o" "Open compose" magent-ui-open-compose-command)
    ("i" "Init project" magent-ui-run-init-command)
    ("!" "Run skill command" magent-ui-run-skill-command)]]
  ["Context"
   [("A" "Agent..." magent-transient-agent-menu)
    ("s" "Skills..." magent-transient-skill-menu)
    ("x" "Capabilities..." magent-transient-capability-menu)]]
  ["Session"
   [("c" "Clear session" magent-ui-clear-session-command)
    ("S" "Session..." magent-transient-session-menu)]]
  ["More"
   [("l" "Logs..." magent-transient-log-menu)
    ("h" "Health..." magent-transient-health-menu)
    ("b" "Buffer..." magent-transient-buffer-menu)]])

;;; Output mode

(defun magent-ui-menu-or-insert-question-mark ()
  "Open the Magent menu."
  (interactive)
  (call-interactively #'magent-transient-menu))

(defvar magent-output-mode-map (make-sparse-keymap)
  "Keymap for `magent-output-mode'.")

(defun magent-ui--setup-output-mode-map ()
  "Install Magent output mode bindings.
This is deliberately repeatable so reloading `magent-ui' updates an
already-created `magent-output-mode-map'."
  (define-key magent-output-mode-map (kbd "?")
              #'magent-ui-menu-or-insert-question-mark)
  (define-key magent-output-mode-map (kbd "C-g") nil)
  (define-key magent-output-mode-map (kbd "C-c C-c")
              #'magent-ui-submit-or-interrupt)
  (define-key magent-output-mode-map (kbd "C-c C-o")
              #'magent-ui-compose-from-output)
  (define-key magent-output-mode-map (kbd "TAB")
              #'magent-ui-toggle-section)
  (define-key magent-output-mode-map (kbd "<tab>")
              #'magent-ui-toggle-section)
  (define-key magent-output-mode-map (kbd "g")
              #'magent-ui-render-history))

(magent-ui--setup-output-mode-map)

(define-derived-mode magent-output-mode special-mode "Magent"
  "Major mode for Magent output.
The workspace is read-only and renders a compact ledger projection.
\\<magent-output-mode-map>
Press \\[magent-ui-submit-or-interrupt] to compose, or interrupt with confirmation.
Press \\[magent-ui-compose-from-output] to open compose directly.
Press \\[magent-ui-menu-or-insert-question-mark] for the command menu."
  (visual-line-mode 1)
  (setq-local magent-ui--buffer-scope
              (or magent-ui--buffer-scope
                  (magent-session-current-scope)))
  (setq-local header-line-format '(:eval (magent-ui--header-line)))
  (setq-local display-fill-column-indicator-column nil)
  (setq-local revert-buffer-function #'magent-ui--revert-buffer)
  (add-hook 'kill-buffer-hook #'magent-ui--cancel-timers nil t))

(defvar magent-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'magent-input-submit)
    (define-key map (kbd "C-c C-k") #'magent-compose-cancel)
    map)
  "Keymap for `magent-compose-mode'.")

(define-derived-mode magent-compose-mode text-mode "MagentCompose"
  "Major mode for composing Magent prompts."
  (setq-local magent-ui--buffer-scope
              (or magent-ui--buffer-scope
                  (magent-session-current-scope))))

(defun magent-ui--revert-buffer (_ignore-auto _noconfirm)
  "Revert the Magent buffer by re-rendering from the ledger."
  (magent-ui-render-history))

(defun magent-ui--cancel-timers ()
  "Cancel any pending streaming timers for the current buffer.
Called via `kill-buffer-hook' to prevent timers firing on dead buffers."
  (when (and (boundp 'magent-ui--streaming-batch-timer)
             magent-ui--streaming-batch-timer)
    (cancel-timer magent-ui--streaming-batch-timer)
    (setq magent-ui--streaming-batch-timer nil)))

(define-derived-mode magent-log-mode fundamental-mode "MagentLog"
  "Major mode for Magent log buffer."
  (setq buffer-read-only t)
  (visual-line-mode 1)
  (setq-local display-fill-column-indicator-column nil)
  (setq-local font-lock-defaults
              '((
                 ("\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)\\]"
                  0 font-lock-comment-face)
                 ("\\<\\(ERROR\\|WARNING\\|INFO\\|DEBUG\\)\\>"
                  0 font-lock-keyword-face)))))

;;; Compose input

(defvar-local magent-ui--buffer-scope nil
  "Scope associated with the current Magent output buffer.")

(defun magent-ui--maybe-show-input-prompt (&optional scope)
  "Ensure SCOPE's compose buffer exists.
This does not display it; workspace rendering stays read-only."
  (magent-ui-compose-buffer scope))

(defun magent-input-submit ()
  "Submit the current compose buffer and send it to the agent."
  (interactive)
  (unless (derived-mode-p 'magent-compose-mode)
    (magent-ui-open-compose (magent-ui--context-scope))
    (user-error "Submit from the Magent compose buffer"))
  (magent--ensure-initialized)
  (magent-ui--activate-context-session)
  (let* ((scope (magent-ui--context-scope))
         (raw (string-trim
               (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-blank-p raw)
      (user-error "Empty input"))
    (let ((compose-buffer (current-buffer))
          (skill-names (magent-ui--pending-skills scope)))
      (erase-buffer)
      (display-buffer (magent-ui-get-buffer scope))
      (run-hooks 'magent-ui-after-input-submit-hook)
      (when magent-compose-close-after-submit
        (magent-ui--maybe-close-compose-window compose-buffer)
        (magent-ui--select-workspace scope))
      (magent-ui-process raw 'compose nil skill-names)
      (magent-ui--clear-pending-skills scope))))

;;; Section folding

(defun magent-ui-toggle-section ()
  "Toggle folding of the section at point in the Magent output buffer."
  (interactive)
  (with-current-buffer (magent-ui-get-buffer (magent-ui--context-scope))
    (let ((pos (save-excursion
                 (beginning-of-line)
                 (point)))
          start end key header-pos)
      (save-excursion
        (beginning-of-line)
        (while (and (not (bobp))
                    (not (get-text-property (point) 'magent-ui-fragment)))
          (forward-line -1))
        (setq header-pos (point))
        (setq start (get-text-property (point)
                                       'magent-ui-fragment-body-start)
              end (get-text-property (point)
                                     'magent-ui-fragment-body-end)
              key (get-text-property (point)
                                     'magent-ui-fragment-key)))
      (unless (get-text-property header-pos 'magent-ui-fragment)
        (user-error "No Magent fragment at point"))
      ;; A fragment header with an empty body (e.g. a turn or response
      ;; that produced no renderable text) has nothing to fold; treat
      ;; toggling as a no-op rather than signalling an error.
      (when (and start end (< start end))
      (let* ((folds (cl-remove-if-not
                     (lambda (ov)
                       (and (overlay-get ov 'magent-ui-fold)
                            (= (overlay-start ov) start)
                            (= (overlay-end ov) end)))
                     (overlays-in start end)))
             (table (magent-ui--fold-state-table)))
        (if folds
            (progn
              (mapc #'delete-overlay folds)
              (when key
                (puthash key 'expanded table))
              (magent-ui--update-fragment-marker header-pos nil))
          (magent-ui--apply-fold-overlay start end)
          (when key
            (puthash key 'folded table))
          (magent-ui--update-fragment-marker header-pos t))))
      (goto-char pos))))

;;; Rendering functions

(defun magent-ui--insert-full-width-line (char face)
  "Insert CHAR with an overlay stretching to the right window edge using FACE.
The overlay's `display' property `(space :align-to right)' makes the
single character visually fill the rest of the line."
  (let ((start (point)))
    (insert (make-string 1 char))
    (let ((ov (make-overlay start (point))))
      (overlay-put ov 'display '(space :align-to right))
      (overlay-put ov 'face face)
      (overlay-put ov 'magent-ui-overlay t))))

(defun magent-ui--insert-separator ()
  "Insert a separator line between conversation turns.
For graphic characters (e.g. ?─), draws a full-width line using an overlay.
For whitespace characters (e.g. ?\\n or ?\\s), inserts a single blank line.
Skipped at buffer start or when `magent-ui-separator-char' is nil."
  (when (and magent-ui-separator-char (> (point) (point-min)))
    (if (= (char-syntax magent-ui-separator-char) ?\s)
        ;; Whitespace separator: ensure exactly one blank line before
        ;; the next heading.  The previous content already ends with
        ;; a newline, so one more newline produces the blank line.
        (insert "\n")
      (insert "\n")
      (magent-ui--insert-full-width-line magent-ui-separator-char 'magent-separator)
      (insert "\n"))))

(defun magent-ui--insert-header (label &optional face)
  "Insert LABEL followed by a dash line extending to window edge.
FACE, when non-nil, is applied to the label text via an overlay.
The trailing line uses the `magent-strike-through' face.  Overlays are
used for consistent visual treatment in special buffers."
  (let ((heading-start (point)))
    (insert label " ")
    (when face
      (let ((ov (make-overlay heading-start
                              (+ heading-start (length label)))))
        (overlay-put ov 'face face)
        (overlay-put ov 'magent-ui-overlay t))))
  (when magent-ui-header-strike-through
    (magent-ui--insert-full-width-line ?\s 'magent-strike-through))
  (insert "\n"))

(defun magent-ui-insert-user-message (text)
  "Insert user message TEXT into the workspace buffer."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (magent-ui--insert-separator)
    (magent-ui--insert-header magent-user-prompt 'magent-user-header)
    (insert text "\n")))

(defun magent-ui-insert-assistant-message (text)
  "Insert assistant message TEXT into the workspace buffer."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (magent-ui--insert-separator)
    (magent-ui--insert-header magent-assistant-prompt 'magent-assistant-header)
    (insert text)
    (unless (string-suffix-p "\n" text)
      (insert "\n"))))

(defun magent-ui--fold-block-at (pos block-re)
  "Compatibility no-op for the old org block folder.
POS and BLOCK-RE are ignored."
  (ignore pos block-re))

(defvar-local magent-ui--tool-call-start nil
  "Buffer position where the current tool row was inserted.")

(defun magent-ui--sanitize-tool-text (text)
  "Collapse newlines in TEXT for compact tool rows."
  (replace-regexp-in-string "\n" "\\\\n" text))

(defun magent-ui-insert-tool-call (tool-name input)
  "Insert a compact tool call notification into the workspace."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (setq magent-ui--tool-call-start (point))
    (insert (propertize (format "Tool %s running" tool-name)
                        'face 'magent-tool-header)
            "\n")
    (let ((input-str (magent-ui--sanitize-tool-text
                      (if (stringp input)
                          input
                        (truncate-string-to-width
                         (magent-json-encode input)
                         magent-ui-tool-input-max-length nil nil "...")))))
      (insert (propertize input-str 'face 'magent-tool-args) "\n"))))

(defun magent-ui-insert-tool-result (_tool-name result)
  "Insert tool RESULT for _TOOL-NAME into the workspace."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (let ((result-str (magent-ui--sanitize-tool-text
                       (truncate-string-to-width
                        (if (stringp result) result (format "%s" result))
                        magent-ui-result-max-length nil nil "..."))))
      (insert (propertize (concat "-> " result-str) 'face 'magent-tool-result)
              "\n"))
    (setq magent-ui--tool-call-start nil)))

(defun magent-ui--agent-job-line (label value)
  "Return a display line for child-agent LABEL and VALUE."
  (when (and value
             (not (and (stringp value)
                       (string-empty-p value))))
    (format "%s: %s"
            label
            (magent-ui--sanitize-tool-text
             (truncate-string-to-width
              (format "%s" value)
              magent-ui-result-max-length nil nil "...")))))

(defun magent-ui--agent-job-status-face (status)
  "Return a face for child-agent STATUS."
  (if (memq status '(failed cancelled closed))
      'magent-error-body
    'magent-tool-result))

(defun magent-ui-insert-agent-job-event (event job &optional detail scope)
  "Insert a compact child-agent lifecycle EVENT for JOB.
DETAIL is an optional short display string.  Full transcript content is
kept in session state and shown by `magent-show-agent-transcript'.
When SCOPE is non-nil, insert into that scoped Magent buffer."
  (when (magent-agent-job-p job)
    (magent-ui--with-insert (magent-ui-get-buffer scope)
      (let* ((status (magent-agent-job-status job))
             (status-name (symbol-name status))
             (event-name (symbol-name event))
             (start (point)))
        (insert (propertize
                 (format "Agent %s %s"
                         event-name
                         (magent-agent-job-id job))
                 'face 'magent-tool-header)
                "\n")
        (dolist (line (delq nil
                            (list
                             (magent-ui--agent-job-line
                              "agent"
                              (magent-agent-job-agent-name job))
                             (magent-ui--agent-job-line
                              "task"
                              (magent-agent-job-task-name job))
                             (magent-ui--agent-job-line
                              "status"
                              status-name)
                             (magent-ui--agent-job-line "detail" detail))))
          (insert (propertize line
                              'face (if (string-prefix-p "status:" line)
                                        (magent-ui--agent-job-status-face status)
                                      'magent-tool-args))
                  "\n"))
        (ignore start)))))

(defun magent-ui-insert-error (error-text)
  "Insert ERROR-TEXT into output buffer with level-1 heading."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (magent-ui--insert-header magent-error-prompt 'magent-error-header)
    (insert (propertize error-text 'face 'magent-error-body))
    (insert "\n")))

(defun magent-ui-insert-status-line (text &optional face)
  "Insert TEXT as a plain status line without creating a heading.
FACE defaults to `magent-error-body'."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (insert (propertize text 'face (or face 'magent-error-body)))
    (insert "\n")))

(defun magent-ui-insert-capability-summary (summary)
  "Insert capability SUMMARY into the output buffer."
  (when (and summary (not (string-empty-p summary)))
    (magent-ui--with-insert (magent-ui-get-buffer)
      (insert (propertize
               (format "Capability resolver: %s\n" summary)
               'face 'font-lock-comment-face)))))

;;; Streaming support

(defvar-local magent-ui--streaming-start nil
  "Buffer position where the current streaming AI text begins.
Set by `magent-ui-start-streaming', used by
`magent-ui-finish-streaming' to handle empty streaming rounds.")

(defvar-local magent-ui--streaming-section-start nil
  "Buffer position where the current streaming section begins.
Used to delete the empty heading when no text was streamed.")

(defvar-local magent-ui--streaming-has-text nil
  "Non-nil if any text was inserted via `magent-ui-insert-streaming'.
Used to distinguish empty streaming rounds from rounds where tool
lines were inserted after the header by other functions.")

(defvar-local magent-ui--response-body-start nil
  "Buffer position where the assistant response body begins.
Set once by `magent-ui-start-streaming', consumed by
`magent-ui-finish-streaming-fontify' for markdown→org conversion.
Not reset between tool-use rounds.")

(defvar-local magent-ui--streaming-batch-buffer ""
  "Accumulated text chunks waiting to be inserted.
Used for batching small streaming chunks to reduce UI updates.")

(defvar-local magent-ui--streaming-batch-timer nil
  "Timer for flushing batched streaming text.")

(defvar-local magent-ui--in-reasoning-block nil
  "Non-nil if currently inside a reasoning block.")

(defvar-local magent-ui--reasoning-start nil
  "Buffer position where the current reasoning block begins.")

(defvar-local magent-ui--reasoning-char-count 0
  "Number of reasoning characters observed in the current reasoning row.")

(defvar-local magent-ui--fontify-timer nil
  "Legacy idle timer slot for removed deferred markdown-to-org conversion.")

(defun magent-ui--reset-streaming-state ()
  "Reset streaming state variables for a new round.
Must be called inside the magent output buffer."
  (setq magent-ui--streaming-start (point-max))
  (setq magent-ui--streaming-has-text nil)
  (setq magent-ui--streaming-batch-buffer "")
  (setq magent-ui--in-reasoning-block nil)
  (setq magent-ui--reasoning-start nil))

(defun magent-ui-start-streaming ()
  "Prepare the output buffer for a streaming response.
Inserts the assistant section heading."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (magent-ui--insert-separator)
    (setq magent-ui--streaming-section-start (point))
    (magent-ui--insert-header magent-assistant-prompt 'magent-assistant-header)
    (setq magent-ui--response-body-start (point-max))
    (magent-ui--reset-streaming-state)))

(defun magent-ui-continue-streaming ()
  "Continue streaming in the current section after a tool-use round.
Flushes any pending text from the current round, then resets
streaming state without inserting a new heading.  Tool blocks and
subsequent text remain under the existing assistant section."
  (let ((buf (magent-ui-get-buffer)))
    (with-current-buffer buf
      (magent-ui--flush-streaming-batch)
      (when magent-ui--streaming-has-text
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (eq (char-before) ?\n)
            (insert "\n"))))
      (setq magent-ui--streaming-section-start nil)
      (magent-ui--reset-streaming-state))))

(defun magent-ui--flush-streaming-batch ()
  "Flush accumulated streaming text to buffer.
All buffer-local variable access is done inside the magent output buffer."
  (let ((buf (magent-ui-get-buffer)))
    (with-current-buffer buf
      (when (> (length magent-ui--streaming-batch-buffer) 0)
        (let ((inhibit-read-only t)
              (ro-start (point-max)))
          (goto-char (point-max))
          (condition-case nil
              (progn
                (insert magent-ui--streaming-batch-buffer)
                (magent-ui--apply-assistant-text-properties
                 ro-start (point-max))
                (setq magent-ui--streaming-has-text t)
                (setq magent-ui--streaming-batch-buffer "")
                (when (> (point-max) ro-start)
                  (add-text-properties ro-start (point-max) '(read-only t)))
                (when magent-auto-scroll
                  (goto-char (point-max))
                  (let ((win (get-buffer-window buf)))
                    (when win
                      (set-window-point win (point-max))))))
            ((beginning-of-buffer end-of-buffer beginning-of-line end-of-line)
             nil))))
      (when magent-ui--streaming-batch-timer
        (cancel-timer magent-ui--streaming-batch-timer)
        (setq magent-ui--streaming-batch-timer nil)))))

(defun magent-ui-insert-streaming (text)
  "Insert streaming TEXT into output buffer.
Small chunks are batched to reduce UI updates."
  (let ((buf (magent-ui-get-buffer)))
    (with-current-buffer buf
      (setq magent-ui--streaming-batch-buffer
            (concat magent-ui--streaming-batch-buffer text))
      (unless magent-ui--streaming-batch-timer
        (setq magent-ui--streaming-batch-timer
              (run-with-timer magent-ui-batch-insert-delay nil
                              #'magent-ui--flush-streaming-batch))))))

(defun magent-ui-finish-streaming-fontify ()
  "Finalize streaming section.
If no text was streamed (tool-only round), removes the orphaned heading.
When text was streamed, leaves the raw markdown/plain text intact."
  (let ((buf (magent-ui-get-buffer)))
    (with-current-buffer buf
      (magent-ui--flush-streaming-batch)
      (when magent-ui--streaming-start
        (condition-case nil
            (if (not magent-ui--streaming-has-text)
                (when magent-ui--streaming-section-start
                  (let ((inhibit-read-only t))
                    (delete-region magent-ui--streaming-section-start
                                   (min magent-ui--streaming-start (point-max)))))
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (unless (eq (char-before) ?\n)
                  (insert "\n"))))
          ((beginning-of-buffer end-of-buffer beginning-of-line end-of-line)
           (magent-log "DEBUG Suppressed cursor error in streaming finish")
           nil))
        (setq magent-ui--streaming-section-start nil)
        (setq magent-ui--response-body-start nil)
        (magent-ui--reset-streaming-state)))))

(defun magent-ui--convert-markdown-region (start end)
  "Compatibility no-op for the removed live markdown-to-org path."
  (ignore start end))

(defun magent-ui--schedule-markdown-conversion (buffer start-marker end-marker)
  "Compatibility no-op for removed deferred markdown conversion."
  (ignore buffer start-marker end-marker))

(defun magent-ui-insert-reasoning-start ()
  "Insert or reset a compact reasoning status row."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (setq magent-ui--in-reasoning-block t)
    (setq magent-ui--reasoning-start (point))
    (setq-local magent-ui--reasoning-char-count 0)
    (insert (propertize "Reasoning [streaming] 0 chars\n"
                        'face 'magent-reasoning-header))))

(defun magent-ui-insert-reasoning-text (text)
  "Update the compact reasoning status with TEXT length."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (setq-local magent-ui--reasoning-char-count
                (+ (or (and (boundp 'magent-ui--reasoning-char-count)
                            magent-ui--reasoning-char-count)
                       0)
                   (length (or text ""))))
    (when magent-ui--reasoning-start
      (let ((inhibit-read-only t)
            (start magent-ui--reasoning-start))
        (save-excursion
          (goto-char start)
          (delete-region start (line-end-position))
          (insert (propertize
                   (format "Reasoning [streaming] %d chars"
                           magent-ui--reasoning-char-count)
                   'face 'magent-reasoning-header)))))))

(defun magent-ui-insert-reasoning-end ()
  "Finalize the compact reasoning status row."
  (let ((buf (magent-ui-get-buffer)))
    (magent-ui--with-insert buf
      (when magent-ui--reasoning-start
        (let ((start magent-ui--reasoning-start))
          (save-excursion
            (goto-char start)
            (delete-region start (line-end-position))
            (insert (propertize
                     (format "Reasoning [done] %d chars"
                             (or (and (boundp
                                       'magent-ui--reasoning-char-count)
                                      magent-ui--reasoning-char-count)
                                 0))
                     'face 'magent-reasoning-header)))))
      (goto-char (point-max))
      (unless (or (bobp) (eq (char-before) ?\n))
        (insert "\n"))
      (setq magent-ui--in-reasoning-block nil)
      (setq magent-ui--streaming-has-text t)
      (when magent-ui--reasoning-start
        (setq magent-ui--reasoning-start nil)))))

(defun magent-ui--capture-buffer-context ()
  "Capture metadata from the current buffer for auto-context.
Returns a context string or nil if context should not be captured."
  (when (and magent-auto-context
             (not (derived-mode-p 'magent-output-mode))
             (not (minibufferp)))
    (let* ((buf-name (buffer-name))
           (file (buffer-file-name))
           (mode (symbol-name major-mode))
           (line (line-number-at-pos nil t))
           (region-active (or (use-region-p)
                              (run-hook-with-args-until-success
                               'magent-ui-region-active-functions)))
           (parts `(,(format "buffer=\"%s\"" buf-name)
                    ,@(when file
                        (list (format "file=\"%s\"" file)))
                    ,(format "mode=%s" mode)
                    ,(format "line=%d" line)
                    ,@(when region-active
                        (list (format "region=%d-%d"
                                      (line-number-at-pos (region-beginning) t)
                                      (line-number-at-pos (region-end) t)))))))
      (format "[Context: %s]" (string-join parts " ")))))

(defconst magent-ui--emacs-diagnosis-display
  "Diagnose the current Emacs session."
  "Display text used for `magent-diagnose-emacs' user messages.")

(defconst magent-ui--emacs-diagnosis-instructions
  (concat
   "Diagnose problems in the current Emacs session.\n\n"
   "Start by collecting evidence instead of guessing.\n"
   "Inspect the live Emacs state with emacs_eval when useful.\n"
   "Check *Messages*, *Warnings*, *Backtrace*, the current buffer state, and minibuffer state when relevant.\n"
   "If no concrete failure is visible yet, summarize the suspicious signals you can observe and ask for the smallest missing reproduction detail.\n"
   "Do not edit files or make state-changing changes until you have a concrete hypothesis.")
  "Base instructions used by `magent-diagnose-emacs'.")

(defconst magent-ui--doctor-display
  "Run Magent doctor."
  "Display text used for `magent-doctor' user messages.")

(defconst magent-ui--doctor-instructions
  (concat
   "Run a Magent self-check and diagnose Magent-related problems in the current Emacs session.\n\n"
   "Start by collecting evidence instead of guessing.\n"
   "Focus on Magent's own runtime state, commands, buffers, and logs.\n"
   "Inspect the live Emacs state with emacs_eval when useful.\n"
   "Check whether Magent features are loaded, whether `magent-mode' is enabled, the current session scope and agent, queue/request state, and whether there are pending approvals or recent errors.\n"
   "Inspect the relevant `*magent:*` buffer, `*magent-log*`, `*Messages*`, `*Warnings*`, and `*Backtrace*` when available.\n"
   "If the problem seems request-specific, inspect the latest Magent conversation and log entries to identify the failing step.\n"
   "If no concrete failure is visible yet, summarize the suspicious signals you can observe and ask for the smallest missing reproduction detail.\n"
   "Do not edit files or make state-changing changes until you have a concrete hypothesis.")
  "Base instructions used by `magent-doctor'.")

(defun magent-ui--diagnosis-agent ()
  "Return the preferred agent for `magent-diagnose-emacs'."
  (or (magent-agent-registry-get "build")
      (magent-agent-registry-get-default)))

(defun magent-ui--build-diagnosis-prompt (instructions)
  "Build a diagnosis prompt from INSTRUCTIONS and current buffer context."
  (let ((context (magent-ui--capture-buffer-context)))
    (mapconcat #'identity
               (delq nil (list instructions
                               context))
               "\n\n")))

(defun magent-ui--dispatch-diagnosis (instructions source display)
  "Dispatch a diagnosis prompt with INSTRUCTIONS, SOURCE, and DISPLAY."
  (magent-ui-dispatch-prompt
   (magent-ui--build-diagnosis-prompt instructions)
   source
   display
   '("systematic-debugging")
   t
   (magent-ui--diagnosis-agent)))

;;;###autoload
(defun magent-dwim ()
  "Display the Magent workspace and focus the compose buffer.
When `magent-auto-context' is non-nil, the calling buffer's metadata is
inserted into compose as editable text."
  (interactive)
  (let (ctx buf)
    (magent-ui--activate-context-session)
    (setq ctx (magent-ui--capture-buffer-context)
          buf (magent-ui-get-buffer))
    (when (zerop (buffer-size buf))
      (magent-ui-render-history t))
    (display-buffer buf)
    (magent-ui-open-compose nil ctx)
    (run-hooks 'magent-dwim-hook)))

(defun magent-send-prompt (prompt)
  "Send PROMPT to Magent agent programmatically."
  (magent-ui-dispatch-prompt prompt 'send-prompt nil nil t))

;;;###autoload
(defun magent-prompt-region (begin end)
  "Send region from BEGIN to END to Magent agent."
  (interactive "r")
  (let ((input (buffer-substring begin end)))
    (magent-ui-dispatch-prompt input 'prompt-region
                               (format "[Region] %s" input)
                               nil t)))

;;;###autoload
(defun magent-ask-at-point ()
  "Ask about the symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (let ((input (format "Explain this code: %s" symbol)))
        (magent-ui-dispatch-prompt input 'ask-at-point nil nil t)))))

;;;###autoload
(defun magent-diagnose-emacs ()
  "Launch a Magent diagnosis of the current Emacs session."
  (interactive)
  (magent-ui--dispatch-diagnosis
   magent-ui--emacs-diagnosis-instructions
   'diagnose-emacs
   magent-ui--emacs-diagnosis-display))

;;;###autoload
(defun magent-doctor ()
  "Run Magent self-check and diagnose Magent-related issues."
  (interactive)
  (magent-ui--dispatch-diagnosis
   magent-ui--doctor-instructions
   'doctor
   magent-ui--doctor-display))

;;; Processing

(defun magent-ui-dispatch-prompt
    (prompt &optional source display skills activate-context agent)
  "Initialize Magent and dispatch PROMPT through the queue.
SOURCE identifies the caller, DISPLAY overrides the user-visible text,
SKILLS is a list of explicit skill names, and ACTIVATE-CONTEXT controls
whether the current command context should switch session scope first.
AGENT is an optional `magent-agent-info' override for this request."
  (magent--ensure-initialized)
  (when (not (string-blank-p prompt))
    (if activate-context
        (magent-ui--activate-context-session)
      (progn
        (magent-session-get)
        (magent-runtime-activate-scope (magent-session-current-scope))))
    (magent-ui-process prompt source display skills agent)))

(defun magent-ui-process (prompt &optional source display skills agent)
  "Submit PROMPT to Magent's runtime queue.
SOURCE is a symbol identifying the caller (default: \\='prompt).
DISPLAY is the text shown in the buffer's user-message heading;
defaults to PROMPT when nil.
SKILLS is a list of explicit instruction skill names for this request.
AGENT is an optional `magent-agent-info' override for this request."
  (let* ((request-context
          (when (require 'magent-capability nil t)
            (magent-capability-capture-context)))
         (capability-resolution
          (when (require 'magent-capability nil t)
            (magent-capability-resolve-for-turn prompt request-context skills))))
    (magent-ui--enqueue prompt (or source 'prompt)
                        display skills agent request-context capability-resolution)))

(defun magent-ui--run-item (item &optional submission-id)
  "Dispatch ITEM (a `magent-ui--request') to the agent.
Called exclusively by `magent-ui--dispatch' after the lock is held.
Inserts the user message into the output buffer, starts the spinner,
and creates the agent loop.  Captures the current request generation so
stale callbacks are discarded."
  (let* ((input (magent-ui--request-prompt item))
         (gen (cl-incf magent-ui--request-generation))
         (scope (magent-session-current-scope))
         (session (magent-session-get))
         (submission
          (and submission-id
               (magent-turn-active-submission)))
         request-state)
    (magent-ui-display-buffer)
    (magent-ui-render-history t scope)
    (when (and (boundp 'magent--spinner) magent--spinner)
      (spinner-start magent--spinner))
    (magent-log "INFO processing [%s] gen=%d: %s"
                (magent-ui--request-source item) gen input)
    (when-let ((summary
                (and (require 'magent-capability nil t)
                     (magent-capability-resolution-summary
                      (magent-ui--request-capability-resolution item)))))
      (magent-log "INFO %s" summary)
      (magent-ui-insert-capability-summary summary))
    (condition-case err
        (let ((request-live-p
               (lambda ()
                 (and (= gen magent-ui--request-generation)
                      (or (null submission-id)
                          (magent-turn-active-id-p submission-id))))))
          (setq request-state
                (magent-request-context-create
                 :id (magent-events-generate-id)
                 :scope scope
                 :session session
                 :turn-id (and submission
                               (magent-turn-submission-turn-id submission))
                 :approval-session session
                 :origin-buffer-name
                 (or (plist-get (magent-ui--request-request-context item) :buffer-name)
                     (when (string-match "buffer=\"\\([^\"]+\\)\"" input)
                       (match-string 1 input)))
                 :origin-context (magent-ui--request-request-context item)
                 :ui-visibility 'full
                 :live-p request-live-p))
          (setq magent--current-request-handle
                (magent-agent-process
                 input
                 (lambda (response)
                   (if (and (= gen magent-ui--request-generation)
                            (or (null submission-id)
                                (magent-turn-active-id-p submission-id)))
                       (magent-ui--finish-processing response submission-id)
                     (magent-log "DEBUG discarding stale callback gen=%d (current=%d)"
                                 gen magent-ui--request-generation)))
                 (magent-ui--request-agent item)
                 (magent-ui--request-skills item)
                 nil
                 (magent-ui--request-request-context item)
                 (magent-ui--request-capability-resolution item)
                 #'magent-ui-insert-streaming
                 request-live-p
                 request-state))
          (when submission-id
            (magent-turn-set-current-request-handle
             magent--current-request-handle)))
      (error
      (magent-log "ERROR in run-item: %s" (error-message-string err))
      (magent-ui-insert-error (error-message-string err))
       (setq magent--current-request-handle nil)
       (when submission-id
         (magent-turn-finish 'failed (error-message-string err)))
       (when (and (boundp 'magent--spinner) magent--spinner)
         (spinner-stop magent--spinner))
       (magent-ui--clear-processing)
       (magent-ui-render-history t scope)
       (magent-ui--maybe-show-input-prompt scope)))))

(defun magent-ui--finish-processing (response &optional submission-id)
  "Finish processing with RESPONSE and advance the queue.
Handles both streaming and non-streaming completion."
  (let ((success (magent-agent-result-success-p response))
        (content (magent-agent-result-content-string response))
        (scope (magent-session-current-scope)))
    (setq magent--current-request-handle nil)
    (when (and (boundp 'magent--spinner) magent--spinner)
      (spinner-stop magent--spinner))
    (if success
        (magent-log "INFO done")
      (magent-log "ERROR request failed or aborted: %s" content)
      (magent-ui-insert-error
       (if (string-empty-p content)
           "Request failed or was aborted"
         content)))
    (magent-ui--clear-processing)
    (when submission-id
      (magent-turn-finish (if success 'completed 'failed) content))
    (magent-ui-render-history t scope)
    (magent-ui--maybe-show-input-prompt scope)
    (condition-case err
        (progn
          (magent-ui--snapshot-buffer-content magent--current-session scope)
          (magent-session-save-deferred))
      (error
       (magent-log "ERROR session save failed: %s"
                   (error-message-string err))))))

;;; Session management commands

;;;###autoload
(defun magent-clear-session ()
  "Clear the current session."
  (interactive)
  (magent-ui--activate-context-session)
  (magent-ui--clear-pending-skills (magent-session-current-scope))
  (magent-session-reset)
  (magent-ui-clear-buffer)
  (magent-ui-render-history t)
  (magent-ui-open-compose))

;;;###autoload
(defun magent-resume-session ()
  "Select and resume a saved session.
Presents all saved sessions sorted newest-first for selection,
including each session's saved date/time, then loads the chosen
one and renders it in the output buffer."
  (interactive)
  (magent-ui--activate-context-session)
  (let ((files (magent-session-list-files)))
    (if (null files)
        (message "Magent: no saved sessions found")
      (let* ((choices (mapcar (lambda (f)
                                (cons (magent-session--format-file f) f))
                              files))
             (group-map (mapcar (lambda (choice)
                                  (cons (car choice)
                                        (magent-session--file-group (cdr choice))))
                                choices))
             (time-map (mapcar (lambda (choice)
                                 (cons (car choice)
                                       (format "[%s] "
                                               (magent-session--format-display-timestamp
                                                (cdr choice)))))
                               choices))
             (completion-extra-properties
              `(:group-function
                ,(lambda (candidate _transform)
                   (cdr (assoc candidate group-map)))
                :affixation-function
                ,(lambda (candidates)
                   (mapcar (lambda (candidate)
                             (list candidate
                                   (or (cdr (assoc candidate time-map)) "")
                                   ""))
                           candidates))))
             (selected (completing-read "Resume session: "
                                        (mapcar #'car choices) nil t
                                        nil nil (caar choices)))
             (filepath (cdr (assoc selected choices))))
        (when filepath
          (magent-ui--snapshot-buffer-content
           magent--current-session
           (magent-session-current-scope))
          (when-let* ((loaded (magent-session-read-file filepath))
                      (scope (plist-get loaded :scope))
                      (session (plist-get loaded :session)))
            (magent-ui--activate-scope scope session t)
            (magent-ui-display-buffer scope)
            (message "Magent: session resumed")))))))

;;;###autoload
(defun magent-show-log ()
  "View the Magent log buffer."
  (interactive)
  (let ((buffer (magent-ui-get-log-buffer)))
    (with-current-buffer buffer
      (goto-char (point-max)))
    (display-buffer buffer)))

;;;###autoload
(defun magent-clear-log ()
  "Clear the Magent log buffer."
  (interactive)
  (with-current-buffer (magent-ui-get-log-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;;;###autoload
(defun magent-toggle-read-only ()
  "Toggle read-only mode in the Magent output buffer.
When making the buffer editable, also strips all per-character
`read-only' text properties, which are set independently of
`buffer-read-only' and would otherwise block editing even after
the buffer flag is cleared."
  (interactive)
  (let ((buffer (magent-ui-get-buffer)))
    (with-current-buffer buffer
      (if buffer-read-only
          (let ((inhibit-read-only t))
            (setq buffer-read-only nil)
            (remove-text-properties (point-min) (point-max) '(read-only nil))
            (message "Magent buffer is now editable"))
        (setq buffer-read-only t)
        (message "Magent buffer is now read-only")))))

;;; Agent selection commands

(defun magent-ui--agent-jobs-for-display (session)
  "Return SESSION child-agent jobs in chronological display order."
  (reverse (magent-session-agent-jobs session)))

(defun magent-ui--agent-job-choice-label (job)
  "Return completion label for child-agent JOB."
  (format "%s  [%s]  %s%s"
          (magent-agent-job-id job)
          (symbol-name (magent-agent-job-status job))
          (or (magent-agent-job-agent-name job) "?")
          (if-let ((task (magent-agent-job-task-name job)))
              (format ": %s" task)
            "")))

(defun magent-ui--metadata-value-string (value)
  "Return VALUE as display text for child-agent metadata."
  (cond
   ((null value) "")
   ((vectorp value)
    (mapconcat #'magent-ui--metadata-value-string (append value nil) ", "))
   ((listp value)
    (format "%S" value))
   (t
    (format "%s" value))))

(defun magent-ui--insert-agent-transcript-entry (entry)
  "Insert one child-agent transcript ENTRY into the current buffer."
  (let ((role (or (cdr (assq 'role entry)) "?"))
        (content (or (cdr (assq 'content entry)) "")))
    (insert (format "%s\n" (upcase (format "%s" role))))
    (insert (format "%s\n\n" content))))

(defun magent-ui--render-agent-transcript (job)
  "Render child-agent JOB into the current transcript buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "Child Agent %s\n\n" (magent-agent-job-id job)))
    (insert (format "Agent: %s\n" (or (magent-agent-job-agent-name job) "?")))
    (insert (format "Task: %s\n" (or (magent-agent-job-task-name job) "")))
    (insert (format "Status: %s\n" (symbol-name (magent-agent-job-status job))))
    (when-let ((result (magent-agent-job-result job)))
      (insert (format "Result: %s\n" result)))
    (when-let ((error (magent-agent-job-error job)))
      (insert (format "Error: %s\n" error)))
    (when-let ((prompt (magent-agent-job-prompt job)))
      (insert "\nPrompt\n")
      (insert prompt "\n"))
    (when-let ((metadata (magent-agent-job-metadata job)))
      (insert "\nMetadata\n")
      (dolist (entry metadata)
        (insert (format "%s: %s\n"
                        (car entry)
                        (magent-ui--metadata-value-string (cdr entry))))))
    (insert "\nTranscript\n\n")
    (let ((transcript (magent-agent-job-transcript job)))
      (if transcript
          (dolist (entry transcript)
            (magent-ui--insert-agent-transcript-entry entry))
        (insert "No transcript captured yet.\n")))
    (goto-char (point-min))
    (setq buffer-read-only t)))

;;;###autoload
(defun magent-show-transcript ()
  "Show the full ledger transcript for the current Magent session."
  (interactive)
  (magent--ensure-initialized)
  (magent-ui--activate-context-session)
  (let* ((session (magent-session-get))
         (thread (magent-session-thread-ledger session))
         (buffer (get-buffer-create
                  (format "*Magent Transcript: %s*"
                          (or (and thread (magent-thread-id thread))
                              "none")))))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Magent Transcript  thread=%s\n\n"
                        (or (and thread (magent-thread-id thread)) "none")))
        (if (and thread (magent-thread-turns thread))
            (dolist (turn (magent-thread-turns thread))
              (magent-ui--insert-fragment
               (magent-ui--turn-title turn)
               'font-lock-function-name-face
               (lambda ()
                 (magent-ui--insert-turn-body turn))
               (format "turn:%s" (magent-thread-turn-id turn))))
          (insert "No transcript items.\n"))
        (add-text-properties (point-min) (point-max) '(read-only t))
        (goto-char (point-min))))
    (display-buffer buffer)))

;;;###autoload
(defun magent-show-agent-transcript ()
  "Inspect a child-agent transcript from the current session."
  (interactive)
  (magent--ensure-initialized)
  (magent-ui--activate-context-session)
  (let* ((session (magent-session-get))
         (jobs (magent-ui--agent-jobs-for-display session)))
    (unless jobs
      (user-error "Magent: no child-agent jobs in this session"))
    (let* ((choices (mapcar
                     (lambda (job)
                       (cons (magent-ui--agent-job-choice-label job) job))
                     jobs))
           (selected (completing-read "Child agent: "
                                      (mapcar #'car choices) nil t))
           (job (cdr (assoc selected choices)))
           (buffer (get-buffer-create
                    (format "*Magent Agent: %s*"
                            (magent-agent-job-id job)))))
      (with-current-buffer buffer
        (special-mode)
        (magent-ui--render-agent-transcript job))
      (display-buffer buffer))))

;;;###autoload
(defun magent-select-agent ()
  "Select an agent for the current session."
  (interactive)
  (magent--ensure-initialized)
  (magent-ui--activate-context-session)
  (let* ((agents (magent-agent-registry-primary-agents))
         (agent-names (mapcar #'magent-agent-info-name agents))
         (selected (completing-read "Select agent: " agent-names nil t)))
    (when selected
      (let* ((agent-info (magent-agent-registry-get selected))
             (session (magent-session-get)))
        (magent-session-set-agent session agent-info)
        (magent-ui--refresh-header-line (magent-session-current-scope))
        (magent-log "INFO agent selected: %s" selected)))))

;;;###autoload
(defun magent-show-current-agent ()
  "Show the current agent for this session in message buffer."
  (interactive)
  (magent--ensure-initialized)
  (magent-ui--activate-context-session)
  (let* ((session (magent-session-get))
         (agent (magent-session-agent session)))
    (if agent
        (message "Magent: agent=%s (%s)"
                 (magent-agent-info-name agent)
                 (or (magent-agent-info-description agent) "no description"))
      (message "Magent: no agent selected (will use default)"))))

(provide 'magent-ui)
;;; magent-ui.el ends here
