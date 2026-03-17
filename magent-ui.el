;;; magent-ui.el --- User interface for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; User interface for Magent with in-buffer input and output.
;; The output buffer derives from org-mode for native folding support.
;; Each message section uses a level-1 heading (*), and LLM content uses
;; level-2+ headings (**).

;;; Code:

(require 'org)
(require 'spinner)
(require 'subr-x)
(require 'transient)
(require 'magent-approval)
(require 'magent-session)
(require 'magent-agent)
(require 'magent-agent-registry)
(require 'magent-fsm)
(require 'magent-queue)
(require 'magent-md2org)

(defvar magent--spinner)

(defvar magent--current-fsm nil
  "Current active FSM instance, if any.")

;; Forward declarations for buffer-local input state (defined in
;; "In-buffer input" section below).
(defvar magent-ui--input-marker)
(defvar magent-ui--input-section-start)

;; Forward declarations for magent-skills (loaded lazily via require)
(declare-function magent-capability-capture-context "magent-capability")
(declare-function magent-explain-last-capability-resolution "magent-capability")
(declare-function magent-list-capabilities-for-current-context "magent-capability")
(declare-function magent-capability-resolution-summary "magent-capability")
(declare-function magent-capability-resolve-for-turn "magent-capability")
(declare-function magent-toggle-capability-locally "magent-capability")
(declare-function magent-skills-get "magent-skills")
(declare-function magent-skills-list "magent-skills")
(declare-function magent-skills-list-by-type "magent-skills")
(declare-function magent-skill-type "magent-skills")
(declare-function magent-skill-description "magent-skills")

;; Forward declarations for evil (loaded via with-eval-after-load)
(declare-function evil-define-key "evil-core")
(declare-function evil-define-key* "evil-core")
(declare-function evil-visual-state-p "evil-states")
(declare-function evil-insert-state "evil-states")

;; Forward declaration for magent entry point (magent.el loaded first)
(declare-function magent--ensure-initialized "magent")

(defvar magent-ui--request-generation 0
  "Monotonically increasing counter for request dispatch cycles.
Incremented on each new dispatch and on interrupt.  Callbacks
capture this value and compare on completion to detect and
discard stale callbacks from interrupted requests.")

;;; Buffer management

(defvar magent-log-buffer-name "*magent-log*"
  "Name of the buffer used for Magent logging.")

(defun magent-ui-get-log-buffer ()
  "Get or create the Magent log buffer."
  (let ((buffer (get-buffer-create magent-log-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magent-log-mode)
        (magent-log-mode)))
    buffer))

(defmacro magent-ui--with-insert (buffer &rest body)
  "Execute BODY at end of BUFFER with `inhibit-read-only' set.
After BODY, marks the newly inserted region as read-only and
auto-scrolls if `magent-auto-scroll' is non-nil.
Buffer-boundary signals are suppressed because callbacks from
gptel process filters can trigger evil-mode cursor adjustments
that hit buffer edges."
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
  (let ((buf (magent-ui-get-log-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
          (insert (format "[%s] %s\n"
                          timestamp
                          (apply #'format format-string args))))))))

(defun magent-ui-get-buffer ()
  "Get or create the Magent output buffer."
  (let ((buffer (get-buffer-create magent-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magent-output-mode)
        (magent-output-mode)))
    buffer))

(defun magent-ui-display-buffer ()
  "Display the Magent output buffer.
If the buffer is empty but the session has history, render all
past messages so the user can see the full conversation."
  (interactive)
  (let ((buffer (magent-ui-get-buffer)))
    (when (zerop (buffer-size buffer))
      (magent-ui-render-history))
    (display-buffer buffer)))

(defun magent-ui--snapshot-buffer-content (session)
  "Capture the output buffer text into SESSION's buffer-content slot.
Returns the captured string, or nil if the buffer is empty or SESSION is nil."
  (when session
    (let ((buf (magent-ui-get-buffer)))
      (when (> (buffer-size buf) 0)
        (let ((content (with-current-buffer buf
                         (buffer-substring-no-properties (point-min) (point-max)))))
          (setf (magent-session-buffer-content session) content)
          content)))))

(defun magent-ui--context-scope ()
  "Return the session scope implied by the current command context."
  (if (derived-mode-p 'magent-output-mode)
      (magent-session-current-scope)
    (magent-session-scope-from-directory default-directory)))

(defun magent-ui--activate-context-session ()
  "Activate the session scope for the current command context.
When switching scopes, snapshot the outgoing buffer before rendering the
new scope into `magent-buffer-name'.  Returns the active session."
  (let ((target-scope (magent-ui--context-scope))
        (current-scope (magent-session-current-scope)))
    (unless (equal target-scope current-scope)
      (when (magent-queue-processing-p)
        (user-error "Magent: cannot switch project while a request is in progress"))
      (magent-ui--snapshot-buffer-content magent--current-session)
      (magent-session-activate target-scope)
      (magent-ui-render-history t))
    (magent-session-get)))

(defun magent-ui--header-specs ()
  "Return Magent heading labels and the faces used to render them."
  `((,magent-user-prompt . magent-user-header)
    (,magent-assistant-prompt . magent-assistant-header)
    (,magent-error-prompt . magent-error-header)))

(defun magent-ui--header-regexp ()
  "Return a regexp matching Magent's level-1 heading lines."
  (concat "^\\* "
          (regexp-opt (mapcar #'car (magent-ui--header-specs)))
          " +$"))

(defun magent-ui--header-spec-for-line (line)
  "Return the `(LABEL . FACE)' spec for heading LINE, or nil."
  (catch 'match
    (dolist (spec (magent-ui--header-specs))
      (when (string-match-p
             (concat "\\`\\* " (regexp-quote (car spec)) " +\\'")
             line)
        (throw 'match spec)))
    nil))

(defun magent-ui--rehydrate-header-line (line-start line-end spec)
  "Rebuild Magent heading overlays for SPEC between LINE-START and LINE-END."
  (let* ((label (car spec))
         (face (cdr spec))
         (label-start (+ line-start 2))
         (label-end (+ label-start (length label))))
    (when (<= label-end line-end)
      (let ((ov (make-overlay label-start label-end)))
        (overlay-put ov 'face face)
        (overlay-put ov 'magent-ui-overlay t)))
    (when (and magent-ui-header-strike-through
               (> line-end (1+ label-end)))
      (let ((ov (make-overlay (1- line-end) line-end)))
        (overlay-put ov 'display '(space :align-to right))
        (overlay-put ov 'face 'magent-strike-through)
        (overlay-put ov 'magent-ui-overlay t)))))

(defun magent-ui--rehydrate-separator-line (line-start line-end)
  "Rebuild a full-width separator overlay between LINE-START and LINE-END."
  (when (and magent-ui-separator-char
             (/= (char-syntax magent-ui-separator-char) ?\s)
             (= (- line-end line-start) 1)
             (= (char-after line-start) magent-ui-separator-char))
    (let ((ov (make-overlay line-start line-end)))
      (overlay-put ov 'display '(space :align-to right))
      (overlay-put ov 'face 'magent-separator)
      (overlay-put ov 'magent-ui-overlay t))))

(defun magent-ui--rehydrate-visual-state ()
  "Rebuild Magent overlays after restoring a plain-text buffer snapshot."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let* ((line-start (point))
             (line-end (line-end-position))
             (line (buffer-substring-no-properties line-start line-end))
             (spec (magent-ui--header-spec-for-line line)))
        (when spec
          (magent-ui--rehydrate-header-line line-start line-end spec))
        (magent-ui--rehydrate-separator-line line-start line-end)
        (forward-line 1)))))

(defun magent-ui--last-session-message-role (session)
  "Return the role of SESSION's last message, or nil when empty."
  (when-let ((last-msg (car (last (magent-session-get-messages session)))))
    (magent-msg-role last-msg)))

(defun magent-ui--input-section-start-from-heading (heading-start)
  "Infer the prompt section start from trailing prompt HEADING-START."
  (cond
   ((= heading-start (point-min))
    heading-start)
   ((null magent-ui-separator-char)
    heading-start)
   ((= (char-syntax magent-ui-separator-char) ?\s)
    (max (point-min) (1- heading-start)))
   (t
    (let ((candidate (max (point-min) (- heading-start 3))))
      (if (and (< (1+ candidate) heading-start)
               (= (char-after (1+ candidate)) magent-ui-separator-char))
          candidate
        heading-start)))))

(defun magent-ui--restore-input-prompt-state (session)
  "Restore prompt markers and editability from a trailing prompt in SESSION."
  (setq magent-ui--input-marker nil)
  (setq magent-ui--input-section-start nil)
  (when (and (not (magent-queue-processing-p))
             (> (point-max) (point-min)))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward (magent-ui--header-regexp) nil t)
        (let* ((line-start (line-beginning-position))
               (line (buffer-substring-no-properties
                      line-start (line-end-position)))
               (spec (magent-ui--header-spec-for-line line)))
          (when (and spec
                     (string= (car spec) magent-user-prompt)
                     (not (eq (magent-ui--last-session-message-role session) 'user)))
            (let* ((section-start
                    (magent-ui--input-section-start-from-heading line-start))
                   (input-start
                    (save-excursion
                      (goto-char line-start)
                      (forward-line 1)
                      (point))))
              (let ((inhibit-read-only t))
                (remove-text-properties input-start (point-max) '(read-only t))
                (when (> input-start section-start)
                  (add-text-properties section-start input-start '(read-only t))
                  (put-text-property (1- input-start) input-start
                                     'rear-nonsticky '(read-only))))
              (setq magent-ui--input-section-start section-start)
              (setq magent-ui--input-marker (copy-marker input-start)))))))))

(defun magent-ui--restore-buffer-snapshot (buffer session saved)
  "Restore SAVED into BUFFER and rebuild Magent UI state for SESSION."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (insert saved)
      (add-text-properties (point-min) (point-max) '(read-only t)))
    (magent-ui--rehydrate-visual-state)
    (magent-ui--restore-input-prompt-state session)
    (goto-char (point-max))))

(defun magent-ui-render-history (&optional skip-snapshot)
  "Render session history into the output buffer.
If the session has saved buffer content (from a previous render),
restore it directly to preserve tool calls, reasoning blocks, and
error messages, then rebuild Magent-specific overlays and prompt
editability.  Otherwise fall back to re-rendering from session
messages (which only contains user/assistant text).
Before clearing the buffer, the current content is saved to the
session so that future calls can restore it losslessly.
When SKIP-SNAPSHOT is non-nil, skip snapshotting the current
buffer before rendering.  This is required when switching to a
different session; otherwise the old buffer text would overwrite the
newly loaded session snapshot."
  (let* ((session (magent-session-get))
         (buf (magent-ui-get-buffer))
         (saved (or (and (not skip-snapshot)
                         (magent-ui--snapshot-buffer-content session))
                    (magent-session-buffer-content session))))
    (magent-ui-clear-buffer)
    (if saved
        ;; Restore saved content losslessly, then rebuild UI overlays/state.
        (magent-ui--restore-buffer-snapshot buf session saved)
      ;; Fallback: re-render from session messages
      (let ((magent-auto-scroll nil))
        (dolist (msg (magent-session-get-messages session))
          (let ((role (magent-msg-role msg))
                (content (magent-msg-content msg)))
            (pcase role
              ('user
               (when (stringp content)
                 (magent-ui-insert-user-message content)))
              ('assistant
               (when (and (stringp content) (> (length content) 0))
                 (magent-ui-insert-assistant-message content)))))))
      (with-current-buffer buf
        (goto-char (point-max))))
    (magent-ui--maybe-show-input-prompt)))

(defun magent-ui-clear-buffer ()
  "Clear the Magent output buffer and reset input state."
  (interactive)
  (with-current-buffer (magent-ui-get-buffer)
    (let ((inhibit-read-only t))
      (remove-overlays (point-min) (point-max) 'magent-ui-overlay t)
      (erase-buffer))
    (setq magent-ui--input-marker nil)
    (setq magent-ui--input-section-start nil)))

;;; Interrupt command

(defun magent-interrupt ()
  "Interrupt the current request.
Bumps the generation counter so stale callbacks from the aborted
request are discarded."
  (interactive)
  (when magent--current-fsm
    (magent-fsm-abort magent--current-fsm)
    (setq magent--current-fsm nil))
  (magent-approval-drop-requests)
  (cl-incf magent-ui--request-generation)
  (when (and (boundp 'magent--spinner) magent--spinner)
    (spinner-stop magent--spinner))
  (magent-ui-insert-error "[Interrupted by user]")
  (magent-log "INFO Request interrupted by user (gen now %d)"
              magent-ui--request-generation)
  (magent-queue-dequeue-and-run)
  (magent-ui--maybe-show-input-prompt))

;;; Transient menu

(defun magent-transient-menu--assign-agent-keys (agents)
  "Return alist of (KEY . AGENT-INFO) for AGENTS with unique single-char keys.
Skips keys already reserved by the static parts of `magent-transient-menu'."
  (let ((used (make-hash-table :test #'equal))
        result)
    ;; Reserve keys used by static menu entries
    (dolist (k '("c" "d" "D" "R" "r" "l" "L" "x" "e" "k"))
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
               'magent-transient-menu
               (list key name
                     (let ((agent-name name))
                       (lambda ()
                         (interactive)
                         (let* ((info    (magent-agent-registry-get agent-name))
                                (session (magent-session-get)))
                           (magent-session-set-agent session info)
                           (magent-log "INFO agent selected: %s" agent-name))))))))
          (magent-transient-menu--assign-agent-keys
           (magent-agent-registry-primary-agents))))

(transient-define-prefix magent-transient-menu ()
  "Magent command menu."
  ["Session"
   [("c" "Clear session" magent-clear-session)
    ("d" "Diagnose Emacs" magent-diagnose-emacs)
    ("D" "Magent doctor" magent-doctor)
    ("R" "Resume session" magent-resume-session)]]
  ["Agent"
   [:class transient-column
           :setup-children magent-transient-menu--agent-suffixes]]
  ["Capabilities"
   [("x" "Current context" magent-list-capabilities-for-current-context)
    ("e" "Last resolution" magent-explain-last-capability-resolution)
    ("k" "Toggle local" magent-toggle-capability-locally)]]
  ["Buffer"
   [("r" "Toggle read-only" magent-toggle-read-only)]]
  ["Logs"
   [("l" "Show log" magent-show-log)
    ("L" "Clear log" magent-clear-log)]])

;;; Output mode

(defvar magent-output-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "C-g") #'magent-interrupt)
    (define-key map (kbd "C-c C-c") #'magent-input-submit)
    map)
  "Keymap for `magent-output-mode'.")

(with-eval-after-load 'evil
  (evil-define-key 'normal magent-output-mode-map (kbd "?") #'magent-transient-menu))

(define-derived-mode magent-output-mode org-mode "M"
  "Major mode for Magent output.
Derives from org-mode for native folding.  Each message section
is a level-1 heading (*), and LLM content uses level-2+ headings (**).
\\<magent-output-mode-map>
Press \\[magent-interrupt] to interrupt the current request.
Press \\[magent-transient-menu] for the command menu."
  (visual-line-mode 1)
  (setq-local display-fill-column-indicator-column nil)
  (setq-local revert-buffer-function #'magent-ui--revert-buffer)
  (setq-local evil-move-beyond-eol t)
  ;; Prevent org-mode from fontifying content inside tool/think blocks
  (setq-local org-protecting-blocks
              (append '("tool" "think") org-protecting-blocks))
  (add-hook 'completion-at-point-functions #'magent-ui--slash-capf nil t)
  (add-hook 'post-self-insert-hook #'magent-ui--maybe-slash-complete nil t)
  (add-hook 'kill-buffer-hook #'magent-ui--cancel-timers nil t))

(with-eval-after-load 'evil
  (evil-define-key* 'normal magent-output-mode-map
    (kbd "C-c C-c") #'magent-input-submit))

(defun magent-ui--revert-buffer (_ignore-auto _noconfirm)
  "Revert the Magent buffer by re-rendering from saved content.
Delegates to `magent-ui-render-history' which saves the current
buffer content to the session before clearing, then restores it."
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

;;; In-buffer input

(defvar-local magent-ui--input-marker nil
  "Marker at the start of the user input area.
Non-nil only when an editable input prompt is active.  Everything
before this marker is read-only; everything at or after it is
editable by the user.")

(defvar-local magent-ui--input-section-start nil
  "Buffer position where the input prompt section starts.
Includes the separator and header that precede the editable area.
Used by `magent-ui--remove-input-prompt' to delete the entire section.")

(defun magent-ui--insert-input-prompt ()
  "Insert a user input prompt at the end of the output buffer.
The prompt consists of a separator, a `* [USER]' heading (both
read-only), and an editable area below where the user can type.
Sets `magent-ui--input-marker' to the start of the editable area."
  (with-current-buffer (magent-ui-get-buffer)
    (let ((inhibit-read-only t)
          (start (point-max)))
      (goto-char (point-max))
      (magent-ui--insert-separator)
      (magent-ui--insert-header magent-user-prompt 'magent-user-header)
      ;; Make separator + header read-only, last char rear-nonsticky
      ;; so user-typed text after it does NOT inherit read-only.
      (when (> (point-max) start)
        (add-text-properties start (point-max) '(read-only t))
        (put-text-property (1- (point-max)) (point-max)
                           'rear-nonsticky '(read-only)))
      (setq magent-ui--input-section-start start)
      (setq magent-ui--input-marker (copy-marker (point-max)))
      ;; Auto-scroll to input area
      (let ((win (get-buffer-window (current-buffer))))
        (when win
          (set-window-point win (point-max)))))))

(defun magent-ui--maybe-show-input-prompt ()
  "Insert an input prompt unless a queued item is about to run."
  (with-current-buffer (magent-ui-get-buffer)
    (unless (or (magent-queue-processing-p)
                magent-ui--input-marker)
      (magent-ui--insert-input-prompt))))

(defun magent-ui--remove-input-prompt ()
  "Remove the input prompt section (separator + header + any draft text).
No-op if no input prompt is active."
  (with-current-buffer (magent-ui-get-buffer)
    (when magent-ui--input-marker
      (let ((inhibit-read-only t))
        (delete-region (or magent-ui--input-section-start
                           (marker-position magent-ui--input-marker))
                       (point-max)))
      (setq magent-ui--input-marker nil)
      (setq magent-ui--input-section-start nil))))

(defun magent-ui--slash-parse (text)
  "Parse /skill-name tokens from TEXT.
Returns a cons (SKILL-NAMES . MESSAGE) where SKILL-NAMES is a list
of recognized skill name strings and MESSAGE is TEXT with those
tokens removed and whitespace normalized.  Unrecognized /tokens
are left in the message text unchanged."
  (require 'magent-skills)
  (let ((names nil)
        (pos 0)
        (parts nil))
    (while (string-match "/\\([a-zA-Z][a-zA-Z0-9_-]*\\)/?" text pos)
      (let* ((match-start (match-beginning 0))
             (match-end   (match-end 0))
             (name        (match-string 1 text)))
        (push (substring text pos match-start) parts)
        (let ((skill (magent-skills-get name)))
          (if (and skill (eq (magent-skill-type skill) 'instruction))
              (push name names)
            (push (match-string 0 text) parts)))
        (setq pos match-end)))
    (push (substring text pos) parts)
    (cons (nreverse names) (string-trim (apply #'concat (nreverse parts))))))

(defun magent-ui--slash-capf ()
  "Completion-at-point function for /skill-name in the input area.
Only active when the input zone is open and point follows a /
with optional partial skill name."
  (when (and magent-ui--input-marker
             (>= (point) (marker-position magent-ui--input-marker)))
    (save-excursion
      (when (re-search-backward "/\\([a-zA-Z0-9_-]*\\)\\=" nil t)
        (let ((word-start (match-beginning 1))
              (word-end   (match-end 1)))
          (magent--ensure-initialized)
          (list word-start word-end
                (magent-skills-list-by-type 'instruction)
                :annotation-function
                (lambda (name)
                  (when-let* ((skill (magent-skills-get name))
                              (desc  (magent-skill-description skill)))
                    (concat "  " (if (listp desc)
                                     (mapconcat #'identity desc ", ")
                                   desc))))
                :exclusive 'no))))))

(defun magent-ui--maybe-slash-complete ()
  "Auto-trigger completion when `/` is typed in the input area."
  (when (and magent-ui--input-marker
             (>= (point) (marker-position magent-ui--input-marker))
             (eq (char-before) ?/))
    (completion-at-point)))

(defun magent-input-submit ()
  "Submit the text in the input area and send it to the agent.
Makes the input text read-only, clears the input marker, and
dispatches to the queue.  The `* [USER]' heading already in the
buffer is reused (the queue skips inserting a duplicate)."
  (interactive)
  (unless magent-ui--input-marker
    (user-error "No input area active"))
  (magent--ensure-initialized)
  ;; Find the actual start of user-editable content: the first position
  ;; after the read-only header (separator + "* USER \n").  We use
  ;; next-single-property-change rather than the marker directly because
  ;; the marker can drift to point-max when text is inserted before it
  ;; (e.g. via org-mode electric indent or completion-at-point), making
  ;; buffer-substring return "" even though the user has typed text.
  (let* ((input-start (or (when magent-ui--input-section-start
                            (next-single-property-change
                             magent-ui--input-section-start 'read-only nil
                             (point-max)))
                          (marker-position magent-ui--input-marker)))
         (raw (string-trim
               (buffer-substring-no-properties input-start (point-max))))
         (parsed      (magent-ui--slash-parse raw))
         (skill-names (car parsed))
         (text        (cdr parsed)))
    (when (string-blank-p text)
      (user-error "Empty input"))
    (let ((inhibit-read-only t))
      ;; Ensure trailing newline
      (goto-char (point-max))
      (unless (eq (char-before) ?\n)
        (insert "\n"))
      ;; Make the user text read-only
      (add-text-properties input-start (point-max) '(read-only t)))
    (setq magent-ui--input-marker nil)
    (setq magent-ui--input-section-start nil)
    (magent-ui-process text 'buffer-input nil skill-names)))

;;; Section folding

(defun magent-ui-toggle-section ()
  "Toggle folding of the section at point in the Magent output buffer."
  (interactive)
  (with-current-buffer (magent-ui-get-buffer)
    (when (derived-mode-p 'magent-output-mode)
      (org-cycle))))

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
  "Insert \"* LABEL\" heading followed by a dash line extending to window edge.
FACE, when non-nil, is applied to the label text via an overlay.
The trailing line uses the `magent-strike-through' face.  Overlays are
required because org-mode font-lock overwrites text-property faces on
heading lines."
  (let ((heading-start (point)))
    (insert "* " label " ")
    (when face
      (let ((ov (make-overlay (+ heading-start 2)
                              (+ heading-start 2 (length label)))))
        (overlay-put ov 'face face)
        (overlay-put ov 'magent-ui-overlay t))))
  (when magent-ui-header-strike-through
    (magent-ui--insert-full-width-line ?\s 'magent-strike-through))
  (insert "\n"))

(defun magent-ui-insert-user-message (text)
  "Insert user message TEXT into output buffer with level-1 heading."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (magent-ui--insert-separator)
    (magent-ui--insert-header magent-user-prompt 'magent-user-header)
    (insert text "\n")))

(defun magent-ui-insert-assistant-message (text)
  "Insert assistant message TEXT into output buffer with level-1 heading.
Converts markdown to org-mode before insertion."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (magent-ui--insert-separator)
    (magent-ui--insert-header magent-assistant-prompt 'magent-assistant-header)
    (let ((body-start (point)))
      (insert text)
      (unless (string-suffix-p "\n" text)
        (insert "\n"))
      (magent-md2org-convert-region body-start (point)))))

(defun magent-ui--fold-block-at (pos block-re)
  "Fold the block at POS if it matches BLOCK-RE.
Defers `org-cycle' via timer to avoid blocking process filters."
  (when pos
    (let ((buf (current-buffer)))
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (let ((inhibit-read-only t))
                           (save-excursion
                             (condition-case nil
                                 (progn
                                   (goto-char pos)
                                   (when (looking-at block-re)
                                     (org-cycle)))
                               (error nil)))))))))))

(defvar-local magent-ui--tool-call-start nil
  "Buffer position where the current #+begin_tool block was inserted.
Set by `magent-ui-insert-tool-call', consumed by
`magent-ui-insert-tool-result' to auto-fold the completed block.")

(defun magent-ui--sanitize-tool-text (text)
  "Collapse newlines in TEXT to prevent org-mode from parsing headings.
Tool result/input strings are display-only summaries inside
#+begin_tool blocks; embedded newlines can produce spurious org
headings (e.g. \"* Introduction\" from a file read)."
  (replace-regexp-in-string "\n" "\\\\n" text))

(defun magent-ui-insert-tool-call (tool-name input)
  "Insert tool call notification into output buffer.
Wraps in #+begin_tool block."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (setq magent-ui--tool-call-start (point))
    (insert (propertize (concat "#+begin_tool " tool-name)
                        'face 'magent-tool-header)
            "\n")
    (let ((input-str (magent-ui--sanitize-tool-text
                      (if (stringp input)
                          input
                        (truncate-string-to-width
                         (json-encode input) magent-ui-tool-input-max-length nil nil "...")))))
      (insert (propertize input-str 'face 'magent-tool-args) "\n"))))

(defun magent-ui-insert-tool-result (_tool-name result)
  "Insert tool RESULT for _TOOL-NAME into output buffer.
Closes the #+begin_tool block and auto-folds it."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (let ((result-str (magent-ui--sanitize-tool-text
                       (truncate-string-to-width
                        (if (stringp result) result (format "%s" result))
                        magent-ui-result-max-length nil nil "..."))))
      (insert (propertize (concat "-> " result-str) 'face 'magent-tool-result)
              "\n"))
    (insert (propertize "#+end_tool" 'face 'magent-tool-header) "\n")
    (when magent-ui--tool-call-start
      (magent-ui--fold-block-at magent-ui--tool-call-start "#\\+begin_tool")
      (setq magent-ui--tool-call-start nil))))

(defun magent-ui-insert-error (error-text)
  "Insert ERROR-TEXT into output buffer with level-1 heading."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (magent-ui--insert-header magent-error-prompt 'magent-error-header)
    (insert (propertize error-text 'face 'magent-error-body))
    (insert "\n")))

(defun magent-ui-insert-capability-summary (summary)
  "Insert capability SUMMARY into the output buffer."
  (when (and summary (not (string-empty-p summary)))
    (magent-ui--with-insert (magent-ui-get-buffer)
      (insert "#+begin_quote\n")
      (insert (format "Capability resolver: %s\n" summary))
      (insert "#+end_quote\n"))))

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
When text was streamed, converts markdown to org-mode in the response body."
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
                (when magent-ui--response-body-start
                  (magent-md2org-convert-region
                   magent-ui--response-body-start (point-max)))
                (goto-char (point-max))
                (unless (eq (char-before) ?\n)
                  (insert "\n"))))
          ((beginning-of-buffer end-of-buffer beginning-of-line end-of-line)
           (magent-log "DEBUG Suppressed cursor error in streaming finish")
           nil))
        (setq magent-ui--streaming-section-start nil)
        (setq magent-ui--response-body-start nil)
        (magent-ui--reset-streaming-state)))))

(defun magent-ui-insert-reasoning-start ()
  "Insert the beginning of a reasoning block."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (setq magent-ui--in-reasoning-block t)
    (setq magent-ui--reasoning-start (point))
    (insert (propertize "#+begin_think" 'face 'magent-reasoning-header) "\n")))

(defun magent-ui-insert-reasoning-text (text)
  "Insert reasoning TEXT into the output buffer."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (insert text)))

(defun magent-ui-insert-reasoning-end ()
  "Insert the end of a reasoning block and fold it."
  (let ((buf (magent-ui-get-buffer)))
    (magent-ui--with-insert buf
      (insert "\n" (propertize "#+end_think" 'face 'magent-reasoning-header) "\n")
      (setq magent-ui--in-reasoning-block nil)
      (setq magent-ui--streaming-has-text t)
      (when magent-ui--reasoning-start
        (magent-ui--fold-block-at magent-ui--reasoning-start "#\\+begin_think")
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
                              (and (bound-and-true-p evil-local-mode)
                                   (funcall 'evil-visual-state-p))))
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
   "Check whether Magent features are loaded, whether `magent-mode' is enabled, the current session scope and agent, queue/FSM state, and whether there are pending approvals or recent errors.\n"
   "Inspect `*magent*`, `*magent-log*`, `*Messages*`, `*Warnings*`, and `*Backtrace*` when available.\n"
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
  "Switch to the Magent buffer and position cursor at the input area.
If no input prompt exists (e.g. first use or after processing),
one is inserted at the end of the buffer.  When `magent-auto-context'
is non-nil, the calling buffer's metadata is pre-filled as editable
text that the user can keep or delete.  In evil-mode the cursor
enters insert state for immediate typing."
  (interactive)
  (let ((ctx (magent-ui--capture-buffer-context))
        (buf (magent-ui-get-buffer)))
    (magent-ui--activate-context-session)
    (when (zerop (buffer-size buf))
      (magent-ui-render-history))
    (display-buffer buf)
    (pop-to-buffer buf)
    (unless magent-ui--input-marker
      (magent-ui--insert-input-prompt))
    (when ctx
      (goto-char magent-ui--input-marker)
      (insert ctx "\n"))
    (goto-char (point-max))
    (when (and (bound-and-true-p evil-mode)
               (fboundp 'evil-insert-state))
      (evil-insert-state))))

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
      (magent-session-get))
    (magent-ui-process prompt source display skills agent)))

(defun magent-ui-process (prompt &optional source display skills agent)
  "Dispatch PROMPT, rejecting if a request is already in flight.
SOURCE is a symbol identifying the caller (default: \\='prompt).
DISPLAY is the text shown in the buffer's user-message heading;
defaults to PROMPT when nil.
SKILLS is a list of skill name strings selected via slash commands.
AGENT is an optional `magent-agent-info' override for this request."
  (let* ((request-context
          (when (require 'magent-capability nil t)
            (magent-capability-capture-context)))
         (capability-resolution
          (when (require 'magent-capability nil t)
            (magent-capability-resolve-for-turn prompt request-context skills))))
    (magent-queue-enqueue prompt (or source 'prompt)
                          display skills agent request-context capability-resolution)))

(defun magent-ui--run-item (item)
  "Dispatch ITEM (a `magent-queue-item') to the agent.
Called exclusively by `magent-queue--dispatch' after the lock is held.
Inserts the user message into the output buffer, starts the spinner,
and creates the FSM.  Captures the current request generation so
stale callbacks are discarded."
  (let ((input (magent-queue-item-prompt item))
        (gen (cl-incf magent-ui--request-generation)))
    (magent-ui-display-buffer)
    (magent-ui--remove-input-prompt)
    (unless (eq (magent-queue-item-source item) 'buffer-input)
      (magent-ui-insert-user-message (or (magent-queue-item-display item) input)))
    (when (and (boundp 'magent--spinner) magent--spinner)
      (spinner-start magent--spinner))
    (magent-log "INFO processing [%s] gen=%d: %s"
                (magent-queue-item-source item) gen input)
    (when-let ((summary
                (and (require 'magent-capability nil t)
                     (magent-capability-resolution-summary
                      (magent-queue-item-capability-resolution item)))))
      (magent-log "INFO %s" summary)
      (magent-ui-insert-capability-summary summary))
    (condition-case err
        (setq magent--current-fsm
              (magent-agent-process
               input
               (lambda (response)
                 (if (= gen magent-ui--request-generation)
                     (magent-ui--finish-processing response)
                   (magent-log "DEBUG discarding stale callback gen=%d (current=%d)"
                               gen magent-ui--request-generation)))
               (magent-queue-item-agent item)
               (magent-queue-item-skills item)
               nil
               (magent-queue-item-request-context item)
               (magent-queue-item-capability-resolution item)))
      (error
       (magent-log "ERROR in run-item: %s" (error-message-string err))
       (magent-ui-insert-error (error-message-string err))
       (setq magent--current-fsm nil)
       (when (and (boundp 'magent--spinner) magent--spinner)
         (spinner-stop magent--spinner))
       (magent-queue-dequeue-and-run)
       (magent-ui--maybe-show-input-prompt)))))

(defun magent-ui--finish-processing (response)
  "Finish processing with RESPONSE and advance the queue.
Handles both streaming and non-streaming completion."
  (setq magent--current-fsm nil)
  (when (and (boundp 'magent--spinner) magent--spinner)
    (spinner-stop magent--spinner))
  (cond
   ((stringp response)
    (magent-log "INFO done")
    (magent-ui--snapshot-buffer-content magent--current-session)
    (magent-session-save))
   (t
    (magent-log "ERROR request failed or aborted")
    (magent-ui-insert-error "Request failed or was aborted")))
  (magent-queue-dequeue-and-run)
  (magent-ui--maybe-show-input-prompt))

;;; Session management commands

;;;###autoload
(defun magent-clear-session ()
  "Clear the current session."
  (interactive)
  (magent-ui--activate-context-session)
  (magent-queue-clear)
  (magent-session-reset)
  (magent-ui-clear-buffer)
  (magent-ui--insert-input-prompt))

;;;###autoload
(defun magent-resume-session ()
  "Select and resume a saved session.
Presents all saved sessions sorted newest-first for selection,
then loads the chosen one and renders it in the output buffer."
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
             (completion-extra-properties
              `(:group-function
                ,(lambda (candidate _transform)
                   (cdr (assoc candidate group-map)))))
             (selected (completing-read "Resume session: "
                                        (mapcar #'car choices) nil t
                                        nil nil (caar choices)))
             (filepath (cdr (assoc selected choices))))
        (when filepath
          (magent-ui--snapshot-buffer-content magent--current-session)
          (when-let* ((loaded (magent-session-read-file filepath))
                      (scope (plist-get loaded :scope))
                      (session (plist-get loaded :session)))
            (magent-session-install scope session)
            (magent-ui-render-history t)
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
