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
(require 'transient)
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

(defun magent-ui-render-history ()
  "Render session history into the output buffer.
If the session has saved buffer content (from a previous render),
restore it directly to preserve tool calls, reasoning blocks, and
error messages.  Otherwise fall back to re-rendering from session
messages (which only contains user/assistant text).
Before clearing the buffer, the current content is saved to the
session so that future calls can restore it losslessly."
  (let* ((session (magent-session-get))
         (buf (magent-ui-get-buffer))
         (saved (magent-session-buffer-content session)))
    ;; Save current buffer content before clearing
    (with-current-buffer buf
      (when (> (buffer-size) 0)
        (let ((content (buffer-substring-no-properties (point-min) (point-max))))
          (setf (magent-session-buffer-content session) content)
          (setq saved content))))
    (magent-ui-clear-buffer)
    (if saved
        ;; Restore saved content losslessly
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (insert saved)
            (add-text-properties (point-min) (point-max) '(read-only t)))
          (goto-char (point-max)))
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
      (erase-buffer))
    (setq magent-ui--input-marker nil)
    (setq magent-ui--input-section-start nil)))

;;; Interrupt command

(defun magent-interrupt ()
  "Interrupt the current request and advance the queue.
The queue is preserved; the next queued item starts automatically.
Bumps the generation counter so stale callbacks from the aborted
request are discarded."
  (interactive)
  (when magent--current-fsm
    (magent-fsm-abort magent--current-fsm)
    (setq magent--current-fsm nil))
  (cl-incf magent-ui--request-generation)
  (when (and (boundp 'magent--spinner) magent--spinner)
    (spinner-stop magent--spinner))
  (magent-ui-insert-error "[Interrupted by user]")
  (magent-log "INFO Request interrupted by user (gen now %d)"
              magent-ui--request-generation)
  (magent-queue-dequeue-and-run)
  (magent-ui--maybe-show-input-prompt))

;;; Transient menu

(transient-define-prefix magent-transient-menu ()
  "Magent command menu."
  ["Session"
   [("p" "Prompt" magent-dwim)
    ("c" "Clear session" magent-clear-session)]]
  ["Agent"
   [("A" "Select agent" magent-select-agent)
    ("i" "Show current agent" magent-show-current-agent)
    ("v" "List agents" magent-list-agents)]]
  ["Queue"
   [("q" "View queue" magent-list-queue)
    ("Q" "Remove from queue" magent-remove-queue-item)]]
  ["Buffer"
   [("r" "Toggle read-only" magent-toggle-read-only)]]
  ["Control"
   [("C-g" "Interrupt" magent-interrupt)]]
  ["Logs"
   [("l" "Show log" magent-show-log)
    ("L" "Clear log" magent-clear-log)]])

;;; Output mode

(defvar magent-output-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "?") #'magent-transient-menu)
    (define-key map (kbd "C-g") #'magent-interrupt)
    (define-key map (kbd "C-c C-c") #'magent-input-submit)
    map)
  "Keymap for `magent-output-mode'.")

(define-derived-mode magent-output-mode org-mode "Magent"
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
  (unless (magent-queue-processing-p)
    (magent-ui--insert-input-prompt)))

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

(defun magent-input-submit ()
  "Submit the text in the input area and send it to the agent.
Makes the input text read-only, clears the input marker, and
dispatches to the queue.  The `* [USER]' heading already in the
buffer is reused (the queue skips inserting a duplicate)."
  (interactive)
  (unless magent-ui--input-marker
    (user-error "No input area active"))
  (let ((text (string-trim
               (buffer-substring-no-properties
                magent-ui--input-marker (point-max)))))
    (when (string-blank-p text)
      (user-error "Empty input"))
    (let ((inhibit-read-only t))
      ;; Ensure trailing newline
      (goto-char (point-max))
      (unless (eq (char-before) ?\n)
        (insert "\n"))
      ;; Make the user text read-only
      (add-text-properties magent-ui--input-marker (point-max) '(read-only t)))
    (setq magent-ui--input-marker nil)
    (setq magent-ui--input-section-start nil)
    (magent--ensure-initialized)
    (magent-ui-process text 'buffer-input)))

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
      (overlay-put ov 'face face))))

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
        (overlay-put ov 'face face))))
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

(defun magent-ui-insert-tool-call (tool-name input)
  "Insert tool call notification into output buffer.
Wraps in #+begin_tool block."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (setq magent-ui--tool-call-start (point))
    (insert (propertize (concat "#+begin_tool " tool-name)
                        'face 'magent-tool-header)
            "\n")
    (let ((input-str (if (stringp input)
                         input
                       (truncate-string-to-width
                        (json-encode input) magent-ui-tool-input-max-length nil nil "..."))))
      (insert (propertize input-str 'face 'magent-tool-args) "\n"))))

(defun magent-ui-insert-tool-result (_tool-name result)
  "Insert tool RESULT for _TOOL-NAME into output buffer.
Closes the #+begin_tool block and auto-folds it."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (let ((result-str (truncate-string-to-width
                       (if (stringp result) result (format "%s" result))
                       magent-ui-result-max-length nil nil "...")))
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
        (magent-ui--with-insert buf
          (insert magent-ui--streaming-batch-buffer)
          (setq magent-ui--streaming-has-text t)
          (setq magent-ui--streaming-batch-buffer "")))
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
      (when magent-ui--streaming-batch-timer
        (cancel-timer magent-ui--streaming-batch-timer))
      (setq magent-ui--streaming-batch-timer
            (run-with-timer magent-ui-batch-insert-delay nil
                            #'magent-ui--flush-streaming-batch)))))

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
  (magent--ensure-initialized)
  (when (not (string-blank-p prompt))
    (magent-ui-process prompt 'send-prompt)))

;;;###autoload
(defun magent-prompt-region (begin end)
  "Send region from BEGIN to END to Magent agent."
  (interactive "r")
  (magent--ensure-initialized)
  (let ((input (buffer-substring begin end)))
    (magent-ui-process input 'prompt-region
                       (format "[Region] %s" input))))

;;;###autoload
(defun magent-ask-at-point ()
  "Ask about the symbol at point."
  (interactive)
  (magent--ensure-initialized)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (let ((input (format "Explain this code: %s" symbol)))
        (magent-ui-process input 'ask-at-point)))))

;;; Processing

(defun magent-ui-process (prompt &optional source display)
  "Queue or immediately dispatch PROMPT.
SOURCE is a symbol identifying the caller (default: \\='prompt).
DISPLAY is the text shown in the buffer's user-message heading;
defaults to PROMPT when nil.
Shows a minibuffer notification when the item is queued rather
than dispatched immediately."
  (let ((queued (magent-queue-enqueue prompt (or source 'prompt) display)))
    (when queued
      (message "Magent: queued (%d waiting)" (magent-queue-length)))))

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
    (condition-case err
        (setq magent--current-fsm
              (magent-agent-process
               input
               (lambda (response)
                 (if (= gen magent-ui--request-generation)
                     (magent-ui--finish-processing response)
                   (magent-log "DEBUG discarding stale callback gen=%d (current=%d)"
                               gen magent-ui--request-generation)))))
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
    (magent-log "INFO done"))
   (t
    (magent-log "ERROR request failed or aborted")
    (magent-ui-insert-error "Request failed or was aborted")))
  (magent-queue-dequeue-and-run)
  (magent-ui--maybe-show-input-prompt))

;;; Session management commands

;;;###autoload
(defun magent-clear-session ()
  "Clear the current session and discard any queued prompts."
  (interactive)
  (magent-queue-clear)
  (magent-session-reset)
  (magent-ui-clear-buffer)
  (magent-ui--insert-input-prompt))

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
  "Toggle read-only mode in the Magent output buffer."
  (interactive)
  (let ((buffer (magent-ui-get-buffer)))
    (with-current-buffer buffer
      (setq buffer-read-only (not buffer-read-only))
      (message "Magent buffer is now %s"
               (if buffer-read-only "read-only" "editable")))))

;;; Agent selection commands

;;;###autoload
(defun magent-select-agent ()
  "Select an agent for the current session."
  (interactive)
  (magent--ensure-initialized)
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
  (let* ((session (magent-session-get))
         (agent (magent-session-get-agent session)))
    (if agent
        (message "Magent: agent=%s (%s)"
                 (magent-agent-info-name agent)
                 (or (magent-agent-info-description agent) "no description"))
      (message "Magent: no agent selected (will use default)"))))

;;; Queue management commands

;;;###autoload
(defun magent-list-queue ()
  "Display the current prompt queue in a temporary buffer."
  (interactive)
  (let ((items (magent-queue-items)))
    (if (null items)
        (message "Magent: queue is empty")
      (magent--with-display-buffer "*magent-queue*"
        (insert (format "Magent prompt queue (%d item(s)):\n\n" (length items)))
        (cl-loop for item in items
                 for i from 0
                 do (insert
                     (format "[%d] (%s) %s\n"
                             i
                             (format-time-string "%H:%M:%S"
                                                 (magent-queue-item-timestamp item))
                             (truncate-string-to-width
                              (magent-queue-item-prompt item) 72 nil nil "..."))))))))

;;;###autoload
(defun magent-remove-queue-item (n)
  "Remove queued item at 0-based index N."
  (interactive
   (list (if (zerop (magent-queue-length))
             (user-error "Magent: queue is empty")
           (read-number
            (format "Remove queue item (0-%d): "
                    (1- (magent-queue-length)))))))
  (magent-queue-remove-nth n)
  (message "Magent: removed item %d (%d remaining)"
           n (magent-queue-length)))

(provide 'magent-ui)
;;; magent-ui.el ends here
