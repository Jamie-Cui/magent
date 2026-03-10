;;; magent-ui.el --- User interface for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; User interface for Magent including minibuffer commands and output buffer.
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

(defvar magent--spinner)

(defvar magent--current-fsm nil
  "Current active FSM instance, if any.")

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
After BODY, auto-scroll if `magent-auto-scroll' is non-nil.
Buffer-boundary signals are suppressed because callbacks from
gptel process filters can trigger evil-mode cursor adjustments
that hit buffer edges."
  (declare (indent 1))
  `(with-current-buffer ,buffer
     (let ((inhibit-read-only t))
       (condition-case nil
           (progn
             (goto-char (point-max))
             ,@body
             (when magent-auto-scroll
               (goto-char (point-max))
               (let ((win (get-buffer-window (current-buffer))))
                 (when win
                   (set-window-point win (point-max))))))
         ((beginning-of-buffer end-of-buffer beginning-of-line end-of-line)
          (magent-log "DEBUG Suppressed cursor error in buffer insert")
          nil)))))

(defun magent-log (format-string &rest args)
  "Log a message to the Magent log buffer.
FORMAT-STRING and ARGS are passed to `format'."
  (magent-ui--with-insert (magent-ui-get-log-buffer)
    (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "[%s] %s\n" timestamp (apply #'format format-string args))))))

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
    (pop-to-buffer buffer)))

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
            (insert saved))
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
        (goto-char (point-max))))))

(defun magent-ui-clear-buffer ()
  "Clear the Magent output buffer."
  (interactive)
  (with-current-buffer (magent-ui-get-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

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
  (magent-queue-dequeue-and-run))

;;; Transient menu

(transient-define-prefix magent-transient-menu ()
  "Magent command menu."
  ["Session"
   [("p" "Prompt" magent-prompt)
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
  (setq buffer-read-only t)
  (setq-local display-fill-column-indicator-column nil)
  (setq-local revert-buffer-function #'magent-ui--revert-buffer)
  (add-hook 'kill-buffer-hook #'magent-ui--cancel-timers nil t))

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

;;; Section folding

(defun magent-ui-toggle-section ()
  "Toggle folding of the section at point in the Magent output buffer."
  (interactive)
  (with-current-buffer (magent-ui-get-buffer)
    (when (derived-mode-p 'magent-output-mode)
      (org-cycle))))

;;; Rendering functions

(defun magent-ui-insert-user-message (text)
  "Insert user message TEXT into output buffer with level-1 heading."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (insert "* " magent-user-prompt "\n")
    (insert text "\n")))

(defun magent-ui-insert-assistant-message (text)
  "Insert assistant message TEXT into output buffer with level-1 heading."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (insert "* " magent-assistant-prompt "\n")
    (insert text)
    (unless (string-suffix-p "\n" text)
      (insert "\n"))))

(defun magent-ui--fold-block-at (pos block-re)
  "Fold the block at POS if it matches BLOCK-RE.
Defers `org-cycle' via timer to avoid blocking process filters."
  (when pos
    (let ((buf (current-buffer)))
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (save-excursion
                           (goto-char pos)
                           (condition-case nil
                               (when (looking-at block-re)
                                 (org-cycle))
                             (error nil))))))))))

(defvar-local magent-ui--tool-call-start nil
  "Buffer position where the current #+begin_tool block was inserted.
Set by `magent-ui-insert-tool-call', consumed by
`magent-ui-insert-tool-result' to auto-fold the completed block.")

(defun magent-ui-insert-tool-call (tool-name input)
  "Insert tool call notification into output buffer.
Wraps in #+begin_tool block."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (setq magent-ui--tool-call-start (point))
    (insert "#+begin_tool " tool-name "\n")
    (let ((input-str (if (stringp input)
                         input
                       (truncate-string-to-width
                        (json-encode input) magent-ui-tool-input-max-length nil nil "..."))))
      (insert input-str "\n"))))

(defun magent-ui-insert-tool-result (_tool-name result)
  "Insert tool RESULT for _TOOL-NAME into output buffer.
Closes the #+begin_tool block and auto-folds it."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (let ((result-str (truncate-string-to-width
                       (if (stringp result) result (format "%s" result))
                       magent-ui-result-max-length nil nil "...")))
      (insert "-> " result-str "\n"))
    (insert "#+end_tool\n")
    (when magent-ui--tool-call-start
      (magent-ui--fold-block-at magent-ui--tool-call-start "#\\+begin_tool")
      (setq magent-ui--tool-call-start nil))))

(defun magent-ui-insert-error (error-text)
  "Insert ERROR-TEXT into output buffer with level-1 heading."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (insert "* " magent-error-prompt "\n")
    (insert (propertize error-text 'face 'org-warning))
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
    (setq magent-ui--streaming-section-start (point))
    (insert "* " magent-assistant-prompt "\n")
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
If no text was streamed (tool-only round), removes the orphaned heading."
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
        (magent-ui--reset-streaming-state)))))

(defun magent-ui-insert-reasoning-start ()
  "Insert the beginning of a reasoning block."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (setq magent-ui--in-reasoning-block t)
    (setq magent-ui--reasoning-start (point))
    (insert "#+begin_think\n")))

(defun magent-ui-insert-reasoning-text (text)
  "Insert reasoning TEXT into the output buffer."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (insert text)))

(defun magent-ui-insert-reasoning-end ()
  "Insert the end of a reasoning block and fold it."
  (let ((buf (magent-ui-get-buffer)))
    (magent-ui--with-insert buf
      (insert "\n#+end_think\n")
      (setq magent-ui--in-reasoning-block nil)
      (setq magent-ui--streaming-has-text t)
      (when magent-ui--reasoning-start
        (magent-ui--fold-block-at magent-ui--reasoning-start "#\\+begin_think")
        (setq magent-ui--reasoning-start nil)))))

;;; Minibuffer interface

;;;###autoload
(defun magent-prompt ()
  "Prompt for input and send to Magent agent."
  (interactive)
  (magent--ensure-initialized)
  (let ((input (read-string "Ask magent: ")))
    (when (not (string-blank-p input))
      (magent-ui-process input 'prompt))))

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
    (magent-ui-insert-user-message (or (magent-queue-item-display item) input))
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
       (magent-queue-dequeue-and-run)))))

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
  (magent-queue-dequeue-and-run))

;;; Session management commands

;;;###autoload
(defun magent-clear-session ()
  "Clear the current session and discard any queued prompts."
  (interactive)
  (magent-queue-clear)
  (magent-session-reset)
  (magent-ui-clear-buffer))

;;;###autoload
(defun magent-show-log ()
  "View the Magent log buffer."
  (interactive)
  (let ((buffer (magent-ui-get-log-buffer)))
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (when magent-auto-scroll
      (recenter -1))))

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
      (with-output-to-temp-buffer "*magent-queue*"
        (princ (format "Magent prompt queue (%d item(s)):\n\n" (length items)))
        (cl-loop for item in items
                 for i from 0
                 do (princ
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