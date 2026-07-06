;;; magent-ui.el --- Magent UI backend router  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Thin UI entry point for Magent.  The default UI backend is agent-shell via
;; the in-process ACP adapter.  The old special-mode workspace/compose UI lives
;; in magent-ui-legacy.el and is loaded only for legacy-only commands or when
;; `magent-ui-backend' is set to `legacy'.

;;; Code:

(require 'cl-lib)
(require 'spinner)
(require 'subr-x)
(require 'magent-agent-shell)
(require 'magent-config)
(require 'magent-runtime)
(require 'magent-runtime-api)
(require 'magent-session)
(require 'magent-turn)

(defvar magent--spinner)
(defvar magent-enable-logging)
(defvar magent-log-level)

(declare-function magent--ensure-initialized "magent")

(defvar magent--current-request-handle nil
  "Current active legacy request handle, if any.")

(defvar magent-ui--processing nil
  "Legacy UI processing flag kept for compatibility.")

(defvar-local magent-ui--buffer-scope nil
  "Session scope associated with a Magent UI buffer.")

(defvar-local magent-ui--fold-state nil
  "Legacy fold state for `magent-output-mode' buffers.")

(defvar magent-ui-after-input-submit-hook nil
  "Hook run after a legacy compose buffer submits input.")

(defvar magent-dwim-hook nil
  "Hook run after `magent-dwim' displays the active UI.")

(defvar magent-ui-region-active-functions nil
  "Extra region-active predicates used by the legacy UI.")

(defvar magent-ui--pending-skills-by-scope (make-hash-table :test #'equal)
  "Legacy one-shot instruction skills selected by scope.")

(defvar magent-ui--request-generation 0
  "Legacy request generation counter.")

(defvar magent-log-buffer-name "*magent-log*"
  "Name of the buffer used for Magent logging.")

(defvar magent-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") 'magent-ui-menu-or-insert-question-mark)
    (define-key map (kbd "C-g") nil)
    (define-key map (kbd "C-c C-c") 'magent-ui-submit-or-interrupt)
    (define-key map (kbd "C-c C-o") 'magent-ui-compose-from-output)
    (define-key map (kbd "TAB") 'magent-ui-toggle-section)
    (define-key map (kbd "<tab>") 'magent-ui-toggle-section)
    (define-key map (kbd "g") 'magent-ui-render-history)
    map)
  "Keymap for the legacy `magent-output-mode'.")

(defvar magent-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'magent-input-submit)
    (define-key map (kbd "C-c C-k") 'magent-compose-cancel)
    map)
  "Keymap for the legacy `magent-compose-mode'.")

;;; Logging

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

(define-derived-mode magent-log-mode fundamental-mode "MagentLog"
  "Major mode for Magent log buffers."
  (setq buffer-read-only t))

(defun magent-ui-get-log-buffer ()
  "Get or create the Magent log buffer."
  (let ((buffer (get-buffer-create magent-log-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magent-log-mode)
        (magent-log-mode)))
    buffer))

(defun magent-log (format-string &rest args)
  "Log a message to the Magent log buffer.
FORMAT-STRING and ARGS are passed to `format'."
  (let ((message (apply #'format format-string args)))
    (when (magent-ui--loggable-message-p message)
      (let ((buffer (magent-ui-get-log-buffer)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format "[%s] %s\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S")
                            message))))))))

;;; Backend routing

(defun magent-ui--agent-shell-backend-p ()
  "Return non-nil when Magent should use the agent-shell backend."
  (eq magent-ui-backend 'agent-shell))

(defun magent-ui--legacy-backend-p ()
  "Return non-nil when Magent should use the legacy UI backend."
  (eq magent-ui-backend 'legacy))

(defun magent-ui--legacy-active-p ()
  "Return non-nil when the legacy UI/turn path owns active work."
  (or magent-ui--processing
      magent--current-request-handle
      (and (fboundp 'magent-turn-processing-p)
           (magent-turn-processing-p))
      (and (fboundp 'magent-turn-pending-p)
           (magent-turn-pending-p))))

(defun magent-ui--agent-shell-dispatch-p (&optional skills agent)
  "Return non-nil when a prompt should dispatch through agent-shell.
Per-request legacy SKILLS and AGENT overrides still require the legacy
dispatcher until the agent-shell ACP path grows equivalent controls."
  (and (magent-ui--agent-shell-backend-p)
       (not skills)
       (not agent)))

(defun magent-ui-processing-p ()
  "Return non-nil if any Magent request is currently being processed."
  (or (magent-ui--legacy-active-p)
      (and (magent-ui--agent-shell-backend-p)
           (magent-agent-shell-processing-p))
      (and (fboundp 'magent-runtime-processing-p)
           (magent-runtime-processing-p))))

(defun magent-ui--legacy-available-p ()
  "Return non-nil when the legacy UI module can be loaded."
  (locate-library "magent-ui-legacy"))

(defun magent-ui--require-legacy (&optional force)
  "Load the legacy Magent UI module, or signal a user-facing error.
When FORCE is non-nil, reload the file even if `magent-ui-legacy' is already
provided.  This keeps development reloads of this thin router from leaving
legacy command shims installed over the real legacy definitions."
  (unless (if force
              (load "magent-ui-legacy" t t)
            (require 'magent-ui-legacy nil t))
    (user-error "Magent legacy UI is unavailable"))
  t)

(defun magent-ui--legacy-call (symbol &rest args)
  "Load legacy UI and call SYMBOL with ARGS."
  (let ((before (and (fboundp symbol) (symbol-function symbol))))
    (magent-ui--require-legacy)
    (let ((after (symbol-function symbol)))
      (when (eq before after)
        (magent-ui--require-legacy t)
        (setq after (symbol-function symbol)))
      (when (eq before after)
        (error "Legacy UI did not define %s" symbol))
      (apply after args))))

(defun magent-ui--legacy-call-interactively (symbol)
  "Load legacy UI and call SYMBOL interactively."
  (let ((before (and (fboundp symbol) (symbol-function symbol))))
    (magent-ui--require-legacy)
    (let ((after (symbol-function symbol)))
      (when (eq before after)
        (magent-ui--require-legacy t)
        (setq after (symbol-function symbol)))
      (when (eq before after)
        (error "Legacy UI did not define %s" symbol))
      (call-interactively after))))

(defmacro magent-ui--define-interactive-legacy-command (name)
  "Define NAME as an interactive lazy legacy command."
  `(defun ,name (&rest args)
     ,(format "Load the legacy Magent UI and call `%s'." name)
     (interactive)
     (if args
         (apply #'magent-ui--legacy-call ',name args)
       (magent-ui--legacy-call-interactively ',name))))

(defun magent-ui--snapshot-buffer-content (session &optional scope)
  "Stop persisting legacy workspace text into SESSION.
SCOPE is ignored.  This remains as a cheap compatibility hook for core code."
  (ignore scope)
  (when session
    (setf (magent-session-buffer-content session) nil))
  nil)

;;; Public commands

;;;###autoload
(defun magent-dwim ()
  "Open the active Magent UI."
  (interactive)
  (if (magent-ui--agent-shell-backend-p)
      (progn
        (magent-agent-shell-dwim)
        (run-hooks 'magent-dwim-hook))
    (magent-ui--legacy-call-interactively 'magent-dwim)))

;;;###autoload
(defun magent-send-prompt (prompt)
  "Send PROMPT to Magent agent programmatically."
  (if (magent-ui--agent-shell-dispatch-p)
      (magent-agent-shell-send-prompt prompt)
    (magent-ui--legacy-call 'magent-send-prompt prompt)))

;;;###autoload
(defun magent-prompt-region (begin end)
  "Send region from BEGIN to END to Magent agent."
  (interactive "r")
  (if (magent-ui--agent-shell-backend-p)
      (magent-agent-shell-prompt-region)
    (magent-ui--legacy-call 'magent-prompt-region begin end)))

;;;###autoload
(defun magent-ask-at-point ()
  "Ask about the symbol at point."
  (interactive)
  (if (magent-ui--agent-shell-backend-p)
      (magent-agent-shell-ask-at-point)
    (magent-ui--legacy-call-interactively 'magent-ask-at-point)))

;;;###autoload
(defun magent-diagnose-emacs ()
  "Launch a Magent diagnosis of the current Emacs session."
  (interactive)
  (magent-ui--legacy-call-interactively 'magent-diagnose-emacs))

;;;###autoload
(defun magent-doctor ()
  "Run Magent self-check and diagnose Magent-related issues."
  (interactive)
  (magent-ui--legacy-call-interactively 'magent-doctor))

;;;###autoload
(defun magent-interrupt ()
  "Interrupt the active Magent request."
  (interactive)
  (if (and (magent-ui--agent-shell-backend-p)
           (not (magent-ui--legacy-active-p)))
      (magent-agent-shell-interrupt)
    (magent-ui--legacy-call-interactively 'magent-interrupt)))

(defun magent-ui-dispatch-prompt
    (prompt &optional source display skills activate-context agent)
  "Dispatch PROMPT through the configured Magent UI backend.
SOURCE, DISPLAY, SKILLS, ACTIVATE-CONTEXT, and AGENT are the legacy dispatch
arguments used by `magent-ui-legacy'.  SKILLS or AGENT force the legacy path
until agent-shell exposes equivalent per-request controls."
  (magent--ensure-initialized)
  (unless (string-blank-p prompt)
    (if (magent-ui--agent-shell-dispatch-p skills agent)
        (magent-agent-shell-send-prompt prompt)
      (magent-ui--legacy-call
       'magent-ui-dispatch-prompt
       prompt source display skills activate-context agent))))

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

(magent-ui--define-interactive-legacy-command magent-clear-session)
(magent-ui--define-interactive-legacy-command magent-resume-session)
(magent-ui--define-interactive-legacy-command magent-toggle-read-only)
(magent-ui--define-interactive-legacy-command magent-show-transcript)
(magent-ui--define-interactive-legacy-command magent-show-agent-transcript)
(magent-ui--define-interactive-legacy-command magent-select-agent)
(magent-ui--define-interactive-legacy-command magent-show-current-agent)
(magent-ui--define-interactive-legacy-command magent-transient-menu)
(magent-ui--define-interactive-legacy-command magent-transient-agent-menu)
(magent-ui--define-interactive-legacy-command magent-transient-skill-menu)
(magent-ui--define-interactive-legacy-command magent-transient-capability-menu)
(magent-ui--define-interactive-legacy-command magent-transient-session-menu)
(magent-ui--define-interactive-legacy-command magent-transient-log-menu)
(magent-ui--define-interactive-legacy-command magent-transient-health-menu)
(magent-ui--define-interactive-legacy-command magent-transient-buffer-menu)
(magent-ui--define-interactive-legacy-command magent-ui-submit-or-interrupt)
(magent-ui--define-interactive-legacy-command magent-ui-compose-from-output)
(magent-ui--define-interactive-legacy-command magent-ui-menu-or-insert-question-mark)
(magent-ui--define-interactive-legacy-command magent-output-mode)
(magent-ui--define-interactive-legacy-command magent-compose-mode)
(magent-ui--define-interactive-legacy-command magent-input-submit)
(magent-ui--define-interactive-legacy-command magent-compose-cancel)
(magent-ui--define-interactive-legacy-command magent-ui-toggle-section)
(magent-ui--define-interactive-legacy-command magent-ui-open-compose-command)
(magent-ui--define-interactive-legacy-command magent-ui-clear-session-command)
(magent-ui--define-interactive-legacy-command magent-ui-toggle-skill-for-next-request)
(magent-ui--define-interactive-legacy-command magent-ui-clear-skills-for-next-request)
(magent-ui--define-interactive-legacy-command magent-ui-run-skill-command)
(magent-ui--define-interactive-legacy-command magent-ui-run-init-command)

;;; Legacy lazy shims

(defconst magent-ui--legacy-lazy-symbols
  '(magent-ui--abort-active-request
    magent-ui--enqueue
    magent-ui--clear-processing
    magent-ui--dispatch
    magent-ui--dispatch-submission
    magent-ui--buffer-name-base
    magent-ui--scope-buffer-label
    magent-ui--buffer-has-scope-p
    magent-ui--scope-buffer-name
    magent-ui--scope-buffer
    magent-ui--compose-buffer-name
    magent-ui-compose-buffer
    magent-ui--display-compose-buffer
    magent-ui-open-compose
    magent-ui--select-workspace
    magent-ui--maybe-close-compose-window
    magent-ui-get-buffer
    magent-ui-display-buffer
    magent-ui--context-scope
    magent-ui--ensure-scope-buffer-rendered
    magent-ui--activate-scope
    magent-ui--activate-context-session
    magent-ui--one-line
    magent-ui--dedupe-string-list
    magent-ui--scope-key
    magent-ui--pending-skills
    magent-ui--set-pending-skills
    magent-ui--clear-pending-skills
    magent-ui--toggle-pending-skill
    magent-ui--header-skills-text
    magent-ui--request-state-text
    magent-ui--header-line
    magent-ui--refresh-header-line
    magent-ui--item-text
    magent-ui--metadata-get
    magent-ui--arg-get
    magent-ui--apply-assistant-text-properties
    magent-ui--insert-rendered-text
    magent-ui--insert-labelled-block
    magent-ui--fold-state-table
    magent-ui--fragment-folded-p
    magent-ui--apply-fold-overlay
    magent-ui--fragment-marker
    magent-ui--section-prefix
    magent-ui--insert-workspace-prefix
    magent-ui--insert-workspace-line
    magent-ui--update-fragment-marker
    magent-ui--insert-fragment
    magent-ui--turn-title
    magent-ui--turn-items
    magent-ui--parse-context-fields
    magent-ui--split-user-context
    magent-ui--item-content-string
    magent-ui--turn-display-data
    magent-ui--turn-preview
    magent-ui--turn-status-label
    magent-ui--turn-start-time-label
    magent-ui--turn-section-title
    magent-ui--tool-approval-attention-p
    magent-ui--turn-alert-summary
    magent-ui--insert-workspace-text
    magent-ui--insert-context-details
    magent-ui--insert-workspace-meta-section
    magent-ui--insert-workspace-tools-section
    magent-ui--insert-workspace-turn-body
    magent-ui--tool-result-preview
    magent-ui--key-label
    magent-ui--keywordize-key
    magent-ui--line-number
    magent-ui--tool-path-entries
    magent-ui--resolve-file-path
    magent-ui--file-button-action
    magent-ui--insert-file-button
    magent-ui--insert-tool-path-details
    magent-ui--insert-tool-approval-detail
    magent-ui--grep-result-entries
    magent-ui--insert-tool-output-details
    magent-ui--insert-tool-item
    magent-ui--insert-reasoning-item
    magent-ui--insert-turn-body
    magent-ui--workspace-turns
    magent-ui-render-history
    magent-ui-clear-buffer
    magent-ui--transient-source-buffer
    magent-ui--call-in-command-context
    magent-ui--activate-command-context
    magent-ui--dispatch-from-command-context
    magent-ui--instruction-skill-names
    magent-ui--command-skill-names
    magent-ui--read-instruction-skill
    magent-transient-menu--assign-agent-keys
    magent-transient-menu--agent-suffixes
    magent-ui--setup-output-mode-map
    magent-ui--revert-buffer
    magent-ui--cancel-timers
    magent-ui--maybe-show-input-prompt
    magent-ui--insert-full-width-line
    magent-ui--insert-separator
    magent-ui--insert-header
    magent-ui-insert-user-message
    magent-ui-insert-assistant-message
    magent-ui--fold-block-at
    magent-ui--sanitize-tool-text
    magent-ui-insert-tool-call
    magent-ui-insert-tool-result
    magent-ui--agent-job-line
    magent-ui--agent-job-status-face
    magent-ui-insert-agent-job-event
    magent-ui-insert-error
    magent-ui-insert-status-line
    magent-ui-insert-capability-summary
    magent-ui--reset-streaming-state
    magent-ui-start-streaming
    magent-ui-continue-streaming
    magent-ui--flush-streaming-batch
    magent-ui-insert-streaming
    magent-ui-finish-streaming-fontify
    magent-ui--convert-markdown-region
    magent-ui--schedule-markdown-conversion
    magent-ui-insert-reasoning-start
    magent-ui-insert-reasoning-text
    magent-ui-insert-reasoning-end
    magent-ui--capture-buffer-context
    magent-ui--diagnosis-agent
    magent-ui--build-diagnosis-prompt
    magent-ui--dispatch-diagnosis
    magent-ui-process
    magent-ui--run-item
    magent-ui--finish-processing
    magent-ui--agent-jobs-for-display
    magent-ui--agent-job-choice-label
    magent-ui--metadata-value-string
    magent-ui--insert-agent-transcript-entry
    magent-ui--render-agent-transcript)
  "Legacy UI symbols that are loaded on first use.")

(dolist (symbol magent-ui--legacy-lazy-symbols)
  (unless (fboundp symbol)
    (let ((target symbol))
      (fset target
            (lambda (&rest args)
              (apply #'magent-ui--legacy-call target args))))))

(provide 'magent-ui)
;;; magent-ui.el ends here
