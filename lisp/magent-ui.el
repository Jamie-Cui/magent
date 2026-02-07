;;; magent-ui.el --- User interface for OpenCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; User interface for OpenCode including minibuffer commands and output buffer.

;;; Code:

(require 'magent-session)
(require 'magent-agent)
(require 'magent-agent-registry)

;;; Buffer management

(defvar-local magent-ui--output-buffer nil
  "The buffer used for OpenCode output.")

(defvar magent-log-buffer-name "*magent-log*"
  "Name of the buffer used for Magent logging.")

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
  (with-current-buffer (magent-ui-get-log-buffer)
    (let ((inhibit-read-only t)
          (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
      (goto-char (point-max))
      (insert (format "[%s] %s\n" timestamp (apply #'format format-string args))))
    (when magent-auto-scroll
      (goto-char (point-max))
      (recenter -1))))

(defun magent-ui-get-buffer ()
  "Get or create the OpenCode output buffer."
  (let ((buffer (get-buffer-create magent-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magent-output-mode)
        (magent-output-mode)))
    buffer))

(defun magent-ui-display-buffer ()
  "Display the OpenCode output buffer."
  (let ((buffer (magent-ui-get-buffer)))
    (display-buffer buffer
                   '((display-buffer-reuse-window
                      display-buffer-in-direction)
                     (direction . bottom)
                     (window-height . 0.3))))
  (select-window (get-buffer-window magent-buffer-name)))

(defun magent-ui-clear-buffer ()
  "Clear the OpenCode output buffer."
  (interactive)
  (with-current-buffer (magent-ui-get-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;;; Output mode

(define-derived-mode magent-output-mode fundamental-mode "OpenCode"
  "Major mode for OpenCode output."
  (setq buffer-read-only t)
  (visual-line-mode 1)
  (setq-local display-fill-column-indicator-column nil))

(define-derived-mode magent-log-mode fundamental-mode "MagentLog"
  "Major mode for Magent log buffer."
  (setq buffer-read-only t)
  (visual-line-mode 1)
  (setq-local display-fill-column-indicator-column nil)
  (setq-local font-lock-defaults
              '((
                 ;; Timestamps
                 ("\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)\\]"
                  0 font-lock-comment-face)
                 ;; Log levels
                 ("\\<\\(ERROR\\|WARNING\\|INFO\\|DEBUG\\)\\>"
                  0 font-lock-keyword-face)))))

;;; Rendering functions

(defun magent-ui-insert-user-message (text)
  "Insert user message TEXT into output buffer."
  (with-current-buffer (magent-ui-get-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (format "\n❯ %s\n" text)
                         'face '(bold font-lock-keyword-face)))
      (when magent-auto-scroll
        (goto-char (point-max))
        (recenter -1)))))

(defun magent-ui-insert-assistant-message (text)
  "Insert assistant message TEXT into output buffer."
  (with-current-buffer (magent-ui-get-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize "\n🤖 " 'face 'font-lock-string-face))
      (insert (magent-ui--render-markdown text))
      (insert "\n")
      (when magent-auto-scroll
        (goto-char (point-max))
        (recenter -1)))))

(defun magent-ui-insert-tool-call (tool-name input)
  "Insert tool call notification into output buffer."
  (with-current-buffer (magent-ui-get-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (format "\n🔧 %s" tool-name)
                         'face 'font-lock-builtin-face))
      (insert (propertize (format " %s\n"
                                 (if (stringp input)
                                     input
                                   (truncate-string-to-width
                                    (json-encode input) 100 nil nil "...")))
                         'face 'font-lock-comment-face))
      (when magent-auto-scroll
        (goto-char (point-max))
        (recenter -1)))))

(defun magent-ui-insert-error (error-text)
  "Insert ERROR-TEXT into output buffer."
  (with-current-buffer (magent-ui-get-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (format "\n⚠ Error: %s\n" error-text)
                         'face '(bold font-lock-warning-face)))
      (when magent-auto-scroll
        (goto-char (point-max))
        (recenter -1)))))

(defun magent-ui-insert-streaming (text)
  "Insert streaming TEXT into output buffer."
  (with-current-buffer (magent-ui-get-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert text))
      (when magent-auto-scroll
        (goto-char (point-max))
        (recenter -1)))))

;;; Basic markdown rendering

(defun magent-ui--render-markdown (text)
  "Basic markdown rendering for TEXT.
Handles code blocks, bold, and inline code."
  (let ((result text))
    ;; Code blocks
    (setq result (replace-regexp-in-string
                  "```\\([^\n]*\\)\n\\([^`]*?\\)\n```"
                  (lambda (match)
                    (let ((lang (match-string 1 match))
                          (code (match-string 2 match)))
                      (propertize code
                                 'face 'font-lock-constant-face
                                 'display `(margin left-margin ,(concat " " lang "\n")))))
                  result t t))
    ;; Inline code
    (setq result (replace-regexp-in-string
                  "`\\([^`]+\\)`"
                  (lambda (match)
                    (propertize (match-string 1 match)
                               'face 'font-lock-constant-face))
                  result t t))
    ;; Bold
    (setq result (replace-regexp-in-string
                  "\\*\\*\\([^*]+\\)\\*\\*"
                  (lambda (match)
                    (propertize (match-string 1 match)
                               'face 'bold))
                  result t t))
    result))

;;; Minibuffer interface

;;;###autoload
(defun magent-prompt ()
  "Prompt for input and send to OpenCode agent."
  (interactive)
  (let ((input (read-string "OpenCode: ")))
    (when (not (string-blank-p input))
      (magent-ui-clear-buffer)
      (magent-ui-display-buffer)
      (magent-ui-insert-user-message input)
      (magent-ui-process input))))

;;;###autoload
(defun magent-prompt-region (begin end)
  "Send region from BEGIN to END to OpenCode agent."
  (interactive "r")
  (let ((input (buffer-substring begin end)))
    (magent-ui-clear-buffer)
    (magent-ui-display-buffer)
    (magent-ui-insert-user-message (format "[Region] %s" input))
    (magent-ui-process input)))

;;;###autoload
(defun magent-ask-at-point ()
  "Ask about the symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (let ((input (format "Explain this code: %s" symbol)))
        (magent-ui-clear-buffer)
        (magent-ui-display-buffer)
        (magent-ui-insert-user-message input)
        (magent-ui-process input)))))

;;; Processing

(defvar magent-ui--processing nil
  "Whether a request is currently being processed.")

(defun magent-ui-process (input)
  "Process INPUT through the agent."
  (when magent-ui--processing
    (error "Already processing a request"))
  (setq magent-ui--processing t)
  (message "OpenCode: Processing...")

  (condition-case err
      (progn
        ;; Add a loading indicator
        (with-current-buffer (magent-ui-get-buffer)
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (insert (propertize "▌" 'face 'font-lock-comment-face)))))

        (magent-agent-process
         input
         (lambda (response)
           (magent-ui--finish-processing response))))
    (error
     (magent-ui-insert-error (error-message-string err))
     (setq magent-ui--processing nil)
     (message "OpenCode: Error"))))

(defun magent-ui--finish-processing (response)
  "Finish processing with RESPONSE."
  (setq magent-ui--processing nil)
  ;; Remove loading indicator
  (with-current-buffer (magent-ui-get-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (when (looking-back "▌" 1)
          (delete-char -1)))))
  (magent-ui-insert-assistant-message response)
  (message "OpenCode: Done"))

;;; Session management commands

;;;###autoload
(defun magent-clear-session ()
  "Clear the current session."
  (interactive)
  (magent-session-reset)
  (magent-ui-clear-buffer)
  (message "OpenCode: Session cleared"))

;;;###autoload
(defun magent-show-session ()
  "Show the current session summary."
  (interactive)
  (let ((session (magent-session-get)))
    (with-output-to-temp-buffer "*OpenCode Session*"
      (princ (magent-session-summarize session)))))

;;;###autoload
(defun magent-view-log ()
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
      (erase-buffer)))
  (message "Magent: Log cleared"))

;;; Agent selection commands

;;;###autoload
(defun magent-select-agent ()
  "Select an agent for the current session."
  (interactive)
  (let* ((agents (magent-agent-registry-primary-agents))
         (agent-names (mapcar #'magent-agent-info-name agents))
         (selected (completing-read "Select agent: " agent-names nil t)))
    (when selected
      (let* ((agent-info (magent-agent-registry-get selected))
             (session (magent-session-get)))
        (magent-session-set-agent session agent-info)
        (message "OpenCode: Agent set to %s" selected)))))

;;;###autoload
(defun magent-show-current-agent ()
  "Show the current agent for this session."
  (interactive)
  (let* ((session (magent-session-get))
         (agent (magent-session-get-agent session)))
    (if agent
        (message "OpenCode: Current agent is %s (%s)"
                 (magent-agent-info-name agent)
                 (or (magent-agent-info-description agent) "no description"))
      (message "OpenCode: No agent selected (will use default)"))))

(provide 'magent-ui)
;;; magent-ui.el ends here
