;;; magent-ui.el --- User interface for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; User interface for Magent including minibuffer commands and output buffer.

;;; Code:

(require 'markdown-mode)
(require 'magent-session)
(require 'magent-agent)
(require 'magent-agent-registry)

(defvar magent-ui--md-buffer nil
  "Temporary markdown-mode buffer for fontifying AI text.")

(defun magent-ui--get-md-buffer ()
  "Return a reusable temporary `markdown-mode' buffer."
  (if (buffer-live-p magent-ui--md-buffer)
      magent-ui--md-buffer
    (setq magent-ui--md-buffer
          (with-current-buffer (generate-new-buffer " *magent-md*")
            (markdown-mode)
            (current-buffer)))))

(defun magent-ui--fontify-md-region (buf start end)
  "Fontify text in BUF between START and END using markdown-mode.
Copies the plain text to a temporary markdown-mode buffer, runs
font-lock there, then transfers face properties back.
Does nothing if the region is empty."
  (when (< start end)
    (let* ((text (with-current-buffer buf
                   (buffer-substring-no-properties start end)))
           (md-buf (magent-ui--get-md-buffer)))
      (with-current-buffer md-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text)
          (font-lock-ensure)))
      ;; Transfer face properties from the markdown buffer to the output
      ;; buffer.  Both buffers have matching text starting at position 1
      ;; (md-buf) and START (output buf), so offset = START - 1.
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (offset (1- start))
              (pos 1)
              (md-end (1+ (length text))))
          (while (< pos md-end)
            (let* ((next (with-current-buffer md-buf
                           (next-single-property-change pos 'face nil md-end)))
                   (face (with-current-buffer md-buf
                           (get-text-property pos 'face))))
              (when face
                (put-text-property (+ offset pos) (+ offset next)
                                   'font-lock-face face buf))
              (setq pos next))))))))

;;; Buffer management

(defvar-local magent-ui--output-buffer nil
  "The buffer used for Magent output.")

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
After BODY, auto-scroll if `magent-auto-scroll' is non-nil."
  (declare (indent 1))
  `(with-current-buffer ,buffer
     (let ((inhibit-read-only t))
       (goto-char (point-max))
       ,@body
       (when magent-auto-scroll
         (goto-char (point-max))
         (let ((win (get-buffer-window (current-buffer))))
           (when win
             (set-window-point win (point-max))))))))

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
  "Render all session messages into the output buffer.
Clears the buffer first, then inserts each message from the
current session in chronological order."
  (let ((session (magent-session-get)))
    (magent-ui-clear-buffer)
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
    (with-current-buffer (magent-ui-get-buffer)
      (goto-char (point-max)))))

(defun magent-ui-clear-buffer ()
  "Clear the Magent output buffer."
  (interactive)
  (with-current-buffer (magent-ui-get-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;;; Output mode

(define-derived-mode magent-output-mode special-mode "Magent"
  "Major mode for Magent output.
Markdown rendering is applied selectively to AI assistant messages only."
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
  (magent-ui--with-insert (magent-ui-get-buffer)
    (when (and (> (point) 1)
               (not (eq (char-before) ?\n))
               (not (and (>= (point) 2)
                         (eq (char-before (1- (point))) ?\n))))
      (insert "\n"))
    (insert (propertize (format "\n%s%s\n" magent-user-prompt text)
                        'font-lock-face '(bold font-lock-keyword-face)))))

(defun magent-ui-insert-assistant-message (text)
  "Insert assistant message TEXT into output buffer."
  (let ((buf (magent-ui-get-buffer)))
    (magent-ui--with-insert buf
      (insert (propertize (concat "\n" magent-assistant-prompt) 'font-lock-face 'font-lock-string-face))
      (let ((start (point)))
        (insert text)
        (magent-ui--fontify-md-region buf start (point))))))

(defun magent-ui-insert-tool-call (tool-name input)
  "Insert tool call notification into output buffer."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (insert (propertize (format "\n%s%s" magent-tool-call-prompt tool-name)
                        'font-lock-face 'font-lock-builtin-face))
    (insert (propertize (format " %s"
                                (if (stringp input)
                                    input
                                  (truncate-string-to-width
                                   (json-encode input) 100 nil nil "...")))
                        'font-lock-face 'font-lock-comment-face))))

(defun magent-ui-insert-error (error-text)
  "Insert ERROR-TEXT into output buffer."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (insert (propertize (format "\n%s%s" magent-error-prompt error-text)
                        'font-lock-face '(bold font-lock-warning-face)))))

(defvar-local magent-ui--streaming-start nil
  "Buffer position where the current streaming AI text begins.
Set by `magent-ui-start-streaming', used by
`magent-ui-finish-streaming-fontify' to apply markdown rendering.")

(defun magent-ui-start-streaming ()
  "Prepare the output buffer for a streaming response.
Inserts the assistant prompt prefix."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (insert (propertize (concat "\n" magent-assistant-prompt)
                        'font-lock-face 'font-lock-string-face))
    (setq magent-ui--streaming-start (point))))

(defun magent-ui-insert-streaming (text)
  "Insert streaming TEXT into output buffer."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (save-excursion
      (insert text))))

(defun magent-ui-finish-streaming-fontify ()
  "Apply markdown fontification to the completed streaming region."
  (let ((buf (magent-ui-get-buffer)))
    (with-current-buffer buf
      (when (and magent-ui--streaming-start
                 (< magent-ui--streaming-start (point-max)))
        (magent-ui--fontify-md-region buf magent-ui--streaming-start (point-max))
        (setq magent-ui--streaming-start nil)))))

;;; Minibuffer interface

;;;###autoload
(defun magent-prompt ()
  "Prompt for input and send to Magent agent."
  (interactive)
  (let ((input (read-string "Ask magent: ")))
    (when (not (string-blank-p input))
      (magent-ui-display-buffer)
      (magent-ui-insert-user-message input)
      (magent-ui-process input))))

;;;###autoload
(defun magent-prompt-region (begin end)
  "Send region from BEGIN to END to Magent agent."
  (interactive "r")
  (let ((input (buffer-substring begin end)))
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
        (magent-ui-display-buffer)
        (magent-ui-insert-user-message input)
        (magent-ui-process input)))))

;;; Processing

(defvar magent-ui--processing nil
  "Whether a request is currently being processed.")

(defun magent-ui-process (input)
  "Process INPUT through the agent."
  (when magent-ui--processing
    (error "Magent: Already processing a request"))
  (setq magent-ui--processing t)

  ;; FIXME(@jamie) there should be no truncating
  (magent-log "INFO processing: %s" (truncate-string-to-width input 80 nil nil "..."))

  (condition-case err
      (magent-agent-process
       input
       (lambda (response)
         (magent-ui--finish-processing response)))
    (error
     (magent-log "ERROR in process: %s" (error-message-string err))
     (magent-ui-insert-error (error-message-string err))
     (setq magent-ui--processing nil))))

(defun magent-ui--finish-processing (response)
  "Finish processing with RESPONSE.
Handles both streaming and non-streaming completion."
  (setq magent-ui--processing nil)
  (cond
   ;; Streaming mode: text was already inserted incrementally;
   ;; markdown fontification was applied at each streaming completion signal.
   ((and magent-enable-streaming (stringp response) (> (length response) 0))
    (magent-log "INFO done (streaming)"))
   ;; Streaming mode produced no text (tool-only round) — nothing to display
   ((and magent-enable-streaming (stringp response) (zerop (length response)))
    (magent-log "INFO done (streaming, tool-only round, no text)"))
   ;; Non-streaming: insert the full response
   ((and (stringp response) (> (length response) 0))
    (magent-ui-insert-assistant-message response)
    (magent-log "INFO done (non-streaming, %d chars)" (length response)))
   ;; Failure
   (t
    (magent-log "ERROR request failed or aborted")
    (magent-ui-insert-error "Request failed or was aborted"))))

;;; Session management commands

(defalias 'magent-show-ui 'magent-ui-display-buffer)

(defalias 'magent-clear-ui 'magent-ui-clear-buffer)

;;;###autoload
(defun magent-clear-session ()
  "Clear the current session."
  (interactive)
  (magent-session-reset)
  (magent-ui-clear-buffer)
  (magent-log "INFO session cleared"))

;;;###autoload
(defun magent-show-session ()
  "Show the current session summary."
  (interactive)
  (let ((session (magent-session-get)))
    (with-output-to-temp-buffer "*Magent Session*"
      (princ (magent-session-summarize session)))))

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
        (magent-log "INFO agent selected: %s" selected)))))

;;;###autoload
(defun magent-show-current-agent ()
  "Show the current agent for this session in message buffer."
  (interactive)
  (let* ((session (magent-session-get))
         (agent (magent-session-get-agent session)))
    (if agent
        (message "Magent: agent=%s (%s)"
                 (magent-agent-info-name agent)
                 (or (magent-agent-info-description agent) "no description"))
      (message "Magent: no agent selected (will use default)"))))

(provide 'magent-ui)
;;; magent-ui.el ends here
