;;; magent-ui.el --- User interface for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; User interface for Magent including minibuffer commands and output buffer.
;; Features collapsible sections with overlay-based fold/unfold (TAB / S-TAB).

;;; Code:

(require 'markdown-mode)
(require 'spinner)
(require 'magent-session)
(require 'magent-agent)
(require 'magent-agent-registry)

(defvar magent--spinner)

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
          (condition-case err
              (font-lock-ensure)
            ((beginning-of-buffer end-of-buffer)
             (magent-log "WARN Cursor adjustment during markdown fontification: %s" err)
             nil))))
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
         ((beginning-of-buffer end-of-buffer)
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

;;; Section header helpers

(defun magent-ui--make-header (tag)
  "Return a section header string for TAG.
Format: [tag] followed by a line that auto-fills to window width."
  (concat (format "[%s]" tag)
          (propertize " " 'display '(space :align-to right-fringe)
                      'face nil)))

;;; Section overlay management

(defun magent-ui--make-section (buf start end type body-start)
  "Create a section overlay in BUF from START to END.
TYPE is a symbol (`user', `assistant', `error').
BODY-START marks where the collapsible body begins."
  (let ((ov (make-overlay start end buf)))
    (overlay-put ov 'magent-section t)
    (overlay-put ov 'magent-section-type type)
    (overlay-put ov 'magent-section-hidden nil)
    (overlay-put ov 'magent-section-body-start body-start)
    ov))

(defun magent-ui--section-at (pos)
  "Return the magent-section overlay at POS, or nil."
  (let ((ovs (overlays-at pos))
        found)
    (while (and ovs (not found))
      (when (overlay-get (car ovs) 'magent-section)
        (setq found (car ovs)))
      (setq ovs (cdr ovs)))
    found))

(defun magent-ui--all-sections ()
  "Return all magent-section overlays in the current buffer."
  (let (sections)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'magent-section)
        (push ov sections)))
    (nreverse sections)))

(defun magent-ui--hide-section (ov)
  "Hide the body of section overlay OV."
  (unless (overlay-get ov 'magent-section-hidden)
    (let* ((body-start (overlay-get ov 'magent-section-body-start))
           (end (overlay-end ov))
           (inv-ov (make-overlay body-start end)))
      (overlay-put inv-ov 'invisible t)
      (overlay-put inv-ov 'magent-section-body t)
      (overlay-put inv-ov 'evaporate t)
      (overlay-put ov 'magent-section-hidden t)
      (overlay-put ov 'magent-section-body-overlay inv-ov))))

(defun magent-ui--show-section (ov)
  "Show (unfold) the body of section overlay OV."
  (when (overlay-get ov 'magent-section-hidden)
    (let ((inv-ov (overlay-get ov 'magent-section-body-overlay)))
      (when inv-ov
        (delete-overlay inv-ov)))
    (overlay-put ov 'magent-section-hidden nil)
    (overlay-put ov 'magent-section-body-overlay nil)))

;;; Folding API

(defun magent-ui-toggle-section ()
  "Toggle fold on the section at point."
  (interactive)
  (let ((ov (magent-ui--section-at (point))))
    (when ov
      (if (overlay-get ov 'magent-section-hidden)
          (magent-ui--show-section ov)
        (magent-ui--hide-section ov)))))

(defun magent-ui-fold-all ()
  "Fold all sections in the output buffer."
  (interactive)
  (dolist (ov (magent-ui--all-sections))
    (magent-ui--hide-section ov)))

(defun magent-ui-unfold-all ()
  "Unfold all sections in the output buffer."
  (interactive)
  (dolist (ov (magent-ui--all-sections))
    (magent-ui--show-section ov)))

(defvar magent-ui--all-folded nil
  "Buffer-local toggle state for S-TAB fold-all/unfold-all.")

(defun magent-ui-toggle-all-sections ()
  "Toggle between folding all and unfolding all sections."
  (interactive)
  (if magent-ui--all-folded
      (progn (magent-ui-unfold-all)
             (setq magent-ui--all-folded nil))
    (magent-ui-fold-all)
    (setq magent-ui--all-folded t)))

;;; Output mode

(defvar magent-output-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'magent-ui-toggle-section)
    (define-key map (kbd "<backtab>") #'magent-ui-toggle-all-sections)
    map)
  "Keymap for `magent-output-mode'.")

(define-derived-mode magent-output-mode special-mode "Magent"
  "Major mode for Magent output.
Markdown rendering is applied selectively to AI assistant messages only.
\\<magent-output-mode-map>
Press \\[magent-ui-toggle-section] to fold/unfold the section at point.
Press \\[magent-ui-toggle-all-sections] to fold/unfold all sections."
  (visual-line-mode 1)
  (setq-local display-fill-column-indicator-column nil)
  (setq-local magent-ui--all-folded nil))

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
  "Insert user message TEXT into output buffer with section header."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (let ((section-start (point)))
      ;; Blank line separator (except at buffer start)
      (when (> (point) 1)
        (insert "\n"))
      (setq section-start (point))
      ;; Header
      (let ((header (magent-ui--make-header magent-user-prompt)))
        (insert (propertize header 'font-lock-face 'font-lock-keyword-face) "\n"))
      (let ((body-start (point)))
        ;; Body
        (insert (propertize text 'font-lock-face '(bold)))
        (insert "\n")
        ;; Create section overlay
        (magent-ui--make-section (current-buffer) section-start (point) 'user body-start)))))

(defun magent-ui-insert-assistant-message (text)
  "Insert assistant message TEXT into output buffer with section header."
  (let ((buf (magent-ui-get-buffer)))
    (magent-ui--with-insert buf
      (let ((section-start (point)))
        ;; Blank line separator
        (when (> (point) 1)
          (insert "\n"))
        (setq section-start (point))
        ;; Header
        (let ((header (magent-ui--make-header magent-assistant-prompt)))
          (insert (propertize header 'font-lock-face 'font-lock-string-face) "\n"))
        (let ((body-start (point))
              (text-start (point)))
          ;; Body
          (insert text)
          (magent-ui--fontify-md-region buf text-start (point))
          (insert "\n")
          ;; Create section overlay
          (magent-ui--make-section buf section-start (point) 'assistant body-start))))))

(defvar-local magent-ui--tool-section-start nil
  "Buffer position where the current tool section started.
Set by `magent-ui-insert-tool-call', used by
`magent-ui-insert-tool-result' to create the section overlay.")

(defvar-local magent-ui--tool-body-start nil
  "Buffer position where the current tool section body starts.
Set by `magent-ui-insert-tool-call' right after the header line.")

(defun magent-ui-insert-tool-call (tool-name input)
  "Insert tool call notification into output buffer.
Starts a collapsible tool section with a `[tool]' header."
  (magent-ui--with-insert (magent-ui-get-buffer)
    ;; Blank line separator (except at buffer start)
    (when (> (point) 1)
      (insert "\n"))
    (let ((section-start (point)))
      (let ((header (magent-ui--make-header
                     (format "%s: %s" magent-tool-call-prompt tool-name))))
        (insert (propertize header 'font-lock-face 'font-lock-builtin-face) "\n"))
      (setq magent-ui--tool-section-start section-start)
      (setq magent-ui--tool-body-start (point))
      (let ((input-str (if (stringp input)
                           input
                         (truncate-string-to-width
                          (json-encode input) 100 nil nil "..."))))
        (insert (propertize (format "%s\n" input-str)
                            'font-lock-face 'font-lock-comment-face))))))

(defun magent-ui-insert-tool-result (_tool-name result)
  "Insert tool RESULT for _TOOL-NAME into output buffer.
Closes the tool section started by `magent-ui-insert-tool-call'."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (let ((result-str (truncate-string-to-width
                       (if (stringp result) result (format "%s" result))
                       120 nil nil "...")))
      (insert (propertize (format "-> %s\n" result-str)
                          'font-lock-face 'font-lock-comment-face))
      ;; Create section overlay spanning tool-call header + body (args + result)
      (when magent-ui--tool-section-start
        (magent-ui--make-section (current-buffer)
                                 magent-ui--tool-section-start (point)
                                 'tool-call magent-ui--tool-body-start)
        (setq magent-ui--tool-section-start nil)
        (setq magent-ui--tool-body-start nil)))))

(defun magent-ui-insert-error (error-text)
  "Insert ERROR-TEXT into output buffer with section header."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (let ((section-start (point)))
      (when (> (point) 1)
        (insert "\n"))
      (setq section-start (point))
      ;; Header
      (let ((header (magent-ui--make-header magent-error-prompt)))
        (insert (propertize header 'font-lock-face '(bold font-lock-warning-face)) "\n"))
      (let ((body-start (point)))
        (insert (propertize error-text 'font-lock-face '(bold font-lock-warning-face)))
        (insert "\n")
        (magent-ui--make-section (current-buffer) section-start (point) 'error body-start)))))

;;; Streaming support

(defvar-local magent-ui--streaming-start nil
  "Buffer position where the current streaming AI text begins.
Set by `magent-ui-start-streaming', used by
`magent-ui-finish-streaming-fontify' to apply markdown rendering.")

(defvar-local magent-ui--streaming-section-start nil
  "Buffer position where the current streaming section begins.
Used to create the section overlay when streaming finishes.")

(defvar-local magent-ui--streaming-has-text nil
  "Non-nil if any text was inserted via `magent-ui-insert-streaming'.
Used to distinguish empty streaming rounds from rounds where tool
lines were inserted after the header by other functions.")

(defun magent-ui-start-streaming ()
  "Prepare the output buffer for a streaming response.
Inserts the assistant section header."
  (magent-ui--with-insert (magent-ui-get-buffer)
    ;; Blank line separator
    (when (> (point) 1)
      (insert "\n"))
    (setq magent-ui--streaming-section-start (point))
    ;; Header
    (let ((header (magent-ui--make-header magent-assistant-prompt)))
      (insert (propertize header 'font-lock-face 'font-lock-string-face) "\n"))
    (setq magent-ui--streaming-start (point))
    (setq magent-ui--streaming-has-text nil)))

(defun magent-ui-insert-streaming (text)
  "Insert streaming TEXT into output buffer."
  (magent-ui--with-insert (magent-ui-get-buffer)
    (save-excursion
      (insert text))
    (setq magent-ui--streaming-has-text t)))

(defun magent-ui-finish-streaming-fontify ()
  "Apply markdown fontification to the completed streaming region.
Also creates the section overlay for the completed assistant block.
If no text was streamed (tool-only round), removes the orphaned header."
  (let ((buf (magent-ui-get-buffer)))
    (with-current-buffer buf
      (when magent-ui--streaming-start
        (condition-case nil
            (if (not magent-ui--streaming-has-text)
                ;; No text was streamed — remove the orphaned header
                (when magent-ui--streaming-section-start
                  (let ((inhibit-read-only t))
                    (delete-region magent-ui--streaming-section-start
                                   (min (1+ magent-ui--streaming-start) (point-max)))))
              ;; Text was streamed — fontify and create section overlay
              (magent-ui--fontify-md-region buf magent-ui--streaming-start (point-max))
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (unless (eq (char-before) ?\n)
                  (insert "\n")))
              (when magent-ui--streaming-section-start
                (magent-ui--make-section buf
                                         magent-ui--streaming-section-start
                                         (point-max)
                                         'assistant
                                         magent-ui--streaming-start)))
          ((beginning-of-buffer end-of-buffer)
           (magent-log "DEBUG Suppressed cursor error in streaming fontify")
           nil))
        (setq magent-ui--streaming-start nil)
        (setq magent-ui--streaming-section-start nil)
        (setq magent-ui--streaming-has-text nil)))))

;;; Minibuffer interface

;;;###autoload
(defun magent-prompt ()
  "Prompt for input and send to Magent agent."
  (interactive)
  (magent--ensure-initialized)  ; Lazy init on first use
  (let ((input (read-string "Ask magent: ")))
    (when (not (string-blank-p input))
      (magent-ui-display-buffer)
      (magent-ui-insert-user-message input)
      (magent-ui-process input))))

;;;###autoload
(defun magent-prompt-region (begin end)
  "Send region from BEGIN to END to Magent agent."
  (interactive "r")
  (magent--ensure-initialized)  ; Lazy init on first use
  (let ((input (buffer-substring begin end)))
    (magent-ui-display-buffer)
    (magent-ui-insert-user-message (format "[Region] %s" input))
    (magent-ui-process input)))

;;;###autoload
(defun magent-ask-at-point ()
  "Ask about the symbol at point."
  (interactive)
  (magent--ensure-initialized)  ; Lazy init on first use
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
  (spinner-start magent--spinner)

  ;; FIXME(@jamie) there should be no truncating
  (magent-log "INFO processing: %s" (truncate-string-to-width input magent-ui-log-truncate-length nil nil "..."))

  (condition-case err
      (magent-agent-process
       input
       (lambda (response)
         (magent-ui--finish-processing response)))
    (error
     (magent-log "ERROR in process: %s" (error-message-string err))
     (magent-ui-insert-error (error-message-string err))
     (setq magent-ui--processing nil)
     (spinner-stop magent--spinner))))

(defun magent-ui--finish-processing (response)
  "Finish processing with RESPONSE.
Handles both streaming and non-streaming completion."
  (setq magent-ui--processing nil)
  (spinner-stop magent--spinner)
  (cond
   ;; Streaming mode: text was already inserted incrementally;
   ;; markdown fontification was applied at each streaming completion signal.
   ((and magent-enable-streaming (stringp response) (> (length response) 0))
    (magent-log "INFO done (streaming)"))
   ;; Streaming mode produced no text (tool-only round) -- nothing to display
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

;;;###autoload
(defun magent-clear-session ()
  "Clear the current session."
  (interactive)
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

;;; Agent selection commands

;;;###autoload
(defun magent-select-agent ()
  "Select an agent for the current session."
  (interactive)
  (magent--ensure-initialized)  ; Lazy init on first use
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
  (magent--ensure-initialized)  ; Lazy init on first use
  (let* ((session (magent-session-get))
         (agent (magent-session-get-agent session)))
    (if agent
        (message "Magent: agent=%s (%s)"
                 (magent-agent-info-name agent)
                 (or (magent-agent-info-description agent) "no description"))
      (message "Magent: no agent selected (will use default)"))))

(provide 'magent-ui)
;;; magent-ui.el ends here
