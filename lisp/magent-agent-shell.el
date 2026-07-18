;;; magent-agent-shell.el --- agent-shell UI backend for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; This module is the agent-shell-specific UI backend.  Magent exposes an
;; in-process ACP client, while agent-shell owns the interactive shell UI.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'magent-acp)
(require 'magent-command)
(require 'magent-config)
(require 'magent-runtime)
(require 'magent-runtime-api)
(require 'magent-session)
(require 'magent-skills)

(defvar gptel-model)
(defvar agent-shell--state)
(defvar agent-shell-agent-configs)
(defvar agent-shell-preferred-agent-config)
(defvar agent-shell-session-strategy)
(defvar comint-last-prompt)
(defvar shell-maker--busy)
(defvar shell-maker--request-process)

(declare-function magent--ensure-initialized "magent")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell--dwim "agent-shell")
(declare-function agent-shell--process-pending-request "agent-shell")
(declare-function agent-shell--send-command "agent-shell")
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell-queue-request "agent-shell")
(declare-function agent-shell-start "agent-shell")
(declare-function shell-maker-busy "shell-maker")
(declare-function magent-acp--runtime-session-by-id "magent-acp")
(declare-function magent-acp--client-session-scope "magent-acp")

(defconst magent-agent-shell--identifier 'magent
  "agent-shell config identifier used by Magent.")

(defvar-local magent-agent-shell--pending-skill-names nil
  "Instruction skills selected for the next Magent agent-shell prompt.")

(defvar-local magent-agent-shell--prompt-skill-queue nil
  "Per-prompt instruction skills waiting for `agent-shell--send-command'.")

(defvar-local magent-agent-shell--owns-session-strategy-p nil
  "Non-nil when Magent installed the buffer-local session strategy.")

(defun magent-agent-shell--ensure-loaded ()
  "Load `agent-shell' before using its runtime API."
  (require 'agent-shell))

(defun magent-agent-shell--model-id ()
  "Return the current gptel model id for agent-shell display."
  (format "%s" (or (and (boundp 'gptel-model) gptel-model) "gptel")))

(defun magent-agent-shell--instruction-skill-names ()
  "Return sorted instruction skill names."
  (sort (copy-sequence (magent-skills-list-by-type 'instruction)) #'string<))

(defun magent-agent-shell--read-instruction-skill (prompt)
  "Read an instruction skill name with PROMPT."
  (let ((names (magent-agent-shell--instruction-skill-names)))
    (unless names
      (user-error "Magent: no instruction skills are registered"))
    (completing-read prompt names nil t)))

(defun magent-agent-shell--prepare-skill-context ()
  "Load project-local skill definitions for the current command context."
  (magent-runtime-prepare-command-context))

(defun magent-agent-shell--make-client (buffer)
  "Create Magent's in-process ACP client for BUFFER.
Install Magent's session strategy before agent-shell snapshots its buffer-local
startup settings.  Preserve an existing buffer-local value so explicit or
directory-local frontend configuration keeps precedence."
  (with-current-buffer buffer
    (when (or magent-agent-shell--owns-session-strategy-p
              (not (local-variable-p 'agent-shell-session-strategy)))
      (setq-local magent-agent-shell--owns-session-strategy-p t)
      (setq-local agent-shell-session-strategy
                  magent-agent-shell-session-strategy)))
  (magent-acp-make-client buffer))

;;;###autoload
(defun magent-agent-shell-make-config ()
  "Return the agent-shell configuration for Magent."
  (magent-agent-shell--ensure-loaded)
  (agent-shell-make-agent-config
   :identifier magent-agent-shell--identifier
   :mode-line-name "Magent"
   :buffer-name "Magent"
   :shell-prompt "Magent> "
   :shell-prompt-regexp "Magent> "
   :welcome-function #'magent-agent-shell--welcome-message
   :client-maker #'magent-agent-shell--make-client
   :default-model-id #'magent-agent-shell--model-id
   :default-session-mode-id (lambda () magent-default-agent)
   :install-instructions "Magent uses an in-process ACP client; no external command is required."))

(defun magent-agent-shell--welcome-message (_config)
  "Return the welcome message for Magent agent-shell buffers."
  "\nMagent\n")

(defun magent-agent-shell-ensure-config ()
  "Ensure Magent's config maker is registered with agent-shell.
Return Magent's identifier for use as `agent-shell-preferred-agent-config'."
  (magent-agent-shell--ensure-loaded)
  (setq agent-shell-agent-configs
        (cons #'magent-agent-shell-make-config
              (delq #'magent-agent-shell-make-config
                    agent-shell-agent-configs)))
  magent-agent-shell--identifier)

(defmacro magent-agent-shell--with-config (&rest body)
  "Evaluate BODY with Magent selected as the preferred agent-shell config."
  (declare (indent 0))
  `(let ((agent-shell-preferred-agent-config
          (magent-agent-shell-ensure-config))
         (agent-shell-session-strategy
          magent-agent-shell-session-strategy))
     (magent-runtime-ensure-initialized)
     ,@body))

(defun magent-agent-shell--directory-key (directory)
  "Return normalized DIRECTORY key for project comparisons."
  (file-name-as-directory (expand-file-name directory)))

(defun magent-agent-shell--magent-buffer-p (buffer)
  "Return non-nil when BUFFER is a Magent agent-shell buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'agent-shell-mode)
              (boundp 'agent-shell--state)
              (eq (map-nested-elt agent-shell--state
                                  '(:agent-config :identifier))
                  magent-agent-shell--identifier)))))

;; Remove the shared-history compaction advice installed by an earlier build.
(advice-remove 'shell-maker--write-input-ring-history
               'magent-agent-shell--write-sanitized-input-history)

(defun magent-agent-shell--runtime-session (&optional shell-buffer)
  "Return the runtime session for SHELL-BUFFER, or nil."
  (let ((buffer (or shell-buffer (current-buffer))))
    (when (magent-agent-shell--magent-buffer-p buffer)
      (with-current-buffer buffer
        (when-let* ((session-id (map-nested-elt agent-shell--state
                                                '(:session :id))))
          (let* ((client (map-elt agent-shell--state :client))
                 (scope
                  (or (and client
                           (magent-acp--client-session-scope
                            client session-id))
                      ;; Compatibility fallback for an agent-shell client that
                      ;; survived a source reload before scope bindings existed.
                      (magent-session-scope-from-directory
                       default-directory))))
          (or (magent-runtime-session-from-id session-id scope)
                (magent-acp--runtime-session-by-id session-id scope))))))))

(defun magent-agent-shell--set-runtime-pending-skills
    (runtime-session skills)
  "Set RUNTIME-SESSION pending SKILLS."
  (setf (magent-runtime-session-pending-skills runtime-session)
        (magent-skills-dedupe-names skills)))

(defun magent-agent-shell--sync-buffer-pending-skills
    (shell-buffer runtime-session)
  "Move SHELL-BUFFER pending skills into RUNTIME-SESSION when needed."
  (with-current-buffer shell-buffer
    (when magent-agent-shell--pending-skill-names
      (magent-agent-shell--set-runtime-pending-skills
       runtime-session
       (append (magent-runtime-session-pending-skills runtime-session)
               magent-agent-shell--pending-skill-names))
      (setq magent-agent-shell--pending-skill-names nil))))

(defun magent-agent-shell--pending-skills (&optional shell-buffer)
  "Return pending instruction skills for SHELL-BUFFER."
  (let ((buffer (or shell-buffer (current-buffer))))
    (if-let* ((runtime-session
               (magent-agent-shell--runtime-session buffer)))
        (progn
          (magent-agent-shell--sync-buffer-pending-skills
           buffer runtime-session)
          (magent-runtime-session-pending-skills runtime-session))
      (with-current-buffer buffer
        magent-agent-shell--pending-skill-names))))

(defun magent-agent-shell--set-pending-skills (shell-buffer skills)
  "Set SHELL-BUFFER pending instruction SKILLS."
  (let ((skills (magent-skills-dedupe-names skills)))
    (if-let* ((runtime-session
               (magent-agent-shell--runtime-session shell-buffer)))
        (magent-agent-shell--set-runtime-pending-skills runtime-session skills)
      (with-current-buffer shell-buffer
        (setq magent-agent-shell--pending-skill-names skills)))))

(defun magent-agent-shell--clear-pending-skills (&optional shell-buffer)
  "Clear pending instruction skills for SHELL-BUFFER."
  (let ((buffer (or shell-buffer (current-buffer))))
    (if-let* ((runtime-session
               (magent-agent-shell--runtime-session buffer)))
        (magent-runtime-session-clear-pending-skills runtime-session)
      (with-current-buffer buffer
        (setq magent-agent-shell--pending-skill-names nil)))))

(defun magent-agent-shell--toggle-pending-skill
    (shell-buffer skill-name)
  "Toggle SKILL-NAME for SHELL-BUFFER's next prompt.
Return non-nil when SKILL-NAME is selected after toggling."
  (let ((skills (magent-agent-shell--pending-skills shell-buffer)))
    (if (member skill-name skills)
        (progn
          (magent-agent-shell--set-pending-skills
           shell-buffer (remove skill-name skills))
          nil)
      (magent-agent-shell--set-pending-skills
       shell-buffer (append skills (list skill-name)))
      t)))

(defun magent-agent-shell--queue-prompt-skills
    (shell-buffer prompt skills)
  "Queue SKILLS for PROMPT in SHELL-BUFFER."
  (when skills
    (with-current-buffer shell-buffer
      (setq magent-agent-shell--prompt-skill-queue
            (append magent-agent-shell--prompt-skill-queue
                    (list (list :prompt prompt
                                :skills
                                (magent-skills-dedupe-names
                                 skills))))))))

(defun magent-agent-shell--pop-prompt-skills
    (shell-buffer prompt)
  "Return queued skills for PROMPT in SHELL-BUFFER, or nil."
  (with-current-buffer shell-buffer
    (let ((entry (car magent-agent-shell--prompt-skill-queue)))
      (when (and entry
                 (equal prompt (plist-get entry :prompt)))
        (setq magent-agent-shell--prompt-skill-queue
              (cdr magent-agent-shell--prompt-skill-queue))
        (plist-get entry :skills)))))

(defun magent-agent-shell--prepare-command-skills
    (prompt shell-buffer)
  "Prepare runtime pending skills for PROMPT in SHELL-BUFFER."
  (when (magent-agent-shell--magent-buffer-p shell-buffer)
    (when-let* ((runtime-session
                 (magent-agent-shell--runtime-session shell-buffer)))
      (if-let* ((skills (magent-agent-shell--pop-prompt-skills
                         shell-buffer prompt)))
          (magent-agent-shell--set-runtime-pending-skills
           runtime-session skills)
        (when-let* ((skills
                     (with-current-buffer shell-buffer
                       magent-agent-shell--pending-skill-names)))
          (magent-agent-shell--set-runtime-pending-skills
           runtime-session skills)
          (with-current-buffer shell-buffer
            (setq magent-agent-shell--pending-skill-names nil)))))))

(defun magent-agent-shell--send-command (orig &rest args)
  "Attach Magent prompt skills before delegating to ORIG with ARGS."
  (let ((prompt (plist-get args :prompt))
        (shell-buffer (plist-get args :shell-buffer)))
    (when (and prompt shell-buffer)
      (magent-agent-shell--prepare-command-skills prompt shell-buffer))
    (apply orig args)))

(unless (advice-member-p #'magent-agent-shell--send-command
                         'agent-shell--send-command)
  (advice-add 'agent-shell--send-command :around
              #'magent-agent-shell--send-command))

(defun magent-agent-shell--trim-trailing-input-whitespace ()
  "Delete trailing whitespace from the active Magent shell input."
  (when (and (magent-agent-shell--magent-buffer-p (current-buffer))
             (boundp 'comint-last-prompt)
             comint-last-prompt)
    (let ((start (marker-position (cdr comint-last-prompt)))
          (end (point-max)))
      (when (and start (<= start end))
        (save-excursion
          (goto-char end)
          (skip-chars-backward " \t\n\r" start)
          (when (< (point) end)
            (let ((inhibit-read-only t))
              (delete-region (point) end))))))))

(defun magent-agent-shell--shell-maker-submit (orig &rest args)
  "Trim Magent prompt input before delegating to ORIG with ARGS."
  (magent-agent-shell--trim-trailing-input-whitespace)
  (apply orig args))

(unless (advice-member-p #'magent-agent-shell--shell-maker-submit
                         'shell-maker-submit)
  (advice-add 'shell-maker-submit :around
              #'magent-agent-shell--shell-maker-submit))

(defun magent-agent-shell--blank-current-line-p ()
  "Return non-nil when the current buffer line is blank."
  (string-blank-p
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun magent-agent-shell--get-current-line-context (orig &rest args)
  "Suppress empty current-line context before delegating to ORIG.

`agent-shell--get-current-line-context' builds line context by
temporarily activating the current line as a region.  On a blank
line this creates an empty BOL region, which agent-shell 0.57.1
formats as an inverted range such as README.org:3-2.  Blank line
context is not useful for Magent, so skip it."
  (unless (magent-agent-shell--blank-current-line-p)
    (apply orig args)))

(unless (advice-member-p #'magent-agent-shell--get-current-line-context
                         'agent-shell--get-current-line-context)
  (advice-add 'agent-shell--get-current-line-context :around
              #'magent-agent-shell--get-current-line-context))

(defun magent-agent-shell--buffers ()
  "Return live Magent agent-shell buffers without creating side effects."
  (seq-filter #'magent-agent-shell--magent-buffer-p (buffer-list)))

(defun magent-agent-shell--project-buffer ()
  "Return the Magent agent-shell buffer for the current project, or nil."
  (let ((project-key (magent-agent-shell--directory-key (agent-shell-cwd))))
    (seq-find
     (lambda (buffer)
       (with-current-buffer buffer
         (equal project-key
                (magent-agent-shell--directory-key default-directory))))
     (magent-agent-shell--buffers))))

(defun magent-agent-shell--buffer (&optional no-create)
  "Return the current project Magent agent-shell buffer.
When NO-CREATE is non-nil, return nil instead of creating one."
  (magent-agent-shell--with-config
    (or (and (magent-agent-shell--magent-buffer-p (current-buffer))
             (current-buffer))
        (magent-agent-shell--project-buffer)
        (unless no-create
          (agent-shell-start :config (magent-agent-shell-make-config))))))

(defun magent-agent-shell--state-option-complete-p (option-key complete-key)
  "Return non-nil when OPTION-KEY either is unset or COMPLETE-KEY is done."
  (let ((getter (map-nested-elt agent-shell--state
                                (list :agent-config option-key))))
    (or (not (functionp getter))
        (not (ignore-errors (funcall getter)))
        (map-elt agent-shell--state complete-key))))

(defun magent-agent-shell--bootstrap-complete-p ()
  "Return non-nil when the current Magent agent-shell finished bootstrap."
  (and (map-elt agent-shell--state :initialized)
       (map-nested-elt agent-shell--state '(:session :id))
       (or (not (map-elt agent-shell--state :needs-authentication))
           (map-elt agent-shell--state :authenticated))
       (magent-agent-shell--state-option-complete-p
        :default-model-id :set-model)
       (magent-agent-shell--state-option-complete-p
        :default-session-mode-id :set-session-mode)))

(defun magent-agent-shell--stale-busy-p ()
  "Return non-nil when current buffer is stuck busy without live work."
  (and (magent-agent-shell--magent-buffer-p (current-buffer))
       (bound-and-true-p shell-maker--busy)
       (magent-agent-shell--bootstrap-complete-p)
       (not (map-elt agent-shell--state :active-requests))
       (not (map-elt agent-shell--state :pending-restore))
       (not (and (boundp 'shell-maker--request-process)
                 shell-maker--request-process
                 (process-live-p shell-maker--request-process)))))

(defun magent-agent-shell--recover-stale-busy (&optional process-pending)
  "Clear stale busy state in current Magent shell.
When PROCESS-PENDING is non-nil, resume the oldest queued agent-shell prompt
after clearing the stale state."
  (when (magent-agent-shell--stale-busy-p)
    (setq shell-maker--busy nil)
    (magent-log "WARN recovered stale agent-shell busy state in %s"
                (buffer-name))
    (when (and process-pending
               (map-elt agent-shell--state :pending-requests))
      (agent-shell--process-pending-request))
    t))

;;;###autoload
(defun magent-agent-shell-start ()
  "Start a fresh Magent agent-shell buffer."
  (interactive)
  (magent-agent-shell--with-config
    (agent-shell-start :config (magent-agent-shell-make-config))))

;;;###autoload
(defun magent-start ()
  "Open or reuse the Magent agent-shell UI."
  (interactive)
  (magent-agent-shell--with-config
    (if-let* ((shell-buffer (magent-agent-shell--buffer t)))
        (progn
          (with-current-buffer shell-buffer
            (magent-agent-shell--recover-stale-busy t))
          (agent-shell--display-buffer shell-buffer))
      (agent-shell--dwim :config (magent-agent-shell-make-config)
                         :new-shell t))))

;;;###autoload
(cl-defun magent-agent-shell-send-prompt (prompt &key no-focus skills)
  "Send PROMPT through the Magent agent-shell backend."
  (unless (and (stringp prompt)
               (not (string-empty-p (string-trim prompt))))
    (user-error "Empty prompt"))
  (magent-agent-shell--with-config
    (let ((shell-buffer (magent-agent-shell--buffer)))
      (magent-agent-shell--queue-prompt-skills shell-buffer prompt skills)
      (with-current-buffer shell-buffer
        (magent-agent-shell--recover-stale-busy t))
      (if (with-current-buffer shell-buffer
            (shell-maker-busy))
          (with-current-buffer shell-buffer
            (agent-shell-queue-request prompt))
        (agent-shell-insert :text prompt
                            :submit t
                            :no-focus no-focus
                            :shell-buffer shell-buffer)))))

;;;###autoload
(defun magent-agent-shell-toggle-skill-for-next-request (&optional skill-name)
  "Toggle an instruction skill for the next Magent agent-shell request."
  (interactive)
  (magent--ensure-initialized)
  (magent-agent-shell--with-config
    (magent-agent-shell--prepare-skill-context)
    (let* ((shell-buffer (magent-agent-shell--buffer))
           (name (or skill-name
                     (magent-agent-shell--read-instruction-skill
                      "Toggle skill for next request: "))))
      (unless (member name (magent-agent-shell--instruction-skill-names))
        (user-error "Magent: '%s' is not an instruction skill" name))
      (message "Magent: skill %s %s for next request"
               name
               (if (magent-agent-shell--toggle-pending-skill
                    shell-buffer name)
                   "selected"
                 "cleared")))))

;;;###autoload
(defun magent-agent-shell-clear-skills-for-next-request ()
  "Clear selected instruction skills for the next Magent agent-shell request."
  (interactive)
  (magent--ensure-initialized)
  (magent-agent-shell--with-config
    (magent-agent-shell--clear-pending-skills
     (magent-agent-shell--buffer))
    (message "Magent: selected skills cleared")))

;;;###autoload
(defun magent-agent-shell-run-command (&optional command-name argument)
  "Run a registered slash COMMAND-NAME through Magent agent-shell.
When ARGUMENT is nil, prompt for optional trailing command text."
  (interactive)
  (magent--ensure-initialized)
  (magent-agent-shell--with-config
    (magent-agent-shell--prepare-skill-context)
    (let* ((shell-buffer (magent-agent-shell--buffer))
           (runtime-session
            (magent-agent-shell--runtime-session shell-buffer))
           (scope
            (if runtime-session
                (magent-runtime-session-scope runtime-session)
              (or (magent-runtime-active-project-scope) 'global)))
           (name
            (or command-name
                (let ((names (mapcar #'magent-command-spec-name
                                     (magent-command-list scope))))
                  (unless names
                    (user-error "Magent: no slash commands are registered"))
                  (completing-read "Run Magent command: " names nil t))))
           (_spec (or (magent-command-get name scope)
                      (user-error "Magent: unknown command /%s" name)))
           (extra (if argument
                      argument
                    (read-string
                     (format "Argument for /%s (optional): " name))))
           (prompt (concat "/" name
                           (unless (string-blank-p extra)
                             (concat " " extra)))))
      (magent-agent-shell-send-prompt prompt))))

;;;###autoload
(defun magent-agent-shell-run-skill-command
    (&optional skill-name extra-instruction)
  "Run a compatibility skill command through Magent agent-shell.
When EXTRA-INSTRUCTION is non-nil, pass it as slash command argument."
  (interactive)
  (magent--ensure-initialized)
  (let ((name (or skill-name
                  (let ((names (magent-skills-command-names)))
                    (unless names
                      (user-error
                       "Magent: no command-like skills are registered"))
                    (completing-read "Run skill command: " names nil t)))))
    (unless (magent-skills-default-prompt name)
      (user-error "Magent: skill '%s' has no command adapter" name))
    (magent-agent-shell-run-command name extra-instruction)))

;;;###autoload
(defun magent-agent-shell-run-init-command (&optional extra-instruction)
  "Run the built-in /init command through Magent agent-shell."
  (interactive)
  (magent-agent-shell-run-command "init" extra-instruction))

;;;###autoload
(defun magent-agent-shell-prompt-region (begin end)
  "Send region from BEGIN to END through the Magent agent-shell backend."
  (interactive "r")
  (magent-agent-shell-send-prompt
   (buffer-substring-no-properties begin end)))

;;;###autoload
(defun magent-agent-shell-ask-at-point ()
  "Ask Magent about the symbol at point using agent-shell."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (unless symbol
      (user-error "No symbol at point"))
    (magent-agent-shell-send-prompt
     (format "Explain this code: %s" symbol))))

;;;###autoload
(defun magent-agent-shell-interrupt (&optional force)
  "Interrupt the current Magent agent-shell request.
When FORCE is non-nil, skip agent-shell's confirmation prompt."
  (interactive "P")
  (if-let* ((shell-buffer (or (and (magent-agent-shell--magent-buffer-p
                                    (current-buffer))
                                   (current-buffer))
                              (magent-agent-shell--buffer t))))
      (with-current-buffer shell-buffer
        (agent-shell-interrupt force))
    (user-error "No Magent agent-shell buffer")))

(defun magent-agent-shell-processing-p ()
  "Return non-nil when a Magent agent-shell buffer is busy."
  (seq-some
   (lambda (shell-buffer)
     (with-current-buffer shell-buffer
       (and (not (magent-agent-shell--stale-busy-p))
            (memq (ignore-errors
                    (agent-shell-status :shell-buffer shell-buffer))
                  '(busy blocked)))))
   (magent-agent-shell--buffers)))

(provide 'magent-agent-shell)
;;; magent-agent-shell.el ends here
