;;; magent-agent-shell.el --- agent-shell UI backend for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module is the agent-shell-specific UI backend.  Magent exposes an
;; in-process ACP client, while agent-shell owns the interactive shell UI.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell)
(require 'magent-acp)
(require 'magent-config)
(require 'magent-runtime)

(defvar gptel-model)
(defvar agent-shell--state)
(defvar agent-shell-session-strategy)
(defvar comint-last-prompt)
(defvar shell-maker--busy)
(defvar shell-maker--request-process)

(declare-function magent--ensure-initialized "magent")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell--dwim "agent-shell")
(declare-function agent-shell--process-pending-request "agent-shell")
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-queue-request "agent-shell")
(declare-function agent-shell-start "agent-shell")
(declare-function shell-maker-busy "shell-maker")

(defconst magent-agent-shell--identifier 'magent
  "agent-shell config identifier used by Magent.")

(defun magent-agent-shell--model-id ()
  "Return the current gptel model id for agent-shell display."
  (format "%s" (or (and (boundp 'gptel-model) gptel-model) "gptel")))

;;;###autoload
(defun magent-agent-shell-make-config ()
  "Return the agent-shell configuration for Magent."
  (agent-shell-make-agent-config
   :identifier magent-agent-shell--identifier
   :mode-line-name "Magent"
   :buffer-name "Magent"
   :shell-prompt "Magent> "
   :shell-prompt-regexp "Magent> "
   :welcome-function #'magent-agent-shell--welcome-message
   :client-maker (lambda (buffer)
                   (magent-acp-make-client buffer))
   :default-model-id #'magent-agent-shell--model-id
   :default-session-mode-id (lambda () magent-default-agent)
   :install-instructions "Magent uses an in-process ACP client; no external command is required."))

(defun magent-agent-shell--welcome-message (_config)
  "Return the welcome message for Magent agent-shell buffers."
  "\nMagent\n")

(defun magent-agent-shell-ensure-config ()
  "Ensure Magent is registered in `agent-shell-agent-configs'."
  (let ((config (magent-agent-shell-make-config)))
    (setq agent-shell-agent-configs
          (cons config
                (seq-remove
                 (lambda (entry)
                   (eq (map-elt entry :identifier)
                       magent-agent-shell--identifier))
                 agent-shell-agent-configs)))
    config))

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
(defun magent-agent-shell-dwim ()
  "Open or reuse the Magent agent-shell UI."
  (interactive)
  (magent-agent-shell--with-config
    (if-let ((shell-buffer (magent-agent-shell--buffer t)))
        (progn
          (with-current-buffer shell-buffer
            (magent-agent-shell--recover-stale-busy t))
          (agent-shell--display-buffer shell-buffer))
      (agent-shell--dwim :config (magent-agent-shell-make-config)
                         :new-shell t))))

;;;###autoload
(cl-defun magent-agent-shell-send-prompt (prompt &key no-focus)
  "Send PROMPT through the Magent agent-shell backend."
  (unless (and (stringp prompt)
               (not (string-empty-p (string-trim prompt))))
    (user-error "Empty prompt"))
  (magent-agent-shell--with-config
    (let ((shell-buffer (magent-agent-shell--buffer)))
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
  (if-let ((shell-buffer (or (and (magent-agent-shell--magent-buffer-p
                                   (current-buffer))
                                  (current-buffer))
                             (magent-agent-shell--buffer t))))
      (with-current-buffer shell-buffer
        (agent-shell-interrupt (or force t)))
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
