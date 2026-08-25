;;; magent-agent-shell.el --- agent-shell UI backend for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; This module exposes Magent's in-process ACP client to agent-shell.  It owns
;; the Magent agent configuration and one deliberately isolated compatibility
;; layer for context collection.  Interactive buffer, prompt, queue, skill,
;; and interruption behavior belongs to agent-shell.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'magent-acp)
(require 'magent-config)
(require 'magent-runtime)

(defvar agent-shell-agent-configs)
(defvar agent-shell-session-strategy)

(declare-function agent-shell--context "agent-shell")
(declare-function agent-shell--get-current-line-context "agent-shell")
(declare-function agent-shell--get-files-context "agent-shell")
(declare-function agent-shell--get-region-context "agent-shell")
(declare-function agent-shell-get-config "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell-start "agent-shell")

(defconst magent-agent-shell--identifier 'magent
  "agent-shell config identifier used by Magent.")

(defvar-local magent-agent-shell--owns-session-strategy-p nil
  "Non-nil when Magent installed the buffer-local session strategy.")

(defvar magent-agent-shell--context-request-p nil
  "Dynamically non-nil while agent-shell builds context for Magent.")

(defun magent-agent-shell--ensure-loaded ()
  "Load `agent-shell' before using its public runtime API."
  (require 'agent-shell))

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
   :default-session-mode-id (lambda () magent-default-agent)
   :install-instructions "Magent uses an in-process ACP client; no external command is required."))

(defun magent-agent-shell--welcome-message (_config)
  "Return the welcome message for Magent agent-shell buffers."
  "\nMagent\n")

(defun magent-agent-shell-ensure-config ()
  "Ensure Magent's config maker is registered with agent-shell.
Return Magent's identifier for use as an agent-shell preferred config.  This
explicit registration remains until agent-shell provides third-party config
discovery."
  (magent-agent-shell--ensure-loaded)
  (setq agent-shell-agent-configs
        (cons #'magent-agent-shell-make-config
              (delq #'magent-agent-shell-make-config
                    agent-shell-agent-configs)))
  magent-agent-shell--identifier)

;;;###autoload
(defun magent-start ()
  "Start Magent's preferred conversational frontend."
  (interactive)
  (magent-runtime-ensure-initialized)
  (let ((agent-shell-session-strategy
         magent-agent-shell-session-strategy))
    (agent-shell-start :config (magent-agent-shell-make-config))))

;;;; Temporary agent-shell context compatibility

;; Keep all remaining private agent-shell coupling in this section.  These
;; advices can be removed together once agent-shell offers remote-safe context
;; construction and suppresses blank current-line context itself.  Magent does
;; not otherwise inspect or mutate agent-shell or shell-maker private state.

(defun magent-agent-shell--magent-buffer-p (buffer)
  "Return non-nil when BUFFER uses Magent's agent-shell config."
  (and (buffer-live-p buffer)
       (eq (map-elt (ignore-errors (agent-shell-get-config buffer))
                    :identifier)
           magent-agent-shell--identifier)))

(defun magent-agent-shell--blank-current-line-p ()
  "Return non-nil when the current buffer line is blank."
  (string-blank-p
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun magent-agent-shell--context (orig &rest args)
  "Call ORIG with ARGS under the exact target shell's context ownership."
  (let ((magent-agent-shell--context-request-p
         (magent-agent-shell--magent-buffer-p
          (plist-get args :shell-buffer))))
    (apply orig args)))

(defun magent-agent-shell--get-region-context (orig &rest args)
  "Build region context through ORIG without remote path queries.

For remote files, agent-shell shortens the file name against
`:agent-cwd' with `file-in-directory-p'.  That can synchronously query
TRAMP while agent-shell is starting and deadlock with the in-process
ACP request.  Keep the full remote file name instead; constructing
context must not perform remote file I/O."
  (let ((agent-cwd (plist-get args :agent-cwd)))
    (when (and magent-agent-shell--context-request-p
               (or (and (stringp buffer-file-name)
                        (file-remote-p buffer-file-name))
                   (and (stringp agent-cwd)
                        (file-remote-p agent-cwd))))
      (setq args (plist-put (copy-sequence args) :agent-cwd nil))))
  (apply orig args))

(defun magent-agent-shell--get-files-context (orig &rest args)
  "Build Magent file context through ORIG without remote file probes.
For remote files, retain full TRAMP names and omit image detection and project
containment checks.  Context rendering must not contact the project host."
  (let* ((files (plist-get args :files))
         (agent-cwd (plist-get args :agent-cwd))
         (expanded
          (mapcar (lambda (file)
                    (if agent-cwd (expand-file-name file agent-cwd) file))
                  files)))
    (if (and files
             magent-agent-shell--context-request-p
             (or (and (stringp agent-cwd) (file-remote-p agent-cwd))
                 (seq-some (lambda (file)
                             (and (stringp file) (file-remote-p file)))
                           expanded)))
        (mapconcat (lambda (file)
                     (propertize (concat "@" file) 'pointer 'hand))
                   expanded
                   "\n\n")
      (apply orig args))))

(defun magent-agent-shell--get-current-line-context (orig &rest args)
  "Suppress empty current-line context before delegating to ORIG.

`agent-shell--get-current-line-context' builds line context by temporarily
activating the current line as a region.  On a blank line this may create an
inverted range.  Blank line context is not useful for Magent, so skip it."
  (if (and magent-agent-shell--context-request-p
           (magent-agent-shell--blank-current-line-p))
      nil
    (apply orig args)))

(defconst magent-agent-shell--context-compatibility-advices
  '((agent-shell--context . magent-agent-shell--context)
    (agent-shell--get-region-context .
                                     magent-agent-shell--get-region-context)
    (agent-shell--get-files-context . magent-agent-shell--get-files-context)
    (agent-shell--get-current-line-context .
                                           magent-agent-shell--get-current-line-context))
  "Private agent-shell context functions temporarily advised by Magent.")

(defun magent-agent-shell--install-context-compatibility ()
  "Install Magent's isolated agent-shell context compatibility advices."
  (dolist (entry magent-agent-shell--context-compatibility-advices)
    (unless (advice-member-p (cdr entry) (car entry))
      (advice-add (car entry) :around (cdr entry)))))

(magent-agent-shell--install-context-compatibility)

(provide 'magent-agent-shell)
;;; magent-agent-shell.el ends here
