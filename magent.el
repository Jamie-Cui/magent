;;; magent.el --- Magent AI coding agent for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Maintainer: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai, copilot
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (spinner "1.7.4") (transient "0.4"))
;; URL: https://github.com/jamie-cui/magent

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; magent.el is an Emacs Lisp AI coding agent built on gptel.
;; It provides intelligent code assistance by integrating with LLMs
;; via gptel's multi-provider backend system.
;;
;; Features:
;; - LLM integration via gptel (Anthropic, OpenAI, Ollama, Gemini, etc.)
;; - File operations (read, write, edit, grep, glob)
;; - Shell command execution
;; - Streaming responses
;; - Session management with conversation history
;; - Minibuffer interface for quick prompts
;; - Agent system with specialized agents and permission control
;;
;; Agent System:
;; - Built-in agents: build (default), plan, explore, research, general, compaction, title, summary
;; - Permission-based tool access control per agent
;; - Custom agent support via .magent/agent/*.md files
;; - Agent selection per session
;;
;; Configuration:
;; LLM provider, model, and API key are managed by gptel.
;; Magent-specific settings are in the `magent' customize group.
;;
;;   M-x customize-group RET magent RET
;;
;; Usage:
;;   M-x magent                - Send a prompt to the AI
;;   M-x magent-prompt-region  - Send the selected region to the AI
;;   M-x magent-ask-at-point   - Ask about the symbol at point
;;   M-x magent-clear-session  - Clear the current session
;;   M-x magent-select-agent   - Select an agent for this session
;;   M-x magent-list-agents    - List all available agents
;;   M-x magent-show-current-agent - Show current session's agent
;;
;; Setup:
;; 1. Configure gptel with your provider and API key:
;;    (setq gptel-backend (gptel-make-anthropic "Claude" :key 'gptel-api-key-from-auth-source))
;;    or set the ANTHROPIC_API_KEY / OPENAI_API_KEY environment variable.
;;
;; 2. Enable globally:
;;    (global-magent-mode 1)

;;; Code:

;; Required modules
(require 'spinner)
(require 'transient)
(require 'magent-config)
(require 'magent-session)
(require 'magent-tools)
(require 'magent-agent)
(require 'magent-ui)
(require 'magent-agent-registry)
(require 'magent-permission)
(require 'magent-agent-file)
(require 'magent-skills)
(require 'magent-skill-file)
(require 'magent-queue)

;;; Initialization

(defvar magent--initialized nil
  "Non-nil if magent has already been initialized.
Registry and custom agents are loaded when non-nil.")

(defvar magent--spinner (spinner-create 'rotating-line)
  "Global spinner displayed in the modeline while magent is processing.")

(defconst magent--lighter
  '(" Magent"
    (:eval (spinner-print magent--spinner))
    (:eval (let ((n (magent-queue-length)))
             (when (> n 0)
               (propertize (format "[+%d]" n)
                           'face 'warning
                           'help-echo (format "Magent: %d prompt(s) queued" n))))))
  "Modeline lighter for `magent-mode'.
Shows \" Magent\" plus an animated spinner while processing,
and \"[+N]\" when N prompts are queued.")
(put 'magent--lighter 'risky-local-variable t)

(defvar magent--mode-line-spinner-construct
  '(:eval (when (spinner--active-p magent--spinner)
            (concat " Magent:" (spinner-print magent--spinner))))
  "Mode-line construct added to `global-mode-string'.
Shows \"Magent:<spinner>\" in `mode-line-misc-info' while processing.")

;;;###autoload
(define-minor-mode magent-mode
  "Minor mode for Magent AI coding agent.
When enabled, Magent commands are available.

\\{magent-mode-map}"
  :init-value nil
  :lighter magent--lighter
  :keymap (let ((map (make-sparse-keymap)))
            ;; Keybindings
            (define-key map (kbd "C-c m p") #'magent-dwim)
            (define-key map (kbd "C-c m r") #'magent-prompt-region)
            (define-key map (kbd "C-c m a") #'magent-ask-at-point)
            (define-key map (kbd "C-c m c") #'magent-clear-session)
            (define-key map (kbd "C-c m l") #'magent-show-log)
            (define-key map (kbd "C-c m L") #'magent-clear-log)
            ;; Section folding
            (define-key map (kbd "C-c m t") #'magent-ui-toggle-section)
            ;; Agent management
            (define-key map (kbd "C-c m A") #'magent-select-agent)
            (define-key map (kbd "C-c m i") #'magent-show-current-agent)
            (define-key map (kbd "C-c m v") #'magent-list-agents)
            map)
  (when magent-mode
    ;; Minimal initialization on mode enable
    (unless (member magent--mode-line-spinner-construct global-mode-string)
      (push magent--mode-line-spinner-construct global-mode-string)
      (magent-log "INFO magent mode enabled (lazy init)"))))

(defun magent--ensure-initialized ()
  "Ensure magent is fully initialized.
Called on first use of any magent command.  Loads agents and skills."
  (unless magent--initialized
    (magent-log "INFO Initializing magent agents and skills...")
    ;; Initialize agent registry (also loads custom agents)
    (magent-agent-registry-init)
    ;; Load skills from files (built-in skills auto-register on require)
    (magent-skill-file-load-all)
    (setq magent--initialized t)
    (magent-log "INFO magent initialization complete")))

;;;###autoload
(define-globalized-minor-mode global-magent-mode magent-mode
  (lambda () (magent-mode 1))
  :group 'magent)

(provide 'magent)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; no-byte-compile: nil
;; no-native-compile: nil
;; End:

;;; magent.el ends here
