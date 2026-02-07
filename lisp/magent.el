;;; magent.el --- OpenCode AI coding agent for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Maintainer: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai, copilot
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/anomalyco/magent

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

;; magent.el is an Emacs Lisp implementation of OpenCode, an open-source
;; AI coding agent.  It provides intelligent code assistance by integrating
;; with Large Language Models (LLMs) like Anthropic's Claude and OpenAI's GPT.
;;
;; Features:
;; - Direct API integration with Anthropic and OpenAI
;; - File operations (read, write, grep, glob)
;; - Shell command execution
;; - Session management with conversation history
;; - Minibuffer interface for quick prompts
;; - Agent system with specialized agents and permission control
;;
;; Agent System:
;; - Built-in agents: build (default), plan, explore, general, compaction, title, summary
;; - Permission-based tool access control per agent
;; - Custom agent support via .opencode/agent/*.md files
;; - Agent selection per session
;;
;; Configuration:
;; Customize the `magent' group to set your API key, model, and other options.
;;
;;   M-x customize-group RET magent RET
;;
;; Usage:
;;   M-x magent-prompt         - Send a prompt to the AI
;;   M-x magent-prompt-region  - Send the selected region to the AI
;;   M-x magent-ask-at-point   - Ask about the symbol at point
;;   M-x magent-clear-session  - Clear the current session
;;   M-x magent-select-agent   - Select an agent for this session
;;   M-x magent-list-agents    - List all available agents
;;   M-x magent-show-current-agent - Show current session's agent
;;
;; Setup:
;; 1. Set your API key:
;;    (setq magent-api-key "your-api-key-here")
;;    or set the ANTHROPIC_API_KEY or OPENAI_API_KEY environment variable.
;;
;; 2. Choose your provider:
;;    (setq magent-provider 'anthropic)  ; or 'openai or 'openai-compatible
;;
;; 3. Optionally set a custom model:
;;    (setq magent-model "claude-sonnet-4-20250514")
;;
;; 4. Enable globally:
;;    (global-magent-mode 1)

;;; Code:

;; Required modules
(require 'magent-config)
(require 'magent-api)
(require 'magent-session)
(require 'magent-tools)
(require 'magent-agent)
(require 'magent-ui)
(require 'magent-agent-registry)
(require 'magent-agent-info)
(require 'magent-permission)
(require 'magent-agent-file)

;;; Initialization

;;;###autoload
(defun magent-mode ()
  "Minor mode for OpenCode AI coding agent.
When enabled, OpenCode commands are available.

\\{magent-mode-map}"
  :init-value nil
  :lighter " OpenCode"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Keybindings
            (define-key map (kbd "C-c o p") #'magent-prompt)
            (define-key map (kbd "C-c o r") #'magent-prompt-region)
            (define-key map (kbd "C-c o a") #'magent-ask-at-point)
            (define-key map (kbd "C-c o c") #'magent-clear-session)
            (define-key map (kbd "C-c o s") #'magent-show-session)
            (define-key map (kbd "C-c o l") #'magent-view-log)
            (define-key map (kbd "C-c o L") #'magent-clear-log)
            ;; Agent management
            (define-key map (kbd "C-c o A") #'magent-select-agent)
            (define-key map (kbd "C-c o i") #'magent-show-current-agent)
            (define-key map (kbd "C-c o v") #'magent-list-agents)
            map)
  (if magent-mode
      (progn
        ;; Initialize agent registry
        (magent-agent-registry-init)
        ;; Load custom agents if enabled
        (when magent-load-custom-agents
          (magent-agent-file-load-all))
        (magent-api-set-credentials)
        (message "OpenCode mode enabled"))
    (message "OpenCode mode disabled")))

;;;###autoload
(define-globalized-minor-mode global-magent-mode magent-mode
  (lambda () (magent-mode 1))
  :group 'magent)

;; Auto-enable on load
(add-hook 'after-init-hook #'magent-api-set-credentials)

(provide 'magent)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; magent.el ends here
