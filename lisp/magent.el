;;; magent.el --- AI coding agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Maintainer: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai, copilot
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.8") (transient "0.7.8") (yaml "1.0.0") (compat "30.1.0.0") (acp "0.12.2") (agent-shell "20260714.1645"))
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
;; - Built-in agents: build (default), plan, explore, general, compaction, title, summary
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
;;   M-x magent-transient-menu - Open the command menu
;;   M-x magent-prompt-region  - Send the selected region to the AI
;;   M-x magent-ask-at-point   - Ask about the symbol at point
;; Commands such as diagnosis, session clearing, agent selection, logs, and
;; one-shot skills are available from `magent-transient-menu'.
;;
;; Setup:
;; 1. Configure gptel with your provider and API key:
;;    (setq gptel-backend (gptel-make-anthropic "Claude" :key 'gptel-api-key-from-auth-source))
;;    or set the ANTHROPIC_API_KEY / OPENAI_API_KEY environment variable.
;;
;; 2. Enable globally:
;;    (global-magent-mode 1)

;;; Code:

;; Ensure the lisp/ directory is on load-path so sibling modules are
;; found.  When installed via package.el (MELPA flattens lisp/*.el to
;; the top level), this is a harmless no-op since the package root is
;; already on load-path.
(eval-and-compile
  (when load-file-name
    (add-to-list 'load-path (file-name-directory load-file-name))))

;; Required modules
(require 'transient)
(require 'magent-config)
(require 'magent-json)
(require 'magent-lifecycle-events)
(require 'magent-audit)
(require 'magent-protocol)
(require 'magent-ledger)
(require 'magent-agent-job)
(require 'magent-llm)
(require 'magent-llm-gptel)
(require 'magent-command)
(require 'magent-memory)
(require 'magent-doctor)
(require 'magent-agent-loop)
(require 'magent-session)
(require 'magent-transcript-context)
(require 'magent-runtime)
(require 'magent-runtime-queue)
(require 'magent-project-instructions)
(require 'magent-repo-summary)
(require 'magent-tools)
(require 'magent-tool-runtime)
(require 'magent-tool-orchestrator)
(require 'magent-agent-info)
(require 'magent-agent-builtins)
(require 'magent-agent)
(require 'magent-runtime-api)
(require 'magent-acp)
(require 'magent-agent-shell)
(require 'magent-ui)
(require 'magent-modeline)
(require 'magent-agent-registry)
(require 'magent-agent-file)
(require 'magent-permission)
(require 'magent-skills)
(require 'magent-capability)

;; Skill management is optional and network-aware, so keep it lazy.
(autoload 'magent-skill-find "magent-skill-manager"
  "Search skills.sh for user-installable Magent skills." t)
(autoload 'magent-skill-install "magent-skill-manager"
  "Install one user-level Magent instruction skill." t)
(autoload 'magent-skill-delete "magent-skill-manager"
  "Permanently delete one user-level Magent skill." t)

(declare-function magent-runtime-ensure-initialized "magent-runtime")

(defconst magent--mode-bindings
  '(("C-c m p" . magent-dwim)
    ("C-c m r" . magent-prompt-region)
    ("C-c m a" . magent-ask-at-point)
    ("C-c m ?" . magent-transient-menu))
  "Declarative keybinding table for `magent-mode'.")

(defun magent--populate-mode-map (map)
  "Install `magent-mode' bindings into MAP."
  (dolist (binding magent--mode-bindings map)
    (define-key map (kbd (car binding)) (cdr binding))))

;;;###autoload
(define-minor-mode magent-mode
  "Minor mode for Magent AI coding agent.
When enabled, Magent commands are available.

\\{magent-mode-map}"
  :init-value nil
  :lighter magent-modeline-lighter
  :keymap (magent--populate-mode-map (make-sparse-keymap))
  (when magent-mode
    ;; Minimal initialization on mode enable
    (when (magent-modeline-install)
      (magent-log "INFO magent mode enabled (lazy init)"))))

;; `define-minor-mode' does not replace an already defined keymap on reload.
;; Re-apply the full binding table so live development stays in sync.
(magent--populate-mode-map magent-mode-map)

(defun magent--ensure-initialized ()
  "Ensure magent is fully initialized.
Called on first use of any magent command.  Loads agents and skills."
  (magent-runtime-ensure-initialized))

;;;###autoload
(define-globalized-minor-mode global-magent-mode magent-mode
  (lambda () (magent-mode 1))
  :group 'magent)

(provide 'magent)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; no-byte-compile: nil
;; no-native-compile: nil
;; End:

;;; magent.el ends here
