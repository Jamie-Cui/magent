;;; magent.el --- AI coding agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Maintainer: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai, copilot
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.8") (yaml "1.0.0") (compat "30.1.0.0") (acp "0.12.2") (agent-shell "20260714.1645"))
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
;; - agent-shell frontend through an in-process ACP adapter
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
;;   M-x magent-agent-shell-dwim         - Open or reuse the project session
;;   M-x magent-agent-shell-start        - Start a fresh Magent session
;;   M-x magent-agent-shell-prompt-region - Send the selected region
;;   M-x magent-agent-shell-ask-at-point - Ask about the symbol at point
;;
;; Setup:
;; 1. Configure gptel with your provider and API key:
;;    (setq gptel-backend (gptel-make-anthropic "Claude" :key 'gptel-api-key-from-auth-source))
;;    or set the ANTHROPIC_API_KEY / OPENAI_API_KEY environment variable.
;;
;;; Code:

;; Ensure the lisp/ directory is on load-path so sibling modules are
;; found.  When installed via package.el (MELPA flattens lisp/*.el to
;; the top level), this is a harmless no-op since the package root is
;; already on load-path.
(eval-and-compile
  (when load-file-name
    (add-to-list 'load-path (file-name-directory load-file-name))))

;; Required modules
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
(require 'magent-agent-registry)
(require 'magent-agent-file)
(require 'magent-permission)
(require 'magent-skills)
(require 'magent-capability)

;; Skill management is optional and network-aware, so keep it lazy.
(autoload 'magent-find-skill "magent-skill-manager"
  "Search skills.sh for user-installable Magent skills." t)
(autoload 'magent-install-skill "magent-skill-manager"
  "Install one user-level Magent instruction skill." t)
(autoload 'magent-delete-skill "magent-skill-manager"
  "Permanently delete one user-level Magent skill." t)

(declare-function magent-runtime-ensure-initialized "magent-runtime")

(defun magent--ensure-initialized ()
  "Ensure magent is fully initialized.
Called on first use of any magent command.  Loads agents and skills."
  (magent-runtime-ensure-initialized))

(provide 'magent)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; no-byte-compile: nil
;; no-native-compile: nil
;; End:

;;; magent.el ends here
