;;; magent-config.el --- Configuration for OpenCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Configuration module for OpenCode using Emacs customize groups.

;;; Code:

(defgroup magent nil
  "OpenCode AI coding agent for Emacs."
  :prefix "magent-"
  :group 'tools
  :link '(url-link :tag "GitHub" "https://github.com/anomalyco/magent"))

(defcustom magent-provider 'anthropic
  "LLM provider to use for AI requests.
Supported providers: 'anthropic, 'openai, 'openai-compatible."
  :type '(choice (const :tag "Anthropic" anthropic)
                 (const :tag "OpenAI" openai)
                 (const :tag "OpenAI Compatible" openai-compatible))
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'magent-api-set-credentials)
           (magent-api-set-credentials)))
  :initialize 'custom-initialize-default
  :group 'magent)

(defcustom magent-api-key nil
  "API key for the LLM provider.
For Anthropic: Get from https://console.anthropic.com/
For OpenAI: Get from https://platform.openai.com/api-keys"
  :type '(string :tag "API Key")
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'magent-api-set-credentials)
           (magent-api-set-credentials)))
  :initialize 'custom-initialize-default
  :group 'magent)

(defcustom magent-base-url nil
  "Base URL for OpenAI-compatible API.
Only used when `magent-provider' is set to 'openai-compatible."
  :type '(string :tag "Base URL")
  :group 'magent)

(defcustom magent-model "claude-sonnet-4-20250514"
  "Model identifier to use for AI requests.
Examples:
- Anthropic: claude-sonnet-4-20250514, claude-3-5-sonnet-20241022
- OpenAI: gpt-4o, gpt-4o-mini, o1"
  :type 'string
  :group 'magent)

(defcustom magent-max-tokens 8192
  "Maximum tokens for AI responses."
  :type 'integer
  :group 'magent)

(defcustom magent-temperature 0.7
  "Temperature for AI response generation (0.0 to 1.0).
Lower values make responses more focused and deterministic.
Higher values make responses more creative and varied."
  :type 'number
  :group 'magent)

(defcustom magent-system-prompt
  "You are OpenCode, an AI coding agent that helps users with software development tasks.

You have access to tools for reading files, editing files, searching code, and running commands. Use these tools to accomplish the user's goals.

When making code changes:
1. Always read existing files before editing them
2. Explain your changes clearly
3. Follow the existing code style and conventions
4. Be concise and direct

If you're unsure about something, ask the user for clarification."
  "System prompt for the AI agent."
  :type 'string
  :group 'magent)

(defcustom magent-buffer-name "*magent*"
  "Name of the buffer used for OpenCode output."
  :type 'string
  :group 'magent)

(defcustom magent-auto-scroll t
  "Automatically scroll output buffer when new content arrives."
  :type 'boolean
  :group 'magent)

(defcustom magent-enable-tools '(read write grep glob bash)
  "List of enabled tools.
Available tools: read, write, grep, glob, bash."
  :type '(set (const :tag "Read files" read)
              (const :tag "Write files" write)
              (const :tag "Search content (grep)" grep)
              (const :tag "Find files (glob)" glob)
              (const :tag "Run shell commands" bash))
  :group 'magent)

(defcustom magent-project-root-function nil
  "Function to find project root directory.
The function should take no arguments and return the project root path as a string.
If nil, uses `projectile-project-root' when available, or `default-directory'."
  :type '(choice (function :tag "Custom function")
                 (const :tag "Default" nil))
  :group 'magent)

(defcustom magent-max-history 100
  "Maximum number of messages to keep in session history."
  :type 'integer
  :group 'magent)

(defcustom magent-enable-logging t
  "Enable logging to the *magent-log* buffer.
When enabled, API requests and responses are logged for debugging."
  :type 'boolean
  :group 'magent)

(defcustom magent-default-agent "build"
  "The default agent to use for new sessions.
Should match one of the registered agent names."
  :type 'string
  :group 'magent)

(defcustom magent-load-custom-agents t
  "Whether to load custom agents from .opencode/agent/*.md files."
  :type 'boolean
  :group 'magent)

;;;###autoload
(defun magent-get-api-key ()
  "Get the API key for current provider.
First checks `magent-api-key', then falls back to environment variable."
  (or magent-api-key
      (let ((env-var (pcase magent-provider
                       ('anthropic "ANTHROPIC_API_KEY")
                       ('openai "OPENAI_API_KEY")
                       ('openai-compatible "OPENAI_API_KEY"))))
        (when env-var (getenv env-var)))))

(provide 'magent-config)
;;; magent-config.el ends here
