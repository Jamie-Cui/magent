;;; magent-config.el --- Configuration for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Configuration module for Magent using Emacs customize groups.
;; LLM provider, model, and API key settings are managed by gptel.
;; See `gptel-backend', `gptel-model', and `gptel-api-key'.

;;; Code:

(defgroup magent nil
  "Magent AI coding agent for Emacs."
  :prefix "magent-"
  :group 'tools
  :link '(url-link :tag "GitHub" "https://github.com/jamie-cui/magent"))

(defcustom magent-system-prompt
  "You are Magent, an AI coding agent that helps users with software development tasks.

You have access to tools for reading files, editing files, searching code, and running commands. Use these tools to accomplish the user's goals.

When making code changes:
1. Always read existing files before editing them
2. Explain your changes clearly
3. Follow the existing code style and conventions
4. Be concise and direct

When answering: DO NOT use markdown syntax, ALWAYS use org-mode syntax

If you're unsure about something, ask the user for clarification."
  "System prompt for the AI agent."
  :type 'string
  :group 'magent)

(defcustom magent-buffer-name "*magent*"
  "Name of the buffer used for Magent output."
  :type 'string
  :group 'magent)

(defcustom magent-auto-scroll t
  "Automatically scroll output buffer when new content arrives."
  :type 'boolean
  :group 'magent)

(defcustom magent-enable-streaming t
  "Enable streaming responses from the LLM.
When non-nil, responses are displayed incrementally as they arrive."
  :type 'boolean
  :group 'magent)

(defcustom magent-enable-tools '(read write edit grep glob bash emacs_eval delegate)
  "List of enabled tools.
Available tools: read, write, edit, grep, glob, bash, emacs_eval, delegate."
  :type '(set (const :tag "Read files" read)
              (const :tag "Write files" write)
              (const :tag "Edit files" edit)
              (const :tag "Search content (grep)" grep)
              (const :tag "Find files (glob)" glob)
              (const :tag "Run shell commands" bash)
              (const :tag "Evaluate Emacs Lisp" emacs_eval)
              (const :tag "Delegate to subagents" delegate))
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
  "Whether to load custom agents from .magent/agent/*.md files."
  :type 'boolean
  :group 'magent)

(defcustom magent-assistant-prompt "[AI  ] "
  "Prompt string displayed before assistant messages."
  :type 'string
  :group 'magent)

(defcustom magent-user-prompt "> "
  "Prompt string displayed before user messages."
  :type 'string
  :group 'magent)

(defcustom magent-tool-call-prompt "[TOOL] "
  "Prompt string displayed before tool call notifications."
  :type 'string
  :group 'magent)

(defcustom magent-error-prompt "[ERR ] "
  "Prompt string displayed before error messages."
  :type 'string
  :group 'magent)

(defcustom magent-loading-indicator " .... "
  "Character displayed as a loading indicator during processing."
  :type 'string
  :group 'magent)

(defcustom magent-loading-done-indicator "[DONE] "
  "Character displayed as a loading done indicator after processing."
  :type 'string
  :group 'magent)

(defcustom magent-agent-directory ".magent/agent"
  "Relative path (from project root) to the custom agent directory."
  :type 'string
  :group 'magent)

(defcustom magent-session-directory
  (expand-file-name "magent-sessions" user-emacs-directory)
  "Directory where session files are stored."
  :type 'directory
  :group 'magent)

(defcustom magent-grep-program (or (executable-find "rg") "rg")
  "Path to the grep program.
Magent uses ripgrep (rg) for fast, recursive code search.
If rg is not found, grep tool calls will fail with an informative error."
  :type 'string
  :group 'magent)

;;; Logging stub
;; Defined here so all modules can call magent-log unconditionally.
;; magent-ui.el overrides this with the real implementation that writes
;; to *magent-log*.  The stub is a no-op so tests and early-load contexts
;; never raise void-function errors.

(defun magent-log (format-string &rest args)
  "Log FORMAT-STRING with ARGS.  No-op until magent-ui is loaded."
  (ignore format-string args))

(provide 'magent-config)
;;; magent-config.el ends here
