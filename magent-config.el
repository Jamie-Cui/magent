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

You have access to tools for reading files, editing files, searching code, running commands, and interacting with the running Emacs instance.

Available Tools:
- read_file, write_file, edit_file: File operations
- grep, glob: Code search and file finding
- bash: Shell command execution
- emacs_eval: Evaluate Emacs Lisp expressions
- delegate: Delegate tasks to specialized subagents (explore, general)
- skill_invoke: Invoke Claude Code skills for Emacs interaction

Emacs Skill (via skill_invoke):
Use skill_invoke with skill_name=\"emacs\" to interact with the running Emacs instance:
- list-functions [PREFIX]: List interactive commands matching prefix
- describe-function [NAME]: Get function documentation
- eval-expression [EXPR]: Evaluate arbitrary elisp
- execute-keys [KEYS]: Simulate keystrokes (kbd format, e.g., \"C-x C-s\")
- minibuffer-prompt: Read current minibuffer state
- current-buffer-state: Get buffer name, mode, and content

When making code changes:
1. Always read existing files before editing them
2. Explain your changes clearly
3. Follow the existing code style and conventions
4. Be concise and direct

If you're unsure about something, ask the user for clarification.

IMPORTANT: Format your responses using Org-mode. Start your content with level-2 headings (**). The output buffer uses level-1 headings (*) for message section markers (user, assistant, tool, error), so your content MUST use level-2 or deeper headings to maintain proper structure. Use *bold*, /italic/, =code= for inline formatting, and #+BEGIN_SRC blocks for multi-line code."
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

(defcustom magent-enable-tools '(read write edit grep glob bash emacs_eval delegate skill web_search)
  "List of enabled tools.
Available tools: read, write, edit, grep, glob, bash, emacs_eval,
delegate, skill, web_search."
  :type '(set (const :tag "Read files" read)
              (const :tag "Write files" write)
              (const :tag "Edit files" edit)
              (const :tag "Search content (grep)" grep)
              (const :tag "Find files (glob)" glob)
              (const :tag "Run shell commands" bash)
              (const :tag "Evaluate Emacs Lisp" emacs_eval)
              (const :tag "Delegate to subagents" delegate)
              (const :tag "Invoke Claude Code skills" skill)
              (const :tag "Search the web" web_search))
  :group 'magent)

(defcustom magent-project-root-function nil
  "Function to find project root directory.
The function should take no arguments and return the project root
path as a string.  If nil, uses `projectile-project-root' when
available, or `default-directory'."
  :type '(choice (function :tag "Custom function")
                 (const :tag "Default" nil))
  :group 'magent)

(defcustom magent-max-history 100
  "Maximum number of messages to keep in session history."
  :type 'integer
  :group 'magent)

(defcustom magent-request-timeout 120
  "Timeout in seconds for LLM requests.
If no callback activity occurs within this period, the FSM
transitions to ERROR.  Set to 0 to disable."
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

(defcustom magent-assistant-prompt "ASSISTANT"
  "Tag text displayed in assistant section headers."
  :type 'string
  :group 'magent)

(defcustom magent-user-prompt "USER"
  "Tag text displayed in user section headers."
  :type 'string
  :group 'magent)

(defcustom magent-tool-call-prompt "tool"
  "Tag text displayed in tool call lines."
  :type 'string
  :group 'magent)

(defcustom magent-error-prompt "error"
  "Tag text displayed in error section headers."
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

(defcustom magent-grep-max-matches 100
  "Maximum number of matches to return from grep searches."
  :type 'integer
  :group 'magent)

(defcustom magent-bash-timeout 30
  "Default timeout in seconds for bash commands."
  :type 'integer
  :group 'magent)

(defcustom magent-emacs-eval-timeout 10
  "Default timeout in seconds for emacs_eval operations."
  :type 'integer
  :group 'magent)

(defcustom magent-ui-result-max-length 200
  "Maximum length for tool result display before truncation."
  :type 'integer
  :group 'magent)

(defcustom magent-ui-result-preview-length 150
  "Length of preview shown for truncated results."
  :type 'integer
  :group 'magent)

(defcustom magent-ui-tool-input-max-length 60
  "Maximum length for tool input display before truncation."
  :type 'integer
  :group 'magent)

(defcustom magent-ui-log-truncate-length 80
  "Maximum length for log messages before truncation."
  :type 'integer
  :group 'magent)

(defcustom magent-ui-fontify-threshold 500
  "Character threshold for async fontification.
Text blocks smaller than this are fontified synchronously.
Larger blocks are fontified with idle timer to avoid blocking."
  :type 'integer
  :group 'magent)

(defcustom magent-ui-fontify-idle-delay 0.1
  "Idle delay in seconds before async fontification starts."
  :type 'float
  :group 'magent)

(defcustom magent-ui-batch-insert-delay 0.05
  "Delay in seconds for batching small streaming chunks.
Streaming text chunks are accumulated for this duration before
rendering to reduce UI updates."
  :type 'float
  :group 'magent)

(defcustom magent-include-reasoning t
  "How to handle LLM reasoning/thinking blocks.
If t, display reasoning blocks wrapped in #+begin_think/#+end_think.
If 'ignore, insert reasoning text but hide it with org folding.
If nil, discard reasoning content entirely."
  :type '(choice (const :tag "Display reasoning" t)
                 (const :tag "Hide reasoning" ignore)
                 (const :tag "Discard reasoning" nil))
  :group 'magent)

(defcustom magent-fsm-backend 'gptel
  "FSM backend to use for tool calling loop.
`magent' uses the custom FSM in magent-fsm.el.
`gptel' uses gptel's built-in FSM (gptel-request.el, default)."
  :type '(choice (const :tag "Magent FSM (magent-fsm.el)" magent)
                 (const :tag "Gptel FSM (gptel-fsm)" gptel))
  :group 'magent)

;;; Shared utilities

(defun magent-project-root ()
  "Return the project root directory.
Uses `magent-project-root-function' if set, then tries projectile
and project.el, falling back to `default-directory'."
  (or (when (bound-and-true-p magent-project-root-function)
        (funcall magent-project-root-function))
      (when (fboundp 'projectile-project-root)
        (ignore-errors (projectile-project-root)))
      (when (fboundp 'project-current)
        (ignore-errors
          (when-let ((proj (project-current nil)))
            (if (fboundp 'project-root)
                (project-root proj)
              (car (with-no-warnings (project-roots proj)))))))
      default-directory))

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
