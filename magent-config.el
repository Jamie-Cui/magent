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

;;; Faces

(defface magent-user-header
  '((t :inherit (bold font-lock-keyword-face)))
  "Face for USER heading labels."
  :group 'magent)

(defface magent-assistant-header
  '((t :inherit (bold font-lock-function-name-face)))
  "Face for ASSISTANT heading labels."
  :group 'magent)

(defface magent-tool-header
  '((t :inherit (bold font-lock-type-face)))
  "Face for `#+begin_tool'/`#+end_tool' lines."
  :group 'magent)

(defface magent-tool-args
  '((t :inherit font-lock-comment-face))
  "Face for tool input arguments."
  :group 'magent)

(defface magent-tool-result
  '((t :inherit font-lock-string-face))
  "Face for `-> result' text in tool blocks."
  :group 'magent)

(defface magent-error-header
  '((t :inherit (bold error)))
  "Face for error heading labels."
  :group 'magent)

(defface magent-error-body
  '((t :inherit error))
  "Face for error body text."
  :group 'magent)

(defface magent-reasoning-header
  '((t :inherit (bold font-lock-doc-face)))
  "Face for `#+begin_think'/`#+end_think' lines."
  :group 'magent)

(defface magent-separator
  '((t :inherit shadow :strike-through t))
  "Face for separator lines between conversation turns."
  :group 'magent)

(defface magent-strike-through
  '((t :inherit shadow :strike-through t))
  "Face for the dash line after section headers."
  :group 'magent)

(defvar magent--prompt-file
  (expand-file-name "prompt.txt"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the system prompt file.")

(defcustom magent-system-prompt
  (with-temp-buffer
    (insert-file-contents magent--prompt-file)
    (buffer-string))
  "System prompt for the AI agent.
Default value is read from prompt.txt next to this file."
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

(defcustom magent-ui-header-strike-through nil
  "Whether to draw a strike-through line after section headers.
When non-nil, a dash line extends from the header label to the
right window edge using the `magent-strike-through' face."
  :type 'boolean
  :group 'magent)

(defcustom magent-ui-separator-char ?\s
  "Character for separator lines between conversation turns.
A whitespace character inserts literal spacing; a graphic character
draws a full-width line with `magent-separator' face.
Set to nil to disable separators."
  :type '(choice character (const :tag "Disabled" nil))
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

;; FIXME: native FSM backend (magent-fsm-backend-native.el) is not ready.
;; Currently only the gptel backend is supported.
(defcustom magent-fsm-backend 'gptel
  "FSM backend to use for tool calling loop.
`gptel' uses gptel's built-in FSM (gptel-request.el)."
  :type '(const gptel)
  :group 'magent)

(defcustom magent-queue-max-size 20
  "Maximum number of prompts that may be queued while processing.
When the queue is full, new prompts are rejected with an error."
  :type 'integer
  :group 'magent)

;;; Shared utilities

(defmacro magent--with-display-buffer (name &rest body)
  "Populate buffer NAME with BODY and display it in `special-mode'.
The buffer is created if needed.  The mode is set before content
insertion so that `kill-all-local-variables' does not wipe locals
set by BODY.  BODY runs with `inhibit-read-only' bound to t."
  (declare (indent 1))
  `(let ((buf (get-buffer-create ,name)))
     (with-current-buffer buf
       (unless (derived-mode-p 'special-mode)
         (special-mode))
       (let ((inhibit-read-only t))
         (erase-buffer)
         ,@body))
     (display-buffer buf)))

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
