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
  (expand-file-name "prompt.org"
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
  "Base name used when deriving Magent output buffer names.
Output buffers are named like `*magent:global*' or
`*magent:PROJECT-NAME*'."
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

(defvaralias 'magent-always-bypass-permission 'magent-by-pass-permission)
(make-obsolete-variable 'magent-always-bypass-permission
                        'magent-by-pass-permission "0.1.0")

(defcustom magent-by-pass-permission nil
  "DANGEROUS: Bypass permission checks for all tools.
When non-nil, all tool calls are executed without confirmation,
ignoring agent permission rules and user-defined overrides.
This is a security risk and should only be enabled in trusted
environments for debugging or automation purposes.

This variable is the canonical permission bypass flag used by
`magent-toggle-by-pass-permission' and permission checks."
  :type 'boolean
  :group 'magent
  :risky t)

(defcustom magent-enable-logging t
  "Enable logging to the *magent-log* buffer.
When enabled, API requests and responses are logged for debugging."
  :type 'boolean
  :group 'magent)

(defcustom magent-enable-audit-log t
  "Enable persistent audit logging for permission and sensitive actions.
When enabled, Magent writes a compact JSONL audit trail to disk."
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
  (expand-file-name "magent/sessions" user-emacs-directory)
  "Directory where session files are stored."
  :type 'directory
  :group 'magent)

(defcustom magent-audit-directory nil
  "Directory where persistent audit log files are stored.
When nil, audit logs are written under `magent-session-directory'/audit."
  :type '(choice (const :tag "Use magent-session-directory/audit" nil)
                 directory)
  :group 'magent)

(defcustom magent-audit-buffer-name "*magent-audit*"
  "Name of the buffer used for the Magent audit browser."
  :type 'string
  :group 'magent)

(defcustom magent-audit-preview-length 120
  "Maximum display width for persisted audit previews."
  :type 'integer
  :group 'magent)

(defcustom magent-audit-flush-delay 0.25
  "Idle delay in seconds before queued audit records are flushed to disk.
Audit writes are batched and deferred so permission and tool event
handlers do not block the main Emacs thread on every record."
  :type 'number
  :group 'magent)

(defcustom magent-audit-default-days 1
  "Number of recent days loaded by default in the audit browser.
When nil or non-positive, show all available audit records."
  :type '(choice (const :tag "All available audit records" nil)
                 integer)
  :group 'magent)

(defcustom magent-audit-max-records 200
  "Maximum number of audit records rendered in the audit browser.
Older matching records remain on disk and can be reached by
widening the time window or lowering filters in future UI
extensions."
  :type 'integer
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

(defcustom magent-emacs-eval-max-calls-per-turn 10
  "Maximum number of emacs_eval calls allowed in a single turn.
When non-nil, Magent stops further emacs_eval exploration after
this many calls in the same turn and forces the model to answer.
Exact duplicate emacs_eval forms are still intercepted even when
this count-based cap is disabled.  Set to nil to disable the
count-based cap."
  :type '(choice (const :tag "Disable count-based cap" nil)
                 (natnum :tag "Max calls per turn"))
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
If t, display reasoning blocks in the Magent UI and retain them.
If `ignore', hide reasoning from the Magent UI but still retain the
received reasoning text internally.
If nil, discard reasoning content entirely instead of showing or
retaining it.

In other words, `ignore' means \"hidden but kept\", while nil means
\"dropped\"."
  :type '(choice (const :tag "Display reasoning and keep it" t)
                 (const :tag "Hide reasoning but keep it internally" ignore)
                 (const :tag "Discard reasoning entirely" nil))
  :group 'magent)

(defcustom magent-ui-wrap-reasoning-in-think-block t
  "Whether to wrap displayed reasoning in `#+begin_think' blocks.
When nil, displayed reasoning is inserted as plain assistant text.
This only affects reasoning shown in the Magent UI, so it has no
effect unless `magent-include-reasoning' is t."
  :type 'boolean
  :group 'magent)

;; FIXME: native FSM backend (magent-fsm-backend-native.el) is not ready.
;; Currently only the gptel backend is supported.
(defcustom magent-fsm-backend 'gptel
  "FSM backend to use for tool calling loop.
`gptel' uses gptel's built-in FSM (gptel-request.el)."
  :type '(const gptel)
  :group 'magent)

(defcustom magent-auto-context t
  "Whether to automatically attach calling buffer context in `magent-dwim'.
When non-nil, buffer name, file path, major mode, line number,
and active region bounds are prepended to the submitted prompt."
  :type 'boolean
  :group 'magent)

(defcustom magent-enable-capabilities t
  "Whether to resolve context-aware capabilities for each request.
When non-nil, Magent selects a small set of active capabilities
and injects the instruction skills linked to them."
  :type 'boolean
  :group 'magent)

(defcustom magent-capability-max-active 3
  "Maximum number of capabilities to auto-activate per request."
  :type 'integer
  :group 'magent)

(defcustom magent-disabled-capabilities nil
  "List of capability names that should never auto-activate.
These names are checked by the capability resolver before any
automatic activation happens."
  :type '(repeat string)
  :group 'magent)

(defcustom magent-disabled-capability-families nil
  "List of capability family names that should never auto-activate.
Families are maintainer-defined strings carried by capability
metadata, for example a package workflow family or a debugging
family.  A locally enabled capability can still override this for
that one capability."
  :type '(repeat string)
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
;; to *magent-log*.  Guard the stub so reloading this file later does not
;; clobber the real logger in live Emacs sessions.

(unless (fboundp 'magent-log)
  (defun magent-log (format-string &rest args)
    "Log FORMAT-STRING with ARGS.  No-op until magent-ui is loaded."
    (ignore format-string args)))

(provide 'magent-config)
;;; magent-config.el ends here
