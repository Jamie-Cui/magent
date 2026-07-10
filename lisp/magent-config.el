;;; magent-config.el --- Configuration for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Configuration module for Magent using Emacs customize groups.
;; LLM provider, model, and API key settings are managed by gptel.
;; See `gptel-backend', `gptel-model', and `gptel-api-key'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

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
  "Face for compact tool status rows."
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
  "Face for compact reasoning status rows."
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
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; prompt.org lives at the package root.  In the git repo sources are
    ;; under lisp/ (one level down); after MELPA install lisp/*.el files
    ;; are flattened to the top level.  Try sibling first, then parent.
    (or (let ((f (expand-file-name "prompt.org" dir)))
          (and (file-exists-p f) f))
        (let ((f (expand-file-name "prompt.org"
                                   (expand-file-name ".." dir))))
          (and (file-exists-p f) f))
        ;; Fallback: original location (lets the error surface if missing).
        (expand-file-name "prompt.org" dir)))
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

(defcustom magent-ui-backend 'agent-shell
  "UI backend used by Magent public commands."
  :type '(choice (const :tag "Agent Shell" agent-shell)
                 (const :tag "Legacy Magent UI" legacy))
  :group 'magent)

(defcustom magent-agent-shell-session-strategy 'new
  "Agent Shell session strategy used by Magent entry points.
The default starts a fresh session immediately, avoiding agent-shell's global
session picker on every `magent-dwim'.  Set this to `prompt' or `latest' when
you want Magent's agent-shell backend to use agent-shell's session selection
flow."
  :type '(choice (const :tag "Always start new session" new)
                 (const :tag "Load latest session" latest)
                 (const :tag "Prompt for session" prompt))
  :group 'magent)

(defcustom magent-compose-window-height 0.25
  "Height used when displaying the Magent compose popup.
If this is an integer, it is interpreted as a number of lines.  If
it is a float, it is interpreted as a fraction of the frame height."
  :type '(choice (integer :tag "Lines")
                 (float :tag "Frame fraction"))
  :group 'magent)

(defcustom magent-compose-close-after-submit t
  "Whether to close the Magent compose popup after submitting a prompt.
When non-nil, submitting from compose closes its window and selects the
Magent workspace buffer.  When nil, the compose window remains open."
  :type 'boolean
  :group 'magent)

(defcustom magent-auto-scroll t
  "Automatically scroll output buffer when new content arrives."
  :type 'boolean
  :group 'magent)

(defcustom magent-enable-tools '(read write edit grep glob bash emacs_eval agent skill web_search)
  "List of enabled tools.
Available tools: read, write, edit, grep, glob, bash, emacs_eval,
agent, skill, web_search."
  :type '(set (const :tag "Read files" read)
              (const :tag "Write files" write)
              (const :tag "Edit files" edit)
              (const :tag "Search content (grep)" grep)
              (const :tag "Find files (glob)" glob)
              (const :tag "Run shell commands" bash)
              (const :tag "Evaluate Emacs Lisp" emacs_eval)
              (const :tag "Coordinate child agents" agent)
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
If no callback activity occurs within this period, the request
transitions to an error state.  Set to 0 to disable."
  :type 'integer
  :group 'magent)

(defcustom magent-max-sampling-requests 0
  "Compatibility safety guard for model sampling requests in one turn.
Magent normally follows Codex-style turn continuation and does not impose a
per-turn sampling limit.  When this value is positive, the initial request
counts as one and tool-output continuations count as additional requests; the
turn fails once the limit is reached.  Set to 0 to disable."
  :type 'integer
  :group 'magent)

(defconst magent-effort-values '(minimal low medium high xhigh)
  "Canonical Magent reasoning effort values sent to providers.")

(defconst magent-effort-option-values
  (cons 'auto magent-effort-values)
  "Reasoning effort values exposed to user-facing selectors.
`auto' means provider or model default and is not sent as a request
parameter.")

(defcustom magent-default-effort nil
  "Default reasoning effort for Magent requests.
Nil means provider or model default.  Per-session agent-shell settings,
request context, and agent definitions can override this value."
  :type '(choice (const :tag "Provider default" nil)
                 (const :tag "Minimal" minimal)
                 (const :tag "Low" low)
                 (const :tag "Medium" medium)
                 (const :tag "High" high)
                 (const :tag "Extra high" xhigh))
  :group 'magent)

(defcustom magent-effort-unsupported-policy 'warn-and-downgrade
  "How Magent handles an effort level unsupported by a provider adapter.
`warn-and-downgrade' downgrades `xhigh' to `high' when a known request
shape supports effort but not `xhigh', and otherwise logs and ignores
unsupported effort settings.  `ignore' silently omits unsupported effort
parameters.  `error' signals an error before sending the request."
  :type '(choice (const :tag "Warn and downgrade when possible" warn-and-downgrade)
                 (const :tag "Ignore" ignore)
                 (const :tag "Error" error))
  :group 'magent)

(defun magent-effort-normalize-option (effort)
  "Return EFFORT as a canonical option symbol, or nil when absent.
Recognized option symbols are `auto', `minimal', `low', `medium',
`high', and `xhigh'.  String values are accepted for file-backed and
wire-protocol configuration."
  (let* ((raw (cond
               ((null effort) nil)
               ((symbolp effort) (symbol-name effort))
               ((stringp effort) effort)
               (t (format "%s" effort))))
         (key (and raw
                   (downcase
                    (replace-regexp-in-string
                     "[_[:space:]]+" "-"
                     (string-trim raw))))))
    (pcase key
      ((or 'nil "") nil)
      ((or "auto" "default" "provider-default") 'auto)
      ("minimal" 'minimal)
      ("low" 'low)
      ("medium" 'medium)
      ("high" 'high)
      ((or "xhigh" "x-high" "extra-high" "extra-highest") 'xhigh)
      (_ (error "Invalid Magent effort value: %S" effort)))))

(defun magent-effort-option-or-auto (effort)
  "Return normalized EFFORT option, defaulting nil to `auto'."
  (or (magent-effort-normalize-option effort) 'auto))

(defun magent-effort-effective (effort)
  "Return provider-facing EFFORT symbol, or nil for `auto'/absent."
  (let ((option (magent-effort-normalize-option effort)))
    (unless (eq option 'auto)
      option)))

(defun magent-effort-option-string (effort)
  "Return EFFORT option as an ACP/frontmatter string."
  (symbol-name (magent-effort-option-or-auto effort)))

(defcustom magent-bypass-permission nil
  "DANGEROUS: Bypass permission checks for all tools.
When non-nil, all tool calls are executed without confirmation,
ignoring agent permission rules and user-defined overrides.
This is a security risk and should only be enabled in trusted
environments for debugging or automation purposes.

This variable is the canonical permission bypass flag used by
`magent-toggle-bypass-permission' and permission checks."
  :type 'boolean
  :group 'magent
  :risky t)

(defcustom magent-enable-logging t
  "Enable logging to the *magent-log* buffer.
When enabled, API requests and responses are logged for debugging."
  :type 'boolean
  :group 'magent)

(defcustom magent-log-level 'info
  "Minimum log severity written to the `*magent-log*' buffer.
Recognized message prefixes are DEBUG, INFO, WARN, ERROR, and PERM.
`PERM' is treated as `info', and messages without a recognized prefix
are also treated as `info'."
  :type '(choice (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warn" warn)
                 (const :tag "Error" error))
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

(defcustom magent-command-session-directory nil
  "Directory where Magent internal command session files are stored.
When nil, internal command sessions are stored under
`magent-session-directory'/internal."
  :type '(choice (const :tag "Use magent-session-directory/internal" nil)
                 directory)
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

(defcustom magent-tool-result-model-max-length 12000
  "Maximum characters of any tool result kept in model-visible history.
Longer results are truncated before they are recorded in the session
ledger, legacy messages, context items, and subsequent gptel prompts."
  :type '(choice (const :tag "Do not truncate tool results" nil)
                 integer)
  :group 'magent)

(defcustom magent-tool-result-model-preview-length 10000
  "Prefix characters retained when a tool result is model-visible truncated.
The final record also includes a compact truncation notice with the
original result length."
  :type 'integer
  :group 'magent)

(defcustom magent-session-save-idle-delay 0.25
  "Idle delay in seconds before UI-triggered session saves run.
Explicit calls to `magent-session-save' remain synchronous."
  :type 'number
  :group 'magent)

(defcustom magent-child-agent-max-depth 1
  "Maximum depth for recursive child-agent spawning.
A value of 1 allows the visible root agent to spawn direct child agents,
but prevents those children from spawning their own children.  Set to nil
to disable the depth guard."
  :type '(choice (const :tag "No child-agent depth limit" nil)
                 (natnum :tag "Maximum child-agent depth"))
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

(defcustom magent-evil-reset-input-method-after-submit t
  "Whether Magent clears Evil's input method state after prompt submission.

When non-nil, submitting from a Magent compose buffer clears Evil's
buffer-local `evil-input-method' after returning to normal state.  This
prevents Evil from restoring a stale input method, such as rime, when the
compose buffer later re-enters insert state."
  :type 'boolean
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

(defcustom magent-memory-directory
  (expand-file-name "magent/memory/" user-emacs-directory)
  "Directory where Magent stores local Emacs profile memory."
  :type 'directory
  :group 'magent)

(defcustom magent-memory-file-name "emacs-profile.org"
  "File name used for the active Emacs profile memory Org file."
  :type 'string
  :group 'magent)

(defcustom magent-memory-use-llm t
  "Whether memory init and refresh use the current gptel provider.
When nil, Magent writes a deterministic skeleton and source index without
sending any configuration excerpts to the provider."
  :type 'boolean
  :group 'magent)

(defcustom magent-memory-open-after-write t
  "Whether interactive memory init and refresh open the memory file."
  :type 'boolean
  :group 'magent)

(defcustom magent-memory-enable-auto-injection t
  "Whether Magent may inject relevant Emacs profile memory into prompts."
  :type 'boolean
  :group 'magent)

(defcustom magent-memory-scan-custom-file nil
  "Whether memory scans may read `custom-file' contents.
When nil, the custom file path is recorded but its contents are not read."
  :type 'boolean
  :group 'magent)

(defcustom magent-memory-max-scan-bytes 200000
  "Maximum total bytes read during one Emacs profile memory scan."
  :type 'integer
  :group 'magent)

(defcustom magent-memory-max-file-bytes 30000
  "Maximum bytes read from any one file during memory scanning."
  :type 'integer
  :group 'magent)

(defcustom magent-memory-max-files 80
  "Maximum number of files read during one Emacs profile memory scan."
  :type 'integer
  :group 'magent)

(defcustom magent-memory-extra-scan-roots nil
  "Additional Emacs configuration roots included in memory scan plans."
  :type '(repeat directory)
  :group 'magent)

(defcustom magent-memory-exclude-patterns
  '("/\\.git/"
    "/\\.cache/"
    "/auto-save-list/"
    "/eln-cache/"
    "/elpa/"
    "/straight/repos/"
    "/straight/build/"
    "/elpaca/repos/"
    "/elpaca/builds/"
    "/var/"
    "/cache/")
  "Regexps for paths excluded from Emacs profile memory scans."
  :type '(repeat regexp)
  :group 'magent)

(defcustom magent-memory-injection-max-chars 6000
  "Maximum characters of Emacs profile memory injected into a prompt."
  :type 'integer
  :group 'magent)

(defcustom magent-memory-max-injected-sections 3
  "Maximum number of memory sections injected into one prompt."
  :type 'integer
  :group 'magent)

(defcustom magent-doctor-probe-timeout 2
  "Default timeout in seconds for one Magent doctor probe."
  :type 'number
  :group 'magent)

(defcustom magent-doctor-process-timeout 10
  "Default timeout in seconds for a doctor probe subprocess."
  :type 'number
  :group 'magent)

(defcustom magent-doctor-total-timeout 30
  "Maximum seconds spent collecting local Magent doctor diagnostics."
  :type 'number
  :group 'magent)

(defcustom magent-doctor-max-diagnostic-chars 24000
  "Maximum characters sent in one Magent doctor diagnostic bundle."
  :type 'integer
  :group 'magent)

(defcustom magent-doctor-max-probe-chars 8000
  "Maximum encoded characters retained from one Magent doctor probe."
  :type 'integer
  :group 'magent)

(defcustom magent-doctor-source-context-max-chars 6000
  "Maximum source characters included by the manual doctor source probe."
  :type 'integer
  :group 'magent)

(defcustom magent-doctor-log-max-lines 100
  "Maximum recent lines read from one allowlisted doctor log buffer."
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

(defun magent-project-root (&optional directory no-fallback)
  "Return the project root for DIRECTORY.
Uses `magent-project-root-function' if set, then tries projectile
and project.el.  Unless NO-FALLBACK is non-nil, return DIRECTORY
(or `default-directory') if no project root can be determined."
  (let ((default-directory (or directory default-directory)))
    (or (when (bound-and-true-p magent-project-root-function)
          (funcall magent-project-root-function))
        (when (fboundp 'projectile-project-root)
          (ignore-errors (projectile-project-root)))
        (when (fboundp 'project-current)
          (ignore-errors
            (when-let* ((proj (project-current nil)))
              (if (fboundp 'project-root)
                  (project-root proj)
                (car (with-no-warnings (project-roots proj)))))))
        (unless no-fallback
          default-directory))))

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
