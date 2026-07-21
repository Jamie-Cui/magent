;;; magent-config.el --- Configuration for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Configuration module for Magent using Emacs customize groups.
;; LLM provider, model, and API key settings are managed by gptel.
;; See `gptel-backend', `gptel-model', and `gptel-api-key'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'magent-log)
(require 'magent-prompt)
(require 'subr-x)

(defgroup magent nil
  "Magent AI coding agent for Emacs."
  :prefix "magent-"
  :group 'tools
  :link '(url-link :tag "GitHub" "https://github.com/jamie-cui/magent"))

(defcustom magent-enable-logging t
  "Whether to write Magent diagnostics to `magent-log-buffer-name'."
  :type 'boolean
  :group 'magent)

(defcustom magent-log-level 'info
  "Minimum severity written to the Magent diagnostic log buffer."
  :type '(choice (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warn" warn)
                 (const :tag "Error" error))
  :group 'magent)

(defcustom magent-log-buffer-name "*magent-log*"
  "Name of the buffer used for Magent diagnostic logging."
  :type 'string
  :group 'magent)

(defcustom magent-system-prompt
  (magent-prompt-read "system.org")
  "System prompt for the AI agent.
The default value is read from prompts/system.org in the package."
  :type 'string
  :group 'magent)

(defcustom magent-skill-search-endpoint "https://skills.sh/api/search"
  "HTTP endpoint used by `magent-find-skill'."
  :type 'string
  :group 'magent)

(defcustom magent-skill-search-limit 10
  "Maximum number of results displayed by `magent-find-skill'."
  :type 'natnum
  :group 'magent)

(defcustom magent-skill-search-timeout 15
  "Seconds before a pending `magent-find-skill' request is cancelled."
  :type 'natnum
  :group 'magent)

(defcustom magent-skill-install-max-files 500
  "Maximum number of files accepted in one installed skill."
  :type 'natnum
  :group 'magent)

(defcustom magent-skill-install-max-bytes (* 10 1024 1024)
  "Maximum total byte size accepted in one installed skill."
  :type 'natnum
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

(defcustom magent-org-roam-directory nil
  "Directory where Magent writes repository summary notes.
When nil, use `org-roam-directory' if that variable is bound.  Magent
does not create this directory automatically; configure it to an existing
org-roam directory before using the `/summarize' skill."
  :type '(choice (const :tag "Use org-roam-directory" nil)
                 (directory :tag "Org-roam directory"))
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
limit replaces the next continuation with one provider-tools-disabled final
request.  Set to 0 to disable."
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

(defcustom magent-agent-shell-session-strategy 'new
  "Agent Shell session strategy used by Magent buffers.
The setting applies both to Magent entry commands and to Magent selected from
the generic agent-shell picker.  The default starts a fresh session immediately,
avoiding agent-shell's global session picker.  Set this to `prompt' or `latest'
when Magent should use agent-shell's session selection flow."
  :type '(choice (const :tag "Always start new session" new)
                 (const :tag "Load latest session" latest)
                 (const :tag "Prompt for session" prompt))
  :group 'magent)

(defcustom magent-load-custom-agents t
  "Whether to load custom agents from .magent/agent/*.md files."
  :type 'boolean
  :group 'magent)

(defcustom magent-trusted-project-skill-companion-roots nil
  "Project roots allowed to load tool-skill companion Elisp files.

Project-local `.magent/skills/*/*.el' files are executable Emacs Lisp and are
not loaded unless their normalized project root appears in this list.
Instruction-only project skills do not require trust.  Built-in and user-level
tool skills retain their existing loading behavior."
  :type '(repeat directory)
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
  "Directory where isolated Magent command session files are stored.
When nil, command sessions are stored under
`magent-session-directory'/commands."
  :type '(choice (const :tag "Use magent-session-directory/commands" nil)
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

(defcustom magent-bash-program "bash"
  "Bash-compatible executable used by the bash tool.
Commands run with pipefail enabled and errexit disabled.  If this executable
cannot be resolved, bash tool calls fail without affecting other Magent tools."
  :type 'string
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
  "Maximum tool output body characters kept in model-visible history.
Longer bodies are truncated before they are recorded in the session ledger,
legacy messages, context items, and subsequent gptel prompts.  Structured
failure status headers are always retained outside this limit."
  :type '(choice (const :tag "Do not truncate tool results" nil)
                 integer)
  :group 'magent)

(defcustom magent-tool-result-model-preview-length 10000
  "Body prefix characters retained when a tool result is truncated.
The final record also includes a compact truncation notice with the original
body length.  Structured failure status headers are not part of this budget."
  :type 'integer
  :group 'magent)

(defcustom magent-command-buffer-context-max-chars 24000
  "Maximum total buffer-content characters attached by one command turn.
The budget is shared by the buffers matched by a standard command turn in
declaration and `buffer-list' order.  Longer content is retained around each
buffer's point and receives a model-visible truncation notice.  Nil disables
Magent-owned truncation for command buffer context."
  :type '(choice (const :tag "Do not truncate command buffer context" nil)
                 natnum)
  :group 'magent)

(defcustom magent-session-save-idle-delay 0.25
  "Idle delay in seconds before UI-triggered session saves run.
Explicit calls to `magent-session-save' remain synchronous."
  :type 'number
  :group 'magent)

(defcustom magent-session-journal-max-events 2000
  "Maximum recent ledger events persisted beside a session snapshot.
The in-memory journal remains append-only.  Saved session files retain a
bounded replay/audit tail because the materialized snapshot already contains
all state through its `last-event-seq'.  Nil keeps the full journal."
  :type '(choice (const :tag "Keep the full journal" nil)
                 (natnum :tag "Recent events"))
  :group 'magent)

(defcustom magent-child-agent-max-depth 1
  "Maximum depth for recursive child-agent spawning.
A value of 1 allows the visible root agent to spawn direct child agents,
but prevents those children from spawning their own children.  Set to nil
to disable the depth guard."
  :type '(choice (const :tag "No child-agent depth limit" nil)
                 (natnum :tag "Maximum child-agent depth"))
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

(defcustom magent-project-instruction-file-names '("AGENTS.md")
  "Project instruction file names discovered for each request.
Files are loaded from the project root toward request-local resource paths.
Deeper files are appended later so their narrower scope is explicit."
  :type '(repeat string)
  :group 'magent)

(defcustom magent-project-instructions-max-bytes 65536
  "Maximum total bytes loaded from project instruction files per request.
Set to nil to disable project instruction discovery."
  :type '(choice (const :tag "Disabled" nil)
                 (natnum :tag "Maximum bytes"))
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
  "Default timeout in seconds for one Magent doctor probe.
Set to 0 to disable this per-probe limit."
  :type 'number
  :group 'magent)

(defcustom magent-doctor-process-timeout 10
  "Default timeout in seconds for a doctor probe subprocess.
Set to 0 to disable this subprocess limit."
  :type 'number
  :group 'magent)

(defcustom magent-doctor-total-timeout 30
  "Maximum seconds spent collecting local Magent doctor diagnostics.
Set to 0 to disable the total collection limit."
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
              (project-root proj))))
        (unless no-fallback
          default-directory))))

(provide 'magent-config)
;;; magent-config.el ends here
