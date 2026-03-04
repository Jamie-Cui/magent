;;; magent-permission.el --- Permission system for Magent agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Permission system for controlling tool access per agent.
;; Rule-based access control for Magent agents.

;;; Code:

(require 'cl-lib)

;;; Permission structure

(cl-defstruct (magent-permission
               (:constructor magent-permission-create)
               (:copier nil))
  "Structure representing permission rules.

A permission can be:
- A symbol: 'allow, 'deny, or 'ask
- A list: For nested permissions (e.g., per-tool or per-file rules)
- nil: Use default behavior

Examples:
  'allow                    => Allow everything
  'deny                     => Deny everything
  '(read allow)             => Allow read tool
  '(read (\"*.el\" allow))   => Allow .el files for read
  '(bash deny)              => Deny bash tool"
  rules)

;;; Permission types

(defconst magent-permission-allow 'allow)
(defconst magent-permission-deny 'deny)
(defconst magent-permission-ask 'ask)

;;; Default permissions

(defun magent-permission-defaults ()
  "Get the default permission ruleset."
  (list
   ;; Default: allow most tools
   (cons '* magent-permission-allow)
   ;; Sensitive tools require user confirmation
   (cons 'bash magent-permission-ask)
   (cons 'emacs_eval magent-permission-ask)
   (cons 'doom_loop magent-permission-ask)
   (cons 'external_directory magent-permission-ask)
   (cons 'delegate magent-permission-allow)
   ;; File read restrictions (mirror .gitignore for .env)
   ;; More specific patterns must come before less specific ones.
   (cons 'read
         (list
          (cons '* magent-permission-allow)
          (cons "*.env.example" magent-permission-allow)
          (cons "*.env" magent-permission-deny)
          (cons "*.env.*" magent-permission-deny)))
   ;; File write requires confirmation, .env files denied
   (cons 'write
         (list
          (cons '* magent-permission-ask)
          (cons "*.env.example" magent-permission-allow)
          (cons "*.env" magent-permission-deny)
          (cons "*.env.*" magent-permission-deny)))
   ;; File edit: allowed by default, .env files denied
   (cons 'edit
         (list
          (cons '* magent-permission-allow)
          (cons "*.env.example" magent-permission-allow)
          (cons "*.env" magent-permission-deny)
          (cons "*.env.*" magent-permission-deny)))))

;;; Permission resolution

(defun magent-permission-resolve (rules tool &optional file)
  "Resolve permission for TOOL with optional FILE path.
Returns \\='allow, \\='deny, or \\='ask.

RULES is a permission structure (list or symbol).
TOOL is the tool name (symbol or string).
FILE is optional file path to check."
  (let ((rules (if (magent-permission-p rules)
                   (magent-permission-rules rules)
                 rules)))
    (cond
     ;; Rules is a single symbol
     ((memq rules '(allow deny ask))
      rules)

     ;; Empty rules - default to allow
     ((null rules)
      magent-permission-allow)

     ;; Check tool-specific rule first (takes priority over wildcard)
     ((assq tool rules)
      (let ((tool-rule (assq tool rules)))
        (if (consp (cdr tool-rule))
            ;; Has nested rules (e.g., file-specific)
            (if file
                (magent-permission--check-file-rules (cdr tool-rule) file)
              ;; No file specified, check if tool is explicitly allowed/denied
              (let ((default (assq '* (cdr tool-rule))))
                (if default
                    (cdr default)
                  magent-permission-allow)))
          ;; Simple allow/deny/ask for tool
          (cdr tool-rule))))

     ;; Fall back to wildcard
     ((assq '* rules)
      (cdr (assq '* rules)))

     ;; Default to allow
     (t magent-permission-allow))))

(defun magent-permission--check-file-rules (rules file)
  "Check if FILE matches any rule in RULES.
RULES is an alist of (pattern . permission).
Specific patterns are checked first; the wildcard \\='* or \"*\" is used as fallback.
Pattern matching is attempted against both the full FILE path and its basename."
  (let ((filename (file-name-nondirectory file))
        (wildcard-permission nil))
    (catch 'found
      (dolist (rule rules)
        (let ((pattern (car rule))
              (permission (cdr rule)))
          (cond
           ;; Save wildcard for fallback (symbol '* or string "*")
           ((or (eq pattern '*) (equal pattern "*"))
            (setq wildcard-permission permission))

           ;; String pattern
           ((stringp pattern)
            (cond
             ;; Glob pattern like "*.env", "*.el", or "src/**/*.env"
             ((string-match-p "\\*" pattern)
              (let ((regexp (wildcard-to-regexp pattern)))
                (when (or (string-match-p regexp filename)
                          (string-match-p regexp file))
                  (throw 'found permission))))
             ;; Exact match against full path or basename
             ((or (string-equal pattern file)
                  (string-equal pattern filename))
              (throw 'found permission)))))))
      ;; No specific match — use wildcard if present, else allow
      (or wildcard-permission magent-permission-allow))))

;;; Permission checking

(defun magent-permission-allow-p (rules tool &optional file)
  "Check if TOOL is allowed (with optional FILE).
Returns t if allowed, nil otherwise."
  (eq (magent-permission-resolve rules tool file) magent-permission-allow))

(defun magent-permission-deny-p (rules tool &optional file)
  "Check if TOOL is denied (with optional FILE).
Returns t if denied, nil otherwise."
  (eq (magent-permission-resolve rules tool file) magent-permission-deny))

(defun magent-permission-ask-p (rules tool &optional file)
  "Check if TOOL requires user confirmation (with optional FILE).
Returns t if ask, nil otherwise."
  (eq (magent-permission-resolve rules tool file) magent-permission-ask))

;;; Session overrides

(defvar magent-permission--session-overrides (make-hash-table :test 'eq)
  "Session-level permission overrides.
Maps permission key symbols to `allow' or `deny'.
Set by user responses to interactive prompts (Always allow / Deny always).
Cleared on session reset.")

(defun magent-permission-session-override (perm-key)
  "Return session override for PERM-KEY, or nil if none."
  (gethash perm-key magent-permission--session-overrides))

(defun magent-permission-set-session-override (perm-key value)
  "Set session override for PERM-KEY to VALUE (\\='allow or \\='deny)."
  (puthash perm-key value magent-permission--session-overrides))

(defun magent-permission-clear-session-overrides ()
  "Clear all session-level permission overrides."
  (clrhash magent-permission--session-overrides))

;;; Tool availability

(defun magent-permission-tool-available-p (rules tool)
  "Return t if TOOL should appear in the tool list for RULES.
Unlike `magent-permission-allow-p', this returns t for tools with
\\='ask permission and for tools with nested file rules where at
least one pattern grants access (e.g., plan agent where edit
defaults to \\='deny but specific paths are \\='allow)."
  (let* ((effective (if (magent-permission-p rules)
                        (magent-permission-rules rules)
                      rules))
         (tool-rule (and (listp effective) (assq tool effective)))
         (tool-value (and tool-rule (cdr tool-rule))))
    (if (and (consp tool-value)
             (not (memq tool-value '(allow deny ask))))
        ;; Nested file rules: available if ANY sub-rule grants access
        (cl-some (lambda (sub-rule)
                   (memq (cdr sub-rule) '(allow ask)))
                 tool-value)
      ;; Simple case: delegate to existing resolver
      (memq (magent-permission-resolve rules tool) '(allow ask)))))

;;; Permission merging

(defun magent-permission-merge (&rest rulesets)
  "Merge multiple RULESETS.
Later rules override earlier ones for same keys.
Returns merged alist.  Each ruleset can be an alist, a `magent-permission'
struct, or a bare symbol (allow/deny/ask)."
  (let ((result nil))
    (dolist (ruleset rulesets)
      (let ((rules (cond
                    ((magent-permission-p ruleset)
                     (magent-permission-rules ruleset))
                    ((memq ruleset '(allow deny ask))
                     ;; Bare symbol — treat as wildcard rule
                     (list (cons '* ruleset)))
                    (t ruleset))))
        (dolist (rule rules)
          (let ((key (car rule))
                (value (cdr rule)))
            (if (and (consp value) (not (memq (car value) '(allow deny ask))))
                ;; Nested rules - merge recursively
                (let ((existing (assq key result)))
                  (if existing
                      (setcdr existing (magent-permission-merge (cdr existing) value))
                    (push (cons key value) result)))
              ;; Simple rule - just set/override
              (let ((existing (assq key result)))
                (if existing
                    (setcdr existing value)
                  (push (cons key value) result))))))))
    result))

(defun magent-permission-from-config (config)
  "Convert CONFIG alist to permission rules.
CONFIG is an alist like ((read . allow) (bash . deny)).
Returns permission rules."
  (magent-permission-create :rules config))

(provide 'magent-permission)
;;; magent-permission.el ends here
