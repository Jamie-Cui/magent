;;; magent-permission.el --- Permission system for OpenCode agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Permission system for controlling tool access per agent.
;; Based on opencode's PermissionNext system with rule-based access control.

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
   ;; Special cases
   (cons 'doom_loop magent-permission-ask)
   (cons 'external_directory magent-permission-ask)
   ;; File read restrictions (mirror .gitignore for .env)
   (cons 'read
         (list
          (cons '* magent-permission-allow)
          (cons "*.env" magent-permission-deny)
          (cons "*.env.*" magent-permission-deny)
          (cons "*.env.example" magent-permission-allow)))))

;;; Permission resolution

(defun magent-permission-resolve (rules tool &optional file)
  "Resolve permission for TOOL with optional FILE path.
Returns 'allow, 'deny, or 'ask.

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

     ;; Check for wildcard
     ((let ((wildcard (assq '* rules)))
        (and wildcard (cdr wildcard))))

     ;; Check tool-specific rule
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

     ;; No specific rule, check wildcard
     ((assq '* rules)
      (cdr (assq '* rules)))

     ;; Default to allow
     (t magent-permission-allow))))

(defun magent-permission--check-file-rules (rules file)
  "Check if FILE matches any rule in RULES.
RULES is an alist of (pattern . permission)."
  (let ((filename (file-name-nondirectory file))
        (basename (file-name-base file)))
    (catch 'found
      (dolist (rule rules)
        (let ((pattern (car rule))
              (permission (cdr rule)))
          (cond
           ;; Wildcard
           ((eq pattern '*)
            (throw 'found permission))

           ;; Glob pattern like "*.el"
           ((string-match-p "^\\*" pattern)
            (let ((ext (substring pattern 1))) ; Remove leading *
              (when (string-suffix-p ext filename)
                (throw 'found permission))))

           ;; Pattern with wildcards like "*.env.*"
           ((string-match-p "\\*" pattern)
            (when (string-match-p (wildcard-to-regexp pattern) filename)
              (throw 'found permission))))

           ;; Exact match
           ((string-equal pattern file)
            (throw 'found permission)))))
      magent-permission-allow)) ; Default if no match

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

;;; Permission merging

(defun magent-permission-merge (&rest rulesets)
  "Merge multiple RULESETS.
Later rules override earlier ones for same keys.
Returns merged alist."
  (let ((result nil))
    (dolist (ruleset (nreverse rulesets))
      (dolist (rule (if (magent-permission-p ruleset)
                        (magent-permission-rules ruleset)
                      ruleset))
        (let ((key (car rule))
              (value (cdr rule)))
          (if (and (consp value) (not (memq value '(allow deny ask))))
              ;; Nested rules - merge recursively
              (let ((existing (assq key result)))
                (if existing
                    (setcdr existing (magent-permission-merge (cdr existing) value))
                  (push (cons key value) result)))
            ;; Simple rule - just set/override
            (let ((existing (assq key result)))
              (if existing
                  (setcdr existing value)
                (push (cons key value) result)))))))
    result))

(defun magent-permission-from-config (config)
  "Convert CONFIG alist to permission rules.
CONFIG is an alist like ((read . allow) (bash . deny)).
Returns permission rules."
  (magent-permission-create :rules config))

;;; Permission filtering for tools

(defun magent-permission-filter-tools (permission-rules tools)
  "Filter TOOLS list based on PERMISSION-RULES.
Returns list of allowed tool names."
  (let ((allowed nil))
    (dolist (tool tools)
      (when (magent-permission-allow-p permission-rules tool)
        (push tool allowed)))
    (nreverse allowed)))

(defun magent-permission-disabled (permission-rules all-tools)
  "Get list of disabled tools from PERMISSION-RULES.
ALL-TABLES is list of all available tool names.
Returns list of tool names that are denied."
  (let ((disabled nil))
    (dolist (tool all-tools)
      (when (magent-permission-deny-p permission-rules tool)
        (push tool disabled)))
    disabled))

;;; User prompts for 'ask' permissions

(defun magent-permission-prompt-user (tool file)
  "Ask user for permission for TOOL with FILE.
Returns 'allow if user approves, 'deny if denied."
  (let ((prompt (format "Allow tool '%s'%s? "
                        tool
                        (if file (format " for file '%s'" file) ""))))
    (when (y-or-n-p prompt)
      magent-permission-allow)))

(provide 'magent-permission)
;;; magent-permission.el ends here
