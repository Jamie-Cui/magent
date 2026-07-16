;;; magent-permission.el --- Permission system for Magent agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Permission system for controlling tool access per agent.
;; Rule-based access control for Magent agents.

;;; Code:

(require 'cl-lib)
(require 'magent-session)

(defvar magent-bypass-permission)  ; from magent-config.el

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

(cl-defstruct (magent-permission-intersection
               (:constructor magent-permission-intersection-create)
               (:copier nil))
  "A permission profile restricted by every profile in PROFILES.

Keeping the source profiles intact is important for nested resource rules:
flattening an intersection to one decision per tool would discard path-specific
allow, ask, and deny rules."
  profiles)

;;; Permission types

(defconst magent-permission-allow 'allow)
(defconst magent-permission-deny 'deny)
(defconst magent-permission-ask 'ask)

(defconst magent-permission-keys
  '(read write edit grep glob bash emacs_eval agent skill web_search)
  "Canonical tool permission group keys used across Magent.")

;;;###autoload
(defun magent-toggle-bypass-permission (&optional arg)
  "Toggle Magent permission bypass mode.
With prefix ARG, enable bypass when ARG is positive and disable it
otherwise.  When bypass is enabled, Magent ignores per-agent
allow/deny/ask rules, session overrides, and file-specific
permission rules."
  (interactive "P")
  (setq magent-bypass-permission
        (if arg
            (> (prefix-numeric-value arg) 0)
          (not magent-bypass-permission)))
  (message "Magent permission bypass %s"
           (if magent-bypass-permission "enabled" "disabled"))
  magent-bypass-permission)

(defun magent-permission-bypass-p ()
  "Return non-nil when Magent should bypass permission checks."
  magent-bypass-permission)

;;; Default permissions

(defun magent-permission-defaults ()
  "Get the default permission ruleset."
  (list
   ;; Default: allow most tools
   (cons '* magent-permission-allow)
   ;; Sensitive tools require user confirmation
   (cons 'bash magent-permission-ask)
   (cons 'emacs_eval magent-permission-ask)
   (cons 'agent magent-permission-allow)
   (cons 'web_search magent-permission-allow)
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

(defconst magent-permission--decision-rank
  '((deny . 0) (ask . 1) (allow . 2))
  "Ordering used when intersecting permission decisions.")

(defun magent-permission--normalize-tool (tool)
  "Return canonical permission key for TOOL when TOOL is a string.
Existing symbols are reused with `intern-soft', so custom permission keys are
honored without interning arbitrary provider input.  Truly unknown strings
remain strings and fall back to the wildcard rule."
  (if (stringp tool)
      (or (and (string= tool "*") '*)
          (cl-find tool magent-permission-keys
                   :key #'symbol-name :test #'string=)
          (intern-soft tool)
          tool)
    tool))

(defun magent-permission--more-restrictive (left right)
  "Return the more restrictive permission decision of LEFT and RIGHT."
  (let ((left-rank (cdr (assq left magent-permission--decision-rank)))
        (right-rank (cdr (assq right magent-permission--decision-rank))))
    (if (<= (or left-rank 2) (or right-rank 2)) left right)))

(defun magent-permission-intersect (&rest profiles)
  "Return a permission profile restricted by all PROFILES.

Nested intersections are flattened, but their original resource rules are
preserved and resolved at invocation time."
  (let (flattened)
    (dolist (profile profiles)
      (if (magent-permission-intersection-p profile)
          (setq flattened
                (append flattened
                        (magent-permission-intersection-profiles profile)))
        (push profile flattened)))
    (setq flattened (nreverse flattened))
    (cond
     ((null flattened) nil)
     ((null (cdr flattened)) (car flattened))
     (t (magent-permission-intersection-create :profiles flattened)))))

(defun magent-permission-resolve (rules tool &optional file project-root)
  "Resolve permission for TOOL with optional FILE path.
Returns \\='allow, \\='deny, or \\='ask.

RULES is a permission structure (list, symbol, or intersection).
TOOL is the tool name (symbol or string).
FILE is optional file path to check.  PROJECT-ROOT, when non-nil, lets relative
resource patterns match the canonical project-relative form of FILE."
  (setq tool (magent-permission--normalize-tool tool))
  (if (magent-permission-intersection-p rules)
      (let ((decisions
             (mapcar (lambda (profile)
                       (magent-permission-resolve
                        profile tool file project-root))
                     (magent-permission-intersection-profiles rules))))
        (if decisions
            (cl-reduce #'magent-permission--more-restrictive decisions)
          magent-permission-allow))
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
                (magent-permission--check-file-rules
                 (cdr tool-rule) file project-root)
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
     (t magent-permission-allow)))))

(defun magent-permission--project-relative-path (file project-root)
  "Return FILE relative to PROJECT-ROOT when FILE is inside that root."
  (when (and (stringp file)
             (stringp project-root)
             (file-name-absolute-p file))
    (let ((relative (file-relative-name
                     (expand-file-name file)
                     (file-name-as-directory (expand-file-name project-root)))))
      (unless (or (equal relative "..")
                  (string-prefix-p "../" relative)
                  (file-name-absolute-p relative))
        relative))))

(defun magent-permission--glob-to-regexp (pattern)
  "Return an anchored, path-aware regexp for glob PATTERN.

A single `*' matches only within one path component.  A `**' matches across
path separators, and `**/' also matches zero directory components.  `?'
matches one non-separator character.  Other characters are literal."
  (let ((index 0)
        (length (length pattern))
        (regexp "\\`"))
    (while (< index length)
      (cond
       ((and (< (+ index 2) length)
             (eq (aref pattern index) ?*)
             (eq (aref pattern (1+ index)) ?*)
             (eq (aref pattern (+ index 2)) ?/))
        (setq regexp (concat regexp "\\(?:.*/\\)?")
              index (+ index 3)))
       ((and (< (1+ index) length)
             (eq (aref pattern index) ?*)
             (eq (aref pattern (1+ index)) ?*))
        (setq regexp (concat regexp ".*")
              index (+ index 2)))
       ((eq (aref pattern index) ?*)
        (setq regexp (concat regexp "[^/]*")
              index (1+ index)))
       ((eq (aref pattern index) ??)
        (setq regexp (concat regexp "[^/]")
              index (1+ index)))
       (t
        (setq regexp
              (concat regexp
                      (regexp-quote
                       (char-to-string (aref pattern index)))))
        (setq index (1+ index)))))
    (concat regexp "\\'")))

(defun magent-permission--resource-pattern-match-p
    (pattern file &optional project-root)
  "Return non-nil when resource PATTERN matches FILE.

Patterns containing a slash match the normalized full path or, when
PROJECT-ROOT applies, its project-relative path.  Slash-free patterns match
the basename, preserving the convenient `*.ext' behavior for files at any
depth."
  (let* ((case-fold-search nil)
         (normalized-pattern (subst-char-in-string ?\\ ?/ pattern))
         (normalized-file (subst-char-in-string ?\\ ?/ file))
         (relative (magent-permission--project-relative-path
                    normalized-file project-root))
         (regexp (magent-permission--glob-to-regexp normalized-pattern))
         (path-pattern-p (string-match-p "/" normalized-pattern)))
    (if path-pattern-p
        (or (string-match-p regexp normalized-file)
            (and relative (string-match-p regexp relative)))
      (string-match-p regexp (file-name-nondirectory normalized-file)))))

(defun magent-permission--check-file-rules (rules file &optional project-root)
  "Check if FILE matches any rule in RULES.
RULES is an alist of (pattern . permission).
Specific patterns are checked first; the wildcard \\='* or \"*\" is used as fallback.
Slash-free patterns match FILE's basename.  Patterns containing a slash match
the canonical full path or its project-relative form when PROJECT-ROOT is
  supplied.  Specific rules keep list-order, first-match semantics.  The bare
catch-all `*' (as a symbol or string) remains an explicit fallback regardless
of where it appears, for compatibility with existing Magent profiles."
  (when (and file (stringp file))
    (let ((wildcard-permission nil))
      (catch 'found
        (dolist (rule rules)
          (let ((pattern (car rule))
                (permission (cdr rule)))
            (cond
             ;; Save wildcard for fallback (symbol '* or string "*").
             ((or (eq pattern '*) (equal pattern "*"))
              (setq wildcard-permission permission))

             ;; All specific rules keep their declared order.
             ((stringp pattern)
              (when (magent-permission--resource-pattern-match-p
                     pattern file project-root)
                (throw 'found permission))))))
        ;; No specific match — use wildcard if present, else allow.
        (or wildcard-permission magent-permission-allow)))))

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

(defun magent-permission--target-session (&optional session)
  "Return SESSION or the current Magent session."
  (or session
      (and (fboundp 'magent-session-get)
           (ignore-errors (magent-session-get)))))

(defun magent-permission-session-override (perm-key &optional session)
  "Return session override for PERM-KEY in SESSION, or nil if none."
  (when-let* ((target (magent-permission--target-session session)))
    (magent-session-approval-override target perm-key)))

(defun magent-permission-set-session-override (perm-key value &optional session)
  "Set session override for PERM-KEY to VALUE in SESSION.
VALUE must be \\='allow or \\='deny."
  (when-let* ((target (magent-permission--target-session session)))
    (magent-session-set-approval-override target perm-key value)))

(defun magent-permission-clear-session-overrides (&optional session)
  "Clear all session-level permission overrides in SESSION."
  (when-let* ((target (magent-permission--target-session session)))
    (magent-session-clear-approval-overrides target)))

;;; Tool availability

(defun magent-permission--access-decision-p (decision)
  "Return non-nil when DECISION permits exposure or approval."
  (memq decision '(allow ask)))

(defun magent-permission--tool-access-patterns (rules tool)
  "Describe TOOL's positive resource rules in RULES.

Return `unbounded' when no resource allowlist constrains TOOL, a list of
positive glob strings for a constrained tool, or nil when no resource can be
accessed."
  (setq tool (magent-permission--normalize-tool tool))
  (let* ((effective (if (magent-permission-p rules)
                        (magent-permission-rules rules)
                      rules))
         (tool-value (and (listp effective) (cdr (assq tool effective))))
         (nested-p (and (consp tool-value)
                        (not (memq tool-value '(allow deny ask))))))
    (if (not nested-p)
        (and (magent-permission--access-decision-p
              (magent-permission-resolve rules tool))
             'unbounded)
      (let ((fallback
             (cl-find-if (lambda (rule)
                           (or (eq (car rule) '*) (equal (car rule) "*")))
                         tool-value)))
        (if (or (null fallback)
                (magent-permission--access-decision-p (cdr fallback)))
            'unbounded
          (cl-loop for (pattern . decision) in tool-value
                   when (and (stringp pattern)
                             (not (equal pattern "*"))
                             (magent-permission--access-decision-p decision))
                   collect pattern))))))

(defun magent-permission--glob-fixed-prefix (pattern)
  "Return PATTERN's literal prefix before its first glob metacharacter."
  (let* ((normalized (subst-char-in-string ?\\ ?/ pattern))
         (index (string-match "[*?]" normalized)))
    (substring normalized 0 (or index (length normalized)))))

(defun magent-permission--glob-fixed-suffix (pattern)
  "Return a conservative literal suffix required by glob PATTERN.
Return nil for `**/' patterns whose optional directory separator makes a
naive textual suffix unsound."
  (let ((normalized (subst-char-in-string ?\\ ?/ pattern)))
    (unless (string-match-p "\\*\\*/" normalized)
      (let ((last-wildcard nil)
            (index 0))
        (while (string-match "[*?]" normalized index)
          (setq last-wildcard (match-beginning 0)
                index (1+ last-wildcard)))
        (substring normalized (if last-wildcard (1+ last-wildcard) 0))))))

(defun magent-permission--incompatible-fixed-strings-p (left right direction)
  "Return non-nil when fixed LEFT and RIGHT cannot coexist in DIRECTION.
DIRECTION is `prefix' or `suffix'.  Empty strings carry no proof."
  (and (not (string-empty-p left))
       (not (string-empty-p right))
       (pcase direction
         ('prefix (not (or (string-prefix-p left right)
                           (string-prefix-p right left))))
         ('suffix (not (or (string-suffix-p left right)
                           (string-suffix-p right left)))))))

(defun magent-permission--resource-patterns-provably-disjoint-p (left right)
  "Return non-nil only when resource globs LEFT and RIGHT cannot overlap.
This proof deliberately handles only incompatible literal prefixes/suffixes;
unknown cases stay exposed and are decided authoritatively at invocation."
  (let* ((left-path-p (string-match-p "/" left))
         (right-path-p (string-match-p "/" right))
         (left-prefix (magent-permission--glob-fixed-prefix left))
         (right-prefix (magent-permission--glob-fixed-prefix right))
         (left-suffix (magent-permission--glob-fixed-suffix left))
         (right-suffix (magent-permission--glob-fixed-suffix right)))
    (or (and left-path-p right-path-p
             (magent-permission--incompatible-fixed-strings-p
              left-prefix right-prefix 'prefix))
        (and left-suffix right-suffix
             (magent-permission--incompatible-fixed-strings-p
              left-suffix right-suffix 'suffix)))))

(defun magent-permission--pattern-unions-provably-disjoint-p (left right)
  "Return non-nil when every glob in LEFT is disjoint from every glob in RIGHT."
  (and left right
       (cl-every
        (lambda (left-pattern)
          (cl-every
           (lambda (right-pattern)
             (magent-permission--resource-patterns-provably-disjoint-p
              left-pattern right-pattern))
           right))
        left)))

(defun magent-permission--intersection-tool-available-p (profiles tool)
  "Return non-nil unless PROFILES are provably disjoint for TOOL.
Execution-time permission resolution remains authoritative.  An uncertain
glob intersection is exposed rather than hiding a genuinely usable tool."
  (let* ((descriptions
          (mapcar (lambda (profile)
                    (magent-permission--tool-access-patterns profile tool))
                  profiles))
         (constrained (remove 'unbounded descriptions)))
    (and (cl-every #'identity descriptions)
         (not
          (cl-loop for tail on constrained
                   thereis
                   (cl-some
                    (lambda (other)
                      (magent-permission--pattern-unions-provably-disjoint-p
                       (car tail) other))
                    (cdr tail)))))))

(defun magent-permission-tool-available-p (rules tool)
  "Return t if TOOL should appear in the tool list for RULES.
Unlike `magent-permission-allow-p', this returns t for tools with
\\='ask permission and for tools with nested file rules where at
least one pattern grants access (e.g., plan agent where edit
defaults to \\='deny but specific paths are \\='allow)."
  (setq tool (magent-permission--normalize-tool tool))
  (or (magent-permission-bypass-p)
      (if (magent-permission-intersection-p rules)
          (let ((profiles (magent-permission-intersection-profiles rules)))
            (and (cl-every (lambda (profile)
                             (magent-permission-tool-available-p profile tool))
                           profiles)
                 (magent-permission--intersection-tool-available-p
                  profiles tool)))
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
          ;; Simple case: reuse normal permission resolution.
          (memq (magent-permission-resolve rules tool) '(allow ask)))))))

;;; Permission merging

(defun magent-permission--normalize-resource-rules (rules)
  "Return a copy of resource RULES with one canonical wildcard spelling."
  (mapcar (lambda (rule)
            (cons (if (or (eq (car rule) '*) (equal (car rule) "*"))
                      '*
                    (car rule))
                  (cdr rule)))
          rules))

(defun magent-permission--merge-resource-rules (earlier later)
  "Merge resource rules EARLIER with higher-priority LATER.
LATER keeps its declared first-match order.  A wildcard in LATER makes it a
complete resource policy; otherwise unmatched EARLIER rules remain as a
lower-priority fallback."
  (let* ((earlier (magent-permission--normalize-resource-rules earlier))
         (later (magent-permission--normalize-resource-rules later))
         (later-keys (mapcar #'car later)))
    (if (assq '* later)
        later
      (append later
              (cl-remove-if
               (lambda (rule)
                 (cl-member (car rule) later-keys :test #'equal))
               earlier)))))

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
          (let ((key (if (or (eq (car rule) '*) (equal (car rule) "*"))
                         '*
                       (car rule)))
                (value (cdr rule)))
            (if (and (consp value) (not (memq (car value) '(allow deny ask))))
                ;; Nested resource rules preserve declared order.  A later
                ;; wildcard is a complete fallback policy and supersedes the
                ;; earlier nested policy.
                (let ((existing (assq key result)))
                  (if existing
                      (setcdr existing
                              (magent-permission--merge-resource-rules
                               (let ((old (cdr existing)))
                                 (if (consp old)
                                     old
                                   (list (cons '* old))))
                               value))
                    (setq result
                          (append result
                                  (list
                                   (cons key
                                         (magent-permission--normalize-resource-rules
                                          value)))))))
              ;; Simple rule - just set/override
              (let ((existing (assq key result)))
                (if existing
                    (setcdr existing value)
                  (setq result (append result (list (cons key value)))))))))))
    result))

(defun magent-permission-from-config (config)
  "Convert CONFIG alist to permission rules.
CONFIG is an alist like ((read . allow) (bash . deny)).
Returns permission rules."
  (magent-permission-create :rules config))

(provide 'magent-permission)
;;; magent-permission.el ends here
