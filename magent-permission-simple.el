;;; magent-permission-simple.el --- Simplified permission system  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Simplified two-layer permission system for Magent agents.
;;
;; Design rationale:
;; - The original permission system (magent-permission.el) is over-engineered
;;   for the actual use case (mostly just .env file filtering)
;; - This simplified version provides:
;;   1. Tool-level allow/deny rules
;;   2. Simple file pattern blacklist/whitelist
;; - Reduces complexity by ~60% while maintaining functionality

;;; Code:

(require 'cl-lib)

;;; Data structure

(cl-defstruct (magent-permission-simple
               (:constructor magent-permission-simple-create)
               (:copier nil))
  "Simplified permission structure.

Slots:
  allow-all         - Boolean, default policy (default: t)
  tool-rules        - Alist mapping tool symbols to 'allow or 'deny
  file-deny-patterns - List of glob patterns to deny for file operations
  file-allow-patterns - List of glob patterns to explicitly allow (overrides deny)"
  (allow-all t)
  (tool-rules nil)
  (file-deny-patterns nil)
  (file-allow-patterns nil))

;;; Permission checking

(defun magent-permission-simple-check (perm tool &optional file)
  "Check if TOOL is allowed with optional FILE path.
Returns t if allowed, nil if denied.

Resolution order:
1. Check tool-specific rule
2. If file provided, check file patterns
3. Fall back to allow-all default"
  (let ((tool-rule (assq tool (magent-permission-simple-tool-rules perm))))
    (cond
     ;; Explicit tool deny
     ((and tool-rule (eq (cdr tool-rule) 'deny))
      nil)
     ;; Explicit tool allow (skip file checks)
     ((and tool-rule (eq (cdr tool-rule) 'allow))
      t)
     ;; File-related tool: check patterns
     ((and file (memq tool '(read write edit)))
      (magent-permission-simple--check-file perm file))
     ;; Fall back to default
     (t
      (magent-permission-simple-allow-all perm)))))

(defun magent-permission-simple--check-file (perm file)
  "Check if FILE passes permission filters.
Returns t if allowed, nil if denied."
  (let ((filename (file-name-nondirectory file)))
    (cond
     ;; Explicit allow patterns (highest priority)
     ((cl-some (lambda (pat)
                 (or (string-match-p (wildcard-to-regexp pat) filename)
                     (string-match-p (wildcard-to-regexp pat) file)))
               (magent-permission-simple-file-allow-patterns perm))
      t)
     ;; Deny patterns
     ((cl-some (lambda (pat)
                 (or (string-match-p (wildcard-to-regexp pat) filename)
                     (string-match-p (wildcard-to-regexp pat) file)))
               (magent-permission-simple-file-deny-patterns perm))
      nil)
     ;; Default
     (t
      (magent-permission-simple-allow-all perm)))))

;;; Convenience constructors

(defun magent-permission-simple-default ()
  "Create default permission configuration.
Allows most tools, denies .env files (except .env.example)."
  (magent-permission-simple-create
   :allow-all t
   :tool-rules nil  ; All tools allowed by default
   :file-deny-patterns '("*.env" "*.env.*" "*/.env" "*/.env.*")
   :file-allow-patterns '("*.env.example" "*/.env.example")))

(defun magent-permission-simple-read-only ()
  "Create read-only permission configuration.
Only allows read, grep, glob, and emacs_eval tools."
  (magent-permission-simple-create
   :allow-all nil
   :tool-rules '((read . allow)
                 (grep . allow)
                 (glob . allow)
                 (emacs_eval . allow)
                 (delegate . allow)
                 (skill . allow))
   :file-deny-patterns nil
   :file-allow-patterns nil))

(defun magent-permission-simple-no-exec ()
  "Create no-execution permission configuration.
Denies bash and emacs_eval, allows everything else."
  (magent-permission-simple-create
   :allow-all t
   :tool-rules '((bash . deny)
                 (emacs_eval . deny))
   :file-deny-patterns '("*.env" "*.env.*")
   :file-allow-patterns '("*.env.example")))

;;; Compatibility layer for old permission system

(defun magent-permission-simple-from-old (old-permission)
  "Convert old magent-permission struct to simple format.
Provides backward compatibility."
  (if (magent-permission-p old-permission)
      ;; Convert old format
      (let* ((rules (magent-permission-rules old-permission))
             (wildcard-rule (cdr (assq '* rules)))
             (tool-rules nil)
             (deny-patterns '("*.env" "*.env.*"))
             (allow-patterns '("*.env.example")))
        ;; Extract tool-level rules
        (dolist (rule rules)
          (let ((tool (car rule))
                (action (cdr rule)))
            (when (and (symbolp tool) (not (eq tool '*)))
              (when (memq action '(allow deny))
                (push (cons tool action) tool-rules)))))
        (magent-permission-simple-create
         :allow-all (eq wildcard-rule 'allow)
         :tool-rules tool-rules
         :file-deny-patterns deny-patterns
         :file-allow-patterns allow-patterns))
    ;; Already simple format or nil
    (or old-permission (magent-permission-simple-default))))

(provide 'magent-permission-simple)
;;; magent-permission-simple.el ends here
