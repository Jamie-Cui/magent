;;; magent-redaction.el --- Sensitive data redaction helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fail-closed helpers for sanitizing bounded Magent-owned outbound data.
;; This module is not a sandbox and does not make arbitrary tool output safe.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(define-error 'magent-redaction-unsafe-value
  "Value cannot be safely redacted")

(defconst magent-redaction-sensitive-path-regexp
  (regexp-opt '("authinfo" ".authinfo" ".netrc" ".env" "envrc" "secret"
                "secrets" "credential" "credentials" "token" "oauth"
                "password" "passwd" "private-key" "id_rsa" "id_ed25519"
                ".gpg" ".age" ".pem" ".p12" ".pfx"))
  "Regexp matching file names that should not be read by safe collectors.")

(defconst magent-redaction-sensitive-label-regexp
  (regexp-opt '("authorization" "auth" "password" "passwd" "token" "secret"
                "credential" "credentials" "oauth" "api-key" "apikey"
                "api_key" "access-key" "access_key" "private-key"
                "private_key" "client-secret" "client_secret")
              'words)
  "Regexp matching labels commonly associated with sensitive values.")

(defconst magent-redaction--known-token-regexp
  (concat
   "\\(?:"
   "\\beyJ[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+\\b"
   "\\|\\bsk-[A-Za-z0-9_-]\\{12,\\}\\b"
   "\\|\\bgithub_pat_[A-Za-z0-9_-]\\{12,\\}\\b"
   "\\|\\bgh[opurs]_[A-Za-z0-9]\\{12,\\}\\b"
   "\\|\\bxox[baprs]-[A-Za-z0-9-]\\{12,\\}\\b"
   "\\|\\bAKIA[A-Z0-9]\\{12,\\}\\b"
   "\\|\\bAIza[A-Za-z0-9_-]\\{12,\\}\\b"
   "\\)")
  "Regexp matching common unlabeled credential formats.")

(defconst magent-redaction--long-token-regexp
  "\\b[A-Za-z0-9_+/-]\\{32,\\}\\(?:=\\{0,2\\}\\)?"
  "Regexp matching candidates for conservative high-entropy redaction.")

(defun magent-redaction--category (label)
  "Return a stable redaction category for LABEL."
  (let ((name (downcase (replace-regexp-in-string "_" "-" label))))
    (cond
     ((string-match-p "auth" name) "authorization")
     ((string-match-p "pass" name) "password")
     ((string-match-p "key" name) "key")
     ((string-match-p "secret" name) "secret")
     ((string-match-p "credential" name) "credential")
     (t "token"))))

(defun magent-redaction--sensitive-key-p (key)
  "Return non-nil when KEY names a sensitive value."
  (let ((case-fold-search t)
        (name (downcase
               (replace-regexp-in-string
                "_" "-"
                (cond
                 ((keywordp key) (substring (symbol-name key) 1))
                 ((symbolp key) (symbol-name key))
                 ((stringp key) key)
                 (t ""))))))
    (or (string-match-p magent-redaction-sensitive-label-regexp name)
        (string-match-p
         (concat
          "\\(?:\\`\\|[-.]\\)"
          "\\(?:authorization\\|auth\\|password\\|passwd\\|token"
          "\\|secret\\|credential\\|credentials\\|oauth\\|api-key"
          "\\|access-key\\|private-key\\|client-secret\\)"
          "\\(?:\\'\\|[-.]\\)")
         name))))

(defun magent-redaction--line-sensitive-label (line)
  "Return the first sensitive compound identifier found in LINE."
  (let ((start 0)
        found)
    (while (and (not found)
                (string-match "[[:alnum:]_.-]+" line start))
      (let ((candidate (match-string 0 line)))
        (when (magent-redaction--sensitive-key-p candidate)
          (setq found candidate)))
      (setq start (match-end 0)))
    found))

(defun magent-redaction--stable-identifier-p (candidate)
  "Return non-nil when CANDIDATE is a known non-secret identifier shape."
  (or (string-match-p
       "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\'"
       candidate)
      (string-match-p "\\`[[:xdigit:]]\\{40\\}\\'" candidate)))

(defun magent-redaction--high-entropy-p (candidate)
  "Return non-nil when CANDIDATE conservatively resembles a secret."
  (and (>= (length candidate) 32)
       (not (magent-redaction--stable-identifier-p candidate))
       (let ((classes 0)
             (chars (make-hash-table :test #'eql)))
         (when (string-match-p "[a-z]" candidate) (cl-incf classes))
         (when (string-match-p "[A-Z]" candidate) (cl-incf classes))
         (when (string-match-p "[0-9]" candidate) (cl-incf classes))
         (when (string-match-p "[_+/=-]" candidate) (cl-incf classes))
         (mapc (lambda (char) (puthash char t chars)) candidate)
         (and (>= classes 2)
              (>= (hash-table-count chars) 8)))))

(defun magent-redaction--replace-regexp (regexp replacement string)
  "Replace every REGEXP match in STRING using REPLACEMENT."
  (let ((start 0)
        parts)
    (while (string-match regexp string start)
      (push (substring string start (match-beginning 0)) parts)
      (push replacement parts)
      (setq start (match-end 0)))
    (push (substring string start) parts)
    (apply #'concat (nreverse parts))))

(defun magent-redaction--redact-long-tokens (string)
  "Return STRING with conservative high-entropy candidates redacted."
  (let ((start 0)
        parts)
    (while (string-match magent-redaction--long-token-regexp string start)
      (let ((candidate (match-string 0 string)))
        (push (substring string start (match-beginning 0)) parts)
        (push (if (magent-redaction--high-entropy-p candidate)
                  "<redacted:token>"
                candidate)
              parts)
        (setq start (match-end 0))))
    (push (substring string start) parts)
    (apply #'concat (nreverse parts))))

(defun magent-redaction--redact-line (line strict)
  "Redact sensitive values from LINE.
When STRICT is non-nil, redact the entire line when it contains a sensitive
label that cannot be parsed as a conventional assignment or header."
  (let ((case-fold-search t))
    (cond
     ((and (string-match
            "\\([[:alnum:]_.-]+\\)[[:space:]]*[:=][[:space:]]*.*\\'"
            line)
           (magent-redaction--sensitive-key-p (match-string 1 line)))
      (concat (substring line 0 (match-beginning 1))
              (match-string 1 line)
              ": <redacted:"
              (magent-redaction--category (match-string 1 line))
              ">"))
     ((and strict
           (magent-redaction--line-sensitive-label line))
      "<redacted:sensitive-line>")
     (t
      (magent-redaction--redact-long-tokens
       (magent-redaction--replace-regexp
        magent-redaction--known-token-regexp "<redacted:token>" line))))))

(defun magent-redaction-string (string &optional strict)
  "Return STRING with sensitive-looking values redacted.
When STRICT is non-nil, ambiguous lines containing sensitive labels are fully
redacted.  Signal `magent-redaction-unsafe-value' for non-string input."
  (unless (stringp string)
    (signal 'magent-redaction-unsafe-value '("Expected string")))
  (mapconcat (lambda (line) (magent-redaction--redact-line line strict))
             (split-string string "\n")
             "\n"))

(defun magent-redaction-normalize-paths (string &optional project-root)
  "Replace sensitive absolute path prefixes in STRING.
PROJECT-ROOT is replaced before the home and temporary directories."
  (unless (stringp string)
    (signal 'magent-redaction-unsafe-value '("Expected path string")))
  (let ((result string)
        (roots `((,project-root . "$PROJECT")
                 (,(expand-file-name "~") . "$HOME")
                 (,temporary-file-directory . "$TMP"))))
    (dolist (entry roots result)
      (when-let* ((root (car entry))
                  (expanded (ignore-errors
                              (directory-file-name (expand-file-name root))))
                  ((not (string-empty-p expanded))))
        (setq result
              (replace-regexp-in-string
               (regexp-quote expanded) (cdr entry) result t t))))))

(defun magent-redaction--value (value strict seen depth)
  "Recursively redact VALUE using STRICT, SEEN, and DEPTH state."
  (when (> depth 24)
    (signal 'magent-redaction-unsafe-value '("Maximum nesting exceeded")))
  (cond
   ((stringp value) (magent-redaction-string value strict))
   ((or (numberp value) (eq value t) (null value)
        (eq value :json-false) (eq value :null))
    value)
   ((symbolp value) (symbol-name value))
   ((or (consp value) (vectorp value) (hash-table-p value))
    (when (gethash value seen)
      (signal 'magent-redaction-unsafe-value '("Circular value")))
    (puthash value t seen)
    (unwind-protect
        (cond
         ((vectorp value)
          (vconcat (mapcar (lambda (item)
                             (magent-redaction--value
                              item strict seen (1+ depth)))
                           (append value nil))))
         ((hash-table-p value)
          (let ((table (make-hash-table :test #'equal)))
            (maphash
             (lambda (key item)
               (unless (or (stringp key) (symbolp key))
                 (signal 'magent-redaction-unsafe-value
                         '("Unsupported hash key")))
               (puthash
                (if (symbolp key) (symbol-name key) key)
                (if (magent-redaction--sensitive-key-p key)
                    (format "<redacted:%s>"
                            (magent-redaction--category (format "%s" key)))
                  (magent-redaction--value item strict seen (1+ depth)))
                table))
             value)
            table))
         ((and (listp value)
               (let ((tail value) (valid t))
                 (while (and valid tail)
                   (if (and (keywordp (car tail)) (consp (cdr tail)))
                       (setq tail (cddr tail))
                     (setq valid nil)))
                 (and valid (null tail))))
          (let ((tail value) result)
            (while tail
              (let ((key (pop tail))
                    (item (pop tail)))
                (setq result
                      (append
                       result
                       (list key
                             (if (magent-redaction--sensitive-key-p key)
                                 (format "<redacted:%s>"
                                         (magent-redaction--category
                                          (symbol-name key)))
                               (magent-redaction--value
                                item strict seen (1+ depth))))))))
            result))
         ((and (listp value)
               (cl-every (lambda (entry)
                           (and (consp entry)
                                (or (stringp (car entry))
                                    (symbolp (car entry)))))
                         value))
          (mapcar
           (lambda (entry)
             (cons (car entry)
                   (if (magent-redaction--sensitive-key-p (car entry))
                       (format "<redacted:%s>"
                               (magent-redaction--category
                                (format "%s" (car entry))))
                     (magent-redaction--value
                      (cdr entry) strict seen (1+ depth)))))
           value))
         ((listp value)
          (mapcar (lambda (item)
                    (magent-redaction--value item strict seen (1+ depth)))
                  value))
         (t
          (signal 'magent-redaction-unsafe-value '("Unsupported value"))))
      (remhash value seen)))
   (t
    (signal 'magent-redaction-unsafe-value '("Unsupported value type")))))

(defun magent-redaction-value (value &optional strict)
  "Return a recursively redacted JSON-safe copy of VALUE.
When STRICT is non-nil, string values use strict line redaction."
  (magent-redaction--value value strict (make-hash-table :test #'eq) 0))

(provide 'magent-redaction)
;;; magent-redaction.el ends here
