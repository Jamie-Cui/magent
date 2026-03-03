;;; magent-yaml.el --- YAML parsing utilities for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Shared YAML parsing utilities for Magent.
;; Used by both agent-file and skill-file modules.

;;; Code:

(defun magent-yaml-parse-frontmatter (content)
  "Parse YAML frontmatter from CONTENT.
Returns (FRONTMATTER . BODY) where FRONTMATTER is a plist.
If no frontmatter found, returns (nil . CONTENT)."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((frontmatter nil)
          (body content))
      (when (looking-at-p "^---")
        (forward-line 1)
        (let ((start (point)))
          (when (re-search-forward "^---" nil t)
            (let ((yaml-str (buffer-substring-no-properties start (match-beginning 0))))
              (setq frontmatter (magent-yaml-parse yaml-str))
              (forward-line 1)
              (setq body (buffer-substring-no-properties (point) (point-max)))))))
      (cons frontmatter body))))

(defun magent-yaml-parse (yaml-str)
  "Parse simple YAML string YAML-STR to plist.
Supports basic key: value pairs and nested structures."
  (let ((result nil)
        (lines (split-string yaml-str "\n")))
    (dolist (line lines)
      (when (string-match "^\\s-*\\([^:]+\\):\\s-*\\(.+\\)$" line)
        (let* ((key (string-trim (match-string 1 line)))
               (value-str (string-trim (match-string 2 line)))
               (value (magent-yaml-parse-value value-str)))
          (setq result (plist-put result (intern (concat ":" key)) value)))))
    result))

(defun magent-yaml-parse-value (str)
  "Parse a YAML value string STR.
Handles booleans, numbers, strings, and comma-separated lists."
  (setq str (string-trim str))
  (cond
   ((string-equal str "true") t)
   ((string-equal str "false") nil)
   ((string-match-p "^[0-9]+\\(?:\\.[0-9]+\\)?$" str) (string-to-number str))
   ((and (> (length str) 1)
         (or (and (eq (aref str 0) ?\") (eq (aref str (1- (length str))) ?\"))
             (and (eq (aref str 0) ?') (eq (aref str (1- (length str))) ?'))))
    (substring str 1 -1))
   ((string-match-p "," str)
    (mapcar #'magent-yaml-parse-value
            (split-string str "," t "[\s,]+")))
   (t str)))

(provide 'magent-yaml)
;;; magent-yaml.el ends here