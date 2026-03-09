;;; magent-frontmatter.el --- Frontmatter parsing utilities for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Shared frontmatter parsing utilities for Magent.
;; Parses Jekyll/Hugo style --- delimited frontmatter (key: value pairs).
;; Used by both agent-file and skill-file modules.

;;; Code:

(defun magent-frontmatter-parse (content)
  "Parse frontmatter from CONTENT.
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
            (let ((header-str (buffer-substring-no-properties start (match-beginning 0))))
              (setq frontmatter (magent-frontmatter--parse-header header-str))
              (forward-line 1)
              (setq body (buffer-substring-no-properties (point) (point-max)))))))
      (cons frontmatter body))))

(defun magent-frontmatter--parse-header (header-str)
  "Parse frontmatter header string HEADER-STR to plist.
Supports basic key: value pairs."
  (let ((result nil)
        (lines (split-string header-str "\n")))
    (dolist (line lines)
      (when (string-match "^\\s-*\\([^:]+\\):\\s-*\\(.+\\)$" line)
        (let* ((key (replace-regexp-in-string "_" "-" (string-trim (match-string 1 line))))
               (value-str (string-trim (match-string 2 line)))
               (value (magent-frontmatter--parse-value value-str)))
          (setq result (plist-put result (intern (concat ":" key)) value)))))
    result))

(defun magent-frontmatter--parse-value (str)
  "Parse a frontmatter value string STR.
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
    (mapcar #'magent-frontmatter--parse-value
            (split-string str "," t "[\s,]+")))
   (t str)))

(provide 'magent-frontmatter)
;;; magent-frontmatter.el ends here
