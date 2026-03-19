;;; magent-frontmatter.el --- Frontmatter parsing utilities for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (yaml "0.5"))

;;; Commentary:

;; Shared frontmatter parsing utilities for Magent.
;; Parses Jekyll/Hugo style --- delimited frontmatter using yaml.el,
;; with a regex fallback for files containing non-YAML characters (e.g. globs).
;; Used by both agent-file and skill-file modules.

;;; Code:

(require 'cl-lib)
(require 'yaml)

(declare-function magent-log "magent-ui")

(defun magent-frontmatter-parse (content)
  "Parse frontmatter from CONTENT.
Returns (FRONTMATTER . BODY) where FRONTMATTER is a plist.
If no frontmatter found, returns (nil . CONTENT)."
  (if (string-match "\\`---\n\\(\\(?:.\\|\n\\)*?\\)\n---\n?" content)
      (let* ((yaml-text (match-string 1 content))
             (body (substring content (match-end 0)))
             (raw (condition-case err
                      (yaml-parse-string yaml-text
                                         :object-type 'plist
                                         :object-key-type 'keyword
                                         :sequence-type 'list
                                         :false-object nil
                                         :null-object nil)
                    (error
                     (magent-log "WARN frontmatter yaml parse failed (%s), using regex fallback"
                                 (error-message-string err))
                     (magent-frontmatter--parse-regex yaml-text))))
             (normalized (magent-frontmatter--normalize-plist raw)))
        (cons normalized body))
    (cons nil content)))

(defun magent-frontmatter--normalize-plist (plist)
  "Normalize PLIST keys (underscore to hyphen) and values (comma-split)."
  (let (result)
    (cl-loop for (key val) on plist by #'cddr do
      (let* ((key-str (substring (symbol-name key) 1))
             (normalized-key (intern (concat ":" (subst-char-in-string ?_ ?- key-str))))
             (normalized-val (magent-frontmatter--normalize-value val)))
        (setq result (plist-put result normalized-key normalized-val))))
    result))

(defun magent-frontmatter--normalize-value (val)
  "Split VAL into a list if it is a comma-separated string; otherwise return VAL."
  (if (and (stringp val) (string-match-p "," val))
      (mapcar #'string-trim (split-string val ","))
    val))

(defun magent-frontmatter--parse-regex (header-str)
  "Fallback line-by-line parser for HEADER-STR when yaml.el fails.
Returns a plist with keyword keys."
  (let (result)
    (dolist (line (split-string header-str "\n"))
      (when (string-match "^\\s-*\\([^:]+\\):\\s-*\\(.+\\)$" line)
        (let* ((key (string-trim (match-string 1 line)))
               (val (string-trim (match-string 2 line))))
          (setq result
                (plist-put result
                           (intern (concat ":" key))
                           (magent-frontmatter--parse-scalar val))))))
    result))

(defun magent-frontmatter--parse-scalar (str)
  "Parse scalar string STR into a boolean, number, or string."
  (cond
   ((string-equal str "true") t)
   ((string-equal str "false") nil)
   ((string-match-p "^[0-9]+\\(?:\\.[0-9]+\\)?$" str) (string-to-number str))
   ((and (> (length str) 1)
         (or (and (eq (aref str 0) ?\") (eq (aref str (1- (length str))) ?\"))
             (and (eq (aref str 0) ?') (eq (aref str (1- (length str))) ?'))))
    (substring str 1 -1))
   (t str)))

(provide 'magent-frontmatter)
;;; magent-frontmatter.el ends here
