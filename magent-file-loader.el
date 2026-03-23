;;; magent-file-loader.el --- Shared file loader and frontmatter helpers for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Shared helper functions for Magent's file-backed definitions such as
;; agents, skills, and capabilities, including frontmatter parsing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'yaml)
(require 'magent-config)

(declare-function magent-log "magent-ui")

;;; Frontmatter parsing

(defun magent-file-loader-parse-frontmatter (content)
  "Parse frontmatter from CONTENT.
Returns (FRONTMATTER . BODY) where FRONTMATTER is a plist.
If no frontmatter is found, returns (nil . CONTENT)."
  (if (string-match "\\`---\n\\(\\(?:.\\|\n\\)*?\\)\n---\n?" content)
      (let* ((yaml-text (match-string 1 content))
             (body (substring content (match-end 0)))
             (raw (or (magent-file-loader--parse-frontmatter-regex yaml-text)
                      (condition-case err
                          (yaml-parse-string yaml-text
                                             :object-type 'plist
                                             :object-key-type 'keyword
                                             :sequence-type 'list
                                             :false-object nil
                                             :null-object nil)
                        (error
                         (magent-log "WARN frontmatter yaml parse failed (%s)"
                                     (error-message-string err))
                         nil))))
             (normalized (magent-file-loader--normalize-frontmatter-plist raw)))
        (cons normalized body))
    (cons nil content)))

(defun magent-file-loader--normalize-frontmatter-plist (plist)
  "Normalize PLIST keys and values for Magent definition files."
  (let (result)
    (cl-loop for (key val) on plist by #'cddr do
      (let* ((key-str (substring (symbol-name key) 1))
             (normalized-key (intern (concat ":" (subst-char-in-string ?_ ?- key-str))))
             (normalized-val (magent-file-loader--normalize-frontmatter-value val)))
        (setq result (plist-put result normalized-key normalized-val))))
    result))

(defun magent-file-loader--normalize-frontmatter-value (val)
  "Split VAL into a list if it is a comma-separated string."
  (if (and (stringp val) (string-match-p "," val))
      (mapcar #'string-trim (split-string val ","))
    val))

(defun magent-file-loader--parse-frontmatter-regex (header-str)
  "Parse simple key-value frontmatter from HEADER-STR.
Returns nil if any non-blank line is not a simple scalar key-value pair,
allowing the caller to fall back to a full YAML parser."
  (let (result)
    (catch 'complex
      (dolist (line (split-string header-str "\n"))
        (cond
         ((string-blank-p line))
         ((string-match "^\\s-*\\([^:]+\\):\\s-*\\(.+\\)$" line)
          (let* ((key (string-trim (match-string 1 line)))
                 (val (string-trim (match-string 2 line))))
            (setq result
                  (plist-put result
                             (intern (concat ":" key))
                             (magent-file-loader--parse-frontmatter-scalar val)))))
         (t (throw 'complex nil))))
      result)))

(defun magent-file-loader--parse-frontmatter-scalar (str)
  "Parse scalar frontmatter STR into a boolean, number, or string."
  (cond
   ((string-equal str "true") t)
   ((string-equal str "false") nil)
   ((string-match-p "^[0-9]+\\(?:\\.[0-9]+\\)?$" str) (string-to-number str))
   ((and (> (length str) 1)
         (or (and (eq (aref str 0) ?\") (eq (aref str (1- (length str))) ?\"))
             (and (eq (aref str 0) ?') (eq (aref str (1- (length str))) ?'))))
    (substring str 1 -1))
   (t str)))

(defun magent-file-loader-project-subdir (relative-dir)
  "Return project-local RELATIVE-DIR as a one-item list when it exists."
  (when-let ((root (magent-project-root)))
    (let ((dir (expand-file-name relative-dir root)))
      (when (file-directory-p dir)
        (list dir)))))

(defun magent-file-loader-list-named-files (directories filename)
  "Return sorted FILENAME paths found in DIRECTORIES and their subdirs.

For each directory in DIRECTORIES, include:
- a direct `FILENAME' inside the directory
- `FILENAME' inside each non-hidden immediate subdirectory"
  (let (files)
    (dolist (dir directories)
      (when (file-directory-p dir)
        (push (expand-file-name filename dir) files)
        (dolist (subdir (directory-files dir t "^[^.]"))
          (when (file-directory-p subdir)
            (push (expand-file-name filename subdir) files)))))
    (sort files #'string<)))

(defun magent-file-loader-list-matching-files (directory regexp)
  "Return sorted files in DIRECTORY matching REGEXP."
  (sort (when (file-directory-p directory)
          (directory-files directory t regexp))
        #'string<))

(defun magent-file-loader-read-definition (filepath)
  "Read FILEPATH and return a plist with parsed frontmatter and body."
  (with-temp-buffer
    (insert-file-contents filepath)
    (let ((parsed (magent-file-loader-parse-frontmatter (buffer-string))))
      (list :frontmatter (car parsed)
            :body (cdr parsed)))))

(defun magent-file-loader-load-all (files load-function)
  "Load FILES with LOAD-FUNCTION and return the success count."
  (let ((count 0))
    (dolist (file files)
      (when (funcall load-function file)
        (cl-incf count)))
    count))

(defun magent-file-loader-remove-file-backed-entries (registry file-path-function)
  "Return REGISTRY without entries whose value has a file path.
FILE-PATH-FUNCTION is called with each registry value."
  (cl-remove-if (lambda (entry)
                  (funcall file-path-function (cdr entry)))
                registry))

(provide 'magent-file-loader)
;;; magent-file-loader.el ends here
