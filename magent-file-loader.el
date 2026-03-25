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
  (when-let ((dir (magent-file-loader-project-directory relative-dir)))
    (list dir)))

(defun magent-file-loader-project-directory (relative-dir &optional scope)
  "Return project-local RELATIVE-DIR under SCOPE or current project root."
  (when-let ((root (or scope (magent-project-root))))
    (let ((dir (expand-file-name relative-dir root)))
      (when (file-directory-p dir)
        dir))))

(defun magent-file-loader-project-subdir-for-scope (relative-dir scope)
  "Return project-local RELATIVE-DIR for SCOPE as a one-item list."
  (when-let ((dir (magent-file-loader-project-directory relative-dir scope)))
    (list dir)))

(defun magent-file-loader-file-under-directory-p (filepath directory)
  "Return non-nil when FILEPATH is inside DIRECTORY."
  (and filepath directory
       (file-exists-p directory)
       (string-prefix-p (file-name-as-directory (file-truename directory))
                        (file-truename filepath))))

(defun magent-file-loader-file-under-any-directory-p (filepath directories)
  "Return non-nil when FILEPATH is inside one of DIRECTORIES."
  (cl-some (lambda (directory)
             (magent-file-loader-file-under-directory-p filepath directory))
           (delq nil directories)))

(defun magent-file-loader-project-root-for-file (filepath relative-dir)
  "Return the project root owning FILEPATH under RELATIVE-DIR, or nil.
RELATIVE-DIR is the project-local subdirectory path such as
\".magent/skills\"."
  (when filepath
    (let ((current (file-name-directory (file-truename filepath)))
          found)
      (while (and current (not found))
        (let* ((candidate (directory-file-name current))
               (subdir (expand-file-name relative-dir candidate))
               (parent (file-name-directory candidate)))
          (if (magent-file-loader-file-under-directory-p filepath subdir)
              (setq found (file-truename candidate))
            (setq current
                  (and parent
                       (not (string= parent current))
                       parent)))))
      found)))

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

(cl-defun magent-file-loader-list-definition-files
    (filename &key builtin-dirs user-dirs project-relative-dir scope extra-dirs)
  "Return sorted FILENAME paths across configured definition directories."
  (magent-file-loader-list-named-files
   (append builtin-dirs
           user-dirs
           (when project-relative-dir
             (magent-file-loader-project-subdir-for-scope
              project-relative-dir scope))
           extra-dirs)
   filename))

(cl-defun magent-file-loader-classify-source
    (filepath &key builtin-dirs user-dirs project-relative-dir default-layer)
  "Return a plist describing the source layer and scope for FILEPATH."
  (let* ((source-scope (and project-relative-dir
                            (magent-file-loader-project-root-for-file
                             filepath project-relative-dir)))
         (source-layer
          (cond
           ((magent-file-loader-file-under-any-directory-p filepath builtin-dirs)
            'builtin)
           ((magent-file-loader-file-under-any-directory-p filepath user-dirs)
            'user)
           (source-scope 'project)
           (t (or default-layer 'builtin)))))
    (list :layer source-layer
          :scope source-scope)))

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

(defun magent-file-loader-remove-project-scope-entries
    (registry source-layer-function source-scope-function scope)
  "Return REGISTRY without project-layer entries from SCOPE."
  (cl-remove-if
   (lambda (entry)
     (let ((value (cdr entry)))
       (and (eq (funcall source-layer-function value) 'project)
            (equal (funcall source-scope-function value) scope))))
   registry))

(defun magent-file-loader-hash-remove-project-scope-entries
    (table source-layer-function source-scope-function scope)
  "Delete project-layer values from TABLE for SCOPE and return TABLE."
  (maphash
   (lambda (key value)
     (when (and (eq (funcall source-layer-function value) 'project)
                (equal (funcall source-scope-function value) scope))
       (remhash key table)))
   table)
  table)

(defun magent-file-loader-reload-file-backed-registry
    (registry-symbol file-path-function load-function &rest args)
  "Remove file-backed entries from REGISTRY-SYMBOL and call LOAD-FUNCTION."
  (set registry-symbol
       (magent-file-loader-remove-file-backed-entries
        (symbol-value registry-symbol)
        file-path-function))
  (apply load-function args))

(provide 'magent-file-loader)
;;; magent-file-loader.el ends here
