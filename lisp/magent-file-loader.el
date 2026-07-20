;;; magent-file-loader.el --- Shared file loader and frontmatter helpers for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Shared helper functions for Magent's file-backed definitions such as
;; agents, skills, and capabilities, including frontmatter parsing.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'yaml)
(require 'magent-config)
(require 'magent-log)

;;; Frontmatter parsing

(defun magent-file-loader-parse-frontmatter (content)
  "Parse frontmatter from CONTENT.
Returns (FRONTMATTER . BODY) where FRONTMATTER is a plist.
If no frontmatter is found, returns (nil . CONTENT)."
  (if (string-match "\\`---\n\\(\\(?:.\\|\n\\)*?\\)\n---\n?" content)
      (let* ((yaml-text (match-string 1 content))
             (body (substring content (match-end 0)))
             (raw (or (magent-file-loader--parse-simple-frontmatter yaml-text)
                      (magent-file-loader--parse-yaml-frontmatter yaml-text)))
             (normalized (magent-file-loader--normalize-frontmatter-plist raw)))
        (cons normalized body))
    (cons nil content)))

(defconst magent-file-loader--comma-list-keys
  '(:tools :skills :capability-skills :modes :features :files
    :prompt-keywords :keywords)
  "Frontmatter keys that accept the legacy comma-separated list syntax.")

(defun magent-file-loader--normalize-frontmatter-plist (plist)
  "Normalize PLIST keys and values for Magent definition files."
  (let (result)
    (cl-loop for (key val) on plist by #'cddr do
      (let* ((key-str (substring (symbol-name key) 1))
             (normalized-key (intern (concat ":" (subst-char-in-string ?_ ?- key-str))))
             (normalized-val
              (magent-file-loader--normalize-frontmatter-value
               normalized-key val)))
        (setq result (plist-put result normalized-key normalized-val))))
    result))

(defun magent-file-loader--normalize-frontmatter-value (key val)
  "Normalize frontmatter VAL for KEY.
Only declared list fields accept the legacy comma-separated shorthand; commas
in descriptions and other scalar strings remain data."
  (if (and (memq key magent-file-loader--comma-list-keys)
           (stringp val)
           (string-match-p "," val))
      (mapcar #'string-trim (split-string val ","))
    val))

(defun magent-file-loader--parse-simple-frontmatter (header)
  "Parse flat scalar HEADER without losing JSON-compatible string escapes.
Return nil for nested or otherwise complex YAML so `yaml-parse-string' owns
those cases.  Saved agent files use JSON-compatible scalar quoting, which is
decoded here because yaml.el currently mishandles a doubled backslash before
letters such as `t'."
  (let (result)
    (catch 'complex
      (dolist (line (split-string header "\n"))
        (cond
         ((string-blank-p line))
         ((or (string-match-p "\\`[[:space:]]" line)
              (not (string-match
                    "\\`\\([^:]+\\):[[:space:]]*\\(.+\\)\\'" line)))
          (throw 'complex nil))
         (t
          (let ((key (string-trim (match-string 1 line)))
                (value (string-trim (match-string 2 line))))
            (when (or (string-match-p "[[:space:]]#" value)
                      (string-match-p "\\`[|>&*!]" value))
              (throw 'complex nil))
            (setq result
                  (plist-put
                   result
                   (intern (concat ":" key))
                   (magent-file-loader--parse-simple-scalar value)))))))
      result)))

(defun magent-file-loader--parse-simple-scalar (value)
  "Parse flat frontmatter VALUE, or throw `complex' for full YAML parsing."
  (cond
   ((and (> (length value) 1)
         (eq (aref value 0) ?')
         (eq (aref value (1- (length value))) ?'))
    (replace-regexp-in-string
     "''" "'" (substring value 1 -1) t t))
   ((or (memq (aref value 0) '(?\" ?\[ ?{))
        (string-match-p
         "\\`\\(?:true\\|false\\|null\\|-?[0-9]+\\(?:\\.[0-9]+\\)?\\)\\'"
         value))
    (condition-case nil
        (json-parse-string value
                           :object-type 'plist
                           :array-type 'list
                           :null-object nil
                           :false-object nil)
      (error
       (if (memq (aref value 0) '(?\[ ?{))
           (condition-case nil
               (yaml-parse-string value
                                  :object-type 'plist
                                  :object-key-type 'keyword
                                  :sequence-type 'list
                                  :false-object nil
                                  :null-object nil)
             (error (throw 'complex nil)))
         (throw 'complex nil)))))
   (t value)))

(defun magent-file-loader--parse-yaml-frontmatter (text)
  "Parse complex YAML frontmatter TEXT without corrupting backslashes."
  (condition-case err
      (pcase-let* ((`(,protected . ,marker)
                    (magent-file-loader--protect-yaml-backslashes text))
                   (parsed
                    (yaml-parse-string protected
                                       :object-type 'plist
                                       :object-key-type 'keyword
                                       :sequence-type 'list
                                       :false-object nil
                                       :null-object nil)))
        (magent-file-loader--restore-yaml-backslashes parsed marker))
    (error
     (magent-log "WARN frontmatter yaml parse failed (%s)"
                 (error-message-string err))
     nil)))

(defun magent-file-loader--protect-yaml-backslashes (text)
  "Return (PROTECTED . MARKER) for doubled slashes in quoted YAML TEXT.
The yaml.el decoder applies named escapes before it collapses a doubled
backslash, so a value such as `C:\\\\tmp' otherwise becomes a tab."
  (let ((code #xe000))
    (while (and (<= code #xf8ff)
                (string-match-p (regexp-quote (char-to-string code)) text))
      (cl-incf code))
    (when (> code #xf8ff)
      (error "Unable to reserve a YAML backslash marker"))
    (let ((marker (char-to-string code))
          (state 'plain)
          (index 0)
          pieces)
      (while (< index (length text))
        (let* ((char (aref text index))
               (next (and (< (1+ index) (length text))
                          (aref text (1+ index))))
               (previous (and (> index 0) (aref text (1- index)))))
          (pcase state
            ('comment
             (push (char-to-string char) pieces)
             (when (eq char ?\n) (setq state 'plain)))
            ('single
             (push (char-to-string char) pieces)
             (when (eq char ?')
               (if (eq next ?')
                   (progn
                     (push (char-to-string next) pieces)
                     (cl-incf index))
                 (setq state 'plain))))
            ('double
             (cond
              ((and (eq char ?\\) next)
               (if (eq next ?\\)
                   (push marker pieces)
                 (push (string char next) pieces))
               (cl-incf index))
              (t
               (push (char-to-string char) pieces)
               (when (eq char ?\") (setq state 'plain)))))
            (_
             (push (char-to-string char) pieces)
             (cond
              ((and (eq char ?#)
                    (or (null previous) (memq previous '(?\s ?\t ?\n))))
               (setq state 'comment))
              ((and (memq char '(?\" ?'))
                    (or (null previous)
                        (memq previous '(?\s ?\t ?\n ?: ?, ?- ?\[ ?{))))
               (setq state (if (eq char ?\") 'double 'single))))))
          (cl-incf index)))
      (cons (apply #'concat (nreverse pieces)) marker))))

(defun magent-file-loader--restore-yaml-backslashes (value marker)
  "Replace protected YAML MARKER recursively in parsed VALUE."
  (cond
   ((stringp value)
    (replace-regexp-in-string
     (regexp-quote marker) (lambda (_match) "\\") value t t))
   ((consp value)
    (cons (magent-file-loader--restore-yaml-backslashes (car value) marker)
          (magent-file-loader--restore-yaml-backslashes (cdr value) marker)))
   (t value)))

(defun magent-file-loader-project-subdir (relative-dir)
  "Return project-local RELATIVE-DIR as a one-item list when it exists."
  (when-let* ((dir (magent-file-loader-project-directory relative-dir)))
    (list dir)))

(defun magent-file-loader-project-directory (relative-dir &optional scope)
  "Return project-local RELATIVE-DIR under SCOPE or current project root."
  (when-let* ((root (or scope (magent-project-root))))
    (let ((dir (expand-file-name relative-dir root)))
      (when (file-directory-p dir)
        dir))))

(defun magent-file-loader-project-subdir-for-scope (relative-dir scope)
  "Return project-local RELATIVE-DIR for SCOPE as a one-item list."
  (when-let* ((dir (magent-file-loader-project-directory relative-dir scope)))
    (list dir)))

(defun magent-file-loader-file-under-directory-p (filepath directory)
  "Return non-nil when FILEPATH is inside DIRECTORY."
  (and filepath directory
       (file-exists-p directory)
       (string-prefix-p (file-name-as-directory (file-truename directory))
                        (file-truename filepath))))

(defun magent-file-loader-lexical-file-under-directory-p (filepath directory)
  "Return non-nil when FILEPATH's entry path is inside DIRECTORY.
Unlike `magent-file-loader-file-under-directory-p', this ownership check does
not follow the final symlink.  It is used to classify project overlay entries;
resource access may still canonicalize the target independently."
  (and filepath directory
       (string-prefix-p
        (file-name-as-directory
         (directory-file-name (expand-file-name directory)))
        (expand-file-name filepath))))

(defun magent-file-loader-project-root-for-file (filepath relative-dir)
  "Return the project root owning FILEPATH under RELATIVE-DIR, or nil.
RELATIVE-DIR is the project-local subdirectory path such as
\".magent/skills\"."
  (when filepath
    (let ((current (file-name-directory (expand-file-name filepath)))
          found)
      (while (and current (not found))
        (let* ((candidate (directory-file-name current))
               (subdir (expand-file-name relative-dir candidate))
               (parent (file-name-directory candidate)))
          (if (magent-file-loader-lexical-file-under-directory-p
               filepath subdir)
              (setq found (file-truename candidate))
            (setq current
                  (and parent
                       (not (string= parent current))
                       parent)))))
      found)))

(defun magent-file-loader-list-named-files (directories filename)
  "Return sorted FILENAME paths found in DIRECTORIES and their subdirs.

For each directory in DIRECTORIES, include:
- FILENAME directly inside the directory
- FILENAME inside each non-hidden immediate subdirectory"
  (let (files)
    (dolist (dir directories)
      (when (file-directory-p dir)
        (let ((direct-file (expand-file-name filename dir)))
          (when (file-exists-p direct-file)
            (push direct-file files)))
        (dolist (subdir (directory-files dir t "^[^.]"))
          (when (file-directory-p subdir)
            (let ((nested-file (expand-file-name filename subdir)))
              (when (file-exists-p nested-file)
                (push nested-file files)))))))
    (sort files #'string<)))

(defun magent-file-loader-list-named-files-ordered (directories filename)
  "Return FILENAME paths while preserving precedence order of DIRECTORIES.
Paths are sorted within each directory so loading remains deterministic."
  (apply #'append
         (mapcar
          (lambda (directory)
            (magent-file-loader-list-named-files
             (list directory) filename))
          (delq nil (copy-sequence directories)))))

(defun magent-file-loader-list-matching-files (directory regexp)
  "Return sorted files in DIRECTORY matching REGEXP."
  (sort (when (file-directory-p directory)
          (directory-files directory t regexp))
        #'string<))

(cl-defun magent-file-loader-list-definition-files
    (filename &key builtin-dirs user-dirs project-relative-dir scope extra-dirs)
  "Return FILENAME paths across configured directories in precedence order."
  (magent-file-loader-list-named-files-ordered
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
           ((cl-some (lambda (directory)
                       (magent-file-loader-lexical-file-under-directory-p
                        filepath directory))
                     (delq nil builtin-dirs))
            'builtin)
           ((cl-some (lambda (directory)
                       (magent-file-loader-lexical-file-under-directory-p
                        filepath directory))
                     (delq nil user-dirs))
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
