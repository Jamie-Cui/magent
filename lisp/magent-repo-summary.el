;;; magent-repo-summary.el --- Single-file Org repository summaries  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Deterministic storage for the `/summarize' skill.  The model supplies an
;; Org fragment; this module owns repository identity, org-roam metadata,
;; canonical filenames, subtree upserts, and atomic writes.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'subr-x)
(require 'magent-config)

(declare-function magent-log "magent-ui")
(declare-function org-roam-db-update-file "org-roam-db")
(defvar org-roam-directory)

(defun magent-repo-summary--directory ()
  "Return the configured org-roam directory or signal a user error."
  (let ((directory
         (or magent-org-roam-directory
             (and (boundp 'org-roam-directory) org-roam-directory))))
    (unless (and (stringp directory) (file-directory-p directory))
      (user-error
       "No org-roam directory is available; customize magent-org-roam-directory"))
    (file-truename directory)))

(defun magent-repo-summary--git-output (directory &rest arguments)
  "Run Git in DIRECTORY with ARGUMENTS and return trimmed output."
  (with-temp-buffer
    (let ((status (apply #'process-file
                         "git" nil (current-buffer) nil
                         "-C" directory arguments)))
      (unless (and (integerp status) (zerop status))
        (error "%s" (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun magent-repo-summary--repository (project-root)
  "Return canonical Git metadata for PROJECT-ROOT."
  (unless (and (stringp project-root) (file-directory-p project-root))
    (user-error "Repository summaries require a project workspace"))
  (condition-case nil
      (let* ((root-output
              (magent-repo-summary--git-output
               (file-truename project-root) "rev-parse" "--show-toplevel"))
             (root (file-truename root-output))
             (commit (magent-repo-summary--git-output
                      root "rev-parse" "--verify" "HEAD")))
        (list :root root
              :name (file-name-nondirectory (directory-file-name root))
              :commit commit))
    (error
     (user-error "Repository summaries require a Git repository with a commit"))))

(defun magent-repo-summary--read-file (path)
  "Return PATH contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun magent-repo-summary--metadata-value (content property)
  "Return PROPERTY from CONTENT's document-level property drawer."
  (let* ((heading (string-match "^\\* " content))
         (preamble (if heading (substring content 0 heading) content))
         (case-fold-search t))
    (when (string-match
           (format "^:%s:[ \\t]+\\(.+\\)$" (regexp-quote property))
           preamble)
      (string-trim (match-string 1 preamble)))))

(defun magent-repo-summary--note-path (directory repository-name root)
  "Return canonical note path in DIRECTORY for REPOSITORY-NAME at ROOT."
  (let* ((canonical
          (expand-file-name (format "%s-summary.org" repository-name)
                            directory))
         (suffix (substring (secure-hash 'sha256 root) 0 12))
         (collision
          (expand-file-name
           (format "%s-%s-summary.org" repository-name suffix)
           directory)))
    (cond
     ((and (file-exists-p canonical)
           (equal (magent-repo-summary--metadata-value
                   (magent-repo-summary--read-file canonical) "REPO_PATH")
                  root))
      canonical)
     ((and (file-exists-p collision)
           (equal (magent-repo-summary--metadata-value
                   (magent-repo-summary--read-file collision) "REPO_PATH")
                  root))
      collision)
     ((not (file-exists-p canonical)) canonical)
     ((not (file-exists-p collision)) collision)
     (t
      (user-error "Org-roam summary filename collision for %s"
                  repository-name)))))

(defun magent-repo-summary--document-parts (content)
  "Return CONTENT as (PREAMBLE . TOP-LEVEL-CONTENT)."
  (if-let* ((heading (string-match "^\\* " content)))
      (cons (substring content 0 heading) (substring content heading))
    (cons content "")))

(defun magent-repo-summary--one-line (value)
  "Return VALUE trimmed and collapsed onto one line."
  (string-trim
   (replace-regexp-in-string "[\n\r\t ]+" " " (format "%s" value))))

(defun magent-repo-summary--validate-fragment (content parent-level)
  "Return normalized Org CONTENT valid below PARENT-LEVEL."
  (unless (stringp content)
    (user-error "Summary content must be a string"))
  (let ((fragment
         (string-trim
          (replace-regexp-in-string "\r\n?" "\n" content))))
    (when (string-blank-p fragment)
      (user-error "Summary content is empty"))
    (when (string-match-p "^#\\+title:" (downcase fragment))
      (user-error "Summary content must not contain a #+title directive"))
    (when (string-match-p "^```" fragment)
      (user-error "Summary content must use Org syntax, not Markdown fences"))
    (with-temp-buffer
      (insert fragment)
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\)[ \\t]+" nil t)
        (when (<= (length (match-string 1)) parent-level)
          (user-error
           "Summary content contains a heading outside its destination subtree")))
      (org-mode)
      (org-element-parse-buffer))
    fragment))

(defun magent-repo-summary--heading-region
    (title level &optional property value)
  "Return region of heading TITLE at LEVEL, optionally matching PROPERTY VALUE."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (re-search-forward org-heading-regexp nil t)
        (goto-char (match-beginning 0))
        (let ((matches
               (and (= (org-outline-level) level)
                    (or (null title)
                        (equal (org-get-heading t t t t) title))
                    (or (null property)
                        (equal (org-entry-get nil property) value)))))
          (if matches
              (let ((begin (point))
                    (end (save-excursion
                           (org-end-of-subtree t t)
                           (point))))
                (throw 'found (cons begin end)))
            (forward-line 1)))))))

(defun magent-repo-summary--append-block (block)
  "Append Org BLOCK to the current buffer with a blank separator."
  (goto-char (point-max))
  (unless (= (point-min) (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n"))
  (insert block))

(defun magent-repo-summary--replace-region (region block)
  "Replace REGION with Org BLOCK in the current buffer."
  (goto-char (car region))
  (delete-region (car region) (cdr region))
  (insert block))

(defun magent-repo-summary--upsert-full (tail content)
  "Upsert full summary CONTENT in document TAIL."
  (with-temp-buffer
    (insert tail)
    (org-mode)
    (let* ((fragment (magent-repo-summary--validate-fragment content 1))
           (block (format "* Repository Summary\n%s\n\n" fragment))
           (region (magent-repo-summary--heading-region
                    "Repository Summary" 1)))
      (cond
       (region (magent-repo-summary--replace-region region block))
       ((magent-repo-summary--heading-region "Scoped Summaries" 1)
        (goto-char (car (magent-repo-summary--heading-region
                        "Scoped Summaries" 1)))
        (insert block))
       (t (magent-repo-summary--append-block block))))
    (buffer-string)))

(defun magent-repo-summary--scope-files (files root)
  "Normalize FILES into repository-relative paths under ROOT."
  (let* ((items
          (cond
           ((null files) nil)
           ((vectorp files) (append files nil))
           ((listp files) files)
           ((stringp files) (split-string files "[,\n]" t "[ \\t]+"))
           (t (user-error "Scope files must be a string or list"))))
         (root-prefix (file-name-as-directory root))
         normalized)
    (dolist (item items)
      (let* ((text (magent-repo-summary--one-line item))
             (absolute (expand-file-name text root)))
        (unless (or (equal absolute root)
                    (string-prefix-p root-prefix absolute))
          (user-error "Scope file is outside the repository: %s" text))
        (unless (string-blank-p text)
          (push (directory-file-name (file-relative-name absolute root))
                normalized))))
    (sort (delete-dups normalized) #'string<)))

(defun magent-repo-summary--scope-key (scope root)
  "Return stable identity text for SCOPE in ROOT."
  (let* ((text (magent-repo-summary--one-line scope))
         (candidate (expand-file-name text root)))
    (if (file-exists-p candidate)
        (directory-file-name (file-relative-name (file-truename candidate) root))
      (downcase text))))

(defun magent-repo-summary--upsert-scoped
    (tail content scope files root)
  "Upsert scoped summary CONTENT in TAIL for SCOPE and FILES under ROOT."
  (let* ((query (magent-repo-summary--one-line scope))
         (_ (when (string-blank-p query)
              (user-error "Scoped summaries require a scope query")))
         (scope-files (magent-repo-summary--scope-files files root))
         (scope-id
          (substring
           (secure-hash 'sha256 (magent-repo-summary--scope-key query root))
           0 16))
         (fragment (magent-repo-summary--validate-fragment content 2))
         (title (if (> (length query) 120) (substring query 0 120) query))
         (block
          (format
           (concat "** %s\n:PROPERTIES:\n:SUMMARY_SCOPE_ID: %s\n"
                   ":SUMMARY_SCOPE_QUERY: %s\n:SUMMARY_SCOPE_FILES: %s\n"
                   ":END:\n%s\n\n")
           title scope-id query (string-join scope-files ", ") fragment)))
    (with-temp-buffer
      (insert tail)
      (org-mode)
      (unless (magent-repo-summary--heading-region "Scoped Summaries" 1)
        (magent-repo-summary--append-block "* Scoped Summaries\n"))
      (if-let* ((region
                 (magent-repo-summary--heading-region
                  nil 2 "SUMMARY_SCOPE_ID" scope-id)))
          (magent-repo-summary--replace-region region block)
        (let ((parent (magent-repo-summary--heading-region
                       "Scoped Summaries" 1)))
          (goto-char (cdr parent))
          (unless (bolp) (insert "\n"))
          (insert "\n" block)))
      (list :tail (buffer-string) :scope-id scope-id))))

(defun magent-repo-summary--preamble (name id root commit)
  "Return the canonical Org preamble for NAME, ID, ROOT, and COMMIT."
  (format
   (concat "#+title: %s\n#+date: %s\n#+filetags: :project:repository:\n\n"
           ":PROPERTIES:\n:ID: %s\n:REPO_PATH: %s\n"
           ":LAST_ANALYZED_COMMIT: %s\n:END:\n\n")
   name (format-time-string "[%Y-%m-%d %a %H:%M]") id root commit))

(defun magent-repo-summary--validate-document (content name)
  "Validate final Org CONTENT for repository NAME."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (unless (looking-at (concat "#\\+title: " (regexp-quote name) "$"))
      (error "Repository summary title is invalid"))
    (unless (= (how-many "^#\\+title:" (point-min) (point-max)) 1)
      (error "Repository summary must contain exactly one title"))
    (org-mode)
    (org-element-parse-buffer)))

(defun magent-repo-summary--write-atomic (path content)
  "Atomically replace PATH with CONTENT."
  (let* ((directory (file-name-directory path))
         (temporary (make-temp-file
                     (expand-file-name ".magent-summary-" directory)
                     nil ".org.tmp")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (let ((coding-system-for-write 'utf-8-unix))
              (insert content)
              (write-region (point-min) (point-max)
                            temporary nil 'silent)))
          (rename-file temporary path t)
          (setq temporary nil))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun magent-repo-summary-write
    (project-root mode content &optional scope scope-files)
  "Write one repository summary for PROJECT-ROOT.
MODE is `full' or `scoped'.  CONTENT is an Org fragment.  Scoped mode also
uses SCOPE and SCOPE-FILES.  Return a plist describing the written note."
  (let* ((repository (magent-repo-summary--repository project-root))
         (root (plist-get repository :root))
         (name (plist-get repository :name))
         (commit (plist-get repository :commit))
         (directory (magent-repo-summary--directory))
         (path (magent-repo-summary--note-path directory name root))
         (created (not (file-exists-p path)))
         (old-content (unless created (magent-repo-summary--read-file path)))
         (parts (magent-repo-summary--document-parts (or old-content "")))
         (id (or (magent-repo-summary--metadata-value
                  (car parts) "ID")
                 (org-id-new)))
         (mode-name (if (symbolp mode) (symbol-name mode) mode))
         tail scope-id)
    (pcase mode-name
      ("full"
       (setq tail (magent-repo-summary--upsert-full (cdr parts) content)))
      ("scoped"
       (let ((result (magent-repo-summary--upsert-scoped
                      (cdr parts) content scope scope-files root)))
         (setq tail (plist-get result :tail)
               scope-id (plist-get result :scope-id))))
      (_ (user-error "Summary mode must be full or scoped")))
    (let ((document (concat
                     (magent-repo-summary--preamble name id root commit)
                     tail)))
      (magent-repo-summary--validate-document document name)
      (magent-repo-summary--write-atomic path document))
    (when (fboundp 'org-roam-db-update-file)
      (condition-case err
          (org-roam-db-update-file path)
        (error
         (when (fboundp 'magent-log)
           (magent-log "WARN org-roam index update failed for %s: %s"
                       path (error-message-string err))))))
    (list :path path :created created :commit commit :scope-id scope-id)))

(provide 'magent-repo-summary)
;;; magent-repo-summary.el ends here
