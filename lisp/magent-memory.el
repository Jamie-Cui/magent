;;; magent-memory.el --- Emacs profile memory for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Local, user-owned Emacs profile memory.  This module scans a bounded,
;; confirmed set of Emacs configuration files, writes a human-editable Org
;; profile, and exposes a small selected subset to Magent prompts when the
;; current request is relevant.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'gptel)
(require 'magent-config)
(require 'magent-command)
(require 'magent-llm)
(require 'magent-llm-gptel)
(require 'magent-prompt)
(require 'magent-redaction)

(declare-function gptel-backend-name "gptel")

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-temperature)

(defconst magent-memory--managed-heading "Magent Managed Profile"
  "Top-level Org heading owned by Magent.")

(defconst magent-memory--user-notes-heading "User Notes"
  "Top-level Org heading preserved for user-authored memory.")

(defconst magent-memory--managed-section-headings
  '("Overview"
    "Configuration Roots"
    "Entry Files"
    "Package Management"
    "Editing And UI Workflow"
    "Completion And Navigation"
    "Org And Notes Workflow"
    "Project And Git Workflow"
    "Programming And LSP Workflow"
    "AI And Magent Workflow"
    "Verification Commands"
    "Cautions And Skipped Sensitive Areas"
    "Source Index")
  "Fixed second-level Org headings under `magent-memory--managed-heading'.")

(cl-defstruct (magent-memory-scan-plan
               (:constructor magent-memory-scan-plan--create)
               (:copier nil))
  roots
  entry-files
  custom-file
  files
  skipped-sensitive
  skipped-excluded
  skipped-budget
  total-bytes
  generated-at
  provider
  model)

(defvar magent-memory--last-selection nil
  "Last memory prompt selection plist for diagnostics.")

(defvar magent-memory--last-scan-plan nil
  "Last memory scan plan built by init or refresh.")

(defvar magent-memory--last-command-result nil
  "Last memory management command result plist.")

(defun magent-memory-file ()
  "Return the active Magent Emacs profile memory file."
  (expand-file-name magent-memory-file-name magent-memory-directory))

(defun magent-memory-snapshots-directory ()
  "Return the directory where memory snapshots are stored."
  (expand-file-name "snapshots" magent-memory-directory))

(defun magent-memory--json-encode (value)
  "Return VALUE encoded as compact JSON."
  (let ((json-encoding-pretty-print nil))
    (json-encode value)))

(defun magent-memory--json-read (string fallback)
  "Read JSON STRING, returning FALLBACK on failure."
  (condition-case nil
      (let ((json-array-type 'list)
            (json-object-type 'alist)
            (json-key-type 'symbol))
        (json-read-from-string string))
    (error fallback)))

(defun magent-memory--file-size (file)
  "Return FILE size in bytes, or 0 if it cannot be read."
  (or (nth 7 (ignore-errors (file-attributes file)))
      0))

(defun magent-memory--file-mtime-float (file)
  "Return FILE modification time as float seconds, or nil."
  (when-let* ((time (nth 5 (ignore-errors (file-attributes file)))))
    (float-time time)))

(defun magent-memory--directory-p (path)
  "Return non-nil when PATH names an existing directory."
  (and (stringp path)
       (file-directory-p path)))

(defun magent-memory--regular-readable-file-p (path)
  "Return non-nil when PATH is a readable regular file."
  (and (stringp path)
       (file-regular-p path)
       (file-readable-p path)))

(defun magent-memory--expand-path (path)
  "Return expanded PATH with a trailing slash when it is a directory."
  (when (and (stringp path) (not (string-empty-p path)))
    (let ((expanded (expand-file-name path)))
      (if (file-directory-p expanded)
          (file-name-as-directory expanded)
        expanded))))

(defun magent-memory--dedupe-paths (paths)
  "Return PATHS without duplicates, preserving order."
  (let (seen result)
    (dolist (path paths (nreverse result))
      (when-let* ((expanded (magent-memory--expand-path path)))
        (let ((key (directory-file-name expanded)))
          (unless (member key seen)
            (push key seen)
            (push expanded result)))))))

(defun magent-memory--bound-string-value (symbol)
  "Return SYMBOL value when it is bound to a non-empty string."
  (when (and (boundp symbol)
             (stringp (symbol-value symbol))
             (not (string-empty-p (symbol-value symbol))))
    (symbol-value symbol)))

(defun magent-memory--discover-entry-files ()
  "Return Emacs entry files that should be indexed for memory."
  (let (files)
    (dolist (symbol '(early-init-file user-init-file))
      (when-let* ((file (magent-memory--bound-string-value symbol)))
        (push file files)))
    (when (and magent-memory-scan-custom-file
               (boundp 'custom-file)
               (stringp custom-file)
               (not (string-empty-p custom-file)))
      (push custom-file files))
    (magent-memory--dedupe-paths
     (seq-filter #'magent-memory--regular-readable-file-p (nreverse files)))))

(defun magent-memory--custom-file-path ()
  "Return `custom-file' path when available."
  (when (and (boundp 'custom-file)
             (stringp custom-file)
             (not (string-empty-p custom-file)))
    (magent-memory--expand-path custom-file)))

(defun magent-memory-discover-roots ()
  "Return default Emacs configuration roots for memory scanning."
  (let (roots)
    (when (boundp 'user-emacs-directory)
      (push user-emacs-directory roots))
    (dolist (file (magent-memory--discover-entry-files))
      (push (file-name-directory file) roots))
    (when-let* ((doomdir (getenv "DOOMDIR")))
      (push doomdir roots))
    (dolist (symbol '(doom-user-dir dotspacemacs-directory))
      (when-let* ((dir (magent-memory--bound-string-value symbol)))
        (push dir roots)))
    (when-let* ((file (magent-memory--bound-string-value
                       'dotspacemacs-filepath)))
      (push (file-name-directory file) roots))
    (setq roots (append (nreverse roots) magent-memory-extra-scan-roots))
    (magent-memory--dedupe-paths
     (seq-filter #'magent-memory--directory-p roots))))

(defun magent-memory--path-matches-any-p (path regexps)
  "Return non-nil when PATH matches any regexp in REGEXPS."
  (cl-some (lambda (regexp)
             (and (stringp regexp)
                  (string-match-p regexp path)))
           regexps))

(defun magent-memory--excluded-path-p (path)
  "Return non-nil when PATH should be excluded by default."
  (let ((normalized (replace-regexp-in-string
                     "\\\\" "/" (expand-file-name path))))
    (or (magent-memory--path-matches-any-p
         normalized
         magent-memory-exclude-patterns)
        (string-match-p magent-redaction-sensitive-path-regexp
                        (file-name-nondirectory path)))))

(defun magent-memory--sensitive-path-p (path)
  "Return non-nil when PATH appears to contain secrets."
  (string-match-p magent-redaction-sensitive-path-regexp
                  (file-name-nondirectory path)))

(defun magent-memory--top-level-readme-p (file root)
  "Return non-nil when FILE is a README-like file directly under ROOT."
  (let ((relative (file-relative-name file root))
        (name (file-name-nondirectory file)))
    (and (not (string-match-p "/" relative))
         (member name '("README" "README.md" "README.org" "readme.md"
                        "readme.org")))))

(defun magent-memory--candidate-file-p
    (file root entry-files custom-file-path)
  "Return non-nil when FILE under ROOT is eligible for memory scanning.
ENTRY-FILES are always eligible except `custom-file' when
`magent-memory-scan-custom-file' is nil.  CUSTOM-FILE-PATH is recorded by
path even when its contents are not read."
  (let ((name (file-name-nondirectory file)))
    (and (magent-memory--regular-readable-file-p file)
         (not (magent-memory--excluded-path-p file))
         (not (and custom-file-path
                   (equal file custom-file-path)
                   (not magent-memory-scan-custom-file)))
         (or (member file entry-files)
             (and magent-memory-scan-custom-file
                  custom-file-path
                  (equal file custom-file-path))
             (string-suffix-p ".el" name)
             (string-suffix-p ".el.in" name)
             (member name '(".emacs" "init" "early-init"))
             (magent-memory--top-level-readme-p file root)))))

(defun magent-memory--walk-root (root entry-files custom-file-path)
  "Return candidate files and skipped paths under ROOT.
The return value is a plist with `:files', `:sensitive', and `:excluded'."
  (let (files sensitive excluded)
    (cl-labels
        ((walk (path)
           (cond
            ((file-symlink-p path) nil)
            ((magent-memory--sensitive-path-p path)
             (push path sensitive))
            ((magent-memory--excluded-path-p path)
             (push path excluded))
            ((file-directory-p path)
             (dolist (child (ignore-errors
                              (directory-files
                               path t directory-files-no-dot-files-regexp)))
               (walk child)))
            ((magent-memory--candidate-file-p
              path root entry-files custom-file-path)
             (push (magent-memory--expand-path path) files)))))
      (walk root))
    (list :files (nreverse files)
          :sensitive (nreverse sensitive)
          :excluded (nreverse excluded))))

(defun magent-memory--file-priority (file entry-files)
  "Return sorting priority for FILE."
  (let ((name (file-name-nondirectory file)))
    (cond
     ((member file entry-files) 0)
     ((member name '("init.el" "early-init.el" ".emacs")) 1)
     ((member name '("config.el" "packages.el")) 2)
     ((string-match-p "\\`README\\(\\.md\\|\\.org\\)?\\'" name) 3)
     (t 10))))

(defun magent-memory--sort-candidates (files entry-files)
  "Return FILES sorted by scan priority."
  (sort (copy-sequence files)
        (lambda (a b)
          (let ((pa (magent-memory--file-priority a entry-files))
                (pb (magent-memory--file-priority b entry-files)))
            (if (= pa pb)
                (string< a b)
              (< pa pb))))))

(defun magent-memory--select-files-for-budget (files entry-files)
  "Return a plist selecting FILES within memory scan budgets."
  (let ((ordered (magent-memory--sort-candidates files entry-files))
        (count 0)
        (total 0)
        selected skipped)
    (dolist (file ordered)
      (let ((size (magent-memory--file-size file)))
        (if (or (>= count magent-memory-max-files)
                (> size magent-memory-max-file-bytes)
                (> (+ total size) magent-memory-max-scan-bytes))
            (push (list :path file :size size) skipped)
          (push (list :path file :size size) selected)
          (cl-incf count)
          (cl-incf total size))))
    (list :files (nreverse selected)
          :skipped-budget (nreverse skipped)
          :total-bytes total)))

(defun magent-memory--provider-name ()
  "Return current gptel backend name for scan disclosure."
  (cond
   ((and (boundp 'gptel-backend)
         gptel-backend
         (fboundp 'gptel-backend-name))
    (ignore-errors (gptel-backend-name gptel-backend)))
   (t "gptel")))

(defun magent-memory-build-scan-plan ()
  "Build a bounded, content-free Emacs profile memory scan plan."
  (let* ((roots (magent-memory-discover-roots))
         (entry-files (magent-memory--discover-entry-files))
         (custom-file (magent-memory--custom-file-path))
         files sensitive excluded)
    (dolist (root roots)
      (let ((walked (magent-memory--walk-root root entry-files custom-file)))
        (setq files (append files (plist-get walked :files))
              sensitive (append sensitive (plist-get walked :sensitive))
              excluded (append excluded (plist-get walked :excluded)))))
    (setq files (magent-memory--dedupe-paths files))
    (let* ((selection (magent-memory--select-files-for-budget
                       files entry-files))
           (plan (magent-memory-scan-plan--create
                  :roots roots
                  :entry-files entry-files
                  :custom-file custom-file
                  :files (plist-get selection :files)
                  :skipped-sensitive (magent-memory--dedupe-paths sensitive)
                  :skipped-excluded (magent-memory--dedupe-paths excluded)
                  :skipped-budget (plist-get selection :skipped-budget)
                  :total-bytes (plist-get selection :total-bytes)
                  :generated-at (current-time)
                  :provider (magent-memory--provider-name)
                  :model (and (boundp 'gptel-model)
                              (format "%s" gptel-model)))))
      (setq magent-memory--last-scan-plan plan)
      plan)))

(defun magent-memory--scan-plan-file-paths (plan)
  "Return file paths selected by PLAN."
  (mapcar (lambda (entry) (plist-get entry :path))
          (magent-memory-scan-plan-files plan)))

(defun magent-memory-scan-plan-summary (plan)
  "Return a human-readable scan summary for PLAN."
  (format
   (concat "Magent will scan %d Emacs configuration file(s) from %d root(s), "
           "%d byte(s) total, using provider %s%s. Sensitive-looking paths "
           "are skipped first, then matching lines are redacted. Review the "
           "plan before proceeding.")
   (length (magent-memory-scan-plan-files plan))
   (length (magent-memory-scan-plan-roots plan))
   (magent-memory-scan-plan-total-bytes plan)
   (or (magent-memory-scan-plan-provider plan) "gptel")
   (if-let* ((model (magent-memory-scan-plan-model plan)))
       (format " / %s" model)
     "")))

(defun magent-memory-scan-plan-details (plan)
  "Return detailed text for PLAN."
  (let ((files (magent-memory-scan-plan-files plan)))
    (string-join
     (delq
      nil
      (list
       "Magent Emacs Profile Memory Scan Plan"
       ""
       (magent-memory-scan-plan-summary plan)
       ""
       "Configuration roots:"
       (if (magent-memory-scan-plan-roots plan)
           (mapconcat (lambda (root) (format "- %s" root))
                      (magent-memory-scan-plan-roots plan) "\n")
         "- none")
       ""
       "Entry files:"
       (if (magent-memory-scan-plan-entry-files plan)
           (mapconcat (lambda (file) (format "- %s" file))
                      (magent-memory-scan-plan-entry-files plan) "\n")
         "- none")
       ""
       (format "Custom file: %s"
               (or (magent-memory-scan-plan-custom-file plan)
                   "none"))
       ""
       "Files that will be read:"
       (if files
           (mapconcat
            (lambda (entry)
              (format "- %s (%d bytes)"
                      (plist-get entry :path)
                      (or (plist-get entry :size) 0)))
            files
            "\n")
         "- none")
       ""
       (format "Sensitive-looking skipped paths: %d"
               (length (magent-memory-scan-plan-skipped-sensitive plan)))
       (format "Excluded skipped paths: %d"
               (length (magent-memory-scan-plan-skipped-excluded plan)))
       (format "Budget skipped files: %d"
               (length (magent-memory-scan-plan-skipped-budget plan)))))
     "\n")))

(defun magent-memory-scan-plan-approval-input (plan)
  "Return raw input alist for approving PLAN."
  `((roots . ,(vconcat (magent-memory-scan-plan-roots plan)))
    (files . ,(length (magent-memory-scan-plan-files plan)))
    (bytes . ,(magent-memory-scan-plan-total-bytes plan))
    (provider . ,(or (magent-memory-scan-plan-provider plan) "gptel"))
    (model . ,(or (magent-memory-scan-plan-model plan) ""))))

(defun magent-memory--redact-content (content)
  "Return CONTENT with sensitive-looking lines redacted."
  (magent-redaction-string content t))

(defun magent-memory--sanitize-outbound (content)
  "Return CONTENT sanitized for an outbound memory request."
  (magent-redaction-string
   (magent-redaction-normalize-paths content)
   t))

(defun magent-memory--read-file-excerpt (file)
  "Return sanitized excerpt from FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 magent-memory-max-file-bytes)
    (magent-memory--redact-content (buffer-string))))

(defun magent-memory--build-source-bundle (plan)
  "Read PLAN files into a sanitized text bundle."
  (let (parts)
    (dolist (entry (magent-memory-scan-plan-files plan))
      (let* ((file (plist-get entry :path))
             (content (condition-case err
                          (magent-memory--read-file-excerpt file)
                        (error
                         (format "[Unable to read: %s]"
                                 (error-message-string err))))))
        (push (format "## File: %s\n%s" file content) parts)))
    (mapconcat #'identity (nreverse parts) "\n\n")))

(defun magent-memory--source-newest-mtime (plan)
  "Return newest mtime among files in PLAN as float seconds."
  (let ((newest 0.0))
    (dolist (file (magent-memory--scan-plan-file-paths plan))
      (when-let* ((mtime (magent-memory--file-mtime-float file)))
        (setq newest (max newest mtime))))
    newest))

(defun magent-memory--metadata-line (key value)
  "Return an Org keyword metadata line for KEY and VALUE."
  (format "#+magent-%s: %s\n" key value))

(defun magent-memory--format-header (plan active)
  "Return Org metadata header for PLAN.
ACTIVE controls whether this memory can be injected into prompts."
  (let ((generated-at (format-time-string "%FT%T%z"
                                          (magent-memory-scan-plan-generated-at
                                           plan)))
        (roots (magent-memory-scan-plan-roots plan))
        (files (magent-memory--scan-plan-file-paths plan)))
    (concat
     "#+title: Magent Emacs Profile Memory\n"
     (magent-memory--metadata-line "active" (if active "true" "false"))
     (magent-memory--metadata-line "generated-at" generated-at)
     (magent-memory--metadata-line
      "generated-at-float"
      (format "%.3f" (float-time (magent-memory-scan-plan-generated-at plan))))
     (magent-memory--metadata-line
      "roots-json" (magent-memory--json-encode (vconcat roots)))
     (magent-memory--metadata-line
      "source-files-json" (magent-memory--json-encode (vconcat files)))
     (magent-memory--metadata-line
      "source-newest-mtime"
      (format "%.3f" (magent-memory--source-newest-mtime plan)))
     (magent-memory--metadata-line
      "provider" (or (magent-memory-scan-plan-provider plan) "gptel"))
     (magent-memory--metadata-line
      "model" (or (magent-memory-scan-plan-model plan) ""))
     "\n")))

(defun magent-memory--section-template (heading body)
  "Return a managed Org section HEADING with BODY."
  (format "** %s\n%s\n" heading (string-trim-right (or body ""))))

(defun magent-memory--source-index-body (plan)
  "Return deterministic Source Index body for PLAN."
  (string-join
   (append
    (mapcar
     (lambda (entry)
       (format "- %s (%d bytes)"
               (plist-get entry :path)
               (or (plist-get entry :size) 0)))
     (magent-memory-scan-plan-files plan))
    (when-let* ((skipped (magent-memory-scan-plan-skipped-budget plan)))
      (list ""
            "Budget skipped:"
            (mapconcat
             (lambda (entry)
               (format "- %s (%d bytes)"
                       (plist-get entry :path)
                       (or (plist-get entry :size) 0)))
             skipped
             "\n"))))
   "\n"))

(defun magent-memory--skeleton-managed-org (plan &optional source-bundle)
  "Return deterministic managed Org content for PLAN.
SOURCE-BUNDLE may be used for lightweight local hints."
  (let* ((bundle (or source-bundle ""))
         (package-hints
          (delq nil
                (list
                 (and (string-match-p "\\buse-package\\b" bundle)
                      "- Uses `use-package` forms.")
                 (and (string-match-p "\\bstraight\\b" bundle)
                      "- Mentions straight.el.")
                 (and (string-match-p "\\belpaca\\b" bundle)
                      "- Mentions Elpaca.")
                 (and (string-match-p "\\bpackage-install\\b" bundle)
                      "- Mentions package.el."))))
         (entry-files (magent-memory-scan-plan-entry-files plan))
         (custom-file-path (magent-memory-scan-plan-custom-file plan)))
    (concat
     "* " magent-memory--managed-heading "\n"
     (magent-memory--section-template
      "Overview"
      "This profile was generated from a bounded scan of Emacs configuration files. It is intentionally conservative; edit `User Notes` for durable preferences that Magent should prioritize.")
     (magent-memory--section-template
      "Configuration Roots"
      (if (magent-memory-scan-plan-roots plan)
          (mapconcat (lambda (root) (format "- %s" root))
                     (magent-memory-scan-plan-roots plan) "\n")
        "- No configuration roots were discovered."))
     (magent-memory--section-template
      "Entry Files"
      (string-join
       (append
        (if entry-files
            (mapcar (lambda (file) (format "- %s" file)) entry-files)
          '("- No readable entry files were discovered."))
        (when custom-file-path
          (list (format "- custom-file path recorded: %s%s"
                        custom-file-path
                        (if magent-memory-scan-custom-file
                            ""
                          " (content not read by default)")))))
       "\n"))
     (magent-memory--section-template
      "Package Management"
      (if package-hints
          (mapconcat #'identity package-hints "\n")
        "- No package-management summary was inferred without LLM summarization."))
     (magent-memory--section-template
      "Editing And UI Workflow"
      "- Not summarized without LLM summarization.")
     (magent-memory--section-template
      "Completion And Navigation"
      "- Not summarized without LLM summarization.")
     (magent-memory--section-template
      "Org And Notes Workflow"
      "- Ordinary Org notes and knowledge bases were not scanned in this Emacs profile memory pass.")
     (magent-memory--section-template
      "Project And Git Workflow"
      "- Not summarized without LLM summarization.")
     (magent-memory--section-template
      "Programming And LSP Workflow"
      "- Not summarized without LLM summarization.")
     (magent-memory--section-template
      "AI And Magent Workflow"
      "- Not summarized without LLM summarization.")
     (magent-memory--section-template
      "Verification Commands"
      "- Not summarized without LLM summarization.")
     (magent-memory--section-template
      "Cautions And Skipped Sensitive Areas"
      (format
       "- Sensitive-looking paths skipped before reading: %d\n- Excluded paths skipped before reading: %d\n- Lines matching credential-like names were redacted after reading."
       (length (magent-memory-scan-plan-skipped-sensitive plan))
       (length (magent-memory-scan-plan-skipped-excluded plan))))
     (magent-memory--section-template
      "Source Index"
      (or (magent-memory--source-index-body plan)
          "- No files were read.")))))

(defun magent-memory--strip-org-code-fence (text)
  "Return TEXT without a surrounding Markdown code fence."
  (let ((trimmed (string-trim (or text ""))))
    (if (and (string-prefix-p "```" trimmed)
             (string-suffix-p "```" trimmed))
        (let ((lines (split-string trimmed "\n")))
          (string-trim
           (mapconcat #'identity
                      (butlast (cdr lines))
                      "\n")))
      trimmed)))

(defun magent-memory--normalize-managed-org (text)
  "Return managed Org TEXT normalized to include the top heading."
  (let ((text (magent-memory--strip-org-code-fence text)))
    (cond
     ((string-match-p
       (concat "\\`[[:space:]\n\r]*\\* "
               (regexp-quote magent-memory--managed-heading))
       text)
      text)
     ((string-match-p "\\`[[:space:]\n\r]*\\*\\* " text)
      (concat "* " magent-memory--managed-heading "\n" text))
     (t text))))

(defun magent-memory--valid-managed-org-p (text)
  "Return non-nil when TEXT contains the fixed managed Org schema."
  (let ((normalized (magent-memory--normalize-managed-org text)))
    (and (string-match-p
          (concat "^\\* " (regexp-quote magent-memory--managed-heading) "$")
          normalized)
         (cl-every
          (lambda (heading)
            (string-match-p
             (concat "^\\*\\* " (regexp-quote heading) "$")
             normalized))
          magent-memory--managed-section-headings))))

(defun magent-memory--managed-org-or-skeleton (text plan source-bundle)
  "Return validated managed Org TEXT or a deterministic skeleton."
  (let ((normalized (magent-memory--normalize-managed-org text)))
    (if (magent-memory--valid-managed-org-p normalized)
        normalized
      (magent-memory--skeleton-managed-org plan source-bundle))))

(defun magent-memory--extract-user-notes (text)
  "Return the `User Notes' subtree body from Org TEXT."
  (when (stringp text)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^\\* "
                     (regexp-quote magent-memory--user-notes-heading)
                     "$")
             nil t)
        (let ((start (line-beginning-position 2))
              (end (or (save-excursion
                         (when (re-search-forward "^\\* " nil t)
                           (line-beginning-position)))
                       (point-max))))
          (string-trim-right (buffer-substring-no-properties start end)))))))

(defun magent-memory--read-existing-user-notes ()
  "Return preserved user notes from the active memory file."
  (let ((file (magent-memory-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (magent-memory--extract-user-notes (buffer-string))))))

(defun magent-memory--backup-active-file ()
  "Back up the active memory file if it exists.
Return the backup path or nil."
  (let ((file (magent-memory-file)))
    (when (file-exists-p file)
      (let* ((dir (magent-memory-snapshots-directory))
             (stamp (format-time-string "%Y%m%dT%H%M%S"))
             (backup (expand-file-name
                      (format "emacs-profile-%s.org" stamp)
                      dir)))
        (make-directory dir t)
        (copy-file file backup t t t t)
        backup))))

(cl-defun magent-memory--write-profile
    (plan managed-org user-notes &key (active t))
  "Write managed memory Org for PLAN.
MANAGED-ORG is the fixed managed subtree.  USER-NOTES are preserved under
the user-owned heading.  ACTIVE defaults to t."
  (make-directory magent-memory-directory t)
  (let* ((file (magent-memory-file))
         (backup (magent-memory--backup-active-file))
         (body (concat
                (magent-memory--format-header plan active)
                (string-trim-right managed-org)
                "\n\n* " magent-memory--user-notes-heading "\n"
                (string-trim-right (or user-notes ""))
                "\n")))
    (with-temp-file file
      (insert body))
    (list :file file
          :backup backup
          :active active)))

(defun magent-memory--empty-plan ()
  "Return a scan plan suitable for writing an empty memory template."
  (magent-memory-scan-plan--create
   :roots (magent-memory-discover-roots)
   :entry-files (magent-memory--discover-entry-files)
   :custom-file (magent-memory--custom-file-path)
   :files nil
   :skipped-sensitive nil
   :skipped-excluded nil
   :skipped-budget nil
   :total-bytes 0
   :generated-at (current-time)
   :provider (magent-memory--provider-name)
   :model (and (boundp 'gptel-model) (format "%s" gptel-model))))

(defun magent-memory--empty-managed-org ()
  "Return an empty managed memory template."
  (concat
   "* " magent-memory--managed-heading "\n"
   (mapconcat
    (lambda (heading)
      (magent-memory--section-template
       heading
       (if (equal heading "Overview")
           "No active Magent Emacs profile memory."
         "")))
    magent-memory--managed-section-headings
    "")))

(defun magent-memory--summarizer-system-prompt ()
  "Return system prompt for Emacs profile memory summarization."
  (magent-prompt-render
   "internal/memory-system.org"
   `((managed-heading . ,magent-memory--managed-heading)
     (section-headings
      . ,(string-join magent-memory--managed-section-headings ", ")))))

(defun magent-memory--summarizer-user-prompt (plan bundle)
  "Return user prompt for summarizing BUNDLE according to PLAN."
  (magent-memory--sanitize-outbound
   (magent-prompt-render
    "internal/memory-user.org"
    `((scan-roots
       . ,(mapconcat (lambda (root) (format "- %s" root))
                     (magent-memory-scan-plan-roots plan)
                     "\n"))
      (entry-files
       . ,(if (magent-memory-scan-plan-entry-files plan)
              (mapconcat (lambda (file) (format "- %s" file))
                         (magent-memory-scan-plan-entry-files plan)
                         "\n")
            "- none"))
      (custom-file
       . ,(or (magent-memory-scan-plan-custom-file plan) "none"))
      (bundle . ,bundle)))))

(cl-defun magent-memory--summarize-with-llm (plan bundle callback)
  "Summarize BUNDLE for PLAN, then call CALLBACK with text or nil."
  (let* ((request
          (magent-llm-request-create
           :prompt (magent-memory--summarizer-user-prompt plan bundle)
           :system (magent-memory--summarizer-system-prompt)
           :stream nil
           :backend (and (boundp 'gptel-backend) gptel-backend)
           :model (and (boundp 'gptel-model) gptel-model)
           :metadata (list :temperature
                           (and (boundp 'gptel-temperature)
                                gptel-temperature)
                           :disable-provider-tools t
                           :include-reasoning nil
                           :magent-memory t)
           :callback
           (lambda (event)
             (pcase (magent-llm-event-type event)
               ('completed
                (funcall
                 callback
                 (condition-case nil
                     (magent-memory--sanitize-outbound
                      (or (magent-llm-event-text event) ""))
                   (magent-redaction-unsafe-value nil))))
               ('error
                (magent-log
                 "WARN magent memory summarization failed: %s"
                 (condition-case nil
                     (magent-redaction-string
                      (format "%s" (magent-llm-event-message event)) t)
                   (magent-redaction-unsafe-value "redacted error")))
                (funcall callback nil)))))))
    (magent-llm-gptel-sample request)))

(defun magent-memory--complete-command (status message on-complete)
  "Record memory command STATUS and MESSAGE, then call ON-COMPLETE."
  (setq magent-memory--last-command-result
        (list :status status
              :message message
              :completed-at (current-time)))
  (when on-complete
    (funcall on-complete status message)))

(cl-defun magent-memory--write-from-plan
    (operation plan notify-fn on-complete &key open-after-write)
  "Write memory for OPERATION using PLAN.
NOTIFY-FN receives progress strings.  ON-COMPLETE receives status and message."
  (let* ((bundle (magent-memory--build-source-bundle plan))
         (user-notes (magent-memory--read-existing-user-notes))
         (finish
          (lambda (text)
            (let* ((managed (magent-memory--managed-org-or-skeleton
                             (or text "") plan bundle))
                   (result (magent-memory--write-profile
                            plan managed user-notes :active t))
                   (message (format "Magent memory %s complete: %s"
                                    operation
                                    (plist-get result :file))))
              (when (and open-after-write magent-memory-open-after-write)
                (find-file (plist-get result :file)))
              (when notify-fn
                (funcall notify-fn message))
              (magent-memory--complete-command
               'completed message on-complete)))))
    (if magent-memory-use-llm
        (progn
          (when notify-fn
            (funcall notify-fn "Summarizing sanitized Emacs profile memory..."))
          (magent-memory--summarize-with-llm plan bundle finish))
      (funcall finish nil))))

(cl-defun magent-memory-run
    (operation &key confirm-fn notify-fn on-complete open-after-write)
  "Run memory management OPERATION.
OPERATION is one of `init', `refresh', or `clear'.  CONFIRM-FN is called
with a plan and continuation for scan-based operations."
  (condition-case err
      (pcase operation
        ((or 'init 'refresh)
         (let ((plan (magent-memory-build-scan-plan)))
           (when notify-fn
             (funcall notify-fn (magent-memory-scan-plan-summary plan)))
           (let ((continue
                  (lambda (approved)
                    (if (not approved)
                        (magent-memory--complete-command
                         'cancelled "Magent memory scan cancelled." on-complete)
                      (magent-memory--write-from-plan
                       operation plan notify-fn on-complete
                       :open-after-write open-after-write)))))
             (if confirm-fn
                 (funcall confirm-fn plan continue)
               (funcall continue t)))))
        ('clear
         (let ((continue
                (lambda (approved)
                  (if (not approved)
                      (magent-memory--complete-command
                       'cancelled "Magent memory clear cancelled." on-complete)
                    (let* ((user-notes (magent-memory--read-existing-user-notes))
                           (result (magent-memory--write-profile
                                    (magent-memory--empty-plan)
                                    (magent-memory--empty-managed-org)
                                    user-notes
                                    :active nil))
                           (message (format "Magent memory cleared: %s"
                                            (plist-get result :file))))
                      (when notify-fn
                        (funcall notify-fn message))
                      (magent-memory--complete-command
                       'completed message on-complete))))))
           (if confirm-fn
               (funcall confirm-fn nil continue)
             (funcall continue t))))
        (_
         (error "Unknown Magent memory operation: %S" operation)))
    (error
     (let ((message (format "Magent memory %s failed: %s"
                            operation
                            (error-message-string err))))
       (when notify-fn
         (funcall notify-fn message))
       (magent-memory--complete-command 'failed message on-complete)))))

(defun magent-memory--interactive-confirm (plan continue)
  "Interactively confirm PLAN, then call CONTINUE with non-nil on approval."
  (if (null plan)
      (funcall continue
               (yes-or-no-p
                "Deactivate and clear managed Magent profile memory? "))
    (magent--with-display-buffer "*Magent Memory Scan Plan*"
      (insert (magent-memory-scan-plan-details plan)))
    (funcall continue
             (yes-or-no-p "Proceed with Magent memory scan? "))))

;;;###autoload
(defun magent-run-memory-init ()
  "Initialize Magent Emacs profile memory in an internal command session."
  (interactive)
  (magent-command-run "memory-init"))

;;;###autoload
(defun magent-run-memory-refresh ()
  "Refresh Magent Emacs profile memory in an internal command session."
  (interactive)
  (magent-command-run "memory-refresh"))

;;;###autoload
(defun magent-run-memory-clear ()
  "Deactivate and clear managed profile memory in an internal session."
  (interactive)
  (magent-command-run "memory-clear"))

;;;###autoload
(defun magent-memory-open ()
  "Open the active Magent Emacs profile memory file."
  (interactive)
  (find-file (magent-memory-file)))

(defun magent-memory--metadata ()
  "Return active memory Org metadata as an alist."
  (let ((file (magent-memory-file))
        metadata)
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file nil 0 20000)
        (goto-char (point-min))
        (while (re-search-forward "^#\\+magent-\\([^:]+\\):[[:space:]]*\\(.*\\)$"
                                  nil t)
          (push (cons (match-string 1)
                      (string-trim (match-string 2)))
                metadata))))
    (nreverse metadata)))

(defun magent-memory--metadata-get (metadata key)
  "Return KEY from METADATA."
  (cdr (assoc key metadata)))

(defun magent-memory-active-p ()
  "Return non-nil when an active memory file exists."
  (let ((metadata (magent-memory--metadata)))
    (and (file-exists-p (magent-memory-file))
         (not (equal (magent-memory--metadata-get metadata "active")
                     "false")))))

(defun magent-memory--metadata-json-list (metadata key)
  "Return JSON list stored at KEY in METADATA."
  (let ((value (magent-memory--metadata-get metadata key)))
    (if value
        (magent-memory--json-read value nil)
      nil)))

(defun magent-memory-stale-status ()
  "Return a plist describing whether the active memory appears stale."
  (let* ((file (magent-memory-file))
         (metadata (magent-memory--metadata))
         (exists (file-exists-p file))
         (active (and exists
                      (not (equal (magent-memory--metadata-get
                                   metadata "active")
                                  "false"))))
         (generated (string-to-number
                     (or (magent-memory--metadata-get
                          metadata "generated-at-float")
                         "0")))
         (recorded-roots (magent-memory--metadata-json-list
                          metadata "roots-json"))
         (current-roots (magent-memory-discover-roots))
         (source-files (magent-memory--metadata-json-list
                        metadata "source-files-json"))
         reasons)
    (unless exists
      (push "memory file missing" reasons))
    (when (and exists (not active))
      (push "memory is inactive" reasons))
    (when (and exists active (= generated 0))
      (push "generated timestamp missing" reasons))
    (when (and exists active recorded-roots
               (not (equal recorded-roots current-roots)))
      (push "scan roots changed" reasons))
    (dolist (file source-files)
      (when-let* ((mtime (magent-memory--file-mtime-float file)))
        (when (> mtime generated)
          (push (format "source changed: %s" file) reasons))))
    (list :stale (and reasons t)
          :reasons (nreverse reasons)
          :active active
          :exists exists
          :generated-at (magent-memory--metadata-get metadata "generated-at")
          :roots recorded-roots)))

(defun magent-memory-status-text ()
  "Return status text for Magent Emacs profile memory."
  (let* ((file (magent-memory-file))
         (metadata (magent-memory--metadata))
         (stale (magent-memory-stale-status))
         (selection magent-memory--last-selection))
    (string-join
     (delq
      nil
      (list
       "Magent Emacs Profile Memory Status"
       ""
       (format "File: %s" file)
       (format "Exists: %s" (if (plist-get stale :exists) "yes" "no"))
       (format "Active: %s" (if (plist-get stale :active) "yes" "no"))
       (format "Generated at: %s"
               (or (magent-memory--metadata-get metadata "generated-at")
                   "unknown"))
       (format "Auto injection: %s"
               (if magent-memory-enable-auto-injection "enabled" "disabled"))
       (format "Stale: %s"
               (if (plist-get stale :stale) "yes" "no"))
       (when (plist-get stale :reasons)
         (concat "Stale reasons:\n"
                 (mapconcat (lambda (reason) (format "- %s" reason))
                            (plist-get stale :reasons)
                            "\n")))
       (when selection
         (format "Last injected headings: %s"
                 (mapconcat #'identity
                            (or (plist-get selection :headings) nil)
                            ", ")))
       (when selection
         (format "Last omitted headings: %s"
                 (mapconcat #'identity
                            (or (plist-get selection :omitted-headings) nil)
                            ", ")))))
     "\n")))

;;;###autoload
(defun magent-memory-status ()
  "Display Magent Emacs profile memory status."
  (interactive)
  (magent--with-display-buffer "*Magent Memory Status*"
    (insert (magent-memory-status-text))))

(defun magent-memory--command-confirm-provider (context operation)
  "Return confirmation function for memory OPERATION in command CONTEXT."
  (lambda (plan continue)
    (magent-command-record-tool
     context "memory_scan_plan"
     (if plan
         (magent-memory-scan-plan-approval-input plan)
       (list :operation operation))
     (if plan
         (magent-memory-scan-plan-details plan)
       (concat "Deactivate and clear managed Magent Emacs profile memory. "
               "User notes and a snapshot are kept."))
     (list :operation operation))
    (if magent-bypass-permission
        (progn
          (magent-command-record-tool
           context "memory_approval"
           (list :operation operation)
           "approved by magent-bypass-permission"
           (list :operation operation
                 :approved t
                 :bypass t))
          (funcall continue t))
      (magent-memory--interactive-confirm
       plan
       (lambda (approved)
         (magent-command-record-tool
          context "memory_approval"
          (list :operation operation)
          (if approved "approved" "cancelled")
          (list :operation operation
                :approved approved))
         (funcall continue approved))))))

(defun magent-memory--command-runner (operation)
  "Return a command runner for memory OPERATION."
  (lambda (context)
    (magent-memory-run
     operation
     :confirm-fn (magent-memory--command-confirm-provider context operation)
     :notify-fn (lambda (message)
                  (message "%s" message)
                  (magent-command-notify context message))
     :on-complete (lambda (status message)
                    (magent-command-complete context status message))
     :open-after-write (memq operation '(init refresh)))))

(defun magent-memory--load-text ()
  "Return active memory file text, or nil."
  (let ((file (magent-memory-file)))
    (when (and (magent-memory-active-p)
               (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun magent-memory--extract-section (text heading level)
  "Return Org section body for HEADING at LEVEL in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((regexp (format "^%s %s$"
                          (make-string level ?*)
                          (regexp-quote heading))))
      (when (re-search-forward regexp nil t)
        (let* ((start (line-beginning-position 2))
               (end-regexp (format "^%s "
                                    (make-string level ?*)))
               (end (or (save-excursion
                          (when (re-search-forward end-regexp nil t)
                            (line-beginning-position)))
                        (point-max))))
          (string-trim (buffer-substring-no-properties start end)))))))

(defun magent-memory--managed-sections (text)
  "Return managed sections parsed from memory TEXT as an alist."
  (let (sections)
    (dolist (heading magent-memory--managed-section-headings
                     (nreverse sections))
      (push (cons heading
                  (or (magent-memory--extract-section text heading 2) ""))
            sections))))

(defun magent-memory--disable-request-p (prompt)
  "Return non-nil when PROMPT asks not to use memory."
  (let ((case-fold-search t)
        (prompt (or prompt "")))
    (or (string-match-p "不要使用[[:space:]]*memory" prompt)
        (string-match-p "ignore[[:space:]]+magent[[:space:]]+memory" prompt)
        (string-match-p "do[[:space:]]+not[[:space:]]+use[[:space:]]+memory" prompt)
        (string-match-p "without[[:space:]]+memory" prompt))))

(defun magent-memory--relevant-request-p (prompt project-root)
  "Return non-nil when PROMPT or PROJECT-ROOT is Emacs-profile relevant."
  (let ((case-fold-search t)
        (text (concat (or prompt "") "\n" (or project-root "")))
        (root (or project-root "")))
    (or (string-match-p "\\.emacs\\.d\\|/emacs\\.d\\|/magent\\b" root)
        (string-match-p
         (regexp-opt '("emacs" "magent" "elisp" "init.el" "early-init"
                       "custom-file" "use-package" "doom" "spacemacs"
                       "org-mode" "eglot" "gptel" "magit" "keybinding"
                       "keymap" "evil" "straight.el" "elpaca"
                       "defcustom")
                     'words)
         text))))

(defun magent-memory--prompt-tokens (prompt)
  "Return lowercase prompt tokens useful for matching user notes."
  (let ((case-fold-search t)
        tokens)
    (dolist (token (split-string (downcase (or prompt ""))
                                 "[^[:alnum:]_-]+" t)
                   (nreverse tokens))
      (when (> (length token) 3)
        (push token tokens)))))

(defun magent-memory--user-notes-relevant-p (prompt user-notes)
  "Return non-nil when USER-NOTES appear relevant to PROMPT."
  (let ((notes (downcase (or user-notes ""))))
    (and (not (string-empty-p notes))
         (cl-some (lambda (token)
                    (string-match-p (regexp-quote token) notes))
                  (magent-memory--prompt-tokens prompt)))))

(defun magent-memory--section-score (heading prompt)
  "Return relevance score for managed HEADING against PROMPT."
  (let ((case-fold-search t)
        (prompt (or prompt "")))
    (pcase heading
      ("Overview" 100)
      ("Configuration Roots"
       (if (string-match-p "root\\|directory\\|where\\|config" prompt) 80 20))
      ("Entry Files"
       (if (string-match-p "init\\|early-init\\|entry\\|startup" prompt) 90 20))
      ("Package Management"
       (if (string-match-p "package\\|use-package\\|straight\\|elpaca\\|install" prompt) 90 10))
      ("Editing And UI Workflow"
       (if (string-match-p "edit\\|ui\\|window\\|buffer\\|theme\\|evil" prompt) 90 10))
      ("Completion And Navigation"
       (if (string-match-p "completion\\|navigation\\|consult\\|vertico\\|company\\|corfu" prompt) 90 10))
      ("Org And Notes Workflow"
       (if (string-match-p "\\borg\\b\\|note\\|agenda\\|roam" prompt) 90 10))
      ("Project And Git Workflow"
       (if (string-match-p "project\\|git\\|magit\\|vc" prompt) 90 10))
      ("Programming And LSP Workflow"
       (if (string-match-p "program\\|lsp\\|eglot\\|tree-sitter\\|compile" prompt) 90 10))
      ("AI And Magent Workflow"
       (if (string-match-p "ai\\|magent\\|agent\\|gptel\\|llm" prompt) 95 10))
      ("Verification Commands"
       (if (string-match-p "test\\|verify\\|compile\\|check\\|lint" prompt) 90 10))
      ("Cautions And Skipped Sensitive Areas"
       (if (string-match-p "secret\\|token\\|credential\\|safe\\|security\\|sensitive" prompt) 90 15))
      (_ 0))))

(defun magent-memory--select-sections (prompt sections user-notes)
  "Select memory SECTIONS and USER-NOTES for PROMPT."
  (let* ((max-sections (max 1 magent-memory-max-injected-sections))
         (scored (sort
                  (copy-sequence sections)
                  (lambda (a b)
                    (> (magent-memory--section-score (car a) prompt)
                       (magent-memory--section-score (car b) prompt)))))
         (selected nil)
         (omitted nil))
    (when (and user-notes
               (magent-memory--user-notes-relevant-p prompt user-notes))
      (push (cons "User Notes" user-notes) selected))
    (dolist (section scored)
      (if (< (length selected) max-sections)
          (push section selected)
        (push (car section) omitted)))
    (list :sections (nreverse selected)
          :omitted-headings (nreverse omitted))))

(defun magent-memory--truncate-sections-to-budget (sections max-chars)
  "Return SECTIONS formatted as Org-ish text capped at MAX-CHARS."
  (let ((remaining max-chars)
        included
        omitted)
    (dolist (section sections)
      (let* ((heading (car section))
             (body (string-trim (or (cdr section) "")))
             (text (format "## %s\n%s\n" heading body)))
        (if (or (<= remaining 0)
                (> (length text) remaining))
            (push heading omitted)
          (push text included)
          (setq remaining (- remaining (length text))))))
    (list :text (string-trim-right (mapconcat #'identity
                                              (nreverse included)
                                              "\n"))
          :omitted (nreverse omitted))))

(defun magent-memory-system-message (prompt &optional _request-context project-root)
  "Return a selected memory system message for PROMPT, or nil."
  (when (and magent-memory-enable-auto-injection
             (not (magent-memory--disable-request-p prompt))
             (magent-memory-active-p)
             (magent-memory--relevant-request-p prompt project-root))
    (when-let* ((text (magent-memory--load-text)))
      (let* ((sections (magent-memory--managed-sections text))
             (user-notes (magent-memory--extract-user-notes text))
             (selection (magent-memory--select-sections
                         prompt sections user-notes))
             (budgeted (magent-memory--truncate-sections-to-budget
                        (plist-get selection :sections)
                        magent-memory-injection-max-chars))
             (included-text
              (condition-case nil
                  (magent-memory--sanitize-outbound
                   (plist-get budgeted :text))
                (magent-redaction-unsafe-value nil)))
             (headings (mapcar #'car (plist-get selection :sections))))
        (unless (or (null included-text) (string-empty-p included-text))
          (setq magent-memory--last-selection
                (list :prompt prompt
                      :headings headings
                      :omitted-headings
                      (append (plist-get selection :omitted-headings)
                              (plist-get budgeted :omitted))
                      :selected-at (current-time)))
          (magent-prompt-render
           "internal/memory-injection.org"
           `((memory . ,included-text))))))))

(magent-command-register
 "memory-init"
 :description "Initialize Magent Emacs profile memory."
 :title "Initialize Magent Emacs profile memory"
 :runner-type 'pipeline
 :runner (magent-memory--command-runner 'init))

(magent-command-register
 "memory-refresh"
 :description "Refresh Magent Emacs profile memory."
 :title "Refresh Magent Emacs profile memory"
 :runner-type 'pipeline
 :runner (magent-memory--command-runner 'refresh))

(magent-command-register
 "memory-clear"
 :description "Deactivate and clear managed Magent Emacs profile memory."
 :title "Deactivate and clear Magent Emacs profile memory"
 :runner-type 'pipeline
 :runner (magent-memory--command-runner 'clear))

(provide 'magent-memory)
;;; magent-memory.el ends here
