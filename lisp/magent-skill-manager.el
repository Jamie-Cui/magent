;;; magent-skill-manager.el --- Find, install, and delete Magent skills  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; User-level skill management for Magent.  Finder results come from
;; skills.sh, while installation writes only to Magent's configured user
;; skill directory.  External skills are restricted to instruction skills;
;; their files are copied and never executed by this module.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'tabulated-list)
(require 'url)
(require 'url-http)
(require 'magent-config)
(require 'magent-file-loader)
(require 'magent-json)
(require 'magent-skills)

(cl-defstruct magent-skill-candidate
  name slug source installs page-url)

(cl-defstruct magent-skill-package
  name description source-kind source source-id source-directory
  repository ref commit repository-subdirectory files file-count
  byte-size contains-code digest)

(defconst magent-skill-manager--provenance-file ".magent-install.json")
(defconst magent-skill-manager--finder-buffer "*Magent Skill Finder*")
(defconst magent-skill-manager--preview-buffer "*Magent Skill Preview*")

(defvar-local magent-skill-manager--finder-query nil)
(defvar-local magent-skill-manager--finder-candidates nil)
(defvar-local magent-skill-manager--finder-generation 0)
(defvar-local magent-skill-manager--finder-request-buffer nil)
(defvar-local magent-skill-manager--finder-timeout-timer nil)

(defun magent-skill-manager--parse-search-response (response)
  "Parse skills.sh search RESPONSE and return ranked candidates."
  (let* ((object (json-parse-string response
                                    :object-type 'plist
                                    :array-type 'list
                                    :null-object nil
                                    :false-object nil))
         (skills (plist-get object :skills))
         candidates)
    (dolist (skill skills)
      (let ((name (plist-get skill :name))
            (source (plist-get skill :source))
            (slug (plist-get skill :id))
            (installs (or (plist-get skill :installs) 0)))
        (when (and (stringp name) (stringp source))
          (push (make-magent-skill-candidate
                 :name name
                 :slug (or slug (concat source "/" name))
                 :source source
                 :installs (if (numberp installs) installs 0)
                 :page-url (and slug (concat "https://skills.sh/" slug)))
                candidates))))
    (setq candidates
          (sort candidates
                (lambda (left right)
                  (> (magent-skill-candidate-installs left)
                     (magent-skill-candidate-installs right)))))
    (if (> (length candidates) magent-skill-search-limit)
        (cl-subseq candidates 0 magent-skill-search-limit)
      candidates)))

(defun magent-skill-manager--user-root ()
  "Return the canonical Magent user skill installation directory."
  (or (car (last magent-skill-directories))
      (user-error "No Magent user skill directory is configured")))

(defun magent-skill-manager--skill-name-at (directory)
  "Return the declared skill name in DIRECTORY, or nil."
  (condition-case nil
      (let* ((file (expand-file-name magent-skill-file-name directory))
             (definition (and (file-readable-p file)
                              (magent-file-loader-read-definition file)))
             (frontmatter (plist-get definition :frontmatter))
             (name (plist-get frontmatter :name)))
        (and name (format "%s" name)))
    (error nil)))

(defun magent-skill-manager--user-installations ()
  "Return user-level skill installation plists.
Bundled and project-local skills are intentionally excluded."
  (let (installations)
    (dolist (root magent-skill-directories)
      (when (file-directory-p root)
        (dolist (entry (directory-files root t directory-files-no-dot-files-regexp t))
          (when (and (or (file-directory-p entry) (file-symlink-p entry))
                     (file-readable-p
                      (expand-file-name magent-skill-file-name entry)))
            (when-let* ((name (magent-skill-manager--skill-name-at entry)))
              (push (list :name name
                          :path entry
                          :root root
                          :managed (file-exists-p
                                    (expand-file-name
                                     magent-skill-manager--provenance-file
                                     entry)))
                    installations))))))
    (nreverse installations)))

(defun magent-skill-manager--installed-p (name)
  "Return non-nil when user skill NAME is installed."
  (cl-some (lambda (item) (equal name (plist-get item :name)))
           (magent-skill-manager--user-installations)))

(defun magent-skill-manager--finder-entries ()
  "Return tabulated entries for the current finder buffer."
  (mapcar
   (lambda (candidate)
     (list (magent-skill-candidate-slug candidate)
           (vector (magent-skill-candidate-name candidate)
                   (magent-skill-candidate-source candidate)
                   (number-to-string (magent-skill-candidate-installs candidate))
                   (if (magent-skill-manager--installed-p
                        (magent-skill-candidate-name candidate))
                       "installed"
                     "available"))))
   magent-skill-manager--finder-candidates))

(define-derived-mode magent-skill-finder-mode tabulated-list-mode
  "Magent-Skills"
  "Major mode for skills.sh search results."
  (setq tabulated-list-format
        [("Name" 28 t)
         ("Repository" 34 t)
         ("Installs" 12 magent-skill-manager--sort-number)
         ("Status" 12 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'kill-buffer-hook
            #'magent-skill-manager--cancel-finder-request nil t)
  (tabulated-list-init-header))

(defun magent-skill-manager--sort-number (left right)
  "Sort tabulated entries LEFT and RIGHT by their numeric column."
  (> (string-to-number (aref (cadr left) 2))
     (string-to-number (aref (cadr right) 2))))

(defun magent-skill-manager--candidate-at-point ()
  "Return finder candidate at point, or signal a user error."
  (let ((id (tabulated-list-get-id)))
    (or (cl-find id magent-skill-manager--finder-candidates
                 :key #'magent-skill-candidate-slug :test #'equal)
        (user-error "No skill at point"))))

(defun magent-skill-manager--render-finder (buffer candidates)
  "Render CANDIDATES in finder BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq magent-skill-manager--finder-candidates candidates)
      (setq tabulated-list-entries (magent-skill-manager--finder-entries))
      (tabulated-list-print t))))

(defun magent-skill-manager--cancel-finder-request ()
  "Cancel the pending finder request owned by the current buffer."
  (when (timerp magent-skill-manager--finder-timeout-timer)
    (cancel-timer magent-skill-manager--finder-timeout-timer))
  (setq magent-skill-manager--finder-timeout-timer nil)
  (when (buffer-live-p magent-skill-manager--finder-request-buffer)
    (kill-buffer magent-skill-manager--finder-request-buffer))
  (setq magent-skill-manager--finder-request-buffer nil))

(defun magent-skill-manager--search-timeout
    (buffer generation request-buffer)
  "Cancel REQUEST-BUFFER when BUFFER still owns GENERATION."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (= generation magent-skill-manager--finder-generation)
        (when (buffer-live-p request-buffer)
          (kill-buffer request-buffer))
        (setq magent-skill-manager--finder-request-buffer nil
              magent-skill-manager--finder-timeout-timer nil)
        (message "skills.sh search timed out")))))

(defun magent-skill-manager--search-callback
    (status buffer query generation)
  "Handle skills.sh STATUS for BUFFER, QUERY, and request GENERATION."
  (let ((response-buffer (current-buffer)))
    (unwind-protect
        (when (and (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (= generation magent-skill-manager--finder-generation)))
          (with-current-buffer buffer
            (when (timerp magent-skill-manager--finder-timeout-timer)
              (cancel-timer magent-skill-manager--finder-timeout-timer))
            (setq magent-skill-manager--finder-timeout-timer nil
                  magent-skill-manager--finder-request-buffer nil))
          (condition-case err
              (progn
                (when (plist-get status :error)
                  (error "skills.sh request failed: %s"
                         (plist-get status :error)))
                (unless (and (boundp 'url-http-response-status)
                             (= url-http-response-status 200))
                  (error "skills.sh returned HTTP %s"
                         (if (boundp 'url-http-response-status)
                             url-http-response-status
                           "unknown")))
                (goto-char (or (and (boundp 'url-http-end-of-headers)
                                    url-http-end-of-headers)
                               (point-min)))
                (let ((candidates
                       (magent-skill-manager--parse-search-response
                        (buffer-substring-no-properties (point) (point-max)))))
                  (magent-skill-manager--render-finder buffer candidates)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (message "Found %d skills for %s"
                               (length candidates) query)))))
            (error
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq tabulated-list-entries nil)
                 (tabulated-list-print t)
                 (message "%s" (error-message-string err)))))))
      (when (buffer-live-p response-buffer)
        (kill-buffer response-buffer)))))

;;;###autoload
(defun magent-skill-find (query)
  "Search skills.sh for QUERY in a dedicated finder buffer."
  (interactive (list (read-string "Find Magent skill: ")))
  (when (string-blank-p query)
    (user-error "Search query cannot be empty"))
  (let ((buffer (get-buffer-create magent-skill-manager--finder-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magent-skill-finder-mode)
        (magent-skill-finder-mode))
      (magent-skill-manager--cancel-finder-request)
      (cl-incf magent-skill-manager--finder-generation)
      (setq magent-skill-manager--finder-query query)
      (setq magent-skill-manager--finder-candidates nil
            tabulated-list-entries nil)
      (tabulated-list-print t))
    (pop-to-buffer buffer)
    (message "Searching skills.sh for %s..." query)
    (let* ((generation
            (buffer-local-value 'magent-skill-manager--finder-generation
                                buffer))
           (request-buffer
            (url-retrieve
             (concat magent-skill-search-endpoint
                     "?q=" (url-hexify-string query)
                     "&limit=" (number-to-string magent-skill-search-limit))
             #'magent-skill-manager--search-callback
             (list buffer query generation) t t)))
      (with-current-buffer buffer
        (when (and (= generation magent-skill-manager--finder-generation)
                   (buffer-live-p request-buffer))
          (setq magent-skill-manager--finder-request-buffer request-buffer
                magent-skill-manager--finder-timeout-timer
                (run-at-time magent-skill-search-timeout nil
                             #'magent-skill-manager--search-timeout
                             buffer generation request-buffer)))))))

(defun magent-skill-manager-finder-refresh ()
  "Run a new finder search, defaulting to the current query."
  (interactive)
  (magent-skill-find
   (read-string "Find Magent skill: " magent-skill-manager--finder-query)))

(defun magent-skill-manager--github-source (source)
  "Parse public GitHub SOURCE into a source plist, or return nil."
  (cond
   ((string-match
     "\\`\\([^/@[:space:]]+\\)/\\([^/@[:space:]]+\\)@\\([^/@[:space:]]+\\)\\'"
     source)
    (let ((repository (format "%s/%s" (match-string 1 source)
                              (match-string 2 source))))
      (list :source-kind 'github
            :source source
            :repository repository
            :expected-name (match-string 3 source)
            :clone-url (concat "https://github.com/" repository ".git"))))
   ((string-prefix-p "https://github.com/" source)
    (let* ((url (url-generic-parse-url source))
           (parts (split-string (url-filename url) "/" t))
           (owner (nth 0 parts))
           (repo (and (nth 1 parts)
                      (string-remove-suffix ".git" (nth 1 parts))))
           (tree-p (equal (nth 2 parts) "tree"))
           (ref (and tree-p (nth 3 parts)))
           (subdirectory (and tree-p (string-join (nthcdr 4 parts) "/"))))
      (when (and (equal (url-host url) "github.com") owner repo
                 (or (not tree-p) ref))
        (list :source-kind 'github
              :source source
              :repository (format "%s/%s" owner repo)
              :ref ref
              :repository-subdirectory
              (unless (string-empty-p (or subdirectory "")) subdirectory)
              :clone-url (format "https://github.com/%s/%s.git" owner repo)))))
   (t nil)))

(defun magent-skill-manager--call-git (&rest arguments)
  "Run git with argv-based ARGUMENTS and return trimmed output."
  (let ((process-environment (cons "GIT_TERMINAL_PROMPT=0"
                                   process-environment)))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil t nil
                           "-c" "credential.helper=" arguments)))
        (unless (zerop status)
          (error "git %s failed: %s"
                 (car arguments) (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun magent-skill-manager--find-skill-directory
    (root &optional expected-name subdirectory)
  "Find a skill directory below ROOT.
Prefer SUBDIRECTORY, otherwise select EXPECTED-NAME or prompt when needed."
  (let* ((explicit (and subdirectory (expand-file-name subdirectory root)))
         (_ (when (and explicit
                       (not (string-prefix-p
                             (file-name-as-directory (expand-file-name root))
                             (file-name-as-directory explicit))))
              (user-error "GitHub skill path escapes the repository")))
         (files (if explicit
                    (list (expand-file-name magent-skill-file-name explicit))
                  (directory-files-recursively
                   root (concat (regexp-quote magent-skill-file-name) "\\'"))))
         choices)
    (dolist (file files)
      (when (and (file-regular-p file)
                 (not (string-match-p "/\\.git/" file)))
        (when-let* ((name (magent-skill-manager--skill-name-at
                           (file-name-directory file))))
          (push (cons name (file-name-directory file)) choices))))
    (setq choices (nreverse choices))
    (cond
     (expected-name
      (or (cdr (assoc expected-name choices))
          (user-error "Skill %s was not found in the source" expected-name)))
     ((= (length choices) 1) (cdar choices))
     ((null choices) (user-error "No valid SKILL.md was found in the source"))
     (t (cdr (assoc (completing-read "Skill to install: " choices nil t)
                    choices))))))

(defun magent-skill-manager--collect-files (directory)
  "Return safe regular files below DIRECTORY, rejecting symbolic links."
  (let (files)
    (cl-labels
        ((walk
          (dir)
          (dolist (entry (directory-files
                          dir t directory-files-no-dot-files-regexp t))
            (let ((name (file-name-nondirectory entry)))
              (unless (or (equal name ".git")
                          (equal name magent-skill-manager--provenance-file))
                (cond
                 ((file-symlink-p entry)
                  (user-error "Skill contains a symbolic link: %s"
                              (file-relative-name entry directory)))
                 ((file-directory-p entry) (walk entry))
                 ((file-regular-p entry) (push entry files))
                 (t (user-error "Skill contains an unsupported file: %s"
                                (file-relative-name entry directory)))))))))
      (when (file-symlink-p directory)
        (user-error "Skill directory cannot be a symbolic link"))
      (walk directory))
    (sort files #'string<)))

(defun magent-skill-manager--digest-files (directory files)
  "Return a stable digest for FILES relative to DIRECTORY."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (dolist (file files)
      (insert (encode-coding-string (file-relative-name file directory) 'utf-8)
              "\0")
      (insert-file-contents-literally file)
      (insert "\0"))
    (secure-hash 'sha256 (current-buffer))))

(defun magent-skill-manager--preflight-directory (directory source)
  "Validate DIRECTORY using SOURCE metadata and return a package."
  (let* ((skill-file (expand-file-name magent-skill-file-name directory))
         (definition (and (file-regular-p skill-file)
                          (magent-file-loader-read-definition skill-file)))
         (frontmatter (plist-get definition :frontmatter))
         (raw-name (plist-get frontmatter :name))
         (name (and raw-name (format "%s" raw-name)))
         (raw-description (plist-get frontmatter :description))
         (description
          (cond ((stringp raw-description) raw-description)
                ((listp raw-description)
                 (mapconcat (lambda (item) (format "%s" item))
                            raw-description ", "))))
         (type (downcase (format "%s" (or (plist-get frontmatter :type)
                                            "instruction")))))
    (unless frontmatter
      (user-error "SKILL.md must contain YAML frontmatter"))
    (unless (and name
                 (string-match-p
                  "\\`[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\'" name))
      (user-error "Skill name must use lowercase letters, digits, and hyphens"))
    (when (string-blank-p (or description ""))
      (user-error "Skill description cannot be empty"))
    (unless (equal type "instruction")
      (user-error "External skill type %s is unsupported; only instruction skills can be installed"
                  type))
    (let* ((files (magent-skill-manager--collect-files directory))
           (file-count (length files))
           (byte-size
            (cl-loop for file in files
                     sum (file-attribute-size (file-attributes file))))
           (contains-code
            (cl-some
             (lambda (file)
               (let ((relative (file-relative-name file directory)))
                 (or (string-match-p "\\`scripts/" relative)
                     (string-match-p "\\.el\\'" relative)
                     (/= 0 (logand #o111 (file-modes file))))))
             files)))
      (when (> file-count magent-skill-install-max-files)
        (user-error "Skill has %d files; limit is %d"
                    file-count magent-skill-install-max-files))
      (when (> byte-size magent-skill-install-max-bytes)
        (user-error "Skill is %d bytes; limit is %d"
                    byte-size magent-skill-install-max-bytes))
      (make-magent-skill-package
       :name name
       :description description
       :source-kind (plist-get source :source-kind)
       :source (plist-get source :source)
       :source-id (or (plist-get source :source-id)
                      (concat "local:" (file-truename directory)))
       :source-directory directory
       :repository (plist-get source :repository)
       :ref (plist-get source :ref)
       :commit (plist-get source :commit)
       :repository-subdirectory (plist-get source :repository-subdirectory)
       :files files
       :file-count file-count
       :byte-size byte-size
       :contains-code contains-code
       :digest (magent-skill-manager--digest-files directory files)))))

(defun magent-skill-manager--resolve-source (source)
  "Resolve SOURCE and return (PACKAGE . TEMP-DIRECTORY)."
  (if (file-directory-p source)
      (let* ((root (expand-file-name source))
             (directory
              (if (file-regular-p
                   (expand-file-name magent-skill-file-name root))
                  root
                (magent-skill-manager--find-skill-directory root))))
        (cons (magent-skill-manager--preflight-directory
               directory
               (list :source-kind 'local
                     :source source
                     :source-id (concat "local:" (file-truename directory))))
              nil))
    (let ((metadata (magent-skill-manager--github-source source)))
      (unless metadata
        (user-error "Use a local directory, owner/repo@skill, or public GitHub URL"))
      (let ((checkout (make-temp-file "magent-skill-checkout-" t)))
        (condition-case err
            (progn
              (let ((arguments
                     (append (list "clone" "--depth" "1")
                             (when (plist-get metadata :ref)
                               (list "--branch" (plist-get metadata :ref)))
                             (list (plist-get metadata :clone-url) checkout))))
                (apply #'magent-skill-manager--call-git arguments))
              (let* ((directory
                      (magent-skill-manager--find-skill-directory
                       checkout
                       (plist-get metadata :expected-name)
                       (plist-get metadata :repository-subdirectory)))
                     (subdirectory (file-relative-name directory checkout))
                     (source-id
                      (format "github:%s#%s"
                              (plist-get metadata :repository)
                              subdirectory))
                     (commit (magent-skill-manager--call-git
                              "-C" checkout "rev-parse" "HEAD")))
                (setq metadata
                      (plist-put metadata :repository-subdirectory subdirectory))
                (setq metadata (plist-put metadata :source-id source-id))
                (setq metadata (plist-put metadata :commit commit))
                (cons (magent-skill-manager--preflight-directory
                       directory metadata)
                      checkout)))
          (error
           (delete-directory checkout t)
           (signal (car err) (cdr err))))))))

(defun magent-skill-manager--read-provenance (directory)
  "Read managed provenance from DIRECTORY, or return nil."
  (let ((file (expand-file-name magent-skill-manager--provenance-file
                                directory)))
    (when (file-regular-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (json-parse-buffer :object-type 'plist
                               :array-type 'list
                               :null-object nil
                               :false-object nil))
        (error nil)))))

(defun magent-skill-manager--write-provenance (directory package)
  "Write provenance for PACKAGE into DIRECTORY."
  (let ((file (expand-file-name magent-skill-manager--provenance-file
                                directory)))
    (with-temp-file file
      (insert
       (magent-json-encode
        (list :schema 1
              :name (magent-skill-package-name package)
              :source (magent-skill-package-source package)
              :source-kind (magent-skill-package-source-kind package)
              :source-id (magent-skill-package-source-id package)
              :repository (magent-skill-package-repository package)
              :ref (magent-skill-package-ref package)
              :commit (magent-skill-package-commit package)
              :subdirectory
              (magent-skill-package-repository-subdirectory package)
              :installed-at (format-time-string "%FT%TZ" nil t)
              :digest (magent-skill-package-digest package)))
       "\n"))))

(defun magent-skill-manager--copy-package (package staging)
  "Copy PACKAGE files into STAGING and write provenance."
  (let ((source (magent-skill-package-source-directory package)))
    (dolist (file (magent-skill-package-files package))
      (let* ((relative (file-relative-name file source))
             (destination (expand-file-name relative staging)))
        (make-directory (file-name-directory destination) t)
        (copy-file file destination t t nil t)))
    (magent-skill-manager--write-provenance staging package)))

(defun magent-skill-manager--remove-path (path)
  "Remove PATH without following symbolic links."
  (if (file-symlink-p path)
      (delete-file path)
    (when (file-exists-p path)
      (delete-directory path t))))

(defun magent-skill-manager--install-package (package)
  "Confirm and atomically install PACKAGE."
  (let* ((root (file-name-as-directory
                (expand-file-name (magent-skill-manager--user-root))))
         (destination
          (expand-file-name (magent-skill-package-name package) root))
         (existing-p (or (file-exists-p destination)
                         (file-symlink-p destination)))
         (provenance (and existing-p
                          (not (file-symlink-p destination))
                          (magent-skill-manager--read-provenance destination))))
    (when existing-p
      (unless (and provenance
                   (equal (plist-get provenance :source-id)
                          (magent-skill-package-source-id package)))
        (user-error "Destination %s is unmanaged or from another source; delete it first"
                    destination)))
    (unless
        (y-or-n-p
         (format "%s %s?\nDescription: %s\nSource: %s\nCommit: %s\nDestination: %s\nFiles: %d (%s)%s\nProceed "
                 (if existing-p "Reinstall" "Install")
                 (magent-skill-package-name package)
                 (magent-skill-package-description package)
                 (magent-skill-package-source package)
                 (or (magent-skill-package-commit package) "local")
                 destination
                 (magent-skill-package-file-count package)
                 (file-size-human-readable
                  (magent-skill-package-byte-size package))
                 (if (magent-skill-package-contains-code package)
                     ", contains scripts/code (copied but not executed)"
                   "")))
      (user-error "Skill installation cancelled"))
    (make-directory root t)
    (let ((staging (make-temp-file
                    (expand-file-name ".magent-skill-staging-" root) t))
          (backup (make-temp-name
                   (expand-file-name ".magent-skill-backup-" root)))
          installed)
      (unwind-protect
          (progn
            (magent-skill-manager--copy-package package staging)
            (when existing-p
              (rename-file destination backup))
            (condition-case err
                (progn
                  (rename-file staging destination)
                  (setq staging nil
                        installed t)
                  (magent-skills-reload)
                  (when (file-exists-p backup)
                    (delete-directory backup t))
                  (message "Installed Magent skill %s"
                           (magent-skill-package-name package))
                  destination)
              (error
               (when installed
                 (magent-skill-manager--remove-path destination))
               (when (file-exists-p backup)
                 (rename-file backup destination))
               (ignore-errors (magent-skills-reload))
               (signal (car err) (cdr err)))))
        (when (and staging (file-exists-p staging))
          (delete-directory staging t))
        (when (file-exists-p backup)
          (delete-directory backup t))))))

;;;###autoload
(defun magent-skill-install (source)
  "Install one instruction skill from SOURCE into Magent's user directory.
SOURCE may be a local directory, OWNER/REPO@SKILL, or a public GitHub URL."
  (interactive (list (read-string "Install Magent skill from: ")))
  (let* ((resolved (magent-skill-manager--resolve-source source))
         (package (car resolved))
         (temporary (cdr resolved)))
    (unwind-protect
        (magent-skill-manager--install-package package)
      (when (and temporary (file-exists-p temporary))
        (delete-directory temporary t)))))

(defun magent-skill-manager-preview ()
  "Preview the finder skill at point after resolving and validating it."
  (interactive)
  (let* ((candidate (magent-skill-manager--candidate-at-point))
         (source (format "%s@%s"
                         (magent-skill-candidate-source candidate)
                         (magent-skill-candidate-name candidate)))
         (resolved (magent-skill-manager--resolve-source source))
         (package (car resolved))
         (temporary (cdr resolved)))
    (unwind-protect
        (with-current-buffer (get-buffer-create magent-skill-manager--preview-buffer)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "%s\n\n%s\n\nSource: %s\nFiles: %d\nSize: %s\nCommit: %s\nCode/scripts: %s\n\n"
                            (magent-skill-package-name package)
                            (magent-skill-package-description package)
                            (magent-skill-package-source package)
                            (magent-skill-package-file-count package)
                            (file-size-human-readable
                             (magent-skill-package-byte-size package))
                            (or (magent-skill-package-commit package) "local")
                            (if (magent-skill-package-contains-code package)
                                "yes (will not be executed)" "no")))
            (insert-file-contents
             (expand-file-name magent-skill-file-name
                               (magent-skill-package-source-directory package)))
            (special-mode))
          (pop-to-buffer (current-buffer)))
      (when (and temporary (file-exists-p temporary))
        (delete-directory temporary t)))))

(defun magent-skill-manager-install-at-point ()
  "Install the finder skill at point without copying a repository name."
  (interactive)
  (let ((candidate (magent-skill-manager--candidate-at-point)))
    (magent-skill-install
     (format "%s@%s"
             (magent-skill-candidate-source candidate)
             (magent-skill-candidate-name candidate)))
    (setq tabulated-list-entries (magent-skill-manager--finder-entries))
    (tabulated-list-print t)))

;;;###autoload
(defun magent-skill-delete ()
  "Permanently delete one user-level Magent skill after one confirmation."
  (interactive)
  (let* ((installations (magent-skill-manager--user-installations))
         (counts (make-hash-table :test #'equal))
         choices)
    (unless installations
      (user-error "No user-level Magent skills are installed"))
    (dolist (item installations)
      (puthash (plist-get item :name)
               (1+ (gethash (plist-get item :name) counts 0)) counts))
    (dolist (item installations)
      (let* ((name (plist-get item :name))
             (display
              (if (> (gethash name counts) 1)
                  (format "%s [%s]" name (plist-get item :root))
                name)))
        (push (cons display item) choices)))
    (setq choices (nreverse choices))
    (let* ((selected (completing-read "Delete Magent skill: " choices nil t))
           (item (cdr (assoc selected choices)))
           (path (plist-get item :path))
           (root (plist-get item :root)))
      (unless (y-or-n-p
               (format "Permanently delete %s%s at %s? "
                       (plist-get item :name)
                       (if (plist-get item :managed) "" " (unmanaged)")
                       path))
        (user-error "Skill deletion cancelled"))
      (if (file-symlink-p path)
          (delete-file path)
        (unless (string-prefix-p
                 (file-name-as-directory (file-truename root))
                 (file-name-as-directory (file-truename path)))
          (error "Refusing to delete a skill outside its user directory"))
        (delete-directory path t))
      (magent-skills-reload)
      (message "Permanently deleted Magent skill %s"
               (plist-get item :name)))))

(define-key magent-skill-finder-mode-map (kbd "RET")
            #'magent-skill-manager-preview)
(define-key magent-skill-finder-mode-map (kbd "i")
            #'magent-skill-manager-install-at-point)
(define-key magent-skill-finder-mode-map (kbd "g")
            #'magent-skill-manager-finder-refresh)

(provide 'magent-skill-manager)
;;; magent-skill-manager.el ends here
