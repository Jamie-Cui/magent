;;; magent-capability-file.el --- Capability file loader for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Load capability definitions from directories using YAML frontmatter.
;;
;; Governance model:
;; - Maintainer-owned policy fields decide activation behavior:
;;   family, disclosure, and risk.
;; - Contributor-owned metadata fields describe facts Magent can score:
;;   skills, modes, features, files, and keywords.
;; - Future package-local capability metadata is intentionally disabled by
;;   default.  When enabled later, package-provided facts can be projected
;;   into Magent's resolver while maintainer-owned policy stays under
;;   Magent control.

;;; Code:

(require 'cl-lib)
(require 'magent-config)
(require 'magent-capability)
(require 'magent-frontmatter)

(declare-function magent-log "magent-ui")

(defconst magent-capability-file--builtin-dir
  (expand-file-name "capabilities"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing built-in capability files bundled with magent.")

(defcustom magent-capability-directories
  (list (expand-file-name "magent/capabilities" user-emacs-directory))
  "List of directories to scan for capability files.
Each directory can contain subdirectories with CAPABILITY.md files."
  :type '(repeat directory)
  :group 'magent)

(defcustom magent-capability-file-name "CAPABILITY.md"
  "Name of the capability definition file."
  :type 'string
  :group 'magent)

(defcustom magent-enable-package-capability-metadata nil
  "Whether to ingest package-local capability metadata.
This future-facing path is disabled by default.  When nil, Magent only
loads built-in, user, and project capability files that it controls
directly."
  :type 'boolean
  :group 'magent)

(defconst magent-capability-file--maintainer-policy-keys
  '(:family :disclosure :risk)
  "Capability frontmatter keys controlled by Magent maintainers.")

(defconst magent-capability-file--contributor-metadata-keys
  '(:skills :modes :features :feature :files :prompt-keywords :keywords)
  "Capability frontmatter keys contributed as matchable metadata facts.")

(defun magent-capability-file--project-capability-dirs ()
  "Return project-local capability directories."
  (let ((root (magent-project-root)))
    (when root
      (let ((cap-dir (expand-file-name ".magent/capabilities" root)))
        (when (file-directory-p cap-dir)
          (list cap-dir))))))

(defun magent-capability-file--package-metadata-dirs ()
  "Return package-local capability metadata directories.
Currently disabled by default until governance and trust boundaries are
better defined."
  (when magent-enable-package-capability-metadata
    nil))

(defun magent-capability-file--list-directories ()
  "Return directories to scan for capability files."
  (append (list magent-capability-file--builtin-dir)
          magent-capability-directories
          (magent-capability-file--project-capability-dirs)
          (magent-capability-file--package-metadata-dirs)))

(defun magent-capability-file--source-owner (filepath)
  "Classify FILEPATH for capability governance purposes."
  (let ((truename (file-truename filepath)))
    (cl-labels
        ((under-any-dir-p (dirs)
           (cl-some (lambda (dir)
                      (string-prefix-p (file-name-as-directory
                                        (file-truename dir))
                                       truename))
                    (delq nil dirs))))
    (cond
     ((under-any-dir-p (list magent-capability-file--builtin-dir))
      'maintainer)
     ((under-any-dir-p (magent-capability-file--project-capability-dirs))
      'project)
     ((under-any-dir-p magent-capability-directories)
      'user)
     (t 'external-metadata)))))

(defun magent-capability-file--list-files (&optional directories)
  "List all capability files in DIRECTORIES."
  (let ((dirs (or directories (magent-capability-file--list-directories)))
        files)
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (let ((direct-file (expand-file-name magent-capability-file-name dir)))
          (when (file-exists-p direct-file)
            (push direct-file files)))
        (dolist (subdir (directory-files dir t "^[^.]"))
          (when (file-directory-p subdir)
            (let ((cap-file (expand-file-name magent-capability-file-name subdir)))
              (when (file-exists-p cap-file)
                (push cap-file files)))))))
    (sort files #'string<)))

(defun magent-capability-file--parse-source-kind (value)
  "Parse capability source kind VALUE."
  (pcase (if (symbolp value) (symbol-name value) (downcase (format "%s" value)))
    ("package" 'package)
    (_ 'builtin)))

(defun magent-capability-file--normalize-list (value)
  "Normalize VALUE into a flat list of strings."
  (cond
   ((null value) nil)
   ((listp value)
    (apply #'append
           (mapcar #'magent-capability-file--normalize-list value)))
   ((symbolp value)
    (list (symbol-name value)))
   ((stringp value)
    (let* ((trimmed (string-trim value))
           (parts (split-string trimmed "," t "[[:space:]\n]*")))
      (if (> (length parts) 1)
          (mapcar #'string-trim parts)
        (list trimmed))))
   (t (list (string-trim (format "%s" value))))))

(defun magent-capability-file--parse-symbol-list (value)
  "Parse VALUE into a list of symbols."
  (mapcar #'intern (magent-capability-file--normalize-list value)))

(defun magent-capability-file--parse-string-list (value)
  "Parse VALUE into a list of strings."
  (magent-capability-file--normalize-list value))

(defun magent-capability-file--parse-disclosure (value)
  "Parse disclosure VALUE."
  (pcase (if (symbolp value) (symbol-name value) (downcase (format "%s" value)))
    ("hidden" 'hidden)
    ("active" 'active)
    (_ 'suggested)))

(defun magent-capability-file--parse-risk (value)
  "Parse risk VALUE."
  (pcase (if (symbolp value) (symbol-name value) (downcase (format "%s" value)))
    ("medium" 'medium)
    ("high" 'high)
    (_ 'low)))

(defun magent-capability-file--policy-family (frontmatter source-kind source-name owner)
  "Return the policy family for FRONTMATTER under SOURCE-KIND and OWNER."
  (if (eq owner 'external-metadata)
      (or (and source-name (format "%s" source-name))
          (symbol-name source-kind))
    (or (plist-get frontmatter :family)
        (and source-name (format "%s" source-name))
        (symbol-name source-kind))))

(defun magent-capability-file--policy-disclosure (frontmatter owner)
  "Return maintainer-controlled disclosure from FRONTMATTER for OWNER."
  (if (eq owner 'external-metadata)
      'suggested
    (magent-capability-file--parse-disclosure
     (plist-get frontmatter :disclosure))))

(defun magent-capability-file--policy-risk (frontmatter owner)
  "Return maintainer-controlled risk from FRONTMATTER for OWNER."
  (if (eq owner 'external-metadata)
      'low
    (magent-capability-file--parse-risk
     (plist-get frontmatter :risk))))

(defun magent-capability-file-load (filepath)
  "Load a capability definition from FILEPATH."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents filepath)
        (let* ((parsed (magent-frontmatter-parse (buffer-string)))
               (frontmatter (car parsed))
               (body (string-trim (cdr parsed))))
          (when frontmatter
            (let* ((name (or (plist-get frontmatter :name)
                             (file-name-nondirectory
                              (directory-file-name
                               (file-name-directory filepath)))))
                   (source-kind (magent-capability-file--parse-source-kind
                                 (plist-get frontmatter :source)))
                   (source-name (or (plist-get frontmatter :source-name)
                                    (plist-get frontmatter :feature)
                                    (plist-get frontmatter :package)))
                   (owner (magent-capability-file--source-owner filepath))
                   (capability
                    (magent-capability-create
                     :name name
                     :title (or (plist-get frontmatter :title) name)
                     :description (plist-get frontmatter :description)
                     :family (magent-capability-file--policy-family
                              frontmatter source-kind source-name owner)
                     :source-kind source-kind
                     :source-name (when source-name
                                    (format "%s" source-name))
                     :skills (magent-capability-file--parse-string-list
                              (plist-get frontmatter :skills))
                     :modes (magent-capability-file--parse-symbol-list
                             (plist-get frontmatter :modes))
                     :features (magent-capability-file--parse-symbol-list
                                (or (plist-get frontmatter :features)
                                    (plist-get frontmatter :feature)))
                     :files (magent-capability-file--parse-string-list
                             (plist-get frontmatter :files))
                     :prompt-keywords (magent-capability-file--parse-string-list
                                       (or (plist-get frontmatter :prompt-keywords)
                                           (plist-get frontmatter :keywords)))
                     :disclosure (magent-capability-file--policy-disclosure
                                  frontmatter owner)
                     :risk (magent-capability-file--policy-risk
                            frontmatter owner)
                     :notes (unless (string-empty-p body) body)
                     :file-path filepath)))
              (magent-capability-register capability)
              (magent-log "INFO loaded capability: %s owner=%s" name owner)
              capability))))
    (error
     (magent-log "ERROR loading capability file %s: %s"
                 filepath (error-message-string err))
     nil)))

(defun magent-capability-file-load-all (&optional directories)
  "Load all capability files from DIRECTORIES."
  (let ((files (magent-capability-file--list-files directories))
        (count 0))
    (dolist (file files)
      (when (magent-capability-file-load file)
        (cl-incf count)))
    (when (> count 0)
      (magent-log "INFO loaded %d capability file(s)" count))
    count))

(defun magent-capability-file-reload ()
  "Reload all file-backed capabilities."
  (interactive)
  (setq magent-capability--registry
        (cl-remove-if (lambda (entry)
                        (magent-capability-file-path (cdr entry)))
                      magent-capability--registry))
  (magent-capability-file-load-all))

;;;###autoload
(defun magent-reload-capabilities ()
  "Reload capabilities from disk."
  (interactive)
  (magent-capability-file-reload)
  (message "Capabilities reloaded: %s"
           (mapconcat #'identity (magent-capability-list) ", ")))

(provide 'magent-capability-file)
;;; magent-capability-file.el ends here
