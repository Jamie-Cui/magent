;;; magent-capability-file.el --- Capability file loader for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Load capability definitions from directories using YAML frontmatter.

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

(defun magent-capability-file--project-capability-dirs ()
  "Return project-local capability directories."
  (let ((root (magent-project-root)))
    (when root
      (let ((cap-dir (expand-file-name ".magent/capabilities" root)))
        (when (file-directory-p cap-dir)
          (list cap-dir))))))

(defun magent-capability-file--list-directories ()
  "Return directories to scan for capability files."
  (append (list magent-capability-file--builtin-dir)
          magent-capability-directories
          (magent-capability-file--project-capability-dirs)))

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

(defun magent-capability-file--parse-symbol-list (value)
  "Parse VALUE into a list of symbols."
  (cond
   ((null value) nil)
   ((symbolp value) (list value))
   ((stringp value) (list (intern (string-trim value))))
   ((listp value)
    (mapcar (lambda (item)
              (if (symbolp item) item (intern (string-trim item))))
            value))
   (t nil)))

(defun magent-capability-file--parse-string-list (value)
  "Parse VALUE into a list of strings."
  (cond
   ((null value) nil)
   ((stringp value) (list (string-trim value)))
   ((symbolp value) (list (symbol-name value)))
   ((listp value)
    (mapcar (lambda (item)
              (string-trim (if (symbolp item) (symbol-name item) item)))
            value))
   (t nil)))

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
                   (source-name (or (plist-get frontmatter :source-name)
                                    (plist-get frontmatter :feature)
                                    (plist-get frontmatter :package)))
                   (capability
                    (magent-capability-create
                     :name name
                     :title (or (plist-get frontmatter :title) name)
                     :description (plist-get frontmatter :description)
                     :source-kind (magent-capability-file--parse-source-kind
                                   (plist-get frontmatter :source))
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
                     :disclosure (magent-capability-file--parse-disclosure
                                  (plist-get frontmatter :disclosure))
                     :risk (magent-capability-file--parse-risk
                            (plist-get frontmatter :risk))
                     :notes (unless (string-empty-p body) body)
                     :file-path filepath)))
              (magent-capability-register capability)
              (magent-log "INFO loaded capability: %s" name)
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
  (dolist (entry magent-capability--registry)
    (when (magent-capability-file-path (cdr entry))
      (magent-capability-unregister (car entry))))
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
