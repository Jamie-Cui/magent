;;; magent-agent-file.el --- File-backed agent loading for Magent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Loader and saver for custom agent definitions stored in
;; `.magent/agent/*.md' files.

;;; Code:

(require 'cl-lib)
(require 'magent-agent-registry)
(require 'magent-config)
(require 'magent-file-loader)

(declare-function magent-log "magent-ui")

(defvar magent-tools--permission-keys)

(defun magent-agent-file--agent-dir (&optional directory)
  "Get the agent directory for DIRECTORY or project root."
  (expand-file-name magent-agent-directory
                    (or directory (magent-project-root))))

(defun magent-agent-file--scope-for-file (filepath)
  "Return the project scope owning FILEPATH, or nil when not project-local."
  (magent-file-loader-project-root-for-file filepath magent-agent-directory))

(defun magent-agent-file--list-files (&optional directory)
  "List all agent .md files in DIRECTORY or project root."
  (magent-file-loader-list-matching-files
   (magent-agent-file--agent-dir directory)
   "\\.md$"))

(defun magent-agent-file--parse-mode (mode-str)
  "Parse mode string MODE-STR to symbol.
Returns \\='primary, \\='subagent, or \\='all (default)."
  (pcase (downcase mode-str)
    ("primary" 'primary)
    ("subagent" 'subagent)
    ("all" 'all)
    (_ 'all)))

(defun magent-agent-file--parse-permission (tools-config)
  "Parse tools config to permission rules.
TOOLS-CONFIG is a plist like (:bash t :read nil)."
  (let ((rules nil)
        (all-tools magent-tools--permission-keys))
    (dolist (tool all-tools)
      (push (cons tool 'allow) rules))
    (when (and tools-config (plistp tools-config))
      (cl-loop for (key value) on tools-config by #'cddr
               do (let ((tool-name (intern (downcase (substring (symbol-name key) 1)))))
                    (when (eq value nil)
                      (setq rules (assq-delete-all tool-name rules))
                      (push (cons tool-name 'deny) rules)))))
    (nreverse rules)))

(defun magent-agent-file-load (filepath)
  "Load an agent from FILEPATH.
Returns the agent info if successful, nil otherwise."
  (condition-case err
      (let* ((definition (magent-file-loader-read-definition filepath))
             (frontmatter (plist-get definition :frontmatter))
             (body (plist-get definition :body))
             (name (file-name-base filepath))
             (source-scope (magent-agent-file--scope-for-file filepath)))
        (when frontmatter
          (let* ((mode-str (plist-get frontmatter :mode))
                 (tools-config (plist-get frontmatter :tools))
                 (permission (when tools-config
                               (magent-agent-file--parse-permission tools-config)))
                 (agent-info (magent-agent-info-create
                              :name name
                              :description (plist-get frontmatter :description)
                              :mode (if mode-str (magent-agent-file--parse-mode mode-str) 'all)
                              :native nil
                              :hidden (plist-get frontmatter :hidden)
                              :temperature (plist-get frontmatter :temperature)
                              :top-p (plist-get frontmatter :top-p)
                              :color (plist-get frontmatter :color)
                              :prompt (when (> (length body) 0) body)
                              :permission permission
                              :file-path filepath
                              :source-layer (if source-scope 'project 'builtin)
                              :source-scope source-scope)))
            (when (magent-agent-info-valid-p agent-info)
              (magent-agent-registry-register agent-info)
              agent-info))))
    (error
     (magent-log "ERROR loading agent file %s: %s" filepath
                 (error-message-string err))
     nil)))

(defun magent-agent-file-load-all (&optional directory)
  "Load all agent files from DIRECTORY or project root.
Returns number of agents loaded."
  (let* ((files (magent-agent-file--list-files directory))
         (count (magent-file-loader-load-all files #'magent-agent-file-load)))
    (when (> count 0)
      (magent-log "INFO loaded %d agent file(s) from %s"
                  count
                  (magent-agent-file--agent-dir directory)))
    count))

(defun magent-agent-file-load-project-scope (scope)
  "Load project-local agents for SCOPE."
  (magent-agent-file-load-all scope))

(defun magent-agent-file-save (agent-info &optional directory)
  "Save AGENT-INFO to a .md file in DIRECTORY or project root.
Returns the filepath if successful."
  (let* ((name (magent-agent-info-name agent-info))
         (agent-dir (magent-agent-file--agent-dir directory))
         (filepath (expand-file-name (concat name ".md") agent-dir)))
    (make-directory agent-dir t)
    (with-temp-file filepath
      (insert "---\n")
      (when (magent-agent-info-description agent-info)
        (insert (format "description: %s\n"
                        (magent-agent-info-description agent-info))))
      (when (magent-agent-info-mode agent-info)
        (insert (format "mode: %s\n"
                        (magent-agent-info-mode agent-info))))
      (when (magent-agent-info-hidden agent-info)
        (insert "hidden: true\n"))
      (when (magent-agent-info-temperature agent-info)
        (insert (format "temperature: %s\n"
                        (magent-agent-info-temperature agent-info))))
      (when (magent-agent-info-top-p agent-info)
        (insert (format "top-p: %s\n"
                        (magent-agent-info-top-p agent-info))))
      (when (magent-agent-info-color agent-info)
        (insert (format "color: %s\n"
                        (magent-agent-info-color agent-info))))
      (insert "---\n\n")
      (when (magent-agent-info-prompt agent-info)
        (insert (magent-agent-info-prompt agent-info))
        (insert "\n")))
    filepath))

(provide 'magent-agent-file)
;;; magent-agent-file.el ends here
