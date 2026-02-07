;;; magent-agent-file.el --- Agent file loader for OpenCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Load custom agents from .opencode/agent/*.md files.
;; Compatible with opencode's agent file format (YAML frontmatter + markdown).

;;; Code:

(require 'cl-lib)
(require 'magent-agent-info)
(require 'magent-agent-registry)
(require 'magent-permission)

;;; File paths

(defun magent-agent-file--project-root ()
  "Get the project root directory for agent files."
  (let ((root (when (bound-and-true-p magent-project-root-function)
                (funcall magent-project-root-function))))
    (or root
        (when (fboundp 'projectile-project-root)
          (projectile-project-root))
        default-directory)))

(defun magent-agent-file--agent-dir (&optional directory)
  "Get the agent directory for DIRECTORY or project root."
  (let* ((root (or directory (magent-agent-file--project-root)))
         (agent-dir (expand-file-name ".opencode/agent" root)))
    agent-dir))

(defun magent-agent-file--list-files (&optional directory)
  "List all agent .md files in DIRECTORY or project root."
  (let* ((agent-dir (magent-agent-file--agent-dir directory))
         (files (when (file-directory-p agent-dir)
                  (directory-files agent-dir t "\\.md$"))))
    (sort files #'string<)))

;;; Frontmatter parsing

(defun magent-agent-file--parse-frontmatter (content)
  "Parse YAML frontmatter from CONTENT.
Returns (FRONTMATTER . BODY) where FRONTMATTER is a plist.
If no frontmatter found, returns (nil . CONTENT)."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((frontmatter nil)
          (body content))
      (when (looking-at-p "^---")
        (forward-line 1)
        (let ((start (point)))
          (when (re-search-forward "^---" nil t)
            (let ((yaml-str (buffer-substring-no-properties start (1- (point)))))
              (setq frontmatter (magent-agent-file--parse-yaml yaml-str))
              (forward-line 1)
              (setq body (buffer-substring-no-properties (point) (point-max)))))))
      (cons frontmatter body))))

(defun magent-agent-file--parse-yaml (yaml-str)
  "Parse simple YAML string to plist.
Supports basic key: value pairs and nested structures."
  (let ((result nil)
        (lines (split-string yaml-str "\n")))
    (dolist (line lines)
      (when (string-match "^\\s-*\\([^:]+\\):\\s-\\(.+\\)$" line)
        (let* ((key (match-string 1 line))
               (value-str (match-string 2 line))
               (value (magent-agent-file--parse-value value-str)))
          (setq result (plist-put result (intern (concat ":" key)) value)))))
    result))

(defun magent-agent-file--parse-value (str)
  "Parse a YAML value string.
Handles booleans, numbers, strings, and lists."
  (setq str (string-trim str))
  (cond
   ;; Boolean
   ((string-equal str "true") t)
   ((string-equal str "false") nil)
   ;; Number
   ((string-match-p "^[0-9]+$" str) (string-to-number str))
   ;; Quoted string
   ((and (> (length str) 1)
         (or (and (eq (aref str 0) ?\") (eq (aref str (1- (length str))) ?\"))
             (and (eq (aref str 0) ?') (eq (aref str (1- (length str))) ?'))))
    (substring str 1 -1))
   ;; List (comma-separated)
   ((string-match-p "," str)
    (mapcar #'magent-agent-file--parse-value
            (split-string str "," t "[\s,]+")))
   ;; Default: return as string
   (t str)))

(defun magent-agent-file--parse-mode (mode-str)
  "Parse mode string MODE-STR to symbol.
Returns 'primary, 'subagent, or 'all (default)."
  (pcase (downcase mode-str)
    ("primary" 'primary)
    ("subagent" 'subagent)
    ("all" 'all)
    (_ 'all)))

(defun magent-agent-file--parse-permission (tools-config)
  "Parse tools config to permission rules.
TOOLS-CONFIG is a plist like (:bash t :read nil)."
  (let ((rules nil)
        (all-tools '(bash read write edit list glob grep webfetch task todowrite todoread)))
    ;; Start with all tools allowed
    (dolist (tool all-tools)
      (push (cons tool 'allow) rules))
    ;; Apply restrictions from config
    (when (plistp tools-config)
      (dolist (key (plist-keys tools-config))
        (let ((value (plist-get tools-config key))
              (tool-name (intern (downcase (substring (symbol-name key) 1)))))
          ;; If tool is explicitly disabled, deny it
          (when (eq value nil)
            (setq rules (assq-delete-all tool-name rules))
            (push (cons tool-name 'deny) rules)))))
    (nreverse rules)))

;;; Loading agents from files

(defun magent-agent-file-load (filepath)
  "Load an agent from FILEPATH.
Returns the agent info if successful, nil otherwise."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents filepath)
        (let* ((content (buffer-string))
               (parsed (magent-agent-file--parse-frontmatter content))
               (frontmatter (car parsed))
               (body (cdr parsed))
               (name (file-name-base filepath)))
          (when frontmatter
            (let* ((mode-str (plist-get frontmatter :mode))
                   (tools-config (plist-get frontmatter :tools))
                   (permission (when tools-config
                                 (magent-agent-file--parse-permission tools-config)))
                   (agent-info (magent-agent-info-create
                                :name name
                                :description (plist-get frontmatter :description)
                                :mode (magent-agent-file--parse-mode mode-str)
                                :native nil
                                :hidden (plist-get frontmatter :hidden)
                                :temperature (plist-get frontmatter :temperature)
                                :top-p (plist-get frontmatter :top-p)
                                :color (plist-get frontmatter :color)
                                :prompt (when (> (length body) 0) body)
                                :permission permission)))
              (when (magent-agent-info-valid-p agent-info)
                (magent-agent-registry-register agent-info)
                agent-info)))))
    (error
     (message "Error loading agent file %s: %s" filepath (error-message-string err))
     nil)))

(defun magent-agent-file-load-all (&optional directory)
  "Load all agent files from DIRECTORY or project root.
Returns number of agents loaded."
  (let ((files (magent-agent-file--list-files directory))
        (count 0))
    (dolist (file files)
      (when (magent-agent-file-load file)
        (cl-incf count)))
    (when (> count 0)
      (message "Loaded %d agent file(s) from %s"
               count
               (magent-agent-file--agent-dir directory)))
    count))

;;; Saving agents to files

(defun magent-agent-file-save (agent-info &optional directory)
  "Save AGENT-INFO to a .md file in DIRECTORY or project root.
Returns the filepath if successful."
  (let* ((name (magent-agent-info-name agent-info))
         (agent-dir (magent-agent-file--agent-dir directory))
         (filepath (expand-file-name (concat name ".md") agent-dir)))
    (make-directory agent-dir t)
    (with-temp-file filepath
      (insert "---\n")
      ;; Write frontmatter
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
        (insert (format "top_p: %s\n"
                        (magent-agent-info-top-p agent-info))))
      (when (magent-agent-info-color agent-info)
        (insert (format "color: %s\n"
                        (magent-agent-info-color agent-info))))
      (insert "---\n")
      (insert "\n")
      ;; Write prompt
      (when (magent-agent-info-prompt agent-info)
        (insert (magent-agent-info-prompt agent-info))
        (insert "\n")))
    filepath))

;;; Interactive functions

;;;###autoload
(defun magent-load-agent-files (&optional directory)
  "Load all agent files from DIRECTORY or project root."
  (interactive)
  (magent-agent-file-load-all directory))

;;;###autoload
(defun magent-save-agent (agent-name &optional directory)
  "Save agent named AGENT-NAME to a file."
  (interactive
   (list (completing-read "Save agent: "
                          (magent-agent-registry-list-names t))))
  (let ((agent-info (magent-agent-registry-get agent-name)))
    (if agent-info
        (let ((filepath (magent-agent-file-save agent-info directory)))
          (message "Agent saved to: %s" filepath))
      (error "Agent not found: %s" agent-name))))

(provide 'magent-agent-file)
;;; magent-agent-file.el ends here
