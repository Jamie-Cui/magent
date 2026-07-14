;;; magent-agent-file.el --- File-backed agent loading for Magent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Loader and saver for custom agent definitions stored in
;; `.magent/agent/*.md' files.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'magent-agent-registry)
(require 'magent-config)
(require 'magent-file-loader)
(require 'magent-log)
(require 'magent-permission)

(defconst magent-agent-file--tool-permission-aliases
  '((read_file . read)
    (write_file . write)
    (write_repo_summary . write)
    (edit_file . edit)
    (spawn_agent . agent)
    (send_agent_message . agent)
    (wait_agent . agent)
    (list_agents . agent)
    (close_agent . agent)
    (skill_invoke . skill))
  "Tool-name aliases accepted in legacy custom agent permissions.")

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

(defun magent-agent-file--mapping-p (value)
  "Return non-nil when VALUE is a keyword plist or an alist."
  (or (and (listp value)
           (let ((tail value)
                 (valid t))
             (while (and tail valid)
               (if (and (keywordp (car tail)) (consp (cdr tail)))
                   (setq tail (cddr tail))
                 (setq valid nil)))
             (and valid (null tail))))
      (and (listp value)
           value
           (cl-every #'consp value))))

(defun magent-agent-file--mapping-entries (value)
  "Return mapping VALUE as an ordered alist."
  (cond
   ((and (listp value) (keywordp (car value)))
    (let (entries)
      (while value
        (push (cons (pop value) (pop value)) entries))
      (nreverse entries)))
   ((and (listp value) (cl-every #'consp value)) value)
   (t nil)))

(defun magent-agent-file--key-name (key)
  "Return normalized string name for mapping KEY."
  (let ((name (cond
               ((keywordp key) (substring (symbol-name key) 1))
               ((symbolp key) (symbol-name key))
               ((stringp key) key)
               (t nil))))
    (and name (downcase (subst-char-in-string ?- ?_ name)))))

(defun magent-agent-file--permission-key (key)
  "Return canonical permission key for KEY, or signal an error."
  (let* ((name (magent-agent-file--key-name key))
         (symbol (and name (intern name)))
         (canonical (or (cdr (assq symbol
                                   magent-agent-file--tool-permission-aliases))
                        symbol)))
    (unless (memq canonical (cons '* magent-permission-keys))
      (error "Unknown agent permission key: %S" key))
    canonical))

(defun magent-agent-file--permission-action (value)
  "Return normalized permission action for VALUE, or signal an error."
  (let ((action (cond
                 ((memq value '(allow deny ask)) value)
                 ((stringp value) (intern (downcase value)))
                 (t nil))))
    (unless (memq action '(allow deny ask))
      (error "Invalid agent permission action: %S" value))
    action))

(defun magent-agent-file--legacy-permission-entry (value)
  "Parse one legacy README-style permission VALUE."
  (unless (and (stringp value)
               (string-match
                "\\`[[:space:]]*(\\([^[:space:].()]+\\)[[:space:]]*\\.[[:space:]]*\\(allow\\|deny\\|ask\\))[[:space:]]*\\'"
                value))
    (error "Invalid legacy agent permission entry: %S" value))
  (cons (magent-agent-file--permission-key (match-string 1 value))
        (magent-agent-file--permission-action (match-string 2 value))))

(defun magent-agent-file--parse-permission-rule (value)
  "Parse one permission rule VALUE."
  (if (magent-agent-file--mapping-p value)
      (mapcar
       (lambda (entry)
         (let ((pattern (cond
                         ((keywordp (car entry))
                          (substring (symbol-name (car entry)) 1))
                         ((symbolp (car entry)) (symbol-name (car entry)))
                         ((stringp (car entry)) (car entry))
                         (t (error "Invalid permission file pattern: %S"
                                   (car entry))))))
           (cons pattern
                 (magent-agent-file--permission-action (cdr entry)))))
       (magent-agent-file--mapping-entries value))
    (magent-agent-file--permission-action value)))

(defun magent-agent-file--parse-permissions (permission-config)
  "Parse canonical or legacy PERMISSION-CONFIG into permission rules.

The canonical format is a YAML mapping from permission groups to
`allow', `deny', `ask', or a nested file-pattern mapping.  The legacy
README list form, such as \='(read_file . allow), is also accepted.
Invalid explicit permission data signals an error instead of falling
back to the permission system's default-allow behavior."
  (cond
   ((magent-agent-file--mapping-p permission-config)
    (let ((entries (magent-agent-file--mapping-entries permission-config)))
      (unless entries
        (error "Agent permissions mapping is empty"))
      (let (seen rules)
        (dolist (entry entries)
          (let ((key (magent-agent-file--permission-key (car entry))))
            (when (memq key seen)
              (error "Duplicate normalized agent permission key: %s" key))
            (push key seen)
            (push (cons key
                        (magent-agent-file--parse-permission-rule (cdr entry)))
                  rules)))
        (nreverse rules))))
   ((and (listp permission-config) permission-config)
    (mapcar #'magent-agent-file--legacy-permission-entry permission-config))
   (t
   (error "Agent permissions must be a non-empty mapping or list"))))

(defun magent-agent-file--parse-permission (permission-config)
  "Compatibility parser for PERMISSION-CONFIG.
Boolean mappings retain the old `tools' semantics; all other values use the
strict canonical permissions parser."
  (if (and (magent-agent-file--mapping-p permission-config)
           (cl-every (lambda (entry) (memq (cdr entry) '(t nil)))
                     (magent-agent-file--mapping-entries permission-config)))
      (magent-agent-file--parse-tools permission-config)
    (magent-agent-file--parse-permissions permission-config)))

(defun magent-agent-file--parse-tools (tools-config)
  "Parse legacy TOOLS-CONFIG into a permission profile.
Boolean mappings override the historical allow-all profile.  A string,
symbol, or list is treated as an explicit allowlist."
  (cond
   ((magent-agent-file--mapping-p tools-config)
    (unless (magent-agent-file--mapping-entries tools-config)
      (error "Legacy agent tools mapping is empty"))
    (let ((rules (mapcar (lambda (key) (cons key 'allow))
                         magent-permission-keys)))
      (dolist (entry (magent-agent-file--mapping-entries tools-config))
        (unless (memq (cdr entry) '(t nil))
          (error "Legacy agent tool value must be boolean: %S" (cdr entry)))
        (let ((key (magent-agent-file--permission-key (car entry))))
          (setf (alist-get key rules) (if (cdr entry) 'allow 'deny))))
      rules))
   ((or (stringp tools-config) (symbolp tools-config)
        (and (listp tools-config) tools-config))
    (let* ((values (if (listp tools-config)
                       tools-config
                     (list tools-config)))
           (keys (mapcar #'magent-agent-file--permission-key values)))
      (cons (cons '* 'deny)
            (mapcar (lambda (key) (cons key 'allow))
                    (delete-dups keys)))))
   (t
    (error "Legacy agent tools must be a boolean mapping or allowlist"))))

(defun magent-agent-file--parse-model (value)
  "Parse model frontmatter VALUE."
  (cond
   ((null value) nil)
   ((symbolp value) value)
   ((stringp value) (intern value))
   (t (error "Agent model must be a string or symbol: %S" value))))

(defun magent-agent-file--parse-options (value)
  "Parse agent options mapping VALUE into an alist."
  (when value
    (unless (magent-agent-file--mapping-p value)
      (error "Agent options must be a mapping"))
    (mapcar (lambda (entry)
              (cons (intern (or (magent-agent-file--key-name (car entry))
                                (error "Invalid agent option key: %S"
                                       (car entry))))
                    (cdr entry)))
            (magent-agent-file--mapping-entries value))))

(defun magent-agent-file--frontmatter-effort (frontmatter)
  "Return normalized effort option from agent FRONTMATTER."
  (magent-effort-normalize-option
   (or (plist-get frontmatter :effort)
       (plist-get frontmatter :reasoning-effort)
       (plist-get frontmatter :model-reasoning-effort))))

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
                 (permission
                  (cond
                   ((plist-member frontmatter :permissions)
                    (magent-agent-file--parse-permissions
                     (plist-get frontmatter :permissions)))
                   ((plist-member frontmatter :permission)
                    (magent-agent-file--parse-permissions
                     (plist-get frontmatter :permission)))
                   ((plist-member frontmatter :tools)
                    (magent-agent-file--parse-tools
                     (plist-get frontmatter :tools)))))
                 (agent-info (magent-agent-info-create
                              :name name
                              :description (plist-get frontmatter :description)
                              :mode (if mode-str (magent-agent-file--parse-mode mode-str) 'all)
                              :native nil
                              :hidden (plist-get frontmatter :hidden)
                              :temperature (plist-get frontmatter :temperature)
                              :top-p (plist-get frontmatter :top-p)
                              :effort (magent-agent-file--frontmatter-effort
                                       frontmatter)
                              :color (plist-get frontmatter :color)
                              :model (magent-agent-file--parse-model
                                      (plist-get frontmatter :model))
                              :prompt (when (> (length body) 0) body)
                              :options (magent-agent-file--parse-options
                                        (plist-get frontmatter :options))
                              :steps (plist-get frontmatter :steps)
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

(defun magent-agent-file--yaml-string (value)
  "Return VALUE as a quoted YAML string."
  (json-encode-string (format "%s" value)))

(defun magent-agent-file--yaml-key (value)
  "Return VALUE as a YAML mapping key."
  (let ((name (cond
               ((keywordp value) (substring (symbol-name value) 1))
               ((symbolp value) (symbol-name value))
               (t (format "%s" value)))))
    (if (string-match-p "\\`[[:alnum:]_-]+\\'" name)
        name
      (magent-agent-file--yaml-string name))))

(defun magent-agent-file--yaml-scalar (value)
  "Return scalar VALUE encoded for YAML frontmatter."
  (cond
   ((eq value t) "true")
   ((null value) "false")
   ((numberp value) (format "%s" value))
   ((symbolp value) (magent-agent-file--yaml-string (symbol-name value)))
   ((stringp value) (magent-agent-file--yaml-string value))
   ((listp value) (json-encode value))
   (t (magent-agent-file--yaml-string (format "%s" value)))))

(defun magent-agent-file--insert-permissions (permission)
  "Insert canonical YAML for PERMISSION into the current buffer."
  (when permission
    (insert "permissions:\n")
    (dolist (entry permission)
      (insert "  " (magent-agent-file--yaml-key (car entry)) ":")
      (if (and (listp (cdr entry))
               (cl-every #'consp (cdr entry)))
          (progn
            (insert "\n")
            (dolist (file-rule (cdr entry))
              (insert "    " (magent-agent-file--yaml-key (car file-rule))
                      ": "
                      (magent-agent-file--yaml-scalar (cdr file-rule))
                      "\n")))
        (insert " " (magent-agent-file--yaml-scalar (cdr entry)) "\n")))))

(defun magent-agent-file--insert-options (options)
  "Insert canonical YAML for OPTIONS into the current buffer."
  (when options
    (insert "options:\n")
    (dolist (entry options)
      (insert "  " (magent-agent-file--yaml-key (car entry)) ": "
              (magent-agent-file--yaml-scalar (cdr entry)) "\n"))))

(defun magent-agent-file--serializable-model (model)
  "Return the model id from MODEL for custom agent frontmatter."
  (cond
   ((null model) nil)
   ((symbolp model) model)
   ((stringp model) model)
   ((and (consp model) (symbolp (cdr model))) (cdr model))
   (t (error "Agent model is not file-serializable: %S" model))))

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
        (insert "description: "
                (magent-agent-file--yaml-scalar
                 (magent-agent-info-description agent-info))
                "\n"))
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
      (when (magent-agent-info-effort agent-info)
        (insert (format "effort: %s\n"
                        (magent-effort-option-string
                         (magent-agent-info-effort agent-info)))))
      (when (magent-agent-info-color agent-info)
        (insert "color: "
                (magent-agent-file--yaml-scalar
                 (magent-agent-info-color agent-info))
                "\n"))
      (when-let* ((model (magent-agent-file--serializable-model
                          (magent-agent-info-model agent-info))))
        (insert "model: " (magent-agent-file--yaml-scalar model) "\n"))
      (when (magent-agent-info-steps agent-info)
        (insert "steps: "
                (magent-agent-file--yaml-scalar
                 (magent-agent-info-steps agent-info))
                "\n"))
      (magent-agent-file--insert-options
       (magent-agent-info-options agent-info))
      (magent-agent-file--insert-permissions
       (magent-agent-info-permission agent-info))
      (insert "---\n\n")
      (when (magent-agent-info-prompt agent-info)
        (insert (magent-agent-info-prompt agent-info))
        (insert "\n")))
    filepath))

(provide 'magent-agent-file)
;;; magent-agent-file.el ends here
