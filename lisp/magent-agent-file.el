;;; magent-agent-file.el --- File-backed agent loading for Magent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

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

(defconst magent-agent-file--frontmatter-keys
  '(:description :mode :hidden :temperature :top-p :effort :color :model
    :permissions)
  "Supported custom-agent frontmatter keys.")

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
  (pcase (downcase (format "%s" mode-str))
    ("primary" 'primary)
    ("subagent" 'subagent)
    ("all" 'all)
    (_ (error "Invalid agent mode: %S" mode-str))))

(defun magent-agent-file--validate-frontmatter (frontmatter)
  "Reject unsupported keys in agent FRONTMATTER."
  (cl-loop for (key _value) on frontmatter by #'cddr
           unless (memq key magent-agent-file--frontmatter-keys)
           do (error "Unsupported agent frontmatter key: %s" key))
  frontmatter)

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
    (and name (downcase name))))

(defun magent-agent-file--permission-key (key)
  "Return canonical permission key for KEY, or signal an error."
  (let* ((name (magent-agent-file--key-name key))
         (canonical (and name (intern name))))
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
  "Parse canonical PERMISSION-CONFIG into permission rules."
  (unless (magent-agent-file--mapping-p permission-config)
    (error "Agent permissions must be a non-empty mapping"))
  (let ((entries (magent-agent-file--mapping-entries permission-config)))
    (unless entries
      (error "Agent permissions mapping is empty"))
    (let (seen rules)
      (dolist (entry entries)
        (let ((key (magent-agent-file--permission-key (car entry))))
          (when (memq key seen)
            (error "Duplicate agent permission key: %s" key))
          (push key seen)
          (push (cons key
                      (magent-agent-file--parse-permission-rule (cdr entry)))
                rules)))
      (nreverse rules))))

(defun magent-agent-file--parse-model (value)
  "Parse model frontmatter VALUE."
  (cond
   ((null value) nil)
   ((symbolp value) value)
   ((stringp value) (intern value))
   (t (error "Agent model must be a string or symbol: %S" value))))

(defun magent-agent-file--frontmatter-effort (frontmatter)
  "Return normalized effort option from agent FRONTMATTER."
  (magent-effort-normalize-option (plist-get frontmatter :effort)))

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
          (magent-agent-file--validate-frontmatter frontmatter)
          (let* ((mode-str (plist-get frontmatter :mode))
                 (permission
                  (when (plist-member frontmatter :permissions)
                    (magent-agent-file--parse-permissions
                     (plist-get frontmatter :permissions))))
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

(defun magent-agent-file--serializable-model (model)
  "Return the model id from MODEL for custom agent frontmatter."
  (cond
   ((null model) nil)
   ((symbolp model) model)
   ((stringp model) model)
   ((and (consp model) (symbolp (cdr model))) (cdr model))
   (t (error "Agent model is not file-serializable: %S" model))))

(defun magent-agent-file-load-project-scope (scope)
  "Reload project-local agents for SCOPE."
  (magent-agent-registry-remove-project-scope scope)
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
      (magent-agent-file--insert-permissions
       (magent-agent-info-permission agent-info))
      (insert "---\n\n")
      (when (magent-agent-info-prompt agent-info)
        (insert (magent-agent-info-prompt agent-info))
        (insert "\n")))
    filepath))

(provide 'magent-agent-file)
;;; magent-agent-file.el ends here
