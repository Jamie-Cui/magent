;;; magent-skill-file.el --- Skill file loader for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Load skills from directories.  Supports Claude Code/OpenCode style
;; skill files with YAML frontmatter + markdown body.
;;
;; Skill file format:
;;
;;   ---
;;   name: skill-name
;;   description: Brief description
;;   tools: read, write, bash
;;   type: instruction
;;   ---
;;
;;   # Skill Instructions
;;
;;   The markdown body becomes part of the system prompt.

;;; Code:

(require 'cl-lib)
(require 'magent-config)
(require 'magent-skills)
(require 'magent-yaml)

(declare-function magent-log "magent-ui")

;;; Customization

(defcustom magent-skill-directories
  (list (expand-file-name "magent-skills" user-emacs-directory))
  "List of directories to scan for skill files.
Each directory can contain subdirectories with SKILL.md files."
  :type '(repeat directory)
  :group 'magent)

(defcustom magent-skill-file-name "SKILL.md"
  "Name of the skill definition file."
  :type 'string
  :group 'magent)

;;; File discovery

(defun magent-skill-file--list-directories ()
  "Get list of skill directories to scan."
  (append magent-skill-directories
          (magent-skill-file--project-skill-dirs)))

(defun magent-skill-file--project-skill-dirs ()
  "Get project-local skill directories."
  (let ((root (when (bound-and-true-p magent-project-root-function)
                (funcall magent-project-root-function))))
    (when root
      (let ((project-skill-dir (expand-file-name ".magent/skills" root)))
        (when (file-directory-p project-skill-dir)
          (list project-skill-dir))))))

(defun magent-skill-file--list-files (&optional directories)
  "List all SKILL.md files in DIRECTORIES or `magent-skill-directories'."
  (let ((dirs (or directories (magent-skill-file--list-directories)))
        (files nil))
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (let ((direct-skill (expand-file-name magent-skill-file-name dir)))
          (when (file-exists-p direct-skill)
            (push direct-skill files)))
        (dolist (subdir (directory-files dir t "^[^.]"))
          (when (file-directory-p subdir)
            (let ((skill-file (expand-file-name magent-skill-file-name subdir)))
              (when (file-exists-p skill-file)
                (push skill-file files)))))))
    (sort files #'string<)))

(defun magent-skill-file--parse-type (type-str)
  "Parse type string TYPE-STR to symbol.
Returns 'instruction or 'tool (default: 'instruction)."
  (pcase (downcase type-str)
    ("tool" 'tool)
    ("instruction" 'instruction)
    (_ 'instruction)))

(defun magent-skill-file--parse-tools (tools-spec)
  "Parse TOOLS-SPEC to list of tool symbols.
TOOLS-SPEC can be a string, symbol, or list."
  (cond
   ((null tools-spec) nil)
   ((stringp tools-spec)
    (if (string-match-p "," tools-spec)
        (mapcar (lambda (s) (intern (string-trim s)))
                (split-string tools-spec "," t))
      (list (intern tools-spec))))
   ((symbolp tools-spec) (list tools-spec))
   ((listp tools-spec)
    (mapcar (lambda (s) (if (stringp s) (intern s) s)) tools-spec))
   (t nil)))

;;; Loading skills from files

(defun magent-skill-file--find-companion-file (skill-file)
  "Find companion implementation file for SKILL-FILE.
Looks for .el file with same basename in the same directory.
Returns the file path if found, nil otherwise."
  (let* ((dir (file-name-directory skill-file))
         (basename (file-name-sans-extension
                    (file-name-nondirectory skill-file)))
         ;; Try SKILL.el in same directory
         (sibling-el (expand-file-name (concat basename ".el") dir))
         ;; Also try <skill-name>.el where skill-name is from directory
         (parent-dir (file-name-directory (directory-file-name dir)))
         (skill-name (file-name-nondirectory
                      (directory-file-name dir)))
         (named-el (expand-file-name (concat skill-name ".el") dir)))
    (cond
     ((file-exists-p sibling-el) sibling-el)
     ((file-exists-p named-el) named-el)
     (t nil))))

(defun magent-skill-file--load-companion (skill-file skill-name)
  "Load companion implementation file for SKILL-FILE.
SKILL-NAME is used to find the invoke function.
Returns the invoke function if found, nil otherwise."
  (when-let* ((el-file (magent-skill-file--find-companion-file skill-file)))
    (magent-log "INFO loading companion file: %s" el-file)
    (load-file el-file)
    ;; Look for invoke function: magent-skill-<name>-invoke
    (let ((invoke-fn (intern (format "magent-skill-%s-invoke" skill-name))))
      (when (fboundp invoke-fn)
        invoke-fn))))

(defun magent-skill-file-load (filepath)
  "Load a skill from FILEPATH.
Returns the skill if successful, nil otherwise.
For tool-type skills, also loads companion .el file if present."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents filepath)
        (let* ((content (buffer-string))
               (parsed (magent-yaml-parse-frontmatter content))
               (frontmatter (car parsed))
               (body (cdr parsed)))
          (when frontmatter
            (let* ((name (or (plist-get frontmatter :name)
                             (file-name-nondirectory
                              (directory-file-name
                               (file-name-directory filepath)))))
                   (description (plist-get frontmatter :description))
                   (type (magent-skill-file--parse-type
                          (or (plist-get frontmatter :type) "instruction")))
                   (tools (magent-skill-file--parse-tools
                           (plist-get frontmatter :tools)))
                   ;; Load companion file for tool-type skills
                   (invoke-fn (when (eq type 'tool)
                                (magent-skill-file--load-companion
                                 filepath name)))
                   (skill (magent-skill-create
                           :name name
                           :description description
                           :type type
                           :tools tools
                           :prompt (when (> (length body) 0) body)
                           :invoke-function invoke-fn
                           :file-path filepath)))
              (when (magent-skill-name skill)
                ;; Warn if tool-type skill has no invoke function
                (when (and (eq type 'tool) (not invoke-fn))
                  (magent-log "WARN tool-type skill '%s' has no companion .el file or invoke function"
                              name))
                (magent-skills-register skill)
                (magent-log "INFO loaded skill: %s (%s)" name type)
                skill)))))
    (error
     (magent-log "ERROR loading skill file %s: %s"
                 filepath (error-message-string err))
     nil)))

(defun magent-skill-file-load-all (&optional directories)
  "Load all skill files from DIRECTORIES or `magent-skill-directories'.
Returns number of skills loaded."
  (let ((files (magent-skill-file--list-files directories))
        (count 0))
    (dolist (file files)
      (when (magent-skill-file-load file)
        (cl-incf count)))
    (when (> count 0)
      (magent-log "INFO loaded %d skill file(s)" count))
    count))

;;; Reloading skills

(defun magent-skill-file-reload ()
  "Reload all skills from files."
  (interactive)
  ;; Clear file-based skills (keep built-in)
  (dolist (entry magent-skills--registry)
    (when (magent-skill-file-path (cdr entry))
      (magent-skills-unregister (car entry))))
  ;; Reload from files
  (magent-skill-file-load-all))

;;; Interactive functions

;;;###autoload
(defun magent-list-skills ()
  "Display a list of all registered skills."
  (interactive)
  (let ((skills (mapcar #'cdr magent-skills--registry)))
    (with-output-to-temp-buffer "*Magent Skills*"
      (princ "Available Skills:\n\n")
      (dolist (skill (sort skills
                           (lambda (a b)
                             (string< (magent-skill-name a)
                                      (magent-skill-name b)))))
        (princ (format "- %s [%s]\n"
                       (magent-skill-name skill)
                       (magent-skill-type skill)))
        (when (magent-skill-description skill)
          (princ (format "  %s\n" (magent-skill-description skill))))
        (when (magent-skill-tools skill)
          (princ (format "  Tools: %s\n"
                         (mapconcat #'symbol-name
                                    (magent-skill-tools skill) ", "))))
        (when (magent-skill-file-path skill)
          (princ (format "  File: %s\n" (magent-skill-file-path skill))))
        (princ "\n"))
      (princ (format "Total: %d skill(s)\n" (length skills))))))

;;;###autoload
(defun magent-reload-skills ()
  "Reload all skills from files.
This clears file-based skills and reloads them from disk.
Built-in skills are preserved."
  (interactive)
  (magent-skill-file-reload)
  (message "Skills reloaded: %s" (mapconcat #'identity (magent-skills-list) ", ")))

;;;###autoload
(defun magent-describe-skill (skill-name)
  "Show detailed information about SKILL-NAME."
  (interactive
   (list (completing-read "Describe skill: " (magent-skills-list) nil t)))
  (let ((skill (magent-skills-get skill-name)))
    (if (not skill)
        (message "Skill '%s' not found" skill-name)
      (with-output-to-temp-buffer (format "*Magent Skill: %s*" skill-name)
        (princ (format "# Skill: %s\n\n" skill-name))
        (princ (format "Type: %s\n" (magent-skill-type skill)))
        (when (magent-skill-description skill)
          (princ (format "\n## Description\n\n%s\n" (magent-skill-description skill))))
        (when (magent-skill-tools skill)
          (princ (format "\n## Required Tools\n\n%s\n"
                         (mapconcat #'symbol-name (magent-skill-tools skill) ", "))))
        (when (magent-skill-prompt skill)
          (princ (format "\n## Prompt\n\n%s\n" (magent-skill-prompt skill))))
        (when (magent-skill-file-path skill)
          (princ (format "\n## Source\n\n%s\n" (magent-skill-file-path skill))))))))

(provide 'magent-skill-file)
;;; magent-skill-file.el ends here