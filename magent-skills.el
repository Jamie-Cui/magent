;;; magent-skills.el --- Skill registry and loading for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Central skills module for magent.  Supports two skill types:
;;
;; - `instruction' type: Markdown body is injected into the system prompt.
;;   LLM follows instructions and uses available tools directly.
;;
;; - `tool' type: Skill is invoked via `skill_invoke' tool with
;;   predefined operations.
;;
;; This file also contains file-backed skill loading and the built-in
;; `skill-creator' instruction skill.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magent-config)
(require 'magent-file-loader)
(require 'magent-runtime)

(declare-function magent-log "magent-ui")

;;; Built-in skill metadata

(defconst magent-skill-creator--description
  "Create new magent skills, modify and improve existing skills. Use when users want to create a skill from scratch, edit an existing skill, or improve a skill's description. Guides through writing SKILL.md, placing it in the right directory, testing via emacs_eval, and iterating."
  "Description of the built-in skill-creator skill.")

(defconst magent-skill-creator--prompt
  "# Skill Creator

A skill for creating new magent skills and iteratively improving them.

Your job: figure out where the user is in the process and help them progress
through these stages: understand intent -> write SKILL.md -> test in Emacs -> iterate.

---

## Skill File Format

```
---
name: skill-name
description: When to trigger and what this skill does
type: instruction        # 'instruction' or 'tool'
tools: bash, read        # optional: tools this skill needs
---

Markdown body: instructions for the AI...
```

## Skill Types

**instruction**: Markdown body is injected into the system prompt every request.
Use for workflow guidance, coding standards, domain knowledge.
Keep under 200 lines to avoid bloating the system prompt.

**tool**: Invoked explicitly via `skill_invoke` with named operations.
Requires a companion `.el` file defining `magent-skill-<name>-invoke`.
Use for structured operations with discrete inputs/outputs.

## Skill Locations

- User global: `~/.emacs.d/magent-skills/<name>/SKILL.md`
- Project-local: `.magent/skills/<name>/SKILL.md`

Place each skill in its own subdirectory named after the skill.

---

## Creating a Skill

### 1. Capture Intent

Ask:
- What should the skill enable magent to do?
- When should it be used? (what user phrases, what context)
- What type fits: instruction (passive guidance) or tool (active operations)?

### 2. Write the Skill

Key principles:
- **description**: This is the primary trigger mechanism. Be specific about *when*
  to use this skill vs. handling the task directly. Include example phrasings.
  Lean toward being slightly \"pushy\" - Claude tends to undertrigger skills.
- **body**: Use imperative form. Explain the *why* behind steps, not just the *what*.
  Keep instruction-type skills concise. If it grows large, split into sections
  with references to separate files.

Write the SKILL.md using `write_file`:
```
~/.emacs.d/magent-skills/<name>/SKILL.md
```

### 3. Test in Emacs

After writing, reload skills:

```elisp
(magent-reload-skills)
```

Use the `emacs_eval` tool to reload without leaving the chat. Then test: start
a new session or prompt magent with a request that
should trigger the skill. Check `M-x magent-list-skills` to confirm registration.

For tool-type skills, test each operation:
```elisp
(magent-skills-invoke \"skill-name\" \"operation\" (list \"arg\")
                      (lambda (r) (message \"%s\" r)))
```

### 4. Iterate

- If the AI ignores the skill: strengthen the description, add trigger phrases
- If instructions are misunderstood: clarify the why, add examples
- If the skill is too verbose: trim sections that aren't pulling their weight
- If tool operations misbehave: check the companion `.el` file

---

## Tool-type Skill Companion File

For tool-type skills, create `.magent/skills/<name>/<name>.el`:

```elisp
;;; -*- lexical-binding: t; -*-
(defun magent-skill-<name>-invoke (operation args callback)
  \"Invoke <name> skill OPERATION with ARGS.
CALLBACK is called with result string.\"
  (run-at-time
   0 nil
   (lambda ()
     (funcall callback
              (pcase operation
                (\"op-name\" (format \"result: %s\" (car args)))
                (_ (format \"Unknown operation: %s\" operation)))))))
```

The companion file is loaded automatically when the skill file is loaded.

---

## Description Writing Guide

Good description (specific, mentions trigger contexts):
> Create new magent skills or modify existing ones. Use when user says
> \"create a skill\", \"add a skill for X\", \"write a SKILL.md\", or asks
> how to extend magent with custom behavior.

Poor description (too generic):
> Helps with skill creation.

The description field is the only thing Claude sees when deciding whether to
use a skill - make it count."
  "Prompt for the built-in skill-creator skill.")

;;; Skill data structure

(cl-defstruct (magent-skill (:constructor magent-skill-create))
  "Represents a magent skill."
  name
  description
  (type 'instruction)
  (tools nil)
  (prompt nil)
  (invoke-function nil)
  (file-path nil)
  (source-layer 'builtin)
  source-scope)

;;; Skill registry

(defvar magent-skills--registry nil
  "Alist of (skill-name . magent-skill) for registered skills.")

(defun magent-skills-get (name)
  "Get skill by NAME from registry."
  (cdr (assoc name magent-skills--registry)))

(defun magent-skills-list ()
  "Return list of all registered skill names."
  (mapcar #'car magent-skills--registry))

(defun magent-skills-list-by-type (type)
  "Return list of skill names of TYPE."
  (delq nil
        (mapcar (lambda (entry)
                  (when (eq (magent-skill-type (cdr entry)) type)
                    (car entry)))
                magent-skills--registry)))

(defun magent-skills-register (skill)
  "Register SKILL in the registry.
If a skill with the same name exists, it will be replaced."
  (let ((name (magent-skill-name skill)))
    (setq magent-skills--registry
          (cl-remove-if (lambda (entry) (equal (car entry) name))
                        magent-skills--registry))
    (push (cons name skill) magent-skills--registry))
  skill)

(defun magent-skills-unregister (name)
  "Remove skill NAME from registry."
  (setq magent-skills--registry
        (cl-remove-if (lambda (entry) (equal (car entry) name))
                      magent-skills--registry)))

(defun magent-skills-clear ()
  "Clear all skills from registry."
  (setq magent-skills--registry nil))

;;; Skill invocation

(defun magent-skills-invoke (skill-name operation args callback)
  "Invoke SKILL-NAME with OPERATION and ARGS asynchronously.
CALLBACK is called with the result.
Only works for tool-type skills."
  (let ((skill (magent-skills-get skill-name)))
    (cond
     ((null skill)
      (funcall callback
               (format "Error: skill '%s' not found. Available skills: %s"
                       skill-name (mapconcat #'identity (magent-skills-list) ", "))))
     ((eq (magent-skill-type skill) 'instruction)
      (funcall callback
               (format "Error: skill '%s' is instruction-type, not invokable via tool"
                       skill-name)))
     ((null (magent-skill-invoke-function skill))
      (funcall callback
               (format "Error: skill '%s' has no invoke function" skill-name)))
     (t
      (condition-case err
          (funcall (magent-skill-invoke-function skill) operation args callback)
        (error (funcall callback
                        (format "Error invoking skill '%s': %s"
                                skill-name (error-message-string err)))))))))

;;; Instruction skill prompts

(defun magent-skills-get-instruction-prompts (&optional skill-names)
  "Get combined prompts from instruction-type skills.
If SKILL-NAMES is nil, return all instruction-type skill prompts.
If SKILL-NAMES is a list, only include those skills."
  (let ((skills (if skill-names
                    (delq nil (mapcar #'magent-skills-get skill-names))
                  (mapcar #'cdr
                          (cl-remove-if-not
                           (lambda (entry)
                             (eq (magent-skill-type (cdr entry)) 'instruction))
                           magent-skills--registry)))))
    (delq nil
          (mapcar (lambda (skill)
                    (when-let* ((prompt (magent-skill-prompt skill))
                                ((> (length prompt) 0)))
                      (format "## Skill: %s\n\n%s"
                              (magent-skill-name skill)
                              prompt)))
                  skills))))

;;; Built-in skill registration

(defun magent-skills--register-builtin ()
  "Register built-in skills."
  (let ((skill (magent-skill-create
                :name "skill-creator"
                :description magent-skill-creator--description
                :type 'instruction
                :prompt magent-skill-creator--prompt
                :source-layer 'builtin)))
    (magent-skills-register skill)
    (magent-log "INFO registered built-in skill: skill-creator (instruction-type)")
    skill))

;;; File-backed skills

(defconst magent-skills--builtin-dir
  (expand-file-name "skills" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing built-in skills bundled with magent.")

(defcustom magent-skill-directories
  (let ((new-dir (expand-file-name "magent/skills" user-emacs-directory))
        (old-dir (expand-file-name "magent-skills" user-emacs-directory)))
    (if (file-directory-p old-dir)
        (list new-dir old-dir)
      (list new-dir)))
  "List of directories to scan for skill files.
Each directory can contain subdirectories with SKILL.md files."
  :type '(repeat directory)
  :group 'magent)

(defcustom magent-skill-file-name "SKILL.md"
  "Name of the skill definition file."
  :type 'string
  :group 'magent)

(defun magent-skills--classify-source (filepath)
  "Return a plist describing the source classification for FILEPATH."
  (magent-file-loader-classify-source
   filepath
   :builtin-dirs (list magent-skills--builtin-dir)
   :user-dirs magent-skill-directories
   :project-relative-dir ".magent/skills"))

(defun magent-skills--list-files (&optional directories)
  "List all SKILL.md files in DIRECTORIES or `magent-skill-directories'."
  (if directories
      (magent-file-loader-list-named-files directories magent-skill-file-name)
    (magent-file-loader-list-definition-files
     magent-skill-file-name
     :builtin-dirs (list magent-skills--builtin-dir)
     :user-dirs magent-skill-directories
     :project-relative-dir ".magent/skills")))

(defun magent-skills--parse-type (type-str)
  "Parse type string TYPE-STR to a skill type symbol."
  (pcase (downcase type-str)
    ("tool" 'tool)
    ("instruction" 'instruction)
    (_ 'instruction)))

(defun magent-skills--parse-tools (tools-spec)
  "Parse TOOLS-SPEC to list of tool symbols.
TOOLS-SPEC can be a string, symbol, or list."
  (cond
   ((null tools-spec) nil)
   ((stringp tools-spec)
    (if (string-match-p "," tools-spec)
        (mapcar (lambda (tool) (intern (string-trim tool)))
                (split-string tools-spec "," t))
      (list (intern tools-spec))))
   ((symbolp tools-spec) (list tools-spec))
   ((listp tools-spec)
    (mapcar (lambda (tool)
              (if (stringp tool) (intern tool) tool))
            tools-spec))
   (t nil)))

(defun magent-skills--find-companion-file (skill-file)
  "Find companion implementation file for SKILL-FILE."
  (let* ((dir (file-name-directory skill-file))
         (basename (file-name-sans-extension
                    (file-name-nondirectory skill-file)))
         (sibling-el (expand-file-name (concat basename ".el") dir))
         (skill-name (file-name-nondirectory
                      (directory-file-name dir)))
         (named-el (expand-file-name (concat skill-name ".el") dir)))
    (cond
     ((file-exists-p sibling-el) sibling-el)
     ((file-exists-p named-el) named-el)
     (t nil))))

(defun magent-skills--load-companion (skill-file skill-name)
  "Load companion implementation file for SKILL-FILE.
SKILL-NAME is used to resolve the invoke function."
  (when-let* ((el-file (magent-skills--find-companion-file skill-file)))
    (magent-log "INFO loading companion file: %s" el-file)
    (load-file el-file)
    (let ((invoke-fn (intern (format "magent-skill-%s-invoke" skill-name))))
      (when (fboundp invoke-fn)
        invoke-fn))))

(defun magent-skills-load-file (filepath)
  "Load a skill from FILEPATH.
Returns the skill if successful, nil otherwise."
  (condition-case err
      (let* ((definition (magent-file-loader-read-definition filepath))
             (frontmatter (plist-get definition :frontmatter))
             (body (plist-get definition :body))
             (source (magent-skills--classify-source filepath)))
        (when frontmatter
          (let* ((name (or (plist-get frontmatter :name)
                           (file-name-nondirectory
                            (directory-file-name
                             (file-name-directory filepath)))))
                 (description (plist-get frontmatter :description))
                 (type (magent-skills--parse-type
                        (or (plist-get frontmatter :type) "instruction")))
                 (tools (magent-skills--parse-tools
                         (plist-get frontmatter :tools)))
                 (invoke-fn (when (eq type 'tool)
                              (magent-skills--load-companion filepath name)))
                 (skill (magent-skill-create
                         :name name
                         :description description
                         :type type
                         :tools tools
                         :prompt (when (> (length body) 0) body)
                         :invoke-function invoke-fn
                         :file-path filepath
                         :source-layer (plist-get source :layer)
                         :source-scope (plist-get source :scope))))
            (when (magent-skill-name skill)
              (when (and (eq type 'tool) (not invoke-fn))
                (magent-log "WARN tool-type skill '%s' has no companion .el file or invoke function"
                            name))
              (magent-skills-register skill)
              (magent-log "INFO loaded skill: %s (%s)" name type)
              skill))))
    (error
     (magent-log "ERROR loading skill file %s: %s"
                 filepath (error-message-string err))
     nil)))

(defun magent-skills-load-all (&optional directories)
  "Load all skill files from DIRECTORIES or `magent-skill-directories'.
Returns number of skills loaded."
  (let* ((files (magent-skills--list-files directories))
         (count (magent-file-loader-load-all files #'magent-skills-load-file)))
    (when (> count 0)
      (magent-log "INFO loaded %d skill file(s)" count))
    count))

(defun magent-skills-initialize-static ()
  "Load built-in and user-global skill definitions."
  (magent-skills--register-builtin)
  (magent-skills-load-all
   (append (list magent-skills--builtin-dir)
           magent-skill-directories)))

(defun magent-skills-load-project-scope (scope)
  "Load project-local skill definitions for SCOPE."
  (magent-skills-load-all
   (magent-file-loader-project-subdir-for-scope ".magent/skills" scope)))

(defun magent-skills-reload ()
  "Reload all skills from files.
When a project overlay is currently active, restore that project's
local skills after static definitions are reloaded."
  (interactive)
  (let ((project-scope
         (or (when (called-interactively-p 'interactive)
               (magent-runtime-prepare-command-context)
               (magent-runtime-active-project-scope))
             (magent-runtime-active-project-scope))))
    (magent-file-loader-reload-file-backed-registry
     'magent-skills--registry
     #'magent-skill-file-path
     #'magent-skills-initialize-static)
    (when project-scope
      (magent-skills-load-project-scope project-scope))))

(defun magent-skills-remove-project-scope (scope)
  "Remove project-local skills registered for SCOPE."
  (setq magent-skills--registry
        (magent-file-loader-remove-project-scope-entries
         magent-skills--registry
         #'magent-skill-source-layer
         #'magent-skill-source-scope
         scope)))

;;; Interactive commands

;;;###autoload
(defun magent-list-skills ()
  "Display a list of all registered skills."
  (interactive)
  (magent-runtime-prepare-command-context)
  (let ((skills (mapcar #'cdr magent-skills--registry)))
    (magent--with-display-buffer "*Magent Skills*"
      (insert "Available Skills:\n\n")
      (dolist (skill (sort skills
                           (lambda (a b)
                             (string< (magent-skill-name a)
                                      (magent-skill-name b)))))
        (insert (format "- %s [%s]\n"
                        (magent-skill-name skill)
                        (magent-skill-type skill)))
        (when (magent-skill-description skill)
          (insert (format "  %s\n" (magent-skill-description skill))))
        (when (magent-skill-tools skill)
          (insert (format "  Tools: %s\n"
                          (mapconcat #'symbol-name
                                     (magent-skill-tools skill) ", "))))
        (when (magent-skill-file-path skill)
          (insert (format "  File: %s\n" (magent-skill-file-path skill))))
        (insert "\n"))
      (insert (format "Total: %d skill(s)\n" (length skills))))))

;;;###autoload
(defun magent-reload-skills ()
  "Reload all skills from files.
This clears file-based skills and reloads them from disk.
Built-in skills are preserved."
  (interactive)
  (magent-runtime-prepare-command-context)
  (magent-skills-reload)
  (message "Skills reloaded: %s" (mapconcat #'identity (magent-skills-list) ", ")))

;;;###autoload
(defun magent-describe-skill (skill-name)
  "Show detailed information about SKILL-NAME."
  (interactive
   (progn
     (magent-runtime-prepare-command-context)
     (list (completing-read "Describe skill: " (magent-skills-list) nil t))))
  (magent-runtime-prepare-command-context)
  (let ((skill (magent-skills-get skill-name)))
    (if (not skill)
        (message "Skill '%s' not found" skill-name)
      (magent--with-display-buffer (format "*Magent Skill: %s*" skill-name)
        (insert (format "# Skill: %s\n\n" skill-name))
        (insert (format "Type: %s\n" (magent-skill-type skill)))
        (when (magent-skill-description skill)
          (insert (format "\n## Description\n\n%s\n" (magent-skill-description skill))))
        (when (magent-skill-tools skill)
          (insert (format "\n## Required Tools\n\n%s\n"
                          (mapconcat #'symbol-name (magent-skill-tools skill) ", "))))
        (when (magent-skill-prompt skill)
          (insert (format "\n## Prompt\n\n%s\n" (magent-skill-prompt skill))))
        (when (magent-skill-file-path skill)
          (insert (format "\n## Source\n\n%s\n" (magent-skill-file-path skill))))))))

(provide 'magent-skills)
;;; magent-skills.el ends here
