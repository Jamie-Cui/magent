;;; magent-skills.el --- Skill registry and loading for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Central registry and file loader for instruction skills.  Markdown bodies
;; are injected into the system prompt and use the request's exact tool set.
;; Executable extensions register commands or tools in trusted Elisp instead
;; of hiding dispatch behind a generic skill gateway.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magent-config)
(require 'magent-file-loader)
(require 'magent-log)
(require 'magent-runtime)

(declare-function magent-session-scope-origin "magent-session")

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
type: instruction
tools: [bash, read_file] # optional: exact provider tool names this skill needs
requires-project: true   # optional: reject the skill in global sessions
capability: true         # optional: auto-activate this instruction skill by context
---

Markdown body: instructions for the AI...
```

## Skill Behavior

The Markdown body is injected into the system prompt when the skill is active.
Use for workflow guidance, coding standards, domain knowledge.
Keep under 200 lines to avoid bloating the system prompt.
Agent-shell exposes every available instruction skill as `/$name`.
A trusted installed Magent extension that needs Elisp execution or a
multi-step workflow should register a first-class action; project-local
Markdown remains data-only.

**capability metadata**: Instruction skills can also declare `capability: true`
plus fields such as `modes`, `features`, `files`, `prompt-keywords`,
`disclosure`, and `risk`.  List-valued fields use YAML sequences.  Magent uses
those fields to auto-activate the skill when the current
buffer or prompt matches the context.  Keep this metadata on the skill when the
capability only exists to activate that same skill.

## Skill Locations

- User global: `magent/skills/<name>/SKILL.md` under `user-emacs-directory`
- Project-local: `.magent/skills/<name>/SKILL.md`

Place each skill in its own subdirectory named after the skill.

---

## Creating a Skill

### 1. Capture Intent

Ask:
- What should the skill enable magent to do?
- When should it be used? (what user phrases, what context)

### 2. Write the Skill

Key principles:
- **description**: This is the primary trigger mechanism. Be specific about *when*
  to use this skill vs. handling the task directly. Include example phrasings.
  Lean toward being slightly \"pushy\" - Claude tends to undertrigger skills.
- **body**: Use imperative form. Explain the *why* behind steps, not just the *what*.
  Keep skills concise. If one grows large, split it into sections
  with references to separate files.

Write the SKILL.md using `write_file`:
```
`magent/skills/<name>/SKILL.md` under `user-emacs-directory`
```

### 3. Test in Emacs

After writing, reload skills:

```elisp
(magent-reload-skills)
```

Use the `emacs_eval` tool to reload without leaving the chat. Then test: start
a new session or prompt magent with a request that
should trigger the skill. Check `M-x magent-list-skills` to confirm registration.

### 4. Iterate

- If the AI ignores the skill: strengthen the description, add trigger phrases
- If instructions are misunderstood: clarify the why, add examples
- If the skill is too verbose: trim sections that aren't pulling their weight

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
  (requires-project nil)
  (file-path nil)
  (source-layer 'builtin)
  source-scope)

(cl-defstruct (magent-skill-descriptor
               (:constructor magent-skill-descriptor-create)
               (:copier nil))
  "Frontend-neutral metadata for one effective Magent skill."
  name
  description
  type
  tools
  requires-project
  source-layer
  source-scope)

;;; Skill registry

(defvar magent-skills--registry nil
  "Layered alist of (skill-name . magent-skill) definitions.
For duplicate names the first entry is effective; lower-layer entries are
kept so unloading a project overlay restores the previous definition.")

(defvar magent-skills--scope-catalog (make-hash-table :test #'equal)
  "Effective skill snapshots keyed by canonical project scope.
The nil key stores the global snapshot.  Project snapshots are retained while
their overlays are inactive so live frontend sessions remain scope-correct.")

(defun magent-skills--same-owner-p (left right)
  "Return non-nil when skills LEFT and RIGHT belong to the same layer."
  (and (eq (magent-skill-source-layer left)
           (magent-skill-source-layer right))
       (equal (magent-skill-source-scope left)
              (magent-skill-source-scope right))))

(defun magent-skills--effective-entries ()
  "Return effective skill registry entries without shadowed duplicates."
  (let (seen effective)
    (dolist (entry magent-skills--registry)
      (unless (member (car entry) seen)
        (push (car entry) seen)
        (push entry effective)))
    (nreverse effective)))

(defun magent-skills--canonical-scope (scope)
  "Return canonical project origin for SCOPE, or nil for global scope."
  (let ((origin (magent-session-scope-origin scope)))
    (cond
     ((or (null origin) (eq origin 'global)) nil)
     ((stringp origin)
      (condition-case nil
          (file-truename (directory-file-name origin))
        (error (directory-file-name (expand-file-name origin)))))
     (t origin))))

(defun magent-skills--resolution-scope (&optional scope)
  "Return canonical catalog scope for optional SCOPE."
  (magent-skills--canonical-scope
   (or scope
       (magent-runtime-active-project-scope)
       'global)))

(defun magent-skills--visible-in-scope-p (skill scope)
  "Return non-nil when SKILL is visible in canonical SCOPE."
  (let ((source-scope
         (magent-skills--canonical-scope
          (magent-skill-source-scope skill))))
    (or (null source-scope)
        (equal source-scope scope))))

(defun magent-skills--registry-skills-for-scope (scope)
  "Return effective registry skills visible in canonical SCOPE."
  (let (seen skills)
    (dolist (entry magent-skills--registry)
      (let ((name (car entry))
            (skill (cdr entry)))
        (when (and (not (member name seen))
                   (magent-skills--visible-in-scope-p skill scope))
          (push name seen)
          (push skill skills))))
    (sort skills
          (lambda (left right)
            (string< (magent-skill-name left)
                     (magent-skill-name right))))))

(defun magent-skills--rebase-project-catalogs ()
  "Rebase cached project catalogs on the current global snapshot."
  (let ((global-skills
         (copy-sequence
          (gethash nil magent-skills--scope-catalog))))
    (maphash
     (lambda (scope skills)
       (when scope
         (let* ((project-skills
                 (cl-remove-if-not
                  (lambda (skill)
                    (equal
                     scope
                     (magent-skills--canonical-scope
                      (magent-skill-source-scope skill))))
                  skills))
                (project-names
                 (mapcar #'magent-skill-name project-skills))
                (rebased
                 (append
                  project-skills
                  (cl-remove-if
                   (lambda (skill)
                     (member (magent-skill-name skill) project-names))
                   global-skills))))
           (puthash
            scope
            (sort rebased
                  (lambda (left right)
                    (string< (magent-skill-name left)
                             (magent-skill-name right))))
            magent-skills--scope-catalog))))
     magent-skills--scope-catalog)))

(defun magent-skills--record-scope-catalog (&optional scope)
  "Record effective skills for SCOPE from the active registry."
  (let ((key (magent-skills--resolution-scope scope)))
    (puthash key
             (magent-skills--registry-skills-for-scope key)
             magent-skills--scope-catalog)
    (when (null key)
      (magent-skills--rebase-project-catalogs))))

(defun magent-skills--descriptor (skill)
  "Return frontend-neutral descriptor for SKILL."
  (magent-skill-descriptor-create
   :name (magent-skill-name skill)
   :description (magent-skill-description skill)
   :type (magent-skill-type skill)
   :tools (copy-sequence (magent-skill-tools skill))
   :requires-project (magent-skill-requires-project skill)
   :source-layer (magent-skill-source-layer skill)
   :source-scope (magent-skill-source-scope skill)))

(defun magent-skills-list-descriptors (&optional scope type)
  "Return effective skill descriptors for SCOPE, optionally limited to TYPE.
When SCOPE is nil, use the currently active project overlay.  Results are
sorted by skill name and do not expose prompt bodies or executable handlers."
  (let* ((key (magent-skills--resolution-scope scope))
         (active-key
          (magent-skills--canonical-scope
           (or (magent-runtime-active-project-scope) 'global)))
         (skills
          (if (equal key active-key)
              (magent-skills--registry-skills-for-scope key)
            (let ((cached (gethash key magent-skills--scope-catalog
                                   'magent-skills--missing)))
              (if (eq cached 'magent-skills--missing)
                  (copy-sequence
                   (gethash nil magent-skills--scope-catalog))
                (copy-sequence cached))))))
    (setq skills
          (cl-remove-if
           (lambda (skill)
             (and (null key)
                  (magent-skill-requires-project skill)))
           skills))
    (mapcar
     #'magent-skills--descriptor
     (if type
         (cl-remove-if-not
          (lambda (skill) (eq (magent-skill-type skill) type))
          skills)
       skills))))

(defun magent-skills-resolve-descriptor (name &optional scope)
  "Return effective skill descriptor NAME for SCOPE, or nil."
  (cl-find name (magent-skills-list-descriptors scope)
           :key #'magent-skill-descriptor-name
           :test #'equal))

(defun magent-skills-get (name)
  "Get skill by NAME from registry."
  (cdr (assoc name magent-skills--registry)))

(defun magent-skills-list ()
  "Return list of all registered skill names."
  (mapcar #'car (magent-skills--effective-entries)))

(defun magent-skills-list-by-type (type)
  "Return list of skill names of TYPE."
  (delq nil
        (mapcar (lambda (entry)
                  (when (eq (magent-skill-type (cdr entry)) type)
                    (car entry)))
                (magent-skills--effective-entries))))

(defun magent-skills-dedupe-names (names)
  "Return string NAMES without duplicates, preserving order."
  (let (seen result)
    (dolist (name names (nreverse result))
      (when (and (stringp name) (not (member name seen)))
        (push name seen)
        (push name result)))))

(defun magent-skills-missing-tools (skill-name available-tools)
  "Return SKILL-NAME's declared tools absent from AVAILABLE-TOOLS.
Tool names may be strings or symbols.  An unknown skill has no requirements."
  (when-let* ((skill (magent-skills-get skill-name)))
    (let ((available
           (mapcar (lambda (tool)
                     (if (symbolp tool) tool (intern (format "%s" tool))))
                   available-tools)))
      (cl-remove-if (lambda (tool) (memq tool available))
                    (magent-skill-tools skill)))))

(defun magent-skills-tool-requirements-satisfied-p
    (skill-name available-tools)
  "Return non-nil when AVAILABLE-TOOLS includes all tools SKILL-NAME declares."
  (null (magent-skills-missing-tools skill-name available-tools)))

(defun magent-skills-register (skill)
  "Register SKILL in the registry.
If a skill with the same name and owner exists, it will be replaced.
Shadowed lower-layer definitions remain registered for later restoration."
  (unless (eq (magent-skill-type skill) 'instruction)
    (error "Unsupported skill type %S; Magent skills are instruction-only"
           (magent-skill-type skill)))
  (let ((name (magent-skill-name skill)))
    (setq magent-skills--registry
          (cl-remove-if (lambda (entry)
                          (and (equal (car entry) name)
                               (magent-skills--same-owner-p
                                (cdr entry) skill)))
                        magent-skills--registry))
    (push (cons name skill) magent-skills--registry)
    (magent-skills--record-scope-catalog
     (or (magent-skill-source-scope skill) 'global)))
  skill)

(defun magent-skills-unregister (name)
  "Remove skill NAME from registry."
  (setq magent-skills--registry
        (cl-remove-if (lambda (entry) (equal (car entry) name))
                      magent-skills--registry))
  (maphash
   (lambda (scope skills)
     (puthash scope
              (cl-remove name skills
                         :key #'magent-skill-name
                         :test #'equal)
              magent-skills--scope-catalog))
   magent-skills--scope-catalog)
  (magent-skills--record-scope-catalog 'global))

(defun magent-skills-clear ()
  "Clear all skills from registry."
  (setq magent-skills--registry nil)
  (clrhash magent-skills--scope-catalog))

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
                           (magent-skills--effective-entries))))))
    (delq nil
          (mapcar (lambda (skill)
                    (when-let* ((prompt (magent-skill-prompt skill))
                                ((> (length prompt) 0)))
                      (format "## Skill: %s\n\n%s%s"
                              (magent-skill-name skill)
                              (if-let* ((file-path (magent-skill-file-path skill)))
                                  (format "Skill directory: %s\n\n"
                                          (file-name-as-directory
                                           (expand-file-name
                                            (file-name-directory file-path))))
                                "")
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
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; In the git repo sources are under lisp/ and skills/ is at the
    ;; root (one level up); after MELPA install lisp/magent*.el is flattened
    ;; to the top level.  Try sibling first, then parent.
    (or (let ((d (expand-file-name "skills" dir)))
          (and (file-directory-p d) d))
        (let ((d (expand-file-name "skills" (expand-file-name ".." dir))))
          (and (file-directory-p d) d))
        (expand-file-name "skills" dir)))
  "Directory containing built-in skills bundled with magent.")

(defcustom magent-skill-directories
  (list (expand-file-name "magent/skills" user-emacs-directory))
  "List of directories to scan for skill files.
Each directory can contain subdirectories with SKILL.md files.
Later directories take precedence over earlier directories when skill
names collide.  The final entry is the canonical installation target."
  :type '(repeat directory)
  :group 'magent)

(defcustom magent-skill-file-name "SKILL.md"
  "Name of the skill definition file."
  :type 'string
  :group 'magent)

(defconst magent-skills--frontmatter-keys
  '(:name :description :type :tools :requires-project
    :capability :title :family :source :source-name :capability-skills
    :modes :features :files :prompt-keywords :disclosure :risk)
  "Supported SKILL.md frontmatter keys.")

(defun magent-skills--validate-frontmatter (frontmatter)
  "Reject unsupported or incomplete skill FRONTMATTER."
  (cl-loop for (key _value) on frontmatter by #'cddr
           unless (memq key magent-skills--frontmatter-keys)
           do (error "Unsupported skill frontmatter key: %s" key))
  (dolist (key '(:name :description :type))
    (unless (plist-member frontmatter key)
      (error "Skill frontmatter is missing required key: %s" key)))
  frontmatter)

(defun magent-skills-definition-directories (&optional scope)
  "Return skill definition directories for static loading and SCOPE.
When SCOPE is non-nil, include that project's `.magent/skills'
directory if it exists."
  (append (list magent-skills--builtin-dir)
          magent-skill-directories
          (when scope
            (magent-file-loader-project-subdir-for-scope
             ".magent/skills" scope))))

(defun magent-skills-classify-source (filepath)
  "Return a plist describing the source classification for FILEPATH."
  (magent-file-loader-classify-source
   filepath
   :builtin-dirs (list magent-skills--builtin-dir)
   :user-dirs magent-skill-directories
   :project-relative-dir ".magent/skills"
   :default-layer 'external))

(defun magent-skills--classify-source (filepath)
  "Return a plist describing the source classification for FILEPATH."
  (magent-skills-classify-source filepath))

(defun magent-skills--list-files (&optional directories)
  "List all SKILL.md files in DIRECTORIES or `magent-skill-directories'."
  (let ((ordered-directories
         (or directories
             (append (list magent-skills--builtin-dir)
                     magent-skill-directories
                     (magent-file-loader-project-subdir ".magent/skills")))))
    (magent-file-loader-list-named-files-ordered
     ordered-directories magent-skill-file-name)))

(defun magent-skills--parse-type (type-str)
  "Validate TYPE-STR and return the sole supported skill type."
  (unless (equal (downcase (format "%s" type-str)) "instruction")
    (error "Unsupported skill type %S; Magent skills are instruction-only"
           type-str))
  'instruction)

(defun magent-skills--parse-tools (tools-spec)
  "Parse TOOLS-SPEC to list of tool symbols.
TOOLS-SPEC must be a YAML sequence when present."
  (cond
   ((null tools-spec) nil)
   ((listp tools-spec)
    (mapcar (lambda (tool)
              (cond
               ((stringp tool) (intern tool))
               ((symbolp tool) tool)
               (t (error "Invalid skill tool name: %S" tool))))
            tools-spec))
   (t (error "Skill tools must be a YAML sequence"))))

(defun magent-skills-load-file (filepath)
  "Load a skill from FILEPATH.
Returns the skill if successful, nil otherwise."
  (condition-case err
      (let* ((definition (magent-file-loader-read-definition filepath))
             (frontmatter (plist-get definition :frontmatter))
             (body (plist-get definition :body))
             (source (magent-skills--classify-source filepath)))
        (when frontmatter
          (magent-skills--validate-frontmatter frontmatter)
          (let* ((name (or (plist-get frontmatter :name)
                           (file-name-nondirectory
                            (directory-file-name
                             (file-name-directory filepath)))))
                 (description (plist-get frontmatter :description))
                 (type (magent-skills--parse-type
                        (or (plist-get frontmatter :type) "instruction")))
                 (tools (magent-skills--parse-tools
                         (plist-get frontmatter :tools)))
                 (skill (magent-skill-create
                         :name name
                         :description description
                         :type type
                         :tools tools
                         :prompt (when (> (length body) 0) body)
                         :requires-project
                         (eq (plist-get frontmatter :requires-project) t)
                         :file-path filepath
                         :source-layer (plist-get source :layer)
                         :source-scope (plist-get source :scope))))
            (when (magent-skill-name skill)
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
  (prog1
      (magent-skills-load-all (magent-skills-definition-directories))
    (magent-skills--record-scope-catalog 'global)))

(defun magent-skills-load-project-scope (scope)
  "Load project-local skill definitions for SCOPE."
  (let ((count
         (if-let* ((directories
                    (magent-file-loader-project-subdir-for-scope
                     ".magent/skills" scope)))
             (magent-skills-load-all directories)
           0)))
    (magent-skills--record-scope-catalog scope)
    count))

(defun magent-skills-reload ()
  "Reload all skills from files.
When a project overlay is currently active, restore that project's
local skills after static definitions are reloaded."
  (let ((project-scope (magent-runtime-active-project-scope)))
    (magent-file-loader-reload-file-backed-registry
     'magent-skills--registry
     #'magent-skill-file-path
     #'magent-skills-initialize-static)
    (when project-scope
      (magent-skills-load-project-scope project-scope))
    (magent-skills--record-scope-catalog 'global)
    (when project-scope
      (magent-skills--record-scope-catalog project-scope))))

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
  (magent-runtime-prepare-context)
  (let ((skills (mapcar #'cdr (magent-skills--effective-entries))))
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
  (magent-runtime-prepare-context)
  (magent-skills-reload)
  (message "Skills reloaded: %s" (mapconcat #'identity (magent-skills-list) ", ")))

;;;###autoload
(defun magent-describe-skill (skill-name)
  "Show detailed information about SKILL-NAME."
  (interactive
   (progn
     (magent-runtime-prepare-context)
     (list (completing-read "Describe skill: " (magent-skills-list) nil t))))
  (magent-runtime-prepare-context)
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
