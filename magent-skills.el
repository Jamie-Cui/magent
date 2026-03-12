;;; magent-skills.el --- Skill registry for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Skill registry for magent.  Supports two skill types:
;;
;; - `instruction' type: Markdown body is injected into system prompt.
;;   LLM follows instructions and uses available tools directly.
;;   (Claude Code/OpenCode style)
;;
;; - `tool' type: Skill is invoked via `skill_invoke' tool with
;;   predefined operations. (Traditional magent style)

;;; Code:

(require 'cl-lib)

(declare-function magent-log "magent-ui")
(declare-function magent-skill-emacs-invoke "magent-skill-emacs")

(defvar magent-skill-emacs--description nil
  "Description of the built-in emacs skill.
Defined in magent-skill-emacs.el.")

(defvar magent-skill-creator--description nil
  "Description of the built-in skill-creator skill.
Defined in magent-skill-creator.el.")

(defvar magent-skill-creator--prompt nil
  "Prompt for the built-in skill-creator skill.
Defined in magent-skill-creator.el.")

;;; Skill data structure

(cl-defstruct (magent-skill (:constructor magent-skill-create))
  "Represents a magent skill.

Fields:
- NAME: Skill identifier (string)
- DESCRIPTION: Brief description for skill selection
- TYPE: 'instruction or 'tool
- TOOLS: List of required tools (symbols)
- PROMPT: Markdown body for instruction-type skills
- INVOKE-FUNCTION: Function for tool-type skills
- FILE-PATH: Source file path (if loaded from file)"
  name                ; Skill name (string)
  description         ; Brief description
  (type 'instruction) ; 'instruction or 'tool
  (tools nil)         ; List of required tools
  (prompt nil)        ; Markdown body (for instruction type)
  (invoke-function nil) ; Function for tool type
  (file-path nil))    ; Source file path

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
    ;; Remove existing entry with same name (string comparison)
    (setq magent-skills--registry
          (cl-remove-if (lambda (entry) (equal (car entry) name))
                        magent-skills--registry))
    ;; Add new entry
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

;;; Get skill prompts for instruction-type skills

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
  (require 'magent-skill-emacs)
  (let ((skill (magent-skill-create
                :name "emacs"
                :description magent-skill-emacs--description
                :type 'tool
                :tools '(bash)
                :invoke-function #'magent-skill-emacs-invoke)))
    (magent-skills-register skill)
    (magent-log "INFO registered built-in skill: emacs (tool-type)"))
  (require 'magent-skill-creator)
  (let ((skill (magent-skill-create
                :name "skill-creator"
                :description magent-skill-creator--description
                :type 'instruction
                :prompt magent-skill-creator--prompt)))
    (magent-skills-register skill)
    (magent-log "INFO registered built-in skill: skill-creator (instruction-type)")))

;; Auto-register built-in skills when this module is loaded
(magent-skills--register-builtin)

(provide 'magent-skills)
;;; magent-skills.el ends here