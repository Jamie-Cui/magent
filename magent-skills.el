;;; magent-skills.el --- Skill registry for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Skill registry for magent.  Built-in skills (e.g., emacs) are
;; registered directly; no external directory scanning required.

;;; Code:

(require 'cl-lib)
(require 'magent-skill-emacs)

(declare-function magent-log "magent-ui")

;;; Skill data structure

(cl-defstruct magent-skill
  "Represents a magent skill."
  name                ; Skill name (string)
  description)        ; Brief description

;;; Skill registry

(defvar magent-skills--registry nil
  "Alist of (skill-name . magent-skill) for registered skills.")

(defun magent-skills-get (name)
  "Get skill by NAME from registry."
  (cdr (assoc name magent-skills--registry)))

(defun magent-skills-list ()
  "Return list of all registered skill names."
  (mapcar #'car magent-skills--registry))

(defun magent-skills-clear ()
  "Clear skill registry."
  (setq magent-skills--registry nil))

;;; Built-in skill registration

(defun magent-skills-load-all ()
  "Register all built-in skills.
Populates `magent-skills--registry'."
  (interactive)
  (let ((skill (magent-skill-create
                :name "emacs"
                :description magent-skill-emacs--description)))
    (push (cons "emacs" skill) magent-skills--registry)
    (magent-log "INFO registered built-in skill: emacs")))

(provide 'magent-skills)
;;; magent-skills.el ends here
