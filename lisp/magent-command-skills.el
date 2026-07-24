;;; magent-command-skills.el --- Skill discovery command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6

;;; Commentary:

;; Provider-free /skills command backed by the scope-aware Magent skill
;; catalog.  Frontends may project the same descriptors through other
;; interaction surfaces without coupling skills to the command registry.

;;; Code:

(require 'subr-x)
(require 'magent-command)
(require 'magent-skills)

(defun magent-command-skills--description-string (value)
  "Return skill description VALUE as display text."
  (cond
   ((stringp value) value)
   ((null value) "")
   ((listp value)
    (string-join (mapcar (lambda (item) (format "%s" item)) value)
                 ", "))
   (t (format "%s" value))))

(magent-command-defworkflow magent-command-skills--workflow (invocation)
  "List instruction skills visible to INVOCATION."
  (let ((descriptors
         (magent-skills-list-descriptors
          (magent-command-invocation-scope invocation)
          'instruction)))
    (if descriptors
        (string-join
         (cons
          "Available skills:"
          (mapcar
           (lambda (descriptor)
             (let ((description
                    (magent-command-skills--description-string
                     (magent-skill-descriptor-description descriptor))))
               (if (string-empty-p description)
                   (format "- %s"
                           (magent-skill-descriptor-name descriptor))
                 (format "- %s: %s"
                         (magent-skill-descriptor-name descriptor)
                         description))))
           descriptors))
         "\n")
      "No skills configured.")))

(defun magent-command-skills-register ()
  "Register the reserved Magent skill discovery command."
  (magent-command-register
   "skills"
   :description "List instruction skills available in this session."
   :session-policy 'current
   :workflow #'magent-command-skills--workflow
   :source-layer 'core))

(provide 'magent-command-skills)
;;; magent-command-skills.el ends here
