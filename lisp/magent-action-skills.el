;;; magent-action-skills.el --- Skill discovery command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6

;;; Commentary:

;; Provider-free /skills command backed by the scope-aware Magent skill
;; catalog.  Frontends may project the same descriptors through other
;; interaction surfaces without coupling skills to the Action registry.

;;; Code:

(require 'subr-x)
(require 'magent-action)
(require 'magent-skills)

(defun magent-action-skills--description-string (value)
  "Return skill description VALUE as display text."
  (cond
   ((stringp value) value)
   ((null value) "")
   ((listp value)
    (string-join (mapcar (lambda (item) (format "%s" item)) value)
                 ", "))
   (t (format "%s" value))))

(magent-define-workflow magent-action-skills--workflow (invocation)
  "List instruction skills visible to INVOCATION."
  (let ((descriptors
         (magent-skills-list-descriptors
          (magent-action-invocation-scope invocation)
          'instruction)))
    (if descriptors
        (string-join
         (cons
          "Available skills:"
          (mapcar
           (lambda (descriptor)
             (let ((description
                    (magent-action-skills--description-string
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

(defun magent-action-skills-register ()
  "Register the reserved Magent skill discovery Action."
  (magent-action-register
   "skills"
   :description "List instruction skills available in this session."
   :session-policy 'current
   :workflow #'magent-action-skills--workflow
   :source-layer 'core))

(provide 'magent-action-skills)
;;; magent-action-skills.el ends here
