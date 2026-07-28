;;; magent-action-builtins.el --- Bundled Magent actions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-action)
(require 'magent-action-controls)
(require 'magent-action-skills)
(require 'magent-doctor)
(require 'magent-memory)
(require 'magent-prompt)

(defconst magent-action-builtins--prompt-actions
  '((:name "explain"
     :title "Explain"
     :description
     "Explain the current code, diff, buffer, error, or project context."
     :prompt "actions/explain.org"
     :tools (read_file read_buffer grep bash emacs_eval))
    (:name "fix"
     :title "Fix"
     :description "Diagnose and fix the current bug, failure, or regression."
     :prompt "actions/fix.org"
     :tools (read_file read_buffer write_file edit_file grep bash emacs_eval))
    (:name "init"
     :title "Initialize project instructions"
     :description "Initialize or refresh project instructions for Magent."
     :prompt "actions/init.org"
     :tools (read_file read_buffer write_file edit_file grep glob bash))
    (:name "review"
     :title "Review"
     :description
     "Review the current changes for defects, risks, and missing tests."
     :prompt "actions/review.org"
     :tools (read_file read_buffer grep bash))
    (:name "summarize"
     :title "Summarize repository"
     :description
     "Summarize the current Git project into one canonical Org note."
     :prompt "actions/summarize.org"
     :tools (read_file read_buffer grep glob bash write_repo_summary))
    (:name "test"
     :title "Run tests"
     :description "Run and interpret the relevant project tests."
     :prompt "actions/test.org"
     :tools (read_file read_buffer grep bash emacs_eval)))
  "Data definitions for bundled one-turn prompt actions.")

(defun magent-action-builtins--prompt-workflow (definition)
  "Return a terminal Workflow for prompt action DEFINITION."
  (iter-lambda (_invocation)
    (magent-workflow-answer
        (plist-get definition :title)
        (magent-prompt-read (plist-get definition :prompt))
      :append-argument-p t
      :tools (plist-get definition :tools))))

(defun magent-action-builtins--register-prompt-actions ()
  "Register bundled data-defined prompt actions."
  (dolist (definition magent-action-builtins--prompt-actions)
    (magent-action-register
     (plist-get definition :name)
     :description (plist-get definition :description)
     :session-policy 'current
     :workflow (magent-action-builtins--prompt-workflow definition)
     :source-layer 'builtin)))

(defun magent-action-builtins-register ()
  "Register every bundled Magent action as one atomic refresh."
  (let ((magent-action--allow-core-registration t)
        (magent-action--suppress-registry-hooks t))
    (magent-action-remove-source 'core)
    (magent-action-controls-register)
    (magent-doctor-register-action)
    (magent-memory-register-actions)
    (magent-action-builtins--register-prompt-actions)
    (magent-action-skills-register))
  (magent-action--registry-changed))

(provide 'magent-action-builtins)
;;; magent-action-builtins.el ends here
