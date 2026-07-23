;;; magent-command-summarize.el --- Summarize slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(magent-command-defworkflow magent-command-summarize--workflow (_invocation)
  "Run the bundled repository summary Workflow."
  (magent-command-answer
      "Summarize repository"
      (magent-prompt-read "commands/summarize.org")
    :append-argument-p t
    :required-tools '(read_file grep glob bash write_repo_summary)))

(defun magent-command-summarize-register ()
  "Register the bundled /summarize command."
  (magent-command-register
   "summarize"
   :description "Summarize the current Git project into one canonical Org note."
   :session-policy 'current
   :workflow #'magent-command-summarize--workflow
   :source-layer 'builtin))

(provide 'magent-command-summarize)
;;; magent-command-summarize.el ends here
