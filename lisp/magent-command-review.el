;;; magent-command-review.el --- Review slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(magent-command-defworkflow magent-command-review--workflow (_invocation)
  "Run the bundled review Workflow."
  (magent-command-answer
      "Review"
      (magent-prompt-read "commands/review.org")
    :append-argument-p t
    :required-tools '(read_file grep bash)))

(defun magent-command-review-register ()
  "Register the bundled /review command."
  (magent-command-register
   "review"
   :description "Review the current changes for defects, risks, and missing tests."
   :session-policy 'current
   :workflow #'magent-command-review--workflow
   :source-layer 'builtin))

(provide 'magent-command-review)
;;; magent-command-review.el ends here
