;;; magent-command-fix.el --- Fix slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(magent-command-defworkflow magent-command-fix--workflow (_invocation)
  "Run the bundled fix Workflow."
  (magent-command-answer
      "Fix"
      (magent-prompt-read "commands/fix.org")
    :append-argument-p t
    :required-tools '(read_file write_file edit_file grep bash emacs_eval)))

(defun magent-command-fix-register ()
  "Register the bundled /fix command."
  (magent-command-register
   "fix"
   :description "Diagnose and fix the current bug, failure, or regression."
   :session-policy 'current
   :workflow #'magent-command-fix--workflow
   :source-layer 'builtin))

(provide 'magent-command-fix)
;;; magent-command-fix.el ends here
