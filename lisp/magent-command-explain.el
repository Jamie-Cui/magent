;;; magent-command-explain.el --- Explain slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(magent-command-defworkflow magent-command-explain--workflow (_invocation)
  "Run the bundled explanation Workflow."
  (magent-command-answer
      "Explain"
      (magent-prompt-read "commands/explain.org")
    :append-argument-p t
    :required-tools '(read_file grep bash emacs_eval)))

(defun magent-command-explain-register ()
  "Register the bundled /explain command."
  (magent-command-register
   "explain"
   :description "Explain the current code, diff, buffer, error, or project context."
   :session-policy 'current
   :workflow #'magent-command-explain--workflow
   :source-layer 'builtin))

(provide 'magent-command-explain)
;;; magent-command-explain.el ends here
