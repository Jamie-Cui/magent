;;; magent-command-test.el --- Run project tests slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(magent-command-defworkflow magent-command-test--workflow (_invocation)
  "Run the bundled test Workflow."
  (magent-command-answer
      "Run tests"
      (magent-prompt-read "commands/test.org")
    :append-argument-p t
    :required-tools '(read_file grep bash emacs_eval)))

(defun magent-command-test-register ()
  "Register the bundled /test command."
  (magent-command-register
   "test"
   :description "Run and interpret the relevant project tests."
   :session-policy 'current
   :workflow #'magent-command-test--workflow
   :source-layer 'builtin))

(provide 'magent-command-test)
;;; magent-command-test.el ends here
