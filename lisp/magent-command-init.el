;;; magent-command-init.el --- Init slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(magent-command-defworkflow magent-command-init--workflow (_invocation)
  "Run the bundled initialization Workflow."
  (magent-command-answer
      "Initialize project instructions"
      (magent-prompt-read "commands/init.org")
    :append-argument-p t
    :required-tools '(read_file write_file edit_file grep glob bash)))

(defun magent-command-init-register ()
  "Register the bundled /init command."
  (magent-command-register
   "init"
   :description "Initialize or refresh project instructions for Magent."
   :session-policy 'current
   :workflow #'magent-command-init--workflow
   :source-layer 'builtin))

(provide 'magent-command-init)
;;; magent-command-init.el ends here
