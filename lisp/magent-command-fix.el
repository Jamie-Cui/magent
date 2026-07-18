;;; magent-command-fix.el --- Fix slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(defun magent-command-fix-register ()
  "Register the bundled /fix command."
  (magent-command-register
   "fix"
   :description "Diagnose and fix the current bug, failure, or regression."
   :turn (lambda (_invocation)
           (magent-command-turn-spec-create
            :prompt (magent-prompt-read "commands/fix.org")))
   :owner 'magent-command-fix
   :source-layer 'builtin
   :required-tools '(read_file write_file edit_file grep bash emacs_eval)))

(provide 'magent-command-fix)
;;; magent-command-fix.el ends here
