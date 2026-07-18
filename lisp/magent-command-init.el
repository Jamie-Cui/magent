;;; magent-command-init.el --- Init slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(defun magent-command-init-register ()
  "Register the bundled /init command."
  (magent-command-register
   "init"
   :description "Initialize or refresh project instructions for Magent."
   :turn (lambda (_invocation)
           (magent-command-turn-spec-create
            :prompt (magent-prompt-read "commands/init.org")))
   :source-layer 'builtin
   :required-tools '(read_file write_file edit_file grep glob bash)))

(provide 'magent-command-init)
;;; magent-command-init.el ends here
