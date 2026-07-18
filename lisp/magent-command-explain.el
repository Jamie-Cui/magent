;;; magent-command-explain.el --- Explain slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(defun magent-command-explain-register ()
  "Register the bundled /explain command."
  (magent-command-register
   "explain"
   :description "Explain the current code, diff, buffer, error, or project context."
   :turn (lambda (_invocation)
           (magent-command-turn-spec-create
            :prompt (magent-prompt-read "commands/explain.org")))
   :owner 'magent-command-explain
   :source-layer 'builtin
   :required-tools '(read_file grep bash emacs_eval)))

(provide 'magent-command-explain)
;;; magent-command-explain.el ends here
