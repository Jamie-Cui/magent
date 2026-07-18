;;; magent-command-test.el --- Run project tests slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(defun magent-command-test-register ()
  "Register the bundled /test command."
  (magent-command-register
   "test"
   :description "Run and interpret the relevant project tests."
   :turn (lambda (_invocation)
           (magent-command-turn-spec-create
            :prompt (magent-prompt-read "commands/test.org")))
   :owner 'magent-command-test
   :source-layer 'builtin
   :required-tools '(read_file grep bash emacs_eval)))

(provide 'magent-command-test)
;;; magent-command-test.el ends here
