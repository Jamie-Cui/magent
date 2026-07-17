;;; magent-command-run-tests.el --- Run project tests slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(defun magent-command-run-tests-register ()
  "Register the bundled /test command."
  (magent-command-register
   "test"
   :description "Run and interpret the relevant project tests."
   :handler (magent-command-prompt-handler
             (lambda (_invocation)
               (magent-prompt-read "commands/test.org")))
   :owner 'magent-command-run-tests
   :source-layer 'builtin
   :tools '(read_file grep bash emacs_eval)))

(provide 'magent-command-run-tests)
;;; magent-command-run-tests.el ends here
