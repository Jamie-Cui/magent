;;; magent-command-summarize.el --- Summarize slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(defun magent-command-summarize-register ()
  "Register the bundled /summarize command."
  (magent-command-register
   "summarize"
   :description "Summarize the current Git project into one canonical Org note."
   :turn (lambda (_invocation)
           (magent-command-turn-spec-create
            :prompt (magent-prompt-read "commands/summarize.org")))
   :source-layer 'builtin
   :requires-project t
   :required-tools '(read_file grep glob bash write_repo_summary)))

(provide 'magent-command-summarize)
;;; magent-command-summarize.el ends here
