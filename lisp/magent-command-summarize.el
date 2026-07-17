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
   :handler (magent-command-prompt-handler
             (lambda (_invocation)
               (magent-prompt-read "commands/summarize.org")))
   :owner 'magent-command-summarize
   :source-layer 'builtin
   :requires-project t
   :tools '(read_file grep glob bash write_repo_summary)))

(provide 'magent-command-summarize)
;;; magent-command-summarize.el ends here
