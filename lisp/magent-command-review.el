;;; magent-command-review.el --- Review slash command  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-prompt)

(defun magent-command-review-register ()
  "Register the bundled /review command."
  (magent-command-register
   "review"
   :description "Review the current changes for defects, risks, and missing tests."
   :handler (magent-command-turn-handler
             (lambda (_invocation)
               (magent-command-turn-spec-create
                :prompt (magent-prompt-read "commands/review.org"))))
   :owner 'magent-command-review
   :source-layer 'builtin
   :tools '(read_file grep bash)))

(provide 'magent-command-review)
;;; magent-command-review.el ends here
