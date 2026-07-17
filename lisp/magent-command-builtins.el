;;; magent-command-builtins.el --- Bundled slash commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command-controls)
(require 'magent-command-explain)
(require 'magent-command-fix)
(require 'magent-command-init)
(require 'magent-command-review)
(require 'magent-command-summarize)
(require 'magent-command-test-runner)

(defun magent-command-builtins-register ()
  "Register every bundled Magent slash command."
  (magent-command-controls-register)
  (magent-command-explain-register)
  (magent-command-fix-register)
  (magent-command-init-register)
  (magent-command-review-register)
  (magent-command-summarize-register)
  (magent-command-test-runner-register))

(provide 'magent-command-builtins)
;;; magent-command-builtins.el ends here
