;;; magent-command-builtin-test.el --- Tests for slash commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'magent-command)
(require 'magent-command-test)

(ert-deftest magent-command-test-registers-production-test-command ()
  "The production test runner registers the /test slash command."
  (let ((magent-command--registry nil)
        (magent-command--sequence 0))
    (magent-command-test-register)
    (let ((command (magent-command-get "test")))
      (should command)
      (should (eq (magent-command-spec-source-layer command) 'builtin))
      (should (functionp (magent-command-spec-workflow command)))
      (should (eq (magent-command-spec-session-policy command) 'current)))))

(provide 'magent-command-builtin-test)
;;; magent-command-builtin-test.el ends here
