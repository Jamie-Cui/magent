;;; magent-command-test.el --- Tests for slash commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'magent-command)
(require 'magent-command-test-runner)

(ert-deftest magent-command-test-registers-production-test-command ()
  "The production test runner registers the /test slash command."
  (let ((magent-command--registry nil)
        (magent-command--sequence 0))
    (magent-command-test-runner-register)
    (let ((command (magent-command-get "test")))
      (should command)
      (should (eq (magent-command-spec-owner command)
                  'magent-command-test-runner))
      (should (eq (magent-command-spec-source-layer command) 'builtin))
      (should (equal (magent-command-spec-tools command)
                     '(read_file grep bash emacs_eval))))))

(provide 'magent-command-test)
;;; magent-command-test.el ends here
