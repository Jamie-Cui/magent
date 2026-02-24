;;; run-tests.el --- Test runner for magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>

;;; Commentary:

;; Main test runner for magent.
;; Usage: emacs -Q --batch -l test/run-tests.el

;;; Code:

;; Add lisp and test directories to load path
(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "test" (file-name-directory load-file-name)))

;; Load test framework
(require 'ert)
(require 'magent-test-helper)

;; Suppress byte-compile warnings during tests
(setq byte-compile-warnings '(not cl-functions))

;; Load all test files
(message "Loading test files...")

(defvar magent-test-files
  '("magent-agent-info-test"
    "magent-permission-test"
    "magent-session-test"
    "magent-agent-registry-test")
  "List of test modules to load.")

(dolist (test-file magent-test-files)
  (message "Loading %s..." test-file)
  (require (intern test-file)))

(message "Loaded %d test file(s)" (length magent-test-files))

;; Run tests when invoked with --batch
(when noninteractive
  (message "\n========================================")
  (message "Running magent test suite...")
  (message "========================================\n")
  (ert-run-tests-batch-and-exit))

(provide 'run-tests)
;;; run-tests.el ends here
