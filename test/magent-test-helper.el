;;; magent-test-helper.el --- Test helpers for magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>

;;; Commentary:

;; Common test utilities and helpers for magent test suite.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))

;; Mock functions for testing

(defvar magent-test-mock-functions nil
  "Alist of (function-name . mock-impl) for temporary function mocking.")

(defmacro magent-test-with-mock (func-name mock-impl &rest body)
  "Temporarily replace FUNC-NAME with MOCK-IMPL while executing BODY."
  (declare (indent 2))
  `(let ((original-func (symbol-function ',func-name)))
     (unwind-protect
         (progn
           (fset ',func-name ,mock-impl)
           ,@body)
       (fset ',func-name original-func))))

(defmacro magent-test-with-temp-file (var content &rest body)
  "Create a temporary file with CONTENT, bind to VAR, execute BODY, then delete."
  (declare (indent 2))
  `(let ((,var (make-temp-file "magent-test-")))
     (unwind-protect
         (progn
           (with-temp-file ,var
             (insert ,content))
           ,@body)
       (when (file-exists-p ,var)
         (delete-file ,var)))))

(defmacro magent-test-with-temp-dir (var &rest body)
  "Create a temporary directory, bind to VAR, execute BODY, then delete."
  (declare (indent 1))
  `(let ((,var (make-temp-file "magent-test-dir-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defmacro magent-test-with-registry (&rest body)
  "Execute BODY with a freshly initialized agent registry.
Dynamically binds registry state so tests do not interfere."
  (declare (indent 0))
  `(let ((magent-agent-registry--agents (make-hash-table :test 'equal))
         (magent-agent-registry--default-agent nil)
         (magent-agent-registry--initialized nil))
     (magent-agent-registry-init)
     ,@body))

(defun magent-test-fixture-path (filename)
  "Get the path to a fixture file FILENAME in test/fixtures/."
  (expand-file-name filename
                    (expand-file-name "fixtures"
                                      (file-name-directory load-file-name))))

(provide 'magent-test-helper)
;;; magent-test-helper.el ends here
