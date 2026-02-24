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

(defun magent-test-assert-equal (expected actual &optional message)
  "Assert that EXPECTED equals ACTUAL with optional MESSAGE."
  (should (equal expected actual)))

(defun magent-test-assert-match (regexp string &optional message)
  "Assert that REGEXP matches STRING with optional MESSAGE."
  (should (string-match-p regexp string)))

(defun magent-test-assert-nil (value &optional message)
  "Assert that VALUE is nil with optional MESSAGE."
  (should (null value)))

(defun magent-test-assert-not-nil (value &optional message)
  "Assert that VALUE is not nil with optional MESSAGE."
  (should value))

(defun magent-test-fixture-path (filename)
  "Get the path to a fixture file FILENAME in test/fixtures/."
  (expand-file-name filename
                    (expand-file-name "fixtures"
                                      (file-name-directory load-file-name))))

(provide 'magent-test-helper)
;;; magent-test-helper.el ends here
