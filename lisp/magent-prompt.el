;;; magent-prompt.el --- Bundled prompt resource loading  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Load Magent-owned prompt text from Org files under the bundled prompt/
;; directory.  Provider transport and dynamic prompt assembly remain in their
;; owning modules; this module only resolves and reads static prompt resources.

;;; Code:

(require 'subr-x)

(defvar magent-prompt-directory
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; In the repository Elisp lives under lisp/.  MELPA flattens Elisp files
    ;; into the package root while retaining prompt/ as package data.
    (or (let ((candidate (expand-file-name "prompt" dir)))
          (and (file-directory-p candidate) candidate))
        (let ((candidate (expand-file-name "prompt"
                                           (expand-file-name ".." dir))))
          (and (file-directory-p candidate) candidate))
        (expand-file-name "prompt" dir)))
  "Directory containing bundled Magent Org prompt resources.")

(defun magent-prompt-path (relative-path)
  "Return the absolute prompt resource path for RELATIVE-PATH.
Signal an error when RELATIVE-PATH escapes `magent-prompt-directory'."
  (let* ((root (file-name-as-directory
                (expand-file-name magent-prompt-directory)))
         (path (expand-file-name relative-path root)))
    (unless (string-prefix-p root path)
      (error "Prompt path escapes prompt directory: %s" relative-path))
    path))

(defun magent-prompt-read (relative-path)
  "Read and return Org prompt resource RELATIVE-PATH.
Trailing whitespace is removed so file-ending newlines do not alter request
assembly."
  (let ((path (magent-prompt-path relative-path)))
    (unless (file-readable-p path)
      (error "Magent prompt resource is not readable: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (string-trim-right (buffer-string)))))

(defun magent-prompt-render (relative-path replacements)
  "Read RELATIVE-PATH and substitute named REPLACEMENTS.
REPLACEMENTS is an alist whose keys name placeholders written as
~{{key}}~ in the Org resource.  Values are inserted literally, so prompt
authors can use percent signs and other formatting characters normally."
  (let ((result (magent-prompt-read relative-path)))
    (dolist (replacement replacements result)
      (setq result
            (replace-regexp-in-string
             (regexp-quote (format "{{%s}}" (car replacement)))
             (format "%s" (cdr replacement))
             result t t)))))

(provide 'magent-prompt)
;;; magent-prompt.el ends here
