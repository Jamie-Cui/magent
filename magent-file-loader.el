;;; magent-file-loader.el --- Shared file loader helpers for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Shared helper functions for Magent's file-backed definitions such as
;; agents, skills, and capabilities.

;;; Code:

(require 'cl-lib)
(require 'magent-config)
(require 'magent-frontmatter)

(defun magent-file-loader-project-subdir (relative-dir)
  "Return project-local RELATIVE-DIR as a one-item list when it exists."
  (when-let ((root (magent-project-root)))
    (let ((dir (expand-file-name relative-dir root)))
      (when (file-directory-p dir)
        (list dir)))))

(defun magent-file-loader-list-named-files (directories filename)
  "Return sorted FILENAME paths found in DIRECTORIES and their subdirs.

For each directory in DIRECTORIES, include:
- a direct `FILENAME' inside the directory
- `FILENAME' inside each non-hidden immediate subdirectory"
  (let (files)
    (dolist (dir directories)
      (when (file-directory-p dir)
        (push (expand-file-name filename dir) files)
        (dolist (subdir (directory-files dir t "^[^.]"))
          (when (file-directory-p subdir)
            (push (expand-file-name filename subdir) files)))))
    (sort files #'string<)))

(defun magent-file-loader-list-matching-files (directory regexp)
  "Return sorted files in DIRECTORY matching REGEXP."
  (sort (when (file-directory-p directory)
          (directory-files directory t regexp))
        #'string<))

(defun magent-file-loader-read-definition (filepath)
  "Read FILEPATH and return a plist with parsed frontmatter and body."
  (with-temp-buffer
    (insert-file-contents filepath)
    (let ((parsed (magent-frontmatter-parse (buffer-string))))
      (list :frontmatter (car parsed)
            :body (cdr parsed)))))

(defun magent-file-loader-load-all (files load-function)
  "Load FILES with LOAD-FUNCTION and return the success count."
  (let ((count 0))
    (dolist (file files)
      (when (funcall load-function file)
        (cl-incf count)))
    count))

(defun magent-file-loader-remove-file-backed-entries (registry file-path-function)
  "Return REGISTRY without entries whose value has a file path.
FILE-PATH-FUNCTION is called with each registry value."
  (cl-remove-if (lambda (entry)
                  (funcall file-path-function (cdr entry)))
                registry))

(provide 'magent-file-loader)
;;; magent-file-loader.el ends here
