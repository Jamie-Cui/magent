;;; magent-source-files.el --- Shared source manifest reader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Keep developer tooling aligned with the production source manifest.

;;; Code:

(require 'subr-x)

(defun magent-test-source-files (root)
  "Return production source paths from ROOT/source-files.txt."
  (let ((manifest (expand-file-name "source-files.txt" root))
        files)
    (unless (file-readable-p manifest)
      (error "Magent source manifest is not readable: %s" manifest))
    (with-temp-buffer
      (insert-file-contents manifest)
      (dolist (line (split-string (buffer-string) "\n" t))
        (setq line (string-trim line))
        (unless (or (string-empty-p line)
                    (string-prefix-p "#" line))
          (push line files))))
    (nreverse files)))

(provide 'magent-source-files)

;;; magent-source-files.el ends here
