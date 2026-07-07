;;; magent-md2org.el --- Compatibility shim for magent-markdown-to-org  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Markdown-to-Org conversion helpers now live in `magent-markdown-to-org.el'.

;;; Code:

(require 'magent-markdown-to-org)

(define-obsolete-function-alias
  'magent-md2org-convert-region
  #'magent-markdown-to-org-convert-region
  "0.1.0")
(define-obsolete-function-alias
  'magent-md2org-convert-string
  #'magent-markdown-to-org-convert-string
  "0.1.0")

(provide 'magent-md2org)
;;; magent-md2org.el ends here
