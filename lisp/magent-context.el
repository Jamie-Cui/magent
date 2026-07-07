;;; magent-context.el --- Compatibility shim for magent-transcript-context  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Structured transcript context helpers now live in
;; `magent-transcript-context.el'.  This file preserves the historical feature
;; and function names.

;;; Code:

(require 'magent-transcript-context)

(define-obsolete-function-alias
  'magent-context-items
  #'magent-transcript-context-items
  "0.1.0")
(define-obsolete-function-alias
  'magent-context-record-message
  #'magent-transcript-context-record-message
  "0.1.0")
(define-obsolete-function-alias
  'magent-context-record-tool-call
  #'magent-transcript-context-record-tool-call
  "0.1.0")
(define-obsolete-function-alias
  'magent-context-record-tool-result
  #'magent-transcript-context-record-tool-result
  "0.1.0")
(define-obsolete-function-alias
  'magent-context-message-items
  #'magent-transcript-context-message-items
  "0.1.0")
(define-obsolete-function-alias
  'magent-context-approx-token-count
  #'magent-transcript-context-approx-token-count
  "0.1.0")

(provide 'magent-context)
;;; magent-context.el ends here
