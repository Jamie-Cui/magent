;;; magent-agent-info.el --- Compatibility shim for legacy agent-info feature -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; Agent info structs and helpers now live in `magent-agent-registry.el'.
;; This file preserves the old feature entry point for callers that still
;; evaluate `(require 'magent-agent-info)'.

;;; Code:

(require 'magent-agent-registry)

(provide 'magent-agent-info)
;;; magent-agent-info.el ends here
