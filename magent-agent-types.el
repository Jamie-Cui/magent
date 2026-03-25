;;; magent-agent-types.el --- Compatibility shim for legacy agent-types feature -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; Built-in agent definitions now live in `magent-agent-registry.el'.  This
;; file preserves the old feature entry point for callers that still evaluate
;; `(require 'magent-agent-types)'.

;;; Code:

(require 'magent-agent-registry)

(provide 'magent-agent-types)
;;; magent-agent-types.el ends here
