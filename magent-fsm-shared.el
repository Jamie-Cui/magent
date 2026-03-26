;;; magent-fsm-shared.el --- Compatibility shim for legacy FSM shared feature  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; The active tool-execution and permission helpers now live in
;; `magent-fsm-tools.el`.  This file remains only so older `require'
;; forms keep working.

;;; Code:

(require 'magent-fsm-tools)

(provide 'magent-fsm-shared)
;;; magent-fsm-shared.el ends here
