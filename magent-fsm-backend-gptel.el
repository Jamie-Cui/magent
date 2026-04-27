;;; magent-fsm-backend-gptel.el --- Compatibility shim for legacy FSM backend feature  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; The active gptel FSM implementation now lives in `magent-fsm.el`.
;; This file remains only so older `require' forms keep working.

;;; Code:

(require 'magent-fsm)

(provide 'magent-fsm-backend-gptel)
;;; magent-fsm-backend-gptel.el ends here
