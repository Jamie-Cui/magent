;;; magent-modeline.el --- Mode-line integration for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Mode-line formatting and installation for `magent-mode'.

;;; Code:

(require 'magent-agent-info)
(require 'magent-config)
(require 'magent-runtime-api)
(require 'magent-session)

(defun magent-modeline--current-agent-name ()
  "Return the current agent name, falling back to the configured default."
  (let ((agent (when magent--current-session
                 (magent-session-agent magent--current-session))))
    (if agent (magent-agent-info-name agent) magent-default-agent)))

(defun magent-modeline--status-string ()
  "Return the Magent mode-line status string."
  (concat " [M/" (magent-modeline--current-agent-name) "] "
          (when (magent-runtime-processing-p)
            (propertize "[busy]" 'face 'warning
                        'help-echo "Magent: request in progress"))))

(defconst magent-modeline-lighter
  '(:eval (magent-modeline--status-string))
  "Mode-line lighter for `magent-mode'.")
(put 'magent-modeline-lighter 'risky-local-variable t)

(provide 'magent-modeline)
;;; magent-modeline.el ends here
