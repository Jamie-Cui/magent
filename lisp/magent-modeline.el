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
(require 'magent-session)
(require 'magent-ui)

(defun magent-modeline--current-agent-name ()
  "Return the current agent name, falling back to the configured default."
  (let ((agent (when magent--current-session
                 (magent-session-agent magent--current-session))))
    (if agent (magent-agent-info-name agent) magent-default-agent)))

(defun magent-modeline--status-string ()
  "Return the Magent mode-line status string."
  (concat " [M/" (magent-modeline--current-agent-name) "] "
          (when (magent-ui-processing-p)
            (propertize "[busy]" 'face 'warning
                        'help-echo "Magent: request in progress"))))

(defconst magent-modeline-lighter
  '(:eval (magent-modeline--status-string))
  "Mode-line lighter for `magent-mode'.")
(put 'magent-modeline-lighter 'risky-local-variable t)

(defun magent-modeline--legacy-output-string ()
  "Return Magent status for a legacy output buffer, or nil elsewhere."
  (when (derived-mode-p 'magent-output-mode)
    (magent-modeline--status-string)))

(defconst magent-modeline--legacy-output-construct
  '(:eval (magent-modeline--legacy-output-string))
  "Mode-line construct for legacy Magent output buffers.")
(put 'magent-modeline--legacy-output-construct 'risky-local-variable t)

(defun magent-modeline-install ()
  "Install the legacy output status construct.
Return non-nil when the construct was newly installed."
  (unless (member magent-modeline--legacy-output-construct global-mode-string)
    (push magent-modeline--legacy-output-construct global-mode-string)
    t))

(provide 'magent-modeline)
;;; magent-modeline.el ends here
