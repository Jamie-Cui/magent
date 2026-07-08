;;; magent-agent-info.el --- Agent configuration data type for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Public agent configuration structure and helpers.

;;; Code:

(require 'cl-lib)
(require 'gptel)

(cl-defstruct (magent-agent-info
               (:constructor nil)
               (:constructor magent-agent-info-create)
               (:copier nil))
  "Structure representing an agent's configuration.

Fields:
- NAME: Agent identifier (e.g., \"build\", \"explore\")
- DESCRIPTION: Human-readable description of when to use this agent
- MODE: One of 'primary, 'subagent, or 'all
- NATIVE: Whether this is a built-in agent (vs user-defined)
- HIDDEN: Whether this agent should be hidden from listings
- TEMPERATURE: Optional temperature override for this agent
- TOP-P: Optional top-p override for this agent
- EFFORT: Optional reasoning effort override for this agent
- COLOR: Optional display color for UI
- MODEL: Optional model specification (providerID . modelID)
- PROMPT: Optional custom system prompt
- OPTIONS: Additional options as an alist
- STEPS: Compatibility metadata from older agent definitions; it no longer
  imposes a tool-call loop limit
- PERMISSION: Permission ruleset for tool access control
- FILE-PATH: Backing file path for file-defined agents
- SOURCE-LAYER: One of \\='builtin or \\='project
- SOURCE-SCOPE: Normalized project root for project-local agents"
  name
  description
  mode
  (native nil)
  (hidden nil)
  temperature
  top-p
  effort
  color
  model
  prompt
  (options nil)
  steps
  (permission nil)
  file-path
  (source-layer 'builtin)
  source-scope)

(defun magent-agent-info-valid-mode-p (mode)
  "Check if MODE is a valid agent mode."
  (memq mode '(primary subagent all)))

(defun magent-agent-info-mode-p (info mode)
  "Check if INFO's mode matches MODE or is \\='all."
  (let ((info-mode (magent-agent-info-mode info)))
    (or (eq info-mode 'all)
        (eq info-mode mode))))

(defun magent-agent-info-apply-gptel-overrides (info body-thunk)
  "Apply per-agent gptel variable overrides and call BODY-THUNK.
INFO is a magent-agent-info struct.  BODY-THUNK is a zero-argument
function called with the overrides in effect.

The agent's MODEL field can be:
- nil: use global `gptel-backend' and `gptel-model'
- A gptel backend object: use it with global `gptel-model'
- A cons (BACKEND . MODEL-SYMBOL): use both overrides

The agent's TEMPERATURE field, if non-nil, overrides `gptel-temperature'."
  (let* ((model-field (magent-agent-info-model info))
         (gptel-backend (cond
                         ((and (consp model-field)
                               (gptel-backend-p (car model-field)))
                          (car model-field))
                         ((gptel-backend-p model-field)
                          model-field)
                         (t (default-value 'gptel-backend))))
         (gptel-model (cond
                       ((and (consp model-field)
                             (symbolp (cdr model-field)))
                        (cdr model-field))
                       (t (default-value 'gptel-model))))
         (gptel-temperature (or (magent-agent-info-temperature info)
                                (default-value 'gptel-temperature))))
    (funcall body-thunk)))

(defun magent-agent-info-format-for-display (info)
  "Format INFO as a string for display in listings."
  (let ((name (magent-agent-info-name info))
        (desc (magent-agent-info-description info))
        (mode (magent-agent-info-mode info)))
    (format "%s [%s]%s\n    %s"
            name
            mode
            (if (magent-agent-info-hidden info) " (hidden)" "")
            (or desc ""))))

(defun magent-agent-info-valid-p (info)
  "Check if INFO is a valid agent info structure."
  (and (magent-agent-info-p info)
       (stringp (magent-agent-info-name info))
       (magent-agent-info-valid-mode-p (magent-agent-info-mode info))))

(provide 'magent-agent-info)
;;; magent-agent-info.el ends here
