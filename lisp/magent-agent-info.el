;;; magent-agent-info.el --- Agent info structure for OpenCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Agent info structure and schema definitions for OpenCode agents.
;; Mirrors the Agent.Info type from the TypeScript implementation.

;;; Code:

(require 'cl-lib)

;;; Agent info structure

(cl-defstruct (magent-agent-info
               (:constructor magent-agent-info-create)
               (:copier nil)
               (:constructor nil))
  "Structure representing an agent's configuration.

Fields:
- NAME: Agent identifier (e.g., \"build\", \"explore\")
- DESCRIPTION: Human-readable description of when to use this agent
- MODE: One of 'primary, 'subagent, or 'all
- NATIVE: Whether this is a built-in agent (vs user-defined)
- HIDDEN: Whether this agent should be hidden from listings
- TEMPERATURE: Optional temperature override for this agent
- TOP-P: Optional top-p override for this agent
- COLOR: Optional display color for UI
- MODEL: Optional model specification (providerID . modelID)
- PROMPT: Optional custom system prompt
- OPTIONS: Additional options as an alist
- STEPS: Optional maximum iteration steps
- PERMISSION: Permission ruleset for tool access control"
  name
  description
  mode
  (native nil)
  (hidden nil)
  temperature
  top-p
  color
  model
  prompt
  (options nil)
  steps
  (permission nil))

;;; Agent mode validation

(defun magent-agent-info-valid-mode-p (mode)
  "Check if MODE is a valid agent mode."
  (memq mode '(primary subagent all)))

(defun magent-agent-info-mode-p (info mode)
  "Check if INFO's mode matches MODE or is \\='all."
  (let ((info-mode (magent-agent-info-mode info)))
    (or (eq info-mode 'all)
        (eq info-mode mode))))

;;; Agent model specification

(defun magent-agent-info-model-string (info)
  "Get the model identifier string for INFO.
Returns either the agent's model or the default from config."
  (let ((model (magent-agent-info-model info)))
    (if model
        (format "%s/%s" (car model) (cdr model))
      (when (bound-and-true-p magent-model)
        magent-model))))

;;; Agent display

(defun magent-agent-info-display-name (info)
  "Get the display name for INFO."
  (let ((name (magent-agent-info-name info)))
    (if (magent-agent-info-native info)
        (format "%s (built-in)" name)
      name)))

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

;;; Agent info validation

(defun magent-agent-info-valid-p (info)
  "Check if INFO is a valid agent info structure."
  (and (magent-agent-info-p info)
       (stringp (magent-agent-info-name info))
       (magent-agent-info-valid-mode-p (magent-agent-info-mode info))))

;;; Agent info defaults

(defun magent-agent-info-merge-defaults (info &optional defaults)
  "Merge DEFAULTS into INFO, preserving INFO's values.
DEFAULTS should be a magent-agent-info structure.
Returns a new magent-agent-info structure."
  (let ((base (or defaults (magent-agent-info-create
                            :name "default"
                            :mode 'all
                            :native t
                            :permission (magent-permission-defaults)))))
    (magent-agent-info-create
     :name (or (magent-agent-info-name info)
               (magent-agent-info-name base))
     :description (or (magent-agent-info-description info)
                      (magent-agent-info-description base))
     :mode (or (magent-agent-info-mode info)
               (magent-agent-info-mode base))
     :native (magent-agent-info-native info)
     :hidden (or (magent-agent-info-hidden info)
                 (magent-agent-info-hidden base))
     :temperature (or (magent-agent-info-temperature info)
                      (magent-agent-info-temperature base))
     :top-p (or (magent-agent-info-top-p info)
                (magent-agent-info-top-p base))
     :color (magent-agent-info-color info)
     :model (or (magent-agent-info-model info)
                (magent-agent-info-model base))
     :prompt (magent-agent-info-prompt info)
     :options (append (magent-agent-info-options base)
                      (magent-agent-info-options info))
     :steps (or (magent-agent-info-steps info)
                (magent-agent-info-steps base))
     :permission (magent-permission-merge
                  (magent-agent-info-permission base)
                  (magent-agent-info-permission info)))))

(provide 'magent-agent-info)
;;; magent-agent-info.el ends here
