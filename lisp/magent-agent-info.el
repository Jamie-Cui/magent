;;; magent-agent-info.el --- Agent configuration data type for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Public agent configuration structure and helpers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magent-sampling-gptel)

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
- THINKING: Optional thinking mode override for this agent
- COLOR: Optional display color for UI
- EXECUTOR: \\='magent for gptel sampling or \\='codex for Codex ACP
- BACKEND: Optional registered gptel backend name
- MODEL: Optional model identifier as a string or symbol
- PROMPT: Optional custom system prompt
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
  thinking
  color
  (executor 'magent)
  backend
  model
  prompt
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

(defun magent-agent-info-model-route (info)
  "Resolve INFO's explicit gptel route from its saved backend and model IDs."
  (unless (magent-agent-info-p info)
    (error "Expected Magent agent info, got: %S" info))
  (let ((name (magent-agent-info-backend info))
        (model (magent-agent-info-model info)))
    (when (or name model)
      (when (eq (magent-agent-info-executor info) 'codex)
        (error "Codex agents must be started as child jobs"))
      (let* ((descriptor
              (and name
                   (cl-find name (magent-sampling-gptel-model-catalog)
                            :key #'magent-model-descriptor-backend-name
                            :test #'equal)))
             (backend (if name
                          (if descriptor
                              (magent-model-descriptor-backend descriptor)
                            (error "Unknown gptel backend: %s" name))
                        (default-value 'gptel-backend))))
        (magent-model-route-create
         :backend backend
         :model (cond ((stringp model) (intern model))
                      (model model)
                      (t (default-value 'gptel-model)))
         :source 'agent
         :profile-agent (magent-agent-info-name info))))))

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
       (magent-agent-info-valid-mode-p (magent-agent-info-mode info))
       (memq (magent-agent-info-executor info) '(magent codex))
       (let ((model (magent-agent-info-model info))
             (backend (magent-agent-info-backend info)))
         (and (or (null model)
                  (and (symbolp model) (not (string-empty-p (symbol-name model))))
                  (and (stringp model) (not (string-empty-p model))))
              (or (null backend)
                  (and (stringp backend) (not (string-empty-p backend))))
              (or (eq (magent-agent-info-executor info) 'magent)
                  (and (eq (magent-agent-info-mode info) 'subagent)
                       (null backend)))))))

(provide 'magent-agent-info)
;;; magent-agent-info.el ends here
