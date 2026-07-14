;;; magent-agent-registry.el --- Agent registry for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Central registry for built-in and file-backed agent definitions.

;;; Code:

(require 'cl-lib)
(require 'magent-agent-builtins)
(require 'magent-agent-info)
(require 'magent-config)
(require 'magent-file-loader)
(require 'magent-runtime)

;;; Agent registry

;;; Registry state

(defvar magent-agent-registry--agents (make-hash-table :test 'equal)
  "Hash table mapping agent names to precedence-ordered agent stacks.
The first item is the effective definition.  Lower-layer definitions remain
available so removing a project overlay restores the previous definition.")

(defvar magent-agent-registry--default-agent nil
  "The default agent name.")

(defvar magent-agent-registry--initialized nil
  "Whether the registry has been initialized.")

;;; Registry initialization

(defun magent-agent-registry-init ()
  "Initialize the agent registry with built-in agents."
  (unless magent-agent-registry--initialized
    (clrhash magent-agent-registry--agents)
    (dolist (agent-info (magent-agent-builtins-list))
      (magent-agent-registry-register agent-info))
    (setq magent-agent-registry--default-agent
          magent-default-agent)
    (setq magent-agent-registry--initialized t)))

(defun magent-agent-initialize-static ()
  "Initialize built-in agent definitions."
  (magent-agent-registry-init))

;;; Agent registration

(defun magent-agent-registry-register (agent-info)
  "Register an AGENT-INFO in the registry.
An existing definition from the same source layer and scope is replaced.
Definitions from lower layers are retained for reversible project overlays.
Returns the registered agent info."
  (when (magent-agent-info-valid-p agent-info)
    (let* ((name (magent-agent-info-name agent-info))
           (current (gethash name magent-agent-registry--agents))
           (stack (cond
                   ((magent-agent-info-p current) (list current))
                   ((listp current) current)
                   (t nil)))
           (layer (magent-agent-info-source-layer agent-info))
           (scope (magent-agent-info-source-scope agent-info)))
      (setq stack
            (cl-remove-if
             (lambda (existing)
               (and (eq (magent-agent-info-source-layer existing) layer)
                    (equal (magent-agent-info-source-scope existing) scope)))
             stack))
      (puthash name (cons agent-info stack)
               magent-agent-registry--agents))
    agent-info))

(defun magent-agent-registry-remove-project-scope (scope)
  "Remove all project-local agents registered for SCOPE."
  (let (updates removals)
    (maphash
     (lambda (name value)
       (let* ((stack (if (magent-agent-info-p value) (list value) value))
              (remaining
               (cl-remove-if
                (lambda (agent)
                  (and (eq (magent-agent-info-source-layer agent) 'project)
                       (equal (magent-agent-info-source-scope agent) scope)))
                stack)))
         (if remaining
             (push (cons name remaining) updates)
           (push name removals))))
     magent-agent-registry--agents)
    (dolist (entry updates)
      (puthash (car entry) (cdr entry) magent-agent-registry--agents))
    (dolist (name removals)
      (remhash name magent-agent-registry--agents)))
  magent-agent-registry--agents)

;;; Agent retrieval

(defun magent-agent-registry-get (name)
  "Get agent info by NAME.
Returns the agent info structure, or nil if not found."
  (magent-agent-registry-ensure-initialized)
  (let ((value (gethash name magent-agent-registry--agents)))
    (if (magent-agent-info-p value) value (car value))))

(defun magent-agent-registry-get-default ()
  "Get the default agent info.
Returns the default agent info structure, or nil if not found."
  (magent-agent-registry-ensure-initialized)
  (magent-agent-registry-get magent-agent-registry--default-agent))

(defun magent-agent-registry-set-default (name)
  "Set the default agent to NAME.
Returns the new default agent info, or nil if not found."
  (magent-agent-registry-ensure-initialized)
  (when (magent-agent-registry-get name)
    (setq magent-agent-registry--default-agent name)
    (magent-agent-registry-get name)))

;;; Agent listing

(defun magent-agent-registry-list (&optional include-hidden mode native-only)
  "List all registered agents.
Returns list of agent info structures sorted by native status and name.

If INCLUDE-HIDDEN is non-nil, include hidden agents.
If MODE is non-nil (primary, subagent, or all), filter by mode.
If NATIVE-ONLY is non-nil, only include native (built-in) agents."
  (magent-agent-registry-ensure-initialized)
  (let ((agents nil))
    (maphash (lambda (_name value)
               (let ((info (if (magent-agent-info-p value)
                               value
                             (car value))))
                 (when (and info
                            (or include-hidden
                                (not (magent-agent-info-hidden info)))
                            (or (null mode)
                                (magent-agent-info-mode-p info mode))
                            (or (null native-only)
                                (magent-agent-info-native info)))
                   (push info agents))))
             magent-agent-registry--agents)
    (sort agents
          (lambda (a b)
            (let ((a-native (magent-agent-info-native a))
                  (b-native (magent-agent-info-native b))
                  (a-name (magent-agent-info-name a))
                  (b-name (magent-agent-info-name b)))
              (cond
               ;; Native agents first
               ((and a-native (not b-native)) t)
               ((and (not a-native) b-native) nil)
               ;; Then sort by name
               (t (string< a-name b-name))))))))

(defun magent-agent-registry-list-names (&optional include-hidden mode)
  "List all registered agent names.
Returns list of agent name strings."
  (mapcar #'magent-agent-info-name
          (magent-agent-registry-list include-hidden mode)))

(defun magent-agent-registry-primary-agents ()
  "List all primary agents (non-hidden, mode is primary or all)."
  (magent-agent-registry-list nil 'primary))

(defun magent-agent-registry-subagents ()
  "List all subagents (mode is subagent or all)."
  (magent-agent-registry-list nil 'subagent))

;;; Agent utilities

(defun magent-agent-registry-clear ()
  "Clear all agents from the registry.
This does not affect built-in agents that will be reloaded on initialization."
  (clrhash magent-agent-registry--agents)
  (setq magent-agent-registry--initialized nil))

(defun magent-agent-registry-reinit ()
  "Reinitialize the registry, reloading all built-in agents."
  (setq magent-agent-registry--initialized nil)
  (magent-agent-registry-init))

;;; Helper functions

(defun magent-agent-registry-ensure-initialized ()
  "Ensure the registry is initialized."
  (unless magent-agent-registry--initialized
    (magent-agent-registry-init)))

;;; Interactive functions

(declare-function magent-agent-file-load-all "magent-agent-file")
(declare-function magent-agent-file-save "magent-agent-file")

;;;###autoload
(defun magent-list-agents (&optional include-hidden)
  "Display a list of all agents.
With prefix argument, include hidden agents."
  (interactive "P")
  (magent-runtime-prepare-command-context)
  (let ((agents (magent-agent-registry-list include-hidden)))
    (magent--with-display-buffer "*Magent Agents*"
      (insert "Available Agents:\n\n")
      (dolist (agent agents)
        (insert (magent-agent-info-format-for-display agent) "\n"))
      (insert (format "\nTotal: %d agent(s)\n" (length agents)))
      (insert (format "\nDefault agent: %s\n"
                      (or magent-agent-registry--default-agent "none"))))))

;;;###autoload
(defun magent-set-default-agent (agent-name)
  "Set the default agent to AGENT-NAME."
  (interactive
   (progn
     (magent-runtime-prepare-command-context)
     (list (completing-read "Set default agent: "
                            (magent-agent-registry-list-names)))))
  (magent-runtime-prepare-command-context)
  (if (magent-agent-registry-set-default agent-name)
      (magent-log "INFO default agent set to: %s" agent-name)
    (error "Agent not found: %s" agent-name)))

;;;###autoload
(defun magent-load-agent-files (&optional directory)
  "Load all agent files from DIRECTORY or project root."
  (interactive)
  (require 'magent-agent-file)
  (magent-agent-file-load-all directory))

;;;###autoload
(defun magent-save-agent (agent-name &optional directory)
  "Save agent named AGENT-NAME to a file."
  (interactive
   (progn
     (magent-runtime-prepare-command-context)
     (list (completing-read "Save agent: "
                            (magent-agent-registry-list-names t)))))
  (require 'magent-agent-file)
  (magent-runtime-prepare-command-context)
  (let ((agent-info (magent-agent-registry-get agent-name)))
    (if agent-info
        (let ((filepath (magent-agent-file-save agent-info directory)))
          (magent-log "INFO agent saved to: %s" filepath))
      (error "Agent not found: %s" agent-name))))

(provide 'magent-agent-registry)
;;; magent-agent-registry.el ends here
