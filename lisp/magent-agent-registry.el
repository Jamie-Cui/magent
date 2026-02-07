;;; magent-agent-registry.el --- Agent registry for OpenCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Central registry for managing all available agents.
;; Provides functions for registering, retrieving, and listing agents.

;;; Code:

(require 'cl-lib)
(require 'magent-agent-info)
(require 'magent-agent-types)
(require 'magent-permission)

;;; Registry state

(defvar magent-agent-registry--agents (make-hash-table :test 'equal)
  "Hash table mapping agent names to agent info structures.")

(defvar magent-agent-registry--default-agent nil
  "The default agent name.")

(defvar magent-agent-registry--initialized nil
  "Whether the registry has been initialized.")

;;; Registry initialization

(defun magent-agent-registry-init ()
  "Initialize the agent registry with built-in agents."
  (unless magent-agent-registry--initialized
    (clrhash magent-agent-registry--agents)
    (dolist (agent-info (magent-agent-types-initialize))
      (magent-agent-registry-register agent-info))
    (setq magent-agent-registry--default-agent
          (magent-agent-types-default-name))
    (setq magent-agent-registry--initialized t)
    ;; Load custom agents from config files
    (when (require 'magent-agent-file nil t)
      (magent-agent-file-load-all))))

;;; Agent registration

(defun magent-agent-registry-register (agent-info)
  "Register an AGENT-INFO in the registry.
If an agent with the same name exists, it will be replaced.
Returns the registered agent info."
  (when (magent-agent-info-valid-p agent-info)
    (puthash (magent-agent-info-name agent-info)
             agent-info
             magent-agent-registry--agents)
    agent-info))

(defun magent-agent-registry-unregister (name)
  "Unregister agent named NAME.
Returns the removed agent info, or nil if not found."
  (remhash name magent-agent-registry--agents))

(defun magent-agent-registry-register-from-config (name config)
  "Create and register an agent from CONFIG alist.
CONFIG should contain keys like :description, :mode, :prompt, etc.
Returns the registered agent info, or nil if invalid."
  (let* ((permission-plist (plist-get config :permission))
         (permission (when permission-plist
                       (magent-permission-from-config permission-plist)))
         (agent-info (magent-agent-info-create
                      :name name
                      :description (plist-get config :description)
                      :mode (or (plist-get config :mode) 'all)
                      :native nil
                      :hidden (plist-get config :hidden)
                      :temperature (plist-get config :temperature)
                      :top-p (plist-get config :top-p)
                      :color (plist-get config :color)
                      :model (plist-get config :model)
                      :prompt (plist-get config :prompt)
                      :options (plist-get config :options)
                      :steps (plist-get config :steps)
                      :permission permission)))
    (when (magent-agent-info-valid-p agent-info)
      (magent-agent-registry-register agent-info))))

;;; Agent retrieval

(defun magent-agent-registry-get (name)
  "Get agent info by NAME.
Returns the agent info structure, or nil if not found."
  (magent-agent-registry-ensure-initialized)
  (gethash name magent-agent-registry--agents))

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
    (maphash (lambda (name info)
               (when (and (or include-hidden
                              (not (magent-agent-info-hidden info)))
                          (or (null mode)
                              (magent-agent-info-mode-p info mode))
                          (or (null native-only)
                              (magent-agent-info-native info)))
                 (push info agents)))
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

(defun magent-agent-registry-exists-p (name)
  "Check if agent named NAME exists in the registry."
  (magent-agent-registry-ensure-initialized)
  (and (gethash name magent-agent-registry--agents) t))

(defun magent-agent-registry-count ()
  "Return the number of registered agents."
  (magent-agent-registry-ensure-initialized)
  (hash-table-count magent-agent-registry--agents))

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

(defun magent-agent-registry-resolve (agent-or-name)
  "Resolve AGENT-OR-NAME to an agent info structure.
If AGENT-OR-NAME is already a magent-agent-info, return it.
If it's a string, look it up in the registry.
Returns nil if not found."
  (cond
   ((magent-agent-info-p agent-or-name) agent-or-name)
   ((stringp agent-or-name) (magent-agent-registry-get agent-or-name))
   (t nil)))

;;; Interactive functions

;;;###autoload
(defun magent-list-agents (&optional include-hidden)
  "Display a list of all agents.
With prefix argument, include hidden agents."
  (interactive "P")
  (let ((agents (magent-agent-registry-list include-hidden)))
    (with-output-to-temp-buffer "*Magent Agents*"
      (princ "Available Agents:\n\n")
      (dolist (agent agents)
        (princ (magent-agent-info-format-for-display agent))
        (princ "\n"))
      (princ (format "\nTotal: %d agent(s)\n" (length agents)))
      (princ (format "\nDefault agent: %s\n"
                     (or magent-agent-registry--default-agent "none"))))
    (display-buffer "*Magent Agents*")))

;;;###autoload
(defun magent-set-default-agent (agent-name)
  "Set the default agent to AGENT-NAME."
  (interactive
   (list (completing-read "Set default agent: "
                          (magent-agent-registry-list-names))))
  (if (magent-agent-registry-set-default agent-name)
      (message "Default agent set to: %s" agent-name)
    (error "Agent not found: %s" agent-name)))

(provide 'magent-agent-registry)
;;; magent-agent-registry.el ends here
