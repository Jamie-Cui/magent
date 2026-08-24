;;; magent-action-builtins.el --- Bundled Magent actions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Register the bundled prompt actions and compose the built-in Action layers.

;;; Code:

(require 'magent-action)
(require 'magent-action-controls)
(require 'magent-action-skills)
(require 'magent-doctor)
(require 'magent-memory)
(require 'magent-prompt)
(require 'magent-agent-registry)
(require 'magent-runtime-api)
(require 'magent-tools)

(defconst magent-action-builtins--prompt-actions
  '((:name "explain"
     :title "Explain"
     :description
     "Explain the current code, diff, buffer, error, or project context."
     :prompt "actions/explain.org"
     :tools (read_file grep bash emacs_read emacs_eval read_tool_output))
    (:name "fix"
     :title "Fix"
     :description "Diagnose and fix the current bug, failure, or regression."
     :prompt "actions/fix.org"
     :tools (read_file write_file edit_file grep bash emacs_read
                       emacs_eval emacs_eval_live read_tool_output))
    (:name "init"
     :title "Initialize project instructions"
     :description "Initialize or refresh project instructions for Magent."
     :prompt "actions/init.org"
     :tools (read_file write_file edit_file grep glob bash read_tool_output))
    (:name "review"
     :title "Review"
     :description
     "Review the current changes for defects, risks, and missing tests."
     :prompt "actions/review.org"
     :tools (read_file grep bash read_tool_output))
    (:name "test"
     :title "Run tests"
     :description "Run and interpret the relevant project tests."
     :prompt "actions/test.org"
     :tools (read_file grep bash emacs_read emacs_eval emacs_eval_live
                       read_tool_output)))
  "Data definitions for bundled one-turn prompt actions.")

(defun magent-action-builtins--prompt-workflow (definition)
  "Return a terminal Workflow for prompt action DEFINITION."
  (iter-lambda (_invocation)
    (magent-workflow-answer
        (plist-get definition :title)
        (magent-prompt-read (plist-get definition :prompt))
      :append-argument-p t
      :tools (plist-get definition :tools))))

(defun magent-action-builtins--register-prompt-actions ()
  "Register bundled data-defined prompt actions."
  (dolist (definition magent-action-builtins--prompt-actions)
    (magent-action-register
     (plist-get definition :name)
     :description (plist-get definition :description)
     :session-policy 'current
     :workflow (magent-action-builtins--prompt-workflow definition)
     :source-layer 'builtin)))

(defun magent-action-builtins--authority-execution (name)
  "Return the execution boundary label for tool NAME."
  (pcase name
    ("emacs_eval" "child-emacs")
    ("emacs_eval_live" "live-emacs")
    ("emacs_read" "trusted-live-read")
    (_ "host")))

(defun magent-action-builtins--authority-rule-source (permission key)
  "Return the rule source label for KEY in PERMISSION."
  (cond
   ((magent-permission-intersection-p permission) 'intersection)
   ((memq permission '(allow deny ask)) 'whole-profile)
   (t
    (let ((rules (if (magent-permission-p permission)
                     (magent-permission-rules permission)
                   permission)))
      (cond
       ((assq key rules) 'tool-rule)
       ((assq '* rules) 'wildcard-rule)
       (t 'default-allow))))))

(defun magent-action-builtins--authority-resource-rules (permission key)
  "Return inspectable resource rules for KEY in PERMISSION, or nil."
  (cond
   ((magent-permission-intersection-p permission)
    (delq nil
          (mapcar
           (lambda (profile)
             (magent-action-builtins--authority-resource-rules profile key))
           (magent-permission-intersection-profiles permission))))
   (t
    (let* ((rules (if (magent-permission-p permission)
                      (magent-permission-rules permission)
                    permission))
           (rule (and (listp rules) (assq key rules))))
      (and rule (consp (cdr rule)) (cdr rule))))))

(magent-define-workflow magent-action-builtins--authority (invocation)
  "Describe the exact effective authority for INVOCATION."
  (let* ((runtime (magent-action-invocation-runtime-session invocation))
         (agent-name (magent-runtime-session-agent-name runtime))
         (agent (magent-agent-registry-get agent-name))
         (permission (and agent (magent-agent-info-permission agent)))
         (session (magent-runtime-session-magent-session runtime))
         (header
          (format "Effective authority\nagent: %s\nsession: %s\n\n"
                  agent-name
                  (magent-session-id session)))
         rows)
    (dolist (entry magent-tools-catalog)
      (let* ((name (plist-get entry :name))
             (key (plist-get entry :permission))
             (enabled (memq key magent-enable-tools))
             (available (and enabled
                             (or (null permission)
                                 (magent-permission-tool-available-p
                                  permission key))))
             (rule-decision (magent-permission-resolve permission key))
             (approval (or (plist-get entry :approval) 'normal))
             (raw-override
              (magent-permission-session-override key session))
             (effective
              (and enabled
                   (magent-permission-effective-decision
                    rule-decision approval raw-override)))
             (decision (if enabled
                           (plist-get effective :decision)
                         'disabled))
             (policy-source (and effective
                                 (plist-get effective :source)))
             (source
              (cond
               ((not enabled) 'global-disable)
               ((eq policy-source 'rule)
                (magent-action-builtins--authority-rule-source
                 permission key))
               (t policy-source)))
             (resource-rules
              (magent-action-builtins--authority-resource-rules
               permission key)))
        (push (format "%-22s permission=%-16s decision=%-8s exposed=%-3s approval=%-9s execution=%-17s source=%s%s"
                      name key decision (if available "yes" "no") approval
                      (magent-action-builtins--authority-execution name)
                      source
                      (if resource-rules
                          (format " resource-rules=%S" resource-rules)
                        ""))
              rows)))
    (concat header (string-join (nreverse rows) "\n"))))

(defun magent-action-builtins--register-authority ()
  "Register the trusted authority inspection Action."
  (magent-action-register
   "authority"
   :description "Show the effective tools, permission decisions, and execution boundaries."
   :title "Show Magent authority"
   :exposure '(slash interactive)
   :session-policy 'current
   :workflow #'magent-action-builtins--authority
   :source-layer 'core))

(defun magent-action-builtins-register ()
  "Register every bundled Magent action as one atomic refresh."
  (let ((magent-action--allow-core-registration t)
        (magent-action--suppress-registry-hooks t))
    (magent-action-remove-source 'core)
    (magent-action-controls-register)
    (magent-action-builtins--register-authority)
    (magent-doctor-register-action)
    (magent-memory-register-actions)
    (magent-action-builtins--register-prompt-actions)
    (magent-action-skills-register))
  (magent-action--registry-changed))

(provide 'magent-action-builtins)
;;; magent-action-builtins.el ends here
