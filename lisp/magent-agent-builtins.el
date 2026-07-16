;;; magent-agent-builtins.el --- Built-in agent definitions for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Built-in primary and subagent definitions.  The registry stores these, but
;; this module owns their prompts and factory functions.

;;; Code:

(require 'magent-agent-info)
(require 'magent-permission)
(require 'magent-prompt)

(defun magent-agent-builtins--build ()
  "Create the build agent (default primary agent)."
  (magent-agent-info-create
   :name "build"
   :description "Default agent for building and general coding tasks"
   :mode 'primary
   :native t
   :source-layer 'builtin
   :permission (magent-permission-defaults)))

(defun magent-agent-builtins--plan ()
  "Create the plan agent (planning mode with restricted edits)."
  (magent-agent-info-create
   :name "plan"
   :description "Planning mode for organizing work before implementation"
   :mode 'primary
   :native t
   :source-layer 'builtin
   :permission (magent-permission-merge
                (magent-permission-defaults)
                (magent-permission-from-config
                 '((edit
                    ("*" . deny)
                    (".magent/plan/*.md" . allow)))))))

(defun magent-agent-builtins--general ()
  "Create the general agent (multi-step subagent)."
  (magent-agent-info-create
   :name "general"
   :description "General-purpose agent for researching complex questions and executing multi-step tasks. Use this agent to execute multiple units of work in parallel."
   :mode 'subagent
   :native t
   :hidden t
   :source-layer 'builtin
   :permission (magent-permission-merge
                (magent-permission-defaults)
                (magent-permission-from-config
                 '((agent . deny))))))

(defun magent-agent-builtins--explore ()
  "Create the explore agent (codebase exploration specialist)."
  (magent-agent-info-create
   :name "explore"
   :description "Fast agent specialized for exploring codebases. Use this when you need to quickly find files by patterns (eg. \"src/components/**/*.tsx\"), search code for keywords (eg. \"API endpoints\"), or answer questions about the codebase (eg. \"how do API endpoints work?\"). When calling this agent, specify the desired thoroughness level: \"quick\" for basic searches, \"medium\" for moderate exploration, or \"very thorough\" for comprehensive analysis across multiple locations and naming conventions."
   :mode 'subagent
   :native t
   :prompt (magent-prompt-read "agents/explore.org")
   :source-layer 'builtin
   :permission (list (cons '* magent-permission-deny)
                     (cons 'grep magent-permission-allow)
                     (cons 'glob magent-permission-allow)
                     (cons 'read magent-permission-allow)
                     (cons 'bash magent-permission-allow))))

(defun magent-agent-builtins--compaction ()
  "Create the compaction agent (session summarization)."
  (magent-agent-info-create
   :name "compaction"
   :description "Session compaction for summarizing long conversations"
   :mode 'primary
   :native t
   :hidden t
   :prompt (magent-prompt-read "agents/compaction.org")
   :source-layer 'builtin
   :permission (magent-permission-from-config
                '((* . deny)))))

(defun magent-agent-builtins--title ()
  "Create the title agent (generates conversation titles)."
  (magent-agent-info-create
   :name "title"
   :description "Generate brief titles for conversations"
   :mode 'primary
   :native t
   :hidden t
   :prompt (magent-prompt-read "agents/title.org")
   :source-layer 'builtin
   :permission (magent-permission-from-config
                '((* . deny)))))

(defun magent-agent-builtins--summary ()
  "Create the summary agent (generates pull-request style summaries)."
  (magent-agent-info-create
   :name "summary"
   :description "Generate pull-request style summaries of conversations"
   :mode 'primary
   :native t
   :hidden t
   :prompt (magent-prompt-read "agents/summary.org")
   :source-layer 'builtin
   :permission (magent-permission-from-config
                '((* . deny)))))

(defun magent-agent-builtins-list ()
  "Return all built-in agent definitions."
  (list
   (magent-agent-builtins--build)
   (magent-agent-builtins--plan)
   (magent-agent-builtins--general)
   (magent-agent-builtins--explore)
   (magent-agent-builtins--compaction)
   (magent-agent-builtins--title)
   (magent-agent-builtins--summary)))

(provide 'magent-agent-builtins)
;;; magent-agent-builtins.el ends here
