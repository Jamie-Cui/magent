;;; magent-agent-registry.el --- Agent info, built-in types, and registry for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Agent info structure, built-in agent definitions, and central registry.
;; This file consolidates what was previously split across:
;;   magent-agent-info.el   (struct + helpers)
;;   magent-agent-types.el  (built-in agent definitions)
;;   magent-agent-registry.el (registry operations)

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'magent-config)
(require 'magent-permission)

(declare-function magent-agent-file-load-all "magent-agent-file")

;; ──────────────────────────────────────────────────────────────────────
;;; Agent info structure
;; ──────────────────────────────────────────────────────────────────────

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

;;; gptel override support

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
                         (t gptel-backend)))
         (gptel-model (cond
                       ((and (consp model-field)
                             (symbolp (cdr model-field)))
                        (cdr model-field))
                       (t gptel-model)))
         (gptel-temperature (or (magent-agent-info-temperature info)
                                gptel-temperature)))
    (funcall body-thunk)))

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

;; ──────────────────────────────────────────────────────────────────────
;;; Built-in agent types
;; ──────────────────────────────────────────────────────────────────────

;;; Built-in agent prompts

(defconst magent-agent--prompt-explore
  "You are a file search specialist. You excel at thoroughly navigating and exploring codebases.

Your strengths:
- Rapidly finding files using glob patterns
- Searching code and text with powerful regex patterns
- Reading and analyzing file contents

Guidelines:
- Use glob for broad file pattern matching
- Use grep for searching file contents with regex
- Use read_file when you know the specific file path you need to read
- Use bash for file operations like copying, moving, or listing directory contents
- Adapt your search approach based on the thoroughness level specified by the caller
- Return file paths as absolute paths in your final response
- For clear communication, avoid using emojis
- Do not create any files, or run bash commands that modify the user's system state in any way

Complete the user's search request efficiently and report your findings clearly."
  "Prompt for the explore agent.")

(defconst magent-agent--prompt-compaction
  "You are a helpful AI assistant tasked with summarizing conversations.

When asked to summarize, provide a detailed but concise summary of the conversation.
Focus on information that would be helpful for continuing the conversation, including:
- What was done
- What is currently being worked on
- Which files are being modified
- What needs to be done next
- Key user requests, constraints, or preferences that should persist
- Important technical decisions and why they were made

Your summary should be comprehensive enough to provide context but concise enough to be quickly understood."
  "Prompt for the compaction agent.")

(defconst magent-agent--prompt-summary
  "Summarize what was done in this conversation. Write like a pull request description.

Rules:
- 2-3 sentences max
- Describe the changes made, not the process
- Do not mention running tests, builds, or other validation steps
- Do not explain what the user asked for
- Write in first person (I added..., I fixed...)
- Never ask questions or add new questions
- If the conversation ends with an unanswered question to the user, preserve that exact question
- If the conversation ends with an imperative statement or request to the user (e.g. \"Now please run the command and paste the console output\"), always include that exact request in the summary"
  "Prompt for the summary agent.")

(defconst magent-agent--prompt-title
  "You are a title generator. You output ONLY a thread title. Nothing else.

<task>
Generate a brief title that would help the user find this conversation later.

Follow all rules in <rules>
Use the <examples> so you know what a good title looks like.
Your output must be:
- A single line
- <= 50 characters
- No explanations
</task>

<rules>
- Focus on the main topic or question the user needs to retrieve
- Use -ing verbs for actions (Debugging, Implementing, Analyzing)
- Keep exact: technical terms, numbers, filenames, HTTP codes
- Remove: the, this, my, a, an
- Never assume tech stack
- Never use tools
- NEVER respond to questions, just generate a title for the conversation
- The title should NEVER include \"summarizing\" or \"generating\" when generating a title
- DO NOT SAY YOU CANNOT GENERATE A TITLE OR COMPLAIN ABOUT THE INPUT
- Always output something meaningful, even if the input is minimal.
- If the user message is short or conversational (e.g. \"hello\", \"lol\", \"what's up\", \"hey\"):
  -> create a title that reflects the user's tone or intent (such as Greeting, Quick check-in, Light chat, Intro message, etc.)
</rules>

<examples>
\"debug 500 errors in production\" -> Debugging production 500 errors
\"refactor user service\" -> Refactoring user service
\"why is app.js failing\" -> Analyzing app.js failure
\"implement rate limiting\" -> Implementing rate limiting
\"how do I connect postgres to my API\" -> Connecting Postgres to API
\"best practices for React hooks\" -> React hooks best practices
</examples>"
  "Prompt for the title agent.")

;;; Built-in agent definitions

(defun magent-agent-types--build ()
  "Create the build agent (default primary agent)."
  (magent-agent-info-create
   :name "build"
   :description "Default agent for building and general coding tasks"
   :mode 'primary
   :native t
   :permission (magent-permission-defaults)))

(defun magent-agent-types--plan ()
  "Create the plan agent (planning mode with restricted edits)."
  (magent-agent-info-create
   :name "plan"
   :description "Planning mode for organizing work before implementation"
   :mode 'primary
   :native t
   :permission (magent-permission-merge
                (magent-permission-defaults)
                (magent-permission-from-config
                 '((edit
                    ("*" . deny)
                    (".magent/plan/*.md" . allow)))))))

(defun magent-agent-types--general ()
  "Create the general agent (multi-step subagent)."
  (magent-agent-info-create
   :name "general"
   :description "General-purpose agent for researching complex questions and executing multi-step tasks. Use this agent to execute multiple units of work in parallel."
   :mode 'subagent
   :native t
   :hidden t
   :permission (magent-permission-merge
                (magent-permission-defaults)
                (magent-permission-from-config
                 '((delegate . deny))))))

(defun magent-agent-types--explore ()
  "Create the explore agent (codebase exploration specialist)."
  (magent-agent-info-create
   :name "explore"
   :description "Fast agent specialized for exploring codebases. Use this when you need to quickly find files by patterns (eg. \"src/components/**/*.tsx\"), search code for keywords (eg. \"API endpoints\"), or answer questions about the codebase (eg. \"how do API endpoints work?\"). When calling this agent, specify the desired thoroughness level: \"quick\" for basic searches, \"medium\" for moderate exploration, or \"very thorough\" for comprehensive analysis across multiple locations and naming conventions."
   :mode 'subagent
   :native t
   :prompt magent-agent--prompt-explore
   :permission (list (cons '* magent-permission-deny)
                     (cons 'grep magent-permission-allow)
                     (cons 'glob magent-permission-allow)
                     (cons 'read magent-permission-allow)
                     (cons 'bash magent-permission-allow))))

(defun magent-agent-types--compaction ()
  "Create the compaction agent (session summarization)."
  (magent-agent-info-create
   :name "compaction"
   :description "Session compaction for summarizing long conversations"
   :mode 'primary
   :native t
   :hidden t
   :prompt magent-agent--prompt-compaction
   :permission (magent-permission-from-config
                '((* . deny)))))

(defun magent-agent-types--title ()
  "Create the title agent (generates conversation titles)."
  (magent-agent-info-create
   :name "title"
   :description "Generate brief titles for conversations"
   :mode 'primary
   :native t
   :hidden t
   :prompt magent-agent--prompt-title
   :permission (magent-permission-from-config
                '((* . deny)))))

(defun magent-agent-types--summary ()
  "Create the summary agent (generates pull-request style summaries)."
  (magent-agent-info-create
   :name "summary"
   :description "Generate pull-request style summaries of conversations"
   :mode 'primary
   :native t
   :hidden t
   :prompt magent-agent--prompt-summary
   :permission (magent-permission-from-config
                '((* . deny)))))

(defun magent-agent-types-initialize ()
  "Initialize all built-in agents in the registry.
Returns list of agent info structures."
  (list
   (magent-agent-types--build)
   (magent-agent-types--plan)
   (magent-agent-types--general)
   (magent-agent-types--explore)
   (magent-agent-types--compaction)
   (magent-agent-types--title)
   (magent-agent-types--summary)))

(defun magent-agent-types-default-name ()
  "Return the default agent name."
  "build")

;; ──────────────────────────────────────────────────────────────────────
;;; Agent registry
;; ──────────────────────────────────────────────────────────────────────

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
    (when (and magent-load-custom-agents
               (require 'magent-agent-file nil t))
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
    (maphash (lambda (_name info)
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
      (magent-log "INFO default agent set to: %s" agent-name)
    (error "Agent not found: %s" agent-name)))

;; Provide feature aliases so that existing (require 'magent-agent-info)
;; and (require 'magent-agent-types) continue to work.
(provide 'magent-agent-info)
(provide 'magent-agent-types)
(provide 'magent-agent-registry)
;;; magent-agent-registry.el ends here
