;;; magent-agent-types.el --- Built-in agent definitions for OpenCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Built-in agent definitions matching opencode's native agents.
;; Includes: build, plan, general, explore, compaction, title, summary

;;; Code:

(require 'cl-lib)
(require 'magent-agent-info)
(require 'magent-permission)

;;; Built-in agent prompts

(defconst magent-agent--prompt-explore
  "You are a file search specialist. You excel at thoroughly navigating and exploring codebases.

Your strengths:
- Rapidly finding files using glob patterns
- Searching code and text with powerful regex patterns
- Reading and analyzing file contents

Guidelines:
- Use Glob for broad file pattern matching
- Use Grep for searching file contents with regex
- Use Read when you know the specific file path you need to read
- Use Bash for file operations like copying, moving, or listing directory contents
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
                   ("*" deny)
                   (".opencode/plan/*.md" allow)))))))

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
                '((todoread deny)
                  (todowrite deny))))))

(defun magent-agent-types--explore ()
  "Create the explore agent (codebase exploration specialist)."
  (magent-agent-info-create
   :name "explore"
   :description "Fast agent specialized for exploring codebases. Use this when you need to quickly find files by patterns (eg. \"src/components/**/*.tsx\"), search code for keywords (eg. \"API endpoints\"), or answer questions about the codebase (eg. \"how do API endpoints work?\"). When calling this agent, specify the desired thoroughness level: \"quick\" for basic searches, \"medium\" for moderate exploration, or \"very thorough\" for comprehensive analysis across multiple locations and naming conventions."
   :mode 'subagent
   :native t
   :prompt magent-agent--prompt-explore
   :permission (magent-permission-merge
               (magent-permission-defaults)
               (magent-permission-from-config
                '(("*" deny)
                  (grep allow)
                  (glob allow)
                  (list allow)
                  (bash allow)
                  (webfetch allow)
                  (websearch allow)
                  (codesearch allow)
                  (read allow))))))

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
                '(("*" deny)))))

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
                '(("*" deny)))))

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
                '(("*" deny)))))

;;; Agent registry initialization

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

(provide 'magent-agent-types)
;;; magent-agent-types.el ends here
