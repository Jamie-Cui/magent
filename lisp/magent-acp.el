;;; magent-acp.el --- In-process ACP adapter for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; ACP is a first-class Magent backend boundary.  The first transport is an
;; in-process `acp.el' client used by agent-shell; handlers are kept separate
;; from agent-shell-specific UI code.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'acp)
(require 'magent-approval)
(require 'magent-agent-registry)
(require 'magent-config)
(require 'magent-json)
(require 'magent-protocol)
(require 'magent-runtime-api)
(require 'magent-session)
(require 'magent-ledger)
(require 'magent-command)
(require 'magent-skills)

(defvar gptel-model)

(defconst magent-acp-protocol-version 1
  "ACP protocol version supported by Magent.")

(defun magent-acp--content-block (text)
  "Return ACP text content block for TEXT."
  `((type . "text")
    (text . ,(if (stringp text) text (format "%s" (or text ""))))))

(defun magent-acp--metadata-string (value)
  "Return VALUE as a string suitable for ACP metadata fields."
  (cond
   ((stringp value) value)
   ((null value) "")
   ((listp value)
    (string-join (mapcar (lambda (item) (format "%s" item)) value)
                 ", "))
   (t (format "%s" value))))

(defun magent-acp--tool-content (text)
  "Return ACP tool result content object for TEXT."
  `((content . ,(magent-acp--content-block text))))

(defun magent-acp--iso-time (time)
  "Return TIME formatted as an ISO-8601 UTC string."
  (format-time-string "%FT%TZ" time t))

(defun magent-acp--mode-entry (agent)
  "Return ACP mode entry for AGENT."
  `((id . ,(magent-agent-info-name agent))
    (name . ,(magent-agent-info-name agent))
    (description . ,(magent-acp--metadata-string
                     (magent-agent-info-description agent)))))

(defun magent-acp--modes (&optional runtime-session)
  "Return ACP modes object for RUNTIME-SESSION."
  `((currentModeId . ,(or (and runtime-session
                                (magent-runtime-session-agent-name
                                 runtime-session))
                           magent-default-agent))
    (availableModes . ,(vconcat
                        (mapcar #'magent-acp--mode-entry
                                (magent-agent-registry-primary-agents))))))

(defun magent-acp--models ()
  "Return ACP models object for current gptel model."
  (let ((model-id (format "%s" (or (and (boundp 'gptel-model) gptel-model)
                                   "gptel"))))
    `((currentModelId . ,model-id)
      (availableModels . [((modelId . ,model-id)
                           (name . ,model-id)
                           (description . "Current gptel model"))]))))

(defun magent-acp--effort-option-entry (effort)
  "Return ACP config option value entry for EFFORT."
  (let* ((option (magent-effort-option-or-auto effort))
         (value (symbol-name option))
         (name (pcase option
                 ('auto "Auto")
                 ('minimal "Minimal")
                 ('low "Low")
                 ('medium "Medium")
                 ('high "High")
                 ('xhigh "Extra high"))))
    `((value . ,value)
      (name . ,name)
      (description . ,(if (eq option 'auto)
                          "Use the provider or model default reasoning effort."
                        (format "Use %s reasoning effort." name))))))

(defun magent-acp--config-options (runtime-session)
  "Return ACP config options for RUNTIME-SESSION."
  (vector
   `((id . "effort")
     (name . "Thought level")
     (description . "Reasoning effort for future Magent turns in this session.")
     (category . "thought_level")
     (type . "select")
     (currentValue
      . ,(magent-effort-option-string
          (magent-runtime-session-effort-option runtime-session)))
     (options . ,(vconcat (mapcar #'magent-acp--effort-option-entry
                                  magent-effort-option-values))))
   `((id . "capabilities")
     (name . "Automatic capabilities")
     (description . "Resolve contextual instruction skills for future turns in this session.")
     (category . "other")
     (type . "select")
     (currentValue
      . ,(if (magent-runtime-session-capabilities-enabled-p runtime-session)
             "enabled"
           "disabled"))
     (options . [((value . "enabled")
                  (name . "Enabled")
                  (description . "Auto-activate matching instruction skills."))
                 ((value . "disabled")
                  (name . "Disabled")
                  (description . "Use only explicitly selected instruction skills."))]))))

(defun magent-acp--command-entry (command)
  "Return ACP available command entry for COMMAND."
  `((name . ,(magent-command-spec-name command))
    (description . ,(magent-acp--metadata-string
                     (magent-command-spec-description command)))))

(defun magent-acp--skill-command-name-p (name)
  "Return non-nil when NAME can be projected as an ACP skill command."
  (and (stringp name)
       (string-match-p "\\`[[:alnum:]_-]+\\'" name)))

(defun magent-acp--skill-command-entry (descriptor)
  "Return ACP available command entry for skill DESCRIPTOR, or nil."
  (let ((name (magent-skill-descriptor-name descriptor)))
    (if (magent-acp--skill-command-name-p name)
        `((name . ,(concat "$" name))
          (description . ,(magent-acp--metadata-string
                           (magent-skill-descriptor-description descriptor))))
      (magent-log "WARN skipped ACP skill command with invalid name: %S" name)
      nil)))

(defun magent-acp--available-commands (&optional runtime-session-or-scope)
  "Return ACP slash commands and skills for RUNTIME-SESSION-OR-SCOPE.
Instruction skills are projected as `$NAME' entries.  The ACP frontend adds
the leading slash, so agent-shell displays them as `/$NAME'."
  (let ((scope
         (if (magent-runtime-session-p runtime-session-or-scope)
             (magent-runtime-session-scope runtime-session-or-scope)
           runtime-session-or-scope)))
    (vconcat
     (append
      (mapcar #'magent-acp--command-entry (magent-command-list scope))
      (delq nil
            (mapcar
             #'magent-acp--skill-command-entry
             (magent-skills-list-descriptors scope 'instruction)))))))

(defun magent-acp--session-response (runtime-session)
  "Return common ACP session response for RUNTIME-SESSION."
  `((sessionId . ,(magent-runtime-session-id runtime-session))
    (modes . ,(magent-acp--modes runtime-session))
    (models . ,(magent-acp--models))
    (configOptions . ,(magent-acp--config-options runtime-session))))

(defun magent-acp--initialize-response ()
  "Return ACP initialize response."
  `((protocolVersion . ,magent-acp-protocol-version)
    (agentCapabilities . ((loadSession . t)
                          (promptCapabilities
                           . ((embeddedContext . t)
                              (image . :false)
                              (audio . :false)))))
    (sessionCapabilities . ((list . t)
                            (load . t)
                            (resume . t)
                            (fork . :false)))
    (modes . ,(magent-acp--modes))
    (models . ,(magent-acp--models))
    (authMethods . [])))

(defun magent-acp--alist-plist-get (value key)
  "Return KEY from VALUE supporting keyword and symbol alists."
  (or (map-elt value key)
      (and (keywordp key)
           (map-elt value (intern (substring (symbol-name key) 1))))
      (and (symbolp key)
           (map-elt value (intern (concat ":" (symbol-name key)))))))

(defun magent-acp--raw-input-key (key)
  "Return KEY as an ACP object key symbol."
  (cond
   ((keywordp key) (intern (substring (symbol-name key) 1)))
   ((symbolp key) key)
   ((stringp key) (intern key))
   (t (intern (format "%s" key)))))

(defun magent-acp--raw-input-value (value)
  "Return VALUE converted for an ACP `rawInput' object."
  (cond
   ((null value) :null)
   ((or (stringp value) (numberp value)
        (eq value t)
        (eq value :json-false)
        (eq value :null))
    value)
   ((symbolp value)
    (magent-json-safe-name value))
   ((magent-json--plist-p value)
    (magent-acp--raw-input-object value))
   ((magent-json--alist-p value)
    (magent-acp--raw-input-object value))
   ((hash-table-p value)
    (magent-acp--raw-input-object value))
   ((vectorp value)
    (vconcat (mapcar #'magent-acp--raw-input-value (append value nil))))
   ((consp value)
    (vconcat (mapcar #'magent-acp--raw-input-value value)))
   (t
    (format "%S" value))))

(defun magent-acp--raw-input-object (value)
  "Return VALUE as an agent-shell-compatible ACP `rawInput' object.

Magent stores tool arguments internally as keyword plists, while
agent-shell treats ACP `rawInput' as a JSON object represented by
an alist.  Normalize at the ACP boundary so agent-shell can read,
format, and transcript tool parameters without tripping over plist
keywords."
  (cond
   ((null value) [])
   ((and (vectorp value) (= (length value) 0)) [])
   ((magent-json--plist-p value)
    (let (out)
      (while value
        (let ((key (pop value))
              (val (pop value)))
          (when val
            (push (cons (magent-acp--raw-input-key key)
                        (magent-acp--raw-input-value val))
                  out))))
      (nreverse out)))
   ((magent-json--alist-p value)
    (delq nil
          (mapcar (lambda (entry)
                    (when (cdr entry)
                      (cons (magent-acp--raw-input-key (car entry))
                            (magent-acp--raw-input-value (cdr entry)))))
                  value)))
   ((hash-table-p value)
    (let (out)
      (maphash (lambda (key val)
                 (when val
                   (push (cons (magent-acp--raw-input-key key)
                               (magent-acp--raw-input-value val))
                         out)))
               value)
      (sort out (lambda (a b)
                  (string< (symbol-name (car a))
                           (symbol-name (car b)))))))
   (t
    `((value . ,(magent-acp--raw-input-value value))))))

(defun magent-acp--prompt-blocks (prompt)
  "Return PROMPT content blocks as a list."
  (cond
   ((vectorp prompt) (append prompt nil))
   ((listp prompt) prompt)
   ((stringp prompt) (list (magent-acp--content-block prompt)))
   (t nil)))

(defun magent-acp--resource-label (block type)
  "Return display label for ACP resource BLOCK of TYPE."
  (let ((resource (magent-acp--alist-plist-get block 'resource)))
    (or (magent-acp--alist-plist-get block 'uri)
        (magent-acp--alist-plist-get block 'name)
        (and resource
             (or (magent-acp--alist-plist-get resource 'uri)
                 (magent-acp--alist-plist-get resource 'name)))
        type)))

(defun magent-acp--normalize-resource-block (block type)
  "Return normalized ACP resource BLOCK of TYPE."
  (let* ((resource (magent-acp--alist-plist-get block 'resource))
         (uri (or (magent-acp--alist-plist-get block 'uri)
                  (and resource
                       (magent-acp--alist-plist-get resource 'uri))))
         (name (or (magent-acp--alist-plist-get block 'name)
                   (and resource
                        (magent-acp--alist-plist-get resource 'name))))
         (mime-type (or (magent-acp--alist-plist-get block 'mimeType)
                        (and resource
                             (magent-acp--alist-plist-get resource 'mimeType))))
         (text (or (magent-acp--alist-plist-get block 'text)
                   (and resource
                        (magent-acp--alist-plist-get resource 'text))))
         (blob (and resource
                    (magent-acp--alist-plist-get resource 'blob))))
    (when blob
      (error "Unsupported binary ACP resource: %s" (or uri name type)))
    (if (equal type "resource")
        `((type . "resource")
          (resource . ((uri . ,uri)
                       ,@(when name `((name . ,name)))
                       ,@(when mime-type `((mimeType . ,mime-type)))
                       ,@(when text `((text . ,text))))))
      `((type . "resource_link")
        (uri . ,uri)
        ,@(when name `((name . ,name)))
        ,@(when mime-type `((mimeType . ,mime-type)))
        ,@(when-let* ((size (magent-acp--alist-plist-get block 'size)))
            `((size . ,size)))))))

(defun magent-acp--normalize-prompt-block (block)
  "Return normalized ACP prompt BLOCK."
  (let ((type (magent-acp--alist-plist-get block 'type)))
    (cond
     ((or (null type) (equal type "text"))
      (magent-acp--content-block
       (or (magent-acp--alist-plist-get block 'text) "")))
     ((member type '("resource" "resource_link" "file"))
      (magent-acp--normalize-resource-block
       block (if (equal type "resource") "resource" "resource_link")))
     (t
      (error "Unsupported ACP prompt content block: %s" type)))))

(defun magent-acp--resource-path (block)
  "Return local file path named by normalized resource BLOCK, or nil."
  (let* ((type (magent-acp--alist-plist-get block 'type))
         (resource (magent-acp--alist-plist-get block 'resource))
         (uri (if (equal type "resource")
                  (and resource
                       (magent-acp--alist-plist-get resource 'uri))
                (magent-acp--alist-plist-get block 'uri))))
    (when (and (stringp uri) (string-prefix-p "file://" uri))
      (url-unhex-string (substring uri 7)))))

(defun magent-acp--prompt-input (prompt)
  "Return normalized structured input parsed from ACP PROMPT."
  (let* ((blocks (mapcar #'magent-acp--normalize-prompt-block
                         (magent-acp--prompt-blocks prompt)))
         (text (string-trim
                (mapconcat
                 (lambda (block)
                   (if (equal (magent-acp--alist-plist-get block 'type) "text")
                       (or (magent-acp--alist-plist-get block 'text) "")
                     ""))
                 blocks "")))
         (resource-blocks
          (cl-remove-if
           (lambda (block)
             (equal (magent-acp--alist-plist-get block 'type) "text"))
           blocks))
         (paths (delete-dups
                 (delq nil (mapcar #'magent-acp--resource-path
                                   resource-blocks))))
         (labels
          (mapcar (lambda (block)
                    (magent-acp--resource-label
                     block (magent-acp--alist-plist-get block 'type)))
                  resource-blocks))
         (display-text
          (string-trim
           (string-join
            (delq nil
                  (list (and (not (string-empty-p text)) text)
                        (and labels
                             (format "[Attached: %s]"
                                     (string-join labels ", ")))))
            "\n"))))
    (list :text text
          :display-text display-text
          :content-blocks (vconcat blocks)
          :resource-blocks resource-blocks
          :context (list :file-path (car paths)
                         :resource-paths paths))))

(defun magent-acp--prompt-input-with-text (input text &optional resources-before)
  "Return INPUT with text replaced by TEXT and RESOURCES-BEFORE prepended.
RESOURCES-BEFORE appear before the frontend resources already stored in INPUT."
  (let* ((resources (append resources-before
                            (plist-get input :resource-blocks)))
         (blocks (vconcat (cons (magent-acp--content-block text) resources)))
         (labels
          (mapcar (lambda (block)
                    (magent-acp--resource-label
                     block (magent-acp--alist-plist-get block 'type)))
                  resources))
         (display-text
          (string-trim
           (string-join
            (delq nil
                  (list text
                        (and labels
                             (format "[Attached: %s]"
                                     (string-join labels ", ")))))
            "\n")))
         (result (copy-sequence input)))
    (setq result (plist-put result :text text)
          result (plist-put result :display-text display-text)
          result (plist-put result :content-blocks blocks))
    result))

(defun magent-acp--slash-command (prompt &optional runtime-session-or-scope)
  "Return slash command parsed from PROMPT for RUNTIME-SESSION-OR-SCOPE."
  (let ((scope
         (if (magent-runtime-session-p runtime-session-or-scope)
             (magent-runtime-session-scope runtime-session-or-scope)
           runtime-session-or-scope)))
    (when-let* ((parsed (magent-command-parse prompt scope)))
      (list :kind 'command
            :spec (car parsed)
            :name (magent-command-spec-name (car parsed))
            :argument (cdr parsed)))))

(defun magent-acp--skill-command (prompt &optional runtime-session-or-scope)
  "Return explicit skill invocation parsed from PROMPT for session scope.
The accepted wire form is `/$NAME' followed by an optional instruction.
Unknown or unavailable instruction skills fail here instead of reaching the
model as ambiguous prompt text."
  (let* ((scope
          (if (magent-runtime-session-p runtime-session-or-scope)
              (magent-runtime-session-scope runtime-session-or-scope)
            runtime-session-or-scope))
         (trimmed (string-trim (or prompt ""))))
    (when (string-match
           "\\`/\\$\\([[:alnum:]_-]+\\)\\(?:[[:space:]]+\\(.*\\)\\)?\\'"
           trimmed)
      (let* ((name (match-string 1 trimmed))
             (descriptor (magent-skills-resolve-descriptor name scope)))
        (unless (and descriptor
                     (eq (magent-skill-descriptor-type descriptor)
                         'instruction))
          (user-error
           "Magent: unknown or unavailable instruction skill '$%s' in this session"
           name))
        (list :kind 'skill
              :name name
              :argument (string-trim (or (match-string 2 trimmed) ""))
              :descriptor descriptor)))))

(defun magent-acp--skill-selection (runtime-session skill-name)
  "Return pending skills plus explicit SKILL-NAME for RUNTIME-SESSION."
  (magent-skills-dedupe-names
   (append (magent-runtime-session-pending-skills runtime-session)
           (list skill-name))))

(defun magent-acp--command-submission-adapter (input)
  "Return a command submission adapter preserving structured ACP INPUT."
  (lambda (prompt resource-blocks)
    (let ((adapted
           (magent-acp--prompt-input-with-text
            input prompt resource-blocks)))
      (list :prompt (plist-get adapted :display-text)
            :content-blocks (plist-get adapted :content-blocks)))))

(defun magent-acp--notify-agent-message (client session-id text)
  "Send one agent message TEXT through CLIENT for SESSION-ID."
  (magent-acp--session-update
   client session-id
   `((sessionUpdate . "agent_message_chunk")
     (content . ,(magent-acp--content-block text)))))

(defun magent-acp--complete-prompt-request (on-success status result)
  "Complete ACP prompt callback ON-SUCCESS from runtime STATUS and RESULT."
  (funcall
   on-success
   `((stopReason . ,(magent-acp--stop-reason status result))
     ,@(unless (eq status 'completed)
         `((error . ,(magent-agent-result-content-string result)))))))

(defun magent-acp--notify (client method params)
  "Deliver incoming ACP notification METHOD PARAMS to CLIENT subscribers.

Each handler runs inside the CLIENT's context buffer (via
`magent-acp--callback-buffer'), so that async callbacks from
process sentinels or timers don't strand the handler in a wrong
buffer where agent-shell's buffer-local state is inaccessible."
  (let ((notification `((method . ,method)
                        (params . ,params)))
        (buffer (magent-acp--callback-buffer client nil)))
    (dolist (handler (map-elt client :notification-handlers))
      (if buffer
          (with-current-buffer buffer
            (funcall handler notification))
        (funcall handler notification)))))

(defun magent-acp--request (client id method params)
  "Deliver incoming ACP request METHOD PARAMS with ID to CLIENT subscribers.

Each handler runs inside the CLIENT's context buffer (via
`magent-acp--callback-buffer')."
  (let ((request `((id . ,id)
                   (method . ,method)
                   (params . ,params)))
        (buffer (magent-acp--callback-buffer client nil)))
    (dolist (handler (map-elt client :request-handlers))
      (if buffer
          (with-current-buffer buffer
            (funcall handler request))
        (funcall handler request)))))

(defun magent-acp--session-update (client session-id update)
  "Send ACP session/update UPDATE for SESSION-ID through CLIENT."
  (magent-acp--notify
   client "session/update"
   `((sessionId . ,session-id)
     (update . ,update))))

(defun magent-acp--notify-available-commands (client runtime-session)
  "Notify CLIENT of available slash commands for RUNTIME-SESSION."
  (magent-acp--session-update
   client
   (magent-runtime-session-id runtime-session)
   `((sessionUpdate . "available_commands_update")
     (availableCommands
      . ,(magent-acp--available-commands runtime-session)))))

(defun magent-acp--notify-session-info (client runtime-session)
  "Notify CLIENT of RUNTIME-SESSION's current display information."
  (when-let* ((title (magent-runtime-session-title runtime-session)))
    (magent-acp--session-update
     client
     (magent-runtime-session-id runtime-session)
     `((sessionUpdate . "session_info_update")
       (title . ,title)))))

(defun magent-acp--refresh-available-commands ()
  "Publish the current command registry to every bound live ACP session."
  (when (boundp 'magent-acp--client-session-scopes)
    (maphash
     (lambda (client bindings)
       (maphash
        (lambda (session-id scope)
          (when-let* ((runtime-session
                      (magent-runtime-session-from-id session-id scope)))
            (condition-case err
                (magent-acp--notify-available-commands client runtime-session)
              (error
               (magent-log "WARN failed to refresh ACP commands: %s"
                           (error-message-string err))))))
        bindings))
     magent-acp--client-session-scopes)))

(defun magent-acp--session-success (client runtime-session on-success)
  "Call ON-SUCCESS with RUNTIME-SESSION response and command metadata."
  (magent-acp--bind-client-session client runtime-session)
  (funcall on-success (magent-acp--session-response runtime-session))
  (magent-acp--notify-available-commands client runtime-session)
  (magent-acp--notify-session-info client runtime-session))

(defun magent-acp--tool-kind (kind)
  "Return ACP tool kind string for Magent KIND."
  (pcase kind
    ((or 'read 'grep 'glob) "read")
    ((or 'write 'edit) "edit")
    ((or 'bash 'emacs_eval) "execute")
    (_ "other")))

(defun magent-acp--observer (client session-id)
  "Return runtime observer that converts Magent events to ACP updates."
  (let ((stream-kind nil)
        (stream-start-p t))
    (cl-labels
        ((reset-stream ()
           (setq stream-kind nil
                 stream-start-p t))
         (stream-text (kind text)
           (unless (eq stream-kind kind)
             (setq stream-kind kind
                   stream-start-p t))
           (let ((text (if (stringp text) text (format "%s" (or text "")))))
             (if stream-start-p
                 (let ((trimmed (string-trim-left text)))
                   (unless (string-empty-p trimmed)
                     (setq stream-start-p nil)
                     trimmed))
               text))))
      (lambda (event)
        (pcase (plist-get event :type)
          ('assistant-delta
           (when-let* ((text (stream-text 'assistant (plist-get event :text))))
             (magent-acp--session-update
              client session-id
              `((sessionUpdate . "agent_message_chunk")
                (content . ,(magent-acp--content-block text))))))
          ('reasoning-delta
           (when (eq magent-include-reasoning t)
             (when-let* ((text (stream-text 'reasoning
                                           (plist-get event :text))))
               (magent-acp--session-update
                client session-id
                `((sessionUpdate . "agent_thought_chunk")
                  (content . ,(magent-acp--content-block text)))))))
          ('command-progress
           (reset-stream)
           (magent-acp--notify-agent-message
            client session-id (or (plist-get event :text) "")))
          ('tool-call-start
           (reset-stream)
           (magent-acp--session-update
            client session-id
            `((sessionUpdate . "tool_call")
              (toolCallId . ,(plist-get event :tool-id))
              (title . ,(or (plist-get event :summary)
                            (plist-get event :name)
                            "tool"))
              (kind . ,(magent-acp--tool-kind (plist-get event :kind)))
              (status . "in_progress")
              (rawInput . ,(magent-acp--raw-input-object
                             (plist-get event :raw-input))))))
          ('tool-call-complete
           (reset-stream)
           (condition-case err
               (magent-acp--session-update
                client session-id
                `((sessionUpdate . "tool_call_update")
                  (toolCallId . ,(or (plist-get event :tool-id) "unknown"))
                  (status . ,(if (eq (plist-get event :status) 'failed)
                                 "failed"
                               "completed"))
                  (content . ,(vector
                               (magent-acp--tool-content
                                (or (plist-get event :output-preview) ""))))))
             (error
              ;; If the full notification path crashes (e.g. due to
              ;; agent-shell's alist merge tripping over plist data
              ;; stored from the prior tool_call), send a minimal
              ;; fallback so the tool-call status always transitions
              ;; from in_progress/… to a terminal icon (✓/✗).
              (magent-log "ERROR acp tool-call-complete failed, sending fallback: %s"
                          (error-message-string err))
              (condition-case nil
                  (magent-acp--session-update
                   client session-id
                   `((sessionUpdate . "tool_call_update")
                     (toolCallId . ,(or (plist-get event :tool-id) "unknown"))
                     (title . "tool")
                     (status . "completed")
                     (content . [])))
                (error nil)))))
          ('turn-error
           (reset-stream)
           (magent-acp--session-update
            client session-id
            `((sessionUpdate . "agent_message_chunk")
              (content . ,(magent-acp--content-block
                           (format "Error: %s"
                                   (or (plist-get event :message) "")))))))
          ('turn-complete
           (reset-stream)
           (when-let* ((scope (magent-acp--client-session-scope
                               client session-id))
                       (runtime-session
                        (magent-runtime-session-from-id session-id scope)))
             (magent-acp--notify-session-info client runtime-session))))))))

(defun magent-acp--permission-options ()
  "Return ACP permission options supported by agent-shell."
  [((optionId . "allow_once")
    (kind . "allow_once")
    (name . "Allow"))
   ((optionId . "allow_always")
    (kind . "allow_always")
    (name . "Always allow"))
   ((optionId . "reject_once")
    (kind . "reject_once")
    (name . "Reject"))])

(defun magent-acp--approval-provider (client session-id)
  "Return Magent approval provider backed by ACP CLIENT."
  (lambda (request)
    (let* ((request-id (plist-get request :request-id))
           (tool-name (or (plist-get request :tool-name) "tool"))
           (summary (or (plist-get request :summary) tool-name))
           (args (or (plist-get request :args) [])))
      (magent-acp--request
       client request-id "session/request_permission"
       `((sessionId . ,session-id)
         (toolCall . ((toolCallId . ,request-id)
                      (title . ,summary)
                      (kind . ,(magent-acp--tool-kind
                                (plist-get request :perm-key)))
                      (status . "pending")
                      (rawInput . ,(magent-acp--raw-input-object args))))
         (options . ,(magent-acp--permission-options)))))))

(defun magent-acp--permission-decision (response)
  "Return Magent approval decision from ACP permission RESPONSE."
  (let ((outcome (map-nested-elt response '(:result outcome outcome)))
        (option-id (map-nested-elt response '(:result outcome optionId))))
    (cond
     ((equal outcome "cancelled") 'deny-once)
     ((equal option-id "allow_once") 'allow-once)
     ((equal option-id "allow_always") 'allow-session)
     ((equal option-id "reject_once") 'deny-once)
     (t 'deny-once))))

(cl-defun magent-acp--response-sender (&key client response)
  "Handle ACP RESPONSE sent by the client to Magent."
  (ignore client)
  (let ((request-id (map-elt response :request-id)))
    (when request-id
      (magent-approval-resolve-request
       request-id
       (magent-acp--permission-decision response)))))

(defun magent-acp--callback-buffer (client buffer)
  "Return callback buffer for CLIENT, preferring explicit BUFFER."
  (or (and (buffer-live-p buffer) buffer)
      (let ((context-buffer (map-elt client :context-buffer)))
        (and (buffer-live-p context-buffer) context-buffer))))

(defvar magent-acp--client-session-scopes
  (make-hash-table :test #'eq :weakness 'key)
  "Frozen session-id to scope bindings for each in-process ACP client.")

(defun magent-acp--client-default-scope (client)
  "Return the current buffer-derived scope for unbound ACP CLIENT."
  (when-let* ((buffer (magent-acp--callback-buffer client nil)))
    (with-current-buffer buffer
      (magent-session-scope-from-directory default-directory))))

(defun magent-acp--bind-client-session (client runtime-session)
  "Bind CLIENT's RUNTIME-SESSION id to its exact creation/load scope."
  (let* ((session-id (magent-runtime-session-id runtime-session))
         (scope (magent-runtime-session-scope runtime-session))
         (bindings
          (or (gethash client magent-acp--client-session-scopes)
              (let ((table (make-hash-table :test #'equal)))
                (puthash client table magent-acp--client-session-scopes)
                table))))
    (puthash session-id scope bindings)
    scope))

(defun magent-acp--client-session-scope (client session-id)
  "Return CLIENT's frozen scope for SESSION-ID, or nil when unbound."
  (when-let* ((bindings (gethash client magent-acp--client-session-scopes)))
    (gethash session-id bindings)))

(defun magent-acp--request-session-scope (client session-id)
  "Return CLIENT's stable scope for SESSION-ID.
The buffer-derived fallback preserves clients that survived a source reload;
new and resumed sessions are always explicitly bound before use."
  (or (magent-acp--client-session-scope client session-id)
      (magent-acp--client-default-scope client)))

(defun magent-acp--wrap-callback (client buffer callback)
  "Return CALLBACK wrapped to run in CLIENT/BUFFER context."
  (when callback
    (lambda (&rest args)
      (with-temp-buffer
        (with-current-buffer (or (magent-acp--callback-buffer client buffer)
                                 (current-buffer))
          (apply callback args))))))

(defun magent-acp--call-failure (on-failure error &optional raw-message)
  "Call ON-FAILURE with ERROR and optional RAW-MESSAGE."
  (when on-failure
    (let* ((arity (func-arity on-failure))
           (min (car arity))
           (max (cdr arity)))
      (cond
       ((and (<= min 2)
             (or (eq max 'many)
                 (null max)
                 (and (numberp max) (>= max 2))))
        (funcall on-failure error raw-message))
       ((and (<= min 1)
             (or (eq max 'many)
                 (null max)
                 (and (numberp max) (>= max 1))))
        (funcall on-failure error))
       (t
        (funcall on-failure))))))

(defun magent-acp--scope-equal-p (left right)
  "Return non-nil when session scopes LEFT and RIGHT are equal."
  (cond
   ((and (eq left 'global) (eq right 'global)) t)
   ((and (stringp left) (stringp right))
    (equal (file-truename (directory-file-name left))
           (file-truename (directory-file-name right))))
   (t nil)))

(defun magent-acp--scope-for-cwd (cwd)
  "Return the Magent session scope for ACP CWD."
  (unless (and (stringp cwd) (not (string-empty-p cwd)))
    (error "ACP session cwd is required"))
  (magent-session-scope-from-directory cwd))

(defun magent-acp--file-scope (file)
  "Return saved session scope represented by FILE metadata."
  (let ((metadata (magent-session--read-file-metadata-cached file)))
    (if (eq (plist-get metadata :scope) 'global)
        'global
      (plist-get metadata :project-root))))

(defun magent-acp--session-file-by-id (session-id &optional expected-scope)
  "Return saved session file for SESSION-ID in EXPECTED-SCOPE, or nil."
  (magent-session-validate-id session-id)
  (let ((matches
         (cl-remove-if-not
          (lambda (file)
            (and (equal (file-name-sans-extension
                         (file-name-nondirectory file))
                        session-id)
                 (or (null expected-scope)
                     (magent-acp--scope-equal-p
                      (magent-acp--file-scope file) expected-scope))))
          (magent-session-list-files))))
    ;; Without an exact scope, ambiguity is safer than silently loading another
    ;; project's conversation.  Duplicate files within one scope are likewise
    ;; treated as corrupt/ambiguous state.
    (and (= (length matches) 1) (car matches))))

(defun magent-acp--runtime-session-by-id (session-id &optional expected-scope)
  "Return runtime SESSION-ID in EXPECTED-SCOPE, loading it if needed."
  (magent-session-validate-id session-id)
  (or (when-let* ((runtime-session
                   (if expected-scope
                       (magent-runtime-session-from-id
                        session-id expected-scope)
                     (magent-runtime-session-from-id session-id)))
                  ((or (null expected-scope)
                       (magent-acp--scope-equal-p
                        (magent-runtime-session-scope runtime-session)
                        expected-scope))))
        runtime-session)
      (when-let* ((file (magent-acp--session-file-by-id
                         session-id expected-scope)))
        (magent-runtime-load-session-file file))))

(defun magent-acp--runtime-session-for-scope (session-id expected-scope)
  "Return SESSION-ID restricted to EXPECTED-SCOPE when it is available."
  (if expected-scope
      (magent-acp--runtime-session-by-id session-id expected-scope)
    (magent-acp--runtime-session-by-id session-id)))

(defun magent-acp--load-candidate (session-id scope)
  "Return a read-only load candidate for SESSION-ID in exact SCOPE.
The result contains either `:runtime-session' or `:session'.  Validating it
does not activate overlays or install a session into the runtime registry."
  (magent-session-validate-id session-id)
  (if-let* ((runtime-session
             (magent-runtime-session-from-id session-id scope)))
      (list :runtime-session runtime-session)
    (when-let* ((file (magent-acp--session-file-by-id session-id scope))
                (loaded (magent-session-read-file file))
                (loaded-scope (plist-get loaded :scope))
                ((magent-acp--scope-equal-p loaded-scope scope))
                (session (plist-get loaded :session)))
      (list :session session))))

(defun magent-acp--session-list-response (cwd)
  "Return ACP session/list response filtered to CWD's exact scope."
  (let ((scope (magent-acp--scope-for-cwd cwd)))
    `((sessions . ,(vconcat
                    (mapcar
                     (lambda (entry)
                       (let ((updated-at (seconds-to-time
                                          (plist-get entry :updated-at))))
                         `((sessionId . ,(plist-get entry :id))
                           (cwd . ,(or (plist-get entry :project-root) cwd))
                           (title . ,(or (plist-get entry :title)
                                         (plist-get entry :id)))
                           (createdAt . ,(magent-acp--iso-time updated-at))
                           (updatedAt . ,(magent-acp--iso-time updated-at)))))
                     (cl-remove-if-not
                      (lambda (entry)
                        (magent-acp--scope-equal-p
                         (plist-get entry :scope) scope))
                      (magent-runtime-list-sessions-for-scope scope))))))))

(defun magent-acp--emit-item-replay (client session-id item)
  "Replay ledger ITEM to CLIENT for SESSION-ID."
  (pcase (magent-thread-item-type item)
    ('message
     (pcase (magent-thread-item-role item)
       ('user
        (magent-acp--session-update
         client session-id
         `((sessionUpdate . "user_message_chunk")
           (content . ,(magent-acp--content-block
                        (magent-thread-item-content item))))))
       ('assistant
        (magent-acp--session-update
         client session-id
         `((sessionUpdate . "agent_message_chunk")
           (content . ,(magent-acp--content-block
                        (magent-thread-item-content item))))))))
    ('reasoning
     (when (eq magent-include-reasoning t)
       (magent-acp--session-update
        client session-id
        `((sessionUpdate . "agent_thought_chunk")
          (content . ,(magent-acp--content-block
                       (magent-thread-item-content item)))))))
    ('tool
     (let ((tool-id (or (magent-thread-item-call-id item)
                        (magent-thread-item-id item))))
       (magent-acp--session-update
        client session-id
        `((sessionUpdate . "tool_call")
          (toolCallId . ,tool-id)
          (title . ,(or (magent-thread-item-name item) "tool"))
          (kind . "other")
          (status . "completed")
          (rawInput . ,(magent-acp--raw-input-object
                         (magent-thread-item-input item)))))
       (magent-acp--session-update
        client session-id
        `((sessionUpdate . "tool_call_update")
          (toolCallId . ,tool-id)
          (title . ,(or (magent-thread-item-name item) "tool"))
          (status . ,(if (eq (magent-thread-item-status item) 'failed)
                         "failed"
                       "completed"))
          (content . ,(vector
                       (magent-acp--tool-content
                        (format "%s" (or (magent-thread-item-output item)
                                         (magent-thread-item-error item)
                                         "")))))))))))

(defun magent-acp--replay-session (client runtime-session)
  "Replay RUNTIME-SESSION ledger to CLIENT."
  (let* ((session-id (magent-runtime-session-id runtime-session))
         (thread (magent-session-thread-ledger
                  (magent-runtime-session-magent-session runtime-session))))
    (dolist (turn (magent-thread-turns thread))
      (dolist (item (magent-thread-turn-items turn))
        (magent-acp--emit-item-replay client session-id item)))))

(defun magent-acp--handle-set-mode (params &optional expected-scope)
  "Handle ACP session/set_mode PARAMS."
  (let* ((session-id (map-elt params 'sessionId))
         (mode-id (map-elt params 'modeId))
         (runtime-session
          (magent-acp--runtime-session-for-scope session-id expected-scope)))
    (unless runtime-session
      (error "Unknown session: %s" session-id))
    (magent-runtime-session-set-agent runtime-session mode-id)
    (magent-acp--session-response runtime-session)))

(defun magent-acp--handle-set-model (params &optional expected-scope)
  "Handle ACP session/set_model PARAMS.
Magent uses gptel as the provider boundary, so the first ACP backend only
advertises the currently configured gptel model.  Accepting that model keeps
agent-shell's default bootstrap flow working without adding ACP-side model
switching semantics."
  (let* ((session-id (map-elt params 'sessionId))
         (model-id (map-elt params 'modelId))
         (runtime-session
          (magent-acp--runtime-session-for-scope session-id expected-scope))
         (current-model-id (format "%s" (or (and (boundp 'gptel-model)
                                                 gptel-model)
                                            "gptel"))))
    (unless runtime-session
      (error "Unknown session: %s" session-id))
    (unless (equal model-id current-model-id)
      (error "Unknown Magent model: %s" model-id))
    (magent-acp--session-response runtime-session)))

(defun magent-acp--handle-set-config-option (params &optional expected-scope)
  "Handle ACP session/set_config_option PARAMS."
  (let* ((session-id (map-elt params 'sessionId))
         (config-id (map-elt params 'configId))
         (value (map-elt params 'value))
         (runtime-session
          (magent-acp--runtime-session-for-scope session-id expected-scope)))
    (unless runtime-session
      (error "Unknown session: %s" session-id))
    (pcase config-id
      ("effort"
       (magent-runtime-session-set-effort runtime-session value))
      ("capabilities"
       (unless (member value '("enabled" "disabled"))
         (error "Invalid capabilities option: %s" value))
       (magent-runtime-session-set-capabilities-enabled
        runtime-session (equal value "enabled")))
      (_
       (error "Unknown Magent config option: %s" config-id)))
    (magent-acp--session-response runtime-session)))

(defun magent-acp--stop-reason (status result)
  "Return ACP stopReason for Magent STATUS and RESULT."
  (pcase status
    ('completed "end_turn")
    ('cancelled "cancelled")
    (_
     (let ((failure-status
            (and (magent-agent-result-p result)
                 (plist-get (magent-agent-result-metadata result) :status))))
       (pcase failure-status
         ('sampling-limit "max_turn_requests")
         ('max-tokens "max_tokens")
         ('refusal "refusal")
         (_ "error"))))))

(defun magent-acp--handle-request (client request on-success on-failure)
  "Handle ACP REQUEST from CLIENT."
  (condition-case err
      (let* ((method (map-elt request :method))
             (params (or (map-elt request :params) nil)))
        (pcase method
          ("initialize"
           (funcall on-success (magent-acp--initialize-response)))
          ("session/new"
           (let* ((cwd (or (map-elt params 'cwd) default-directory))
                  (scope (magent-session-scope-from-directory cwd))
                  (_ (magent-runtime-prepare-command-context scope))
                  (runtime-session (magent-runtime-session-new scope)))
             (magent-acp--session-success client runtime-session on-success)))
          ("session/list"
           (let ((cwd (or (map-elt params 'cwd) default-directory)))
             (funcall on-success (magent-acp--session-list-response cwd))))
          ((or "session/load" "session/resume")
           (let* ((session-id (map-elt params 'sessionId))
                  (cwd (or (map-elt params 'cwd) default-directory))
                  (scope (magent-acp--scope-for-cwd cwd))
                  (candidate (magent-acp--load-candidate session-id scope)))
             (unless candidate
               (error "Unknown session in requested cwd: %s" session-id))
             ;; Preflight exact-session replacement before changing overlays.
             (let ((candidate-session
                    (or (plist-get candidate :session)
                        (when-let* ((runtime-session
                                    (plist-get candidate :runtime-session)))
                          (magent-runtime-session-magent-session
                           runtime-session)))))
               (magent-runtime-session-ensure-registerable
                scope candidate-session))
             (magent-runtime-prepare-command-context scope)
             (let ((runtime-session
                    (or (plist-get candidate :runtime-session)
                        (magent-runtime-session-register
                         scope (plist-get candidate :session)))))
               (when (equal method "session/load")
                 (magent-acp--replay-session client runtime-session))
               (magent-acp--session-success
                client runtime-session on-success))))
          ("session/set_mode"
           (let ((session-id (map-elt params 'sessionId)))
             (funcall
              on-success
              (magent-acp--handle-set-mode
               params (magent-acp--request-session-scope client session-id)))))
          ("session/set_model"
           (let ((session-id (map-elt params 'sessionId)))
             (funcall
              on-success
              (magent-acp--handle-set-model
               params (magent-acp--request-session-scope client session-id)))))
          ("session/set_config_option"
           (let ((session-id (map-elt params 'sessionId)))
             (funcall
              on-success
              (magent-acp--handle-set-config-option
               params (magent-acp--request-session-scope client session-id)))))
          ("session/prompt"
           (let* ((session-id (map-elt params 'sessionId))
                  (runtime-session
                   (magent-acp--runtime-session-for-scope
                    session-id
                    (magent-acp--request-session-scope client session-id)))
                  (input (magent-acp--prompt-input (map-elt params 'prompt)))
                  (instruction-text (plist-get input :text))
                  (command
                   (and runtime-session
                        (magent-acp--slash-command
                         instruction-text runtime-session)))
                  (skill-command
                   (and runtime-session
                        (not command)
                        (magent-acp--skill-command
                         instruction-text runtime-session)))
                  (resources (plist-get input :resource-blocks)))
             (unless runtime-session
               (error "Unknown session: %s" session-id))
             (cond
              (command
               (magent-command-invoke
                (plist-get command :spec) runtime-session
                :raw-input instruction-text
                :argument (plist-get command :argument)
                :request-context
                (append (plist-get input :context)
                        (list :capabilities-enabled
                              (magent-runtime-session-capabilities-enabled-p
                               runtime-session)))
                :resource-blocks resources
                :observer (magent-acp--observer client session-id)
                :approval-provider
                (magent-acp--approval-provider client session-id)
                :submission-adapter
                (magent-acp--command-submission-adapter input)
                :on-complete
                (lambda (status result)
                  (magent-acp--complete-prompt-request
                   on-success status result))))
              (skill-command
               (let ((skill-name (plist-get skill-command :name)))
                 (magent-runtime-submit
                  runtime-session (plist-get input :display-text)
                  :context
                  (append
                   (plist-get input :context)
                   (list :capabilities-enabled
                         (magent-runtime-session-capabilities-enabled-p
                          runtime-session)))
                  :skills
                  (magent-acp--skill-selection runtime-session skill-name)
                  :turn-metadata
                  (list :content-blocks (plist-get input :content-blocks)
                        :explicit-skill skill-name
                        :skill-invocation 'acp-command)
                  :observer (magent-acp--observer client session-id)
                  :approval-provider
                  (magent-acp--approval-provider client session-id)
                  :on-complete
                  (lambda (status result)
                    (magent-acp--complete-prompt-request
                     on-success status result)))))
              (t
               (magent-runtime-submit
                runtime-session (plist-get input :display-text)
                :context
                (append (plist-get input :context)
                        (list :capabilities-enabled
                              (magent-runtime-session-capabilities-enabled-p
                               runtime-session)))
                :turn-metadata
                (list :content-blocks (plist-get input :content-blocks))
                :observer (magent-acp--observer client session-id)
                :approval-provider
                (magent-acp--approval-provider client session-id)
                :on-complete
                (lambda (status result)
                  (magent-acp--complete-prompt-request
                   on-success status result)))))))
          (_
           (error "Unsupported ACP method: %s" method))))
    (error
     (when on-failure
       (magent-acp--call-failure
        on-failure
        (acp-make-error
         :code -32602
         :message (error-message-string err)))))))

(cl-defun magent-acp--request-sender
    (&key client request buffer on-success on-failure sync)
  "Send ACP REQUEST to Magent in-process.
Responses are dispatched on the next event loop via `run-at-time', so
this in-process transport behaves like a real subprocess-backed ACP
agent whose replies arrive asynchronously.  Callers such as agent-shell
subscribe to session events immediately after issuing a request and rely
on the response landing after those subscriptions register; a synchronous
callback would race that registration (e.g. leaving the \"Loading...\"
active message running forever)."
  (ignore sync)
  (run-at-time 0 nil
               (lambda ()
                 (magent-acp--handle-request
                  client request
                  (magent-acp--wrap-callback client buffer on-success)
                  (magent-acp--wrap-callback client buffer on-failure)))))

(cl-defun magent-acp--notification-sender (&key client notification sync)
  "Handle ACP NOTIFICATION from agent-shell."
  (ignore sync)
  (let ((method (map-elt notification :method))
        (params (map-elt notification :params)))
    (pcase method
      ("session/cancel"
       (when-let* ((session-id (map-elt params 'sessionId)))
         (let* ((scope (magent-acp--request-session-scope client session-id))
                (runtime-session
                 (magent-acp--runtime-session-for-scope session-id scope)))
           (when runtime-session
             (magent-log "INFO ACP session cancel: session=%s reason=%s"
                         session-id
                         (or (map-elt params 'reason) ""))
             (or (magent-command-cancel-session runtime-session)
                 (magent-runtime-cancel runtime-session))))))
      (_
       (magent-log "WARN unsupported ACP notification: %s" method)))))

(defun magent-acp--ensure-command-refresh-hook ()
  "Register command refresh after the ACP frontend is first activated."
  (add-hook 'magent-command-registry-changed-hook
            #'magent-acp--refresh-available-commands))

;;;###autoload
(defun magent-acp-make-client (&optional context-buffer)
  "Return an in-process ACP client for Magent.
CONTEXT-BUFFER is passed to `acp-make-client'."
  (magent-acp--ensure-command-refresh-hook)
  (acp-make-client
   :context-buffer context-buffer
   :command "cat"
   :request-sender #'magent-acp--request-sender
   :notification-sender #'magent-acp--notification-sender
   :response-sender #'magent-acp--response-sender))

(provide 'magent-acp)
;;; magent-acp.el ends here
