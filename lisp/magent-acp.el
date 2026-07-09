;;; magent-acp.el --- In-process ACP adapter for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ACP is a first-class Magent backend boundary.  The first transport is an
;; in-process `acp.el' client used by agent-shell; handlers are kept separate
;; from agent-shell-specific UI code.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'acp)
(require 'magent-approval)
(require 'magent-agent-registry)
(require 'magent-config)
(require 'magent-json)
(require 'magent-protocol)
(require 'magent-runtime-api)
(require 'magent-session)
(require 'magent-ledger)
(require 'magent-skills)

(defvar gptel-model)

(defconst magent-acp-protocol-version 1
  "ACP protocol version supported by Magent.")

(defun magent-acp--bool (value)
  "Return ACP boolean VALUE."
  (if value t :false))

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
                                  magent-effort-option-values))))))

(defun magent-acp--command-skill-names ()
  "Return sorted instruction skill names that can act as slash commands."
  (sort (cl-remove-if-not #'magent-skills-default-prompt
                          (magent-skills-list-by-type 'instruction))
        #'string<))

(defun magent-acp--command-entry (skill-name)
  "Return ACP available command entry for command-like SKILL-NAME."
  (let ((skill (magent-skills-get skill-name)))
    `((name . ,skill-name)
      (description . ,(magent-acp--metadata-string
                       (and skill
                            (magent-skill-description skill)))))))

(defun magent-acp--available-commands ()
  "Return ACP available command entries for command-like skills."
  (vconcat (mapcar #'magent-acp--command-entry
                   (magent-acp--command-skill-names))))

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
          (unless (null val)
            (push (cons (magent-acp--raw-input-key key)
                        (magent-acp--raw-input-value val))
                  out))))
      (nreverse out)))
   ((magent-json--alist-p value)
    (delq nil
          (mapcar (lambda (entry)
                    (unless (null (cdr entry))
                      (cons (magent-acp--raw-input-key (car entry))
                            (magent-acp--raw-input-value (cdr entry)))))
                  value)))
   ((hash-table-p value)
    (let (out)
      (maphash (lambda (key val)
                 (unless (null val)
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

(defun magent-acp--resource-text (block)
  "Return embedded text content from ACP resource BLOCK, or nil."
  (let ((resource (magent-acp--alist-plist-get block 'resource)))
    (or (magent-acp--alist-plist-get block 'text)
        (and resource
             (magent-acp--alist-plist-get resource 'text)))))

(defun magent-acp--resource-label (block type)
  "Return display label for ACP resource BLOCK of TYPE."
  (let ((resource (magent-acp--alist-plist-get block 'resource)))
    (or (magent-acp--alist-plist-get block 'uri)
        (magent-acp--alist-plist-get block 'name)
        (and resource
             (or (magent-acp--alist-plist-get resource 'uri)
                 (magent-acp--alist-plist-get resource 'name)))
        type)))

(defun magent-acp--resource-prompt-text (block type)
  "Return prompt text preserving ACP resource BLOCK contents."
  (let ((label (magent-acp--resource-label block type))
        (text (magent-acp--resource-text block)))
    (if (and (stringp text) (not (string-empty-p text)))
        (format "[Context resource: %s]\n%s" label text)
      (format "[Context resource: %s]" label))))

(defun magent-acp--prompt-text (prompt)
  "Return plain text extracted from ACP PROMPT content blocks."
  (let (parts unsupported)
    (dolist (block (magent-acp--prompt-blocks prompt))
      (let ((type (magent-acp--alist-plist-get block 'type)))
        (cond
         ((or (null type) (equal type "text"))
          (push (or (magent-acp--alist-plist-get block 'text) "")
                parts))
         ((member type '("resource_link" "resource" "file"))
          (push (magent-acp--resource-prompt-text block type) parts))
         (t
          (push type unsupported)))))
    (when unsupported
      (error "Unsupported ACP prompt content block(s): %s"
             (string-join (nreverse unsupported) ", ")))
    (string-trim (mapconcat #'identity (nreverse parts) "\n"))))

(defun magent-acp--dedupe-string-list (strings)
  "Return STRINGS without duplicates, preserving order."
  (let (seen result)
    (dolist (string strings)
      (when (and (stringp string)
                 (not (member string seen)))
        (push string seen)
        (push string result)))
    (nreverse result)))

(defun magent-acp--skill-command-text (skill-name extra-instruction)
  "Return default prompt text for SKILL-NAME plus EXTRA-INSTRUCTION."
  (let ((prompt (magent-skills-default-prompt skill-name))
        (extra (string-trim (or extra-instruction ""))))
    (unless prompt
      (error "Magent skill '%s' has no default prompt" skill-name))
    (if (string-blank-p extra)
        prompt
      (concat prompt "\n\nAdditional instruction:\n" extra))))

(defun magent-acp--slash-command (prompt)
  "Return slash command plist parsed from PROMPT, or nil.
Only command-like instruction skills are recognized."
  (let ((trimmed (string-trim prompt)))
    (when (string-prefix-p "/" trimmed)
      (let ((body (substring trimmed 1)))
        (when (string-match "\\`\\([[:alnum:]_-]+\\)" body)
          (let* ((name (match-string 1 body))
                 (rest (substring body (match-end 1))))
            (when (and (or (string-empty-p rest)
                           (string-match-p "\\`[[:space:]]" rest))
                       (magent-skills-default-prompt name))
              (list :name name
                    :prompt (magent-acp--skill-command-text name rest)
                    :skills (list name)))))))))

(defun magent-acp--apply-slash-command (runtime-session prompt)
  "Apply slash command PROMPT to RUNTIME-SESSION and return prompt text."
  (if-let* ((command (magent-acp--slash-command prompt)))
      (let ((skills (plist-get command :skills)))
        (setf (magent-runtime-session-pending-skills runtime-session)
              (magent-acp--dedupe-string-list
               (append (magent-runtime-session-pending-skills runtime-session)
                       skills)))
        (plist-get command :prompt))
    prompt))

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
     (availableCommands . ,(magent-acp--available-commands)))))

(defun magent-acp--session-success (client runtime-session on-success)
  "Call ON-SUCCESS with RUNTIME-SESSION response and command metadata."
  (funcall on-success (magent-acp--session-response runtime-session))
  (magent-acp--notify-available-commands client runtime-session))

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
           (when-let ((text (stream-text 'assistant (plist-get event :text))))
             (magent-acp--session-update
              client session-id
              `((sessionUpdate . "agent_message_chunk")
                (content . ,(magent-acp--content-block text))))))
          ('reasoning-delta
           (when (eq magent-include-reasoning t)
             (when-let ((text (stream-text 'reasoning
                                           (plist-get event :text))))
               (magent-acp--session-update
                client session-id
                `((sessionUpdate . "agent_thought_chunk")
                  (content . ,(magent-acp--content-block text)))))))
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
                  (title . ,(or (plist-get event :name) "tool"))
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
                                   (or (plist-get event :message) ""))))))))))))

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

(defun magent-acp--session-file-by-id (session-id)
  "Return saved session file for SESSION-ID, or nil."
  (seq-find
   (lambda (file)
     (equal (file-name-sans-extension (file-name-nondirectory file))
            session-id))
   (magent-session-list-files)))

(defun magent-acp--runtime-session-by-id (session-id)
  "Return runtime session SESSION-ID, loading it from disk if needed."
  (or (magent-runtime-session-from-id session-id)
      (when-let ((file (magent-acp--session-file-by-id session-id)))
        (magent-runtime-load-session-file file))))

(defun magent-acp--session-list-response ()
  "Return ACP session/list response."
  `((sessions . ,(vconcat
                  (mapcar
                   (lambda (entry)
                     (let ((updated-at (seconds-to-time
                                        (plist-get entry :updated-at))))
                       `((sessionId . ,(plist-get entry :id))
                         (cwd . ,(or (plist-get entry :project-root)
                                     default-directory))
                         (title . ,(or (plist-get entry :title)
                                       (plist-get entry :id)))
                         (createdAt . ,(magent-acp--iso-time updated-at))
                         (updatedAt . ,(magent-acp--iso-time updated-at)))))
                   (magent-runtime-list-sessions))))))

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

(defun magent-acp--handle-set-mode (params)
  "Handle ACP session/set_mode PARAMS."
  (let* ((session-id (map-elt params 'sessionId))
         (mode-id (map-elt params 'modeId))
         (runtime-session (magent-acp--runtime-session-by-id session-id)))
    (unless runtime-session
      (error "Unknown session: %s" session-id))
    (magent-runtime-session-set-agent runtime-session mode-id)
    (magent-acp--session-response runtime-session)))

(defun magent-acp--handle-set-model (params)
  "Handle ACP session/set_model PARAMS.
Magent uses gptel as the provider boundary, so the first ACP backend only
advertises the currently configured gptel model.  Accepting that model keeps
agent-shell's default bootstrap flow working without adding ACP-side model
switching semantics."
  (let* ((session-id (map-elt params 'sessionId))
         (model-id (map-elt params 'modelId))
         (runtime-session (magent-acp--runtime-session-by-id session-id))
         (current-model-id (format "%s" (or (and (boundp 'gptel-model)
                                                 gptel-model)
                                            "gptel"))))
    (unless runtime-session
      (error "Unknown session: %s" session-id))
    (unless (equal model-id current-model-id)
      (error "Unknown Magent model: %s" model-id))
    (magent-acp--session-response runtime-session)))

(defun magent-acp--handle-set-config-option (params)
  "Handle ACP session/set_config_option PARAMS."
  (let* ((session-id (map-elt params 'sessionId))
         (config-id (map-elt params 'configId))
         (value (map-elt params 'value))
         (runtime-session (magent-acp--runtime-session-by-id session-id)))
    (unless runtime-session
      (error "Unknown session: %s" session-id))
    (unless (equal config-id "effort")
      (error "Unknown Magent config option: %s" config-id))
    (magent-runtime-session-set-effort runtime-session value)
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
           (funcall on-success (magent-acp--session-list-response)))
          ((or "session/load" "session/resume")
           (let* ((session-id (map-elt params 'sessionId))
                  (runtime-session
                   (magent-acp--runtime-session-by-id session-id)))
             (unless runtime-session
               (error "Unknown session: %s" session-id))
             (magent-runtime-prepare-command-context
              (magent-runtime-session-scope runtime-session))
             (when (equal method "session/load")
               (magent-acp--replay-session client runtime-session))
             (magent-acp--session-success client runtime-session on-success)))
          ("session/set_mode"
           (funcall on-success (magent-acp--handle-set-mode params)))
          ("session/set_model"
           (funcall on-success (magent-acp--handle-set-model params)))
          ("session/set_config_option"
           (funcall on-success (magent-acp--handle-set-config-option params)))
          ("session/prompt"
           (let* ((session-id (map-elt params 'sessionId))
                  (runtime-session
                   (magent-acp--runtime-session-by-id session-id))
                  (prompt (magent-acp--prompt-text (map-elt params 'prompt))))
             (unless runtime-session
               (error "Unknown session: %s" session-id))
             (setq prompt
                   (magent-acp--apply-slash-command runtime-session prompt))
             (magent-runtime-submit
              runtime-session prompt
              :observer (magent-acp--observer client session-id)
              :approval-provider
              (magent-acp--approval-provider client session-id)
              :on-complete
              (lambda (status result)
                (funcall
                 on-success
                 `((stopReason . ,(magent-acp--stop-reason
                                    status result))
                   ,@(unless (eq status 'completed)
                       `((error . ,(magent-agent-result-content-string
                                    result))))))))))
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
  (ignore client sync)
  (let ((method (map-elt notification :method))
        (params (map-elt notification :params)))
    (pcase method
      ("session/cancel"
       (when-let* ((session-id (map-elt params 'sessionId))
                   (runtime-session
                    (magent-acp--runtime-session-by-id session-id)))
         (magent-runtime-cancel runtime-session)))
      (_
       (magent-log "WARN unsupported ACP notification: %s" method)))))

;;;###autoload
(defun magent-acp-make-client (&optional context-buffer)
  "Return an in-process ACP client for Magent.
CONTEXT-BUFFER is passed to `acp-make-client'."
  (acp-make-client
   :context-buffer context-buffer
   :command "cat"
   :request-sender #'magent-acp--request-sender
   :notification-sender #'magent-acp--notification-sender
   :response-sender #'magent-acp--response-sender))

(provide 'magent-acp)
;;; magent-acp.el ends here
