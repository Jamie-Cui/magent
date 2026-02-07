;;; magent-api.el --- HTTP API client for LLM providers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; HTTP API client for communicating with LLM providers (Anthropic, OpenAI).

;;; Code:

(require 'json)
(require 'url)
(require 'magent-config)
(eval-when-compile (require 'cl-lib))

;;; Logging

(defvar magent-api--log-buffer-name "*magent-log*"
  "Name of the log buffer.")

(defun magent-api--log (format-string &rest args)
  "Log to the Magent log buffer if it exists and logging is enabled."
  (when (and magent-enable-logging
             (get-buffer magent-api--log-buffer-name))
    (with-current-buffer (get-buffer magent-api--log-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "[%s] [API] %s\n"
                       (format-time-string "%H:%M:%S")
                       (apply #'format format-string args)))))))

;;; Variables

(defvar magent-api--anthropic-url "https://api.anthropic.com/v1/messages"
  "Endpoint for Anthropic Messages API.")

(defvar magent-api--openai-url "https://api.openai.com/v1/chat/completions"
  "Endpoint for OpenAI Chat Completions API.")

(defvar magent-api--request-timeout 120
  "Timeout in seconds for API requests.")

;;; Helper functions

(defun magent-api--json-encode (object)
  "Encode OBJECT to JSON string."
  (json-encode object))

(defun magent-api--json-decode (string)
  "Decode JSON STRING to Lisp object."
  (json-read-from-string string))

(defun magent-api--get-endpoint ()
  "Get the API endpoint URL based on current provider.
Automatically adds https:// protocol if missing for custom endpoints."
  (let ((url (pcase magent-provider
               ('anthropic magent-api--anthropic-url)
               ('openai magent-api--openai-url)
               ('openai-compatible magent-base-url)
               (_ (error "Unknown provider: %s" magent-provider)))))
    ;; Ensure URL has a protocol scheme
    (when (and url (not (string-match-p "\\`https?://" url)))
      (setq url (concat "https://" url)))
    url))

(defun magent-api--get-headers ()
  "Get HTTP headers for API request."
  (let ((api-key (magent-get-api-key)))
    (unless api-key
      (error "API key not set. Configure magent-api-key or set environment variable"))
    (pcase magent-provider
      ('anthropic
       `(("x-api-key" . ,api-key)
         ("anthropic-version" . "2023-06-01")
         ("content-type" . "application/json")))
      (_
       `(("authorization" . ,(concat "Bearer " api-key))
         ("content-type" . "application/json"))))))

;;; Credentials

;;;###autoload
(defun magent-api-set-credentials ()
  "Validate and set API credentials.
Checks if API key is available from config or environment."
  (interactive)
  (let ((api-key (magent-get-api-key)))
    (if api-key
        (message "OpenCode: API key configured for %s" magent-provider)
      (message "OpenCode: API key not configured. Set magent-api-key or %s"
               (pcase magent-provider
                 ('anthropic "ANTHROPIC_API_KEY")
                 ('openai "OPENAI_API_KEY")
                 (_ "API_KEY env"))))))

;;; Core API functions

(defun magent-api--convert-messages (messages)
  "Convert internal MESSAGE format to provider-specific format.
Internal format: ((role . \"user\") (content . \"text\")) or
                 ((role . \"user\") (content . ((\"type\" . \"text\") (\"text\" . \"...\")))))"
  (mapcar (lambda (msg)
            (let ((role (cdr (assq 'role msg)))
                  (content (cdr (assq 'content msg))))
              (pcase magent-provider
                ('anthropic
                 `((role . ,role)
                   (content . ,(if (listp content)
                                   content
                                 `((type . "text") (text . ,content))))))
                (_ ; OpenAI format
                 `((role . ,(if (equal role "assistant") "assistant" "user"))
                   (content . ,(if (listp content)
                                   (let ((text-parts (cl-loop for c in content
                                                              when (equal (cdr (assq 'type c)) "text")
                                                              collect (cdr (assq 'text c))))
                                         (image-parts (cl-loop for c in content
                                                               when (equal (cdr (assq 'type c)) "image")
                                                               collect c)))
                                     (if (null image-parts)
                                         (mapconcat #'identity text-parts "\n")
                                       ;; TODO: Handle images for OpenAI
                                       (mapconcat #'identity text-parts "\n")))
                                 content)))))))
          messages))

(cl-defun magent-api-chat (messages &key
                                      (tools nil)
                                      (stream nil)
                                      (model magent-model)
                                      (max-tokens magent-max-tokens)
                                      (temperature magent-temperature)
                                      (callback nil))
  "Send chat completion request to LLM provider.
MESSAGES is a list of message objects with 'role and 'content keys.
TOOLS is a list of available tool definitions (optional).
STREAM if non-nil enables streaming responses.
CALLBACK is a function called with the response when complete.
Returns immediately; response is delivered via CALLBACK."
  (let* ((endpoint (magent-api--get-endpoint))
         (headers (magent-api--get-headers))
         (converted-messages (magent-api--convert-messages messages))
         (body (pcase magent-provider
                 ('anthropic
                  `((model . ,model)
                    (max_tokens . ,max-tokens)
                    (messages . ,converted-messages)
                    (temperature . ,temperature)
                    ,@(when tools `((tools . ,tools)))))
                 (_ ; OpenAI format
                  `((model . ,model)
                    (max_tokens . ,max-tokens)
                    (messages . ,converted-messages)
                    (temperature . ,temperature)
                    ,@(when tools `((tools . ,(magent-api--convert-tools tools))))))))
         (request-data (magent-api--json-encode body))
         (url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data request-data))

    (magent-api--log "Sending request to: %s" endpoint)
    (magent-api--log "Messages count: %d" (length messages))
    (magent-api--log "Request body: %s" request-data)

    (url-retrieve endpoint (if callback
                               (lambda (status &optional args)
                                 (let* ((buffer (current-buffer))
                                        (response (magent-api--parse-response buffer)))
                                   (magent-api--log "Response received: %S" response)
                                   (kill-buffer buffer)
                                   (funcall callback response)))
                             #'magent-api--handle-response)
                  (list (list :messages messages
                              :stream stream
                              :on-complete callback))
                  t t)))

(defun magent-api--convert-tools (tools)
  "Convert internal tool definitions to OpenAI format."
  (mapcar (lambda (tool)
            `((type . "function")
              (function . ((name . ,(cdr (assq 'name tool)))
                           (description . ,(cdr (assq 'description tool)))
                           (parameters . ,(cdr (assq 'parameters tool)))))))
          tools))

(defun magent-api--handle-response (status &optional args)
  "Handle API response.
STATUS is the HTTP status.
ARGS contains context data."
  (let* ((buffer (current-buffer))
         (response (magent-api--parse-response buffer)))
    (kill-buffer buffer)
    (message "Response received: %S" response)))

(defun magent-api--parse-response (buffer)
  "Parse JSON response from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^$" nil t) ; Skip headers
    (let* ((json-string (buffer-substring (point) (point-max)))
           (response (magent-api--json-decode json-string)))
      response)))

(defun magent-api--extract-content (response)
  "Extract content from provider RESPONSE.
Handles both successful responses and error responses."
  ;; First check for HTTP error responses
  (when (or (assq 'error response)
            (assq 'status response))
    (let ((err (cdr (assq 'error response)))
          (status (cdr (assq 'status response)))
          (path (cdr (assq 'path response))))
      (error "API error: %s (status: %s, path: %s)"
             (or err "Unknown error") status path)))

  (pcase magent-provider
    ('anthropic
     (let* ((content (cdr (assq 'content response)))
            (text-blocks (cl-loop for block in content
                                  when (equal (cdr (assq 'type block)) "text")
                                  collect (cdr (assq 'text block)))))
       (mapconcat #'identity text-blocks "\n")))
    (_ ; OpenAI
     (let* ((choices (cdr (assq 'choices response)))
            (message (when choices (cdr (assq 'message (aref choices 0)))))
            (content (when message (cdr (assq 'content message)))))
       (or content
           (error "No content in API response: %S" response))))))

(defun magent-api--extract-tool-uses (response)
  "Extract tool use blocks from provider RESPONSE.
Returns nil if no tool uses are found or if response is an error."
  ;; First check for HTTP error responses
  (if (or (assq 'error response)
          (assq 'status response))
      nil
    (pcase magent-provider
      ('anthropic
       (cl-loop for block in (cdr (assq 'content response))
                when (equal (cdr (assq 'type block)) "tool_use")
                collect `((id . ,(cdr (assq 'id block)))
                          (name . ,(cdr (assq 'name block)))
                          (input . ,(cdr (assq 'input block))))))
      (_ ; OpenAI
       (let ((tool-calls (cdr (assq 'tool_calls response))))
         (when tool-calls
           (cl-loop for call across tool-calls
                    collect `((id . ,(cdr (assq 'id call)))
                              (name . ,(cdr (assq 'name (cdr (assq 'function call)))))
                              (input . ,(magent-api--json-decode
                                         (cdr (assq 'arguments call))))))))))))

;;; Tool calling support

(defun magent-api--make-tool-definition (name description parameters)
  "Create a tool definition.
NAME is the tool name.
DESCRIPTION is what the tool does.
PARAMETERS is a JSON schema for tool arguments."
  `((name . ,name)
    (description . ,description)
    (input_schema . ,parameters))) ; Anthropic format

(defun magent-api-send-tool-results (tool-use-id results)
  "Send tool execution results back to the API.
TOOL-USE-ID is the ID of the tool use being responded to.
RESULTS is the string result of tool execution."
  ;; This would append a tool_result message and call chat again
  ;; Implementation depends on session management
  )

;;; Streaming support (basic)

(defvar-local magent-api--stream-accumulator nil
  "Accumulator for streaming response chunks.")

(defun magent-api--handle-stream-chunk (chunk callback)
  "Handle a streaming response CHUNK.
CALLBACK is called with each content delta."
  (let ((lines (split-string chunk "\n")))
    (dolist (line lines)
      (when (string-prefix-p "data: " line)
        (let* ((json-str (substring line 6))
               (data (when (> (length json-str) 0)
                       (condition-case nil
                           (magent-api--json-decode json-str)
                         (error nil)))))
          (when data
            (let ((delta (magent-api--extract-stream-delta data)))
              (when delta
                (funcall callback delta)))))))))

(defun magent-api--extract-stream-delta (data)
  "Extract content delta from streaming DATA."
  (pcase magent-provider
    ('anthropic
     (when (equal (cdr (assq 'type data)) "content_block_delta")
       (let* ((delta (cdr (assq 'delta data)))
              (text (cdr (assq 'text delta))))
         text)))
    (_ ; OpenAI
     (let* ((choices (cdr (assq 'choices data)))
            (delta (cdr (assq 'delta (aref choices 0))))
            (content (cdr (assq 'content delta))))
       content))))

(provide 'magent-api)
;;; magent-api.el ends here
