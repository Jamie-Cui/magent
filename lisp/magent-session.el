;;; magent-session.el --- Session management for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Session management for storing conversation history and state.

;;; Code:

(require 'cl-lib)
(require 'magent-config)

;;; Session state structure

(cl-defstruct (magent-session
               (:constructor magent-session-create)
               (:copier nil))
  messages
  (max-history magent-max-history)
  (id nil)
  (agent nil))

;;; Message helpers

(defsubst magent-msg-role (msg)
  "Return the role symbol of message MSG."
  (cdr (assq 'role msg)))

(defsubst magent-msg-content (msg)
  "Return the content of message MSG (string or content-block list)."
  (cdr (assq 'content msg)))

(defsubst magent-session--content-to-string (content)
  "Coerce CONTENT to a plain string.
If CONTENT is a string, return it unchanged.
If CONTENT is a list of content blocks, concatenate their text fields."
  (if (stringp content) content
    (mapconcat (lambda (b) (or (cdr (assq 'text b)) "")) content "")))

;;; Session management

(defvar magent--current-session nil
  "The current active session.")

(defun magent-session-get ()
  "Get the current session, creating one if needed."
  (unless magent--current-session
    (setq magent--current-session (magent-session-create)))
  magent--current-session)

(defun magent-session-reset ()
  "Reset the current session, clearing all messages."
  (interactive)
  (setq magent--current-session nil)
  (magent-log "INFO session cleared"))

(defun magent-session-get-id (session)
  "Get or generate a unique ID for SESSION."
  (or (magent-session-id session)
      (let ((id (format "session-%s" (format-time-string "%Y%m%d-%H%M%S"))))
        (setf (magent-session-id session) id)
        id)))

(defun magent-session-get-agent (session)
  "Get the current agent for SESSION."
  (magent-session-agent session))

(defun magent-session-set-agent (session agent)
  "Set the agent for SESSION to AGENT."
  (setf (magent-session-agent session) agent))

;;; Message management

(defun magent-session-add-message (session role content)
  "Add a message to SESSION.
ROLE is either 'user', 'assistant', or 'tool'.
CONTENT can be a string or a list of content blocks."
  (let ((messages (magent-session-messages session)))
    (push (list (cons 'role role)
                (cons 'content content))
          messages)
    ;; Trim to max history
    (when (> (length messages) (magent-session-max-history session))
      (setf messages (butlast messages (- (length messages)
                                          (magent-session-max-history session)))))
    ;; Always update the session messages (was missing before!)
    (setf (magent-session-messages session) messages))
  session)

(defun magent-session-get-messages (session)
  "Get all messages from SESSION in chronological order."
  (reverse (magent-session-messages session)))

(defun magent-session-add-tool-result (session tool-use-id result)
  "Add a tool result message to SESSION.
TOOL-USE-ID is the ID of the tool use being responded to.
RESULT is the string result of tool execution."
  (magent-session-add-message
   session 'tool
   `((type . "tool_result")
     (tool_use_id . ,tool-use-id)
     (content . ,result))))

(defun magent-session-get-context-size (session)
  "Calculate approximate token count of SESSION messages.
This is a rough estimate assuming ~4 chars per token."
  (let ((total-chars 0))
    (dolist (msg (magent-session-get-messages session))
      (cl-incf total-chars
               (length (magent-session--content-to-string (magent-msg-content msg)))))
    (/ total-chars 4)))

;;; Session persistence

(defun magent-session-save (session &optional file)
  "Save SESSION to FILE.
If FILE is nil, uses a default location based on session ID."
  (let* ((session-id (magent-session-get-id session))
         (default-file (expand-file-name (concat session-id ".json") magent-session-directory))
         (filename (or file default-file)))
    (make-directory magent-session-directory t)
    (with-temp-file filename
      (insert (json-encode `((id . ,session-id)
                            (messages . ,(magent-session-messages session))
                            (timestamp . ,(format-time-string "%Y-%m-%dT%T%z"))))))
    filename))

(defun magent-session-load (file)
  "Load session from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((data (json-read))
           (session (magent-session-create)))
      (setf (magent-session-id session) (cdr (assq 'id data)))
      (setf (magent-session-messages session) (cdr (assq 'messages data)))
      session)))

(defun magent-session-list-saved ()
  "List all saved session files."
  (when (file-directory-p magent-session-directory)
    (directory-files magent-session-directory t "\\.json$")))

;;; Session context helpers

(defun magent-session-get-project-files (session &optional directory)
  "Get a list of relevant project files for context.
Uses DIRECTORY or current project root."
  (let* ((default-directory (or directory
                               (when (fboundp 'projectile-project-root)
                                 (projectile-project-root))
                               default-directory))
         (files (condition-case nil
                    (all-completions "" (directory-files default-directory))
                  (error nil))))
    files))

(defun magent-session-summarize (session)
  "Create a summary of SESSION messages.
Returns a condensed version of the conversation."
  (let ((messages (magent-session-get-messages session)))
    (when messages
      (with-temp-buffer
        (insert "Session Summary:\n\n")
        (dolist (msg (last messages 20))
          (let ((role (magent-msg-role msg))
                (content (magent-msg-content msg)))
            (insert (format "[%s] " (upcase (symbol-name role))))
            (insert (truncate-string-to-width
                     (magent-session--content-to-string content) 80 nil nil "..."))
            (insert "\n\n")))
        (buffer-string)))))

;;; gptel prompt list conversion

(defun magent-session-to-gptel-prompt-list (session)
  "Convert SESSION messages to a gptel-request prompt list.
Returns a list in gptel's advanced format:
  ((prompt . \"user msg\") (response . \"assistant msg\") ...)
Tool result messages are skipped; gptel handles tool round-trips
internally within each request."
  (let ((messages (magent-session-get-messages session)))
    (delq nil
          (mapcar
           (lambda (msg)
             (let ((role (magent-msg-role msg))
                   (content (magent-msg-content msg)))
               (pcase role
                 ('user
                  (cons 'prompt (magent-session--content-to-string content)))
                 ('assistant
                  (cons 'response (magent-session--content-to-string content)))
                 (_ nil))))
           messages))))

(provide 'magent-session)
;;; magent-session.el ends here
