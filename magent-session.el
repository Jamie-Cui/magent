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

;;; Session display

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
