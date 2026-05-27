;;; magent-context.el --- Structured context history for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; Structured transcript helpers.  The first implementation stores
;; Codex-like response items next to the legacy message list so existing
;; gptel prompting remains compatible while the runtime gains a richer
;; history model.

;;; Code:

(require 'cl-lib)
(require 'magent-protocol)
(require 'magent-session)

(defun magent-context-items (session)
  "Return SESSION's structured response items."
  (and session (magent-session-context-items session)))

(defun magent-context-record-message (session role content &optional phase)
  "Append a structured message item to SESSION."
  (when session
    (let ((item (magent-protocol-message-item role content phase)))
      (setf (magent-session-context-items session)
            (nconc (magent-session-context-items session) (list item)))
      item)))

(defun magent-context-record-tool-call (session call)
  "Append structured tool CALL to SESSION."
  (when session
    (let ((item (magent-protocol-tool-call-item call)))
      (setf (magent-session-context-items session)
            (nconc (magent-session-context-items session) (list item)))
      item)))

(defun magent-context-record-tool-result (session result)
  "Append structured tool RESULT to SESSION."
  (when session
    (let ((item (magent-protocol-tool-result-item result)))
      (setf (magent-session-context-items session)
            (nconc (magent-session-context-items session) (list item)))
      item)))

(defun magent-context-message-items (session)
  "Return only message items from SESSION's structured context."
  (cl-remove-if-not
   (lambda (item) (eq (magent-response-item-type item) 'message))
   (magent-context-items session)))

(defun magent-context-approx-token-count (session)
  "Return a coarse token estimate for SESSION's structured context."
  (let ((chars 0))
    (dolist (item (magent-context-items session))
      (cl-incf chars
               (length (format "%s%s%s"
                               (or (magent-response-item-content item) "")
                               (or (magent-response-item-output item) "")
                               (or (magent-response-item-metadata item) "")))))
    (/ (+ chars 3) 4)))

(provide 'magent-context)
;;; magent-context.el ends here
