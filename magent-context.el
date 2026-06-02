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
(require 'magent-thread)

(defun magent-context-items (session)
  "Return SESSION's structured response items."
  (and session (magent-session-context-items session)))

(defun magent-context-record-message (session role content &optional phase)
  "Append a structured message item to SESSION."
  (when session
    (let* ((thread (magent-session-thread-ledger session))
           (turn (or (magent-thread-active-turn thread)
                     (magent-thread-create-turn
                      thread
                      (and (eq role 'user) content))))
           (item (magent-thread-record-message
                  thread
                  (magent-thread-turn-id turn)
                  role content phase)))
      (magent-session-refresh-projections session)
      (magent-thread-item-to-response-item item))))

(defun magent-context-record-tool-call (session call)
  "Append structured tool CALL to SESSION."
  (when session
    (let* ((thread (magent-session-thread-ledger session))
           (turn (or (magent-thread-active-turn thread)
                     (magent-thread-create-turn
                      thread nil nil (list :synthetic t))))
           (item (magent-thread-start-item
                  thread
                  (magent-thread-turn-id turn)
                  'tool
                  :id (magent-tool-call-id call)
                  :call-id (magent-tool-call-id call)
                  :name (magent-tool-call-name call)
                  :input (magent-tool-call-arguments call)
                  :metadata (list :perm-key (magent-tool-call-perm-key call)
                                  :reason (magent-tool-call-reason call)))))
      (magent-session-refresh-projections session)
      (magent-thread-item-to-response-item item))))

(defun magent-context-record-tool-result (session result)
  "Append structured tool RESULT to SESSION."
  (when session
    (let* ((thread (magent-session-thread-ledger session))
           (turn (or (magent-thread-active-turn thread)
                     (magent-thread-create-turn
                      thread nil nil (list :synthetic t))))
           (item (magent-thread-record-tool-result
                  thread
                  (magent-thread-turn-id turn)
                  (magent-tool-result-call-id result)
                  (magent-tool-result-name result)
                  nil
                  (magent-tool-result-output result)
                  (magent-tool-result-metadata result))))
      (magent-session-refresh-projections session)
      (magent-thread-item-to-response-item item))))

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
