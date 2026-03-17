;;; magent-approval.el --- Approval providers for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; Pluggable approval providers for Magent tool confirmations.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar magent-approval-provider-function #'magent-approval-local-request
  "Function used to request tool approval.
The function is called with REQUEST plist and CALLBACK.  CALLBACK
receives one of the symbols `allow-once', `deny-once',
`allow-session', or `deny-session'.")

(defun magent-approval-request (request callback)
  "Request approval for REQUEST and deliver the decision to CALLBACK."
  (funcall magent-approval-provider-function request callback))

(defun magent-approval-local-request (request callback)
  "Request local approval for REQUEST, then invoke CALLBACK."
  (run-at-time
   0 nil
   (lambda ()
     (let* ((tool-name (plist-get request :tool-name))
            (summary (or (plist-get request :summary) ""))
            (prompt (format "magent: allow %s%s? [y]es/[n]o/[A]lways/[D]eny always: "
                            tool-name
                            (if (string-empty-p summary)
                                ""
                              (format " (%s)" summary))))
            (choice (read-char-choice prompt '(?y ?n ?A ?D))))
       (funcall callback
                (pcase choice
                  (?y 'allow-once)
                  (?n 'deny-once)
                  (?A 'allow-session)
                  (?D 'deny-session)))))))

(provide 'magent-approval)
;;; magent-approval.el ends here
