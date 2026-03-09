;;; magent-fsm-backend-gptel.el --- Gptel FSM backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Gptel-based FSM backend implementation.

;;; Code:

(require 'cl-lib)
(require 'gptel)

(declare-function magent-fsm--convert-tools-to-gptel "magent-fsm-backend-native")
(declare-function magent-ui-start-streaming "magent-ui")
(declare-function magent-ui-insert-streaming "magent-ui")
(declare-function magent-ui-finish-streaming-fontify "magent-ui")

(defvar magent-include-reasoning)

(defun magent-fsm-backend-gptel-create (&rest args)
  "Create gptel FSM from keyword ARGS (returns plist with parameters).
Accepts the same keyword arguments as `magent-fsm-create'."
  (list :backend 'gptel
        :session (plist-get args :session)
        :agent (plist-get args :agent)
        :gptel-backend (plist-get args :backend)
        :model (plist-get args :model)
        :prompt-list (plist-get args :prompt-list)
        :system-prompt (plist-get args :system-prompt)
        :tools (plist-get args :tools)
        :permission (plist-get args :permission)
        :callback (plist-get args :callback)
        :ui-callback (plist-get args :ui-callback)))

(defun magent-fsm-backend-gptel-start (params)
  "Start gptel request with PARAMS plist."
  (let* ((prompt-list (plist-get params :prompt-list))
         (system-prompt (plist-get params :system-prompt))
         (tools (plist-get params :tools))
         (permission (plist-get params :permission))
         (callback (plist-get params :callback))
         (ui-callback (plist-get params :ui-callback))
         (backend (plist-get params :gptel-backend))
         (model (plist-get params :model))
         (request-buffer (generate-new-buffer " *magent-gptel-request*"))
         ;; Don't pass permission — gptel's confirm UI requires an interactive
         ;; buffer, but the gptel backend uses a hidden request buffer.
         (gptel-tools (magent-fsm--convert-tools-to-gptel tools nil)))

    (magent-ui-start-streaming)

    (with-current-buffer request-buffer
      (setq-local gptel-tools gptel-tools)
      (setq-local gptel-use-tools (if gptel-tools t nil))
      (setq-local gptel-include-reasoning magent-include-reasoning))

    (let ((gptel-backend backend)
          (gptel-model model))
      (gptel-request
          prompt-list
        :buffer request-buffer
        :system system-prompt
        :stream t
        :callback (lambda (response info)
                    (magent-fsm-backend-gptel--callback
                     response info callback ui-callback request-buffer))))))

(defun magent-fsm-backend-gptel--callback (response info callback ui-callback buffer)
  "Handle gptel response."
  (cond
   ((stringp response)
    (when ui-callback (funcall ui-callback response)))
   ((eq response t)
    (magent-ui-finish-streaming-fontify)
    (if (plist-get info :tool-use)
        ;; Tool use round: gptel will continue the loop, don't finish yet.
        (magent-ui-start-streaming)
      ;; Final response: no more tool calls, finish processing.
      (when callback (funcall callback (or (plist-get info :content) "")))
      (when (buffer-live-p buffer) (kill-buffer buffer))))
   ((null response)
    (when callback (funcall callback nil))
    (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun magent-fsm-backend-gptel-abort (params)
  "Abort gptel request (no-op)."
  (ignore params))

(defun magent-fsm-backend-gptel-destroy (params)
  "Destroy gptel FSM (no-op)."
  (ignore params))

(provide 'magent-fsm-backend-gptel)
;;; magent-fsm-backend-gptel.el ends here
