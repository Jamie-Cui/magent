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

(require 'magent-events)
(require 'magent-fsm-backend-native)
(declare-function magent-ui-start-streaming "magent-ui")
(declare-function magent-ui-continue-streaming "magent-ui")
(declare-function magent-ui-insert-streaming "magent-ui")
(declare-function magent-ui-insert-reasoning-start "magent-ui")
(declare-function magent-ui-insert-reasoning-text "magent-ui")
(declare-function magent-ui-insert-reasoning-end "magent-ui")
(declare-function magent-ui-finish-streaming-fontify "magent-ui")
(declare-function magent-log "magent-config")

(defvar magent-include-reasoning)

(defun magent-fsm-backend-gptel--make-stream-state ()
  "Create mutable per-request streaming state for the gptel backend."
  (let ((state (make-hash-table :test 'eq)))
    (puthash :text-chunks nil state)
    (puthash :reasoning-chunks nil state)
    (puthash :in-reasoning-block nil state)
    state))

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
        :event-context (plist-get args :event-context)
        :permission (plist-get args :permission)
        :callback (plist-get args :callback)
        :ui-callback (plist-get args :ui-callback)))

(defun magent-fsm-backend-gptel-start (params)
  "Start gptel request with PARAMS plist."
  (let* ((prompt-list (plist-get params :prompt-list))
         (system-prompt (plist-get params :system-prompt))
         (tools (plist-get params :tools))
         (event-context (plist-get params :event-context))
         (permission (plist-get params :permission))
         (callback (plist-get params :callback))
         (ui-callback (plist-get params :ui-callback))
         (backend (plist-get params :gptel-backend))
         (model (plist-get params :model))
         (request-buffer (generate-new-buffer " *magent-gptel-request*"))
         ;; Install permission-aware :confirm functions and handle
         ;; `(tool-call . ...)' callbacks ourselves instead of relying on
         ;; gptel's default in-buffer confirmation UI.
         (gptel-tools (magent-fsm--convert-tools-to-gptel
                       tools permission event-context))
         (stream-state (magent-fsm-backend-gptel--make-stream-state)))

    (magent-ui-start-streaming)

    (with-current-buffer request-buffer
      (setq-local gptel-backend backend)
      (setq-local gptel-model model)
      (setq-local gptel-tools gptel-tools)
      (setq-local gptel-use-tools (and gptel-tools t))
      (setq-local gptel-confirm-tool-calls 'auto)
      (setq-local gptel-include-reasoning magent-include-reasoning)
      (gptel-request
          prompt-list
        :buffer (current-buffer)
        :system system-prompt
        :stream t
        :callback (lambda (response info)
                    (magent-fsm-backend-gptel--callback
                     response info callback ui-callback request-buffer
                     permission event-context stream-state))))))

(defun magent-fsm-backend-gptel--callback (response info callback ui-callback buffer
                                                    permission event-context
                                                    &optional stream-state)
  "Handle gptel response.
Wraps all UI operations in `condition-case' to suppress benign
cursor-boundary signals that can leak from evil-mode adjustments
inside gptel's process filter."
  (let ((state (or stream-state
                   (magent-fsm-backend-gptel--make-stream-state))))
    (condition-case nil
        (cond
         ((and (consp response) (eq (car response) 'reasoning)
               magent-include-reasoning)
          (let ((reasoning-text (cdr response)))
            (if (eq reasoning-text t)
                (when (gethash :in-reasoning-block state)
                  (magent-ui-insert-reasoning-end)
                  (puthash :in-reasoning-block nil state))
              (progn
                (unless (gethash :in-reasoning-block state)
                  (magent-ui-insert-reasoning-start)
                  (puthash :in-reasoning-block t state))
                (magent-ui-insert-reasoning-text reasoning-text)
                (push reasoning-text (gethash :reasoning-chunks state))
                (puthash :reasoning-chunks
                         (gethash :reasoning-chunks state)
                         state)))))
         ((stringp response)
          (push response (gethash :text-chunks state))
          (puthash :text-chunks (gethash :text-chunks state) state)
          (magent-events-emit 'text-delta :context event-context :text response)
          (when ui-callback (funcall ui-callback response)))
         ((and (consp response) (eq (car response) 'tool-call))
          (when (gethash :in-reasoning-block state)
            (magent-ui-insert-reasoning-end)
            (puthash :in-reasoning-block nil state))
          (magent-fsm--handle-tool-call-confirmation-with-permission
           permission (cdr response)))
         ((and (consp response) (eq (car response) 'tool-result))
          (when (gethash :in-reasoning-block state)
            (magent-ui-insert-reasoning-end)
            (puthash :in-reasoning-block nil state))
          (magent-ui-continue-streaming))
         ((eq response t)
          (when (gethash :in-reasoning-block state)
            (magent-ui-insert-reasoning-end)
            (puthash :in-reasoning-block nil state))
          (if (plist-get info :tool-use)
              ;; Tool use round: gptel will continue the loop.
              ;; Stay in the same section — don't finalize or create a new heading.
              (magent-ui-continue-streaming)
            ;; Final response: finalize the streaming section and finish.
            (magent-ui-finish-streaming-fontify)
            (when callback
              (funcall callback (or (and (gethash :text-chunks state)
                                         (apply #'concat
                                                (nreverse (gethash :text-chunks state))))
                                    (plist-get info :content) "")))
            (when (buffer-live-p buffer) (kill-buffer buffer))))
         ((null response)
          (when callback (funcall callback nil))
          (when (buffer-live-p buffer) (kill-buffer buffer))))
      ((beginning-of-buffer end-of-buffer beginning-of-line end-of-line)
       nil))))

(defun magent-fsm-backend-gptel-abort (params)
  "Abort gptel request."
  (let ((context (plist-get params :event-context)))
    (magent-events-end-turn context 'cancelled "User aborted")))

(defun magent-fsm-backend-gptel-destroy (params)
  "Destroy gptel FSM (no-op)."
  (ignore params))

;;; Unknown-tool advice for gptel FSM

(defun magent--handle-unknown-tools-a (orig-fn fsm)
  "Around advice for `gptel--handle-tool-use'.
Pre-fill :result for unknown tool calls so gptel filters them out,
preventing the FSM from hanging when the model hallucinates tool names."
  (let* ((info (gptel-fsm-info fsm))
         (patched nil))
    (when-let* ((all-tool-use (plist-get info :tool-use))
                (tools (plist-get info :tools)))
      (let ((avail (mapconcat #'gptel-tool-name tools ", ")))
        (dolist (tc all-tool-use)
          (unless (plist-get tc :result)
            (let ((name (plist-get tc :name)))
              (unless (cl-find-if
                       (lambda (ts) (equal (gptel-tool-name ts) name))
                       tools)
                (plist-put tc :result
                           (format "Error: tool '%s' not found. Available: %s"
                                   name avail))
                (setq patched t)
                (magent-log "WARN unknown tool '%s' — returned error to model"
                            name)))))))
    ;; Run original handler
    (funcall orig-fn fsm)
    ;; All-unknown edge case: handler body didn't execute, FSM still in TOOL
    (when (and patched (eq (gptel-fsm-state fsm) 'TOOL))
      (let ((remaining (cl-remove-if
                        (lambda (tc) (plist-get tc :result))
                        (plist-get info :tool-use))))
        (when (null remaining)
          (plist-put info :tool-success t)
          (let ((backend (plist-get info :backend)))
            (gptel--inject-prompt
             backend (plist-get info :data)
             (gptel--parse-tool-results backend (plist-get info :tool-use))))
          (funcall (plist-get info :callback)
                   (cons 'tool-result nil) info)
          (gptel--fsm-transition fsm))))))

(advice-add 'gptel--handle-tool-use :around #'magent--handle-unknown-tools-a)

(provide 'magent-fsm-backend-gptel)
;;; magent-fsm-backend-gptel.el ends here
