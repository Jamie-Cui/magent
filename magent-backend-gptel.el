;;; magent-backend-gptel.el --- gptel backend for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Backend implementation that delegates to gptel's built-in tool-calling loop.
;; This is the default backend and leverages gptel's native streaming and
;; tool execution capabilities.

;;; Code:

(require 'cl-lib)
(require 'magent-backend)
(require 'magent-session)

;; Forward declarations
(declare-function magent-log "magent-ui")
(declare-function magent-ui-insert-streaming "magent-ui")
(declare-function magent-ui-start-streaming "magent-ui")
(declare-function magent-session-add-message "magent-session")
(declare-function gptel-request "ext:gptel")
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-confirm-tool-calls)

;;; Backend Implementation

(cl-defstruct (magent-backend-gptel (:include magent-backend-request))
  "gptel backend handle."
  (buffer nil)      ; Request buffer (for cancellation)
  (process nil))    ; HTTP process (for cancellation)

(defun magent-backend-gptel--create ()
  "Create a gptel backend instance."
  'gptel)

(cl-defmethod magent-backend-start ((_backend (eql gptel)) request)
  "Start processing REQUEST using gptel backend."
  (let* ((session (magent-backend-request-session request))
         (_agent (magent-backend-request-agent request))
         (backend (magent-backend-request-backend request))
         (model (magent-backend-request-model request))
         (prompt-list (magent-backend-request-prompt-list request))
         (system-prompt (magent-backend-request-system-prompt request))
         (tools (magent-backend-request-tools request))
         (streaming-p (magent-backend-request-streaming-p request))
         (callback (magent-backend-request-callback request))
         (ui-callback (magent-backend-request-ui-callback request))
         (handle (make-magent-backend-gptel))
         (request-buffer (generate-new-buffer " *magent-gptel-request*"))
         (received-text nil))  ; Track if we received any streaming text in current round

    (magent-log "INFO gptel backend: starting request with %d tools" (length tools))

    ;; Set up request buffer with gptel configuration
    (with-current-buffer request-buffer
      (setq-local gptel-backend backend)
      (setq-local gptel-model model)
      (setq-local gptel-tools tools)
      (setq-local gptel-use-tools (if tools t nil))
      ;; Disable gptel's tool confirmation - magent has its own permission system
      (setq-local gptel-confirm-tool-calls nil))

    (setf (magent-backend-gptel-buffer handle) request-buffer)

    ;; For streaming, prepare the UI with assistant prompt prefix
    (when streaming-p
      (magent-ui-start-streaming))

    (gptel-request
     prompt-list
     :buffer request-buffer
     :system system-prompt
     :stream streaming-p
     :callback
     (lambda (response info)
         (cond
          ;; Streaming chunks
          ((and streaming-p (stringp response))
           (setq received-text t)
           (when ui-callback
             (funcall ui-callback response)))
          ;; Streaming round completed - check if there are pending tool calls
          ((eq response t)
           (let ((tool-use (plist-get info :tool-use)))
             (if tool-use
                 (progn
                   (magent-log "DEBUG gptel backend: streaming round completed, continuing with tool calls")
                   (setq received-text nil))  ; Reset for next round, but keep started-streaming
               (progn
                 (magent-log "INFO gptel backend: streaming request completed")
                 ;; Clean up request buffer
                 (when (buffer-live-p request-buffer)
                   (kill-buffer request-buffer))
                 (when callback
                   (funcall callback (if received-text "streamed" "")))))))
        ;; Non-streaming success
        ((stringp response)
         (magent-log "INFO gptel backend: request completed")
         (magent-session-add-message session 'assistant response)
         ;; Clean up request buffer
         (when (buffer-live-p request-buffer)
           (kill-buffer request-buffer))
         (when callback
           (funcall callback response)))
        ;; Reasoning tokens (cons cell with 'reasoning as car)
        ((and (consp response) (eq (car response) 'reasoning))
         (magent-log "DEBUG gptel backend: ignoring reasoning token"))
        ;; Tool calls (cons cell with 'tool-call as car)
        ((and (consp response) (eq (car response) 'tool-call))
         (magent-log "DEBUG gptel backend: tool-call received: %S" response))
        ;; Tool results (cons cell with 'tool-result as car)
        ((and (consp response) (eq (car response) 'tool-result))
         (magent-log "DEBUG gptel backend: ignoring tool-result callback"))
        ;; Ignore intermediate callbacks (gptel internal state updates)
        ((null response)
         (magent-log "DEBUG gptel backend: ignoring intermediate callback"))
        ;; Real error (response is error symbol or unexpected value)
        (t
         (let ((status (plist-get info :status)))
           (magent-log "ERROR gptel backend: request failed, response=%S status=%s"
                      response status)
           ;; Clean up request buffer
           (when (buffer-live-p request-buffer)
             (kill-buffer request-buffer))
           (when callback
             (funcall callback nil)))))))

    handle))

(cl-defmethod magent-backend-abort ((_backend (eql gptel)) handle)
  "Abort gptel request identified by HANDLE."
  (magent-log "INFO gptel backend: aborting request")
  ;; gptel doesn't expose abort API, so we just log
  ;; The request will complete or timeout naturally
  (ignore handle))

(cl-defmethod magent-backend-destroy ((_backend (eql gptel)) handle)
  "Clean up resources for gptel HANDLE."
  (magent-log "INFO gptel backend: destroying handle")
  (ignore handle))

;;; Registration

(magent-backend-register 'gptel (magent-backend-gptel--create))

(provide 'magent-backend-gptel)
;;; magent-backend-gptel.el ends here
