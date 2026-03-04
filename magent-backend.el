;;; magent-backend.el --- Backend abstraction for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Backend abstraction layer for Magent.  Defines the protocol that all
;; backends must implement.  Two backends are provided:
;;
;; - magent-backend-gptel: Delegates to gptel's built-in tool-calling loop
;; - magent-backend-fsm: Custom FSM implementation with fine-grained control
;;
;; Backends must implement:
;;   - magent-backend-start: Start processing a request
;;   - magent-backend-abort: Abort an in-progress request
;;   - magent-backend-destroy: Clean up resources

;;; Code:

(require 'cl-lib)

;;; Backend Protocol

(cl-defstruct (magent-backend-request (:constructor magent-backend-request-create))
  "Request parameters passed to backend."
  session           ; magent-session struct
  agent             ; magent-agent-info struct
  backend           ; gptel-backend
  model             ; Model symbol
  prompt-list       ; Conversation history
  system-prompt     ; System prompt string
  tools             ; List of available tools
  streaming-p       ; Whether to stream responses
  permission        ; Permission rules
  callback          ; User callback (response-string) -> void
  ui-callback)      ; UI streaming callback (chunk) -> void

(cl-defgeneric magent-backend-start (backend request)
  "Start processing REQUEST using BACKEND.
Returns an opaque handle that can be passed to abort/destroy.")

(cl-defgeneric magent-backend-abort (backend handle)
  "Abort the in-progress request identified by HANDLE.")

(cl-defgeneric magent-backend-destroy (backend handle)
  "Clean up resources for HANDLE.")

;;; Backend Registry

(defvar magent-backend--registry (make-hash-table :test 'eq)
  "Registry mapping backend symbols to backend instances.")

(defun magent-backend-register (name backend)
  "Register BACKEND under NAME (a symbol)."
  (puthash name backend magent-backend--registry))

(defun magent-backend-get (name)
  "Retrieve backend registered under NAME."
  (gethash name magent-backend--registry))

(defun magent-backend-list ()
  "Return list of all registered backend names."
  (let (keys)
    (maphash (lambda (k _v) (push k keys)) magent-backend--registry)
    keys))

(provide 'magent-backend)
;;; magent-backend.el ends here
