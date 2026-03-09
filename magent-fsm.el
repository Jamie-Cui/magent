;;; magent-fsm.el --- FSM for tool calling loop  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Finite state machine for managing LLM request/response cycles with tool calling.
;; This FSM handles the complete loop: send request → receive response → execute tools
;; → inject results → send new request → ... until done.
;;
;; States:
;;   INIT    - Initial state, ready to start
;;   SEND    - Preparing HTTP request
;;   WAIT    - HTTP request in flight
;;   PROCESS - Processing LLM response
;;   TOOL    - Executing tool calls
;;   DONE    - Terminal state (success)
;;   ERROR   - Terminal state (failure)

;;; Code:

;; Forward declarations
(declare-function magent-fsm-backend-gptel-create "magent-fsm-backend-gptel")
(declare-function magent-fsm-backend-gptel-start "magent-fsm-backend-gptel")
(declare-function magent-fsm-backend-gptel-abort "magent-fsm-backend-gptel")
(declare-function magent-fsm-backend-gptel-destroy "magent-fsm-backend-gptel")
(declare-function magent-fsm-native-create "magent-fsm-backend-native")
(declare-function magent-fsm-native-start "magent-fsm-backend-native")
(declare-function magent-fsm-native-abort "magent-fsm-backend-native")
(declare-function magent-fsm-native-destroy "magent-fsm-backend-native")
(defvar magent-fsm-backend)

;;; Unified FSM API

(defun magent-fsm--require-backend ()
  "Ensure the configured FSM backend is loaded."
  (require 'magent-config)
  (when (eq magent-fsm-backend 'gptel)
    (require 'magent-fsm-backend-gptel)))

(defun magent-fsm-create (&rest args)
  "Create FSM using configured backend."
  (magent-fsm--require-backend)
  (pcase magent-fsm-backend
    ('magent (apply #'magent-fsm-native-create args))
    ('gptel (apply #'magent-fsm-backend-gptel-create args))
    (_ (error "Unknown FSM backend: %s" magent-fsm-backend))))

(defun magent-fsm-start (fsm)
  "Start FSM execution."
  (magent-fsm--require-backend)
  (pcase magent-fsm-backend
    ('magent (magent-fsm-native-start fsm))
    ('gptel (magent-fsm-backend-gptel-start fsm))
    (_ (error "Unknown FSM backend: %s" magent-fsm-backend))))

(defun magent-fsm-abort (fsm)
  "Abort FSM execution."
  (magent-fsm--require-backend)
  (pcase magent-fsm-backend
    ('magent (magent-fsm-native-abort fsm))
    ('gptel (magent-fsm-backend-gptel-abort fsm))
    (_ (error "Unknown FSM backend: %s" magent-fsm-backend))))

(defun magent-fsm-destroy (fsm)
  "Destroy FSM and clean up resources."
  (magent-fsm--require-backend)
  (pcase magent-fsm-backend
    ('magent (magent-fsm-native-destroy fsm))
    ('gptel (magent-fsm-backend-gptel-destroy fsm))
    (_ (error "Unknown FSM backend: %s" magent-fsm-backend))))


;; Load native backend
(require 'magent-fsm-backend-native)

(provide 'magent-fsm)
;;; magent-fsm.el ends here
