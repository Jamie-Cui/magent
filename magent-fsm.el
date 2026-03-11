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

;; FIXME: native FSM backend (magent-fsm-backend-native.el) is disabled.
;; Only the gptel backend is currently supported.

(require 'magent-config)
(require 'magent-fsm-backend-gptel)

;;; Unified FSM API

(defun magent-fsm-create (&rest args)
  "Create FSM using configured backend."
  (apply #'magent-fsm-backend-gptel-create args))

(defun magent-fsm-start (fsm)
  "Start FSM execution."
  (magent-fsm-backend-gptel-start fsm))

(defun magent-fsm-abort (fsm)
  "Abort FSM execution."
  (magent-fsm-backend-gptel-abort fsm))

(defun magent-fsm-destroy (fsm)
  "Destroy FSM and clean up resources."
  (magent-fsm-backend-gptel-destroy fsm))

(provide 'magent-fsm)
;;; magent-fsm.el ends here
