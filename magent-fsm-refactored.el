;;; magent-fsm-refactored.el --- Refactored FSM with state pattern  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Refactored FSM implementation using the State pattern.
;;
;; Benefits of this refactoring:
;; 1. Cleaner state separation - each state is a separate handler function
;; 2. Easier to add new states - just add to the handler table
;; 3. Better testability - states can be tested independently
;; 4. More maintainable - state logic is localized
;;
;; This file demonstrates the refactoring but doesn't replace magent-fsm.el
;; to maintain backward compatibility. To use this version, agents would need
;; to switch from `magent-fsm-start` to `magent-fsm-refactored-start`.

;;; Code:

(require 'cl-lib)
(require 'magent-fsm)  ; Reuse data structures and utilities

;;; State handler table

(defconst magent-fsm-refactored--handlers
  '((INIT    . magent-fsm-refactored--handle-init)
    (SEND    . magent-fsm-refactored--handle-send)
    (WAIT    . ignore)  ; Waiting state has no handler
    (PROCESS . magent-fsm-refactored--handle-process)
    (TOOL    . magent-fsm-refactored--handle-tool)
    (DONE    . magent-fsm-refactored--handle-done)
    (ERROR   . magent-fsm-refactored--handle-error))
  "Alist mapping FSM states to their handler functions.
Adding a new state requires:
1. Add entry to this table
2. Implement the handler function
3. Update transitions in relevant handlers")

;;; Core state machine logic

(defun magent-fsm-refactored-transition (fsm new-state)
  "Transition FSM to NEW-STATE and execute its handler.
Uses the handler table for dispatch instead of pcase."
  (magent-log "FSM transition: %s -> %s" (magent-fsm-state fsm) new-state)
  (setf (magent-fsm-state fsm) new-state)
  (let ((handler (alist-get new-state magent-fsm-refactored--handlers)))
    (when handler
      (funcall handler fsm))))

;;; State handlers (delegates to original implementations)

(defun magent-fsm-refactored--handle-init (fsm)
  "INIT state: validate and prepare for first request."
  ;; Reuse original implementation
  (magent-fsm--handle-init fsm))

(defun magent-fsm-refactored--handle-send (fsm)
  "SEND state: fire HTTP request via gptel."
  ;; Reuse original implementation
  (magent-fsm--handle-send fsm))

(defun magent-fsm-refactored--handle-process (fsm)
  "PROCESS state: determine next action based on response."
  ;; Reuse original implementation
  (magent-fsm--handle-process fsm))

(defun magent-fsm-refactored--handle-tool (fsm)
  "TOOL state: execute pending tool calls."
  ;; Reuse original implementation
  (magent-fsm--handle-tool fsm))

(defun magent-fsm-refactored--handle-done (fsm)
  "DONE state: finalize and cleanup."
  ;; Reuse original implementation
  (magent-fsm--handle-done fsm))

(defun magent-fsm-refactored--handle-error (fsm)
  "ERROR state: handle errors and cleanup."
  ;; Reuse original implementation
  (magent-fsm--handle-error fsm))

;;; Public API

(defun magent-fsm-refactored-start (fsm)
  "Start FSM execution using refactored state pattern.
Drop-in replacement for `magent-fsm-start'."
  (unless (eq (magent-fsm-state fsm) 'INIT)
    (error "FSM must be in INIT state to start"))
  (magent-fsm-refactored-transition fsm 'INIT))

;;; Example: Adding a new state

;; To add a new VALIDATE state that runs before SEND:
;;
;; 1. Add to handler table:
;;    (defconst magent-fsm-refactored--handlers
;;      '(... (VALIDATE . magent-fsm-refactored--handle-validate) ...))
;;
;; 2. Implement handler:
;;    (defun magent-fsm-refactored--handle-validate (fsm)
;;      "VALIDATE state: check inputs before sending."
;;      (if (magent-fsm-refactored--validate-inputs fsm)
;;          (magent-fsm-refactored-transition fsm 'SEND)
;;        (setf (magent-fsm-error fsm) "Validation failed")
;;        (magent-fsm-refactored-transition fsm 'ERROR)))
;;
;; 3. Update INIT handler to transition to VALIDATE instead of SEND
;;
;; That's it! The state pattern makes this much cleaner than adding
;; another case to a large pcase statement.

(provide 'magent-fsm-refactored)
;;; magent-fsm-refactored.el ends here
