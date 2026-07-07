;;; magent-events.el --- Compatibility shim for magent-lifecycle-events  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Lifecycle event sink support now lives in `magent-lifecycle-events.el'.

;;; Code:

(require 'magent-lifecycle-events)

(defvaralias 'magent-events--sinks 'magent-lifecycle-events--sinks)
(defvaralias 'magent-events--current-context
  'magent-lifecycle-events--current-context)

(define-obsolete-function-alias
  'magent-events-context-create
  #'magent-lifecycle-events-context-create
  "0.1.0")
(define-obsolete-function-alias
  'magent-events-generate-id
  #'magent-lifecycle-events-generate-id
  "0.1.0")
(define-obsolete-function-alias
  'magent-events-current-context
  #'magent-lifecycle-events-current-context
  "0.1.0")
(define-obsolete-function-alias
  'magent-events-add-sink
  #'magent-lifecycle-events-add-sink
  "0.1.0")
(define-obsolete-function-alias
  'magent-events-remove-sink
  #'magent-lifecycle-events-remove-sink
  "0.1.0")
(define-obsolete-function-alias
  'magent-events-clear-sinks
  #'magent-lifecycle-events-clear-sinks
  "0.1.0")
(define-obsolete-function-alias
  'magent-events-emit
  #'magent-lifecycle-events-emit
  "0.1.0")
(define-obsolete-function-alias
  'magent-events-begin-turn
  #'magent-lifecycle-events-begin-turn
  "0.1.0")
(define-obsolete-function-alias
  'magent-events-end-turn
  #'magent-lifecycle-events-end-turn
  "0.1.0")
(define-obsolete-function-alias
  'magent-events-create-subagent-context
  #'magent-lifecycle-events-create-subagent-context
  "0.1.0")
(define-obsolete-function-alias
  'magent-events-stop-subagent
  #'magent-lifecycle-events-stop-subagent
  "0.1.0")

(dolist (slot '(turn-id subagent-id title ended))
  (let ((old (intern (format "magent-events-context-%s" slot)))
        (new (intern (format "magent-lifecycle-events-context-%s" slot))))
    (defalias old new)))

(provide 'magent-events)
;;; magent-events.el ends here
