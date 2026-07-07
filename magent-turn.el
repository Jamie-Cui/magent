;;; magent-turn.el --- Compatibility shim for magent-legacy-queue  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Legacy UI submission queue code now lives in `magent-legacy-queue.el'.
;; This file preserves the historical `(require 'magent-turn)' feature and
;; function names for external callers.

;;; Code:

(require 'magent-legacy-queue)

(defvaralias 'magent-turn--active 'magent-legacy-queue--active)
(defvaralias 'magent-turn--queue 'magent-legacy-queue--pending)
(defvaralias 'magent-turn--current-request-handle
  'magent-legacy-queue--current-request-handle)

(define-obsolete-function-alias
  'magent-turn-submission-create
  #'magent-legacy-queue-submission-create
  "0.1.0")
(define-obsolete-function-alias
  'magent-turn-submission-p
  #'magent-legacy-queue-submission-p
  "0.1.0")
(define-obsolete-function-alias
  'magent-turn-active-submission
  #'magent-legacy-queue-active-submission
  "0.1.0")
(define-obsolete-function-alias
  'magent-turn-processing-p
  #'magent-legacy-queue-processing-p
  "0.1.0")
(define-obsolete-function-alias
  'magent-turn-pending-p
  #'magent-legacy-queue-pending-p
  "0.1.0")
(define-obsolete-function-alias
  'magent-turn-queue-length
  #'magent-legacy-queue-length
  "0.1.0")
(define-obsolete-function-alias
  'magent-turn-current-request-handle
  #'magent-legacy-queue-current-request-handle
  "0.1.0")
(define-obsolete-function-alias
  'magent-turn-set-current-request-handle
  #'magent-legacy-queue-set-current-request-handle
  "0.1.0")
(define-obsolete-function-alias
  'magent-turn-active-id-p
  #'magent-legacy-queue-active-id-p
  "0.1.0")
(define-obsolete-function-alias
  'magent-turn-submit
  #'magent-legacy-queue-submit
  "0.1.0")
(define-obsolete-function-alias
  'magent-turn-finish
  #'magent-legacy-queue-finish
  "0.1.0")
(define-obsolete-function-alias
  'magent-turn-clear-queue
  #'magent-legacy-queue-clear
  "0.1.0")
(define-obsolete-function-alias
  'magent-turn-interrupt
  #'magent-legacy-queue-interrupt
  "0.1.0")

(dolist (slot '(id op payload dispatcher status submitted-at started-at
                finished-at turn-id))
  (let ((old (intern (format "magent-turn-submission-%s" slot)))
        (new (intern (format "magent-legacy-queue-submission-%s" slot))))
    (defalias old new)))

(provide 'magent-turn)
;;; magent-turn.el ends here
