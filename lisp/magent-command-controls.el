;;; magent-command-controls.el --- Core slash controls  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-runtime-api)

(defun magent-command-controls--clear (invocation)
  "Clear the session belonging to INVOCATION."
  (unless (string-empty-p (magent-command-invocation-argument invocation))
    (user-error "/clear does not accept an argument"))
  (magent-runtime-session-clear
   (magent-command-invocation-runtime-session invocation))
  (magent-command-progress invocation "Conversation cleared.")
  (magent-command-complete invocation ""))

(defun magent-command-controls--compact (invocation)
  "Compact the session belonging to INVOCATION."
  (magent-command-defer invocation)
  (magent-runtime-session-compact
   (magent-command-invocation-runtime-session invocation)
   :instruction (magent-command-invocation-argument invocation)
   :observer (magent-command-invocation-observer invocation)
   :approval-provider
   (magent-command-invocation-approval-provider invocation)
   :turn-metadata (magent-command-turn-metadata invocation)
   :on-complete
   (lambda (status result)
     (magent-command-finish invocation status result))))

(defun magent-command-controls-register ()
  "Register reserved Magent session control commands."
  (let ((magent-command--allow-core-registration t))
    (magent-command-register
     "clear"
     :description "Clear the current conversation context."
     :handler #'magent-command-controls--clear
     :owner 'magent-command-controls
     :source-layer 'core)
    (magent-command-register
     "compact"
     :description "Summarize and compact the current conversation context."
     :handler #'magent-command-controls--compact
     :owner 'magent-command-controls
     :source-layer 'core)))

(provide 'magent-command-controls)
;;; magent-command-controls.el ends here
