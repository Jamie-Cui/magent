;;; magent-command-controls.el --- Core slash controls  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command)
(require 'magent-runtime-api)

(magent-command-defworkflow magent-command-controls--compact (invocation)
  "Compact the session belonging to INVOCATION."
  (let ((result
         (magent-command-callback
             "Compact conversation"
             (lambda (done)
               (magent-runtime-session-compact
                (magent-command-invocation-runtime-session invocation)
                :instruction (magent-command-invocation-argument invocation)
                :approval-provider
                (magent-command-invocation-approval-provider invocation)
                :turn-metadata (magent-command-turn-metadata invocation)
                :on-complete done)
               (lambda ()
                 (magent-runtime-cancel
                  (magent-command-invocation-runtime-session invocation)))))))
    (magent-agent-result-content-string result)))

(defun magent-command-controls-register ()
  "Register the reserved Magent session control command."
  (let ((magent-command--allow-core-registration t))
    (magent-command-register
     "compact"
     :description "Summarize and compact the current conversation context."
     :session-policy 'current
     :workflow #'magent-command-controls--compact
     :source-layer 'core)))

(provide 'magent-command-controls)
;;; magent-command-controls.el ends here
