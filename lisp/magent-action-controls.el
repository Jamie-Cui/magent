;;; magent-action-controls.el --- Core slash controls  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-action)
(require 'magent-runtime-api)

(magent-define-workflow magent-action-controls--compact (invocation)
  "Compact the session belonging to INVOCATION."
  (let ((result
         (magent-workflow-callback
             "Compact conversation"
             (lambda (done)
               (magent-runtime-session-compact
                (magent-action-invocation-runtime-session invocation)
                :instruction (magent-action-invocation-argument invocation)
                :approval-provider
                (magent-action-invocation-approval-provider invocation)
                :turn-metadata (magent-action-turn-metadata invocation)
                :on-complete done)
               (lambda ()
                 (magent-runtime-cancel
                  (magent-action-invocation-runtime-session invocation)))))))
    (magent-execution-result-content-string result)))

(defun magent-action-controls-register ()
  "Register the reserved Magent session control Action."
  (let ((magent-action--allow-core-registration t))
    (magent-action-register
     "compact"
     :description "Summarize and compact the current conversation context."
     :session-policy 'current
     :workflow #'magent-action-controls--compact
     :source-layer 'core)))

(provide 'magent-action-controls)
;;; magent-action-controls.el ends here
