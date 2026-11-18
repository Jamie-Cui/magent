;;; magent-codex.el --- Codex execution for child-agent jobs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Own a Codex ACP connection for one turn.  Magent owns the durable job and
;; supplies the thread ID when another turn should continue the same work.

;;; Code:

(require 'acp)
(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'magent-approval)
(require 'magent-log)
(require 'magent-redaction)

(declare-function codex-acp-make-client "ext:codex-acp" (&optional buffer scope))

(defun magent-codex--permission-option (decision options)
  "Translate DECISION into an exact ACP option ID from OPTIONS."
  (if (stringp decision)
      (and (cl-find decision options
                    :key (lambda (option) (map-elt option 'optionId))
                    :test #'equal)
           decision)
    (let* ((kind (pcase decision
                   ('allow-once "allow_once")
                   ('allow-session "allow_always")
                   (_ "reject_once")))
           (option (cl-find kind options
                            :key (lambda (item) (map-elt item 'kind))
                            :test #'equal)))
      (map-elt option 'optionId))))

(cl-defun magent-codex-run-turn
    (directory thread-id model prompt
               &key effort approval-provider audit-context on-session on-complete)
  "Run PROMPT in DIRECTORY on Codex THREAD-ID, or create a fresh thread.
MODEL and EFFORT override Codex defaults when supplied.  ON-SESSION receives
the thread ID and actual model before submission.  ON-COMPLETE receives a
status symbol and text.  Return a function that cancels this turn."
  (unless (require 'codex-acp nil t)
    (user-error "Install codex-acp.el to use the Codex executor"))
  (let ((buffer (generate-new-buffer " *magent-codex*"))
        client session-id active stopping finished cancel-timer
        chunks approval-ids)
    (cl-labels
        ((drop-approvals ()
           (when approval-ids
             (magent-approval-drop-requests
              (lambda (id _entry) (member id approval-ids)))
             (setq approval-ids nil)))
         (finish (status message)
           (unless finished
             (setq finished t)
             (drop-approvals)
             (when cancel-timer (cancel-timer cancel-timer))
             (unwind-protect
                 (when client
                   (condition-case err
                       (acp-shutdown :client client)
                     (error
                      (magent-log "ERROR closing Codex ACP: %s"
                                  (error-message-string err)))))
               (when (buffer-live-p buffer) (kill-buffer buffer)))
             (funcall on-complete status (magent-redaction-string message))))
         (fail (err)
           (finish (if stopping 'cancelled 'failed)
                   (if stopping "Codex turn interrupted"
                     (or (map-elt err 'message) "Codex ACP request failed"))))
         (request (message success)
           (acp-send-request
            :client client :buffer buffer :request message
            :on-failure #'fail
            :on-success
            (lambda (response)
              (unless finished
                (condition-case err
                    (funcall success response)
                  (error
                   (fail `((message . ,(error-message-string err))))))))))
         (notification (event)
           (when (and active (not stopping) (not finished)
                      (equal (map-elt event 'method) "session/update")
                      (equal (map-nested-elt event '(params sessionId))
                             session-id)
                      (equal (map-nested-elt event '(params update sessionUpdate))
                             "agent_message_chunk"))
             (let ((content (map-nested-elt event '(params update content))))
               (when (and (equal (map-elt content 'type) "text")
                          (stringp (map-elt content 'text)))
                 (push (map-elt content 'text) chunks)))))
         (permission (incoming)
           (let* ((id (map-elt incoming 'id))
                  (params (map-elt incoming 'params))
                  (call (map-elt params 'toolCall))
                  (options (append (map-elt params 'options) nil)))
             (when (and id (equal (map-elt incoming 'method)
                                  "session/request_permission"))
               (if (and active (not stopping) (not finished) options
                        (equal (map-elt params 'sessionId) session-id))
                   (let ((approval-id (format "codex-%s" (gensym))))
                     (push approval-id approval-ids)
                     (condition-case err
                         (magent-approval-request
                          (list :request-id approval-id
                                :provider approval-provider
                                :audit-context audit-context
                                :tool-name "codex"
                                :perm-key 'agent
                                :summary (map-elt call 'title)
                                :acp-tool-call call
                                :acp-permission-options (vconcat options))
                          (lambda (decision)
                            (setq approval-ids (delete approval-id approval-ids))
                            (unless (or stopping finished)
                              (let ((option-id
                                     (magent-codex--permission-option
                                      decision options)))
                                (acp-send-response
                                 :client client
                                 :response
                                 (acp-make-session-request-permission-response
                                  :request-id id :option-id option-id
                                  :cancelled (not option-id)))))))
                       (error
                        (acp-send-response
                         :client client
                         :response (acp-make-session-request-permission-response
                                    :request-id id :cancelled t))
                        (magent-log "ERROR forwarding Codex approval: %s"
                                    (error-message-string err)))))
                 (acp-send-response
                  :client client
                  :response (acp-make-session-request-permission-response
                             :request-id id :cancelled t))))))
         (prompt-turn (actual-model)
           (funcall on-session session-id actual-model)
           (unless finished
             (setq active t)
             (request
              (acp-make-session-prompt-request
               :session-id session-id
               :prompt (vector `((type . "text")
                                 (text . ,(magent-redaction-string prompt)))))
              (lambda (response)
                (let ((reason (map-elt response 'stopReason))
                      (output (string-join (nreverse chunks))))
                  (cond
                   ((or stopping (equal reason "cancelled"))
                    (finish 'cancelled "Codex turn interrupted"))
                   ((and (equal reason "end_turn") (not (string-blank-p output)))
                    (finish 'completed output))
                   (t (finish 'failed
                              (format "Codex ended without a complete answer (%s)%s"
                                      reason
                                      (if (string-blank-p output) ""
                                        (concat "\n" output)))))))))))
         (select-effort (actual-model options)
           (if (and effort (not (equal effort "auto")))
               (let* ((option (cl-find "effort" (append options nil)
                                       :key (lambda (item) (map-elt item 'id))
                                       :test #'equal))
                      (values (append (map-elt option 'options) nil)))
                 (unless (cl-find effort values
                                  :key (lambda (item) (map-elt item 'value))
                                  :test #'equal)
                   (error "Codex model %s does not offer effort %s"
                          actual-model effort))
                 (request (acp-make-session-set-config-option-request
                           :session-id session-id :config-id "effort"
                           :value effort)
                          (lambda (_) (prompt-turn actual-model))))
             (prompt-turn actual-model)))
         (session-ready (response)
           (setq session-id (map-elt response 'sessionId))
           (unless (and (stringp session-id) (not (string-empty-p session-id))
                        (or (null thread-id) (equal thread-id session-id)))
             (error "Codex returned an unexpected session ID"))
           (let ((current (map-nested-elt response '(models currentModelId))))
             ;; Save the thread even if the requested model or effort fails.
             (funcall on-session session-id current)
             (unless finished
               (if (and model (not (equal model current)))
                   (request (acp-make-session-set-config-option-request
                             :session-id session-id :config-id "model" :value model)
                            (lambda (result)
                              (select-effort model (map-elt result 'configOptions))))
                 (select-effort current (map-elt response 'configOptions))))))
         (cancel ()
           (unless (or stopping finished)
             (setq stopping t)
             (drop-approvals)
             (if active
                 (condition-case err
                     (progn
                       (setq cancel-timer
                             (run-at-time 5 nil #'finish 'cancelled
                                          "Codex cancellation timed out; connection closed"))
                       (acp-send-notification
                        :client client
                        :notification (acp-make-session-cancel-notification
                                       :session-id session-id)))
                   (error
                    (finish 'cancelled (error-message-string err))))
               (finish 'cancelled "Codex startup cancelled")))))
      (condition-case err
          (progn
            (with-current-buffer buffer
              (setq default-directory directory))
            (setq client (codex-acp-make-client buffer 'directory))
            (acp-subscribe-to-notifications :client client :buffer buffer
                                            :on-notification #'notification)
            (acp-subscribe-to-requests :client client :buffer buffer
                                       :on-request #'permission)
            (request
             (acp-make-initialize-request
              :protocol-version 1 :client-info '((name . "magent")))
             (lambda (response)
               (unless (equal (map-elt response 'protocolVersion) 1)
                 (error "Unsupported Codex ACP protocol version"))
               (request (if thread-id
                            (acp-make-session-load-request
                             :cwd directory :session-id thread-id)
                          (acp-make-session-new-request :cwd directory))
                        #'session-ready))))
        (error (fail `((message . ,(error-message-string err))))))
      #'cancel)))

(provide 'magent-codex)
;;; magent-codex.el ends here
