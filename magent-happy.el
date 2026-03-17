;;; magent-happy.el --- Happy bridge for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; Bridge Magent events and approvals to an external helper process that
;; speaks Happy-compatible session semantics.  The helper protocol is
;; line-delimited JSON over stdio.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magent-approval)
(require 'magent-events)
(require 'magent-queue)

(declare-function magent-interrupt "magent-ui")
(declare-function magent-log "magent-config")
(declare-function magent-ui-dispatch-prompt "magent-ui")
(declare-function magent-ui-process "magent-ui")

(defcustom magent-happy-helper-command nil
  "Command vector used to start the Happy helper process.
The process must speak line-delimited JSON over stdio."
  :type '(repeat string)
  :group 'magent)

(defcustom magent-happy-session-tag "magent"
  "Default tag sent to the Happy helper when starting a remote session."
  :type 'string
  :group 'magent)

(defvar magent-happy--process nil
  "Current Happy helper process.")

(defvar magent-happy--buffer ""
  "Buffered partial helper output.")

(defvar magent-happy--connected nil
  "Non-nil when Happy remote control is enabled.")

(defvar magent-happy--session-id nil
  "Current Happy session identifier.")

(defvar magent-happy--pending-approvals (make-hash-table :test 'equal)
  "Pending approval callbacks keyed by request id.")

(defvar magent-happy--completed-approvals nil
  "Completed approval decisions keyed by request id.")

(defvar magent-happy--previous-approval-provider nil
  "Approval provider active before Happy remote control was enabled.")

(defun magent-happy-connected-p ()
  "Return non-nil when Happy remote control is enabled."
  magent-happy--connected)

(defun magent-happy--json-encode (obj)
  "Encode OBJ as a compact JSON string."
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-false :false)
        (json-null nil))
    (json-encode obj)))

(defun magent-happy--send (payload)
  "Send PAYLOAD to the helper process."
  (when (process-live-p magent-happy--process)
    (process-send-string magent-happy--process
                         (concat (magent-happy--json-encode payload) "\n"))))

(defun magent-happy--update-agent-state ()
  "Push the current remote-control state to the helper."
  (magent-happy--send
   (list :type "agent-state"
         :session_id magent-happy--session-id
         :controlled_by_user (if magent-happy--connected t :false)
         :requests (hash-table-count magent-happy--pending-approvals)
         :completed_requests (length magent-happy--completed-approvals))))

(defun magent-happy--event-title (event)
  "Return a short title for EVENT."
  (pcase (plist-get event :type)
    ('tool-call-start
     (format "%s" (or (plist-get event :title)
                      (plist-get event :tool-name))))
    (_ (or (plist-get event :title) ""))))

(defun magent-happy--event-description (event)
  "Return a longer description for EVENT."
  (pcase (plist-get event :type)
    ('tool-call-start
     (or (plist-get event :description)
         (format "%s" (or (plist-get event :summary)
                          (plist-get event :tool-name)))))
    (_ (or (plist-get event :detail) ""))))

(defun magent-happy--event->envelope (event)
  "Convert a Magent EVENT plist to a Happy session envelope plist."
  (let* ((context (plist-get event :context))
         (turn-id (plist-get event :turn-id))
         (subagent-id (plist-get event :subagent-id))
         (event-id (magent-events-generate-id))
         (ev
          (pcase (plist-get event :type)
            ('turn-start (list :t "turn-start"))
            ('turn-end (list :t "turn-end"
                             :status (symbol-name (plist-get event :status))))
            ('subagent-start (list :t "start"
                                   :title (or (plist-get event :title)
                                              (and context
                                                   (magent-events-context-title context)))))
            ('subagent-stop (list :t "stop"))
            ('service (list :t "service"
                            :text (or (plist-get event :text) "")))
            ('text-delta (list :t "text"
                               :text (or (plist-get event :text) "")))
            ('tool-call-start
             (list :t "tool-call-start"
                   :call (plist-get event :call-id)
                   :name (or (plist-get event :tool-name) "")
                   :title (magent-happy--event-title event)
                   :description (magent-happy--event-description event)
                   :args (or (plist-get event :args) (list))))
            ('tool-call-end
             (list :t "tool-call-end"
                   :call (plist-get event :call-id)))
            (_ nil))))
    (when ev
      (append (list :id event-id
                    :time (floor (* 1000 (or (plist-get event :time) (float-time))))
                    :role "agent"
                    :ev ev)
              (when turn-id (list :turn turn-id))
              (when subagent-id (list :subagent subagent-id))))))

(defun magent-happy--event-sink (event)
  "Forward EVENT to the Happy helper."
  (when-let ((envelope (magent-happy--event->envelope event)))
    (magent-happy--send (list :type "event"
                              :session_id magent-happy--session-id
                              :envelope envelope))
    (when (eq (plist-get event :type) 'turn-end)
      (magent-happy--send (list :type "ready"
                                :session_id magent-happy--session-id)))))

(defun magent-happy--approval-provider (request callback)
  "Forward REQUEST to the Happy helper and resolve via CALLBACK."
  (let ((request-id (or (plist-get request :request-id)
                        (magent-events-generate-id))))
    (puthash request-id callback magent-happy--pending-approvals)
    (push (cons request-id nil) magent-happy--completed-approvals)
    (magent-happy--send
     (list :type "approval-request"
           :session_id magent-happy--session-id
           :request_id request-id
           :tool_name (plist-get request :tool-name)
           :perm_key (when-let ((perm-key (plist-get request :perm-key)))
                       (symbol-name perm-key))
           :summary (plist-get request :summary)
           :args (plist-get request :args)))
    (magent-happy--update-agent-state)))

(defun magent-happy--handle-approval-response (payload)
  "Handle approval response PAYLOAD from the helper."
  (let* ((request-id (plist-get payload :request_id))
         (decision (intern (or (plist-get payload :decision) "deny-once")))
         (callback (gethash request-id magent-happy--pending-approvals)))
    (when callback
      (remhash request-id magent-happy--pending-approvals)
      (setf (alist-get request-id magent-happy--completed-approvals nil nil #'equal)
            decision)
      (magent-happy--update-agent-state)
      (funcall callback decision))))

(defun magent-happy--resolve-pending-approvals (decision)
  "Resolve all pending approvals with DECISION."
  (let (pending)
    (maphash (lambda (request-id callback)
               (push (cons request-id callback) pending))
             magent-happy--pending-approvals)
    (dolist (entry pending)
      (let ((request-id (car entry))
            (callback (cdr entry)))
        (remhash request-id magent-happy--pending-approvals)
        (setf (alist-get request-id magent-happy--completed-approvals nil nil #'equal)
              decision)
        (when callback
          (condition-case err
              (funcall callback decision)
            (error
             (message "Magent Happy approval cleanup failed: %s"
                      (error-message-string err)))))))))

(defun magent-happy--cleanup (&optional resolve-decision)
  "Tear down Happy remote-control state.
When RESOLVE-DECISION is non-nil, resolve all pending approvals first."
  (when resolve-decision
    (magent-happy--resolve-pending-approvals resolve-decision))
  (setq magent-happy--connected nil)
  (magent-events-remove-sink #'magent-happy--event-sink)
  (when magent-happy--previous-approval-provider
    (setq magent-approval-provider-function magent-happy--previous-approval-provider)
    (setq magent-happy--previous-approval-provider nil))
  (setq magent-happy--process nil
        magent-happy--session-id nil
        magent-happy--buffer "")
  (clrhash magent-happy--pending-approvals)
  (setq magent-happy--completed-approvals nil))

(defun magent-happy--handle-user-message (payload)
  "Handle remote user message PAYLOAD."
  (let ((text (plist-get payload :text)))
    (if (magent-queue-processing-p)
        (magent-events-emit 'service
                            :text "Magent is busy; wait for the current request to finish.")
      (magent-ui-dispatch-prompt text 'happy-remote text nil nil))))

(defun magent-happy--handle-message (payload)
  "Handle decoded helper PAYLOAD."
  (pcase (plist-get payload :type)
    ("connected"
     (setq magent-happy--session-id (plist-get payload :session_id))
     (magent-events-emit 'service
                         :text (format "Connected to Happy session %s"
                                       magent-happy--session-id))
     (magent-happy--update-agent-state))
    ("session-attached"
     (setq magent-happy--session-id (plist-get payload :session_id))
     (magent-happy--update-agent-state))
    ("user-message"
     (magent-happy--handle-user-message payload))
    ("approval-response"
     (magent-happy--handle-approval-response payload))
    ("abort"
     (magent-interrupt))
    ("error"
     (message "Magent Happy error: %s" (or (plist-get payload :message) "unknown error")))))

(defun magent-happy--process-filter (_proc chunk)
  "Process helper CHUNK."
  (setq magent-happy--buffer (concat magent-happy--buffer chunk))
  (while (string-match "\n" magent-happy--buffer)
    (let* ((line (substring magent-happy--buffer 0 (match-beginning 0)))
           (rest (substring magent-happy--buffer (match-end 0))))
      (setq magent-happy--buffer rest)
      (unless (string-empty-p line)
        (condition-case err
            (let ((json-object-type 'plist)
                  (json-array-type 'list)
                  (json-false :false)
                  (json-null nil))
              (magent-happy--handle-message (json-read-from-string line)))
          (error
           (message "Magent Happy parse error: %s" (error-message-string err))))))))

(defun magent-happy--process-sentinel (_proc event)
  "Handle helper process EVENT."
  (unless (process-live-p magent-happy--process)
    (magent-happy--cleanup 'deny-once)
    (message "Magent Happy helper exited: %s" (string-trim event))))

(defun magent-happy--start-helper ()
  "Start the Happy helper process."
  (unless (and (listp magent-happy-helper-command)
               magent-happy-helper-command)
    (user-error "Set `magent-happy-helper-command' before enabling Happy remote control"))
  (let ((proc (make-process
               :name "magent-happy"
               :buffer (get-buffer-create " *magent-happy*")
               :command magent-happy-helper-command
               :coding 'utf-8
               :filter #'magent-happy--process-filter
               :sentinel #'magent-happy--process-sentinel)))
    (setq magent-happy--process proc)
    proc))

;;;###autoload
(defun magent-happy-enable-remote-control (&optional session-id)
  "Enable Happy remote control, optionally attaching to SESSION-ID."
  (interactive)
  (unless (process-live-p magent-happy--process)
    (magent-happy--start-helper))
  (unless magent-happy--connected
    (setq magent-happy--connected t)
    (setq magent-happy--previous-approval-provider magent-approval-provider-function)
    (setq magent-approval-provider-function #'magent-happy--approval-provider)
    (magent-events-add-sink #'magent-happy--event-sink))
  (magent-happy--send
   (if session-id
       (list :type "attach-session" :session_id session-id)
     (list :type "create-session" :tag magent-happy-session-tag)))
  (magent-events-emit 'service :text "Happy remote control enabled."))

;;;###autoload
(defun magent-happy-disable-remote-control ()
  "Disable Happy remote control and stop the helper process."
  (interactive)
  (let ((proc magent-happy--process))
    (when (process-live-p proc)
      (magent-happy--send (list :type "disconnect"
                                :session_id magent-happy--session-id))
      (delete-process proc)))
  (magent-happy--cleanup 'deny-once)
  (message "Magent Happy remote control disabled"))

;;;###autoload
(defun magent-happy-connect ()
  "Alias for `magent-happy-enable-remote-control'."
  (interactive)
  (magent-happy-enable-remote-control))

;;;###autoload
(defun magent-happy-disconnect ()
  "Alias for `magent-happy-disable-remote-control'."
  (interactive)
  (magent-happy-disable-remote-control))

;;;###autoload
(defun magent-happy-start-session ()
  "Start a new Happy-backed remote session."
  (interactive)
  (magent-happy-enable-remote-control nil))

;;;###autoload
(defun magent-happy-attach-session (session-id)
  "Attach to an existing Happy SESSION-ID."
  (interactive "sHappy session id: ")
  (magent-happy-enable-remote-control session-id))

(provide 'magent-happy)
;;; magent-happy.el ends here
