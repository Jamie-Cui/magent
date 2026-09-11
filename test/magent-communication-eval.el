;;; magent-communication-eval.el --- Bounded provider acceptance -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Manual, explicitly authorized real-provider evaluation. Run only in an
;; isolated Emacs. The reviewed JSON packet freezes all outbound instructions
;; and fixture observations. Loading this library never sends a request.
;; Reports contain normal assistant messages and reasoning counts, not raw
;; reasoning, credentials, request headers, or unrelated editor context.

;;; Code:

(require 'auth-source)
(require 'gptel)
(require 'gptel-openai-extras)
(require 'magent-agent)
(require 'magent-tools)

(declare-function server-eval-at "server" (server form))

(defvar magent-communication-eval--credential nil
  "Authorized DeepSeek credential held only in the isolated process memory.")

(defun magent-communication-eval-transfer-credential (server)
  "Send the configured DeepSeek credential to local isolated SERVER.
The value stays in Emacs memory and local socket traffic, never command
arguments, files or tool output. Return only whether transfer succeeded."
  (require 'server)
  (let ((gptel-backend (alist-get "DeepSeek" gptel--known-backends nil nil #'equal)))
    (unless (and gptel-backend
                 (equal (gptel-backend-host gptel-backend) "api.deepseek.com"))
      (error "The configured DeepSeek backend is unavailable"))
    (let ((key (gptel--get-api-key)))
      (unless (and (stringp key) (not (string-empty-p key)))
        (error "The configured DeepSeek credential is unavailable"))
      (server-eval-at server `(progn (setq magent-communication-eval--credential ,key) t)))))

(defun magent-communication-eval--read (file)
  "Read the reviewed JSON packet from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :object-type 'plist :array-type 'array
                       :null-object nil :false-object :json-false)))

(defun magent-communication-eval--key ()
  "Resolve only the authorized DeepSeek credential without logging it."
  (let* ((entry (unless magent-communication-eval--credential
                  (car (auth-source-search :host "deepseek" :max 1 :require '(:secret)))))
         (secret (plist-get entry :secret))
         (value (or magent-communication-eval--credential
                    (if (functionp secret) (funcall secret) secret))))
    (unless (and (stringp value) (not (string-empty-p value)))
      (error "DeepSeek credential is unavailable"))
    value))

(defun magent-communication-eval--items (session start)
  "Project SESSION into reviewable items relative to START."
  (vconcat
   (cl-loop for item in (magent-thread-all-items (magent-session-thread-ledger session))
            for type = (magent-thread-item-type item)
            unless (and (eq type 'message) (eq (magent-thread-item-role item) 'user))
            collect
            (append
             (list :type type :status (magent-thread-item-status item)
                   :at (- (magent-thread-item-created-at item) start))
             (pcase type
               ('reasoning (list :characters (length (or (magent-thread-item-content item) ""))))
               ('message (list :phase (magent-thread-item-phase item)
                               :text (magent-thread-item-content item)))
               ('tool (list :name (magent-thread-item-name item)
                            :input (magent-thread-item-input item)))
               ('plan (list :entries (magent-thread-item-output item)))
               (_ nil))))))

(defun magent-communication-eval-run (packet-file report-file run-number &optional dry-run)
  "Run authorized PACKET-FILE once and write REPORT-FILE for RUN-NUMBER.
DRY-RUN validates the initial request without credentials or network access.
Use only in a dedicated Emacs; temporary bindings isolate the fixture tools."
  (let* ((packet (magent-communication-eval--read packet-file))
         (bounds (plist-get packet :bounds))
         (start (float-time))
         (deadline (+ start (plist-get bounds :deadline_seconds_per_run)))
         (session (magent-session-create :id (format "communication-eval-%d" run-number)))
         (gptel-backend (gptel-make-deepseek
                           "Communication fixture" :stream t
                           :key (if dry-run "fixture" #'magent-communication-eval--key)))
         (gptel-model 'deepseek-v4-pro)
         (gptel-max-tokens (plist-get bounds :max_output_tokens_per_request))
         (gptel-temperature nil)
         (gptel--request-params nil)
         (gptel--schema nil)
         (gptel-context nil)
         (gptel-use-context nil)
         (gptel-use-curl t)
         (gptel-log-level nil)
         (gptel-proxy "")
         (gptel-curl-extra-args '("--connect-timeout" "20" "--max-time" "120"))
         (gptel-post-request-hook nil)
         (magent-include-reasoning 'ignore)
         (magent-enable-capabilities nil)
         (magent-context-provider-functions nil)
         (magent-max-sampling-requests (plist-get bounds :max_sampling_requests_per_run))
         (magent-request-timeout 120)
         (request-count 0)
         (transport (symbol-function 'gptel-curl-get-response))
         (agent (magent-agent-info-create :name "build" :permission '(("*" . allow))))
         requests events result loop validation-error
         (context (magent-request-context-create
                   :session session :scope 'global :agent agent
                   :backend gptel-backend :model gptel-model
                   :effort 'xhigh :thinking 'auto :ui-visibility 'none
                   :tool-names '("update_plan" "emacs_read")
                   :prompt (plist-get packet :user)
                   :observer
                   (lambda (event)
                     (when (memq (plist-get event :type)
                                 '(assistant-delta sampling-retry plan-update tool-start tool-complete))
                       (push (list :type (plist-get event :type)
                                   :at (- (float-time) start)
                                   :text (when (eq (plist-get event :type) 'assistant-delta)
                                           (plist-get event :text))) events)))))
         (plan-tool (gptel-make-tool
                     :name "update_plan" :description "Record fixture plan"
                     :args (gptel-tool-args magent-tools--update-plan-tool)
                     :function #'magent-tools--update-plan))
         (read-tool
          (gptel-make-tool
           :name "emacs_read" :description "Read fictional observations"
           :args '((:name "operation" :type string)
                   (:name "check_id" :type string :optional t)
                   (:name "reason" :type string :optional t))
           :function
           (lambda (operation &optional check-id)
             (unless (or (equal operation "route_config")
                         (and (equal operation "test_result") (equal check-id "prefix_routes")))
               (error "Invalid fixture query"))
             (magent-tool-result-create
              :status 'completed :success t
              :output (magent-json-encode
                       (plist-get (plist-get packet :fixture_results) (intern (concat ":" operation)))))))))
    (unless (and (equal (plist-get packet :destination) "https://api.deepseek.com/v1/chat/completions")
                 (equal (plist-get packet :model) "deepseek-v4-pro")
                 (<= 1 run-number (plist-get bounds :runs)))
      (error "Evaluation does not match the reviewed destination, model or run bounds"))
    (cl-letf (((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _) (list plan-tool read-tool)))
              ((symbol-function 'magent-agent--compose-system-message)
               (lambda (&rest _) (plist-get packet :system)))
              ((symbol-function 'magent-session-save-deferred-for-session) #'ignore)
              ((symbol-function 'gptel-curl-get-response)
               (lambda (fsm)
                 (let* ((info (gptel-fsm-info fsm))
                        (data (plist-get info :data))
                        (messages (plist-get data :messages)))
                   ;; Use the reviewed schemas verbatim, keeping their executable
                   ;; counterparts limited to the two local fixture functions.
                   (plist-put data :tools (copy-tree (plist-get packet :tools) t))
                   (unless (and (< request-count magent-max-sampling-requests)
                                (< (float-time) deadline)
                                (equal (gptel-backend-url (plist-get info :backend))
                                       (plist-get packet :destination))
                                (equal (plist-get data :model) (plist-get packet :model))
                                (equal (plist-get data :max_tokens) gptel-max-tokens)
                                (equal (plist-get (aref messages 0) :content) (plist-get packet :system))
                                (equal (plist-get (aref messages 1) :content) (plist-get packet :user))
                                (cl-every (lambda (message)
                                            (member (plist-get message :role) '("assistant" "tool")))
                                          (seq-drop messages 2)))
                     (setq validation-error "Outbound request failed the reviewed scope check")
                     (error "%s" validation-error))
                   (push (list :at (- (float-time) start) :messages (length messages)
                               :max-tokens (plist-get data :max_tokens)
                               :effort (plist-get data :reasoning_effort)) requests)
                   (unless dry-run
                     (cl-incf request-count)
                     (funcall transport fsm))))))
      (unwind-protect
          (condition-case err
              (progn
                (unless dry-run (magent-communication-eval--key))
                (setq loop (magent-agent-run-turn context :on-complete (lambda (value) (setq result value))))
                (unless dry-run
                  (while (and (not result) (< (float-time) deadline))
                    (accept-process-output nil 0.05))))
            (error (setq validation-error (error-message-string err))))
        (when (and loop (not result)) (magent-agent-loop-abort loop))))
    (let ((report (list :run run-number :dry-run (if dry-run t :json-false)
                        :status (cond (validation-error "harness-error")
                                      (dry-run "validated")
                                      (result (symbol-name (magent-execution-result-status result)))
                                      (t "deadline"))
                        :elapsed (- (float-time) start) :requests request-count
                        :request-metadata (vconcat (nreverse requests))
                        :error (or validation-error (and result (magent-execution-result-error result)))
                        :items (magent-communication-eval--items session start)
                        :events (vconcat (nreverse events)))))
      (with-temp-file report-file (insert (magent-json-encode report)))
      (list :run run-number :status (plist-get report :status) :requests request-count
            :report report-file))))

(provide 'magent-communication-eval)
;;; magent-communication-eval.el ends here
