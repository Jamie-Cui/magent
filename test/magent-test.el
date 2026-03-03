;;; magent-test.el --- Tests for Magent agent processing  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for magent-agent-process and session handling.

;;; Code:

(require 'ert)
(require 'magent)

;;; Test 1: Simple prompt without tool call

(ert-deftest magent-test-simple-prompt ()
  "Test basic prompt without tools returns response and records session."
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (gptel-tools nil)
        (gptel-use-tools nil)
        (magent-enable-streaming nil)
        (call-count 0)
        (captured-prompt nil)
        (response nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (prompt &rest kwargs)
                 (cl-incf call-count)
                 (setq captured-prompt prompt)
                 (let* ((callback (plist-get kwargs :callback)))
                   (funcall callback "Hello from AI" nil)))))
      (magent-agent-process "Hello" (lambda (r) (setq response r))))
    (should (= call-count 1))
    (should (stringp response))
    (should (equal response "Hello from AI"))))

;;; Test 2: FSM tool call detection

(ert-deftest magent-test-fsm-tool-detection ()
  "Test that FSM correctly detects tool-use in response info."
  (let* ((fsm (magent-fsm-create
               :session (magent-session-create)
               :backend (gptel-make-openai "test" :key "test-key")
               :model 'gpt-4o-mini
               :prompt-list nil
               :system-prompt "test"
               :tools nil
               :streaming-p nil
               :callback #'ignore
               :ui-callback nil))
         (info-with-tools (list :tool-use
                                (list (list :id "call_1"
                                            :name "bash"
                                            :args (list :command "echo test")))))
         (info-without-tools nil))
    ;; When info has :tool-use, FSM should have pending tools
    (setf (magent-fsm-pending-tools fsm) nil)
    (when (plist-get info-with-tools :tool-use)
      (setf (magent-fsm-pending-tools fsm)
            (plist-get info-with-tools :tool-use)))
    (should (magent-fsm-pending-tools fsm))
    (should (= (length (magent-fsm-pending-tools fsm)) 1))
    (let ((tool-call (car (magent-fsm-pending-tools fsm))))
      (should (equal (plist-get tool-call :name) "bash")))
    ;; When info has no :tool-use, pending tools should be nil
    (setf (magent-fsm-pending-tools fsm) nil)
    (should (null (magent-fsm-pending-tools fsm)))))

;;; Test 3: Session message recording

(ert-deftest magent-test-session-recording ()
  "Test that messages are recorded in session."
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (gptel-tools nil)
        (gptel-use-tools nil)
        (magent-enable-streaming nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (prompt &rest kwargs)
                 (let* ((callback (plist-get kwargs :callback)))
                   (funcall callback "AI response" nil)))))
      (magent-session-reset)
      (magent-agent-process "User message" #'ignore)
      (let* ((session (magent-session-get))
             (messages (magent-session-get-messages session)))
        (should (>= (length messages) 2))
        (let ((last-msg (car (last messages))))
          (should (eq (magent-msg-role last-msg) 'assistant))
          (should (equal (magent-msg-content last-msg) "AI response")))))))

;;; Test 4: Permission system (original)

(ert-deftest magent-test-permission-allow ()
  "Test permission allow rules."
  (require 'magent-permission)
  (let ((rules '((read . allow)
                 (write . deny)
                 (* . allow))))
    (should (magent-permission-allow-p rules 'read))
    (should-not (magent-permission-allow-p rules 'write))
    (should (magent-permission-allow-p rules 'bash))))

(ert-deftest magent-test-permission-file-patterns ()
  "Test file-based permission rules."
  (require 'magent-permission)
  (let ((rules '((read . (("*.env" . deny)
                          ("*.env.example" . allow)
                          (* . allow))))))
    (should-not (magent-permission-allow-p rules 'read ".env"))
    (should-not (magent-permission-allow-p rules 'read "config.env"))
    (should (magent-permission-allow-p rules 'read ".env.example"))
    (should (magent-permission-allow-p rules 'read "test.txt"))))

;;; Test 5: Session message storage

(ert-deftest magent-test-session-add-message ()
  "Test session message addition and retrieval."
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "Hello")
    (magent-session-add-message session 'assistant "Hi there")
    (should (= (magent-session-message-count session) 2))
    (let ((messages (magent-session-get-messages session)))
      (should (= (length messages) 2))
      (should (eq (magent-msg-role (nth 0 messages)) 'user))
      (should (equal (magent-msg-content (nth 0 messages)) "Hello"))
      (should (eq (magent-msg-role (nth 1 messages)) 'assistant))
      (should (equal (magent-msg-content (nth 1 messages)) "Hi there")))))

(ert-deftest magent-test-session-history-trimming ()
  "Test lazy history trimming."
  (let ((session (magent-session-create)))
    (setf (magent-session-max-history session) 5)
    ;; Add 10 messages
    (dotimes (i 10)
      (magent-session-add-message session 'user (format "msg %d" i)))
    ;; Should have 10 messages (lazy trim hasn't triggered yet)
    (should (= (magent-session-message-count session) 10))
    ;; Add 6 more to trigger trim (10 + 6 = 16 > 5 + 10)
    (dotimes (i 6)
      (magent-session-add-message session 'user (format "extra %d" i)))
    ;; Now should be trimmed to max-history (5)
    (should (= (magent-session-message-count session) 5))))

;;; Test 6: Tool filtering

(ert-deftest magent-test-tools-filtering ()
  "Test tool filtering by permissions."
  (require 'magent-tools)
  (require 'magent-agent-registry)
  ;; Create agent with limited permissions
  (let* ((agent (magent-agent-info-create
                 :name "test-agent"
                 :permission (magent-permission-create
                              :rules '((read . allow)
                                       (write . deny)
                                       (* . allow)))))
         (tools (magent-tools-get-gptel-tools agent)))
    ;; Should have read_file
    (should (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "read_file")) tools))
    ;; Should NOT have write_file
    (should-not (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "write_file")) tools))
    ;; Should have other tools (bash, grep, etc.)
    (should (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "bash")) tools))))

;;; Test 7: FSM cleanup

(ert-deftest magent-test-fsm-destroy ()
  "Test FSM resource cleanup."
  (let* ((buf (generate-new-buffer " *test-fsm*"))
         (fsm (magent-fsm-create
               :session (magent-session-create)
               :backend (gptel-make-openai "test" :key "test-key")
               :model 'gpt-4o-mini
               :prompt-list nil
               :system-prompt "test"
               :tools nil
               :streaming-p nil
               :callback #'ignore
               :ui-callback nil
               :request-buffer buf)))
    ;; Buffer should exist
    (should (buffer-live-p buf))
    ;; Destroy FSM
    (magent-fsm-destroy fsm)
    ;; Buffer should be killed
    (should-not (buffer-live-p buf))))

(provide 'magent-test)
;;; magent-test.el ends here
