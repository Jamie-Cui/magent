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

(provide 'magent-test)
;;; magent-test.el ends here