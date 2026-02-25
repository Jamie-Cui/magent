;;; magent-session-test.el --- Tests for magent-session  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>

;;; Commentary:

;; Unit tests for magent-session.el

;;; Code:

(require 'ert)
(require 'magent-test-helper)
(require 'magent-session)

;;; Session creation tests

(ert-deftest magent-session-test-create ()
  "Test creating session structures."
  (let ((session (magent-session-create)))
    (should (magent-session-p session))
    (should (null (magent-session-messages session)))
    (should (null (magent-session-id session)))
    (should (null (magent-session-agent session)))))

(ert-deftest magent-session-test-create-with-options ()
  "Test creating session with custom options."
  (let ((session (magent-session-create
                  :max-history 50
                  :id "test-session"
                  :agent "build")))
    (should (= 50 (magent-session-max-history session)))
    (should (string= "test-session" (magent-session-id session)))
    (should (string= "build" (magent-session-agent session)))))

;;; Session get/reset tests

(ert-deftest magent-session-test-get ()
  "Test getting current session."
  (setq magent--current-session nil)
  (let ((session (magent-session-get)))
    (should (magent-session-p session))
    (should (eq session magent--current-session))))

(ert-deftest magent-session-test-reset ()
  "Test resetting session."
  (setq magent--current-session (magent-session-create))
  (magent-session-reset)
  (should (null magent--current-session)))

;;; Session ID tests

(ert-deftest magent-session-test-get-id ()
  "Test getting or generating session ID."
  (let ((session (magent-session-create)))
    (let ((id (magent-session-get-id session)))
      (should (stringp id))
      (should (string-prefix-p "session-" id))
      (should (eq id (magent-session-id session))))))

(ert-deftest magent-session-test-get-id-existing ()
  "Test getting existing session ID."
  (let ((session (magent-session-create :id "existing-id")))
    (should (string= "existing-id" (magent-session-get-id session)))))

;;; Agent tests

(ert-deftest magent-session-test-get-set-agent ()
  "Test getting and setting session agent."
  (let ((session (magent-session-create)))
    (should (null (magent-session-get-agent session)))
    (magent-session-set-agent session "explore")
    (should (string= "explore" (magent-session-get-agent session)))))

;;; Message management tests

(ert-deftest magent-session-test-add-message ()
  "Test adding messages to session."
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "Hello")
    (should (= 1 (length (magent-session-messages session))))
    (magent-session-add-message session 'assistant "Hi there")
    (should (= 2 (length (magent-session-messages session))))))

(ert-deftest magent-session-test-get-messages ()
  "Test getting messages in chronological order."
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "First")
    (magent-session-add-message session 'assistant "Second")
    (magent-session-add-message session 'user "Third")
    (let ((messages (magent-session-get-messages session)))
      (should (= 3 (length messages)))
      (should (string= "First" (magent-msg-content (nth 0 messages))))
      (should (string= "Second" (magent-msg-content (nth 1 messages))))
      (should (string= "Third" (magent-msg-content (nth 2 messages)))))))

(ert-deftest magent-session-test-add-structured-content ()
  "Test adding messages with structured content."
  (let ((session (magent-session-create))
        (content '((type . "text") (text . "Hello"))))
    (magent-session-add-message session 'user content)
    (let ((messages (magent-session-get-messages session)))
      (should (equal content (magent-msg-content (car messages)))))))

(ert-deftest magent-session-test-max-history ()
  "Test that messages are trimmed to max history."
  (let ((session (magent-session-create :max-history 3)))
    (magent-session-add-message session 'user "1")
    (magent-session-add-message session 'user "2")
    (magent-session-add-message session 'user "3")
    (magent-session-add-message session 'user "4")
    (should (= 3 (length (magent-session-messages session))))
    (let ((messages (magent-session-get-messages session)))
      (should (string= "2" (magent-msg-content (nth 0 messages))))
      (should (string= "3" (magent-msg-content (nth 1 messages))))
      (should (string= "4" (magent-msg-content (nth 2 messages)))))))

;;; Tool result tests

(ert-deftest magent-session-test-add-tool-result ()
  "Test adding tool results."
  (let ((session (magent-session-create)))
    (magent-session-add-tool-result session "tool-123" "Result content")
    (let ((messages (magent-session-get-messages session)))
      (should (= 1 (length messages)))
      (let* ((msg (car messages))
             (content (magent-msg-content msg)))
        (should (eq 'tool (magent-msg-role msg)))
        (should (string= "tool_result" (cdr (assq 'type content))))
        (should (string= "tool-123" (cdr (assq 'tool_use_id content))))
        (should (string= "Result content" (cdr (assq 'content content))))))))

;;; Context size tests

(ert-deftest magent-session-test-get-context-size ()
  "Test estimating context size."
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "Hello")
    (magent-session-add-message session 'assistant "Hi there!")
    (let ((size (magent-session-get-context-size session)))
      (should (> size 0))
      ;; Very rough estimate: "Hello" (5) + "Hi there!" (9) = 14 chars / 4 ~= 3 tokens
      (should (>= size 3)))))

;;; Session persistence tests

(ert-deftest magent-session-test-save-load ()
  "Test saving and loading sessions."
  (magent-test-with-temp-file temp-file ""
                              (let* ((session (magent-session-create :id "test-123"))
                                     (magent-max-history 100))
                                (magent-session-add-message session 'user "Test message")
                                (magent-session-add-message session 'assistant "Response")
                                (magent-session-save session temp-file)
                                (should (file-exists-p temp-file))
                                (let ((loaded (magent-session-load temp-file)))
                                  (should (magent-session-p loaded))
                                  (should (string= "test-123" (magent-session-id loaded)))
                                  (should (= 2 (length (magent-session-messages loaded))))))))

(ert-deftest magent-session-test-save-default-location ()
  "Test saving to default location."
  (let* ((session (magent-session-create))
         (magent-max-history 100)
         (session-id (magent-session-get-id session))
         (expected-file (expand-file-name (concat session-id ".json") magent-session-directory)))
    (unwind-protect
        (progn
          (magent-session-add-message session 'user "Test")
          (let ((saved-file (magent-session-save session)))
            (should (string= expected-file saved-file))
            (should (file-exists-p saved-file))))
      ;; Cleanup
      (when (file-exists-p expected-file)
        (delete-file expected-file)))))

;;; Session summarization tests

(ert-deftest magent-session-test-summarize ()
  "Test session summarization."
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "Question")
    (magent-session-add-message session 'assistant "Answer")
    (let ((summary (magent-session-summarize session)))
      (should (stringp summary))
      (should (string-match-p "Question" summary))
      (should (string-match-p "Answer" summary)))))

(ert-deftest magent-session-test-summarize-empty ()
  "Test summarizing empty session."
  (let ((session (magent-session-create)))
    (should (null (magent-session-summarize session)))))

(provide 'magent-session-test)
;;; magent-session-test.el ends here
