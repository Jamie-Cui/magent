;;; magent-test.el --- Tests for Magent agent processing  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for magent-agent-process and session handling.

;;; Code:

(require 'ert)
(require 'magent)
(require 'magent-fsm-backend-native)

;; ──────────────────────────────────────────────────────────────────────
;;; Integration tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-simple-prompt ()
  "Test basic prompt without tools returns response and records session."
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (gptel-tools nil)
        (gptel-use-tools nil)
        (call-count 0)
        (response nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (prompt &rest kwargs)
                 (cl-incf call-count)
                 ;; Simulate gptel streaming: string chunks then t with :content
                 (let* ((callback (plist-get kwargs :callback)))
                   (funcall callback "Hello from AI" nil)
                   (funcall callback t (list :content "Hello from AI")))))
              ((symbol-function 'magent-ui-start-streaming) #'ignore)
              ((symbol-function 'magent-ui-insert-streaming) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process "Hello" (lambda (r) (setq response r))))
    (should (= call-count 1))
    (should (stringp response))
    (should (equal response "Hello from AI"))))

(ert-deftest magent-test-session-recording ()
  "Test that user message is recorded in session by magent-agent-process."
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (gptel-tools nil)
        (gptel-use-tools nil)
        (response nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (prompt &rest kwargs)
                 (let* ((callback (plist-get kwargs :callback)))
                   (funcall callback "AI response" nil)
                   (funcall callback t (list :content "AI response")))))
              ((symbol-function 'magent-ui-start-streaming) #'ignore)
              ((symbol-function 'magent-ui-insert-streaming) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-session-reset)
      (magent-agent-process
       "User message"
       (lambda (r)
         (setq response r)
         ;; Record assistant response in session (as caller would)
         (magent-session-add-message (magent-session-get) 'assistant r)))
      (let* ((session (magent-session-get))
             (messages (magent-session-get-messages session)))
        (should (>= (length messages) 2))
        (should (equal response "AI response"))
        (let ((user-msg (nth 0 messages)))
          (should (eq (magent-msg-role user-msg) 'user))
          (should (equal (magent-msg-content user-msg) "User message")))
        (let ((assistant-msg (nth 1 messages)))
          (should (eq (magent-msg-role assistant-msg) 'assistant))
          (should (equal (magent-msg-content assistant-msg) "AI response")))))))

;; ──────────────────────────────────────────────────────────────────────
;;; Frontmatter parsing tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-frontmatter-basic ()
  "Test basic frontmatter key-value parsing."
  (require 'magent-frontmatter)
  (let* ((content "---\nname: my-agent\ndescription: A test agent\n---\nBody text here")
         (result (magent-frontmatter-parse content))
         (fm (car result))
         (body (cdr result)))
    (should (equal (plist-get fm :name) "my-agent"))
    (should (equal (plist-get fm :description) "A test agent"))
    (should (equal (string-trim body) "Body text here"))))

(ert-deftest magent-test-frontmatter-boolean-values ()
  "Test frontmatter boolean value parsing."
  (require 'magent-frontmatter)
  (let* ((content "---\nhidden: true\nnative: false\n---\n")
         (result (magent-frontmatter-parse content))
         (fm (car result)))
    (should (eq (plist-get fm :hidden) t))
    (should (eq (plist-get fm :native) nil))))

(ert-deftest magent-test-frontmatter-numeric-values ()
  "Test frontmatter numeric value parsing."
  (require 'magent-frontmatter)
  (let* ((content "---\ntemperature: 0.7\nsteps: 10\n---\n")
         (result (magent-frontmatter-parse content))
         (fm (car result)))
    (should (= (plist-get fm :temperature) 0.7))
    (should (= (plist-get fm :steps) 10))))

(ert-deftest magent-test-frontmatter-quoted-strings ()
  "Test frontmatter quoted string value parsing."
  (require 'magent-frontmatter)
  (let* ((content "---\nname: \"my agent\"\ncolor: 'blue'\n---\n")
         (result (magent-frontmatter-parse content))
         (fm (car result)))
    (should (equal (plist-get fm :name) "my agent"))
    (should (equal (plist-get fm :color) "blue"))))

(ert-deftest magent-test-frontmatter-comma-separated-list ()
  "Test frontmatter comma-separated list value parsing."
  (require 'magent-frontmatter)
  (let* ((content "---\ntools: bash, read, write\n---\n")
         (result (magent-frontmatter-parse content))
         (fm (car result)))
    (should (listp (plist-get fm :tools)))
    (should (= (length (plist-get fm :tools)) 3))
    (should (equal (plist-get fm :tools) '("bash" "read" "write")))))

(ert-deftest magent-test-frontmatter-no-frontmatter ()
  "Test content without frontmatter."
  (require 'magent-frontmatter)
  (let* ((content "Just regular content\nno frontmatter")
         (result (magent-frontmatter-parse content)))
    (should (null (car result)))
    (should (equal (cdr result) content))))

(ert-deftest magent-test-frontmatter-empty-body ()
  "Test frontmatter with empty body."
  (require 'magent-frontmatter)
  (let* ((content "---\nname: test\n---\n")
         (result (magent-frontmatter-parse content)))
    (should (equal (plist-get (car result) :name) "test"))
    (should (string-empty-p (string-trim (cdr result))))))

(ert-deftest magent-test-frontmatter-underscore-to-hyphen ()
  "Test that underscores in keys are converted to hyphens."
  (require 'magent-frontmatter)
  (let* ((content "---\ntop_p: 0.9\nmax_tokens: 100\n---\n")
         (result (magent-frontmatter-parse content))
         (fm (car result)))
    (should (= (plist-get fm :top-p) 0.9))
    (should (= (plist-get fm :max-tokens) 100))))

;; ──────────────────────────────────────────────────────────────────────
;;; Permission system tests
;; ──────────────────────────────────────────────────────────────────────

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

(ert-deftest magent-test-permission-resolve-with-struct ()
  "Test permission resolution using magent-permission struct."
  (require 'magent-permission)
  (let ((perm (magent-permission-create
               :rules '((read . allow)
                         (bash . deny)
                         (* . ask)))))
    (should (eq (magent-permission-resolve perm 'read) 'allow))
    (should (eq (magent-permission-resolve perm 'bash) 'deny))
    (should (eq (magent-permission-resolve perm 'grep) 'ask))))

(ert-deftest magent-test-permission-resolve-nil-rules ()
  "Test that nil rules default to allow."
  (require 'magent-permission)
  (should (eq (magent-permission-resolve nil 'read) 'allow)))

(ert-deftest magent-test-permission-resolve-single-symbol ()
  "Test that a single symbol rule applies to all tools."
  (require 'magent-permission)
  (should (eq (magent-permission-resolve 'deny 'read) 'deny))
  (should (eq (magent-permission-resolve 'allow 'bash) 'allow))
  (should (eq (magent-permission-resolve 'ask 'grep) 'ask)))

(ert-deftest magent-test-permission-ask-p ()
  "Test permission ask-p predicate."
  (require 'magent-permission)
  (let ((rules '((bash . ask)
                 (read . allow)
                 (* . deny))))
    (should (magent-permission-ask-p rules 'bash))
    (should-not (magent-permission-ask-p rules 'read))
    (should-not (magent-permission-ask-p rules 'grep))))

(ert-deftest magent-test-permission-deny-p ()
  "Test permission deny-p predicate."
  (require 'magent-permission)
  (let ((rules '((write . deny)
                 (read . allow)
                 (* . ask))))
    (should (magent-permission-deny-p rules 'write))
    (should-not (magent-permission-deny-p rules 'read))
    (should-not (magent-permission-deny-p rules 'bash))))

(ert-deftest magent-test-permission-tool-specific-over-wildcard ()
  "Test that tool-specific rules take priority over wildcard."
  (require 'magent-permission)
  (let ((rules '((read . deny)
                 (* . allow))))
    (should-not (magent-permission-allow-p rules 'read))
    (should (magent-permission-allow-p rules 'bash))
    (should (magent-permission-allow-p rules 'grep))))

(ert-deftest magent-test-permission-nested-file-rules-no-file ()
  "Test nested file rules when no file is specified."
  (require 'magent-permission)
  (let ((rules '((read . (("*.el" . allow)
                           (* . deny))))))
    ;; No file specified: should use wildcard default
    (should (eq (magent-permission-resolve rules 'read) 'deny))))

(ert-deftest magent-test-permission-file-exact-match ()
  "Test file-based permission with exact path match."
  (require 'magent-permission)
  (let ((rules '((read . (("secret.key" . deny)
                           (* . allow))))))
    (should-not (magent-permission-allow-p rules 'read "secret.key"))
    (should (magent-permission-allow-p rules 'read "other.txt"))))

(ert-deftest magent-test-permission-tool-available-p ()
  "Test tool-available-p includes 'ask tools."
  (require 'magent-permission)
  (let ((rules '((read . allow)
                 (bash . ask)
                 (write . deny)
                 (* . deny))))
    (should (magent-permission-tool-available-p rules 'read))
    (should (magent-permission-tool-available-p rules 'bash))
    (should-not (magent-permission-tool-available-p rules 'write))))

(ert-deftest magent-test-permission-tool-available-p-nested ()
  "Test tool-available-p with nested file rules containing some allow."
  (require 'magent-permission)
  (let ((rules '((edit . (("*.md" . allow)
                           (* . deny))))))
    ;; edit has nested rules where at least one grants access
    (should (magent-permission-tool-available-p rules 'edit))))

(ert-deftest magent-test-permission-merge-simple ()
  "Test merging two simple rulesets."
  (require 'magent-permission)
  (let ((base '((read . allow) (write . allow) (* . allow)))
        (override '((write . deny))))
    (let ((merged (magent-permission-merge base override)))
      (should (eq (magent-permission-resolve merged 'read) 'allow))
      (should (eq (magent-permission-resolve merged 'write) 'deny)))))

(ert-deftest magent-test-permission-merge-with-struct ()
  "Test merging a struct with an alist."
  (require 'magent-permission)
  (let ((base (magent-permission-create :rules '((* . allow))))
        (override '((bash . deny))))
    (let ((merged (magent-permission-merge base override)))
      (should (eq (magent-permission-resolve merged 'bash) 'deny))
      ;; Wildcard from base still applies
      (should (eq (magent-permission-resolve merged 'read) 'allow)))))

(ert-deftest magent-test-permission-merge-bare-symbol ()
  "Test merging a bare symbol as a ruleset."
  (require 'magent-permission)
  (let ((merged (magent-permission-merge 'deny '((read . allow)))))
    (should (eq (magent-permission-resolve merged 'read) 'allow))
    (should (eq (magent-permission-resolve merged 'bash) 'deny))))

(ert-deftest magent-test-permission-session-overrides ()
  "Test session-level permission overrides."
  (require 'magent-permission)
  (magent-permission-clear-session-overrides)
  ;; Initially no override
  (should (null (magent-permission-session-override 'bash)))
  ;; Set override
  (magent-permission-set-session-override 'bash 'allow)
  (should (eq (magent-permission-session-override 'bash) 'allow))
  ;; Clear overrides
  (magent-permission-clear-session-overrides)
  (should (null (magent-permission-session-override 'bash))))

(ert-deftest magent-test-permission-defaults ()
  "Test default permission ruleset."
  (require 'magent-permission)
  (let ((defaults (magent-permission-defaults)))
    ;; bash and emacs_eval should be ask
    (should (eq (magent-permission-resolve defaults 'bash) 'ask))
    (should (eq (magent-permission-resolve defaults 'emacs_eval) 'ask))
    ;; delegate should be allow
    (should (eq (magent-permission-resolve defaults 'delegate) 'allow))
    ;; read without file should be allow
    (should (eq (magent-permission-resolve defaults 'read) 'allow))
    ;; read .env should be deny
    (should (eq (magent-permission-resolve defaults 'read ".env") 'deny))
    ;; read .env.example should be allow
    (should (eq (magent-permission-resolve defaults 'read ".env.example") 'allow))
    ;; write without file should be ask
    (should (eq (magent-permission-resolve defaults 'write) 'ask))
    ;; write .env should be deny
    (should (eq (magent-permission-resolve defaults 'write ".env") 'deny))))

(ert-deftest magent-test-permission-from-config ()
  "Test converting config alist to permission struct."
  (require 'magent-permission)
  (let ((perm (magent-permission-from-config '((read . allow) (bash . deny)))))
    (should (magent-permission-p perm))
    (should (eq (magent-permission-resolve perm 'read) 'allow))
    (should (eq (magent-permission-resolve perm 'bash) 'deny))))

;; ──────────────────────────────────────────────────────────────────────
;;; Session tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-session-add-message ()
  "Test session message addition and retrieval."
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "Hello")
    (magent-session-add-message session 'assistant "Hi there")
    (should (= (length (magent-session-messages session)) 2))
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
    (should (= (length (magent-session-messages session)) 10))
    ;; Add 6 more to trigger trim (10 + 6 = 16 > 5 + 10)
    (dotimes (i 6)
      (magent-session-add-message session 'user (format "extra %d" i)))
    ;; Now should be trimmed to max-history (5)
    (should (= (length (magent-session-messages session)) 5))))

(ert-deftest magent-test-session-trimming-preserves-recent ()
  "Test that trimming keeps the most recent messages."
  (let ((session (magent-session-create)))
    (setf (magent-session-max-history session) 3)
    ;; Add enough to trigger trim (3 + 10 = 13, need > 13)
    (dotimes (i 14)
      (magent-session-add-message session 'user (format "msg-%d" i)))
    (should (= (length (magent-session-messages session)) 3))
    (let* ((messages (magent-session-get-messages session))
           (first-content (magent-msg-content (nth 0 messages))))
      ;; Oldest remaining should be msg-11 (14 - 3 = 11)
      (should (equal first-content "msg-11")))))

(ert-deftest magent-test-session-empty ()
  "Test freshly created session has no messages."
  (let ((session (magent-session-create)))
    (should (= (length (magent-session-messages session)) 0))
    (should (null (magent-session-get-messages session)))))

(ert-deftest magent-test-session-content-to-string ()
  "Test content coercion for different content types."
  (require 'magent-session)
  ;; String content returns unchanged
  (should (equal (magent-session--content-to-string "hello") "hello"))
  ;; Content block list concatenates text fields
  (let ((blocks (list (list (cons 'text "hello "))
                      (list (cons 'text "world")))))
    (should (equal (magent-session--content-to-string blocks) "hello world")))
  ;; Block without text field produces empty string
  (let ((blocks (list (list (cons 'type "image")))))
    (should (equal (magent-session--content-to-string blocks) ""))))

(ert-deftest magent-test-session-to-gptel-prompt-list ()
  "Test conversion from session messages to gptel prompt list."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "What is Emacs?")
    (magent-session-add-message session 'assistant "A text editor.")
    (magent-session-add-message session 'user "Tell me more.")
    (let ((prompt-list (magent-session-to-gptel-prompt-list session)))
      (should (= (length prompt-list) 3))
      (should (equal (car (nth 0 prompt-list)) 'prompt))
      (should (equal (cdr (nth 0 prompt-list)) "What is Emacs?"))
      (should (equal (car (nth 1 prompt-list)) 'response))
      (should (equal (cdr (nth 1 prompt-list)) "A text editor."))
      (should (equal (car (nth 2 prompt-list)) 'prompt))
      (should (equal (cdr (nth 2 prompt-list)) "Tell me more.")))))

(ert-deftest magent-test-session-to-gptel-prompt-list-skips-tool ()
  "Test that tool messages are skipped in gptel prompt list."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "Run ls")
    (magent-session-add-message session 'tool "file1.txt\nfile2.txt")
    (magent-session-add-message session 'assistant "Here are the files.")
    (let ((prompt-list (magent-session-to-gptel-prompt-list session)))
      ;; Tool message should be skipped
      (should (= (length prompt-list) 2))
      (should (equal (car (nth 0 prompt-list)) 'prompt))
      (should (equal (car (nth 1 prompt-list)) 'response)))))

(ert-deftest magent-test-session-get-id ()
  "Test session ID generation."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    ;; No ID initially
    (should (null (magent-session-id session)))
    ;; Get-id generates one
    (let ((id (magent-session-get-id session)))
      (should (stringp id))
      (should (string-prefix-p "session-" id))
      ;; Same ID on subsequent calls
      (should (equal id (magent-session-get-id session))))))

(ert-deftest magent-test-session-get-id-unique-within-same-second ()
  "Test session IDs remain unique when created in the same second."
  (require 'magent-session)
  (let ((magent-session--last-id-stem nil)
        (magent-session--last-id-seq 0))
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (&rest _args) "20260316-173000")))
      (let ((id1 (magent-session-get-id (magent-session-create)))
            (id2 (magent-session-get-id (magent-session-create))))
        (should (equal id1 "session-20260316-173000"))
        (should (equal id2 "session-20260316-173000-01"))
        (should-not (equal id1 id2))))))

(ert-deftest magent-test-session-agent-assignment ()
  "Test session agent get/set."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (should (null (magent-session-agent session)))
    (magent-session-set-agent session "build")
    (should (equal (magent-session-agent session) "build"))))

(ert-deftest magent-test-session-summarize ()
  "Test session summarize produces output."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    ;; Empty session
    (should (null (magent-session-summarize session)))
    ;; With messages
    (magent-session-add-message session 'user "Hello")
    (magent-session-add-message session 'assistant "Hi!")
    (let ((summary (magent-session-summarize session)))
      (should (stringp summary))
      (should (string-match-p "Session Summary:" summary))
      (should (string-match-p "\\[USER\\]" summary))
      (should (string-match-p "\\[ASSISTANT\\]" summary)))))

(ert-deftest magent-test-session-get-creates-singleton ()
  "Test magent-session-get creates a single session."
  (require 'magent-session)
  (magent-session-reset)
  (let ((s1 (magent-session-get))
        (s2 (magent-session-get)))
    (should (eq s1 s2))))

(ert-deftest magent-test-session-reset-clears ()
  "Test magent-session-reset clears the current session."
  (require 'magent-session)
  (magent-session-reset)
  (let ((s1 (magent-session-get)))
    (magent-session-add-message s1 'user "test")
    (magent-session-reset)
    (let ((s2 (magent-session-get)))
      (should-not (eq s1 s2))
      (should (= (length (magent-session-messages s2)) 0)))))

;; ──────────────────────────────────────────────────────────────────────
;;; Agent registry tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-agent-info-valid-p ()
  "Test agent info validation."
  (require 'magent-agent-registry)
  ;; Valid agent
  (let ((agent (magent-agent-info-create :name "test" :mode 'primary)))
    (should (magent-agent-info-valid-p agent)))
  ;; Missing name
  (let ((agent (magent-agent-info-create :mode 'primary)))
    (should-not (magent-agent-info-valid-p agent)))
  ;; Invalid mode
  (let ((agent (magent-agent-info-create :name "test" :mode 'invalid)))
    (should-not (magent-agent-info-valid-p agent))))

(ert-deftest magent-test-agent-info-valid-mode-p ()
  "Test agent mode validation."
  (require 'magent-agent-registry)
  (should (magent-agent-info-valid-mode-p 'primary))
  (should (magent-agent-info-valid-mode-p 'subagent))
  (should (magent-agent-info-valid-mode-p 'all))
  (should-not (magent-agent-info-valid-mode-p 'invalid))
  (should-not (magent-agent-info-valid-mode-p nil)))

(ert-deftest magent-test-agent-info-mode-p ()
  "Test agent mode matching."
  (require 'magent-agent-registry)
  (let ((primary-agent (magent-agent-info-create :name "a" :mode 'primary))
        (all-agent (magent-agent-info-create :name "b" :mode 'all)))
    ;; primary matches primary
    (should (magent-agent-info-mode-p primary-agent 'primary))
    ;; primary doesn't match subagent
    (should-not (magent-agent-info-mode-p primary-agent 'subagent))
    ;; all matches both
    (should (magent-agent-info-mode-p all-agent 'primary))
    (should (magent-agent-info-mode-p all-agent 'subagent))))

(ert-deftest magent-test-agent-registry-register-and-get ()
  "Test agent registration and retrieval."
  (require 'magent-agent-registry)
  (let ((magent-agent-registry--agents (make-hash-table :test 'equal))
        (magent-agent-registry--initialized t))
    (let ((agent (magent-agent-info-create :name "test-reg" :mode 'primary)))
      (magent-agent-registry-register agent)
      (let ((retrieved (magent-agent-registry-get "test-reg")))
        (should retrieved)
        (should (equal (magent-agent-info-name retrieved) "test-reg"))))))

(ert-deftest magent-test-agent-registry-register-invalid ()
  "Test that invalid agents are not registered."
  (require 'magent-agent-registry)
  (let ((magent-agent-registry--agents (make-hash-table :test 'equal))
        (magent-agent-registry--initialized t))
    ;; Agent without name should not register
    (let ((agent (magent-agent-info-create :mode 'primary)))
      (should (null (magent-agent-registry-register agent)))
      (should (= (hash-table-count magent-agent-registry--agents) 0)))))

(ert-deftest magent-test-agent-registry-replace ()
  "Test that registering with same name replaces the agent."
  (require 'magent-agent-registry)
  (let ((magent-agent-registry--agents (make-hash-table :test 'equal))
        (magent-agent-registry--initialized t))
    (let ((agent1 (magent-agent-info-create :name "test" :mode 'primary :description "first"))
          (agent2 (magent-agent-info-create :name "test" :mode 'subagent :description "second")))
      (magent-agent-registry-register agent1)
      (magent-agent-registry-register agent2)
      (should (= (hash-table-count magent-agent-registry--agents) 1))
      (should (equal (magent-agent-info-description
                      (magent-agent-registry-get "test"))
                     "second")))))

(ert-deftest magent-test-agent-registry-get-nonexistent ()
  "Test retrieval of non-existent agent returns nil."
  (require 'magent-agent-registry)
  (let ((magent-agent-registry--agents (make-hash-table :test 'equal))
        (magent-agent-registry--initialized t))
    (should (null (magent-agent-registry-get "nonexistent")))))

(ert-deftest magent-test-agent-registry-list-filters ()
  "Test agent listing with various filters."
  (require 'magent-agent-registry)
  (let ((magent-agent-registry--agents (make-hash-table :test 'equal))
        (magent-agent-registry--initialized t))
    (magent-agent-registry-register
     (magent-agent-info-create :name "a" :mode 'primary :native t))
    (magent-agent-registry-register
     (magent-agent-info-create :name "b" :mode 'subagent :native t))
    (magent-agent-registry-register
     (magent-agent-info-create :name "c" :mode 'primary :hidden t :native nil))
    (magent-agent-registry-register
     (magent-agent-info-create :name "d" :mode 'primary :native nil))
    ;; Default listing: non-hidden
    (let ((agents (magent-agent-registry-list)))
      (should (= (length agents) 3)))
    ;; Include hidden
    (let ((agents (magent-agent-registry-list t)))
      (should (= (length agents) 4)))
    ;; Filter by mode: primary only
    (let ((agents (magent-agent-registry-list nil 'primary)))
      (should (= (length agents) 2))
      (should (cl-every (lambda (a) (magent-agent-info-mode-p a 'primary)) agents)))
    ;; Native only
    (let ((agents (magent-agent-registry-list nil nil t)))
      (should (= (length agents) 2))
      (should (cl-every #'magent-agent-info-native agents)))))

(ert-deftest magent-test-agent-registry-list-names ()
  "Test agent name listing."
  (require 'magent-agent-registry)
  (let ((magent-agent-registry--agents (make-hash-table :test 'equal))
        (magent-agent-registry--initialized t))
    (magent-agent-registry-register
     (magent-agent-info-create :name "alpha" :mode 'primary :native t))
    (magent-agent-registry-register
     (magent-agent-info-create :name "beta" :mode 'subagent :native t))
    (let ((names (magent-agent-registry-list-names)))
      (should (= (length names) 2))
      (should (member "alpha" names))
      (should (member "beta" names)))))

(ert-deftest magent-test-agent-registry-set-default ()
  "Test setting default agent."
  (require 'magent-agent-registry)
  (let ((magent-agent-registry--agents (make-hash-table :test 'equal))
        (magent-agent-registry--initialized t)
        (magent-agent-registry--default-agent nil))
    (magent-agent-registry-register
     (magent-agent-info-create :name "test" :mode 'primary))
    ;; Set valid default
    (should (magent-agent-registry-set-default "test"))
    (should (equal magent-agent-registry--default-agent "test"))
    ;; Set non-existent agent returns nil
    (should (null (magent-agent-registry-set-default "nonexistent")))))

(ert-deftest magent-test-agent-registry-clear ()
  "Test clearing the registry."
  (require 'magent-agent-registry)
  (let ((magent-agent-registry--agents (make-hash-table :test 'equal))
        (magent-agent-registry--initialized t))
    (magent-agent-registry-register
     (magent-agent-info-create :name "test" :mode 'primary))
    (should (= (hash-table-count magent-agent-registry--agents) 1))
    (magent-agent-registry-clear)
    (should (= (hash-table-count magent-agent-registry--agents) 0))
    (should-not magent-agent-registry--initialized)))

(ert-deftest magent-test-agent-info-format-for-display ()
  "Test agent display formatting."
  (require 'magent-agent-registry)
  (let ((agent (magent-agent-info-create
                :name "build" :mode 'primary :description "Build agent")))
    (let ((display (magent-agent-info-format-for-display agent)))
      (should (string-match-p "build" display))
      (should (string-match-p "primary" display))
      (should (string-match-p "Build agent" display))))
  ;; Hidden agent
  (let ((agent (magent-agent-info-create
                :name "hidden" :mode 'primary :hidden t)))
    (let ((display (magent-agent-info-format-for-display agent)))
      (should (string-match-p "(hidden)" display)))))

(ert-deftest magent-test-agent-info-apply-gptel-overrides ()
  "Test applying agent-level gptel variable overrides."
  (require 'magent-agent-registry)
  (let ((gptel-backend (gptel-make-openai "default" :key "key"))
        (gptel-model 'default-model)
        (gptel-temperature 1.0))
    ;; Agent with temperature override
    (let ((agent (magent-agent-info-create
                  :name "t" :mode 'primary :temperature 0.3)))
      (magent-agent-info-apply-gptel-overrides
       agent
       (lambda ()
         (should (= gptel-temperature 0.3))
         ;; Backend and model unchanged
         (should (equal (gptel-backend-name gptel-backend) "default")))))
    ;; Agent with no overrides
    (let ((agent (magent-agent-info-create :name "t2" :mode 'primary)))
      (magent-agent-info-apply-gptel-overrides
       agent
       (lambda ()
         (should (= gptel-temperature 1.0)))))))

(ert-deftest magent-test-builtin-agents-count ()
  "Test that all 7 built-in agents are created."
  (require 'magent-agent-registry)
  (let ((agents (magent-agent-types-initialize)))
    (should (= (length agents) 7))
    (let ((names (mapcar #'magent-agent-info-name agents)))
      (should (member "build" names))
      (should (member "plan" names))
      (should (member "general" names))
      (should (member "explore" names))
      (should (member "compaction" names))
      (should (member "title" names))
      (should (member "summary" names)))))

(ert-deftest magent-test-builtin-agents-valid ()
  "Test that all built-in agents pass validation."
  (require 'magent-agent-registry)
  (dolist (agent (magent-agent-types-initialize))
    (should (magent-agent-info-valid-p agent))
    (should (magent-agent-info-native agent))))

;; ──────────────────────────────────────────────────────────────────────
;;; Agent file tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-agent-file-parse-mode ()
  "Test mode string parsing."
  (require 'magent-agent-file)
  (should (eq (magent-agent-file--parse-mode "primary") 'primary))
  (should (eq (magent-agent-file--parse-mode "subagent") 'subagent))
  (should (eq (magent-agent-file--parse-mode "all") 'all))
  (should (eq (magent-agent-file--parse-mode "PRIMARY") 'primary))
  (should (eq (magent-agent-file--parse-mode "unknown") 'all)))

(ert-deftest magent-test-agent-file-parse-permission ()
  "Test permission generation from tools config."
  (require 'magent-agent-file)
  (let ((rules (magent-agent-file--parse-permission '(:bash nil :read t))))
    ;; bash should be denied
    (should (eq (cdr (assq 'bash rules)) 'deny))
    ;; read should remain allowed
    (should (eq (cdr (assq 'read rules)) 'allow))))

(ert-deftest magent-test-agent-file-load-from-temp ()
  "Test loading an agent from a temporary file."
  (require 'magent-agent-file)
  (let* ((magent-agent-registry--agents (make-hash-table :test 'equal))
         (magent-agent-registry--initialized t)
         (tmpfile (make-temp-file "test-agent-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "---\ndescription: Test agent\nmode: subagent\n---\nYou are a test agent."))
          (let ((agent (magent-agent-file-load tmpfile)))
            (should agent)
            (should (equal (magent-agent-info-description agent) "Test agent"))
            (should (eq (magent-agent-info-mode agent) 'subagent))
            (should (string-match-p "You are a test agent"
                                    (magent-agent-info-prompt agent)))
            (should-not (magent-agent-info-native agent))))
      (delete-file tmpfile))))

(ert-deftest magent-test-agent-file-save-roundtrip ()
  "Test saving and reloading an agent preserves fields."
  (require 'magent-agent-file)
  (let* ((magent-agent-registry--agents (make-hash-table :test 'equal))
         (magent-agent-registry--initialized t)
         (tmpdir (make-temp-file "agent-dir-" t)))
    (unwind-protect
        (let* ((agent (magent-agent-info-create
                       :name "roundtrip"
                       :description "Roundtrip test"
                       :mode 'subagent
                       :temperature 0.5
                       :prompt "System prompt here."))
               (filepath (magent-agent-file-save agent tmpdir)))
          (should (file-exists-p filepath))
          ;; Reload
          (let ((loaded (magent-agent-file-load filepath)))
            (should loaded)
            (should (equal (magent-agent-info-description loaded) "Roundtrip test"))
            (should (eq (magent-agent-info-mode loaded) 'subagent))
            (should (= (magent-agent-info-temperature loaded) 0.5))
            (should (string-match-p "System prompt here"
                                    (magent-agent-info-prompt loaded)))))
      (delete-directory tmpdir t))))

;; ──────────────────────────────────────────────────────────────────────
;;; Skills tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-skills-register-and-get ()
  "Test skill registration and retrieval."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (let ((skill (magent-skill-create
                  :name "test-skill"
                  :description "Test"
                  :type 'instruction
                  :prompt "Do things.")))
      (magent-skills-register skill)
      (should (magent-skills-get "test-skill"))
      (should (equal (magent-skill-name (magent-skills-get "test-skill"))
                     "test-skill")))))

(ert-deftest magent-test-skills-register-replaces ()
  "Test that registering a skill with same name replaces it."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (magent-skills-register
     (magent-skill-create :name "s" :description "v1" :type 'instruction))
    (magent-skills-register
     (magent-skill-create :name "s" :description "v2" :type 'instruction))
    (should (= (length magent-skills--registry) 1))
    (should (equal (magent-skill-description (magent-skills-get "s")) "v2"))))

(ert-deftest magent-test-skills-unregister ()
  "Test skill unregistration."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (magent-skills-register
     (magent-skill-create :name "to-remove" :type 'instruction))
    (should (magent-skills-get "to-remove"))
    (magent-skills-unregister "to-remove")
    (should (null (magent-skills-get "to-remove")))))

(ert-deftest magent-test-skills-list ()
  "Test listing registered skill names."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (magent-skills-register
     (magent-skill-create :name "a" :type 'instruction))
    (magent-skills-register
     (magent-skill-create :name "b" :type 'tool))
    (let ((names (magent-skills-list)))
      (should (= (length names) 2))
      (should (member "a" names))
      (should (member "b" names)))))

(ert-deftest magent-test-skills-list-by-type ()
  "Test filtering skills by type."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (magent-skills-register
     (magent-skill-create :name "inst1" :type 'instruction))
    (magent-skills-register
     (magent-skill-create :name "inst2" :type 'instruction))
    (magent-skills-register
     (magent-skill-create :name "tool1" :type 'tool))
    (should (= (length (magent-skills-list-by-type 'instruction)) 2))
    (should (= (length (magent-skills-list-by-type 'tool)) 1))))

(ert-deftest magent-test-skills-clear ()
  "Test clearing all skills."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (magent-skills-register
     (magent-skill-create :name "x" :type 'instruction))
    (magent-skills-clear)
    (should (null magent-skills--registry))))

(ert-deftest magent-test-skills-invoke-not-found ()
  "Test invoking a non-existent skill returns error."
  (require 'magent-skills)
  (let ((magent-skills--registry nil)
        (result nil))
    (magent-skills-invoke "nonexistent" "op" nil
                          (lambda (r) (setq result r)))
    (should (string-match-p "not found" result))))

(ert-deftest magent-test-skills-invoke-instruction-type-error ()
  "Test that invoking instruction-type skill returns error."
  (require 'magent-skills)
  (let ((magent-skills--registry nil)
        (result nil))
    (magent-skills-register
     (magent-skill-create :name "inst" :type 'instruction :prompt "prompt"))
    (magent-skills-invoke "inst" "op" nil
                          (lambda (r) (setq result r)))
    (should (string-match-p "instruction-type" result))))

(ert-deftest magent-test-skills-invoke-tool-type ()
  "Test invoking a tool-type skill calls its invoke function."
  (require 'magent-skills)
  (let ((magent-skills--registry nil)
        (result nil)
        (invoked-args nil))
    (magent-skills-register
     (magent-skill-create
      :name "my-tool"
      :type 'tool
      :invoke-function (lambda (operation args callback)
                         (setq invoked-args (list operation args))
                         (funcall callback "tool-result"))))
    (magent-skills-invoke "my-tool" "do-thing" '("arg1")
                          (lambda (r) (setq result r)))
    (should (equal result "tool-result"))
    (should (equal (car invoked-args) "do-thing"))))

(ert-deftest magent-test-skills-get-instruction-prompts ()
  "Test collecting instruction-type skill prompts."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (magent-skills-register
     (magent-skill-create :name "s1" :type 'instruction :prompt "Prompt 1"))
    (magent-skills-register
     (magent-skill-create :name "s2" :type 'tool :prompt "Tool prompt"))
    (magent-skills-register
     (magent-skill-create :name "s3" :type 'instruction :prompt "Prompt 3"))
    ;; Get all instruction prompts
    (let ((prompts (magent-skills-get-instruction-prompts)))
      (should (= (length prompts) 2))
      (should (cl-every (lambda (p) (string-match-p "## Skill:" p)) prompts)))
    ;; Get specific skill prompt
    (let ((prompts (magent-skills-get-instruction-prompts '("s1"))))
      (should (= (length prompts) 1))
      (should (string-match-p "s1" (car prompts))))))

;; ──────────────────────────────────────────────────────────────────────
;;; Skill file parsing tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-skill-file-parse-type ()
  "Test skill type string parsing."
  (require 'magent-skill-file)
  (should (eq (magent-skill-file--parse-type "tool") 'tool))
  (should (eq (magent-skill-file--parse-type "instruction") 'instruction))
  (should (eq (magent-skill-file--parse-type "TOOL") 'tool))
  (should (eq (magent-skill-file--parse-type "unknown") 'instruction)))

(ert-deftest magent-test-skill-file-parse-tools ()
  "Test tool spec parsing."
  (require 'magent-skill-file)
  ;; Comma-separated string
  (should (equal (magent-skill-file--parse-tools "bash, read, write")
                 '(bash read write)))
  ;; Single string
  (should (equal (magent-skill-file--parse-tools "bash") '(bash)))
  ;; Symbol
  (should (equal (magent-skill-file--parse-tools 'bash) '(bash)))
  ;; List of strings
  (should (equal (magent-skill-file--parse-tools '("bash" "read")) '(bash read)))
  ;; nil
  (should (null (magent-skill-file--parse-tools nil))))

(ert-deftest magent-test-skill-file-load-from-temp ()
  "Test loading a skill from a temporary file."
  (require 'magent-skill-file)
  (let* ((magent-skills--registry nil)
         (tmpdir (make-temp-file "skill-" t))
         (skillfile (expand-file-name "SKILL.md" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file skillfile
            (insert "---\nname: test-skill\ndescription: A test\ntype: instruction\ntools: bash, read\n---\nDo the thing."))
          (let ((skill (magent-skill-file-load skillfile)))
            (should skill)
            (should (equal (magent-skill-name skill) "test-skill"))
            (should (equal (magent-skill-description skill) "A test"))
            (should (eq (magent-skill-type skill) 'instruction))
            (should (equal (magent-skill-tools skill) '(bash read)))
            (should (string-match-p "Do the thing" (magent-skill-prompt skill)))))
      (delete-directory tmpdir t))))

;; ──────────────────────────────────────────────────────────────────────
;;; Tools tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-tools-permission-key ()
  "Test tool name to permission key mapping."
  (require 'magent-tools)
  (should (eq (magent-tools-permission-key "read_file") 'read))
  (should (eq (magent-tools-permission-key "write_file") 'write))
  (should (eq (magent-tools-permission-key "edit_file") 'edit))
  (should (eq (magent-tools-permission-key "grep") 'grep))
  (should (eq (magent-tools-permission-key "glob") 'glob))
  (should (eq (magent-tools-permission-key "bash") 'bash))
  (should (eq (magent-tools-permission-key "emacs_eval") 'emacs_eval))
  (should (eq (magent-tools-permission-key "delegate") 'delegate))
  (should (eq (magent-tools-permission-key "skill_invoke") 'skill))
  (should (eq (magent-tools-permission-key "web_search") 'web_search))
  (should (null (magent-tools-permission-key "nonexistent"))))

(ert-deftest magent-test-tools-all-registered ()
  "Test that all 10 tools are registered."
  (require 'magent-tools)
  (should (= (length magent-tools--all-gptel-tools) 10)))

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

(ert-deftest magent-test-tools-filtering-deny-all ()
  "Test that deny-all permission removes all tools."
  (require 'magent-tools)
  (require 'magent-agent-registry)
  (let* ((agent (magent-agent-info-create
                 :name "no-tools"
                 :permission (magent-permission-create
                              :rules '((* . deny)))))
         (tools (magent-tools-get-gptel-tools agent)))
    (should (= (length tools) 0))))

(ert-deftest magent-test-tools-filtering-allow-all ()
  "Test that allow-all permission includes all globally enabled tools."
  (require 'magent-tools)
  (require 'magent-agent-registry)
  (let* ((agent (magent-agent-info-create
                 :name "all-tools"
                 :permission (magent-permission-create
                              :rules '((* . allow)))))
         (tools (magent-tools-get-gptel-tools agent)))
    (should (= (length tools) (length magent-tools--all-gptel-tools)))))

(ert-deftest magent-test-tools-filtering-no-permission ()
  "Test that agent without permission gets all tools."
  (require 'magent-tools)
  (require 'magent-agent-registry)
  (let* ((agent (magent-agent-info-create :name "no-perm" :mode 'primary))
         (tools (magent-tools-get-gptel-tools agent)))
    (should (= (length tools) (length magent-tools--all-gptel-tools)))))

(ert-deftest magent-test-tools-filtering-ask-included ()
  "Test that tools with 'ask permission are included in the list."
  (require 'magent-tools)
  (require 'magent-agent-registry)
  (let* ((agent (magent-agent-info-create
                 :name "ask-agent"
                 :permission (magent-permission-create
                              :rules '((bash . ask)
                                       (* . deny)))))
         (tools (magent-tools-get-gptel-tools agent)))
    (should (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "bash")) tools))))

(ert-deftest magent-test-tools-read-file ()
  "Test read_file tool implementation."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-test-"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "file contents here"))
          (magent-tools--read-file (lambda (r) (setq result r)) tmpfile)
          (should (equal result "file contents here")))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-read-file-relative-to-project-root ()
  "Test read_file resolves relative paths against the project root."
  (require 'magent-tools)
  (let* ((tmpdir (make-temp-file "magent-root-" t))
         (default-directory "/tmp/")
         (relative-path "notes.txt")
         (target (expand-file-name relative-path tmpdir))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file target
            (insert "root-relative"))
          (let ((magent-project-root-function (lambda () tmpdir)))
            (magent-tools--read-file (lambda (r) (setq result r)) relative-path))
          (should (equal result "root-relative")))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-tools-read-file-nonexistent ()
  "Test read_file with non-existent file returns error."
  (require 'magent-tools)
  (let ((result nil))
    (magent-tools--read-file (lambda (r) (setq result r)) "/tmp/magent-nonexistent-file-xyz")
    (should (string-match-p "Error" result))))

(ert-deftest magent-test-tools-write-file ()
  "Test write_file tool implementation."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-write-"))
         (result nil))
    (unwind-protect
        (progn
          (magent-tools--write-file (lambda (r) (setq result r))
                                   tmpfile "new content")
          (should (string-match-p "Successfully" result))
          (should (equal (with-temp-buffer
                           (insert-file-contents tmpfile)
                           (buffer-string))
                         "new content")))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-write-file-creates-dirs ()
  "Test write_file creates parent directories."
  (require 'magent-tools)
  (let* ((tmpdir (make-temp-file "magent-dir-" t))
         (filepath (expand-file-name "sub/dir/test.txt" tmpdir))
         (result nil))
    (unwind-protect
        (progn
          (magent-tools--write-file (lambda (r) (setq result r))
                                   filepath "nested content")
          (should (string-match-p "Successfully" result))
          (should (file-exists-p filepath)))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-tools-write-file-relative-to-project-root ()
  "Test write_file resolves relative output paths against the project root."
  (require 'magent-tools)
  (let* ((tmpdir (make-temp-file "magent-root-" t))
         (default-directory "/tmp/")
         (relative-path "nested/out.txt")
         (target (expand-file-name relative-path tmpdir))
         (result nil))
    (unwind-protect
        (progn
          (let ((magent-project-root-function (lambda () tmpdir)))
            (magent-tools--write-file (lambda (r) (setq result r))
                                      relative-path "root-write"))
          (should (string-match-p "Successfully" result))
          (should (equal (with-temp-buffer
                           (insert-file-contents target)
                           (buffer-string))
                         "root-write")))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-tools-edit-file ()
  "Test edit_file tool implementation."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-edit-"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "hello world"))
          (magent-tools--edit-file (lambda (r) (setq result r))
                                  tmpfile "hello" "goodbye")
          (should (string-match-p "Successfully" result))
          (should (equal (with-temp-buffer
                           (insert-file-contents tmpfile)
                           (buffer-string))
                         "goodbye world")))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-edit-file-not-found ()
  "Test edit_file when old_text is not found."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-edit-"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "hello world"))
          (magent-tools--edit-file (lambda (r) (setq result r))
                                  tmpfile "nonexistent" "replacement")
          (should (string-match-p "not found" result)))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-edit-file-multiple-matches ()
  "Test edit_file when old_text appears multiple times."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-edit-"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "hello hello hello"))
          (magent-tools--edit-file (lambda (r) (setq result r))
                                  tmpfile "hello" "bye")
          (should (string-match-p "found 3 times" result)))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-web-search-callback-cleans-up-buffer ()
  "Test web_search callback kills the temporary retrieval buffer."
  (require 'magent-tools)
  (let ((result nil)
        (buf (generate-new-buffer " *magent-web-test*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "HTTP/1.1 200 OK\n\n<html></html>")
          (cl-letf (((symbol-function 'libxml-parse-html-region) (lambda (&rest _args) 'dom))
                    ((symbol-function 'magent-tools--parse-ddg-results)
                     (lambda (_dom _max-results)
                       (list (list :title "Example" :url "https://example.com")))))
            (magent-tools--web-search-callback nil (lambda (r) (setq result r)) "test" 5))
          (should (string-match-p "Example" result))
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest magent-test-mode-line-lighter-does-not-require-queue-length ()
  "Test the mode-line lighter no longer depends on queue depth APIs."
  (require 'magent)
  (let ((magent--spinner (spinner-create 'progress-bar-filled)))
    (cl-letf (((symbol-function 'magent-queue-processing-p) (lambda () nil)))
      (should (string-match-p "\\[M/" (eval (cadr magent--lighter)))))))

(ert-deftest magent-test-tools-gptel-to-magent-tool ()
  "Test conversion from gptel-tool to magent tool plist."
  (require 'magent-tools)
  (let* ((gptel-tool (car magent-tools--all-gptel-tools))  ; read_file
         (magent-tool (magent-tools--gptel-to-magent-tool gptel-tool)))
    (should (plist-get magent-tool :name))
    (should (plist-get magent-tool :description))
    (should (plist-get magent-tool :function))
    (should (plist-get magent-tool :perm-key))))

(ert-deftest magent-test-tools-format-search-results ()
  "Test web search result formatting."
  (require 'magent-tools)
  (let ((results (list (list :title "Result 1" :url "https://example.com/1")
                       (list :title "Result 2" :url "https://example.com/2"))))
    (let ((formatted (magent-tools--format-search-results "test query" results)))
      (should (string-match-p "test query" formatted))
      (should (string-match-p "Result 1" formatted))
      (should (string-match-p "Result 2" formatted))
      (should (string-match-p "1\\." formatted))
      (should (string-match-p "2\\." formatted)))))

(ert-deftest magent-test-gptel-backend-installs-permission-confirmation ()
  "Test gptel backend keeps permission-aware confirmation active."
  (require 'magent-fsm-backend-gptel)
  (let ((captured-permission nil)
        (captured-confirm-mode nil)
        (permission '((bash . ask)))
        (gptel-backend (gptel-make-openai "test" :key "test-key")))
    (cl-letf (((symbol-function 'magent-fsm--convert-tools-to-gptel)
               (lambda (_tools arg-permission &optional _event-context)
                 (setq captured-permission arg-permission)
                 nil))
              ((symbol-function 'gptel-request)
               (lambda (&rest _args)
                 (setq captured-confirm-mode gptel-confirm-tool-calls)))
              ((symbol-function 'magent-ui-start-streaming) #'ignore))
      (magent-fsm-backend-gptel-start
       (list :prompt-list nil
             :system-prompt "test"
             :tools nil
             :permission permission
             :callback #'ignore
             :ui-callback nil
             :gptel-backend gptel-backend
             :model 'gpt-4o-mini)))
    (should (equal captured-permission permission))
    (should (eq captured-confirm-mode 'auto))))

(ert-deftest magent-test-gptel-backend-routes-tool-call-confirmation ()
  "Test gptel backend routes pending tool calls through magent permissions."
  (require 'magent-fsm-backend-gptel)
  (let ((captured nil)
        (tool-calls '((tool-spec arg-values callback)))
        (permission '((bash . ask)))
        (request-buffer (generate-new-buffer " *magent-gptel-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'magent-fsm--handle-tool-call-confirmation-with-permission)
                   (lambda (arg-permission arg-tool-calls)
                     (setq captured (list arg-permission arg-tool-calls)))))
          (magent-fsm-backend-gptel--callback
           (cons 'tool-call tool-calls) nil nil nil request-buffer permission nil))
      (when (buffer-live-p request-buffer)
        (kill-buffer request-buffer)))
    (should (equal captured (list permission tool-calls)))))

(ert-deftest magent-test-permission-prompt-choice-once-allow ()
  "Test tool confirmation accepts a one-time allow choice."
  (require 'magent-fsm-backend-native)
  (require 'magent-permission)
  (magent-permission-clear-session-overrides)
  (let ((result nil)
        (tool-ran nil)
        (tool (gptel-make-tool
               :name "bash"
               :args (list '(:name "command" :type string))
               :function (lambda (_command) "ok")
               :async nil)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args)))
              ((symbol-function 'magent-approval-request)
               (lambda (_request cb)
                 (funcall cb 'allow-once)))
              ((symbol-function 'magent-fsm--run-tool)
               (lambda (_tool-spec cb arg-values)
                 (setq tool-ran (car arg-values))
                 (funcall cb "ok"))))
      (magent-fsm--prompt-tool-calls-serially
       (list (list tool (list "echo hi") (lambda (r) (setq result r))))))
    (should (equal tool-ran "echo hi"))
    (should (equal result "ok"))
    (should (null (magent-permission-session-override 'bash)))))

(ert-deftest magent-test-permission-prompt-choice-once-deny ()
  "Test tool confirmation accepts a one-time deny choice."
  (require 'magent-fsm-backend-native)
  (require 'magent-permission)
  (magent-permission-clear-session-overrides)
  (let ((result nil)
        (tool-ran nil)
        (tool (gptel-make-tool
               :name "bash"
               :args (list '(:name "command" :type string))
               :function (lambda (_command)
                           (setq tool-ran t)
                           "ok")
               :async nil)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args)))
              ((symbol-function 'magent-approval-request)
               (lambda (_request cb)
                 (funcall cb 'deny-once))))
      (magent-fsm--prompt-tool-calls-serially
       (list (list tool (list "echo hi") (lambda (r) (setq result r))))))
    (should-not tool-ran)
    (should (string-match-p "denied by user" result))
    (should (null (magent-permission-session-override 'bash)))))

(ert-deftest magent-test-permission-prompt-choice-always-allow ()
  "Test tool confirmation persists an always-allow choice."
  (require 'magent-fsm-backend-native)
  (require 'magent-permission)
  (magent-permission-clear-session-overrides)
  (let ((result nil)
        (tool-ran nil)
        (tool (gptel-make-tool
               :name "bash"
               :args (list '(:name "command" :type string))
               :function (lambda (_command) "ok")
               :async nil)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args)))
              ((symbol-function 'magent-approval-request)
               (lambda (_request cb)
                 (funcall cb 'allow-session)))
              ((symbol-function 'magent-fsm--run-tool)
               (lambda (_tool-spec cb arg-values)
                 (setq tool-ran (car arg-values))
                 (funcall cb "ok"))))
      (magent-fsm--prompt-tool-calls-serially
       (list (list tool (list "echo hi") (lambda (r) (setq result r))))))
    (should (equal tool-ran "echo hi"))
    (should (equal result "ok"))
    (should (eq (magent-permission-session-override 'bash) 'allow))
    (magent-permission-clear-session-overrides)))

(ert-deftest magent-test-permission-prompt-choice-always-deny ()
  "Test tool confirmation persists an always-deny choice."
  (require 'magent-fsm-backend-native)
  (require 'magent-permission)
  (magent-permission-clear-session-overrides)
  (let ((result nil)
        (tool-ran nil)
        (tool (gptel-make-tool
               :name "bash"
               :args (list '(:name "command" :type string))
               :function (lambda (_command)
                           (setq tool-ran t)
                           "ok")
               :async nil)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args)))
              ((symbol-function 'magent-approval-request)
               (lambda (_request cb)
                 (funcall cb 'deny-session))))
      (magent-fsm--prompt-tool-calls-serially
       (list (list tool (list "echo hi") (lambda (r) (setq result r))))))
    (should-not tool-ran)
    (should (string-match-p "denied by user" result))
    (should (eq (magent-permission-session-override 'bash) 'deny))
    (magent-permission-clear-session-overrides)))

(ert-deftest magent-test-local-approval-drop-cancels-stale-prompt ()
  "Test dropping a queued local approval prevents any later prompt."
  (require 'magent-approval)
  (let ((magent-approval-provider-function #'magent-approval-local-request)
        (magent-approval--pending-requests (make-hash-table :test 'equal))
        (magent-approval--completed-requests (make-hash-table :test 'equal))
        (magent-approval--local-prompt-timers (make-hash-table :test 'equal))
        (magent-approval-state-change-functions '(magent-approval--local-state-changed))
        (scheduled nil)
        (cancelled nil)
        (prompted nil)
        (decision nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (setq scheduled (lambda () (apply fn args)))
                 'fake-timer))
              ((symbol-function 'cancel-timer)
               (lambda (timer)
                 (setq cancelled timer)))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _args)
                 (setq prompted t)
                 ?y)))
      (magent-approval-request
       '(:request-id "req-local" :tool-name "bash" :summary "echo hi")
       (lambda (value) (setq decision value)))
      (should (equal (gethash "req-local" magent-approval--local-prompt-timers)
                     'fake-timer))
      (should (= (magent-approval-drop-requests) 1))
      (funcall scheduled))
    (should (eq cancelled 'fake-timer))
    (should-not prompted)
    (should-not decision)
    (should-not (magent-approval-pending-request "req-local"))))

(ert-deftest magent-test-gptel-backend-streaming-tool-roundtrip-sequence ()
  "Test gptel backend handles streaming with a tool round-trip in order."
  (require 'magent-fsm-backend-gptel)
  (let ((events nil)
        (final-response nil)
        (request-callback nil)
        (gptel-backend (gptel-make-openai "test" :key "test-key")))
    (cl-letf (((symbol-function 'magent-fsm--convert-tools-to-gptel)
               (lambda (_tools _permission &optional _event-context) nil))
              ((symbol-function 'magent-ui-start-streaming)
               (lambda () (push 'start events)))
              ((symbol-function 'magent-ui-continue-streaming)
               (lambda () (push 'continue events)))
              ((symbol-function 'magent-ui-finish-streaming-fontify)
               (lambda () (push 'finish events)))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (setq request-callback (plist-get kwargs :callback))
                 (funcall request-callback "Hello " nil)
                 (funcall request-callback
                          (cons 'tool-call '((tool-spec args cb))) nil)
                 (funcall request-callback
                          (cons 'tool-result '((tool-spec args "ok"))) nil)
                 (funcall request-callback "world" nil)
                 (funcall request-callback t (list :content "Hello world"))))
              ((symbol-function 'magent-fsm--handle-tool-call-confirmation-with-permission)
               (lambda (_permission tool-calls)
                 (push (list 'tool-call tool-calls) events))))
      (magent-fsm-backend-gptel-start
       (list :prompt-list nil
             :system-prompt "test"
             :tools nil
             :permission '((bash . ask))
             :callback (lambda (r) (setq final-response r))
             :ui-callback (lambda (chunk) (push (list 'chunk chunk) events))
             :gptel-backend gptel-backend
             :model 'gpt-4o-mini)))
    (should (equal final-response "Hello world"))
    (should (equal (nreverse events)
                   (list 'start
                         (list 'chunk "Hello ")
                         (list 'tool-call '((tool-spec args cb)))
                         'continue
                         (list 'chunk "world")
                         'finish)))))

;; ──────────────────────────────────────────────────────────────────────
;;; FSM tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-fsm-tool-detection ()
  "Test that native FSM correctly detects tool-use in response info."
  (require 'magent-fsm-backend-native)
  (let* ((fsm (magent-fsm-native-create
               :session (magent-session-create)
               :backend (gptel-make-openai "test" :key "test-key")
               :model 'gpt-4o-mini
               :prompt-list nil
               :system-prompt "test"
               :tools nil
               :streaming-p t
               :callback #'ignore
               :ui-callback nil))
         (info-with-tools (list :tool-use
                                (list (list :id "call_1"
                                            :name "bash"
                                            :args (list :command "echo test"))))))
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

(ert-deftest magent-test-fsm-destroy ()
  "Test native FSM resource cleanup."
  (require 'magent-fsm-backend-native)
  (let* ((buf (generate-new-buffer " *test-fsm*"))
         (fsm (magent-fsm-native-create
               :session (magent-session-create)
               :backend (gptel-make-openai "test" :key "test-key")
               :model 'gpt-4o-mini
               :prompt-list nil
               :system-prompt "test"
               :tools nil
               :streaming-p t
               :callback #'ignore
               :ui-callback nil
               :request-buffer buf)))
    ;; Buffer should exist
    (should (buffer-live-p buf))
    ;; Destroy FSM (call native destroy directly since magent-fsm-destroy
    ;; always dispatches to gptel backend which is a no-op)
    (magent-fsm-native-destroy fsm)
    ;; Buffer should be killed
    (should-not (buffer-live-p buf))))

(ert-deftest magent-test-fsm-multiple-tool-calls ()
  "Test native FSM handling of multiple tool calls in one response."
  (require 'magent-fsm-backend-native)
  (let* ((fsm (magent-fsm-native-create
               :session (magent-session-create)
               :backend (gptel-make-openai "test" :key "test-key")
               :model 'gpt-4o-mini
               :prompt-list nil
               :system-prompt "test"
               :tools nil
               :streaming-p t
               :callback #'ignore
               :ui-callback nil))
         (tools (list (list :id "call_1" :name "bash" :args '(:command "ls"))
                      (list :id "call_2" :name "grep" :args '(:pattern "foo" :path "."))
                      (list :id "call_3" :name "read_file" :args '(:path "test.el")))))
    (setf (magent-fsm-pending-tools fsm) tools)
    (should (= (length (magent-fsm-pending-tools fsm)) 3))
    (should (equal (plist-get (nth 0 (magent-fsm-pending-tools fsm)) :name) "bash"))
    (should (equal (plist-get (nth 1 (magent-fsm-pending-tools fsm)) :name) "grep"))
    (should (equal (plist-get (nth 2 (magent-fsm-pending-tools fsm)) :name) "read_file"))))

(ert-deftest magent-test-fsm-create-initial-state ()
  "Test native FSM is created in INIT state."
  (require 'magent-fsm-backend-native)
  (let ((fsm (magent-fsm-native-create
              :session (magent-session-create)
              :backend (gptel-make-openai "test" :key "test-key")
              :model 'gpt-4o-mini
              :prompt-list nil
              :system-prompt "test"
              :tools nil
              :streaming-p t
              :callback #'ignore
              :ui-callback nil)))
    (should (eq (magent-fsm-state fsm) 'INIT))
    (should (null (magent-fsm-pending-tools fsm)))))

;; ──────────────────────────────────────────────────────────────────────
;;; UI/session regression tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-session-scope-from-directory-falls-back-to-global ()
  "Test session scope is global when no project root is detected."
  (let ((magent-project-root-function (lambda () nil)))
    (should (eq (magent-session-scope-from-directory "/tmp/") 'global))))

(ert-deftest magent-test-session-save-uses-project-storage-directory ()
  "Test project-scoped sessions save under a hashed project directory."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (project-root (make-temp-file "magent-project-" t)))
    (unwind-protect
        (progn
          (magent-session-activate (file-truename (directory-file-name project-root)))
          (magent-session-add-message (magent-session-get) 'user "hello")
          (magent-session-save)
          (let* ((storage-dir (expand-file-name
                               (concat "projects/" (secure-hash 'sha1
                                                                (file-truename
                                                                 (directory-file-name project-root))))
                               magent-session-directory))
                 (files (directory-files storage-dir nil "\\.json$")))
            (should (= (length files) 1))
            (with-temp-buffer
              (insert-file-contents (expand-file-name (car files) storage-dir))
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (data (json-read)))
                (should (equal (cdr (assq 'scope data)) "project"))
                (should (equal (cdr (assq 'project-root data))
                               (file-truename (directory-file-name project-root))))))))
      (delete-directory magent-session-directory t)
      (delete-directory project-root t))))

(ert-deftest magent-test-session-save-global-uses-legacy-directory ()
  "Test global sessions still save directly under `magent-session-directory'."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (magent-session-add-message (magent-session-get) 'user "hello")
          (magent-session-save)
          (should (= (length (directory-files magent-session-directory nil "\\.json$")) 1)))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-session-list-files-prefers-project-then-global ()
  "Test resume ordering groups current project first, then others, then global."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (project-root (file-truename (directory-file-name (make-temp-file "magent-project-" t))))
         (other-project (file-truename (directory-file-name (make-temp-file "magent-project-" t))))
         (project-dir (expand-file-name
                       (concat "projects/" (secure-hash 'sha1 project-root))
                       magent-session-directory))
         (other-project-dir (expand-file-name
                             (concat "projects/" (secure-hash 'sha1 other-project))
                             magent-session-directory))
         (global-file (expand-file-name "session-20260316-100000.json" magent-session-directory))
         (project-file (expand-file-name "session-20260317-100000.json" project-dir))
         (other-project-file (expand-file-name "session-20260315-100000.json" other-project-dir))
         (older-time (date-to-time "2026-03-16 10:00:00"))
         (newer-time (date-to-time "2026-03-17 10:00:00"))
         (oldest-time (date-to-time "2026-03-15 10:00:00")))
    (unwind-protect
        (progn
          (make-directory project-dir t)
          (make-directory other-project-dir t)
          (with-temp-file global-file
            (insert "{\"scope\":\"global\",\"summary-title\":\"Global chat\"}"))
          (with-temp-file project-file
            (insert (format
                     "{\"scope\":\"project\",\"project-root\":\"%s\",\"summary-title\":\"Project work item\"}"
                     project-root)))
          (with-temp-file other-project-file
            (insert (format
                     "{\"scope\":\"project\",\"project-root\":\"%s\",\"summary-title\":\"Other project item\"}"
                     other-project)))
          (set-file-times global-file older-time)
          (set-file-times project-file newer-time)
          (set-file-times other-project-file oldest-time)
          (setq magent-session--current-scope project-root)
          (should (equal (magent-session-list-files)
                         (list project-file other-project-file global-file)))
          (should (equal (magent-session--format-file project-file)
                         (format "2026-03-17 10:00:00  (%s)  Project work item"
                                 (abbreviate-file-name project-root))))
          (should (equal (magent-session--file-group project-file)
                         (format "Current Project: %s"
                                 (abbreviate-file-name project-root))))
          (should (equal (magent-session--file-group other-project-file)
                         (format "Project: %s"
                                 (abbreviate-file-name other-project))))
          (should (equal (magent-session--format-file global-file)
                         "2026-03-16 10:00:00  (global)  Global chat")))
      (delete-directory magent-session-directory t)
      (delete-directory project-root t)
      (delete-directory other-project t))))

(ert-deftest magent-test-session-format-file-derives-summary-title-from-messages ()
  "Test resume labels derive summary title from stored messages when needed."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (session-file (expand-file-name "session-20260317-120000.json"
                                         magent-session-directory)))
    (unwind-protect
        (progn
          (with-temp-file session-file
            (insert
             "{\"scope\":\"global\",\"messages\":[{\"role\":\"user\",\"content\":\"   Investigate resume menu title rendering regression   \"}]}"))
          (should (equal (magent-session--format-file session-file)
                         "2026-03-17 12:00:00  (global)  Investigate resume menu title rendering regre...")))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-ui-scope-switch-snapshots-outgoing-session ()
  "Test switching project scopes snapshots the outgoing UI buffer."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (project-a (make-temp-file "magent-project-a-" t))
         (project-b (make-temp-file "magent-project-b-" t))
         (buffer (magent-ui-get-buffer))
         (magent-project-root-function
          (lambda ()
            (cond
             ((string-prefix-p project-a default-directory) project-a)
             ((string-prefix-p project-b default-directory) project-b)
             (t nil)))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq default-directory (file-name-as-directory project-a))
            (magent-ui--activate-context-session))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "project-a snapshot")))
          (let ((session-a (magent-session-get)))
            (with-temp-buffer
              (setq default-directory (file-name-as-directory project-b))
              (magent-ui--activate-context-session))
            (should (equal (magent-session-buffer-content session-a)
                           "project-a snapshot"))
            (should (equal (magent-session-current-scope)
                           (file-truename (directory-file-name project-b))))
            (should-not (equal (with-current-buffer buffer (buffer-string))
                               "project-a snapshot"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest magent-test-ui-scope-switch-blocked-while-processing ()
  "Test scope switching is refused while a request is in flight."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (project-a (make-temp-file "magent-project-a-" t))
         (project-b (make-temp-file "magent-project-b-" t)))
    (unwind-protect
        (progn
          (magent-session-activate (file-truename (directory-file-name project-a)))
          (let ((magent-queue--processing t))
            (cl-letf (((symbol-function 'magent-ui--context-scope)
                       (lambda ()
                         (file-truename (directory-file-name project-b)))))
              (should-error (magent-ui--activate-context-session)
                            :type 'user-error)))
          (should (equal (magent-session-current-scope)
                         (file-truename (directory-file-name project-a)))))
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest magent-test-session-reset-clears-only-active-scope ()
  "Test resetting a session only clears the active scope."
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (project-root (file-truename (directory-file-name (make-temp-file "magent-project-" t)))))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (magent-session-add-message (magent-session-get) 'user "global")
          (magent-session-activate project-root)
          (magent-session-add-message (magent-session-get) 'user "project")
          (magent-session-reset)
          (should-not (gethash project-root magent-session--scoped-sessions))
          (should (gethash 'global magent-session--scoped-sessions))
          (magent-session-activate 'global)
          (should (equal (magent-msg-content
                          (car (magent-session-get-messages (magent-session-get))))
                         "global")))
      (delete-directory project-root t))))

(ert-deftest magent-test-resume-session-restores-loaded-buffer-content ()
  "Test resuming a session does not overwrite the loaded snapshot."
  (require 'magent-ui)
  (let* ((buffer (magent-ui-get-buffer))
         (loaded-session (magent-session-create
                          :id "loaded"
                          :buffer-content "* [ASSISTANT]\nLoaded session\n")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "stale buffer content")))
          (setq magent--current-session (magent-session-create
                                         :id "current"
                                         :buffer-content "stale snapshot"))
          (cl-letf (((symbol-function 'magent-session-list-files)
                     (lambda () '("/tmp/session.json")))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _args) "2026-03-16 10:00:00  (global)  Loaded session"))
                    ((symbol-function 'magent-session--format-file)
                     (lambda (_filepath) "2026-03-16 10:00:00  (global)  Loaded session"))
                    ((symbol-function 'magent-session-read-file)
                     (lambda (_filepath)
                       (list :scope 'global :session loaded-session :id "loaded")))
                    ((symbol-function 'magent-session-install)
                     (lambda (_scope session)
                       (setq magent--current-session session)
                       session))
                    ((symbol-function 'message) #'ignore))
            (magent-resume-session))
          (with-current-buffer buffer
            (should (string-prefix-p "* [ASSISTANT]\nLoaded session\n"
                                     (buffer-string)))
            (should-not (string-prefix-p "stale buffer content"
                                         (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-resume-session-snapshots-current-session-before-cross-scope-load ()
  "Test resuming another scope snapshots the outgoing buffer first."
  (require 'magent-ui)
  (let* ((buffer (magent-ui-get-buffer))
         (current-session (magent-session-create :id "current"))
         (loaded-session (magent-session-create :id "loaded"
                                                :buffer-content "* [ASSISTANT]\nLoaded\n"))
         (captured-snapshot nil))
    (unwind-protect
        (progn
          (setq magent--current-session current-session)
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "* [USER]\nUnsaved current content\n")))
          (cl-letf (((symbol-function 'magent-session-list-files)
                     (lambda () '("/tmp/session.json")))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _args) "2026-03-16 10:00:00  (global)  Loaded"))
                    ((symbol-function 'magent-session--format-file)
                     (lambda (_filepath) "2026-03-16 10:00:00  (global)  Loaded"))
                    ((symbol-function 'magent-session-read-file)
                     (lambda (_filepath)
                       (setq captured-snapshot
                             (magent-session-buffer-content current-session))
                       (list :scope 'global :session loaded-session :id "loaded")))
                    ((symbol-function 'magent-session-install)
                     (lambda (_scope session)
                       (setq magent--current-session session)
                       session))
                    ((symbol-function 'message) #'ignore))
            (magent-resume-session))
          (should (string-prefix-p "* [USER]\nUnsaved current content\n" captured-snapshot)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-agent-process-emits-turn-events ()
  "Test `magent-agent-process' emits turn lifecycle and text events."
  (require 'magent-events)
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (captured nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (let ((cb (plist-get kwargs :callback)))
                   (funcall cb "Hello" nil)
                   (funcall cb t (list :content "Hello")))))
              ((symbol-function 'magent-ui-start-streaming) #'ignore)
              ((symbol-function 'magent-ui-insert-streaming) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (unwind-protect
        (progn
            (magent-events-add-sink (lambda (event) (push event captured)))
            (magent-agent-process "Hello" #'ignore))
        (magent-events-clear-sinks)))
    (should (cl-find-if (lambda (event)
                          (eq (plist-get event :type) 'turn-start))
                        captured))
    (should (cl-find-if (lambda (event)
                          (eq (plist-get event :type) 'text-delta))
                        captured))
    (should (cl-find-if (lambda (event)
                          (and (eq (plist-get event :type) 'turn-end)
                               (eq (plist-get event :status) 'completed)))
                        captured))))

(ert-deftest magent-test-happy-envelope-mapping ()
  "Test Happy bridge envelope mapping for tool calls."
  (require 'magent-events)
  (require 'magent-happy)
  (let* ((context (magent-events-context-create :turn-id "turn123"))
         (event (list :type 'tool-call-start
                      :context context
                      :turn-id "turn123"
                      :call-id "call123"
                      :tool-name "bash"
                      :summary "echo hi"
                      :description "echo hi"
                      :args '(:command "echo hi")))
         (envelope (magent-happy--event->envelope event)))
    (should (equal (plist-get envelope :role) "agent"))
    (should (equal (plist-get envelope :turn) "turn123"))
    (should (equal (plist-get (plist-get envelope :ev) :t) "tool-call-start"))
    (should (equal (plist-get (plist-get envelope :ev) :call) "call123"))))

(ert-deftest magent-test-happy-helper-user-message-busy-emits-service ()
  "Test remote Happy messages respect Magent busy semantics."
  (require 'magent-events)
  (require 'magent-happy)
  (let ((captured nil)
        (magent-queue--processing t))
    (unwind-protect
        (progn
          (magent-events-add-sink (lambda (event) (push event captured)))
          (magent-happy--handle-user-message '(:text "hello")))
      (magent-events-clear-sinks))
    (should (eq (plist-get (car captured) :type) 'service))
    (should (string-match-p "busy" (plist-get (car captured) :text)))))

(ert-deftest magent-test-happy-helper-user-message-dispatches-through-unified-entry ()
  "Test remote Happy messages use the shared prompt dispatch path."
  (require 'magent-happy)
  (let (captured)
    (cl-letf (((symbol-function 'magent-ui-dispatch-prompt)
               (lambda (prompt &optional source display skills activate-context)
                 (setq captured (list prompt source display skills activate-context)))))
      (magent-happy--handle-user-message '(:text "hello from happy")))
    (should (equal captured
                   '("hello from happy" happy-remote "hello from happy" nil nil)))))

(ert-deftest magent-test-happy-approval-provider-roundtrip ()
  "Test Happy approval provider resolves pending callbacks from helper responses."
  (require 'magent-happy)
  (require 'magent-approval)
  (let ((magent-happy--session-id "session123")
        (magent-happy--connected t)
        (magent-approval--pending-requests (make-hash-table :test 'equal))
        (magent-approval--completed-requests (make-hash-table :test 'equal))
        (magent-approval-state-change-functions nil)
        (magent-approval-provider-function #'magent-happy--approval-provider)
        (sent nil)
        (decision nil))
    (cl-letf (((symbol-function 'magent-happy--send)
               (lambda (payload)
                 (push payload sent))))
      (add-hook 'magent-approval-state-change-functions
                #'magent-happy--approval-state-changed)
      (magent-approval-request
       '(:request-id "req1" :tool-name "bash" :perm-key bash :summary "echo hi")
       (lambda (value) (setq decision value)))
      (magent-happy--handle-approval-response
       '(:request_id "req1" :decision "allow-session")))
    (should (eq decision 'allow-session))
    (should-not (magent-approval-pending-request "req1"))
    (should (equal (plist-get (magent-approval-completed-request "req1") :decision)
                   'allow-session))
    (should (cl-find-if (lambda (payload)
                          (equal (plist-get payload :type) "agent-state"))
                        sent))))

(ert-deftest magent-test-happy-sentinel-resolves-pending-approvals ()
  "Test helper exit resolves pending approvals and restores local provider."
  (require 'magent-happy)
  (require 'magent-approval)
  (let ((magent-happy--process nil)
        (magent-happy--connected t)
        (magent-happy--session-id "session123")
        (magent-approval--pending-requests (make-hash-table :test 'equal))
        (magent-approval--completed-requests (make-hash-table :test 'equal))
        (magent-happy--previous-approval-provider #'ignore)
        (magent-approval-provider-function #'magent-happy--approval-provider)
        (decision nil))
    (magent-approval-request
     '(:request-id "req1" :tool-name "bash" :summary "echo hi")
     (lambda (value) (setq decision value)))
    (cl-letf (((symbol-function 'message) #'ignore))
      (magent-happy--process-sentinel nil "finished\n"))
    (should (eq decision 'deny-once))
    (should-not (magent-approval-pending-request "req1"))
    (should-not (magent-approval-completed-request "req1"))
    (should (eq magent-approval-provider-function #'ignore))
    (should-not magent-happy--connected)))

(provide 'magent-test)
;;; magent-test.el ends here
