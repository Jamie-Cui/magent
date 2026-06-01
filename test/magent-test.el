;;; magent-test.el --- Tests for Magent agent processing  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for magent-agent-process and session handling.

;;; Code:

(require 'ert)
(require 'magent)
(require 'gptel-openai)

(defun magent-test--read-audit-records (directory)
  "Return all JSONL audit records stored under DIRECTORY."
  (let (records)
    (dolist (file (directory-files directory t "\\.jsonl$"))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (line (split-string (buffer-string) "\n" t))
          (let ((json-object-type 'alist)
                (json-array-type 'list))
            (push (json-read-from-string line) records)))))
    (nreverse records)))

(defun magent-test--write-audit-record-file (directory filename records)
  "Write RECORDS as JSONL into DIRECTORY/FILENAME."
  (make-directory directory t)
  (with-temp-file (expand-file-name filename directory)
    (dolist (record records)
      (insert (json-encode record))
      (insert "\n"))))

(defconst magent-test--root-directory
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root used by reload-oriented tests.")

(defun magent-test--count-heading-lines (buffer label)
  "Return the number of top-level Magent headings matching LABEL in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((count 0)
            (regexp (concat "^\\* " (regexp-quote label) " +$")))
        (while (re-search-forward regexp nil t)
          (cl-incf count))
        count))))

(defun magent-test--count-overlays-with-face (buffer face)
  "Return the number of overlays using FACE in BUFFER."
  (with-current-buffer buffer
    (cl-count-if (lambda (ov)
                   (eq (overlay-get ov 'face) face))
                 (overlays-in (point-min) (point-max)))))

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
         (setq response r)))
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

(ert-deftest magent-test-agent-process-passes-max-tool-rounds-to-loop ()
  "Test that agent step limits are forwarded to the Magent loop."
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (captured nil)
        (agent (magent-agent-info-create
                :name "test"
                :mode 'primary
                :steps 7
                :permission (magent-permission-defaults))))
    (cl-letf (((symbol-function 'magent-agent-loop-start)
               (lambda (loop)
                 (setq captured loop)
                 loop))
              ((symbol-function 'magent-tool-registry-for-agent)
               (lambda (_agent) nil)))
      (magent-session-reset)
      (magent-agent-process "Hello" nil agent))
    (should (= (magent-agent-loop-max-tool-rounds captured) 7))))

;; ──────────────────────────────────────────────────────────────────────
;;; Frontmatter parsing tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-frontmatter-basic ()
  "Test basic frontmatter key-value parsing."
  (require 'magent-file-loader)
  (let* ((content "---\nname: my-agent\ndescription: A test agent\n---\nBody text here")
         (result (magent-file-loader-parse-frontmatter content))
         (fm (car result))
         (body (cdr result)))
    (should (equal (plist-get fm :name) "my-agent"))
    (should (equal (plist-get fm :description) "A test agent"))
    (should (equal (string-trim body) "Body text here"))))

(ert-deftest magent-test-file-loader-lists-direct-and-nested-definition-files ()
  "Test shared file loader finds direct and nested definition files."
  (require 'magent-file-loader)
  (let* ((tmpdir (make-temp-file "magent-file-loader-" t))
         (nested-dir (expand-file-name "nested" tmpdir))
         (direct-file (expand-file-name "SKILL.md" tmpdir))
         (nested-file (expand-file-name "SKILL.md" nested-dir)))
    (unwind-protect
        (progn
          (make-directory nested-dir t)
          (with-temp-file direct-file
            (insert "---\nname: direct\n---\n"))
          (with-temp-file nested-file
            (insert "---\nname: nested\n---\n"))
          (should (equal (magent-file-loader-list-named-files
                          (list tmpdir) "SKILL.md")
                         (list direct-file nested-file))))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-file-loader-skips-missing-definition-files ()
  "Test shared file loader skips missing direct and nested definition files."
  (require 'magent-file-loader)
  (let* ((tmpdir (make-temp-file "magent-file-loader-" t))
         (nested-dir (expand-file-name "nested" tmpdir))
         (nested-file (expand-file-name "SKILL.md" nested-dir)))
    (unwind-protect
        (progn
          (make-directory nested-dir t)
          (with-temp-file nested-file
            (insert "---\nname: nested\n---\n"))
          (should (equal (magent-file-loader-list-named-files
                          (list tmpdir) "SKILL.md")
                         (list nested-file))))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-file-loader-read-definition-without-frontmatter ()
  "Test shared file loader preserves body when no frontmatter exists."
  (require 'magent-file-loader)
  (let ((tmpfile (make-temp-file "magent-file-loader-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "Plain body only"))
          (let ((definition (magent-file-loader-read-definition tmpfile)))
            (should-not (plist-get definition :frontmatter))
            (should (equal (plist-get definition :body) "Plain body only"))))
      (delete-file tmpfile))))

(ert-deftest magent-test-file-loader-removes-file-backed-registry-entries ()
  "Test shared file loader strips only file-backed entries from a registry."
  (require 'magent-file-loader)
  (let* ((builtin '("builtin" . (:file-path nil)))
         (file-backed '("file-backed" . (:file-path "/tmp/skill.md")))
         (registry (list file-backed builtin)))
    (should (equal (magent-file-loader-remove-file-backed-entries
                    registry
                    (lambda (value) (plist-get value :file-path)))
                   (list builtin)))))

(ert-deftest magent-test-frontmatter-boolean-values ()
  "Test frontmatter boolean value parsing."
  (require 'magent-file-loader)
  (let* ((content "---\nhidden: true\nnative: false\n---\n")
         (result (magent-file-loader-parse-frontmatter content))
         (fm (car result)))
    (should (eq (plist-get fm :hidden) t))
    (should (eq (plist-get fm :native) nil))))

(ert-deftest magent-test-frontmatter-numeric-values ()
  "Test frontmatter numeric value parsing."
  (require 'magent-file-loader)
  (let* ((content "---\ntemperature: 0.7\nsteps: 10\n---\n")
         (result (magent-file-loader-parse-frontmatter content))
         (fm (car result)))
    (should (= (plist-get fm :temperature) 0.7))
    (should (= (plist-get fm :steps) 10))))

(ert-deftest magent-test-frontmatter-quoted-strings ()
  "Test frontmatter quoted string value parsing."
  (require 'magent-file-loader)
  (let* ((content "---\nname: \"my agent\"\ncolor: 'blue'\n---\n")
         (result (magent-file-loader-parse-frontmatter content))
         (fm (car result)))
    (should (equal (plist-get fm :name) "my agent"))
    (should (equal (plist-get fm :color) "blue"))))

(ert-deftest magent-test-frontmatter-comma-separated-list ()
  "Test frontmatter comma-separated list value parsing."
  (require 'magent-file-loader)
  (let* ((content "---\ntools: bash, read, write\n---\n")
         (result (magent-file-loader-parse-frontmatter content))
         (fm (car result)))
    (should (listp (plist-get fm :tools)))
    (should (= (length (plist-get fm :tools)) 3))
    (should (equal (plist-get fm :tools) '("bash" "read" "write")))))

(ert-deftest magent-test-frontmatter-multiline-yaml-list ()
  "Test multiline YAML frontmatter falls back to the YAML parser."
  (require 'magent-file-loader)
  (let* ((content "---\nname: test-agent\ntools:\n  - read\n  - write\nskills:\n  - skill-a\n  - skill-b\n---\nBody text")
         (result (magent-file-loader-parse-frontmatter content))
         (fm (car result)))
    (should (equal (plist-get fm :name) "test-agent"))
    (should (equal (plist-get fm :tools) '("read" "write")))
    (should (equal (plist-get fm :skills) '("skill-a" "skill-b")))
    (should (equal (string-trim (cdr result)) "Body text"))))

(ert-deftest magent-test-frontmatter-no-frontmatter ()
  "Test content without frontmatter."
  (require 'magent-file-loader)
  (let* ((content "Just regular content\nno frontmatter")
         (result (magent-file-loader-parse-frontmatter content)))
    (should (null (car result)))
    (should (equal (cdr result) content))))

(ert-deftest magent-test-frontmatter-empty-body ()
  "Test frontmatter with empty body."
  (require 'magent-file-loader)
  (let* ((content "---\nname: test\n---\n")
         (result (magent-file-loader-parse-frontmatter content)))
    (should (equal (plist-get (car result) :name) "test"))
    (should (string-empty-p (string-trim (cdr result))))))

(ert-deftest magent-test-frontmatter-underscore-to-hyphen ()
  "Test that underscores in keys are converted to hyphens."
  (require 'magent-file-loader)
  (let* ((content "---\ntop_p: 0.9\nmax_tokens: 100\n---\n")
         (result (magent-file-loader-parse-frontmatter content))
         (fm (car result)))
    (should (= (plist-get fm :top-p) 0.9))
    (should (= (plist-get fm :max-tokens) 100))))

(ert-deftest magent-test-md2org-convert-inline-restores-bold-markers ()
  "Test inline markdown conversion restores Org bold markers."
  (require 'magent-ui)
  (let ((converted (magent-md2org-convert-string "Plain **foo** and *bar* with `baz`.")))
    (should (equal converted "Plain *foo* and /bar/ with ~baz~."))
    (should-not (string-match-p (regexp-quote (string 1)) converted))
    (should-not (string-match-p (regexp-quote (string 2)) converted))))

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

(ert-deftest magent-test-permission-bypass-makes-tools-available ()
  "Test bypass config exposes tools even when permissions deny them."
  (require 'magent-permission)
  (let ((magent-by-pass-permission t)
        (rules '((bash . deny)
                 (* . deny))))
    (should (magent-permission-tool-available-p rules 'bash))
    (should (magent-permission-tool-available-p rules 'write))))

(ert-deftest magent-test-toggle-by-pass-permission-command ()
  "Test the interactive permission bypass toggle command."
  (require 'magent-permission)
  (let ((magent-by-pass-permission nil)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (should (eq (magent-toggle-by-pass-permission) t))
      (should magent-by-pass-permission)
      (should (equal (car messages) "Magent permission bypass enabled"))
      (should (eq (magent-toggle-by-pass-permission 0) nil))
      (should-not magent-by-pass-permission)
      (should (equal (car messages) "Magent permission bypass disabled")))))

(ert-deftest magent-test-toggle-by-pass-permission-command-clears-obsolete-alias-state ()
  "Test the toggle command clears bypass enabled through the obsolete alias."
  (require 'magent-permission)
  (let ((magent-by-pass-permission nil)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (setq magent-always-bypass-permission t)
      (should magent-by-pass-permission)
      (should (magent-permission-bypass-p))
      (should (eq (magent-toggle-by-pass-permission 0) nil))
      (should-not magent-by-pass-permission)
      (should-not (magent-permission-bypass-p))
      (should (equal (car messages) "Magent permission bypass disabled")))))

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
    ;; child-agent coordination should be allow
    (should (eq (magent-permission-resolve defaults 'agent) 'allow))
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

(ert-deftest magent-test-session-to-gptel-prompt-list-keeps-structured-tool ()
  "Test structured tool messages are included in gptel prompt list."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "Run ls")
    (magent-session-add-tool-message
     session "call_1" "bash" '(:command "ls") "file1.txt\nfile2.txt")
    (magent-session-add-message session 'assistant "Here are the files.")
    (let ((prompt-list (magent-session-to-gptel-prompt-list session)))
      (should (= (length prompt-list) 3))
      (should (equal (nth 0 prompt-list) '(prompt . "Run ls")))
      (should (equal (nth 1 prompt-list)
                     '(tool . (:id "call_1"
                               :name "bash"
                               :args (:command "ls")
                               :result "file1.txt\nfile2.txt"))))
      (should (equal (nth 2 prompt-list)
                     '(response . "Here are the files."))))))

(ert-deftest magent-test-session-to-gptel-prompt-list-json-sanitizes-tool-args ()
  "Test tool data reused in gptel prompts is safe for `json-serialize'."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "Run tool")
    (magent-session-add-tool-message
     session "call_1" 'emacs_eval
     '(:sexp "(+ 20 22)" :tool emacs_eval :values [emacs_eval nil])
     "42")
    (let* ((prompt-list (magent-session-to-gptel-prompt-list session))
           (tool (cdr (nth 1 prompt-list)))
           (args (plist-get tool :args)))
      (should (equal (plist-get tool :name) "emacs_eval"))
      (should (equal args
                     '(:sexp "(+ 20 22)"
                       :tool "emacs_eval"
                       :values ["emacs_eval" :null])))
      (should
       (if (fboundp 'json-serialize)
           (json-serialize (list :name (plist-get tool :name)
                                 :args args)
                           :null-object :null
                           :false-object :json-false)
         (let ((json-null :null)
               (json-false :json-false))
           (json-encode (list :name (plist-get tool :name)
                              :args args))))))))

(ert-deftest magent-test-session-to-gptel-prompt-list-skips-legacy-tool-string ()
  "Test old string-only tool messages are skipped in gptel prompt list."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "Run ls")
    (magent-session-add-message session 'tool "file1.txt\nfile2.txt")
    (magent-session-add-message session 'assistant "Here are the files.")
    (should (equal (magent-session-to-gptel-prompt-list session)
                   '((prompt . "Run ls")
                     (response . "Here are the files."))))))

(ert-deftest magent-test-session-to-gptel-prompt-list-drops-failed-turns ()
  "Test failed turns do not leak into later prompt reuse."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "emacs 有几个 buffer")
    (magent-session-add-message session 'assistant "")
    (magent-session-add-message session 'user "magent 有几个 skills")
    (magent-session-add-message
     session 'assistant
     "Error: tool_use_limit_reached. emacs_eval exceeded 3 calls in this turn.")
    (magent-session-add-message session 'user "emacs 有几个 实例")
    (should (equal (magent-session-to-gptel-prompt-list session)
                   '((prompt . "emacs 有几个 实例"))))))

(ert-deftest magent-test-session-to-gptel-prompt-list-keeps-completed-turns ()
  "Test completed turns remain even when a later turn failed."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "What is Emacs?")
    (magent-session-add-message session 'assistant "A text editor.")
    (magent-session-add-message session 'user "magent 有几个 skills")
    (magent-session-add-message
     session 'assistant
     "Error: tool_use_limit_reached. emacs_eval exceeded 3 calls in this turn.")
    (magent-session-add-message session 'user "Tell me more.")
    (should (equal (magent-session-to-gptel-prompt-list session)
                   '((prompt . "What is Emacs?")
                     (response . "A text editor.")
                     (prompt . "Tell me more."))))))

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

(ert-deftest magent-test-session-reset-clears-capability-local-overrides ()
  "Test session reset clears local capability toggles."
  (require 'magent-capability)
  (let ((magent-capability--local-disabled-capabilities '("org-structure"))
        (magent-capability--local-enabled-capabilities '("magit-workflow")))
    (magent-session-reset)
    (should-not magent-capability--local-disabled-capabilities)
    (should-not magent-capability--local-enabled-capabilities)))

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

(ert-deftest magent-test-agent-legacy-features-remain-requireable ()
  "Test old agent feature names load through explicit shim files."
  (require 'magent-agent-info)
  (require 'magent-agent-types)
  (should (featurep 'magent-agent-info))
  (should (featurep 'magent-agent-types))
  (should (featurep 'magent-agent-registry)))

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

(ert-deftest magent-test-agent-process-records-runtime-inheritance ()
  "Test request context records inherited runtime sampling settings."
  (require 'magent-agent)
  (let* ((backend (gptel-make-openai "inherited" :key "key"))
         (gptel-backend backend)
         (gptel-model 'parent-model)
         (gptel-temperature 0.42)
         (agent (magent-agent-info-create
                 :name "build"
                 :mode 'primary
                 :top-p 0.88
                 :permission (magent-permission-from-config
                              '((agent . ask)
                                (bash . deny)
                                (* . allow)))))
         (session (magent-session-create :id "parent"))
         (request-state (magent-request-context-create
                         :id "req"
                         :scope "/tmp/project"
                         :session session))
         (capability-resolution
          (magent-capability-resolution-create
           :prompt "inspect"
           :context '(:project-root "/tmp/project")
           :skill-names '("cap-skill")))
         captured-loop)
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-agent-loop-start)
               (lambda (loop)
                 (setq captured-loop loop)
                 'started))
              ((symbol-function 'magent-tool-registry-for-agent)
               (lambda (_agent) nil))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-events-emit) #'ignore)
              ((symbol-function 'magent-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-events-end-turn) #'ignore)
              ((symbol-function 'magent-skills-get-instruction-prompts)
               (lambda (_skills) nil)))
      (magent-agent-process
       "inspect"
       nil
       agent
       nil
       nil
       '(:project-root "/tmp/project")
       capability-resolution
       nil
       nil
       request-state))
    (let* ((request (magent-agent-loop-request captured-loop))
           (metadata (magent-llm-request-metadata request)))
      (should (eq (magent-request-context-model request-state) 'parent-model))
      (should (eq (magent-request-context-backend request-state) backend))
      (should (= (magent-request-context-temperature request-state) 0.42))
      (should (= (magent-request-context-top-p request-state) 0.88))
      (should (equal (magent-request-context-project-root request-state)
                     "/tmp/project"))
      (should (equal (magent-request-context-skill-names request-state)
                     '("cap-skill")))
      (should (equal (plist-get metadata :temperature) 0.42))
      (should (equal (plist-get metadata :top-p) 0.88))
      (should (equal (magent-permission-resolve
                      (magent-request-context-permission-profile request-state)
                      'agent)
                     'ask)))))

(ert-deftest magent-test-llm-gptel-applies-temperature-metadata ()
  "Test the gptel adapter applies request temperature metadata."
  (require 'magent-llm-gptel)
  (let ((gptel-backend (gptel-make-openai "test" :key "key"))
        (gptel-model 'test-model)
        (gptel-temperature 1.0)
        captured-temperature)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (setq captured-temperature gptel-temperature)
                 (funcall (plist-get kwargs :callback)
                          t
                          (list :content "ok")))))
      (magent-llm-gptel-sample
       (magent-llm-request-create
        :prompt '("hello")
        :system "sys"
        :stream t
        :metadata '(:temperature 0.25)
        :callback #'ignore)))
    (should (= captured-temperature 0.25))))

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

(ert-deftest magent-test-skills-register-builtin ()
  "Test builtin skill registration excludes the removed emacs tool skill."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (cl-letf (((symbol-function 'magent-log) #'ignore))
      (magent-skills--register-builtin))
    (should (null (magent-skills-get "emacs")))
    (let ((skill (magent-skills-get "skill-creator")))
      (should skill)
      (should (eq (magent-skill-type skill) 'instruction)))))

(ert-deftest magent-test-skill-reload-restores-builtin-skill ()
  "Test skill reload restores code-defined built-ins even from an empty registry."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (cl-letf (((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-skills-load-all)
               (lambda (&optional _directories) 0)))
      (magent-skills-reload))
    (let ((skill (magent-skills-get "skill-creator")))
      (should skill)
      (should (eq (magent-skill-type skill) 'instruction)))))

(ert-deftest magent-test-skills-reload-restores-active-project-skill ()
  "Test skill reload restores the active project's local skills."
  (require 'magent-skills)
  (let* ((magent-skills--registry nil)
         (magent-skill-directories nil)
         (project-root (file-truename
                        (directory-file-name
                         (make-temp-file "magent-project-" t))))
         (skill-dir (expand-file-name ".magent/skills/project-skill" project-root))
         (skill-file (expand-file-name "SKILL.md" skill-dir))
         (magent-runtime--active-project-scope project-root))
    (unwind-protect
        (progn
          (make-directory skill-dir t)
          (with-temp-file skill-file
            (insert "---\n"
                    "name: project-skill\n"
                    "description: First description\n"
                    "type: instruction\n"
                    "---\n"
                    "Use for project tasks.\n"))
          (cl-letf (((symbol-function 'magent-log) #'ignore))
            (magent-skills-load-project-scope project-root))
          (should (equal (magent-skill-description
                          (magent-skills-get "project-skill"))
                         "First description"))
          (with-temp-file skill-file
            (insert "---\n"
                    "name: project-skill\n"
                    "description: Updated description\n"
                    "type: instruction\n"
                    "---\n"
                    "Use for project tasks.\n"))
          (cl-letf (((symbol-function 'magent-log) #'ignore))
            (magent-skills-reload))
          (should (equal (magent-skill-description
                          (magent-skills-get "project-skill"))
                         "Updated description")))
      (delete-directory project-root t))))

(ert-deftest magent-test-reload-skills-prepares-project-context ()
  "Test interactive skill reload restores project-local skills on first use."
  (require 'magent-skills)
  (let* ((project-root (file-truename
                        (directory-file-name
                         (make-temp-file "magent-project-" t))))
         (skill-dir (expand-file-name ".magent/skills/project-skill" project-root))
         (skill-file (expand-file-name "SKILL.md" skill-dir))
         (magent--initialized nil)
         (magent-runtime--active-project-scope nil)
         (magent--current-session nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent-agent-registry--agents (make-hash-table :test #'equal))
         (magent-agent-registry--initialized nil)
         (magent-skills--registry nil)
         (magent-capability--registry nil))
    (unwind-protect
        (progn
          (make-directory skill-dir t)
          (with-temp-file skill-file
            (insert "---\n"
                    "name: project-skill\n"
                    "description: Project-only helper\n"
                    "type: instruction\n"
                    "---\n"
                    "Use for project tasks.\n"))
          (with-temp-buffer
            (setq default-directory project-root)
            (cl-letf (((symbol-function 'magent-project-root)
                       (lambda (&optional directory no-fallback)
                         (ignore directory no-fallback)
                         project-root))
                      ((symbol-function 'magent-log) #'ignore))
              (magent-reload-skills)))
          (should (equal (magent-runtime-active-project-scope) project-root))
          (should (equal (magent-skill-description
                          (magent-skills-get "project-skill"))
                         "Project-only helper")))
      (delete-directory project-root t))))

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
;;; Capability tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-capabilities-register-and-get ()
  "Test capability registration and retrieval."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (let ((capability (magent-capability-create
                       :name "runtime"
                       :description "Inspect runtime"
                       :skills '("emacs-runtime-inspection"))))
      (magent-capability-register capability)
      (should (magent-capability-get "runtime"))
      (should (equal (magent-capability-name
                      (magent-capability-get "runtime"))
                     "runtime")))))

(ert-deftest magent-test-capability-resolve-activates-matching-skill ()
  "Test context-aware capability resolution."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-register
     (magent-capability-create
      :name "org-structure"
      :description "Org structure edits"
      :skills '("org-structure-workflow")
      :modes '(org-mode)
      :features '(org)
      :prompt-keywords '("heading")
      :disclosure 'active))
    (let* ((resolution (magent-capability-resolve
                        "Please reorganize this heading"
                        '(:major-mode org-mode :features (org))
                        nil))
           (active (magent-capability-resolution-active-capabilities resolution)))
      (should (= (length active) 1))
      (should (equal (magent-capability-resolution-skill-names resolution)
                     '("org-structure-workflow")))
      (should (equal (magent-capability-name
                      (magent-capability-match-capability (car active)))
                     "org-structure")))))

(ert-deftest magent-test-capability-parse-context-prompt-derives-fields ()
  "Test prompt context parsing derives normalized resolver fields."
  (require 'magent-capability)
  (let ((context (magent-capability--parse-context-prompt
                  "[Context: buffer=\"notes.org\" file=\"/tmp/notes.org\" mode=org-mode modified=true region=1-4]")))
    (should (equal (plist-get context :buffer-name) "notes.org"))
    (should (equal (plist-get context :file-path) "/tmp/notes.org"))
    (should (equal (plist-get context :file-extension) "org"))
    (should (eq (plist-get context :major-mode) 'org-mode))
    (should (memq 'org-mode (plist-get context :major-mode-family)))
    (should (eq (plist-get context :buffer-modified-p) t))
    (should (eq (plist-get context :region-active) t))))

(ert-deftest magent-test-capability-resolve-tie-breaks-by-name ()
  "Test equal-score active capabilities are sorted by name."
  (require 'magent-capability)
  (let ((magent-capability--registry nil)
        (magent-capability-max-active 3))
    (dolist (name '("zeta" "alpha"))
      (magent-capability-register
       (magent-capability-create
        :name name
        :skills (list (concat name "-skill"))
        :modes '(org-mode)
        :features '(org)
        :disclosure 'active)))
    (let* ((resolution (magent-capability-resolve
                        "Refile this subtree"
                        '(:major-mode org-mode
                          :major-mode-family (org-mode text-mode)
                          :features (org))
                        nil))
           (active (magent-capability-resolution-active-capabilities resolution)))
      (should (equal (mapcar (lambda (match)
                               (magent-capability-name
                                (magent-capability-match-capability match)))
                             active)
                     '("alpha" "zeta"))))))

(ert-deftest magent-test-capability-resolve-respects-disabled-capabilities ()
  "Test disabled capabilities stay hidden even when they match."
  (require 'magent-capability)
  (let ((magent-capability--registry nil)
        (magent-disabled-capabilities '("org-structure")))
    (magent-capability-register
     (magent-capability-create
      :name "org-structure"
      :skills '("org-structure-workflow")
      :modes '(org-mode)
      :features '(org)
      :prompt-keywords '("heading")
      :disclosure 'active))
    (let* ((resolution (magent-capability-resolve
                        "Please reorganize this heading"
                        '(:major-mode org-mode
                          :major-mode-family (org-mode text-mode)
                          :features (org))
                        nil))
           (match (car (magent-capability-resolution-matches resolution))))
      (should (eq (magent-capability-match-status match) 'hidden))
      (should-not (magent-capability-resolution-active-capabilities resolution))
      (should-not (plist-get (magent-capability-match-details match) :enabled)))))

(ert-deftest magent-test-capability-resolve-respects-disabled-family ()
  "Test disabled capability families suppress auto-activation."
  (require 'magent-capability)
  (let ((magent-capability--registry nil)
        (magent-disabled-capability-families '("org")))
    (magent-capability-register
     (magent-capability-create
      :name "org-structure"
      :family "org"
      :skills '("org-structure-workflow")
      :modes '(org-mode)
      :features '(org)
      :disclosure 'active))
    (let* ((resolution (magent-capability-resolve
                        "Refactor this subtree"
                        '(:major-mode org-mode
                          :major-mode-family (org-mode outline-mode text-mode)
                          :features (org))
                        nil))
           (match (car (magent-capability-resolution-matches resolution))))
      (should (eq (magent-capability-match-status match) 'hidden))
      (should-not (plist-get (magent-capability-match-details match) :enabled)))))

(ert-deftest magent-test-capability-toggle-locally-overrides-disabled-state ()
  "Test local capability toggles override disabled capability settings."
  (require 'magent-capability)
  (let ((magent-capability--registry nil)
        (magent-disabled-capabilities '("org-structure"))
        (magent-capability--local-disabled-capabilities nil)
        (magent-capability--local-enabled-capabilities nil))
    (magent-capability-register
     (magent-capability-create
      :name "org-structure"
      :family "org"))
    (should-not (magent-capability-enabled-p
                 (magent-capability-get "org-structure")))
    (should (eq (magent-capability-toggle-locally "org-structure") 'enabled))
    (should (magent-capability-enabled-p
             (magent-capability-get "org-structure")))
    (should (eq (magent-capability-toggle-locally "org-structure") 'disabled))
    (should-not (magent-capability-enabled-p
                 (magent-capability-get "org-structure")))))

(ert-deftest magent-test-capability-resolution-summary-includes-active-and-suggested ()
  "Test capability resolution summary remains concise and inspectable."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-register
     (magent-capability-create
      :name "org-structure"
      :family "org"
      :skills '("org-structure-workflow")
      :modes '(org-mode)
      :features '(org)
      :disclosure 'active))
    (magent-capability-register
     (magent-capability-create
      :name "git-workflow"
      :family "git"
      :skills '("git-workflow")
      :files '("*COMMIT_EDITMSG")
      :prompt-keywords '("commit")
      :disclosure 'suggested))
    (let ((summary (magent-capability-resolution-summary
                    (magent-capability-resolve
                     "Commit after reorganizing this subtree"
                     '(:major-mode org-mode
                       :major-mode-family (org-mode outline-mode text-mode)
                       :file-path "/tmp/COMMIT_EDITMSG"
                       :features (org))
                     nil))))
      (should (string-match-p "Auto capabilities: org-structure" summary))
      (should (string-match-p "Suggested: git-workflow" summary)))))

(ert-deftest magent-test-capability-resolve-zero-max-active-keeps-explicit-skills ()
  "Test zero auto-activation limit suppresses capability skill injection."
  (require 'magent-capability)
  (let ((magent-capability--registry nil)
        (magent-capability-max-active 0))
    (magent-capability-register
     (magent-capability-create
      :name "org-structure"
      :skills '("auto-skill")
      :modes '(org-mode)
      :features '(org)
      :prompt-keywords '("heading")
      :disclosure 'active))
    (let ((resolution (magent-capability-resolve
                       "Please reorganize this heading"
                       '(:major-mode org-mode
                         :major-mode-family (org-mode text-mode)
                         :features (org))
                       '("manual-skill"))))
      (should-not (magent-capability-resolution-active-capabilities resolution))
      (should (equal (magent-capability-resolution-skill-names resolution)
                     '("manual-skill"))))))

(ert-deftest magent-test-capability-resolve-records-debug-contributions ()
  "Test resolver debug details preserve individual score contributions."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-register
     (magent-capability-create
      :name "org-structure"
      :skills '("org-structure-workflow")
      :modes '(org-mode)
      :features '(org)
      :files '("*.org")
      :prompt-keywords '("heading")
      :disclosure 'active))
    (let* ((resolution (magent-capability-resolve
                        "Please reorganize this heading"
                        '(:major-mode org-mode
                          :major-mode-family (org-mode text-mode)
                          :file-path "/tmp/notes.org"
                          :file-extension "org"
                          :features (org))
                        nil))
           (match (car (magent-capability-resolution-active-capabilities resolution)))
           (contributions (plist-get (magent-capability-match-details match)
                                     :contributions)))
      (should (= (magent-capability-match-score match) 8))
      (should (equal (mapcar (lambda (entry) (plist-get entry :kind)) contributions)
                     '(mode feature file keyword))))))

(ert-deftest magent-test-capability-resolve-mixed-org-and-git-context ()
  "Test org context plus git wording does not hide the org capability."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-register
     (magent-capability-create
      :name "git-workflow"
      :skills '("git-workflow")
      :prompt-keywords '("commit")
      :disclosure 'suggested))
    (magent-capability-register
     (magent-capability-create
      :name "org-structure"
      :skills '("org-structure-workflow")
      :modes '(org-mode)
      :features '(org)
      :disclosure 'active))
    (let* ((resolution (magent-capability-resolve
                        "Please commit the result after reorganizing this subtree"
                        '(:major-mode org-mode
                          :major-mode-family (org-mode text-mode)
                          :features (org))
                        nil))
           (matches (magent-capability-resolution-matches resolution)))
      (should (equal (magent-capability-name
                      (magent-capability-match-capability (car matches)))
                     "org-structure"))
      (should (eq (magent-capability-match-status (car matches)) 'active))
      (should (equal (magent-capability-name
                      (magent-capability-match-capability (cadr matches)))
                     "git-workflow"))
      (should (eq (magent-capability-match-status (cadr matches)) 'hidden)))))

(ert-deftest magent-test-capability-resolve-magit-mode-family-and-keyword ()
  "Test a Magit family match combines with commit wording deterministically."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-register
     (magent-capability-create
      :name "magit-workflow"
      :skills '("magit-workflow")
      :modes '(magit-mode)
      :prompt-keywords '("commit")
      :disclosure 'active))
    (let* ((resolution (magent-capability-resolve
                        "Help me commit these changes"
                        '(:major-mode magit-status-mode
                          :major-mode-family (magit-status-mode magit-mode special-mode fundamental-mode))
                        nil))
           (match (car (magent-capability-resolution-active-capabilities resolution))))
      (should match)
      (should (= (magent-capability-match-score match) 4))
      (should (equal (mapcar #'identity (magent-capability-match-reasons match))
                     '("mode-family=magit-mode" "keyword=commit"))))))

;; ──────────────────────────────────────────────────────────────────────
;;; Skill file parsing tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-skills-parse-type ()
  "Test skill type string parsing."
  (require 'magent-skills)
  (should (eq (magent-skills--parse-type "tool") 'tool))
  (should (eq (magent-skills--parse-type "instruction") 'instruction))
  (should (eq (magent-skills--parse-type "TOOL") 'tool))
  (should (eq (magent-skills--parse-type "unknown") 'instruction)))

(ert-deftest magent-test-skills-parse-tools ()
  "Test tool spec parsing."
  (require 'magent-skills)
  ;; Comma-separated string
  (should (equal (magent-skills--parse-tools "bash, read, write")
                 '(bash read write)))
  ;; Single string
  (should (equal (magent-skills--parse-tools "bash") '(bash)))
  ;; Symbol
  (should (equal (magent-skills--parse-tools 'bash) '(bash)))
  ;; List of strings
  (should (equal (magent-skills--parse-tools '("bash" "read")) '(bash read)))
  ;; nil
  (should (null (magent-skills--parse-tools nil))))

(ert-deftest magent-test-skills-load-file-from-temp ()
  "Test loading a skill from a temporary file."
  (require 'magent-skills)
  (let* ((magent-skills--registry nil)
         (tmpdir (make-temp-file "skill-" t))
         (skillfile (expand-file-name "SKILL.md" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file skillfile
            (insert "---\nname: test-skill\ndescription: A test\ntype: instruction\ntools: bash, read\n---\nDo the thing."))
          (let ((skill (magent-skills-load-file skillfile)))
            (should skill)
            (should (equal (magent-skill-name skill) "test-skill"))
            (should (equal (magent-skill-description skill) "A test"))
            (should (eq (magent-skill-type skill) 'instruction))
            (should (equal (magent-skill-tools skill) '(bash read)))
            (should (string-match-p "Do the thing" (magent-skill-prompt skill)))))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-skills-load-all-includes-systematic-debugging ()
  "Test builtin skill loading includes the systematic debugging workflow."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (cl-letf (((symbol-function 'magent-log) #'ignore))
      (magent-skills-load-all (list magent-skills--builtin-dir)))
    (let ((skill (magent-skills-get "systematic-debugging")))
      (should skill)
      (should (eq (magent-skill-type skill) 'instruction))
      (should (string-match-p "Systematic Debugging"
                              (or (magent-skill-prompt skill) ""))))))

(ert-deftest magent-test-capability-load-file-from-temp ()
  "Test loading a capability from a temporary file."
  (require 'magent-capability)
  (let* ((magent-capability--registry nil)
         (tmpdir (make-temp-file "capability-" t))
         (capfile (expand-file-name "CAPABILITY.md" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file capfile
            (insert
             "---\n"
             "name: org-structure\n"
             "description: Structured org editing\n"
             "source: package\n"
             "package: org\n"
             "skills: org-structure-workflow\n"
             "modes: org-mode\n"
             "features: org\n"
             "keywords: heading, subtree\n"
             "disclosure: active\n"
             "---\n"
             "Use for org structure.\n"))
          (let ((capability (magent-capability-load-file capfile)))
            (should capability)
            (should (equal (magent-capability-name capability) "org-structure"))
            (should (eq (magent-capability-source-kind capability) 'package))
            (should (equal (magent-capability-skills capability)
                           '("org-structure-workflow")))
            (should (equal (magent-capability-modes capability) '(org-mode)))
            (should (equal (magent-capability-features capability) '(org)))))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-capability-load-file-normalizes-list-metadata ()
  "Test capability file loader normalizes strings, CSV fields, and lists."
  (require 'magent-capability)
  (let* ((magent-capability--registry nil)
         (tmpdir (make-temp-file "capability-" t))
         (capfile (expand-file-name "CAPABILITY.md" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file capfile
            (insert
             "---\n"
             "name: package-reload\n"
             "skills: reload-workflow, diagnose-workflow\n"
             "modes: emacs-lisp-mode, lisp-interaction-mode\n"
             "features: emacs-lisp, lisp-mode\n"
             "files: *.el, init.el\n"
             "keywords: reload, package, config\n"
             "---\n"))
          (let ((capability (magent-capability-load-file capfile)))
            (should capability)
            (should (equal (magent-capability-skills capability)
                           '("reload-workflow" "diagnose-workflow")))
            (should (equal (magent-capability-modes capability)
                           '(emacs-lisp-mode lisp-interaction-mode)))
            (should (equal (magent-capability-features capability)
                           '(emacs-lisp lisp-mode)))
            (should (equal (magent-capability-files capability)
                           '("*.el" "init.el")))
            (should (equal (magent-capability-prompt-keywords capability)
                           '("reload" "package" "config")))))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-capability-load-file-derives-family ()
  "Test capability files carry explicit or derived family metadata."
  (require 'magent-capability)
  (let* ((magent-capability--registry nil)
         (tmpdir (make-temp-file "capability-" t))
         (capfile (expand-file-name "CAPABILITY.md" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file capfile
            (insert
             "---\n"
             "name: org-structure\n"
             "source: package\n"
             "package: org\n"
             "---\n"))
          (let ((capability (magent-capability-load-file capfile)))
            (should capability)
            (should (equal (magent-capability-family capability) "org"))))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-capability-external-metadata-does-not-override-policy ()
  "Test external metadata cannot override maintainer-controlled policy fields."
  (require 'magent-capability)
  (let* ((magent-capability--registry nil)
         (magent-capability-directories nil)
         (tmpdir (make-temp-file "capability-external-" t))
         (capdir (expand-file-name "pkg-cap" tmpdir))
         (capfile (expand-file-name "CAPABILITY.md" capdir)))
    (unwind-protect
        (progn
          (make-directory capdir t)
          (with-temp-file capfile
            (insert
             "---\n"
             "name: package-cap\n"
             "source: package\n"
             "package: pkg-demo\n"
             "family: attacker-family\n"
             "disclosure: active\n"
             "risk: high\n"
             "skills: project-workflow\n"
             "keywords: package demo\n"
             "---\n"))
          (let ((capability (magent-capability-load-file capfile)))
            (should capability)
            (should (equal (magent-capability-family capability) "pkg-demo"))
            (should (eq (magent-capability-disclosure capability) 'suggested))
            (should (eq (magent-capability-risk capability) 'low))))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-capability-reload-updates-running-registry ()
  "Test reloading capabilities updates file-backed definitions in place."
  (require 'magent-capability)
  (let* ((magent-capability--registry nil)
         (magent-capability-directories nil)
         (tmpdir (make-temp-file "capability-reload-" t))
         (capdir (expand-file-name "reload-cap" tmpdir))
         (capfile (expand-file-name "CAPABILITY.md" capdir)))
    (unwind-protect
        (progn
          (setq magent-capability-directories (list tmpdir))
          (make-directory capdir t)
          (with-temp-file capfile
            (insert
             "---\n"
             "name: reload-cap\n"
             "description: First description\n"
             "skills: project-workflow\n"
             "keywords: first\n"
             "---\n"))
          (magent-capability-load-all (list tmpdir))
          (should (equal (magent-capability-description
                          (magent-capability-get "reload-cap"))
                         "First description"))
          (with-temp-file capfile
            (insert
             "---\n"
             "name: reload-cap\n"
             "description: Updated description\n"
             "skills: project-workflow\n"
             "keywords: second\n"
             "---\n"))
          (magent-capability-reload)
          (should (equal (magent-capability-description
                          (magent-capability-get "reload-cap"))
                         "Updated description"))
          (should (equal (magent-capability-prompt-keywords
                          (magent-capability-get "reload-cap"))
                         '("second"))))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-capability-reload-drops-removed-file-entry ()
  "Test reloading capabilities drops removed file-defined entries."
  (require 'magent-capability)
  (let* ((magent-capability--registry nil)
         (magent-capability-directories nil)
         (tmpdir (make-temp-file "capability-reload-" t))
         (capdir (expand-file-name "reload-cap" tmpdir))
         (capfile (expand-file-name "CAPABILITY.md" capdir)))
    (unwind-protect
        (progn
          (setq magent-capability-directories (list tmpdir))
          (make-directory capdir t)
          (with-temp-file capfile
            (insert
             "---\n"
             "name: reload-cap\n"
             "skills: project-workflow\n"
             "---\n"))
          (magent-capability-load-all (list tmpdir))
          (should (magent-capability-get "reload-cap"))
          (delete-file capfile)
          (magent-capability-reload)
          (should-not (magent-capability-get "reload-cap")))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-capability-reload-restores-active-project-capability ()
  "Test capability reload restores the active project's local capabilities."
  (require 'magent-capability)
  (let* ((magent-capability--registry nil)
         (magent-capability-directories nil)
         (project-root (file-truename
                        (directory-file-name
                         (make-temp-file "magent-project-" t))))
         (cap-dir (expand-file-name ".magent/capabilities/project-cap" project-root))
         (cap-file (expand-file-name "CAPABILITY.md" cap-dir))
         (magent-runtime--active-project-scope project-root))
    (unwind-protect
        (progn
          (make-directory cap-dir t)
          (with-temp-file cap-file
            (insert
             "---\n"
             "name: project-cap\n"
             "description: First description\n"
             "skills: project-workflow\n"
             "keywords: first\n"
             "---\n"))
          (cl-letf (((symbol-function 'magent-log) #'ignore))
            (magent-capability-load-project-scope project-root))
          (should (equal (magent-capability-description
                          (magent-capability-get "project-cap"))
                         "First description"))
          (with-temp-file cap-file
            (insert
             "---\n"
             "name: project-cap\n"
             "description: Updated description\n"
             "skills: project-workflow\n"
             "keywords: second\n"
             "---\n"))
          (cl-letf (((symbol-function 'magent-log) #'ignore))
            (magent-capability-reload))
          (should (equal (magent-capability-description
                          (magent-capability-get "project-cap"))
                         "Updated description"))
          (should (equal (magent-capability-prompt-keywords
                          (magent-capability-get "project-cap"))
                         '("second"))))
      (delete-directory project-root t))))

(ert-deftest magent-test-reload-capabilities-prepares-project-context ()
  "Test interactive capability reload restores project-local capabilities on first use."
  (require 'magent-capability)
  (let* ((project-root (file-truename
                        (directory-file-name
                         (make-temp-file "magent-project-" t))))
         (cap-dir (expand-file-name ".magent/capabilities/project-cap" project-root))
         (cap-file (expand-file-name "CAPABILITY.md" cap-dir))
         (magent--initialized nil)
         (magent-runtime--active-project-scope nil)
         (magent--current-session nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent-agent-registry--agents (make-hash-table :test #'equal))
         (magent-agent-registry--initialized nil)
         (magent-skills--registry nil)
         (magent-capability--registry nil))
    (unwind-protect
        (progn
          (make-directory cap-dir t)
          (with-temp-file cap-file
            (insert
             "---\n"
             "name: project-cap\n"
             "description: Project-local capability\n"
             "skills: project-workflow\n"
             "---\n"))
          (with-temp-buffer
            (setq default-directory project-root)
            (cl-letf (((symbol-function 'magent-project-root)
                       (lambda (&optional directory no-fallback)
                         (ignore directory no-fallback)
                         project-root))
                      ((symbol-function 'magent-log) #'ignore))
              (magent-reload-capabilities)))
          (should (equal (magent-runtime-active-project-scope) project-root))
          (should (equal (magent-capability-description
                          (magent-capability-get "project-cap"))
                         "Project-local capability")))
      (delete-directory project-root t))))

(ert-deftest magent-test-capability-load-all-includes-builtin-families ()
  "Test builtin capability loading includes new builtin and curated package families."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-load-all (list magent-capability--builtin-dir))
    (dolist (name '("emacs-hook-debugging"
                    "emacs-config-reload"
                    "emacs-command-variable-introspection"
                    "project-workflow"
                    "lsp-workspace-workflow"))
      (should (magent-capability-get name)))))

(ert-deftest magent-test-builtin-capability-activates-for-hook-debugging ()
  "Test builtin hook debugging capability activates in Emacs Lisp buffers."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-load-all (list magent-capability--builtin-dir))
    (let* ((resolution (magent-capability-resolve
                        "Diagnose why this hook and key binding are shadowed"
                        '(:major-mode emacs-lisp-mode
                          :major-mode-family (emacs-lisp-mode prog-mode)
                          :features (emacs))
                        nil))
           (active-names (mapcar (lambda (match)
                                   (magent-capability-name
                                    (magent-capability-match-capability match)))
                                 (magent-capability-resolution-active-capabilities resolution))))
      (should (member "emacs-hook-debugging" active-names)))))

(ert-deftest magent-test-builtin-capability-activates-for-command-variable-introspection ()
  "Test builtin command and variable introspection activates in scratch-like contexts."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-load-all (list magent-capability--builtin-dir))
    (let* ((resolution (magent-capability-resolve
                        "Inspect this command and variable binding for me"
                        '(:major-mode lisp-interaction-mode
                          :major-mode-family (lisp-interaction-mode emacs-lisp-mode prog-mode)
                          :features (emacs))
                        nil))
           (active-names (mapcar (lambda (match)
                                   (magent-capability-name
                                    (magent-capability-match-capability match)))
                                 (magent-capability-resolution-active-capabilities resolution))))
      (should (member "emacs-command-variable-introspection" active-names)))))

(ert-deftest magent-test-builtin-capability-activates-for-config-reload-diagnosis ()
  "Test builtin config reload capability activates for diagnosis-style reload prompts."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-load-all (list magent-capability--builtin-dir))
    (let* ((resolution (magent-capability-resolve
                        "Diagnose why reloading init.el leaves stale package state"
                        '(:major-mode emacs-lisp-mode
                          :major-mode-family (emacs-lisp-mode prog-mode)
                          :file-path "/tmp/init.el"
                          :features (emacs))
                        nil))
           (active-names (mapcar (lambda (match)
                                   (magent-capability-name
                                    (magent-capability-match-capability match)))
                                 (magent-capability-resolution-active-capabilities resolution))))
      (should (member "emacs-config-reload" active-names)))))

(ert-deftest magent-test-curated-project-capability-activates-with-project-wording ()
  "Test curated project capability activates from explicit project workflow wording."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-load-all (list magent-capability--builtin-dir))
    (let* ((resolution (magent-capability-resolve
                        "Switch project and show me the current project root"
                        '(:major-mode emacs-lisp-mode
                          :major-mode-family (emacs-lisp-mode prog-mode)
                          :features (project))
                        nil))
           (active-names (mapcar (lambda (match)
                                   (magent-capability-name
                                    (magent-capability-match-capability match)))
                                 (magent-capability-resolution-active-capabilities resolution))))
      (should (member "project-workflow" active-names)))))

(ert-deftest magent-test-curated-lsp-capability-activates-with-lsp-context ()
  "Test curated LSP capability activates only in programming/LSP contexts."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-load-all (list magent-capability--builtin-dir))
    (let* ((resolution (magent-capability-resolve
                        "Use diagnostics and rename symbol across the workspace"
                        '(:major-mode python-mode
                          :major-mode-family (python-mode prog-mode)
                          :features (lsp-mode))
                        nil))
           (active-names (mapcar (lambda (match)
                                   (magent-capability-name
                                    (magent-capability-match-capability match)))
                                 (magent-capability-resolution-active-capabilities resolution))))
      (should (member "lsp-workspace-workflow" active-names)))))

(ert-deftest magent-test-curated-package-features-do-not_auto_activate_irrelevant_prompt ()
  "Test installed package features alone do not force curated capability activation."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-load-all (list magent-capability--builtin-dir))
    (let* ((resolution (magent-capability-resolve
                        "Hello there"
                        '(:major-mode fundamental-mode
                          :major-mode-family (fundamental-mode)
                          :features (project lsp-mode org magit))
                        nil))
           (active-names (mapcar (lambda (match)
                                   (magent-capability-name
                                    (magent-capability-match-capability match)))
                                 (magent-capability-resolution-active-capabilities resolution))))
    (should-not (member "project-workflow" active-names))
    (should-not (member "lsp-workspace-workflow" active-names))
    (should-not (member "magit-workflow" active-names))
    (should-not (member "org-structure-workflow" active-names)))))

(ert-deftest magent-test-ensure-initialized-loads-skills-before-capabilities ()
  "Test Magent initialization loads skills before capabilities."
  (let ((magent--initialized nil)
        calls)
    (cl-letf (((symbol-function 'magent-audit-enable)
               (lambda () (push 'audit calls)))
              ((symbol-function 'magent-agent-initialize-static)
               (lambda () (push 'agent-registry calls)))
              ((symbol-function 'magent-skills-initialize-static)
               (lambda (&optional _dirs) (push 'skills calls)))
              ((symbol-function 'magent-capability-initialize-static)
               (lambda (&optional _dirs) (push 'capabilities calls)))
              ((symbol-function 'magent-log) #'ignore))
      (magent--ensure-initialized))
    (should (equal (nreverse calls)
                   '(audit agent-registry skills capabilities)))))

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
  (should (eq (magent-tools-permission-key "spawn_agent") 'agent))
  (should (eq (magent-tools-permission-key "send_agent_message") 'agent))
  (should (eq (magent-tools-permission-key "wait_agent") 'agent))
  (should (eq (magent-tools-permission-key "list_agents") 'agent))
  (should (eq (magent-tools-permission-key "close_agent") 'agent))
  (should (eq (magent-tools-permission-key "skill_invoke") 'skill))
  (should (eq (magent-tools-permission-key "web_search") 'web_search))
  (should (null (magent-tools-permission-key "nonexistent"))))

(ert-deftest magent-test-tools-spawn-agent-creates-durable-job ()
  "Test spawn_agent records a child job and uses summary-only UI."
  (require 'magent-tools)
  (let* ((parent-session (magent-session-create :id "parent"))
         (parent-context (magent-request-context-create
                          :id "req-parent"
                          :scope "/tmp/project-parent"
                          :session parent-session
                          :approval-session parent-session
                          :origin-buffer-name "*origin*"
                          :origin-context 'origin
                          :agent-depth 0
                          :project-root "/tmp/project-parent"
                          :model 'parent-model
                          :temperature 0.2
                          :top-p 0.9
                          :skill-names '("parent-skill")
                          :capability-context
                          '(:skill-names ("parent-skill")
                            :context (:project-root "/tmp/project-parent"))
                          :permission-profile
                          (magent-permission-from-config
                           '((agent . ask)
                             (read . allow)
                             (bash . deny)
                             (* . allow)))
                          :live-p (lambda () t)
                          :event-context 'parent-event))
         (agent (magent-agent-info-create
                 :name "explore"
                 :mode 'subagent
                 :permission (magent-permission-from-config
                              '((agent . deny)
                                (read . allow)
                                (* . deny)))))
         (child-loop (progn
                       (require 'magent-agent-loop)
                       (magent-agent-loop-create)))
         (captured nil)
         (stopped nil)
         (cleanup nil)
         (aborted nil)
         (result nil))
    (let ((magent-tools--request-context parent-context)
          (magent-tools--register-cancel (lambda (fn) (setq cleanup fn)))
          (magent-tools--agent-job-runtimes (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'magent-agent-registry-get)
                 (lambda (_name) agent))
                ((symbol-function 'magent-events-create-subagent-context)
                 (lambda (title parent)
                   (list :title title :parent parent)))
                ((symbol-function 'magent-events-stop-subagent)
                 (lambda (context)
                   (setq stopped context)))
                ((symbol-function 'magent-agent-process)
                 (lambda (prompt callback agent-info skill-names event-context
                                 request-context capability-resolution ui-callback
                                 request-live-p request-state)
                   (setq captured
                         (list :prompt prompt
                               :agent agent-info
                               :skill-names skill-names
                               :event-context event-context
                               :request-context request-context
                               :capability-resolution capability-resolution
                               :ui-callback ui-callback
                               :request-live-p request-live-p
                               :request-state request-state))
                   (funcall callback "child answer")
                   child-loop))
                ((symbol-function 'magent-agent-loop-abort)
                 (lambda (loop)
                   (setq aborted loop))))
        (magent-tools--spawn-agent
         (lambda (value) (setq result value))
         "explore"
         "inspect"
         "scan")
        (when cleanup
          (funcall cleanup))))
    (let* ((decoded (let ((json-object-type 'alist)
                          (json-array-type 'list))
                      (json-read-from-string result)))
           (job-payload (cdr (assq 'job decoded)))
           (job-id (cdr (assq 'id job-payload)))
           (job (magent-session-agent-job parent-session job-id))
           (child-state (plist-get captured :request-state))
           (metadata (magent-agent-job-metadata job))
           (permission-profile (cdr (assq 'permission-profile metadata))))
      (should (equal (cdr (assq 'status decoded)) "spawned"))
      (should (magent-agent-job-p job))
      (should (equal (magent-agent-job-agent-name job) "explore"))
      (should (equal (magent-agent-job-task-name job) "scan"))
      (should (eq (magent-agent-job-status job) 'completed))
      (should (equal (magent-agent-job-result job) "child answer"))
      (should (equal (cdr (assq 'status job-payload)) "completed"))
      (should (equal (plist-get captured :prompt) "inspect"))
      (should (eq (plist-get captured :agent) agent))
      (should (equal (plist-get captured :event-context)
                     '(:title "Agent explore: scan" :parent parent-event)))
      (should (eq (plist-get captured :request-context) 'origin))
      (should (null (plist-get captured :capability-resolution)))
      (should (null (plist-get captured :ui-callback)))
      (should (null (plist-get captured :request-live-p)))
      (should (magent-request-context-p child-state))
      (should (eq (magent-request-context-ui-visibility child-state) 'summary-only))
      (should (equal (magent-request-context-parent-request-id child-state) "req-parent"))
      (should (equal (magent-request-context-scope child-state) "/tmp/project-parent"))
      (should (equal (magent-request-context-project-root child-state) "/tmp/project-parent"))
      (should (= (magent-request-context-agent-depth child-state) 1))
      (should (eq (magent-request-context-model child-state) 'parent-model))
      (should (= (magent-request-context-temperature child-state) 0.2))
      (should (= (magent-request-context-top-p child-state) 0.9))
      (should (equal (magent-request-context-skill-names child-state)
                     '("parent-skill")))
      (should (equal (magent-request-context-capability-context child-state)
                     '(:skill-names ("parent-skill")
                       :context (:project-root "/tmp/project-parent"))))
      (should (equal (magent-permission-resolve
                      (magent-request-context-permission-profile child-state)
                      'agent)
                     'deny))
      (should (equal (magent-permission-resolve
                      (magent-request-context-permission-profile child-state)
                      'read)
                     'allow))
      (should (equal (magent-permission-resolve
                      (magent-request-context-permission-profile child-state)
                      'bash)
                     'deny))
      (should (eq (magent-request-context-approval-session child-state) parent-session))
      (should-not (eq (magent-request-context-session child-state) parent-session))
      (should (eq (magent-session-agent (magent-request-context-session child-state))
                  agent))
      (should (equal (cdr (assq 'project-root metadata)) "/tmp/project-parent"))
      (should (= (cdr (assq 'agent-depth metadata)) 1))
      (should (equal (cdr (assq 'ui-visibility metadata)) "summary-only"))
      (should (equal (cdr (assq 'model metadata)) "parent-model"))
      (should (= (cdr (assq 'temperature metadata)) 0.2))
      (should (= (cdr (assq 'top-p metadata)) 0.9))
      (should (equal (append (cdr (assq 'skill-names metadata)) nil)
                     '("parent-skill")))
      (should (equal (cdr (assq 'agent permission-profile)) "deny"))
      (should (equal (cdr (assq 'read permission-profile)) "allow"))
      (should (equal stopped '(:title "Agent explore: scan" :parent parent-event)))
      (should cleanup)
      (should-not aborted))))

(ert-deftest magent-test-tools-spawn-agent-enforces-max-depth ()
  "Test recursive child-agent spawning is blocked by depth guard."
  (require 'magent-tools)
  (let* ((parent-session (magent-session-create :id "parent"))
         (parent-context (magent-request-context-create
                          :id "child-req"
                          :session parent-session
                          :approval-session parent-session
                          :agent-depth 1
                          :project-root "/tmp/project"))
         (agent (magent-agent-info-create :name "general" :mode 'subagent))
         (magent-child-agent-max-depth 1)
         (started nil)
         result)
    (let ((magent-tools--request-context parent-context)
          (magent-tools--agent-job-runtimes (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'magent-agent-registry-get)
                 (lambda (_name) agent))
                ((symbol-function 'magent-agent-process)
                 (lambda (&rest _args)
                   (setq started t))))
        (magent-tools--spawn-agent
         (lambda (value) (setq result value))
         "general"
         "nested"
         "nested-task")))
    (let* ((decoded (let ((json-object-type 'alist)
                          (json-array-type 'list))
                      (json-read-from-string result)))
           (job-payload (cdr (assq 'job decoded)))
           (job-id (cdr (assq 'id job-payload)))
           (job (magent-session-agent-job parent-session job-id))
           (metadata (magent-agent-job-metadata job)))
      (should (equal (cdr (assq 'status decoded)) "failed"))
      (should (magent-agent-job-p job))
      (should (eq (magent-agent-job-status job) 'failed))
      (should (string-match-p "max depth 1 exceeded"
                              (magent-agent-job-error job)))
      (should (= (cdr (assq 'agent-depth metadata)) 2))
      (should (= (cdr (assq 'max-depth metadata)) 1))
      (should-not started))))

(ert-deftest magent-test-tools-resolve-path-uses-request-project-root ()
  "Test relative file paths resolve against inherited request project root."
  (require 'magent-tools)
  (let* ((tmpdir (file-truename
                  (directory-file-name (make-temp-file "magent-root-" t))))
         (default-directory "/tmp/")
         (context (magent-request-context-create
                   :project-root tmpdir
                   :scope "/different/scope"))
         result)
    (unwind-protect
        (let ((magent-tools--request-context context))
          (with-temp-file (expand-file-name "notes.txt" tmpdir)
            (insert "from inherited root"))
          (magent-tools--read-file
           (lambda (value) (setq result value))
           "notes.txt")
          (should (equal result "from inherited root")))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-tools-list-wait-send-close-agent-jobs ()
  "Test child-agent lifecycle tools over durable job state."
  (require 'magent-tools)
  (let* ((parent-session (magent-session-create :id "parent"))
         (agent (magent-agent-info-create :name "explore" :mode 'subagent))
         (child-session (magent-session-create :agent agent))
         (job (magent-agent-job-create
               :id "agent-1"
               :parent-session-id "parent"
               :agent-name "explore"
               :task-name "scan"
               :status 'completed
               :prompt "inspect"
               :result "first result"))
         list-result
         wait-result
         send-result
         close-result
         (captured-prompts nil)
         (aborted nil))
    (magent-session-add-agent-job parent-session job)
    (let ((runtime-table (make-hash-table :test #'equal)))
      (puthash "agent-1"
               (list :session child-session
                     :agent agent
                     :request-context nil
                     :subagent-context nil
                     :loop nil)
               runtime-table)
      (let ((magent-tools--request-context
             (magent-request-context-create
              :id "parent-req"
              :session parent-session
              :approval-session parent-session))
            (magent-tools--agent-job-runtimes runtime-table))
        (cl-letf (((symbol-function 'magent-events-create-subagent-context)
                   (lambda (title parent)
                     (list :title title :parent parent)))
                  ((symbol-function 'magent-events-stop-subagent) #'ignore)
                  ((symbol-function 'magent-agent-process)
                   (lambda (prompt callback _agent-info _skill-names
                                   _event-context _request-context
                                   _capability-resolution _ui-callback
                                   _request-live-p _request-state)
                     (push prompt captured-prompts)
                     (funcall callback (concat "reply: " prompt))
                     nil))
                  ((symbol-function 'magent-agent-loop-abort)
                   (lambda (loop) (setq aborted loop))))
          (magent-tools--list-agents
           (lambda (value) (setq list-result value)))
          (magent-tools--wait-agent
           (lambda (value) (setq wait-result value))
           "agent-1" nil 0)
          (magent-tools--send-agent-message
           (lambda (value) (setq send-result value))
           "agent-1"
           "follow up")
          (magent-tools--close-agent
           (lambda (value) (setq close-result value))
           "agent-1"
           "done"))))
    (let* ((list-json (let ((json-object-type 'alist)
                            (json-array-type 'list))
                        (json-read-from-string list-result)))
           (wait-json (let ((json-object-type 'alist)
                            (json-array-type 'list))
                        (json-read-from-string wait-result)))
           (send-json (let ((json-object-type 'alist)
                            (json-array-type 'list))
                        (json-read-from-string send-result)))
           (close-json (let ((json-object-type 'alist)
                             (json-array-type 'list))
                         (json-read-from-string close-result))))
      (should (equal (cdr (assq 'status list-json)) "ok"))
      (should (= (length (cdr (assq 'jobs list-json))) 1))
      (should (equal (cdr (assq 'status wait-json)) "completed"))
      (should (equal (cdr (assq 'status send-json)) "sent"))
      (should (equal captured-prompts '("follow up")))
      (should (eq (magent-agent-job-status job) 'closed))
      (should (equal (magent-agent-job-result job) "reply: follow up"))
      (should (equal (cdr (assq 'status close-json)) "closed"))
      (should-not aborted))))

(ert-deftest magent-test-tools-all-registered ()
  "Test that all core tools are registered."
  (require 'magent-tools)
  (should (= (length magent-tools--all-gptel-tools) 14)))

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

(ert-deftest magent-test-tools-filtering-bypass-permission ()
  "Test bypass config ignores per-agent permission filtering."
  (require 'magent-tools)
  (require 'magent-agent-registry)
  (let* ((magent-by-pass-permission t)
         (magent-enable-tools magent-tools--permission-keys)
         (agent (magent-agent-info-create
                 :name "no-tools"
                 :permission (magent-permission-create
                              :rules '((* . deny)))))
         (tools (magent-tools-get-gptel-tools agent)))
    (should (= (length tools) (length magent-tools--all-gptel-tools)))))

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

(ert-deftest magent-test-mode-line-lighter-renders-from-processing-state ()
  "Test the mode-line lighter depends only on processing-state APIs."
  (require 'magent)
  (let ((magent--spinner (spinner-create 'progress-bar-filled)))
    (cl-letf (((symbol-function 'magent-ui-processing-p) (lambda () nil)))
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

(ert-deftest magent-test-permission-prompt-choice-once-allow ()
  "Test tool confirmation accepts a one-time allow choice."
  (require 'magent-tool-orchestrator)
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
                 (funcall cb 'allow-once))))
      (magent-tool-orchestrator-handle-tool-calls
       (magent-tool-orchestrator-create
        :permission '((bash . ask))
        :run-tool-function
        (lambda (_tool-spec cb arg-values)
          (setq tool-ran (car arg-values))
          (funcall cb "ok"))
        :args-to-plist-function (lambda (_args-spec arg-values) arg-values)
        :summarize-function (lambda (arg-values _args-spec) (car arg-values)))
       (list (list tool (list "echo hi") (lambda (r) (setq result r))))))
    (should (equal tool-ran "echo hi"))
    (should (equal result "ok"))
    (should (null (magent-permission-session-override 'bash)))))

(ert-deftest magent-test-permission-prompt-choice-once-deny ()
  "Test tool confirmation accepts a one-time deny choice."
  (require 'magent-tool-orchestrator)
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
      (magent-tool-orchestrator-handle-tool-calls
       (magent-tool-orchestrator-create
        :permission '((bash . ask))
        :run-tool-function
        (lambda (tool-spec cb arg-values)
          (setq tool-ran t)
          (funcall cb (apply (gptel-tool-function tool-spec) arg-values)))
        :args-to-plist-function (lambda (_args-spec arg-values) arg-values)
        :summarize-function (lambda (arg-values _args-spec) (car arg-values)))
       (list (list tool (list "echo hi") (lambda (r) (setq result r))))))
    (should-not tool-ran)
    (should (string-match-p "denied by user" result))
    (should (null (magent-permission-session-override 'bash)))))

(ert-deftest magent-test-permission-prompt-choice-always-allow ()
  "Test tool confirmation persists an always-allow choice."
  (require 'magent-tool-orchestrator)
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
                 (funcall cb 'allow-session))))
      (magent-tool-orchestrator-handle-tool-calls
       (magent-tool-orchestrator-create
        :permission '((bash . ask))
        :run-tool-function
        (lambda (_tool-spec cb arg-values)
          (setq tool-ran (car arg-values))
          (funcall cb "ok"))
        :args-to-plist-function (lambda (_args-spec arg-values) arg-values)
        :summarize-function (lambda (arg-values _args-spec) (car arg-values)))
       (list (list tool (list "echo hi") (lambda (r) (setq result r))))))
    (should (equal tool-ran "echo hi"))
    (should (equal result "ok"))
    (should (eq (magent-permission-session-override 'bash) 'allow))
    (magent-permission-clear-session-overrides)))

(ert-deftest magent-test-permission-prompt-choice-always-deny ()
  "Test tool confirmation persists an always-deny choice."
  (require 'magent-tool-orchestrator)
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
      (magent-tool-orchestrator-handle-tool-calls
       (magent-tool-orchestrator-create
        :permission '((bash . ask))
        :run-tool-function
        (lambda (tool-spec cb arg-values)
          (setq tool-ran t)
          (funcall cb (apply (gptel-tool-function tool-spec) arg-values)))
        :args-to-plist-function (lambda (_args-spec arg-values) arg-values)
        :summarize-function (lambda (arg-values _args-spec) (car arg-values)))
       (list (list tool (list "echo hi") (lambda (r) (setq result r))))))
    (should-not tool-ran)
    (should (string-match-p "denied by user" result))
    (should (eq (magent-permission-session-override 'bash) 'deny))
    (magent-permission-clear-session-overrides)))

(ert-deftest magent-test-permission-bypass-skips-deny-and-prompt ()
  "Test bypass config executes tool calls without prompting."
  (require 'magent-tool-orchestrator)
  (require 'magent-permission)
  (magent-permission-clear-session-overrides)
  (let ((magent-by-pass-permission t)
        (result nil)
        (tool-ran nil)
        (tool (gptel-make-tool
               :name "bash"
               :args (list '(:name "command" :type string))
               :function (lambda (_command) "ok")
               :async nil)))
    (unwind-protect
        (progn
          (magent-permission-set-session-override 'bash 'deny)
          (cl-letf (((symbol-function 'magent-approval-request)
                     (lambda (&rest _)
                       (ert-fail "bypass should not prompt for approval"))))
            (magent-tool-orchestrator-handle-tool-calls
             (magent-tool-orchestrator-create
              :permission '((bash . ask))
              :run-tool-function
              (lambda (_tool-spec cb arg-values)
                (setq tool-ran (car arg-values))
                (funcall cb "ok"))
              :args-to-plist-function (lambda (_args-spec arg-values) arg-values)
              :summarize-function (lambda (arg-values _args-spec) (car arg-values)))
             (list (list tool (list "echo hi")
                         (lambda (r) (setq result r)))))))
      (magent-permission-clear-session-overrides))
    (should (equal tool-ran "echo hi"))
    (should (equal result "ok"))))

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

(ert-deftest magent-test-ui-interrupt-renders-plain-status-line ()
  "Test interrupt renders plain text instead of an error heading."
  (require 'magent-agent-loop)
  (let ((buffer (magent-ui-get-buffer))
        (magent--current-fsm (magent-agent-loop-create))
        (magent-ui--request-generation 0))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (cl-letf (((symbol-function 'magent-agent-loop-abort) #'ignore)
              ((symbol-function 'magent-approval-drop-requests) #'ignore)
              ((symbol-function 'spinner-stop) #'ignore)
              ((symbol-function 'magent-ui--clear-processing) #'ignore)
              ((symbol-function 'magent-ui--maybe-show-input-prompt) #'ignore))
      (magent-interrupt))
    (with-current-buffer buffer
      (should (string-match-p (regexp-quote "[Interrupted by user]")
                              (buffer-string)))
      (should-not (string-match-p
                   (concat "^\\* " (regexp-quote magent-error-prompt) " +$")
                   (buffer-string))))))

(ert-deftest magent-test-ui-interrupt-does-not-double-abort-turn-loop ()
  "Test interrupt aborts the active turn loop only once."
  (require 'magent-agent-loop)
  (require 'magent-turn)
  (let* ((buffer (magent-ui-get-buffer))
         (loop (magent-agent-loop-create))
         (magent--current-fsm loop)
         (magent-turn--current-fsm loop)
         (magent-turn--active
          (magent-turn-submission-create
           :id "sub-interrupt"
           :op (magent-protocol-interrupt-op)
           :status 'running))
         (magent-turn--queue nil)
         aborted)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (cl-letf (((symbol-function 'magent-agent-loop-abort)
               (lambda (value) (push value aborted)))
              ((symbol-function 'magent-approval-drop-requests) #'ignore)
              ((symbol-function 'spinner-stop) #'ignore)
              ((symbol-function 'magent-ui--clear-processing) #'ignore)
              ((symbol-function 'magent-ui--maybe-show-input-prompt) #'ignore))
      (magent-interrupt))
    (should (equal aborted (list loop)))))

(ert-deftest magent-test-emacs-eval-cancel-cleanup-prevents-late-callback ()
  "Test cancelling emacs_eval before its timer fires suppresses the callback."
  (require 'magent-tools)
  (let ((quit-flag nil)
        (registered-cleanup nil)
        (scheduled nil)
        (cancelled nil)
        (callback-result nil)
        (thread-object 'fake-thread)
        (signaled nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest _args)
                 (setq scheduled fn)
                 'fake-timer))
              ((symbol-function 'make-thread)
               (lambda (_fn &optional _name)
                 thread-object))
              ((symbol-function 'thread-live-p)
               (lambda (thread)
                 (eq thread thread-object)))
              ((symbol-function 'thread-signal)
               (lambda (thread signal data)
                 (setq signaled (list thread signal data))))
              ((symbol-function 'cancel-timer)
               (lambda (timer)
                 (setq cancelled timer))))
      (let ((magent-tools--register-cancel
             (lambda (cleanup)
               (setq registered-cleanup cleanup))))
        (magent-tools--emacs-eval
         (lambda (result)
           (setq callback-result result))
         "(+ 1 2)")
        (should (functionp registered-cleanup))
        (let ((inhibit-quit t))
          (funcall registered-cleanup)
          (should (eq cancelled 'fake-timer))
          (should (equal signaled '(fake-thread quit nil)))
          (setq quit-flag nil))
        (funcall scheduled)))
    (should-not callback-result)))

(ert-deftest magent-test-emacs-eval-timeout-interrupts-worker ()
  "Test emacs_eval timeout signals the worker thread and returns a timeout."
  (require 'magent-tools)
  (let ((scheduled nil)
        (callback-result nil)
        (thread-object 'fake-thread)
        (signaled nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest _args)
                 (setq scheduled fn)
                 'fake-timer))
              ((symbol-function 'make-thread)
               (lambda (_fn &optional _name)
                 thread-object))
              ((symbol-function 'thread-live-p)
               (lambda (thread)
                 (eq thread thread-object)))
              ((symbol-function 'thread-signal)
               (lambda (thread signal data)
                 (setq signaled (list thread signal data))))
              ((symbol-function 'cancel-timer) #'ignore))
      (magent-tools--emacs-eval
       (lambda (result)
         (setq callback-result result))
       "(+ 1 2)"
       1)
      (should (functionp scheduled))
      (funcall scheduled))
    (should (equal signaled '(fake-thread quit nil)))
    (should (equal callback-result "Error: Evaluation timed out"))))

;; ──────────────────────────────────────────────────────────────────────
;;; UI/session regression tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-audit-record-appends-jsonl ()
  "Test audit records append into one daily JSONL file."
  (require 'magent-audit)
  (let* ((magent-enable-audit-log t)
         (magent-audit-directory (make-temp-file "magent-audit-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session (magent-session-create)))
    (unwind-protect
        (progn
          (magent-audit-record 'permission-decision
                               :decision 'allow
                               :decision-source 'bypass)
          (magent-audit-record 'permission-decision
                               :decision 'deny
                               :decision-source 'file-rule-deny)
          (magent-audit--flush-pending)
          (should (= (length (directory-files magent-audit-directory nil "\\.jsonl$")) 1))
          (should (= (length (magent-test--read-audit-records magent-audit-directory)) 2)))
      (delete-directory magent-audit-directory t))))

(ert-deftest magent-test-audit-record-disabled-skips-write ()
  "Test disabled audit logging does not create any files."
  (require 'magent-audit)
  (let ((magent-enable-audit-log nil)
        (magent-audit-directory (make-temp-file "magent-audit-" t)))
    (unwind-protect
        (progn
          (magent-audit-record 'permission-decision
                               :decision 'allow
                               :decision-source 'bypass)
          (magent-audit--flush-pending)
          (should-not (directory-files magent-audit-directory nil "\\.jsonl$")))
      (delete-directory magent-audit-directory t))))

(ert-deftest magent-test-audit-tool-events-redact-write-and-edit-payloads ()
  "Test persisted tool audit records redact write/edit file bodies."
  (require 'magent-audit)
  (let* ((magent-enable-audit-log t)
         (magent-audit-directory (make-temp-file "magent-audit-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session (magent-session-create))
         (context (magent-events-context-create :turn-id "turn-audit")))
    (unwind-protect
        (progn
          (magent-audit-enable)
          (magent-events-emit 'tool-call-start
                              :context context
                              :call-id "call-write"
                              :tool-name "write_file"
                              :summary "secret.txt"
                              :args '(:path "secret.txt" :content "super secret body"))
          (magent-events-emit 'tool-call-start
                              :context context
                              :call-id "call-edit"
                              :tool-name "edit_file"
                              :summary "secret.txt"
                              :args '(:path "secret.txt"
                                      :old_text "old secret"
                                      :new_text "new secret value"))
          (magent-audit--flush-pending)
          (let* ((records (magent-test--read-audit-records magent-audit-directory))
                 (write-record (car records))
                 (edit-record (cadr records))
                 (write-preview (cdr (assq 'args_preview write-record)))
                 (edit-preview (cdr (assq 'args_preview edit-record))))
            (should (equal (cdr (assq 'tool_name write-record)) "write_file"))
            (should (equal (cdr (assq 'path write-preview)) "secret.txt"))
            (should (= (cdr (assq 'content_length write-preview))
                       (length "super secret body")))
            (should-not (assq 'content write-preview))
            (should (equal (cdr (assq 'tool_name edit-record)) "edit_file"))
            (should (equal (cdr (assq 'path edit-preview)) "secret.txt"))
            (should (= (cdr (assq 'old_text_length edit-preview))
                       (length "old secret")))
            (should (= (cdr (assq 'new_text_length edit-preview))
                       (length "new secret value")))
            (should-not (assq 'old_text edit-preview))
            (should-not (assq 'new_text edit-preview))))
      (magent-audit-disable)
      (delete-directory magent-audit-directory t))))

(ert-deftest magent-test-audit-approval-hooks-persist-request-and-resolution ()
  "Test approval lifecycle events are persisted with decision metadata."
  (require 'magent-audit)
  (let* ((magent-enable-audit-log t)
         (magent-audit-directory (make-temp-file "magent-audit-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session (magent-session-create))
         (magent-approval-provider-function (lambda (_request) nil))
         (magent-approval--pending-requests (make-hash-table :test 'equal))
         (magent-approval--completed-requests (make-hash-table :test 'equal))
         (magent-approval-state-change-functions nil)
         (context (magent-events-context-create :turn-id "turn-approval")))
    (unwind-protect
        (progn
          (magent-audit-enable)
          (magent-approval-request
           (list :request-id "req-1"
                 :context context
                 :tool-name "bash"
                 :perm-key 'bash
                 :summary "echo hi"
                 :args '(:command "echo hi"))
           #'ignore)
          (magent-approval-resolve-request "req-1" 'allow-session)
          (magent-audit--flush-pending)
          (let ((records (magent-test--read-audit-records magent-audit-directory)))
            (should (= (length records) 2))
            (should (equal (mapcar (lambda (record) (cdr (assq 'event record))) records)
                           '("approval-requested" "approval-resolved")))
            (should (equal (cdr (assq 'turn_id (car records))) "turn-approval"))
            (should (equal (cdr (assq 'decision_source (cadr records)))
                           "user-allow-session"))
            (should (equal (cdr (assq 'decision (cadr records))) "allow"))))
      (magent-audit-disable)
      (delete-directory magent-audit-directory t))))

(ert-deftest magent-test-audit-permission-session-override-is-persisted ()
  "Test session override permission decisions are persisted."
  (require 'magent-audit)
  (require 'magent-agent-loop)
  (require 'magent-permission)
  (let* ((magent-enable-audit-log t)
         (magent-audit-directory (make-temp-file "magent-audit-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session (magent-session-create))
         (magent-permission--session-overrides (make-hash-table :test 'eq))
         (tool (gptel-make-tool
                :name "bash"
                :args (list '(:name "command" :type string))
                :function (lambda (_command) "ok")
                :async nil))
         result)
    (unwind-protect
        (progn
          (magent-audit-enable)
          (magent-permission-set-session-override 'bash 'allow)
          (magent-tool-orchestrator-handle-tool-calls
           (magent-tool-orchestrator-create
            :permission '((bash . ask))
            :run-tool-function
            (lambda (_tool-spec cb arg-values)
              (funcall cb (format "ran %s" (car arg-values))))
            :audit-function #'magent-agent-loop-audit-permission-decision
            :file-arg-index-function #'magent-agent-loop-find-file-arg-index
            :args-to-plist-function #'magent-agent-loop-args-to-plist
            :summarize-function #'magent-agent-loop-summarize-args)
           (list (list tool (list "echo hi") (lambda (value)
                                                (setq result value)))))
          (should (equal result "ran echo hi"))
          (magent-audit--flush-pending)
          (let* ((records (magent-test--read-audit-records magent-audit-directory))
                 (record (car records)))
            (should (= (length records) 1))
            (should (equal (cdr (assq 'event record)) "permission-decision"))
            (should (equal (cdr (assq 'tool_name record)) "bash"))
            (should (equal (cdr (assq 'decision record)) "allow"))
            (should (equal (cdr (assq 'decision_source record))
                           "session-override-allow"))))
      (magent-audit-disable)
      (delete-directory magent-audit-directory t))))

(ert-deftest magent-test-audit-permission-file-rule-deny-is-persisted ()
  "Test file-rule deny permission decisions are persisted."
  (require 'magent-audit)
  (require 'magent-agent-loop)
  (let* ((magent-enable-audit-log t)
         (magent-audit-directory (make-temp-file "magent-audit-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session (magent-session-create))
         (tool (gptel-make-tool
                :name "read_file"
                :args (list '(:name "path" :type string))
                :function (lambda (_path) "ok")
                :async nil))
         result)
    (unwind-protect
        (progn
          (magent-audit-enable)
          (magent-tool-orchestrator-handle-tool-calls
           (magent-tool-orchestrator-create
            :permission '((read . (("*.env" . deny)
                                   (* . ask))))
            :run-tool-function (lambda (&rest _args)
                                 (ert-fail "denied tool should not run"))
            :audit-function #'magent-agent-loop-audit-permission-decision
            :file-arg-index-function #'magent-agent-loop-find-file-arg-index
            :args-to-plist-function #'magent-agent-loop-args-to-plist
            :summarize-function #'magent-agent-loop-summarize-args)
           (list (list tool (list ".env") (lambda (value)
                                             (setq result value)))))
          (should (string-match-p "access denied" result))
          (magent-audit--flush-pending)
          (let* ((records (magent-test--read-audit-records magent-audit-directory))
                 (record (car records))
                 (args-preview (cdr (assq 'args_preview record))))
            (should (= (length records) 1))
            (should (equal (cdr (assq 'decision record)) "deny"))
            (should (equal (cdr (assq 'decision_source record)) "file-rule-deny"))
            (should (equal (cdr (assq 'path args-preview)) ".env"))))
      (magent-audit-disable)
      (delete-directory magent-audit-directory t))))

(ert-deftest magent-test-audit-write-failure-does-not-signal ()
  "Test audit persistence failures never interrupt Magent execution."
  (require 'magent-audit)
  (let ((magent-enable-audit-log t)
        (magent-audit-directory (make-temp-file "magent-audit-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'append-to-file)
                   (lambda (&rest _args)
                     (error "disk full"))))
          (should-not
           (condition-case nil
               (progn
                 (magent-audit-record 'permission-decision
                                      :decision 'allow
                                      :decision-source 'bypass)
                 (magent-audit--flush-pending)
                 nil)
             (error t))))
      (delete-directory magent-audit-directory t))))

(ert-deftest magent-test-audit-record-queues-write-until-flush ()
  "Test audit writes stay queued until the deferred flush runs."
  (require 'magent-audit)
  (let* ((magent-enable-audit-log t)
         (magent-audit-directory (make-temp-file "magent-audit-" t))
         (magent-audit-flush-delay 60)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session (magent-session-create)))
    (unwind-protect
        (progn
          (magent-audit-record 'permission-decision
                               :decision 'allow
                               :decision-source 'bypass)
          (should magent-audit--pending-writes)
          (should-not (directory-files magent-audit-directory nil "\\.jsonl$"))
          (magent-audit--flush-pending)
          (should-not magent-audit--pending-writes)
          (should (= (length (magent-test--read-audit-records magent-audit-directory)) 1)))
      (magent-audit--flush-pending)
      (delete-directory magent-audit-directory t))))

(ert-deftest magent-test-audit-browser-respects-default-time-window ()
  "Test the audit browser only shows records inside the default day window."
  (require 'magent-audit)
  (let* ((magent-audit-directory (make-temp-file "magent-audit-ui-" t))
         (magent-audit-default-days 1)
         (magent-audit-max-records 50)
         (recent-time (format-time-string "%Y-%m-%dT%H:%M:%S%z" (current-time)))
         (old-time (format-time-string
                    "%Y-%m-%dT%H:%M:%S%z"
                    (time-subtract (current-time) (days-to-time 3))))
         buffer)
    (unwind-protect
        (progn
          (magent-test--write-audit-record-file
           magent-audit-directory
           "audit-test.jsonl"
           `(((timestamp . ,recent-time)
              (event . "permission-decision")
              (decision . "allow")
              (tool_name . "bash")
              (summary . "recent audit record"))
             ((timestamp . ,old-time)
              (event . "permission-decision")
              (decision . "deny")
              (tool_name . "read_file")
              (summary . "stale audit record"))))
          (setq buffer (magent-show-audit))
          (with-current-buffer buffer
            (should (derived-mode-p 'magent-audit-mode))
            (should (= (length magent-audit--all-records) 1))
            (should (= (length magent-audit--visible-records) 1))
            (should (string-match-p "recent audit record" (buffer-string)))
            (should-not (string-match-p "stale audit record" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory magent-audit-directory t))))

(ert-deftest magent-test-audit-browser-filters-and-expands-details ()
  "Test audit browser filters records and expands inline details."
  (require 'magent-audit)
  (let* ((magent-audit-directory (make-temp-file "magent-audit-ui-" t))
         (magent-audit-default-days 7)
         (magent-audit-max-records 50)
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z" (current-time)))
         buffer)
    (unwind-protect
        (progn
          (magent-test--write-audit-record-file
           magent-audit-directory
           "audit-test.jsonl"
           `(((timestamp . ,timestamp)
              (event . "permission-decision")
              (decision . "allow")
              (tool_name . "bash")
              (request_id . "req-allow")
              (summary . "allowed command"))
             ((timestamp . ,timestamp)
              (event . "permission-decision")
              (decision . "deny")
              (tool_name . "read_file")
              (request_id . "req-deny")
              (summary . "blocked env read")
              (args_preview . ((path . ".env"))))))
          (setq buffer (magent-show-audit))
          (with-current-buffer buffer
            (magent-audit--set-filter-value :decision "deny")
            (should (= (length magent-audit--visible-records) 1))
            (should (string-match-p "blocked env read" (buffer-string)))
            (should-not (string-match-p "allowed command" (buffer-string)))
            (goto-char (point-min))
            (re-search-forward "blocked env read")
            (beginning-of-line)
            (magent-audit-toggle-entry)
            (should (string-match-p "request-id: req-deny" (buffer-string)))
            (should (string-match-p "args-preview:" (buffer-string)))
            (should (string-match-p "\\(path \\. \".env\"\\)" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory magent-audit-directory t))))

(ert-deftest magent-test-audit-browser-skips-malformed-jsonl ()
  "Test malformed audit lines are ignored without breaking the browser."
  (require 'magent-audit)
  (let* ((magent-audit-directory (make-temp-file "magent-audit-ui-" t))
         (magent-audit-default-days 7)
         (magent-audit-max-records 50)
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z" (current-time)))
         (file (expand-file-name "audit-test.jsonl" magent-audit-directory))
         buffer)
    (unwind-protect
        (progn
          (make-directory magent-audit-directory t)
          (with-temp-file file
            (insert "{not-json}\n")
            (insert
             (json-encode
              `((timestamp . ,timestamp)
                (event . "tool-call-end")
                (status . "ok")
                (tool_name . "bash")
                (summary . "valid record after malformed line"))))
            (insert "\n"))
          (setq buffer (magent-show-audit))
          (with-current-buffer buffer
            (should (= magent-audit--load-errors 1))
            (should (= (length magent-audit--visible-records) 1))
            (should (string-match-p "valid record after malformed line"
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory magent-audit-directory t))))

(ert-deftest magent-test-session-scope-from-directory-falls-back-to-global ()
  "Test session scope is global when no project root is detected."
  (cl-letf (((symbol-function 'magent-project-root)
             (lambda (&optional _directory _no-fallback) nil)))
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
      (delete-directory magent-session-directory t)))

(ert-deftest magent-test-session-save-load-preserves-approval-overrides ()
  "Test session approval overrides persist through save/load."
  (require 'magent-permission)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (let ((session (magent-session-get)))
            (magent-session-add-message session 'user "hello")
            (magent-permission-set-session-override 'bash 'allow session)
            (magent-session-save))
          (let* ((files (directory-files magent-session-directory t "\\.json$"))
                 (loaded (magent-session-read-file (car files)))
                 (loaded-session (plist-get loaded :session)))
            (should (eq (magent-session-approval-override loaded-session 'bash)
                        'allow))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-session-save-load-preserves-tool-message ()
  "Test structured tool messages persist through save/load."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (let ((session (magent-session-get)))
            (magent-session-add-message session 'user "Run ls")
            (magent-session-add-tool-message
             session "call_1" "bash" '(:command "ls") "ok")
            (magent-session-save))
          (let* ((files (directory-files magent-session-directory t "\\.json$"))
                 (loaded (magent-session-read-file (car files)))
                 (loaded-session (plist-get loaded :session))
                 (messages (magent-session-get-messages loaded-session)))
            (should (= (length messages) 2))
            (should (equal (magent-msg-content (nth 1 messages))
                           '(:id "call_1"
                             :name "bash"
                             :args (:command "ls")
                             :result "ok")))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-session-save-load-sanitizes-symbol-tool-args ()
  "Test session persistence handles symbol tool names and arguments."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (let ((session (magent-session-get)))
            (magent-session-add-message session 'user "Run tool")
            (magent-session-add-tool-message
             session "call_1" 'emacs_eval
             '(:sexp "(+ 20 22)" :tool emacs_eval :values [emacs_eval nil])
             "42")
            (magent-session-save))
          (let* ((files (directory-files magent-session-directory t "\\.json$"))
                 (loaded (magent-session-read-file (car files)))
                 (loaded-session (plist-get loaded :session))
                 (messages (magent-session-get-messages loaded-session)))
            (should (equal (magent-msg-content (nth 1 messages))
                           '(:id "call_1"
                             :name "emacs_eval"
                             :args (:sexp "(+ 20 22)"
                                    :tool "emacs_eval"
                                    :values ("emacs_eval" nil))
                             :result "42")))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-agent-job-create-and-transition ()
  "Test basic child-agent job creation and status transitions."
  (require 'magent-agent-job)
  (let ((job (magent-agent-job-create
              :id "agent-1"
              :parent-session-id "parent"
              :agent-name "explore"
              :task-name "scan"
              :prompt "inspect files"
              :created-at 100.0
              :updated-at 100.0)))
    (should (magent-agent-job-p job))
    (should (equal (magent-agent-job-id job) "agent-1"))
    (should (eq (magent-agent-job-status job) 'queued))
    (should (equal (magent-agent-job-parent-session-id job) "parent"))
    (magent-agent-job-set-status job 'running)
    (should (eq (magent-agent-job-status job) 'running))
    (magent-agent-job-set-status job 'completed "done")
    (should (eq (magent-agent-job-status job) 'completed))
    (should (equal (magent-agent-job-result job) "done"))))

(ert-deftest magent-test-agent-job-find ()
  "Test finding child-agent jobs by id."
  (require 'magent-agent-job)
  (let ((job-a (magent-agent-job-create :id "agent-a"))
        (job-b (magent-agent-job-create :id "agent-b")))
    (should (eq (magent-agent-job-find (list job-a job-b) "agent-b")
                job-b))
    (should-not (magent-agent-job-find (list job-a job-b) "missing"))))

(ert-deftest magent-test-session-agent-job-lookup-and-status ()
  "Test session helpers for child-agent jobs."
  (require 'magent-agent-job)
  (let* ((session (magent-session-create :id "parent"))
         (job (magent-agent-job-create
               :id "agent-1"
               :parent-session-id "parent"
               :agent-name "general")))
    (magent-session-add-agent-job session job)
    (should (eq (magent-session-agent-job session "agent-1") job))
    (magent-session-set-agent-job-status session "agent-1" 'failed nil "boom")
    (should (eq (magent-agent-job-status job) 'failed))
    (should (equal (magent-agent-job-error job) "boom"))))

(ert-deftest magent-test-session-save-load-preserves-agent-jobs ()
  "Test child-agent jobs persist through session save/load."
  (require 'magent-agent-job)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (let ((session (magent-session-get)))
            (magent-session-add-message session 'user "spawn child")
            (magent-session-add-agent-job
             session
             (magent-agent-job-create
              :id "agent-1"
              :parent-session-id "parent"
              :agent-name "explore"
              :task-name "scan"
              :status 'completed
              :prompt "inspect files"
              :created-at 100.0
              :updated-at 120.0
              :transcript '(((role . "assistant") (content . "found it")))
              :result "found it")))
            (magent-session-save))
          (let* ((files (directory-files magent-session-directory t "\\.json$"))
                 (loaded (magent-session-read-file (car files)))
                 (loaded-session (plist-get loaded :session))
                 (job (magent-session-agent-job loaded-session "agent-1")))
            (should job)
            (should (equal (magent-agent-job-agent-name job) "explore"))
            (should (equal (magent-agent-job-task-name job) "scan"))
            (should (eq (magent-agent-job-status job) 'completed))
            (should (equal (magent-agent-job-prompt job) "inspect files"))
            (should (equal (magent-agent-job-result job) "found it"))
            (should (equal (magent-agent-job-transcript job)
                           '(((role . "assistant") (content . "found it")))))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-llm-event-constructors ()
  "Test normalized LLM event constructors."
  (require 'magent-llm)
  (let ((text (magent-llm-text-delta-event "hello"))
        (reasoning (magent-llm-reasoning-delta-event "thinking"))
        (reasoning-end (magent-llm-reasoning-end-event))
        (tool (magent-llm-tool-call-event
               "call-1" "read_file" '(:path "README.org") 'raw))
        (completed (magent-llm-completed-event
                    "done" '(:input 10 :output 5) 'stop))
        (err (magent-llm-error-event "boom" '(:status 500))))
    (should (eq (magent-llm-event-type text) 'text-delta))
    (should (equal (magent-llm-event-text text) "hello"))
    (should (eq (magent-llm-event-type reasoning) 'reasoning-delta))
    (should (equal (magent-llm-event-text reasoning) "thinking"))
    (should (eq (magent-llm-event-type reasoning-end) 'reasoning-end))
    (should (eq (magent-llm-event-type tool) 'tool-call))
    (should (equal (magent-llm-event-id tool) "call-1"))
    (should (equal (magent-llm-event-name tool) "read_file"))
    (should (equal (magent-llm-event-arguments tool) '(:path "README.org")))
    (should (eq (magent-llm-event-raw tool) 'raw))
    (should (eq (magent-llm-event-type completed) 'completed))
    (should (equal (magent-llm-event-text completed) "done"))
    (should (equal (magent-llm-event-usage completed) '(:input 10 :output 5)))
    (should (eq (magent-llm-event-stop-reason completed) 'stop))
    (should (eq (magent-llm-event-type err) 'error))
    (should (equal (magent-llm-event-message err) "boom"))
    (should (equal (magent-llm-event-metadata err) '(:status 500)))))

(ert-deftest magent-test-llm-event-plist-round-trip ()
  "Test normalized LLM events round-trip through plist shape."
  (require 'magent-llm)
  (let* ((event (magent-llm-tool-call-event
                 "call-1" "bash" '(:command "pwd") 'raw '(:provider gptel)))
         (plist (magent-llm-event-to-plist event))
         (round-tripped (magent-llm-event-from-plist plist)))
    (should (equal plist
                   '(:type tool-call
                     :id "call-1"
                     :name "bash"
                     :arguments (:command "pwd")
                     :raw raw
                     :metadata (:provider gptel))))
    (should (eq (magent-llm-event-type round-tripped) 'tool-call))
    (should (equal (magent-llm-event-id round-tripped) "call-1"))
    (should (equal (magent-llm-event-name round-tripped) "bash"))
    (should (equal (magent-llm-event-arguments round-tripped)
                   '(:command "pwd")))
    (should (equal (magent-llm-event-metadata round-tripped)
                   '(:provider gptel)))))

(ert-deftest magent-test-llm-request-validation ()
  "Test normalized LLM request construction and validation."
  (require 'magent-llm)
  (let ((request (magent-llm-request-create
                  :prompt '((user . "hello"))
                  :system "system"
                  :tools '(read_file)
                  :model 'gpt-4o-mini
                  :backend 'gptel
                  :stream t
                  :callback #'ignore
                  :metadata '(:turn-id "turn-1"))))
    (should (magent-llm-request-p request))
    (should (equal (magent-llm-request-prompt request)
                   '((user . "hello"))))
    (should (equal (magent-llm-request-system request) "system"))
    (should (equal (magent-llm-request-tools request) '(read_file)))
    (should (eq (magent-llm-request-model request) 'gpt-4o-mini))
    (should (eq (magent-llm-request-backend request) 'gptel))
    (should (eq (magent-llm-request-stream request) t))
    (should (eq (magent-llm-request-callback request) #'ignore))
    (should-error (magent-llm-request-create :callback "not-callable"))
    (should-error (magent-llm-event-create 'not-an-event))))

(ert-deftest magent-test-llm-gptel-sample-calls-gptel-request ()
  "Test gptel adapter calls `gptel-request' through the request boundary."
  (require 'magent-llm-gptel)
  (let ((captured-prompt nil)
        (captured-kwargs nil)
        (events nil)
        sample-handle)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (prompt &rest kwargs)
                 (setq captured-prompt prompt
                       captured-kwargs kwargs)
                 (funcall (plist-get kwargs :callback)
                          "hello"
                          '(:status "ok"))
                 (funcall (plist-get kwargs :callback)
                          t
                          '(:content "hello" :status "ok" :tokens (:total 3)))
                 'fake-fsm)))
      (setq sample-handle
            (magent-llm-gptel-sample
             (magent-llm-request-create
              :prompt '((user . "hello"))
              :system "system"
              :tools '(fake-tool)
              :model 'fake-model
              :backend 'fake-backend
              :stream t
              :callback (lambda (event) (push event events)))))
      (should (bufferp sample-handle)))
    (when (buffer-live-p sample-handle)
      (kill-buffer sample-handle))
    (should (equal captured-prompt '((user . "hello"))))
    (should (equal (plist-get captured-kwargs :system) "system"))
    (should (eq (plist-get captured-kwargs :stream) t))
    (should (plist-get captured-kwargs :fsm))
    (should (= (length events) 2))
    (let ((completed (car events))
          (delta (cadr events)))
      (should (eq (magent-llm-event-type delta) 'text-delta))
      (should (equal (magent-llm-event-text delta) "hello"))
      (should (eq (magent-llm-event-type completed) 'completed))
      (should (equal (magent-llm-event-text completed) "hello"))
      (should (equal (magent-llm-event-usage completed) '(:total 3))))))

(ert-deftest magent-test-llm-gptel-callback-maps-reasoning-and-error ()
  "Test gptel adapter maps reasoning and error callbacks."
  (require 'magent-llm-gptel)
  (let* ((events nil)
         (request (magent-llm-request-create
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state)))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback
           request state buffer '(reasoning . "think") '(:status "ok"))
          (magent-llm-gptel--callback
           request state buffer '(reasoning . t) '(:status "ok"))
          (magent-llm-gptel--callback
           request state buffer nil '(:status "bad request" :http-status 400))
          (should (= (length events) 3))
          (should (eq (magent-llm-event-type (nth 2 events)) 'reasoning-delta))
          (should (equal (magent-llm-event-text (nth 2 events)) "think"))
          (should (eq (magent-llm-event-type (nth 1 events)) 'reasoning-end))
          (should (eq (magent-llm-event-type (nth 0 events)) 'error))
          (should (equal (magent-llm-event-message (nth 0 events))
                         "bad request")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-llm-gptel-normalizes-tool-call ()
  "Test gptel adapter maps tool-call callbacks to normalized events."
  (require 'magent-llm-gptel)
  (let* ((events nil)
         (request (magent-llm-request-create
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state)))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback
           request state buffer
           '(tool-call . ((nil (:path "README.org") nil
                               (:id "call-1"
                                :name "read_file"
                                :args (:path "README.org")))))
           '(:status "ok"))
          (should (= (length events) 1))
          (let ((event (car events)))
            (should (eq (magent-llm-event-type event) 'tool-call))
            (should (equal (magent-llm-event-id event) "call-1"))
            (should (equal (magent-llm-event-name event) "read_file"))
            (should (equal (magent-llm-event-arguments event)
                           '(:path "README.org")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-agent-loop-accumulates-normalized-events ()
  "Test agent loop state updates from normalized events."
  (require 'magent-agent-loop)
  (let ((loop (magent-agent-loop-create)))
    (magent-agent-loop-apply-event
     loop (magent-llm-text-delta-event "hel"))
    (magent-agent-loop-apply-event
     loop (magent-llm-text-delta-event "lo"))
    (magent-agent-loop-apply-event
     loop (magent-llm-reasoning-delta-event "think"))
    (magent-agent-loop-apply-event
     loop (magent-llm-tool-call-event "call-1" "read_file" '(:path "README.org")))
    (should (equal (magent-agent-loop-text loop) "hello"))
    (should (equal (magent-agent-loop-reasoning loop) "think"))
    (should (= (length (magent-agent-loop-tool-calls loop)) 1))
    (should (eq (magent-agent-loop-status loop) 'tool-pending))))

(ert-deftest magent-test-agent-loop-completes-and-fails-from-events ()
  "Test completed and error events update loop terminal state."
  (require 'magent-agent-loop)
  (let ((completed-loop (magent-agent-loop-create))
        (failed-loop (magent-agent-loop-create)))
    (magent-agent-loop-apply-event
     completed-loop
     (magent-llm-completed-event "done" '(:total 4) 'stop))
    (should (eq (magent-agent-loop-status completed-loop) 'completed))
    (should (equal (magent-agent-loop-result completed-loop) "done"))
    (should (equal (magent-agent-loop-usage completed-loop) '(:total 4)))
    (should (eq (magent-agent-loop-stop-reason completed-loop) 'stop))
    (magent-agent-loop-apply-event
     failed-loop
     (magent-llm-error-event "boom"))
    (should (eq (magent-agent-loop-status failed-loop) 'failed))
    (should (equal (magent-agent-loop-error failed-loop) "boom"))))

(ert-deftest magent-test-agent-loop-start-wraps-request-callback ()
  "Test loop start wraps request callback and invokes the sampler."
  (require 'magent-agent-loop)
  (let* ((forwarded nil)
         (sampled-request nil)
         (request (magent-llm-request-create
                   :prompt '((user . "hello"))
                   :stream t
                   :callback (lambda (event) (push event forwarded))))
         (loop (magent-agent-loop-create
                :request request
                :sampler (lambda (sample-request)
                           (setq sampled-request sample-request)
                           (funcall (magent-llm-request-callback sample-request)
                                    (magent-llm-text-delta-event "hi"))
                           (funcall (magent-llm-request-callback sample-request)
                                    (magent-llm-completed-event "hi"))
                           'sample-started))))
    (should (eq (magent-agent-loop-start loop) 'sample-started))
    (should (magent-llm-request-p sampled-request))
    (should (not (eq sampled-request request)))
    (should (equal (magent-agent-loop-text loop) "hi"))
    (should (equal (magent-agent-loop-result loop) "hi"))
    (should (eq (magent-agent-loop-status loop) 'completed))
    (should (= (length forwarded) 2))))

(ert-deftest magent-test-agent-loop-tool-round-limit ()
  "Test agent loop owns tool round counting and limit messages."
  (require 'magent-agent-loop)
  (let ((loop (magent-agent-loop-create :max-tool-rounds 2)))
    (should-not (magent-agent-loop-tool-round-limit-exceeded-p loop))
    (should (= (magent-agent-loop-tool-round-count loop) 1))
    (should-not (magent-agent-loop-tool-round-limit-exceeded-p loop))
    (should (= (magent-agent-loop-tool-round-count loop) 2))
    (should (magent-agent-loop-tool-round-limit-exceeded-p loop))
    (should (= (magent-agent-loop-tool-round-count loop) 3))
    (should (string-match-p
             "tool loop exceeded 2 rounds"
             (magent-agent-loop-tool-round-limit-message loop)))))

(ert-deftest magent-test-agent-loop-records-tool-result-in-session ()
  "Test agent loop records model-visible tool results in session."
  (require 'magent-agent-loop)
  (let* ((session (magent-session-create :id "session-1"))
         (loop (magent-agent-loop-create :session session)))
    (magent-agent-loop-record-tool-result
     loop nil '(:path "README.org") '(:id "call-1" :name "read_file") "content")
    (let* ((messages (magent-session-get-messages session))
           (message (car messages))
           (content (magent-msg-content message)))
      (should (eq (magent-msg-role message) 'tool))
      (should (equal (plist-get content :id) "call-1"))
      (should (equal (plist-get content :name) "read_file"))
      (should (equal (plist-get content :args) '(:path "README.org")))
      (should (equal (plist-get content :result) "content")))))

(ert-deftest magent-test-agent-loop-dispatches-known-tool-call ()
  "Test agent loop dispatches known tools through the orchestrator."
  (require 'magent-agent-loop)
  (require 'gptel)
  (let* ((session (magent-session-create :id "session-1"))
         (tool (gptel-make-tool
                :name "read_file"
                :description "read"
                :args (list '(:name "path" :type string))
                :function (lambda (path) (format "read %s" path))
                :async nil))
         (request (magent-llm-request-create :tools (list tool)))
         (loop (magent-agent-loop-create
                :session session
                :request request
                :max-tool-rounds 3))
         done)
    (magent-agent-loop-apply-event
     loop
     (magent-llm-tool-call-event
      "call-1" "read_file" '("README.org") '(:id "call-1" :name "read_file")))
    (magent-agent-loop-dispatch-tool-calls
     loop
     (magent-tool-orchestrator-create
      :permission (magent-permission-defaults)
      :run-tool-function
      (lambda (tool-spec cb arg-values)
        (funcall cb (apply (gptel-tool-function tool-spec) arg-values)))
      :file-arg-index-function (lambda (_args-spec) 0)
      :args-to-plist-function (lambda (_args-spec arg-values) arg-values)
      :summarize-function (lambda (arg-values _args-spec) (car arg-values)))
     (lambda (&optional _result) (setq done t)))
    (should done)
    (should (= (magent-agent-loop-tool-round-count loop) 1))
    (let* ((message (car (magent-session-get-messages session)))
           (content (magent-msg-content message)))
      (should (eq (magent-msg-role message) 'tool))
      (should (equal (plist-get content :id) "call-1"))
      (should (equal (plist-get content :name) "read_file"))
      (should (equal (plist-get content :args) '(:path "README.org")))
      (should (equal (plist-get content :result) "read README.org")))))

(ert-deftest magent-test-agent-loop-records-unknown-tool-error ()
  "Test agent loop records unknown tools as model-visible errors."
  (require 'magent-agent-loop)
  (require 'gptel)
  (let* ((session (magent-session-create :id "session-1"))
         (known (gptel-make-tool
                 :name "read_file"
                 :description "read"
                 :args nil
                 :function #'ignore
                 :async nil))
         (loop (magent-agent-loop-create
                :session session
                :request (magent-llm-request-create :tools (list known))
                :max-tool-rounds 3))
         done)
    (magent-agent-loop-apply-event
     loop
     (magent-llm-tool-call-event
      "call-1" "missing_tool" '(:x 1) '(:id "call-1" :name "missing_tool")))
    (magent-agent-loop-dispatch-tool-calls
     loop
     (magent-tool-orchestrator-create
      :run-tool-function (lambda (&rest _args)
                           (error "should not run")))
     (lambda (&optional _result) (setq done t)))
    (should done)
    (let* ((message (car (magent-session-get-messages session)))
           (content (magent-msg-content message)))
      (should (eq (magent-msg-role message) 'tool))
      (should (equal (plist-get content :name) "missing_tool"))
      (should (string-match-p
               "tool 'missing_tool' not found"
               (plist-get content :result))))))

(ert-deftest magent-test-agent-loop-tool-round-limit-blocks-dispatch ()
  "Test agent loop does not dispatch when the tool round limit is exceeded."
  (require 'magent-agent-loop)
  (let* ((loop (magent-agent-loop-create
                :request (magent-llm-request-create)
                :max-tool-rounds 0))
         ran done-result)
    (magent-agent-loop-apply-event
     loop
     (magent-llm-tool-call-event "call-1" "read_file" nil))
    (magent-agent-loop-dispatch-tool-calls
     loop
     (magent-tool-orchestrator-create
      :run-tool-function (lambda (&rest _args) (setq ran t)))
     (lambda (&optional result) (setq done-result result)))
    (should-not ran)
    (should (eq (magent-agent-loop-status loop) 'failed))
    (should (string-match-p "tool loop exceeded 0 rounds"
                            (magent-agent-loop-error loop)))
    (should (equal done-result (magent-agent-loop-error loop)))))

(ert-deftest magent-test-agent-loop-continue-rebuilds-prompt-from-session ()
  "Test loop continuation samples from the latest session transcript."
  (require 'magent-agent-loop)
  (let* ((session (magent-session-create :id "session-1"))
         (sampled-prompt nil)
         (request (magent-llm-request-create
                   :prompt '((prompt . "old"))
                   :system "system"
                   :stream t))
         (loop (magent-agent-loop-create
                :session session
                :request request
                :sampler (lambda (sample-request)
                           (setq sampled-prompt
                                 (magent-llm-request-prompt sample-request))
                           'continued))))
    (magent-session-add-message session 'user "Run tool")
    (magent-agent-loop-record-tool-result
     loop nil '(:path "README.org") '(:id "call-1" :name "read_file") "content")
    (should (eq (magent-agent-loop-continue loop) 'continued))
    (should (equal sampled-prompt
                   '((prompt . "Run tool")
                     (tool :id "call-1"
                           :name "read_file"
                           :args (:path "README.org")
                           :result "content"))))))

(ert-deftest magent-test-agent-loop-tool-guard-intercepts-duplicate-emacs-eval ()
  "Test duplicate emacs_eval calls are short-circuited by the new loop."
  (require 'magent-agent-loop)
  (require 'gptel)
  (let* ((tool-ran nil)
         (results nil)
         (tool (gptel-make-tool
                :name "emacs_eval"
                :description "eval"
                :args (list '(:name "sexp" :type string))
                :function (lambda (sexp)
                            (setq tool-ran sexp)
                            "first result")
                :async nil))
         (loop (magent-agent-loop-create))
         (context (magent-request-context-create
                   :ui-visibility 'summary-only)))
    (magent-agent-loop-run-tool
     loop context tool (lambda (result) (push result results))
     (list "(length (buffer-list))"))
    (magent-agent-loop-run-tool
     loop context tool (lambda (result) (push result results))
     (list "(length (buffer-list))"))
    (should (equal tool-ran "(length (buffer-list))"))
    (should (= (length results) 2))
    (should (string-match-p
             "already executed this exact emacs_eval query"
             (car results)))))

(ert-deftest magent-test-agent-loop-tool-guard-respects-emacs-eval-limit ()
  "Test new loop guard uses `magent-emacs-eval-max-calls-per-turn'."
  (require 'magent-agent-loop)
  (let ((guard-state (magent-agent-loop-tool-guard-state-create))
        (magent-emacs-eval-max-calls-per-turn 1))
    (should-not
     (magent-agent-loop-maybe-intercept-tool-call
      "emacs_eval" '(:sexp "(length (buffer-list))") guard-state))
    (let ((message
           (magent-agent-loop-maybe-intercept-tool-call
            "emacs_eval" '(:sexp "(buffer-list)") guard-state)))
      (should (string-match-p "tool_use_limit_reached" message))
      (should (string-match-p "emacs_eval exceeded 1 call" message)))
    (should (= (gethash :emacs-eval-count guard-state) 1))))

(ert-deftest magent-test-agent-loop-tool-guard-tracks-large-raw-results ()
  "Test new loop guard remembers large raw tool results."
  (require 'magent-agent-loop)
  (let ((guard-state (magent-agent-loop-tool-guard-state-create))
        (large-result (make-string 70000 ?x)))
    (magent-agent-loop-tool-guard-track-result
     "read_file" large-result guard-state)
    (should (equal (gethash :last-raw-content-tool guard-state)
                   "read_file"))
    (should (= (gethash :last-raw-content-bytes guard-state)
               (string-bytes large-result)))
    (should (string-match-p
             "70000 bytes of raw content"
             (magent-agent-loop-emacs-eval-limit-message
              1 guard-state)))))

(ert-deftest magent-test-agent-loop-run-tool-emits-events-and-renders-visible-ui ()
  "Test loop tool runner emits tool events and renders visible tool UI."
  (require 'magent-agent-loop)
  (require 'gptel)
  (let* ((events nil)
         (ui-events nil)
         (result nil)
         (tool (gptel-make-tool
                :name "bash"
                :description "shell"
                :args (list '(:name "command" :type string)
                            '(:name "reason" :type string))
                :function (lambda (command)
                            (format "ran %s" command))
                :async nil))
         (context (magent-request-context-create
                   :ui-visibility 'full
                   :event-context 'ctx))
         (loop (magent-agent-loop-create)))
    (cl-letf (((symbol-function 'magent-events-emit)
               (lambda (type &rest props)
                 (push (cons type props) events)))
              ((symbol-function 'magent-ui-insert-tool-call)
               (lambda (name summary)
                 (push (list 'call name summary) ui-events)))
              ((symbol-function 'magent-ui-insert-tool-result)
               (lambda (name summary)
                 (push (list 'result name summary) ui-events))))
      (magent-agent-loop-run-tool
       loop context tool (lambda (value) (setq result value))
       (list "echo hi" "inspect shell")))
    (should (equal result "ran echo hi"))
    (let ((ordered-events (nreverse events)))
      (should (equal (mapcar #'car ordered-events)
                     '(tool-call-start tool-call-end)))
      (let ((start (cdr (car ordered-events))))
        (should (eq (plist-get start :context) 'ctx))
        (should (equal (plist-get start :tool-name) "bash"))
        (should (equal (plist-get start :summary)
                       "[inspect shell] echo hi"))))
    (should (equal (nreverse ui-events)
                   '((call "bash" "[inspect shell] echo hi")
                     (result "bash" "ran echo hi"))))))

(ert-deftest magent-test-agent-loop-start-installs-request-abort-controller ()
  "Test loop start exposes its abort controller on request context."
  (require 'magent-agent-loop)
  (let* ((context (magent-request-context-create))
         (request (magent-llm-request-create
                   :prompt '((prompt . "hello"))))
         (loop (magent-agent-loop-create
                :request request
                :request-context context
                :sampler (lambda (_request) 'handle))))
    (should (eq (magent-agent-loop-start loop) 'handle))
    (should (eq (magent-request-context-abort-controller context)
                (magent-agent-loop-abort-controller loop)))))

(ert-deftest magent-test-agent-loop-abort-clears-request-context-controller ()
  "Test loop abort clears its request-context abort controller."
  (require 'magent-agent-loop)
  (let* ((context (magent-request-context-create))
         (loop (magent-agent-loop-create :request-context context)))
    (setf (magent-request-context-abort-controller context)
          (magent-agent-loop-abort-controller loop))
    (cl-letf (((symbol-function 'magent-events-end-turn) #'ignore))
      (magent-agent-loop-abort loop))
    (should (eq (magent-agent-loop-status loop) 'cancelled))
    (should-not (magent-request-context-abort-controller context))))

(ert-deftest magent-test-ui-interrupt-aborts-agent-loop ()
  "Test UI interrupt aborts the active Magent-owned loop."
  (require 'magent-ui)
  (require 'magent-turn)
  (let ((buffer (magent-ui-get-buffer))
        (loop (magent-agent-loop-create))
        (magent-turn--current-fsm nil)
        (magent-turn--active nil)
        (magent-turn--queue nil)
        (magent-ui--request-generation 0)
        aborted)
    (setq magent--current-fsm loop)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (cl-letf (((symbol-function 'magent-agent-loop-abort)
               (lambda (value) (setq aborted value)))
              ((symbol-function 'magent-approval-drop-requests) #'ignore)
              ((symbol-function 'spinner-stop) #'ignore)
              ((symbol-function 'magent-ui--clear-processing) #'ignore)
              ((symbol-function 'magent-ui--maybe-show-input-prompt) #'ignore))
      (magent-interrupt))
    (should (eq aborted loop))
    (should-not magent--current-fsm)))

(ert-deftest magent-test-agent-loop-run-tool-hides-summary-only-ui ()
  "Test summary-only request contexts suppress direct tool UI rendering."
  (require 'magent-agent-loop)
  (require 'gptel)
  (let* ((ui-events nil)
         (result nil)
         (tool (gptel-make-tool
                :name "read_file"
                :description "read"
                :args (list '(:name "path" :type string))
                :function (lambda (path) (format "read %s" path))
                :async nil))
         (context (magent-request-context-create
                   :ui-visibility 'summary-only))
         (loop (magent-agent-loop-create)))
    (cl-letf (((symbol-function 'magent-ui-insert-tool-call)
               (lambda (&rest args) (push args ui-events)))
              ((symbol-function 'magent-ui-insert-tool-result)
               (lambda (&rest args) (push args ui-events)))
              ((symbol-function 'magent-events-emit) #'ignore))
      (magent-agent-loop-run-tool
       loop context tool (lambda (value) (setq result value))
       (list "README.org")))
    (should (equal result "read README.org"))
    (should-not ui-events)))

(ert-deftest magent-test-agent-loop-tool-queue-serializes-async-tools ()
  "Test loop tool queue runs the next tool after async completion."
  (require 'magent-agent-loop)
  (require 'gptel)
  (let* ((first-callback nil)
         (order nil)
         (results nil)
         (async-tool (gptel-make-tool
                      :name "first"
                      :description "first"
                      :args nil
                      :function (lambda (callback)
                                  (push 'first-start order)
                                  (setq first-callback callback))
                      :async t))
         (sync-tool (gptel-make-tool
                     :name "second"
                     :description "second"
                     :args nil
                     :function (lambda ()
                                 (push 'second-run order)
                                 "second-result")
                     :async nil))
         (loop (magent-agent-loop-create)))
    (cl-letf (((symbol-function 'magent-events-emit) #'ignore))
      (magent-agent-loop-run-tool
       loop nil async-tool (lambda (value) (push value results)) nil)
      (magent-agent-loop-run-tool
       loop nil sync-tool (lambda (value) (push value results)) nil)
      (should (equal order '(first-start)))
      (should first-callback)
      (funcall first-callback "first-result"))
    (should (equal (nreverse order) '(first-start second-run)))
    (should (equal (nreverse results)
                   '("first-result" "second-result")))))

(ert-deftest magent-test-agent-loop-abort-drops-late-async-tool-result ()
  "Test loop abort suppresses late async tool completions."
  (require 'magent-agent-loop)
  (require 'magent-tools)
  (require 'gptel)
  (let* ((tool-callback nil)
         (cleanup nil)
         (result nil)
         (ui-events nil)
         (tool (gptel-make-tool
                :name "emacs_eval"
                :description "eval"
                :args (list '(:name "sexp" :type string))
                :function (lambda (callback sexp)
                            (should (equal sexp "(sleep-for 1)"))
                            (magent-tools--register-cancel-cleanup
                             (lambda () (setq cleanup 'ran)))
                            (setq tool-callback callback))
                :async t))
         (loop (magent-agent-loop-create)))
    (cl-letf (((symbol-function 'magent-events-emit) #'ignore)
              ((symbol-function 'magent-ui-insert-tool-call)
               (lambda (&rest _args) (push 'tool-call ui-events)))
              ((symbol-function 'magent-ui-insert-tool-result)
               (lambda (&rest _args) (push 'tool-result ui-events))))
      (magent-agent-loop-run-tool
       loop nil tool (lambda (value) (setq result value))
       (list "(sleep-for 1)"))
      (should tool-callback)
      (should (member 'tool-call ui-events))
      (magent-agent-loop-abort loop)
      (should (eq cleanup 'ran))
      (funcall tool-callback "\"done\""))
    (should-not result)
    (should-not (member 'tool-result ui-events))))

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

(ert-deftest magent-test-resume-session-candidates-include-time ()
  "Test `magent-resume-session' presents timestamps with time-of-day."
  (require 'magent-ui)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (session-file (expand-file-name "session-20260317-120000.json"
                                         magent-session-directory))
         captured-collection)
    (unwind-protect
        (progn
          (with-temp-file session-file
            (insert "{\"scope\":\"global\",\"summary-title\":\"Resume label\"}"))
          (cl-letf (((symbol-function 'magent-ui--activate-context-session) #'ignore)
                    ((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _args)
                       (setq captured-collection collection)
                       nil))
                    ((symbol-function 'message) #'ignore))
            (magent-resume-session))
          (should (equal captured-collection
                         '("2026-03-17 12:00:00  (global)  Resume label"))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-resume-session-affixation-shows-time ()
  "Test `magent-resume-session' provides a visible date/time prefix."
  (require 'magent-ui)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (session-file (expand-file-name "session-20260317-120000.json"
                                         magent-session-directory))
         captured-collection
         captured-properties)
    (unwind-protect
        (progn
          (with-temp-file session-file
            (insert "{\"scope\":\"global\",\"summary-title\":\"Resume label\"}"))
          (cl-letf (((symbol-function 'magent-ui--activate-context-session) #'ignore)
                    ((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _args)
                       (setq captured-collection collection
                             captured-properties completion-extra-properties)
                       nil))
                    ((symbol-function 'message) #'ignore))
            (magent-resume-session))
          (let* ((affixation (plist-get captured-properties :affixation-function))
                 (rows (funcall affixation captured-collection)))
            (should (equal rows
                           '(("2026-03-17 12:00:00  (global)  Resume label"
                              "[2026-03-17 12:00:00] "
                              ""))))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-session-format-file-legacy-name-falls-back-to-mtime ()
  "Test legacy session filenames still display a timestamp."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (session-file (expand-file-name "default.json" magent-session-directory))
         (mtime (encode-time 6 5 4 3 2 2026)))
    (unwind-protect
        (progn
          (with-temp-file session-file
            (insert "{\"scope\":\"global\",\"summary-title\":\"Legacy session\"}"))
          (set-file-times session-file mtime)
          (should (equal (magent-session--format-file session-file)
                         "2026-02-03 04:05:06  (global)  Legacy session")))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-session-list-files-sorts-within-group-by-session-time ()
  "Test files inside one group are ordered newest-to-oldest by session time."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (newer-file (expand-file-name "session-20260317-120000.json"
                                       magent-session-directory))
         (older-file (expand-file-name "session-20260317-110000.json"
                                       magent-session-directory))
         (newer-mtime (encode-time 0 0 1 1 1 2020))
         (older-mtime (encode-time 0 0 1 1 1 2030)))
    (unwind-protect
        (progn
          (with-temp-file newer-file
            (insert "{\"scope\":\"global\",\"summary-title\":\"Newer session\"}"))
          (with-temp-file older-file
            (insert "{\"scope\":\"global\",\"summary-title\":\"Older session\"}"))
          ;; Reverse mtimes so the test catches accidental mtime-based sorting.
          (set-file-times newer-file newer-mtime)
          (set-file-times older-file older-mtime)
          (should (equal (magent-session-list-files)
                         (list newer-file older-file))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-runtime-activate-scope-switches-project-overlays ()
  "Test runtime activation unloads the old overlay before loading the new one."
  (require 'magent-runtime)
  (let ((magent-runtime--active-project-scope nil)
        (magent-load-custom-agents t)
        (magent-session--scoped-sessions (make-hash-table :test #'equal))
        (events nil)
        (scope-a "/tmp/magent-project-a")
        (scope-b "/tmp/magent-project-b"))
    (puthash scope-a (magent-session-create :id "session-a")
             magent-session--scoped-sessions)
    (puthash scope-b (magent-session-create :id "session-b")
             magent-session--scoped-sessions)
    (cl-letf (((symbol-function 'magent-agent-file-load-project-scope)
               (lambda (scope) (push (list 'load-agent scope) events)))
              ((symbol-function 'magent-skills-load-project-scope)
               (lambda (scope) (push (list 'load-skill scope) events)))
              ((symbol-function 'magent-capability-load-project-scope)
               (lambda (scope) (push (list 'load-capability scope) events)))
              ((symbol-function 'magent-agent-registry-remove-project-scope)
               (lambda (scope) (push (list 'unload-agent scope) events)))
              ((symbol-function 'magent-skills-remove-project-scope)
               (lambda (scope) (push (list 'unload-skill scope) events)))
              ((symbol-function 'magent-capability-remove-project-scope)
               (lambda (scope) (push (list 'unload-capability scope) events)))
              ((symbol-function 'magent-session-refresh-agent)
               (lambda (session)
                 (push (list 'refresh (magent-session-id session)) events)
                 session))
              ((symbol-function 'magent-log) #'ignore))
      (magent-runtime-activate-scope scope-a)
      (magent-runtime-activate-scope scope-b)
      (magent-runtime-activate-scope 'global))
    (should (equal (nreverse events)
                   `((load-agent ,scope-a)
                     (load-skill ,scope-a)
                     (load-capability ,scope-a)
                     (refresh "session-a")
                     (unload-agent ,scope-a)
                     (unload-skill ,scope-a)
                     (unload-capability ,scope-a)
                     (load-agent ,scope-b)
                     (load-skill ,scope-b)
                     (load-capability ,scope-b)
                     (refresh "session-b")
                     (unload-agent ,scope-b)
                     (unload-skill ,scope-b)
                     (unload-capability ,scope-b))))
    (should-not (magent-runtime-active-project-scope))))

(ert-deftest magent-test-runtime-prepare-command-context-initializes-and-activates-scope ()
  "Test command-context preparation initializes static state once and activates scope."
  (require 'magent-runtime)
  (let* ((project-root (file-truename
                        (directory-file-name
                         (make-temp-file "magent-project-" t))))
         (default-directory project-root)
         (magent--initialized nil)
         (events nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magent-project-root)
                   (lambda (&optional directory no-fallback)
                     (ignore directory no-fallback)
                     project-root))
                  ((symbol-function 'magent-audit-enable)
                   (lambda () (push 'audit events)))
                  ((symbol-function 'magent-agent-initialize-static)
                   (lambda () (push 'agents events)))
                  ((symbol-function 'magent-skills-initialize-static)
                   (lambda () (push 'skills events)))
                  ((symbol-function 'magent-capability-initialize-static)
                   (lambda () (push 'capabilities events)))
                  ((symbol-function 'magent-runtime-activate-scope)
                   (lambda (scope &optional _force)
                     (push (list 'scope scope) events)
                     scope))
                  ((symbol-function 'magent-log) #'ignore))
          (magent-runtime-prepare-command-context)
          (magent-runtime-prepare-command-context))
      (delete-directory project-root t))
    (should (equal (nreverse events)
                   `(audit
                     agents
                     skills
                     capabilities
                     (scope ,project-root)
                     (scope ,project-root))))))

(ert-deftest magent-test-ui-scope-switch-snapshots-outgoing-session ()
  "Test switching project scopes snapshots the outgoing UI buffer."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (project-a (make-temp-file "magent-project-a-" t))
         (project-b (make-temp-file "magent-project-b-" t))
         (buffer-a nil)
         (buffer-b nil)
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
          (setq buffer-a (magent-ui-get-buffer))
          (with-current-buffer buffer-a
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "project-a snapshot")))
          (let ((session-a (magent-session-get)))
            (with-temp-buffer
              (setq default-directory (file-name-as-directory project-b))
              (magent-ui--activate-context-session))
            (setq buffer-b (magent-ui-get-buffer))
            (should (equal (magent-session-buffer-content session-a)
                           "project-a snapshot"))
            (should (equal (magent-session-current-scope)
                           (file-truename (directory-file-name project-b))))
            (should-not (eq buffer-a buffer-b))
            (should-not (equal (with-current-buffer buffer-b (buffer-string))
                               "project-a snapshot"))))
      (when (buffer-live-p buffer-a)
        (kill-buffer buffer-a))
      (when (buffer-live-p buffer-b)
        (kill-buffer buffer-b))
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest magent-test-ui-uses-distinct-buffers-per-scope ()
  "Test global and project scopes resolve to distinct Magent buffers."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-test*")
         (parent-a (make-temp-file "magent-parent-a-" t))
         (parent-b (make-temp-file "magent-parent-b-" t))
         (project-a (expand-file-name "app" parent-a))
         (project-b (expand-file-name "app" parent-b))
         (scope-a nil)
         (scope-b nil)
         (global-buffer nil)
         (buffer-a nil)
         (buffer-b nil))
    (unwind-protect
        (progn
          (make-directory project-a t)
          (make-directory project-b t)
          (setq scope-a (file-truename (directory-file-name project-a))
                scope-b (file-truename (directory-file-name project-b))
                global-buffer (magent-ui-get-buffer 'global)
                buffer-a (magent-ui-get-buffer scope-a)
                buffer-b (magent-ui-get-buffer scope-b))
          (should (equal (buffer-name global-buffer) "*magent-test:global*"))
          (should (equal (buffer-name buffer-a) "*magent-test:app*"))
          (should-not (eq buffer-a buffer-b))
          (should (string-match-p "\\`\\*magent-test:app#" (buffer-name buffer-b)))
          (with-current-buffer buffer-a
            (should (equal magent-ui--buffer-scope scope-a)))
          (with-current-buffer buffer-b
            (should (equal magent-ui--buffer-scope scope-b))))
      (when (buffer-live-p global-buffer)
        (kill-buffer global-buffer))
      (when (buffer-live-p buffer-a)
        (kill-buffer buffer-a))
      (when (buffer-live-p buffer-b)
        (kill-buffer buffer-b))
      (delete-directory parent-a t)
      (delete-directory parent-b t))))

(ert-deftest magent-test-ui-context-scope-prefers-buffer-local-scope ()
  "Test Magent buffers keep their own scope even when another session is active."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-scope*")
         (project-a (make-temp-file "magent-project-a-" t))
         (project-b (make-temp-file "magent-project-b-" t))
         (scope-a (file-truename (directory-file-name project-a)))
         (scope-b (file-truename (directory-file-name project-b)))
         (buffer-a nil))
    (unwind-protect
        (progn
          (setq buffer-a (magent-ui-get-buffer scope-a))
          (magent-session-activate scope-b)
          (with-current-buffer buffer-a
            (should (equal (magent-ui--context-scope) scope-a))))
      (when (buffer-live-p buffer-a)
        (kill-buffer buffer-a))
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest magent-test-input-submit-reactivates-buffer-scope ()
  "Test submitting from an older Magent buffer reactivates that buffer's scope."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-submit*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (project-a (make-temp-file "magent-project-a-" t))
         (project-b (make-temp-file "magent-project-b-" t))
         (scope-a (file-truename (directory-file-name project-a)))
         (scope-b (file-truename (directory-file-name project-b)))
         (buffer-a nil)
         (captured nil))
    (unwind-protect
        (progn
          (magent-session-activate scope-a)
          (setq buffer-a (magent-ui-get-buffer scope-a))
          (with-current-buffer buffer-a
            (magent-ui--insert-input-prompt scope-a)
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "hello from project a")))
          (magent-session-activate scope-b)
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent
                                            (magent-session-current-scope))))))
            (with-current-buffer buffer-a
              (magent-input-submit)))
          (should (equal (car captured) "hello from project a"))
          (should (eq (nth 1 captured) 'buffer-input))
          (should (equal (nth 5 captured) scope-a))
          (should (equal (magent-session-current-scope) scope-a)))
      (when (buffer-live-p buffer-a)
        (kill-buffer buffer-a))
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest magent-test-input-submit-restores-evil-normal-state ()
  "Test successful input submission returns Evil to normal state."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-evil-submit*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (buffer nil)
         (captured nil)
         (normal-state-called nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq buffer (magent-ui-get-buffer 'global))
          (with-current-buffer buffer
            (magent-ui--insert-input-prompt 'global)
            (setq-local evil-local-mode t)
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "return to normal")))
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent))))
                    ((symbol-function 'evil-normal-state)
                     (lambda ()
                       (setq normal-state-called t))))
            (with-current-buffer buffer
              (magent-input-submit)))
          (should normal-state-called)
          (should (equal (car captured) "return to normal"))
          (should (eq (nth 1 captured) 'buffer-input)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-evil-c-g-interrupts-only-in-normal-state ()
  "Test Evil C-g only interrupts in normal state."
  (skip-unless (require 'evil nil t))
  (dolist (case '((normal . magent-interrupt)
                  (insert . evil-normal-state)
                  (replace . evil-normal-state)
                  (visual . keyboard-quit)
                  (emacs . keyboard-quit)))
    (with-temp-buffer
      (insert "x")
      (magent-output-mode)
      (evil-local-mode 1)
      (pcase (car case)
        ('normal (evil-normal-state))
        ('insert (evil-insert-state))
        ('replace (evil-replace-state))
        ('visual
         (set-mark (point-min))
         (goto-char (point-max))
         (evil-visual-state))
        ('emacs (evil-emacs-state)))
      (should (eq evil-state (car case)))
      (should (eq (key-binding (kbd "C-g")) (cdr case)))
      (unless (eq (car case) 'normal)
        (should-not (eq (key-binding (kbd "C-g")) 'magent-interrupt))))))

(ert-deftest magent-test-config-reload-preserves-ui-logger ()
  "Test reloading config does not clobber the UI log implementation."
  (require 'magent-ui)
  (let* ((config-file (expand-file-name "magent-config.el"
                                        magent-test--root-directory))
         (buffer (magent-ui-get-log-buffer))
         (before-file (symbol-file 'magent-log 'defun))
         (before-fn (symbol-function 'magent-log)))
    (unwind-protect
        (progn
          (should (string-match-p "magent-ui\\.elc?$" (or before-file "")))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (load config-file nil t)
          (should (eq (symbol-function 'magent-log) before-fn))
          (should (string-match-p "magent-ui\\.elc?$"
                                  (or (symbol-file 'magent-log 'defun) "")))
          (magent-log "reload-safe %s" 1)
          (with-current-buffer buffer
            (should (string-match-p "reload-safe 1" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-ui-log-level-info-hides-debug ()
  "Test INFO log level suppresses DEBUG messages."
  (require 'magent-ui)
  (let ((magent-enable-logging t)
        (magent-log-level 'info)
        (buffer (magent-ui-get-log-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (magent-log "DEBUG hidden")
          (magent-log "INFO shown")
          (magent-log "PERM shown")
          (magent-log "plain shown")
          (with-current-buffer buffer
            (let ((contents (buffer-string)))
              (should-not (string-match-p "DEBUG hidden" contents))
              (should (string-match-p "INFO shown" contents))
              (should (string-match-p "PERM shown" contents))
              (should (string-match-p "plain shown" contents)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-ui-log-level-warn-hides-info-and-perm ()
  "Test WARN log level keeps only WARN and ERROR messages."
  (require 'magent-ui)
  (let ((magent-enable-logging t)
        (magent-log-level 'warn)
        (buffer (magent-ui-get-log-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (magent-log "INFO hidden")
          (magent-log "PERM hidden")
          (magent-log "WARN shown")
          (magent-log "ERROR shown")
          (with-current-buffer buffer
            (let ((contents (buffer-string)))
              (should-not (string-match-p "INFO hidden" contents))
              (should-not (string-match-p "PERM hidden" contents))
              (should (string-match-p "WARN shown" contents))
              (should (string-match-p "ERROR shown" contents)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-ui-log-level-disabled-suppresses-output ()
  "Test disabled logging suppresses all log output."
  (require 'magent-ui)
  (let ((magent-enable-logging nil)
        (magent-log-level 'debug)
        (buffer (magent-ui-get-log-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (magent-log "ERROR hidden")
          (with-current-buffer buffer
            (should (equal (buffer-string) ""))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
          (let ((magent-ui--processing t))
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

(ert-deftest magent-test-ui-render-history-rehydrates-saved-headings ()
  "Test restoring a saved snapshot rebuilds heading overlays and a fresh prompt."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (buffer (magent-ui-get-buffer))
         (session (magent-session-create :id "rehydrate")))
    (unwind-protect
        (progn
          (magent-session-install 'global session)
          (magent-session-add-message session 'user "hello")
          (magent-session-add-message session 'assistant "world")
          (setf (magent-session-buffer-content session)
                (with-current-buffer buffer
                  (let ((inhibit-read-only t))
                    (erase-buffer))
                  (magent-ui-insert-user-message "hello")
                  (magent-ui-insert-assistant-message "world")
                  (prog1 (buffer-substring-no-properties (point-min) (point-max))
                    (let ((inhibit-read-only t))
                      (erase-buffer)))))
          (magent-ui-render-history t)
          (should (= (magent-test--count-heading-lines buffer magent-user-prompt) 2))
          (should (= (magent-test--count-heading-lines buffer magent-assistant-prompt) 1))
          (should (= (magent-test--count-overlays-with-face buffer 'magent-user-header) 2))
          (should (= (magent-test--count-overlays-with-face buffer 'magent-assistant-header) 1))
          (with-current-buffer buffer
            (should magent-ui--input-marker)
            (should (= (marker-position magent-ui--input-marker) (point-max)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-ui-revert-buffer-preserves-draft-without-duplicate-prompt ()
  "Test `revert-buffer' restores a single editable prompt with the draft intact."
  (require 'magent-ui)
  (let* ((buffer (magent-ui-get-buffer))
         (session (magent-session-create :id "revert")))
    (unwind-protect
        (progn
          (setq magent--current-session session)
          (magent-session-add-message session 'user "hello")
          (magent-session-add-message session 'assistant "world")
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer))
            (magent-ui-insert-user-message "hello")
            (magent-ui-insert-assistant-message "world")
            (magent-ui--insert-input-prompt)
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "draft")))
          (magent-ui--revert-buffer nil nil)
          (should (= (magent-test--count-heading-lines buffer magent-user-prompt) 2))
          (should (= (magent-test--count-heading-lines buffer magent-assistant-prompt) 1))
          (should (= (magent-test--count-overlays-with-face buffer 'magent-user-header) 2))
          (should (= (magent-test--count-overlays-with-face buffer 'magent-assistant-header) 1))
          (with-current-buffer buffer
            (should magent-ui--input-marker)
            (should (string= (buffer-substring-no-properties
                              (marker-position magent-ui--input-marker)
                              (point-max))
                             "draft"))
            (should-not (get-text-property (marker-position magent-ui--input-marker)
                                           'read-only))
            (should (get-text-property (1- (marker-position magent-ui--input-marker))
                                       'read-only))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-resume-session-restores-loaded-buffer-content ()
  "Test resuming a session does not overwrite the loaded snapshot."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (buffer (magent-ui-get-buffer))
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
                     (lambda (scope session)
                       (puthash scope session magent-session--scoped-sessions)
                       (setq magent-session--current-scope scope
                             magent--current-session session)
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

(ert-deftest magent-test-resume-session-restores-loaded-prompt-state ()
  "Test resuming a session with a saved prompt restores one editable draft."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (buffer (magent-ui-get-buffer))
         (loaded-session (magent-session-create :id "loaded"))
         (current-session (magent-session-create :id "current"
                                                 :buffer-content "stale snapshot")))
    (unwind-protect
        (progn
          (magent-session-add-message loaded-session 'user "hello")
          (magent-session-add-message loaded-session 'assistant "world")
          (setf (magent-session-buffer-content loaded-session)
                (with-current-buffer buffer
                  (let ((inhibit-read-only t))
                    (erase-buffer))
                  (magent-ui-insert-user-message "hello")
                  (magent-ui-insert-assistant-message "world")
                  (magent-ui--insert-input-prompt)
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    (insert "draft"))
                  (prog1 (buffer-substring-no-properties (point-min) (point-max))
                    (let ((inhibit-read-only t))
                      (erase-buffer)))))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "stale buffer content")))
          (setq magent--current-session current-session)
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
                     (lambda (scope session)
                       (puthash scope session magent-session--scoped-sessions)
                       (setq magent-session--current-scope scope
                             magent--current-session session)
                       session))
                    ((symbol-function 'message) #'ignore))
            (magent-resume-session))
          (should (= (magent-test--count-heading-lines buffer magent-user-prompt) 2))
          (should (= (magent-test--count-heading-lines buffer magent-assistant-prompt) 1))
          (should (= (magent-test--count-overlays-with-face buffer 'magent-user-header) 2))
          (should (= (magent-test--count-overlays-with-face buffer 'magent-assistant-header) 1))
          (with-current-buffer buffer
            (should magent-ui--input-marker)
            (should (string= (buffer-substring-no-properties
                              (marker-position magent-ui--input-marker)
                              (point-max))
                             "draft"))
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

(ert-deftest magent-test-resume-session-displays-target-project-buffer ()
  "Test resuming another project displays and restores that project's buffer."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-resume*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (project-a (make-temp-file "magent-project-a-" t))
         (project-b (make-temp-file "magent-project-b-" t))
         (scope-a (file-truename (directory-file-name project-a)))
         (scope-b (file-truename (directory-file-name project-b)))
         (current-session (magent-session-create :id "current"))
         (loaded-session (magent-session-create
                          :id "loaded"
                          :buffer-content "* [ASSISTANT]\nLoaded project buffer\n"))
         (buffer-a nil)
         (buffer-b nil)
         (displayed nil))
    (unwind-protect
        (progn
          (magent-session-install scope-a current-session)
          (setq buffer-a (magent-ui-get-buffer scope-a))
          (with-current-buffer buffer-a
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "* [USER]\nProject A draft\n")))
          (cl-letf (((symbol-function 'magent-session-list-files)
                     (lambda () '("/tmp/project-b-session.json")))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _args) "project-b session"))
                    ((symbol-function 'magent-session--format-file)
                     (lambda (_filepath) "project-b session"))
                    ((symbol-function 'magent-session-read-file)
                     (lambda (_filepath)
                       (list :scope scope-b :session loaded-session :id "loaded")))
                    ((symbol-function 'display-buffer)
                     (lambda (buffer &rest _args)
                       (setq displayed buffer)
                       buffer))
                    ((symbol-function 'message) #'ignore))
            (magent-resume-session))
          (setq buffer-b (magent-ui-get-buffer scope-b))
          (should (eq displayed buffer-b))
          (with-current-buffer buffer-b
            (should (string-prefix-p "* [ASSISTANT]\nLoaded project buffer\n"
                                     (buffer-string))))
          (with-current-buffer buffer-a
            (should (string-prefix-p "* [USER]\nProject A draft\n"
                                     (buffer-string)))))
      (when (buffer-live-p buffer-a)
        (kill-buffer buffer-a))
      (when (buffer-live-p buffer-b)
        (kill-buffer buffer-b))
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest magent-test-ui-revert-buffer-keeps-buffer-local-scope ()
  "Test reverting an inactive Magent buffer uses that buffer's scope."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-revert*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (project-a (make-temp-file "magent-project-a-" t))
         (project-b (make-temp-file "magent-project-b-" t))
         (scope-a (file-truename (directory-file-name project-a)))
         (scope-b (file-truename (directory-file-name project-b)))
         (session-a (magent-session-create :id "a"))
         (session-b (magent-session-create
                     :id "b"
                     :buffer-content "* [ASSISTANT]\nProject B\n"))
         (buffer-a nil))
    (unwind-protect
        (progn
          (magent-session-install scope-a session-a)
          (setq buffer-a (magent-ui-get-buffer scope-a))
          (with-current-buffer buffer-a
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "* [ASSISTANT]\nProject A\n")))
          (magent-session-install scope-b session-b)
          (with-current-buffer buffer-a
            (magent-ui--revert-buffer nil nil))
          (with-current-buffer buffer-a
            (should (string-prefix-p "* [ASSISTANT]\nProject A\n"
                                     (buffer-string))))
          (should (string-prefix-p "* [ASSISTANT]\nProject A\n"
                                   (magent-session-buffer-content session-a))))
      (when (buffer-live-p buffer-a)
        (kill-buffer buffer-a))
      (delete-directory project-a t)
      (delete-directory project-b t))))

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
                          (eq (plist-get event :type) 'llm-request-start))
                        captured))
    (should (cl-find-if (lambda (event)
                          (eq (plist-get event :type) 'llm-request-end))
                        captured))
    (should (cl-find-if (lambda (event)
                          (and (eq (plist-get event :type) 'turn-end)
                               (eq (plist-get event :status) 'completed)))
                        captured))))

(ert-deftest magent-test-agent-process-resolves-capability-skills ()
  "Test `magent-agent-process' merges capability-derived skills."
  (require 'magent-capability)
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (magent-capability--registry nil)
        (magent-enable-capabilities t)
        (captured-skill-names nil))
    (magent-capability-register
     (magent-capability-create
      :name "org-structure"
      :description "Org structure edits"
      :skills '("auto-skill")
      :modes '(org-mode)
      :features '(org)
      :prompt-keywords '("heading")
      :disclosure 'active))
    (cl-letf (((symbol-function 'magent-skills-get-instruction-prompts)
               (lambda (skill-names)
                 (setq captured-skill-names skill-names)
                 '("## Skill: captured\n\nDo things.")))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (let ((callback (plist-get kwargs :callback)))
                   (funcall callback "Hello" nil)
                   (funcall callback t (list :content "Hello")))))
              ((symbol-function 'magent-ui-start-streaming) #'ignore)
              ((symbol-function 'magent-ui-insert-streaming) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
       "Please reorganize this heading"
       #'ignore
       nil
       '("manual-skill")
       nil
       '(:major-mode org-mode :features (org))))
    (should (equal captured-skill-names '("manual-skill" "auto-skill")))))

(ert-deftest magent-test-agent-process-dedupes-explicit-and-capability-skills ()
  "Test diagnosis-style explicit skills and capability skills are deduplicated."
  (require 'magent-capability)
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (captured-skill-names nil))
    (cl-letf (((symbol-function 'magent-skills-get-instruction-prompts)
               (lambda (skill-names)
                 (setq captured-skill-names skill-names)
                 '("## Skill: captured\n\nDo things.")))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (let ((callback (plist-get kwargs :callback)))
                   (funcall callback "Hello" nil)
                   (funcall callback t (list :content "Hello")))))
              ((symbol-function 'magent-ui-start-streaming) #'ignore)
              ((symbol-function 'magent-ui-insert-streaming) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
       "Diagnose why this hook is not running"
       #'ignore
       nil
       '("systematic-debugging")
       nil
       '(:major-mode emacs-lisp-mode :major-mode-family (emacs-lisp-mode prog-mode))
       (magent-capability-resolution-create
        :skill-names '("systematic-debugging" "emacs-runtime-inspection" "systematic-debugging"))))
    (should (equal captured-skill-names
                   '("systematic-debugging" "emacs-runtime-inspection")))))

(ert-deftest magent-test-agent-process-emits-capability-resolution-event ()
  "Test `magent-agent-process' emits capability resolution metadata."
  (require 'magent-capability)
  (require 'magent-events)
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (magent-capability--registry nil)
        (magent-enable-capabilities t)
        (captured nil))
    (magent-capability-register
     (magent-capability-create
      :name "org-structure"
      :description "Org structure edits"
      :skills '("auto-skill")
      :modes '(org-mode)
      :features '(org)
      :prompt-keywords '("heading")
      :disclosure 'active))
    (cl-letf (((symbol-function 'magent-skills-get-instruction-prompts)
               (lambda (_skill-names)
                 '("## Skill: captured\n\nDo things.")))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (let ((callback (plist-get kwargs :callback)))
                   (funcall callback "Hello" nil)
                   (funcall callback t (list :content "Hello")))))
              ((symbol-function 'magent-ui-start-streaming) #'ignore)
              ((symbol-function 'magent-ui-insert-streaming) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (unwind-protect
          (progn
            (magent-events-add-sink (lambda (event) (push event captured)))
            (magent-agent-process
             "Please reorganize this heading"
             #'ignore
             nil nil nil
             '(:major-mode org-mode :features (org))))
        (magent-events-clear-sinks)))
    (let* ((event (cl-find-if (lambda (item)
                                (eq (plist-get item :type)
                                    'capability-resolution))
                              captured))
           (resolution (plist-get event :resolution)))
      (should event)
      (should (equal (plist-get resolution :active-capabilities)
                     '("org-structure")))
      (should (equal (plist-get resolution :skill-names)
                     '("auto-skill"))))))

(ert-deftest magent-test-diagnose-emacs-dispatches-structured-prompt ()
  "Test `magent-diagnose-emacs' dispatches a structured diagnosis request."
  (require 'magent-ui)
  (let ((agent (magent-agent-info-create :name "build" :mode 'primary))
        captured)
    (cl-letf (((symbol-function 'magent-ui--capture-buffer-context)
               (lambda ()
                 "[Context: buffer=\"*scratch*\" mode=lisp-interaction-mode line=1]"))
              ((symbol-function 'magent-agent-registry-get)
               (lambda (name)
                 (and (equal name "build") agent)))
              ((symbol-function 'magent-agent-registry-get-default)
               (lambda ()
                 agent))
              ((symbol-function 'magent-ui-dispatch-prompt)
               (lambda (prompt &optional source display skills activate-context agent-info)
                 (setq captured (list prompt source display skills activate-context agent-info)))))
      (magent-diagnose-emacs))
    (should (string-match-p "Diagnose problems in the current Emacs session"
                            (nth 0 captured)))
    (should (string-match-p "\\*Messages\\*" (nth 0 captured)))
    (should (string-match-p "\\[Context: buffer=\"\\*scratch\\*\"" (nth 0 captured)))
    (should (equal (nth 1 captured) 'diagnose-emacs))
    (should (equal (nth 2 captured) "Diagnose the current Emacs session."))
    (should (equal (nth 3 captured) '("systematic-debugging")))
    (should (eq (nth 4 captured) t))
    (should (eq (nth 5 captured) agent))))

(ert-deftest magent-test-doctor-dispatches-magent-self-check-prompt ()
  "Test `magent-doctor' dispatches a Magent-specific diagnosis request."
  (require 'magent-ui)
  (let ((agent (magent-agent-info-create :name "build" :mode 'primary))
        captured)
    (cl-letf (((symbol-function 'magent-ui--capture-buffer-context)
               (lambda ()
                 "[Context: buffer=\"*magent*\" mode=magent-output-mode line=1]"))
              ((symbol-function 'magent-agent-registry-get)
               (lambda (name)
                 (and (equal name "build") agent)))
              ((symbol-function 'magent-agent-registry-get-default)
               (lambda ()
                 agent))
              ((symbol-function 'magent-ui-dispatch-prompt)
               (lambda (prompt &optional source display skills activate-context agent-info)
                 (setq captured (list prompt source display skills activate-context agent-info)))))
      (magent-doctor))
    (should (string-match-p "Run a Magent self-check and diagnose Magent-related problems"
                            (nth 0 captured)))
    (should (string-match-p "\\*magent-log\\*" (nth 0 captured)))
    (should (string-match-p "magent-mode" (nth 0 captured)))
    (should (string-match-p "\\[Context: buffer=\"\\*magent\\*\"" (nth 0 captured)))
    (should (equal (nth 1 captured) 'doctor))
    (should (equal (nth 2 captured) "Run Magent doctor."))
    (should (equal (nth 3 captured) '("systematic-debugging")))
    (should (eq (nth 4 captured) t))
    (should (eq (nth 5 captured) agent))))

(ert-deftest magent-test-ui-dispatch-prompt-forwards-agent-override ()
  "Test `magent-ui-dispatch-prompt' forwards per-request agent overrides."
  (require 'magent-ui)
  (let ((agent (magent-agent-info-create :name "build" :mode 'primary))
        captured)
    (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
              ((symbol-function 'magent-ui--activate-context-session) (lambda () 'session))
              ((symbol-function 'magent-capability-capture-context)
               (lambda () '(:buffer-name "*scratch*" :major-mode emacs-lisp-mode)))
              ((symbol-function 'magent-capability-resolve-for-turn)
               (lambda (_prompt _request-context _skills)
                 'resolution))
              ((symbol-function 'magent-ui--enqueue)
               (lambda (prompt source &optional display skills agent-info request-context capability-resolution)
                 (setq captured (list prompt source display skills agent-info request-context capability-resolution)))))
      (magent-ui-dispatch-prompt "diag" 'diagnose "display" '("systematic-debugging") t agent))
    (should (equal (nth 0 captured) "diag"))
    (should (equal (nth 1 captured) 'diagnose))
    (should (equal (nth 2 captured) "display"))
    (should (equal (nth 3 captured) '("systematic-debugging")))
    (should (eq (nth 4 captured) agent))
    (should (equal (nth 5 captured)
                   '(:buffer-name "*scratch*" :major-mode emacs-lisp-mode)))
    (should (eq (nth 6 captured) 'resolution))))

(ert-deftest magent-test-ui-run-item-shows-capability-summary ()
  "Test `magent-ui--run-item' renders a capability summary for the turn."
  (require 'magent-ui)
  (let* ((buffer (magent-ui-get-buffer))
         (resolution (magent-capability-resolution-create
                      :active-capabilities
                      (list (magent-capability-match-create
                             :capability (magent-capability-create :name "org-structure")
                             :status 'active))
                      :suggested-capabilities
                      (list (magent-capability-match-create
                             :capability (magent-capability-create :name "git-workflow")
                             :status 'suggested))))
         (item (magent-ui--request-create
                :prompt "hello"
                :source 'prompt
                :capability-resolution resolution)))
    (magent-ui-clear-buffer)
    (cl-letf (((symbol-function 'magent-ui-display-buffer) #'ignore)
              ((symbol-function 'spinner-start) #'ignore)
              ((symbol-function 'magent-agent-process)
               (lambda (&rest _args) 'fsm)))
      (magent-ui--run-item item))
    (with-current-buffer buffer
      (should (string-match-p "Capability resolver: Auto capabilities: org-structure | Suggested: git-workflow"
                              (buffer-string))))))

(ert-deftest magent-test-ui-capability-summary-folds-quote-block ()
  "Test capability summaries default-fold their quote block in the UI."
  (require 'magent-ui)
  (let ((fold-call nil))
    (magent-ui-clear-buffer)
    (cl-letf (((symbol-function 'magent-ui--fold-block-at)
               (lambda (pos block-re)
                 (setq fold-call (list pos block-re)))))
      (magent-ui-insert-capability-summary "Auto capabilities: org-structure"))
    (should fold-call)
    (should (equal (cadr fold-call) "#\\+begin_quote"))
    (with-current-buffer (magent-ui-get-buffer)
      (should (eq (car fold-call) (point-min)))
      (should (string-match-p "#\\+begin_quote\nCapability resolver: Auto capabilities: org-structure\n#\\+end_quote\n"
                              (buffer-string))))))

(ert-deftest magent-test-ui-reasoning-block-stays-expanded ()
  "Test reasoning blocks stay expanded in the UI."
  (require 'magent-ui)
  (let ((fold-call nil)
        (magent-ui-wrap-reasoning-in-think-block t))
    (magent-ui-clear-buffer)
    (cl-letf (((symbol-function 'magent-ui--fold-block-at)
               (lambda (pos block-re)
                 (setq fold-call (list pos block-re)))))
      (magent-ui-insert-reasoning-start)
      (magent-ui-insert-reasoning-text "alpha")
      (magent-ui-insert-reasoning-end))
    (should-not fold-call)
    (with-current-buffer (magent-ui-get-buffer)
      (should (equal (buffer-string)
                     "#+begin_think\nalpha\n#+end_think\n"))
      (should (null magent-ui--reasoning-start))
      (should magent-ui--streaming-has-text))))

(ert-deftest magent-test-ui-reasoning-can-skip-think-block-wrapper ()
  "Test reasoning can be shown without `#+begin_think' wrappers."
  (require 'magent-ui)
  (let ((magent-ui-wrap-reasoning-in-think-block nil))
    (magent-ui-clear-buffer)
    (magent-ui-insert-reasoning-start)
    (magent-ui-insert-reasoning-text "alpha")
    (magent-ui-insert-reasoning-end)
    (with-current-buffer (magent-ui-get-buffer)
      (should (equal (buffer-string) "alpha\n"))
      (should (null magent-ui--reasoning-start))
      (should magent-ui--streaming-has-text))))

(ert-deftest magent-test-ui-tool-call-renders-symbol-args ()
  "Test tool-call rendering tolerates symbol values in structured args."
  (require 'magent-ui)
  (magent-ui-clear-buffer)
  (magent-ui-insert-tool-call
   "emacs_eval"
   '(:tool emacs_eval :values [emacs_eval nil]))
  (with-current-buffer (magent-ui-get-buffer)
    (let ((text (buffer-string)))
      (should (string-match-p "#\\+begin_tool emacs_eval" text))
      (should (string-match-p "\"tool\":\"emacs_eval\"" text))
      (should (string-match-p "\"values\":\\[\"emacs_eval\",null\\]" text)))))

(ert-deftest magent-test-mode-map-binds-diagnose-emacs ()
  "Test `magent-mode-map' binds the Emacs diagnosis command."
  (require 'magent)
  (should (eq (lookup-key magent-mode-map (kbd "C-c m d"))
              'magent-diagnose-emacs)))

(ert-deftest magent-test-mode-map-binds-magent-doctor ()
  "Test `magent-mode-map' binds the Magent doctor command."
  (require 'magent)
  (should (eq (lookup-key magent-mode-map (kbd "C-c m D"))
              'magent-doctor)))

(ert-deftest magent-test-mode-map-binds-capability-context-list ()
  "Test `magent-mode-map' binds current-context capability listing."
  (require 'magent)
  (should (eq (lookup-key magent-mode-map (kbd "C-c m x"))
              'magent-list-capabilities-for-current-context)))

(ert-deftest magent-test-list-agents-loads-project-scope-before-first-prompt ()
  "Test listing agents loads project-local agents without a prior prompt."
  (require 'magent)
  (let* ((project-root (file-truename
                        (directory-file-name
                         (make-temp-file "magent-project-" t))))
         (agent-dir (expand-file-name ".magent/agent" project-root))
         (agent-file (expand-file-name "project-helper.md" agent-dir))
         (magent-load-custom-agents t)
         (magent--initialized nil)
         (magent-runtime--active-project-scope nil)
         (magent--current-session nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent-agent-registry--agents (make-hash-table :test 'equal))
         (magent-agent-registry--initialized nil)
         (magent-skills--registry nil)
         (magent-capability--registry nil))
    (unwind-protect
        (progn
          (make-directory agent-dir t)
          (with-temp-file agent-file
            (insert "---\n"
                    "description: Project helper\n"
                    "mode: primary\n"
                    "---\n"
                    "You are the project helper.\n"))
          (with-temp-buffer
            (setq default-directory project-root)
            (cl-letf (((symbol-function 'magent-project-root)
                       (lambda (&optional directory no-fallback)
                         (ignore directory no-fallback)
                         project-root))
                      ((symbol-function 'magent-log) #'ignore)
                      ((symbol-function 'display-buffer) #'ignore))
              (magent-list-agents)))
          (with-current-buffer "*Magent Agents*"
            (should (string-match-p "project-helper" (buffer-string)))))
      (when (get-buffer "*Magent Agents*")
        (kill-buffer "*Magent Agents*"))
      (delete-directory project-root t))))

(ert-deftest magent-test-list-skills-loads-project-scope-before-first-prompt ()
  "Test listing skills loads project-local skills without a prior prompt."
  (require 'magent)
  (let* ((project-root (file-truename
                        (directory-file-name
                         (make-temp-file "magent-project-" t))))
         (skill-dir (expand-file-name ".magent/skills/project-skill" project-root))
         (skill-file (expand-file-name "SKILL.md" skill-dir))
         (magent--initialized nil)
         (magent-runtime--active-project-scope nil)
         (magent--current-session nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent-agent-registry--agents (make-hash-table :test 'equal))
         (magent-agent-registry--initialized nil)
         (magent-skills--registry nil)
         (magent-capability--registry nil))
    (unwind-protect
        (progn
          (make-directory skill-dir t)
          (with-temp-file skill-file
            (insert "---\n"
                    "name: project-skill\n"
                    "description: Project-only helper\n"
                    "type: instruction\n"
                    "---\n"
                    "Use for project tasks.\n"))
          (with-temp-buffer
            (setq default-directory project-root)
            (cl-letf (((symbol-function 'magent-project-root)
                       (lambda (&optional directory no-fallback)
                         (ignore directory no-fallback)
                         project-root))
                      ((symbol-function 'magent-log) #'ignore)
                      ((symbol-function 'display-buffer) #'ignore))
              (magent-list-skills)))
          (with-current-buffer "*Magent Skills*"
            (should (string-match-p "project-skill" (buffer-string)))))
      (when (get-buffer "*Magent Skills*")
        (kill-buffer "*Magent Skills*"))
      (delete-directory project-root t))))

(ert-deftest magent-test-list-capabilities-loads-project-scope-before-first-prompt ()
  "Test current-context capability listing loads project-local capabilities."
  (require 'magent)
  (let* ((project-root (file-truename
                        (directory-file-name
                         (make-temp-file "magent-project-" t))))
         (cap-dir (expand-file-name ".magent/capabilities/project-capability" project-root))
         (cap-file (expand-file-name "CAPABILITY.md" cap-dir))
         (magent--initialized nil)
         (magent-runtime--active-project-scope nil)
         (magent--current-session nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent-agent-registry--agents (make-hash-table :test 'equal))
         (magent-agent-registry--initialized nil)
         (magent-skills--registry nil)
         (magent-capability--registry nil))
    (unwind-protect
        (progn
          (make-directory cap-dir t)
          (with-temp-file cap-file
            (insert "---\n"
                    "name: project-capability\n"
                    "description: Project-local capability\n"
                    "skills: project-skill\n"
                    "---\n"
                    "Use for project scope.\n"))
          (with-temp-buffer
            (setq default-directory project-root)
            (cl-letf (((symbol-function 'magent-project-root)
                       (lambda (&optional directory no-fallback)
                         (ignore directory no-fallback)
                         project-root))
                      ((symbol-function 'magent-log) #'ignore)
                      ((symbol-function 'display-buffer) #'ignore))
              (magent-list-capabilities-for-current-context)))
          (with-current-buffer "*Magent Capability Resolution*"
            (should (string-match-p "project-capability" (buffer-string)))))
      (when (get-buffer "*Magent Capability Resolution*")
        (kill-buffer "*Magent Capability Resolution*"))
      (delete-directory project-root t))))

(ert-deftest magent-test-describe-skill-completion-loads-project-scope ()
  "Test interactive skill completion sees project-local skills before any prompt."
  (require 'magent)
  (let* ((project-root (file-truename
                        (directory-file-name
                         (make-temp-file "magent-project-" t))))
         (skill-dir (expand-file-name ".magent/skills/project-skill" project-root))
         (skill-file (expand-file-name "SKILL.md" skill-dir))
         (magent--initialized nil)
         (magent-runtime--active-project-scope nil)
         (magent--current-session nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent-agent-registry--agents (make-hash-table :test 'equal))
         (magent-agent-registry--initialized nil)
         (magent-skills--registry nil)
         (magent-capability--registry nil))
    (unwind-protect
        (progn
          (make-directory skill-dir t)
          (with-temp-file skill-file
            (insert "---\n"
                    "name: project-skill\n"
                    "description: Project-only helper\n"
                    "type: instruction\n"
                    "---\n"
                    "Use for project tasks.\n"))
          (with-temp-buffer
            (setq default-directory project-root)
            (cl-letf (((symbol-function 'magent-project-root)
                       (lambda (&optional directory no-fallback)
                         (ignore directory no-fallback)
                         project-root))
                      ((symbol-function 'magent-log) #'ignore)
                      ((symbol-function 'display-buffer) #'ignore)
                      ((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (should (member "project-skill" collection))
                         "project-skill")))
              (call-interactively #'magent-describe-skill)))
          (with-current-buffer "*Magent Skill: project-skill*"
            (should (string-match-p "Project-only helper" (buffer-string)))))
      (when (get-buffer "*Magent Skill: project-skill*")
        (kill-buffer "*Magent Skill: project-skill*"))
      (delete-directory project-root t))))

(ert-deftest magent-test-mode-map-binds-capability-last-resolution ()
  "Test `magent-mode-map' binds last capability resolution explanation."
  (require 'magent)
  (should (eq (lookup-key magent-mode-map (kbd "C-c m e"))
              'magent-explain-last-capability-resolution)))

(ert-deftest magent-test-mode-map-binds-capability-local-toggle ()
  "Test `magent-mode-map' binds local capability toggling."
  (require 'magent)
  (should (eq (lookup-key magent-mode-map (kbd "C-c m k"))
              'magent-toggle-capability-locally)))

;; ──────────────────────────────────────────────────────────────────────
;;; Codex-like runtime skeleton tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-turn-runtime-queues-submissions ()
  "Test turn runtime queues submissions and starts the next one on finish."
  (require 'magent-turn)
  (let ((magent-turn--active nil)
        (magent-turn--queue nil)
        (magent-turn--current-fsm nil)
        started)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args)
                 'timer)))
      (magent-turn-submit
       (magent-protocol-user-input-op "one")
       "one"
       (lambda (submission)
         (push (magent-turn-submission-payload submission) started)))
      (magent-turn-submit
       (magent-protocol-user-input-op "two")
       "two"
       (lambda (submission)
         (push (magent-turn-submission-payload submission) started)))
      (should (equal (nreverse started) '("one")))
      (should (magent-turn-processing-p))
      (should (magent-turn-pending-p))
      (magent-turn-finish 'completed)
      (should (equal (nreverse started) '("one" "two")))
      (should (magent-turn-processing-p))
      (should-not (magent-turn-pending-p)))))

(ert-deftest magent-test-session-records-structured-context-items ()
  "Test session messages also populate Codex-like context items."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "hello")
    (magent-session-add-message session 'assistant "world")
    (let ((items (magent-session-context-items session)))
      (should (= (length items) 2))
      (should (eq (magent-response-item-type (car items)) 'message))
      (should (eq (magent-response-item-role (car items)) 'user))
      (should (equal (magent-response-item-content (cadr items)) "world")))))

(ert-deftest magent-test-tool-orchestrator-denies-file-rule ()
  "Test tool orchestrator preserves Magent file-rule denial behavior."
  (require 'magent-tool-orchestrator)
  (require 'gptel)
  (let* ((permission (magent-permission-defaults))
         (tool (gptel-make-tool
                :name "write_file"
                :description "write"
                :args (list '(:name "path" :type string)
                            '(:name "content" :type string))
                :function (lambda (_path _content) "ok")
                :async nil))
         result ran)
    (magent-tool-orchestrator-handle-tool-calls
     (magent-tool-orchestrator-create
      :permission permission
      :run-tool-function
      (lambda (tool-spec cb arg-values)
        (setq ran t)
        (funcall cb (apply (gptel-tool-function tool-spec) arg-values)))
      :file-arg-index-function (lambda (_args-spec) 0)
      :args-to-plist-function (lambda (_args-spec arg-values) arg-values)
      :summarize-function (lambda (arg-values _args-spec) (car arg-values)))
     (list (list tool (list ".env" "SECRET=1")
                 (lambda (value) (setq result value)))))
    (should-not ran)
    (should (string-match-p "access denied" result))))

(provide 'magent-test)
;;; magent-test.el ends here
