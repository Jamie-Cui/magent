;;; magent-test.el --- Tests for Magent agent processing  -*- lexical-binding: t; -*-
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Tests for the canonical turn API and ledger-backed session handling.

;;; Code:

(require 'ert)
(require 'magent)
(require 'gptel-openai)

(defvar agent-shell-agent-configs)

(defun magent-test-tool-result (output &optional failed)
  "Return a structured tool result containing OUTPUT."
  (magent-tool-result-create
   :status (if failed 'failed 'completed)
   :success (not failed)
   :output output
   :error (and failed output)))

(defun magent-test-tool-output (result)
  "Return model-visible text from structured tool RESULT."
  (magent-tool-result-output-string result))

(defun magent-test--session-transcript (session)
  "Return SESSION's current ledger transcript projection."
  (magent-session-context-view session 'transcript))

(defsubst magent-test--transcript-role (entry)
  "Return transcript ENTRY's role."
  (cdr (assq 'role entry)))

(defsubst magent-test--transcript-content (entry)
  "Return transcript ENTRY's content."
  (cdr (assq 'content entry)))

(defun magent-test--record-session-entry (session role content)
  "Record a test transcript entry in SESSION's current ledger."
  (let* ((thread (magent-session-thread-ledger session))
         (turn (magent-thread-active-turn thread)))
    (pcase role
      ('user
       (when turn
         (magent-thread-complete-turn thread (magent-thread-turn-id turn)))
       (setq turn (magent-thread-create-turn thread content))
       (magent-thread-record-user-message-if-needed
        thread (magent-thread-turn-id turn) content nil (list :source 'test)))
      ('assistant
       (unless turn
         (error "Assistant test entry requires an active turn"))
       (magent-thread-record-message
        thread (magent-thread-turn-id turn) 'assistant content nil
        (list :source 'test))
       (magent-thread-complete-turn thread (magent-thread-turn-id turn)))
      ('tool
       (unless turn
         (error "Tool test entry requires an active turn"))
       (let* ((call-id (or (plist-get content :id)
                           (magent-protocol-generate-id "tool")))
              (value (plist-get content :result))
              (result (if (magent-tool-result-p value)
                          value
                        (magent-test-tool-result value))))
         (magent-thread-record-tool-result
          thread (magent-thread-turn-id turn) call-id
          (plist-get content :name) (plist-get content :args) result
          (list :source 'test))))
      (_ (error "Unsupported test transcript role: %S" role)))
    (when (> (length (magent-test--session-transcript session))
             (+ (magent-session-max-history session) 10))
      (magent-session--trim-history session))
    session))

(defun magent-test--record-tool-entry (session id name args result)
  "Record a structured tool entry in SESSION's current ledger."
  (magent-test--record-session-entry
   session 'tool (list :id id :name name :args args :result result)))

(defun magent-test--session-with-transcript (id entries &optional metadata)
  "Return a current-format session ID containing transcript ENTRIES."
  (let ((session (magent-session-create :id id :metadata metadata)))
    (dolist (entry entries session)
      (magent-test--record-session-entry
       session (car entry) (cadr entry)))))

(defun magent-test--latest-tool-transcript (session)
  "Return the newest tool transcript entry in SESSION."
  (cl-find 'tool (reverse (magent-test--session-transcript session))
           :key #'magent-test--transcript-role))

(defun magent-test--loop-create-for-session (session prompt &rest args)
  "Create an agent loop for a real current-format turn in SESSION."
  (magent-test--record-session-entry session 'user prompt)
  (let* ((thread (magent-session-thread-ledger session))
         (turn (magent-thread-active-turn thread)))
    (apply #'magent-agent-loop-create
           :session session :turn-id (magent-thread-turn-id turn) args)))

(defun magent-test--provider-context (session &optional current-turn-id)
  "Return SESSION's provider replay context."
  (magent-session-context-view session 'provider current-turn-id))

(defun magent-test--all-tool-names ()
  "Return every canonical Magent tool name for resolver tests."
  (mapcar (lambda (entry) (intern (plist-get entry :name)))
          magent-tools-catalog))

(defun magent-test--save-current-session ()
  "Save the explicitly bound current test session."
  (when magent--current-session
    (magent-session-save-for-session
     magent--current-session magent-session--current-scope)))

(defun magent-test--session-files (directory)
  "Return current session JSON files recursively under DIRECTORY."
  (directory-files-recursively directory "\\.json\\'"))

(defun magent-test--write-session-fixture (directory id scope title)
  "Write a valid current session ID for SCOPE under DIRECTORY."
  (let* ((magent-session-directory directory)
         (magent-session--current-scope scope)
         (session (magent-session-create
                   :id id :metadata `((title . ,title)))))
    (magent-test--record-session-entry session 'user title)
    (magent-session-save-for-session session scope)))

(defun magent-test--current-session-json-data (id &optional schema-version)
  "Return current-format session JSON data for ID and SCHEMA-VERSION."
  (let ((thread (magent-thread-create :id id :session-id id :scope 'global)))
    `((id . ,id)
      (schema-version . ,(or schema-version magent-session-schema-version))
      (scope . "global")
      (snapshot . ,(magent-thread-snapshot-to-alist thread))
      (journal . [])
      (agent-jobs . [])
      (approval-overrides . []))))

(defun magent-test--acp-client-for-runtime
    (runtime-session &optional notification-handlers context-buffer)
  "Return an ACP client explicitly bound to RUNTIME-SESSION."
  (let ((client `((:context-buffer . ,context-buffer)
                  (:notification-handlers . ,notification-handlers)
                  (:request-handlers . nil))))
    (magent-acp--bind-client-session client runtime-session)
    client))

(defun magent-test--run-turn
    (prompt &optional callback agent skills event-context context
            capability-resolution text-callback request-live-p request-context)
  "Exercise `magent-agent-run-turn' using compact test-call arguments."
  (let* ((session (or (and request-context
                            (magent-request-context-session request-context))
                       (magent-session-get)))
         (request-context
          (or request-context
              (magent-request-context-create
               :session session :ui-visibility 'none)))
         (existing-observer (magent-request-context-observer request-context)))
    (when event-context
      (setf (magent-request-context-event-context request-context) event-context))
    (when text-callback
      (setf (magent-request-context-observer request-context)
            (lambda (event)
              (when existing-observer (funcall existing-observer event))
              (when (eq (plist-get event :type) 'assistant-delta)
                (funcall text-callback (plist-get event :text))))))
    (magent-agent-run-turn
     :session session :prompt prompt :agent agent :skills skills
     :context context :request-context request-context
     :capability-resolution capability-resolution
     :on-complete callback :request-live-p request-live-p)))

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

(defun magent-test--make-git-repository (prefix)
  "Create and return a temporary Git repository named with PREFIX."
  (let ((directory (make-temp-file prefix t)))
    (unless (zerop (process-file "git" nil nil nil
                                 "-C" directory "init" "--quiet"))
      (error "Could not initialize test Git repository"))
    (with-temp-file (expand-file-name "README.md" directory)
      (insert "# Test repository\n"))
    (unless (and (zerop (process-file "git" nil nil nil
                                      "-C" directory "add" "README.md"))
                 (zerop (process-file
                         "git" nil nil nil
                         "-C" directory
                         "-c" "user.name=Magent Tests"
                         "-c" "user.email=magent@example.invalid"
                         "commit" "--quiet" "-m" "Initial commit")))
      (delete-directory directory t)
      (error "Could not commit test Git repository"))
    directory))

(defconst magent-test--root-directory
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root used by reload-oriented tests.")

(load (expand-file-name "test/magent-source-files.el"
                        magent-test--root-directory)
      nil t)

(ert-deftest magent-test-source-manifest-covers-production-elisp ()
  "Test every production Elisp module appears once in the source manifest."
  (let* ((manifest (magent-test-source-files magent-test--root-directory))
         (actual
          (cons
           "lisp/magent.el"
           (mapcar
            (lambda (file) (concat "lisp/" file))
            (delete
             "magent-autoloads.el"
             (directory-files
              (expand-file-name "lisp" magent-test--root-directory)
              nil "\\`magent-.*\\.el\\'")))))
         (sorted-manifest (sort (copy-sequence manifest) #'string<)))
    (should (= (length manifest)
               (length (delete-dups (copy-sequence manifest)))))
    (should (equal sorted-manifest (sort actual #'string<)))))

(ert-deftest magent-test-readme-vc-recipe-selects-lisp-directory ()
  "Test the documented package-vc recipe loads Magent from lisp/."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "README.org" magent-test--root-directory))
    (goto-char (point-min))
    (should
     (re-search-forward
      "^\\*\\* Installing from Git with ~use-package~$" nil t))
    (should (re-search-forward "^#\\+begin_src elisp$" nil t))
    (let ((recipe-start (point)))
      (should (re-search-forward "^#\\+end_src$" nil t))
      (let ((recipe (buffer-substring-no-properties
                     recipe-start (match-beginning 0))))
        (should (string-match-p "(use-package magent" recipe))
        (should (string-match-p ":vc" recipe))
        (should
         (string-match-p
          ":lisp-dir[[:blank:]]+\\\"lisp\\\"" recipe))))))

(ert-deftest magent-test-production-elisp-has-commentary-sections ()
  "Test every production Elisp module has a Commentary section."
  (dolist (relative-file
           (magent-test-source-files magent-test--root-directory))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name relative-file magent-test--root-directory))
      (should (re-search-forward "^;;; Commentary:$" nil t)))))

(ert-deftest magent-test-action-builtin-module-names-are-canonical ()
  "Doctor and Memory use their Action builtin module names exclusively."
  (dolist (entry '((magent-action-builtin-doctor
                    . "lisp/magent-action-builtin-doctor.el")
                   (magent-action-builtin-memory
                    . "lisp/magent-action-builtin-memory.el")))
    (should (featurep (car entry)))
    (should (file-exists-p
             (expand-file-name (cdr entry) magent-test--root-directory))))
  (dolist (feature '(magent-doctor magent-memory))
    (should-not (featurep feature)))
  (dolist (file '("lisp/magent-doctor.el" "lisp/magent-memory.el"))
    (should-not
     (file-exists-p (expand-file-name file magent-test--root-directory))))
  (should (fboundp 'magent-action-builtin-doctor-register))
  (should (fboundp 'magent-action-builtin-memory-register))
  (should-not (fboundp 'magent-doctor-register-action))
  (should-not (fboundp 'magent-memory-register-actions)))

(ert-deftest magent-test-production-elisp-declarations-are-valid ()
  "Test declarations in every production Elisp module resolve."
  (require 'check-declare)
  (dolist (relative-file
           (magent-test-source-files magent-test--root-directory))
    (should-not
     (check-declare-file
      (expand-file-name relative-file magent-test--root-directory)))))

(ert-deftest magent-test-generated-external-accessors-use-ext-declarations ()
  "Test generated external accessors use explicit ext declarations."
  (let ((doctor-file
         (expand-file-name "lisp/magent-action-builtin-doctor.el"
                           magent-test--root-directory)))
    (with-temp-buffer
      (insert-file-contents doctor-file)
      (goto-char (point-min))
      (should
       (re-search-forward
        (rx "(declare-function flymake-diagnostic-text "
            "\"ext:flymake\" t t)")
        nil t)))))

(ert-deftest magent-test-gptel-adapter-does-not-declare-private-state ()
  "Test the gptel adapter does not hide private variable API changes."
  (let ((adapter-file
         (expand-file-name "lisp/magent-llm-gptel.el"
                           magent-test--root-directory))
        declarations)
    (with-temp-buffer
      (insert-file-contents adapter-file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (pcase (read (current-buffer))
              (`(defvar ,(and variable (pred symbolp)) . ,_)
               (when (string-prefix-p "gptel--" (symbol-name variable))
                 (push variable declarations)))))
        (end-of-file nil)))
    (should-not declarations)))

(ert-deftest magent-test-melpazoid-recipe-packages-production-libraries ()
  "Test the MELPA recipe includes all production libraries and runtime data."
  (let ((workflow (expand-file-name ".github/workflows/melpazoid.yml"
                                    magent-test--root-directory))
        (expected-files '("lisp/magent*.el" "prompts" "skills"))
        recipe)
    (with-temp-buffer
      (insert-file-contents workflow)
      (should (re-search-forward "^[[:space:]]*RECIPE:[[:space:]]*\\(.*\\)$"
                                 nil t))
      (setq recipe (read (match-string 1))))
    (should
     (equal (plist-get (cdr recipe) :files)
            expected-files))
    (dolist (relative-file '("docs/TROUBLESHOOTING.org"
                             "docs/TROUBLESHOOTING.zh.org"))
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name relative-file magent-test--root-directory))
        (should (re-search-forward "^(magent[[:space:]]+:fetcher" nil t))
        (goto-char (match-beginning 0))
        (should (equal (plist-get (cdr (read (current-buffer))) :files)
                       expected-files))))
    (should (member "lisp/magent-action-builtins.el"
                    (magent-test-source-files
                     magent-test--root-directory)))))

(ert-deftest magent-test-melpazoid-treats-warnings-as-errors ()
  "Test melpazoid warnings fail CI."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name ".github/workflows/melpazoid.yml"
                       magent-test--root-directory))
    (should
     (re-search-forward
      "^[[:space:]]*WARN_IS_ERROR:[[:space:]]*true[[:space:]]*$"
      nil t))))

(ert-deftest magent-test-package-dependencies-use-stable-agent-shell ()
  "Test package metadata requires the reviewed stable frontend releases."
  (let ((main-file (expand-file-name "lisp/magent.el"
                                     magent-test--root-directory))
        requirements)
    (with-temp-buffer
      (insert-file-contents main-file)
      (should
       (re-search-forward "^;; Package-Requires: \\(.*\\)$" nil t))
      (setq requirements (read (match-string 1))))
    (should (version<= "0.13.1" (cadr (assq 'acp requirements))))
    (should (version<= "0.66.1" (cadr (assq 'agent-shell requirements))))))

(defconst magent-test--builtin-slash-command-names
  '("explain" "fix" "init" "review" "test")
  "Bundled Elisp-native prompt commands.")

(defconst magent-test--builtin-control-command-names
  '("authority" "compact" "skills")
  "Magent-owned session control exposed as a slash command.")

(defconst magent-test--builtin-maintenance-command-names
  '("doctor" "memory-clear" "memory-init" "memory-refresh")
  "Magent-owned isolated workflows exposed as slash commands.")

(ert-deftest magent-test-bundled-command-docs-match-runtime-inventory ()
  "Test user docs enumerate every bundled slash command and the exact count."
  (let ((names (append magent-test--builtin-control-command-names
                       magent-test--builtin-maintenance-command-names
                       magent-test--builtin-slash-command-names)))
    (should (= (length names) 12))
    (dolist (entry '(("README.org" . "twelve bundled")
                     ("docs/COMMANDS.org" . "twelve slash commands")
                     ("docs/COMMANDS.zh.org" . "12 个 slash commands")))
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name (car entry) magent-test--root-directory))
        (should (search-forward (cdr entry) nil t))
        (dolist (name names)
          (goto-char (point-min))
          (should (search-forward (format "~/%s~" name) nil t)))))))

(magent-define-workflow magent-test--empty-action-workflow (_invocation)
  "Return immediately for registry and frontend discovery tests."
  nil)

(defmacro magent-test--without-action-step-ledger (&rest body)
  "Run BODY while replacing durable command Step recording with stubs."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'magent-action--workflow-step-start)
              (lambda (&rest _) "workflow-step-test"))
             ((symbol-function 'magent-action--workflow-step-finish)
              #'ignore))
     ,@body))

(defun magent-test--load-builtin-skills-only ()
  "Load bundled skill files into the caller's test skill registry."
  (require 'magent-skills)
  (cl-letf (((symbol-function 'magent-log) #'ignore))
    (magent-skills-load-all (list magent-skills--builtin-dir))))

(defun magent-test--register-builtin-commands-only ()
  "Register bundled commands into the caller's test command registry."
  (require 'magent-action-builtins)
  (cl-letf (((symbol-function 'magent-log) #'ignore))
    (magent-action-builtins-register)))

(defun magent-test--load-builtin-capabilities-only ()
  "Load bundled capability definitions into the caller's test registry."
  (require 'magent-capability)
  (let ((magent-skill-directories nil)
        (magent-capability-directories nil))
    (cl-letf (((symbol-function 'magent-log) #'ignore))
      (magent-capability-initialize-static))))

;; ──────────────────────────────────────────────────────────────────────
;;; Integration tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-aa-supported-agent-shell-config-lives-in-core-config ()
  "Test supported agent-shell settings live in core configuration."
  (let ((config-file (expand-file-name "lisp/magent-config.el"
                                       magent-test--root-directory))
        (symbols '(magent-agent-shell-session-strategy)))
    (dolist (symbol symbols)
      (let ((regexp
             (format "^(defcustom %s\\_>"
                     (regexp-quote (symbol-name symbol)))))
        (with-temp-buffer
          (insert-file-contents config-file)
          (should (re-search-forward regexp nil t)))))))

(ert-deftest magent-test-aa-agent-shell-session-strategy-defaults-to-prompt ()
  "Test new Magent shells offer scoped session selection by default."
  (should (eq (default-value 'magent-agent-shell-session-strategy)
              'prompt)))

(ert-deftest magent-test-aa-retired-compatibility-files-and-symbols-are-removed ()
  "Test production sources contain no retired compatibility implementation."
  (dolist (file '("lisp/magent-ui.el"
                  "lisp/magent-ui-legacy.el"
                  "lisp/magent-evil.el"
                  "lisp/magent-modeline.el"
                  "lisp/magent-thread.el"
                  "lisp/magent-transcript-context.el"))
    (should-not (file-exists-p
                 (expand-file-name file magent-test--root-directory))))
  (dolist (file (magent-test-source-files magent-test--root-directory))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name file magent-test--root-directory))
      (should-not
       (re-search-forward
        "magent-\\(?:ui\\|legacy-queue\\|output-mode\\|compose-mode\\|evil\\)"
        nil t))))
  (dolist (symbol '(magent-action-prompt-handler
                    magent-agent-process
                    magent-session-add-message
                    magent-session-add-tool-message
                    magent-session-get-messages
                    magent-session-refresh-projections
                    magent-session-to-gptel-prompt-list
                    magent-tool-result-migrate-legacy
                    magent-memory-scan-plan-approval-input
                    magent-runtime-queue-kick
                    magent-thread-bound-tool-result-for-model
                    magent-action-session-record-tool
                    magent-skills--classify-source
                    magent-acp--runtime-session-for-scope
                    magent-action--canonical-scope
                    magent-skills--canonical-scope
                    magent-tools--glob-to-regexp
                    magent-permission--glob-to-regexp
                    magent-session--validate-json-state
                    magent-runtime-queue--set-submission-starter
                    magent-runtime-queue--bootstrap-preserved-backends
                    magent-agent-shell-start
                    magent-agent-shell-send-prompt
                    magent-agent-shell-toggle-skill-for-next-request
                    magent-agent-shell-clear-skills-for-next-request
                    magent-agent-shell-run-command
                    magent-agent-shell-run-init-command
                    magent-agent-shell-prompt-region
                    magent-agent-shell-ask-at-point
                    magent-agent-shell-interrupt
                    magent-agent-shell-processing-p))
    (should-not (fboundp symbol))))

(ert-deftest magent-test-aa-interactive-command-names-are-canonical ()
  "Test public commands use canonical verb-first names."
  (dolist (command '(magent-start
                     magent-find-skill
                     magent-install-skill
                     magent-delete-skill
                     magent-open-memory
                     magent-open-audit
                     magent-action-open-session
                     magent-action-list-sessions
                     magent-action-cancel
                     magent-action-run-doctor
                     magent-action-run-memory-init
                     magent-action-run-memory-refresh
                     magent-action-run-memory-clear
                     magent-clear-capability-overrides
                     magent-open-active-capabilities))
    (should (commandp command)))
  (dolist (api '(magent-skills-reload
                 magent-capability-reload
                 magent-session-reset))
    (should (fboundp api))
    (should-not (commandp api)))
  (dolist (retired '(magent-skill-find
                     magent-skill-install
                     magent-skill-delete
                     magent-memory-open
                     magent-memory-status
                     magent-open-memory-status
                     magent-show-audit
                     magent-internal-command-open-session
                     magent-list-internal-sessions
                     magent-cancel-internal-command
                     magent-run-doctor
                     magent-run-memory-init
                     magent-run-memory-refresh
                     magent-run-memory-clear
                     magent-capability-clear-local-overrides
                     magent-show-active-capabilities
                     magent-agent-shell-dwim))
    (should-not (fboundp retired))))

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
                   (funcall callback t (list :content "Hello from AI"))))))
      (magent-test--run-turn "Hello" (lambda (r) (setq response r))))
    (should (= call-count 1))
    (should (magent-execution-result-p response))
    (should (magent-execution-result-success-p response))
    (should (equal (magent-execution-result-content-string response)
                   "Hello from AI"))))

(ert-deftest magent-test-session-recording ()
  "Test that the public turn runner records the user message in the ledger."
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (gptel-tools nil)
        (gptel-use-tools nil)
        (response nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (prompt &rest kwargs)
                 (let* ((callback (plist-get kwargs :callback)))
                   (funcall callback "AI response" nil)
                   (funcall callback t (list :content "AI response"))))))
      (magent-session-reset)
      (magent-test--run-turn
       "User message"
       (lambda (r)
         (setq response r)))
      (let* ((session (magent-session-get))
             (messages (magent-test--session-transcript session)))
        (should (>= (length messages) 2))
        (should (equal (magent-execution-result-content-string response)
                       "AI response"))
        (let ((user-msg (nth 0 messages)))
          (should (eq (magent-test--transcript-role user-msg) 'user))
          (should (equal (magent-test--transcript-content user-msg) "User message")))
        (let ((assistant-msg (nth 1 messages)))
          (should (eq (magent-test--transcript-role assistant-msg) 'assistant))
          (should (equal (magent-test--transcript-content assistant-msg) "AI response")))))))

(ert-deftest magent-test-agent-run-turn-renders-completed-delta-after-stream-prefix ()
  "Test final completion text after streamed prefix is still rendered."
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (gptel-tools nil)
        (gptel-use-tools nil)
        (ui-chunks nil)
        (response nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (let ((callback (plist-get kwargs :callback)))
                   (funcall callback "Checking buffers. " '(:stream t))
                   (funcall callback t '(:content "Done.")))))
              ((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _args) nil)))
      (magent-session-reset)
      (magent-test--run-turn
       "Hello"
       (lambda (r) (setq response r))
       nil nil nil nil nil
       (lambda (text) (push text ui-chunks))))
    (should (equal (magent-execution-result-content-string response)
                   "Checking buffers. Done."))
    (should (equal (nreverse ui-chunks)
                   '("Checking buffers. " "Done.")))))

(ert-deftest magent-test-agent-run-turn-observer-dedupes-completed-full-text ()
  "Test UI-neutral observers do not receive completed text already streamed."
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (gptel-tools nil)
        (gptel-use-tools nil)
        (events nil)
        (response nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (let ((callback (plist-get kwargs :callback)))
                   (funcall callback "MAGENT_HELLO" '(:stream t))
                   (funcall callback t '(:content "MAGENT_HELLO")))))
              ((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _args) nil)))
      (magent-session-reset)
      (let* ((session (magent-session-get))
             (request-context
              (magent-request-context-create
               :session session
               :ui-visibility 'none
               :observer (lambda (event) (push event events)))))
        (magent-agent-run-turn
         :session session
         :prompt "Hello"
         :request-context request-context
         :on-complete (lambda (result) (setq response result)))))
    (should (equal (magent-execution-result-content-string response)
                   "MAGENT_HELLO"))
    (should (equal
             (delq nil
                   (mapcar (lambda (event)
                             (when (eq (plist-get event :type)
                                       'assistant-delta)
                               (plist-get event :text)))
                           (nreverse events)))
             '("MAGENT_HELLO")))))

(ert-deftest magent-test-agent-run-turn-error-returns-failed-result ()
  "Test provider errors are returned as failed agent results."
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (gptel-tools nil)
        (gptel-use-tools nil)
        (response nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (funcall (plist-get kwargs :callback)
                          nil
                          '(:status "Request timed out after 5 seconds"))))
              ((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _args) nil)))
      (magent-session-reset)
      (magent-test--run-turn
       "Hello"
       (lambda (result)
         (setq response result))))
    (should (magent-execution-result-p response))
    (should-not (magent-execution-result-success-p response))
    (should (equal (magent-execution-result-content-string response)
                   "Request timed out after 5 seconds"))
    (should (equal (magent-test--provider-context
                    (magent-session-get))
                   nil))))

(ert-deftest magent-test-agent-info-rejects-retired-agent-fields ()
  "Agent records expose no inert options or steps fields."
  (should-error
   (apply #'magent-agent-info-create '(:name "test" :steps 7)))
  (should-error
   (apply #'magent-agent-info-create '(:name "test" :options (:style strict)))))

(ert-deftest magent-test-agent-run-turn-continues-after-tool-output ()
  "Test a tool continuation returns only its terminal sample."
  (let* ((gptel-backend (gptel-make-openai "test" :key "test-key"))
         (gptel-model 'gpt-4o-mini)
         (magent-max-sampling-requests 0)
         (call-count 0)
         (sampled-prompts nil)
         (response nil)
         (session (magent-session-create :id "session-1"))
         (request-state (magent-request-context-create
                         :session session
                         :ui-visibility 'summary-only))
         (agent (magent-agent-info-create
                 :name "build"
                 :mode 'primary
                 :permission '((emacs_eval . allow)
                               (* . allow))))
         (tool-runtime
          (gptel-make-tool
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function
           (lambda (sexp)
             (magent-test-tool-result (format "eval:%s" sexp)))
           :async nil)))
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _args) (list tool-runtime)))
              ((symbol-function 'magent-tools-approval-policy)
               (lambda (_name) nil))
              ((symbol-function 'gptel-request)
               (lambda (prompt &rest kwargs)
                 (cl-incf call-count)
                 (push prompt sampled-prompts)
                 (let ((callback (plist-get kwargs :callback)))
                   (pcase call-count
                     (1
                      (funcall
                       callback
                       "Checking buffers. "
                       '(:tool-use ((:id "call-1"
					 :name "emacs_eval"
					 :args (:sexp "(+ 1 2)")))))
                      (funcall
                       callback
                       '(tool-call . ((nil ("(+ 1 2)") nil
                                           (:id "call-1"
						:name "emacs_eval"
						:args (:sexp "(+ 1 2)")))))
                       '(:tool-use t)))
                     (2
                      (funcall callback "Done."
                               '(:stream t :content "Done."))
                      (funcall callback t '(:content "Done.")))
                     (_
                      (error "unexpected sampling request %d" call-count))))))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore))
      (magent-test--run-turn
       "Run eval"
       (lambda (result) (setq response result))
       agent nil nil nil nil nil nil request-state))
    (should (= call-count 2))
    (should (equal (magent-execution-result-content-string response)
                   "Done."))
    (let* ((turn (car (magent-thread-turns
                       (magent-session-thread-ledger session))))
           (assistant
            (cl-find-if
             (lambda (item)
               (and (eq (magent-thread-item-type item) 'message)
                    (eq (magent-thread-item-role item) 'assistant)))
             (magent-thread-turn-items turn))))
      (should assistant)
      (should (equal (magent-thread-item-content assistant)
                     "Checking buffers. Done.")))
    (let ((second-prompt (car sampled-prompts)))
      (should (equal second-prompt
                     '((prompt . "Run eval")
                       (tool :id "call-1"
                             :name "emacs_eval"
                             :args (:sexp "(+ 1 2)")
                             :result "eval:(+ 1 2)")))))))

(ert-deftest magent-test-agent-run-turn-prefers-provider-native-continuation ()
  "Tool output resumes an available provider context without resampling."
  (let* ((gptel-backend (gptel-make-openai "test" :key "test-key"))
         (gptel-model 'gpt-4o-mini)
         (magent-max-sampling-requests 0)
         (request-count 0)
         provider-result
         continuation-called
         response
         (session (magent-session-create :id "native-continuation"))
         (request-state (magent-request-context-create
                         :session session
                         :ui-visibility 'summary-only))
         (agent (magent-agent-info-create
                 :name "build"
                 :mode 'primary
                 :permission '((emacs_eval . allow) (* . allow))))
         (tool-runtime
          (gptel-make-tool
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function
           (lambda (sexp)
             (magent-test-tool-result (format "eval:%s" sexp)))
           :async nil)))
    (cl-letf (((symbol-function 'magent-session-get) (lambda () session))
              ((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _args) (list tool-runtime)))
              ((symbol-function 'magent-tools-approval-policy)
               (lambda (_name) nil))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (cl-incf request-count)
                 (let ((callback (plist-get kwargs :callback)))
                   (funcall
                    callback
                    (list 'tool-call
                          (list nil '("(+ 1 2)")
                                (lambda (result)
                                  (setq provider-result result))
                                '(:id "call-1"
                                  :name "emacs_eval"
                                  :args (:sexp "(+ 1 2)"))))
                    (list
                     :tool-use t
                     :magent-tool-continuation
                     (lambda ()
                       (setq continuation-called t)
                       (funcall callback "Done." '(:stream t))
                       (funcall callback t '(:content "Done."))))))))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore))
      (magent-test--run-turn
       "Run eval"
       (lambda (result) (setq response result))
       agent nil nil nil nil nil nil request-state))
    (should (= request-count 1))
    (should continuation-called)
    (should (equal provider-result "eval:(+ 1 2)"))
    (should (equal (magent-execution-result-content-string response)
                   "Done."))))










(ert-deftest magent-test-agent-run-turn-async-continuation-preserves-tools ()
  "Async continuation preserves tools and accepts an empty completion."
  (let* ((gptel-backend (gptel-make-openai "test" :key "test-key"))
         (gptel-model 'gpt-4o-mini)
         (magent-max-sampling-requests 0)
         (call-count 0)
         (callbacks nil)
         (sampled-tool-use nil)
         (sampled-streams nil)
         (response nil)
         (session (magent-session-create :id "session-1"))
         (request-state (magent-request-context-create
                         :session session
                         :ui-visibility 'summary-only))
         (agent (magent-agent-info-create
                 :name "build"
                 :mode 'primary
                 :permission '((emacs_eval . allow)
                               (* . allow))))
         (tool-runtime
          (gptel-make-tool
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function
           (lambda (sexp)
             (magent-test-tool-result (format "eval:%s" sexp)))
           :async nil)))
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _args) (list tool-runtime)))
              ((symbol-function 'magent-tools-approval-policy)
               (lambda (_name) nil))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (cl-incf call-count)
                 (setq callbacks
                       (append callbacks
                               (list (plist-get kwargs :callback))))
                 (push gptel-use-tools sampled-tool-use)
                 (push (plist-get kwargs :stream) sampled-streams)))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore))
      (magent-test--run-turn
       "Run eval"
       (lambda (result) (setq response result))
       agent nil nil nil nil nil nil request-state)
      (should (= call-count 1))
      (funcall
       (nth 0 callbacks)
       '(tool-call . ((nil ("(+ 1 2)") nil
                           (:id "call-1"
				:name "emacs_eval"
				:args (:sexp "(+ 1 2)")))))
       '(:tool-use t))
      (should (= call-count 2))
      (funcall (nth 1 callbacks) t '(:content ""))
      (should (= call-count 2)))
    (should (magent-execution-result-success-p response))
    (should (equal (magent-execution-result-content-string response) ""))
    (should (eq (plist-get (magent-execution-result-metadata response) :reason)
                'empty-completion))
    (should (equal (nreverse sampled-tool-use) '(t t)))
    (should (equal (nreverse sampled-streams) '(t t)))
    (let* ((turn (car (magent-thread-turns
                       (magent-session-thread-ledger session))))
           (tool-items
            (cl-remove-if-not
             (lambda (item)
               (eq (magent-thread-item-type item) 'tool))
             (magent-thread-turn-items turn))))
      (should (= (length tool-items) 1))
      (should (equal
               (magent-thread-item-output (car tool-items))
               "eval:(+ 1 2)")))))







(ert-deftest magent-test-agent-run-turn-fails-directly-at-sampling-budget ()
  "The optional sampling budget fails without another provider request."
  (let* ((gptel-backend (gptel-make-openai "test" :key "test-key"))
         (gptel-model 'gpt-4o-mini)
         (magent-max-sampling-requests 1)
         (call-count 0)
         (sampled-tool-use nil)
         (response nil)
         (session (magent-session-create :id "session-1"))
         (request-state (magent-request-context-create
                         :session session
                         :ui-visibility 'summary-only))
         (agent (magent-agent-info-create
                 :name "build"
                 :mode 'primary
                 :permission '((emacs_eval . allow)
                               (* . allow))))
         (tool-runtime
          (gptel-make-tool
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function (lambda (_sexp) (magent-test-tool-result "ok"))
           :async nil)))
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _args) (list tool-runtime)))
              ((symbol-function 'magent-tools-approval-policy)
               (lambda (_name) nil))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (cl-incf call-count)
                 (push gptel-use-tools sampled-tool-use)
                 (let ((callback (plist-get kwargs :callback)))
                   (if (= call-count 1)
                       (funcall
                        callback
                        '(tool-call . ((nil ("(+ 1 2)") nil
                                            (:id "call-1"
						 :name "emacs_eval"
						 :args (:sexp "(+ 1 2)")))))
                        '(:tool-use t))
                     (error "unexpected sampling request %d" call-count)))))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore))
      (magent-test--run-turn
       "Run eval"
       (lambda (result) (setq response result))
       agent nil nil nil nil nil nil request-state))
    (should (= call-count 1))
    (should (equal (nreverse sampled-tool-use) '(t)))
    (should-not (magent-execution-result-success-p response))
    (should (eq (plist-get (magent-execution-result-metadata response) :reason)
                'sampling-limit))))

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
  (let* ((content "---\ntemperature: 0.7\nmax-items: 10\n---\n")
         (result (magent-file-loader-parse-frontmatter content))
         (fm (car result)))
    (should (= (plist-get fm :temperature) 0.7))
    (should (= (plist-get fm :max-items) 10))))

(ert-deftest magent-test-frontmatter-quoted-strings ()
  "Test frontmatter quoted string value parsing."
  (require 'magent-file-loader)
  (let* ((content "---\nname: \"my agent\"\ncolor: 'blue'\n---\n")
         (result (magent-file-loader-parse-frontmatter content))
         (fm (car result)))
    (should (equal (plist-get fm :name) "my agent"))
    (should (equal (plist-get fm :color) "blue"))))

(ert-deftest magent-test-frontmatter-decodes-yaml-string-escapes ()
  "Quoted YAML scalars and flow lists survive parsing without comma splitting."
  (require 'magent-file-loader)
  (let* ((content
          (concat "---\n"
                  "description: \"Say \\\"hi\\\", then C:\\\\tmp\\nnext\"\n"
                  "tools: [bash, read_file]\n"
                  "---\n"))
         (fm (car (magent-file-loader-parse-frontmatter content))))
    (should (equal (plist-get fm :description)
                   "Say \"hi\", then C:\\tmp\nnext"))
    (should (equal (plist-get fm :tools) '("bash" "read_file")))))

(ert-deftest magent-test-frontmatter-keeps-commas-in-scalar-fields ()
  "Comma-containing scalar fields remain scalar values."
  (require 'magent-file-loader)
  (let* ((content "---\ndescription: Fast, focused, composable\n---\n")
         (fm (car (magent-file-loader-parse-frontmatter content))))
    (should (equal (plist-get fm :description)
                   "Fast, focused, composable"))))

(ert-deftest magent-test-frontmatter-does-not-infer-comma-separated-lists ()
  "Generic frontmatter parsing does not infer a list from commas."
  (require 'magent-file-loader)
  (let* ((content "---\ntools: bash, read, write\n---\n")
         (result (magent-file-loader-parse-frontmatter content))
         (fm (car result)))
    (should (equal (plist-get fm :tools) "bash, read, write"))))

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

(ert-deftest magent-test-frontmatter-rejects-underscore-keys ()
  "Frontmatter keys must use the canonical kebab-case spelling."
  (require 'magent-file-loader)
  (should-error
   (magent-file-loader-parse-frontmatter
    "---\ntop_p: 0.9\nmax_tokens: 100\n---\n")))

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

(ert-deftest magent-test-permission-resource-globs-are-path-aware ()
  "Resource globs distinguish one-level `*' from recursive `**'."
  (require 'magent-permission)
  (let ((rules '((read . (("src/*.el" . allow)
                          ("src/**/*.el" . ask)
                          (* . deny))))))
    (should (eq (magent-permission-resolve rules 'read "src/top.el")
                'allow))
    (should (eq (magent-permission-resolve rules 'read "src/deep/item.el")
                'ask))
    (should (eq (magent-permission-resolve rules 'read "docs/item.el")
                'deny))))

(ert-deftest magent-test-permission-resource-globs-use-exact-declared-order ()
  "Every resource rule, including `*', participates in first-match order."
  (require 'magent-permission)
  (let ((rules '((read . ((* . allow)
                          ("src/**" . ask)
                          ("src/private/**" . deny))))))
    ;; The declared catch-all wins because it is first.
    (should (eq (magent-permission-resolve
                 rules 'read "src/private/secret.el")
                'allow))
    (should (eq (magent-permission-resolve rules 'read "docs/public.el")
                'allow))))

(ert-deftest magent-test-permission-slash-free-glob-matches-basename ()
  "A slash-free resource glob applies to basenames at every depth."
  (require 'magent-permission)
  (let ((rules '((read . (("*.env" . deny)
                          (* . allow))))))
    (should (eq (magent-permission-resolve
                 rules 'read "nested/config.env")
                'deny))
    (should (eq (magent-permission-resolve
                 rules 'read "nested/config.txt")
                'allow))))

(ert-deftest magent-test-permission-resource-globs-are-case-sensitive ()
  "Linux resource allowlists do not inherit ambient case folding."
  (require 'magent-permission)
  (let ((case-fold-search t)
        (rules '((edit . (("src/*.el" . allow)
                          (* . deny))))))
    (should (eq (magent-permission-resolve rules 'edit "src/main.el")
                'allow))
    (should (eq (magent-permission-resolve rules 'edit "SRC/MAIN.EL")
                'deny))))

(ert-deftest magent-test-permission-string-tool-name-honors-exact-rule ()
  "The documented string TOOL form cannot fall through an exact deny."
  (require 'magent-permission)
  (let ((rules '((read . deny)
                 (custom_operation . (("safe/*" . allow) (* . deny)))
                 (* . allow))))
    (should (eq (magent-permission-resolve rules "read") 'deny))
    (should (eq (magent-permission-resolve
                 rules "custom_operation" "unsafe/file")
                'deny))
    (should (magent-permission-tool-available-p
             rules "custom_operation"))
    (should (eq (magent-permission-resolve rules "write") 'allow))))

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
  (let ((magent-bypass-permission t)
        (rules '((bash . deny)
                 (* . deny))))
    (should (magent-permission-tool-available-p rules 'bash))
    (should (magent-permission-tool-available-p rules 'write))))

(ert-deftest magent-test-toggle-bypass-permission-command ()
  "Test the interactive permission bypass toggle command."
  (require 'magent-permission)
  (let ((magent-bypass-permission nil)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (should (eq (magent-toggle-bypass-permission) t))
      (should magent-bypass-permission)
      (should (equal (car messages) "Magent permission bypass enabled"))
      (should (eq (magent-toggle-bypass-permission 0) nil))
      (should-not magent-bypass-permission)
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

(ert-deftest magent-test-permission-plan-keeps-specific-rule-before-wildcard ()
  "Plan policy has one canonical wildcard after its writable path rule."
  (require 'magent-agent-builtins)
  (let* ((permission
          (magent-agent-info-permission (magent-agent-builtins--plan)))
         (edit-rules (cdr (assq 'edit permission))))
    (should (= (cl-count-if
                (lambda (rule)
                  (or (eq (car rule) '*) (equal (car rule) "*")))
                edit-rules)
               1))
    (should (eq (magent-permission-resolve
                 permission 'edit ".magent/plan/work.md")
                'allow))
    (should (eq (magent-permission-resolve permission 'edit "README.org")
                'deny))))

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

(ert-deftest magent-test-session-records-ledger-messages ()
  "Test session message addition and retrieval."
  (let ((session (magent-session-create)))
    (magent-test--record-session-entry session 'user "Hello")
    (magent-test--record-session-entry session 'assistant "Hi there")
    (should (= (length (magent-test--session-transcript session)) 2))
    (let ((messages (magent-test--session-transcript session)))
      (should (= (length messages) 2))
      (should (eq (magent-test--transcript-role (nth 0 messages)) 'user))
      (should (equal (magent-test--transcript-content (nth 0 messages)) "Hello"))
      (should (eq (magent-test--transcript-role (nth 1 messages)) 'assistant))
      (should (equal (magent-test--transcript-content (nth 1 messages)) "Hi there")))))

(ert-deftest magent-test-session-history-trimming ()
  "Test lazy history trimming."
  (let ((session (magent-session-create)))
    (setf (magent-session-max-history session) 5)
    ;; Add 10 messages
    (dotimes (i 10)
      (magent-test--record-session-entry session 'user (format "msg %d" i)))
    ;; Should have 10 messages (lazy trim hasn't triggered yet)
    (should (= (length (magent-test--session-transcript session)) 10))
    ;; Add 6 more to trigger trim (10 + 6 = 16 > 5 + 10)
    (dotimes (i 6)
      (magent-test--record-session-entry session 'user (format "extra %d" i)))
    ;; Now should be trimmed to max-history (5)
    (should (= (length (magent-test--session-transcript session)) 5))))

(ert-deftest magent-test-session-trimming-preserves-recent ()
  "Test that trimming keeps the most recent messages."
  (let ((session (magent-session-create)))
    (setf (magent-session-max-history session) 3)
    ;; Add enough to trigger trim (3 + 10 = 13, need > 13)
    (dotimes (i 14)
      (magent-test--record-session-entry session 'user (format "msg-%d" i)))
    (should (= (length (magent-test--session-transcript session)) 3))
    (let* ((messages (magent-test--session-transcript session))
           (first-content (magent-test--transcript-content (nth 0 messages))))
      ;; Oldest remaining should be msg-11 (14 - 3 = 11)
      (should (equal first-content "msg-11")))))

(ert-deftest magent-test-session-empty ()
  "Test freshly created session has no messages."
  (let ((session (magent-session-create)))
    (should (= (length (magent-test--session-transcript session)) 0))
    (should (null (magent-test--session-transcript session)))))

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

(ert-deftest magent-test-session-provider-context ()
  "Test conversion from session messages to gptel prompt list."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-test--record-session-entry session 'user "What is Emacs?")
    (magent-test--record-session-entry session 'assistant "A text editor.")
    (magent-test--record-session-entry session 'user "Tell me more.")
    (let ((prompt-list (magent-test--provider-context session)))
      (should (= (length prompt-list) 3))
      (should (equal (car (nth 0 prompt-list)) 'prompt))
      (should (equal (cdr (nth 0 prompt-list)) "What is Emacs?"))
      (should (equal (car (nth 1 prompt-list)) 'response))
      (should (equal (cdr (nth 1 prompt-list)) "A text editor."))
      (should (equal (car (nth 2 prompt-list)) 'prompt))
      (should (equal (cdr (nth 2 prompt-list)) "Tell me more.")))))

(ert-deftest magent-test-session-context-views-are-explicit ()
  "Each supported consumer asks for a named session projection."
  (require 'magent-session)
  (let ((session (magent-session-create :id "context-views")))
    (magent-test--record-session-entry session 'user "Question")
    (magent-test--record-session-entry session 'assistant "Answer")
    (should
     (equal (magent-session-context-view session 'provider)
            (magent-test--provider-context session)))
    (should
     (equal (magent-session-context-view session 'compaction)
            (magent-session-context-view session 'provider)))
    (should
     (equal (magent-session-context-view session 'transcript)
            (magent-test--session-transcript session)))
    (let ((ledger (magent-session-context-view session 'ledger))
          (audit (magent-session-context-view session 'audit)))
      (should (equal (cdr (assq 'session-id ledger)) "context-views"))
      (should (equal (cdr (assq 'session-id
                                (cdr (assq 'snapshot audit))))
                     "context-views"))
      (should (equal (cdr (assq 'turns (cdr (assq 'snapshot audit))))
                     (cdr (assq 'turns ledger))))
      (should (vectorp (cdr (assq 'journal audit)))))
    (should-error (magent-session-context-view session 'implicit))))

(ert-deftest magent-test-session-provider-context-keeps-structured-tool ()
  "Test structured tool messages are included in gptel prompt list."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-test--record-session-entry session 'user "Run ls")
    (magent-test--record-tool-entry
     session "call_1" "bash" '(:command "ls")
     (magent-test-tool-result "file1.txt\nfile2.txt"))
    (magent-test--record-session-entry session 'assistant "Here are the files.")
    (let ((prompt-list (magent-test--provider-context session)))
      (should (= (length prompt-list) 3))
      (should (equal (nth 0 prompt-list) '(prompt . "Run ls")))
      (should (equal (nth 1 prompt-list)
                     '(tool . (:id "call_1"
				   :name "bash"
				   :args (:command "ls")
				   :result "file1.txt\nfile2.txt"))))
      (should (equal (nth 2 prompt-list)
                     '(response . "Here are the files."))))))

(ert-deftest magent-test-session-provider-context-json-sanitizes-tool-args ()
  "Test tool data reused in gptel prompts is safe for `json-serialize'."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-test--record-session-entry session 'user "Run tool")
    (magent-test--record-tool-entry
     session "call_1" 'emacs_eval
     '(:sexp "(+ 20 22)" :tool emacs_eval :values [emacs_eval nil])
     (magent-test-tool-result "42"))
    (let* ((prompt-list (magent-test--provider-context session))
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

(ert-deftest magent-test-json-safe-tool-args-preserves-nil-values ()
  "Test Lisp nil values are kept distinct from provider JSON null sentinels."
  (require 'magent-json)
  (should
   (equal (magent-json-safe-tool-args
           '(:optional nil :missing :null :name "value"))
          '(:optional :null :name "value"))))

(ert-deftest magent-test-session-provider-context-drops-non-reusable-turns ()
  "Test empty and failed assistant turns do not leak into prompt reuse."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-test--record-session-entry session 'user "emacs 有几个 buffer")
    (magent-test--record-session-entry session 'assistant "")
    (should (eq (magent-thread-turn-status
                 (car (magent-thread-turns
                       (magent-session-thread-ledger session))))
                'completed))
    (magent-test--record-session-entry session 'user "magent 有几个 skills")
    (magent-test--record-session-entry
     session 'assistant
     "Error: provider request failed.")
    (magent-test--record-session-entry session 'user "emacs 有几个 实例")
    (should (equal (magent-test--provider-context session)
                   '((prompt . "emacs 有几个 实例"))))))

(ert-deftest magent-test-session-provider-context-keeps-completed-turns ()
  "Test completed turns remain even when a later turn failed."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-test--record-session-entry session 'user "What is Emacs?")
    (magent-test--record-session-entry session 'assistant "A text editor.")
    (magent-test--record-session-entry session 'user "magent 有几个 skills")
    (magent-test--record-session-entry
     session 'assistant
     "Error: provider request failed.")
    (magent-test--record-session-entry session 'user "Tell me more.")
    (should (equal (magent-test--provider-context session)
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

(ert-deftest magent-test-session-prompt-history-starts-at-last-compaction ()
  "Test completed compaction turns replace older model-visible history."
  (require 'magent-session)
  (let* ((session (magent-session-create))
         (thread (magent-session-thread-ledger session)))
    (cl-labels
        ((add-turn (prompt response &optional metadata)
           (let ((turn (magent-thread-create-turn
                        thread prompt nil metadata)))
             (magent-thread-record-message
              thread (magent-thread-turn-id turn) 'user prompt)
             (magent-thread-record-message
              thread (magent-thread-turn-id turn) 'assistant response)
             (magent-thread-complete-turn
              thread (magent-thread-turn-id turn)))))
      (add-turn "old question" "old answer" '(:source runtime-queue))
      (add-turn "compact this" "continuation summary"
                '((compaction . t)))
      (add-turn "new question" "new answer" '(:source runtime-queue)))
    (should
     (equal (magent-test--provider-context session)
            '((prompt . "compact this")
              (response . "continuation summary")
              (prompt . "new question")
              (response . "new answer"))))))

(ert-deftest magent-test-session-failed-compaction-keeps-earlier-history ()
  "Test a failed compaction turn never becomes a prompt-history boundary."
  (require 'magent-session)
  (let* ((session (magent-session-create))
         (thread (magent-session-thread-ledger session))
         (first (magent-thread-create-turn thread "old question"))
         (compact (magent-thread-create-turn
                   thread "compact this" nil '(:compaction t))))
    (magent-thread-record-message
     thread (magent-thread-turn-id first) 'user "old question")
    (magent-thread-record-message
     thread (magent-thread-turn-id first) 'assistant "old answer")
    (magent-thread-complete-turn thread (magent-thread-turn-id first))
    (magent-thread-record-message
     thread (magent-thread-turn-id compact) 'user "compact this")
    (magent-thread-fail-turn
     thread (magent-thread-turn-id compact) "provider failed")
    (should
     (equal (magent-test--provider-context session)
            '((prompt . "old question")
              (response . "old answer"))))))

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
    (magent-test--record-session-entry s1 'user "test")
    (magent-session-reset)
    (let ((s2 (magent-session-get)))
      (should-not (eq s1 s2))
      (should (= (length (magent-test--session-transcript s2)) 0)))))

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
         (should (= gptel-temperature 1.0))
         (should (eq gptel-model 'default-model)))))))

(ert-deftest magent-test-agent-run-turn-records-runtime-inheritance ()
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
                 :effort 'xhigh
                 :permission (magent-permission-from-config
                              '((agent . ask)
                                (bash . deny)
                                (* . allow)))))
         (session (magent-session-create :id "parent"))
         (request-state (magent-request-context-create
                         :id "req"
                         :scope "/tmp/project"
                         :session session
                         :backend backend
                         :model 'parent-model
                         :temperature 0.42))
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
              ((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _args) nil))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-skills-get-instruction-prompts)
               (lambda (_skills) nil)))
      (magent-test--run-turn
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
      (should (eq (magent-request-context-effort request-state) 'xhigh))
      (should (equal (magent-request-context-project-root request-state)
                     "/tmp/project"))
      (should (equal (magent-request-context-skill-names request-state)
                     '("cap-skill")))
      (should (equal (plist-get metadata :temperature) 0.42))
      (should (equal (plist-get metadata :top-p) 0.88))
      (should (equal (plist-get metadata :effort) 'xhigh))
      (should (equal (magent-permission-resolve
                      (magent-request-context-permission-profile request-state)
                      'agent)
                     'ask)))))

(ert-deftest magent-test-agent-run-turn-normalizes-request-context-skills ()
  "Inherited and capability skills share one normalized request-state value."
  (require 'magent-agent)
  (require 'magent-capability)
  (let* ((backend (gptel-make-openai "skills" :key "key"))
         (gptel-backend backend)
         (gptel-model 'skills-model)
         (agent (magent-agent-info-create
                 :name "explore"
                 :mode 'subagent))
         (session (magent-session-create :id "child-skills"))
         (request-state (magent-request-context-create
                         :session session
                         :skill-names '("parent-skill")))
         (resolution
          (magent-capability-resolution-create
           :prompt "inspect"
           :skill-names '("auto-skill")))
         captured-skills)
    (cl-letf (((symbol-function 'magent-agent-loop-start)
               (lambda (_loop) 'started))
              ((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _args) nil))
              ((symbol-function 'magent-skills-get-instruction-prompts)
               (lambda (skill-names)
                 (setq captured-skills skill-names)
                 '("captured skill prompt")))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore))
      (magent-test--run-turn
       "inspect" nil agent nil nil nil resolution nil nil request-state))
    (should (equal captured-skills '("parent-skill" "auto-skill")))
    (should (equal (magent-request-context-skill-names request-state)
                   '("parent-skill" "auto-skill")))))

(ert-deftest magent-test-agent-run-turn-startup-error-respects-context-ownership ()
  "Startup errors close owned contexts and preserve inherited contexts."
  (require 'magent-agent)
  (let* ((gptel-backend (gptel-make-openai "startup" :key "key"))
         (gptel-model 'startup-model)
         (agent-permission '((bash . allow) (* . deny)))
         (request-permission '((bash . deny) (* . allow)))
         (agent (magent-agent-info-create
                 :name "build" :mode 'primary
                 :permission agent-permission))
         (owned-event
          (magent-lifecycle-events-context-create :turn-id "owned"))
         (inherited-event
          (magent-lifecycle-events-context-create :turn-id "inherited"))
         (owned-state
          (magent-request-context-create
           :scope 'global
           :session (magent-session-create :id "owned-session")
           :permission-profile request-permission))
         (inherited-state
          (magent-request-context-create
           :scope 'global
           :session (magent-session-create :id "inherited-session")
           :event-context inherited-event))
         ended
         exposed-permissions)
    (cl-letf (((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) owned-event))
              ((symbol-function 'magent-lifecycle-events-end-turn)
               (lambda (context status &optional detail)
                 (push (list context status detail) ended)))
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-memory-system-message)
               (lambda (&rest _args) nil))
              ((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (permission &rest _tool-names)
                 (push permission exposed-permissions)
                 nil))
              ((symbol-function 'magent-agent-loop-start)
               (lambda (_loop) (error "sampler startup failed")))
              ((symbol-function 'magent-session-save-deferred-for-session)
               #'ignore)
              ((symbol-function 'magent-log) #'ignore))
      (should-error
       (magent-test--run-turn
        "owned" nil agent nil nil nil nil nil nil owned-state))
      (should-error
       (magent-test--run-turn
        "inherited" nil agent nil inherited-event nil nil nil nil
        inherited-state)))
    (should (= (length ended) 1))
    (should (eq (caar ended) owned-event))
    (should (eq (cadar ended) 'failed))
    (dolist (state (list owned-state inherited-state))
      (let* ((session (magent-request-context-session state))
             (turn (magent-thread-find-turn
                    (magent-session-thread-ledger session)
                    (magent-request-context-turn-id state))))
        (should (eq (magent-thread-turn-status turn) 'failed))
        (should (equal (magent-thread-turn-error turn)
                       "sampler startup failed"))))
    (should (equal (nreverse exposed-permissions)
                   (list request-permission agent-permission)))))

(ert-deftest magent-test-agent-run-turn-system-prompt-includes-scope-root ()
  "Test project-scoped runtime turns tell the model the current repo root."
  (require 'magent-agent)
  (require 'magent-capability)
  (let* ((backend (gptel-make-openai "scope-root" :key "key"))
         (gptel-backend backend)
         (gptel-model 'scope-model)
         (magent-system-prompt "Global system.")
         (agent (magent-agent-info-create
                 :name "build"
                 :mode 'primary
                 :prompt "Base system."))
         (session (magent-session-create :id "scope-root"))
         (request-state (magent-request-context-create
                         :id "req"
                         :scope "/tmp/project"
                         :session session))
         (capability-resolution
          (magent-capability-resolution-create
           :prompt "summarize this repo"
           :context '(:project-root "/tmp/project")))
         captured-loop)
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-agent-loop-start)
               (lambda (loop)
                 (setq captured-loop loop)
                 'started))
              ((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _args) nil))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-skills-get-instruction-prompts)
               (lambda (_skills) nil)))
      (magent-test--run-turn
       "summarize this repo"
       nil
       agent
       nil
       nil
       nil
       capability-resolution
       nil
       nil
       request-state))
    (let* ((request (magent-agent-loop-request captured-loop))
           (system (magent-llm-request-system request)))
      (should (string-match-p
               (regexp-quote "Current project root: /tmp/project")
               system))
      (should (string-match-p
               (regexp-quote "do not invent unrelated absolute paths")
               system))
      (should (string-prefix-p "Global system." system))
      (should (< (string-match "Global system" system)
                 (string-match "Base system" system)))
      (should (< (string-match "Base system" system)
                 (string-match "Current project root" system)))
      (should (string-match-p
               (regexp-quote "* Runtime Trust Boundary")
               system))
      (should (< (string-match "Current project root" system)
                 (string-match "Runtime Trust Boundary" system)))
      (should (equal (magent-request-context-project-root request-state)
                     "/tmp/project")))))

(ert-deftest magent-test-agent-compose-system-prompt-orders-runtime-policy-last ()
  "Test dynamic context precedes the universal runtime trust policy."
  (require 'magent-agent)
  (let* ((system (magent-agent--compose-system-message
                  "Global system contract."
                  "Agent-specific output contract."
                  "/tmp/project"
                  "Memory block."
                  '("Skill block.")
                  "Project instructions block."))
         (global-pos (string-match "Global system contract" system))
         (role-pos (string-match "Agent-specific output contract" system))
         (project-pos (string-match "Current project root" system))
         (instructions-pos (string-match "Project instructions block" system))
         (memory-pos (string-match "Memory block" system))
         (skill-pos (string-match "Skill block" system))
         (policy-pos (string-match "Runtime Trust Boundary" system)))
    (should (cl-every #'integerp
                      (list global-pos role-pos project-pos
                            instructions-pos memory-pos
                            skill-pos policy-pos)))
    (should (< global-pos role-pos))
    (should (< role-pos project-pos))
    (should (< project-pos instructions-pos))
    (should (< instructions-pos memory-pos))
    (should (< memory-pos skill-pos))
    (should (< skill-pos policy-pos))
    (should (string-match-p
             (regexp-quote "current user request is instruction")
             system))
    (should (string-match-p
             (regexp-quote "Quoted, retrieved, or embedded content")
             system))
    (should (string-match-p
             (regexp-quote "cannot promote itself")
             system))))

(ert-deftest magent-test-memory-scan-plan-skips-sensitive-and-org-notes ()
  "Test memory scan plans avoid secrets, custom-file contents, and Org notes."
  (require 'magent-action-builtin-memory)
  (let* ((root (file-name-as-directory
                (make-temp-file "magent-memory-root" t)))
         (init-file (expand-file-name "init.el" root))
         (custom-path (expand-file-name "custom.el" root))
         (readme (expand-file-name "README.org" root))
         (notes (expand-file-name "notes.org" root))
         (secret (expand-file-name "secrets.el" root))
         (user-emacs-directory root)
         (user-init-file init-file)
         (early-init-file nil)
        (custom-file custom-path)
        (magent-memory-extra-scan-roots nil)
        (magent-memory-exclude-patterns
         (remove "/var/" magent-memory-exclude-patterns))
        (magent-memory-scan-custom-file nil)
         (magent-memory-max-files 20)
         (magent-memory-max-file-bytes 10000)
         (magent-memory-max-scan-bytes 50000))
    (with-temp-file init-file
      (insert "(use-package magit)\n"))
    (with-temp-file custom-path
      (insert "(custom-set-variables '(secret-token \"abc\"))\n"))
    (with-temp-file readme
      (insert "# Emacs config\n"))
    (with-temp-file notes
      (insert "* Personal notes\n"))
    (with-temp-file secret
      (insert "(setq token \"secret\")\n"))
    (let* ((plan (magent-memory-build-scan-plan))
           (files (magent-memory--scan-plan-file-paths plan)))
      (should (member init-file files))
      (should (member readme files))
      (should-not (member custom-path files))
      (should-not (member notes files))
      (should-not (member secret files))
      (should (member secret
                      (magent-memory-scan-plan-skipped-sensitive plan))))))

(ert-deftest magent-test-memory-refresh-preserves-user-notes ()
  "Test memory refresh rewrites managed content and preserves User Notes."
  (require 'magent-action-builtin-memory)
  (let* ((root (file-name-as-directory
                (make-temp-file "magent-memory-root" t)))
         (memory-dir (file-name-as-directory
                      (make-temp-file "magent-memory-store" t)))
         (init-file (expand-file-name "init.el" root))
         (user-emacs-directory root)
         (user-init-file init-file)
         (early-init-file nil)
         (custom-file nil)
         (magent-memory-directory memory-dir)
         (magent-memory-use-llm nil)
         (magent-memory-open-after-write nil)
         (magent-memory-extra-scan-roots nil))
    (with-temp-file init-file
      (insert "(use-package project)\n"))
    (magent-memory-run
     'init
     :confirm-fn (lambda (_plan continue) (funcall continue t)))
    (with-temp-buffer
      (insert-file-contents (magent-memory-file))
      (goto-char (point-max))
      (insert "Prefer minibuffer-driven confirmations.\n")
      (write-region (point-min) (point-max) (magent-memory-file)))
    (magent-memory-run
     'refresh
     :confirm-fn (lambda (_plan continue) (funcall continue t)))
    (with-temp-buffer
      (insert-file-contents (magent-memory-file))
      (let ((text (buffer-string)))
        (should (string-match-p
                 (regexp-quote "* Magent Managed Profile")
                 text))
        (should (string-match-p
                 (regexp-quote "Prefer minibuffer-driven confirmations.")
                 text))
        (should (file-directory-p
                 (magent-memory-snapshots-directory)))))))

(ert-deftest magent-test-memory-profile-and-snapshots-use-private-modes ()
  "Memory persistence enforces 0700 directories and 0600 files."
  (require 'magent-action-builtin-memory)
  (let* ((memory-dir (file-name-as-directory
                      (make-temp-file "magent-memory-private-" t)))
         (magent-memory-directory memory-dir)
         (file (magent-memory-file))
         (plan (magent-memory-scan-plan--create
                :roots nil
                :entry-files nil
                :files nil
                :skipped-sensitive nil
                :skipped-excluded nil
                :skipped-budget nil
                :total-bytes 0
                :generated-at (current-time)
                :provider "test"
                :model "test")))
    (unwind-protect
        (progn
          (set-file-modes memory-dir #o755)
          (with-temp-file file
            (insert "old profile\n"))
          (set-file-modes file #o644)
          (magent-memory--write-profile
           plan "* Magent Managed Profile\n" "private note")
          (let* ((snapshots (magent-memory-snapshots-directory))
                 (backup (car (directory-files snapshots t "\\.org\\'"))))
            (should backup)
            (should (= (logand (file-modes memory-dir) #o777) #o700))
            (should (= (logand (file-modes snapshots) #o777) #o700))
            (should (= (logand (file-modes file) #o777) #o600))
            (should (= (logand (file-modes backup) #o777) #o600))))
      (delete-directory memory-dir t))))

(ert-deftest magent-test-redaction-removes-labeled-and-unlabeled-secrets ()
  "Test outbound redaction removes secrets while retaining stable ids."
  (require 'magent-redaction)
  (let* ((secret "sk-DoctorCanaryAbCdEf1234567890")
         (sha "abcdef0123456789abcdef0123456789abcdef01")
         (uuid "123e4567-e89b-12d3-a456-426614174000")
         (text (format
                (concat "Authorization: Bearer %s\nraw=%s\n"
                        "OPENAI_API_KEY=short-value\nsha=%s\nuuid=%s")
                secret secret sha uuid))
         (redacted (magent-redaction-string text t)))
    (should-not (string-match-p (regexp-quote secret) redacted))
    (should (string-match-p "<redacted:authorization>" redacted))
    (should (string-match-p "<redacted:token>" redacted))
    (should-not (string-match-p "short-value" redacted))
    (should (string-match-p "OPENAI_API_KEY: <redacted:key>" redacted))
    (should (string-match-p (regexp-quote sha) redacted))
    (should (string-match-p (regexp-quote uuid) redacted))))

(ert-deftest magent-test-redaction-value-fails-closed-on-live-object ()
  "Test structured redaction rejects non-JSON-safe live objects."
  (require 'magent-redaction)
  (should-error
   (magent-redaction-value (list :buffer (current-buffer)) t)
   :type 'magent-redaction-unsafe-value))

(ert-deftest magent-test-redaction-value-redacts-compound-secret-key ()
  "Test recursive redaction recognizes provider-prefixed secret keys."
  (require 'magent-redaction)
  (should
   (equal (magent-redaction-value
           '((OPENAI_API_KEY . "short-value") (safe . "visible")) t)
          '((OPENAI_API_KEY . "<redacted:key>") (safe . "visible")))))

(ert-deftest magent-test-memory-clear-deactivates-and-preserves-user-notes ()
  "Test memory clear writes inactive metadata and keeps local user notes."
  (require 'magent-action-builtin-memory)
  (let* ((root (file-name-as-directory
                (make-temp-file "magent-memory-root" t)))
         (memory-dir (file-name-as-directory
                      (make-temp-file "magent-memory-store" t)))
         (init-file (expand-file-name "init.el" root))
         (user-emacs-directory root)
         (user-init-file init-file)
         (early-init-file nil)
         (custom-file nil)
         (magent-memory-directory memory-dir)
         (magent-memory-use-llm nil)
         (magent-memory-open-after-write nil)
         (magent-memory-extra-scan-roots nil))
    (unwind-protect
        (progn
          (with-temp-file init-file
            (insert "(use-package project)\n"))
          (magent-memory-run
           'init :confirm-fn
           (lambda (_plan continue) (funcall continue t)))
          (with-temp-buffer
            (insert-file-contents (magent-memory-file))
            (goto-char (point-max))
            (insert "Keep minibuffer confirmations.\n")
            (write-region (point-min) (point-max) (magent-memory-file)))
          (magent-memory-run
           'clear :confirm-fn
           (lambda (_plan continue) (funcall continue t)))
          (let ((text (with-temp-buffer
                        (insert-file-contents (magent-memory-file))
                        (buffer-string))))
            (should (string-match-p
                     (regexp-quote "#+magent-active: false") text))
            (should (string-match-p
                     (regexp-quote "Keep minibuffer confirmations.") text))
            (should-not (magent-memory-active-p))
            (should-not (magent-memory-system-message "help with Emacs"))
            (should (directory-files
                     (magent-memory-snapshots-directory) nil "\\.org$"))))
      (delete-directory root t)
      (delete-directory memory-dir t))))

(ert-deftest magent-test-memory-outbound-injection-redacts-user-secret ()
  "Test prompt-time memory injection never emits a user-note secret."
  (require 'magent-action-builtin-memory)
  (let* ((memory-dir (file-name-as-directory
                      (make-temp-file "magent-memory-store" t)))
         (magent-memory-directory memory-dir)
         (magent-memory-enable-auto-injection t)
         (magent-memory-injection-max-chars 6000)
         (secret "sk-MemoryCanaryAbCdEf1234567890"))
    (unwind-protect
        (progn
          (with-temp-file (magent-memory-file)
            (insert "#+magent-active: true\n\n"
                    "* Magent Managed Profile\n"
                    "** Overview\nUse Emacs daily.\n"
                    "* User Notes\n"
                    "api-key: " secret "\n"))
          (cl-letf (((symbol-function 'magent-memory--relevant-request-p)
                     (lambda (&rest _) t)))
            (let ((message (magent-memory-system-message "Emacs api-key")))
              (should message)
              (should-not (string-match-p (regexp-quote secret) message))
              (should (string-match-p "<redacted:key>" message)))))
      (delete-directory memory-dir t))))

(ert-deftest magent-test-action-memory-init-uses-isolated-action-session ()
  "The memory M-x wrapper and slash spec share one isolated handler."
  (require 'magent-action-session)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-action-session-directory nil)
         (magent-action--registry nil)
         (magent-action--active-invocations (make-hash-table :test #'eq))
         (magent-action-session--active-invocations
          (make-hash-table :test #'equal))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (parent (magent-session-create :id "session-parent"))
         operation)
    (unwind-protect
        (progn
          (magent-session-install 'global parent)
          (magent-test--register-builtin-commands-only)
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-runtime-context-scope)
                     (lambda () 'global))
                    ((symbol-function 'magent-runtime-prepare-context)
                     (lambda (&optional scope) (or scope 'global)))
                    ((symbol-function 'magent-session-save-deferred-for-session)
                     #'ignore)
                    ((symbol-function 'magent-memory-run)
                     (lambda (op &rest args)
                       (setq operation op)
                       (funcall (plist-get args :notify-fn)
                                "memory init progress")
                       (funcall (plist-get args :on-complete)
                                'completed
                                "memory init complete"))))
            (magent-action-run-memory-init))
          (let* ((files (magent-session-list-action-files "memory-init"))
                 (meta (magent-session--read-file-metadata-cached (car files)))
                 (spec (magent-action-get "memory-init" 'global 'interactive)))
            (should (eq operation 'init))
            (should (= (length files) 1))
            (should (equal (plist-get meta :kind) "action"))
            (should (equal (plist-get meta :status) "completed"))
            (should (equal (magent-action-spec-session-policy spec) 'isolated))
            (should (equal (magent-action-spec-exposure spec)
                           '(slash interactive)))
            (should (eq magent--current-session parent))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-action-memory-confirm-respects-bypass-permission ()
  "Memory command confirmation continues to honor permission bypass."
  (require 'magent-action-session)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-action-session-directory nil)
         (magent-bypass-permission t)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (spec (magent-action-spec-create
                :name "memory-init"
                :title "Initialize memory"
                :exposure '(interactive)
                :session-policy 'isolated
                :workflow #'magent-test--empty-action-workflow))
         (invocation (magent-action-invocation-create
                      :id "invocation-memory"
                      :spec spec
                      :origin-scope 'global))
         approved)
    (unwind-protect
        (progn
          (magent-action-session-initialize invocation)
          (cl-letf (((symbol-function 'magent-memory--interactive-confirm)
                     (lambda (&rest _)
                       (error "interactive confirmation must be bypassed"))))
            (funcall (magent-memory--action-confirm-provider invocation 'init)
                     nil (lambda (value) (setq approved value))))
          (should approved))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-action-session-save-is-explicit-and-step-saves-once ()
  "Action persistence avoids ambient rebinding and duplicate first-step saves."
  (require 'magent-action-session)
  (let* ((ambient (magent-session-create :id "ambient"))
         (session (magent-session-create :id "action-session"))
         (scope (magent-session-action-scope
                 "action-session" "test-action" 'global))
         (spec (magent-action-spec-create
                :name "test-action"
                :title "Test action"
                :session-policy 'isolated
                :workflow #'magent-test--empty-action-workflow))
         (invocation (magent-action-invocation-create
                      :id "test-invocation"
                      :spec spec
                      :session session
                      :scope scope))
         (step (magent-action-step-create
                :type 'callback :name "First step"))
         (magent--current-session ambient)
         (magent-session--current-scope 'global)
         saves)
    (cl-letf (((symbol-function 'magent-session-save-for-session)
               (lambda (saved-session saved-scope)
                 (push (list saved-session saved-scope
                             magent--current-session
                             magent-session--current-scope)
                       saves))))
      (magent-action-session--save invocation)
      (should (= (length saves) 1))
      (pcase-let ((`(,saved-session ,saved-scope
                                  ,current-session ,current-scope)
                   (car saves)))
        (should (eq saved-session session))
        (should (equal saved-scope scope))
        (should (eq current-session ambient))
        (should (eq current-scope 'global)))
      (setq saves nil)
      (should (stringp
               (magent-action-session-start-step invocation step)))
      (should (= (length saves) 1))
      (should (eq magent--current-session ambient))
      (should (eq magent-session--current-scope 'global)))))

(ert-deftest magent-test-isolated-action-completion-preserves-current-session ()
  "Late isolated completion never restores stale ambient session state."
  (require 'magent-action-session)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-action-session-directory nil)
         (magent-action--registry nil)
         (magent-action-session--active-invocations
          (make-hash-table :test #'equal))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (parent (magent-session-create :id "session-parent"))
         (new-current (magent-session-create :id "session-new"))
         captured finish)
    (unwind-protect
        (progn
          (magent-session-install 'global parent)
          (magent-action-register
           "async-test"
           :title "Async test"
           :exposure '(interactive)
           :session-policy 'isolated
           :workflow
           (iter-lambda (invocation)
             (setq captured invocation)
             (magent-workflow-callback
                 "Wait"
                 (lambda (done)
                   (setq finish done)
                   #'ignore))))
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-runtime-context-scope)
                     (lambda () 'global))
                    ((symbol-function 'magent-runtime-prepare-context)
                     #'ignore)
                    ((symbol-function 'magent-session-save-deferred-for-session)
                     #'ignore))
            (magent-action-run "async-test")
            (magent-session-install "/tmp/magent-other-project" new-current)
            (funcall finish 'completed "Async test complete"))
          (should (eq magent--current-session new-current))
          (should (equal magent-session--current-scope
                         "/tmp/magent-other-project"))
          (should-not (magent-action-session-active-invocations)))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-action-session-viewer-leads-with-final-result ()
  "The Action-session viewer shows the result before folded activity."
  (require 'magent-action-session)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-action-session-directory nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (spec (magent-action-spec-create
                :name "viewer-test" :title "Viewer test"
                :exposure '(interactive) :session-policy 'isolated
                :workflow #'magent-test--empty-action-workflow))
         (invocation (magent-action-invocation-create
                      :id "invocation-viewer" :spec spec
                      :origin-scope 'global))
         buffer)
    (unwind-protect
        (progn
          (magent-action-session-initialize invocation)
          (let* ((step (magent-action--make-callback-step
                        "Collect diagnostics" #'ignore
                        :activity-input '(:probe "core")
                        :activity-formatter
                        (lambda (_status value) value)))
                 (item-id
                  (magent-action-session-start-step invocation step)))
            (magent-action-session-finish-step
             invocation step item-id 'completed "bounded detail"))
          (magent-action--respond
           invocation "* Diagnosis\n** Summary\nVisible result"
           (list :source 'magent-doctor-final))
          (magent-action--finish-completed invocation nil)
          (let ((file (car (magent-session-list-action-files "viewer-test"))))
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (value &rest _) value)))
              (magent-action-open-session file))
            (setq buffer
                  (get-buffer (format "*Magent Action Session: %s*"
                                      (file-name-base file)))))
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (should (derived-mode-p 'magent-action-session-mode))
            (should magent-action-session--details-hidden)
            (goto-char (point-min))
            (should (search-forward "Visible result" nil t))
            (let ((result-position (point)))
              (should (search-forward "* Activity" nil t))
              (should (< result-position (point))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-doctor-action-sends-one-tool-free-direct-request ()
  "Doctor emits its diagnosis without entering the runtime queue."
  (require 'magent-action-builtin-doctor)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-action-session-directory nil)
         (magent-action--registry nil)
         (magent-action--active-invocations (make-hash-table :test #'eq))
         (magent-action-session--active-invocations
          (make-hash-table :test #'equal))
         (magent-doctor--registry (make-hash-table :test #'equal))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-bypass-permission t)
         (parent (magent-session-create :id "session-parent"))
         (runtime (magent-runtime-session-create
                   :id "session-parent" :scope 'global
                   :magent-session parent))
         (diagnosis (concat "* Diagnosis\n** Summary\nHealthy\n"
                            "** Findings\n- None\n"
                            "** Recommended Actions\n- None\n"
                            "** Limitations\n- Test transport"))
         request events completion (sample-count 0))
    (unwind-protect
        (progn
          (magent-session-install 'global parent)
          (magent-doctor-register-probe
           "safe" :collector (lambda (_invocation _state) '((ok . t)))
           :required t)
          (let ((magent-action--allow-core-registration t))
            (magent-action-builtin-doctor-register))
          (cl-letf (((symbol-function 'magent-runtime-submit)
                     (lambda (&rest _)
                       (error "Doctor must not enter the runtime queue")))
                    ((symbol-function 'magent-session-save-deferred-for-session)
                     #'ignore)
                    ((symbol-function 'magent-llm-gptel-sample)
                     (lambda (value)
                       (setq request value)
                       (cl-incf sample-count)
                       (funcall (magent-llm-request-callback value)
                                (magent-llm-event-create
                                 'completed :text diagnosis))
                       nil)))
            (magent-action-invoke
             "doctor" runtime
             :observer (lambda (event) (push event events))
             :on-complete (lambda (status result)
                            (setq completion (list status result)))))
          (ert-info ((format "Doctor completion: %S" completion))
            (should (= sample-count 1)))
          (should-not (magent-llm-request-tools request))
          (should-not (magent-llm-request-stream request))
          (should (eq (car completion) 'completed))
          (should (cl-find 'assistant-delta events
                           :key (lambda (event) (plist-get event :type))))
          (let* ((file (car (magent-session-list-action-files "doctor")))
                 (meta (magent-session--read-file-metadata-cached file)))
            (should (equal (plist-get meta :status) "completed"))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-doctor-action-unsafe-probe-fails-before-sampling ()
  "Doctor rejects unsafe probe values before provider sampling."
  (require 'magent-action-builtin-doctor)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-action-session-directory nil)
         (magent-action--registry nil)
         (magent-action-session--active-invocations
          (make-hash-table :test #'equal))
         (magent-doctor--registry (make-hash-table :test #'equal))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-bypass-permission t)
         sampled)
    (unwind-protect
        (progn
          (magent-doctor-register-probe
           "unsafe" :collector (lambda (_invocation _state) (current-buffer))
           :required t)
          (let ((magent-action--allow-core-registration t))
            (magent-action-builtin-doctor-register))
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-runtime-context-scope)
                     (lambda () 'global))
                    ((symbol-function 'magent-runtime-prepare-context)
                     #'ignore)
                    ((symbol-function 'magent-session-save-deferred-for-session)
                     #'ignore)
                    ((symbol-function 'magent-llm-gptel-sample)
                     (lambda (_request) (setq sampled t))))
            (magent-action-run "doctor"))
          (should-not sampled)
          (let* ((file (car (magent-session-list-action-files "doctor")))
                 (meta (magent-session--read-file-metadata-cached file)))
            (should (equal (plist-get meta :status) "failed"))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-doctor-action-active-request-is-cancellable ()
  "Cancelling by parent session aborts Doctor's direct request handle."
  (require 'magent-action-builtin-doctor)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-action-session-directory nil)
         (magent-action--registry nil)
         (magent-action--active-invocations (make-hash-table :test #'eq))
         (magent-action-session--active-invocations
          (make-hash-table :test #'equal))
         (magent-doctor--registry (make-hash-table :test #'equal))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-bypass-permission t)
         (parent (magent-session-create :id "session-parent"))
         (runtime (magent-runtime-session-create
                   :id "session-parent" :scope 'global
                   :magent-session parent))
         (request-buffer (generate-new-buffer " *doctor-cancel-test*"))
         aborted invocation)
    (unwind-protect
        (progn
          (magent-session-install 'global parent)
          (magent-doctor-register-probe
           "safe" :collector (lambda (_invocation _state) '((ok . t)))
           :required t)
          (let ((magent-action--allow-core-registration t))
            (magent-action-builtin-doctor-register))
          (cl-letf (((symbol-function 'magent-llm-gptel-sample)
                     (lambda (_request) request-buffer))
                    ((symbol-function 'gptel-abort)
                     (lambda (buffer) (setq aborted buffer)))
                    ((symbol-function 'magent-runtime-cancel) #'ignore)
                    ((symbol-function 'magent-session-save-deferred-for-session)
                     #'ignore))
            (setq invocation (magent-action-invoke "doctor" runtime))
            (should (magent-action-cancel-session runtime)))
          (should (eq aborted request-buffer))
          (should-not (buffer-live-p request-buffer))
          (should (eq (magent-action-invocation-status invocation) 'cancelled))
          (let* ((file (car (magent-session-list-action-files "doctor")))
                 (meta (magent-session--read-file-metadata-cached file)))
            (should (equal (plist-get meta :status) "cancelled"))))
      (when (buffer-live-p request-buffer) (kill-buffer request-buffer))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-memory-system-message-selects-relevant-sections ()
  "Test prompt-time memory injection selects bounded relevant sections."
  (require 'magent-action-builtin-memory)
  (let* ((memory-dir (file-name-as-directory
                      (make-temp-file "magent-memory-store" t)))
         (magent-memory-directory memory-dir)
         (magent-memory-enable-auto-injection t)
         (magent-memory-max-injected-sections 2)
         (magent-memory-injection-max-chars 2000))
    (make-directory memory-dir t)
    (with-temp-file (magent-memory-file)
      (insert "#+magent-active: true\n")
      (insert "#+magent-generated-at: 2026-07-09T00:00:00+0800\n")
      (insert "#+magent-generated-at-float: 1783526400.000\n")
      (insert "#+magent-roots-json: []\n")
      (insert "#+magent-source-files-json: []\n\n")
      (insert "* Magent Managed Profile\n")
      (dolist (heading magent-memory--managed-section-headings)
        (insert "** " heading "\n")
        (insert "Body for " heading ".\n"))
      (insert "* User Notes\n")
      (insert "For magent completion work, prefer concise status updates.\n"))
    (let ((message (magent-memory-system-message
                    "debug magent completion workflow"
                    nil
                    "/tmp/magent")))
      (should message)
      (should (string-match-p
               (regexp-quote "* Magent Emacs Profile Memory")
               message))
      (should (string-match-p (regexp-quote "User Notes") message))
      (should (<= (length message) 2100)))
    (should-not
     (magent-memory-system-message
      "ignore magent memory and debug completion"
      nil
      "/tmp/magent"))
    (should-not
     (magent-memory-system-message
      "review this git config"
      nil
      "/tmp/project"))))

(ert-deftest magent-test-agent-run-turn-keeps-streaming-for-tool-requests ()
  "Test tool-enabled requests still use streaming provider sampling."
  (require 'magent-agent)
  (let* ((backend (gptel-make-openai "tools" :key "key"))
         (gptel-backend backend)
         (gptel-model 'tool-model)
         (agent (magent-agent-info-create
                 :name "build"
                 :mode 'primary
                 :permission (magent-permission-defaults)))
         (tool-runtime
          (gptel-make-tool
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function #'ignore
           :async t))
         captured-loop)
    (cl-letf (((symbol-function 'magent-agent-loop-start)
               (lambda (loop)
                 (setq captured-loop loop)
                 'started))
              ((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _args) (list tool-runtime)))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore))
      (magent-session-reset)
      (magent-test--run-turn "use a tool" nil agent))
    (let ((request (magent-agent-loop-request captured-loop)))
      (should (magent-llm-request-tools request))
      (should (magent-llm-request-stream request)))))

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

(ert-deftest magent-test-llm-gptel-applies-top-p-metadata-openai ()
  "Test the gptel adapter maps top-p to OpenAI request params."
  (require 'magent-llm-gptel)
  (let ((backend (gptel-make-openai "test" :key "key"))
        (gptel--request-params '(:seed 7))
        captured-params)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (setq captured-params gptel--request-params)
                 (funcall (plist-get kwargs :callback)
                          t
                          (list :content "ok")))))
      (magent-llm-gptel-sample
       (magent-llm-request-create
        :prompt '("hello")
        :system "sys"
        :backend backend
        :model 'test-model
        :stream t
        :metadata '(:top-p 0.37)
        :callback #'ignore)))
    (should (= (plist-get captured-params :top_p) 0.37))
    (should (= (plist-get captured-params :seed) 7))))

(ert-deftest magent-test-llm-gptel-suppresses-connect-headers-for-managed-proxy ()
  "Managed proxied requests suppress proxy CONNECT response headers."
  (require 'magent-llm-gptel)
  (let ((gptel-proxy "http://127.0.0.1:10808"))
    (should
     (equal
      (magent-llm-gptel--suppress-connect-headers-a
       (lambda (_info) '("--base"))
       '(:magent-llm-gptel t))
      '("--base" "--suppress-connect-headers")))))

(ert-deftest magent-test-llm-gptel-does-not-duplicate-connect-header-argument ()
  "Managed proxy suppression is inserted at most once."
  (require 'magent-llm-gptel)
  (let ((gptel-proxy "http://127.0.0.1:10808"))
    (should
     (equal
      (magent-llm-gptel--suppress-connect-headers-a
       (lambda (_info) '("--base" "--suppress-connect-headers"))
       '(:magent-llm-gptel t))
      '("--base" "--suppress-connect-headers")))))

(ert-deftest magent-test-llm-gptel-leaves-unproxied-managed-curl-args-unchanged ()
  "Managed requests without a proxy keep gptel's curl arguments."
  (require 'magent-llm-gptel)
  (let ((gptel-proxy ""))
    (should
     (equal
      (magent-llm-gptel--suppress-connect-headers-a
       (lambda (_info) '("--base"))
       '(:magent-llm-gptel t))
      '("--base")))))

(ert-deftest magent-test-llm-gptel-leaves-unmanaged-proxy-curl-args-unchanged ()
  "Unmanaged gptel requests keep their original curl arguments."
  (require 'magent-llm-gptel)
  (let ((gptel-proxy "http://127.0.0.1:10808"))
    (should
     (equal
      (magent-llm-gptel--suppress-connect-headers-a
       (lambda (_info) '("--base"))
       '(:other-client t))
      '("--base")))))

(ert-deftest magent-test-llm-gptel-installs-connect-header-advice-once ()
  "Repeated boundary initialization installs proxy advice once."
  (require 'magent-llm-gptel)
  (let (installed
        (add-count 0))
    (cl-letf
        (((symbol-function 'advice-member-p)
          (lambda (function symbol)
            (if (and
                 (eq function
                     #'magent-llm-gptel--suppress-connect-headers-a)
                 (eq symbol 'gptel-curl--get-config-args))
                installed
              t)))
         ((symbol-function 'advice-add)
          (lambda (symbol _where function &optional _props)
            (when (and
                   (eq function
                       #'magent-llm-gptel--suppress-connect-headers-a)
                   (eq symbol 'gptel-curl--get-config-args))
              (setq installed t)
              (cl-incf add-count)))))
      (magent-llm-gptel--install-boundary-advice)
      (magent-llm-gptel--install-boundary-advice))
    (should (= add-count 1))))

(ert-deftest magent-test-llm-gptel-merges-top-p-into-gemini-data ()
  "Test Gemini top-p preserves gptel's generated sampling config."
  (require 'magent-llm-gptel)
  (require 'gptel-gemini)
  (let* ((backend (gptel-make-gemini "test" :key "key"))
         (info (list :backend backend
                     :context '(:magent-llm-gptel t :top-p 0.37)
                     :data '(:generationConfig
                             (:temperature 0.2 :maxOutputTokens 100)))))
    (magent-llm-gptel--apply-top-p-to-info info)
    (let ((config (plist-get (plist-get info :data) :generationConfig)))
      (should (= (plist-get config :topP) 0.37))
      (should (= (plist-get config :temperature) 0.2))
      (should (= (plist-get config :maxOutputTokens) 100)))))

(ert-deftest magent-test-llm-gptel-applies-effort-metadata-openai-responses ()
  "Test the gptel adapter maps effort to OpenAI Responses request params."
  (require 'magent-llm-gptel)
  (require 'gptel-openai-responses)
  (let ((backend (gptel-make-openai-responses "responses" :key "key"))
        captured-params)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (setq captured-params gptel--request-params)
                 (funcall (plist-get kwargs :callback)
                          t
                          (list :content "ok")))))
      (magent-llm-gptel-sample
       (magent-llm-request-create
        :prompt '("hello")
        :system "sys"
        :backend backend
        :model 'gpt-5
        :stream t
        :metadata '(:effort xhigh)
        :callback #'ignore)))
    (should (equal captured-params '(:reasoning (:effort "xhigh"))))))

(ert-deftest magent-test-llm-gptel-downgrades-xhigh-for-openai-chat ()
  "Test OpenAI-compatible chat effort maps xhigh according to policy."
  (require 'magent-llm-gptel)
  (let ((backend (gptel-make-openai
                  "chat"
                  :host "openai-compatible.local"
                  :key "key"))
        (magent-effort-unsupported-policy 'warn-and-downgrade)
        captured-params)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (setq captured-params gptel--request-params)
                 (funcall (plist-get kwargs :callback)
                          t
                          (list :content "ok"))))
              ((symbol-function 'magent-log) #'ignore))
      (magent-llm-gptel-sample
       (magent-llm-request-create
        :prompt '("hello")
        :system "sys"
        :backend backend
        :model 'o3
        :stream t
        :metadata '(:effort xhigh)
        :callback #'ignore)))
    (should (equal captured-params '(:reasoning_effort "high")))))

(ert-deftest magent-test-prompt-read-is-file-backed ()
  "Test prompt resources are read from the configured directory on demand."
  (require 'magent-prompt)
  (let* ((directory (make-temp-file "magent-prompts-" t))
         (magent-prompt-directory directory)
         (file (expand-file-name "sample.org" directory)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "first {{value}} 100%\n"))
          (should (equal (magent-prompt-render
                          "sample.org" '((value . "pass")))
                         "first pass 100%"))
          (with-temp-file file
            (insert "second\n"))
          (should (equal (magent-prompt-read "sample.org") "second"))
          (should-error (magent-prompt-path "../outside.org")))
      (delete-directory directory t))))

(ert-deftest magent-test-prompt-manifest-covers-bundled-org-resources ()
  "Test every bundled Org prompt is declared once in the package manifest."
  (require 'magent-prompt)
  (let* ((manifest (magent-prompt-manifest))
         (actual
          (mapcar (lambda (file)
                    (file-relative-name file magent-prompt-directory))
                  (directory-files-recursively
                   magent-prompt-directory "\\.org\\'")))
         (sorted-manifest (sort (copy-sequence manifest) #'string<)))
    (should (= (length manifest)
               (length (delete-dups (copy-sequence manifest)))))
    (should (equal sorted-manifest (sort actual #'string<)))
    (should (member "internal/runtime-policy.org" manifest))
    (should (member "internal/project-instructions.org" manifest))))

(ert-deftest magent-test-project-instructions-follow-target-scope ()
  "Test AGENTS discovery walks root-to-target and ignores sibling scopes."
  (require 'magent-project-instructions)
  (let* ((root (file-name-as-directory
                (file-truename (make-temp-file "magent-instructions-" t))))
         (target-dir (expand-file-name "src/nested" root))
         (sibling-dir (expand-file-name "docs" root))
         (target (expand-file-name "module.el" target-dir))
         (root-agents (expand-file-name "AGENTS.md" root))
         (nested-agents (expand-file-name "src/AGENTS.md" root))
         (sibling-agents (expand-file-name "AGENTS.md" sibling-dir))
         (magent-project-instructions-max-bytes 4096))
    (unwind-protect
        (progn
          (make-directory target-dir t)
          (make-directory sibling-dir t)
          (with-temp-file target (insert "code"))
          (with-temp-file root-agents (insert "Root rule."))
          (with-temp-file nested-agents (insert "Source rule."))
          (with-temp-file sibling-agents (insert "Docs-only rule."))
          (let* ((entries
                  (magent-project-instructions-discover
                   root (list :file-path target)))
                 (files (mapcar (lambda (entry)
                                  (file-relative-name
                                   (plist-get entry :file) root))
                                entries))
                 (message
                  (magent-project-instructions-system-message
                   root (list :file-path target))))
            (should (equal files '("AGENTS.md" "src/AGENTS.md")))
            (should (< (string-match "Root rule" message)
                       (string-match "Source rule" message)))
            (should-not (string-match-p "Docs-only rule" message))))
      (delete-directory root t))))

(ert-deftest magent-test-project-instructions-reject-symlink-escape ()
  "Test project instruction discovery ignores files resolving outside root."
  (require 'magent-project-instructions)
  (let* ((root (file-name-as-directory
                (file-truename (make-temp-file "magent-instructions-" t))))
         (outside (make-temp-file "magent-outside-instructions-" t))
         (target-dir (expand-file-name "src" root))
         (target (expand-file-name "module.el" target-dir))
         (root-agents (expand-file-name "AGENTS.md" root))
         (outside-agents (expand-file-name "AGENTS.md" outside))
         (nested-link (expand-file-name "AGENTS.md" target-dir))
         (magent-project-instructions-max-bytes 4096))
    (unwind-protect
        (progn
          (make-directory target-dir t)
          (with-temp-file target (insert "code"))
          (with-temp-file root-agents (insert "Root rule."))
          (with-temp-file outside-agents (insert "Escaped rule."))
          (make-symbolic-link outside-agents nested-link)
          (let ((entries
                 (magent-project-instructions-discover
                  root (list :file-path target))))
            (should (= (length entries) 1))
            (should (equal (plist-get (car entries) :content) "Root rule."))))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest magent-test-project-instructions-respect-total-byte-limit ()
  "Test project instruction discovery enforces its aggregate byte budget."
  (require 'magent-project-instructions)
  (let* ((root (file-name-as-directory
                (file-truename (make-temp-file "magent-instructions-" t))))
         (target-dir (expand-file-name "src" root))
         (target (expand-file-name "module.el" target-dir))
         (magent-project-instructions-max-bytes 3))
    (unwind-protect
        (progn
          (make-directory target-dir t)
          (with-temp-file target (insert "code"))
          (with-temp-file (expand-file-name "AGENTS.md" root)
            (insert "12345"))
          (with-temp-file (expand-file-name "AGENTS.md" target-dir)
            (insert "nested"))
          (let ((entries
                 (magent-project-instructions-discover
                  root (list :file-path target))))
            (should (= (length entries) 1))
            (should (equal (plist-get (car entries) :content) "123"))))
      (delete-directory root t))))

(ert-deftest magent-test-project-instructions-remain-json-safe-at-unicode-cutoff ()
  "Test decoded project instructions remain serializable at a byte cutoff."
  (require 'magent-project-instructions)
  (let* ((root (file-name-as-directory
                (file-truename (make-temp-file "magent-instructions-" t))))
         (target (expand-file-name "module.el" root))
         ;; UTF-8 encodes this as six bytes, so four bytes cut the second
         ;; character after its leading byte.
         (magent-project-instructions-max-bytes 4))
    (unwind-protect
        (progn
          (with-temp-file target (insert "code"))
          (with-temp-file (expand-file-name "AGENTS.md" root)
            (insert "中文"))
          (let* ((entries
                  (magent-project-instructions-discover
                   root (list :file-path target)))
                 (content (plist-get (car entries) :content))
                 (message
                  (magent-project-instructions-system-message
                   root (list :file-path target))))
            (should (string-prefix-p "中" content))
            (should-not
             (seq-find (lambda (char)
                         (eq (char-charset char) 'eight-bit))
                       content))
            (should (stringp (json-serialize message)))))
      (delete-directory root t))))

(ert-deftest magent-test-project-instructions-canonicalize-symlinked-root ()
  "Test rendered instruction paths stay relative to a symlinked project root."
  (require 'magent-project-instructions)
  (let* ((real-root (file-name-as-directory
                     (file-truename (make-temp-file
                                     "magent-instructions-real-" t))))
         (link-parent (make-temp-file "magent-instructions-link-" t))
         (linked-root (expand-file-name "project" link-parent))
         (target (expand-file-name "module.el" real-root))
         (magent-project-instructions-max-bytes 4096))
    (unwind-protect
        (progn
          (with-temp-file target (insert "code"))
          (with-temp-file (expand-file-name "AGENTS.md" real-root)
            (insert "Root rule."))
          (make-symbolic-link real-root linked-root)
          (let ((message
                 (magent-project-instructions-system-message
                  linked-root (list :file-path target))))
            (should (string-match-p
                     (regexp-quote "** AGENTS.md (scope: ./)") message))
            (should-not (string-match-p "\\.\\./" message))))
      (delete-directory link-parent t)
      (delete-directory real-root t))))

(ert-deftest magent-test-default-system-prompt-uses-calibrated-contract ()
  "Test the default prompt stays dynamic and avoids stale host conventions."
  (require 'magent-prompt)
  (let ((prompt (magent-prompt-read "system.org")))
    (should (string-match-p "Match detail to the task" prompt))
    (should (string-match-p "capability resolver" prompt))
    (should (string-match-p "Do not claim.*succeeded" prompt))
    (should (string-match-p
             "intended tests were actually collected and exited successfully"
             prompt))
    (should (string-match-p "zero tests does not count as verification" prompt))
    (should (string-match-p
             "final diff, affected callers, and observable invariants" prompt))
    (should (string-match-p "remaining failed validation" prompt))
    (should-not (string-match-p "fewer than 4 lines" prompt))
    (should-not (string-match-p "One word answers are best" prompt))
    (should-not (string-match-p "<system-reminder>" prompt))
    (should-not (string-match-p "Available Tools:" prompt))
    (should-not (string-match-p "Built-in Skill files" prompt))))

(ert-deftest magent-test-build-agent-prompt-defines-debugging-protocol ()
  "Test the build role adds causal verification and conditional delegation."
  (require 'magent-agent-builtins)
  (let ((prompt (magent-agent-info-prompt
                 (magent-agent-builtins--build))))
    (should (string-match-p "exact operation order" prompt))
    (should (string-match-p "potentially state-changing" prompt))
    (should (string-match-p "observable invariants" prompt))
    (should (string-match-p "causal checklist" prompt))
    (should (string-match-p "direct[[:space:]]+assertion" prompt))
    (should (string-match-p "prove it[[:space:]]+irrelevant" prompt))
    (should (string-match-p "arbitrary in-range sentinel" prompt))
    (should (string-match-p "every[[:space:]]+state derived" prompt))
    (should (string-match-p "compatibility oracle" prompt))
    (should (string-match-p "immediately after construction" prompt))
    (should (string-match-p "Spawn an explore agent when" prompt))
    (should (string-match-p "collected zero tests" prompt))))

(ert-deftest magent-test-memory-prompt-declares-precedence ()
  "Test injected profile memory cannot silently override current state."
  (require 'magent-prompt)
  (let ((prompt (magent-prompt-render
                 "internal/memory-injection.org"
                 '((memory . "Stored preference.")))))
    (should (string-match-p "incomplete or stale" prompt))
    (should (string-match-p "live Emacs or repository state take precedence"
                            prompt))
    (should (string-suffix-p "Stored preference." prompt))))

(ert-deftest magent-test-builtin-agents-count ()
  "Test that all 7 built-in agents are created."
  (require 'magent-agent-builtins)
  (let ((agents (magent-agent-builtins-list)))
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
  (require 'magent-agent-builtins)
  (dolist (agent (magent-agent-builtins-list))
    (should (magent-agent-info-valid-p agent))
    (should (magent-agent-info-native agent))))

(ert-deftest magent-test-builtin-agents-live-eval-policy ()
  "Only interactive built-ins expose live eval, still subject to approval."
  (require 'magent-agent-builtins)
  (let ((agents (magent-agent-builtins-list)))
    (dolist (name '("build" "general"))
      (let ((agent (cl-find name agents
                            :key #'magent-agent-info-name
                            :test #'equal)))
        (should (eq (magent-permission-resolve
                     (magent-agent-info-permission agent)
                     'emacs_eval_live)
                    'ask))))
    (dolist (name '("plan" "explore" "compaction" "title" "summary"))
      (let ((agent (cl-find name agents
                            :key #'magent-agent-info-name
                            :test #'equal)))
        (should (eq (magent-permission-resolve
                     (magent-agent-info-permission agent)
                     'emacs_eval_live)
                    'deny))))))

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
  (should-error (magent-agent-file--parse-mode "unknown")))

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
                       :effort 'xhigh
                       :prompt "System prompt here."))
               (filepath (magent-agent-file-save agent tmpdir)))
          (should (file-exists-p filepath))
          ;; Reload
          (let ((loaded (magent-agent-file-load filepath)))
            (should loaded)
            (should (equal (magent-agent-info-description loaded) "Roundtrip test"))
            (should (eq (magent-agent-info-mode loaded) 'subagent))
            (should (= (magent-agent-info-temperature loaded) 0.5))
            (should (eq (magent-agent-info-effort loaded) 'xhigh))
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
     (magent-skill-create :name "b" :type 'instruction))
    (let ((names (magent-skills-list)))
      (should (= (length names) 2))
      (should (member "a" names))
      (should (member "b" names)))))

(ert-deftest magent-test-skills-registry-rejects-tool-type ()
  "Test executable tool-type skills cannot enter the registry."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (magent-skills-register
     (magent-skill-create :name "inst1" :type 'instruction))
    (magent-skills-register
     (magent-skill-create :name "inst2" :type 'instruction))
    (should (= (length (magent-skills-list-by-type 'instruction)) 2))
    (should-error
     (magent-skills-register
      (magent-skill-create :name "tool1" :type 'tool)))
    (should-not (magent-skills-list-by-type 'tool))))

(ert-deftest magent-test-skills-descriptors-are-frontend-neutral ()
  "Descriptors expose instruction skills without requiring a default prompt."
  (require 'magent-skills)
  (let* ((magent-skills--registry nil)
        (magent-skills--scope-catalog (make-hash-table :test #'equal))
        (magent-runtime--active-project-scope nil)
        (project-root (make-temp-file "magent-skill-project-" t)))
    (unwind-protect
        (progn
          (magent-skills-register
           (magent-skill-create
            :name "reviewer"
            :description "Review code."
            :type 'instruction))
          (magent-skills-register
           (magent-skill-create
            :name "project-only"
            :description "Needs a project."
            :type 'instruction
            :requires-project t))
          (let ((global
                 (magent-skills-list-descriptors 'global 'instruction))
                (project
                 (magent-skills-list-descriptors project-root 'instruction)))
            (should (equal (mapcar #'magent-skill-descriptor-name global)
                           '("reviewer")))
            (should
             (equal (mapcar #'magent-skill-descriptor-name project)
                    '("project-only" "reviewer")))
            (should (equal
                     (magent-skill-descriptor-description (car global))
                     "Review code."))))
      (delete-directory project-root t))))

(ert-deftest magent-test-skills-descriptors-retain-inactive-project-scopes ()
  "Project skill catalogs remain exact while another scope is active."
  (require 'magent-skills)
  (let* ((magent-skills--registry nil)
         (magent-skills--scope-catalog (make-hash-table :test #'equal))
         (magent-runtime--active-project-scope nil)
         (project-a (file-truename
                     (directory-file-name
                      (make-temp-file "magent-skill-project-a-" t))))
         (project-b (file-truename
                     (directory-file-name
                      (make-temp-file "magent-skill-project-b-" t)))))
    (unwind-protect
        (progn
          (magent-skills-register
           (magent-skill-create
            :name "policy" :description "Global policy."
            :type 'instruction :source-layer 'user))
          (magent-skills-register
           (magent-skill-create
            :name "policy" :description "Project A policy."
            :type 'instruction :source-layer 'project
            :source-scope project-a))
          (magent-skills-remove-project-scope project-a)
          (magent-skills-register
           (magent-skill-create
            :name "policy" :description "Project B policy."
            :type 'instruction :source-layer 'project
            :source-scope project-b))
          (should
           (equal
            (magent-skill-descriptor-description
             (magent-skills-resolve-descriptor "policy" project-a))
            "Project A policy."))
          (should
           (equal
            (magent-skill-descriptor-description
             (magent-skills-resolve-descriptor "policy" project-b))
            "Project B policy."))
          (should
           (equal
            (magent-skill-descriptor-description
             (magent-skills-resolve-descriptor "policy" 'global))
            "Global policy.")))
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest magent-test-skills-clear ()
  "Test clearing all skills."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (magent-skills-register
     (magent-skill-create :name "x" :type 'instruction))
    (magent-skills-clear)
    (should (null magent-skills--registry))))

(ert-deftest magent-test-skills-register-builtin ()
  "Test code-defined builtin skill registration and canonical tool examples."
  (require 'magent-skills)
  (require 'magent-tools)
  (let ((magent-skills--registry nil))
    (cl-letf (((symbol-function 'magent-log) #'ignore))
      (magent-skills--register-builtin))
    (should (null (magent-skills-get "emacs")))
    (let ((skill (magent-skills-get "skill-creator")))
      (should skill)
      (should (eq (magent-skill-type skill) 'instruction))
      (let ((prompt (magent-skill-prompt skill)))
        (should (string-match "tools: \\[\\([^]]+\\)\\]" prompt))
        (dolist (name (split-string (match-string 1 prompt)
                                    "[[:space:],]+" t))
          (should (magent-tools-catalog-entry name)))))))

(ert-deftest magent-test-skills-load-order-preserves-directory-precedence ()
  "Test later skill directories override earlier directories deterministically."
  (require 'magent-skills)
  (let* ((root (make-temp-file "magent-skill-order-" t))
         ;; Deliberately choose reverse-lexical directory names so a global
         ;; pathname sort would produce the wrong winner.
         (builtin-dir (expand-file-name "z-builtin" root))
         (user-dir (expand-file-name "y-user" root))
         (canonical-dir (expand-file-name "x-canonical" root))
         (directories (list builtin-dir user-dir canonical-dir))
         (magent-skills--registry nil)
         (magent-skills--builtin-dir builtin-dir)
         (magent-skill-directories (list user-dir canonical-dir)))
    (unwind-protect
        (progn
          (cl-loop for directory in directories
                   for description in '("builtin" "user" "canonical")
                   do
                   (let ((skill-dir (expand-file-name "same-skill" directory)))
                     (make-directory skill-dir t)
                     (with-temp-file (expand-file-name "SKILL.md" skill-dir)
                       (insert (format
                                "---\nname: same-skill\ndescription: %s\ntype: instruction\n---\n%s\n"
                                description description)))))
          (cl-letf (((symbol-function 'magent-log) #'ignore))
            (magent-skills-load-all directories))
          (should (equal
                   (magent-skill-description
                    (magent-skills-get "same-skill"))
                   "canonical")))
      (delete-directory root t))))

(ert-deftest magent-test-skills-instruction-prompt-includes-source-directory ()
  "Test file-backed skill prompts expose their resource base directory."
  (require 'magent-skills)
  (let* ((directory (make-temp-file "magent-skill-source-" t))
         (skill-file (expand-file-name "SKILL.md" directory))
         (magent-skills--registry nil))
    (unwind-protect
        (progn
          (magent-skills-register
           (magent-skill-create
            :name "resource-skill"
            :description "Uses references"
            :type 'instruction
            :prompt "Read references/guide.md."
            :file-path skill-file
            :source-layer 'user))
          (let ((prompt (car (magent-skills-get-instruction-prompts
                              '("resource-skill")))))
            (should (string-match-p
                     (regexp-quote
                      (format "Skill directory: %s"
                              (file-name-as-directory directory)))
                     prompt))))
      (delete-directory directory t))))

(ert-deftest magent-test-skill-manager-parses-and-ranks-search-results ()
  "Finder keeps the most-installed skills and preserves install sources."
  (require 'magent-skill-manager)
  (let* ((magent-skill-search-limit 2)
         (response
          "{\"skills\":[{\"id\":\"one/a\",\"name\":\"a\",\"installs\":3,\"source\":\"one/repo\"},{\"id\":\"two/b\",\"name\":\"b\",\"installs\":20,\"source\":\"two/repo\"},{\"id\":\"three/c\",\"name\":\"c\",\"installs\":10,\"source\":\"three/repo\"}]}" )
         (results (magent-skill-manager--parse-search-response response)))
    (should (equal (mapcar #'magent-skill-candidate-name results)
                   '("b" "c")))
    (should (equal (magent-skill-candidate-source (car results))
                   "two/repo"))))

(ert-deftest magent-test-skill-manager-git-is-local-under-remote-cwd ()
  "Skill installation never sends its temporary checkout command to TRAMP."
  (require 'magent-skill-manager)
  (let ((default-directory "/ssh:test.invalid:/srv/project/")
        process-directory)
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _args)
                 (setq process-directory default-directory)
                 0)))
      (should (equal (magent-skill-manager--call-git "--version") "")))
    (should-not (file-remote-p process-directory))))

(ert-deftest magent-test-skill-manager-preflight-rejects-tool-skills ()
  "External tool skills are rejected before installation writes anything."
  (require 'magent-skill-manager)
  (let ((directory (make-temp-file "magent-tool-skill-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "SKILL.md" directory)
            (insert "---\nname: unsafe-tool\ndescription: Tool skill\ntype: tool\n---\nBody\n"))
          (should-error
           (magent-skill-manager--preflight-directory
            directory (list :source-kind 'local :source directory))
           :type 'user-error))
      (delete-directory directory t))))

(ert-deftest magent-test-skill-manager-preflight-rejects-symbolic-links ()
  "Preflight rejects links so installation cannot copy outside content."
  (require 'magent-skill-manager)
  (let ((directory (make-temp-file "magent-linked-source-" t))
        (outside (make-temp-file "magent-linked-file-")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "SKILL.md" directory)
            (insert "---\nname: linked-source\ndescription: Linked source\n---\nBody\n"))
          (make-symbolic-link outside (expand-file-name "outside" directory))
          (should-error
           (magent-skill-manager--preflight-directory
            directory (list :source-kind 'local :source directory))
           :type 'user-error))
      (delete-directory directory t)
      (delete-file outside))))

(ert-deftest magent-test-skill-manager-rejects-github-path-escape ()
  "A GitHub tree subdirectory cannot escape its temporary checkout."
  (require 'magent-skill-manager)
  (let ((checkout (make-temp-file "magent-checkout-" t)))
    (unwind-protect
        (should-error
         (magent-skill-manager--find-skill-directory
          checkout nil "../outside")
         :type 'user-error)
      (delete-directory checkout t))))

(ert-deftest magent-test-skill-manager-installs-local-copy-with-provenance ()
  "Local installs copy resources, write provenance, and reload skills."
  (require 'magent-skill-manager)
  (let* ((root (make-temp-file "magent-skill-root-" t))
         (source (make-temp-file "magent-skill-source-" t))
         (magent-skill-directories (list root))
         (reload-count 0))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "SKILL.md" source)
            (insert "---\nname: copied-skill\ndescription: Copy me\n---\nUse references.\n"))
          (make-directory (expand-file-name "references" source))
          (with-temp-file (expand-file-name "references/guide.md" source)
            (insert "guide"))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'magent-skills-reload)
                     (lambda () (cl-incf reload-count))))
            (magent-install-skill source))
          (let ((destination (expand-file-name "copied-skill" root)))
            (should (file-exists-p (expand-file-name "SKILL.md" destination)))
            (should (file-exists-p
                     (expand-file-name "references/guide.md" destination)))
            (should (file-exists-p
                     (expand-file-name ".magent-install.json" destination)))
            (should-not (file-symlink-p destination))
            (should (= reload-count 1))))
      (delete-directory root t)
      (delete-directory source t))))

(ert-deftest magent-test-skill-manager-rolls-back-when-reload-fails ()
  "A failed registry reload leaves no partially installed new skill."
  (require 'magent-skill-manager)
  (let* ((root (make-temp-file "magent-skill-rollback-root-" t))
         (source (make-temp-file "magent-skill-rollback-source-" t))
         (magent-skill-directories (list root)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "SKILL.md" source)
            (insert "---\nname: rollback-skill\ndescription: Roll back\n---\nBody\n"))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'magent-skills-reload)
                     (lambda () (error "reload failed"))))
            (should-error (magent-install-skill source)))
          (should-not (file-exists-p
                       (expand-file-name "rollback-skill" root))))
      (delete-directory root t)
      (delete-directory source t))))

(ert-deftest magent-test-skill-manager-reinstalls-only-the-same-source ()
  "Managed same-source installs replace atomically; unmanaged collisions fail."
  (require 'magent-skill-manager)
  (let* ((root (make-temp-file "magent-skill-reinstall-root-" t))
         (source (make-temp-file "magent-skill-reinstall-source-" t))
         (other (make-temp-file "magent-skill-unmanaged-source-" t))
         (magent-skill-directories (list root))
         (reload-count 0))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "SKILL.md" source)
            (insert "---\nname: reinstall-skill\ndescription: Reinstall\n---\nVersion one\n"))
          (with-temp-file (expand-file-name "SKILL.md" other)
            (insert "---\nname: unmanaged-skill\ndescription: Unmanaged\n---\nBody\n"))
          (make-directory (expand-file-name "unmanaged-skill" root))
          (with-temp-file (expand-file-name "unmanaged-skill/SKILL.md" root)
            (insert "---\nname: unmanaged-skill\ndescription: Existing\n---\nBody\n"))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'magent-skills-reload)
                     (lambda () (cl-incf reload-count))))
            (magent-install-skill source)
            (with-temp-file (expand-file-name "SKILL.md" source)
              (insert "---\nname: reinstall-skill\ndescription: Reinstall\n---\nVersion two\n"))
            (magent-install-skill source)
            (should-error (magent-install-skill other) :type 'user-error))
          (should (= reload-count 2))
          (with-temp-buffer
            (insert-file-contents
             (expand-file-name "reinstall-skill/SKILL.md" root))
            (should (search-forward "Version two" nil t))))
      (delete-directory root t)
      (delete-directory source t)
      (delete-directory other t))))

(ert-deftest magent-test-skill-manager-delete-confirms-once-and-unlinks-symlink ()
  "Deletion asks once and never follows an unmanaged skill symlink."
  (require 'magent-skill-manager)
  (let* ((root (make-temp-file "magent-skill-delete-root-" t))
         (outside (make-temp-file "magent-skill-delete-target-" t))
         (link (expand-file-name "linked-skill" root))
         (magent-skill-directories (list root))
         (confirm-count 0)
         (reload-count 0))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "SKILL.md" outside)
            (insert "---\nname: linked-skill\ndescription: Linked\n---\nBody\n"))
          (make-symbolic-link outside link)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "linked-skill"))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (cl-incf confirm-count) t))
                    ((symbol-function 'magent-skills-reload)
                     (lambda () (cl-incf reload-count))))
            (magent-delete-skill))
          (should (= confirm-count 1))
          (should (= reload-count 1))
          (should-not (file-symlink-p link))
          (should (file-exists-p (expand-file-name "SKILL.md" outside))))
      (when (file-symlink-p link) (delete-file link))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest magent-test-bundled-commands-are-elisp-native ()
  "Test bundled workflow commands register without same-name skills."
  (require 'magent-action)
  (require 'magent-skills)
  (let ((magent-action--registry nil)
        (magent-skills--registry nil))
    (magent-test--load-builtin-skills-only)
    (magent-test--register-builtin-commands-only)
    (dolist (name magent-test--builtin-slash-command-names)
      (let ((command (magent-action-get name)))
        (should command)
        (should (eq (magent-action-spec-source-layer command) 'builtin))
        (should (functionp (magent-action-spec-workflow command)))
        (should-not (magent-skills-get name))))))

(ert-deftest magent-test-action-enabled-builtins-default-to-doctor-and-memory ()
  "Doctor and Memory Actions remain available by default."
  (should (equal (default-value 'magent-action-enabled-builtins)
                 '(doctor memory))))

(ert-deftest magent-test-action-builtins-respect-enabled-groups ()
  "Optional built-in groups control only Doctor and Memory registrations."
  (require 'magent-action-builtins)
  (let ((magent-action--registry nil))
    (magent-action-builtins-register nil)
    (should-not (magent-action-get "doctor" 'global))
    (dolist (name '("memory-init" "memory-refresh" "memory-clear"))
      (should-not (magent-action-get name 'global)))
    (dolist (name (append magent-test--builtin-control-command-names
                          magent-test--builtin-slash-command-names))
      (should (magent-action-get name 'global)))
    (magent-action-builtins-register '(doctor))
    (should (magent-action-get "doctor" 'global))
    (should-not (magent-action-get "memory-init" 'global))
    (magent-action-builtins-register '(memory))
    (should-not (magent-action-get "doctor" 'global))
    (dolist (name '("memory-init" "memory-refresh" "memory-clear"))
      (should (magent-action-get name 'global)))))

(ert-deftest magent-test-action-enabled-builtins-refreshes-live-registry ()
  "Custom changes refresh Action discovery after runtime initialization."
  (require 'magent-action-builtins)
  (let ((original (default-value 'magent-action-enabled-builtins)))
    (unwind-protect
        (let ((magent--initialized t)
              (magent-action--registry nil)
              (magent-action-registry-changed-hook nil)
              (changes 0))
          (add-hook 'magent-action-registry-changed-hook
                    (lambda () (cl-incf changes)))
          (magent-action-builtins-register '(doctor memory))
          (setq changes 0)
          (customize-set-variable 'magent-action-enabled-builtins '(doctor))
          (should (= changes 1))
          (should (magent-action-get "doctor" 'global))
          (should-not (magent-action-get "memory-init" 'global))
          (customize-set-variable 'magent-action-enabled-builtins '(memory))
          (should (= changes 2))
          (should-not (magent-action-get "doctor" 'global))
          (should (magent-action-get "memory-init" 'global)))
      (set-default 'magent-action-enabled-builtins original))))

(ert-deftest magent-test-action-skills-lists-scope-without-provider ()
  "The core /skills workflow lists descriptors without submitting a turn."
  (require 'magent-action-skills)
  (let ((magent-action--registry nil)
        (magent-action--active-invocations (make-hash-table :test #'eq))
        (magent-skills--registry nil)
        (magent-skills--scope-catalog (make-hash-table :test #'equal))
        (runtime-session
         (magent-runtime-session-create
          :id "session-1"
          :scope 'global
          :magent-session (magent-session-create)))
        completion)
    (magent-skills-register
     (magent-skill-create
      :name "reviewer"
      :description "Review code."
      :type 'instruction))
    (let ((magent-action--allow-core-registration t))
      (magent-action-skills-register))
    (cl-letf (((symbol-function 'magent-runtime-submit)
               (lambda (&rest _)
                 (error "/skills must not submit a provider turn")))
              ((symbol-function 'magent-session-save-deferred-for-session)
               #'ignore)
              ((symbol-function 'magent-action-session--save)
               #'ignore))
      (magent-test--without-action-step-ledger
        (magent-action-invoke
         "skills" runtime-session
         :on-complete
         (lambda (status result)
           (setq completion (list status result))))))
    (should (eq (car completion) 'completed))
    (should
     (equal
      (magent-execution-result-content-string (cadr completion))
      "Available skills:\n- reviewer: Review code."))))

(ert-deftest magent-test-action-register-requires-workflow-and-policy ()
  "Test command registration requires one Workflow and explicit policy."
  (require 'magent-action)
  (let ((magent-action--registry nil))
    (should-error (magent-action-register "missing"))
    (should-error
     (magent-action-register "missing-policy"
                              :workflow #'magent-test--empty-action-workflow))
    (should (magent-action-register
             "workflow" :session-policy 'current
             :workflow #'magent-test--empty-action-workflow))
    (should-error
     (magent-action-register
      "retired-owner" :session-policy 'current :workflow #'magent-test--empty-action-workflow :owner 'retired))))

(ert-deftest magent-test-action-register-validates-exposure-and-session-policy ()
  "Command exposure is orthogonal to registry identity and defaults to slash."
  (let ((magent-action--registry nil))
    (let ((slash (magent-action-register "slash" :session-policy 'current :workflow #'magent-test--empty-action-workflow))
          (both (magent-action-register
                 "both" :workflow #'magent-test--empty-action-workflow
                 :exposure '(slash interactive slash)
                 :session-policy 'isolated)))
      (should (equal (magent-action-spec-exposure slash) '(slash)))
      (should (eq (magent-action-spec-session-policy slash) 'current))
      (should (equal (magent-action-spec-exposure both)
                     '(slash interactive)))
      (should (eq (magent-action-get "both" nil 'interactive) both))
      (should (eq (magent-action-get "both") both))
      (should-not (magent-action-get "slash" nil 'interactive)))
    (should-error
     (magent-action-register "bad-exposure" :session-policy 'current :workflow #'magent-test--empty-action-workflow
                              :exposure '(menu)))
    (should-error
     (magent-action-register "bad-policy" :workflow #'magent-test--empty-action-workflow
                              :session-policy 'temporary))))

(ert-deftest magent-test-action-registry-resolves-layered-overrides ()
  "Test command precedence and exact registration removal."
  (require 'magent-action)
  (let ((magent-action--registry nil)
        (magent-action--sequence 0))
    (let ((builtin (magent-action-register
                    "demo" :description "builtin" :session-policy 'current :workflow #'magent-test--empty-action-workflow
                    :source-layer 'builtin))
          (package (magent-action-register
                    "demo" :description "package" :session-policy 'current :workflow #'magent-test--empty-action-workflow
                    :source-layer 'package))
          (user (magent-action-register
                 "demo" :description "user" :session-policy 'current :workflow #'magent-test--empty-action-workflow
                 :source-layer 'user))
          (project (magent-action-register
                    "demo" :description "project" :session-policy 'current :workflow #'magent-test--empty-action-workflow
                    :source-layer 'project)))
      (should (eq (magent-action-get "demo") project))
      (should (magent-action-unregister project))
      (should (eq (magent-action-get "demo") user))
      (should (magent-action-unregister user))
      (should (eq (magent-action-get "demo") package))
      (should (magent-action-unregister package))
      (should (eq (magent-action-get "demo") builtin)))))

(ert-deftest magent-test-action-register-replaces-one-layer-scope-slot ()
  "Test one name/layer/scope slot retains only its newest registration."
  (require 'magent-action)
  (let ((magent-action--registry nil)
        (magent-action--sequence 0))
    (let ((old (magent-action-register
                "demo" :description "old" :session-policy 'current :workflow #'magent-test--empty-action-workflow
                :source-layer 'project :source-scope "/tmp/project-a"))
          new other-scope)
      (setq new
            (magent-action-register
             "demo" :description "new" :session-policy 'current :workflow #'magent-test--empty-action-workflow
             :source-layer 'project :source-scope "/tmp/project-a"))
      (setq other-scope
            (magent-action-register
             "demo" :description "other" :session-policy 'current :workflow #'magent-test--empty-action-workflow
             :source-layer 'project :source-scope "/tmp/project-b"))
      (should (= (length magent-action--registry) 2))
      (should (eq (magent-action-get "demo" "/tmp/project-a") new))
      (should (eq (magent-action-get "demo" "/tmp/project-b") other-scope))
      (should-not (magent-action-unregister old))
      (should (magent-action-unregister new))
      (should-not (magent-action-get "demo" "/tmp/project-a")))))

(ert-deftest magent-test-action-remove-source-is-layer-and-scope-bounded ()
  "Test source removal preserves registrations outside its layer and scope."
  (require 'magent-action)
  (let ((magent-action--registry nil))
    (let ((global (magent-action-register
                   "global" :session-policy 'current :workflow #'magent-test--empty-action-workflow :source-layer 'user))
          (project-b
           (magent-action-register
            "project-b" :session-policy 'current :workflow #'magent-test--empty-action-workflow :source-layer 'project
            :source-scope "/tmp/project-b")))
      (magent-action-register
       "project-a" :session-policy 'current :workflow #'magent-test--empty-action-workflow :source-layer 'project
       :source-scope "/tmp/project-a")
      (should (= (magent-action-remove-source
                  'project "/tmp/project-a")
                 1))
      (should (eq (magent-action-get "global") global))
      (should (eq (magent-action-get "project-b" "/tmp/project-b")
                  project-b))
      (should-not (magent-action-get "project-a" "/tmp/project-a"))
      (should (= (magent-action-remove-source 'project) 1))
      (should (eq (magent-action-get "global") global))
      (should (= (magent-action-remove-source 'user 'global) 1))
      (should-not (magent-action-get "global")))))

(ert-deftest magent-test-action-registry-change-hook-tracks-mutations ()
  "Test frontend discovery hooks run for register and unregister."
  (require 'magent-action)
  (let ((magent-action--registry nil)
        (magent-action-registry-changed-hook nil)
        (changes 0))
    (add-hook 'magent-action-registry-changed-hook
              (lambda () (cl-incf changes)))
    (let ((registration
           (magent-action-register "demo" :session-policy 'current :workflow #'magent-test--empty-action-workflow)))
      (should (= changes 1))
      (should (magent-action-unregister registration))
      (should (= changes 2)))))

(ert-deftest magent-test-action-core-layer-is-reserved-by-precedence ()
  "Test project definitions cannot shadow the core session control."
  (require 'magent-action)
  (let ((magent-action--registry nil))
    (let ((core (let ((magent-action--allow-core-registration t))
                  (magent-action-register
                   "compact" :session-policy 'current :workflow #'magent-test--empty-action-workflow
                   :source-layer 'core))))
      (magent-action-register
       "compact" :session-policy 'current :workflow #'magent-test--empty-action-workflow :source-layer 'project)
      (should (eq (magent-action-get "compact") core))
      (should-error
       (magent-action-register
        "reserved" :session-policy 'current :workflow #'magent-test--empty-action-workflow :source-layer 'core)
       :type 'error)
      (should-error (magent-action-unregister core) :type 'error)
      (should-error (magent-action-remove-source 'core) :type 'error))))

(ert-deftest magent-test-action-answer-submits-structured-turn ()
  "Test terminal Answer Steps own one structured runtime submission."
  (require 'magent-action)
  (let* ((magent-action--registry nil)
         (magent-action--active-invocations (make-hash-table :test #'eq))
         (runtime-session
          (magent-runtime-session-create
           :id "session-1" :scope 'global
           :magent-session (magent-session-create)))
         submitted completion)
    (magent-action-register
     "demo" :description "Demo"
     :session-policy 'current
     :workflow
     (iter-lambda (_invocation)
       (magent-workflow-answer
           "Answer" "Base prompt"
         :skills '("review")
         :agent 'review-agent
         :append-argument-p t))
     :source-layer 'package)
    (cl-letf (((symbol-function 'magent-runtime-submit)
               (lambda (session prompt &rest args)
                 (setq submitted (list session prompt args))
                 (funcall (plist-get args :on-complete)
                          'completed (magent-execution-result-completed "done"))
                 "submission-1")))
      (magent-action-invoke
       "demo" runtime-session
       :raw-input "/demo focus"
       :argument "focus"
       :request-context '(:file-path "/tmp/frontend.el"
                          :resource-paths ("/tmp/frontend.el"))
       :on-complete
       (lambda (status result) (setq completion (list status result)))))
    (should (eq (car submitted) runtime-session))
    (should (equal (cadr submitted)
                   "Base prompt\n\nAdditional instruction:\nfocus"))
    (let* ((args (nth 2 submitted))
           (context (plist-get args :context))
           (metadata (plist-get args :turn-metadata)))
      (should (equal (plist-get args :skills) '("review")))
      (should (eq (plist-get args :agent) 'review-agent))
      (should (equal (plist-get context :file-path) "/tmp/frontend.el"))
      (should (equal (plist-get context :resource-paths)
                     '("/tmp/frontend.el")))
      (should (eq (plist-get metadata :source) 'magent-action))
      (should (equal (plist-get metadata :action) "demo"))
      (should (equal (plist-get metadata :action-input) "/demo focus"))
      (should-not (plist-member metadata :workflow)))
    (should (eq (car completion) 'completed))
    (should (equal (magent-execution-result-content-string (cadr completion))
                   "done"))))

(ert-deftest magent-test-action-workflow-may-consume-argument ()
  "Test a Workflow can own argument expansion without duplicate text."
  (require 'magent-action)
  (let* ((magent-action--registry nil)
         (magent-action--active-invocations (make-hash-table :test #'eq))
         (runtime-session
          (magent-runtime-session-create
           :id "session-1" :scope 'global
           :magent-session (magent-session-create)))
         submitted-prompt)
    (magent-action-register
     "demo" :description "Demo"
     :session-policy 'current
     :workflow
     (iter-lambda (invocation)
       (magent-workflow-answer
           "Answer"
           (format "Review target: %s"
                   (magent-action-invocation-argument invocation))))
     :source-layer 'package)
    (cl-letf (((symbol-function 'magent-runtime-submit)
               (lambda (_session prompt &rest args)
                 (setq submitted-prompt prompt)
                 (funcall (plist-get args :on-complete)
                          'completed (magent-execution-result-completed "done"))
                 "submission-1")))
      (magent-action-invoke "demo" runtime-session :argument "lisp/"))
    (should (equal submitted-prompt "Review target: lisp/"))))

(ert-deftest magent-test-action-process-step-returns-full-result ()
  "Test an argv process Step resumes with its complete result."
  (require 'magent-action)
  (let ((program (executable-find "printf")))
    (skip-unless program)
    (let ((magent-action--registry nil)
          (magent-action--active-invocations (make-hash-table :test #'eq))
          (runtime-session (magent-runtime-session-create :id "session-1"))
          captured
          completion)
      (magent-action-register
       "process"
       :session-policy 'current
       :workflow
       (iter-lambda (_invocation)
         (setq captured
               (magent-workflow-process
                   "Print value" (list program "%s" "hello")
                 :result 'full))
         "process completed"))
      (magent-test--without-action-step-ledger
        (magent-action-invoke
         "process" runtime-session
         :on-complete
         (lambda (status result) (setq completion (list status result))))
        (let ((deadline (+ (float-time) 3)))
          (while (and (null completion) (< (float-time) deadline))
            (accept-process-output nil 0.02))))
      (should (eq (car completion) 'completed))
      (should (magent-action-process-result-p captured))
      (should (equal (magent-action-process-result-argv captured)
                     (list program "%s" "hello")))
      (should (zerop (magent-action-process-result-exit-status captured)))
      (should (equal (magent-action-process-result-stdout captured) "hello"))
      (should (string-empty-p
               (magent-action-process-result-stderr captured)))
      (should-not (magent-action-process-result-timed-out-p captured)))))

(ert-deftest magent-test-action-process-step-rejects-remote-directory ()
  "Action argv Steps cannot become implicit remote process launchers."
  (require 'magent-action)
  (let ((default-directory "/ssh:test.invalid:/srv/project/"))
    (should-error
     (magent-action--make-process-step "Remote" '("true"))
     :type 'error)
    (should-error
     (magent-action--make-process-step
      "Remote" '("true") :directory default-directory)
     :type 'error)))

(ert-deftest magent-test-action-process-step-failure-is-recoverable ()
  "Test a failed process Step signals its typed condition in the Workflow."
  (require 'magent-action)
  (let ((program (executable-find "false")))
    (skip-unless program)
    (let ((magent-action--registry nil)
          (magent-action--active-invocations (make-hash-table :test #'eq))
          (runtime-session (magent-runtime-session-create :id "session-1"))
          condition
          completion)
      (magent-action-register
       "process-failure"
       :session-policy 'current
       :workflow
       (iter-lambda (_invocation)
         (condition-case err
             (magent-workflow-process
                 "Expected failure" (list program))
           (magent-action-process-error
            (setq condition err)
            "failure recovered"))))
      (magent-test--without-action-step-ledger
        (magent-action-invoke
         "process-failure" runtime-session
         :on-complete
         (lambda (status result) (setq completion (list status result))))
        (let ((deadline (+ (float-time) 3)))
          (while (and (null completion) (< (float-time) deadline))
            (accept-process-output nil 0.02))))
      (should (eq (car completion) 'completed))
      (should (eq (car condition) 'magent-action-process-error))
      (should (magent-action-process-result-p (nth 2 condition)))
      (should (= (magent-action-process-result-exit-status
                  (nth 2 condition))
                 1))
      (should (equal
               (magent-execution-result-content-string (cadr completion))
               "failure recovered")))))

(ert-deftest magent-test-action-process-step-start-error-is-typed ()
  "Test a process startup error remains useful and recoverable."
  (require 'magent-action)
  (let ((magent-action--registry nil)
        (magent-action--active-invocations (make-hash-table :test #'eq))
        (runtime-session (magent-runtime-session-create :id "session-1"))
        condition
        completion)
    (magent-action-register
     "process-start-error"
     :session-policy 'current
     :workflow
     (iter-lambda (_invocation)
       (condition-case err
           (magent-workflow-process
               "Missing executable"
               '("magent-test-executable-that-does-not-exist"))
         (magent-action-process-error
          (setq condition err)
          "start error recovered"))))
    (magent-test--without-action-step-ledger
      (magent-action-invoke
       "process-start-error" runtime-session
       :on-complete
       (lambda (status result) (setq completion (list status result)))))
    (should (eq (car completion) 'completed))
    (should (eq (car condition) 'magent-action-process-error))
    (should (eq (car (nth 2 condition)) 'file-missing))
    (should (string-match-p
             "magent-test-executable-that-does-not-exist"
             (error-message-string condition)))
    (should (equal
             (magent-execution-result-content-string (cadr completion))
             "start error recovered"))))

(ert-deftest magent-test-action-process-step-timeout-is-typed ()
  "Test process timeout kills the child and remains recoverable."
  (require 'magent-action)
  (let ((program (executable-find "sleep")))
    (skip-unless program)
    (let ((magent-action--registry nil)
          (magent-action--active-invocations (make-hash-table :test #'eq))
          (runtime-session (magent-runtime-session-create :id "session-1"))
          process-result
          completion)
      (magent-action-register
       "process-timeout"
       :session-policy 'current
       :workflow
       (iter-lambda (_invocation)
         (condition-case err
             (magent-workflow-process
                 "Expected timeout" (list program "1")
               :timeout 0.01)
           (magent-action-process-error
            (setq process-result (nth 2 err))
            "timeout recovered"))))
      (magent-test--without-action-step-ledger
        (magent-action-invoke
         "process-timeout" runtime-session
         :on-complete
         (lambda (status result) (setq completion (list status result))))
        (let ((deadline (+ (float-time) 3)))
          (while (and (null completion) (< (float-time) deadline))
            (accept-process-output nil 0.02))))
      (should (eq (car completion) 'completed))
      (should (magent-action-process-result-p process-result))
      (should (magent-action-process-result-timed-out-p process-result))
      (should (= (magent-action-process-result-exit-status process-result)
                 124))
      (should (equal
               (magent-execution-result-content-string (cadr completion))
               "timeout recovered")))))

(ert-deftest magent-test-action-callback-step-failure-is-typed ()
  "Test a callback failure resumes as its typed Workflow condition."
  (require 'magent-action)
  (let ((magent-action--registry nil)
        (magent-action--active-invocations (make-hash-table :test #'eq))
        (runtime-session (magent-runtime-session-create :id "session-1"))
        condition
        completion)
    (magent-action-register
     "callback-failure"
     :session-policy 'current
     :workflow
     (iter-lambda (_invocation)
       (condition-case err
           (magent-workflow-callback
               "Fail callback"
               (lambda (_done) (error "callback exploded")))
         (magent-action-callback-error
          (setq condition err)
          "callback recovered"))))
    (magent-test--without-action-step-ledger
      (magent-action-invoke
       "callback-failure" runtime-session
       :on-complete
       (lambda (status result) (setq completion (list status result)))))
    (should (eq (car completion) 'completed))
    (should (eq (car condition) 'magent-action-callback-error))
    (should (eq (car (nth 2 condition)) 'error))
    (should (string-match-p "callback exploded"
                            (error-message-string condition)))
    (should (equal
             (magent-execution-result-content-string (cadr completion))
             "callback recovered"))))

(ert-deftest magent-test-action-workflow-rejects-non-iterator-result ()
  "Test an invalid Workflow result fails before runtime submission."
  (require 'magent-action)
  (let* ((magent-action--registry nil)
         (magent-action--active-invocations (make-hash-table :test #'eq))
         (runtime-session
          (magent-runtime-session-create
           :id "session-1" :scope 'global
           :magent-session (magent-session-create)))
         submitted completion)
    (magent-action-register
     "demo" :description "Demo"
     :session-policy 'current
     :workflow (lambda (_invocation) "invalid")
     :source-layer 'package)
    (cl-letf (((symbol-function 'magent-runtime-submit)
               (lambda (&rest _args) (setq submitted t))))
      (magent-action-invoke
       "demo" runtime-session
       :on-complete
       (lambda (status result) (setq completion (list status result)))))
    (should-not submitted)
    (should (eq (car completion) 'failed))
    (should (string-match-p
             "did not return an iterator"
             (magent-execution-result-content-string (cadr completion))))))

(ert-deftest magent-test-action-agent-step-validates-structured-fields ()
  "Test malformed structured Step fields fail before submission ownership."
  (require 'magent-action)
  (let ((valid-buffer (generate-new-buffer " *magent-turn-validation*")))
    (unwind-protect
        (progn
          (should-error
           (apply #'magent-action--make-agent-step
                  '("Demo" "Prompt" :context (:file-path "/tmp/example.el"))))
          (dolist (buffers
                   (list '(("[" :regexp t))
                         (list (list valid-buffer :unknown t))))
            (let* ((step (magent-action--make-agent-step
                          "Demo" "Prompt" :buffers buffers))
                   (invocation
                    (magent-action-invocation-create
                     :id "validation"
                     :spec (magent-action-spec-create :name "validation")
                     :runtime-session
                     (magent-runtime-session-create
                      :id "validation" :scope 'global))))
              (should-error
               (magent-action--resolve-step-buffers step invocation)))))
      (kill-buffer valid-buffer))))

(ert-deftest magent-test-action-buffer-patterns-use-popwin-semantics ()
  "Test mode, regexp, predicate, exact matching, scope, and deduplication."
  (require 'magent-action)
  (let* ((project-a (make-temp-file "magent-buffer-project-a-" t))
         (project-b (make-temp-file "magent-buffer-project-b-" t))
         (buffer-a (generate-new-buffer " *magent-pattern-one*"))
         (buffer-b (generate-new-buffer " *magent-pattern-two*"))
         (buffer-other (generate-new-buffer " *magent-pattern-other*"))
         (runtime-session
          (magent-runtime-session-create :id "session-1" :scope project-a))
         (spec (magent-action-spec-create :name "buffers"))
         (invocation
          (magent-action-invocation-create
           :id "invocation-1" :spec spec :runtime-session runtime-session)))
    (unwind-protect
        (progn
          (dolist (buffer (list buffer-a buffer-b))
            (with-current-buffer buffer
              (setq default-directory (file-name-as-directory project-a)
                    major-mode 'magent-test-context-mode)))
          (with-current-buffer buffer-other
            (setq default-directory (file-name-as-directory project-b)
                  major-mode 'magent-test-context-mode))
          (cl-letf (((symbol-function 'magent-test-context-buffer-p)
                     (lambda (buffer)
                       (string-match-p "two" (buffer-name buffer)))))
            (let* ((step
                    (magent-action--make-agent-step
                     "Inspect" "Inspect"
                     :buffers
                     '(magent-test-context-mode
                       ("^ \\*magent-pattern-" :regexp t)
                       (magent-test-context-buffer-p :predicate t))))
                   (matches
                    (magent-action--resolve-step-buffers step invocation)))
              (should (= (length matches) 2))
              (should (memq buffer-a matches))
              (should (memq buffer-b matches))
              (should-not (memq buffer-other matches))))
          ;; Exact selections are intentional and therefore global by default.
          (let ((matches
                 (magent-action--matching-buffers
                  (magent-action--normalize-buffer-config buffer-other)
                  invocation)))
            (should (equal matches (list buffer-other))))
          ;; Exact selections can opt back into project isolation.
          (should-not
           (magent-action--matching-buffers
            (magent-action--normalize-buffer-config
             (list buffer-other :project-only-p t))
            invocation)))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list buffer-a buffer-b buffer-other))
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest magent-test-action-buffer-patterns-distinguish-required-optional ()
  "Test required patterns fail while optional patterns log and continue."
  (require 'magent-action)
  (let* ((runtime-session
          (magent-runtime-session-create :id "session-1" :scope 'global))
         (spec (magent-action-spec-create :name "buffers"))
         (invocation
          (magent-action-invocation-create
           :id "invocation-1" :spec spec :runtime-session runtime-session))
         logged)
    (should-error
     (magent-action--resolve-step-buffers
      (magent-action--make-agent-step
       "Inspect" "Inspect" :buffers '("*magent-missing-required*"))
      invocation)
     :type 'user-error)
    (cl-letf (((symbol-function 'magent-log)
               (lambda (format-string &rest args)
                 (setq logged (apply #'format format-string args)))))
      (should-not
       (magent-action--resolve-step-buffers
        (magent-action--make-agent-step
         "Inspect" "Inspect"
         :buffers '(("*magent-missing-optional*" :required-p nil)))
        invocation)))
    (should (string-match-p "optional buffer pattern" logged))))

(ert-deftest magent-test-action-buffer-snapshot-honors-region-and-budget ()
  "Test snapshots use active regions, drop properties, and truncate at point."
  (require 'magent-action)
  (let ((buffer (generate-new-buffer " *magent-snapshot*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "0123456789abcdefghij")
          (add-text-properties 1 21 '(face bold secret-property t))
          (setq transient-mark-mode t)
          (goto-char 6)
          (set-mark 12)
          (activate-mark)
          (let* ((snapshot (magent-action--buffer-resource-block buffer 4))
                 (block (car snapshot))
                 (resource (alist-get 'resource block))
                 (text (alist-get 'text resource)))
            (should (= (cdr snapshot) 4))
            (should (string-match-p "Selection: active-region" text))
            (should (string-match-p "original 6 characters" text))
            (should (string-match-p "Content:\n5678" text))
            (should-not (text-property-any 0 (length text)
                                           'secret-property t text)))
          (deactivate-mark)
          (widen)
          (narrow-to-region 3 19)
          (goto-char 10)
          (let* ((snapshot (magent-action--buffer-resource-block buffer 6))
                 (resource (alist-get 'resource (car snapshot)))
                 (text (alist-get 'text resource)))
            (should (= (cdr snapshot) 6))
            (should (string-match-p "Selection: accessible-buffer" text))
            (should (string-match-p "Narrowed: true" text))
            (should (string-match-p "omitted [0-9]+ before" text))
            (should (string-match-p "omitted [0-9]+ before and [0-9]+ after"
                                    text))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest magent-test-action-buffer-context-shares-declaration-budget ()
  "Test command buffer resources share the configured total budget in order."
  (require 'magent-action)
  (let ((first (generate-new-buffer " *magent-budget-first*"))
        (second (generate-new-buffer " *magent-budget-second*"))
        (magent-action-buffer-context-max-chars 5))
    (unwind-protect
        (progn
          (with-current-buffer first (insert "abcdefghij"))
          (with-current-buffer second (insert "klmnopqrst"))
          (let* ((blocks
                  (magent-action--buffer-resource-blocks
                   (list first second)))
                 (first-text
                  (alist-get 'text (alist-get 'resource (nth 0 blocks))))
                 (second-text
                  (alist-get 'text (alist-get 'resource (nth 1 blocks)))))
            (should (string-match-p "retained bounds 6..11" first-text))
            (should (string-match-p "retained bounds 11..11" second-text))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list first second)))))

(ert-deftest magent-test-action-callback-progresses-and-completes ()
  "Test an asynchronous callback Step owns invocation completion."
  (require 'magent-action)
  (let* ((magent-action--registry nil)
         (magent-action--active-invocations (make-hash-table :test #'eq))
         (runtime-session (magent-runtime-session-create :id "session-1"))
         invocation done events completion)
    (magent-action-register
     "async"
     :session-policy 'current
     :workflow
     (iter-lambda (value)
       (setq invocation value)
       (magent-action-progress value "phase one")
       (magent-workflow-callback
           "Wait" (lambda (callback) (setq done callback) #'ignore))))
    (magent-test--without-action-step-ledger
      (magent-action-invoke
       "async" runtime-session
       :observer (lambda (event) (push event events))
       :on-complete
       (lambda (status result) (setq completion (list status result))))
      (should (eq (magent-action-invocation-status invocation) 'active))
      (should (cl-find 'action-progress events
                       :key (lambda (e) (plist-get e :type))))
      (funcall done 'completed "finished"))
    (should (eq (car completion) 'completed))
    (should (equal (magent-execution-result-content-string (cadr completion))
                   "finished"))
    (funcall done 'completed "again")
    (should (equal (magent-execution-result-content-string (cadr completion))
                   "finished"))))

(ert-deftest magent-test-action-workflow-rejects-non-string-return ()
  "A Workflow may return only a string or nil."
  (require 'magent-action)
  (let* ((spec (magent-action-spec-create :name "strict-result"))
         (invocation
          (magent-action-invocation-create :id "strict-result" :spec spec))
         completion)
    (setf (magent-action-invocation-completion-function invocation)
          (lambda (status result) (setq completion (list status result))))
    (magent-action--start-workflow invocation (iter-lambda (_value) 42))
    (should (eq (car completion) 'failed))
    (should (string-match-p
             "invalid result: 42"
             (magent-execution-result-content-string (cadr completion))))))

(ert-deftest magent-test-action-agent-steps-support-sequential-workflows ()
  "Test a Workflow resumes into its following agent Step."
  (require 'magent-action)
  (let* ((magent-action--registry nil)
         (magent-action--active-invocations (make-hash-table :test #'eq))
         (runtime-session (magent-runtime-session-create :id "session-1"))
         submitted)
    (magent-action-register
     "workflow"
     :session-policy 'current
     :workflow
     (iter-lambda (_invocation)
       (magent-workflow-agent-turn "First" "first")
       (magent-workflow-answer "Second" "second")))
    (magent-test--without-action-step-ledger
      (cl-letf (((symbol-function 'magent-runtime-submit)
                 (lambda (_session prompt &rest args)
                   (push prompt submitted)
                   (when (equal prompt "first")
                     (funcall (plist-get args :on-complete)
                              'completed
                              (magent-execution-result-completed "first result")))
                   prompt)))
        (magent-action-invoke "workflow" runtime-session)))
    (should (equal (nreverse submitted) '("first" "second")))
    (should (gethash runtime-session magent-action--active-invocations))))

(ert-deftest magent-test-action-agent-step-merges-explicit-request-context ()
  "Test agent Steps merge request-only runtime hints."
  (require 'magent-action)
  (let* ((runtime-session (magent-runtime-session-create :id "session-1"))
         (invocation
          (magent-action-invocation-create
           :id "invocation-1"
           :spec (magent-action-spec-create :name "workflow")
           :runtime-session runtime-session
           :request-context '(:file-path "/tmp/frontend.el"
                              :features (frontend))))
         submitted-context)
    (cl-letf (((symbol-function 'magent-runtime-submit)
               (lambda (_session _prompt &rest args)
                 (setq submitted-context (plist-get args :context))
                 "submission-1")))
      (magent-action--start-agent-step
       invocation
       (magent-action--make-agent-step
        "Step" "step"
        :request-context '(:features (workflow) :workflow-step review))
       #'ignore))
    (should (equal (plist-get submitted-context :features) '(workflow)))
    (should (eq (plist-get submitted-context :workflow-step) 'review))
    (should (equal (plist-get submitted-context :file-path)
                   "/tmp/frontend.el"))
    (should-error
     (magent-action--make-agent-step
      "Step" "step" :context '(:features (ambiguous))))))

(ert-deftest magent-test-action-workflow-fails-closed-after-callback-error ()
  "Test Workflow code that fails after a callback cannot strand invocation."
  (require 'magent-action)
  (let* ((magent-action--registry nil)
         (magent-action--active-invocations (make-hash-table :test #'eq))
         (runtime-session (magent-runtime-session-create :id "session-1"))
         completion)
    (magent-action-register
     "workflow"
     :session-policy 'current
     :workflow
     (iter-lambda (_invocation)
       (magent-workflow-callback
           "Callback"
           (lambda (done) (funcall done 'completed "done") nil))
       (error "broken step")))
    (magent-test--without-action-step-ledger
      (magent-action-invoke
       "workflow" runtime-session
       :on-complete (lambda (status result)
                      (setq completion (list status result)))))
    (should (eq (car completion) 'failed))
    (should (string-match-p
             "broken step"
             (magent-execution-result-content-string (cadr completion))))))

(ert-deftest magent-test-action-stale-finish-preserves-new-session-owner ()
  "Test stale completion cannot remove a newer invocation for one session."
  (require 'magent-action)
  (let* ((magent-action--active-invocations (make-hash-table :test #'eq))
         (runtime-session (magent-runtime-session-create :id "session-1"))
         (spec (magent-action-spec-create :name "demo"))
         (stale (magent-action-invocation-create
                 :id "old" :spec spec :runtime-session runtime-session))
         (current (magent-action-invocation-create
                   :id "new" :spec spec :runtime-session runtime-session)))
    (puthash runtime-session current magent-action--active-invocations)
    (should (magent-action--finish-completed stale nil))
    (should (eq (gethash runtime-session magent-action--active-invocations)
                current))))

(ert-deftest magent-test-action-workflow-nil-return-completes ()
  "Test an empty Workflow completes without an assistant response."
  (require 'magent-action)
  (let ((magent-action--registry nil)
        (magent-action--active-invocations (make-hash-table :test #'eq))
        (runtime-session
         (magent-runtime-session-create
          :id "session-1" :scope 'global
          :magent-session (magent-session-create)))
        completion)
    (magent-action-register "broken" :session-policy 'current :workflow #'magent-test--empty-action-workflow)
    (magent-action-invoke
     "broken" runtime-session
     :on-complete (lambda (status result)
                    (setq completion (list status result))))
    (should (eq (car completion) 'completed))
    (should (string-empty-p
             (magent-execution-result-content-string (cadr completion))))))

(ert-deftest magent-test-action-callback-start-error-fails-invocation ()
  "Test a callback starter error fails and releases its invocation."
  (require 'magent-action)
  (let ((magent-action--registry nil)
        (magent-action--active-invocations (make-hash-table :test #'eq))
        (runtime-session (magent-runtime-session-create :id "session-1"))
        completion
        (completion-count 0)
        order)
    (magent-action-register
     "broken-callback"
     :session-policy 'current
     :workflow
     (iter-lambda (_invocation)
       (magent-workflow-callback
           "Broken" (lambda (_done) (error "starter exploded")))))
    (magent-test--without-action-step-ledger
      (magent-action-invoke
       "broken-callback" runtime-session
       :on-complete
       (lambda (status result)
         (cl-incf completion-count)
         (setq completion (list status result))
         (push 'completion order))))
    (should (= completion-count 1))
    (should (eq (car completion) 'failed))
    (should (string-match-p
             "starter exploded"
             (magent-execution-result-content-string (cadr completion))))
    (should-not (gethash runtime-session
                         magent-action--active-invocations))
    (should (equal order '(completion)))))

(ert-deftest magent-test-action-finalization-error-is-terminal-failure ()
  "A persistence failure is reported instead of publishing false success."
  (require 'magent-action)
  (let* ((spec
          (magent-action-spec-create
           :name "demo"
           :session-policy 'current
           :workflow #'magent-test--empty-action-workflow))
         (invocation
          (magent-action-invocation-create
           :id "invocation-1"
           :spec spec
           :status 'active))
         completion)
    (setf (magent-action-invocation-completion-function invocation)
          (lambda (status result)
            (setq completion (list status result))))
    (cl-letf (((symbol-function
                'magent-action-session-finalize-workflow-turn)
               (lambda (&rest _)
                 (error "disk unavailable"))))
      (magent-action--finish-completed invocation nil))
    (should (eq (magent-action-invocation-status invocation) 'failed))
    (should (eq (car completion) 'failed))
    (let ((result (cadr completion)))
      (should (eq (plist-get (magent-execution-result-metadata result)
                             :status)
                  'finalization-error))
      (should (string-match-p
               "disk unavailable"
               (magent-execution-result-content-string result))))))

(ert-deftest magent-test-action-validates-elisp-and-step-tool-requirements ()
  "Test Elisp requirements and Step tools fail before model work."
  (require 'magent-action)
  (let ((magent-action--registry nil)
        (magent-action--active-invocations (make-hash-table :test #'eq))
        (global-session (magent-runtime-session-create :id "global" :scope 'global))
        (project-session
         (magent-runtime-session-create
          :id "project" :scope "/tmp/project"
          :magent-session
          (magent-session-create
           :agent (magent-agent-info-create :name "build"))))
        workflow-ran
        global-completion
        tool-completion)
    (magent-action-register
     "missing-feature"
     :session-policy 'current
     :workflow (iter-lambda (_invocation) (setq workflow-ran t))
     :requires 'magent-test-feature-that-does-not-exist)
    (magent-action-invoke
     "missing-feature" global-session
     :on-complete (lambda (status result)
                    (setq global-completion (list status result))))
    (should-not workflow-ran)
    (should (eq (car global-completion) 'failed))
    (should (string-match-p
             "requires unavailable feature"
             (magent-execution-result-content-string (cadr global-completion))))
    (magent-action-register
     "needs-tools"
     :session-policy 'current
     :workflow
     (iter-lambda (_invocation)
       (magent-workflow-agent-turn
           "Needs tools" "Use tools"
         :tools '(read_file bash))))
    (magent-test--without-action-step-ledger
      (cl-letf (((symbol-function
                  'magent-tools-get-gptel-tools-for-permission)
                 (lambda (_permission _tool-names)
                   (list
                    (plist-get
                     (magent-tools-catalog-entry 'read_file)
                     :tool)))))
        (magent-action-invoke
         "needs-tools" project-session
         :on-complete (lambda (status result)
                        (setq tool-completion (list status result))))))
    (should (eq (car tool-completion) 'failed))
    (should (string-match-p
             "tools unavailable to agent build: bash"
             (magent-execution-result-content-string (cadr tool-completion))))))

(ert-deftest magent-test-action-cancel-cleans-up-current-step-once ()
  "Test session cancellation invokes current Step cleanup once."
  (require 'magent-action)
  (let ((magent-action--registry nil)
        (magent-action--active-invocations (make-hash-table :test #'eq))
        (runtime-session (magent-runtime-session-create :id "session-1"))
        invocation (cleanup 0) completion)
    (magent-action-register
     "wait"
     :session-policy 'current
     :workflow
     (iter-lambda (value)
       (setq invocation value)
       (magent-workflow-callback
           "Wait" (lambda (_done) (lambda () (cl-incf cleanup))))))
    (magent-test--without-action-step-ledger
      (magent-action-invoke
       "wait" runtime-session
       :on-complete (lambda (status _result) (setq completion status)))
      (should (magent-action-cancel-session runtime-session))
      (should-not (magent-action-cancel-session runtime-session)))
    (should (= cleanup 1))
    (should (eq completion 'cancelled))
    (should (eq (magent-action-invocation-status invocation) 'cancelled))))

(ert-deftest magent-test-action-cancel-survives-reentrant-step-completion ()
  "Test synchronous Step completion still reports command cancellation."
  (require 'magent-action)
  (let ((magent-action--registry nil)
        (magent-action--active-invocations (make-hash-table :test #'eq))
        (runtime-session (magent-runtime-session-create :id "session-1"))
        invocation done
        (cleanups 0))
    (magent-action-register
     "wait"
     :session-policy 'current
     :workflow
     (iter-lambda (value)
       (setq invocation value)
       (magent-workflow-callback
           "Wait"
           (lambda (callback)
             (setq done callback)
             (lambda ()
               (cl-incf cleanups)
               (funcall done 'cancelled "reentrant"))))))
    (magent-test--without-action-step-ledger
      (magent-action-invoke "wait" runtime-session)
      (should (magent-action-cancel-session runtime-session)))
    (should (= cleanups 1))
    (should (eq (magent-action-invocation-status invocation) 'cancelled))))

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

(ert-deftest magent-test-skills-empty-project-scope-skips-static-fallback ()
  "Test a missing project skill directory does not reload static skills."
  (require 'magent-skills)
  (let ((project-root (make-temp-file "magent-project-" t))
        called)
    (unwind-protect
        (cl-letf (((symbol-function 'magent-skills-load-all)
                   (lambda (&optional directories)
                     (setq called directories)
                     1)))
          (should (= (magent-skills-load-project-scope project-root) 0))
          (should-not called))
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

(ert-deftest magent-test-skills-get-instruction-prompts ()
  "Test collecting instruction-type skill prompts."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (magent-skills-register
     (magent-skill-create :name "s1" :type 'instruction :prompt "Prompt 1"))
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
                        nil (magent-test--all-tool-names)))
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
        :prompt-keywords '("subtree")
        :disclosure 'active)))
    (let* ((resolution (magent-capability-resolve
                        "Refile this subtree"
                        '(:major-mode org-mode
				      :major-mode-family (org-mode text-mode)
				      :features (org))
                        nil (magent-test--all-tool-names)))
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
                        nil (magent-test--all-tool-names)))
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
      :prompt-keywords '("subtree")
      :disclosure 'active))
    (let* ((resolution (magent-capability-resolve
                        "Refactor this subtree"
                        '(:major-mode org-mode
				      :major-mode-family (org-mode outline-mode text-mode)
				      :features (org))
                        nil (magent-test--all-tool-names)))
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
      :prompt-keywords '("subtree")
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
                     nil (magent-test--all-tool-names)))))
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
                       '("manual-skill") (magent-test--all-tool-names))))
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
                        nil (magent-test--all-tool-names)))
           (match (car (magent-capability-resolution-active-capabilities resolution)))
           (contributions (plist-get (magent-capability-match-details match)
                                     :contributions)))
      (should (= (magent-capability-match-score match) 8))
      (should (equal (mapcar (lambda (entry) (plist-get entry :kind)) contributions)
                     '(mode feature file keyword))))))

(ert-deftest magent-test-capability-keywords-require-word-boundaries ()
  "Test short capability keywords do not match inside unrelated words."
  (require 'magent-capability)
  (should (magent-capability--keyword-match-p "org" "Edit this Org heading"))
  (should-not
   (magent-capability--keyword-match-p "org" "Reorganize this function"))
  (should-not
   (magent-capability--keyword-match-p "command" "Inspect the commando unit")))

(ert-deftest magent-test-capability-context-without-intent-stays-suggested ()
  "Test context signals alone do not auto-activate an active capability."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-capability-register
     (magent-capability-create
      :name "project-workflow"
      :skills '("project-workflow")
      :modes '(prog-mode)
      :features '(project)
      :prompt-keywords '("project")
      :disclosure 'active))
    (let* ((resolution
            (magent-capability-resolve
             "Hello there"
             '(:major-mode emacs-lisp-mode
				   :major-mode-family (emacs-lisp-mode prog-mode)
				   :features (project))
             nil (magent-test--all-tool-names)))
           (match (car (magent-capability-resolution-matches resolution))))
      (should (= (magent-capability-match-score match) 5))
      (should (eq (magent-capability-match-status match) 'suggested))
      (should-not (magent-capability-resolution-active-capabilities
                   resolution)))))

(ert-deftest magent-test-capability-tool-requirements-gate-auto-activation ()
  "Test auto capabilities do not inject skills with unavailable tools."
  (require 'magent-capability)
  (let ((magent-capability--registry nil)
        (magent-skills--registry nil))
    (magent-skills-register
     (magent-skill-create
      :name "runtime-skill"
      :type 'instruction
      :tools '(emacs_eval)
      :prompt "Inspect runtime."))
    (magent-capability-register
     (magent-capability-create
      :name "runtime"
      :skills '("runtime-skill")
      :modes '(emacs-lisp-mode)
      :prompt-keywords '("runtime")
      :disclosure 'active))
    (let* ((blocked
            (magent-capability-resolve
             "Inspect runtime"
             '(:major-mode emacs-lisp-mode)
             nil '(read_file)))
           (enabled
            (magent-capability-resolve
             "Inspect runtime"
             '(:major-mode emacs-lisp-mode)
             nil '(emacs_eval))))
      (should-not (magent-capability-resolution-active-capabilities blocked))
      (should-not (magent-capability-resolution-skill-names blocked))
      (should (equal (magent-capability-resolution-skill-names enabled)
                     '("runtime-skill"))))))

(ert-deftest magent-test-agent-explicit-skill-rejects-unavailable-tools ()
  "Test explicit skills fail clearly when their required tools are unavailable."
  (require 'magent-agent)
  (let ((magent-skills--registry nil))
    (magent-skills-register
     (magent-skill-create
      :name "runtime-skill"
      :type 'instruction
      :tools '(emacs_eval)))
    (should-error
     (magent-agent--validate-explicit-skill-tools
      '("runtime-skill") '(read_file))
     :type 'error)
    (should-not
     (magent-agent--validate-explicit-skill-tools
      '("runtime-skill") '(emacs_eval)))))

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
      :prompt-keywords '("subtree")
      :disclosure 'active))
    (let* ((resolution (magent-capability-resolve
                        "Please commit the result after reorganizing this subtree"
                        '(:major-mode org-mode
				      :major-mode-family (org-mode text-mode)
				      :features (org))
                        nil (magent-test--all-tool-names)))
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
                        nil (magent-test--all-tool-names)))
           (match (car (magent-capability-resolution-active-capabilities resolution))))
      (should match)
      (should (= (magent-capability-match-score match) 4))
      (should (equal (mapcar #'identity (magent-capability-match-reasons match))
                     '("mode-family=magit-mode" "keyword=commit"))))))

;; ──────────────────────────────────────────────────────────────────────
;;; Skill file parsing tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-skills-parse-type ()
  "Test Magent accepts only instruction skills."
  (require 'magent-skills)
  (should (eq (magent-skills--parse-type "instruction") 'instruction))
  (should (eq (magent-skills--parse-type "INSTRUCTION") 'instruction))
  (should-error (magent-skills--parse-type "tool"))
  (should-error (magent-skills--parse-type "unknown")))

(ert-deftest magent-test-skills-parse-tools ()
  "Test canonical YAML-sequence tool parsing."
  (require 'magent-skills)
  (should (equal (magent-skills--parse-tools '("bash" "read_file"))
                 '(bash read_file)))
  (should-error (magent-skills--parse-tools "bash, read, write"))
  (should-error (magent-skills--parse-tools "bash"))
  (should-error (magent-skills--parse-tools 'bash))
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
            (insert "---\nname: test-skill\ndescription: A test\ntype: instruction\ntools: [bash, read_file]\nrequires-project: true\n---\nDo the thing."))
          (let ((skill (magent-skills-load-file skillfile)))
            (should skill)
            (should (equal (magent-skill-name skill) "test-skill"))
            (should (equal (magent-skill-description skill) "A test"))
            (should (eq (magent-skill-type skill) 'instruction))
            (should (equal (magent-skill-tools skill) '(bash read_file)))
            (should (magent-skill-requires-project skill))
            (should (string-match-p "Do the thing" (magent-skill-prompt skill)))))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-skills-load-all-includes-emacs-runtime-inspection ()
  "Test builtin skill loading includes the Emacs runtime inspection workflow."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (cl-letf (((symbol-function 'magent-log) #'ignore))
      (magent-skills-load-all (list magent-skills--builtin-dir)))
    (let ((skill (magent-skills-get "emacs-runtime-inspection")))
      (should skill)
      (should (eq (magent-skill-type skill) 'instruction))
      (should (string-match-p "Emacs Runtime Inspection"
                              (or (magent-skill-prompt skill) "")))
      (should (string-match-p "first correctly scoped"
                              (or (magent-skill-prompt skill) "")))
      (should (string-match-p "scope or semantics were wrong"
                              (or (magent-skill-prompt skill) "")))
      (should-not (string-match-p "first successful result"
                                  (or (magent-skill-prompt skill) "")))
      (should (string-match-p "loop signal"
                              (or (magent-skill-prompt skill) ""))))))

(ert-deftest magent-test-skills-command-introspection-inspects-value-shape ()
  "Test command introspection carries type-safe value guidance."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (magent-test--load-builtin-skills-only)
    (let ((prompt (magent-skill-prompt
                   (magent-skills-get
                    "emacs-command-variable-introspection"))))
      (should (string-match-p "type and shape" prompt))
      (should (string-match-p "Wrong type argument" prompt)))))

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
             "source-name: org\n"
             "capability-skills: [org-structure-workflow]\n"
             "modes: [org-mode]\n"
             "features: [org]\n"
             "prompt-keywords: [heading, subtree]\n"
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

(ert-deftest magent-test-capability-load-skill-file-from-temp ()
  "Test loading embedded capability metadata from a skill file."
  (require 'magent-capability)
  (let* ((magent-capability--registry nil)
         (tmpdir (make-temp-file "skill-capability-" t))
         (skillfile (expand-file-name "SKILL.md" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file skillfile
            (insert
             "---\n"
             "name: project-workflow\n"
             "description: Project workflow helper\n"
             "type: instruction\n"
             "tools: [emacs_eval]\n"
             "capability: true\n"
             "source: package\n"
             "source-name: project\n"
             "modes: [prog-mode, text-mode]\n"
             "features: [project]\n"
             "prompt-keywords: [project root, project compile]\n"
             "disclosure: active\n"
             "---\n"
             "Use project.el state.\n"))
          (let ((capability (magent-capability-load-skill-file skillfile)))
            (should capability)
            (should (equal (magent-capability-name capability)
                           "project-workflow"))
            (should (equal (magent-capability-skills capability)
                           '("project-workflow")))
            (should (eq (magent-capability-source-kind capability) 'package))
            (should (equal (magent-capability-source-name capability)
                           "project"))
            (should (equal (magent-capability-modes capability)
                           '(prog-mode text-mode)))
            (should (equal (magent-capability-features capability)
                           '(project)))
            (should (equal (magent-capability-prompt-keywords capability)
                           '("project root" "project compile")))
            ;; Arbitrary external skill metadata cannot elevate its own policy.
            (should (eq (magent-capability-disclosure capability) 'suggested))
            (should-not (magent-capability-notes capability))))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-capability-load-file-accepts-sequence-metadata ()
  "Test capability file loader accepts canonical YAML sequences."
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
             "capability-skills: [reload-workflow, diagnose-workflow]\n"
             "modes: [emacs-lisp-mode, lisp-interaction-mode]\n"
             "features: [emacs-lisp, lisp-mode]\n"
             "files: [\"*.el\", init.el]\n"
             "prompt-keywords: [reload, package, config]\n"
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
             "source-name: org\n"
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
             "source-name: pkg-demo\n"
             "family: attacker-family\n"
             "disclosure: active\n"
             "risk: high\n"
             "capability-skills: [project-workflow]\n"
             "prompt-keywords: [package demo]\n"
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
             "capability-skills: [project-workflow]\n"
             "prompt-keywords: [first]\n"
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
             "capability-skills: [project-workflow]\n"
             "prompt-keywords: [second]\n"
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
             "capability-skills: [project-workflow]\n"
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
             "capability-skills: [project-workflow]\n"
             "prompt-keywords: [first]\n"
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
             "capability-skills: [project-workflow]\n"
             "prompt-keywords: [second]\n"
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

(ert-deftest magent-test-capability-empty-project-scope-skips-static-fallback ()
  "Test missing project capability directories do not reload static files."
  (require 'magent-capability)
  (let ((project-root (make-temp-file "magent-project-" t))
        skill-called
        capability-called)
    (unwind-protect
        (cl-letf (((symbol-function
                    'magent-capability-load-skill-capabilities)
                   (lambda (&optional directories)
                     (setq skill-called directories)
                     1))
                  ((symbol-function 'magent-capability-load-all)
                   (lambda (&optional directories)
                     (setq capability-called directories)
                     1)))
          (should (= (magent-capability-load-project-scope project-root) 0))
          (should-not skill-called)
          (should-not capability-called))
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
             "capability-skills: [project-workflow]\n"
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
    (magent-test--load-builtin-capabilities-only)
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
    (magent-test--load-builtin-capabilities-only)
    (let* ((resolution (magent-capability-resolve
                        "Diagnose why this hook and key binding are shadowed"
                        '(:major-mode emacs-lisp-mode
				      :major-mode-family (emacs-lisp-mode prog-mode)
				      :features (emacs))
                        nil (magent-test--all-tool-names)))
           (active-names (mapcar (lambda (match)
                                   (magent-capability-name
                                    (magent-capability-match-capability match)))
                                 (magent-capability-resolution-active-capabilities resolution))))
      (should (member "emacs-hook-debugging" active-names)))))

(ert-deftest magent-test-builtin-capability-activates-for-command-variable-introspection ()
  "Test builtin command and variable introspection activates in scratch-like contexts."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-test--load-builtin-capabilities-only)
    (let* ((resolution (magent-capability-resolve
                        "Inspect this command and variable binding for me"
                        '(:major-mode lisp-interaction-mode
				      :major-mode-family (lisp-interaction-mode emacs-lisp-mode prog-mode)
				      :features (emacs))
                        nil (magent-test--all-tool-names)))
           (active-names (mapcar (lambda (match)
                                   (magent-capability-name
                                    (magent-capability-match-capability match)))
                                 (magent-capability-resolution-active-capabilities resolution))))
      (should (member "emacs-command-variable-introspection" active-names)))))

(ert-deftest magent-test-builtin-capability-activates-for-config-reload-diagnosis ()
  "Test builtin config reload capability activates for diagnosis-style reload prompts."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-test--load-builtin-capabilities-only)
    (let* ((resolution (magent-capability-resolve
                        "Diagnose why reloading init.el leaves stale package state"
                        '(:major-mode emacs-lisp-mode
				      :major-mode-family (emacs-lisp-mode prog-mode)
				      :file-path "/tmp/init.el"
				      :features (emacs))
                        nil (magent-test--all-tool-names)))
           (active-names (mapcar (lambda (match)
                                   (magent-capability-name
                                    (magent-capability-match-capability match)))
                                 (magent-capability-resolution-active-capabilities resolution))))
      (should (member "emacs-config-reload" active-names)))))

(ert-deftest magent-test-curated-project-capability-activates-with-project-wording ()
  "Test curated project capability activates from explicit project workflow wording."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-test--load-builtin-capabilities-only)
    (let* ((resolution (magent-capability-resolve
                        "Switch project and show me the current project root"
                        '(:major-mode emacs-lisp-mode
				      :major-mode-family (emacs-lisp-mode prog-mode)
				      :features (project))
                        nil (magent-test--all-tool-names)))
           (active-names (mapcar (lambda (match)
                                   (magent-capability-name
                                    (magent-capability-match-capability match)))
                                 (magent-capability-resolution-active-capabilities resolution))))
      (should (member "project-workflow" active-names)))))

(ert-deftest magent-test-curated-lsp-capability-activates-with-lsp-context ()
  "Test curated LSP capability activates only in programming/LSP contexts."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-test--load-builtin-capabilities-only)
    (let* ((resolution (magent-capability-resolve
                        "Use diagnostics and rename symbol across the workspace"
                        '(:major-mode python-mode
				      :major-mode-family (python-mode prog-mode)
				      :features (lsp-mode))
                        nil (magent-test--all-tool-names)))
           (active-names (mapcar (lambda (match)
                                   (magent-capability-name
                                    (magent-capability-match-capability match)))
                                 (magent-capability-resolution-active-capabilities resolution))))
      (should (member "lsp-workspace-workflow" active-names)))))

(ert-deftest magent-test-curated-package-features-do-not_auto_activate_irrelevant_prompt ()
  "Test installed package features alone do not force curated capability activation."
  (require 'magent-capability)
  (let ((magent-capability--registry nil))
    (magent-test--load-builtin-capabilities-only)
    (let* ((resolution (magent-capability-resolve
                        "Hello there"
                        '(:major-mode fundamental-mode
				      :major-mode-family (fundamental-mode)
				      :features (project lsp-mode org magit))
                        nil (magent-test--all-tool-names)))
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
  (should (eq (magent-tools-permission-key "emacs_read") 'read))
  (should (eq (magent-tools-permission-key "read_tool_output") 'read))
  (should (eq (magent-tools-permission-key "write_file") 'write))
  (should (eq (magent-tools-permission-key "edit_file") 'edit))
  (should (eq (magent-tools-permission-key "grep") 'grep))
  (should (eq (magent-tools-permission-key "glob") 'glob))
  (should (eq (magent-tools-permission-key "bash") 'bash))
  (should (eq (magent-tools-permission-key "emacs_eval") 'emacs_eval))
  (should (eq (magent-tools-permission-key "emacs_eval_live")
              'emacs_eval_live))
  (should (eq (magent-tools-permission-key "spawn_agent") 'agent))
  (should (eq (magent-tools-permission-key "send_agent_message") 'agent))
  (should (eq (magent-tools-permission-key "wait_agent") 'agent))
  (should (eq (magent-tools-permission-key "list_agents") 'agent))
  (should (eq (magent-tools-permission-key "close_agent") 'agent))
  (should (eq (magent-tools-permission-key "web_search") 'web_search))
  (should (null (magent-tools-permission-key "nonexistent"))))

(ert-deftest magent-test-tools-locality-is-explicit-and-complete ()
  "Every canonical tool declares whether it may cross a TRAMP boundary."
  (require 'magent-tools)
  (let ((expected
         '(("read_file" . tramp-file)
           ("write_file" . tramp-file)
           ("edit_file" . tramp-file)
           ("grep" . project-process)
           ("glob" . tramp-file)
           ("bash" . project-process)
           ("emacs_eval" . local)
           ("emacs_read" . local)
           ("read_tool_output" . local)
           ("emacs_eval_live" . local)
           ("spawn_agent" . local)
           ("send_agent_message" . local)
           ("wait_agent" . local)
           ("list_agents" . local)
           ("close_agent" . local)
           ("web_search" . local))))
    (should (= (length expected) (length magent-tools-catalog)))
    (dolist (entry expected)
      (should (eq (magent-tools-locality (car entry)) (cdr entry))))
    (should-not (magent-tools-locality "unknown"))))

(ert-deftest magent-test-project-process-launcher-is-explicitly-tramp-aware ()
  "Only the project-process launcher opts into remote process handling."
  (require 'magent-tools)
  (let (calls)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (push (list default-directory args) calls)
                 'process)))
      (magent-tools--start-project-process
       "bash" "/tmp/project/" :name "local" :command '("true"))
      (magent-tools--start-project-process
       "grep" "/ssh:test.invalid:/srv/project/"
       :name "remote" :command '("true")))
    (setq calls (nreverse calls))
    (should (equal (caar calls) "/tmp/project/"))
    (should-not (plist-get (cadar calls) :file-handler))
    (should (equal (car (cadr calls))
                   "/ssh:test.invalid:/srv/project/"))
    (should (eq (plist-get (cadr (cadr calls)) :file-handler) t))
    (should-error
     (magent-tools--start-project-process
      "emacs_eval" "/ssh:test.invalid:/srv/project/"
      :name "forbidden" :command '("true"))
     :type 'error)))

(ert-deftest magent-test-tool-result-rejects-unstructured-strings ()
  "Test runtime consumers reject unstructured tool results."
  (should-error
   (magent-tool-result-require "ordinary output")
   :type 'wrong-type-argument))

(defun magent-test--run-bash (command &optional timeout)
  "Run Magent bash COMMAND and return its single callback result."
  (let ((callback-count 0)
        (magent-bash-timeout (or timeout 2))
        result)
    (magent-tools--bash
     (lambda (value)
       (cl-incf callback-count)
       (setq result value))
     command)
    (let ((deadline (+ (float-time) 3)))
      (while (and (null result) (< (float-time) deadline))
        (accept-process-output nil 0.02)))
    (accept-process-output nil 0.05)
    (should result)
    (should (= callback-count 1))
    result))

(ert-deftest magent-test-tools-bash-timeout-is-host-owned ()
  "Test Bash exposes no model timeout and uses the host default."
  (require 'magent-tools)
  (should (= (default-value 'magent-bash-timeout) 300))
  (should-not
   (cl-find "timeout" (gptel-tool-args magent-tools--bash-tool)
            :key (lambda (arg) (plist-get arg :name))
            :test #'equal)))

(ert-deftest magent-test-tools-bash-background-job-does-not-escape ()
  "Test background work ends with its synchronous Bash tool call."
  (require 'magent-tools)
  (skip-unless (executable-find "bash"))
  (let ((marker (make-temp-file "magent-background-marker-"))
        (default-directory temporary-file-directory)
        (magent-bash-program "bash"))
    (delete-file marker)
    (unwind-protect
        (progn
          (should
           (magent-tool-result-success-p
            (magent-test--run-bash
             (format "(sleep 0.1; touch %s) &"
                     (shell-quote-argument marker)))))
          (sleep-for 0.2)
          (should-not (file-exists-p marker)))
      (when (file-exists-p marker)
        (delete-file marker)))))

(ert-deftest magent-test-tools-bash-reports-nonzero-exit-status ()
  "Test bash returns a structured failed result for a nonzero exit."
  (require 'magent-tools)
  (skip-unless (executable-find "bash"))
  (let* ((default-directory temporary-file-directory)
         (magent-bash-program "bash")
         (result (magent-test--run-bash "printf failure-output; exit 7")))
    (should (magent-tool-result-p result))
    (should (eq (magent-tool-result-status-value result) 'failed))
    (should (= (magent-tool-result-exit-code result) 7))
    (should (string-match-p "failure-output"
                            (magent-tool-result-output-string result)))))

(ert-deftest magent-test-tools-bash-keeps-full-failure-for-central-spill ()
  "Test Bash leaves failure bounding to the central spill projection."
  (require 'magent-tools)
  (skip-unless (executable-find "bash"))
  (let* ((default-directory temporary-file-directory)
         (magent-bash-program "bash")
         (result
          (magent-test--run-bash
           "for ((i=1; i<=305; i++)); do printf 'line-%03d\\n' \"$i\"; done; exit 7"))
         (output (magent-tool-result-output-string result)))
    (should-not (magent-tool-result-success-p result))
    (should (= (magent-tool-result-exit-code result) 7))
    (should (string-prefix-p "line-001\n" output))
    (should-not (string-match-p "Bash failure output truncated" output))
    (should (string-match-p "^line-002$" output))
    (should (string-match-p "^line-006$" output))
    (should (string-match-p "^line-007$" output))
    (should (string-suffix-p "line-305" output))
    (should (= 305
               (length
                (seq-filter
                 (lambda (line) (string-prefix-p "line-" line))
                 (split-string output "\n" t)))))))

(ert-deftest magent-test-tools-bash-keeps-long-success-output ()
  "Test the Bash execution layer does not compact successful output."
  (require 'magent-tools)
  (skip-unless (executable-find "bash"))
  (let* ((default-directory temporary-file-directory)
         (magent-bash-program "bash")
         (result
          (magent-test--run-bash
           "for ((i=1; i<=305; i++)); do printf 'line-%03d\\n' \"$i\"; done"))
         (output (magent-tool-result-output-string result)))
    (should (magent-tool-result-success-p result))
    (should (string-prefix-p "line-001\n" output))
    (should (string-suffix-p "line-305" output))
    (should-not (string-match-p "Bash failure output truncated" output))))

(ert-deftest magent-test-tools-bash-enforces-pipefail-without-errexit ()
  "Test Bash exposes pipeline failures without forcing errexit."
  (require 'magent-tools)
  (skip-unless (executable-find "bash"))
  (let ((default-directory temporary-file-directory)
        (magent-bash-program "bash"))
    (let ((result
           (magent-test--run-bash
            "{ printf pipeline-output; exit 7; } | tail -n 1")))
      (should (eq (magent-tool-result-status-value result) 'failed))
      (should (= (magent-tool-result-exit-code result) 7))
      (should (string-match-p
               "pipeline-output" (magent-tool-result-output-string result))))
    (let ((result
           (magent-test--run-bash
            "printf before; false; printf reachable")))
      (should (magent-tool-result-success-p result))
      (should (= (magent-tool-result-exit-code result) 0))
      (should (string-match-p "before"
                              (magent-tool-result-output-string result)))
      (should (string-match-p
               "reachable" (magent-tool-result-output-string result))))
    (let ((result
           (magent-test--run-bash "{ exit 141; } | true")))
      (should (eq (magent-tool-result-status-value result) 'failed))
      (should (= (magent-tool-result-exit-code result) 141)))))

(ert-deftest magent-test-tools-bash-supports-explicit-control-flow ()
  "Test Bash permits explicit failure handling and fail-fast sequencing."
  (require 'magent-tools)
  (skip-unless (executable-find "bash"))
  (let ((default-directory temporary-file-directory)
        (magent-bash-program "bash"))
    (dolist (command '("printf success | tail -n 1"
                       "if false; then printf bad; fi; printf recovered"
                       "false || true; printf handled"
                       "set +o pipefail; { exit 7; } | true; printf pipeline-relaxed"))
      (let ((result (magent-test--run-bash command)))
        (should (magent-tool-result-success-p result))
        (should (= (magent-tool-result-exit-code result) 0))))
    (let ((result (magent-test--run-bash "false && printf unreachable")))
      (should-not (magent-tool-result-success-p result))
      (should-not (string-match-p
                   "unreachable" (magent-tool-result-output-string result))))
    (let* ((magent-bash-program (executable-find "bash"))
           (result (magent-test--run-bash "printf path-ok")))
      (should (magent-tool-result-success-p result)))))

(ert-deftest magent-test-tools-bash-rejects-invalid-command-and-program ()
  "Test invalid bash inputs fail before a process starts."
  (require 'magent-tools)
  (dolist (command '(nil "" "  \n"))
    (let ((result (magent-test--run-bash command)))
      (should (magent-tool-result-p result))
      (should-not (magent-tool-result-success-p result))
      (should-not (magent-tool-result-exit-code result))
      (should (string-match-p
               "non-blank" (magent-tool-result-output-string result)))))
  (let* ((magent-bash-program "/magent/definitely-missing/bash")
         (result (magent-test--run-bash "printf never")))
    (should-not (magent-tool-result-success-p result))
    (should-not (magent-tool-result-exit-code result))
    (should (string-match-p
             "Bash executable not found"
             (magent-tool-result-output-string result)))))

(ert-deftest magent-test-tools-bash-structures-start-errors-and-timeouts ()
  "Test Bash startup errors and timeouts share the failure contract."
  (require 'magent-tools)
  (skip-unless (executable-find "bash"))
  (let ((magent-bash-program "bash"))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _args) (error "synthetic start failure"))))
      (let ((result (magent-test--run-bash "printf never")))
        (should-not (magent-tool-result-success-p result))
        (should-not (magent-tool-result-exit-code result))
        (should (string-match-p
                 "synthetic start failure"
                 (magent-tool-result-output-string result)))))
    (let ((result (magent-test--run-bash "sleep 1" 0.02)))
      (should-not (magent-tool-result-success-p result))
      (should-not (magent-tool-result-exit-code result))
      (should (plist-get (magent-tool-result-metadata result) :timeout)))
    (let* ((result
            (magent-test--run-bash
             "for ((i=1; i<=305; i++)); do printf 'line-%03d\\n' \"$i\"; done; sleep 1"
             0.05))
           (output (magent-tool-result-output-string result)))
      (should-not (magent-tool-result-success-p result))
      (should (plist-get (magent-tool-result-metadata result) :timeout))
      (should (string-prefix-p "Command timed out. Partial output:\n" output))
      (should-not (string-match-p "Bash failure output truncated" output))
      (should (string-suffix-p "line-305" output)))))

(ert-deftest magent-test-tools-bash-ignores-host-bash-env ()
  "Test host BASH_ENV cannot disable the tool's pipefail execution."
  (require 'magent-tools)
  (skip-unless (executable-find "bash"))
  (let ((bash-env (make-temp-file "magent-bash-env-"))
        (original-bash-env (getenv "BASH_ENV"))
        (magent-bash-program "bash"))
    (unwind-protect
        (progn
          (with-temp-file bash-env
            (insert "set +e\nset +o pipefail\n"))
          (let ((process-environment (copy-sequence process-environment)))
            (setenv "BASH_ENV" bash-env)
            (let ((result
                   (magent-test--run-bash
                    "{ exit 7; } | true")))
              (should-not (magent-tool-result-success-p result))
              (should (= (magent-tool-result-exit-code result) 7))))
          (should (equal (getenv "BASH_ENV") original-bash-env)))
      (delete-file bash-env))))

(ert-deftest magent-test-ledger-records-structured-tool-failure ()
  "Test ledger status comes from structured tool result, not text prefixes."
  (let* ((thread (magent-thread-create :id "thread-tool-result"))
         (turn (magent-thread-create-turn thread "run"))
         (result (magent-tool-result-create
                  :status 'failed
                  :success nil
                  :output "plain diagnostic"
                  :error "exit failure"
                  :exit-code 9))
         (item (magent-thread-record-tool-result
                thread (magent-thread-turn-id turn) "call-9" "bash" nil result))
         (session (magent-session-create :thread thread))
         (prompt (magent-test--provider-context
                  session (magent-thread-turn-id turn)))
         (prompt-tool (cdr (cadr prompt))))
    (should (eq (magent-thread-item-status item) 'failed))
    (should (equal (magent-thread-item-output item)
                   (concat "[Tool result: status=failed; exit-code=9]\n"
                           "plain diagnostic")))
    (should (equal (plist-get prompt-tool :result)
                   (magent-thread-item-output item)))
    (should (equal (plist-get (magent-thread-item-metadata item) :exit-code) 9))))

(ert-deftest magent-test-ledger-exposes-failure-without-exit-code ()
  "Test model-visible failures identify a missing process exit code."
  (let* ((thread (magent-thread-create :id "thread-tool-error"))
         (turn (magent-thread-create-turn thread "run"))
         (result (magent-tool-result-create
                  :status 'failed
                  :success nil
                  :error "process could not start"))
         (item (magent-thread-record-tool-result
                thread (magent-thread-turn-id turn) "call-error" "bash" nil
                result)))
    (should (eq (magent-thread-item-status item) 'failed))
    (should (equal (magent-thread-item-output item)
                   (concat "[Tool result: status=failed; exit-code=unavailable]\n"
                           "process could not start")))))

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
                          :effort 'xhigh
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
          (magent-agent-job--runtimes (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'magent-agent-registry-get)
                 (lambda (_name) agent))
                ((symbol-function 'magent-lifecycle-events-create-subagent-context)
                 (lambda (title parent _audit-context)
                   (list :title title :parent parent)))
                ((symbol-function 'magent-lifecycle-events-stop-subagent)
                 (lambda (context)
                   (setq stopped context)))
                ((symbol-function 'magent-agent-run-turn)
                 (lambda (&rest args)
                   (let ((request-state (plist-get args :request-context)))
                   (setq captured
                         (list :prompt (plist-get args :prompt)
                               :agent (plist-get args :agent)
                               :event-context
                               (magent-request-context-event-context request-state)
                               :request-context (plist-get args :context)
                               :capability-resolution
                               (plist-get args :capability-resolution)
                               :request-state request-state))
                   (funcall (plist-get args :on-complete)
                            (magent-execution-result-completed "child answer"))
                   child-loop)))
                ((symbol-function 'magent-agent-loop-abort)
                 (lambda (loop)
                   (setq aborted loop))))
        (magent-tools--spawn-agent
         (lambda (value) (setq result (magent-test-tool-output value)))
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
      (let* ((next-action (cdr (assq 'next_action decoded)))
             (arguments (cdr (assq 'arguments next-action))))
        (should (equal (cdr (assq 'tool next-action)) "wait_agent"))
        (should (equal (cdr (assq 'job_id arguments)) job-id))
        (should-not (assq 'timeout arguments)))
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
      (should (eq (magent-request-context-effort child-state) 'xhigh))
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
      (should (equal (cdr (assq 'effort metadata)) "xhigh"))
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
          (magent-agent-job--runtimes (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'magent-agent-registry-get)
                 (lambda (_name) agent))
                ((symbol-function 'magent-agent-run-turn)
                 (lambda (&rest _args)
                   (setq started t))))
        (magent-tools--spawn-agent
         (lambda (value) (setq result (magent-test-tool-output value)))
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
      (should-not (assq 'next_action decoded))
      (should (magent-agent-job-p job))
      (should (eq (magent-agent-job-status job) 'failed))
      (should (string-match-p "max depth 1 exceeded"
                              (magent-agent-job-error job)))
      (should (= (cdr (assq 'agent-depth metadata)) 2))
      (should (= (cdr (assq 'max-depth metadata)) 1))
      (should-not started))))

(ert-deftest magent-test-tools-spawn-agent-marks-failed-result-failed ()
  "Test child-agent failed results update the durable job as failed."
  (require 'magent-tools)
  (let* ((parent-session (magent-session-create :id "parent"))
         (parent-context (magent-request-context-create
                          :id "parent-req"
                          :session parent-session
                          :approval-session parent-session))
         (agent (magent-agent-info-create
                 :name "explore"
                 :mode 'subagent))
         result)
    (let ((magent-tools--request-context parent-context)
          (magent-agent-job--runtimes (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'magent-agent-registry-get)
                 (lambda (_name) agent))
                ((symbol-function 'magent-lifecycle-events-create-subagent-context)
                 (lambda (title parent _audit-context)
                   (list :title title :parent parent)))
                ((symbol-function 'magent-lifecycle-events-stop-subagent) #'ignore)
                ((symbol-function 'magent-agent-run-turn)
                 (lambda (&rest args)
                   (funcall (plist-get args :on-complete)
                            (magent-execution-result-failed
                             "Request timed out after 5 seconds"))
                   nil)))
        (magent-tools--spawn-agent
         (lambda (value) (setq result (magent-test-tool-output value)))
         "explore"
         "inspect"
         "scan")))
    (let* ((decoded (let ((json-object-type 'alist)
                          (json-array-type 'list))
                      (json-read-from-string result)))
           (job-payload (cdr (assq 'job decoded)))
           (job-id (cdr (assq 'id job-payload)))
           (job (magent-session-agent-job parent-session job-id)))
      (should (equal (cdr (assq 'status decoded)) "failed"))
      (should (eq (magent-agent-job-status job) 'failed))
      (should (equal (magent-agent-job-error job)
                     "Request timed out after 5 seconds")))))

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
           (lambda (value)
             (setq result (magent-test--strip-read-revision
                           (magent-test-tool-output value))))
           "notes.txt" "disk")
          (should
           (equal result
                  (concat
                   "[read_file: source=disk; modified=false; "
                   "lines=1-1; total_lines=1; has_more=false]\n"
                   "from inherited root"))))
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
            (magent-agent-job--runtimes runtime-table))
        (cl-letf (((symbol-function 'magent-lifecycle-events-create-subagent-context)
                   (lambda (title parent _audit-context)
                     (list :title title :parent parent)))
                  ((symbol-function 'magent-lifecycle-events-stop-subagent) #'ignore)
                  ((symbol-function 'magent-agent-run-turn)
                   (lambda (&rest args)
                     (let ((prompt (plist-get args :prompt)))
                     (push prompt captured-prompts)
                     (funcall (plist-get args :on-complete)
                              (magent-execution-result-completed
                               (concat "reply: " prompt)))
                     nil)))
                  ((symbol-function 'magent-agent-loop-abort)
                   (lambda (loop) (setq aborted loop))))
          (magent-tools--list-agents
           (lambda (value) (setq list-result (magent-test-tool-output value))))
          (magent-tools--wait-agent
           (lambda (value) (setq wait-result (magent-test-tool-output value)))
           "agent-1" nil 0)
          (magent-tools--send-agent-message
           (lambda (value) (setq send-result (magent-test-tool-output value)))
           "agent-1"
           "follow up")
          (magent-tools--close-agent
           (lambda (value) (setq close-result (magent-test-tool-output value)))
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

(ert-deftest magent-test-tools-wait-agent-default-timeout-follows-host ()
  "Test child waits can outlive 30 seconds without becoming unbounded."
  (require 'magent-tools)
  (let ((magent-request-timeout 120))
    (should (= (magent-tools--agent-wait-timeout) 120))
    (should (= (magent-tools--agent-wait-timeout 0) 0)))
  (let ((magent-request-timeout 0))
    (should (= (magent-tools--agent-wait-timeout) 300)))
  (let ((magent-request-timeout nil))
    (should (= (magent-tools--agent-wait-timeout) 300))))

(ert-deftest magent-test-tools-wait-agent-observes-status-without-polling ()
  "A running child wait completes from a status observer and one deadline."
  (require 'magent-tools)
  (let* ((session (magent-session-create :id "parent"))
         (job (magent-agent-job-create
               :id "agent-running"
               :parent-session-id "parent"
               :agent-name "explore"
               :task-name "scan"
               :status 'running))
         (magent-tools--request-context
          (magent-request-context-create
           :session session :approval-session session))
         (magent-agent-job--observers (make-hash-table :test #'equal))
         scheduled
         cancelled
         result)
    (magent-session-add-agent-job session job)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (seconds repeat function &rest args)
                 (push (list seconds repeat function args) scheduled)
                 'wait-deadline))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (setq cancelled timer)))
              ((symbol-function 'magent-tools--persist-parent-session)
               #'ignore)
              ((symbol-function 'magent-tools--render-agent-job-event)
               #'ignore))
      (magent-tools--wait-agent
       (lambda (value) (setq result value))
       "agent-running" nil 5)
      (should-not result)
      (should (= (length scheduled) 1))
      (should (= (caar scheduled) 5))
      (should (gethash "agent-running" magent-agent-job--observers))
      (magent-agent-job-set-status job 'completed "done")
      (should (magent-tool-result-success-p result))
      (should (string-match-p
               "\"status\":\"completed\""
               (magent-test-tool-output result)))
      (should (eq cancelled 'wait-deadline))
      (should-not (gethash "agent-running" magent-agent-job--observers)))))

(ert-deftest magent-test-tools-all-registered ()
  "Test that all core tools are registered."
  (require 'magent-tools)
  (should (= (length magent-tools-catalog) 16))
  (should-not (magent-tools-catalog-entry "read_buffer"))
  (should (magent-tools-catalog-entry "emacs_read"))
  (should (magent-tools-catalog-entry "emacs_eval_live"))
  (should (magent-tools-catalog-entry "read_tool_output")))

(ert-deftest magent-test-tools-web-search-description-matches-result-shape ()
  "Test web_search does not imply that result pages were fetched."
  (require 'magent-tools)
  (let ((description
         (gptel-tool-description magent-tools--web-search-tool)))
    (should (string-match-p "titles and URLs only" description))
    (should (string-match-p "does not fetch result pages" description))
    (should (string-match-p "do not claim to have read page content"
                            description))))

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
         (tools (magent-tools-get-gptel-tools-for-permission
                 (magent-agent-info-permission agent) :all)))
    ;; Explicit disk/live-buffer reads share one read_file tool.
    (should (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "read_file")) tools))
    (should (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "emacs_read")) tools))
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
         (tools (magent-tools-get-gptel-tools-for-permission
                 (magent-agent-info-permission agent) :all)))
    (should (= (length tools) 0))))

(ert-deftest magent-test-tools-exact-selection-is-ordered-and-validated ()
  "Exact request tool names select only those catalog entries, in order."
  (require 'magent-tools)
  (let* ((magent-enable-tools magent-permission-keys)
         (permission '((* . allow)))
         (tools (magent-tools-get-gptel-tools-for-permission
                 permission '(bash read_file)))
         (names (mapcar #'gptel-tool-name tools)))
    (should (equal names '("bash" "read_file")))
    (should-not
     (magent-tools-get-gptel-tools-for-permission permission nil))
    (should-error
     (magent-tools-get-gptel-tools-for-permission
      permission '(missing_tool))
     :type 'error)))

(ert-deftest magent-test-tools-filtering-allow-all ()
  "Test that allow-all permission includes all globally enabled tools."
  (require 'magent-tools)
  (require 'magent-agent-registry)
  (let* ((agent (magent-agent-info-create
                 :name "all-tools"
                 :permission (magent-permission-create
                              :rules '((* . allow)))))
         (tools (magent-tools-get-gptel-tools-for-permission
                 (magent-agent-info-permission agent) :all)))
    (should (= (length tools) (length magent-tools-catalog)))))

(ert-deftest magent-test-tools-filtering-no-permission ()
  "Test that agent without permission gets all tools."
  (require 'magent-tools)
  (require 'magent-agent-registry)
  (let* ((agent (magent-agent-info-create :name "no-perm" :mode 'primary))
         (tools (magent-tools-get-gptel-tools-for-permission
                 (magent-agent-info-permission agent) :all)))
    (should (= (length tools) (length magent-tools-catalog)))))

(ert-deftest magent-test-tools-filtering-ask-included ()
  "Test that tools with 'ask permission are included in the list."
  (require 'magent-tools)
  (require 'magent-agent-registry)
  (let* ((agent (magent-agent-info-create
                 :name "ask-agent"
                 :permission (magent-permission-create
                              :rules '((bash . ask)
                                       (* . deny)))))
         (tools (magent-tools-get-gptel-tools-for-permission
                 (magent-agent-info-permission agent) :all)))
    (should (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "bash")) tools))))

(ert-deftest magent-test-tools-filtering-bypass-permission ()
  "Test bypass config ignores per-agent permission filtering."
  (require 'magent-tools)
  (require 'magent-agent-registry)
  (let* ((magent-bypass-permission t)
         (magent-enable-tools magent-permission-keys)
         (agent (magent-agent-info-create
                 :name "no-tools"
                 :permission (magent-permission-create
                              :rules '((* . deny)))))
         (tools (magent-tools-get-gptel-tools-for-permission
                 (magent-agent-info-permission agent) :all)))
    (should (= (length tools) (length magent-tools-catalog)))))

(defun magent-test--strip-read-revision (value)
  "Remove dynamic revision metadata from read tool output VALUE."
  (replace-regexp-in-string "; revision=[[:xdigit:]]+" "" value))

(ert-deftest magent-test-tools-read-file ()
  "Test read_file tool implementation."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-test-"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "file contents here"))
          (should-not (find-buffer-visiting tmpfile))
          (magent-tools--read-file
           (lambda (r)
             (setq result
                   (magent-test--strip-read-revision
                    (magent-test-tool-output r))))
           tmpfile "disk")
          (should
           (equal result
                  (concat
                   "[read_file: source=disk; modified=false; "
                   "lines=1-1; total_lines=1; has_more=false]\n"
                   "file contents here")))
          (should-not (find-buffer-visiting tmpfile)))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-disk-and-live-buffer-reads-are-distinct ()
  "Test read_file keeps disk and live-buffer sources distinct."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-live-read-"))
         (buffer nil)
         disk-result
         buffer-result)
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "disk one\ndisk two\ndisk three\n"))
          (setq buffer (find-file-noselect tmpfile t))
          (with-current-buffer buffer
            (erase-buffer)
            (insert "live one\nlive two\nlive three\n")
            (goto-char (point-min))
            (forward-line 1)
            (let ((narrow-start (point)))
              (forward-line 1)
              (narrow-to-region narrow-start (point)))
            (goto-char (point-min))
            (let ((original-point (point))
                  (original-min (point-min))
                  (original-max (point-max))
                  (original-modified (buffer-modified-p)))
              (magent-tools--read-file
               (lambda (value)
                 (setq disk-result
                       (magent-test--strip-read-revision
                        (magent-test-tool-output value))))
               tmpfile "disk")
              (should
               (equal disk-result
                      (concat
                       "[read_file: source=disk; modified=false; "
                       "lines=1-3; total_lines=3; has_more=false]\n"
                       "disk one\ndisk two\ndisk three\n")))
              (magent-tools--read-file
               (lambda (value)
                 (setq buffer-result
                       (magent-test--strip-read-revision
                        (magent-test-tool-output value))))
               tmpfile "live-buffer")
              (should
               (string-prefix-p
                (format
                 "[read_file: source=live-buffer; buffer=%S; modified=true; narrowed=true; "
                 (buffer-name buffer))
                buffer-result))
              (should
               (string-suffix-p
                (concat
                 "lines=1-3; total_lines=3; has_more=false]\n"
                 "live one\nlive two\nlive three\n")
                buffer-result))
              (should (= (point) original-point))
              (should (= (point-min) original-min))
              (should (= (point-max) original-max))
              (should (eq (buffer-modified-p) original-modified)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-live-buffer-source-requires-visiting-buffer ()
  "Test live-buffer source does not create a buffer or fall back to disk."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-buffer-read-"))
         result)
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "disk only"))
          (should-not (find-buffer-visiting tmpfile))
          (magent-tools--read-file
           (lambda (value) (setq result (magent-test-tool-output value)))
           tmpfile "live-buffer")
          (should (string-match-p "buffer_not_found" result))
          (should-not (find-buffer-visiting tmpfile)))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-read-file-line-range ()
  "Test read_file supports one-based line ranges."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-range-read-"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "one\ntwo\nthree\nfour\n"))
          (magent-tools--read-file
           (lambda (value)
             (setq result (magent-test--strip-read-revision
                           (magent-test-tool-output value))))
           tmpfile "disk" 2 2)
          (should
           (equal result
                  (concat
                   "[read_file: source=disk; modified=false; "
                   "lines=2-3; total_lines=4; has_more=true; "
                   "next_start_line=4]\n"
                   "two\nthree\n")))
          (magent-tools--read-file
           (lambda (value)
             (setq result (magent-test--strip-read-revision
                           (magent-test-tool-output value))))
           tmpfile "disk" 3 nil)
          (should
           (equal result
                  (concat
                   "[read_file: source=disk; modified=false; "
                   "lines=3-4; total_lines=4; has_more=false]\n"
                   "three\nfour\n")))
          (magent-tools--read-file
           (lambda (value)
             (setq result (magent-test--strip-read-revision
                           (magent-test-tool-output value))))
           tmpfile "disk" nil 2)
          (should
           (equal result
                  (concat
                   "[read_file: source=disk; modified=false; "
                   "lines=1-2; total_lines=4; has_more=true; "
                   "next_start_line=3]\n"
                   "one\ntwo\n"))))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-read-file-default-pagination ()
  "Test read_file defaults to bounded, self-describing pages."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-page-read-"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (dotimes (index 205)
              (insert (format "line-%03d\n" (1+ index)))))
          (magent-tools--read-file
           (lambda (value)
             (setq result (magent-test--strip-read-revision
                           (magent-test-tool-output value))))
           tmpfile "disk")
          (should
           (string-prefix-p
            (concat
             "[read_file: source=disk; modified=false; "
             "lines=1-200; total_lines=205; has_more=true; "
             "next_start_line=201]\n")
            result))
          (should (string-match-p "line-200\n" result))
          (should-not (string-match-p "line-201\n" result))
          (magent-tools--read-file
           (lambda (value)
             (setq result (magent-test--strip-read-revision
                           (magent-test-tool-output value))))
           tmpfile "disk" 201)
          (should
           (string-prefix-p
            (concat
             "[read_file: source=disk; modified=false; "
             "lines=201-205; total_lines=205; has_more=false]\n")
            result))
          (should (string-match-p "line-201\n" result))
          (should (string-match-p "line-205\n" result)))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-read-file-keeps-pages-under-character-budget ()
  "Test read_file ends large pages early at a complete line boundary."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-page-budget-read-"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (dotimes (index 5)
              (insert (format "line-%d-%s\n"
                              (1+ index)
                              (make-string 2990 ?x)))))
          (magent-tools--read-file
           (lambda (value)
             (setq result (magent-test--strip-read-revision
                           (magent-test-tool-output value))))
           tmpfile "disk")
          (should
           (string-prefix-p
            (concat
             "[read_file: source=disk; modified=false; "
             "lines=1-2; total_lines=5; has_more=true; "
             "next_start_line=3]\n")
            result))
          (should (string-match-p "line-2-" result))
          (should-not (string-match-p "line-3-" result))
          (should (< (length result)
                     magent-tools--read-file-page-max-characters)))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-read-file-rejects-invalid-line-range ()
  "Test read_file rejects invalid line range arguments clearly."
  (require 'magent-tools)
  (let ((tmpfile (make-temp-file "magent-invalid-range-read-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "one\ntwo\n"))
          (dolist (args '((0 nil) (-1 nil) ("1" nil)))
            (let (result)
              (magent-tools--read-file
               (lambda (value) (setq result (magent-test-tool-output value)))
               tmpfile "disk" (car args) (cadr args))
              (should (string-match-p "start_line must be a positive integer"
                                      result))))
          (dolist (args '((1 0) (1 -1) (1 "2")))
            (let (result)
              (magent-tools--read-file
               (lambda (value) (setq result (magent-test-tool-output value)))
               tmpfile "disk" (car args) (cadr args))
              (should (string-match-p "line_count must be a positive integer"
                                      result)))))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-read-schema-requires-source ()
  "Test read_file exposes explicit source and range arguments."
  (require 'magent-tools)
  (should
   (equal (mapcar (lambda (spec) (plist-get spec :name))
                  (gptel-tool-args magent-tools--read-file-tool))
          '("path" "source" "start_line" "line_count" "reason"))))

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
            (magent-tools--read-file
             (lambda (r)
               (setq result (magent-test--strip-read-revision
                             (magent-test-tool-output r))))
             relative-path "disk"))
          (should
           (equal result
                  (concat
                   "[read_file: source=disk; modified=false; "
                   "lines=1-1; total_lines=1; has_more=false]\n"
                   "root-relative"))))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-tools-read-file-nonexistent ()
  "Test read_file with non-existent file returns error."
  (require 'magent-tools)
  (let ((result nil))
    (magent-tools--read-file
     (lambda (r) (setq result (magent-test-tool-output r)))
     "/tmp/magent-nonexistent-file-xyz" "disk")
    (should (string-match-p "Error" result))))

(ert-deftest magent-test-tools-read-file-null-path ()
  "Test read_file rejects JSON null path values clearly."
  (require 'magent-tools)
  (let ((result nil))
    (magent-tools--read-file
     (lambda (r) (setq result (magent-test-tool-output r))) :null "disk")
    (should
     (string-match-p "Missing required argument .*path" result))))

(ert-deftest magent-test-tools-write-file ()
  "Test write_file tool implementation."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-write-"))
         (result nil))
    (unwind-protect
        (progn
          (magent-tools--write-file (lambda (r) (setq result (magent-test-tool-output r)))
                                    tmpfile "new content"
                                    (magent-tools--file-revision tmpfile))
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
          (magent-tools--write-file (lambda (r) (setq result (magent-test-tool-output r)))
                                    filepath "nested content" "absent")
          (should (string-match-p "Successfully" result))
          (should (file-exists-p filepath)))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-tools-write-file-preserves-old-file-on-rename-error ()
  "Test atomic write failure preserves the old file and removes its temp file."
  (require 'magent-tools)
  (let* ((tmpdir (make-temp-file "magent-atomic-write-" t))
         (path (expand-file-name "target.txt" tmpdir))
         result)
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "old content"))
          (cl-letf (((symbol-function 'rename-file)
                     (lambda (&rest _args)
                       (error "simulated rename failure"))))
            (magent-tools--write-file
             (lambda (value) (setq result (magent-test-tool-output value)))
             path "new content" (magent-tools--file-revision path)))
          (should (string-match-p "simulated rename failure" result))
          (should
           (equal (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))
                  "old content"))
          (should
           (equal (directory-files tmpdir nil directory-files-no-dot-files-regexp)
                  '("target.txt"))))
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
            (magent-tools--write-file (lambda (r) (setq result (magent-test-tool-output r)))
                                      relative-path "root-write" "absent"))
          (should (string-match-p "Successfully" result))
          (should (equal (with-temp-buffer
                           (insert-file-contents target)
                           (buffer-string))
                         "root-write")))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-tools-write-file-rejects-dirty-visiting-buffer ()
  "Test write_file never bypasses unsaved live-buffer contents."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-write-conflict-"))
         (buffer nil)
         result)
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "disk content"))
          (setq buffer (find-file-noselect tmpfile t))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert " plus unsaved"))
          (magent-tools--write-file
           (lambda (value) (setq result (magent-test-tool-output value)))
           tmpfile "replacement" (magent-tools--file-revision tmpfile))
          (should (string-match-p "buffer_conflict" result))
          (should
           (equal (with-temp-buffer
                    (insert-file-contents tmpfile)
                    (buffer-string))
                  "disk content"))
          (with-current-buffer buffer
            (should (equal (buffer-string) "disk content plus unsaved"))
            (should (buffer-modified-p))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-write-file-keeps-clean-buffer-synchronized ()
  "Test write_file updates and saves a clean visiting buffer."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-write-buffer-"))
         (buffer nil)
         result)
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "old"))
          (setq buffer (find-file-noselect tmpfile t))
          (magent-tools--write-file
           (lambda (value) (setq result (magent-test-tool-output value)))
           tmpfile "new" (magent-tools--file-revision tmpfile))
          (should (string-match-p "Successfully" result))
          (with-current-buffer buffer
            (should (equal (buffer-string) "new"))
            (should-not (buffer-modified-p)))
          (should
           (equal (with-temp-buffer
                    (insert-file-contents tmpfile)
                    (buffer-string))
                  "new")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-write-file-rejects-clean-stale-visiting-buffer ()
  "Test write_file does not overwrite a disk change behind a clean buffer."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-write-stale-"))
         (buffer nil)
         result)
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "old buffer text"))
          (setq buffer (find-file-noselect tmpfile t))
          (let ((expected (magent-tools--file-revision tmpfile)))
            (with-temp-file tmpfile
            (insert "new disk text"))
            (with-current-buffer buffer
              (set-visited-file-modtime (seconds-to-time 0)))
            (magent-tools--write-file
             (lambda (value) (setq result (magent-test-tool-output value)))
             tmpfile "replacement" expected))
          (should (string-match-p "stale_revision" result))
          (should
           (equal (with-temp-buffer
                    (insert-file-contents tmpfile)
                    (buffer-string))
                  "new disk text"))
          (with-current-buffer buffer
            (should (equal (buffer-string) "old buffer text"))
            (should-not (buffer-modified-p))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-edit-file ()
  "Test edit_file tool implementation."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-edit-"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "hello world"))
          (magent-tools--edit-file (lambda (r) (setq result (magent-test-tool-output r)))
                                   tmpfile "hello" "goodbye"
                                   (magent-tools--file-revision tmpfile))
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
          (magent-tools--edit-file (lambda (r) (setq result (magent-test-tool-output r)))
                                   tmpfile "nonexistent" "replacement"
                                   (magent-tools--file-revision tmpfile))
          (should (string-match-p "not found" result)))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-edit-file-rejects-stale-revision ()
  "Test edit_file cannot apply a patch to disk state it did not read."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-edit-revision-"))
         expected result)
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "original text"))
          (setq expected (magent-tools--file-revision tmpfile))
          (with-temp-file tmpfile
            (insert "concurrent text"))
          (magent-tools--edit-file
           (lambda (value) (setq result (magent-test-tool-output value)))
           tmpfile "original" "replacement" expected)
          (should (string-match-p "stale_revision" result))
          (should
           (equal (with-temp-buffer
                    (insert-file-contents tmpfile)
                    (buffer-string))
                  "concurrent text")))
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
          (magent-tools--edit-file (lambda (r) (setq result (magent-test-tool-output r)))
                                   tmpfile "hello" "bye"
                                   (magent-tools--file-revision tmpfile))
          (should (string-match-p "found 3 times" result)))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-edit-file-rejects-dirty-visiting-buffer ()
  "Test edit_file never bypasses unsaved live-buffer contents."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-edit-conflict-"))
         (buffer nil)
         result)
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "hello disk"))
          (setq buffer (find-file-noselect tmpfile t))
          (with-current-buffer buffer
            (erase-buffer)
            (insert "hello live"))
          (magent-tools--edit-file
           (lambda (value) (setq result (magent-test-tool-output value)))
           tmpfile "hello" "goodbye" (magent-tools--file-revision tmpfile))
          (should (string-match-p "buffer_conflict" result))
          (should
           (equal (with-temp-buffer
                    (insert-file-contents tmpfile)
                    (buffer-string))
                  "hello disk"))
          (with-current-buffer buffer
            (should (equal (buffer-string) "hello live"))
            (should (buffer-modified-p))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-edit-file-keeps-clean-buffer-synchronized ()
  "Test edit_file updates and saves a clean visiting buffer."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-edit-buffer-"))
         (buffer nil)
         result)
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "hello world"))
          (setq buffer (find-file-noselect tmpfile t))
          (magent-tools--edit-file
           (lambda (value) (setq result (magent-test-tool-output value)))
           tmpfile "hello" "goodbye" (magent-tools--file-revision tmpfile))
          (should (string-match-p "Successfully" result))
          (with-current-buffer buffer
            (should (equal (buffer-string) "goodbye world"))
            (should-not (buffer-modified-p)))
          (should
           (equal (with-temp-buffer
                    (insert-file-contents tmpfile)
                    (buffer-string))
                  "goodbye world")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file tmpfile))))

(ert-deftest magent-test-tools-edit-file-refreshes-clean-stale-buffer ()
  "Test edit_file safely refreshes a clean buffer after a disk change."
  (require 'magent-tools)
  (let* ((tmpfile (make-temp-file "magent-edit-stale-"))
         (buffer nil)
         major-mode-before
         before-revert-ran
         after-revert-ran
         result)
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "hello old"))
          (setq buffer (find-file-noselect tmpfile t))
          (with-temp-file tmpfile
            (insert "hello changed on disk"))
          (with-current-buffer buffer
            (setq major-mode-before major-mode)
            (set-visited-file-modtime (seconds-to-time 0))
            (add-hook 'before-revert-hook
                      (lambda () (setq before-revert-ran t)) nil t)
            (add-hook 'after-revert-hook
                      (lambda () (setq after-revert-ran t)) nil t))
          (magent-tools--edit-file
           (lambda (value) (setq result (magent-test-tool-output value)))
           tmpfile "changed" "updated" (magent-tools--file-revision tmpfile))
          (should (string-match-p "Successfully" result))
          (should-not before-revert-ran)
          (should-not after-revert-ran)
          (with-current-buffer buffer
            (should (equal (buffer-string) "hello updated on disk"))
            (should (eq major-mode major-mode-before))
            (should-not (buffer-modified-p))
            (should (verify-visited-file-modtime buffer)))
          (should
           (equal (with-temp-buffer
                    (insert-file-contents tmpfile)
                    (buffer-string))
                  "hello updated on disk")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
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
            (magent-tools--web-search-callback nil (lambda (r) (setq result (magent-test-tool-output r))) "test" 5))
          (should (string-match-p "Example" result))
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest magent-test-tools-parse-ddg-results ()
  "Test web_search result parsing from a DuckDuckGo-style DOM."
  (require 'magent-tools)
  (let* ((dom '(html nil
                     (body nil
                           (a ((class . "result__a")
                               (href . "https://example.com/1"))
                              " Result 1 ")
                           (a ((class . "other")
                               (href . "https://example.com/ignored"))
                              "Ignored")
                           (a ((class . "result__a")
                               (href . "https://example.com/2"))
                              "Result 2"))))
         (results (magent-tools--parse-ddg-results dom 1)))
    (should (equal results
                   (list (list :title "Result 1"
                               :url "https://example.com/1"))))))

(ert-deftest magent-test-tools-catalog-is-canonical-and-unique ()
  "Test every catalog entry owns name, tool, permission, and locality."
  (require 'magent-tools)
  (let ((names (mapcar (lambda (entry) (plist-get entry :name))
                       magent-tools-catalog)))
    (should (= (length names) (length (delete-dups (copy-sequence names)))))
    (dolist (entry magent-tools-catalog)
      (should (equal (gptel-tool-name (plist-get entry :tool))
                     (plist-get entry :name)))
      (should (memq (plist-get entry :permission)
                    magent-permission-keys))
      (should (memq (plist-get entry :locality)
                    '(local tramp-file project-process))))))

(ert-deftest magent-test-agent-loop-tools-for-provider-json-sanitizes-schema ()
  "Test gptel tool schemas are safe for strict JSON serialization."
  (require 'magent-agent-loop)
  (let* ((tools
          (magent-agent-loop-tools-for-provider
           (list
            (gptel-make-tool
             :name "emacs_eval"
             :description "Eval"
             :args '((:name "sexp" :type string :description "Expression")
                     (:name "timeout" :type integer
                            :description "Timeout" :optional t))
             :function #'ignore
             :async t))))
         (tool (car tools))
         (args (gptel-tool-args tool)))
    (should (equal (plist-get (car args) :type) "string"))
    (should (equal (plist-get (cadr args) :type) "integer"))
    (if (fboundp 'json-serialize)
        (json-serialize (vconcat args)
                        :null-object :null
                        :false-object :json-false)
      (let ((json-null :null)
            (json-false :json-false))
        (json-encode (vconcat args))))))

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
        (lambda (_tool-spec cb arg-values _resource-identity)
          (setq tool-ran (car arg-values))
          (funcall cb (magent-test-tool-result "ok")))
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
        (lambda (tool-spec cb arg-values _resource-identity)
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
        (lambda (_tool-spec cb arg-values _resource-identity)
          (setq tool-ran (car arg-values))
          (funcall cb (magent-test-tool-result "ok")))
        :args-to-plist-function (lambda (_args-spec arg-values) arg-values)
        :summarize-function (lambda (arg-values _args-spec) (car arg-values)))
       (list (list tool (list "echo hi") (lambda (r) (setq result r))))))
    (should (equal tool-ran "echo hi"))
    (should (equal result "ok"))
    (should (eq (magent-permission-session-override 'bash) 'allow))
    (magent-permission-clear-session-overrides)))

(ert-deftest magent-test-emacs-eval-approval-is-once-only-and-not-bypassable ()
  "Eval prompts per invocation despite allow rules, bypass, or session choice."
  (require 'magent-tool-orchestrator)
  (let* ((session (magent-session-create :id "eval-once-only"))
         (context (magent-request-context-create
                   :session session :approval-session session))
         (magent-bypass-permission t)
         (tool-ran nil)
         captured-request result
         (tool (gptel-make-tool
                :name "emacs_eval"
                :args (list '(:name "sexp" :type string))
                :function #'ignore
                :async t)))
    (cl-letf (((symbol-function 'magent-approval-request)
               (lambda (request callback)
                 (setq captured-request request)
                 ;; Even a custom provider returning the disallowed persistent
                 ;; choice must be downgraded by the orchestrator.
                 (funcall callback 'allow-session))))
      (magent-tool-orchestrator-handle-tool-calls
       (magent-tool-orchestrator-create
        :permission '((emacs_eval . allow) (* . allow))
        :request-context context
        :run-tool-function
        (lambda (_tool-spec callback arg-values _resource-identity)
          (setq tool-ran (car arg-values))
          (funcall callback (magent-test-tool-result "42")))
        :args-to-plist-function (lambda (_spec values) values)
        :summarize-function (lambda (values _spec) (car values)))
       (list (list tool '("(+ 20 22)")
                   (lambda (value) (setq result value))))))
    (should (equal tool-ran "(+ 20 22)"))
    (should (equal result "42"))
    (should (eq (plist-get captured-request :approval-policy) 'once-only))
    (should-not (magent-permission-session-override 'emacs_eval session))))

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
        (lambda (tool-spec cb arg-values _resource-identity)
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
  (let ((magent-bypass-permission t)
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
              (lambda (_tool-spec cb arg-values _resource-identity)
                (setq tool-ran (car arg-values))
                (funcall cb (magent-test-tool-result "ok")))
              :args-to-plist-function (lambda (_args-spec arg-values) arg-values)
              :summarize-function (lambda (arg-values _args-spec) (car arg-values)))
             (list (list tool (list "echo hi")
                         (lambda (r) (setq result r)))))))
      (magent-permission-clear-session-overrides))
    (should (equal tool-ran "echo hi"))
    (should (equal result "ok"))))

(ert-deftest magent-test-permission-bypass-is-applied-per-call-in-mixed-batch ()
  "Bypass auto-allows ordinary siblings while prompting once-only eval."
  (require 'magent-tool-orchestrator)
  (let* ((magent-bypass-permission t)
         (bash (gptel-make-tool
                :name "bash"
                :args (list '(:name "command" :type string))
                :function #'ignore :async t))
         (eval (gptel-make-tool
                :name "emacs_eval"
                :args (list '(:name "sexp" :type string))
                :function #'ignore :async t))
         prompts
         runs
         results)
    (cl-letf (((symbol-function 'magent-approval-request)
               (lambda (request callback)
                 (push (plist-get request :tool-name) prompts)
                 (funcall callback 'allow-once))))
      (magent-tool-orchestrator-handle-tool-calls
       (magent-tool-orchestrator-create
        :permission '((bash . deny) (emacs_eval . deny) (* . deny))
        :run-tool-function
        (lambda (tool callback _args _resource-identity)
          (push (gptel-tool-name tool) runs)
          (funcall callback
                   (magent-test-tool-result (gptel-tool-name tool)))))
       (list
        (list bash '("pwd") (lambda (value) (push value results)))
        (list eval '("(+ 1 1)") (lambda (value) (push value results))))))
    (should (equal prompts '("emacs_eval")))
    (should (equal (sort runs #'string<) '("bash" "emacs_eval")))
    (should (equal (sort results #'string<) '("bash" "emacs_eval")))))

(ert-deftest magent-test-local-approval-drop-cancels-stale-prompt ()
  "Test dropping a queued local approval prevents any later prompt."
  (require 'magent-approval)
  (let ((magent-approval-provider-function #'magent-approval-local-request)
        (magent-approval--pending-requests (make-hash-table :test 'equal))
        (magent-approval--completed-requests (make-hash-table :test 'equal))
        (magent-approval--local-prompt-timers (make-hash-table :test 'equal))
        (magent-approval-state-change-functions nil)
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

(ert-deftest magent-test-emacs-eval-live-cancel-cleanup-prevents-late-callback ()
  "Test cancelling live eval before its timer fires suppresses the callback."
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
        (magent-tools--emacs-eval-live
         (lambda (result)
           (setq callback-result result))
         "(+ 1 2)")
        (should (functionp registered-cleanup))
        (let ((inhibit-quit t))
          (funcall registered-cleanup)
          (should (eq cancelled 'fake-timer))
          (should (equal signaled '(fake-thread quit nil)))
          (should-not quit-flag))
        (funcall scheduled)))
    (should-not callback-result)))

(ert-deftest magent-test-emacs-eval-live-timeout-interrupts-worker ()
  "Test live eval timeout signals the worker thread and returns a timeout."
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
      (magent-tools--emacs-eval-live
       (lambda (result)
         (setq callback-result (magent-test-tool-output result)))
       "(+ 1 2)"
       1)
      (should (functionp scheduled))
      (funcall scheduled))
    (should (equal signaled '(fake-thread quit nil)))
    (should (equal callback-result "Error: Evaluation timed out"))))

(ert-deftest magent-test-emacs-eval-live-worker-suppresses-debugger-settings ()
  "Test worker errors are tool results even when caller debugging is enabled."
  (require 'magent-tools)
  (let (worker callback-result timeout-timer)
    (cl-letf (((symbol-function 'make-thread)
               (lambda (function &optional _name)
                 (setq worker function)
                 'fake-thread))
              ((symbol-function 'thread-live-p) (lambda (_thread) nil))
              ((symbol-function 'run-at-time)
               (lambda (seconds _repeat function &rest _args)
                 (if (zerop seconds)
                     (funcall function)
                   (setq timeout-timer function))
                 'fake-timer))
              ((symbol-function 'cancel-timer) #'ignore))
      (let ((debug-on-error t)
            (debug-on-quit t)
            (debug-on-signal t))
        (magent-tools--emacs-eval-live
         (lambda (result)
           (setq callback-result (magent-test-tool-output result)))
         "(error \"worker boom\")")
        (should (functionp worker))
        (funcall worker)))
    (should (functionp timeout-timer))
    (should (string-match-p "worker boom" callback-result))))

(ert-deftest magent-test-emacs-eval-live-fallback-suppresses-debugger-settings ()
  "Test fallback evaluation also ignores interactive debugger variables."
  (require 'magent-tools)
  (let ((real-fboundp (symbol-function 'fboundp))
        callback-result)
    (cl-letf (((symbol-function 'fboundp)
               (lambda (symbol)
                 (and (not (eq symbol 'make-thread))
                      (funcall real-fboundp symbol))))
              ((symbol-function 'run-at-time)
               (lambda (seconds _repeat function &rest _args)
                 (when (zerop seconds) (funcall function))
                 'fake-timer))
              ((symbol-function 'cancel-timer) #'ignore))
      (let ((debug-on-error t)
            (debug-on-quit t)
            (debug-on-signal t))
        (magent-tools--emacs-eval-live
         (lambda (result)
           (setq callback-result (magent-test-tool-output result)))
         "(error \"fallback boom\")")))
    (should (string-match-p "fallback boom" callback-result))))

(defun magent-test--await-tool-callback (starter &optional timeout)
  "Run STARTER with a callback and wait up to TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 5)))
        done result)
    (funcall starter (lambda (value) (setq result value done t)))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should done)
    result))

(ert-deftest magent-test-emacs-eval-runs-in-fresh-child-process ()
  "Child eval returns values and does not retain globals between calls."
  (let ((first
         (magent-test--await-tool-callback
          (lambda (callback)
            (magent-tools--emacs-eval
             callback "(progn (setq magent-child-probe 42) magent-child-probe)"))))
        (second
         (magent-test--await-tool-callback
          (lambda (callback)
            (magent-tools--emacs-eval
             callback "(boundp 'magent-child-probe)")))))
    (should (equal (magent-test-tool-output first) "42"))
    (should (equal (magent-test-tool-output second) "nil"))))

(ert-deftest magent-test-emacs-eval-launches-locally-with-remote-project-data ()
  "A remote project root reaches child Emacs as data, not process locality."
  (require 'magent-tools)
  (let* ((remote-root "/ssh:test.invalid:/srv/project/")
         (magent-tools--request-context
          (magent-request-context-create
           :scope remote-root :project-root remote-root))
         (original-make-process (symbol-function 'make-process))
         launch-directory
         result)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq launch-directory default-directory)
                 (apply original-make-process args))))
      (setq result
            (magent-test--await-tool-callback
             (lambda (callback)
               (magent-tools--emacs-eval callback "default-directory")))))
    (should-not (file-remote-p launch-directory))
    (should (equal (magent-test-tool-output result)
                   (prin1-to-string remote-root)))))

(ert-deftest magent-test-emacs-eval-child-crash-is-a-tool-failure ()
  "Killing the disposable child does not terminate the test Emacs."
  (let ((result
         (magent-test--await-tool-callback
          (lambda (callback)
            (magent-tools--emacs-eval callback "(kill-emacs 17)")))))
    (should-not (magent-tool-result-success-p result))
    (should (string-match-p "exited without a result"
                            (magent-test-tool-output result)))
    (should (= (+ 20 22) 42))))

(ert-deftest magent-test-emacs-eval-child-timeout-is-contained ()
  "A non-yielding child form is killed at the host deadline."
  (let ((result
         (magent-test--await-tool-callback
          (lambda (callback)
            (magent-tools--emacs-eval callback "(while t)" 0.1)) 3)))
    (should-not (magent-tool-result-success-p result))
    (should (string-match-p "timed out" (magent-test-tool-output result)))
    (should (= (+ 1 1) 2))))

(ert-deftest magent-test-emacs-read-is-fixed-read-only-live-inspection ()
  "Structured live reads preserve buffer state and expose no arbitrary form."
  (require 'magent-tools)
  (let ((buffer (generate-new-buffer " *magent-emacs-read*"))
        context hook-info unsupported)
    (unwind-protect
        (with-current-buffer buffer
          (insert "alpha\nbeta\ngamma\n")
          (goto-char 8)
          (narrow-to-region 7 12)
          (setq-local after-save-hook '(ignore whitespace-cleanup))
          (let ((original-point (point))
                (original-min (point-min))
                (original-max (point-max))
                (magent-tools--request-context
                 (magent-request-context-create
                  :origin-buffer-name (buffer-name buffer))))
            (magent-tools--emacs-read
             (lambda (value)
               (setq context
                     (read (magent-test-tool-output value))))
             "current_context")
            (magent-tools--emacs-read
             (lambda (value)
               (setq hook-info
                     (read (magent-test-tool-output value))))
             "hook_members" "after-save-hook")
            (magent-tools--emacs-read
             (lambda (value)
               (setq unsupported (magent-test-tool-output value)))
             "eval" "(erase-buffer)")
            (should (equal (plist-get context :buffer) (buffer-name buffer)))
            (should (plist-get context :narrowed))
            (should (plist-get hook-info :local))
            (should (equal (plist-get hook-info :members)
                           '("ignore" "whitespace-cleanup")))
            (should (string-match-p "unsupported emacs_read operation"
                                    unsupported))
            (should (= (point) original-point))
            (should (= (point-min) original-min))
            (should (= (point-max) original-max))
            (should (equal (buffer-string) "beta\n"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-emacs-read-rejects-missing-explicit-buffer-target ()
  "buffer_info never substitutes the origin for a missing explicit target."
  (require 'magent-tools)
  (let ((origin (generate-new-buffer " *magent-buffer-origin*"))
        result)
    (unwind-protect
        (let ((magent-tools--request-context
               (magent-request-context-create
                :origin-buffer-name (buffer-name origin))))
          (magent-tools--emacs-read
           (lambda (value) (setq result (magent-test-tool-output value)))
           "buffer_info" " *magent-definitely-missing-buffer*")
          (should (string-match-p "buffer_not_found" result))
          (should-not (string-match-p (regexp-quote (buffer-name origin))
                                      result)))
      (kill-buffer origin))))

;; ──────────────────────────────────────────────────────────────────────
;;; UI/session regression tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-audit-record-appends-jsonl ()
  "Test a file audit destination receives appended JSONL records."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-" t))
         (magent-audit (expand-file-name "audit.jsonl" directory))
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
          (should (file-exists-p magent-audit))
          (should (= (length (magent-test--read-audit-records directory)) 2)))
      (delete-directory directory t))))

(ert-deftest magent-test-audit-relative-file-is-user-emacs-relative ()
  "Test a relative audit file is stable across buffer default directories."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-relative-" t))
         (user-emacs-directory (file-name-as-directory directory))
         (magent-audit "logs/audit.jsonl")
         (expected (expand-file-name magent-audit user-emacs-directory))
         (magent-audit-buffer-name "*magent-test-file-audit*")
         (magent-audit--pending-writes nil)
         (magent-audit--flush-timer nil))
    (unwind-protect
        (progn
          (when-let* ((buffer (get-buffer magent-audit-buffer-name)))
            (kill-buffer buffer))
          (let ((default-directory temporary-file-directory))
            (magent-audit-record 'permission-decision
                                 :decision 'allow
                                 :decision-source 'bypass))
          (magent-audit--flush-pending)
          (should (file-exists-p expected))
          (should-not (get-buffer magent-audit-buffer-name)))
      (magent-audit--flush-pending)
      (when-let* ((buffer (get-buffer magent-audit-buffer-name)))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest magent-test-audit-record-disabled-skips-write ()
  "Test nil audit recording creates neither a buffer nor a file."
  (require 'magent-audit)
  (let ((magent-audit nil)
        (magent-audit-buffer-name "*magent-test-disabled-audit*")
        (directory (make-temp-file "magent-audit-" t)))
    (unwind-protect
        (progn
          (when-let* ((buffer (get-buffer magent-audit-buffer-name)))
            (kill-buffer buffer))
          (magent-audit-record 'permission-decision
                               :decision 'allow
                               :decision-source 'bypass)
          (magent-audit--flush-pending)
          (should-not (get-buffer magent-audit-buffer-name))
          (should-not (directory-files directory nil "\\.jsonl$")))
      (when-let* ((buffer (get-buffer magent-audit-buffer-name)))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest magent-test-audit-defaults-to-live-buffer-only ()
  "Test the default destination records only in the live audit buffer."
  (require 'magent-audit)
  (let* ((magent-audit (default-value 'magent-audit))
         (magent-audit-buffer-name "*magent-test-buffer-audit*")
         (directory (make-temp-file "magent-audit-buffer-only-" t))
         (user-emacs-directory (file-name-as-directory directory))
         (magent-audit--pending-writes nil)
         buffer)
    (unwind-protect
        (progn
          (should (eq magent-audit 'buffer))
          (when-let* ((existing (get-buffer magent-audit-buffer-name)))
            (kill-buffer existing))
          (magent-audit-record 'permission-decision
                               :decision 'allow
                               :decision-source 'bypass)
          (setq buffer (get-buffer magent-audit-buffer-name))
          (should (buffer-live-p buffer))
          (should-not magent-audit--pending-writes)
          (should-not (directory-files directory nil "\\.jsonl$"))
          (with-current-buffer buffer
            (should (= (length magent-audit--live-records) 1))
            (should (= (length magent-audit--visible-records) 1))
            (should (string-match-p "permission-decision" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest magent-test-audit-tool-events-redact-write-payloads ()
  "Test persisted tool audit records redact write and edit bodies."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-" t))
         (magent-audit (expand-file-name "audit.jsonl" directory))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (session (magent-session-create :id "audit-payload-session"))
         (magent--current-session session)
         (context (magent-lifecycle-events-context-create :turn-id "turn-audit"))
         (request-context
          (magent-request-context-create
           :scope 'global
           :session session
           :event-context context))
         (audit-context
          (magent-request-context-audit-snapshot request-context)))
    (unwind-protect
        (progn
          (magent-audit-enable)
          (magent-lifecycle-events-emit 'tool-call-start
					:context context
					:audit-context audit-context
					:call-id "call-write"
					:tool-name "write_file"
					:summary "notes.txt"
					:args '(:path "notes.txt" :content "super secret body"))
          (magent-lifecycle-events-emit 'tool-call-start
					:context context
					:audit-context audit-context
					:call-id "call-edit"
					:tool-name "edit_file"
					:summary "notes.txt"
					:args '(:path "notes.txt"
						      :old_text "old secret"
						      :new_text "new secret value"))
          (magent-audit--flush-pending)
          (let* ((records
                  (magent-test--read-audit-records directory))
                 (find-tool
                  (lambda (name)
                    (cl-find-if
                     (lambda (record)
                       (equal (cdr (assq 'tool_name record)) name))
                     records)))
                 (write-record (funcall find-tool "write_file"))
                 (edit-record (funcall find-tool "edit_file"))
                 (write-preview (cdr (assq 'args_preview write-record)))
                 (edit-preview (cdr (assq 'args_preview edit-record))))
            (should (equal (cdr (assq 'tool_name write-record)) "write_file"))
            (should (equal (cdr (assq 'path write-preview)) "notes.txt"))
            (should (= (cdr (assq 'content_length write-preview))
                       (length "super secret body")))
            (should-not (assq 'content write-preview))
            (should (equal (cdr (assq 'tool_name edit-record)) "edit_file"))
            (should (equal (cdr (assq 'path edit-preview)) "notes.txt"))
            (should (= (cdr (assq 'old_text_length edit-preview))
                       (length "old secret")))
            (should (= (cdr (assq 'new_text_length edit-preview))
                       (length "new secret value")))
            (should-not (assq 'old_text edit-preview))
            (should-not (assq 'new_text edit-preview))))
      (magent-audit-disable)
      (delete-directory directory t))))

(ert-deftest magent-test-audit-approval-hooks-persist-request-and-resolution ()
  "Test approval lifecycle events are persisted with decision metadata."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-" t))
         (magent-audit (expand-file-name "audit.jsonl" directory))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (session (magent-session-create :id "approval-session"))
         (magent--current-session session)
         (magent-approval-provider-function (lambda (_request) nil))
         (magent-approval--pending-requests (make-hash-table :test 'equal))
         (magent-approval--completed-requests (make-hash-table :test 'equal))
         (magent-approval-state-change-functions nil)
         (context (magent-lifecycle-events-context-create :turn-id "turn-approval"))
         (request-context
          (magent-request-context-create
           :scope 'global
           :session session
           :event-context context))
         (audit-context
          (magent-request-context-audit-snapshot request-context)))
    (unwind-protect
        (progn
          (magent-audit-enable)
          (magent-approval-request
           (list :request-id "req-1"
                 :audit-context audit-context
                 :tool-name "bash"
                 :perm-key 'bash
                 :summary "echo hi"
                 :args '(:command "echo hi"))
           #'ignore)
          (magent-approval-resolve-request "req-1" 'allow-session)
          (magent-audit--flush-pending)
          (let ((records (magent-test--read-audit-records directory)))
            (should (= (length records) 2))
            (should (equal (mapcar (lambda (record) (cdr (assq 'event record))) records)
                           '("approval-requested" "approval-resolved")))
            (should (equal (cdr (assq 'attribution_source (car records)))
                           "request-snapshot"))
            (should (equal (cdr (assq 'turn_id (car records))) "turn-approval"))
            (should (equal (cdr (assq 'decision_source (cadr records)))
                           "user-allow-session"))
            (should (equal (cdr (assq 'decision (cadr records))) "allow"))))
      (magent-audit-disable)
      (delete-directory directory t))))

(ert-deftest magent-test-audit-args-normalize-captured-project-and-home-paths ()
  "Audit argument previews never expose captured project or home prefixes."
  (require 'magent-audit)
  (let* ((project-root (make-temp-file "magent-audit-project-" t))
         (ambient-root (make-temp-file "magent-audit-ambient-" t))
         (captured-session (magent-session-create :id "captured"))
         (ambient-session (magent-session-create :id "ambient"))
         (magent-session--current-scope ambient-root)
         (magent--current-session ambient-session)
         (home-path (expand-file-name ".config/magent/audit" "~"))
         (request-context
          (magent-request-context-create
           :scope project-root
           :project-root project-root
           :session captured-session))
         (audit-context
          (magent-request-context-audit-snapshot request-context)))
    (unwind-protect
        (dolist (case
                 `(("grep"
                    (:pattern ,home-path
			      :path ,(expand-file-name "src" project-root)))
                   ("glob"
                    (:pattern "**/*.el"
			      :path ,(expand-file-name "lisp" project-root)))
                   ("custom_tool"
                    (:project_file ,(expand-file-name "README.org" project-root)
				   :home_file ,home-path))))
          (let* ((record
                  (magent-audit--build-record
                   'permission-decision
                   (list :audit-context audit-context
                         :tool-name (car case)
                         :summary (format "inspect %s" (cadr case))
                         :args (cadr case))))
                 (preview (cdr (assq 'args_preview record)))
                 (encoded (json-encode record)))
            (should (equal (cdr (assq 'session_id record)) "captured"))
            (should (equal (cdr (assq 'project_root record)) "$PROJECT"))
            (should-not (string-match-p (regexp-quote project-root) encoded))
            (should-not (string-match-p
                         (regexp-quote (expand-file-name "~")) encoded))
            (pcase (car case)
              ((or "grep" "glob")
               (should (string-prefix-p
                        "$PROJECT/" (cdr (assq 'path preview))))
               (should (= (cdr (assq 'pattern_length preview))
                          (length (plist-get (cadr case) :pattern))))
               (should-not (assq 'pattern preview)))
              ("custom_tool"
               (should (= (cdr (assq 'field_count preview)) 2))
               (should-not (assq 'project_file preview))
               (should-not (assq 'home_file preview))))))
      (delete-directory project-root t)
      (delete-directory ambient-root t))))

(ert-deftest magent-test-audit-captured-context-survives-ambient-switch ()
  "Tool and approval audit records retain their request's session and scope."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-context-" t))
         (project-root (make-temp-file "magent-audit-project-" t))
         (ambient-root (make-temp-file "magent-audit-ambient-" t))
         (captured-session (magent-session-create :id "captured-session"))
         (ambient-session (magent-session-create :id "ambient-session"))
         (event-context
          (magent-lifecycle-events-context-create :turn-id "captured-turn"))
         (request-context
          (magent-request-context-create
           :scope project-root
           :project-root project-root
           :session captured-session
           :event-context event-context))
         (audit-context
          (magent-request-context-audit-snapshot request-context))
         (magent-audit (expand-file-name "audit.jsonl" directory))
         (magent-audit--enabled nil)
         (magent-audit--pending-writes nil)
         (magent-audit--flush-timer nil)
         (magent-lifecycle-events--sinks nil)
         (magent-approval-provider-function #'ignore)
         (magent-approval--pending-requests (make-hash-table :test 'equal))
         (magent-approval--completed-requests (make-hash-table :test 'equal))
         (magent-approval-state-change-functions nil)
         (magent-session--current-scope ambient-root)
         (magent--current-session ambient-session)
         (command (format "ls %s" (expand-file-name "src" project-root))))
    (unwind-protect
        (progn
          (magent-audit-enable)
          (magent-lifecycle-events-emit
           'tool-call-start
           :context event-context
           :audit-context audit-context
           :call-id "captured-call"
           :tool-name "bash"
           :summary command
           :args (list :command command))
          (magent-approval-request
           (list :request-id "captured-approval"
                 :audit-context audit-context
                 :tool-name "bash"
                 :perm-key 'bash
                 :summary command
                 :args (list :command command))
           #'ignore)
          ;; Resolution happens after mutable ambient state has moved elsewhere.
          (setq magent-session--current-scope 'global
                magent--current-session ambient-session)
          (magent-approval-resolve-request
           "captured-approval" 'allow-once)
          (magent-audit--flush-pending)
          (let ((records (magent-test--read-audit-records directory)))
            (should (= (length records) 3))
            (dolist (record records)
              (should (equal (cdr (assq 'attribution_source record))
                             "request-snapshot"))
              (should (equal (cdr (assq 'session_id record))
                             "captured-session"))
              (should (equal (cdr (assq 'scope record)) "project"))
              (should (equal (cdr (assq 'project_root record)) "$PROJECT"))
              (should (equal (cdr (assq 'project_id record))
                             (plist-get audit-context :project-id)))
              (should (equal (cdr (assq 'turn_id record)) "captured-turn"))
              (should-not (string-match-p
                           (regexp-quote project-root)
                           (json-encode record)))
              (should-not (string-match-p
                           (regexp-quote ambient-root)
                           (json-encode record))))))
      (magent-audit-disable)
      (delete-directory directory t)
      (delete-directory project-root t)
      (delete-directory ambient-root t))))

(ert-deftest magent-test-audit-canonicalizes-symlinked-project-root ()
  "A symlink request root still normalizes canonical resource paths."
  (require 'magent-audit)
  (let* ((parent (make-temp-file "magent-audit-symlink-" t))
         (real-root (expand-file-name "real-project" parent))
         (link-root (expand-file-name "linked-project" parent))
         (session (magent-session-create :id "symlink-project-session")))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src" real-root) t)
          (make-symbolic-link real-root link-root t)
          (let* ((snapshot
                  (magent-request-context-audit-snapshot
                   (magent-request-context-create
                    :scope link-root
                    :project-root link-root
                    :session session)))
                 (record
                  (magent-audit--build-record
                   'permission-decision
                   (list :audit-context snapshot
                         :tool-name "read_file"
                         :args (list :path
                                     (expand-file-name "src/main.el"
                                                       real-root)))))
                 (path
                  (cdr (assq 'path (cdr (assq 'args_preview record)))))
                 (encoded (json-encode record)))
            (should (equal (plist-get snapshot :project-root)
                           (file-truename real-root)))
            (should (equal path "$PROJECT/src/main.el"))
            (should-not (string-match-p (regexp-quote real-root) encoded))
            (should-not (string-match-p (regexp-quote link-root) encoded))))
      (delete-directory parent t))))

(ert-deftest magent-test-audit-free-text-tools-persist-lengths-only ()
  "Command, prompt, message, result, and UI text never reach audit records."
  (require 'magent-audit)
  (let* ((session (magent-session-create :id "free-text-session"))
         (event-context
          (magent-lifecycle-events-context-create :turn-id "free-text-turn"))
         (request-context
          (magent-request-context-create
           :scope 'global
           :session session
           :event-context event-context))
         (audit-context
          (magent-request-context-audit-snapshot request-context))
         (result-secret "#<closure ((token . closure-secret-9812))>")
         (summary-secret "summary-secret-4107")
         (title-secret "title-secret-5519")
         (detail-secret "detail-secret-2264"))
    (dolist (case
             `(("bash"
                (:command "curl -u alice:hunter2 https://example.invalid")
                command_length
                :command
                "alice:hunter2")
               ("emacs_eval"
                (:sexp "(let ((secret 'sexp-secret-7318)) secret)")
                sexp_length
                :sexp
                "sexp-secret-7318")
               ("spawn_agent"
                (:agent "explore"
			:task_name "task-secret-6914"
			:prompt "prompt-secret-8735")
                prompt_length
                :prompt
                "prompt-secret-8735")
               ("send_agent_message"
                (:job_id "job-1" :message "message-secret-2356")
                message_length
                :message
                "message-secret-2356")))
      (pcase-let* ((`(,tool-name ,args ,length-field ,arg-key ,body-secret) case)
                   (record
                    (magent-audit--build-record
                     'tool-call-end
                     (list :audit-context audit-context
                           :tool-name tool-name
                           :args args
                           :result result-secret
                           :summary summary-secret
                           :title title-secret
                           :detail detail-secret)))
                   (preview (cdr (assq 'args_preview record)))
                   (encoded (json-encode record)))
        (dolist (secret (list body-secret result-secret summary-secret
                              title-secret detail-secret))
          (should-not (string-match-p (regexp-quote secret) encoded)))
        (should (= (cdr (assq length-field preview))
                   (length (plist-get args arg-key))))
        (should-not (assq (intern (substring (symbol-name arg-key) 1))
                          preview))
        (should (= (cdr (assq 'result_length record))
                   (length result-secret)))
        (should (= (cdr (assq 'summary_length record))
                   (length summary-secret)))
        (should (= (cdr (assq 'title_length record))
                   (length title-secret)))
        (should (= (cdr (assq 'detail_length record))
                   (length detail-secret)))
        (should-not (cdr (assq 'summary record)))
        (should-not (cdr (assq 'result_preview record)))
        (should-not (cdr (assq 'title record)))
        (should-not (cdr (assq 'detail record)))))))

(ert-deftest magent-test-audit-metadata-scalars-reject-free-text-types ()
  "Malformed scalar arguments cannot smuggle bodies into audit metadata."
  (require 'magent-audit)
  (let ((secret "audit-scalar-secret-7462"))
    (dolist (case
             `(("bash" (:command "ok" :timeout ,secret) timeout)
               ("emacs_eval" (:sexp "(+ 1 1)" :timeout ,secret) timeout)
               ("grep" (:pattern "x" :path "." :case_sensitive ,secret)
                case_sensitive)
               ("list_agents" (:include_closed ,secret) include_closed)))
      (let* ((record
              (magent-audit--build-record
               'permission-decision
               (list :tool-name (car case) :args (cadr case))))
             (preview (cdr (assq 'args_preview record)))
             (encoded (json-encode record)))
        (should-not (assq (nth 2 case) preview))
        (should-not (string-match-p (regexp-quote secret) encoded))))
    (let* ((record
            (magent-audit--build-record
             'permission-decision
             (list :tool-name "bash"
                   :args '(:command "ok" :timeout 1.5))))
           (preview (cdr (assq 'args_preview record))))
      (should (= (cdr (assq 'timeout preview)) 1.5)))))

(ert-deftest magent-test-audit-external-paths-use-stable-markers ()
  "External absolute paths are correlated by stable, non-disclosing markers."
  (require 'magent-audit)
  (let* ((project-root (make-temp-file "magent-audit-project-" t))
         (session (magent-session-create :id "external-path-session"))
         (request-context
          (magent-request-context-create
           :scope project-root
           :project-root project-root
           :session session))
         (audit-context
          (magent-request-context-audit-snapshot request-context))
         (external-path "/srv/customer/private.txt"))
    (unwind-protect
        (let* ((record-a
                (magent-audit--build-record
                 'permission-decision
                 (list :audit-context audit-context
                       :tool-name "read_file"
                       :args (list :path external-path))))
               (record-b
                (magent-audit--build-record
                 'tool-call-start
                 (list :audit-context audit-context
                       :tool-name "read_file"
                       :args (list :path external-path))))
               (marker-a
                (cdr (assq 'path (cdr (assq 'args_preview record-a)))))
               (marker-b
                (cdr (assq 'path (cdr (assq 'args_preview record-b)))))
               (encoded (json-encode (list record-a record-b))))
          (should (equal marker-a marker-b))
          (should (string-match-p
                   "\\`<external-path:[[:xdigit:]]\\{12\\}>\\'" marker-a))
          (should-not (string-match-p (regexp-quote external-path) encoded))
          (should-not (string-match-p (regexp-quote "/srv/customer") encoded)))
      (delete-directory project-root t))))

(ert-deftest magent-test-audit-project-identities-distinguish-roots ()
  "Project audit ids distinguish roots without disclosing either root."
  (require 'magent-audit)
  (let* ((root-a (make-temp-file "magent-audit-project-a-" t))
         (root-b (make-temp-file "magent-audit-project-b-" t))
         (session-a (magent-session-create :id "project-session-a"))
         (session-b (magent-session-create :id "project-session-b")))
    (unwind-protect
        (let* ((snapshot-a
                (magent-request-context-audit-snapshot
                 (magent-request-context-create
                  :scope root-a :project-root root-a :session session-a)))
               (snapshot-b
                (magent-request-context-audit-snapshot
                 (magent-request-context-create
                  :scope root-b :project-root root-b :session session-b)))
               (record-a
                (magent-audit--build-record
                 'permission-decision (list :audit-context snapshot-a)))
               (record-b
                (magent-audit--build-record
                 'permission-decision (list :audit-context snapshot-b)))
               (project-id-a (cdr (assq 'project_id record-a)))
               (project-id-b (cdr (assq 'project_id record-b)))
               (encoded (json-encode (list record-a record-b))))
          (should (stringp project-id-a))
          (should (stringp project-id-b))
          (should-not (equal project-id-a project-id-b))
          (should (equal (cdr (assq 'project_root record-a)) "$PROJECT"))
          (should (equal (cdr (assq 'project_root record-b)) "$PROJECT"))
          (should-not (string-match-p (regexp-quote root-a) encoded))
          (should-not (string-match-p (regexp-quote root-b) encoded)))
      (delete-directory root-a t)
      (delete-directory root-b t))))

(ert-deftest magent-test-request-audit-snapshot-does-not-share-strings ()
  "Mutating a consumer snapshot cannot corrupt its session or frozen source."
  (require 'magent-runtime)
  (let* ((session-id (copy-sequence "audit-session"))
         (session (magent-session-create :id session-id))
         (context (magent-request-context-create
                   :scope 'global :session session))
         (first (magent-request-context-audit-snapshot context)))
    (aset (plist-get first :session-id) 0 ?X)
    (let ((second (magent-request-context-audit-snapshot context)))
      (should (equal (magent-session-get-id session) "audit-session"))
      (should (equal (plist-get second :session-id) "audit-session"))
      (should-not (eq (plist-get first :session-id)
                      (plist-get second :session-id))))))

(ert-deftest magent-test-audit-malformed-context-still-persists-missing-attribution ()
  "Malformed attribution is dropped without dropping its audit event."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-malformed-" t))
         (magent-audit (expand-file-name "audit.jsonl" directory))
         (magent-audit--pending-writes nil)
         (magent-audit--flush-timer nil)
         (secret "malformed-context-secret-8821")
         (bad-context
          (list :attribution-source 'request-snapshot
                :session-id (lambda () secret)
                :scope 'global)))
    (unwind-protect
        (progn
          (magent-audit-record
           'tool-call-end
           :audit-context bad-context
           :tool-name "bash"
           :args (list :command secret)
           :result secret)
          (magent-audit--flush-pending)
          (let* ((records
                  (magent-test--read-audit-records directory))
                 (record (car records))
                 (encoded (json-encode record)))
            (should (= (length records) 1))
            (should (equal (cdr (assq 'attribution_source record)) "missing"))
            (should-not (cdr (assq 'session_id record)))
            (should-not (cdr (assq 'scope record)))
            (should-not (string-match-p (regexp-quote secret) encoded))))
      (magent-audit--flush-pending)
      (delete-directory directory t))))

(ert-deftest magent-test-audit-subagent-snapshot-survives-ambient-switch ()
  "Subagent start and stop retain one scalar request attribution snapshot."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-subagent-" t))
         (project-root (make-temp-file "magent-audit-project-" t))
         (ambient-root (make-temp-file "magent-audit-ambient-" t))
         (captured-session (magent-session-create :id "subagent-captured"))
         (ambient-session (magent-session-create :id "subagent-ambient"))
         (parent-context
          (magent-lifecycle-events-context-create :turn-id "subagent-turn"))
         (request-context
          (magent-request-context-create
           :scope project-root
           :project-root project-root
           :session captured-session
           :event-context parent-context))
         (audit-context
          (magent-request-context-audit-snapshot request-context))
         (magent-audit (expand-file-name "audit.jsonl" directory))
         (magent-audit--enabled nil)
         (magent-audit--pending-writes nil)
         (magent-audit--flush-timer nil)
         (magent-lifecycle-events--sinks nil)
         (magent-approval-state-change-functions nil)
         (magent-session--current-scope project-root)
         (magent--current-session captured-session))
    (unwind-protect
        (progn
          (magent-audit-enable)
          (let ((subagent-context
                 (magent-lifecycle-events-create-subagent-context
                  "private subagent title" parent-context audit-context)))
            (setq magent-session--current-scope ambient-root
                  magent--current-session ambient-session
                  magent-lifecycle-events--current-context
                  (magent-lifecycle-events-context-create
                   :turn-id "ambient-turn"))
            (magent-lifecycle-events-stop-subagent subagent-context))
          (magent-audit--flush-pending)
          (let* ((records (magent-test--read-audit-records directory))
                 (start (car records))
                 (stop (cadr records))
                 (subagent-id (cdr (assq 'subagent_id start))))
            (should (= (length records) 2))
            (should (equal (mapcar (lambda (record)
                                     (cdr (assq 'event record)))
                                   records)
                           '("subagent-start" "subagent-stop")))
            (should (stringp subagent-id))
            (should (equal (cdr (assq 'subagent_id stop)) subagent-id))
            (dolist (record records)
              (should-not (string-match-p
                           "private subagent title"
                           (json-encode record)))
              (should (equal (cdr (assq 'attribution_source record))
                             "request-snapshot"))
              (should (equal (cdr (assq 'session_id record))
                             "subagent-captured"))
              (should (equal (cdr (assq 'turn_id record)) "subagent-turn"))
              (should (equal (cdr (assq 'scope record)) "project"))
              (should (equal (cdr (assq 'project_id record))
                             (plist-get audit-context :project-id)))
              (should-not (string-match-p
                           (regexp-quote ambient-root)
                           (json-encode record))))))
      (magent-audit-disable)
      (delete-directory directory t)
      (delete-directory project-root t)
      (delete-directory ambient-root t))))

(ert-deftest magent-test-approval-completed-request-keeps-only-scalar-attribution ()
  "Completed approvals do not retain a live request or lifecycle graph."
  (require 'magent-audit)
  (let* ((session (magent-session-create :id "approval-graph-session"))
         (event-context
          (magent-lifecycle-events-context-create :turn-id "approval-graph-turn"))
         (request-context
          (magent-request-context-create
           :scope 'global
           :session session
           :event-context event-context
           :observer (lambda (&rest _args) "observer-secret")))
         (audit-context
          (magent-request-context-audit-snapshot request-context))
         (provider-secret "provider-closure-secret-1448")
         (provider (lambda (_request) provider-secret))
         (command-secret "approval-command-secret-3791")
         (summary-secret "approval-summary-secret-9402")
         (magent-approval-provider-function provider)
         (magent-approval--pending-requests (make-hash-table :test 'equal))
         (magent-approval--completed-requests (make-hash-table :test 'equal))
         (magent-approval-state-change-functions nil))
    (magent-approval-request
     (list :request-id "approval-graph"
           :request-context request-context
           :context event-context
           :audit-context audit-context
           :tool-name "bash"
           :summary summary-secret
           :args (list :command command-secret))
     #'ignore)
    (magent-approval-resolve-request "approval-graph" 'allow-once)
    (let* ((entry (magent-approval-completed-request "approval-graph"))
           (request (plist-get entry :request))
           (stored-audit-context (plist-get request :audit-context))
           (rendered (format "%S" entry)))
      (should-not (plist-member request :request-context))
      (should-not (plist-member request :context))
      (should-not (plist-member request :args))
      (should-not (plist-member request :summary))
      (should-not (plist-member request :callback))
      (should-not (plist-member request :provider))
      (should-not (plist-get entry :provider))
      (dolist (secret (list provider-secret command-secret summary-secret
                            "observer-secret"))
        (should-not (string-match-p (regexp-quote secret) rendered)))
      (should (equal stored-audit-context audit-context))
      (should (eq (plist-get stored-audit-context :attribution-source)
                  'request-snapshot))
      (cl-loop for (_key value) on stored-audit-context by #'cddr
               do (should (or (null value)
                              (stringp value)
                              (symbolp value)))))))

(ert-deftest magent-test-approval-completed-audit-context-is-strict-whitelist ()
  "Completed history drops unknown scalar fields as well as live objects."
  (require 'magent-approval)
  (let* ((secret "approval-extra-secret-8227")
         (magent-approval-provider-function #'ignore)
         (magent-approval--pending-requests (make-hash-table :test 'equal))
         (magent-approval--completed-requests (make-hash-table :test 'equal))
         (magent-approval-state-change-functions nil))
    (magent-approval-request
     (list :request-id "strict-audit"
           :tool-name "bash"
           :perm-key 'bash
           :audit-context
           (list :attribution-source 'request-snapshot
                 :session-id "session-1"
                 :scope 'global
                 :secret secret))
     #'ignore)
    (magent-approval-resolve-request "strict-audit" 'allow-once)
    (let* ((entry (magent-approval-completed-request "strict-audit"))
           (snapshot (plist-get (plist-get entry :request) :audit-context))
           (rendered (format "%S" entry)))
      (should (equal (plist-get snapshot :session-id) "session-1"))
      (should-not (plist-member snapshot :secret))
      (should-not (string-match-p (regexp-quote secret) rendered)))))

(ert-deftest magent-test-approval-rejects-invalid-provider-decision ()
  "An invalid decision leaves the request pending and never calls its callback."
  (require 'magent-approval)
  (let ((magent-approval-provider-function #'ignore)
        (magent-approval--pending-requests (make-hash-table :test 'equal))
        (magent-approval--completed-requests (make-hash-table :test 'equal))
        (magent-approval-state-change-functions nil)
        callback-called)
    (magent-approval-request
     (list :request-id "invalid-decision" :tool-name "bash")
     (lambda (_decision) (setq callback-called t)))
    (should-error
     (magent-approval-resolve-request "invalid-decision" 'surprise-allow)
     :type 'error)
    (should (magent-approval-pending-request "invalid-decision"))
    (should-not callback-called)
    (should-not (magent-approval-completed-request "invalid-decision"))))

(ert-deftest magent-test-once-only-approval-normalizes-before-publication ()
  "Once-only history, hooks, and callbacks never publish allow-session."
  (require 'magent-approval)
  (let ((magent-approval-provider-function #'ignore)
        (magent-approval--pending-requests (make-hash-table :test 'equal))
        (magent-approval--completed-requests (make-hash-table :test 'equal))
        (magent-approval-state-change-functions nil)
        callback-decision
        hook-decision)
    (add-hook
     'magent-approval-state-change-functions
     (lambda (event _request-id entry)
       (when (eq event 'resolved)
         (setq hook-decision (plist-get entry :decision)))))
    (magent-approval-request
     '(:request-id "once-normalized"
       :tool-name "emacs_eval"
       :approval-policy once-only)
     (lambda (decision) (setq callback-decision decision)))
    (magent-approval-resolve-request "once-normalized" 'allow-session)
    (should (eq callback-decision 'allow-once))
    (should (eq hook-decision 'allow-once))
    (should (eq (plist-get
                 (magent-approval-completed-request "once-normalized")
                 :decision)
                'allow-once))))

(ert-deftest magent-test-approval-sync-provider-preserves-lifecycle-order ()
  "A provider may resolve synchronously without inverting state events."
  (require 'magent-approval)
  (let ((magent-approval--pending-requests (make-hash-table :test 'equal))
        (magent-approval--completed-requests (make-hash-table :test 'equal))
        (magent-approval-state-change-functions nil)
        events)
    (add-hook
     'magent-approval-state-change-functions
     (lambda (event _request-id entry)
       (setq events
             (append events
                     (list (list event
                                 (and entry
                                      (plist-get
                                       (or (plist-get entry :request) entry)
                                       :tool-name))))))))
    (let ((magent-approval-provider-function
           (lambda (request)
             (magent-approval-resolve-request
              (plist-get request :request-id) 'allow-once))))
      (magent-approval-request
       (list :request-id "sync-provider" :tool-name "bash") #'ignore))
    (should (equal events '((requested "bash") (resolved "bash"))))))

(ert-deftest magent-test-audit-permission-session-override-is-persisted ()
  "Test session override permission decisions are persisted."
  (require 'magent-audit)
  (require 'magent-agent-loop)
  (require 'magent-permission)
  (let* ((directory (make-temp-file "magent-audit-" t))
         (magent-audit (expand-file-name "audit.jsonl" directory))
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
            (lambda (_tool-spec cb arg-values _resource-identity)
              (funcall cb
                       (magent-test-tool-result
                        (format "ran %s" (car arg-values)))))
            :audit-function #'magent-agent-loop-audit-permission-decision
            :file-arg-index-function #'magent-agent-loop-find-file-arg-index
            :args-to-plist-function #'magent-agent-loop-args-to-plist
            :summarize-function #'magent-agent-loop-summarize-args)
           (list (list tool (list "echo hi") (lambda (value)
                                               (setq result value)))))
          (should (equal result "ran echo hi"))
          (magent-audit--flush-pending)
          (let* ((records (magent-test--read-audit-records directory))
                 (record (car records)))
            (should (= (length records) 1))
            (should (equal (cdr (assq 'event record)) "permission-decision"))
            (should (equal (cdr (assq 'tool_name record)) "bash"))
            (should (equal (cdr (assq 'decision record)) "allow"))
            (should (equal (cdr (assq 'decision_source record))
                           "session-override-allow"))))
      (magent-audit-disable)
      (delete-directory directory t))))

(ert-deftest magent-test-audit-permission-file-rule-deny-is-persisted ()
  "Test file-rule deny permission decisions are persisted."
  (require 'magent-audit)
  (require 'magent-agent-loop)
  (let* ((directory (make-temp-file "magent-audit-" t))
         (magent-audit (expand-file-name "audit.jsonl" directory))
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
          (let* ((records (magent-test--read-audit-records directory))
                 (record (car records))
                 (args-preview (cdr (assq 'args_preview record))))
            (should (= (length records) 1))
            (should (equal (cdr (assq 'decision record)) "deny"))
            (should (equal (cdr (assq 'decision_source record)) "file-rule-deny"))
            (let ((path (cdr (assq 'path args-preview))))
              (should (string-suffix-p "/.env" path))
              (should-not (string-match-p
                           (regexp-quote (expand-file-name "~")) path)))))
      (magent-audit-disable)
      (delete-directory directory t))))

(ert-deftest magent-test-audit-write-failure-does-not-signal ()
  "Test audit persistence failures never interrupt Magent execution."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-" t))
         (magent-audit (expand-file-name "audit.jsonl" directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'magent-audit--append-batch)
                   (lambda (&rest _args) (error "disk full"))))
          (should-not
           (condition-case nil
               (progn
                 (magent-audit-record 'permission-decision
                                      :decision 'allow
                                      :decision-source 'bypass)
                 (magent-audit--flush-pending)
                 nil)
             (error t))))
      (delete-directory directory t))))

(ert-deftest magent-test-audit-record-queues-write-until-flush ()
  "Test audit writes stay queued until the deferred flush runs."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-" t))
         (magent-audit (expand-file-name "audit.jsonl" directory))
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
          (should-not (file-exists-p magent-audit))
          (magent-audit--flush-pending)
          (should-not magent-audit--pending-writes)
          (should (= (length (magent-test--read-audit-records directory)) 1)))
      (magent-audit--flush-pending)
      (delete-directory directory t))))

(ert-deftest magent-test-audit-browser-respects-default-time-window ()
  "Test the audit browser only shows records inside the default day window."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-ui-" t))
         (magent-audit (expand-file-name "audit-test.jsonl" directory))
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
           directory
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
          (setq buffer (magent-open-audit))
          (with-current-buffer buffer
            (should (derived-mode-p 'magent-audit-mode))
            (should (= (length magent-audit--all-records) 1))
            (should (= (length magent-audit--visible-records) 1))
            (should (string-match-p "recent audit record" (buffer-string)))
            (should-not (string-match-p "stale audit record" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest magent-test-audit-browser-filters-and-expands-details ()
  "Test audit browser filters records and expands inline details."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-ui-" t))
         (magent-audit (expand-file-name "audit-test.jsonl" directory))
         (magent-audit-default-days 7)
         (magent-audit-max-records 50)
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z" (current-time)))
         buffer)
    (unwind-protect
        (progn
          (magent-test--write-audit-record-file
           directory
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
          (setq buffer (magent-open-audit))
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
      (delete-directory directory t))))

(ert-deftest magent-test-audit-browser-skips-malformed-jsonl ()
  "Test malformed audit lines are ignored without breaking the browser."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-ui-" t))
         (magent-audit (expand-file-name "audit-test.jsonl" directory))
         (magent-audit-default-days 7)
         (magent-audit-max-records 50)
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z" (current-time)))
         (file magent-audit)
         buffer)
    (unwind-protect
        (progn
          (make-directory directory t)
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
          (setq buffer (magent-open-audit))
          (with-current-buffer buffer
            (should (= magent-audit--load-errors 1))
            (should (= (length magent-audit--visible-records) 1))
            (should (string-match-p "valid record after malformed line"
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest magent-test-session-scope-from-directory-falls-back-to-global ()
  "Test session scope is global when no project root is detected."
  (cl-letf (((symbol-function 'magent-project-root)
             (lambda (&optional _directory _no-fallback) nil)))
    (should (eq (magent-session-scope-from-directory "/tmp/") 'global))))

(ert-deftest magent-test-session-remote-scope-normalization-performs-no-file-io ()
  "Remote session routing normalizes TRAMP names without contacting the host."
  (let ((remote-root "/ssh:test.invalid:/srv/project/"))
    (cl-letf (((symbol-function 'magent-project-root)
               (lambda (&optional _directory _no-fallback) remote-root))
              ((symbol-function 'file-truename)
               (lambda (&rest _args)
                 (ert-fail "Remote scope normalization performed file I/O"))))
      (should (equal (magent-session-scope-from-directory remote-root)
                     (directory-file-name remote-root)))
      (should (equal (magent-session-canonical-scope remote-root)
                     (directory-file-name remote-root))))))

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
          (magent-test--record-session-entry (magent-session-get) 'user "hello")
          (magent-test--save-current-session)
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

(ert-deftest magent-test-session-save-global-uses-current-directory ()
  "Test global sessions save under the dedicated global directory."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (magent-test--record-session-entry (magent-session-get) 'user "hello")
          (magent-test--save-current-session)
          (should (= (length (magent-test--session-files
                              (expand-file-name "global"
                                                magent-session-directory)))
                     1)))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-action-session-saves-outside-normal-session-list ()
  "Test Action sessions use actions/ and stay out of conversation listings."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-action-session-directory nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (session (magent-session-create))
         (id (magent-session-get-id session))
         (scope (magent-session-action-scope id "memory-init" 'global)))
    (unwind-protect
        (progn
          (dolist (entry '((kind . "action")
                           (action . "memory-init")
                           (status . "completed")
                           (title . "Memory Init")
                           (origin-scope . global)))
            (magent-session-set-metadata-value session (car entry) (cdr entry)))
          (setq magent--current-session session
                magent-session--current-scope scope)
          (magent-test--record-session-entry session 'user "run memory init")
          (magent-test--save-current-session)
          (let* ((action-files
                  (magent-session-list-action-files "memory-init"))
                 (file (car action-files))
                 (meta (magent-session--read-file-metadata-cached file))
                 (loaded (magent-session-read-file file))
                 (loaded-session (plist-get loaded :session)))
            (should (= (length action-files) 1))
            (should (string-match-p "/actions/memory-init/" file))
            (should-not (member file (magent-session-list-files)))
            (should (equal (plist-get meta :kind) "action"))
            (should (equal (plist-get meta :action) "memory-init"))
            (should (equal (plist-get meta :status) "completed"))
            (should (equal (magent-session-metadata-value
                            loaded-session 'action)
                           "memory-init"))))
      (delete-directory magent-session-directory t))))

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
            (magent-test--record-session-entry session 'user "hello")
            (magent-permission-set-session-override 'bash 'allow session)
            (magent-test--save-current-session))
          (let* ((files (magent-test--session-files magent-session-directory))
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
            (magent-test--record-session-entry session 'user "Run ls")
            (magent-test--record-tool-entry
             session "call_1" "bash" '(:command "ls")
             (magent-test-tool-result "ok"))
            (magent-test--save-current-session))
          (let* ((files (magent-test--session-files magent-session-directory))
                 (loaded (magent-session-read-file (car files)))
                 (loaded-session (plist-get loaded :session))
                 (messages (magent-test--session-transcript loaded-session)))
            (should (= (length messages) 2))
            (should (equal (magent-test--transcript-content (nth 1 messages))
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
            (magent-test--record-session-entry session 'user "Run tool")
            (magent-test--record-tool-entry
             session "call_1" 'emacs_eval
             '(:sexp "(+ 20 22)" :tool emacs_eval :values [emacs_eval nil])
             (magent-test-tool-result "42"))
            (magent-test--save-current-session))
          (let* ((files (magent-test--session-files magent-session-directory))
                 (loaded (magent-session-read-file (car files)))
                 (loaded-session (plist-get loaded :session))
                 (messages (magent-test--session-transcript loaded-session)))
            (should (equal (magent-test--transcript-content (nth 1 messages))
                           '(:id "call_1"
				 :name "emacs_eval"
				     :args (:sexp "(+ 20 22)"
						      :tool "emacs_eval"
						      :values ["emacs_eval" :null])
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
    (magent-agent-job-set-status job 'failed nil "boom")
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
            (magent-test--record-session-entry session 'user "spawn child")
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
              :result "found it"))
            (magent-test--save-current-session))
          (let* ((files (magent-test--session-files magent-session-directory))
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

(ert-deftest magent-test-tools-child-agent-completion-persists-parent-session ()
  "Test child-agent completion updates and saves the parent session."
  (require 'magent-tools)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent-session--pending-saves nil)
         (magent-session--save-timer nil)
         (magent--current-session nil)
         (parent-session (magent-session-create :id "parent-session"))
         (parent-context (magent-request-context-create
                          :id "parent-request"
                          :scope 'global
                          :session parent-session
                          :approval-session parent-session
                          :ui-visibility 'full))
         (agent (magent-agent-info-create
                 :name "explore"
                 :mode 'subagent))
         child-callback
         ui-events)
    (unwind-protect
        (let ((magent-tools--request-context parent-context)
              (magent-agent-job--runtimes (make-hash-table :test #'equal)))
          (magent-session-install 'global parent-session)
          (magent-test--record-session-entry parent-session 'user "spawn child")
          (cl-letf (((symbol-function 'magent-agent-registry-get)
                     (lambda (_name) agent))
                    ((symbol-function 'magent-lifecycle-events-create-subagent-context)
                     (lambda (_title _parent _audit-context) 'child-context))
                    ((symbol-function 'magent-lifecycle-events-stop-subagent) #'ignore)
                    ((symbol-function 'magent-agent-run-turn)
                     (lambda (&rest args)
                       (let* ((prompt (plist-get args :prompt))
                              (request-state
                               (plist-get args :request-context)))
                       (setq child-callback (plist-get args :on-complete))
                       (magent-test--record-session-entry
                        (magent-request-context-session request-state)
                        'user prompt)
                       (magent-test--record-session-entry
                        (magent-request-context-session request-state)
                        'assistant
                        (concat "child saw " prompt))
                       nil)))
                    ((symbol-function 'magent-lifecycle-events-emit)
                     (lambda (type &rest props)
                       (when (eq type 'agent-job-event)
                         (let ((job (plist-get props :job)))
                           (push (list (plist-get props :event)
                                       (magent-agent-job-id job)
                                       (magent-agent-job-status job)
                                       (plist-get props :detail))
                                 ui-events))))))
            (magent-tools--spawn-agent #'ignore "explore" "inspect" "scan")
            (should child-callback)
            (funcall child-callback
                     (magent-execution-result-completed "child answer"))
            (magent-session--flush-deferred-saves))
          (let* ((files (magent-test--session-files magent-session-directory))
                 (loaded (magent-session-read-file (car files)))
                 (loaded-session (plist-get loaded :session))
                 (job (car (magent-session-agent-jobs loaded-session))))
            (should (= (length files) 1))
            (should job)
            (should (eq (magent-agent-job-status job) 'completed))
            (should (equal (magent-agent-job-result job) "child answer"))
            (should (equal (magent-agent-job-transcript job)
                           '(((role . "user")
                              (content . "inspect"))
                             ((role . "assistant")
                              (content . "child saw inspect")))))
            (should (assoc 'completed ui-events))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-llm-event-constructors ()
  "Test normalized LLM event constructors."
  (require 'magent-llm)
  (let ((text (magent-llm-text-delta-event "hello"))
        (reasoning (magent-llm-reasoning-delta-event "thinking"))
        (reasoning-end (magent-llm-reasoning-end-event))
        (tool (magent-llm-tool-call-event
               "call-1" "read_file" '(:path "README.org") 'raw))
        (tool-batch-end
         (magent-llm-tool-call-batch-end-event '(:provider gptel)))
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
    (should (eq (magent-llm-event-type tool-batch-end)
                'tool-call-batch-end))
    (should (equal (magent-llm-event-metadata tool-batch-end)
                   '(:provider gptel)))
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
                          '(:status "ok" :stream t))
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

(ert-deftest magent-test-llm-gptel-disable-provider-tools-keeps-tool-specs ()
  "Test metadata can hide tools from gptel while keeping request tools."
  (require 'magent-llm-gptel)
  (let ((captured-use-tools :unset)
        sample-handle)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (setq captured-use-tools gptel-use-tools)
                 (funcall (plist-get kwargs :callback)
                          t '(:content "done" :status "ok"))
                 'fake-fsm)))
      (setq sample-handle
            (magent-llm-gptel-sample
             (magent-llm-request-create
              :prompt '((user . "hello"))
              :tools '(fake-tool)
              :stream t
              :metadata '(:disable-provider-tools t)
              :callback #'ignore))))
    (when (buffer-live-p sample-handle)
      (kill-buffer sample-handle))
    (should-not captured-use-tools)))

(ert-deftest magent-test-llm-gptel-include-reasoning-metadata-overrides-default ()
  "Test request metadata can override `magent-include-reasoning'."
  (require 'magent-llm-gptel)
  (let ((magent-include-reasoning t)
        (captured-include-reasoning :unset)
        sample-handle)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (setq captured-include-reasoning gptel-include-reasoning)
                 (funcall (plist-get kwargs :callback)
                          t '(:content "done" :status "ok"))
                 'fake-fsm)))
      (setq sample-handle
            (magent-llm-gptel-sample
             (magent-llm-request-create
              :prompt '((user . "hello"))
              :stream t
              :metadata '(:include-reasoning nil)
              :callback #'ignore))))
    (when (buffer-live-p sample-handle)
      (kill-buffer sample-handle))
    (should-not captured-include-reasoning)))

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
           request state buffer '(reasoning . "think") '(:status "ok" :stream t))
          (magent-llm-gptel--callback
           request state buffer '(reasoning . t) '(:status "ok" :stream t))
          (magent-llm-gptel--callback
           request state buffer nil '(:status "HTTP/2 400"
                                      :http-status 400
                                      :error
                                      (:message "context length exceeded"
                                       :type "invalid_request_error")))
          (should (= (length events) 3))
          (should (eq (magent-llm-event-type (nth 2 events)) 'reasoning-delta))
          (should (equal (magent-llm-event-text (nth 2 events)) "think"))
          (should (eq (magent-llm-event-type (nth 1 events)) 'reasoning-end))
          (should (eq (magent-llm-event-type (nth 0 events)) 'error))
          (should (equal (magent-llm-event-message (nth 0 events))
                         "context length exceeded"))
          (should (equal (plist-get
                          (magent-llm-event-metadata (nth 0 events))
                          :status)
                         "HTTP/2 400"))
          (should (= (plist-get
                      (magent-llm-event-metadata (nth 0 events))
                      :http-status)
                     400)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-llm-gptel-reasoning-only-done-completes-empty ()
  "Test reasoning-only provider responses complete without leaking reasoning."
  (require 'magent-llm-gptel)
  (let* ((events nil)
         (request (magent-llm-request-create
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state))
         (fsm (gptel-make-fsm
               :info '(:status "ok"
			       :tokens (:total 3)
			       :stop-reason "stop"))))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback
           request state buffer '(reasoning . "你好！") '(:status "ok"))
          (magent-llm-gptel--handle-done request state buffer fsm)
          (magent-llm-gptel--handle-done request state buffer fsm)
          (should-not (buffer-live-p buffer))
          (should (= (length events) 1))
          (let ((completed (car events)))
            (should (eq (magent-llm-event-type completed) 'completed))
            (should (equal (magent-llm-event-text completed) ""))
            (should (equal (magent-llm-event-usage completed) '(:total 3)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-llm-gptel-nonstream-reasoning-flushes-before-content ()
  "Test cached non-streaming reasoning is emitted before final content."
  (require 'magent-llm-gptel)
  (let* ((events nil)
         (request (magent-llm-request-create
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state)))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback
           request state buffer '(reasoning . "thinking") '(:status "ok"))
          (magent-llm-gptel--callback
           request state buffer "answer" '(:status "ok"))
          (should (= (length events) 2))
          (let ((completed (car events))
                (reasoning (cadr events)))
            (should (eq (magent-llm-event-type reasoning) 'reasoning-delta))
            (should (equal (magent-llm-event-text reasoning) "thinking"))
            (should (eq (magent-llm-event-type completed) 'completed))
            (should (equal (magent-llm-event-text completed) "answer"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-llm-gptel-nonstream-string-completes ()
  "Test non-streaming gptel string responses map to completion events."
  (require 'magent-llm-gptel)
  (let* ((events nil)
         (request (magent-llm-request-create
                   :stream nil
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state)))
    (magent-llm-gptel--callback
     request state buffer "done" '(:status "ok" :tokens (:total 3)))
    (should-not (buffer-live-p buffer))
    (should (= (length events) 1))
    (let ((event (car events)))
      (should (eq (magent-llm-event-type event) 'completed))
      (should (equal (magent-llm-event-text event) "done"))
      (should (equal (magent-llm-event-usage event) '(:total 3))))))

(ert-deftest magent-test-llm-gptel-textual-dsml-tool-call-becomes-tool-event ()
  "Test pure textual DSML tool calls are normalized before completion."
  (require 'magent-llm-gptel)
  (let* ((events nil)
         (text
          (concat
           "<｜｜DSML｜｜tool_calls>\n"
           "<｜｜DSML｜｜invoke name=\"bash\">\n"
           "<｜｜DSML｜｜parameter name=\"command\" string=\"true\">"
           "git diff test/magent-test.el | tail -n 120"
           "</｜｜DSML｜｜parameter>\n"
           "<｜｜DSML｜｜parameter name=\"reason\" string=\"true\">"
           "Read remainder of test diff"
           "</｜｜DSML｜｜parameter>\n"
           "</｜｜DSML｜｜invoke>\n"
           "</｜｜DSML｜｜tool_calls>"))
         (request (magent-llm-request-create
                   :stream nil
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state)))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback
           request state buffer text (list :status "ok" :content text))
          (should-not (buffer-live-p buffer))
          (let ((ordered (nreverse events)))
            (should (= (length ordered) 2))
            (let ((tool-event (car ordered))
                  (batch-event (cadr ordered)))
              (should (eq (magent-llm-event-type tool-event) 'tool-call))
              (should (equal (magent-llm-event-name tool-event) "bash"))
              (should (equal
                       (magent-llm-event-arguments tool-event)
                       '(:command
                         "git diff test/magent-test.el | tail -n 120"
                         :reason
                         "Read remainder of test diff")))
              (should (eq (magent-llm-event-type batch-event)
                          'tool-call-batch-end)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-agent-loop-coerces-textual-dsml-integer-arguments ()
  "Test textual DSML numeric pages are restored from the tool schema."
  (require 'magent-agent-loop)
  (require 'magent-llm-gptel)
  (require 'gptel)
  (let* ((text
          (concat
           "<｜｜DSML｜｜tool_calls>\n"
           "<｜｜DSML｜｜invoke name=\"read_file\">\n"
           "<｜｜DSML｜｜parameter name=\"path\" string=\"true\">"
           "lisp/magent-agent-loop.el"
           "</｜｜DSML｜｜parameter>\n"
           "<｜｜DSML｜｜parameter name=\"start_line\" string=\"true\">"
           "121"
           "</｜｜DSML｜｜parameter>\n"
           "<｜｜DSML｜｜parameter name=\"line_count\" string=\"true\">"
           "100"
           "</｜｜DSML｜｜parameter>\n"
           "</｜｜DSML｜｜invoke>\n"
           "</｜｜DSML｜｜tool_calls>"))
         (event (car (magent-llm-gptel--parse-dsml-tool-calls text)))
         (tool (gptel-make-tool
                :name "read_file"
                :description "read"
                :args (list '(:name "path" :type string)
                            '(:name "start_line" :type integer :optional t)
                            '(:name "line_count" :type integer :optional t))
                :function #'ignore))
         (loop (magent-agent-loop-create
                :request (magent-llm-request-create :tools (list tool))))
         (call (magent-agent-loop-tool-event-to-call loop event))
         (raw-call (nth 3 call)))
    (should (equal (cadr call)
                   '("lisp/magent-agent-loop.el" 121 100)))
    (should (equal (plist-get raw-call :args)
                   '(:path "lisp/magent-agent-loop.el"
			   :start_line 121
			   :line_count 100)))))

(ert-deftest magent-test-agent-loop-rejects-unknown-textual-dsml-argument ()
  "Test textual DSML arguments outside the schema become tool errors."
  (require 'magent-agent-loop)
  (require 'magent-llm-gptel)
  (require 'gptel)
  (let* ((text
          (concat
           "<｜｜DSML｜｜tool_calls>\n"
           "<｜｜DSML｜｜invoke name=\"read_file\">\n"
           "<｜｜DSML｜｜parameter name=\"path\" string=\"true\">"
           "lisp/magent-agent-shell.el"
           "</｜｜DSML｜｜parameter>\n"
           "<｜｜DSML｜｜parameter name=\"end_line\" string=\"true\">"
           "100"
           "</｜｜DSML｜｜parameter>\n"
           "</｜｜DSML｜｜invoke>\n"
           "</｜｜DSML｜｜tool_calls>"))
         (event (car (magent-llm-gptel--parse-dsml-tool-calls text)))
         (tool (gptel-make-tool
                :name "read_file"
                :description "read"
                :args (list '(:name "path" :type string)
                            '(:name "start_line" :type integer :optional t)
                            '(:name "line_count" :type integer :optional t))
                :function #'ignore))
         (session (magent-session-create :id "session-dsml-invalid-args"))
         (loop (magent-test--loop-create-for-session
                session "Read file"
                :request (magent-llm-request-create :tools (list tool))))
         outcome)
    (magent-agent-loop-apply-event loop event)
    (magent-agent-loop-dispatch-tool-calls
     loop
     (magent-tool-orchestrator-create
      :run-tool-function (lambda (&rest _args) (error "should not run")))
     (lambda (result) (setq outcome result)))
    (should (eq (plist-get outcome :status) 'failed))
    (let* ((message (magent-test--latest-tool-transcript session))
           (content (magent-test--transcript-content message))
           (result (plist-get content :result)))
      (should (string-match-p "unknown argument: end_line" result))
      (should (string-match-p
               "available arguments: path, start_line, line_count"
               result)))))

(ert-deftest magent-test-agent-loop-coerces-textual-dsml-schema-types ()
  "Test textual DSML values follow number, boolean, and array schemas."
  (require 'magent-agent-loop)
  (require 'gptel)
  (let* ((tool (gptel-make-tool
                :name "typed_tool"
                :description "typed"
                :args (list '(:name "ratio" :type number)
                            '(:name "enabled" :type boolean)
                            '(:name "names" :type array
                                    :items (:type string)))
                :function #'ignore))
         (normalized
          (magent-agent-loop--normalize-textual-tool-args
           tool '(:ratio "1.5" :enabled "false" :names "[\"a\",\"b\"]"))))
    (should (equal normalized
                   '(:ratio 1.5 :enabled :json-false :names ["a" "b"])))
    (should (equal (magent-agent-loop--tool-arg-values tool normalized t)
                   '(1.5 nil ["a" "b"])))))

(ert-deftest magent-test-llm-gptel-mixed-textual-dsml-tool-call-becomes-tool-event ()
  "Test mixed prose plus textual DSML tool calls becomes tool events."
  (require 'magent-llm-gptel)
  (let* ((events nil)
         (text
          (concat
           "I'll continue reading the remaining diff sections.\n\n"
           "<｜｜DSML｜｜tool_calls>\n"
           "<｜｜DSML｜｜invoke name=\"bash\">\n"
           "<｜｜DSML｜｜parameter name=\"command\" string=\"true\">"
           "git diff test/magent-test.el | sed -n '550,750p'"
           "</｜｜DSML｜｜parameter>\n"
           "<｜｜DSML｜｜parameter name=\"reason\" string=\"true\">"
           "Continue test diff"
           "</｜｜DSML｜｜parameter>\n"
           "</｜｜DSML｜｜invoke>\n"
           "</｜｜DSML｜｜tool_calls>"))
         (request (magent-llm-request-create
                   :stream nil
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state)))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback
           request state buffer text (list :status "ok" :content text))
          (should-not (buffer-live-p buffer))
          (let ((ordered (nreverse events)))
            (should (= (length ordered) 2))
            (let ((tool-event (car ordered))
                  (batch-event (cadr ordered)))
              (should (eq (magent-llm-event-type tool-event) 'tool-call))
              (should (equal (magent-llm-event-name tool-event) "bash"))
              (should (equal
                       (magent-llm-event-arguments tool-event)
                       '(:command
                         "git diff test/magent-test.el | sed -n '550,750p'"
                         :reason
                         "Continue test diff")))
              (should (eq (magent-llm-event-type batch-event)
                          'tool-call-batch-end)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-llm-gptel-textual-dsml-malformed-completes ()
  "Test malformed textual DSML envelopes complete as text without looping."
  (require 'magent-llm-gptel)
  (let* ((events nil)
         (text
          (concat
           "<｜｜DSML｜｜tool_calls>\n"
           "<｜｜DSML｜｜invoke name=\"bash\">\n"
           (make-string 10000 ?x)
           "\n</｜｜DSML｜｜tool_calls>"))
         (request (magent-llm-request-create
                   :stream nil
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state)))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback
           request state buffer text (list :status "ok" :content text))
          (should-not (buffer-live-p buffer))
          (should (= (length events) 1))
          (let ((event (car events)))
            (should (eq (magent-llm-event-type event) 'completed))
            (should (equal (magent-llm-event-text event) text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest magent-test-llm-gptel-stream-final-empty-content-keeps-chunks ()
  "Test streaming completion does not replace chunks with empty content."
  (require 'magent-llm-gptel)
  (let* ((events nil)
         (request (magent-llm-request-create
                   :stream t
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state)))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback
           request state buffer "MAGENT_TOOL_OK=42" '(:stream t))
          (magent-llm-gptel--callback
           request state buffer t '(:content "" :tokens (:total 5)))
          (let ((event (car events)))
            (should (eq (magent-llm-event-type event) 'completed))
            (should (equal (magent-llm-event-text event)
                           "MAGENT_TOOL_OK=42"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-llm-gptel-normalizes-tool-call ()
  "Test gptel adapter maps tool-call callbacks to normalized events."
  (require 'magent-llm-gptel)
  (let* ((events nil)
         (result-callback #'ignore)
         (continuation #'ignore)
         (request (magent-llm-request-create
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state)))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback
           request state buffer
           (list 'tool-call
                 (list nil '(:path "README.org") result-callback
                       '(:id "call-1"
                         :name "read_file"
                         :args (:path "README.org"))))
           (list :status "ok" :magent-tool-continuation continuation))
          (should (= (length events) 2))
          (let ((batch-end (car events))
                (event (cadr events)))
            (should (eq (magent-llm-event-type batch-end)
                        'tool-call-batch-end))
            (should (eq (magent-llm-event-type event) 'tool-call))
            (should (equal (magent-llm-event-id event) "call-1"))
            (should (equal (magent-llm-event-name event) "read_file"))
            (should (equal (magent-llm-event-arguments event)
                           '(:path "README.org")))
            (should (eq (magent-llm-event-result-callback event)
                        result-callback))
            (should (eq (magent-llm-event-continuation batch-end)
                        continuation))
            (should-not
             (plist-member (magent-llm-event-metadata event) :last)))
          (should (buffer-live-p buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-llm-gptel-continues-native-parallel-tool-context ()
  "Tool results resume gptel's original parallel assistant message unchanged."
  (require 'magent-llm-gptel)
  (require 'gptel-openai)
  (let* ((backend (gptel-make-openai
                    "continuation"
                    :key "test-key"
                    :host "api.deepseek.com"
                    :endpoint "/v1/chat/completions"))
         (tool (gptel-make-tool
                :name "grep"
                :description "Search"
                :args '((:name "pattern" :type string))
                :function #'ignore))
         (tool-calls
          [(:id "call-1" :type "function"
            :function (:name "grep" :arguments "{\"pattern\":\"one\"}"))
           (:id "call-2" :type "function"
            :function (:name "grep" :arguments "{\"pattern\":\"two\"}"))])
         (assistant (list :role "assistant"
                          :content :null
                          :reasoning_content "Inspect both locations."
                          :tool_calls tool-calls))
         (data (list :messages (vector assistant)))
         (tool-use (list (list :id "call-1" :name "grep"
                               :args '(:pattern "one"))
                         (list :id "call-2" :name "grep"
                               :args '(:pattern "two"))))
         response
         next-state
         (state (magent-llm-gptel--make-state))
         (info (list :backend backend
                     :data data
                     :tools (list tool)
                     :tool-use tool-use
                     :callback (lambda (value _info) (setq response value))))
         (fsm (gptel-make-fsm :state 'TOOL :info info)))
    (magent-llm-gptel--handle-tool-use state fsm)
    (let ((calls (cdr response)))
      (should (= (length calls) 2))
      (funcall (nth 2 (nth 0 calls))
               (magent-tool-result-create
                :status 'completed :success t :output "one.el:10"))
      (funcall (nth 2 (nth 1 calls)) "two.el:20"))
    (cl-letf (((symbol-function 'gptel--fsm-transition)
               (lambda (_fsm state) (setq next-state state))))
      (funcall (plist-get info :magent-tool-continuation)))
    (let ((messages (plist-get data :messages)))
      (should (eq next-state 'WAIT))
      (should (= (length messages) 3))
      (should (eq (aref messages 0) assistant))
      (should (= (length (plist-get (aref messages 0) :tool_calls)) 2))
      (should (equal (plist-get (aref messages 0) :reasoning_content)
                     "Inspect both locations."))
      (should (equal (mapcar (lambda (message)
                               (plist-get message :tool_call_id))
                             (cdr (append messages nil)))
                     '("call-1" "call-2"))))))

(ert-deftest magent-test-llm-gptel-recovers-textual-tool-in-native-context ()
  "Textual tool recovery continues OpenAI chat context without forged history."
  (require 'magent-llm-gptel)
  (require 'gptel-openai)
  (let* ((backend (gptel-make-openai
                    "textual-continuation"
                    :key "test-key"
                    :host "api.deepseek.com"
                    :endpoint "/v1/chat/completions"))
         (prior-assistant
          (list :role "assistant"
                :content :null
                :reasoning_content "Inspect the widget implementation."
                :tool_calls
                [(:id "call-1" :type "function"
                  :function
                  (:name "read_file"
                   :arguments "{\"path\":\"widgets.py\"}"))]))
         (prior-result
          (list :role "tool" :tool_call_id "call-1" :content "source"))
         (data (list :messages (vector prior-assistant prior-result)))
         (text
          (concat
           "<｜｜DSML｜｜tool_calls>\n"
           "<｜｜DSML｜｜invoke name=\"grep\">\n"
           "<｜｜DSML｜｜parameter name=\"pattern\" string=\"true\">"
           "def update"
           "</｜｜DSML｜｜parameter>\n"
           "</｜｜DSML｜｜invoke>\n"
           "</｜｜DSML｜｜tool_calls>"))
         events
         next-state
         (request (magent-llm-request-create
                   :stream t
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state))
         (info (list :stream t :backend backend :data data :content text))
         (fsm (gptel-make-fsm :state 'DONE :info info)))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback
           request state buffer text info fsm)
          (magent-llm-gptel--callback
           request state buffer t info fsm)
          (let* ((ordered (nreverse events))
                 (tool-event
                  (cl-find 'tool-call ordered
                           :key #'magent-llm-event-type))
                 (batch-event
                  (cl-find 'tool-call-batch-end ordered
                           :key #'magent-llm-event-type))
                 (continuation
                  (magent-llm-event-continuation batch-event)))
            (should tool-event)
            (should (functionp
                     (magent-llm-event-result-callback tool-event)))
            (should (functionp continuation))
            (should (eq (plist-get (magent-llm-event-metadata batch-event)
                                   :source)
                        'textual-dsml))
            (should (buffer-live-p buffer))
            (funcall
             (magent-llm-event-result-callback tool-event)
             (magent-tool-result-create
              :status 'completed
              :success t
              :output "widgets.py:1862:def update(self):"))
            (cl-letf (((symbol-function 'gptel--fsm-transition)
                       (lambda (_fsm state-name)
                         (setq next-state state-name))))
              (funcall continuation)
              (funcall continuation))
            (let* ((messages (plist-get data :messages))
                   (recovery (aref messages 2)))
              (should (eq next-state 'WAIT))
              (should (= (length messages) 3))
              (should (eq (aref messages 0) prior-assistant))
              (should (eq (aref messages 1) prior-result))
              (should (equal
                       (plist-get prior-assistant :reasoning_content)
                       "Inspect the widget implementation."))
              (should (= (cl-count "assistant" messages
                                   :key (lambda (message)
                                          (plist-get message :role))
                                   :test #'equal)
                         1))
              (should (equal (plist-get recovery :role) "user"))
              (should (string-match-p
                       "widgets.py:1862:def update"
                       (plist-get recovery :content)))
              (should (string-match-p
                       "native tool-call protocol"
                       (plist-get recovery :content))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-llm-gptel-closes-empty-native-tool-context ()
  "Empty post-tool replies close the FSM so policy can change on retry."
  (require 'magent-llm-gptel)
  (require 'gptel-openai)
  (let* ((backend (gptel-make-openai
                    "empty-continuation"
                    :key "test-key"
                    :host "api.deepseek.com"
                    :endpoint "/v1/chat/completions"))
         (assistant
          (list :role "assistant"
                :content :null
                :reasoning_content "Inspect the source."
                :tool_calls
                [(:id "call-1" :type "function"
                  :function (:name "grep"
                             :arguments "{\"pattern\":\"update\"}"))]))
         (tool-result
          (list :role "tool" :tool_call_id "call-1" :content "match"))
         (data (list :messages (vector assistant tool-result)))
         events
         (request (magent-llm-request-create
                   :stream t
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state))
         (info (list :stream t
                     :backend backend
                     :data data
                     :content ""
                     :magent-after-tool-output t))
         (fsm (gptel-make-fsm :state 'DONE :info info)))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback request state buffer t info fsm)
          (let* ((completed (car events))
                 (continuation
                  (magent-llm-event-continuation completed)))
            (should (eq (magent-llm-event-type completed) 'completed))
            (should (equal (magent-llm-event-text completed) ""))
            (should-not continuation)
            (should-not (buffer-live-p buffer))
            (let ((messages (plist-get data :messages)))
              (should (= (length messages) 2))
              (should (eq (aref messages 0) assistant))
              (should (eq (aref messages 1) tool-result))
              (should (equal (plist-get assistant :reasoning_content)
                             "Inspect the source.")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-llm-gptel-sanitizes-tool-use-info ()
  "Test gptel adapter normalizes tool-use data before serialization."
  (require 'magent-llm-gptel)
  (let* ((info (list :context '(:magent-llm-gptel t)
                     :tool-use (list (list :id "call-1"
                                           :name 'emacs_eval
                                           :args '(:sexp (+ 20 22)
							 :missing nil)))))
         (tool-call (car (plist-get info :tool-use))))
    (magent-llm-gptel--sanitize-info info)
    (should (equal (plist-get tool-call :name) "emacs_eval"))
    (should (equal (plist-get tool-call :args)
                   '(:sexp ["+" 20 22])))))

(ert-deftest magent-test-llm-gptel-sanitizes-assistant-tool-call-history ()
  "Test assistant tool-call history is safe for gptel JSON encoding."
  (require 'magent-llm-gptel)
  (let* ((func (list :name 'emacs_eval
                     :arguments '(:sexp (+ 20 22)
					:ignored nil)))
         (tool-call (list :type "function"
                          :id "call-1"
                          :function func))
         (message (list :role "assistant"
                        :content :null
                        :tool_calls (vector tool-call)))
         (data (list :messages (vector message)))
         (info (list :context '(:magent-llm-gptel t)
                     :data data)))
    (magent-llm-gptel--sanitize-info info)
    (should (equal (plist-get func :name) "emacs_eval"))
    (should (stringp (plist-get func :arguments)))
    (should (equal (let ((json-object-type 'plist)
                         (json-array-type 'list))
                     (json-read-from-string
                      (plist-get func :arguments)))
                   '(:sexp ("+" 20 22))))))

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
    (magent-agent-loop-apply-event
     loop (magent-llm-tool-call-batch-end-event))
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

(ert-deftest magent-test-agent-loop-completion-keeps-streamed-prefix ()
  "Test completion keeps text streamed within the current sample."
  (require 'magent-agent-loop)
  (let ((loop (magent-agent-loop-create)))
    (magent-agent-loop-apply-event
     loop (magent-llm-text-delta-event "Checking buffers. "))
    (magent-agent-loop-apply-event
     loop (magent-llm-completed-event "Done."))
    (should (equal (magent-agent-loop-result loop)
                   "Checking buffers. Done."))))

(ert-deftest magent-test-agent-loop-terminal-result-is-current-sample-only ()
  "Test sample boundaries separate the terminal result from the transcript."
  (require 'magent-agent-loop)
  (let ((loop (magent-agent-loop-create)))
    (magent-agent-loop-begin-sample loop)
    (magent-agent-loop-apply-event
     loop (magent-llm-text-delta-event "Investigating. "))
    (magent-agent-loop-apply-event
     loop (magent-llm-tool-call-event
           "call-1" "read_file" '(:path "one.el")))
    (magent-agent-loop-begin-sample loop)
    (magent-agent-loop-apply-event
     loop (magent-llm-text-delta-event "Verifying. "))
    (magent-agent-loop-apply-event
     loop (magent-llm-tool-call-event
           "call-2" "read_file" '(:path "two.el")))
    (magent-agent-loop-begin-sample loop)
    (magent-agent-loop-apply-event
     loop (magent-llm-text-delta-event "Fixed."))
    (magent-agent-loop-apply-event
     loop (magent-llm-completed-event "Fixed."))
    (should (equal (magent-agent-loop-sample-text loop) "Fixed."))
    (should (equal (magent-agent-loop-result loop) "Fixed."))
    (should (equal (magent-agent-loop-text loop)
                   "Investigating. Verifying. Fixed."))
    (should (equal (magent-agent-loop-transcript loop)
                   "Investigating. Verifying. Fixed."))))

(ert-deftest magent-test-agent-loop-discards-only-current-textual-sample ()
  "Test textual tool normalization preserves earlier sample transcript."
  (require 'magent-agent-loop)
  (let ((loop (magent-agent-loop-create)))
    (magent-agent-loop-begin-sample loop)
    (magent-agent-loop-apply-event
     loop (magent-llm-text-delta-event "Keep this. "))
    (magent-agent-loop-begin-sample loop)
    (magent-agent-loop-apply-event
     loop (magent-llm-text-delta-event "<tool>discard</tool>"))
    (magent-agent-loop-discard-sample-text loop)
    (should (equal (magent-agent-loop-text loop) "Keep this. "))
    (should (equal (magent-agent-loop-sample-text loop) ""))
    (should-not (magent-agent-loop-result loop))))

(ert-deftest magent-test-agent-loop-completion-does-not-use-reasoning ()
  "Test empty final content does not leak reasoning as assistant text."
  (require 'magent-agent-loop)
  (let ((loop (magent-agent-loop-create)))
    (magent-agent-loop-apply-event
     loop (magent-llm-reasoning-delta-event "MAGENT_TOOL_OK=42"))
    (magent-agent-loop-apply-event
     loop (magent-llm-completed-event ""))
    (should (equal (magent-agent-loop-result loop)
                   ""))))

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

(ert-deftest magent-test-agent-loop-records-tool-result-in-session ()
  "Test agent loop records model-visible tool results in session."
  (require 'magent-agent-loop)
  (let* ((session (magent-session-create :id "session-1"))
         (loop (magent-test--loop-create-for-session
                session "Read README")))
    (magent-agent-loop-record-tool-result
     loop nil '(:path "README.org") '(:id "call-1" :name "read_file")
     (magent-test-tool-result "content"))
    (let* ((message (magent-test--latest-tool-transcript session))
           (content (magent-test--transcript-content message))
           (thread (magent-session-thread-ledger session))
           (turn (car (magent-thread-turns thread)))
           (item (cl-find 'tool (magent-thread-turn-items turn)
                          :key #'magent-thread-item-type)))
      (should (eq (magent-test--transcript-role message) 'tool))
      (should (equal (plist-get content :id) "call-1"))
      (should (equal (plist-get content :name) "read_file"))
      (should (equal (plist-get content :args) '(:path "README.org")))
      (should (equal (plist-get content :result) "content"))
      (should (equal (magent-thread-item-metadata item)
                     '(:source "tool-result"))))))

(ert-deftest magent-test-agent-loop-projects-oversized-tool-result-once ()
  "Provider and ledger share one failed-result projection and spill id."
  (require 'magent-agent-loop)
  (let* ((magent-session-directory (make-temp-file "magent-loop-spill-" t))
         (magent-tool-result-model-max-length 40)
         (magent-tool-result-model-preview-length 20)
         (session (magent-session-create :id "loop-spill"))
         (context (magent-request-context-create
                   :scope 'global :session session))
         (payload (make-string 200 ?f))
         (tool (gptel-make-tool
                :name "bash"
                :args (list '(:name "command" :type string))
                :function
                (lambda (_command)
                  (magent-tool-result-create
                   :status 'failed :success nil :exit-code 7
                   :output payload :error payload))
                :async nil))
         (loop (magent-test--loop-create-for-session
                session "Run large command"
                :request-context context
                :request (magent-llm-request-create :tools (list tool))))
         provider-result
         done)
    (unwind-protect
        (progn
          (magent-agent-loop-apply-event
           loop
           (magent-llm-tool-call-event
            "spill-call" "bash" '("large")
            '(:id "spill-call" :name "bash")
            nil
            (lambda (value) (setq provider-result value))))
          (magent-agent-loop-dispatch-tool-calls
           loop
           (magent-agent-loop-create-orchestrator
            loop '((bash . allow)) context)
           (lambda (&optional _outcome) (setq done t)))
          (should done)
          (let* ((thread (magent-session-thread-ledger session))
                 (item (cl-find "spill-call" (magent-thread-all-items thread)
                                :key #'magent-thread-item-call-id
                                :test #'equal))
                 (ledger-result (magent-thread-item-output item))
                 (spill (plist-get (magent-thread-item-metadata item) :spill))
                 (result-id (plist-get spill :result-id))
                 (directory (magent-tool-output-spill--directory
                             'global "loop-spill")))
            (should (equal provider-result ledger-result))
            (should (= 1 (length (magent-tool-output-spill--files directory))))
            (let ((header-start
                   (string-match "\\[Tool result: status=failed;"
                                 ledger-result)))
              (should header-start)
              (should-not
               (string-match "\\[Tool result: status=failed;"
                             ledger-result (1+ header-start))))
            (let ((id-start (string-match (regexp-quote result-id)
                                          ledger-result)))
              (should id-start)
              (should-not (string-match (regexp-quote result-id)
                                        ledger-result (1+ id-start))))
            (should (equal
                     payload
                     (with-temp-buffer
                       (insert-file-contents
                        (magent-tool-output-spill-file
                         'global "loop-spill" result-id))
                       (buffer-string))))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-agent-loop-tool-args-drop-json-null ()
  "Test JSON null tool args are omitted from model-visible args."
  (require 'magent-agent-loop)
  (require 'gptel)
  (let* ((tool (gptel-make-tool
                :name "read_file"
                :description "read"
                :args (list '(:name "path" :type string)
                            '(:name "reason" :type string))
                :function #'ignore))
         (values (magent-agent-loop--tool-arg-values
                  tool
                  '(:path :null :reason :null))))
    (should (equal values '(nil nil)))
    (should
     (equal
      (magent-agent-loop--tool-args-plist
       tool values '(:id "call-1" :name "read_file"))
      nil))))

(ert-deftest magent-test-agent-loop-records-tool-approval-metadata ()
  "Test tool approval metadata is preserved in ledger tool items."
  (require 'magent-agent-loop)
  (let* ((session (magent-session-create :id "session-approval"))
         (loop (magent-test--loop-create-for-session
                session "Read with approval")))
    (magent-agent-loop-record-tool-result
     loop nil '(:path "README.org")
     '(:id "call-1"
	   :name "read_file"
	   :approval-decision allow
	   :approval-source rule-allow)
     (magent-test-tool-result "content"))
    (let* ((thread (magent-session-thread-ledger session))
           (turn (car (magent-thread-turns thread)))
           (item (cl-find 'tool (magent-thread-turn-items turn)
                          :key #'magent-thread-item-type)))
      (should (equal (magent-thread-item-metadata item)
                     '(:source "tool-result"
			       :approval-decision "allow"
			       :approval-source "rule-allow"))))))

(ert-deftest magent-test-agent-loop-dispatches-known-tool-call ()
  "Test agent loop dispatches known tools through the orchestrator."
  (require 'magent-agent-loop)
  (require 'gptel)
  (let* ((session (magent-session-create :id "session-1"))
         (project-root (file-truename default-directory))
         (canonical-readme
          (file-truename (expand-file-name "README.org" project-root)))
         (request-context
          (magent-request-context-create
           :scope project-root :project-root project-root :session session))
         (tool (gptel-make-tool
                :name "read_file"
                :description "read"
                :args (list '(:name "path" :type string))
                :function
                (lambda (path)
                  (magent-test-tool-result (format "read %s" path)))
                :async nil))
         (request (magent-llm-request-create :tools (list tool)))
         (loop (magent-test--loop-create-for-session
                session "Read README"
                :request request))
         done)
    (magent-agent-loop-apply-event
     loop
     (magent-llm-tool-call-event
      "call-1" "read_file" '("README.org") '(:id "call-1" :name "read_file")))
    (magent-agent-loop-dispatch-tool-calls
     loop
     (magent-tool-orchestrator-create
      :permission (magent-permission-defaults)
      :request-context request-context
      :run-tool-function
      (lambda (tool-spec cb arg-values _resource-identity)
        (funcall cb (apply (gptel-tool-function tool-spec) arg-values)))
      :file-arg-index-function (lambda (_args-spec) 0)
      :args-to-plist-function (lambda (_args-spec arg-values) arg-values)
      :summarize-function (lambda (arg-values _args-spec) (car arg-values)))
     (lambda (&optional _result) (setq done t)))
    (should done)
    (let* ((message (magent-test--latest-tool-transcript session))
           (content (magent-test--transcript-content message)))
      (should (eq (magent-test--transcript-role message) 'tool))
      (should (equal (plist-get content :id) "call-1"))
      (should (equal (plist-get content :name) "read_file"))
      (should (equal (plist-get content :args)
                     (list :path canonical-readme)))
      (should (equal (plist-get content :result)
                     (format "read %s" canonical-readme))))))

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
         (loop (magent-test--loop-create-for-session
                session "Use missing tool"
                :request (magent-llm-request-create :tools (list known))))
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
    (let* ((message (magent-test--latest-tool-transcript session))
           (content (magent-test--transcript-content message)))
      (should (eq (magent-test--transcript-role message) 'tool))
      (should (equal (plist-get content :name) "missing_tool"))
      (should (string-match-p
               "tool 'missing_tool' not found"
               (plist-get content :result))))))

(ert-deftest magent-test-agent-loop-continue-rebuilds-prompt-from-session ()
  "Test loop continuation samples from the latest session transcript."
  (require 'magent-agent-loop)
  (let* ((session (magent-session-create :id "session-1"))
         (sampled-prompt nil)
         (request (magent-llm-request-create
                   :prompt '((prompt . "old"))
                   :system "system"
                   :stream t))
         (loop (magent-test--loop-create-for-session
                session "Run tool"
                :request request
                :sampler (lambda (sample-request)
                           (setq sampled-prompt
                                 (magent-llm-request-prompt sample-request))
                           'continued))))
    (magent-agent-loop-record-tool-result
     loop nil '(:path "README.org") '(:id "call-1" :name "read_file")
     (magent-test-tool-result "content"))
    (should (eq (magent-agent-loop-continue loop) 'continued))
    (should (equal sampled-prompt
                   '((prompt . "Run tool")
                     (tool :id "call-1"
                           :name "read_file"
                           :args (:path "README.org")
                           :result "content"))))))

(ert-deftest magent-test-agent-loop-runs-duplicate-emacs-eval-calls ()
  "Test duplicate emacs_eval calls flow through normal tool execution."
  (require 'magent-agent-loop)
  (require 'gptel)
  (let* ((tool-runs nil)
         (results nil)
         (tool (gptel-make-tool
                :name "emacs_eval"
                :description "eval"
                :args (list '(:name "sexp" :type string))
                :function (lambda (sexp)
                            (push sexp tool-runs)
                            (magent-test-tool-result
                             (format "result %d" (length tool-runs))))
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
    (should (equal (nreverse tool-runs)
                   '("(length (buffer-list))"
                     "(length (buffer-list))")))
    (should (= (length results) 2))
    (should (equal (mapcar #'magent-test-tool-output (nreverse results))
                   '("result 1" "result 2")))))

(ert-deftest magent-test-agent-loop-run-tool-emits-visible-events ()
  "Test loop tool events carry normalized visible presentation data."
  (require 'magent-agent-loop)
  (require 'gptel)
  (let* ((events nil)
         (result nil)
         (tool (gptel-make-tool
                :name "bash"
                :description "shell"
                :args (list '(:name "command" :type string)
                            '(:name "reason" :type string))
                :function (lambda (command)
                            (magent-test-tool-result
                             (format "ran %s" command)))
                :async nil))
         (context (magent-request-context-create
                   :ui-visibility 'full
                   :event-context 'ctx))
         (loop (magent-agent-loop-create)))
    (cl-letf (((symbol-function 'magent-lifecycle-events-emit)
               (lambda (type &rest props)
                 (push (cons type props) events))))
      (magent-agent-loop-run-tool
       loop context tool (lambda (value) (setq result value))
       (list "echo hi" "inspect shell")))
    (should (equal (magent-test-tool-output result) "ran echo hi"))
    (let ((ordered-events (nreverse events)))
      (should (equal (mapcar #'car ordered-events)
                     '(tool-call-start tool-call-end)))
      (let ((start (cdr (car ordered-events))))
        (should (eq (plist-get start :context) 'ctx))
        (should (equal (plist-get start :tool-name) "bash"))
        (should (plist-get start :ui-visible))
        (should (equal (plist-get start :summary)
                       "[inspect shell] echo hi"))))))

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

(ert-deftest magent-test-agent-loop-start-schedules-request-timeout ()
  "Test loop start aborts and reports timeout for hung provider requests."
  (require 'magent-agent-loop)
  (let ((scheduled nil)
        (cancelled nil)
        (aborted-handle nil)
        (events nil)
        (magent-request-timeout 5))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (secs repeat fn &rest args)
                 (setq scheduled (list secs repeat fn args))
                 'timeout-timer))
              ((symbol-function 'cancel-timer)
               (lambda (timer)
                 (setq cancelled timer)))
              ((symbol-function 'magent-agent-loop--abort-request-handle)
               (lambda (handle)
                 (setq aborted-handle handle))))
      (let* ((request (magent-llm-request-create
                       :prompt '((prompt . "hello"))
                       :callback (lambda (event) (push event events))))
             (loop (magent-agent-loop-create
                    :request request
                    :sampler (lambda (_request) 'provider-handle))))
        (should (eq (magent-agent-loop-start loop) 'provider-handle))
        (should (equal (car scheduled) 5))
        (should (eq (magent-agent-loop-request-timeout-timer loop)
                    'timeout-timer))
        (apply (nth 2 scheduled) (nth 3 scheduled))
        (should (eq aborted-handle 'provider-handle))
        (should (eq (magent-agent-loop-status loop) 'failed))
        (should (string-match-p "Request timed out after 5 seconds"
                                (magent-agent-loop-error loop)))
        (should (eq (magent-llm-event-type (car events)) 'error))
        (should-not (magent-agent-loop-request-timeout-timer loop))
        (should-not cancelled)))))

(ert-deftest magent-test-agent-loop-terminal-event-cancels-request-timeout ()
  "Test completed provider requests cancel their timeout timer."
  (require 'magent-agent-loop)
  (let ((cancelled nil)
        (magent-request-timeout 5))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (list :timer fn args)))
              ((symbol-function 'cancel-timer)
               (lambda (timer)
                 (setq cancelled timer))))
      (let* ((sampled-request nil)
             (request (magent-llm-request-create
                       :prompt '((prompt . "hello"))))
             (loop (magent-agent-loop-create
                    :request request
                    :sampler (lambda (sample-request)
                               (setq sampled-request sample-request)
                               'provider-handle))))
        (magent-agent-loop-start loop)
        (let ((timer (magent-agent-loop-request-timeout-timer loop)))
          (funcall (magent-llm-request-callback sampled-request)
                   (magent-llm-completed-event "done"))
          (should (eq cancelled timer))
          (should-not (magent-agent-loop-request-timeout-timer loop)))))))

(ert-deftest magent-test-agent-loop-tool-call-without-batch-end-times-out ()
  "A provider that omits tool-call-batch-end cannot leave the loop hung."
  (require 'magent-agent-loop)
  (let ((magent-request-timeout 5)
        scheduled
        sampled-request
        aborted-handle)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (setq scheduled (cons fn args))
                 (list :timer fn args)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'magent-agent-loop--abort-request-handle)
               (lambda (handle) (setq aborted-handle handle))))
      (let* ((request (magent-llm-request-create
                       :prompt '((prompt . "hello"))))
             (loop (magent-agent-loop-create
                    :request request
                    :sampler (lambda (sample-request)
                               (setq sampled-request sample-request)
                               'provider-handle))))
        (magent-agent-loop-start loop)
        (funcall (magent-llm-request-callback sampled-request)
                 (magent-llm-tool-call-event
                  "call-1" "read_file" '(:path "README.org")))
        (should (eq (magent-agent-loop-status loop) 'tool-pending))
        (apply (car scheduled) (cdr scheduled))
        (should (eq aborted-handle 'provider-handle))
        (should (eq (magent-agent-loop-status loop) 'failed))
        (should (string-match-p "timed out"
                                (magent-agent-loop-error loop)))))))

(ert-deftest magent-test-agent-loop-synchronous-batch-end-leaves-no-timeout ()
  "A synchronous tool batch terminal event is not re-armed after sampling."
  (require 'magent-agent-loop)
  (let ((magent-request-timeout 5)
        (scheduled-count 0))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (setq scheduled-count (1+ scheduled-count))
                 (list :timer fn args)))
              ((symbol-function 'cancel-timer) #'ignore))
      (let* ((request (magent-llm-request-create
                       :prompt '((prompt . "hello"))))
             (loop (magent-agent-loop-create
                    :request request
                    :sampler
                    (lambda (sample-request)
                      (let ((callback
                             (magent-llm-request-callback sample-request)))
                        (funcall callback
                                 (magent-llm-tool-call-event
                                  "call-1" "read_file"
                                  '(:path "README.org")))
                        (funcall callback
                                 (magent-llm-tool-call-batch-end-event)))
                      'provider-handle))))
        (magent-agent-loop-start loop)
        (should (= scheduled-count 1))
        (should (eq (magent-agent-loop-status loop) 'tool-pending))
        (should-not (magent-agent-loop-request-timeout-timer loop))))))

(ert-deftest magent-test-agent-loop-abort-clears-request-context-controller ()
  "Test loop abort clears its request-context abort controller."
  (require 'magent-agent-loop)
  (let* ((context (magent-request-context-create))
         (loop (magent-agent-loop-create :request-context context)))
    (setf (magent-request-context-abort-controller context)
          (magent-agent-loop-abort-controller loop))
    (cl-letf (((symbol-function 'magent-lifecycle-events-end-turn) #'ignore))
      (magent-agent-loop-abort loop))
    (should (eq (magent-agent-loop-status loop) 'cancelled))
    (should-not (magent-request-context-abort-controller context))))

(ert-deftest magent-test-agent-loop-abort-ends-only-owned-event-context ()
  "Abort terminalizes a loop-owned turn but never an inherited context."
  (require 'magent-agent-loop)
  (let* ((owned-event (magent-lifecycle-events-context-create :turn-id "owned"))
         (inherited-event
          (magent-lifecycle-events-context-create :turn-id "inherited"))
         (owned-request
          (magent-request-context-create :event-context owned-event))
         (inherited-request
          (magent-request-context-create :event-context inherited-event))
         (owned-loop
          (magent-agent-loop-create
           :request-context owned-request
           :event-context owned-event
           :owns-event-context-p t))
         (inherited-loop
          (magent-agent-loop-create
           :request-context inherited-request
           :event-context inherited-event
           :owns-event-context-p nil))
         ended)
    (cl-letf (((symbol-function 'magent-lifecycle-events-end-turn)
               (lambda (context status &optional detail)
                 (push (list context status detail) ended))))
      (magent-agent-loop-abort inherited-loop)
      (magent-agent-loop-abort owned-loop))
    (should (= (length ended) 1))
    (should (eq (caar ended) owned-event))
    (should (eq (cadar ended) 'cancelled))))

(ert-deftest magent-test-agent-loop-abort-interrupts-ledger-turn ()
  "Test loop abort marks its ledger turn interrupted and schedules a save."
  (require 'magent-agent-loop)
  (let* ((magent-session--current-scope "/tmp/project")
         (session (magent-session-create :id "session-1"))
         (thread (magent-session-thread-ledger session))
         (turn (magent-thread-create-turn thread "hello"))
         (item (magent-thread-start-item
                thread (magent-thread-turn-id turn) 'reasoning))
         (context (magent-request-context-create
                   :scope "/tmp/project"
                   :session session
                   :turn-id (magent-thread-turn-id turn)))
         (loop (magent-agent-loop-create
                :session session
                :turn-id (magent-thread-turn-id turn)
                :request-context context))
         saved)
    (setf (magent-request-context-abort-controller context)
          (magent-agent-loop-abort-controller loop))
    (cl-letf (((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-session-save-deferred-for-session)
               (lambda (saved-session saved-scope &optional _delay)
                 (setq saved (list saved-session saved-scope)))))
      (magent-agent-loop-abort loop))
    (should (eq (magent-thread-turn-status turn) 'interrupted))
    (should (eq (magent-thread-item-status item) 'cancelled))
    (should (eq (car saved) session))
    (should (equal (cadr saved) "/tmp/project"))))

(ert-deftest magent-test-agent-loop-run-tool-marks-summary-only-events-hidden ()
  "Test summary-only request contexts mark lifecycle events non-visible."
  (require 'magent-agent-loop)
  (require 'gptel)
  (let* ((events nil)
         (result nil)
         (tool (gptel-make-tool
                :name "read_file"
                :description "read"
                :args (list '(:name "path" :type string))
                :function
                (lambda (path)
                  (magent-test-tool-result (format "read %s" path)))
                :async nil))
         (context (magent-request-context-create
                   :ui-visibility 'summary-only))
         (loop (magent-agent-loop-create)))
    (cl-letf (((symbol-function 'magent-lifecycle-events-emit)
               (lambda (type &rest props)
                 (push (cons type props) events))))
      (magent-agent-loop-run-tool
       loop context tool (lambda (value) (setq result value))
       (list "README.org")))
    (should (equal (magent-test-tool-output result) "read README.org"))
    (let ((ordered (nreverse events)))
      (should (equal (mapcar #'car ordered)
                     '(tool-call-start tool-call-end)))
      (dolist (event ordered)
        (should-not (plist-get (cdr event) :ui-visible))))))

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
                                 (magent-test-tool-result "second-result"))
                     :async nil))
         (loop (magent-agent-loop-create)))
    (cl-letf (((symbol-function 'magent-lifecycle-events-emit) #'ignore))
      (magent-agent-loop-run-tool
       loop nil async-tool (lambda (value) (push value results)) nil)
      (magent-agent-loop-run-tool
       loop nil sync-tool (lambda (value) (push value results)) nil)
      (should (equal order '(first-start)))
      (should first-callback)
      (funcall first-callback (magent-test-tool-result "first-result")))
    (should (equal (nreverse order) '(first-start second-run)))
    (should (equal (mapcar #'magent-test-tool-output (nreverse results))
                   '("first-result" "second-result")))))

(ert-deftest magent-test-agent-loop-abort-drops-late-async-tool-result ()
  "Test loop abort suppresses late async tool completions."
  (require 'magent-agent-loop)
  (require 'magent-tools)
  (require 'gptel)
  (let* ((tool-callback nil)
         (cleanup nil)
         (result nil)
         (events nil)
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
    (cl-letf (((symbol-function 'magent-lifecycle-events-emit)
               (lambda (type &rest props)
                 (push (cons type props) events))))
      (magent-agent-loop-run-tool
       loop nil tool (lambda (value) (setq result value))
       (list "(sleep-for 1)"))
      (should tool-callback)
      (should (assq 'tool-call-start events))
      (magent-agent-loop-abort loop)
      (should (eq cleanup 'ran))
      (funcall tool-callback (magent-test-tool-result "\"done\"")))
    (should-not result)
    (should-not (assq 'tool-call-end events))))

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
         (global-file (expand-file-name
                       "session-20260316-100000.json"
                       (expand-file-name "global" magent-session-directory)))
         (project-file (expand-file-name "session-20260317-100000.json" project-dir))
         (other-project-file (expand-file-name "session-20260315-100000.json" other-project-dir))
         (older-time (date-to-time "2026-03-16 10:00:00"))
         (newer-time (date-to-time "2026-03-17 10:00:00"))
         (oldest-time (date-to-time "2026-03-15 10:00:00")))
    (unwind-protect
        (progn
          (magent-test--write-session-fixture
           magent-session-directory "session-20260316-100000"
           'global "Global chat")
          (magent-test--write-session-fixture
           magent-session-directory "session-20260317-100000"
           project-root "Project work item")
          (magent-test--write-session-fixture
           magent-session-directory "session-20260315-100000"
           other-project "Other project item")
          (set-file-times global-file older-time)
          (set-file-times project-file newer-time)
          (set-file-times other-project-file oldest-time)
          (setq magent-session--current-scope project-root)
          (should (equal (magent-session-list-files)
                         (list project-file other-project-file global-file)))
          (should (equal (magent-session--file-group project-file)
                         (format "Current Project: %s"
                                 (abbreviate-file-name project-root))))
          (should (equal (magent-session--file-group other-project-file)
                         (format "Project: %s"
                                 (abbreviate-file-name other-project)))))
      (delete-directory magent-session-directory t)
      (delete-directory project-root t)
      (delete-directory other-project t))))

(ert-deftest magent-test-session-list-files-sorts-within-group-by-mtime ()
  "Test files inside one group are ordered newest-to-oldest by mtime."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (global-directory (expand-file-name "global" magent-session-directory))
         (newer-file (expand-file-name "session-20260317-120000.json"
                                       global-directory))
         (older-file (expand-file-name "session-20260317-110000.json"
                                       global-directory))
         (newer-mtime (encode-time 0 0 1 1 1 2020))
         (older-mtime (encode-time 0 0 1 1 1 2030)))
    (unwind-protect
        (progn
          (magent-test--write-session-fixture
           magent-session-directory "session-20260317-120000"
           'global "Newer session")
          (magent-test--write-session-fixture
           magent-session-directory "session-20260317-110000"
           'global "Older session")
          (set-file-times newer-file newer-mtime)
          (set-file-times older-file older-mtime)
          (should (equal (magent-session-list-files)
                         (list older-file newer-file))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-session-list-files-caches-metadata-while-sorting ()
  "Test saved-session listing parses each file's metadata at most once."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--metadata-cache (make-hash-table :test #'equal))
         (project-root (file-truename
                        (directory-file-name
                         (make-temp-file "magent-project-" t))))
         (project-dir (expand-file-name
                       (concat "projects/" (secure-hash 'sha1 project-root))
                       magent-session-directory))
         (global-directory (expand-file-name "global" magent-session-directory))
         (global-file-a (expand-file-name "session-20260317-100000.json"
                                          global-directory))
         (global-file-b (expand-file-name "session-20260317-090000.json"
                                          global-directory))
         (project-file-a (expand-file-name "session-20260317-120000.json"
                                           project-dir))
         (project-file-b (expand-file-name "session-20260317-110000.json"
                                           project-dir))
         (files (list project-file-a project-file-b
                      global-file-a global-file-b))
         (read-count 0)
         (original-read
          (symbol-function 'magent-session--read-file-metadata)))
    (unwind-protect
        (progn
          (dolist (id '("session-20260317-100000"
                        "session-20260317-090000"))
            (magent-test--write-session-fixture
             magent-session-directory id 'global "Global"))
          (dolist (id '("session-20260317-120000"
                        "session-20260317-110000"))
            (magent-test--write-session-fixture
             magent-session-directory id project-root "Project"))
          (setq magent-session--current-scope project-root)
          (cl-letf (((symbol-function 'magent-session--read-file-metadata)
                     (lambda (file)
                       (cl-incf read-count)
                       (funcall original-read file))))
            (should (equal (sort (magent-session-list-files) #'string<)
                           (sort (copy-sequence files) #'string<)))
            (should (<= read-count (length files)))))
      (delete-directory magent-session-directory t)
      (delete-directory project-root t))))

(ert-deftest magent-test-session-list-files-for-scope-does-not-scan-other-scopes ()
  "Test exact-scope listing reads only the requested storage directory."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--metadata-cache (make-hash-table :test #'equal))
         (project-a (file-truename
                     (directory-file-name
                      (make-temp-file "magent-project-a-" t))))
         (project-b (file-truename
                     (directory-file-name
                      (make-temp-file "magent-project-b-" t))))
         (directory-a (magent-session--scope-storage-directory project-a))
         (directory-b (magent-session--scope-storage-directory project-b))
         (file-a (expand-file-name "session-20260718-120000.json" directory-a))
         (file-b (expand-file-name "session-20260718-130000.json" directory-b))
         (read-files nil)
         (original-read
          (symbol-function 'magent-session--read-file-metadata)))
    (unwind-protect
        (progn
          (magent-test--write-session-fixture
           magent-session-directory "session-20260718-120000"
           project-a "Project A")
          (magent-test--write-session-fixture
           magent-session-directory "session-20260718-130000"
           project-b "Project B")
          (cl-letf (((symbol-function 'magent-session--read-file-metadata)
                     (lambda (file)
                       (push file read-files)
                       (funcall original-read file))))
            (should (equal (magent-session-list-files-for-scope project-a)
                           (list file-a)))
            (should (equal read-files (list file-a)))))
      (delete-directory magent-session-directory t)
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest magent-test-session-summary-title-has-one-canonical-projection ()
  "Test live and saved titles share explicit-title and message fallback rules."
  (let ((explicit
         (magent-test--session-with-transcript
          "explicit" '((user "First prompt"))
          '((title . "  Explicit\n title  "))))
        (derived
         (magent-test--session-with-transcript
          "derived" '((user "  First\n prompt  ")))))
    (should (equal (magent-session-summary-title explicit) "Explicit title"))
    (should (equal (magent-session-summary-title derived) "First prompt"))))

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

(ert-deftest magent-test-runtime-activate-scope-rolls-back-partial-overlay ()
  "A failed project load restores the exact previously active registries."
  (require 'magent-runtime)
  (let* ((old-scope "/tmp/magent-project-old")
         (new-scope "/tmp/magent-project-new")
         (old-agent (list :owner 'old-agent))
         (old-skill (list (cons "old-skill" (list :owner 'old-skill))))
         (old-capability
          (list (cons "old-capability" (list :owner 'old-capability))))
         (magent-runtime--active-project-scope old-scope)
         (magent-load-custom-agents t)
         (magent-agent-registry--agents (make-hash-table :test #'equal))
         (magent-skills--registry old-skill)
         (magent-capability--registry old-capability))
    (puthash "old-agent" old-agent magent-agent-registry--agents)
    (cl-letf (((symbol-function 'magent-agent-registry-remove-project-scope)
               (lambda (_scope)
                 (clrhash magent-agent-registry--agents)))
              ((symbol-function 'magent-skills-remove-project-scope)
               (lambda (_scope) (setq magent-skills--registry nil)))
              ((symbol-function 'magent-capability-remove-project-scope)
               (lambda (_scope) (setq magent-capability--registry nil)))
              ((symbol-function 'magent-agent-file-load-project-scope)
               (lambda (_scope)
                 (puthash "new-agent" (list :owner 'new-agent)
                          magent-agent-registry--agents)))
              ((symbol-function 'magent-skills-load-project-scope)
               (lambda (_scope)
                 (setq magent-skills--registry
                       (list (cons "new-skill" (list :owner 'new-skill))))))
              ((symbol-function 'magent-capability-load-project-scope)
               (lambda (_scope) (error "broken capability")))
              ((symbol-function 'magent-log) #'ignore))
      (should-error (magent-runtime-activate-scope new-scope)
                    :type 'error)
      (should (equal (magent-runtime-active-project-scope) old-scope))
      (should (equal (gethash "old-agent" magent-agent-registry--agents)
                     old-agent))
      (should-not (gethash "new-agent" magent-agent-registry--agents))
      (should (equal magent-skills--registry old-skill))
      (should (equal magent-capability--registry old-capability)))))

(ert-deftest magent-test-runtime-prepare-context-initializes-and-activates-scope ()
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
          (magent-runtime-prepare-context)
          (magent-runtime-prepare-context))
      (delete-directory project-root t))
    (should (equal (nreverse events)
                   `(audit
                     agents
                     skills
                     capabilities
                     (scope ,project-root)
                     (scope ,project-root))))))

(ert-deftest magent-test-config-reload-preserves-buffer-logger ()
  "Test reloading config preserves the core logger and buffer sink."
  (require 'magent-log)
  (let* ((config-file (expand-file-name "lisp/magent-config.el"
                                        magent-test--root-directory))
         (buffer (magent-log-buffer))
         (before-file (symbol-file 'magent-log 'defun))
         (before-fn (symbol-function 'magent-log)))
    (unwind-protect
        (progn
          (should (string-match-p "magent-log\\.elc?$" (or before-file "")))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (load config-file nil t)
          (should (eq (symbol-function 'magent-log) before-fn))
          (should (string-match-p "magent-log\\.elc?$"
                                  (or (symbol-file 'magent-log 'defun) "")))
          (magent-log "reload-safe %s" 1)
          (with-current-buffer buffer
            (should (string-match-p "reload-safe 1" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-log-buffer-respects-level ()
  "Test the built-in log sink filters messages by configured severity."
  (let ((magent-enable-logging t)
        (magent-log-level 'warn)
        (magent-log-buffer-name "*magent-test-log-level*"))
    (unwind-protect
        (progn
          (magent-log "INFO hidden")
          (magent-log "WARN visible")
          (with-current-buffer (magent-log-buffer)
            (should-not (string-match-p "hidden" (buffer-string)))
            (should (string-match-p "visible" (buffer-string)))))
      (when (get-buffer magent-log-buffer-name)
        (kill-buffer magent-log-buffer-name)))))

(ert-deftest magent-test-log-buffer-can-be-disabled ()
  "Test disabling buffer logging suppresses all buffer writes."
  (let ((magent-enable-logging nil)
        (magent-log-level 'debug)
        (magent-log-buffer-name "*magent-test-log-disabled*"))
    (magent-log "ERROR hidden")
    (should-not (get-buffer magent-log-buffer-name))))

(ert-deftest magent-test-session-reset-clears-only-active-scope ()
  "Test resetting a session only clears the active scope."
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (project-root (file-truename (directory-file-name (make-temp-file "magent-project-" t)))))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (magent-test--record-session-entry (magent-session-get) 'user "global")
          (magent-session-activate project-root)
          (magent-test--record-session-entry (magent-session-get) 'user "project")
          (magent-session-reset)
          (should-not (gethash project-root magent-session--scoped-sessions))
          (should (gethash 'global magent-session--scoped-sessions))
          (magent-session-activate 'global)
          (should (equal (magent-test--transcript-content
                          (car (magent-test--session-transcript (magent-session-get))))
                         "global")))
      (delete-directory project-root t))))

(ert-deftest magent-test-agent-run-turn-emits-turn-events ()
  "Test `magent-agent-run-turn' emits turn lifecycle and text events."
  (require 'magent-lifecycle-events)
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (captured nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (let ((cb (plist-get kwargs :callback)))
                   (funcall cb "Hello" '(:stream t))
                   (funcall cb t (list :content "Hello"))))))
      (unwind-protect
          (progn
            (magent-lifecycle-events-add-sink (lambda (event) (push event captured)))
            (magent-test--run-turn "Hello" #'ignore))
        (magent-lifecycle-events-clear-sinks)))
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

(ert-deftest magent-test-agent-run-turn-resolves-capability-skills ()
  "Test `magent-agent-run-turn' merges capability-derived skills."
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
                   (funcall callback t (list :content "Hello"))))))
      (magent-test--run-turn
       "Please reorganize this heading"
       #'ignore
       nil
       '("manual-skill")
       nil
       '(:major-mode org-mode :features (org))))
    (should (equal captured-skill-names '("manual-skill" "auto-skill")))))

(ert-deftest magent-test-agent-run-turn-dedupes-explicit-and-capability-skills ()
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
                   (funcall callback t (list :content "Hello"))))))
      (magent-test--run-turn
       "Diagnose why this hook is not running"
       #'ignore
       nil
       '("emacs-runtime-inspection")
       nil
       '(:major-mode emacs-lisp-mode :major-mode-family (emacs-lisp-mode prog-mode))
       (magent-capability-resolution-create
        :skill-names '("emacs-runtime-inspection" "emacs-hook-debugging" "emacs-runtime-inspection"))))
    (should (equal captured-skill-names
                   '("emacs-runtime-inspection" "emacs-hook-debugging")))))

(ert-deftest magent-test-agent-run-turn-emits-capability-resolution-event ()
  "Test `magent-agent-run-turn' emits capability resolution metadata."
  (require 'magent-capability)
  (require 'magent-lifecycle-events)
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
                   (funcall callback t (list :content "Hello"))))))
      (unwind-protect
          (progn
            (magent-lifecycle-events-add-sink (lambda (event) (push event captured)))
            (magent-test--run-turn
             "Please reorganize this heading"
             #'ignore
             nil nil nil
             '(:major-mode org-mode :features (org))))
        (magent-lifecycle-events-clear-sinks)))
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

(ert-deftest magent-test-action-run-doctor-dispatches-action ()
  "Test the Doctor M-x wrapper dispatches through `magent-action-run'."
  (let (captured)
    (cl-letf (((symbol-function 'magent-action-run)
               (lambda (name &rest args)
                 (setq captured (cons name args)))))
      (magent-action-run-doctor))
    (should (equal (car captured) "doctor"))))

(ert-deftest magent-test-acp-request-sender-initialize ()
  "Test in-process ACP request sender handles initialize."
  (require 'magent-acp)
  (let (response failure)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args)))
              ((symbol-function 'magent-agent-registry-primary-agents)
               (lambda ()
                 (list (magent-agent-info-create
                        :name "build"
                        :description "Build"
                        :mode 'primary)))))
      (magent-acp--request-sender
       :client '((:notification-handlers . nil)
                 (:request-handlers . nil))
       :request '((:method . "initialize"))
       :on-success (lambda (value) (setq response value))
       :on-failure (lambda (err) (setq failure err))))
    (should-not failure)
    (should (= (map-elt response 'protocolVersion)
               magent-acp-protocol-version))
    (should (equal (map-nested-elt response '(modes currentModeId))
                   magent-default-agent))))

(ert-deftest magent-test-acp-in-process-client-starts-on-local-host ()
  "Magent's ACP placeholder process cannot inherit a remote shell cwd."
  (require 'magent-acp)
  (let ((client (magent-acp-make-client))
        launch-directory)
    (should (advice-member-p #'magent-acp--start-client-locally
                             'acp--start-client))
    (should
     (eq
      (magent-acp--start-client-locally
       (lambda (&rest _args)
         (setq launch-directory default-directory)
         'started)
       :client client)
      'started))
    (should-not (file-remote-p launch-directory))))

(ert-deftest magent-test-acp-available-commands-list-command-skills ()
  "ACP exposes every instruction skill as an explicit skill command."
  (require 'magent-acp)
  (require 'magent-action-controls)
  (let* ((magent-skills--registry nil)
        (magent-skills--scope-catalog (make-hash-table :test #'equal))
        (magent-action--registry nil))
    (magent-action-controls-register)
    (magent-skills-register
     (magent-skill-create
      :name "init"
      :description '("Initialize project instructions"
                     "similar to Codex /init.")
      :type 'instruction))
    (magent-skills-register
     (magent-skill-create
      :name "note"
      :description "Plain instruction skill."
      :type 'instruction))
    (let* ((commands (magent-acp--available-commands))
           (names (mapcar (lambda (command)
                            (map-elt command 'name))
                          (append commands nil)))
           (skill-command
            (cl-find-if (lambda (entry)
                          (equal (map-elt entry 'name) "$init"))
                        (append commands nil))))
      (should (equal names '("compact" "$init" "$note")))
      (should skill-command)
      (should (equal (map-elt skill-command 'description)
                     "Initialize project instructions, similar to Codex /init.")))))

(ert-deftest magent-test-acp-available-commands-lists-all-bundled-slash-commands ()
  "Test ACP available commands expose every bundled Elisp command."
  (require 'magent-acp)
  (let* ((magent-action--registry nil)
        (magent-skills--registry nil)
        (magent-skills--scope-catalog (make-hash-table :test #'equal)))
    (magent-test--register-builtin-commands-only)
    (let* ((commands (append (magent-acp--available-commands) nil))
           (names (mapcar (lambda (command)
                            (map-elt command 'name))
                          commands)))
      (should (equal names
                     (sort (copy-sequence
                            (append
                             magent-test--builtin-control-command-names
                             magent-test--builtin-maintenance-command-names
                             magent-test--builtin-slash-command-names))
                           #'string<)))
      (dolist (command commands)
        (let ((spec (magent-action-get (map-elt command 'name))))
          (should spec)
          (should-not (string-empty-p (map-elt command 'description)))
          (should (equal (map-elt command 'description)
                         (magent-action-spec-description spec))))))))

(ert-deftest magent-test-authority-action-explains-effective-boundaries ()
  "The authority view reports exposure, execution, rules, and eval policy."
  (require 'magent-action-builtins)
  (let* ((magent-agent-registry--agents (make-hash-table :test 'equal))
         (magent-agent-registry--default-agent "build")
         (magent-agent-registry--initialized t)
         (agent (magent-agent-builtins--build))
         (session (magent-session-create :id "authority-session"
                                         :agent agent))
         (runtime (magent-runtime-session-create
                   :id "authority-session" :scope 'global
                   :magent-session session))
         (spec (magent-action-spec-create
                :name "authority" :title "Authority"
                :session-policy 'current))
         (invocation (magent-action-invocation-create
                      :id "authority-invocation" :spec spec
                      :runtime-session runtime))
         output)
    (magent-agent-registry-register agent)
    (magent-permission-set-session-override 'emacs_eval 'allow session)
    (condition-case condition
        (iter-next (magent-action-builtins--authority invocation))
      (iter-end-of-sequence
       (setq output (cdr condition))))
    (should (string-match-p "agent: build" output))
    (should (string-match-p
             "emacs_eval[[:space:]]+permission=emacs_eval[[:space:]]+decision=ask"
             output))
    (should (string-match-p "execution=child-emacs" output))
    (should (string-match-p "once-only-ignores-session-allow" output))
    (should (string-match-p "emacs_eval_live.*execution=live-emacs" output))
    (should (string-match-p "read_file.*resource-rules=" output))))

(ert-deftest magent-test-authority-action-reports-bypass-effective-decisions ()
  "Authority reports bypass allow, except once-only eval remains ask."
  (require 'magent-action-builtins)
  (let* ((magent-bypass-permission t)
         (magent-agent-registry--agents (make-hash-table :test 'equal))
         (magent-agent-registry--default-agent "build")
         (magent-agent-registry--initialized t)
         (agent (magent-agent-builtins--build))
         (session (magent-session-create :id "authority-bypass" :agent agent))
         (runtime (magent-runtime-session-create
                   :id "authority-bypass" :scope 'global
                   :magent-session session))
         (spec (magent-action-spec-create
                :name "authority" :title "Authority"
                :session-policy 'current))
         (invocation (magent-action-invocation-create
                      :id "authority-bypass-invocation" :spec spec
                      :runtime-session runtime))
         output)
    (setf (magent-agent-info-permission agent)
          '((bash . deny) (emacs_eval . deny) (* . deny)))
    (magent-agent-registry-register agent)
    (condition-case condition
        (iter-next (magent-action-builtins--authority invocation))
      (iter-end-of-sequence
       (setq output (cdr condition))))
    (should (string-match-p
             "bash[[:space:]]+permission=bash[[:space:]]+decision=allow.*source=bypass"
             output))
    (should (string-match-p
             "emacs_eval[[:space:]]+permission=emacs_eval[[:space:]]+decision=ask.*source=once-only-bypass"
             output))))

(ert-deftest magent-test-explain-action-can-read-its-spilled-output ()
  "The explain Action exposes read_tool_output in its exact allowlist."
  (require 'magent-action-builtins)
  (let ((definition
         (cl-find "explain" magent-action-builtins--prompt-actions
                  :key (lambda (entry) (plist-get entry :name))
                  :test #'equal)))
    (should (memq 'read_tool_output (plist-get definition :tools)))))

(ert-deftest magent-test-acp-available-skill-commands-use-session-scope ()
  "ACP skill projection remains exact for concurrently retained projects."
  (require 'magent-acp)
  (let* ((magent-action--registry nil)
         (magent-skills--registry nil)
         (magent-skills--scope-catalog (make-hash-table :test #'equal))
         (magent-runtime--active-project-scope nil)
         (project-a (file-truename
                     (directory-file-name
                      (make-temp-file "magent-acp-project-a-" t))))
         (project-b (file-truename
                     (directory-file-name
                      (make-temp-file "magent-acp-project-b-" t))))
         (session-a
          (magent-runtime-session-create :id "session-a" :scope project-a))
         (session-b
          (magent-runtime-session-create :id "session-b" :scope project-b)))
    (unwind-protect
        (progn
          (magent-skills-register
           (magent-skill-create
            :name "global-skill" :type 'instruction
            :description "Global."))
          (magent-skills-register
           (magent-skill-create
            :name "project-a" :type 'instruction
            :description "Project A."
            :source-layer 'project :source-scope project-a))
          (magent-skills-remove-project-scope project-a)
          (magent-skills-register
           (magent-skill-create
            :name "project-b" :type 'instruction
            :description "Project B."
            :source-layer 'project :source-scope project-b))
          (let ((names-a
                 (mapcar
                  (lambda (entry) (map-elt entry 'name))
                  (append (magent-acp--available-commands session-a) nil)))
                (names-b
                 (mapcar
                  (lambda (entry) (map-elt entry 'name))
                  (append (magent-acp--available-commands session-b) nil))))
            (should (equal names-a '("$global-skill" "$project-a")))
            (should (equal names-b '("$global-skill" "$project-b")))))
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest magent-test-acp-skill-command-resolves-without-action-adapter ()
  "ACP `/$skill' syntax resolves against instruction skill descriptors."
  (require 'magent-acp)
  (let ((magent-skills--registry nil)
        (magent-skills--scope-catalog (make-hash-table :test #'equal))
        (project-root (make-temp-file "magent-acp-skill-project-" t)))
    (unwind-protect
        (progn
          (magent-skills-register
           (magent-skill-create
            :name "reviewer"
            :description "Review code."
            :type 'instruction))
          (magent-skills-register
           (magent-skill-create
            :name "project-only"
            :description "Project policy."
            :type 'instruction
            :requires-project t))
          (let ((parsed
                 (magent-acp--skill-command
                  "/$reviewer focus on tests" 'global)))
            (should (eq (plist-get parsed :kind) 'skill))
            (should (equal (plist-get parsed :name) "reviewer"))
            (should (equal (plist-get parsed :argument) "focus on tests")))
          (should-error
           (magent-acp--skill-command "/$missing" 'global)
           :type 'user-error)
          (should-error
           (magent-acp--skill-command "/$project-only" 'global)
           :type 'user-error)
          (should
           (magent-acp--skill-command "/$project-only" project-root)))
      (delete-directory project-root t))))

(ert-deftest magent-test-acp-slash-command-resolves-all-bundled-commands ()
  "Test every bundled slash command resolves to its Elisp Action spec."
  (require 'magent-acp)
  (let ((magent-action--registry nil))
    (magent-test--register-builtin-commands-only)
    (dolist (name magent-test--builtin-slash-command-names)
      (let ((parsed (magent-acp--slash-command
                     (format "/%s focus on tests" name))))
        (should (eq (plist-get parsed :kind) 'command))
        (should (eq (plist-get parsed :spec) (magent-action-get name)))
        (should (equal (plist-get parsed :argument) "focus on tests"))))))

(ert-deftest magent-test-acp-slash-command-resolves-by-session-scope ()
  "Test ACP dispatch resolves same-name commands in the session project."
  (require 'magent-acp)
  (let* ((magent-action--registry nil)
        (magent-action--sequence 0)
        (project-a (magent-runtime-session-create
                    :id "session-a" :scope "/tmp/project-a"))
        (project-b (magent-runtime-session-create
                    :id "session-b" :scope "/tmp/project-b")))
    (let ((command-a
           (magent-action-register
            "project-command" :session-policy 'current :workflow #'magent-test--empty-action-workflow
            :source-layer 'project :source-scope "/tmp/project-a"))
          (command-b
           (magent-action-register
            "project-command" :session-policy 'current :workflow #'magent-test--empty-action-workflow
            :source-layer 'project :source-scope "/tmp/project-b")))
      (should (eq (plist-get (magent-acp--slash-command
                              "/project-command" project-a)
                             :spec)
                  command-a))
      (should (eq (plist-get (magent-acp--slash-command
                              "/project-command" project-b)
                             :spec)
                  command-b)))))

(ert-deftest magent-test-acp-slash-command-parses-session-control ()
  "Test the atomic core rebuild recognizes /compact and removes stale /clear."
  (require 'magent-acp)
  (let ((magent-action--registry nil))
    (let ((magent-action--allow-core-registration t))
      (magent-action-register
       "clear" :session-policy 'current :workflow #'magent-test--empty-action-workflow
       :source-layer 'core))
    (magent-test--register-builtin-commands-only)
    (let ((compact (magent-acp--slash-command
                    "/compact preserve the failing test")))
      (should-not (magent-action-get "clear"))
      (should-not (magent-acp--slash-command "/clear"))
      (should (eq (plist-get compact :spec)
                  (magent-action-get "compact")))
      (should (equal (plist-get compact :argument)
                     "preserve the failing test")))))

(ert-deftest magent-test-acp-session-new-notifies-available-commands ()
  "Test ACP session creation notifies agent-shell of slash commands."
  (require 'magent-acp)
  (let ((runtime-session (magent-runtime-session-create
                          :id "session-1"
                          :scope 'global
                          :magent-session (magent-session-create)))
        response notification failure)
    (cl-letf (((symbol-function 'magent-runtime-prepare-context)
               (lambda (_scope) 'global))
              ((symbol-function 'magent-runtime-session-new)
               (lambda (_scope) runtime-session))
              ((symbol-function 'magent-agent-registry-primary-agents)
               (lambda ()
                 (list (magent-agent-info-create
                        :name "build"
                        :description "Build"))))
              ((symbol-function 'magent-acp--available-commands)
               (lambda (&optional _runtime-session)
                 [((name . "init")
                   (description . "Initialize project instructions."))])))
      (magent-acp--handle-request
       `((:notification-handlers
          . (,(lambda (value) (setq notification value))))
         (:request-handlers . nil))
       '((:method . "session/new")
         (:params . ((cwd . "/tmp"))))
       (lambda (value) (setq response value))
       (lambda (err) (setq failure err))))
    (should-not failure)
    (should (equal (map-elt response 'sessionId) "session-1"))
    (should (equal (map-elt notification 'method) "session/update"))
    (should (equal (map-nested-elt notification
                                   '(params update sessionUpdate))
                   "available_commands_update"))
    (should (equal (map-elt
                    (aref (map-nested-elt notification
                                          '(params update availableCommands))
                          0)
                   'name)
                   "init"))))

(ert-deftest magent-test-acp-refresh-commands-isolated-by-session-scope ()
  "Test command refresh publishes each ACP session's scoped command view."
  (require 'magent-acp)
  (let* ((magent-action--registry nil)
         (magent-action--sequence 0)
         (magent-action-registry-changed-hook nil)
         (magent-acp--client-session-scopes
          (make-hash-table :test #'eq))
         (client (list :client "test"))
         (bindings (make-hash-table :test #'equal))
         (project-a
          (magent-runtime-session-create
           :id "session-a" :scope "/tmp/project-a"))
         (project-b
          (magent-runtime-session-create
           :id "session-b" :scope "/tmp/project-b"))
         notifications)
    (magent-action-register
     "global-command" :session-policy 'current :workflow #'magent-test--empty-action-workflow)
    (magent-action-register
     "project-a-command" :session-policy 'current :workflow #'magent-test--empty-action-workflow
     :source-layer 'project :source-scope "/tmp/project-a")
    (magent-action-register
     "project-b-command" :session-policy 'current :workflow #'magent-test--empty-action-workflow
     :source-layer 'project :source-scope "/tmp/project-b")
    (puthash "session-a" "/tmp/project-a" bindings)
    (puthash "session-b" "/tmp/project-b" bindings)
    (puthash client bindings magent-acp--client-session-scopes)
    (cl-letf (((symbol-function 'magent-runtime-session-from-id)
               (lambda (session-id scope)
                 (cond
                  ((and (equal session-id "session-a")
                        (equal scope "/tmp/project-a"))
                   project-a)
                  ((and (equal session-id "session-b")
                        (equal scope "/tmp/project-b"))
                   project-b))))
              ((symbol-function 'magent-acp--session-update)
               (lambda (_client session-id update)
                 (push
                  (cons session-id
                        (mapcar
                         (lambda (command) (map-elt command 'name))
                         (append (map-elt update 'availableCommands) nil)))
                  notifications)
                 nil)))
      (magent-acp--refresh-available-commands))
    (should (equal (sort (cdr (assoc "session-a" notifications)) #'string<)
                   '("global-command" "project-a-command")))
    (should (equal (sort (cdr (assoc "session-b" notifications)) #'string<)
                   '("global-command" "project-b-command")))))

(ert-deftest magent-test-acp-models-use-model-id ()
  "Test ACP available model entries expose modelId for agent-shell."
  (require 'magent-acp)
  (let* ((gptel-model 'test-model)
         (models (magent-acp--models))
         (available (map-elt models 'availableModels))
         (entry (aref available 0)))
    (should (equal (map-elt entry 'modelId) "test-model"))
    (should-not (assq 'id entry))))

(ert-deftest magent-test-acp-session-response-advertises-effort-config ()
  "Test ACP session responses advertise thought level options."
  (require 'magent-acp)
  (let* ((runtime-session (magent-runtime-session-create
                           :id "session-1"
                           :magent-session (magent-session-create)
                           :effort 'xhigh))
         response option values)
    (cl-letf (((symbol-function 'magent-runtime-session-agent-name)
               (lambda (_session) "build"))
              ((symbol-function 'magent-agent-registry-primary-agents)
               (lambda ()
                 (list (magent-agent-info-create
                        :name "build"
                        :description "Build")))))
      (setq response (magent-acp--session-response runtime-session)))
    (setq option (aref (map-elt response 'configOptions) 0)
          values (append (map-elt option 'options) nil))
    (should (equal (map-elt option 'id) "effort"))
    (should (equal (map-elt option 'category) "thought_level"))
    (should (equal (map-elt option 'currentValue) "xhigh"))
    (should (member "xhigh"
                    (mapcar (lambda (entry) (map-elt entry 'value))
                            values)))
    (let ((capabilities (aref (map-elt response 'configOptions) 1)))
      (should (equal (map-elt capabilities 'id) "capabilities"))
      (should (equal (map-elt capabilities 'currentValue) "enabled")))))

(ert-deftest magent-test-acp-prompt-input-preserves-resource-structure ()
  "Test ACP resources remain structured and separate from instruction text."
  (require 'magent-acp)
  (let* ((input
          (magent-acp--prompt-input
           '[((type . "text") (text . "Review this file"))
             ((type . "resource")
              (resource . ((uri . "file:///tmp/example.txt")
                           (mimeType . "text/plain")
                           (text . "line 1\nline 2"))))]))
         (blocks (plist-get input :content-blocks))
         (resource (map-elt (aref blocks 1) 'resource)))
    (should (equal (plist-get input :text) "Review this file"))
    (should-not (string-match-p "line 1" (plist-get input :text)))
    (should (equal (map-elt resource 'text) "line 1\nline 2"))
    (should (equal (plist-get (plist-get input :context) :resource-paths)
                   '("/tmp/example.txt")))))

(ert-deftest magent-test-session-prompt-list-rebuilds-structured-resources ()
  "Test ledger metadata reconstructs ACP resources for model history."
  (require 'magent-session)
  (let* ((session (magent-session-create))
         (thread (magent-session-thread-ledger session))
         (blocks '[((type . "text") (text . "Review"))
                   ((type . "resource")
                    (resource . ((uri . "file:///tmp/example.txt")
                                 (text . "resource body"))))])
         (turn (magent-thread-queue-turn
                thread "Review\n[Attached: example.txt]" nil
                (list :content-blocks blocks))))
    (magent-thread-record-user-message-if-needed
     thread (magent-thread-turn-id turn) "Review\n[Attached: example.txt]"
     nil (list :content-blocks blocks))
    (let ((prompt (cdr (assq 'prompt
                             (magent-test--provider-context session)))))
      (should (string-match-p "Review" prompt))
      (should (string-match-p "URI: file:///tmp/example.txt" prompt))
      (should (string-match-p "resource body" prompt))
      (should-not (string-match-p "Attached: example.txt" prompt)))))

(ert-deftest magent-test-session-save-load-preserves-structured-user-resources ()
  "Test structured ACP resources survive JSON persistence as user context."
  (require 'magent-session)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (blocks '[((type . "text") (text . "Review"))
                   ((type . "resource")
                    (resource . ((uri . "file:///tmp/example.txt")
                                 (text . "persisted body"))))]))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (let* ((session (magent-session-get))
                 (thread (magent-session-thread-ledger session))
                 (turn (magent-thread-queue-turn
                        thread "Review\n[Attached: example.txt]" nil
                        (list :content-blocks blocks))))
            (magent-thread-record-user-message-if-needed
             thread (magent-thread-turn-id turn)
             "Review\n[Attached: example.txt]" nil
             (list :content-blocks blocks))
            (magent-thread-start-turn thread (magent-thread-turn-id turn))
            (magent-thread-record-message
             thread (magent-thread-turn-id turn) 'assistant "Reviewed.")
            (magent-thread-complete-turn thread (magent-thread-turn-id turn))
            (magent-test--save-current-session))
          (let* ((file (car (magent-test--session-files
                             magent-session-directory)))
                 (loaded (plist-get (magent-session-read-file file) :session))
                 (prompt (cdr (assq
                               'prompt
                               (magent-test--provider-context loaded)))))
            (should (string-match-p "URI: file:///tmp/example.txt" prompt))
            (should (string-match-p "persisted body" prompt))
            (should-not (string-match-p "Attached: example.txt" prompt))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-acp-session-prompt-forwards-structured-context ()
  "Test ACP submission stores blocks and exposes local resource paths."
  (require 'magent-acp)
  (let* ((runtime-session (magent-runtime-session-create
                           :id "session-1" :scope 'global))
         (client (magent-test--acp-client-for-runtime runtime-session))
        submitted response failure)
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (_session-id _scope) runtime-session))
              ((symbol-function 'magent-runtime-submit)
               (lambda (session prompt &rest args)
                 (setq submitted (list session prompt args))
                 (funcall (plist-get args :on-complete)
                          'completed (magent-execution-result-completed "ok")))))
      (magent-acp--handle-request
       client
       '((:method . "session/prompt")
         (:params . ((sessionId . "session-1")
                     (prompt . [((type . "text") (text . "Review"))
                                ((type . "resource")
                                 (resource
                                  . ((uri . "file:///tmp/example.txt")
                                     (text . "body"))))]))))
       (lambda (value) (setq response value))
       (lambda (err) (setq failure err))))
    (should-not failure)
    (should (equal (cadr submitted)
                   "Review\n[Attached: file:///tmp/example.txt]"))
    (let ((args (nth 2 submitted)))
      (should (equal (plist-get (plist-get args :context) :file-path)
                     "/tmp/example.txt"))
      (should (= (length (plist-get (plist-get args :turn-metadata)
                                    :content-blocks))
                 2)))
    (should (equal (map-elt response 'stopReason) "end_turn"))))

(ert-deftest magent-test-acp-session-prompt-selects-skill-as-normal-turn ()
  "ACP `/$skill' keeps raw input while explicitly selecting the skill."
  (require 'magent-acp)
  (let* ((magent-skills--registry nil)
        (magent-skills--scope-catalog (make-hash-table :test #'equal))
        (runtime-session
         (magent-runtime-session-create
          :id "session-1"
          :scope 'global
          :pending-skills '("existing-skill")))
        (client (magent-test--acp-client-for-runtime runtime-session))
        submitted response failure)
    (magent-skills-register
     (magent-skill-create
      :name "reviewer"
      :description "Review code."
      :type 'instruction))
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (session-id _scope)
                 (and (equal session-id "session-1") runtime-session)))
              ((symbol-function 'magent-runtime-submit)
               (lambda (session prompt &rest args)
                 (setq submitted (list session prompt args))
                 (setf (magent-runtime-session-pending-skills session) nil)
                 (funcall (plist-get args :on-complete)
                          'completed (magent-execution-result-completed "ok"))
                 "submission-1")))
      (magent-acp--handle-request
       client
       '((:method . "session/prompt")
         (:params . ((sessionId . "session-1")
                     (prompt
                      . [((type . "text")
                          (text . "/$reviewer focus on tests"))
                         ((type . "resource")
                          (resource
                           . ((uri . "file:///tmp/example.txt")
                              (text . "body"))))]))))
       (lambda (value) (setq response value))
       (lambda (err) (setq failure err))))
    (should-not failure)
    (should (eq (car submitted) runtime-session))
    (should
     (equal (cadr submitted)
            (concat "/$reviewer focus on tests\n"
                    "[Attached: file:///tmp/example.txt]")))
    (let* ((args (nth 2 submitted))
           (metadata (plist-get args :turn-metadata))
           (blocks (plist-get metadata :content-blocks)))
      (should (equal (plist-get args :skills)
                     '("existing-skill" "reviewer")))
      (should (equal (plist-get metadata :explicit-skill) "reviewer"))
      (should (eq (plist-get metadata :skill-invocation) 'acp-command))
      (should (= (length blocks) 2))
      (should (equal (map-elt (aref blocks 0) 'text)
                     "/$reviewer focus on tests")))
    (should-not (magent-runtime-session-pending-skills runtime-session))
    (should (equal (map-elt response 'stopReason) "end_turn"))))

(ert-deftest magent-test-acp-session-prompt-rejects-unknown-skill-command ()
  "Unknown `/$skill' input fails before an ordinary model submission."
  (require 'magent-acp)
  (let* ((magent-skills--registry nil)
        (magent-skills--scope-catalog (make-hash-table :test #'equal))
        (runtime-session
         (magent-runtime-session-create :id "session-1" :scope 'global))
        (client (magent-test--acp-client-for-runtime runtime-session))
        submitted response failure)
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (_session-id _scope) runtime-session))
              ((symbol-function 'magent-runtime-submit)
               (lambda (&rest _args) (setq submitted t))))
      (magent-acp--handle-request
       client
       '((:method . "session/prompt")
         (:params . ((sessionId . "session-1")
                     (prompt . [((type . "text")
                                 (text . "/$missing"))]))))
       (lambda (value) (setq response value))
       (lambda (err) (setq failure err))))
    (should-not submitted)
    (should-not response)
    (should failure)
    (should (string-match-p "unknown or unavailable instruction skill"
                            (map-elt failure 'message)))))

(ert-deftest magent-test-acp-command-answer-preserves-resource-context ()
  "Test a terminal Answer retains ACP resources and request context."
  (require 'magent-acp)
  (let* ((magent-action--registry nil)
        (magent-action--active-invocations (make-hash-table :test #'eq))
        (runtime-session
         (magent-runtime-session-create
          :id "session-1"
          :scope "/tmp/project"
          :magent-session (magent-session-create)))
        (client (magent-test--acp-client-for-runtime runtime-session))
        submitted response failure)
    (magent-action-register
     "review" :description "Review"
     :session-policy 'current
     :workflow
     (iter-lambda (_invocation)
       (magent-workflow-answer
           "Review" "Review the attached context."
         :append-argument-p t))
     :source-layer 'package)
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (session-id _scope)
                 (and (equal session-id "session-1") runtime-session)))
              ((symbol-function 'magent-runtime-submit)
               (lambda (session prompt &rest args)
                 (setq submitted (list session prompt args))
                 (funcall (plist-get args :on-complete)
                          'completed (magent-execution-result-completed "ok"))
                 "submission-1")))
      (magent-acp--handle-request
       client
       '((:method . "session/prompt")
         (:params . ((sessionId . "session-1")
                     (prompt
                      . [((type . "text") (text . "/review focus on tests"))
                         ((type . "resource")
                          (resource
                           . ((uri . "file:///tmp/example.txt")
                              (mimeType . "text/plain")
                              (text . "resource body"))))]))))
       (lambda (value) (setq response value))
       (lambda (err) (setq failure err))))
    (should-not failure)
    (should (eq (car submitted) runtime-session))
    (should
     (equal
      (cadr submitted)
      (concat "Review the attached context."
              "\n\nAdditional instruction:\nfocus on tests"
              "\n[Attached: file:///tmp/example.txt]")))
    (let* ((args (nth 2 submitted))
           (context (plist-get args :context))
           (metadata (plist-get args :turn-metadata))
           (blocks (plist-get metadata :content-blocks))
           (resource (map-elt (aref blocks 1) 'resource)))
      (should (equal (plist-get context :file-path) "/tmp/example.txt"))
      (should (equal (plist-get context :resource-paths)
                     '("/tmp/example.txt")))
      (should-not (plist-member metadata :workflow))
      (should (equal (map-elt (aref blocks 0) 'text)
                     (concat "Review the attached context."
                             "\n\nAdditional instruction:\nfocus on tests")))
      (should (equal (map-elt resource 'text) "resource body")))
    (should (equal (map-elt response 'stopReason) "end_turn"))))

(ert-deftest magent-test-acp-command-answer-prepends-buffer-snapshots ()
  "Test Answer buffers precede frontend resources and stay immutable."
  (require 'magent-acp)
  (let* ((magent-action--registry nil)
        (magent-action--active-invocations (make-hash-table :test #'eq))
        (runtime-session
         (magent-runtime-session-create
          :id "session-1" :scope 'global
          :magent-session (magent-session-create)))
        (client (magent-test--acp-client-for-runtime runtime-session))
        submitted response failure)
    (with-temp-buffer
      (rename-buffer " *magent-acp-context*")
      (insert "buffer snapshot body")
      (let ((context-buffer (current-buffer)))
        (magent-action-register
         "inspect" :description "Inspect"
         :session-policy 'current
         :workflow
         (iter-lambda (_invocation)
           (magent-workflow-answer
               "Inspect" "Inspect resources."
             :buffers (list context-buffer))))
        (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
                   (lambda (_session-id _scope) runtime-session))
                  ((symbol-function 'magent-runtime-submit)
                   (lambda (session prompt &rest args)
                     (setq submitted (list session prompt args))
                     (funcall (plist-get args :on-complete)
                              'completed (magent-execution-result-completed "ok"))
                     "submission-1")))
          (magent-acp--handle-request
           client
           '((:method . "session/prompt")
             (:params . ((sessionId . "session-1")
                         (prompt
                          . [((type . "text") (text . "/inspect"))
                             ((type . "resource")
                              (resource
                               . ((uri . "file:///tmp/frontend.txt")
                                  (text . "frontend body"))))]))))
           (lambda (value) (setq response value))
           (lambda (err) (setq failure err))))
        (insert " changed after submission")
        (should-not failure)
        (should (eq (car submitted) runtime-session))
        (should
         (equal
          (cadr submitted)
          (concat
           "Inspect resources.\n"
           "[Attached: emacs-buffer:///%20%2Amagent-acp-context%2A, "
           "file:///tmp/frontend.txt]")))
        (let* ((metadata (plist-get (nth 2 submitted) :turn-metadata))
               (blocks (plist-get metadata :content-blocks))
               (buffer-resource (map-elt (aref blocks 1) 'resource))
               (frontend-resource (map-elt (aref blocks 2) 'resource))
               (snapshot (map-elt buffer-resource 'text)))
          (should (= (length blocks) 3))
          (should (equal (map-elt (aref blocks 0) 'text)
                         "Inspect resources."))
          (should (string-match-p "buffer snapshot body" snapshot))
          (should-not (string-match-p "changed after submission" snapshot))
          (should (equal (map-elt frontend-resource 'text) "frontend body"))))
    (should (equal (map-elt response 'stopReason) "end_turn")))))

(ert-deftest magent-test-acp-session-prompt-expands-all-bundled-slash-commands ()
  "Test ACP session prompts dispatch every bundled Elisp prompt command."
  (require 'magent-acp)
  (let ((magent-action--registry nil)
        (magent-action--active-invocations (make-hash-table :test #'eq)))
    (magent-test--register-builtin-commands-only)
    (dolist (name magent-test--builtin-slash-command-names)
      (let* ((runtime-session
             (magent-runtime-session-create
              :id "session-1"
              :scope "/tmp/project"
              :magent-session (magent-session-create)
              :pending-skills '("existing-skill")))
            (client (magent-test--acp-client-for-runtime runtime-session))
            submitted response failure)
        (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
                   (lambda (session-id _scope)
                     (and (equal session-id "session-1")
                          runtime-session)))
                  ((symbol-function 'magent-runtime-submit)
                   (lambda (session prompt &rest args)
                     (setq submitted (list session prompt args))
                     (setf (magent-runtime-session-pending-skills session) nil)
                     (funcall (plist-get args :on-complete)
                              'completed (magent-execution-result-completed "ok"))
                     "submission-1"))
                  ((symbol-function
                    'magent-runtime-session-available-tool-names)
                   (lambda (&rest _)
                     '(read_file write_file edit_file grep glob bash
                       emacs_read emacs_eval emacs_eval_live
                       read_tool_output))))
          (magent-acp--handle-request
           client
           `((:method . "session/prompt")
             (:params . ((sessionId . "session-1")
                         (prompt . [((type . "text")
                                     (text . ,(format "/%s focus on tests"
                                                      name)))]))))
           (lambda (value) (setq response value))
           (lambda (err) (setq failure err))))
        (should-not failure)
        (should (eq (car submitted) runtime-session))
        (should (equal (cadr submitted)
                       (concat (magent-prompt-read
                                (format "actions/%s.org" name))
                               "\n\nAdditional instruction:\nfocus on tests")))
        (should-not (plist-get (nth 2 submitted) :skills))
        (should-not (magent-runtime-session-pending-skills runtime-session))
        (should (equal (map-elt response 'stopReason) "end_turn"))))))

(ert-deftest magent-test-acp-session-prompt-leaves-unknown-slash-command-unchanged ()
  "Test unknown slash commands are submitted as normal prompt text."
  (require 'magent-acp)
  (let* ((runtime-session (magent-runtime-session-create
                           :id "session-1" :scope 'global
                           :pending-skills '("existing-skill")))
         (client (magent-test--acp-client-for-runtime runtime-session))
        submitted response failure)
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (session-id _scope)
                 (and (equal session-id "session-1")
                      runtime-session)))
              ((symbol-function 'magent-runtime-submit)
               (lambda (session prompt &rest args)
                 (setq submitted (list session prompt))
                 (funcall (plist-get args :on-complete)
                          'completed (magent-execution-result-completed "ok")))))
      (magent-acp--handle-request
       client
       '((:method . "session/prompt")
         (:params . ((sessionId . "session-1")
                     (prompt . [((type . "text")
                                 (text . "/unknown focus on tests"))]))))
       (lambda (value) (setq response value))
       (lambda (err) (setq failure err))))
    (should-not failure)
    (should (eq (car submitted) runtime-session))
    (should (equal (cadr submitted) "/unknown focus on tests"))
    (should (equal (magent-runtime-session-pending-skills runtime-session)
                   '("existing-skill")))
    (should (equal (map-elt response 'stopReason) "end_turn"))))

(ert-deftest magent-test-acp-session-prompt-compacts-through-runtime ()
  "Test /compact invokes runtime compaction and forwards its completion."
  (require 'magent-acp)
  (require 'magent-action-controls)
  (let* ((magent-action--registry nil)
        (magent-action--active-invocations (make-hash-table :test #'eq))
        (magent-session-directory (make-temp-file "magent-compact-" t))
        (runtime-session
         (magent-runtime-session-create
          :id "session-1" :scope 'global
          :magent-session (magent-session-create)))
        (client (magent-test--acp-client-for-runtime runtime-session))
        compact-args response failure)
    (unwind-protect
        (progn
          (magent-action-controls-register)
          (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
                     (lambda (_session-id _scope) runtime-session))
                    ((symbol-function 'magent-runtime-session-compact)
                     (lambda (session &rest args)
                       (setq compact-args (cons session args))
                       (funcall (plist-get args :on-complete)
                                'completed
                                (magent-execution-result-completed "summary")))))
            (magent-acp--handle-request
             client
             '((:method . "session/prompt")
               (:params . ((sessionId . "session-1")
                           (prompt . [((type . "text")
                                       (text . "/compact keep decisions"))]))))
             (lambda (value) (setq response value))
             (lambda (err) (setq failure err))))
          (should-not failure)
          (should (eq (car compact-args) runtime-session))
          (should (equal (plist-get (cdr compact-args) :instruction)
                         "keep decisions"))
          (let ((metadata (plist-get (cdr compact-args) :turn-metadata)))
            (should (eq (plist-get metadata :source) 'magent-action))
            (should (equal (plist-get metadata :action) "compact"))
            (should (equal (plist-get metadata :action-input)
                           "/compact keep decisions")))
          (should (equal (map-elt response 'stopReason) "end_turn")))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-acp-session-prompt-does-not-run-memory-slash-locally ()
  "Test memory slash text is submitted as a normal prompt."
  (require 'magent-acp)
  (let* ((runtime-session (magent-runtime-session-create
                           :id "session-1" :scope 'global))
         notifications
         (client
          (magent-test--acp-client-for-runtime
           runtime-session
           (list (lambda (value) (push value notifications)))))
         submitted response failure)
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (session-id _scope)
                 (and (equal session-id "session-1")
                      runtime-session)))
              ((symbol-function 'magent-runtime-submit)
               (lambda (session prompt &rest args)
                 (setq submitted (list session prompt))
                 (funcall (plist-get args :on-complete)
                          'completed
                          "ok"))))
      (magent-acp--handle-request
       client
       '((:method . "session/prompt")
         (:params . ((sessionId . "session-1")
                     (prompt . [((type . "text")
                                 (text . "/magent-memory-clear"))]))))
       (lambda (value) (setq response value))
       (lambda (err) (setq failure err))))
    (should-not failure)
    (should (equal submitted
                   (list runtime-session "/magent-memory-clear")))
    (should (equal (map-elt response 'stopReason) "end_turn"))
    (should-not notifications)))

(ert-deftest magent-test-acp-call-failure-uses-current-callback-contract ()
  "Test ACP failures call the current one-argument callback contract."
  (require 'magent-acp)
  (let (received)
    (magent-acp--call-failure
     (magent-acp--wrap-callback
      '((:context-buffer . nil))
      nil
      (lambda (err)
        (setq received err)))
     'test-error)
    (should (eq received 'test-error))))

(ert-deftest magent-test-acp-notify-runs-in-client-context-buffer ()
  "Test ACP notifications run in the agent-shell client buffer."
  (require 'magent-acp)
  (let ((buffer (generate-new-buffer "*magent-acp-notify-test*"))
        observed-buffer)
    (unwind-protect
        (let ((client `((:context-buffer . ,buffer)
                        (:notification-handlers
                         . (,(lambda (_notification)
                               (setq observed-buffer (current-buffer)))))
                        (:request-handlers . nil))))
          (with-temp-buffer
            (magent-acp--notify client "session/update" nil))
          (should (eq observed-buffer buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-acp-raw-input-object-normalizes-tool-args ()
  "Test ACP rawInput is an alist object, not a Magent plist."
  (require 'magent-acp)
  (let ((raw-input (magent-acp--raw-input-object
                    '(:command "pwd"
			       :timeout nil
			       :count 2
			       :flag t
			       :nested (:path "/tmp")))))
    (should (equal raw-input
                   '((command . "pwd")
                     (count . 2)
                     (flag . t)
                     (nested . ((path . "/tmp"))))))
    (should (equal (mapcar #'car raw-input)
                   '(command count flag nested)))
    (should (equal (map-elt raw-input 'command) "pwd"))
    (should-not (assq 'timeout raw-input))))

(ert-deftest magent-test-acp-session-prompt-responds-after-runtime-completes ()
  "Test ACP prompt requests remain pending until the runtime turn completes."
  (require 'magent-acp)
  (let* ((runtime-session (magent-runtime-session-create
                           :id "session-1" :scope 'global))
         (client (magent-test--acp-client-for-runtime runtime-session))
         success failure submitted-prompt complete)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args)))
              ((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (session-id _scope)
                 (and (equal session-id "session-1")
                      runtime-session)))
              ((symbol-function 'magent-runtime-submit)
               (lambda (_runtime-session prompt &rest args)
                 (setq submitted-prompt prompt
                       complete (plist-get args :on-complete))
                 "submission-1")))
      (magent-acp--request-sender
       :client client
       :request
       '((:method . "session/prompt")
         (:params . ((sessionId . "session-1")
                     (prompt . [((type . "text")
                                 (text . "hello"))]))))
       :on-success (lambda (value) (setq success value))
       :on-failure (lambda (err) (setq failure err))))
    (should-not failure)
    (should (equal submitted-prompt "hello"))
    (should complete)
    (should-not success)
    (funcall complete 'completed "ok")
    (should (equal (map-elt success 'stopReason) "end_turn"))))

(ert-deftest magent-test-acp-stop-reason-preserves-failure-kind ()
  "Test ACP stopReason does not report internal failures as refusals."
  (require 'magent-acp)
  (should (equal (magent-acp--stop-reason
                  'failed
                  (magent-execution-result-failed
                   "Maximum sampling requests reached"
                   '(:status sampling-limit)))
                 "max_turn_requests"))
  (should (equal (magent-acp--stop-reason
                  'failed
                  (magent-execution-result-failed
                   "Request timed out"
                   '(:status timeout)))
                 "error"))
  (should (equal (magent-acp--stop-reason
                  'failed
                  (magent-execution-result-failed
                   "Model refused"
                   '(:status refusal)))
                 "refusal")))

(ert-deftest magent-test-acp-session-prompt-success-runs-in-request-buffer ()
  "Test deferred ACP prompt callbacks run in the buffer supplied by acp.el."
  (require 'magent-acp)
  (let* ((buffer (generate-new-buffer "*magent-acp-test*"))
         (runtime-session (magent-runtime-session-create
                           :id "session-1" :scope 'global))
         (client (magent-test--acp-client-for-runtime runtime-session))
         complete callback-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args)
                     (apply fn args)))
                  ((symbol-function 'magent-acp--runtime-session-by-id)
                   (lambda (session-id _scope)
                     (and (equal session-id "session-1")
                          runtime-session)))
                  ((symbol-function 'magent-runtime-submit)
                   (lambda (_runtime-session _prompt &rest args)
                     (setq complete (plist-get args :on-complete))
                     "submission-1")))
          (magent-acp--request-sender
           :client client
           :buffer buffer
           :request
           '((:method . "session/prompt")
             (:params . ((sessionId . "session-1")
                         (prompt . [((type . "text")
                                     (text . "hello"))]))))
           :on-success (lambda (_value)
                         (setq callback-buffer (current-buffer))))
          (with-temp-buffer
            (funcall complete 'completed "ok"))
          (should (eq callback-buffer buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-acp-observer-wraps-tool-result-content ()
  "Test tool result updates use the shape agent-shell renders."
  (require 'magent-acp)
  (let* (notifications
         (client `((:notification-handlers
                    . (,(lambda (notification)
                          (push notification notifications))))
                   (:request-handlers . nil)))
         (observer (magent-acp--observer client "session-1")))
    (funcall observer
             '(:type tool-call-complete
		     :tool-id "tool-1"
		     :name "bash"
		     :status completed
		     :output-preview "done"))
    (let* ((notification (car notifications))
           (params (map-elt notification 'params))
           (update (map-elt params 'update))
           (content-item (aref (map-elt update 'content) 0))
           (content-block (map-elt content-item 'content)))
      (should (equal (map-elt update 'sessionUpdate) "tool_call_update"))
      (should (equal (map-elt update 'toolCallId) "tool-1"))
      (should (equal (map-elt content-block 'type) "text"))
      (should (equal (map-elt content-block 'text) "done")))))

(ert-deftest magent-test-acp-observer-preserves-tool-title-on-completion ()
  "Test completion updates do not replace a descriptive tool title."
  (require 'magent-acp)
  (let* (notifications
         (client `((:notification-handlers
                    . (,(lambda (notification)
                          (push notification notifications))))
                   (:request-handlers . nil)))
         (observer (magent-acp--observer client "session-1"))
         (summary "[Find callers] capability in lisp"))
    (funcall observer
             `(:type tool-call-start
		     :tool-id "tool-1"
		     :name "grep"
		     :kind grep
		     :summary ,summary
		     :raw-input (:pattern "capability"
					  :path "lisp"
					  :reason "Find callers")))
    (funcall observer
             '(:type tool-call-complete
		     :tool-id "tool-1"
		     :name "grep"
		     :status completed
		     :output-preview "lisp/magent.el:118"))
    (let* ((updates
            (mapcar (lambda (notification)
                      (map-nested-elt notification '(params update)))
                    (nreverse notifications)))
           (start (nth 0 updates))
           (complete (nth 1 updates)))
      (should (equal (map-elt start 'sessionUpdate) "tool_call"))
      (should (equal (map-elt start 'title) summary))
      (should (equal (map-elt start 'kind) "read"))
      (should (equal (map-nested-elt start '(rawInput pattern))
                     "capability"))
      (should (equal (map-nested-elt start '(rawInput path)) "lisp"))
      (should (equal (map-nested-elt start '(rawInput reason))
                     "Find callers"))
      (should (equal (map-elt complete 'sessionUpdate)
                     "tool_call_update"))
      (should (equal (map-elt complete 'status) "completed"))
      (should-not (assq 'title complete))
      (should (equal
               (map-nested-elt complete '(content 0 content text))
               "lisp/magent.el:118")))))

(ert-deftest magent-test-acp-observer-pushes-session-title-on-completion ()
  "Test turn completion publishes the canonical title through ACP."
  (require 'magent-acp)
  (let* ((magent-acp--client-session-scopes
          (make-hash-table :test #'eq :weakness 'key))
         notifications
         (client `((:notification-handlers
                    . (,(lambda (notification)
                          (push notification notifications))))
                   (:request-handlers . nil)))
         (session
          (magent-test--session-with-transcript
           "session-1" '((user "  Startup\n delay  "))))
         (runtime-session
          (magent-runtime-session-create
           :id "session-1"
           :scope "/project-a"
           :magent-session session))
         (observer (magent-acp--observer client "session-1")))
    (magent-acp--bind-client-session client runtime-session)
    (cl-letf (((symbol-function 'magent-runtime-session-from-id)
               (lambda (session-id scope)
                 (should (equal session-id "session-1"))
                 (should (equal scope "/project-a"))
                 runtime-session)))
      (funcall observer '(:type turn-complete)))
    (let ((update (map-nested-elt (car notifications) '(params update))))
      (should (equal (map-elt update 'sessionUpdate)
                     "session_info_update"))
      (should (equal (map-elt update 'title) "Startup delay")))))

(ert-deftest magent-test-acp-observer-drops-leading-stream-whitespace ()
  "Test ACP observer does not emit blank blocks at stream start."
  (require 'magent-acp)
  (let* ((magent-include-reasoning t)
         notifications
         (client `((:notification-handlers
                    . (,(lambda (notification)
                          (push notification notifications))))
                   (:request-handlers . nil)))
         (observer (magent-acp--observer client "session-1")))
    (funcall observer '(:type assistant-delta :text "\n\n"))
    (funcall observer '(:type assistant-delta :text "  hello"))
    (funcall observer '(:type assistant-delta :text "\n\nworld"))
    (funcall observer '(:type reasoning-delta :text "\n\n"))
    (funcall observer '(:type reasoning-delta :text "  thought"))
    (let ((updates (mapcar (lambda (notification)
                             (map-nested-elt notification
                                             '(params update)))
                           (nreverse notifications))))
      (should (equal (mapcar (lambda (update)
                               (map-elt update 'sessionUpdate))
                             updates)
                     '("agent_message_chunk"
                       "agent_message_chunk"
                       "agent_thought_chunk")))
      (should (equal (map-nested-elt (nth 0 updates) '(content text))
                     "hello"))
      (should (equal (map-nested-elt (nth 1 updates) '(content text))
                     "\n\nworld"))
      (should (equal (map-nested-elt (nth 2 updates) '(content text))
                     "thought")))))

(ert-deftest magent-test-acp-observer-normalizes-tool-call-raw-input ()
  "Test tool_call updates send agent-shell-compatible rawInput."
  (require 'magent-acp)
  (let* (notifications
         (client `((:notification-handlers
                    . (,(lambda (notification)
                          (push notification notifications))))
                   (:request-handlers . nil)))
         (observer (magent-acp--observer client "session-1")))
    (funcall observer
             '(:type tool-call-start
		     :tool-id "tool-1"
		     :name "bash"
		     :kind bash
		     :summary "pwd"
		     :raw-input (:command "pwd"
					  :timeout nil
					  :reason "Print current directory")))
    (let* ((update (map-nested-elt (car notifications) '(params update)))
           (raw-input (map-elt update 'rawInput)))
      (should (equal (map-elt update 'sessionUpdate) "tool_call"))
      (should (equal raw-input
                     '((command . "pwd")
                       (reason . "Print current directory"))))
      (should (equal (map-elt raw-input 'command) "pwd"))
      (should (equal (mapcar #'car raw-input) '(command reason))))))

(ert-deftest magent-test-acp-approval-provider-normalizes-raw-input ()
  "Test permission requests expose rawInput as an ACP object."
  (require 'magent-acp)
  (let* (requests
         (client `((:notification-handlers . nil)
                   (:request-handlers
                    . (,(lambda (request)
                          (push request requests)))))))
    (funcall (magent-acp--approval-provider client "session-1")
             '(:request-id "request-1"
			   :tool-name "bash"
			   :summary "pwd"
			   :perm-key bash
			   :args (:command "pwd" :timeout nil)))
    (let ((raw-input (map-nested-elt (car requests)
                                     '(params toolCall rawInput))))
      (should (equal raw-input '((command . "pwd"))))
      (should (equal (map-elt raw-input 'command) "pwd")))))

(ert-deftest magent-test-acp-once-only-approval-hides-always-allow ()
  "ACP does not advertise a persistent grant for once-only eval tools."
  (require 'magent-acp)
  (let* (requests
         (client `((:notification-handlers . nil)
                   (:request-handlers
                    . (,(lambda (request) (push request requests)))))))
    (funcall (magent-acp--approval-provider client "session-1")
             '(:request-id "request-eval"
               :tool-name "emacs_eval"
               :summary "evaluate"
               :perm-key emacs_eval
               :approval-policy once-only
               :args (:sexp "(+ 1 1)")))
    (let* ((options (map-nested-elt (car requests) '(params options)))
           (ids (mapcar (lambda (option) (map-elt option 'optionId))
                        (append options nil))))
      (should (equal ids '("allow_once" "reject_once"))))))

(ert-deftest magent-test-acp-session-replay-normalizes-tool-raw-input ()
  "Test replayed tool calls expose rawInput as an ACP object."
  (require 'magent-acp)
  (let* (notifications
         (client `((:notification-handlers
                    . (,(lambda (notification)
                          (push notification notifications))))
                   (:request-handlers . nil)))
         (item (magent-thread-item-create
                :id "item-1"
                :call-id "tool-1"
                :type 'tool
                :status 'completed
                :name "bash"
                :input '(:command "pwd" :timeout nil)
                :output "/home/jamie")))
    (magent-acp--emit-item-replay client "session-1" item)
    (let* ((updates (mapcar (lambda (notification)
                              (map-nested-elt notification '(params update)))
                            (nreverse notifications)))
           (tool-call (car updates))
           (raw-input (map-elt tool-call 'rawInput)))
      (should (equal (map-elt tool-call 'sessionUpdate) "tool_call"))
      (should (equal raw-input '((command . "pwd")))))))

(ert-deftest magent-test-acp-response-sender-resolves-approval ()
  "Test ACP permission responses resolve Magent approval requests."
  (require 'magent-acp)
  (let (captured)
    (cl-letf (((symbol-function 'magent-approval-resolve-request)
               (lambda (request-id decision)
                 (setq captured (list request-id decision)))))
      (magent-acp--response-sender
       :client nil
       :response
       (acp-make-session-request-permission-response
        :request-id "req-1"
        :option-id "allow_once")))
    (should (equal captured '("req-1" allow-once)))))

(ert-deftest magent-test-acp-set-model-accepts-current-gptel-model ()
  "Test ACP session/set_model works for agent-shell bootstrap."
  (require 'magent-acp)
  (let* ((gptel-model 'test-model)
         (magent-default-agent "build")
         (runtime-session (magent-runtime-session-create :id "session-1"))
         response)
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (session-id _scope)
                 (and (equal session-id "session-1")
                      runtime-session)))
              ((symbol-function 'magent-runtime-session-agent-name)
               (lambda (_session) "build"))
              ((symbol-function 'magent-agent-registry-primary-agents)
               (lambda ()
                 (list (magent-agent-info-create
                        :name "build"
                        :description "Build")))))
      (setq response
            (magent-acp--handle-set-model
             '((sessionId . "session-1")
               (modelId . "test-model"))))
      (should (equal (map-nested-elt response '(models currentModelId))
                     "test-model"))
      (should (equal (map-elt response 'sessionId) "session-1")))))

(ert-deftest magent-test-acp-set-config-option-updates-effort ()
  "Test ACP session/set_config_option updates Magent effort."
  (require 'magent-acp)
  (let* ((runtime-session (magent-runtime-session-create :id "session-1"))
         response)
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (session-id _scope)
                 (and (equal session-id "session-1")
                      runtime-session)))
              ((symbol-function 'magent-runtime-session-agent-name)
               (lambda (_session) "build"))
              ((symbol-function 'magent-agent-registry-primary-agents)
               (lambda ()
                 (list (magent-agent-info-create
                        :name "build"
                        :description "Build")))))
      (setq response
            (magent-acp--handle-set-config-option
             '((sessionId . "session-1")
               (configId . "effort")
               (value . "xhigh"))))
      (should (eq (magent-runtime-session-effort runtime-session) 'xhigh))
      (should (equal (map-elt (aref (map-elt response 'configOptions) 0)
                              'currentValue)
                     "xhigh")))))

(ert-deftest magent-test-acp-set-config-option-updates-capabilities ()
  "Test ACP exposes a per-session automatic capability switch."
  (require 'magent-acp)
  (let ((runtime-session (magent-runtime-session-create :id "session-1")))
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (_session-id _scope) runtime-session))
              ((symbol-function 'magent-runtime-session-agent-name)
               (lambda (_session) "build"))
              ((symbol-function 'magent-agent-registry-primary-agents)
               (lambda () nil)))
      (let ((response
             (magent-acp--handle-set-config-option
              '((sessionId . "session-1")
                (configId . "capabilities")
                (value . "disabled")))))
        (should-not
         (magent-runtime-session-capabilities-enabled-p runtime-session))
        (should (equal (map-elt (aref (map-elt response 'configOptions) 1)
                                'currentValue)
                       "disabled"))))))

(ert-deftest magent-test-agent-shell-config-creates-in-process-client ()
  "Test Magent agent-shell config creates an in-process ACP client."
  (require 'magent-agent-shell)
  (let* ((other-maker (lambda () '((:identifier . other))))
         (agent-shell-agent-configs (list other-maker))
         (magent-action-registry-changed-hook nil)
         (identifier (magent-agent-shell-ensure-config))
         (config (funcall (car agent-shell-agent-configs)))
         (client (with-temp-buffer
                   (funcall (map-elt config :client-maker)
                            (current-buffer)))))
    (should (eq identifier 'magent))
    (should (eq (map-elt config :identifier) 'magent))
    (should (eq (car agent-shell-agent-configs)
                #'magent-agent-shell-make-config))
    (should (eq (cadr agent-shell-agent-configs) other-maker))
    (should (equal (map-elt client :command) "cat"))
    (should (eq (map-elt client :request-sender)
                #'magent-acp--request-sender))
    (should (eq (map-elt client :notification-sender)
                #'magent-acp--notification-sender))
    (should (eq (map-elt client :response-sender)
                #'magent-acp--response-sender))
    (should (memq #'magent-acp--refresh-available-commands
                  magent-action-registry-changed-hook))))

(ert-deftest magent-test-agent-shell-config-installs-magent-session-strategy ()
  "Test generic agent-shell selection uses Magent's session strategy."
  (require 'magent-agent-shell)
  (let ((magent-agent-shell-session-strategy 'new))
    (let* ((config (magent-agent-shell-make-config))
           (client-maker (map-elt config :client-maker))
           (previous-strategy
            (default-value 'agent-shell-session-strategy)))
      (unwind-protect
          (progn
            (set-default 'agent-shell-session-strategy 'prompt)
            (with-temp-buffer
              (should-not (local-variable-p 'agent-shell-session-strategy))
              (funcall client-maker (current-buffer))
              (should (local-variable-p 'agent-shell-session-strategy))
              (should
               (eq (buffer-local-value 'agent-shell-session-strategy
                                       (current-buffer))
                   'new))
              ;; agent-shell snapshots its explicit generic-picker strategy
              ;; after the first client construction, then constructs the
              ;; real client once more during bootstrap.
              (setq-local agent-shell-session-strategy 'prompt)
              (funcall client-maker (current-buffer))
              (should (eq agent-shell-session-strategy 'new)))
            (with-temp-buffer
              (set (make-local-variable 'agent-shell-session-strategy)
                   'latest)
              (funcall client-maker (current-buffer))
              (should
               (eq (buffer-local-value 'agent-shell-session-strategy
                                       (current-buffer))
                   'latest))))
        (set-default 'agent-shell-session-strategy previous-strategy)))))

(ert-deftest magent-test-generic-agent-shell-start-uses-magent-session-strategy ()
  "Test generic agent-shell startup cannot overwrite Magent's strategy."
  (require 'magent-agent-shell)
  (let ((agent-shell-session-strategy 'prompt)
        (magent-agent-shell-session-strategy 'new)
        shell-buffer)
    (cl-letf (((symbol-function 'magent-acp--request-sender)
               (lambda (&rest _args) nil)))
      (unwind-protect
          (progn
            (setq shell-buffer
                  (agent-shell-start
                   :config (magent-agent-shell-make-config)))
            (should
             (eq (buffer-local-value 'agent-shell-session-strategy
                                     shell-buffer)
                 'new)))
        (when (buffer-live-p shell-buffer)
          (kill-buffer shell-buffer))))))

(ert-deftest magent-test-start-is-canonical-agent-shell-entry-point ()
  "Test `magent-start' opens Magent through the supported frontend."
  (require 'magent-agent-shell)
  (let ((magent-agent-shell-session-strategy 'new)
        captured-config captured-strategy)
    (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
               #'ignore)
              ((symbol-function 'agent-shell-start)
               (lambda (&rest args)
                 (setq captured-config (plist-get args :config))
                 (setq captured-strategy agent-shell-session-strategy)
                 'shell-buffer)))
      (should (commandp 'magent-start))
      (should (eq (magent-start) 'shell-buffer))
      (should (eq (map-elt captured-config :identifier) 'magent))
      (should (eq captured-strategy 'new)))))

(ert-deftest magent-test-agent-shell-adapter-has-a-narrow-private-boundary ()
  "Test production coupling is limited to context compatibility advices."
  (require 'magent-agent-shell)
  (let ((source (expand-file-name "lisp/magent-agent-shell.el"
                                  magent-test--root-directory)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (private-state '("agent-shell--state"
                               "agent-shell--send-command"
                               "agent-shell--display-buffer"
                               "agent-shell--dwim"
                               "shell-maker--"))
        (goto-char (point-min))
        (should-not (search-forward private-state nil t)))))
  (should
   (equal
    magent-agent-shell--context-compatibility-advices
    '((agent-shell--context . magent-agent-shell--context)
      (agent-shell--get-region-context .
                                       magent-agent-shell--get-region-context)
      (agent-shell--get-files-context . magent-agent-shell--get-files-context)
      (agent-shell--get-current-line-context .
                                             magent-agent-shell--get-current-line-context)))))

(ert-deftest magent-test-agent-shell-suppresses-blank-line-context ()
  "Test blank current-line context does not produce inverted line ranges."
  (require 'magent-agent-shell)
  (with-temp-buffer
    (insert "first\n\nthird\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((called nil)
          (magent-agent-shell--context-request-p t))
      (should-not
       (magent-agent-shell--get-current-line-context
        (lambda (&rest _args)
          (setq called t)
          "bad-context")
        :agent-cwd default-directory))
      (should-not called))
    (goto-char (point-min))
    (let ((called nil)
          (magent-agent-shell--context-request-p t))
      (should
       (equal
        (magent-agent-shell--get-current-line-context
         (lambda (&rest _args)
           (setq called t)
           "line-context")
         :agent-cwd default-directory)
        "line-context"))
      (should called))))

(ert-deftest magent-test-agent-shell-remote-line-context-avoids-file-io ()
  "Test remote current-line context does not query the TRAMP filesystem."
  (require 'magent-agent-shell)
  (magent-agent-shell--ensure-loaded)
  (with-temp-buffer
    (insert "remote line\n")
    (goto-char (point-min))
    (setq buffer-file-name "/ssh:test.invalid:/srv/project/example.el"
          default-directory "/ssh:test.invalid:/srv/project/")
    (let ((magent-agent-shell--context-request-p t)
          context)
      (cl-letf (((symbol-function 'file-in-directory-p)
                 (lambda (&rest _args)
                   (ert-fail "Remote line context performed file I/O"))))
        (setq context
              (agent-shell--get-current-line-context
               :agent-cwd "/ssh:test.invalid:/srv/project/")))
      (setq context (substring-no-properties context))
      (should (string-match-p
               (regexp-quote buffer-file-name)
               context))
      (should (string-match-p "remote line" context)))))

(ert-deftest magent-test-agent-shell-remote-region-context-avoids-file-io ()
  "Test explicit remote region context does not query the TRAMP filesystem."
  (require 'magent-agent-shell)
  (magent-agent-shell--ensure-loaded)
  (with-temp-buffer
    (insert "first remote line\nsecond remote line\n")
    (setq-local transient-mark-mode t)
    (goto-char (point-min))
    (push-mark (line-end-position) t t)
    (setq buffer-file-name "/ssh:test.invalid:/srv/project/example.el"
          default-directory "/ssh:test.invalid:/srv/project/")
    (let ((magent-agent-shell--context-request-p t)
          context)
      (cl-letf (((symbol-function 'file-in-directory-p)
                 (lambda (&rest _args)
                   (ert-fail "Remote region context performed file I/O"))))
        (setq context
              (agent-shell--get-region-context
               :deactivate t
               :agent-cwd "/ssh:test.invalid:/srv/project/")))
      (setq context (substring-no-properties context))
      (should (string-match-p
               (regexp-quote buffer-file-name)
               context))
      (should (string-match-p "first remote line" context)))))

(ert-deftest magent-test-agent-shell-remote-files-context-avoids-file-io ()
  "Test Dired-style remote file context does not probe the TRAMP host."
  (require 'magent-agent-shell)
  (magent-agent-shell--ensure-loaded)
  (let* ((remote-root "/ssh:test.invalid:/srv/project/")
         (remote-file (concat remote-root "README.org"))
         (magent-agent-shell--context-request-p t)
         context)
    (cl-letf (((symbol-function 'agent-shell--load-image)
               (lambda (&rest _args)
                 (ert-fail "Remote file context attempted image I/O")))
              ((symbol-function 'file-in-directory-p)
               (lambda (&rest _args)
                 (ert-fail "Remote file context attempted containment I/O"))))
      (setq context
            (agent-shell--get-files-context
             :files (list remote-file) :agent-cwd remote-root)))
    (should (equal (substring-no-properties context)
                   (concat "@" remote-file)))))

(ert-deftest magent-test-agent-shell-empty-remote-files-context-stays-nil ()
  "An empty files source must not mask later region or line context."
  (require 'magent-agent-shell)
  (magent-agent-shell--ensure-loaded)
  (let ((magent-agent-shell--context-request-p t))
    (should-not
     (agent-shell--get-files-context
      :files nil :agent-cwd "/ssh:test.invalid:/srv/project/"))))

(ert-deftest magent-test-agent-shell-context-workaround-is-magent-scoped ()
  "Remote path handling leaves non-Magent agent-shell backends unchanged."
  (require 'magent-agent-shell)
  (let ((magent-shell (generate-new-buffer " *magent-context-shell*"))
        (other-shell (generate-new-buffer " *other-context-shell*"))
        (remote-cwd "/ssh:test.invalid:/srv/project/"))
    (unwind-protect
        (progn
          (with-current-buffer magent-shell
            (setq major-mode 'agent-shell-mode))
          (with-current-buffer other-shell
            (setq major-mode 'agent-shell-mode))
          (with-temp-buffer
            (setq buffer-file-name
                  "/ssh:test.invalid:/srv/project/example.el")
            (cl-letf (((symbol-function 'agent-shell-get-config)
                       (lambda (buffer)
                         `((:identifier
                            . ,(if (eq buffer magent-shell)
                                   'magent
                                 'other))))))
              (dolist (case `((,magent-shell . nil)
                              (,other-shell . ,remote-cwd)))
                (let (captured-cwd)
                  (magent-agent-shell--context
                   (lambda (&rest _args)
                     (magent-agent-shell--get-region-context
                      (lambda (&rest region-args)
                        (setq captured-cwd
                              (plist-get region-args :agent-cwd)))
                      :agent-cwd remote-cwd))
                   :shell-buffer (car case))
                  (should (equal captured-cwd (cdr case))))))))
      (kill-buffer magent-shell)
      (kill-buffer other-shell))))

(ert-deftest magent-test-runtime-cancel-is-session-scoped ()
  "Test runtime cancellation removes only the requested session's work."
  (require 'magent-runtime-api)
  (let ((magent-runtime-queue--active nil)
        (magent-runtime-queue--pending nil)
        (magent--current-session nil)
        (magent-session--current-scope 'global)
        callbacks notifications started save-calls)
    (cl-labels
        ((make-runtime-session
           (id scope)
           (magent-runtime-session-create
            :id id
            :scope scope
            :magent-session (magent-session-create :id id)))
         (make-submission
           (id session)
           (let* ((session-id (magent-runtime-session-id session))
                  (magent-session
                   (magent-runtime-session-magent-session session))
                  (thread (magent-session-thread-ledger magent-session))
                  (turn (magent-thread-queue-turn
                         thread id nil (list :source 'test))))
             (magent-runtime-submission-create
              :id id
              :session session
              :session-id session-id
              :turn-id (magent-thread-turn-id turn)
              :observer
              (lambda (event)
		(push (list id (plist-get event :type)) notifications))
              :on-complete
              (lambda (status result)
		(push (list id status result) callbacks))))))
      (let* ((session-a (make-runtime-session "session-a" "/tmp/project-a"))
             (session-b (make-runtime-session "session-b" "/tmp/project-b"))
             (active-a (make-submission "active-a" session-a))
             (queued-a (make-submission "queued-a" session-a))
             (queued-b (make-submission "queued-b" session-b)))
        (setq magent--current-session
              (magent-runtime-session-magent-session session-b)
              magent-session--current-scope "/tmp/project-b")
        (magent-runtime-queue-submit active-a #'ignore)
        (magent-runtime-queue-submit queued-a #'ignore)
        (magent-runtime-queue-submit
         queued-b #'magent-runtime-api--start-submission)
        (cl-letf (((symbol-function 'magent-runtime-api--start-submission)
                   (lambda (submission)
                     (push (magent-runtime-submission-id submission)
                           started)
                     (setf (magent-runtime-submission-status submission)
                           'running)))
                  ((symbol-function
                    'magent-session-save-deferred-for-session)
                   (lambda (session &optional scope _delay)
                     (push (list session scope) save-calls))))
          (should (= (magent-runtime-cancel session-a) 2)))
        (should (eq (magent-runtime-submission-status active-a)
                    'cancelled))
        (should (eq (magent-runtime-submission-status queued-a)
                    'cancelled))
        (should (eq (magent-runtime-submission-status queued-b)
                    'running))
        (should (eq (magent-runtime-queue-active-submission)
                    queued-b))
        (should-not magent-runtime-queue--pending)
        (should (equal started '("queued-b")))
        (should (assoc "active-a" callbacks))
        (should (assoc "queued-a" callbacks))
        (should-not (assoc "queued-b" callbacks))
        (should (assoc "active-a" notifications))
        (should (assoc "queued-a" notifications))
        (let* ((thread-a
                (magent-session-thread-ledger
                 (magent-runtime-session-magent-session session-a)))
               (active-turn
                (magent-thread-find-turn
                 thread-a (magent-runtime-submission-turn-id active-a)))
               (queued-turn
                (magent-thread-find-turn
                 thread-a (magent-runtime-submission-turn-id queued-a))))
          (should (eq (magent-thread-turn-status active-turn)
                      'interrupted))
          (should (eq (magent-thread-turn-status queued-turn)
                      'dropped)))
        (should (= (cl-count
                    (magent-runtime-session-magent-session session-a)
                    save-calls
                    :key #'car
                    :test #'eq)
                   2))
        (should-not
         (cl-find (magent-runtime-session-magent-session session-b)
                  save-calls
                  :key #'car
                  :test #'eq))
        (should (cl-every
                 (lambda (call)
                   (equal (cadr call) "/tmp/project-a"))
                 save-calls))))))

(ert-deftest magent-test-runtime-session-clear-preserves-runtime-identity ()
  "Test runtime clear replaces conversation state without changing its id."
  (require 'magent-runtime-api)
  (let* ((magent-session-directory
          (make-temp-file "magent-runtime-clear-test-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (agent (magent-agent-info-create :name "build"))
         (old-session (magent-session-create
                       :id "session-1" :agent agent :max-history 42))
         (runtime-session
          (magent-runtime-session-create
           :id "session-1"
           :scope 'global
           :magent-session old-session
           :pending-skills '("review")))
         (persisted (expand-file-name
                     "session-1.json"
                     (magent-session--scope-storage-directory 'global)))
         cancelled overrides-cleared)
    (unwind-protect
        (progn
          (magent-test--record-session-entry old-session 'user "old context")
          (make-directory (file-name-directory persisted) t)
          (with-temp-file persisted (insert "old transcript"))
          (cl-letf (((symbol-function 'magent-runtime-cancel)
                     (lambda (session)
                       (setq cancelled session)
                       0))
                    ((symbol-function
                      'magent-clear-capability-overrides)
                     (lambda () (setq overrides-cleared t))))
            (magent-runtime-session-clear runtime-session))
          (let ((new-session
                 (magent-runtime-session-magent-session runtime-session)))
            (should (eq cancelled runtime-session))
            (should overrides-cleared)
            (should (eq old-session new-session))
            (should (equal (magent-runtime-session-id runtime-session)
                           "session-1"))
            (should (equal (magent-session-id new-session) "session-1"))
            (should (eq (magent-session-agent new-session) agent))
            (should (= (magent-session-max-history new-session) 42))
            (should-not (magent-test--session-transcript new-session))
            (should-not
             (magent-runtime-session-pending-skills runtime-session))
            (should-not (file-exists-p persisted))
            (should (eq (magent-session-get-if-present 'global)
                        new-session))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-runtime-session-compact-keeps-selected-agent ()
  "Test compaction uses a request-local hidden agent and history boundary."
  (require 'magent-runtime-api)
  (let* ((selected-agent (magent-agent-info-create :name "build"))
         (compaction-agent (magent-agent-info-create :name "compaction"))
         (session (magent-session-create
                   :id "session-1" :agent selected-agent))
         (runtime-session
          (magent-runtime-session-create
           :id "session-1" :scope 'global :magent-session session
           :pending-skills '("review")))
         submitted completion pending-during-submit)
    (cl-letf (((symbol-function 'magent-agent-registry-get)
               (lambda (name)
                 (and (equal name "compaction") compaction-agent)))
              ((symbol-function 'magent-runtime-submit)
               (lambda (_runtime prompt &rest args)
                 (setq submitted (cons prompt args)
                       pending-during-submit
                       (magent-runtime-session-pending-skills
                        runtime-session))
                 "submission-1")))
      (magent-runtime-session-compact
       runtime-session
       :instruction "keep exact filenames"
       :turn-metadata '(:source test :request-id "request-1")
       :on-complete
       (lambda (status result)
         (setq completion (list status result))))
      (should (eq (plist-get (cdr submitted) :agent) compaction-agent))
      (should (equal (plist-get (cdr submitted) :turn-metadata)
                     '(:compaction t :source test :request-id "request-1")))
      (should-not (plist-get (cdr submitted) :skills))
      (should-not pending-during-submit)
      (should (equal (magent-runtime-session-pending-skills runtime-session)
                     '("review")))
      (should (string-match-p
               (regexp-quote
                "Additional instruction:\nkeep exact filenames")
               (car submitted)))
      (funcall (plist-get (cdr submitted) :on-complete)
               'completed "summary"))
    (should (eq (magent-session-agent session) selected-agent))
    (should (equal completion '(completed "summary")))))

(ert-deftest magent-test-runtime-submit-agent-is-request-local ()
  "An explicit submission agent does not replace the selected session agent."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-runtime-queue--arbiter-ticket-adapters
          (make-hash-table :test #'eq))
         (selected-agent (magent-agent-info-create :name "build"))
         (review-agent (magent-agent-info-create :name "review"))
         (session
          (magent-session-create :id "session-1" :agent selected-agent))
         (runtime-session
          (magent-runtime-session-create
           :id "session-1" :scope 'global :magent-session session))
         (blocker (magent-runtime-submission-create :id "blocker")))
    (magent-runtime-queue-submit blocker #'ignore)
    (cl-letf (((symbol-function 'magent-agent-registry-get)
               (lambda (name)
                 (and (equal name "review") review-agent))))
      (magent-runtime-submit
       runtime-session "review this" :agent 'review :tools nil))
    (let ((submission (car magent-runtime-queue--pending)))
      (should (eq (magent-runtime-submission-agent submission) review-agent))
      (should-not (magent-runtime-submission-tool-names submission))
      (should (eq (magent-session-agent session) selected-agent)))))

(ert-deftest magent-test-runtime-cancel-submission-leaves-peer-queued ()
  "Submission cancellation removes only the exact queued request."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-runtime-queue--arbiter-ticket-adapters
          (make-hash-table :test #'eq))
         (runtime-session
          (magent-runtime-session-create
           :id "session-1" :scope 'global
           :magent-session (magent-session-create :id "session-1")))
         (blocker (magent-runtime-submission-create :id "blocker"))
         (target
          (magent-runtime-submission-create
           :id "target" :session runtime-session :session-id "session-1"))
         (peer
          (magent-runtime-submission-create
           :id "peer" :session runtime-session :session-id "session-1"))
         completion)
    (setf (magent-runtime-submission-on-complete target)
          (lambda (status _result) (setq completion status)))
    (magent-runtime-queue-submit blocker #'ignore)
    (magent-runtime-queue-submit target #'ignore)
    (magent-runtime-queue-submit peer #'ignore)
    (should
     (magent-runtime-cancel-submission runtime-session "target"))
    (should (eq completion 'cancelled))
    (should (eq (magent-runtime-queue-active-submission) blocker))
    (should (equal magent-runtime-queue--pending (list peer)))
    (should-not
     (magent-runtime-cancel-submission runtime-session "missing"))))

(ert-deftest magent-test-runtime-submit-carries-session-effort ()
  "Test runtime submissions copy session effort into request context."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-session--current-scope 'global)
         (session (magent-session-create :id "session-1"))
         (runtime-session
          (magent-runtime-session-create
           :id "session-1"
           :scope "/tmp/project"
           :magent-session session
           :effort 'xhigh))
         captured-context)
    (cl-letf (((symbol-function 'magent-agent-run-turn)
               (lambda (&rest args)
                 (setq captured-context (plist-get args :request-context))
                 'loop))
              ((symbol-function 'magent-runtime-api--finish-submission)
               #'ignore)
              ((symbol-function 'magent-session-save-deferred-for-session)
               #'ignore))
      (magent-runtime-submit runtime-session "hello"))
    (should (magent-request-context-p captured-context))
    (should (eq (magent-request-context-effort captured-context) 'xhigh))
    (should (eq (magent-request-context-tool-names captured-context) :all))
    (should (eq (magent-thread-turn-status
                 (car (magent-thread-turns
                       (magent-session-thread-ledger session))))
                'in-progress))
    (should (equal (magent-thread-scope
                    (magent-session-thread-ledger session))
                   "/tmp/project"))))

(ert-deftest magent-test-runtime-submit-carries-exact-tool-names ()
  "Explicit runtime tool names reach the request context unchanged."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (session (magent-session-create :id "session-tools"))
         (runtime-session
          (magent-runtime-session-create
           :id "session-tools"
           :scope 'global
           :magent-session session))
         captured-context)
    (cl-letf (((symbol-function 'magent-agent-run-turn)
               (lambda (&rest args)
                 (setq captured-context (plist-get args :request-context))
                 'loop))
              ((symbol-function 'magent-runtime-api--finish-submission)
               #'ignore)
              ((symbol-function 'magent-session-save-deferred-for-session)
               #'ignore))
      (magent-runtime-submit
       runtime-session "hello" :tools '("read_file" bash)))
    (should (equal (magent-request-context-tool-names captured-context)
                   '(read_file bash)))))

(ert-deftest magent-test-runtime-submit-rejects-project-skill-in-global-scope ()
  "Test project-only skills fail before creating a ledger turn."
  (require 'magent-runtime-api)
  (require 'magent-skills)
  (let* ((magent-skills--registry nil)
         (session (magent-session-create :id "global-session"))
         (runtime-session
          (magent-runtime-session-create
           :id "global-session" :scope 'global :magent-session session))
         prepared)
    (magent-skills-register
     (magent-skill-create
      :name "summarize" :type 'instruction :requires-project t))
    (cl-letf (((symbol-function 'magent-runtime-api--prepare-turn)
               (lambda (&rest _args)
                 (setq prepared t)
                 "unexpected-turn")))
      (should-error
       (magent-runtime-submit
        runtime-session "Summarize the workspace" :skills '("summarize"))
       :type 'user-error))
    (should-not prepared)
    (should-not (magent-session-thread session))))

(ert-deftest magent-test-runtime-submit-omits-unset-session-effort ()
  "Test unset runtime session effort leaves agent/default effort available."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-default-effort nil)
         (session (magent-session-create :id "session-1"))
         (runtime-session
          (magent-runtime-session-create
           :id "session-1"
           :scope "/tmp/project"
           :magent-session session))
         captured-context)
    (cl-letf (((symbol-function 'magent-agent-run-turn)
               (lambda (&rest args)
                 (setq captured-context (plist-get args :request-context))
                 'loop))
              ((symbol-function 'magent-runtime-api--finish-submission)
               #'ignore)
              ((symbol-function 'magent-session-save-deferred-for-session)
               #'ignore))
      (magent-runtime-submit runtime-session "hello"))
    (should (magent-request-context-p captured-context))
    (should-not (magent-request-context-effort captured-context))))

(ert-deftest magent-test-runtime-start-activates-submission-project-scope ()
  "Test queued runtime work activates its own project overlay at start time."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (session (magent-session-create :id "session-project"))
         (runtime-session
          (magent-runtime-session-create
           :id "session-project"
           :scope "/tmp/project-runtime"
           :magent-session session))
         activated)
    (cl-letf (((symbol-function 'magent-runtime-activate-scope)
               (lambda (scope &optional _force)
                 (push scope activated)))
              ((symbol-function 'magent-agent-run-turn)
               (lambda (&rest _args) 'loop))
              ((symbol-function 'magent-session-save-deferred-for-session)
               #'ignore))
      (magent-runtime-submit runtime-session "hello"))
    (should (equal activated '("/tmp/project-runtime")))))

(ert-deftest magent-test-runtime-start-activates-action-origin-scope ()
  "Test isolated Action runtime work activates its originating overlay."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (scope (magent-session-action-scope
                 "command-1" "test" "/tmp/project-origin"))
         (session (magent-session-create :id "command-1"))
         (runtime-session
          (magent-runtime-session-create
           :id "command-1" :scope scope :magent-session session))
         activated)
    (cl-letf (((symbol-function 'magent-runtime-activate-scope)
               (lambda (target &optional _force)
                 (push target activated)))
              ((symbol-function 'magent-agent-run-turn)
               (lambda (&rest _args) 'loop))
              ((symbol-function 'magent-session-save-deferred-for-session)
               #'ignore))
      (magent-runtime-submit runtime-session "hello"))
    (should (equal activated '("/tmp/project-origin")))))

(ert-deftest magent-test-runtime-start-error-fails-and-advances-queue ()
  "Test synchronous startup failure terminalizes work and starts the next item."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (dummy (magent-runtime-submission-create
                 :id "dummy" :session-id "dummy"))
         (bad-session (magent-session-create :id "bad-session"))
         (good-session (magent-session-create :id "good-session"))
         (bad-runtime (magent-runtime-session-create
                       :id "bad-session" :scope 'global
                       :magent-session bad-session))
         (good-runtime (magent-runtime-session-create
                        :id "good-session" :scope 'global
                        :magent-session good-session))
         completions started)
    (cl-letf (((symbol-function 'magent-runtime-activate-scope) #'ignore)
              ((symbol-function 'magent-session-save-deferred-for-session)
               #'ignore)
              ((symbol-function 'magent-agent-run-turn)
               (lambda (&rest args)
                 (let ((prompt (plist-get args :prompt)))
                   (push prompt started)
                   (if (equal prompt "bad")
                       (error "startup exploded")
                     'good-loop)))))
      (magent-runtime-queue-submit dummy #'ignore)
      (magent-runtime-submit
       bad-runtime "bad"
       :on-complete (lambda (status _result)
                      (push (cons 'bad status) completions)))
      (magent-runtime-submit
       good-runtime "good"
       :on-complete (lambda (status _result)
                      (push (cons 'good status) completions)))
      (magent-runtime-api--finish-submission dummy 'completed "done"))
    (should (equal (nreverse started) '("bad" "good")))
    (should (equal (cdr (assq 'bad completions)) 'failed))
    (should (equal (magent-runtime-submission-prompt
                    (magent-runtime-queue-active-submission))
                   "good"))
    (let* ((thread (magent-session-thread-ledger bad-session))
           (turn (car (magent-thread-turns thread))))
      (should (eq (magent-thread-turn-status turn) 'failed)))))

(ert-deftest magent-test-runtime-cancel-isolates-queued-completion-errors ()
  "Test one queued completion error cannot prevent cancelling its siblings."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (session (magent-session-create :id "session-cancel"))
         (runtime-session
          (magent-runtime-session-create
           :id "session-cancel" :scope 'global :magent-session session))
         callbacks)
    (magent-runtime-queue-submit
     (magent-runtime-submission-create :id "active" :session-id "other")
     #'ignore)
    (dolist (entry '(("first" . bad) ("second" . good)))
      (let* ((turn (magent-thread-queue-turn
                    (magent-session-thread-ledger session) (car entry)))
             (kind (cdr entry)))
        (magent-runtime-queue-submit
         (magent-runtime-submission-create
          :id (car entry)
          :session runtime-session
          :session-id "session-cancel"
          :turn-id (magent-thread-turn-id turn)
          :on-complete
          (lambda (_status _result)
            (push kind callbacks)
            (when (eq kind 'bad)
              (error "completion exploded"))))
         #'ignore)))
    (cl-letf (((symbol-function 'magent-session-save-deferred-for-session)
               #'ignore))
      (should (= (magent-runtime-cancel runtime-session) 2)))
    (should (equal (sort callbacks
                         (lambda (a b)
                           (string< (symbol-name a) (symbol-name b))))
                   '(bad good)))))

(ert-deftest magent-test-runtime-finish-clears-active-before-completion-callback ()
  "Test backend completion callback failures cannot leave runtime busy."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (events nil)
         (submission
          (magent-runtime-submission-create
           :id "submission-1"
           :session-id "session-1"
           :observer (lambda (event)
                       (push (plist-get event :type) events))
           :on-complete
           (lambda (_status _result)
             (should-not (magent-runtime-queue-active-submission))
             (error "backend callback failed")))))
    (magent-runtime-queue-submit submission #'ignore)
    (magent-runtime-api--finish-submission submission 'completed "ok")
    (should-not (magent-runtime-queue-active-submission))
    (should (equal events '(turn-complete)))))

(ert-deftest magent-test-runtime-list-sessions-does-not-load-full-sessions ()
  "Test ACP session/list avoids replaying full session files."
  (require 'magent-runtime-api)
  (let ((file "/tmp/session-20260705-231500.json"))
    (cl-letf (((symbol-function 'magent-session-list-files)
               (lambda () (list file)))
              ((symbol-function 'magent-session--read-file-metadata)
               (lambda (_file)
                 (list :scope 'global
                       :project-root nil
                       :summary-title "Hello")))
              ((symbol-function 'magent-session--file-display-time)
               (lambda (_file) (seconds-to-time 0)))
              ((symbol-function 'magent-session-read-file)
               (lambda (_file)
                 (error "session/list should not load full sessions"))))
      (should
       (equal (magent-runtime-list-sessions)
              `((:id "session-20260705-231500"
                     :file ,file
                     :scope global
                     :project-root nil
                     :title "Hello"
                     :updated-at 0.0)))))))

(ert-deftest magent-test-runtime-list-sessions-for-scope-uses-scope-api ()
  "Test scoped runtime listing does not enumerate the all-session catalog."
  (require 'magent-runtime-api)
  (let ((file "/tmp/session-20260718-120000.json"))
    (cl-letf (((symbol-function 'magent-session-list-files-for-scope)
               (lambda (scope)
                 (should (equal scope "/project-a"))
                 (list file)))
              ((symbol-function 'magent-session-list-files)
               (lambda () (error "scoped listing should not enumerate all files")))
              ((symbol-function 'magent-session--read-file-metadata-cached)
               (lambda (_file)
                 '(:valid t
                   :id "session-20260718-120000"
                   :scope project
                   :project-root "/project-a"
                   :summary-title "Scoped chat")))
              ((symbol-function 'magent-session--file-display-time)
               (lambda (_file) (seconds-to-time 0))))
      (should
       (equal (magent-runtime-list-sessions-for-scope "/project-a")
              `((:id "session-20260718-120000"
                     :file ,file
                     :scope "/project-a"
                     :project-root "/project-a"
                     :title "Scoped chat"
                     :updated-at 0.0)))))))

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
  "Test capability listing loads project-local capabilities."
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
                    "capability-skills: [project-skill]\n"
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
              (magent-list-capabilities)))
          (with-current-buffer "*Magent Capabilities*"
            (should (string-match-p "project-capability" (buffer-string)))))
      (when (get-buffer "*Magent Capabilities*")
        (kill-buffer "*Magent Capabilities*"))
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

;; ──────────────────────────────────────────────────────────────────────
;;; Codex-like runtime skeleton tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-session-truncates-model-visible-tool-results ()
  "Test oversized tool results are truncated before session prompt reuse."
  (require 'magent-session)
  (let ((magent-session-directory (make-temp-file "magent-spill-test-" t))
        (magent-tool-result-model-max-length 80)
        (magent-tool-result-model-preview-length 40)
        (session (magent-session-create))
        (payload (make-string 200 ?x)))
    (unwind-protect
        (progn
          (magent-test--record-session-entry session 'user "Run tool")
          (magent-test--record-tool-entry
           session "call-1" "emacs_eval" '(:sexp "(big)")
           (magent-test-tool-result payload))
          (let* ((tool-msg (cadr (magent-test--session-transcript session)))
                 (result (plist-get (magent-test--transcript-content tool-msg) :result))
                 (prompt (magent-test--provider-context session))
                 (prompt-tool (cdr (cadr prompt))))
            (should (string-prefix-p (make-string 40 ?x) result))
            (should (string-match-p "Tool result truncated" result))
            (should (string-match-p
                     "Full tool result available as result-" result))
            (should-not (equal result payload))
            (should (equal (plist-get prompt-tool :result) result))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-spilled-tool-output-is-session-scoped-and-paged ()
  "Full oversized output is private to its session and retrievable by page."
  (require 'magent-session)
  (require 'magent-tools)
  (let* ((magent-session-directory (make-temp-file "magent-spill-page-" t))
         (magent-session--current-scope 'global)
         (magent-tool-result-model-max-length 30)
         (magent-tool-result-model-preview-length 20)
         (magent-tool-output-spill-page-characters 10000)
         (session (magent-session-create :id "spill_session.1"))
         (other (magent-session-create :id "other-session"))
         (payload (concat "first\n" (make-string 120 ?z) "\nlast\n"))
         result-id page denied spill-file read-file-page-budget)
    (unwind-protect
        (progn
          (magent-test--record-session-entry session 'user "Run large tool")
          (magent-test--record-tool-entry
           session "call-spill" "bash" '(:command "large")
           (magent-test-tool-result payload))
          (let* ((thread (magent-session-thread-ledger session))
                 (turn (car (magent-thread-turns thread)))
                 (item (cl-find-if
                        (lambda (candidate)
                          (eq (magent-thread-item-type candidate) 'tool))
                        (magent-thread-turn-items turn)))
                 (spill (plist-get (magent-thread-item-metadata item) :spill)))
            (setq result-id (plist-get spill :result-id)
                  spill-file (magent-tool-output-spill-file
                              'global "spill_session.1" result-id)))
          (should (string-prefix-p "result-" result-id))
          (should (= (logand (file-modes spill-file) #o777) #o600))
          (let ((magent-tool-result-model-max-length 1000)
                (magent-tools--request-context
                 (magent-request-context-create
                  :scope 'global :session session)))
            (magent-tools--read-tool-output
             (lambda (value)
               (setq page (magent-test-tool-output value)
                     read-file-page-budget
                     magent-tools--read-file-page-max-characters))
             result-id 1 500))
          (should (= read-file-page-budget
                     magent-tools--read-file-page-max-characters))
          (should (string-match-p (regexp-quote payload) page))
          (let ((magent-tools--request-context
                 (magent-request-context-create
                  :scope 'global :session other)))
            (magent-tools--read-tool-output
             (lambda (value) (setq denied (magent-test-tool-output value)))
             result-id))
          (should (string-match-p "tool_result_not_found" denied)))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-spilled-tool-output-keys-include-exact-scope ()
  "Equal session ids in different scopes cannot read or clean each other."
  (require 'magent-tools)
  (let* ((magent-session-directory (make-temp-file "magent-spill-scope-" t))
         (magent-tool-result-model-max-length 20)
         (magent-tool-result-model-preview-length 10)
         (scope-a "/tmp/magent-project-a")
         (scope-b "/tmp/magent-project-b")
         (thread-a (magent-thread-create
                    :id "thread-a" :session-id "shared" :scope scope-a))
         (thread-b (magent-thread-create
                    :id "thread-b" :session-id "shared" :scope scope-b))
         (turn-a (magent-thread-create-turn thread-a "a"))
         (turn-b (magent-thread-create-turn thread-b "b"))
         (item-a (magent-thread-record-tool-result
                  thread-a (magent-thread-turn-id turn-a) "call-a" "bash" nil
                  (magent-test-tool-result (make-string 80 ?a))))
         (item-b (magent-thread-record-tool-result
                  thread-b (magent-thread-turn-id turn-b) "call-b" "bash" nil
                  (magent-test-tool-result (make-string 80 ?b))))
         (id-a (plist-get
                (plist-get (magent-thread-item-metadata item-a) :spill)
                :result-id))
         (id-b (plist-get
                (plist-get (magent-thread-item-metadata item-b) :spill)
                :result-id)))
    (unwind-protect
        (progn
          (should (file-regular-p
                   (magent-tool-output-spill-file scope-a "shared" id-a)))
          (should (file-regular-p
                   (magent-tool-output-spill-file scope-b "shared" id-b)))
          (should-error
           (magent-tool-output-spill-file scope-b "shared" id-a))
          (magent-tool-output-spill-cleanup scope-a "shared")
          (should (file-regular-p
                   (magent-tool-output-spill-file scope-b "shared" id-b))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-spilled-single-line-output-uses-character-cursors ()
  "A long line can be reconstructed through character continuation."
  (require 'magent-tools)
  (let* ((magent-session-directory (make-temp-file "magent-spill-char-" t))
         (magent-tool-result-model-max-length 120)
         (magent-tool-result-model-preview-length 40)
         (magent-tool-output-spill-page-characters 25)
         (session (magent-session-create :id "char-session"))
         (thread (magent-session-thread-ledger session))
         (turn (magent-thread-create-turn thread "long line"))
         (payload (make-string 250 ?q))
         (item (magent-thread-record-tool-result
                thread (magent-thread-turn-id turn) "char-call" "bash" nil
                (magent-test-tool-result payload) nil 'global))
         (result-id (plist-get
                     (plist-get (magent-thread-item-metadata item) :spill)
                     :result-id))
         (start 1)
         (rebuilt ""))
    (unwind-protect
        (let ((magent-tool-result-model-max-length 1000)
              (magent-tools--request-context
               (magent-request-context-create
                :scope 'global :session session)))
          (while (<= start (length payload))
            (let (page)
              (magent-tools--read-tool-output
               (lambda (value) (setq page (magent-test-tool-output value)))
               result-id start 25)
              (should (string-match
                       "next_start_character=[0-9]+]\n\\|has_more=false]\n"
                       page))
              (let* ((magent-tool-result-model-max-length 10)
                     (projected
                      (magent-thread-project-tool-result-for-model
                       (magent-tool-result-create
                        :name "read_tool_output"
                        :status 'completed :success t :output page)
                       thread 'global)))
                (should (equal (magent-tool-result-output projected) page))
                (should-not (plist-get
                             (magent-tool-result-metadata projected) :spill)))
              (setq rebuilt
                    (concat rebuilt
                            (substring page (1+ (string-match "\n" page)))))
              (setq start (+ start 25))))
          (should (equal rebuilt payload)))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-spill-write-failure-keeps-bounded-tool-result ()
  "Spill I/O errors do not change tool status or escape the ledger boundary."
  (let* ((magent-tool-result-model-max-length 20)
         (magent-tool-result-model-preview-length 10)
         (thread (magent-thread-create :id "spill-failure" :scope 'global))
         (turn (magent-thread-create-turn thread "run"))
         logs
         item)
    (cl-letf (((symbol-function 'magent-tool-output-spill-put)
               (lambda (&rest _args) (error "disk full")))
              ((symbol-function 'magent-log)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) logs))))
      (setq item
            (magent-thread-record-tool-result
             thread (magent-thread-turn-id turn) "spill-call" "bash" nil
             (magent-test-tool-result (make-string 100 ?x)))))
    (should (eq (magent-thread-item-status item) 'completed))
    (should (plist-get (magent-thread-item-metadata item) :spill-error))
    (should (string-match-p
             "Full tool result unavailable: spill storage failed"
             (magent-thread-item-output item)))
    (should (string-match-p "disk full" (car logs)))))

(ert-deftest magent-test-session-keeps-failure-header-outside-body-budget ()
  "Test failure status and diagnostic tail survive a small body preview."
  (require 'magent-session)
  (let* ((magent-tool-result-model-max-length 8)
         (magent-tool-result-model-preview-length 8)
         (magent-tool-output-spill-session-max-bytes 0)
         (thread (magent-thread-create :id "thread-truncated-failure"))
         (turn (magent-thread-create-turn thread "Run failing tool"))
         (result (magent-tool-result-create
                  :status 'failed
                  :success nil
                  :exit-code 23
                  :output "ab0123456789uvwxyz"))
         (item (magent-thread-record-tool-result
                thread (magent-thread-turn-id turn) "call-23" "bash" nil
                result))
         (visible (magent-thread-item-output item)))
    (should (string-prefix-p
             "[Tool result: status=failed; exit-code=23]\nab\n\n"
             visible))
    (should (string-match-p
             "original 18 characters; kept first 2 and last 6; omitted 10"
             visible))
    (should (string-suffix-p "uvwxyz" visible))
    (should-not (string-match-p "0123456789" visible))
    (should (equal "ab0123456789uvwxyz"
                   (magent-tool-result-output result)))))

(ert-deftest magent-test-thread-ledger-turn-and-item-state-machine ()
  "Test explicit thread/turn/item state transitions."
  (require 'magent-ledger)
  (let* ((thread (magent-thread-create :id "thread-1"))
         (turn (magent-thread-create-turn thread "hello"))
         (item (magent-thread-start-item
                thread (magent-thread-turn-id turn) 'tool
                :id "call-1"
                :call-id "call-1"
                :name "grep"
                :input '(:pattern "hello"))))
    (should (eq (magent-thread-status thread) 'active))
    (should (eq (magent-thread-turn-status turn) 'in-progress))
    (should (eq (magent-thread-item-status item) 'in-progress))
    (magent-thread-complete-item thread item :output "match")
    (should (eq (magent-thread-item-status item) 'completed))
    (should (equal (magent-thread-item-output item) "match"))
    (magent-thread-complete-turn thread (magent-thread-turn-id turn))
    (should (eq (magent-thread-turn-status turn) 'completed))
    (should (eq (magent-thread-status thread) 'idle))))

(ert-deftest magent-test-thread-journal-start-events-are-immutable ()
  "Test early journal events do not grow later item output."
  (require 'magent-ledger)
  (let* ((thread (magent-thread-create :id "thread-immutable"))
         (turn (magent-thread-create-turn thread "hello"))
         (item (magent-thread-start-item
                thread (magent-thread-turn-id turn) 'tool
                :id "call-1"
                :call-id "call-1"
                :name "emacs_eval"
                :input '(:sexp "(big)")))
         (started-event (cl-find 'item-started
                                 (magent-thread-journal thread)
                                 :key #'magent-thread-event-type)))
    (magent-thread-complete-item thread item :output (make-string 200 ?x))
    (let* ((payload (magent-thread-event-payload started-event))
           (started-item (magent-thread--event-payload-item payload)))
      (should (magent-thread-item-p started-item))
      (should-not (magent-thread-item-output started-item)))
    (let* ((alist (magent-thread-event-to-alist started-event))
           (item-alist (cdr (assq 'item (cdr (assq 'payload alist))))))
      (should-not (cdr (assq 'output item-alist))))))

(ert-deftest magent-test-thread-ledger-replays-journal-from-snapshot ()
  "Test snapshot plus journal replay materializes latest state."
  (require 'magent-ledger)
  (let* ((thread (magent-thread-create :id "thread-replay"))
         (turn (magent-thread-create-turn thread "hello"))
         (item (magent-thread-start-item
                thread (magent-thread-turn-id turn) 'message
                :role 'assistant
                :content "working"))
         (snapshot (magent-thread-snapshot-to-alist thread)))
    (magent-thread-complete-item thread item :content "done")
    (magent-thread-complete-turn thread (magent-thread-turn-id turn))
    (let* ((events (mapcar #'magent-thread-event-to-alist
                           (cl-remove-if
                            (lambda (event)
                              (<= (magent-thread-event-seq event)
                                  (cdr (assq 'last-event-seq snapshot))))
                            (magent-thread-journal thread))))
           (loaded (magent-thread-replay
                    snapshot
                    (mapcar #'magent-thread-event-from-alist events)))
           (loaded-turn (car (magent-thread-turns loaded)))
           (loaded-item (car (magent-thread-turn-items loaded-turn))))
      (should (eq (magent-thread-status loaded) 'idle))
      (should (eq (magent-thread-turn-status loaded-turn) 'completed))
      (should (eq (magent-thread-item-status loaded-item) 'completed))
      (should (equal (magent-thread-item-content loaded-item) "done")))))

(ert-deftest magent-test-session-save-load-preserves-thread-snapshot-and-journal ()
  "Test session persistence stores and restores ledger snapshot plus journal."
  (require 'magent-session)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (let ((session (magent-session-get)))
            (magent-test--record-session-entry session 'user "hello")
            (magent-test--record-tool-entry
             session "call-1" "grep" '(:pattern "hello")
             (magent-test-tool-result "match"))
            (magent-test--record-session-entry session 'assistant "done")
            (magent-test--save-current-session))
          (let* ((files (magent-test--session-files magent-session-directory))
                 (loaded (magent-session-read-file (car files)))
                 (loaded-session (plist-get loaded :session))
                 (thread (magent-session-thread loaded-session))
                 (turn (car (magent-thread-turns thread)))
                 (tool (cl-find 'tool (magent-thread-turn-items turn)
                                :key #'magent-thread-item-type)))
            (should thread)
            (should (magent-thread-journal thread))
            (should (eq (magent-thread-turn-status turn) 'completed))
            (should (equal (magent-thread-item-call-id tool) "call-1"))
            (should (eq (magent-thread-item-status tool) 'completed))
            (should (equal (magent-thread-item-output tool) "match"))
            (should (equal (mapcar #'magent-test--transcript-role
                                   (magent-test--session-transcript loaded-session))
                           '(user tool assistant)))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-session-atomic-write-preserves-old-file-on-rename-error ()
  "Test a failed atomic replacement leaves the previous session readable."
  (require 'magent-session)
  (let* ((directory (make-temp-file "magent-session-atomic-" t))
         (file (expand-file-name "session.json" directory)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "{\"old\":true}"))
          (cl-letf (((symbol-function 'rename-file)
                     (lambda (&rest _args) (error "rename failed"))))
            (should-error
             (magent-session--write-json-atomic file '((new . t)))))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "{\"old\":true}"))
          (should-not
           (directory-files directory nil "\\`\\.magent-session-")))
      (delete-directory directory t))))

(ert-deftest magent-test-session-save-bounds-persisted-journal-tail ()
  "Test snapshots persist only the configured recent journal tail."
  (require 'magent-session)
  (let* ((magent-session-directory (make-temp-file "magent-journal-tail-" t))
         (magent-session-journal-max-events 3)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (let ((session (magent-session-get)))
            (dotimes (index 8)
              (magent-test--record-session-entry
               session 'user (format "message-%d" index)))
            (should (> (length (magent-thread-journal
                                (magent-session-thread-ledger session)))
                       magent-session-journal-max-events))
            (magent-test--save-current-session))
          (let* ((file (car (magent-test--session-files
                             magent-session-directory)))
                 (json-object-type 'alist)
                 (json-array-type 'list)
                 (data (with-temp-buffer
                         (insert-file-contents file)
                         (json-read)))
                 (journal (cdr (assq 'journal data)))
                 (loaded (magent-session-read-file file)))
            (should (= (length journal) 3))
            (should (= (length (magent-test--session-transcript
                                (plist-get loaded :session)))
                       8))))
      (delete-directory magent-session-directory t))))

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
      (lambda (tool-spec cb arg-values _resource-identity)
        (setq ran t)
        (funcall cb (apply (gptel-tool-function tool-spec) arg-values)))
      :file-arg-index-function (lambda (_args-spec) 0)
      :args-to-plist-function (lambda (_args-spec arg-values) arg-values)
      :summarize-function (lambda (arg-values _args-spec) (car arg-values)))
     (list (list tool (list ".env" "SECRET=1")
                 (lambda (value) (setq result value)))))
    (should-not ran)
    (should (string-match-p "access denied" result))))

(ert-deftest magent-test-tool-orchestrator-freezes-approved-canonical-path ()
  "Approval and execution share one canonical path despite symlink changes."
  (require 'magent-tool-orchestrator)
  (let* ((root (make-temp-file "magent-tool-identity-" t))
         (target-a (expand-file-name "a" root))
         (target-b (expand-file-name "b" root))
         (link (expand-file-name "current" root))
         (context (magent-request-context-create
                   :scope root :project-root root
                   :session (magent-session-create)))
         (tool (gptel-make-tool
                :name "write_file"
                :args (list '(:name "path" :type string)
                            '(:name "content" :type string))
                :function #'ignore
                :async t))
         approval-callback
         approval-request
         executed-path)
    (unwind-protect
        (progn
          (make-directory target-a)
          (make-directory target-b)
          (make-symbolic-link target-a link)
          (cl-letf (((symbol-function 'magent-approval-request)
                     (lambda (request callback)
                       (setq approval-request request
                             approval-callback callback)
                       "approval-1")))
            (magent-tool-orchestrator-handle-tool-calls
             (magent-tool-orchestrator-create
              :permission '((write . ask))
              :request-context context
              :run-tool-function
              (lambda (_tool callback arg-values _resource-identity)
                (setq executed-path (car arg-values))
                (funcall callback (magent-test-tool-result "ok")))
              :file-arg-index-function (lambda (_args-spec) 0)
              :args-to-plist-function
              (lambda (_args-spec arg-values)
                (list :path (car arg-values)
                      :content (cadr arg-values)))
              :summarize-function
              (lambda (arg-values _args-spec) (car arg-values)))
             (list (list tool (list "current/file.txt" "data") nil))))
          (let ((approved-path
                 (plist-get (plist-get approval-request :args) :path)))
            (delete-file link)
            (make-symbolic-link target-b link)
            (funcall approval-callback 'allow-once)
            (should (equal executed-path approved-path))
            (should (equal executed-path
                           (file-truename
                            (expand-file-name "file.txt" target-a))))))
      (delete-directory root t))))

(ert-deftest magent-test-tool-queue-rejects-changed-canonical-identity ()
  "A queued file tool fails when its frozen target becomes a symlink."
  (require 'magent-agent-loop)
  (let* ((project (make-temp-file "magent-identity-project-" t))
         (outside (make-temp-file "magent-identity-outside-"))
         (context (magent-request-context-create
                   :scope project :project-root project
                   :session (magent-session-create)))
         (loop (magent-agent-loop-create :request-context context))
         (queue (magent-agent-loop-tool-queue loop))
         (tool (gptel-make-tool
                :name "write_file"
                :args (list '(:name "path" :type string)
                            '(:name "content" :type string))
                :function #'magent-tools--write-file
                :async t))
         result)
    (unwind-protect
        (progn
          (with-temp-file outside (insert "before"))
          ;; Hold the actual executor busy so orchestration freezes and queues
          ;; the resource before the filesystem identity changes.
          (setf (magent-agent-loop-tool-queue-busy queue) t)
          (magent-tool-orchestrator-handle-tool-calls
           (magent-agent-loop-create-orchestrator
            loop
            '((write . (("new" . allow) (* . deny))))
            context)
           (list (list tool (list "new" "after")
                       (lambda (value) (setq result value)))))
          (should (= (length (magent-agent-loop-tool-queue-items queue)) 1))
          (make-symbolic-link outside (expand-file-name "new" project))
          (setf (magent-agent-loop-tool-queue-busy queue) nil)
          (magent-agent-loop-tool-queue-run queue)
          (should (string-match-p "resource identity changed" result))
          (should (equal (with-temp-buffer
                           (insert-file-contents outside)
                           (buffer-string))
                         "before")))
      (delete-directory project t)
      (delete-file outside))))

(ert-deftest magent-test-tool-orchestrator-canonicalization-fails-closed ()
  "Unresolved variables and canonicalization errors are denied before run."
  (require 'magent-tool-orchestrator)
  (let* ((project (make-temp-file "magent-canonical-project-" t))
         (loop-link (expand-file-name "loop" project))
         (context (magent-request-context-create
                   :scope project :project-root project
                   :session (magent-session-create)))
         (tool (gptel-make-tool
                :name "read_file"
                :args (list '(:name "path" :type string))
                :function #'ignore
                :async nil))
         (variable "MAGENT_TEST_UNRESOLVED_RESOURCE_9182")
         ran
         results)
    (unwind-protect
        (progn
          (setenv variable nil)
          (make-symbolic-link "loop" loop-link)
          (dolist (path (list (format "$%s/file" variable) "loop/file"))
            (magent-tool-orchestrator-handle-tool-calls
             (magent-tool-orchestrator-create
              :permission '((read . allow))
              :request-context context
              :run-tool-function (lambda (&rest _args) (setq ran t))
              :file-arg-index-function (lambda (_args-spec) 0))
             (list (list tool (list path)
                         (lambda (value) (push value results))))))
          (should-not ran)
          (should (= (length results) 2))
          (should (cl-every
                   (lambda (result)
                     (string-match-p "invalid or unstable resource path"
                                     result))
                   results)))
      (setenv variable nil)
      (delete-directory project t))))

(ert-deftest magent-test-session-save-load-sanitizes-ledger-items ()
  "Test canonical ledger items persist with JSON-safe values."
  (require 'magent-protocol)
  (require 'magent-ledger)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (let ((session (magent-session-get)))
            (magent-test--record-session-entry session 'user "Run tool")
            (let ((thread (magent-session-thread-ledger session))
                  (turn-id (magent-thread-turn-id
                            (magent-thread-active-turn
                             (magent-session-thread-ledger session)))))
              (magent-thread-record-tool-result
               thread
               turn-id
               "call-1"
               'emacs_eval
               '(:tool emacs_eval :values [emacs_eval nil])
               (magent-test-tool-result "ok")
               '(:provider gptel :tool emacs_eval)))
            (magent-test--save-current-session))
          (let* ((files (magent-test--session-files magent-session-directory))
                 (loaded (magent-session-read-file (car files)))
                 (loaded-session (plist-get loaded :session))
                 (item (cl-find 'tool
                                (magent-thread-all-items
                                 (magent-session-thread-ledger loaded-session))
                                :key #'magent-thread-item-type)))
            (should (equal (cdr (assq 'tool (magent-thread-item-input item)))
                           "emacs_eval"))
            (should (equal (cdr (assq 'values (magent-thread-item-input item)))
                           '("emacs_eval" nil)))
            (should (equal (magent-thread-item-name item) "emacs_eval"))
            (should (equal (magent-thread-item-output item) "ok"))
            (should (equal (cdr (assq 'provider (magent-thread-item-metadata item)))
                           "gptel"))
            (should (equal (cdr (assq 'tool (magent-thread-item-metadata item)))
                           "emacs_eval"))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-session-save-load-sanitizes-agent-job-transcript-and-metadata ()
  "Test child-agent job persistence sanitizes transcript and metadata."
  (require 'magent-agent-job)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (let ((session (magent-session-get)))
            (magent-test--record-session-entry session 'user "spawn child")
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
              :transcript '(((role . assistant)
                             (content . (:tool emacs_eval :values [emacs_eval nil]))))
              :result 'ok
              :error nil
              :metadata '((permission-profile . (agent bash emacs_eval))
                          (model . gpt-4o-mini)))))
          (magent-test--save-current-session))
      (let* ((files (magent-test--session-files magent-session-directory))
             (loaded (magent-session-read-file (car files)))
             (loaded-session (plist-get loaded :session))
             (job (magent-session-agent-job loaded-session "agent-1")))
        (should (equal (magent-agent-job-result job) "ok"))
        (let ((entry (car (magent-agent-job-transcript job))))
          (should (equal (cdr (assq 'role entry)) "assistant"))
          (let ((content (cdr (assq 'content entry))))
            (should (equal (cdr (assq 'tool content)) "emacs_eval"))
            (should (equal (cdr (assq 'values content))
                           '("emacs_eval" nil)))))
        (let ((metadata (magent-agent-job-metadata job)))
          (should (equal (cdr (assq 'permission-profile metadata))
                         '("agent" "bash" "emacs_eval")))
          (should (equal (cdr (assq 'model metadata))
                         "gpt-4o-mini")))))
    (delete-directory magent-session-directory t)))

(ert-deftest magent-test-session-read-file-decodes-snapshot-once ()
  "A full session load reuses the validated decoded snapshot."
  (let* ((directory (make-temp-file "magent-session-decode-" t))
         (file (expand-file-name "single-decode.json" directory))
         (original-decoder
          (symbol-function 'magent-thread-snapshot-from-alist))
         (decode-count 0))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             (json-encode
              (magent-test--current-session-json-data "single-decode"))))
          (cl-letf (((symbol-function 'magent-thread-snapshot-from-alist)
                     (lambda (snapshot)
                       (cl-incf decode-count)
                       (funcall original-decoder snapshot))))
            (should (magent-session-read-file file)))
          (should (= decode-count 1)))
      (delete-directory directory t))))

(ert-deftest magent-test-session-explicit-save-does-not-use-ambient-state ()
  "Explicit and deferred saves retain the captured session/scope pair."
  (let* ((target (magent-session-create :id "target"))
         (ambient (magent-session-create :id "ambient"))
         (magent--current-session ambient)
         (magent-session--current-scope 'global)
         (magent-session--pending-saves nil)
         (magent-session--save-timer nil)
         scheduled saved)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (_delay _repeat fn)
                 (setq scheduled fn)
                 'save-timer))
              ((symbol-function 'magent-session-save-for-session)
               (lambda (session scope)
                 (setq saved (list session scope)))))
      (magent-session-save-deferred-for-session
       target "/tmp/explicit-project" 0.1)
      (funcall scheduled)
      (should (eq (car saved) target))
      (should (equal (cadr saved) "/tmp/explicit-project"))
      (should (eq magent--current-session ambient))
      (should (eq magent-session--current-scope 'global)))))

(ert-deftest magent-test-session-deferred-saves-coalesce-per-session-and-scope ()
  "One idle timer saves each captured session/scope pair at most once."
  (let* ((first (magent-session-create :id "first"))
         (second (magent-session-create :id "second"))
         (magent-session--pending-saves nil)
         (magent-session--save-timer nil)
         scheduled
         (timer-count 0)
         saved)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (_delay _repeat fn)
                 (cl-incf timer-count)
                 (setq scheduled fn)
                 'save-timer))
              ((symbol-function 'magent-session-save-for-session)
               (lambda (session scope)
                 (setq saved (append saved (list (list session scope)))))))
      (magent-session-save-deferred-for-session first "/a")
      (magent-session-save-deferred-for-session first "/a")
      (magent-session-save-deferred-for-session second "/b")
      (should (= timer-count 1))
      (should (= (length magent-session--pending-saves) 2))
      (funcall scheduled)
      (should (equal saved (list (list first "/a")
                                 (list second "/b"))))
      (should-not magent-session--pending-saves)
      (should-not magent-session--save-timer))))

(ert-deftest magent-test-session-install-reconciles-and-persists-restart-state ()
  "Installing persisted work terminalizes non-durable turns, items, and jobs."
  (let* ((magent-session-directory (make-temp-file "magent-recovery-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (session (magent-session-create :id "recovery"))
         (thread (magent-session-thread-ledger session))
         (turn (magent-thread-create-turn thread "running"))
         (item (magent-thread-start-item
                thread (magent-thread-turn-id turn) 'reasoning
                :content "partial"))
         (queued (magent-thread-queue-turn thread "queued"))
         (job (magent-agent-job-create
               :id "child" :status 'waiting :prompt "work")))
    (unwind-protect
        (progn
          (magent-thread-record-user-message-if-needed
           thread (magent-thread-turn-id turn) "running")
          (magent-session-add-agent-job session job)
          (magent-session-save-for-session session 'global)
          (let* ((file (car (magent-test--session-files
                             magent-session-directory)))
                 (loaded (magent-session-read-file file))
                 (restored (plist-get loaded :session)))
            (magent-session-install 'global restored)
            (let* ((restored-thread (magent-session-thread restored))
                   (restored-turn
                    (magent-thread-find-turn
                     restored-thread (magent-thread-turn-id turn)))
                   (restored-item
                    (cl-find (magent-thread-item-id item)
                             (magent-thread-turn-items restored-turn)
                             :key #'magent-thread-item-id :test #'equal))
                   (restored-queued
                    (magent-thread-find-turn
                     restored-thread (magent-thread-turn-id queued)))
                   (restored-job
                    (magent-session-agent-job restored "child")))
              (should (eq (magent-thread-status restored-thread) 'idle))
              (should (eq (magent-thread-turn-status restored-turn)
                          'interrupted))
              (should (eq (magent-thread-item-status restored-item)
                          'cancelled))
              (should (eq (magent-thread-turn-status restored-queued)
                          'dropped))
              (should (eq (magent-agent-job-status restored-job) 'cancelled))
              (should (string-match-p
                       "Emacs restart" (magent-agent-job-error restored-job))))
            ;; Recovery is atomically persisted, so a fresh read is terminal.
            (let* ((again (plist-get (magent-session-read-file file) :session))
                   (again-turn (car (magent-thread-turns
                                     (magent-session-thread again)))))
              (should (memq (magent-thread-turn-status again-turn)
                            '(interrupted dropped)))
              (should (eq (magent-agent-job-status
                           (magent-session-agent-job again "child"))
                          'cancelled)))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-session-schema-version-requires-exact-current-format ()
  "Unversioned and non-current session schemas fail clearly."
  (let* ((directory (make-temp-file "magent-schema-" t))
         (magent-session-directory directory)
         (unversioned (expand-file-name "unversioned.json" directory))
         (future (expand-file-name "future.json" directory))
         logs)
    (unwind-protect
        (progn
          (with-temp-file unversioned
            (insert (json-encode
                     (assq-delete-all
                      'schema-version
                      (magent-test--current-session-json-data
                       "unversioned")))))
          (with-temp-file future
            (insert (json-encode
                     (magent-test--current-session-json-data
                      "future" (1+ magent-session-schema-version)))))
          (cl-letf (((symbol-function 'magent-log)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) logs))))
            (should-not (magent-session-read-file unversioned))
            (should-not (magent-session-read-file future)))
          (should (cl-some (lambda (line)
                             (string-match-p
                              "Unsupported session schema version" line))
                           logs)))
      (delete-directory directory t))))

(ert-deftest magent-test-session-schema-rejects-redundant-messages-field ()
  "Current session files reject a second persisted conversation projection."
  (let* ((directory (make-temp-file "magent-schema-fields-" t))
         (magent-session-directory directory)
         (file (expand-file-name "redundant.json" directory))
         (data (append (magent-test--current-session-json-data "redundant")
                       '((messages . [])))))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (json-encode data)))
          (cl-letf (((symbol-function 'magent-log) #'ignore))
            (should-not (magent-session-read-file file))))
      (delete-directory directory t))))

(ert-deftest magent-test-ledger-rejects-flat-journal-payload ()
  "Journal replay accepts only the current nested event payload schema."
  (let* ((thread (magent-thread-create :id "thread" :session-id "session"))
         (_turn (magent-thread-queue-turn thread "hello"))
         (event (magent-thread-event-to-alist
                 (car (magent-thread-journal thread)))))
    (setcdr (assq 'payload event) '((input . "hello")))
    (should-error (magent-thread-event-from-alist event))))

(ert-deftest magent-test-memory-async-commit-preserves-latest-user-notes ()
  "Memory completion re-reads notes edited while the provider is sampling."
  (require 'magent-action-builtin-memory)
  (let* ((directory (make-temp-file "magent-memory-stale-" t))
         (magent-memory-directory directory)
         (magent-memory-open-after-write nil)
         (magent-memory-use-llm t)
         (magent-memory--operation-generation 0)
         (magent-memory--active-operation nil)
         callback)
    (unwind-protect
        (progn
          (with-temp-file (magent-memory-file)
            (insert "* Magent Managed Profile\n** Overview\nold\n\n* User Notes\nold note\n"))
          (cl-letf (((symbol-function 'magent-memory--build-source-bundle)
                     (lambda (_plan) "bundle"))
                    ((symbol-function 'magent-memory--summarize-with-llm)
                     (lambda (_plan _bundle fn)
                       (setq callback fn)
                       nil)))
            (magent-memory--write-from-plan
             'refresh (magent-memory--empty-plan) nil nil)
            (with-temp-file (magent-memory-file)
              (insert "* Magent Managed Profile\n** Overview\nold\n\n* User Notes\nlatest note\n"))
            (funcall callback
                     "* Magent Managed Profile\n** Overview\nnew\n"))
          (with-temp-buffer
            (insert-file-contents (magent-memory-file))
            (should (string-match-p "latest note" (buffer-string)))
            (should-not (string-match-p "old note" (buffer-string)))))
      (delete-directory directory t))))

(ert-deftest magent-test-memory-deleted-source-marks-profile-stale ()
  "A source recorded by memory generation is stale when later deleted."
  (require 'magent-action-builtin-memory)
  (let* ((directory (make-temp-file "magent-memory-source-missing-" t))
         (magent-memory-directory directory)
         (missing (expand-file-name "deleted.el" directory)))
    (unwind-protect
        (progn
          (with-temp-file (magent-memory-file) (insert "memory"))
          (cl-letf (((symbol-function 'magent-memory--metadata)
                     (lambda ()
                       '(("active" . "true")
                         ("generated-at-float" . "100"))))
                    ((symbol-function 'magent-memory--metadata-json-list)
                     (lambda (_metadata key)
                       (pcase key
                         ("roots-json" '("/root"))
                         ("source-files-json" (list missing)))))
                    ((symbol-function 'magent-memory-discover-roots)
                     (lambda () '("/root"))))
            (let ((status (magent-memory-stale-status)))
              (should (plist-get status :stale))
              (should (member (format "source missing: %s" missing)
                              (plist-get status :reasons))))))
      (delete-directory directory t))))

(ert-deftest magent-test-memory-new-generation-cancels-stale-request ()
  "A newer memory operation aborts and terminalizes the older generation."
  (require 'magent-action-builtin-memory)
  (let ((magent-memory-use-llm t)
        (magent-memory--operation-generation 0)
        (magent-memory--active-operation nil)
        callbacks aborted completions)
    (cl-letf (((symbol-function 'magent-memory--build-source-bundle)
               (lambda (_plan) "bundle"))
              ((symbol-function 'magent-memory--summarize-with-llm)
               (lambda (_plan _bundle fn)
                 (push fn callbacks)
                 (generate-new-buffer " *magent-memory-test*")))
              ((symbol-function 'magent-memory--abort-handle)
               (lambda (handle)
                 (push handle aborted)
                 (when (buffer-live-p handle) (kill-buffer handle))))
              ((symbol-function 'magent-memory--write-profile)
               (lambda (&rest _args) (list :file "/tmp/memory.org"))))
      (magent-memory--write-from-plan
       'refresh (magent-memory--empty-plan) nil
       (lambda (status _message) (push status completions)))
      (magent-memory--write-from-plan
       'refresh (magent-memory--empty-plan) nil
       (lambda (status _message) (push status completions)))
      (should (= (length aborted) 1))
      (should (memq 'cancelled completions))
      ;; The first callback is now stale and cannot complete or write.
      (funcall (cadr callbacks) "* Magent Managed Profile\n")
      (should-not (memq 'completed completions))
      (funcall (car callbacks) "* Magent Managed Profile\n")
      (should (memq 'completed completions)))))

(ert-deftest magent-test-memory-supersession-callback-cannot-clobber-newest-operation ()
  "A cancelled operation callback may reenter without leaking the middle run."
  (require 'magent-action-builtin-memory)
  (let ((magent-memory--operation-generation 0)
        (magent-memory--active-operation nil)
        newest
        middle-confirmed
        aborted
        completions)
    (cl-letf (((symbol-function 'magent-memory--abort-handle)
               (lambda (handle)
                 (when handle (push handle aborted)))))
      (let ((old
             (magent-memory--begin-operation
              'old
              (lambda (status _message)
                (push (list 'old status) completions)
                (setq newest
                      (magent-memory--begin-operation 'newest nil))))))
        (setf (magent-memory-operation-handle old) 'old-handle)
        (let ((middle
               (magent-memory-run
                'clear
                :confirm-fn
                (lambda (_plan _continue) (setq middle-confirmed t))
                :on-complete
                (lambda (status _message)
                  (push (list 'middle status) completions)))))
          (should (magent-memory-operation-completed-p old))
          (should (magent-memory-operation-completed-p middle))
          (should-not middle-confirmed)
          (should (eq magent-memory--active-operation newest))
          (should (magent-memory--operation-current-p newest))
          (should-not (magent-memory--operation-current-p middle))
          (should (equal aborted '(old-handle)))
          (should (member '(old cancelled) completions))
          (should (member '(middle cancelled) completions)))))))

(ert-deftest magent-test-doctor-zero-timeout-disables-local-deadline ()
  "A zero probe timeout runs without installing a local timeout."
  (require 'magent-action-builtin-doctor)
  (let* ((called nil)
         (probe (magent-doctor-probe-create
                 :id "zero"
                 :timeout 0
                 :collector (lambda (_context _state)
                              (setq called t)
                              '((ok . t)))))
         (state (magent-doctor-state-create :deadline nil)))
    (cl-letf (((symbol-function 'magent-action-progress) #'ignore))
      (let ((result (magent-doctor--run-probe probe nil state)))
        (should called)
        (should (equal (cdr (assq 'status result)) "completed"))))))

(ert-deftest magent-test-doctor-project-probe-does-not-freeze-process-timeout ()
  "The project probe leaves subprocess timeout policy to its collector."
  (require 'magent-action-builtin-doctor)
  (let ((probe (gethash "project" magent-doctor--registry)))
    (should probe)
    (should (= (magent-doctor-probe-timeout probe) 0))))

(ert-deftest magent-test-doctor-process-rejects-remote-directory ()
  "Doctor process probes fail closed instead of running through TRAMP."
  (require 'magent-action-builtin-doctor)
  (let ((state (magent-doctor-state-create
                :project-root "/ssh:test.invalid:/srv/project/")))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _args)
                 (ert-fail "Doctor started a remote process"))))
      (should-error
       (magent-doctor-run-process state "true" nil)
       :type 'error))))

(ert-deftest magent-test-memory-stale-clear-confirmation-cannot-write ()
  "A delayed clear approval cannot write after a newer operation supersedes it."
  (require 'magent-action-builtin-memory)
  (let ((magent-memory--operation-generation 0)
        (magent-memory--active-operation nil)
        old-continue new-continue (writes 0))
    (cl-letf (((symbol-function 'magent-memory--write-profile)
               (lambda (&rest _args)
                 (cl-incf writes)
                 (list :file "/tmp/memory.org"))))
      (magent-memory-run
       'clear :confirm-fn
       (lambda (_plan continue) (setq old-continue continue)))
      (magent-memory-run
       'clear :confirm-fn
       (lambda (_plan continue) (setq new-continue continue)))
      (funcall old-continue t)
      (should (= writes 0))
      (funcall new-continue t)
      (should (= writes 1)))))

(ert-deftest magent-test-permission-child-intersection-preserves-resource-rules ()
  "Child permission intersections keep both profiles' resource rules."
  (require 'magent-tools)
  (let* ((root (make-temp-file "magent-permission-root-" t))
         (other-root (make-temp-file "magent-other-root-" t))
         (parent '((edit . ((".magent/plan/*.md" . allow)
                            ("*.env" . deny)
                            (* . deny)))
                   (bash . deny)
                   (* . allow)))
         (child '((edit . ((".magent/plan/*.md" . allow)
                           (* . deny)))
                  (* . allow)))
         (context (magent-request-context-create
                   :project-root root
                   :permission-profile parent))
         (agent (magent-agent-info-create
                 :name "child"
                 :permission child))
         (effective (magent-tools--effective-child-permission context agent))
         (inside (expand-file-name ".magent/plan/work.md" root))
         (outside (expand-file-name ".magent/plan/work.md" other-root)))
    (unwind-protect
        (progn
          (should (magent-permission-intersection-p effective))
          (should (eq (magent-permission-resolve
                       effective 'edit inside root)
                      'allow))
          (should (eq (magent-permission-resolve
                       effective 'edit (expand-file-name ".env" root) root)
                      'deny))
          (should (eq (magent-permission-resolve
                       effective 'edit outside root)
                      'deny))
          (should (eq (magent-permission-resolve effective 'bash) 'deny)))
      (delete-directory root t)
      (delete-directory other-root t))))

(ert-deftest magent-test-tools-filter-explicit-permission-profile ()
  "Explicit effective permission profiles drive tool exposure."
  (require 'magent-tools)
  (let* ((magent-enable-tools magent-permission-keys)
         (profile (magent-permission-intersect
                   '((edit . (("src/*.el" . allow) (* . deny)))
                     (bash . deny)
                     (* . allow))
                   '((* . allow))))
         (tools (magent-tools-get-gptel-tools-for-permission profile :all))
         (names (mapcar #'gptel-tool-name tools)))
    (should (member "edit_file" names))
    (should-not (member "bash" names))))

(ert-deftest magent-test-tools-hide-empty-resource-permission-intersection ()
  "Do not expose a tool when intersected resource allowlists are disjoint."
  (require 'magent-tools)
  (let* ((magent-enable-tools magent-permission-keys)
         (disjoint
          (magent-permission-intersect
           '((edit . (("src/*.el" . allow) (* . deny))))
           '((edit . (("docs/*.md" . allow) (* . deny))))))
         (overlapping
          (magent-permission-intersect
           '((edit . (("src/*.el" . allow) (* . deny))))
           '((edit . (("src/**" . ask) (* . deny))))))
         (disjoint-names
          (mapcar #'gptel-tool-name
                  (magent-tools-get-gptel-tools-for-permission disjoint :all)))
         (overlapping-names
          (mapcar #'gptel-tool-name
                  (magent-tools-get-gptel-tools-for-permission
                   overlapping :all))))
    (should-not (magent-permission-tool-available-p disjoint 'edit))
    (should-not (member "edit_file" disjoint-names))
    (should (magent-permission-tool-available-p overlapping 'edit))
    (should (member "edit_file" overlapping-names))))

(ert-deftest magent-test-tools-expose-nontrivial-resource-glob-intersection ()
  "Exposure has no witness-heuristic false negative for overlapping globs."
  (require 'magent-tools)
  (let* ((magent-enable-tools magent-permission-keys)
         (permission
          (magent-permission-intersect
           '((edit . (("src/*a.el" . allow) (* . deny))))
           '((edit . (("src/a*.el" . allow) (* . deny))))))
         (names
          (mapcar #'gptel-tool-name
                  (magent-tools-get-gptel-tools-for-permission
                   permission :all))))
    (should (eq (magent-permission-resolve
                 permission 'edit "src/aa.el")
                'allow))
    (should (magent-permission-tool-available-p permission 'edit))
    (should (member "edit_file" names))))

(ert-deftest magent-test-tool-orchestrator-session-allow-cannot-override-deny ()
  "A session allow only resolves asks and never relaxes resource denies."
  (require 'magent-tool-orchestrator)
  (let* ((root (make-temp-file "magent-permission-root-" t))
         (session (magent-session-create))
         (context (magent-request-context-create
                   :scope root
                   :project-root root
                   :session session
                   :approval-session session))
         (tool (gptel-make-tool
                :name "write_file"
                :args (list '(:name "path" :type string)
                            '(:name "content" :type string))
                :function (lambda (&rest _args) "ok")
                :async nil))
         (permission '((write . (("*.env" . deny) (* . ask)))))
         results
         ran)
    (unwind-protect
        (progn
          (magent-permission-set-session-override 'write 'allow session)
          (dolist (path (list ".env" (expand-file-name ".env" root)))
            (magent-tool-orchestrator-handle-tool-calls
             (magent-tool-orchestrator-create
              :permission permission
              :request-context context
              :run-tool-function (lambda (&rest _args) (setq ran t))
              :file-arg-index-function (lambda (_args-spec) 0))
             (list (list tool (list path "SECRET=1")
                         (lambda (value) (push value results))))))
          (should-not ran)
          (should (= (length results) 2))
          (should (cl-every (lambda (result)
                              (string-match-p "access denied" result))
                            results)))
      (delete-directory root t))))

(ert-deftest magent-test-tool-orchestrator-canonical-project-resource ()
  "Absolute and relative project paths resolve against the same policy rule."
  (require 'magent-tool-orchestrator)
  (let* ((root (make-temp-file "magent-permission-root-" t))
         (context (magent-request-context-create
                   :scope root :project-root root))
         (tool (gptel-make-tool
                :name "edit_file"
                :args (list '(:name "path" :type string))
                :function #'identity
                :async nil))
         (permission '((edit . ((".magent/plan/*.md" . allow)
                                (* . deny)))))
         (ran nil))
    (unwind-protect
        (dolist (path (list ".magent/plan/work.md"
                            (expand-file-name ".magent/plan/work.md" root)))
          (magent-tool-orchestrator-handle-tool-calls
           (magent-tool-orchestrator-create
            :permission permission
            :request-context context
            :run-tool-function
            (lambda (_tool cb args _resource-identity)
              (push (car args) ran)
              (funcall cb (magent-test-tool-result "ok")))
            :file-arg-index-function (lambda (_args-spec) 0))
           (list (list tool (list path) #'ignore))))
      (delete-directory root t))
    (should (= (length ran) 2))))

(ert-deftest magent-test-canonical-resource-resolves-symlink-ancestors ()
  "Canonical resource identity follows symlink ancestors before policy checks."
  (require 'magent-tools)
  (let* ((root (make-temp-file "magent-resource-root-" t))
         (outside (make-temp-file "magent-resource-outside-" t))
         (link-parent (expand-file-name ".magent" root))
         (link (expand-file-name "plan" link-parent))
         (permission '((edit . ((".magent/plan/*.md" . allow)
                                (* . deny))))))
    (unwind-protect
        (progn
          (make-directory link-parent t)
          (make-symbolic-link outside link)
          (let ((resource (magent-tools-canonical-resource-path
                           ".magent/plan/work.md" root)))
            (should (string-prefix-p
                     (file-name-as-directory (file-truename outside))
                                     resource))
            (should (eq (magent-permission-resolve
                         permission 'edit resource root)
                        'deny))))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest magent-test-tools-edit-file-rejects-empty-old-text ()
  "edit_file rejects empty old_text without modifying the file."
  (require 'magent-tools)
  (let ((file (make-temp-file "magent-edit-empty-"))
        result)
    (unwind-protect
        (progn
          (with-temp-file file (insert "unchanged"))
          (magent-tools--edit-file
           (lambda (value)
             (setq result (magent-test-tool-output value)))
           file "" "replacement" (magent-tools--file-revision file))
          (should (string-match-p "non-empty" result))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "unchanged")))
      (delete-file file))))

(ert-deftest magent-test-tools-grep-single-file-and-global-limit ()
  "grep accepts one file and applies its result limit globally."
  (require 'magent-tools)
  (skip-unless (executable-find magent-grep-program))
  (let* ((root (make-temp-file "magent-grep-root-" t))
         (single (expand-file-name "single.txt" root))
         (other (expand-file-name "other.txt" root))
         (magent-grep-max-matches 2)
         single-result
         global-result)
    (unwind-protect
        (progn
          (with-temp-file single
            (insert "needle one\nneedle two\nneedle three\n"))
          (with-temp-file other
            (insert "needle four\nneedle five\nneedle six\n"))
          (magent-tools--grep
           (lambda (value) (setq single-result value)) "needle" single t)
          (let ((deadline (+ (float-time) 5)))
            (while (and (null single-result) (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (magent-tool-result-success-p single-result))
          (should (equal (plist-get
                          (magent-tool-result-metadata single-result)
                          :backend)
                         "ripgrep"))
          (let* ((output (magent-tool-result-output-string single-result))
                 (revisions (plist-get
                             (magent-tool-result-metadata single-result)
                             :revisions)))
            (should (string-match-p "needle" output))
            (should (string-prefix-p "[file revisions]\n" output))
            (should (= (length revisions) 1))
            (should (string-match-p
                     (regexp-quote (magent-tools--file-revision single))
                     output)))
          (magent-tools--grep
           (lambda (value) (setq global-result value)) "needle" root t)
          (let ((deadline (+ (float-time) 5)))
            (while (and (null global-result) (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (magent-tool-result-success-p global-result))
          (should (plist-get (magent-tool-result-metadata global-result)
                             :truncated))
          (let ((output (magent-tool-result-output-string global-result))
                (count 0)
                (start 0))
            (while (string-match "needle" output start)
              (setq count (1+ count)
                    start (match-end 0)))
            (should (= count 2))))
      (delete-directory root t))))

(ert-deftest magent-test-tools-grep-backend-selection-is-ordered ()
  "Search backend selection prefers ripgrep and falls back only to git."
  (require 'magent-tools)
  (let (available calls)
    (cl-letf (((symbol-function 'magent-tools--project-executable)
               (lambda (program _directory)
                 (push program calls)
                 (and (member program available) (concat "/bin/" program)))))
      (setq available (list magent-grep-program "git")
            calls nil)
      (should (equal (magent-tools--grep-backend "/tmp/")
                     `(:name ripgrep :program ,(concat "/bin/"
                                                       magent-grep-program))))
      (should (equal (nreverse calls) (list magent-grep-program)))
      (setq available '("git")
            calls nil)
      (should (equal (magent-tools--grep-backend "/tmp/")
                     '(:name git-grep :program "/bin/git")))
      (should (equal (nreverse calls) (list magent-grep-program "git")))
      (setq available nil
            calls nil)
      (should-error (magent-tools--grep-backend "/tmp/") :type 'error)
      (should (equal (nreverse calls) (list magent-grep-program "git"))))))

(ert-deftest magent-test-tools-grep-does-not-fall-back-to-basic-grep ()
  "Missing ripgrep and Git fail before any project process starts."
  (require 'magent-tools)
  (let ((root (make-temp-file "magent-no-grep-backend-" t))
        started
        result)
    (unwind-protect
        (cl-letf (((symbol-function 'magent-tools--project-executable)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'magent-tools--start-project-process)
                   (lambda (&rest _args) (setq started t))))
          (let ((magent-tools--request-context
                 (magent-request-context-create
                  :scope root :project-root root)))
            (magent-tools--grep (lambda (value) (setq result value))
                                "needle" "." t))
          (should-not started)
          (should-not (magent-tool-result-success-p result))
          (should (string-match-p
                   "tried .* and git"
                   (magent-tool-result-output-string result))))
      (delete-directory root t))))

(ert-deftest magent-test-tools-git-grep-command-is-self-contained ()
  "The git fallback works inside or outside a Git worktree."
  (require 'magent-tools)
  (let ((command
         (magent-tools--grep-command
          '(:name git-grep :program "/usr/bin/git")
          "pattern" "." nil)))
    (should (equal (car command) "/usr/bin/git"))
    (dolist (argument '("--no-pager" "grep" "--no-index"
                        "--exclude-standard" "-I" "-n" "-z"
                        "--no-color" "--extended-regexp" "--ignore-case"
                        "-e" "pattern" "--" "."))
      (should (member argument command)))))

(ert-deftest magent-test-tools-git-grep-fallback-honors-ignore-files ()
  "git fallback searches untracked files and honors ignores outside a repo."
  (require 'magent-tools)
  (let ((git-program (executable-find "git")))
    (skip-unless git-program)
    (let* ((root (make-temp-file "magent-git-grep-root-" t))
           (file (expand-file-name "untracked.txt" root))
           (ignored (expand-file-name "ignored.txt" root))
           (original-resolver
            (symbol-function 'magent-tools--project-executable))
           result)
      (unwind-protect
          (progn
            (with-temp-file file (insert "fallback needle\n"))
            (with-temp-file ignored (insert "ignored needle\n"))
            (with-temp-file (expand-file-name ".gitignore" root)
              (insert "ignored.txt\n"))
            (cl-letf (((symbol-function 'magent-tools--project-executable)
                       (lambda (program directory)
                         (if (equal program magent-grep-program)
                             nil
                           (funcall original-resolver program directory)))))
              (magent-tools--grep
               (lambda (value) (setq result value)) "needle" root t)
              (let ((deadline (+ (float-time) 5)))
                (while (and (null result) (< (float-time) deadline))
                  (accept-process-output nil 0.05))))
            (should (magent-tool-result-success-p result))
            (should (equal (plist-get (magent-tool-result-metadata result)
                                      :backend)
                           "git-grep"))
            (should (string-match-p "untracked.txt:1:fallback needle"
                                    (magent-tool-result-output-string result)))
            (should-not (string-match-p
                         "ignored needle"
                         (magent-tool-result-output-string result))))
        (delete-directory root t)))))

(ert-deftest magent-test-tools-grep-revision-parser-stops-before-match-text ()
  "NUL-delimited grep parsing ignores colon-number-colon in matched text."
  (require 'magent-tools)
  (let* ((root (make-temp-file "magent-grep-revision-" t))
         (file (expand-file-name "sample.txt" root))
         (raw (concat "sample.txt" (string 0) "1:value:12:tail"))
         revisions)
    (unwind-protect
        (progn
          (with-temp-file file (insert "value:12:tail\n"))
          (setq revisions (magent-tools--grep-revisions raw root))
          (should (equal (mapcar #'car revisions) '("sample.txt")))
          (should (equal (cdar revisions)
                         (magent-tools--file-revision file)))
          (should (equal (magent-tools--grep-display-output raw)
                         "sample.txt:1:value:12:tail")))
      (delete-directory root t))))

(ert-deftest magent-test-tools-git-grep-parser-preserves-colons ()
  "git grep parsing keeps colons in both filenames and matching text."
  (require 'magent-tools)
  (let* ((root (make-temp-file "magent-git-grep-revision-" t))
         (file (expand-file-name "sample:12.txt" root))
         (raw (concat "sample:12.txt" (string 0) "1" (string 0)
                      "value:34:tail"))
         revisions)
    (unwind-protect
        (progn
          (with-temp-file file (insert "value:34:tail\n"))
          (setq revisions
                (magent-tools--grep-revisions raw root 'git-grep))
          (should (equal (mapcar #'car revisions) '("sample:12.txt")))
          (should (equal (cdar revisions)
                         (magent-tools--file-revision file)))
          (should (equal (magent-tools--grep-display-output raw 'git-grep)
                         "sample:12.txt:1:value:34:tail")))
      (delete-directory root t))))

(ert-deftest magent-test-tools-glob-double-star-keeps-prefix ()
  "A ** glob searches recursively without discarding its path prefix."
  (require 'magent-tools)
  (let* ((root (make-temp-file "magent-glob-root-" t))
         (src (expand-file-name "src" root))
         (nested (expand-file-name "nested" src))
         (test-dir (expand-file-name "test" root))
         result)
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory test-dir t)
          (dolist (file (list (expand-file-name "one.el" src)
                              (expand-file-name "two.el" nested)
                              (expand-file-name "outside.el" test-dir)
                              (expand-file-name "root.el" root)))
            (with-temp-file file (insert "")))
          (magent-tools--glob
           (lambda (value) (setq result value)) "src/**/*.el" root)
          (let ((deadline (+ (float-time) 2)))
            (while (and (null result) (< (float-time) deadline))
              (sit-for 0.01)))
          (should (magent-tool-result-success-p result))
          (let ((output (magent-test-tool-output result)))
            (should (string-match-p (regexp-quote "src/one.el") output))
            (should (string-match-p
                     (regexp-quote "src/nested/two.el") output))
            (should-not
             (string-match-p (regexp-quote "test/outside.el") output))
            (should-not (string-match-p (regexp-quote "root.el") output))))
      (delete-directory root t))))

(ert-deftest magent-test-tools-glob-enforces-result-limit ()
  "Glob stops at its result limit and reports structured truncation metadata."
  (require 'magent-tools)
  (let* ((root (make-temp-file "magent-glob-limit-" t))
         (magent-glob-max-results 2)
         (magent-glob-max-files-scanned 100)
         (magent-glob-batch-size 1)
         result)
    (unwind-protect
        (progn
          (dotimes (index 4)
            (with-temp-file
                (expand-file-name (format "file-%d.el" index) root)))
          (magent-tools--glob
           (lambda (value) (setq result value)) "*.el" root)
          (let ((deadline (+ (float-time) 2)))
            (while (and (null result) (< (float-time) deadline))
              (sit-for 0.01)))
          (should (magent-tool-result-success-p result))
          (should (plist-get (magent-tool-result-metadata result)
                             :truncated))
          (should (equal (plist-get (magent-tool-result-metadata result)
                                    :reason)
                         "result limit reached"))
          (should (= (plist-get (magent-tool-result-metadata result)
                                :matched)
                     2)))
      (delete-directory root t))))

(ert-deftest magent-test-audit-strict-redaction-and-private-modes ()
  "Audit persistence omits arbitrary free text and enforces private modes."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-private-" t))
         (magent-audit (expand-file-name "audit.jsonl" directory))
         (magent-audit--pending-writes nil)
         (magent-audit--flush-timer nil)
         (magent-session--current-scope 'global)
         (magent--current-session (magent-session-create))
         (file (magent-audit--file-path))
         (secret "alice:hunter2")
         (result-secret "#<closure ((password . winter-is-coming))>"))
    (unwind-protect
        (progn
          (set-file-modes directory #o755)
          (with-temp-file file)
          (set-file-modes file #o644)
          (magent-audit-record
           'tool-call-end
           :tool-name "bash"
           :args (list :command
                       (format "curl -u %s https://example.invalid" secret))
           :result result-secret)
          (magent-audit--flush-pending)
          (let* ((raw (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string))))
            (should-not (string-match-p (regexp-quote secret) raw))
            (should-not (string-match-p (regexp-quote result-secret) raw))
            (should-not (string-match-p "winter-is-coming" raw))
            (should (string-match-p "command_length" raw))
            (should (string-match-p "result_length" raw))
            (should (= (logand (file-modes directory) #o777) #o700))
            (should (= (logand (file-modes file) #o777) #o600))))
      (magent-audit--flush-pending)
      (delete-directory directory t))))

(ert-deftest magent-test-agent-loop-activity-resets-request-timeout ()
  "Each nonterminal provider event restarts the inactivity timeout."
  (require 'magent-agent-loop)
  (let ((magent-request-timeout 5)
        (timer-count 0)
        cancelled
        sampled-request)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (setq timer-count (1+ timer-count))
                 (list :timer timer-count fn args)))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (push timer cancelled))))
      (let* ((request (magent-llm-request-create
                       :prompt '((prompt . "hello"))))
             (loop (magent-agent-loop-create
                    :request request
                    :sampler (lambda (sample-request)
                               (setq sampled-request sample-request)
                               'provider-handle))))
        (magent-agent-loop-start loop)
        (let ((initial (magent-agent-loop-request-timeout-timer loop)))
          (funcall (magent-llm-request-callback sampled-request)
                   (magent-llm-text-delta-event "progress"))
          (let ((reset (magent-agent-loop-request-timeout-timer loop)))
            (should (member initial cancelled))
            (should-not (equal initial reset))
            (funcall (magent-llm-request-callback sampled-request)
                     (magent-llm-completed-event "done"))
            (should (member reset cancelled))
            (should-not (magent-agent-loop-request-timeout-timer loop))))))))

(ert-deftest magent-test-agent-file-canonical-roundtrip-preserves-policy ()
  "Custom agent save/load preserves canonical fields and permission rules."
  (require 'magent-agent-file)
  (let* ((magent-agent-registry--agents (make-hash-table :test #'equal))
         (magent-agent-registry--initialized t)
         (directory (make-temp-file "magent-agent-canonical-" t))
         (permission '((* . deny)
                       (read . allow)
                       (write . (("*.env.example" . allow)
                                 ("*.env" . deny)
                                 ("*" . ask)))))
         (agent (magent-agent-info-create
                 :name "canonical"
                 :description "Review: #safe"
                 :mode 'subagent
                 :color "#aabbcc"
                 :model 'model-x
                 :permission permission
                 :prompt "Review carefully.")))
    (unwind-protect
        (let* ((file (magent-agent-file-save agent directory))
               (loaded (magent-agent-file-load file)))
          (should loaded)
          (should (equal (magent-agent-info-description loaded)
                         "Review: #safe"))
          (should (equal (magent-agent-info-color loaded) "#aabbcc"))
          (should (eq (magent-agent-info-model loaded) 'model-x))
          (should (equal (magent-agent-info-permission loaded) permission)))
      (delete-directory directory t))))

(ert-deftest magent-test-agent-file-simple-roundtrip-preserves-yaml-scalars ()
  "Canonical scalar-only agent files decode JSON-compatible YAML quoting."
  (require 'magent-agent-file)
  (let* ((magent-agent-registry--agents (make-hash-table :test #'equal))
         (magent-agent-registry--initialized t)
         (directory (make-temp-file "magent-agent-scalar-" t))
         (description "Say \"hi\", use C:\\tmp\nthen continue")
         (agent (magent-agent-info-create
                 :name "scalar"
                 :description description
                 :mode 'primary
                 :prompt "Keep the prompt.")))
    (unwind-protect
        (let ((loaded
               (magent-agent-file-load
                (magent-agent-file-save agent directory))))
          (should loaded)
          (should (equal (magent-agent-info-description loaded) description))
          (should (equal (string-trim (magent-agent-info-prompt loaded))
                         "Keep the prompt.")))
      (delete-directory directory t))))

(ert-deftest magent-test-skill-capability-loading-preserves-directory-precedence ()
  "Embedded capability metadata follows the same later-directory wins rule."
  (require 'magent-capability)
  (let* ((root (make-temp-file "magent-skill-order-" t))
         (first (expand-file-name "z-first" root))
         (second (expand-file-name "a-second" root))
         (magent-skill-directories (list first second))
         (magent-skills--registry nil)
         (magent-capability--registry nil))
    (unwind-protect
        (progn
          (make-directory first)
          (make-directory second)
          (with-temp-file (expand-file-name "SKILL.md" first)
            (insert "---\nname: ordered\ndescription: First skill\ntype: instruction\ncapability: true\ntitle: First Title\nprompt-keywords: [first-only]\n---\nFirst body."))
          (with-temp-file (expand-file-name "SKILL.md" second)
            (insert "---\nname: ordered\ndescription: Second skill\ntype: instruction\ncapability: true\ntitle: Second Title\nprompt-keywords: [second-only]\n---\nSecond body."))
          (cl-letf (((symbol-function 'magent-log) #'ignore))
            (magent-skills-load-all (list first second))
            (magent-capability-load-skill-capabilities (list first second)))
          (should (string-match-p
                   "Second body" (magent-skill-prompt
                                  (magent-skills-get "ordered"))))
          (let ((capability (magent-capability-get "ordered")))
            (should (equal (magent-capability-title capability)
                           "Second Title"))
            (should (equal (magent-capability-prompt-keywords capability)
                           '("second-only")))))
      (delete-directory root t))))

(ert-deftest magent-test-agent-file-rejects-ambiguous-or-malformed-permissions ()
  "Explicit malformed and alias-duplicate permissions fail closed."
  (require 'magent-agent-file)
  (should-error
   (magent-agent-file--parse-permissions
    '(:read "allow" :read_file "deny")))
  (should-error
   (magent-agent-file--parse-permissions '(:bash "sometimes")))
  (let* ((magent-agent-registry--agents (make-hash-table :test #'equal))
         (magent-agent-registry--initialized t)
         (file (make-temp-file "magent-agent-invalid-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "---\nmode: primary\npermissions:\n  bash: sometimes\n---\nNo tools."))
          (cl-letf (((symbol-function 'magent-log) #'ignore))
            (should-not (magent-agent-file-load file)))
          (should (= (hash-table-count magent-agent-registry--agents) 0)))
      (delete-file file))))

(ert-deftest magent-test-agent-file-rejects-retired-tools-field ()
  "Custom agents reject the retired tools field instead of ignoring it."
  (require 'magent-agent-file)
  (let* ((magent-agent-registry--agents (make-hash-table :test #'equal))
         (file (make-temp-file "magent-agent-retired-tools-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "---\nmode: primary\ntools: [read_file, grep]\n---\n"))
          (cl-letf (((symbol-function 'magent-log) #'ignore))
            (should-not (magent-agent-file-load file))))
      (delete-file file))))

(ert-deftest magent-test-agent-info-symbol-model-overrides-gptel-model ()
  "A file-backed symbol model is applied without changing the backend."
  (require 'magent-agent-info)
  (let ((gptel-model 'default-model)
        captured)
    (magent-agent-info-apply-gptel-overrides
     (magent-agent-info-create :name "model" :mode 'primary :model 'custom-model)
     (lambda () (setq captured gptel-model)))
    (should (eq captured 'custom-model))))

(ert-deftest magent-test-project-tool-skill-is-rejected-without-loading-code ()
  "Project tool skills fail closed and never load companion Elisp."
  (require 'magent-skills)
  (let* ((root (file-truename (make-temp-file "magent-skill-project-" t)))
         (directory (expand-file-name ".magent/skills/trust-test" root))
         (file (expand-file-name "SKILL.md" directory))
         (companion (expand-file-name "trust-test.el" directory))
         (magent-skills--registry nil)
         loaded)
    (unwind-protect
        (progn
          (make-directory directory t)
          (with-temp-file file
            (insert "---\nname: trust-test\ntype: tool\n---\nTool docs."))
          (with-temp-file companion (insert "; project companion\n"))
          (cl-letf (((symbol-function 'load-file)
                     (lambda (_file) (setq loaded t)))
                    ((symbol-function 'magent-log) #'ignore))
            (should-not (magent-skills-load-file file))
            (should-not loaded)
            (should-not (magent-skills-get "trust-test"))))
      (delete-directory root t))))

(ert-deftest magent-test-project-symlink-definitions-retain-overlay-ownership ()
  "Symlinked project agent, skill, and capability entries unload by scope."
  (require 'magent-agent-file)
  (require 'magent-skills)
  (require 'magent-capability)
  (let* ((root (file-truename (make-temp-file "magent-overlay-link-" t)))
         (outside (make-temp-file "magent-overlay-source-" t))
         (agent-dir (expand-file-name ".magent/agent" root))
         (skill-dir (expand-file-name ".magent/skills/link-skill" root))
         (cap-dir (expand-file-name ".magent/capabilities/link-cap" root))
         (agent-source (expand-file-name "link-agent.md" outside))
         (skill-source (expand-file-name "SKILL.md" outside))
         (cap-source (expand-file-name "CAPABILITY.md" outside))
         (agent-link (expand-file-name "link-agent.md" agent-dir))
         (skill-link (expand-file-name "SKILL.md" skill-dir))
         (cap-link (expand-file-name "CAPABILITY.md" cap-dir))
         (magent-agent-registry--agents (make-hash-table :test #'equal))
         (magent-agent-registry--initialized t)
         (magent-skills--registry nil)
         (magent-capability--registry nil))
    (unwind-protect
        (progn
          (mapc (lambda (dir) (make-directory dir t))
                (list agent-dir skill-dir cap-dir))
          (with-temp-file agent-source
            (insert "---\nmode: primary\ndescription: linked\n---\nAgent."))
          (with-temp-file skill-source
            (insert "---\nname: link-skill\ndescription: Linked skill\ntype: instruction\n---\nSkill."))
          (with-temp-file cap-source
            (insert "---\nname: link-cap\ntitle: Linked\ncapability-skills: [link-skill]\n---\nCap."))
          (make-symbolic-link agent-source agent-link)
          (make-symbolic-link skill-source skill-link)
          (make-symbolic-link cap-source cap-link)
          (cl-letf (((symbol-function 'magent-log) #'ignore))
            (let ((agent (magent-agent-file-load agent-link))
                  (skill (magent-skills-load-file skill-link))
                  (capability (magent-capability-load-file cap-link)))
              (should (equal (magent-agent-info-source-scope agent) root))
              (should (equal (magent-skill-source-scope skill) root))
              (should (equal (magent-capability-source-scope capability) root)))
            (magent-agent-registry-remove-project-scope root)
            (magent-skills-remove-project-scope root)
            (magent-capability-remove-project-scope root)
            (should-not (magent-agent-registry-get "link-agent"))
            (should-not (magent-skills-get "link-skill"))
            (should-not (magent-capability-get "link-cap"))))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest magent-test-project-symlink-tool-skill-remains-rejected ()
  "A symlinked tool SKILL.md remains unsupported and cannot execute."
  (require 'magent-skills)
  (let* ((root (file-truename (make-temp-file "magent-skill-link-" t)))
         (outside (make-temp-file "magent-skill-link-source-" t))
         (directory (expand-file-name ".magent/skills/link-tool" root))
         (source (expand-file-name "SKILL.md" outside))
         (file (expand-file-name "SKILL.md" directory))
         (companion (expand-file-name "link-tool.el" directory))
         (magent-skills--registry nil)
         loaded)
    (unwind-protect
        (progn
          (make-directory directory t)
          (with-temp-file source
            (insert "---\nname: link-tool\ntype: tool\n---\nDocs."))
          (with-temp-file companion (insert "; untrusted\n"))
          (make-symbolic-link source file)
          (cl-letf (((symbol-function 'load-file)
                     (lambda (_file) (setq loaded t)))
                    ((symbol-function 'magent-log) #'ignore))
            (let ((skill (magent-skills-load-file file)))
              (should-not skill)
              (should-not (magent-skills-get "link-tool"))
              (should-not loaded))))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest magent-test-layered-registries-restore-shadowed-definitions ()
  "Removing project definitions reveals the prior agent/skill/capability."
  (require 'magent-agent-registry)
  (require 'magent-skills)
  (require 'magent-capability)
  (let ((magent-agent-registry--agents (make-hash-table :test #'equal))
        (magent-agent-registry--initialized t)
        (magent-skills--registry nil)
        (magent-capability--registry nil))
    (let ((base-agent (magent-agent-info-create
                       :name "same" :mode 'primary :source-layer 'builtin))
          (project-agent (magent-agent-info-create
                          :name "same" :mode 'primary :source-layer 'project
                          :source-scope "/project-a"))
          (base-skill (magent-skill-create
                       :name "same" :source-layer 'builtin))
          (project-skill (magent-skill-create
                          :name "same" :source-layer 'project
                          :source-scope "/project-a"))
          (base-cap (magent-capability-create
                     :name "same" :source-layer 'builtin))
          (project-cap (magent-capability-create
                        :name "same" :source-layer 'project
                        :source-scope "/project-a")))
      (magent-agent-registry-register base-agent)
      (magent-agent-registry-register project-agent)
      (magent-skills-register base-skill)
      (magent-skills-register project-skill)
      (magent-capability-register base-cap)
      (magent-capability-register project-cap)
      (should (eq (magent-agent-registry-get "same") project-agent))
      (should (eq (magent-skills-get "same") project-skill))
      (should (eq (magent-capability-get "same") project-cap))
      (magent-agent-registry-remove-project-scope "/project-a")
      (magent-skills-remove-project-scope "/project-a")
      (magent-capability-remove-project-scope "/project-a")
      (should (eq (magent-agent-registry-get "same") base-agent))
      (should (eq (magent-skills-get "same") base-skill))
      (should (eq (magent-capability-get "same") base-cap)))))

(ert-deftest magent-test-global-arbiter-rolls-back-failed-starter-before-next ()
  "A failed backend starter is rolled back before the next FIFO ticket starts."
  (require 'magent-runtime-queue)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (blocker (magent-runtime-submission-create :id "blocker"))
         (bad (magent-runtime-submission-create :id "bad"))
         (good (magent-runtime-submission-create :id "good"))
         (original-rollback
          (symbol-function 'magent-runtime-queue--rollback-start))
         order)
    (cl-letf (((symbol-function 'magent-runtime-queue--rollback-start)
               (lambda (submission err)
                 (push 'rollback order)
                 (funcall original-rollback submission err)))
              ((symbol-function 'display-warning) #'ignore))
      (magent-runtime-queue-submit blocker #'ignore)
      (magent-runtime-queue-submit
       bad (lambda (_submission)
             (push 'bad order)
             (error "starter failed")))
      (magent-runtime-queue-submit
       good (lambda (_submission)
              (should-not (eq magent-runtime-queue--active bad))
              (should (eq (magent-runtime-submission-status bad) 'failed))
              (push 'good order)))
      (magent-runtime-queue-finish-active 'completed)
      (should (equal (nreverse order) '(bad rollback good)))
      (should (eq magent-runtime-queue--active good))
      (should (eq (magent-runtime-queue-arbiter-owner) 'runtime)))))

(ert-deftest magent-test-global-arbiter-defers-fifo-advance-until-starter-returns ()
  "Synchronous finalization inside a starter cannot reentrantly start its peer."
  (require 'magent-runtime-queue)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (blocker (magent-runtime-submission-create :id "blocker"))
         (synchronous (magent-runtime-submission-create :id "sync"))
         (next (magent-runtime-submission-create :id "next"))
         next-started
         starter-returned)
    (magent-runtime-queue-submit blocker #'ignore)
    (magent-runtime-queue-submit
     synchronous
     (lambda (_submission)
       (magent-runtime-queue-finish-active 'completed)
       (should-not next-started)
       (setq starter-returned t)))
    (magent-runtime-queue-submit
     next
     (lambda (_submission)
       (should starter-returned)
       (setq next-started t)))
    (magent-runtime-queue-finish-active 'completed)
    (should next-started)
    (should (eq magent-runtime-queue--active next))))

(ert-deftest magent-test-acp-session-list-filters-exact-cwd-scope ()
  "ACP session/list exposes only sessions belonging to the requested cwd."
  (require 'magent-acp)
  (cl-letf (((symbol-function 'magent-session-scope-from-directory)
             (lambda (cwd) (if (equal cwd "/project-a") "/project-a" 'global)))
            ((symbol-function 'magent-runtime-list-sessions-for-scope)
             (lambda (scope)
               (if (equal scope "/project-a")
                   '((:id "a" :scope "/project-a" :project-root "/project-a"
                          :updated-at 0.0)
                     (:id "b" :scope "/project-b" :project-root "/project-b"
                          :updated-at 0.0))
                 '((:id "g" :scope global :project-root nil
                        :updated-at 0.0))))))
    (let ((project (map-elt (magent-acp--session-list-response "/project-a")
                            'sessions))
          (global (map-elt (magent-acp--session-list-response "/tmp")
                           'sessions)))
      (should (= (length project) 1))
      (should (equal (map-elt (aref project 0) 'sessionId) "a"))
      (should (= (length global) 1))
      (should (equal (map-elt (aref global 0) 'sessionId) "g")))))

(ert-deftest magent-test-runtime-session-registry-keys-by-scope-and-id ()
  "Equal persisted ids in different scopes retain distinct wrappers."
  (require 'magent-runtime-api)
  (let ((magent-runtime-api--sessions (make-hash-table :test #'equal)))
    (let* ((session-a (magent-session-create :id "same-id"))
           (session-b (magent-session-create :id "same-id"))
           (runtime-a (magent-runtime-api--wrap-session session-a "/a"))
           (runtime-b (magent-runtime-api--wrap-session session-b "/b")))
      (should-not (eq runtime-a runtime-b))
      (should (eq (magent-runtime-session-from-id "same-id" "/a") runtime-a))
      (should (eq (magent-runtime-session-from-id "same-id" "/b") runtime-b))
      (should-not (magent-runtime-session-from-id "same-id")))))

(ert-deftest magent-test-runtime-completion-holds-global-lease-through-callback ()
  "Completion releases backend busy state before callbacks, but not FIFO order."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         callback-ran
         next-started
         (first
          (magent-runtime-submission-create
           :id "first"
           :on-complete
           (lambda (_status _result)
             (should-not (magent-runtime-queue-active-submission))
             (should (eq (magent-runtime-queue-arbiter-owner) 'runtime))
             (should-not next-started)
             (setq callback-ran t))))
         (next (magent-runtime-submission-create :id "next")))
    (magent-runtime-queue-submit first #'ignore)
    (magent-runtime-queue-submit
     next (lambda (_submission) (setq next-started t)))
    (magent-runtime-api--finish-submission first 'completed "done")
    (should callback-ran)
    (should next-started)
    (should (eq (magent-runtime-queue-active-submission) next))))

(ert-deftest magent-test-runtime-cancel-distinguishes-equal-ids-across-scopes ()
  "Cancellation and counts use wrapper identity, not an ambiguous session id."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (runtime-a
          (magent-runtime-session-create
           :id "same" :scope "/a"
           :magent-session (magent-session-create :id "same")))
         (runtime-b
          (magent-runtime-session-create
           :id "same" :scope "/b"
           :magent-session (magent-session-create :id "same")))
         (blocker (magent-runtime-submission-create :id "blocker"))
         completion-a
         started-b
         (submission-a
          (magent-runtime-submission-create
           :id "a" :session runtime-a :session-id "same"
           :on-complete
           (lambda (status _result) (setq completion-a status))))
         (submission-b
          (magent-runtime-submission-create
           :id "b" :session runtime-b :session-id "same")))
    (magent-runtime-queue-submit blocker #'ignore)
    (magent-runtime-queue-submit submission-a #'ignore)
    (magent-runtime-queue-submit
     submission-b (lambda (_submission) (setq started-b t)))
    (should (= (magent-runtime-pending-count runtime-a) 1))
    (should (= (magent-runtime-pending-count runtime-b) 1))
    (cl-letf (((symbol-function 'magent-session-save-deferred-for-session)
               #'ignore))
      (should (= (magent-runtime-cancel runtime-a) 1)))
    (should (eq completion-a 'cancelled))
    (should-not started-b)
    (should (= (magent-runtime-pending-count runtime-a) 0))
    (should (= (magent-runtime-pending-count runtime-b) 1))
    (magent-runtime-queue-finish-active 'completed)
    (should started-b)
    (should (eq (magent-runtime-queue-active-submission) submission-b))))

(ert-deftest magent-test-runtime-stale-equal-id-finish-keeps-active-token ()
  "A stale object cannot release a different active token with the same id."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (active (magent-runtime-submission-create :id "reused"))
         (stale (magent-runtime-submission-create :id "reused")))
    (magent-runtime-queue-submit active #'ignore)
    (magent-runtime-api--finish-submission stale 'completed "stale")
    (should (eq (magent-runtime-queue-active-submission) active))
    (should (eq (magent-runtime-queue-arbiter-owner) 'runtime))
    (should (magent-runtime-submission-finalized stale))
    (should-not (magent-runtime-submission-finalized active))))

(ert-deftest magent-test-runtime-observer-cancel-at-start-never-launches-loop ()
  "A synchronous turn-start cancellation prevents provider sampling."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--loaded-sessions
          (make-hash-table :test #'eq :weakness 'key))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (session (magent-session-create :id "observer-cancel"))
         (runtime-session
          (magent-runtime-session-create
           :id "observer-cancel" :scope 'global :magent-session session))
         launched
         completion
         events)
    (cl-letf (((symbol-function 'magent-runtime-activate-scope) #'ignore)
              ((symbol-function 'magent-session-refresh-agent) #'ignore)
              ((symbol-function 'magent-session-save-deferred-for-session)
               #'ignore)
              ((symbol-function 'magent-agent-run-turn)
               (lambda (&rest _args) (setq launched t))))
      (magent-runtime-submit
       runtime-session "cancel me"
       :observer
       (lambda (event)
         (push (plist-get event :type) events)
         (when (eq (plist-get event :type) 'turn-start)
           (magent-runtime-cancel runtime-session)))
       :on-complete
       (lambda (status result) (setq completion (list status result)))))
    (should-not launched)
    (should (eq (car completion) 'cancelled))
    (should-not (magent-execution-result-success-p (cadr completion)))
    (should (equal (magent-execution-result-content-string (cadr completion))
                   "Active turn cancelled"))
    (should (memq 'turn-start events))
    (should (memq 'turn-cancelled events))
    (should-not (magent-runtime-queue-active-submission))
    (should-not (magent-runtime-queue-arbiter-owner))
    (let ((turn (car (magent-thread-turns
                      (magent-session-thread-ledger session)))))
      (should (eq (magent-thread-turn-status turn) 'interrupted)))))

(ert-deftest magent-test-runtime-prepare-context-refuses-cross-scope-during-lease ()
  "Interactive registry activation cannot steal another turn's project scope."
  (require 'magent-runtime)
  (require 'magent-runtime-queue)
  (let* ((magent--initialized t)
         (magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (submission
          (magent-runtime-submission-create :id "leased" :scope "/a"))
         activated)
    (magent-runtime-queue-submit submission #'ignore)
    (cl-letf (((symbol-function 'magent-runtime-activate-scope)
               (lambda (scope &optional _force) (push scope activated))))
      (should-error (magent-runtime-prepare-context "/b")
                    :type 'user-error)
      (should-not activated)
      (magent-runtime-prepare-context "/a")
      (should (equal activated '("/a"))))))

(ert-deftest magent-test-acp-prompt-and-cancel-use-client-exact-scope ()
  "ACP does not resolve an equal session id from another project."
  (require 'magent-acp)
  (let* ((magent-runtime-api--sessions (make-hash-table :test #'equal))
         (buffer (generate-new-buffer "*magent-acp-scope-test*"))
         (runtime-a
          (magent-runtime-session-create
           :id "same" :scope "/a"
           :magent-session (magent-session-create :id "same")))
         (runtime-b
          (magent-runtime-session-create
           :id "same" :scope "/b"
           :magent-session (magent-session-create :id "same")))
         (client `((:context-buffer . ,buffer)
                   (:notification-handlers . nil)
                   (:request-handlers . nil)))
         submitted
         cancelled
         failure)
    (unwind-protect
        (progn
          (puthash (list "/a" "same") runtime-a magent-runtime-api--sessions)
          (puthash (list "/b" "same") runtime-b magent-runtime-api--sessions)
          (magent-acp--bind-client-session client runtime-b)
          (cl-letf (((symbol-function 'magent-session-scope-from-directory)
                     (lambda (_directory) "/b"))
                    ((symbol-function 'magent-runtime-submit)
                     (lambda (runtime-session _prompt &rest _args)
                       (setq submitted runtime-session)))
                    ((symbol-function 'magent-runtime-cancel)
                     (lambda (runtime-session)
                       (setq cancelled runtime-session))))
            (magent-acp--handle-request
             client
             '((:method . "session/prompt")
               (:params . ((sessionId . "same")
                           (prompt . [((type . "text") (text . "hello"))]))))
             #'ignore
             (lambda (err) (setq failure err)))
            (magent-acp--notification-sender
             :client client
             :notification
             '((:method . "session/cancel")
               (:params . ((sessionId . "same") (reason . "test"))))))
          (should-not failure)
          (should (eq submitted runtime-b))
          (should (eq cancelled runtime-b)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-acp-client-session-binding-survives-buffer-cwd-change ()
  "ACP prompt and cancel keep the scope captured at session bootstrap."
  (require 'magent-acp)
  (let* ((magent-acp--client-session-scopes
          (make-hash-table :test #'eq :weakness 'key))
         (magent-runtime-api--sessions (make-hash-table :test #'equal))
         (buffer (generate-new-buffer "*magent-acp-fixed-scope*"))
         (client `((:context-buffer . ,buffer)
                   (:notification-handlers . nil)
                   (:request-handlers . nil)))
         (session-id "fixed")
         (fixed
          (magent-runtime-session-create
           :id session-id :scope "/explicit"
           :magent-session (magent-session-create :id session-id)))
         (changed
          (magent-runtime-session-create
           :id session-id :scope "/changed"
           :magent-session (magent-session-create :id session-id)))
         submitted cancelled failure)
    (unwind-protect
        (progn
          (puthash (list "/explicit" session-id) fixed
                   magent-runtime-api--sessions)
          (puthash (list "/changed" session-id) changed
                   magent-runtime-api--sessions)
          (with-current-buffer buffer
            (setq-local default-directory "/buffer-before/"))
          (magent-acp--bind-client-session client fixed)
          (with-current-buffer buffer
            (setq-local default-directory "/changed/"))
          (cl-letf (((symbol-function 'magent-session-scope-from-directory)
                     (lambda (_directory) "/changed"))
                    ((symbol-function 'magent-runtime-submit)
                     (lambda (runtime-session _prompt &rest _args)
                       (setq submitted runtime-session)))
                    ((symbol-function 'magent-runtime-cancel)
                     (lambda (runtime-session)
                       (setq cancelled runtime-session))))
            (magent-acp--handle-request
             client
             `((:method . "session/prompt")
               (:params . ((sessionId . ,session-id)
                           (prompt . [((type . "text")
                                       (text . "hello"))]))))
             #'ignore
             (lambda (err) (setq failure err)))
            (magent-acp--notification-sender
             :client client
             :notification
             `((:method . "session/cancel")
               (:params . ((sessionId . ,session-id))))))
          (should-not failure)
          (should (eq submitted fixed))
          (should (eq cancelled fixed))
          (should (equal (magent-acp--client-session-scope
                          client session-id)
                         "/explicit")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-acp-failed-load-does-not-prepare-or-switch-scope ()
  "An invalid load candidate fails before command-context activation."
  (require 'magent-acp)
  (let ((session (magent-session-create :id "candidate"))
        prepared failure)
    (cl-letf (((symbol-function 'magent-acp--scope-for-cwd)
               (lambda (_cwd) "/target"))
              ((symbol-function 'magent-acp--load-candidate)
               (lambda (_id _scope) (list :session session)))
              ((symbol-function
                'magent-runtime-session-ensure-registerable)
               (lambda (&rest _args)
                 (user-error "leased session")))
              ((symbol-function 'magent-runtime-prepare-context)
               (lambda (scope) (setq prepared scope))))
      (magent-acp--handle-request
       nil
       '((:method . "session/load")
         (:params . ((sessionId . "candidate") (cwd . "/target"))))
       #'ignore
       (lambda (err) (setq failure err))))
    (should failure)
    (should-not prepared)))

(ert-deftest magent-test-runtime-register-refuses-same-scope-lease-replacement ()
  "A same-scope session cannot replace the exact session owning the lease."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-api--sessions (make-hash-table :test #'equal))
         (magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (scope "/same")
         (first (magent-session-create :id "first"))
         (replacement (magent-session-create :id "replacement"))
         (runtime
          (magent-runtime-session-create
           :id "first" :scope scope :magent-session first))
         (submission
          (magent-runtime-submission-create
           :id "leased" :scope scope :session runtime)))
    (magent-session-install scope first)
    (puthash (list scope "first") runtime magent-runtime-api--sessions)
    (magent-runtime-queue-submit submission #'ignore)
    (should-error (magent-runtime-session-register scope replacement)
                  :type 'user-error)
    (should (eq (magent-session-get-if-present scope) first))
    (should-not (magent-runtime-session-from-id "replacement" scope))))

(ert-deftest magent-test-acp-session-new-defers-install-under-scope-lease ()
  "ACP can create a fresh same-scope session while another turn executes."
  (require 'magent-acp)
  (let* ((magent--initialized t)
         (magent-runtime-api--sessions (make-hash-table :test #'equal))
         (magent-acp--client-session-scopes
          (make-hash-table :test #'eq :weakness 'key))
         (magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (scope "/same")
         (first (magent-session-create :id "first"))
         (active-runtime
          (magent-runtime-session-create
           :id "first" :scope scope :magent-session first))
         (submission
          (magent-runtime-submission-create
           :id "leased" :scope scope :session active-runtime))
         (client '((:notification-handlers . nil)
                   (:request-handlers . nil)))
         response failure)
    (magent-session-install scope first)
    (puthash (list scope "first") active-runtime
             magent-runtime-api--sessions)
    (magent-runtime-queue-submit submission #'ignore)
    (cl-letf (((symbol-function 'magent-session-scope-from-directory)
               (lambda (_cwd) scope))
              ((symbol-function 'magent-runtime-prepare-context)
               #'ignore)
              ((symbol-function 'magent-agent-registry-primary-agents)
               (lambda () nil))
              ((symbol-function 'magent-acp--available-commands)
               (lambda (&optional _runtime-session) [])))
      (magent-acp--handle-request
       client
       '((:method . "session/new") (:params . ((cwd . "/same"))))
       (lambda (value) (setq response value))
       (lambda (err) (setq failure err))))
    (should-not failure)
    (let* ((fresh-runtime
            (magent-runtime-session-from-id
             (map-elt response 'sessionId) scope))
           (fresh (magent-runtime-session-magent-session fresh-runtime)))
      (should (magent-runtime-session-p fresh-runtime))
      (should-not (eq fresh first))
      (should (eq (magent-session-get-if-present scope) first))
      (should (eq (magent-runtime-session-from-id
                   (magent-session-id fresh) scope)
                  fresh-runtime)))))

(ert-deftest magent-test-runtime-current-does-not-create-under-scope-lease ()
  "Missing scoped state stays missing when another exact session owns it."
  (require 'magent-runtime-api)
  (let* ((magent--initialized t)
         (magent-runtime-api--sessions (make-hash-table :test #'equal))
         (magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (session (magent-session-create :id "leased"))
         (runtime
          (magent-runtime-session-create
           :id "leased" :scope "/same" :magent-session session))
         (submission
          (magent-runtime-submission-create
           :id "active" :scope "/same" :session runtime)))
    (magent-runtime-queue-submit submission #'ignore)
    (should-error (magent-runtime-session-current "/same")
                  :type 'user-error)
    (should-not (magent-session-get-if-present "/same"))))

(ert-deftest magent-test-runtime-register-refuses-queued-wrapper-replacement ()
  "Queued work prevents replacing an equal-id wrapper's session object."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-api--sessions (make-hash-table :test #'equal))
         (magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (first (magent-session-create :id "same-id"))
         (replacement (magent-session-create :id "same-id"))
         (runtime
          (magent-runtime-session-create
           :id "same-id" :scope "/queued" :magent-session first))
         (blocker (magent-runtime-submission-create
                   :id "blocker" :scope "/other"))
         (queued (magent-runtime-submission-create
                  :id "queued" :scope "/queued" :session runtime)))
    (magent-session-install "/queued" first)
    (puthash (list "/queued" "same-id") runtime
             magent-runtime-api--sessions)
    (magent-runtime-queue-submit blocker #'ignore)
    (magent-runtime-queue-submit queued #'ignore)
    (should-error
     (magent-runtime-session-register "/queued" replacement)
     :type 'user-error)
    (should (eq (magent-runtime-session-magent-session runtime) first))))

(ert-deftest magent-test-runtime-clear-blocks-reentrant-submit-transaction ()
  "A cancellation callback cannot enqueue a turn into a half-cleared session."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-api--clearing-sessions
          (make-hash-table :test #'eq :weakness 'key))
         (magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (session (magent-session-create :id "clear-transaction"))
         (runtime
          (magent-runtime-session-create
           :id "clear-transaction" :scope 'global
           :magent-session session))
         reentrant-error
         (submission
          (magent-runtime-submission-create
           :id "active" :scope 'global :session runtime
           :on-complete
           (lambda (_status _result)
             (condition-case err
                 (magent-runtime-submit runtime "must not queue")
               (user-error (setq reentrant-error err)))))))
    (magent-session-install 'global session)
    (magent-runtime-queue-submit submission #'ignore)
    (cl-letf (((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-clear-capability-overrides)
               #'ignore))
      (magent-runtime-session-clear runtime))
    (should reentrant-error)
    (should-not (magent-runtime-queue-active-submission))
    (should (= (magent-runtime-pending-count runtime) 0))
    (should-not (magent-test--session-transcript session))))

(ert-deftest magent-test-runtime-clear-refuses-stale-reused-id ()
  "Clearing a stale wrapper cannot delete the registered equal-id session."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-api--clearing-sessions
          (make-hash-table :test #'eq :weakness 'key))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (registered (magent-session-create :id "reused"))
         (stale (magent-session-create :id "reused"))
         (runtime
          (magent-runtime-session-create
           :id "reused" :scope 'global :magent-session stale)))
    (magent-session-install 'global registered)
    (should-error (magent-runtime-session-clear runtime) :type 'user-error)
    (should (eq (magent-session-get-if-present 'global) registered))))

(ert-deftest magent-test-session-id-validation-and-filename-consistency ()
  "Unsafe ids cannot escape storage and mismatched files cannot load."
  (let* ((directory (make-temp-file "magent-session-id-" t))
         (magent-session-directory directory)
         (bad (magent-test--session-with-transcript
               "../escape" '((user "unsafe"))))
         (mismatch (expand-file-name "filename.json" directory))
         (missing (expand-file-name "missing.json" directory))
         (invalid (expand-file-name "invalid.json" directory))
         logs)
    (unwind-protect
        (progn
          (should-error (magent-session-save-for-session bad 'global)
                        :type 'magent-session-schema-error)
          (with-temp-file mismatch
            (insert (json-encode
                     (magent-test--current-session-json-data "other"))))
          (with-temp-file missing
            (insert (json-encode
                     (assq-delete-all
                      'id (magent-test--current-session-json-data "missing")))))
          (with-temp-file invalid
            (insert (json-encode
                     (assq-delete-all
                      'schema-version
                      (magent-test--current-session-json-data "invalid")))))
          (cl-letf (((symbol-function 'magent-log)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) logs))))
            (should-not (magent-session-read-file mismatch))
            (should-not (magent-session-read-file missing))
            (should-not (magent-session-read-file invalid)))
          (should (cl-some
                   (lambda (line)
                     (string-match-p "does not match filename" line))
                   logs))
          (should-not (magent-session-list-files)))
      (delete-directory directory t))))

(ert-deftest magent-test-session-fork-deep-copies-ledger-and-resets-owned-state ()
  "Forked sessions share history but no mutable conversation state."
  (let* ((agent (magent-agent-info-create :name "build" :mode 'primary))
         (source
          (magent-session-create
           :id "source-session"
           :max-history 17
           :agent agent
           :approval-overrides '((bash . allow))
           :agent-jobs '(source-job)
           :metadata '((title . "Source title"))))
         (_ (magent-test--record-session-entry source 'user "hello"))
         (_ (magent-test--record-session-entry source 'assistant "world"))
         (source-thread (magent-session-thread source))
         (source-turn (car (magent-thread-turns source-thread)))
         (source-item (car (magent-thread-turn-items source-turn)))
         (source-transcript (magent-test--session-transcript source))
         (fork (magent-session-fork source 'global))
         (fork-thread (magent-session-thread fork))
         (fork-turn (car (magent-thread-turns fork-thread)))
         (fork-item (car (magent-thread-turn-items fork-turn))))
    (should-not (equal (magent-session-id fork) (magent-session-id source)))
    (should (equal (magent-session-metadata-value fork 'parent-session-id)
                   "source-session"))
    (should (equal (magent-session-metadata-value fork 'title)
                   "Source title"))
    (should (numberp (magent-session-metadata-value fork 'forked-at)))
    (should (= (magent-session-max-history fork) 17))
    (should (eq (magent-session-agent fork) agent))
    (should-not (magent-session-approval-overrides fork))
    (should-not (magent-session-agent-jobs fork))
    (should (equal (magent-test--session-transcript fork) source-transcript))
    (should-not (eq fork-thread source-thread))
    (should-not (eq fork-turn source-turn))
    (should-not (eq fork-item source-item))
    (should-not (eq (magent-thread-item-content fork-item)
                    (magent-thread-item-content source-item)))
    (should (equal (magent-thread-id fork-thread) (magent-session-id fork)))
    (should (equal (magent-thread-session-id fork-thread)
                   (magent-session-id fork)))
    (should (equal (magent-thread-turn-thread-id fork-turn)
                   (magent-session-id fork)))
    (should (eq (magent-thread-status fork-thread) 'idle))
    (should-not (magent-thread-journal fork-thread))
    (should (= (magent-thread-last-event-seq fork-thread) 0))
    (magent-test--record-session-entry fork 'user "fork only")
    (magent-test--record-session-entry fork 'assistant "diverged")
    (should (equal (magent-test--session-transcript source)
                   source-transcript))
    (should (= (length (magent-thread-turns fork-thread)) 2))
    (should (= (length (magent-thread-turns source-thread)) 1))))

(ert-deftest magent-test-session-fork-rejects-non-terminal-ledger ()
  "A fork cannot capture a queued or otherwise non-terminal turn."
  (let* ((source (magent-session-create :id "busy-source"))
         (thread (magent-session-thread-ledger source)))
    (magent-thread-queue-turn thread "queued")
    (should-error (magent-session-fork source 'global) :type 'user-error)
    (should (= (length (magent-thread-turns thread)) 1))))

(ert-deftest magent-test-runtime-session-fork-copies-config-spills-and-persists ()
  "Runtime fork preserves stable options and session-private tool results."
  (require 'magent-runtime-api)
  (let* ((magent-session-directory (make-temp-file "magent-fork-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (magent-runtime-api--sessions (make-hash-table :test #'equal))
         (magent-runtime-api--clearing-sessions
          (make-hash-table :test #'eq :weakness 'key))
         (magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-tool-result-model-max-length 30)
         (magent-tool-result-model-preview-length 20)
         (payload (make-string 200 ?f))
         (agent (magent-agent-info-create :name "build" :mode 'primary))
         (source
          (magent-session-create :id "fork-source" :agent agent
                                 :approval-overrides '((bash . allow))
                                 :agent-jobs '(source-job)))
         (source-runtime
          (magent-runtime-session-create
           :id "fork-source" :scope 'global :magent-session source
           :effort 'xhigh :pending-skills '(one-shot)
           :metadata '(:capabilities-enabled nil))))
    (unwind-protect
        (progn
          (magent-test--record-session-entry source 'user "large tool")
          (magent-test--record-tool-entry
           source "fork-spill" "bash" '(:command "large")
           (magent-test-tool-result payload))
          (magent-test--record-session-entry source 'assistant "done")
          (magent-session-install 'global source)
          (puthash (list 'global "fork-source") source-runtime
                   magent-runtime-api--sessions)
          (let* ((source-result-id
                  (car (magent-thread-spill-result-ids
                        (magent-session-thread source))))
                 (fork-runtime
                  (magent-runtime-session-fork source-runtime))
                 (fork (magent-runtime-session-magent-session fork-runtime))
                 (fork-id (magent-runtime-session-id fork-runtime))
                 (fork-file
                  (expand-file-name
                   (concat fork-id ".json")
                   (magent-session--scope-storage-directory 'global))))
            (should (eq (magent-session-get-if-present 'global) source))
            (should (eq (magent-runtime-session-effort fork-runtime) 'xhigh))
            (should-not
             (magent-runtime-session-capabilities-enabled-p fork-runtime))
            (should-not (magent-runtime-session-pending-skills fork-runtime))
            (should (eq (magent-session-agent fork) agent))
            (should-not (magent-session-approval-overrides fork))
            (should-not (magent-session-agent-jobs fork))
            (should (file-regular-p fork-file))
            (let ((loaded
                   (plist-get (magent-session-read-file fork-file) :session)))
              (should (equal
                       (magent-session-metadata-value
                        loaded 'parent-session-id)
                       "fork-source"))
              (should
               (equal (magent-test--session-transcript loaded)
                      (magent-test--session-transcript source))))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents
                 (magent-tool-output-spill-file
                  'global fork-id source-result-id))
                (buffer-string))
              payload))
            (should
             (equal (magent-test--session-transcript fork)
                    (magent-test--session-transcript source)))
            (should (eq (magent-runtime-session-from-id fork-id 'global)
                        fork-runtime))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-runtime-session-fork-rejects-queue-owner-before-clone ()
  "Runtime fork checks the exact queue owner before creating a session."
  (require 'magent-runtime-api)
  (let* ((source (magent-session-create :id "queue-source"))
         (runtime
          (magent-runtime-session-create
           :id "queue-source" :scope 'global :magent-session source))
         cloned)
    (cl-letf (((symbol-function 'magent-runtime-queue-session-busy-p)
               (lambda (candidate) (eq candidate source)))
              ((symbol-function 'magent-session-fork)
               (lambda (&rest _args) (setq cloned t))))
      (should-error (magent-runtime-session-fork runtime) :type 'user-error))
    (should-not cloned)))

(ert-deftest magent-test-runtime-session-fork-rolls-back-new-artifacts ()
  "A failed durable save removes only the new fork artifacts."
  (require 'magent-runtime-api)
  (let* ((source (magent-session-create :id "rollback-source"))
         (fork (magent-session-create :id "rollback-fork"))
         (runtime
          (magent-runtime-session-create
           :id "rollback-source" :scope 'global :magent-session source))
         cleared spill-deleted)
    (cl-letf (((symbol-function 'magent-runtime-queue-session-busy-p)
               (lambda (_session) nil))
              ((symbol-function 'magent-session-fork)
               (lambda (_source _scope) fork))
              ((symbol-function 'magent-thread-spill-result-ids)
               (lambda (_thread) '("result-one")))
              ((symbol-function 'magent-tool-output-spill-fork-session)
               (lambda (&rest _args) t))
              ((symbol-function 'magent-session-save-for-session)
               (lambda (&rest _args) (error "disk full")))
              ((symbol-function 'magent-session-clear)
               (lambda (session scope)
                 (setq cleared (list session scope))))
              ((symbol-function 'magent-tool-output-spill-delete-session)
               (lambda (scope id)
                 (setq spill-deleted (list scope id)))))
      (should-error (magent-runtime-session-fork runtime)))
    (should (equal cleared (list fork 'global)))
    (should (equal spill-deleted '(global "rollback-fork")))))

(ert-deftest magent-test-acp-session-fork-negotiates-and-binds-new-session ()
  "ACP advertises fork only with a working exact-scope request path."
  (require 'magent-acp)
  (let* ((magent-acp--client-session-scopes
          (make-hash-table :test #'eq :weakness 'key))
         (magent-runtime-api--sessions (make-hash-table :test #'equal))
         (source-session (magent-session-create :id "acp-source"))
         (source
          (magent-runtime-session-create
           :id "acp-source" :scope "/project" :magent-session source-session))
         (fork-session (magent-session-create :id "acp-fork"))
         (fork
          (magent-runtime-session-create
           :id "acp-fork" :scope "/project" :magent-session fork-session))
         (client '((:notification-handlers . nil)
                   (:request-handlers . nil)))
         response failure captured)
    (puthash (list "/project" "acp-source") source
             magent-runtime-api--sessions)
    (let* ((capabilities
            (map-elt (magent-acp--initialize-response)
                     'sessionCapabilities))
           (fork-capability (assq 'fork capabilities)))
      (should fork-capability)
      (should-not (cdr fork-capability)))
    (cl-letf (((symbol-function 'magent-session-scope-from-directory)
               (lambda (cwd)
                 (if (equal cwd "/project") "/project" "/other")))
              ((symbol-function 'magent-runtime-session-fork)
               (lambda (runtime-session)
                 (setq captured runtime-session)
                 fork))
              ((symbol-function 'magent-acp--available-commands)
               (lambda (&optional _runtime-session) []))
              ((symbol-function 'magent-runtime-session-title)
               (lambda (_runtime-session) nil))
              ((symbol-function 'magent-agent-registry-primary-agents)
               (lambda () nil)))
      (magent-acp--handle-request
       client
       '((:method . "session/fork")
         (:params . ((sessionId . "acp-source") (cwd . "/project"))))
       (lambda (value) (setq response value))
       (lambda (err) (setq failure err))))
    (should-not failure)
    (should (eq captured source))
    (should (equal (map-elt response 'sessionId) "acp-fork"))
    (should (equal (magent-acp--client-session-scope client "acp-fork")
                   "/project"))))

(ert-deftest magent-test-acp-session-fork-rejects-cross-scope-source ()
  "ACP fork cannot resolve a source session from another cwd scope."
  (require 'magent-acp)
  (let* ((magent-runtime-api--sessions (make-hash-table :test #'equal))
         (source
          (magent-runtime-session-create
           :id "scope-source" :scope "/source"
           :magent-session (magent-session-create :id "scope-source")))
         failure forked)
    (puthash (list "/source" "scope-source") source
             magent-runtime-api--sessions)
    (cl-letf (((symbol-function 'magent-session-scope-from-directory)
               (lambda (_cwd) "/other"))
              ((symbol-function 'magent-runtime-session-fork)
               (lambda (_runtime-session) (setq forked t))))
      (magent-acp--handle-request
       nil
       '((:method . "session/fork")
         (:params . ((sessionId . "scope-source") (cwd . "/other"))))
       #'ignore
       (lambda (err) (setq failure err))))
    (should failure)
    (should-not forked)))

(ert-deftest magent-test-runtime-struct-layouts-match-current-contract ()
  "Runtime structs expose the current request and submission contract."
  (require 'magent-agent-loop)
  (should (= (length (magent-lifecycle-events-context-create)) 6))
  (should (= (length (magent-request-context-create)) 29))
  (should (= (length (magent-agent-loop-create)) 23))
  (should (= (length (magent-runtime-submission-create)) 23)))

(provide 'magent-test)
;;; magent-test.el ends here
