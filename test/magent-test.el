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

(defconst magent-test--builtin-slash-command-names
  '("explain" "fix" "init" "review" "summarize" "test")
  "Bundled instruction skills that are exposed as slash commands.")

(defconst magent-test--builtin-control-command-names
  '("clear" "compact")
  "Magent-owned session controls exposed as slash commands.")

(defun magent-test--load-builtin-skills-only ()
  "Load bundled skill files into the caller's test skill registry."
  (require 'magent-skills)
  (cl-letf (((symbol-function 'magent-log) #'ignore))
    (magent-skills-load-all (list magent-skills--builtin-dir))))

(defun magent-test--load-builtin-capabilities-only ()
  "Load bundled capability definitions into the caller's test registry."
  (require 'magent-capability)
  (let ((magent-skill-directories nil)
        (magent-capability-directories nil))
    (cl-letf (((symbol-function 'magent-log) #'ignore))
      (magent-capability-initialize-static))))

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

(defun magent-test--compose-with-text (scope text)
  "Return SCOPE's compose buffer filled with TEXT."
  (let ((buffer (magent-ui-compose-buffer scope)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert text))
    buffer))

;; ──────────────────────────────────────────────────────────────────────
;;; Integration tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-aa-ui-router-default-does-not-load-legacy ()
  "Test plain default dispatch stays on agent-shell without loading legacy UI."
  (skip-unless (not (featurep 'magent-ui-legacy)))
  (let ((magent-ui-backend 'agent-shell)
        captured)
    (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
              ((symbol-function 'magent-ui--require-legacy)
               (lambda ()
                 (error "legacy UI should not be loaded")))
              ((symbol-function 'magent-agent-shell-send-prompt)
               (lambda (prompt &rest _args)
                 (setq captured prompt))))
      (magent-ui-dispatch-prompt "hello" 'prompt nil nil t))
    (should (equal captured "hello"))
    (should-not (featurep 'magent-ui-legacy))))

(ert-deftest magent-test-aa-ui-router-region-submits-agent-shell-prompt ()
  "Test region prompts submit selected text through Magent's agent-shell path."
  (require 'magent-ui)
  (let ((magent-ui-backend 'agent-shell)
        captured)
    (cl-letf (((symbol-function 'magent-agent-shell-send-prompt)
               (lambda (prompt &rest _args)
                 (setq captured prompt))))
      (with-temp-buffer
        (insert "zero selected tail")
        (magent-prompt-region 6 14)))
    (should (equal captured "selected"))))

(ert-deftest magent-test-ab-ui-router-legacy-shim-loads-legacy ()
  "Test legacy-only functions load the isolated legacy UI module lazily."
  (skip-unless (not (featurep 'magent-ui-legacy)))
  (let ((buffer (magent-ui-get-buffer 'global)))
    (should (buffer-live-p buffer))
    (should (featurep 'magent-ui-legacy))))

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

(ert-deftest magent-test-agent-process-renders-completed-delta-after-stream-prefix ()
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
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) nil))
              ((symbol-function 'magent-ui-start-streaming) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify)
               #'ignore))
      (magent-session-reset)
      (magent-agent-process
       "Hello"
       (lambda (r) (setq response r))
       nil nil nil nil nil
       (lambda (text) (push text ui-chunks))))
    (should (equal response "Checking buffers. Done."))
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
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) nil)))
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
    (should (equal response "MAGENT_HELLO"))
    (should (equal
             (delq nil
                   (mapcar (lambda (event)
                             (when (eq (plist-get event :type)
                                       'assistant-delta)
                               (plist-get event :text)))
                           (nreverse events)))
             '("MAGENT_HELLO")))))

(ert-deftest magent-test-agent-process-closes-reasoning-before-completed-text ()
  "Test completed text after reasoning is rendered outside the reasoning row."
  (require 'magent-ui)
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (gptel-tools nil)
        (gptel-use-tools nil)
        (magent-include-reasoning t)
        (response nil))
    (magent-session-reset)
    (magent-ui-clear-buffer)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (let ((callback (plist-get kwargs :callback)))
                   (funcall callback '(reasoning . "thinking") '(:stream t))
                   (funcall callback t '(:content "Hello from AI")))))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) nil)))
      (magent-agent-process
       "Hello"
       (lambda (result)
         (setq response result))))
    (should (equal response "Hello from AI"))
    (with-current-buffer (magent-ui-get-buffer)
      (let* ((text (buffer-string))
             (reasoning (string-match-p "Reasoning \\[done\\] 8 chars" text))
             (assistant (string-match-p "^ASSISTANT +$" text)))
        (should reasoning)
        (should assistant)
        (should (< reasoning assistant))
        (should-not (string-match-p "thinking" text))))))

(ert-deftest magent-test-agent-process-error-returns-failed-result ()
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
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) nil))
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-session-reset)
      (magent-agent-process
       "Hello"
       (lambda (result)
         (setq response result))))
    (should (magent-agent-result-p response))
    (should-not (magent-agent-result-success-p response))
    (should (equal (magent-agent-result-content-string response)
                   "Request timed out after 5 seconds"))
    (should (equal (magent-session-to-gptel-prompt-list
                    (magent-session-get))
                   nil))))

(ert-deftest magent-test-agent-process-does-not-limit-tool-rounds-from-agent-steps ()
  "Test agent step metadata does not impose a tool-loop limit."
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
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) nil)))
      (magent-session-reset)
      (magent-agent-process "Hello" nil agent))
    (should (magent-agent-loop-p captured))))

(ert-deftest magent-test-agent-process-continues-after-tool-output ()
  "Test tool output is recorded before the next sampling request."
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
          (magent-tool-runtime-create
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function (lambda (sexp) (format "eval:%s" sexp))
           :async nil)))
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) (list tool-runtime)))
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
                      (funcall callback "Done." '(:content "Done.")))
                     (_
                      (error "unexpected sampling request %d" call-count))))))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
       "Run eval"
       (lambda (result) (setq response result))
       agent nil nil nil nil nil nil request-state))
    (should (= call-count 2))
    (should (equal response "Checking buffers. Done."))
    (let ((second-prompt (car sampled-prompts)))
      (should (equal second-prompt
                     '((prompt . "Run eval")
                       (tool :id "call-1"
                             :name "emacs_eval"
                             :args (:sexp "(+ 1 2)")
                             :result "eval:(+ 1 2)")))))))

(ert-deftest magent-test-agent-process-retries-empty-after-tool-output ()
  "Test empty assistant completion after tool output gets one recovery retry."
  (let* ((gptel-backend (gptel-make-openai "test" :key "test-key"))
         (gptel-model 'gpt-4o-mini)
         (magent-max-sampling-requests 0)
         (call-count 0)
         (sampled-prompts nil)
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
          (magent-tool-runtime-create
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function (lambda (_sexp) "eval:3")
           :async nil)))
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) (list tool-runtime)))
              ((symbol-function 'gptel-request)
               (lambda (prompt &rest kwargs)
                 (cl-incf call-count)
                 (push prompt sampled-prompts)
                 (push gptel-use-tools sampled-tool-use)
                 (push (plist-get kwargs :stream) sampled-streams)
                 (let ((callback (plist-get kwargs :callback)))
                   (pcase call-count
                     (1
                      (funcall
                       callback
                       '(tool-call . ((nil ("(+ 1 2)") nil
                                           (:id "call-1"
                                            :name "emacs_eval"
                                            :args (:sexp "(+ 1 2)")))))
                       '(:tool-use t)))
                     (2
                      (funcall callback t '(:content "")))
                     (3
                      (funcall callback "Final answer." '(:content "Final answer.")))
                     (_
                      (error "unexpected sampling request %d" call-count))))))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
       "Run eval"
       (lambda (result) (setq response result))
       agent nil nil nil nil nil nil request-state))
    (should (= call-count 3))
    (should (equal response "Final answer."))
    (should (equal (nreverse sampled-tool-use) '(t t nil)))
    (should (equal (nreverse sampled-streams) '(t t nil)))
    (let ((final-prompt (car sampled-prompts)))
      (should (equal final-prompt
                     `((prompt . "Run eval")
                       (tool :id "call-1"
                             :name "emacs_eval"
                             :args (:sexp "(+ 1 2)")
                             :result "eval:3")
                       (prompt
                        . ,(magent-agent--empty-final-response-retry-prompt))))))
    (let ((turn (car (magent-thread-turns
                      (magent-session-thread-ledger session)))))
      (should (eq (magent-thread-turn-status turn) 'completed)))))

(ert-deftest magent-test-agent-process-stops-after-empty-final-retry ()
  "Test empty post-tool final-response retry fails visibly after one retry."
  (let* ((gptel-backend (gptel-make-openai "test" :key "test-key"))
         (gptel-model 'gpt-4o-mini)
         (magent-max-sampling-requests 0)
         (call-count 0)
         (sampled-prompts nil)
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
          (magent-tool-runtime-create
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function (lambda (_sexp) "eval:3")
           :async nil)))
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) (list tool-runtime)))
              ((symbol-function 'gptel-request)
               (lambda (prompt &rest kwargs)
                 (cl-incf call-count)
                 (push prompt sampled-prompts)
                 (push gptel-use-tools sampled-tool-use)
                 (push (plist-get kwargs :stream) sampled-streams)
                 (let ((callback (plist-get kwargs :callback)))
                   (pcase call-count
                     (1
                      (funcall
                       callback
                       '(tool-call . ((nil ("(+ 1 2)") nil
                                           (:id "call-1"
                                            :name "emacs_eval"
                                            :args (:sexp "(+ 1 2)")))))
                       '(:tool-use t)))
                     ((or 2 3)
                      (funcall callback t '(:content "")))
                     (_
                      (error "unexpected sampling request %d" call-count))))))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
       "Run eval"
       (lambda (result) (setq response result))
       agent nil nil nil nil nil nil request-state))
    (should (= call-count 3))
    (should (magent-agent-result-p response))
    (should-not (magent-agent-result-success-p response))
    (should (equal
             (magent-agent-result-content-string response)
             "Error: Model returned an empty final response after tool output retry."))
    (should (eq (plist-get (magent-agent-result-metadata response) :status)
                'empty-final-response-retry))
    (should (equal (nreverse sampled-tool-use) '(t t nil)))
    (should (equal (nreverse sampled-streams) '(t t nil)))
    (let ((final-prompt (car sampled-prompts)))
      (should (equal (car (last final-prompt))
                     (cons
                      'prompt
                      (magent-agent--empty-final-response-retry-prompt)))))))

(ert-deftest magent-test-agent-metadata-without-retry-flags-drops-circular ()
  "Test retry metadata cleanup does not loop on circular metadata."
  (let ((metadata (list :temperature 0.3 :final-response-retry t)))
    (setcdr (last metadata) metadata)
    (should-not
     (magent-agent--metadata-without-retry-flags metadata))))

(ert-deftest magent-test-agent-metadata-without-retry-flags-preserves-normal ()
  "Test retry metadata cleanup preserves ordinary non-retry metadata."
  (should
   (equal
    (magent-agent--metadata-without-retry-flags
     '(:temperature 0.3
       :final-response-retry t
       :top-p 0.8
       :disable-provider-tools t
       :effort xhigh))
    '(:temperature 0.3 :top-p 0.8 :effort xhigh))))

(ert-deftest magent-test-agent-abort-error-detects-duplicate-status ()
  "Test provider abort detection scans duplicate status metadata keys."
  (let ((event
         (magent-llm-error-event
          "Request aborted"
          '(:provider gptel
            :status "HTTP/1.1 200 Connection established"
            :http-status "200"
            :status abort))))
    (should (magent-agent--abort-error-event-p event))))

(ert-deftest magent-test-agent-process-runs-textual-tool-call-from-empty-retry ()
  "Test textual DSML tool calls from an empty-response retry keep the loop going."
  (let* ((gptel-backend (gptel-make-openai "test" :key "test-key"))
         (gptel-model 'gpt-4o-mini)
         (magent-max-sampling-requests 0)
         (call-count 0)
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
          (magent-tool-runtime-create
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function (lambda (sexp) (format "eval:%s" sexp))
           :async nil))
         (textual-tool-call
          (concat
           "<｜｜DSML｜｜tool_calls>\n"
           "<｜｜DSML｜｜invoke name=\"emacs_eval\">\n"
           "<｜｜DSML｜｜parameter name=\"sexp\" string=\"true\">"
           "(+ 40 2)"
           "</｜｜DSML｜｜parameter>\n"
           "</｜｜DSML｜｜invoke>\n"
           "</｜｜DSML｜｜tool_calls>")))
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) (list tool-runtime)))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (cl-incf call-count)
                 (push gptel-use-tools sampled-tool-use)
                 (push (plist-get kwargs :stream) sampled-streams)
                 (let ((callback (plist-get kwargs :callback)))
                   (pcase call-count
                     (1
                      (funcall
                       callback
                       '(tool-call . ((nil ("(+ 1 2)") nil
                                           (:id "call-1"
                                            :name "emacs_eval"
                                            :args (:sexp "(+ 1 2)")))))
                       '(:tool-use t)))
                     (2
                      (funcall callback t '(:content "")))
                     (3
                      (funcall callback textual-tool-call
                               (list :content textual-tool-call)))
                     (4
                      (funcall callback "Final answer."
                               '(:content "Final answer.")))
                     (_
                      (error "unexpected sampling request %d" call-count))))))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
       "Run eval"
       (lambda (result) (setq response result))
       agent nil nil nil nil nil nil request-state))
    (should (= call-count 4))
    (should (equal response "Final answer."))
    (should (equal (nreverse sampled-tool-use) '(t t nil t)))
    (should (equal (nreverse sampled-streams) '(t t nil t)))
    (let* ((turn (car (magent-thread-turns
                       (magent-session-thread-ledger session))))
           (tool-items
            (cl-remove-if-not
             (lambda (item)
               (eq (magent-thread-item-type item) 'tool))
             (magent-thread-turn-items turn))))
      (should (= (length tool-items) 2))
      (should (equal
               (magent-thread-item-output (cadr tool-items))
               "eval:(+ 40 2)")))))

(ert-deftest magent-test-agent-process-async-continuation-preserves-tools ()
  "Test async callbacks keep request tools after dynamic gptel bindings unwind."
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
          (magent-tool-runtime-create
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function (lambda (sexp) (format "eval:%s" sexp))
           :async nil))
         (textual-tool-call
          (concat
           "<｜｜DSML｜｜tool_calls>\n"
           "<｜｜DSML｜｜invoke name=\"emacs_eval\">\n"
           "<｜｜DSML｜｜parameter name=\"sexp\" string=\"true\">"
           "(+ 40 2)"
           "</｜｜DSML｜｜parameter>\n"
           "</｜｜DSML｜｜invoke>\n"
           "</｜｜DSML｜｜tool_calls>")))
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) (list tool-runtime)))
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
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
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
      (should (= call-count 3))
      (funcall (nth 2 callbacks) textual-tool-call
               (list :content textual-tool-call))
      (should (= call-count 4))
      (funcall (nth 3 callbacks) "Final answer."
               '(:content "Final answer.")))
    (should (equal response "Final answer."))
    (should (equal (nreverse sampled-tool-use) '(t t nil t)))
    (should (equal (nreverse sampled-streams) '(t t nil t)))
    (let* ((turn (car (magent-thread-turns
                       (magent-session-thread-ledger session))))
           (tool-items
            (cl-remove-if-not
             (lambda (item)
               (eq (magent-thread-item-type item) 'tool))
             (magent-thread-turn-items turn))))
      (should (= (length tool-items) 2))
      (should (equal
               (magent-thread-item-output (cadr tool-items))
               "eval:(+ 40 2)")))))

(ert-deftest magent-test-agent-process-retries-post-tool-reasoning-stall ()
  "Test post-tool reasoning-only stalls are retried as final responses."
  (let* ((gptel-backend (gptel-make-openai "test" :key "test-key"))
         (gptel-model 'gpt-4o-mini)
         (magent-max-sampling-requests 0)
         (magent-post-tool-reasoning-idle-retry-delay 0.01)
         (call-count 0)
         (callbacks nil)
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
          (magent-tool-runtime-create
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function (lambda (_sexp) "eval:3")
           :async nil)))
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) (list tool-runtime)))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (cl-incf call-count)
                 (setq callbacks
                       (append callbacks
                               (list (plist-get kwargs :callback))))
                 (push gptel-use-tools sampled-tool-use)))
              ((symbol-function 'gptel-abort) #'ignore)
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
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
      (funcall (nth 1 callbacks)
               '(reasoning . "I have enough information.")
               '(:stream t))
      (funcall (nth 1 callbacks)
               '(reasoning . t)
               '(:stream t))
      (cl-loop repeat 20
               until (= call-count 3)
               do (accept-process-output nil 0.01))
      (should (= call-count 3))
      (funcall (nth 2 callbacks) "Final answer."
               '(:content "Final answer.")))
    (should (equal response "Final answer."))
    (should (equal (nreverse sampled-tool-use) '(t t nil)))))

(ert-deftest magent-test-agent-process-retries-post-tool-reasoning-stream ()
  "Test continuous post-tool reasoning without text is eventually retried."
  (let* ((gptel-backend (gptel-make-openai "test" :key "test-key"))
         (gptel-model 'gpt-4o-mini)
         (magent-max-sampling-requests 0)
         (magent-post-tool-reasoning-idle-retry-delay nil)
         (magent-post-tool-reasoning-max-duration 0.01)
         (call-count 0)
         (callbacks nil)
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
          (magent-tool-runtime-create
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function (lambda (_sexp) "eval:3")
           :async nil)))
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) (list tool-runtime)))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (cl-incf call-count)
                 (setq callbacks
                       (append callbacks
                               (list (plist-get kwargs :callback))))
                 (push gptel-use-tools sampled-tool-use)))
              ((symbol-function 'gptel-abort) #'ignore)
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
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
      (funcall (nth 1 callbacks)
               '(reasoning . "still thinking")
               '(:stream t))
      (cl-loop repeat 20
               until (= call-count 3)
               do (accept-process-output nil 0.01))
      (should (= call-count 3))
      (funcall (nth 2 callbacks) "Final answer."
               '(:content "Final answer.")))
    (should (equal response "Final answer."))
    (should (equal (nreverse sampled-tool-use) '(t t nil)))))

(ert-deftest magent-test-agent-process-retries-post-tool-reasoning-stream-in-callback ()
  "Test post-tool reasoning deadline is enforced from streaming callbacks."
  (let* ((gptel-backend (gptel-make-openai "test" :key "test-key"))
         (gptel-model 'gpt-4o-mini)
         (magent-max-sampling-requests 0)
         (magent-post-tool-reasoning-idle-retry-delay nil)
         (magent-post-tool-reasoning-max-duration 1)
         (fake-time 0.0)
         (call-count 0)
         (callbacks nil)
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
          (magent-tool-runtime-create
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function (lambda (_sexp) "eval:3")
           :async nil)))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _time) fake-time))
              ((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) (list tool-runtime)))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (cl-incf call-count)
                 (setq callbacks
                       (append callbacks
                               (list (plist-get kwargs :callback))))
                 (push gptel-use-tools sampled-tool-use)))
              ((symbol-function 'gptel-abort) #'ignore)
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
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
      (setq fake-time 10.0)
      (funcall (nth 1 callbacks)
               '(reasoning . "still thinking")
               '(:stream t))
      (should (= call-count 2))
      (setq fake-time 11.1)
      (funcall (nth 1 callbacks)
               '(reasoning . "still thinking more")
               '(:stream t))
      (should (= call-count 3))
      (funcall (nth 2 callbacks) "Final answer."
               '(:content "Final answer.")))
    (should (equal response "Final answer."))
    (should (equal (nreverse sampled-tool-use) '(t t nil)))))

(ert-deftest magent-test-agent-process-strict-retries-final-retry-reasoning ()
  "Test final-response retry reasoning-only streams get one strict retry."
  (let* ((gptel-backend (gptel-make-openai "test" :key "test-key"))
         (gptel-model 'gpt-4o-mini)
         (magent-max-sampling-requests 0)
         (magent-post-tool-reasoning-idle-retry-delay nil)
         (magent-post-tool-reasoning-max-duration 1)
         (fake-time 0.0)
         (call-count 0)
         (callbacks nil)
         (sampled-tool-use nil)
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
          (magent-tool-runtime-create
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function (lambda (_sexp) "eval:3")
           :async nil)))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _time) fake-time))
              ((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) (list tool-runtime)))
              ((symbol-function 'gptel-request)
               (lambda (prompt &rest kwargs)
                 (cl-incf call-count)
                 (push prompt sampled-prompts)
                 (setq callbacks
                       (append callbacks
                               (list (plist-get kwargs :callback))))
                 (push gptel-use-tools sampled-tool-use)))
              ((symbol-function 'gptel-abort) #'ignore)
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
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
      (should (= call-count 3))
      (setq fake-time 10.0)
      (funcall (nth 2 callbacks)
               '(reasoning . "still thinking during final retry")
               '(:stream t))
      (should (= call-count 3))
      (setq fake-time 11.1)
      (funcall (nth 2 callbacks)
               '(reasoning . "still no final answer")
               '(:stream t))
      (should (= call-count 4))
      (funcall (nth 3 callbacks) "Final answer."
               '(:content "Final answer.")))
    (should (equal response "Final answer."))
    (should (equal (nreverse sampled-tool-use) '(t t nil nil)))
    (let ((strict-prompt (car sampled-prompts)))
      (should (equal (car (last strict-prompt))
                     (cons
                      'prompt
                      (magent-agent--strict-final-response-retry-prompt)))))))

(ert-deftest magent-test-agent-process-fails-after-empty-strict-final-retry ()
  "Test an empty strict final-response retry fails with strict status."
  (let* ((gptel-backend (gptel-make-openai "test" :key "test-key"))
         (gptel-model 'gpt-4o-mini)
         (magent-max-sampling-requests 0)
         (magent-post-tool-reasoning-idle-retry-delay nil)
         (magent-post-tool-reasoning-max-duration 1)
         (fake-time 0.0)
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
          (magent-tool-runtime-create
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function (lambda (_sexp) "eval:3")
           :async nil)))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _time) fake-time))
              ((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) (list tool-runtime)))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (cl-incf call-count)
                 (setq callbacks
                       (append callbacks
                               (list (plist-get kwargs :callback))))
                 (push gptel-use-tools sampled-tool-use)
                 (push (plist-get kwargs :stream) sampled-streams)))
              ((symbol-function 'gptel-abort) #'ignore)
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
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
      (should (= call-count 3))
      (setq fake-time 10.0)
      (funcall (nth 2 callbacks)
               '(reasoning . "still thinking during final retry")
               '(:stream t))
      (should (= call-count 3))
      (setq fake-time 11.1)
      (funcall (nth 2 callbacks)
               '(reasoning . "still no final answer")
               '(:stream t))
      (should (= call-count 4))
      (funcall (nth 3 callbacks) t '(:content "")))
    (should (= call-count 4))
    (should (magent-agent-result-p response))
    (should-not (magent-agent-result-success-p response))
    (should (equal
             (magent-agent-result-content-string response)
             "Error: Model returned no assistant text after strict final-response retry."))
    (should (eq (plist-get (magent-agent-result-metadata response) :status)
                'strict-final-response-retry))
    (should (eq (plist-get (magent-agent-result-metadata response) :reason)
                'completed-empty))
    (should (equal (nreverse sampled-tool-use) '(t t nil nil)))
    (should (equal (nreverse sampled-streams) '(t t nil nil)))))

(ert-deftest magent-test-agent-process-forces-final-at-configured-sampling-budget ()
  "Test the optional sampling budget permits one no-tool final request."
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
          (magent-tool-runtime-create
           :name "emacs_eval"
           :description "Eval"
           :args (list '(:name "sexp" :type string))
           :function (lambda (_sexp) "ok")
           :async nil)))
    (cl-letf (((symbol-function 'magent-session-get)
               (lambda () session))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) (list tool-runtime)))
              ((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (cl-incf call-count)
                 (push gptel-use-tools sampled-tool-use)
                 (let ((callback (plist-get kwargs :callback)))
                   (pcase call-count
                     (1
                      (funcall
                       callback
                       '(tool-call . ((nil ("(+ 1 2)") nil
                                           (:id "call-1"
                                            :name "emacs_eval"
                                            :args (:sexp "(+ 1 2)")))))
                       '(:tool-use t)))
                     (2
                      (funcall callback "Budget final." '(:content "Budget final.")))
                     (_
                      (error "unexpected sampling request %d" call-count))))))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (magent-agent-process
       "Run eval"
       (lambda (result) (setq response result))
       agent nil nil nil nil nil nil request-state))
    (should (= call-count 2))
    (should (equal (nreverse sampled-tool-use) '(t nil)))
    (should (equal response "Budget final."))))

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

(ert-deftest magent-test-frontmatter-decodes-yaml-string-escapes ()
  "Quoted YAML scalars and flow lists survive parsing without comma splitting."
  (require 'magent-file-loader)
  (let* ((content
          (concat "---\n"
                  "description: \"Say \\\"hi\\\", then C:\\\\tmp\\nnext\"\n"
                  "tools: [bash, read]\n"
                  "---\n"))
         (fm (car (magent-file-loader-parse-frontmatter content))))
    (should (equal (plist-get fm :description)
                   "Say \"hi\", then C:\\tmp\nnext"))
    (should (equal (plist-get fm :tools) '("bash" "read")))))

(ert-deftest magent-test-frontmatter-keeps-commas-in-scalar-fields ()
  "Legacy comma shorthand applies only to declared list fields."
  (require 'magent-file-loader)
  (let* ((content "---\ndescription: Fast, focused, composable\n---\n")
         (fm (car (magent-file-loader-parse-frontmatter content))))
    (should (equal (plist-get fm :description)
                   "Fast, focused, composable"))))

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
  (require 'magent-markdown-to-org)
  (let ((converted (magent-markdown-to-org-convert-string "Plain **foo** and *bar* with `baz`.")))
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

(ert-deftest magent-test-permission-resource-globs-keep-specific-order ()
  "Specific resource rules are first-match; bare `*' remains fallback."
  (require 'magent-permission)
  (let ((rules '((read . ((* . allow)
                           ("src/**" . ask)
                           ("src/private/**" . deny))))))
    ;; The catch-all is a fallback even when declared first.  Between the two
    ;; matching specific rules, the first rule wins.
    (should (eq (magent-permission-resolve
                 rules 'read "src/private/secret.el")
                'ask))
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

(ert-deftest magent-test-permission-plan-merge-replaces-wildcard-fallback ()
  "Plan's string wildcard cannot coexist with defaults' symbol wildcard."
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

(ert-deftest magent-test-json-safe-tool-args-preserves-nil-values ()
  "Test Lisp nil values are kept distinct from provider JSON null sentinels."
  (require 'magent-json)
  (should
   (equal (magent-json-safe-tool-args
           '(:optional nil :missing :null :name "value"))
          '(:optional :null :name "value"))))

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

(ert-deftest magent-test-session-to-gptel-prompt-list-drops-non-reusable-turns ()
  "Test empty and failed assistant turns do not leak into prompt reuse."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-session-add-message session 'user "emacs 有几个 buffer")
    (magent-session-add-message session 'assistant "")
    (should (eq (magent-thread-turn-status
                 (car (magent-thread-turns
                       (magent-session-thread-ledger session))))
                'completed))
    (magent-session-add-message session 'user "magent 有几个 skills")
    (magent-session-add-message
     session 'assistant
     "Error: provider request failed.")
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
     "Error: provider request failed.")
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
     (equal (magent-session-to-gptel-prompt-list session)
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
     (equal (magent-session-to-gptel-prompt-list session)
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
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) nil))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
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

(ert-deftest magent-test-agent-process-startup-error-respects-context-ownership ()
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
              ((symbol-function 'magent-capability-resolve-for-turn)
               (lambda (&rest _args) nil))
              ((symbol-function 'magent-memory-system-message)
               (lambda (&rest _args) nil))
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (permission)
                 (push permission exposed-permissions)
                 nil))
              ((symbol-function 'magent-agent-loop-start)
               (lambda (_loop) (error "sampler startup failed")))
              ((symbol-function 'magent-log) #'ignore))
      (should-error
       (magent-agent-process
        "owned" nil agent nil nil nil nil nil nil owned-state))
      (should-error
       (magent-agent-process
        "inherited" nil agent nil inherited-event nil nil nil nil
        inherited-state)))
    (should (= (length ended) 1))
    (should (eq (caar ended) owned-event))
    (should (eq (cadar ended) 'failed))
    (should (equal (nreverse exposed-permissions)
                   (list request-permission agent-permission)))))

(ert-deftest magent-test-agent-process-system-prompt-includes-scope-root ()
  "Test project-scoped runtime turns tell the model the current repo root."
  (require 'magent-agent)
  (require 'magent-capability)
  (let* ((backend (gptel-make-openai "scope-root" :key "key"))
         (gptel-backend backend)
         (gptel-model 'scope-model)
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
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) nil))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore)
              ((symbol-function 'magent-skills-get-instruction-prompts)
               (lambda (_skills) nil)))
      (magent-agent-process
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
      (should (equal (magent-request-context-project-root request-state)
                     "/tmp/project")))))

(ert-deftest magent-test-memory-scan-plan-skips-sensitive-and-org-notes ()
  "Test memory scan plans avoid secrets, custom-file contents, and Org notes."
  (require 'magent-memory)
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
  (require 'magent-memory)
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
  (require 'magent-memory)
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
  (require 'magent-memory)
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

(ert-deftest magent-test-run-memory-init-creates-internal-session ()
  "Test `magent-run-memory-init' runs through the internal command layer."
  (require 'magent-memory)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-command-session-directory nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         operation)
    (unwind-protect
        (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                   #'ignore)
                  ((symbol-function 'magent-runtime-command-scope)
                   (lambda () 'global))
                  ((symbol-function 'magent-runtime-prepare-command-context)
                   (lambda (&optional scope) (or scope 'global)))
                  ((symbol-function 'magent-memory-run)
                   (lambda (op &rest args)
                     (setq operation op)
                     (funcall (plist-get args :notify-fn)
                              "memory init progress")
                     (funcall (plist-get args :on-complete)
                              'completed
                              "memory init complete"))))
          (magent-run-memory-init)
          (let* ((files (magent-session-list-internal-files "memory-init"))
                 (meta (magent-session--read-file-metadata-cached
                        (car files))))
            (should (eq operation 'init))
            (should (= (length files) 1))
            (should (equal (plist-get meta :kind) "internal-command"))
            (should (equal (plist-get meta :command) "memory-init"))
            (should (equal (plist-get meta :status) "completed"))
            (should (equal (plist-get meta :title)
                           "Initialize Magent Emacs profile memory"))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-memory-command-confirm-respects-bypass-permission ()
  "Test memory command confirmation reuses `magent-bypass-permission'."
  (require 'magent-memory)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-command-session-directory nil)
         (magent-bypass-permission t)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (session (magent-session-create))
         (id (magent-session-get-id session))
         (scope (magent-session-internal-scope id "memory-init" 'global))
         (spec (magent-command-spec-create
                :name "memory-init"
                :title "Initialize Magent Emacs profile memory"))
         (context (magent-command-context-create
                   :spec spec
                   :session session
                   :scope scope
                   :origin-scope 'global))
         approved)
    (unwind-protect
        (progn
          (magent-session-install scope session)
          (cl-letf (((symbol-function 'magent-memory--interactive-confirm)
                     (lambda (&rest _)
                       (error "interactive confirm should be bypassed"))))
            (funcall (magent-memory--command-confirm-provider context 'init)
                     nil
                     (lambda (value)
                       (setq approved value))))
          (let* ((thread (magent-session-thread-ledger session))
                 (items (apply #'append
                               (mapcar #'magent-thread-turn-items
                                       (magent-thread-turns thread))))
                 (approval (cl-find
                            "memory_approval" items
                            :key #'magent-thread-item-name
                            :test #'equal)))
            (should approved)
            (should approval)
            (should (string-match-p
                     "magent-bypass-permission"
                     (or (magent-thread-item-output approval) "")))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-command-quit-cancels-and-restores-parent-session ()
  "Test `quit' cancels an internal command and restores its parent session."
  (require 'magent-command)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-command-session-directory nil)
         (magent-command--registry (make-hash-table :test #'equal))
         (magent-command--active-contexts (make-hash-table :test #'equal))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (parent (magent-session-create :id "session-parent"))
         quit-signalled)
    (unwind-protect
        (progn
          (magent-session-install 'global parent)
          (magent-command-register
           "quit-test"
           :title "Quit test"
           :runner-type 'pipeline
           :runner (lambda (_context) (signal 'quit nil)))
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-runtime-command-scope)
                     (lambda () 'global))
                    ((symbol-function 'magent-runtime-prepare-command-context)
                     (lambda (&optional scope) (or scope 'global)))
                    ((symbol-function
                      'magent-session-save-deferred-for-session)
                     #'ignore))
            (condition-case nil
                (magent-command-run "quit-test")
              (quit (setq quit-signalled t))))
          (should quit-signalled)
          (should (eq magent--current-session parent))
          (should (eq magent-session--current-scope 'global))
          (should (= (hash-table-count magent-command--active-contexts) 0))
          (let* ((files (magent-session-list-internal-files "quit-test"))
                 (file (car files))
                 (meta (magent-session--read-file-metadata-cached file))
                 (loaded (magent-session-read-file file))
                 (session (plist-get loaded :session))
                 (turn (car (magent-thread-turns
                             (magent-session-thread-ledger session)))))
            (should (= (length files) 1))
            (should (equal (plist-get meta :status) "cancelled"))
            (should (eq (magent-thread-turn-status turn) 'interrupted))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-command-async-completion-preserves-current-session ()
  "Test late command completion does not restore an invocation-time session."
  (require 'magent-command)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-command-session-directory nil)
         (magent-command--registry (make-hash-table :test #'equal))
         (magent-command--active-contexts (make-hash-table :test #'equal))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (parent (magent-session-create :id "session-parent"))
         (new-current (magent-session-create :id "session-new-current"))
         captured-context)
    (unwind-protect
        (progn
          (magent-session-install 'global parent)
          (magent-command-register
           "async-test"
           :title "Async test"
           :runner-type 'pipeline
           :runner (lambda (context) (setq captured-context context)))
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-runtime-command-scope)
                     (lambda () 'global))
                    ((symbol-function 'magent-runtime-prepare-command-context)
                     (lambda (&optional scope) (or scope 'global)))
                    ((symbol-function
                      'magent-session-save-deferred-for-session)
                     #'ignore))
            (magent-command-run "async-test")
            (should (eq magent--current-session parent))
            (magent-session-install "/tmp/magent-other-project" new-current)
            (magent-command-complete
             captured-context 'completed "Async test complete")
            (should (eq magent--current-session new-current))
            (should (equal magent-session--current-scope
                           "/tmp/magent-other-project"))
            (should (= (hash-table-count
                        magent-command--active-contexts)
                       0))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-command-agent-loop-session-is-cancellable ()
  "Test agent-loop commands retain a registered runtime cancellation handle."
  (require 'magent-command)
  (require 'magent-runtime-api)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-command-session-directory nil)
         (magent-command--registry (make-hash-table :test #'equal))
         (magent-command--active-contexts (make-hash-table :test #'equal))
         (magent-runtime-api--sessions (make-hash-table :test #'equal))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (parent (magent-session-create :id "session-parent"))
         completion
         captured-context
         cancelled-runtime)
    (unwind-protect
        (progn
          (magent-session-install 'global parent)
          (magent-command-register
           "agent-loop-test"
           :title "Agent loop test"
           :runner-type 'agent-loop
           :runner (magent-command--agent-loop-runner "diagnose"))
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-runtime-command-scope)
                     (lambda () 'global))
                    ((symbol-function 'magent-runtime-prepare-command-context)
                     (lambda (&optional scope) (or scope 'global)))
                    ((symbol-function 'magent-agent-registry-get)
                     (lambda (_name) nil))
                    ((symbol-function 'magent-agent-registry-get-default)
                     (lambda () nil))
                    ((symbol-function 'magent-runtime-submit)
                     (lambda (runtime-session _prompt &rest args)
                       (setq captured-context
                             (gethash (magent-runtime-session-id runtime-session)
                                      magent-command--active-contexts)
                             completion (plist-get args :on-complete))
                       "submission-test"))
                    ((symbol-function 'magent-runtime-cancel)
                     (lambda (runtime-session)
                       (setq cancelled-runtime runtime-session)
                       (funcall completion 'cancelled "Active turn cancelled")
                       1))
                    ((symbol-function
                      'magent-session-save-deferred-for-session)
                     #'ignore))
            (magent-command-run "agent-loop-test")
            (let* ((session-id
                    (magent-command--context-session-id captured-context))
                   (runtime-session
                    (magent-command-context-runtime-session captured-context)))
              (should (eq magent--current-session parent))
              (should (eq (magent-runtime-session-from-id session-id)
                          runtime-session))
              (should (equal
                       (magent-command-context-submission-id captured-context)
                       "submission-test"))
              (should (= (magent-command-cancel session-id) 1))
              (should (eq cancelled-runtime runtime-session))
              (should (eq magent--current-session parent))
              (should (= (hash-table-count
                          magent-command--active-contexts)
                         0)))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-command-session-label-disambiguates-same-second-runs ()
  "Test internal session labels include their unique session ids."
  (require 'magent-command)
  (let ((file-a "/tmp/session-20260710-120000.json")
        (file-b "/tmp/session-20260710-120000-01.json"))
    (cl-letf (((symbol-function 'magent-session--read-file-metadata-cached)
               (lambda (_file)
                 '(:command "doctor" :status "completed" :title "Doctor")))
              ((symbol-function 'magent-session--format-display-timestamp)
               (lambda (_file) "2026-07-10 12:00:00")))
      (let ((label-a (magent-command--session-label file-a))
            (label-b (magent-command--session-label file-b)))
        (should-not (equal label-a label-b))
        (should (string-match-p "session-20260710-120000>" label-a))
        (should (string-match-p "session-20260710-120000-01>" label-b))))))

(ert-deftest magent-test-command-session-viewer-folds-activity-after-result ()
  "Test internal session viewer leads with result and hides activity details."
  (require 'magent-command)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-command-session-directory nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (session (magent-session-create))
         (id (magent-session-get-id session))
         (scope (magent-session-internal-scope id "viewer-test" 'global))
         (spec (magent-command-spec-create
                :name "viewer-test" :title "Viewer test"))
         (context (magent-command-context-create
                   :spec spec :session session :scope scope
                   :origin-scope 'global))
         buffer)
    (unwind-protect
        (progn
          (dolist (entry '((kind . "internal-command")
                           (command . "viewer-test")
                           (title . "Viewer test")
                           (status . "running")
                           (origin-scope . global)))
            (magent-session-set-metadata-value session (car entry) (cdr entry)))
          (magent-session-install scope session)
          (magent-command-record-tool
           context "doctor_probe" '(:probe "core") "bounded detail")
          (magent-command-record-message
           context 'assistant "* Diagnosis\n** Summary\nVisible result" nil
           (list :source 'magent-doctor-final))
          (magent-command-complete
           context 'completed "Viewer complete" :record-message nil)
          (let ((file (car (magent-session-list-internal-files "viewer-test"))))
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (value &rest _args) value)))
              (magent-command-show-session file))
            (setq buffer
                  (get-buffer
                   (format "*Magent Internal Session: %s*"
                           (file-name-base file)))))
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (should (derived-mode-p 'magent-command-session-mode))
            (should (< (save-excursion
                         (goto-char (point-min))
                         (search-forward "* Result"))
                       (save-excursion
                         (goto-char (point-min))
                         (search-forward "* Activity"))))
            (let ((detail-position
                   (save-excursion
                     (goto-char (point-min))
                     (search-forward "bounded detail")
                     (1- (point)))))
              (should (invisible-p detail-position))
              (magent-command-session-toggle-all)
              (should-not (invisible-p detail-position)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-doctor-probe-registration-validates-public-contract ()
  "Test public Doctor probes require stable ids, callables, and timeouts."
  (require 'magent-doctor)
  (let ((magent-doctor--registry (make-hash-table :test #'equal)))
    (should-error
     (magent-doctor-register-probe
      "Unsafe Probe" :collector #'ignore))
    (should
     (magent-doctor-register-probe
      "valid" :collector #'ignore :timeout 0))
    (should
     (magent-doctor-register-probe
      "valid-probe" :collector #'ignore :timeout 1))))

(ert-deftest magent-test-doctor-sends-one-sanitized-tool-free-request ()
  "Test Doctor never exposes probe or backend secrets to its request/session."
  (require 'magent-doctor)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-command-session-directory nil)
         (magent-command--registry (make-hash-table :test #'equal))
         (magent-command--active-contexts (make-hash-table :test #'equal))
         (magent-doctor--registry (make-hash-table :test #'equal))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (magent-bypass-permission t)
         (secret "sk-DoctorProbeCanaryAbCdEf1234567890")
         (backend-secret "sk-DoctorBackendCanaryAbCdEf1234567890")
         (gptel-backend (list :name "unsafe-test-backend"
                              :key backend-secret))
         (gptel-model 'doctor-test-model)
         (parent (magent-session-create :id "session-parent"))
         request
         (sample-count 0)
         context)
    (unwind-protect
        (progn
          (magent-session-install 'global parent)
          (magent-doctor-register-probe
           "canary"
           :description "Canary security probe"
           :collector
           (lambda (_context _state)
             `((authorization . ,secret)
               (safe . "diagnostic value")))
           :data-categories '(runtime)
           :required t)
          (magent-command-register
           "doctor"
           :title "Run Magent Doctor"
           :runner-type 'diagnostic-pipeline
           :runner #'magent-doctor--runner)
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-runtime-command-scope)
                     (lambda () 'global))
                    ((symbol-function 'magent-runtime-prepare-command-context)
                     (lambda (&optional scope) (or scope 'global)))
                    ((symbol-function 'magent-runtime-submit)
                     (lambda (&rest _)
                       (error "Doctor must not use runtime agent loop")))
                    ((symbol-function 'magent-session-save-deferred-for-session)
                     #'ignore)
                    ((symbol-function 'magent-llm-gptel-sample)
                     (lambda (value)
                       (setq request value)
                       (cl-incf sample-count)
                       (funcall
                        (magent-llm-request-callback value)
                        (magent-llm-completed-event
                         (concat
                          "* Diagnosis\n"
                          "** Summary\nNo credential exposure.\n"
                          "** Findings\n- low [canary] Safe.\n"
                          "** Recommended Actions\n- None.\n"
                          "** Limitations\n- Synthetic probe.")))
                       (generate-new-buffer " *doctor-finished-test*"))))
            (setq context (magent-command-run "doctor")))
          (should (= sample-count 1))
          (should-not (magent-llm-request-tools request))
          (should (plist-get (magent-llm-request-metadata request)
                             :disable-provider-tools))
          (should-not (string-match-p
                       (regexp-quote secret)
                       (magent-llm-request-prompt request)))
          (should-not (string-match-p
                       (regexp-quote backend-secret)
                       (magent-llm-request-prompt request)))
          (should (string-match-p
                   "<redacted:authorization>"
                   (magent-llm-request-prompt request)))
          (should (magent-command-context-completed-p context))
          (should (eq magent--current-session parent))
          (let* ((file (car (magent-session-list-internal-files "doctor")))
                 (text (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))
                 (meta (magent-session--read-file-metadata-cached file)))
            (should (equal (plist-get meta :status) "completed"))
            (should-not (string-match-p (regexp-quote secret) text))
            (should-not (string-match-p (regexp-quote backend-secret) text)))
          (should-not
           (string-match-p
            (regexp-quote secret)
            (format "%S" (magent-session-messages parent)))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-doctor-unsafe-probe-fails-closed-before-sampling ()
  "Test a live object from a probe prevents any provider request."
  (require 'magent-doctor)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-command-session-directory nil)
         (magent-command--registry (make-hash-table :test #'equal))
         (magent-command--active-contexts (make-hash-table :test #'equal))
         (magent-doctor--registry (make-hash-table :test #'equal))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (magent-bypass-permission t)
         (parent (magent-session-create :id "session-parent"))
         sampled)
    (unwind-protect
        (progn
          (magent-session-install 'global parent)
          (magent-doctor-register-probe
           "unsafe"
           :collector (lambda (_context _state) (current-buffer))
           :required t)
          (magent-command-register
           "doctor" :title "Run Magent Doctor"
           :runner-type 'diagnostic-pipeline
           :runner #'magent-doctor--runner)
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-runtime-command-scope)
                     (lambda () 'global))
                    ((symbol-function 'magent-runtime-prepare-command-context)
                     (lambda (&optional scope) (or scope 'global)))
                    ((symbol-function 'magent-session-save-deferred-for-session)
                     #'ignore)
                    ((symbol-function 'magent-llm-gptel-sample)
                     (lambda (_request) (setq sampled t))))
            (magent-command-run "doctor"))
          (should-not sampled)
          (let* ((file (car (magent-session-list-internal-files "doctor")))
                 (meta (magent-session--read-file-metadata-cached file)))
            (should (equal (plist-get meta :status) "failed"))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-doctor-active-request-is-cancellable ()
  "Test Doctor retains and aborts its direct gptel request handle."
  (require 'magent-doctor)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-command-session-directory nil)
         (magent-command--registry (make-hash-table :test #'equal))
         (magent-command--active-contexts (make-hash-table :test #'equal))
         (magent-doctor--registry (make-hash-table :test #'equal))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (magent-bypass-permission t)
         (parent (magent-session-create :id "session-parent"))
         (request-buffer (generate-new-buffer " *doctor-cancel-test*"))
         aborted
         context)
    (unwind-protect
        (progn
          (magent-session-install 'global parent)
          (magent-doctor-register-probe
           "safe" :collector (lambda (_context _state) '((ok . t)))
           :required t)
          (magent-command-register
           "doctor" :title "Run Magent Doctor"
           :runner-type 'diagnostic-pipeline
           :runner #'magent-doctor--runner)
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-runtime-command-scope)
                     (lambda () 'global))
                    ((symbol-function 'magent-runtime-prepare-command-context)
                     (lambda (&optional scope) (or scope 'global)))
                    ((symbol-function 'magent-session-save-deferred-for-session)
                     #'ignore)
                    ((symbol-function 'magent-llm-gptel-sample)
                     (lambda (_request) request-buffer))
                    ((symbol-function 'gptel-abort)
                     (lambda (buffer) (setq aborted buffer))))
            (setq context (magent-command-run "doctor"))
            (should-not (magent-command-context-completed-p context))
            (should (gethash (magent-command--context-session-id context)
                             magent-command--active-contexts))
            (should (magent-command-cancel
                     (magent-command--context-session-id context))))
          (should (eq aborted request-buffer))
          (should-not (buffer-live-p request-buffer))
          (should (magent-command-context-completed-p context))
          (should (eq magent--current-session parent))
          (let* ((file (car (magent-session-list-internal-files "doctor")))
                 (meta (magent-session--read-file-metadata-cached file)))
            (should (equal (plist-get meta :status) "cancelled"))))
      (when (buffer-live-p request-buffer) (kill-buffer request-buffer))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-memory-system-message-selects-relevant-sections ()
  "Test prompt-time memory injection selects bounded relevant sections."
  (require 'magent-memory)
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

(ert-deftest magent-test-agent-process-keeps-streaming-for-tool-requests ()
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
          (magent-tool-runtime-create
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
              ((symbol-function 'magent-tool-runtime-for-permission)
               (lambda (_agent) (list tool-runtime)))
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'magent-lifecycle-events-begin-turn)
               (lambda (_title) 'turn))
              ((symbol-function 'magent-lifecycle-events-end-turn) #'ignore))
      (magent-session-reset)
      (magent-agent-process "use a tool" nil agent))
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
  "Test code-defined builtin skill registration."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (cl-letf (((symbol-function 'magent-log) #'ignore))
      (magent-skills--register-builtin))
    (should (null (magent-skills-get "emacs")))
    (let ((skill (magent-skills-get "skill-creator")))
      (should skill)
      (should (eq (magent-skill-type skill) 'instruction)))))

(ert-deftest magent-test-skills-load-order-preserves-directory-precedence ()
  "Test later skill directories override earlier directories deterministically."
  (require 'magent-skills)
  (let* ((root (make-temp-file "magent-skill-order-" t))
         ;; Deliberately choose reverse-lexical directory names so a global
         ;; pathname sort would produce the wrong winner.
         (builtin-dir (expand-file-name "z-builtin" root))
         (legacy-dir (expand-file-name "y-legacy" root))
         (canonical-dir (expand-file-name "x-canonical" root))
         (directories (list builtin-dir legacy-dir canonical-dir))
         (magent-skills--registry nil)
         (magent-skills--builtin-dir builtin-dir)
         (magent-skill-directories (list legacy-dir canonical-dir)))
    (unwind-protect
        (progn
          (cl-loop for directory in directories
                   for description in '("builtin" "legacy" "canonical")
                   do
                   (let ((skill-dir (expand-file-name "same-skill" directory)))
                     (make-directory skill-dir t)
                     (with-temp-file (expand-file-name "SKILL.md" skill-dir)
                       (insert (format
                                "---\nname: same-skill\ndescription: %s\n---\n%s\n"
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
            (magent-skill-install source))
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
            (should-error (magent-skill-install source)))
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
            (magent-skill-install source)
            (with-temp-file (expand-file-name "SKILL.md" source)
              (insert "---\nname: reinstall-skill\ndescription: Reinstall\n---\nVersion two\n"))
            (magent-skill-install source)
            (should-error (magent-skill-install other) :type 'user-error))
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
            (magent-skill-delete))
          (should (= confirm-count 1))
          (should (= reload-count 1))
          (should-not (file-symlink-p link))
          (should (file-exists-p (expand-file-name "SKILL.md" outside))))
      (when (file-symlink-p link) (delete-file link))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest magent-test-skills-load-all-includes-init-skill ()
  "Test bundled skill loading includes the init workflow skill."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (cl-letf (((symbol-function 'magent-log) #'ignore))
      (magent-skills-load-all (list magent-skills--builtin-dir)))
    (let ((skill (magent-skills-get "init")))
      (should skill)
      (should (eq (magent-skill-type skill) 'instruction))
      (should (string-match-p "Codex /init"
                              (format "%s"
                                      (or (magent-skill-description skill)
                                          ""))))
      (should (string-match-p "AGENTS.md"
                              (or (magent-skill-default-prompt skill) ""))))))

(ert-deftest magent-test-skills-load-all-includes-command-skills ()
  "Test bundled slash command skills have default prompts."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (magent-test--load-builtin-skills-only)
    (dolist (name magent-test--builtin-slash-command-names)
      (let ((skill (magent-skills-get name)))
        (should skill)
        (should (eq (magent-skill-type skill) 'instruction))
        (should-not (string-blank-p
                     (or (magent-skill-default-prompt skill) "")))))))

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
            (insert "---\nname: test-skill\ndescription: A test\ntype: instruction\ntools: bash, read\ndefault-prompt: Do the default thing.\nrequires-project: true\n---\nDo the thing."))
          (let ((skill (magent-skills-load-file skillfile)))
            (should skill)
            (should (equal (magent-skill-name skill) "test-skill"))
            (should (equal (magent-skill-description skill) "A test"))
            (should (eq (magent-skill-type skill) 'instruction))
            (should (equal (magent-skill-tools skill) '(bash read)))
            (should (equal (magent-skill-default-prompt skill)
                           "Do the default thing."))
            (should (magent-skill-requires-project skill))
            (should (string-match-p "Do the thing" (magent-skill-prompt skill)))))
      (delete-directory tmpdir t))))

(ert-deftest magent-test-skills-load-all-includes-summarize-skill ()
  "Test the bundled summarize skill is project-bound and tool-backed."
  (require 'magent-skills)
  (let ((magent-skills--registry nil))
    (magent-test--load-builtin-skills-only)
    (let ((skill (magent-skills-get "summarize")))
      (should skill)
      (should (magent-skill-requires-project skill))
      (should (memq 'write_repo_summary (magent-skill-tools skill)))
      (should-not (string-blank-p
                   (or (magent-skill-default-prompt skill) ""))))))

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
             "tools: emacs_eval\n"
             "capability: true\n"
             "source: package\n"
             "package: project\n"
             "modes: prog-mode, text-mode\n"
             "features: project\n"
             "keywords: project root, project compile\n"
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
             "files: \"*.el, init.el\"\n"
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
    (magent-test--load-builtin-capabilities-only)
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
    (magent-test--load-builtin-capabilities-only)
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
    (magent-test--load-builtin-capabilities-only)
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
    (magent-test--load-builtin-capabilities-only)
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
    (magent-test--load-builtin-capabilities-only)
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
  (should (eq (magent-tools-permission-key "write_repo_summary") 'write))
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

(ert-deftest magent-test-repo-summary-writes-and-updates-one-org-note ()
  "Test full and scoped summaries coexist in one canonical Org note."
  (require 'magent-repo-summary)
  (require 'org-element)
  (let* ((repository (magent-test--make-git-repository
                      "magent-summary-repository-"))
         (roam-directory (make-temp-file "magent-summary-roam-" t))
         (magent-org-roam-directory roam-directory)
         first-path first-id)
    (cl-letf (((symbol-function
                'magent-repo-summary--org-roam-capture-available-p)
               #'ignore))
      (unwind-protect
          (progn
            (setq first-path
                  (plist-get
                   (magent-repo-summary-write
                    repository "full"
                    "The repository exists to exercise summary writing.\n\n** Architecture\nA small test fixture.")
                   :path))
            (should (= (length (directory-files roam-directory nil
                                                 "\\.org\\'"))
                       1))
            (with-temp-buffer
              (insert-file-contents first-path)
              (should (re-search-forward
                       (concat "^#\\+title: "
                               (regexp-quote
                                (file-name-nondirectory repository))
                               "$")
                       nil t))
              (goto-char (point-min))
              (should (re-search-forward "^:REPO_PATH: " nil t))
              (should (re-search-forward
                       "^:LAST_ANALYZED_COMMIT: [0-9a-f]+$" nil t))
              (goto-char (point-min))
              (org-mode)
              (goto-char (point-min))
              (should (looking-at ":PROPERTIES:\n"))
              (setq first-id (org-id-get))
              (should (stringp first-id))
              (should (org-element-parse-buffer)))

            (magent-repo-summary-write
             repository "scoped" "Only the parser is summarized."
             "src/parser :internal:" "src/parser.el, test/parser-test.el")
            (magent-repo-summary-write
             repository "scoped" "The parser summary was updated."
             "src/parser :internal:" "src/parser.el, test/parser-test.el")
            (magent-repo-summary-write
             repository "full" "The full repository summary was updated.")

            (should (= (length (directory-files roam-directory nil
                                                 "\\.org\\'"))
                       1))
            (with-temp-buffer
              (insert-file-contents first-path)
              (org-mode)
              (goto-char (point-min))
              (should (equal (org-id-get) first-id))
              (should (= (how-many "^:ID:" (point-min) (point-max)) 1))
              (should (re-search-forward "^\\* Repository Summary$" nil t))
              (should (re-search-forward
                       "The full repository summary was updated" nil t))
              (should (re-search-forward "^\\* Scoped Summaries$" nil t))
              (should (= (how-many "^:SUMMARY_SCOPE_ID:"
                                   (point-min) (point-max))
                         1))
              (should (re-search-forward
                       "The parser summary was updated" nil t))
              (should-not
               (re-search-forward "Only the parser is summarized" nil t))))
        (delete-directory repository t)
        (delete-directory roam-directory t)))))

(ert-deftest magent-test-repo-summary-migrates-legacy-preamble-to-file-node ()
  "Test updates repair summaries whose property drawer follows Org keywords."
  (require 'magent-repo-summary)
  (let* ((repository (magent-test--make-git-repository
                      "magent-summary-legacy-repository-"))
         (root (file-truename repository))
         (name (file-name-nondirectory repository))
         (roam-directory (make-temp-file "magent-summary-legacy-roam-" t))
         (magent-org-roam-directory roam-directory)
         (path (expand-file-name (format "%s-summary.org" name)
                                 roam-directory))
         (legacy-id "a20c2e63-87d9-40af-a56d-a52f18f6f15c"))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert (format
                     (concat "#+title: %s\n\n:PROPERTIES:\n:ID: %s\n"
                             ":REPO_PATH: %s\n"
                             ":LAST_ANALYZED_COMMIT: legacy\n:END:\n\n"
                             "* Repository Summary\nLegacy summary.\n")
                     name legacy-id root)))
          (magent-repo-summary-write
           repository "full" "Migrated repository summary.")
          (with-temp-buffer
            (insert-file-contents path)
            (org-mode)
            (goto-char (point-min))
            (should (looking-at ":PROPERTIES:\n"))
            (should (equal (org-id-get) legacy-id))
            (should (re-search-forward "^#\\+title: " nil t))
            (should (re-search-forward "Migrated repository summary" nil t))
            (should-not (re-search-forward "Legacy summary" nil t))))
      (delete-directory repository t)
      (delete-directory roam-directory t))))

(ert-deftest magent-test-repo-summary-creates-new-note-through-org-roam ()
  "Test new summaries use noninteractive Org-roam capture when available."
  (require 'magent-repo-summary)
  (let* ((repository (magent-test--make-git-repository
                      "magent-summary-roam-capture-repository-"))
         (roam-directory (make-temp-file "magent-summary-roam-capture-" t))
         (magent-org-roam-directory roam-directory)
         (org-roam-directory "/wrong-org-roam-directory/")
         captured-arguments indexed-directory indexed-path)
    (unwind-protect
        (cl-letf (((symbol-function
                    'magent-repo-summary--org-roam-capture-available-p)
                   (lambda () t))
                  ((symbol-function 'org-roam-node-create)
                   (lambda (&rest properties) properties))
                  ((symbol-function 'org-roam-capture-)
                   (lambda (&rest arguments)
                     (setq captured-arguments arguments)
                     (let* ((node (plist-get arguments :node))
                            (template
                             (car (plist-get arguments :templates)))
                            (target (plist-get (nthcdr 4 template) :target))
                            (path (cadr target)))
                       (with-temp-file path
                         (insert ":PROPERTIES:\n:ID: "
                                 (plist-get node :id)
                                 "\n:END:\n#+title: "
                                 (plist-get node :title)
                                 "\n")))))
                  ((symbol-function 'org-roam-db-update-file)
                   (lambda (path)
                     (setq indexed-directory
                           (symbol-value 'org-roam-directory)
                           indexed-path path))))
          (let* ((result
                  (magent-repo-summary-write
                   repository "full" "Captured repository summary."))
                 (path (plist-get result :path))
                 (template
                  (car (plist-get captured-arguments :templates))))
            (should captured-arguments)
            (should (equal (cadr (plist-get (nthcdr 4 template) :target))
                           path))
            (should (plist-get (nthcdr 4 template) :immediate-finish))
            (should (plist-get (nthcdr 4 template) :kill-buffer))
            (should (equal indexed-directory roam-directory))
            (should (equal indexed-path path))
            (with-temp-buffer
              (insert-file-contents path)
              (org-mode)
              (goto-char (point-min))
              (should (equal
                       (org-id-get)
                       (plist-get (plist-get captured-arguments :node) :id))))))
      (delete-directory repository t)
      (delete-directory roam-directory t))))

(ert-deftest magent-test-repo-summary-rejects-invalid-destinations ()
  "Test repository summaries require a Git project and org-roam directory."
  (require 'magent-repo-summary)
  (let ((repository (magent-test--make-git-repository
                     "magent-summary-repository-"))
        (plain-directory (make-temp-file "magent-summary-plain-" t)))
    (unwind-protect
        (progn
          (let ((magent-org-roam-directory nil)
                (org-roam-directory nil))
            (should-error
             (magent-repo-summary-write repository "full" "Summary")))
          (let ((magent-org-roam-directory plain-directory)
                (new-id-calls 0))
            (cl-letf (((symbol-function 'magent-repo-summary--new-id)
                       (lambda (_path)
                         (cl-incf new-id-calls)
                         "unexpected-id")))
              (should-error
               (magent-repo-summary-write 'global "full" "Summary"))
              (should-error
               (magent-repo-summary-write plain-directory "full" "Summary"))
              (should-error
               (magent-repo-summary-write
                repository "scoped" "Summary" "parser" '("../outside.el")))
              (should-error
               (magent-repo-summary-write repository "invalid" "Summary"))
              (should-error
               (magent-repo-summary-write repository "full" "")))
            (should (zerop new-id-calls))
            (should-not (directory-files plain-directory nil "\\.org\\'"))))
      (delete-directory repository t)
      (delete-directory plain-directory t))))

(ert-deftest magent-test-tool-result-normalizes-legacy-error-text ()
  "Test legacy tool error strings enter the structured failure contract."
  (dolist (text '("Error: explicit failure"
                  "Error reading file: missing"
                  "HTTP error: connection failed"
                  "Command timed out with no output"))
    (let ((result (magent-tool-result-normalize text "test-tool" "call-1")))
      (should (magent-tool-result-p result))
      (should (eq (magent-tool-result-status-value result) 'failed))
      (should-not (magent-tool-result-success-p result))
      (should (stringp (magent-tool-result-output-string result)))))
  (should (magent-tool-result-success-p
           (magent-tool-result-normalize "ordinary output"))))

(ert-deftest magent-test-tools-bash-reports-nonzero-exit-status ()
  "Test bash returns a structured failed result for a nonzero exit."
  (require 'magent-tools)
  (let ((default-directory temporary-file-directory)
        result)
    (magent-tools--bash
     (lambda (value) (setq result value))
     "printf failure-output; exit 7"
     2)
    (let ((deadline (+ (float-time) 3)))
      (while (and (null result) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (magent-tool-result-p result))
    (should (eq (magent-tool-result-status-value result) 'failed))
    (should (= (magent-tool-result-exit-code result) 7))
    (should (string-match-p "failure-output"
                            (magent-tool-result-output-string result)))))

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
                thread (magent-thread-turn-id turn) "call-9" "bash" nil result)))
    (should (eq (magent-thread-item-status item) 'failed))
    (should (equal (magent-thread-item-output item) "plain diagnostic"))
    (should (equal (plist-get (magent-thread-item-metadata item) :exit-code) 9))))

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
          (magent-tools--agent-job-runtimes (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'magent-agent-registry-get)
                 (lambda (_name) agent))
                ((symbol-function 'magent-lifecycle-events-create-subagent-context)
                 (lambda (title parent)
                   (list :title title :parent parent)))
                ((symbol-function 'magent-lifecycle-events-stop-subagent)
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
          (magent-tools--agent-job-runtimes (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'magent-agent-registry-get)
                 (lambda (_name) agent))
                ((symbol-function 'magent-lifecycle-events-create-subagent-context)
                 (lambda (title parent)
                   (list :title title :parent parent)))
                ((symbol-function 'magent-lifecycle-events-stop-subagent) #'ignore)
                ((symbol-function 'magent-agent-process)
                 (lambda (_prompt callback _agent-info _skill-names
                                  _event-context _request-context
                                  _capability-resolution _ui-callback
                                  _request-live-p _request-state)
                   (funcall callback
                            (magent-agent-result-failed
                             "Request timed out after 5 seconds"))
                   nil))
                ((symbol-function 'magent-ui--snapshot-buffer-content)
                 #'ignore))
        (magent-tools--spawn-agent
         (lambda (value) (setq result value))
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
        (cl-letf (((symbol-function 'magent-lifecycle-events-create-subagent-context)
                   (lambda (title parent)
                     (list :title title :parent parent)))
                  ((symbol-function 'magent-lifecycle-events-stop-subagent) #'ignore)
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
  (should (= (length magent-tools--all-gptel-tools) 15)))

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
  (let* ((magent-bypass-permission t)
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

(ert-deftest magent-test-tools-read-file-null-path ()
  "Test read_file rejects JSON null path values clearly."
  (require 'magent-tools)
  (let ((result nil))
    (magent-tools--read-file (lambda (r) (setq result r)) :null)
    (should
     (string-match-p "Missing required argument .*path" result))))

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

(ert-deftest magent-test-mode-line-lighter-renders-from-processing-state ()
  "Test the mode-line lighter depends only on processing-state APIs."
  (require 'magent)
  (cl-letf (((symbol-function 'magent-ui-processing-p) (lambda () nil)))
    (let ((lighter (eval (cadr magent--lighter))))
      (should (string-match-p "\\[M/" lighter))
      (should-not (string-match-p "\\[busy\\]" lighter))))
  (cl-letf (((symbol-function 'magent-ui-processing-p) (lambda () t)))
    (should (string-match-p "\\[busy\\]" (eval (cadr magent--lighter))))))

(ert-deftest magent-test-tools-gptel-to-magent-tool ()
  "Test conversion from gptel-tool to magent tool plist."
  (require 'magent-tools)
  (let* ((gptel-tool (car magent-tools--all-gptel-tools))  ; read_file
         (magent-tool (magent-tools--gptel-to-magent-tool gptel-tool)))
    (should (plist-get magent-tool :name))
    (should (plist-get magent-tool :description))
    (should (plist-get magent-tool :function))
    (should (plist-get magent-tool :perm-key))))

(ert-deftest magent-test-agent-loop-tools-to-gptel-json-sanitizes-schema ()
  "Test gptel tool schemas are safe for strict JSON serialization."
  (require 'magent-agent-loop)
  (let* ((tools (magent-agent-loop-tools-to-gptel
                 (list (list :name "emacs_eval"
                             :description "Eval"
                             :args '((:name "sexp"
                                      :type string
                                      :description "Expression")
                                     (:name "timeout"
                                      :type integer
                                      :description "Timeout"
                                      :optional t))
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
        (magent--current-request-handle (magent-agent-loop-create))
        (magent-ui--request-generation 0))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (cl-letf (((symbol-function 'magent-agent-loop-abort) #'ignore)
              ((symbol-function 'magent-approval-drop-requests) #'ignore)
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
  (require 'magent-legacy-queue)
  (let* ((buffer (magent-ui-get-buffer))
         (loop (magent-agent-loop-create))
         (magent--current-request-handle loop)
         (magent-legacy-queue--current-request-handle loop)
         (magent-legacy-queue--active
          (magent-legacy-queue-submission-create
           :id "sub-interrupt"
           :op (magent-protocol-interrupt-op)
           :status 'running))
         (magent-legacy-queue--pending nil)
         aborted)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (cl-letf (((symbol-function 'magent-agent-loop-abort)
               (lambda (value) (push value aborted)))
              ((symbol-function 'magent-approval-drop-requests) #'ignore)
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
          (should-not quit-flag))
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

(ert-deftest magent-test-emacs-eval-worker-suppresses-debugger-settings ()
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
        (magent-tools--emacs-eval
         (lambda (result) (setq callback-result result))
         "(error \"worker boom\")")
        (should (functionp worker))
        (funcall worker)))
    (should (functionp timeout-timer))
    (should (string-match-p "worker boom" callback-result))))

(ert-deftest magent-test-emacs-eval-fallback-suppresses-debugger-settings ()
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
        (magent-tools--emacs-eval
         (lambda (result) (setq callback-result result))
         "(error \"fallback boom\")")))
    (should (string-match-p "fallback boom" callback-result))))

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

(ert-deftest magent-test-audit-tool-events-redact-write-payloads ()
  "Test persisted tool audit records redact general and summary bodies."
  (require 'magent-audit)
  (let* ((magent-enable-audit-log t)
         (magent-audit-directory (make-temp-file "magent-audit-" t))
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
                              :call-id "call-summary"
                              :tool-name "write_repo_summary"
                              :summary "full"
                              :args '(:mode "full"
                                      :content "private repository details"))
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
                  (magent-test--read-audit-records magent-audit-directory))
                 (find-tool
                  (lambda (name)
                    (cl-find-if
                     (lambda (record)
                       (equal (cdr (assq 'tool_name record)) name))
                     records)))
                 (write-record (funcall find-tool "write_file"))
                 (summary-record
                  (funcall find-tool "write_repo_summary"))
                 (edit-record (funcall find-tool "edit_file"))
                 (write-preview (cdr (assq 'args_preview write-record)))
                 (summary-preview
                  (cdr (assq 'args_preview summary-record)))
                 (edit-preview (cdr (assq 'args_preview edit-record))))
            (should (equal (cdr (assq 'tool_name write-record)) "write_file"))
            (should (equal (cdr (assq 'path write-preview)) "notes.txt"))
            (should (= (cdr (assq 'content_length write-preview))
                       (length "super secret body")))
            (should-not (assq 'content write-preview))
            (should (equal (cdr (assq 'tool_name summary-record))
                           "write_repo_summary"))
            (should (= (cdr (assq 'content_length summary-preview))
                       (length "private repository details")))
            (should-not (assq 'content summary-preview))
            (should (equal (cdr (assq 'tool_name edit-record)) "edit_file"))
            (should (equal (cdr (assq 'path edit-preview)) "notes.txt"))
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
          (let ((records (magent-test--read-audit-records magent-audit-directory)))
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
      (delete-directory magent-audit-directory t))))

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
         (magent-enable-audit-log t)
         (magent-audit-directory directory)
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
  (let* ((magent-enable-audit-log t)
         (magent-audit-directory (make-temp-file "magent-audit-malformed-" t))
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
                  (magent-test--read-audit-records magent-audit-directory))
                 (record (car records))
                 (encoded (json-encode record)))
            (should (= (length records) 1))
            (should (equal (cdr (assq 'attribution_source record)) "missing"))
            (should-not (cdr (assq 'session_id record)))
            (should-not (cdr (assq 'scope record)))
            (should-not (string-match-p (regexp-quote secret) encoded))))
      (magent-audit--flush-pending)
      (delete-directory magent-audit-directory t))))

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
         (magent-enable-audit-log t)
         (magent-audit-directory directory)
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
            (let ((path (cdr (assq 'path args-preview))))
              (should (string-suffix-p "/.env" path))
              (should-not (string-match-p
                           (regexp-quote (expand-file-name "~")) path)))))
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
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-internal-session-saves-outside-normal-session-list ()
  "Test internal command sessions persist under the internal namespace."
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-command-session-directory nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (session (magent-session-create))
         (id (magent-session-get-id session))
         (scope (magent-session-internal-scope id "memory-init" 'global)))
    (unwind-protect
        (progn
          (dolist (entry '((kind . "internal-command")
                           (command . "memory-init")
                           (status . "completed")
                           (title . "Memory Init")
                           (origin-scope . global)))
            (magent-session-set-metadata-value session (car entry) (cdr entry)))
          (setq magent--current-session session
                magent-session--current-scope scope)
          (magent-session-add-message session 'user "run memory init")
          (magent-session-save)
          (let* ((internal-files
                  (magent-session-list-internal-files "memory-init"))
                 (file (car internal-files))
                 (meta (magent-session--read-file-metadata-cached file))
                 (loaded (magent-session-read-file file))
                 (loaded-session (plist-get loaded :session)))
            (should (= (length internal-files) 1))
            (should (string-match-p "/internal/memory-init/" file))
            (should-not (member file (magent-session-list-files)))
            (should (equal (plist-get meta :kind) "internal-command"))
            (should (equal (plist-get meta :command) "memory-init"))
            (should (equal (plist-get meta :status) "completed"))
            (should (equal (magent-session-metadata-value
                            loaded-session 'command)
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
              :result "found it"))
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

(ert-deftest magent-test-tools-child-agent-completion-persists-parent-session ()
  "Test child-agent completion updates and saves the parent session."
  (require 'magent-tools)
  (let* ((magent-session-directory (make-temp-file "magent-sessions-" t))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
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
              (magent-tools--agent-job-runtimes (make-hash-table :test #'equal)))
          (magent-session-install 'global parent-session)
          (magent-session-add-message parent-session 'user "spawn child")
          (cl-letf (((symbol-function 'magent-agent-registry-get)
                     (lambda (_name) agent))
                    ((symbol-function 'magent-lifecycle-events-create-subagent-context)
                     (lambda (_title _parent) 'child-context))
                    ((symbol-function 'magent-lifecycle-events-stop-subagent) #'ignore)
                    ((symbol-function 'magent-agent-process)
                     (lambda (prompt callback _agent-info _skill-names
                                     _event-context _request-context
                                     _capability-resolution _ui-callback
                                     _request-live-p request-state)
                       (magent-session-add-message
                        (magent-request-context-session request-state)
                        'assistant
                        (concat "child saw " prompt))
                       (setq child-callback callback)
                       nil))
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
            (funcall child-callback "child answer"))
          (let* ((files (directory-files magent-session-directory t "\\.json$"))
                 (loaded (magent-session-read-file (car files)))
                 (loaded-session (plist-get loaded :session))
                 (job (car (magent-session-agent-jobs loaded-session))))
            (should (= (length files) 1))
            (should job)
            (should (eq (magent-agent-job-status job) 'completed))
            (should (equal (magent-agent-job-result job) "child answer"))
            (should (equal (magent-agent-job-transcript job)
                           '(((role . "assistant")
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

(ert-deftest magent-test-llm-gptel-final-retry-buffers-streamed-dsml-tool-call ()
  "Test final-response retry buffers streamed DSML instead of emitting text."
  (require 'magent-llm-gptel)
  (let* ((events nil)
         (text
          (concat
           "<｜｜DSML｜｜tool_calls>\n"
           "<｜｜DSML｜｜invoke name=\"bash\">\n"
           "<｜｜DSML｜｜parameter name=\"command\" string=\"true\">"
           "git diff"
           "</｜｜DSML｜｜parameter>\n"
           "</｜｜DSML｜｜invoke>\n"
           "</｜｜DSML｜｜tool_calls>"))
         (request (magent-llm-request-create
                   :stream t
                   :metadata '(:final-response-retry t)
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state)))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback
           request state buffer text '(:stream t))
          (should (null events))
          (magent-llm-gptel--callback
           request state buffer t (list :stream t :content text))
          (should-not (buffer-live-p buffer))
          (let ((ordered (nreverse events)))
            (should (= (length ordered) 2))
            (should (eq (magent-llm-event-type (car ordered)) 'tool-call))
            (should (eq (magent-llm-event-type (cadr ordered))
                        'tool-call-batch-end))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-llm-gptel-strict-final-never-emits-textual-tool-call ()
  "Strict no-tool final requests treat textual DSML as assistant text."
  (require 'magent-llm-gptel)
  (let* ((events nil)
         (text
          (concat
           "<｜｜DSML｜｜tool_calls>\n"
           "<｜｜DSML｜｜invoke name=\"bash\">\n"
           "<｜｜DSML｜｜parameter name=\"command\" string=\"true\">"
           "git diff"
           "</｜｜DSML｜｜parameter>\n"
           "</｜｜DSML｜｜invoke>\n"
           "</｜｜DSML｜｜tool_calls>"))
         (request (magent-llm-request-create
                   :stream nil
                   :metadata '(:final-response-retry t
                               :strict-final-response-retry t
                               :disable-provider-tools t)
                   :callback (lambda (event) (push event events))))
         (buffer (generate-new-buffer " *magent-test-gptel*"))
         (state (magent-llm-gptel--make-state)))
    (unwind-protect
        (progn
          (magent-llm-gptel--callback
           request state buffer text (list :status "ok" :content text))
          (should-not (buffer-live-p buffer))
          (should (= (length events) 1))
          (should (eq (magent-llm-event-type (car events)) 'completed))
          (should (equal (magent-llm-event-text (car events)) text)))
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
            (should-not
             (plist-member (magent-llm-event-metadata event) :last)))
          (should-not (buffer-live-p buffer)))
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
  "Test completion keeps text streamed before a tool continuation."
  (require 'magent-agent-loop)
  (let ((loop (magent-agent-loop-create)))
    (magent-agent-loop-apply-event
     loop (magent-llm-text-delta-event "Checking buffers. "))
    (magent-agent-loop-apply-event
     loop (magent-llm-completed-event "Done."))
    (should (equal (magent-agent-loop-result loop)
                   "Checking buffers. Done."))))

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
         (loop (magent-agent-loop-create :session session)))
    (magent-agent-loop-record-tool-result
     loop nil '(:path "README.org") '(:id "call-1" :name "read_file") "content")
    (let* ((messages (magent-session-get-messages session))
           (message (car messages))
           (content (magent-msg-content message))
           (thread (magent-session-thread-ledger session))
           (turn (car (magent-thread-turns thread)))
           (item (car (magent-thread-turn-items turn))))
      (should (eq (magent-msg-role message) 'tool))
      (should (equal (plist-get content :id) "call-1"))
      (should (equal (plist-get content :name) "read_file"))
      (should (equal (plist-get content :args) '(:path "README.org")))
      (should (equal (plist-get content :result) "content"))
      (should (equal (magent-thread-item-metadata item)
                     '(:source "tool-result"))))))

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
         (loop (magent-agent-loop-create :session session)))
    (magent-agent-loop-record-tool-result
     loop nil '(:path "README.org")
     '(:id "call-1"
       :name "read_file"
       :approval-decision allow
       :approval-source rule-allow)
     "content")
    (let* ((thread (magent-session-thread-ledger session))
           (turn (car (magent-thread-turns thread)))
           (item (car (magent-thread-turn-items turn))))
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
                :function (lambda (path) (format "read %s" path))
                :async nil))
         (request (magent-llm-request-create :tools (list tool)))
         (loop (magent-agent-loop-create
                :session session
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
      (lambda (tool-spec cb arg-values)
        (funcall cb (apply (gptel-tool-function tool-spec) arg-values)))
      :file-arg-index-function (lambda (_args-spec) 0)
      :args-to-plist-function (lambda (_args-spec arg-values) arg-values)
      :summarize-function (lambda (arg-values _args-spec) (car arg-values)))
     (lambda (&optional _result) (setq done t)))
    (should done)
    (let* ((message (car (magent-session-get-messages session)))
           (content (magent-msg-content message)))
      (should (eq (magent-msg-role message) 'tool))
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
         (loop (magent-agent-loop-create
                :session session
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
    (let* ((message (car (magent-session-get-messages session)))
           (content (magent-msg-content message)))
      (should (eq (magent-msg-role message) 'tool))
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
                            (format "result %d" (length tool-runs)))
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
    (should (equal (nreverse results) '("result 1" "result 2")))))

(ert-deftest magent-test-agent-loop-run-tool-emits-events-and-renders-visible-ui ()
  "Test loop tool events carry enough data for the UI projection sink."
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
    (cl-letf (((symbol-function 'magent-lifecycle-events-emit)
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
       (list "echo hi" "inspect shell"))
      (dolist (event (nreverse (copy-sequence events)))
        (magent-ui--lifecycle-render-sink
         (append (list :type (car event)) (cdr event)))))
    (should (equal result "ran echo hi"))
    (let ((ordered-events (nreverse events)))
      (should (equal (mapcar #'car ordered-events)
                     '(tool-call-start tool-call-end)))
      (let ((start (cdr (car ordered-events))))
        (should (eq (plist-get start :context) 'ctx))
        (should (equal (plist-get start :tool-name) "bash"))
        (should (plist-get start :ui-visible))
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

(ert-deftest magent-test-ui-interrupt-aborts-agent-loop ()
  "Test UI interrupt aborts the active Magent-owned loop."
  (require 'magent-ui)
  (require 'magent-legacy-queue)
  (let ((buffer (magent-ui-get-buffer))
        (loop (magent-agent-loop-create))
        (magent-legacy-queue--current-request-handle nil)
        (magent-legacy-queue--active nil)
        (magent-legacy-queue--pending nil)
        (magent-ui--request-generation 0)
        aborted)
    (setq magent--current-request-handle loop)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (cl-letf (((symbol-function 'magent-agent-loop-abort)
               (lambda (value) (setq aborted value)))
              ((symbol-function 'magent-approval-drop-requests) #'ignore)
              ((symbol-function 'magent-ui--clear-processing) #'ignore)
              ((symbol-function 'magent-ui--maybe-show-input-prompt) #'ignore))
    (magent-interrupt))
    (should (eq aborted loop))
    (should-not magent--current-request-handle)))

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
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore))
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
    (cl-letf (((symbol-function 'magent-lifecycle-events-emit) #'ignore))
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
      (funcall tool-callback "\"done\""))
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
         (global-file-a (expand-file-name "session-20260317-100000.json"
                                          magent-session-directory))
         (global-file-b (expand-file-name "session-20260317-090000.json"
                                          magent-session-directory))
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
          (make-directory project-dir t)
          (dolist (file (list global-file-a global-file-b))
            (with-temp-file file
              (insert "{\"scope\":\"global\",\"summary-title\":\"Global\"}")))
          (dolist (file (list project-file-a project-file-b))
            (with-temp-file file
              (insert
               (format
                "{\"scope\":\"project\",\"project-root\":\"%s\",\"summary-title\":\"Project\"}"
                project-root))))
          (setq magent-session--current-scope project-root)
          (cl-letf (((symbol-function 'magent-session--read-file-metadata)
                     (lambda (file)
                       (cl-incf read-count)
                       (funcall original-read file))))
            (should (equal (magent-session-list-files) files))
            (should (<= read-count (length files)))))
      (delete-directory magent-session-directory t)
      (delete-directory project-root t))))

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

(ert-deftest magent-test-ui-scope-switch-does-not-snapshot-workspace ()
  "Test switching project scopes does not persist workspace buffer text."
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
            (should (null (magent-session-buffer-content session-a)))
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
  "Test submitting from an older compose buffer reactivates that scope."
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
          (setq buffer-a (magent-test--compose-with-text
                          scope-a "hello from project a"))
          (magent-session-activate scope-b)
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent
                                            (magent-session-current-scope))))))
            (with-current-buffer buffer-a
              (magent-input-submit)))
          (should (equal (car captured) "hello from project a"))
          (should (eq (nth 1 captured) 'compose))
          (should (equal (nth 5 captured) scope-a))
          (should (equal (magent-session-current-scope) scope-a)))
      (when (buffer-live-p buffer-a)
        (kill-buffer buffer-a))
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest magent-test-open-compose-displays-bottom-popup ()
  "Test opening compose uses a bottom side-window display action."
  (require 'magent-ui)
  (let* ((magent-compose-window-height 7)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         displayed
         action)
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buffer action-arg &rest _args)
                       (setq displayed buffer)
                       (setq action action-arg)
                       nil)))
            (magent-ui-open-compose 'global "draft"))
          (should (eq displayed (magent-ui-compose-buffer 'global)))
          (should (equal (car action) '(display-buffer-in-side-window)))
          (should (eq (cdr (assq 'side action)) 'bottom))
          (should (= (cdr (assq 'window-height action)) 7))
          (with-current-buffer (magent-ui-compose-buffer 'global)
            (should (equal (buffer-string) "draft\n"))))
      (when (buffer-live-p (magent-ui-compose-buffer 'global))
        (kill-buffer (magent-ui-compose-buffer 'global)))
      (magent-session-reset))))

(ert-deftest magent-test-input-submit-closes-compose-and-focuses-workspace ()
  "Test prompt submission closes compose and selects the workspace by default."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-submit-close*")
         (magent-compose-close-after-submit t)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (workspace nil)
         (compose nil)
         (captured nil)
         (closed-window nil)
         (selected-window nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq workspace (magent-ui-get-buffer 'global))
          (setq compose (magent-test--compose-with-text 'global "send it"))
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent))))
                    ((symbol-function 'display-buffer)
                     (lambda (buffer &rest _args)
                       (if (eq buffer workspace)
                           'workspace-window
                         'other-window)))
                    ((symbol-function 'get-buffer-window)
                     (lambda (buffer &optional _all-frames)
                       (when (eq buffer compose)
                         'compose-window)))
                    ((symbol-function 'windowp)
                     (lambda (value)
                       (memq value '(workspace-window compose-window))))
                    ((symbol-function 'window-live-p)
                     (lambda (value)
                       (memq value '(workspace-window compose-window))))
                    ((symbol-function 'quit-window)
                     (lambda (&optional _kill window)
                       (setq closed-window window)))
                    ((symbol-function 'select-window)
                     (lambda (window &optional _norecord)
                       (setq selected-window window))))
            (with-current-buffer compose
              (magent-input-submit)))
          (should (equal (car captured) "send it"))
          (should (eq closed-window 'compose-window))
          (should (eq selected-window 'workspace-window))
          (with-current-buffer compose
            (should (equal (buffer-string) ""))))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p workspace)
        (kill-buffer workspace))
      (magent-session-reset))))

(ert-deftest magent-test-input-submit-can-keep-compose-open ()
  "Test `magent-compose-close-after-submit' can keep compose visible."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-submit-keep-compose*")
         (magent-compose-close-after-submit nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (workspace nil)
         (compose nil)
         (captured nil)
         (closed nil)
         (selected nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq workspace (magent-ui-get-buffer 'global))
          (setq compose (magent-test--compose-with-text 'global "keep window"))
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent))))
                    ((symbol-function 'display-buffer)
                     (lambda (&rest _args) 'workspace-window))
                    ((symbol-function 'quit-window)
                     (lambda (&rest _args) (setq closed t)))
                    ((symbol-function 'select-window)
                     (lambda (&rest _args) (setq selected t))))
            (with-current-buffer compose
              (magent-input-submit)))
          (should (equal (car captured) "keep window"))
          (should-not closed)
          (should-not selected)
          (with-current-buffer compose
            (should (equal (buffer-string) ""))))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p workspace)
        (kill-buffer workspace))
      (magent-session-reset))))

(ert-deftest magent-test-input-submit-runs-submit-hook ()
  "Test successful input submission runs `magent-ui-after-input-submit-hook'."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-submit-hook*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (buffer nil)
         (captured nil)
         (hook-called nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq buffer (magent-test--compose-with-text
                        'global "run submit hook"))
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent)))))
            (with-current-buffer buffer
              (add-hook 'magent-ui-after-input-submit-hook
                        (lambda () (setq hook-called t))
                        nil t)
              (magent-input-submit)))
          (should hook-called)
          (should (equal (car captured) "run submit hook"))
          (should (eq (nth 1 captured) 'compose)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-input-submit-former-clear-command-is-prompt-text ()
  "Test formerly special @clear input is now sent as prompt text."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-submit-clear*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (buffer nil)
         (captured nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (magent-session-add-message (magent-session-get) 'user "old context")
          (setq buffer (magent-test--compose-with-text 'global "@clear"))
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent)))))
            (with-current-buffer buffer
              (magent-input-submit)))
          (should (equal (nth 0 captured) "@clear"))
          (should (eq (nth 1 captured) 'compose))
          (should (null (nth 3 captured)))
          (should (= (length (magent-session-messages (magent-session-get))) 1)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-input-submit-uses-and-clears-pending-skills ()
  "Test compose submission consumes one-shot skills selected for the scope."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-submit-clear-text*")
         (magent-ui--pending-skills-by-scope (make-hash-table :test #'equal))
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (buffer nil)
         (captured nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (magent-ui--set-pending-skills
           'global '("magit-workflow" "git-workflow"))
          (setq buffer (magent-test--compose-with-text
                        'global "review this change"))
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent)))))
            (with-current-buffer buffer
              (magent-input-submit)))
          (should (equal (nth 0 captured) "review this change"))
          (should (eq (nth 1 captured) 'compose))
          (should (equal (nth 3 captured)
                         '("magit-workflow" "git-workflow")))
          (should-not (magent-ui--pending-skills 'global)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-input-submit-former-init-command-is-prompt-text ()
  "Test formerly special @init input is now ordinary prompt text."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-submit-init*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (buffer nil)
         (captured nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq buffer (magent-test--compose-with-text 'global "@init"))
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent)))))
            (with-current-buffer buffer
              (magent-input-submit)))
          (should (equal (nth 0 captured) "@init"))
          (should (eq (nth 1 captured) 'compose))
          (should (null (nth 3 captured))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-toggle-skill-for-next-request-uses-instruction-skills ()
  "Test pending skill toggling only accepts instruction skills."
  (require 'magent-ui)
  (let ((magent-skills--registry nil)
        (magent-ui--pending-skills-by-scope (make-hash-table :test #'equal)))
    (magent-skills-register
     (magent-skill-create :name "inst" :type 'instruction))
    (magent-skills-register
     (magent-skill-create :name "tool-skill" :type 'tool))
    (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
              ((symbol-function 'magent-ui--activate-command-context)
               (lambda () 'global)))
      (magent-ui-toggle-skill-for-next-request "inst")
      (should (equal (magent-ui--pending-skills 'global) '("inst")))
      (magent-ui-toggle-skill-for-next-request "inst")
      (should-not (magent-ui--pending-skills 'global))
      (should-error
       (magent-ui-toggle-skill-for-next-request "tool-skill")
       :type 'user-error))))

(ert-deftest magent-test-run-skill-command-dispatches-default-prompt ()
  "Test command-like skills dispatch default prompt with extra instruction."
  (require 'magent-ui)
  (let ((magent-skills--registry nil)
        (magent-ui--pending-skills-by-scope (make-hash-table :test #'equal))
        captured)
    (magent-skills-register
     (magent-skill-create
      :name "init"
      :type 'instruction
      :default-prompt "Initialize AGENTS.md."))
    (magent-ui--set-pending-skills 'global '("git-workflow"))
    (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
              ((symbol-function 'magent-ui--activate-command-context)
               (lambda () 'global))
              ((symbol-function 'magent-ui--dispatch-from-command-context)
               (lambda (prompt source display skills agent)
                 (setq captured (list prompt source display skills agent)))))
      (magent-ui-run-skill-command "init" "focus on tests")
      (should (string-match-p "Initialize AGENTS\\.md\\." (nth 0 captured)))
      (should (string-match-p "Additional instruction:\nfocus on tests"
                              (nth 0 captured)))
      (should (eq (nth 1 captured) 'skill-command))
      (should (equal (nth 3 captured) '("git-workflow" "init")))
      (should-not (magent-ui--pending-skills 'global)))))

(ert-deftest magent-test-transient-submenus-are-commands ()
  "Test Magent's grouped transient submenus are interactive commands."
  (require 'magent-ui)
  (dolist (command '(magent-transient-menu
                     magent-transient-agent-menu
                     magent-transient-skill-menu
                     magent-transient-capability-menu
                     magent-transient-session-menu
                     magent-transient-log-menu
                     magent-transient-health-menu
                     magent-transient-buffer-menu))
    (should (commandp command))))

(ert-deftest magent-test-transient-agent-menu-uses-mnemonic-agent-keys ()
  "Test dynamic agent suffixes prefer mnemonic keys in the Agent submenu."
  (require 'magent-ui)
  (let* ((agents (list (magent-agent-info-create :name "build" :mode 'primary)
                       (magent-agent-info-create :name "plan" :mode 'primary)
                       (magent-agent-info-create :name "explore" :mode 'primary)))
         (pairs (magent-transient-menu--assign-agent-keys agents)))
    (should (equal (mapcar #'car pairs) '("b" "p" "e")))))

(ert-deftest magent-test-header-line-shows-pending-skills ()
  "Test workspace header-line displays scope, agent, status, and skills."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-header*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (magent-ui--pending-skills-by-scope (make-hash-table :test #'equal))
         (buffer nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq buffer (magent-ui-get-buffer 'global))
          (magent-ui--set-pending-skills 'global '("git-workflow"))
          (with-current-buffer buffer
            (let ((line (magent-ui--header-line)))
              (should (string-match-p "scope: global" line))
              (should (string-match-p "agent: build" line))
              (should (string-match-p "thread: idle" line))
              (should (string-match-p "request: idle" line))
              (should (string-match-p "queue: 0" line))
              (should (string-match-p "session:" line))
              (should (string-match-p "skills: git-workflow" line)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-evil-mode-input-submit-restores-normal-state ()
  "Test optional Evil integration returns to normal state after submission."
  (skip-unless (require 'evil nil t))
  (require 'magent-evil)
  (let* ((magent-buffer-name "*magent-evil-submit*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (buffer nil)
         (captured nil)
         (normal-state-called nil)
         (magent-evil-mode nil)
         (magent-evil--enabled nil)
         (magent-evil-reset-input-method-after-submit t))
    (magent-evil-mode 1)
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq buffer (magent-test--compose-with-text
                        'global "return to normal"))
          (with-current-buffer buffer
            (evil-local-mode 1)
            (setq-local evil-input-method "rime"))
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
          (should (eq (nth 1 captured) 'compose))
          (with-current-buffer buffer
            (should-not evil-input-method)))
      (magent-evil-mode -1)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-evil-mode-input-submit-can-preserve-input-method ()
  "Test Evil input method reset after Magent submission is configurable."
  (skip-unless (require 'evil nil t))
  (require 'magent-evil)
  (let* ((magent-buffer-name "*magent-evil-submit-input-method*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (buffer nil)
         (captured nil)
         (magent-evil-mode nil)
         (magent-evil--enabled nil)
         (magent-evil-reset-input-method-after-submit nil))
    (magent-evil-mode 1)
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq buffer (magent-test--compose-with-text
                        'global "keep input method"))
          (with-current-buffer buffer
            (evil-local-mode 1)
            (setq-local evil-input-method "rime"))
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent))))
                    ((symbol-function 'evil-normal-state) #'ignore))
            (with-current-buffer buffer
              (magent-input-submit)))
          (should (equal (car captured) "keep input method"))
          (with-current-buffer buffer
            (should (equal evil-input-method "rime"))))
      (magent-evil-mode -1)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-evil-mode-input-submit-deactivates-input-method ()
  "Test Magent submission deactivates a live input method, not just Evil's copy.
Regression: clearing `evil-input-method' alone left `current-input-method'
active, so an activated input method (e.g. rime) stayed on after submit."
  (skip-unless (require 'evil nil t))
  (require 'magent-evil)
  (let* ((magent-buffer-name "*magent-evil-submit-deactivate-im*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (buffer nil)
         (captured nil)
         (magent-evil-mode nil)
         (magent-evil--enabled nil)
         (magent-evil-reset-input-method-after-submit t))
    (magent-evil-mode 1)
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq buffer (magent-test--compose-with-text
                        'global "deactivate input method"))
          (with-current-buffer buffer
            (evil-local-mode 1)
            ;; Activate a real, always-available input method the proper
            ;; way (same mechanism rime uses via `current-input-method').
            (activate-input-method "greek"))
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent))))
                    ((symbol-function 'evil-normal-state) #'ignore))
            (with-current-buffer buffer
              (magent-input-submit)))
          (should (equal (car captured) "deactivate input method"))
          (with-current-buffer buffer
            (should-not current-input-method)
            (should-not evil-input-method)))
      (magent-evil-mode -1)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-output-submit-or-interrupt-confirms-running-request ()
  "Test output `C-c C-c' confirms before interrupting an active request."
  (require 'magent-ui)
  (let ((prompt nil)
        (interrupted nil))
    (cl-letf (((symbol-function 'magent-ui-processing-p)
               (lambda () t))
              ((symbol-function 'y-or-n-p)
               (lambda (prompt-arg)
                 (setq prompt prompt-arg)
                 t))
              ((symbol-function 'magent-interrupt)
               (lambda () (setq interrupted t))))
      (magent-ui-submit-or-interrupt))
    (should (equal prompt "Interrupt current Magent request? "))
    (should interrupted)))

(ert-deftest magent-test-output-submit-or-interrupt-respects-no-answer ()
  "Test output `C-c C-c' leaves an active request alone when declined."
  (require 'magent-ui)
  (let ((interrupted nil))
    (cl-letf (((symbol-function 'magent-ui-processing-p)
               (lambda () t))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'magent-interrupt)
               (lambda () (setq interrupted t))))
      (magent-ui-submit-or-interrupt))
    (should-not interrupted)))

(ert-deftest magent-test-output-submit-or-interrupt-opens-compose-when-idle ()
  "Test output `C-c C-c' opens compose when no request is active."
  (require 'magent-ui)
  (let ((opened nil))
    (cl-letf (((symbol-function 'magent-ui-processing-p)
               (lambda () nil))
              ((symbol-function 'magent-ui-open-compose)
               (lambda (&optional scope initial-text)
                 (setq opened (list scope initial-text)))))
      (with-temp-buffer
        (magent-output-mode)
        (setq-local magent-ui--buffer-scope 'global)
        (magent-ui-submit-or-interrupt)))
    (should (equal opened '(global nil)))))

(ert-deftest magent-test-output-compose-command-opens-current-scope ()
  "Test explicit output compose command opens compose for current scope."
  (require 'magent-ui)
  (let ((opened nil))
    (cl-letf (((symbol-function 'magent-ui-open-compose)
               (lambda (&optional scope initial-text)
                 (setq opened (list scope initial-text)))))
      (with-temp-buffer
        (magent-output-mode)
        (setq-local magent-ui--buffer-scope 'global)
        (magent-ui-compose-from-output)))
    (should (equal opened '(global nil)))))

(ert-deftest magent-test-output-mode-c-g-is-not-magent-interrupt ()
  "Test `C-g' in Magent output mode keeps its normal quit role."
  (require 'magent-ui)
  (with-temp-buffer
    (magent-output-mode)
    (should-not (eq (key-binding (kbd "C-g")) 'magent-interrupt))
    (should (eq (lookup-key magent-output-mode-map (kbd "C-c C-c"))
                'magent-ui-submit-or-interrupt))
    (should (eq (lookup-key magent-output-mode-map (kbd "C-c C-o"))
                'magent-ui-compose-from-output))
    (should (eq (lookup-key magent-output-mode-map (kbd "TAB"))
                'magent-ui-toggle-section))
    (should (eq (lookup-key magent-output-mode-map (kbd "<tab>"))
                'magent-ui-toggle-section))))

(ert-deftest magent-test-evil-mode-does-not-override-c-g-in-output-mode ()
  "Test optional Evil integration leaves output `C-g' for quit behavior."
  (skip-unless (require 'evil nil t))
  (require 'magent-evil)
  (let ((magent-evil-mode nil)
        (magent-evil--enabled nil))
    (magent-evil-mode 1)
    (unwind-protect
        (with-temp-buffer
          (magent-output-mode)
          (evil-local-mode 1)
          (evil-normal-state)
          (should-not (eq (key-binding (kbd "C-g")) 'magent-interrupt))
          (should (eq (key-binding (kbd "C-c C-c"))
                      'magent-ui-submit-or-interrupt))
          (should (eq (key-binding (kbd "i"))
                      'magent-ui-compose-from-output)))
      (magent-evil-mode -1))))

(ert-deftest magent-test-evil-does-not-modify-agent-shell-keymap ()
  "Test optional Evil integration only modifies Magent-owned keymaps."
  (require 'magent-evil)
  (let ((magent-evil--enabled t)
        touched-keymaps)
    (cl-letf (((symbol-function 'evil-define-key*)
               (lambda (_state keymap _key _definition &rest _bindings)
                 (push keymap touched-keymaps))))
      (magent-evil--setup-keys)
      (magent-evil--unset-keys))
    (should touched-keymaps)
    (should (seq-every-p (lambda (keymap)
                           (eq keymap magent-output-mode-map))
                         touched-keymaps))))

(ert-deftest magent-test-config-reload-preserves-ui-logger ()
  "Test reloading config preserves the core logger and UI sink."
  (require 'magent-ui)
  (let* ((config-file (expand-file-name "lisp/magent-config.el"
                                        magent-test--root-directory))
         (buffer (magent-ui-get-log-buffer))
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
  "Test rendering history uses the ledger instead of saved buffer text."
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
                "stale saved buffer text")
          (magent-ui-render-history t)
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              (should-not (string-match-p "active=" text))
              (should-not (string-match-p "^Magent  " text))
              (should (string-match-p
                       "- \\[DONE\\] Turn 1  [0-9][0-9]:[0-9][0-9]  hello"
                       text))
              (should (string-match-p "  - Prompt" text))
              (should (string-match-p "    hello" text))
              (should (string-match-p "  - Response" text))
              (should-not (string-match-p "Response \\[completed\\]" text))
              (should (string-match-p "    world" text))
              (should-not (string-match-p "stale saved buffer text" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-ui-render-history-preserves-fragment-fold-state ()
  "Test workspace turn fold state survives ledger re-rendering."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (session (magent-session-create :id "fold"))
         (buffer (magent-ui-get-buffer)))
    (unwind-protect
        (progn
          (magent-session-install 'global session)
          (magent-session-add-message session 'user "hello")
          (magent-session-add-message session 'assistant "world")
          (magent-ui-render-history t)
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "Turn 1")
            (beginning-of-line)
            (magent-ui-toggle-section)
            (should
             (cl-some (lambda (ov) (overlay-get ov 'magent-ui-fold))
                      (overlays-in (point-min) (point-max)))))
          (magent-ui-render-history t)
          (with-current-buffer buffer
            (should
             (cl-some (lambda (ov) (overlay-get ov 'magent-ui-fold))
                      (overlays-in (point-min) (point-max))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-ui-turn-section-title-uses-fixed-status-and-start-time ()
  "Test timeline turn titles use fixed-width status and started-at time."
  (require 'magent-ui)
  (let* ((turn (magent-thread-turn-create
                :status 'queued
                :input "pending prompt"
                :started-at nil))
         (labels (mapcar
                  #'magent-ui--turn-status-label
                  '(queued in-progress completed failed interrupted dropped))))
    (should (equal (delete-dups (mapcar #'length labels)) '(4)))
    (should (equal (magent-ui--turn-section-title turn 7)
                   "[WAIT] Turn 7  --:--  pending prompt"))))

(ert-deftest magent-test-ui-toggle-section-only-toggles-current-fragment ()
  "Test TAB on a parent section ignores folded child sections."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (session (magent-session-create :id "parent-fold"))
         (buffer (magent-ui-get-buffer)))
    (unwind-protect
        (progn
          (magent-session-install 'global session)
          (magent-session-add-message session 'user "use a tool")
          (magent-session-add-tool-message
           session "call-1" "grep" '(:pattern "foo") "foo:1:match")
          (magent-session-add-message session 'assistant "done")
          (magent-ui-render-history t)
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "Turn 1")
            (beginning-of-line)
            (let* ((header-pos (point))
                   (start (get-text-property
                           header-pos 'magent-ui-fragment-body-start))
                   (end (get-text-property
                         header-pos 'magent-ui-fragment-body-end)))
              (should
               (cl-some
                (lambda (ov)
                  (and (overlay-get ov 'magent-ui-fold)
                       (not (and (= (overlay-start ov) start)
                                 (= (overlay-end ov) end)))))
                (overlays-in start end)))
              (magent-ui-toggle-section)
              (should
               (cl-some
                (lambda (ov)
                  (and (overlay-get ov 'magent-ui-fold)
                       (= (overlay-start ov) start)
                       (= (overlay-end ov) end)))
                (overlays-in start end))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-ui-render-history-parses-context-into-meta ()
  "Test auto-context is removed from Prompt and rendered as Meta fields."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (session (magent-session-create :id "context-meta"))
         (buffer (magent-ui-get-buffer)))
    (unwind-protect
        (progn
          (magent-session-install 'global session)
          (magent-session-add-message
           session
           'user
           "[Context: buffer=\"notes.org\" file=\"/tmp/notes.org\" mode=org-mode line=7]\nhello")
          (magent-session-add-message session 'assistant "world")
          (magent-ui-render-history t)
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              (should (string-match-p "    hello" text))
              (should-not (string-match-p
                           "Prompt\n[[:space:]]*\\[Context:"
                           text))
              (should (string-match-p (regexp-quote "  + Meta") text))
              (should (string-match-p "buffer: notes.org" text))
              (should (string-match-p "file: /tmp/notes.org" text))
              (should (string-match-p "mode: org-mode" text))
              (should (string-match-p "line: 7" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-ui-render-history-skips-empty-response-fragment ()
  "Test an empty-content assistant item renders no toggle-breaking fragment.
An assistant turn that produced no text (e.g. tool-only or an empty
completion) must not emit a Response header whose empty body makes
`magent-ui-toggle-section' signal an error."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (session (magent-session-create :id "tool-only-turn"))
         (buffer (magent-ui-get-buffer)))
    (unwind-protect
        (progn
          (magent-session-install 'global session)
          (magent-session-add-message session 'user "run a tool")
          (magent-session-add-message session 'assistant "")
          (magent-ui-render-history t)
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              (should (string-match-p "  - Prompt" text))
              (should-not (string-match-p "Response" text)))
            ;; Toggling the turn section must not error even though the
            ;; turn body is otherwise sparse.
            (goto-char (point-min))
            (search-forward "Turn 1")
            (beginning-of-line)
            (magent-ui-toggle-section)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-ui-render-history-highlights-indented-markdown-heading ()
  "Test markdown headings in assistant text are highlighted despite indentation.
Workspace Response bodies are indented, so the heading regex must
tolerate leading whitespace."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (session (magent-session-create :id "indented-heading"))
         (buffer (magent-ui-get-buffer)))
    (unwind-protect
        (progn
          (magent-session-install 'global session)
          (magent-session-add-message session 'user "summarize")
          (magent-session-add-message session 'assistant "# Summary\nbody text")
          (magent-ui-render-history t)
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "Summary" nil t))
            (let ((face (get-text-property (match-beginning 0) 'face)))
              (should (eq face 'font-lock-function-name-face)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-ui-render-history-timeline-latest-open-history-folded ()
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (session (magent-session-create :id "timeline-folds"))
         (buffer (magent-ui-get-buffer)))
    (unwind-protect
        (progn
          (magent-session-install 'global session)
          (magent-session-add-message session 'user "first")
          (magent-session-add-message session 'assistant "one")
          (magent-session-add-message session 'user "second")
          (magent-session-add-message session 'assistant "two")
          (magent-ui-render-history t)
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              (should (string-match-p
                       "[+] \\[DONE\\] Turn 1  [0-9][0-9]:[0-9][0-9]  first"
                       text))
              (should (string-match-p
                       "- \\[DONE\\] Turn 2  [0-9][0-9]:[0-9][0-9]  second"
                       text))
              (should-not (string-match-p "Current Turn" text))
              (should-not (string-match-p "Recent Turns" text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-ui-render-history-shows-all-timeline-turns ()
  "Test workspace timeline does not truncate to the last four turns."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (session (magent-session-create :id "all-turns"))
         (buffer (magent-ui-get-buffer)))
    (unwind-protect
        (progn
          (magent-session-install 'global session)
          (cl-loop for i from 1 to 5
                   do
                   (magent-session-add-message
                    session 'user (format "prompt-%d" i))
                   (magent-session-add-message
                    session 'assistant (format "reply-%d" i)))
          (magent-ui-render-history t)
          (with-current-buffer buffer
            (let ((text (buffer-string))
                  (pos 0))
              (cl-loop for i from 1 to 5
                       do
                       (setq pos
                             (or (string-match
                                  (format
                                   "\\[DONE\\] Turn %d  [0-9][0-9]:[0-9][0-9]  prompt-%d"
                                   i i)
                                  text
                                  pos)
                                 (ert-fail
                                  (format "Missing timeline turn %d" i))))
                       (setq pos (match-end 0))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-ui-render-history-applies-assistant-text-properties ()
  "Test assistant history text gets lightweight markdown properties."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (session (magent-session-create :id "markdown-props"))
         (buffer (magent-ui-get-buffer)))
    (unwind-protect
        (progn
          (magent-session-install 'global session)
          (magent-session-add-message session 'user "hello")
          (magent-session-add-message
           session 'assistant "# Title\nUse `code` and **bold**.")
          (magent-ui-render-history t)
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "code")
            (should (eq (get-text-property (match-beginning 0) 'face)
                        'font-lock-constant-face))
            (goto-char (point-min))
            (search-forward "bold")
            (should (eq (get-text-property (match-beginning 0) 'face)
                        'bold))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-ui-render-history-renders-tool-details ()
  "Test workspace tool rows include approval and clickable file details."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (session (magent-session-create :id "tool-details"))
         (thread (magent-session-thread-ledger session))
         (turn (magent-thread-create-turn thread "grep"))
         (buffer (magent-ui-get-buffer)))
    (unwind-protect
        (progn
          (magent-thread-record-tool-result
           thread
           (magent-thread-turn-id turn)
           "call-1"
           "grep"
           '(:path "magent-ui.el" :line 10)
           "magent-ui.el:10:match"
           '(:approval-decision allow :approval-source rule-allow))
          (magent-thread-complete-turn thread (magent-thread-turn-id turn))
          (magent-session-install 'global session)
          (magent-session-refresh-projections session)
          (magent-ui-render-history t)
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              (should (string-match-p (regexp-quote "  + Tools (1)") text))
              (should (string-match-p
                       "approval: allow (rule-allow)"
                       text))
              (should (string-match-p "match: magent-ui.el:10" text)))
            (goto-char (point-min))
            (search-forward "  path: ")
            (let ((button (button-at (point))))
              (should button)
              (should (equal (button-get button 'magent-ui-line) 10))
              (should (string-suffix-p
                       "/magent-ui.el"
                       (button-get button 'magent-ui-file))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-ui-revert-buffer-preserves-draft-without-duplicate-prompt ()
  "Test `revert-buffer' leaves compose drafts outside the workspace."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (buffer nil)
         (session (magent-session-create :id "revert")))
    (unwind-protect
        (progn
          (magent-session-install 'global session)
          (setq buffer (magent-ui-get-buffer 'global))
          (magent-session-add-message session 'user "hello")
          (magent-session-add-message session 'assistant "world")
          (magent-test--compose-with-text 'global "draft")
          (magent-ui--revert-buffer nil nil)
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              (should (string-match-p "  - Prompt" text))
              (should (string-match-p "    hello" text))
              (should (string-match-p "  - Response" text))
              (should-not (string-match-p "Response \\[completed\\]" text))
              (should (string-match-p "    world" text))
              (should-not (string-match-p "draft" text))))
          (with-current-buffer (magent-ui-compose-buffer 'global)
            (should (equal (buffer-string) "draft"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-input-prompt-heading-is-editable-until-submit ()
  "Test compose text is editable and cleared after submit."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-editable-input*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (buffer nil)
         (captured nil)
         (section-start nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq buffer (magent-test--compose-with-text
                        'global "editable compose prompt"))
          (with-current-buffer buffer
            (setq section-start (point-min))
            (should-not (get-text-property (point-min) 'read-only)))
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent)))))
            (with-current-buffer buffer
              (magent-input-submit)))
          (should (equal (car captured) "editable compose prompt"))
          (with-current-buffer buffer
            (should (equal (buffer-string) ""))
            (should-not (get-text-property section-start 'read-only))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-input-submit-allows-deleted-prompt-heading ()
  "Test editing compose content before submit does not corrupt submitted text."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-deleted-input-heading*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (buffer nil)
         (captured nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq buffer (magent-test--compose-with-text
                        'global "deleted prefix body survives"))
          (with-current-buffer buffer
            (delete-region (point-min) (+ (point-min) (length "deleted prefix "))))
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui-process)
                     (lambda (text &optional source display skills agent)
                       (setq captured (list text source display skills agent)))))
            (with-current-buffer buffer
              (magent-input-submit)))
          (should (equal (car captured) "body survives")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-resume-session-restores-loaded-buffer-content ()
  "Test resuming a session renders loaded ledger content."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (buffer (magent-ui-get-buffer))
         (loaded-session (magent-session-create
                          :id "loaded"
                          :buffer-content "* [ASSISTANT]\nLoaded session\n")))
    (unwind-protect
        (progn
          (magent-session-add-message loaded-session 'user "hello")
          (magent-session-add-message loaded-session 'assistant "Loaded session")
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
            (should (string-match-p "Loaded session" (buffer-string)))
            (should-not (string-prefix-p "stale buffer content"
                                         (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-resume-session-restores-loaded-prompt-state ()
  "Test resuming a session ignores saved prompt text and renders ledger."
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
                "legacy draft should not render")
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
          (with-current-buffer buffer
            (should (string-match-p "  - Prompt" (buffer-string)))
            (should (string-match-p "    hello" (buffer-string)))
            (should (string-match-p "  - Response" (buffer-string)))
            (should-not (string-match-p "Response \\[completed\\]"
                                        (buffer-string)))
            (should (string-match-p "    world" (buffer-string)))
            (should-not (string-match-p "legacy draft should not render"
                                        (buffer-string)))
            (should-not (string-prefix-p "stale buffer content"
                                         (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-resume-session-snapshots-current-session-before-cross-scope-load ()
  "Test resuming another scope does not snapshot outgoing workspace text."
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
          (should (null captured-snapshot)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (magent-session-reset))))

(ert-deftest magent-test-resume-session-displays-target-project-buffer ()
  "Test resuming another project displays that project's workspace."
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
          (magent-session-add-message loaded-session 'user "hello")
          (magent-session-add-message loaded-session 'assistant
                                      "Loaded project buffer")
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
            (should (string-match-p "Loaded project buffer"
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

(ert-deftest magent-test-resume-session-restores-agent-jobs-for-inspection ()
  "Test resuming a session restores child-agent jobs for transcript inspect."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (loaded-session (magent-session-create
                          :id "loaded"
                          :buffer-content "* [ASSISTANT]\nLoaded\n"))
         displayed)
    (magent-session-add-agent-job
     loaded-session
     (magent-agent-job-create
      :id "agent-1"
      :agent-name "explore"
      :task-name "scan"
      :status 'completed
      :transcript '(((role . "assistant") (content . "found it")))
      :result "found it"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'magent-session-list-files)
                     (lambda () '("/tmp/session.json")))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _args)
                       (car collection)))
                    ((symbol-function 'magent-session--format-file)
                     (lambda (_filepath) "loaded session"))
                    ((symbol-function 'magent-session-read-file)
                     (lambda (_filepath)
                       (list :scope 'global :session loaded-session :id "loaded")))
                    ((symbol-function 'display-buffer)
                     (lambda (buffer &rest _args)
                       buffer))
                    ((symbol-function 'message) #'ignore))
            (magent-resume-session))
          (should (eq (magent-session-agent-job
                       (magent-session-get)
                       "agent-1")
                      (car (magent-session-agent-jobs loaded-session))))
          (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                    ((symbol-function 'magent-ui--activate-context-session)
                     (lambda () (magent-session-get)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _args)
                       (car collection)))
                    ((symbol-function 'display-buffer)
                     (lambda (buffer &rest _args)
                       (setq displayed buffer)
                       buffer)))
            (magent-show-agent-transcript))
          (with-current-buffer displayed
            (should (string-match-p "found it" (buffer-string)))))
      (when (get-buffer "*Magent Agent: agent-1*")
        (kill-buffer "*Magent Agent: agent-1*")))))

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
         (session-b (magent-session-create :id "b"))
         (buffer-a nil))
    (unwind-protect
        (progn
          (magent-session-install scope-a session-a)
          (magent-session-add-message session-a 'user "Project A")
          (magent-session-add-message session-a 'assistant "Project A")
          (setq buffer-a (magent-ui-get-buffer scope-a))
          (with-current-buffer buffer-a
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "* [ASSISTANT]\nProject A\n")))
          (magent-session-install scope-b session-b)
          (with-current-buffer buffer-a
            (magent-ui--revert-buffer nil nil))
          (with-current-buffer buffer-a
            (should (string-match-p "Project A" (buffer-string))))
          (should (null (magent-session-buffer-content session-a))))
      (when (buffer-live-p buffer-a)
        (kill-buffer buffer-a))
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest magent-test-agent-process-emits-turn-events ()
  "Test `magent-agent-process' emits turn lifecycle and text events."
  (require 'magent-lifecycle-events)
  (let ((gptel-backend (gptel-make-openai "test" :key "test-key"))
        (gptel-model 'gpt-4o-mini)
        (captured nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest kwargs)
                 (let ((cb (plist-get kwargs :callback)))
                   (funcall cb "Hello" '(:stream t))
                   (funcall cb t (list :content "Hello")))))
              ((symbol-function 'magent-ui-start-streaming) #'ignore)
              ((symbol-function 'magent-ui-insert-streaming) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (unwind-protect
        (progn
            (magent-lifecycle-events-add-sink (lambda (event) (push event captured)))
            (magent-agent-process "Hello" #'ignore))
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
       '("emacs-runtime-inspection")
       nil
       '(:major-mode emacs-lisp-mode :major-mode-family (emacs-lisp-mode prog-mode))
       (magent-capability-resolution-create
        :skill-names '("emacs-runtime-inspection" "emacs-hook-debugging" "emacs-runtime-inspection"))))
    (should (equal captured-skill-names
                   '("emacs-runtime-inspection" "emacs-hook-debugging")))))

(ert-deftest magent-test-agent-process-emits-capability-resolution-event ()
  "Test `magent-agent-process' emits capability resolution metadata."
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
                   (funcall callback t (list :content "Hello")))))
              ((symbol-function 'magent-ui-start-streaming) #'ignore)
              ((symbol-function 'magent-ui-insert-streaming) #'ignore)
              ((symbol-function 'magent-ui-finish-streaming-fontify) #'ignore))
      (unwind-protect
          (progn
            (magent-lifecycle-events-add-sink (lambda (event) (push event captured)))
            (magent-agent-process
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
    (should (equal (nth 3 captured) '("emacs-runtime-inspection")))
    (should (eq (nth 4 captured) t))
    (should (eq (nth 5 captured) agent))))

(ert-deftest magent-test-run-doctor-dispatches-internal-command ()
  "Test `magent-run-doctor' dispatches the doctor internal command."
  (require 'magent-command)
  (let (captured)
    (cl-letf (((symbol-function 'magent-command-run)
               (lambda (name &rest args)
                 (setq captured (cons name args)))))
      (magent-run-doctor))
    (should (equal (car captured) "doctor"))))

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
      (magent-ui-dispatch-prompt "diag" 'diagnose "display" '("emacs-runtime-inspection") t agent))
    (should (equal (nth 0 captured) "diag"))
    (should (equal (nth 1 captured) 'diagnose))
    (should (equal (nth 2 captured) "display"))
    (should (equal (nth 3 captured) '("emacs-runtime-inspection")))
    (should (eq (nth 4 captured) agent))
    (should (equal (nth 5 captured)
                   '(:buffer-name "*scratch*" :major-mode emacs-lisp-mode)))
    (should (eq (nth 6 captured) 'resolution))))

(ert-deftest magent-test-ui-dispatch-prompt-defaults-to-agent-shell ()
  "Test default UI backend dispatches plain prompts through agent-shell."
  (require 'magent-ui)
  (let ((magent-ui-backend 'agent-shell)
        captured)
    (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
              ((symbol-function 'magent-agent-shell-send-prompt)
               (lambda (prompt &rest _args)
                 (setq captured prompt)))
              ((symbol-function 'magent-ui--enqueue)
               (lambda (&rest _args)
                 (error "legacy enqueue should not be called"))))
      (magent-ui-dispatch-prompt "hello" 'prompt nil nil t))
    (should (equal captured "hello"))))

(ert-deftest magent-test-ui-dispatch-prompt-with-skills-uses-agent-shell ()
  "Test skill-bearing prompts stay on the agent-shell backend."
  (require 'magent-ui)
  (let ((magent-ui-backend 'agent-shell)
        captured)
    (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
              ((symbol-function 'magent-agent-shell-send-prompt)
               (lambda (prompt &rest args)
                 (setq captured (list prompt (plist-get args :skills)))))
              ((symbol-function 'magent-ui--legacy-call)
               (lambda (&rest _args)
                 (error "legacy dispatch should not be called"))))
      (magent-ui-dispatch-prompt
       "hello with skill" 'prompt nil '("init") t))
    (should (equal captured '("hello with skill" ("init"))))))

(ert-deftest magent-test-ui-dispatch-prompt-with-skills-after-legacy-load-uses-agent-shell ()
  "Test skill-bearing prompts keep agent-shell routing after legacy UI loads."
  (require 'magent-ui)
  (require 'magent-ui-legacy)
  (let ((magent-ui-backend 'agent-shell)
        captured)
    (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
              ((symbol-function 'magent-agent-shell-send-prompt)
               (lambda (prompt &rest args)
                 (setq captured (list prompt (plist-get args :skills)))))
              ((symbol-function 'magent-ui-process)
               (lambda (&rest _args)
                 (error "legacy process should not be called"))))
      (magent-ui-dispatch-prompt
       "hello after legacy" 'prompt nil '("init") t))
    (should (equal captured '("hello after legacy" ("init"))))))

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
       :on-failure (lambda (err &optional _raw) (setq failure err))))
    (should-not failure)
    (should (= (map-elt response 'protocolVersion)
               magent-acp-protocol-version))
    (should (equal (map-nested-elt response '(modes currentModeId))
                   magent-default-agent))))

(ert-deftest magent-test-acp-available-commands-list-command-skills ()
  "Test ACP available commands expose command-like skills."
  (require 'magent-acp)
  (let ((magent-skills--registry nil))
    (magent-skills-register
     (magent-skill-create
      :name "init"
      :description '("Initialize project instructions"
                     "similar to Codex /init.")
      :type 'instruction
      :default-prompt "Initialize this project."))
    (magent-skills-register
     (magent-skill-create
      :name "note"
      :description "Plain instruction skill."
      :type 'instruction))
    (let* ((commands (magent-acp--available-commands))
           (names (mapcar (lambda (command)
                            (map-elt command 'name))
                          (append commands nil)))
           (command (cl-find-if (lambda (entry)
                                  (equal (map-elt entry 'name) "init"))
                                (append commands nil))))
      (should (equal names
                     (sort (copy-sequence
                            '("clear" "compact" "init"))
                           #'string<)))
      (should command)
      (should (equal (map-elt command 'description)
                     "Initialize project instructions, similar to Codex /init.")))))

(ert-deftest magent-test-acp-available-commands-lists-all-bundled-slash-commands ()
  "Test ACP available commands expose every bundled slash command skill."
  (require 'magent-acp)
  (let ((magent-skills--registry nil))
    (magent-test--load-builtin-skills-only)
    (let* ((commands (append (magent-acp--available-commands) nil))
           (names (mapcar (lambda (command)
                            (map-elt command 'name))
                          commands)))
      (should (equal names
                     (sort (copy-sequence
                            (append
                             magent-test--builtin-control-command-names
                             magent-test--builtin-slash-command-names))
                           #'string<)))
      (dolist (command commands)
        (let* ((name (map-elt command 'name))
               (skill (magent-skills-get name)))
          (if (member name magent-test--builtin-control-command-names)
              (should-not (string-empty-p
                           (map-elt command 'description)))
            (should skill)
            (should (equal (map-elt command 'description)
                           (magent-acp--metadata-string
                            (magent-skill-description skill))))))))))

(ert-deftest magent-test-acp-slash-command-expands-all-bundled-commands ()
  "Test every bundled slash command expands to its default prompt."
  (require 'magent-acp)
  (let ((magent-skills--registry nil))
    (magent-test--load-builtin-skills-only)
    (should (equal (magent-acp--command-skill-names)
                   magent-test--builtin-slash-command-names))
    (dolist (name magent-test--builtin-slash-command-names)
      (let ((default-prompt (magent-skills-default-prompt name)))
        (should-not (string-blank-p default-prompt))
        (should (equal (plist-get (magent-acp--slash-command
                                   (format "/%s" name))
                                  :prompt)
                       default-prompt))
        (should (equal (plist-get (magent-acp--slash-command
                                   (format "/%s focus on tests" name))
                                  :prompt)
                       (concat default-prompt
                               "\n\nAdditional instruction:\nfocus on tests")))
        (should (equal (plist-get (magent-acp--slash-command
                                   (format "/%s focus on tests" name))
                                  :skills)
                       (list name)))))))

(ert-deftest magent-test-acp-slash-command-parses-session-controls ()
  "Test ACP recognizes clear and compact independently of skills."
  (require 'magent-acp)
  (let ((magent-skills--registry nil))
    (should (equal (magent-acp--slash-command "/clear")
                   '(:kind control :name "clear" :argument "")))
    (should (equal (magent-acp--slash-command
                    "/compact preserve the failing test")
                   '(:kind control
                     :name "compact"
                     :argument "preserve the failing test")))))

(ert-deftest magent-test-acp-session-new-notifies-available-commands ()
  "Test ACP session creation notifies agent-shell of slash commands."
  (require 'magent-acp)
  (let ((runtime-session (magent-runtime-session-create
                          :id "session-1"
                          :scope 'global
                          :magent-session (magent-session-create)))
        response notification failure)
    (cl-letf (((symbol-function 'magent-runtime-prepare-command-context)
               (lambda (_scope) 'global))
              ((symbol-function 'magent-runtime-session-new)
               (lambda (_scope) runtime-session))
              ((symbol-function 'magent-agent-registry-primary-agents)
               (lambda ()
                 (list (magent-agent-info-create
                        :name "build"
                        :description "Build"))))
              ((symbol-function 'magent-acp--available-commands)
               (lambda ()
                 [((name . "init")
                   (description . "Initialize project instructions."))])))
      (magent-acp--handle-request
       `((:notification-handlers
          . (,(lambda (value) (setq notification value))))
         (:request-handlers . nil))
       '((:method . "session/new")
         (:params . ((cwd . "/tmp"))))
       (lambda (value) (setq response value))
       (lambda (err &optional _raw) (setq failure err))))
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
                            values)))))

(ert-deftest magent-test-acp-prompt-text-preserves-embedded-resource ()
  "Test ACP embedded resource blocks keep nested file text in prompts."
  (require 'magent-acp)
  (let ((text (magent-acp--prompt-text
               '[((type . "text")
                  (text . "Review this file"))
                 ((type . "resource")
                  (resource . ((uri . "file:///tmp/example.txt")
                               (text . "line 1\nline 2"))))])))
    (should (string-match-p (regexp-quote "Review this file") text))
    (should (string-match-p
             (regexp-quote "[Context resource: file:///tmp/example.txt]")
             text))
    (should (string-match-p (regexp-quote "line 1\nline 2") text))
    (should-not (string-match-p
                 (regexp-quote "[Context resource: resource]")
                 text))))

(ert-deftest magent-test-acp-session-prompt-expands-slash-command ()
  "Test ACP slash command prompts dispatch command-like skills."
  (require 'magent-acp)
  (let ((runtime-session (magent-runtime-session-create
                          :id "session-1"
                          :pending-skills '("existing-skill")))
        submitted response failure)
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (session-id)
                 (and (equal session-id "session-1")
                      runtime-session)))
              ((symbol-function 'magent-skills-default-prompt)
               (lambda (skill-name)
                 (and (equal skill-name "init")
                      "Initialize this project.")))
              ((symbol-function 'magent-runtime-submit)
               (lambda (session prompt &rest args)
                 (setq submitted (list session prompt))
                 (funcall (plist-get args :on-complete)
                          'completed "ok"))))
      (magent-acp--handle-request
       '((:notification-handlers . nil)
         (:request-handlers . nil))
       '((:method . "session/prompt")
         (:params . ((sessionId . "session-1")
                     (prompt . [((type . "text")
                                 (text . "/init focus on tests"))]))))
       (lambda (value) (setq response value))
       (lambda (err &optional _raw) (setq failure err))))
    (should-not failure)
    (should (equal (nth 1 submitted)
                   "Initialize this project.\n\nAdditional instruction:\nfocus on tests"))
    (should (equal (magent-runtime-session-pending-skills runtime-session)
                   '("existing-skill" "init")))
    (should (equal (map-elt response 'stopReason) "end_turn"))))

(ert-deftest magent-test-acp-session-prompt-expands-all-bundled-slash-commands ()
  "Test ACP session prompts dispatch every bundled slash command skill."
  (require 'magent-acp)
  (let ((magent-skills--registry nil))
    (magent-test--load-builtin-skills-only)
    (dolist (name magent-test--builtin-slash-command-names)
      (let ((runtime-session
             (magent-runtime-session-create
              :id "session-1"
              :pending-skills '("existing-skill")))
            submitted response failure)
        (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
                   (lambda (session-id)
                     (and (equal session-id "session-1")
                          runtime-session)))
                  ((symbol-function 'magent-runtime-submit)
                   (lambda (session prompt &rest args)
                     (setq submitted (list session prompt))
                     (funcall (plist-get args :on-complete)
                              'completed "ok"))))
          (magent-acp--handle-request
           '((:notification-handlers . nil)
             (:request-handlers . nil))
           `((:method . "session/prompt")
             (:params . ((sessionId . "session-1")
                         (prompt . [((type . "text")
                                     (text . ,(format "/%s focus on tests"
                                                      name)))]))))
           (lambda (value) (setq response value))
           (lambda (err &optional _raw) (setq failure err))))
        (should-not failure)
        (should (eq (car submitted) runtime-session))
        (should (equal (cadr submitted)
                       (concat (magent-skills-default-prompt name)
                               "\n\nAdditional instruction:\nfocus on tests")))
        (should (equal (magent-runtime-session-pending-skills runtime-session)
                       (list "existing-skill" name)))
        (should (equal (map-elt response 'stopReason) "end_turn"))))))

(ert-deftest magent-test-acp-session-prompt-leaves-unknown-slash-command-unchanged ()
  "Test unknown slash commands are submitted as normal prompt text."
  (require 'magent-acp)
  (let ((runtime-session (magent-runtime-session-create
                          :id "session-1"
                          :pending-skills '("existing-skill")))
        submitted response failure)
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (session-id)
                 (and (equal session-id "session-1")
                      runtime-session)))
              ((symbol-function 'magent-skills-default-prompt)
               (lambda (_skill-name) nil))
              ((symbol-function 'magent-runtime-submit)
               (lambda (session prompt &rest args)
                 (setq submitted (list session prompt))
                 (funcall (plist-get args :on-complete)
                          'completed "ok"))))
      (magent-acp--handle-request
       '((:notification-handlers . nil)
         (:request-handlers . nil))
       '((:method . "session/prompt")
         (:params . ((sessionId . "session-1")
                     (prompt . [((type . "text")
                                 (text . "/unknown focus on tests"))]))))
       (lambda (value) (setq response value))
       (lambda (err &optional _raw) (setq failure err))))
    (should-not failure)
    (should (eq (car submitted) runtime-session))
    (should (equal (cadr submitted) "/unknown focus on tests"))
    (should (equal (magent-runtime-session-pending-skills runtime-session)
                   '("existing-skill")))
    (should (equal (map-elt response 'stopReason) "end_turn"))))

(ert-deftest magent-test-acp-session-prompt-clears-without-model-submission ()
  "Test /clear resets its runtime session and completes locally."
  (require 'magent-acp)
  (let ((runtime-session (magent-runtime-session-create :id "session-1"))
        cleared submitted response failure notifications)
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (_session-id) runtime-session))
              ((symbol-function 'magent-runtime-session-clear)
               (lambda (session) (setq cleared session)))
              ((symbol-function 'magent-runtime-submit)
               (lambda (&rest _args) (setq submitted t))))
      (magent-acp--handle-request
       `((:notification-handlers
          . (,(lambda (value) (push value notifications))))
         (:request-handlers . nil))
       '((:method . "session/prompt")
         (:params . ((sessionId . "session-1")
                     (prompt . [((type . "text")
                                 (text . "/clear"))]))))
       (lambda (value) (setq response value))
       (lambda (err &optional _raw) (setq failure err))))
    (should-not failure)
    (should (eq cleared runtime-session))
    (should-not submitted)
    (should (equal (map-elt response 'stopReason) "end_turn"))
    (should (equal
             (map-nested-elt (car notifications)
                             '(params update sessionUpdate))
             "agent_message_chunk"))))

(ert-deftest magent-test-acp-session-prompt-compacts-through-runtime ()
  "Test /compact invokes runtime compaction and forwards its completion."
  (require 'magent-acp)
  (let ((runtime-session (magent-runtime-session-create :id "session-1"))
        compact-args response failure)
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (_session-id) runtime-session))
              ((symbol-function 'magent-runtime-session-compact)
               (lambda (session &rest args)
                 (setq compact-args (cons session args))
                 (funcall (plist-get args :on-complete)
                          'completed "summary"))))
      (magent-acp--handle-request
       '((:notification-handlers . nil)
         (:request-handlers . nil))
       '((:method . "session/prompt")
         (:params . ((sessionId . "session-1")
                     (prompt . [((type . "text")
                                 (text . "/compact keep decisions"))]))))
       (lambda (value) (setq response value))
       (lambda (err &optional _raw) (setq failure err))))
    (should-not failure)
    (should (eq (car compact-args) runtime-session))
    (should (equal (plist-get (cdr compact-args) :instruction)
                   "keep decisions"))
    (should (equal (map-elt response 'stopReason) "end_turn"))))

(ert-deftest magent-test-acp-session-prompt-does-not-run-memory-slash-locally ()
  "Test memory slash text is submitted as a normal prompt."
  (require 'magent-acp)
  (let ((runtime-session (magent-runtime-session-create
                          :id "session-1"))
        submitted response failure notifications)
    (cl-letf (((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (session-id)
                 (and (equal session-id "session-1")
                      runtime-session)))
              ((symbol-function 'magent-runtime-submit)
               (lambda (session prompt &rest args)
                 (setq submitted (list session prompt))
                 (funcall (plist-get args :on-complete)
                          'completed
                          "ok"))))
      (magent-acp--handle-request
       `((:notification-handlers
          . (,(lambda (value) (push value notifications))))
         (:request-handlers . nil))
       '((:method . "session/prompt")
         (:params . ((sessionId . "session-1")
                     (prompt . [((type . "text")
                                 (text . "/magent-memory-clear"))]))))
       (lambda (value) (setq response value))
       (lambda (err &optional _raw) (setq failure err))))
    (should-not failure)
    (should (equal submitted
                   (list runtime-session "/magent-memory-clear")))
    (should (equal (map-elt response 'stopReason) "end_turn"))
    (should-not notifications)))

(ert-deftest magent-test-acp-call-failure-supports-rest-wrapper ()
  "Test ACP failures preserve two-argument callbacks through wrapper layers."
  (require 'magent-acp)
  (let (received)
    (magent-acp--call-failure
     (magent-acp--wrap-callback
      '((:context-buffer . nil))
      nil
      (lambda (err raw-message)
        (setq received (list err raw-message))))
     'test-error)
    (should (equal received '(test-error nil)))))

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
  (let* ((runtime-session (magent-runtime-session-create :id "session-1"))
         (client '((:notification-handlers . nil)
                   (:request-handlers . nil)))
         success failure submitted-prompt complete)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args)))
              ((symbol-function 'magent-acp--runtime-session-by-id)
               (lambda (session-id)
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
       :on-failure (lambda (err &optional _raw) (setq failure err))))
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
                  (magent-agent-result-failed
                   "Maximum sampling requests reached"
                   '(:status sampling-limit)))
                 "max_turn_requests"))
  (should (equal (magent-acp--stop-reason
                  'failed
                  (magent-agent-result-failed
                   "Request timed out"
                   '(:status timeout)))
                 "error"))
  (should (equal (magent-acp--stop-reason
                  'failed
                  (magent-agent-result-failed
                   "Model refused"
                   '(:status refusal)))
                 "refusal")))

(ert-deftest magent-test-acp-session-prompt-success-runs-in-request-buffer ()
  "Test deferred ACP prompt callbacks run in the buffer supplied by acp.el."
  (require 'magent-acp)
  (let* ((buffer (generate-new-buffer "*magent-acp-test*"))
         (runtime-session (magent-runtime-session-create :id "session-1"))
         (client '((:notification-handlers . nil)
                   (:request-handlers . nil)))
         complete callback-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args)
                     (apply fn args)))
                  ((symbol-function 'magent-acp--runtime-session-by-id)
                   (lambda (session-id)
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
               (lambda (session-id)
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
               (lambda (session-id)
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

(ert-deftest magent-test-agent-shell-config-creates-in-process-client ()
  "Test Magent agent-shell config creates an in-process ACP client."
  (require 'magent-agent-shell)
  (let* ((agent-shell-agent-configs nil)
         (config (magent-agent-shell-ensure-config))
         (client (funcall (map-elt config :client-maker)
                          (current-buffer))))
    (should (eq (map-elt config :identifier) 'magent))
    (should (eq (map-elt (car agent-shell-agent-configs) :identifier)
                'magent))
    (should (equal (map-elt client :command) "cat"))
    (should (eq (map-elt client :request-sender)
                #'magent-acp--request-sender))
    (should (eq (map-elt client :notification-sender)
                #'magent-acp--notification-sender))
      (should (eq (map-elt client :response-sender)
                #'magent-acp--response-sender))))

(ert-deftest magent-test-agent-shell-start-uses-magent-session-strategy ()
  "Test Magent agent-shell entry points do not inherit global prompt strategy."
  (require 'magent-agent-shell)
  (let ((magent-agent-shell-session-strategy 'new)
        captured)
    (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
               #'ignore)
              ((symbol-function 'agent-shell-start)
               (lambda (&rest _args)
                 (setq captured agent-shell-session-strategy)
                 'shell-buffer)))
      (should (eq (magent-agent-shell-start) 'shell-buffer))
      (should (eq captured 'new)))))

(ert-deftest magent-test-agent-shell-suppresses-blank-line-context ()
  "Test blank current-line context does not produce inverted line ranges."
  (require 'magent-agent-shell)
  (with-temp-buffer
    (insert "first\n\nthird\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((called nil))
      (should-not
       (magent-agent-shell--get-current-line-context
        (lambda (&rest _args)
          (setq called t)
          "bad-context")
        :agent-cwd default-directory))
      (should-not called))
    (goto-char (point-min))
    (let ((called nil))
      (should
       (equal
        (magent-agent-shell--get-current-line-context
         (lambda (&rest _args)
           (setq called t)
           "line-context")
         :agent-cwd default-directory)
        "line-context"))
      (should called))))

(ert-deftest magent-test-agent-shell-send-prompt-queues-skills ()
  "Test agent-shell prompt submission records request-local skills."
  (require 'magent-agent-shell)
  (let ((buffer (generate-new-buffer "*magent-skill-queue*"))
        queued)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq major-mode 'agent-shell-mode)
            (setq-local shell-maker--busy t)
            (setq-local agent-shell--state
                        '((:agent-config . ((:identifier . magent))))))
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-agent-shell--buffer)
                     (lambda (&optional _no-create) buffer))
                    ((symbol-function 'magent-agent-shell--recover-stale-busy)
                     #'ignore)
                    ((symbol-function 'shell-maker-busy)
                     (lambda () t))
                    ((symbol-function 'agent-shell-queue-request)
                     (lambda (prompt)
                       (setq queued prompt))))
            (magent-agent-shell-send-prompt
             "hello" :skills '("init") :no-focus t))
          (should (equal queued "hello"))
          (with-current-buffer buffer
            (should (equal magent-agent-shell--prompt-skill-queue
                           '((:prompt "hello" :skills ("init")))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-agent-shell-prepares-prompt-skills-for-runtime ()
  "Test queued prompt skills are applied to the runtime session."
  (require 'magent-agent-shell)
  (let ((buffer (generate-new-buffer "*magent-skill-runtime*"))
        (runtime-session (magent-runtime-session-create :id "session-1")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq major-mode 'agent-shell-mode)
            (setq-local agent-shell--state
                        '((:agent-config . ((:identifier . magent)))
                          (:session . ((:id . "session-1")))))
            (setq-local magent-agent-shell--prompt-skill-queue
                        '((:prompt "hello" :skills ("init")))))
          (cl-letf (((symbol-function 'magent-runtime-session-from-id)
                     (lambda (session-id &optional _scope)
                       (and (equal session-id "session-1")
                            runtime-session))))
            (magent-agent-shell--prepare-command-skills "hello" buffer))
          (should (equal (magent-runtime-session-pending-skills
                          runtime-session)
                         '("init")))
          (with-current-buffer buffer
            (should-not magent-agent-shell--prompt-skill-queue)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-agent-shell-keeps-prompt-skills-without-runtime ()
  "Test queued prompt skills are not consumed before runtime session exists."
  (require 'magent-agent-shell)
  (let ((buffer (generate-new-buffer "*magent-skill-no-runtime*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq major-mode 'agent-shell-mode)
            (setq-local agent-shell--state
                        '((:agent-config . ((:identifier . magent)))))
            (setq-local magent-agent-shell--prompt-skill-queue
                        '((:prompt "hello" :skills ("init")))))
          (magent-agent-shell--prepare-command-skills "hello" buffer)
          (with-current-buffer buffer
            (should (equal magent-agent-shell--prompt-skill-queue
                           '((:prompt "hello" :skills ("init")))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-agent-shell-run-skill-command-submits-skills ()
  "Test command-like skills submit their default prompt through agent-shell."
  (require 'magent-agent-shell)
  (let (sent cleared)
    (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
              ((symbol-function 'magent-runtime-ensure-initialized) #'ignore)
              ((symbol-function 'magent-agent-shell--prepare-skill-context)
               #'ignore)
              ((symbol-function 'magent-agent-shell--buffer)
               (lambda (&optional _no-create) 'shell-buffer))
              ((symbol-function 'magent-agent-shell--pending-skills)
               (lambda (&optional _shell-buffer)
                 '("existing-skill")))
              ((symbol-function 'magent-agent-shell--clear-pending-skills)
               (lambda (&optional _shell-buffer)
                 (setq cleared t)))
              ((symbol-function 'magent-skills-default-prompt)
               (lambda (skill-name)
                 (and (equal skill-name "init")
                      "Initialize this project.")))
              ((symbol-function 'magent-agent-shell-send-prompt)
               (lambda (prompt &rest args)
                 (setq sent (list prompt (plist-get args :skills))))))
      (magent-agent-shell-run-skill-command "init" "focus on tests"))
    (should cleared)
    (should (equal (nth 0 sent)
                   "Initialize this project.\n\nAdditional instruction:\nfocus on tests"))
    (should (equal (nth 1 sent) '("existing-skill" "init")))))

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
         (persisted (expand-file-name "session-1.json"
                                      magent-session-directory))
         cancelled overrides-cleared)
    (unwind-protect
        (progn
          (magent-session-add-message old-session 'user "old context")
          (with-temp-file persisted (insert "old transcript"))
          (cl-letf (((symbol-function 'magent-runtime-cancel)
                     (lambda (session)
                       (setq cancelled session)
                       0))
                    ((symbol-function
                      'magent-capability-clear-local-overrides)
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
            (should-not (magent-session-messages new-session))
            (should-not
             (magent-runtime-session-pending-skills runtime-session))
            (should-not (file-exists-p persisted))
            (should (eq (magent-session-get-if-present 'global)
                        new-session))))
      (delete-directory magent-session-directory t))))

(ert-deftest magent-test-runtime-session-compact-marks-turn-and-restores-agent ()
  "Test runtime compaction uses the hidden agent and a history boundary."
  (require 'magent-runtime-api)
  (let* ((selected-agent (magent-agent-info-create :name "build"))
         (compaction-agent (magent-agent-info-create :name "compaction"))
         (session (magent-session-create
                   :id "session-1" :agent selected-agent))
         (runtime-session
          (magent-runtime-session-create
           :id "session-1" :scope 'global :magent-session session
           :pending-skills '("review")))
         submitted completion saved pending-during-submit)
    (cl-letf (((symbol-function 'magent-agent-registry-get)
               (lambda (name)
                 (and (equal name "compaction") compaction-agent)))
              ((symbol-function 'magent-runtime-submit)
               (lambda (_runtime prompt &rest args)
                 (setq submitted (cons prompt args)
                       pending-during-submit
                       (magent-runtime-session-pending-skills
                        runtime-session))
                 "submission-1"))
              ((symbol-function 'magent-session-save-deferred-for-session)
               (lambda (saved-session scope &optional _delay)
                 (setq saved (list saved-session scope)))))
      (magent-runtime-session-compact
       runtime-session
       :instruction "keep exact filenames"
       :on-complete
       (lambda (status result)
         (setq completion (list status result))))
      (should (eq (plist-get (cdr submitted) :agent) compaction-agent))
      (should (equal (plist-get (cdr submitted) :turn-metadata)
                     '(:compaction t)))
      (should-not (plist-get (cdr submitted) :skills))
      (should-not pending-during-submit)
      (should (equal (magent-runtime-session-pending-skills runtime-session)
                     '("review")))
      (should (string-match-p
               (regexp-quote
                "Additional instruction:\nkeep exact filenames")
               (car submitted)))
      (magent-session-set-agent session compaction-agent)
      (funcall (plist-get (cdr submitted) :on-complete)
               'completed "summary"))
    (should (eq (magent-session-agent session) selected-agent))
    (should (equal completion '(completed "summary")))
    (should (equal saved (list session 'global)))))

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
    (should (eq (magent-thread-turn-status
                 (car (magent-thread-turns
                       (magent-session-thread-ledger session))))
                'in-progress))
    (should (equal (magent-thread-scope
                    (magent-session-thread-ledger session))
                   "/tmp/project"))))

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

(ert-deftest magent-test-runtime-start-activates-internal-origin-scope ()
  "Test internal runtime work activates its originating project overlay."
  (require 'magent-runtime-api)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (scope (magent-session-internal-scope
                 "internal-1" "test" "/tmp/project-origin"))
         (session (magent-session-create :id "internal-1"))
         (runtime-session
          (magent-runtime-session-create
           :id "internal-1" :scope scope :magent-session session))
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

(ert-deftest magent-test-agent-shell-buffer-selects-only-magent-shells ()
  "Test Magent backend ignores non-Magent agent-shell buffers."
  (require 'magent-agent-shell)
  (let ((foreign (generate-new-buffer "*foreign-agent-shell*"))
        (magent-buffer (generate-new-buffer "*magent-agent-shell*")))
    (unwind-protect
        (progn
          (with-current-buffer foreign
            (setq major-mode 'agent-shell-mode)
            (setq-local default-directory "/tmp/")
            (setq-local agent-shell--state
                        '((:agent-config . ((:identifier . codex))))))
          (with-current-buffer magent-buffer
            (setq major-mode 'agent-shell-mode)
            (setq-local default-directory
                        (file-name-as-directory default-directory))
            (setq-local agent-shell--state
                        '((:agent-config . ((:identifier . magent))))))
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-agent-shell-ensure-config)
                     (lambda () 'config))
                    ((symbol-function 'agent-shell-cwd)
                     (lambda () default-directory))
                    ((symbol-function 'agent-shell-start)
                     (lambda (&rest _args)
                       (error "should not create a shell"))))
            (should (eq (magent-agent-shell--buffer t)
                        magent-buffer))))
      (when (buffer-live-p foreign)
        (kill-buffer foreign))
      (when (buffer-live-p magent-buffer)
        (kill-buffer magent-buffer)))))

(ert-deftest magent-test-agent-shell-submit-trims-trailing-input-whitespace ()
  "Test Magent shell submit removes trailing blank input lines."
  (require 'magent-agent-shell)
  (let ((buffer (generate-new-buffer "*magent-trim-input*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq major-mode 'agent-shell-mode)
          (setq-local agent-shell--state
                      '((:agent-config . ((:identifier . magent)))))
          (insert "Magent> hi\n\nthere\n\n")
          (setq-local comint-last-prompt
                      (cons (copy-marker (point-min))
                            (copy-marker (+ (point-min) (length "Magent> ")))))
          (magent-agent-shell--trim-trailing-input-whitespace)
          (should (equal (buffer-string)
                         "Magent> hi\n\nthere")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-agent-shell-processing-p-is-side-effect-free ()
  "Test processing checks do not initialize runtime or inspect projects."
  (require 'magent-ui)
  (let ((magent-ui-backend 'agent-shell)
        (initialized nil)
        (configured nil)
        (project-looked-up nil)
        (runtime-checked nil))
    (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
               (lambda ()
                 (setq initialized t)
                 (error "processing-p should not initialize runtime")))
              ((symbol-function 'magent-agent-shell-ensure-config)
               (lambda ()
                 (setq configured t)
                 (error "processing-p should not ensure config")))
              ((symbol-function 'agent-shell-cwd)
               (lambda ()
                 (setq project-looked-up t)
                 (error "processing-p should not inspect project")))
              ((symbol-function 'magent-runtime-processing-p)
               (lambda ()
                 (setq runtime-checked t)
                 nil)))
      (should-not (magent-agent-shell-processing-p))
      (should-not (magent-ui-processing-p))
      (should-not initialized)
      (should-not configured)
      (should-not project-looked-up)
      (should runtime-checked))))

(ert-deftest magent-test-agent-shell-send-recovers-stale-busy ()
  "Test prompt dispatch clears agent-shell busy state when no work is live."
  (require 'magent-agent-shell)
  (let ((buffer (generate-new-buffer "*magent-stale-busy*"))
        inserted queued)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq major-mode 'agent-shell-mode)
            (setq-local shell-maker--busy t)
            (setq-local shell-maker--request-process nil)
            (setq-local agent-shell--state
                        `((:agent-config
                           . ((:identifier . magent)
                              (:default-model-id . ,(lambda () "model"))
                              (:default-session-mode-id . ,(lambda () "build"))))
                          (:initialized . t)
                          (:set-model . t)
                          (:set-session-mode . t)
                          (:session . ((:id . "session-1")))
                          (:active-requests . nil)
                          (:pending-restore . nil)
                          (:pending-requests . nil))))
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-agent-shell--buffer)
                     (lambda (&optional _no-create) buffer))
                    ((symbol-function 'shell-maker-busy)
                     (lambda () shell-maker--busy))
                    ((symbol-function 'agent-shell-insert)
                     (lambda (&rest args)
                       (setq inserted args)))
                    ((symbol-function 'agent-shell-queue-request)
                     (lambda (prompt)
                       (setq queued prompt))))
            (magent-agent-shell-send-prompt "hello" :no-focus t))
          (with-current-buffer buffer
            (should-not shell-maker--busy))
          (should-not queued)
          (should (equal (plist-get inserted :text) "hello"))
          (should (plist-get inserted :submit))
          (should (plist-get inserted :no-focus))
          (should (eq (plist-get inserted :shell-buffer) buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-agent-shell-send-queues-real-busy ()
  "Test prompt dispatch keeps using agent-shell queue for live work."
  (require 'magent-agent-shell)
  (let ((buffer (generate-new-buffer "*magent-real-busy*"))
        inserted queued)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq major-mode 'agent-shell-mode)
            (setq-local shell-maker--busy t)
            (setq-local shell-maker--request-process nil)
            (setq-local agent-shell--state
                        `((:agent-config
                           . ((:identifier . magent)
                              (:default-model-id . ,(lambda () "model"))
                              (:default-session-mode-id . ,(lambda () "build"))))
                          (:initialized . t)
                          (:set-model . t)
                          (:set-session-mode . t)
                          (:session . ((:id . "session-1")))
                          (:active-requests . (((:method . "session/prompt"))))
                          (:pending-restore . nil)
                          (:pending-requests . nil))))
          (cl-letf (((symbol-function 'magent-runtime-ensure-initialized)
                     #'ignore)
                    ((symbol-function 'magent-agent-shell--buffer)
                     (lambda (&optional _no-create) buffer))
                    ((symbol-function 'shell-maker-busy)
                     (lambda () shell-maker--busy))
                    ((symbol-function 'agent-shell-insert)
                     (lambda (&rest args)
                       (setq inserted args)))
                    ((symbol-function 'agent-shell-queue-request)
                     (lambda (prompt)
                       (setq queued prompt))))
            (magent-agent-shell-send-prompt "hello" :no-focus t))
          (with-current-buffer buffer
            (should shell-maker--busy))
          (should-not inserted)
          (should (equal queued "hello")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-agent-shell-processing-p-ignores-stale-busy ()
  "Test stale shell-maker busy state is not reported as Magent processing."
  (require 'magent-agent-shell)
  (let ((buffer (generate-new-buffer "*magent-stale-processing*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq major-mode 'agent-shell-mode)
            (setq-local default-directory
                        (file-name-as-directory default-directory))
            (setq-local shell-maker--busy t)
            (setq-local shell-maker--request-process nil)
            (setq-local agent-shell--state
                        `((:agent-config
                           . ((:identifier . magent)
                              (:default-model-id . ,(lambda () "model"))
                              (:default-session-mode-id . ,(lambda () "build"))))
                          (:initialized . t)
                          (:set-model . t)
                          (:set-session-mode . t)
                          (:session . ((:id . "session-1")))
                          (:active-requests . nil)
                          (:pending-restore . nil)
                          (:pending-requests . nil))))
          (should-not (magent-agent-shell-processing-p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
              ((symbol-function 'magent-agent-process)
               (lambda (&rest _args) 'fsm)))
      (magent-ui--run-item item))
    (with-current-buffer buffer
      (should (string-match-p "Capability resolver: Auto capabilities: org-structure | Suggested: git-workflow"
                              (buffer-string))))))

(ert-deftest magent-test-ui-finish-processing-restores-prompt-before-save ()
  "Test request completion ensures compose exists before session save."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-finish-order*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (magent-ui--processing t)
         (magent-legacy-queue--active nil)
         (magent-legacy-queue--pending nil)
         (buffer nil)
         (prompt-visible-at-save nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq buffer (magent-ui-get-buffer 'global))
          (cl-letf (((symbol-function
                      'magent-session-save-deferred-for-session)
                     (lambda (_session _scope &optional _delay)
                       (setq prompt-visible-at-save
                             (buffer-live-p
                              (magent-ui-compose-buffer 'global))))))
            (magent-ui--finish-processing "done"))
          (should-not magent-ui--processing)
          (should (buffer-live-p (magent-ui-compose-buffer 'global)))
          (should prompt-visible-at-save))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-ui-finish-processing-failed-result-releases-input ()
  "Test failed agent results release processing and keep compose available."
  (require 'magent-ui)
  (let* ((magent-buffer-name "*magent-finish-failed*")
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (magent-ui--processing t)
         (magent-legacy-queue--active nil)
         (magent-legacy-queue--pending nil)
         (buffer nil))
    (unwind-protect
        (progn
          (magent-session-activate 'global)
          (setq buffer (magent-ui-get-buffer 'global))
          (cl-letf (((symbol-function 'magent-session-save-deferred) #'ignore))
            (magent-ui--finish-processing
             (magent-agent-result-failed "Request timed out after 5 seconds")))
          (should-not magent-ui--processing)
          (should (buffer-live-p (magent-ui-compose-buffer 'global))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-ui-capability-summary-renders-status-line ()
  "Test capability summaries render as compact status lines."
  (require 'magent-ui)
  (magent-ui-clear-buffer)
  (magent-ui-insert-capability-summary "Auto capabilities: org-structure")
  (with-current-buffer (magent-ui-get-buffer)
    (should (equal (buffer-string)
                   "Capability resolver: Auto capabilities: org-structure\n"))))

(ert-deftest magent-test-ui-streaming-applies-text-properties-to-flushed-chunk ()
  "Test streaming text properties are applied to newly flushed chunks."
  (require 'magent-ui)
  (magent-ui-clear-buffer)
  (magent-ui-start-streaming)
  (magent-ui-insert-streaming "Use `code`")
  (magent-ui--flush-streaming-batch)
  (with-current-buffer (magent-ui-get-buffer)
    (goto-char (point-min))
    (search-forward "code")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-constant-face))))

(ert-deftest magent-test-ui-reasoning-block-stays-expanded ()
  "Test reasoning renders as a compact status row."
  (require 'magent-ui)
  (let ((fold-call nil))
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
                     "Reasoning [done] 5 chars\n"))
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
      (should (string-match-p "Tool emacs_eval running" text))
      (should (string-match-p "\"tool\":\"emacs_eval\"" text))
      (should (string-match-p "\"values\":\\[\"emacs_eval\",null\\]" text)))))

(ert-deftest magent-test-ui-agent-job-event-renders-compact-block ()
  "Test child-agent lifecycle UI renders compact metadata only."
  (require 'magent-ui)
  (let* ((buffer (magent-ui-get-buffer))
         (job (magent-agent-job-create
               :id "agent-1"
               :agent-name "explore"
               :task-name "scan"
               :status 'completed
               :transcript '(((role . "assistant")
                              (content . "large transcript body")))
               :result "answer"))
         fold-call)
    (magent-ui-clear-buffer)
    (cl-letf (((symbol-function 'magent-ui--fold-block-at)
               (lambda (pos block-re)
                 (setq fold-call (list pos block-re)))))
      (magent-ui-insert-agent-job-event 'completed job "answer"))
    (with-current-buffer buffer
      (let ((text (buffer-string)))
        (should (string-match-p "Agent completed agent-1" text))
        (should (string-match-p "agent: explore" text))
        (should (string-match-p "task: scan" text))
        (should (string-match-p "status: completed" text))
        (should (string-match-p "detail: answer" text))
        (should-not (string-match-p "large transcript body" text))))
    (should-not fold-call)))

(ert-deftest magent-test-show-agent-transcript-displays-session-job ()
  "Test child-agent transcript inspection displays persisted job details."
  (require 'magent-ui)
  (let* ((magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--current-scope 'global)
         (magent--current-session (magent-session-create :id "parent"))
         (job (magent-agent-job-create
               :id "agent-1"
               :agent-name "explore"
               :task-name "scan"
               :status 'completed
               :prompt "inspect files"
               :metadata '((project-root . "/tmp/project")
                           (skill-names . ["search"]))
               :transcript '(((role . "user") (content . "inspect"))
                             ((role . "assistant") (content . "found it")))
               :result "found it"))
         displayed)
    (magent-session-add-agent-job magent--current-session job)
    (unwind-protect
        (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                  ((symbol-function 'magent-ui--activate-context-session)
                   (lambda () magent--current-session))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _args)
                     (car collection)))
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _args)
                     (setq displayed buffer)
                     buffer)))
          (magent-show-agent-transcript)
          (should displayed)
          (with-current-buffer displayed
            (let ((text (buffer-string)))
              (should (string-match-p "Child Agent agent-1" text))
              (should (string-match-p "Agent: explore" text))
              (should (string-match-p "Task: scan" text))
              (should (string-match-p "Status: completed" text))
              (should (string-match-p "Prompt" text))
              (should (string-match-p "inspect files" text))
              (should (string-match-p "found it" text))
              (should (string-match-p "skill-names: search" text)))))
      (when (get-buffer "*Magent Agent: agent-1*")
        (kill-buffer "*Magent Agent: agent-1*")))))

(ert-deftest magent-test-mode-map-binds-entry-points ()
  "Test `magent-mode-map' keeps prompt and transient entry points."
  (require 'magent)
  (should (eq (lookup-key magent-mode-map (kbd "C-c m p"))
              'magent-dwim))
  (should (eq (lookup-key magent-mode-map (kbd "C-c m r"))
              'magent-prompt-region))
  (should (eq (lookup-key magent-mode-map (kbd "C-c m a"))
              'magent-ask-at-point))
  (should (eq (lookup-key magent-mode-map (kbd "C-c m ?"))
              'magent-transient-menu)))

(ert-deftest magent-test-mode-map-keeps-formal-commands-in-transient ()
  "Test formal commands are not directly bound under `magent-mode-map'."
  (require 'magent)
  (dolist (key '("C-c m d" "C-c m D" "C-c m c" "C-c m R"
                 "C-c m x" "C-c m e" "C-c m k" "C-c m l"
                 "C-c m L" "C-c m t" "C-c m A" "C-c m T"
                 "C-c m j" "C-c m i" "C-c m v"))
    (should-not (lookup-key magent-mode-map (kbd key)))))

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

;; ──────────────────────────────────────────────────────────────────────
;;; Codex-like runtime skeleton tests
;; ──────────────────────────────────────────────────────────────────────

(ert-deftest magent-test-turn-runtime-queues-submissions ()
  "Test turn runtime queues submissions and starts the next one on finish."
  (require 'magent-legacy-queue)
  (let ((magent-legacy-queue--active nil)
        (magent-legacy-queue--pending nil)
        (magent-legacy-queue--current-request-handle nil)
        started)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args)
                 'timer)))
      (magent-legacy-queue-submit
       (magent-protocol-user-input-op "one")
       "one"
       (lambda (submission)
         (push (magent-legacy-queue-submission-payload submission) started)))
      (magent-legacy-queue-submit
       (magent-protocol-user-input-op "two")
       "two"
       (lambda (submission)
         (push (magent-legacy-queue-submission-payload submission) started)))
      (should (equal (nreverse started) '("one")))
      (should (magent-legacy-queue-processing-p))
      (should (magent-legacy-queue-pending-p))
      (magent-legacy-queue-finish 'completed)
      (should (equal (nreverse started) '("one" "two")))
      (should (magent-legacy-queue-processing-p))
      (should-not (magent-legacy-queue-pending-p)))))

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

(ert-deftest magent-test-session-records-tool-results-in-structured-context ()
  "Test tool result messages also populate Codex-like context items."
  (require 'magent-session)
  (let ((session (magent-session-create)))
    (magent-session-add-tool-message
     session "call-1" "grep" '(:pattern "foo") "match")
    (let ((items (magent-session-context-items session)))
      (should (= (length items) 1))
      (should (eq (magent-response-item-type (car items)) 'tool))
      (should (equal (magent-response-item-call-id (car items)) "call-1"))
      (should (equal (magent-response-item-name (car items)) "grep"))
      (should (equal (magent-response-item-output (car items)) "match"))
      (should (eq (magent-response-item-status (car items)) 'completed)))))

(ert-deftest magent-test-session-truncates-model-visible-tool-results ()
  "Test oversized tool results are truncated before session prompt reuse."
  (require 'magent-session)
  (let ((magent-tool-result-model-max-length 80)
        (magent-tool-result-model-preview-length 40)
        (session (magent-session-create))
        (payload (make-string 200 ?x)))
    (magent-session-add-message session 'user "Run tool")
    (magent-session-add-tool-message
     session "call-1" "emacs_eval" '(:sexp "(big)") payload)
    (let* ((tool-msg (cadr (magent-session-get-messages session)))
           (result (plist-get (magent-msg-content tool-msg) :result))
           (prompt (magent-session-to-gptel-prompt-list session))
           (prompt-tool (cdr (cadr prompt))))
      (should (< (length result) (length payload)))
      (should (string-prefix-p (make-string 40 ?x) result))
      (should (string-match-p "Tool result truncated" result))
      (should-not (equal result payload))
      (should (equal (plist-get prompt-tool :result) result)))))

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
            (magent-session-add-message session 'user "hello")
            (magent-session-add-tool-message
             session "call-1" "grep" '(:pattern "hello") "match")
            (magent-session-add-message session 'assistant "done")
            (magent-session-save))
          (let* ((files (directory-files magent-session-directory t "\\.json$"))
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
            (should (equal (mapcar #'magent-msg-role
                                   (magent-session-get-messages loaded-session))
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
              (magent-session-add-message
               session 'user (format "message-%d" index)))
            (should (> (length (magent-thread-journal
                                (magent-session-thread-ledger session)))
                       magent-session-journal-max-events))
            (magent-session-save))
          (let* ((file (car (directory-files
                             magent-session-directory t "\\.json\\'")))
                 (json-object-type 'alist)
                 (json-array-type 'list)
                 (data (with-temp-buffer
                         (insert-file-contents file)
                         (json-read)))
                 (journal (cdr (assq 'journal data)))
                 (loaded (magent-session-read-file file)))
            (should (= (length journal) 3))
            (should (= (length (magent-session-get-messages
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
              (lambda (_tool callback arg-values)
                (setq executed-path (car arg-values))
                (funcall callback "ok"))
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
          (should (magent-tool-result-p result))
          (should (eq (magent-tool-result-status-value result) 'failed))
          (should (plist-get (magent-tool-result-metadata result)
                             :resource-identity-changed))
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

(ert-deftest magent-test-session-save-load-sanitizes-structured-context ()
  "Test structured context items persist with JSON-safe metadata."
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
            (magent-session-add-message session 'user "Run tool")
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
               '(:result ok)
               '(:provider gptel :tool emacs_eval))
              (magent-session-refresh-projections session))
            (magent-session-save))
          (let* ((files (directory-files magent-session-directory t "\\.json$"))
                 (loaded (magent-session-read-file (car files)))
                 (loaded-session (plist-get loaded :session))
                 (item (cadr (magent-session-context-items loaded-session))))
            (should (equal (cdr (assq 'tool (magent-response-item-content item)))
                           "emacs_eval"))
            (should (equal (cdr (assq 'values (magent-response-item-content item)))
                           '("emacs_eval" nil)))
            (should (equal (magent-response-item-name item) "emacs_eval"))
            (should (equal (cdr (assq 'result (magent-response-item-output item)))
                           "ok"))
            (should (equal (cdr (assq 'provider (magent-response-item-metadata item)))
                           "gptel"))
            (should (equal (cdr (assq 'tool (magent-response-item-metadata item)))
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
              :transcript '(((role . assistant)
                             (content . (:tool emacs_eval :values [emacs_eval nil]))))
              :result 'ok
              :error nil
              :metadata '((permission-profile . (agent bash emacs_eval))
                           (model . gpt-4o-mini)))))
            (magent-session-save))
          (let* ((files (directory-files magent-session-directory t "\\.json$"))
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

(ert-deftest magent-test-session-live-legacy-messages-migrate-before-projection ()
  "A live legacy session must not lose messages when its ledger is created."
  (let* ((messages '(((role . user) (content . "hello"))
                     ((role . assistant) (content . "hi"))))
         (session (magent-session-create
                   :id "legacy-live" :messages (copy-tree messages))))
    (should-not (magent-session-thread session))
    (let ((thread (magent-session-thread-ledger session)))
      (should (eq (plist-get (magent-thread-metadata thread) :migrated-from)
                  'messages))
      (should (equal (mapcar #'magent-msg-content
                             (magent-session-messages session))
                     '("hello" "hi"))))))

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
          (magent-session-refresh-projections session)
          (magent-session-save-for-session session 'global)
          (let* ((file (car (directory-files
                             magent-session-directory t "\\.json\\'")))
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

(ert-deftest magent-test-session-schema-version-gates-future-files ()
  "Unversioned legacy sessions load, while future schemas fail clearly."
  (let* ((directory (make-temp-file "magent-schema-" t))
         (magent-session-directory directory)
         (legacy (expand-file-name "legacy.json" directory))
         (future (expand-file-name "future.json" directory))
         logs)
    (unwind-protect
        (progn
          (with-temp-file legacy
            (insert "{\"id\":\"legacy\",\"scope\":\"global\",\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}"))
          (with-temp-file future
            (insert (format
                     "{\"id\":\"future\",\"schema-version\":%d,\"scope\":\"global\",\"messages\":[]}"
                     (1+ magent-session-schema-version))))
          (should (magent-session-read-file legacy))
          (cl-letf (((symbol-function 'magent-log)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) logs))))
            (should-not (magent-session-read-file future)))
          (should (cl-some (lambda (line)
                             (string-match-p "newer than supported" line))
                           logs)))
      (delete-directory directory t))))

(ert-deftest magent-test-memory-async-commit-preserves-latest-user-notes ()
  "Memory completion re-reads notes edited while the provider is sampling."
  (require 'magent-memory)
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
  (require 'magent-memory)
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
  (require 'magent-memory)
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
  (require 'magent-memory)
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
  (require 'magent-doctor)
  (let* ((called nil)
         (probe (magent-doctor-probe-create
                 :id "zero"
                 :timeout 0
                 :collector (lambda (_context _state)
                              (setq called t)
                              '((ok . t)))))
         (state (magent-doctor-state-create :deadline nil)))
    (cl-letf (((symbol-function 'magent-command-notify) #'ignore)
              ((symbol-function 'magent-command-record-tool) #'ignore))
      (let ((result (magent-doctor--run-probe probe nil state)))
        (should called)
        (should (equal (cdr (assq 'status result)) "completed"))))))

(ert-deftest magent-test-doctor-project-probe-does-not-freeze-process-timeout ()
  "The project probe leaves subprocess timeout policy to its collector."
  (require 'magent-doctor)
  (let ((probe (gethash "project" magent-doctor--registry)))
    (should probe)
    (should (= (magent-doctor-probe-timeout probe) 0))))

(ert-deftest magent-test-memory-stale-clear-confirmation-cannot-write ()
  "A delayed clear approval cannot write after a newer operation supersedes it."
  (require 'magent-memory)
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
  (let* ((magent-enable-tools magent-tools--permission-keys)
         (profile (magent-permission-intersect
                   '((edit . (("src/*.el" . allow) (* . deny)))
                     (bash . deny)
                     (* . allow))
                   '((* . allow))))
         (tools (magent-tools-get-gptel-tools-for-permission profile))
         (names (mapcar #'gptel-tool-name tools)))
    (should (member "edit_file" names))
    (should-not (member "bash" names))))

(ert-deftest magent-test-tools-hide-empty-resource-permission-intersection ()
  "Do not expose a tool when intersected resource allowlists are disjoint."
  (require 'magent-tools)
  (let* ((magent-enable-tools magent-tools--permission-keys)
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
                  (magent-tools-get-gptel-tools-for-permission disjoint)))
         (overlapping-names
          (mapcar #'gptel-tool-name
                  (magent-tools-get-gptel-tools-for-permission overlapping))))
    (should-not (magent-permission-tool-available-p disjoint 'edit))
    (should-not (member "edit_file" disjoint-names))
    (should (magent-permission-tool-available-p overlapping 'edit))
    (should (member "edit_file" overlapping-names))))

(ert-deftest magent-test-tools-expose-nontrivial-resource-glob-intersection ()
  "Exposure has no witness-heuristic false negative for overlapping globs."
  (require 'magent-tools)
  (let* ((magent-enable-tools magent-tools--permission-keys)
         (permission
          (magent-permission-intersect
           '((edit . (("src/*a.el" . allow) (* . deny))))
           '((edit . (("src/a*.el" . allow) (* . deny))))))
         (names
          (mapcar #'gptel-tool-name
                  (magent-tools-get-gptel-tools-for-permission permission))))
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
            (lambda (_tool cb args)
              (push (car args) ran)
              (funcall cb "ok"))
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
            (should (string-prefix-p (file-name-as-directory outside)
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
           (lambda (value) (setq result value)) file "" "replacement")
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
          (should (string-match-p "needle" (magent-tool-result-output-string
                                             single-result)))
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
          (should (string-match-p (regexp-quote "src/one.el") result))
          (should (string-match-p (regexp-quote "src/nested/two.el") result))
          (should-not (string-match-p (regexp-quote "test/outside.el") result))
          (should-not (string-match-p (regexp-quote "root.el") result)))
      (delete-directory root t))))

(ert-deftest magent-test-audit-strict-redaction-and-private-modes ()
  "Audit persistence omits arbitrary free text and enforces private modes."
  (require 'magent-audit)
  (let* ((directory (make-temp-file "magent-audit-private-" t))
         (magent-enable-audit-log t)
         (magent-audit-directory directory)
         (magent-audit--pending-writes nil)
         (magent-audit--flush-timer nil)
         (magent-session--current-scope 'global)
         (magent--current-session (magent-session-create))
         (file (magent-audit--daily-file-path))
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
                 :options '((style . "strict")
                            (path . "C:\\tmp")
                            (count . 2))
                 :steps 7
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
          (should (equal (magent-agent-info-options loaded)
                         '((style . "strict")
                           (path . "C:\\tmp")
                           (count . 2))))
          (should (= (magent-agent-info-steps loaded) 7))
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
            (insert "---\nname: ordered\ntype: instruction\ncapability: true\ntitle: First Title\nkeywords: first-only\n---\nFirst body."))
          (with-temp-file (expand-file-name "SKILL.md" second)
            (insert "---\nname: ordered\ntype: instruction\ncapability: true\ntitle: Second Title\nkeywords: second-only\n---\nSecond body."))
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

(ert-deftest magent-test-agent-file-legacy-tools-allowlist-is-restrictive ()
  "Legacy comma-list tools enable only the named permission groups."
  (require 'magent-agent-file)
  (let ((rules (magent-agent-file--parse-tools '("read_file" "grep"))))
    (should (eq (magent-permission-resolve rules 'read) 'allow))
    (should (eq (magent-permission-resolve rules 'grep) 'allow))
    (should (eq (magent-permission-resolve rules 'bash) 'deny))))

(ert-deftest magent-test-agent-info-symbol-model-overrides-gptel-model ()
  "A file-backed symbol model is applied without changing the backend."
  (require 'magent-agent-info)
  (let ((gptel-model 'default-model)
        captured)
    (magent-agent-info-apply-gptel-overrides
     (magent-agent-info-create :name "model" :mode 'primary :model 'custom-model)
     (lambda () (setq captured gptel-model)))
    (should (eq captured 'custom-model))))

(ert-deftest magent-test-project-tool-skill-companion-requires-explicit-trust ()
  "Project companion Elisp is skipped by default and loaded when trusted."
  (require 'magent-skills)
  (let* ((root (file-truename (make-temp-file "magent-skill-project-" t)))
         (directory (expand-file-name ".magent/skills/trust-test" root))
         (file (expand-file-name "SKILL.md" directory))
         (companion (expand-file-name "trust-test.el" directory))
         (invoke 'magent-skill-trust-test-invoke)
         (magent-skills--registry nil)
         (magent-trusted-project-skill-companion-roots nil)
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
            (let ((skill (magent-skills-load-file file)))
              (should skill)
              (should-not loaded)
              (should-not (magent-skill-invoke-function skill))))
          (setq magent-skills--registry nil
                magent-trusted-project-skill-companion-roots (list root)
                loaded nil)
          (cl-letf (((symbol-function 'load-file)
                     (lambda (_file)
                       (setq loaded t)
                       (fset invoke #'ignore)))
                    ((symbol-function 'magent-log) #'ignore))
            (let ((skill (magent-skills-load-file file)))
              (should loaded)
              (should (eq (magent-skill-invoke-function skill) invoke)))))
      (when (fboundp invoke) (fmakunbound invoke))
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
            (insert "---\nname: link-skill\ntype: instruction\n---\nSkill."))
          (with-temp-file cap-source
            (insert "---\nname: link-cap\ntitle: Linked\nskills: link-skill\n---\nCap."))
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

(ert-deftest magent-test-project-symlink-tool-skill-does-not-bypass-trust ()
  "A symlinked SKILL.md remains project-owned and cannot execute by default."
  (require 'magent-skills)
  (let* ((root (file-truename (make-temp-file "magent-skill-link-" t)))
         (outside (make-temp-file "magent-skill-link-source-" t))
         (directory (expand-file-name ".magent/skills/link-tool" root))
         (source (expand-file-name "SKILL.md" outside))
         (file (expand-file-name "SKILL.md" directory))
         (companion (expand-file-name "link-tool.el" directory))
         (magent-skills--registry nil)
         (magent-trusted-project-skill-companion-roots nil)
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
              (should (eq (magent-skill-source-layer skill) 'project))
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

(ert-deftest magent-test-global-arbiter-preserves-cross-backend-fifo ()
  "Legacy and runtime submissions share one arrival-ordered global FIFO."
  (require 'magent-legacy-queue)
  (let ((magent-runtime-queue--active nil)
        (magent-runtime-queue--pending nil)
        (magent-runtime-queue--arbiter-active nil)
        (magent-runtime-queue--arbiter-pending nil)
        (magent-legacy-queue--active nil)
        (magent-legacy-queue--pending nil)
        (magent-legacy-queue--current-request-handle nil)
        order)
    (cl-letf (((symbol-function 'magent-session-get) (lambda () nil))
              ((symbol-function 'magent-session-current-scope)
               (lambda () 'global))
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args)
                 'timer)))
      (magent-legacy-queue-submit
       nil "L1" (lambda (_submission) (setq order (append order '(L1)))))
      (magent-runtime-queue-submit
       (magent-runtime-submission-create :id "R1")
       (lambda (_submission) (setq order (append order '(R1)))))
      (magent-legacy-queue-submit
       nil "L2" (lambda (_submission) (setq order (append order '(L2)))))
      (should (equal order '(L1)))
      (magent-legacy-queue-finish 'completed)
      (should (equal order '(L1 R1)))
      (magent-runtime-queue-finish-active 'completed)
      (should (equal order '(L1 R1 L2)))
      (magent-legacy-queue-finish 'completed)
      (magent-runtime-queue-submit
       (magent-runtime-submission-create :id "R2")
       (lambda (_submission) (setq order (append order '(R2)))))
      (magent-legacy-queue-submit
       nil "L3" (lambda (_submission) (setq order (append order '(L3)))))
      (magent-runtime-queue-submit
       (magent-runtime-submission-create :id "R3")
       (lambda (_submission) (setq order (append order '(R3)))))
      (magent-runtime-queue-finish-active 'completed)
      (magent-legacy-queue-finish 'completed)
      (magent-runtime-queue-finish-active 'completed)
      (should (equal order '(L1 R1 L2 R2 L3 R3)))
      (should-not (magent-runtime-queue-arbiter-owner)))))

(ert-deftest magent-test-legacy-interrupt-invalidates-deferred-dispatch ()
  "An interrupted legacy submission's zero-delay timer cannot dispatch."
  (require 'magent-legacy-queue)
  (let ((magent-runtime-queue--active nil)
        (magent-runtime-queue--pending nil)
        (magent-runtime-queue--arbiter-active nil)
        (magent-runtime-queue--arbiter-pending nil)
        (magent-legacy-queue--active nil)
        (magent-legacy-queue--pending nil)
        (magent-legacy-queue--current-request-handle nil)
        deferred dispatched)
    (cl-letf (((symbol-function 'magent-session-get) (lambda () nil))
              ((symbol-function 'magent-session-current-scope)
               (lambda () 'global))
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (setq deferred (cons fn args))
                 'timer)))
      (magent-legacy-queue-submit
       nil "stale" (lambda (_submission) (setq dispatched t)))
      (magent-legacy-queue-interrupt)
      (apply (car deferred) (cdr deferred))
      (should-not dispatched)
      (should-not (magent-runtime-queue-arbiter-owner)))))

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

(ert-deftest magent-test-legacy-starter-error-rolls-back-backend-and-ledger ()
  "A synchronous legacy startup error leaves neither backend nor turn active."
  (require 'magent-legacy-queue)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-legacy-queue--active nil)
         (magent-legacy-queue--pending nil)
         (magent-legacy-queue--current-request-handle nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--loaded-sessions (make-hash-table :test #'eq :weakness 'key))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (session (magent-session-create :id "legacy-start-failure")))
    (magent-session-install 'global session)
    (cl-letf (((symbol-function 'magent-runtime-activate-scope) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (&rest _args) (error "timer setup failed"))))
      (should-error
       (magent-legacy-queue-submit nil "prompt" #'ignore)
       :type 'error))
    (should-not magent-legacy-queue--active)
    (should-not magent-legacy-queue--pending)
    (should-not (magent-runtime-queue-arbiter-owner))
    (let ((turn (car (magent-thread-turns
                      (magent-session-thread-ledger session)))))
      (should (eq (magent-thread-turn-status turn) 'failed)))))

(ert-deftest magent-test-legacy-queue-start-and-finish-use-captured-session ()
  "Legacy start and finish never mutate another ambient session's ledger."
  (require 'magent-legacy-queue)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-legacy-queue--active nil)
         (magent-legacy-queue--pending nil)
         (magent-legacy-queue--current-request-handle nil)
         (magent-session--scoped-sessions (make-hash-table :test #'equal))
         (magent-session--loaded-sessions (make-hash-table :test #'eq :weakness 'key))
         (magent-session--current-scope 'global)
         (magent--current-session nil)
         (session-a (magent-session-create :id "legacy-a"))
         (session-b (magent-session-create :id "legacy-b"))
         (scope-a "/tmp/magent-legacy-a")
         (scope-b "/tmp/magent-legacy-b")
         timers
         dispatched)
    (cl-letf (((symbol-function 'magent-runtime-activate-scope) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (setq timers (append timers (list (cons fn args))))
                 'timer)))
      (magent-session-install scope-a session-a)
      (magent-legacy-queue-submit
       nil "prompt-a"
       (lambda (_submission)
         (push (list 'a (magent-session-current-scope)
                     (magent-session-get))
               dispatched)))
      ;; Switch ambient state before queueing B and before A's timer runs.
      (magent-session-install scope-b session-b)
      (magent-legacy-queue-submit
       nil "prompt-b"
       (lambda (_submission)
         (push (list 'b (magent-session-current-scope)
                     (magent-session-get))
               dispatched)))
      (apply (caar timers) (cdar timers))
      (should (equal (car dispatched) (list 'a scope-a session-a)))
      (magent-session-install scope-b session-b)
      (magent-legacy-queue-finish 'completed)
      (let* ((turn-a (car (magent-thread-turns
                           (magent-session-thread-ledger session-a))))
             (turn-b (car (magent-thread-turns
                           (magent-session-thread-ledger session-b)))))
        (should (eq (magent-thread-turn-status turn-a) 'completed))
        (should (eq (magent-thread-turn-status turn-b) 'in-progress)))
      (apply (caadr timers) (cdadr timers))
      (should (equal (car dispatched) (list 'b scope-b session-b)))
      (magent-session-install scope-a session-a)
      (magent-legacy-queue-finish 'completed)
      (let ((turn-b (car (magent-thread-turns
                          (magent-session-thread-ledger session-b)))))
        (should (eq (magent-thread-turn-status turn-b) 'completed)))
      (should (equal (mapcar #'magent-msg-content
                            (magent-session-messages session-a))
                     '("prompt-a")))
      (should (equal (mapcar #'magent-msg-content
                            (magent-session-messages session-b))
                     '("prompt-b"))))))

(ert-deftest magent-test-legacy-stale-zero-timer-cannot-touch-new-active ()
  "An old zero-delay timer cannot dispatch or finish a replacement submission."
  (require 'magent-legacy-queue)
  (let ((magent-runtime-queue--active nil)
        (magent-runtime-queue--pending nil)
        (magent-runtime-queue--arbiter-active nil)
        (magent-runtime-queue--arbiter-pending nil)
        (magent-legacy-queue--active nil)
        (magent-legacy-queue--pending nil)
        (magent-legacy-queue--current-request-handle nil)
        timers
        dispatched)
    (cl-letf (((symbol-function 'magent-session-get) (lambda () nil))
              ((symbol-function 'magent-session-current-scope)
               (lambda () 'global))
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ;; Reusing ids makes this a regression for identity checks, not
              ;; merely for the normal uniqueness of generated ids.
              ((symbol-function 'magent-protocol-generate-id)
               (lambda (&optional _prefix) "reused-id"))
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (setq timers (append timers (list (cons fn args))))
                 'timer)))
      (magent-legacy-queue-submit
       nil "old" (lambda (_submission) (push 'old dispatched)))
      (let ((old-timer (car timers)))
        (magent-legacy-queue-interrupt)
        (magent-legacy-queue-submit
         nil "new" (lambda (_submission) (push 'new dispatched)))
        (let ((new-active (magent-legacy-queue-active-submission))
              (new-timer (cadr timers)))
          (should (equal (magent-legacy-queue-submission-id new-active)
                         "reused-id"))
          (apply (car old-timer) (cdr old-timer))
          (should-not dispatched)
          (should (eq (magent-legacy-queue-active-submission) new-active))
          (should (eq (magent-legacy-queue-submission-status new-active)
                      'running))
          (apply (car new-timer) (cdr new-timer))
          (should (equal dispatched '(new)))
          (should (eq (magent-legacy-queue-active-submission) new-active)))))))

(ert-deftest magent-test-ui-finish-saves-captured-legacy-session-and-scope ()
  "Legacy UI completion persists the completed submission, not ambient state."
  (require 'magent-ui)
  ;; Load the implementation before installing function mocks; otherwise the
  ;; thin router's lazy load would legitimately replace those mock bindings.
  (require 'magent-ui-legacy)
  (let* ((magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-legacy-queue--pending nil)
         (session-a (magent-session-create :id "ui-a"))
         (session-b (magent-session-create :id "ui-b"))
         (submission
          (magent-legacy-queue-submission-create
           :id "ui-a-turn" :session session-a :scope "/tmp/ui-a"
           :status 'running))
         (magent-legacy-queue--active submission)
         (magent--current-session session-b)
         (magent-session--current-scope "/tmp/ui-b")
         saved
         snapshots
         rendered)
    (cl-letf (((symbol-function 'magent-ui--refresh-header-line) #'ignore)
              ((symbol-function 'magent-ui-render-history)
               (lambda (_skip scope) (push scope rendered)))
              ((symbol-function 'magent-ui--maybe-show-input-prompt) #'ignore)
              ((symbol-function 'magent-ui--snapshot-buffer-content)
               (lambda (session scope) (push (list session scope) snapshots)))
              ((symbol-function 'magent-session-save-deferred-for-session)
               (lambda (session scope &optional _delay)
                 (push (list session scope) saved))))
      (magent-ui--finish-processing "done" "ui-a-turn" submission))
    (should (equal saved (list (list session-a "/tmp/ui-a"))))
    (should (equal snapshots (list (list session-a "/tmp/ui-a"))))
    (should (equal rendered '("/tmp/ui-a")))
    (should-not magent-legacy-queue--active)))

(ert-deftest magent-test-acp-session-list-filters-exact-cwd-scope ()
  "ACP session/list exposes only sessions belonging to the requested cwd."
  (require 'magent-acp)
  (cl-letf (((symbol-function 'magent-session-scope-from-directory)
             (lambda (cwd) (if (equal cwd "/project-a") "/project-a" 'global)))
            ((symbol-function 'magent-runtime-list-sessions)
             (lambda ()
               '((:id "a" :scope "/project-a" :project-root "/project-a"
                  :updated-at 0.0)
                 (:id "b" :scope "/project-b" :project-root "/project-b"
                  :updated-at 0.0)
                 (:id "g" :scope global :project-root nil :updated-at 0.0)))))
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
    (should (equal completion '(cancelled "Active turn cancelled")))
    (should (memq 'turn-start events))
    (should (memq 'turn-cancelled events))
    (should-not (magent-runtime-queue-active-submission))
    (should-not (magent-runtime-queue-arbiter-owner))
    (let ((turn (car (magent-thread-turns
                      (magent-session-thread-ledger session)))))
      (should (eq (magent-thread-turn-status turn) 'interrupted)))))

(ert-deftest magent-test-legacy-finish-is-reentrant-safe ()
  "A finishing lifecycle sink cannot advance twice and clobber its successor."
  (require 'magent-legacy-queue)
  (let ((magent-runtime-queue--active nil)
        (magent-runtime-queue--pending nil)
        (magent-runtime-queue--arbiter-active nil)
        (magent-runtime-queue--arbiter-pending nil)
        (magent-legacy-queue--active nil)
        (magent-legacy-queue--pending nil)
        (magent-legacy-queue--current-request-handle nil)
        (magent-lifecycle-events--sinks nil)
        first
        second
        reentered)
    (cl-letf (((symbol-function 'magent-session-get) (lambda () nil))
              ((symbol-function 'magent-session-current-scope)
               (lambda () 'global))
              ((symbol-function 'magent-runtime-activate-scope) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (&rest _args) 'timer)))
      (magent-lifecycle-events-add-sink
       (lambda (event)
         (when (and (eq (plist-get event :type) 'submission-finished)
                    (equal (plist-get event :submission-id)
                           (magent-legacy-queue-submission-id first))
                    (not reentered))
           (setq reentered t)
           (magent-legacy-queue-finish 'completed))))
      (magent-legacy-queue-submit nil "first" #'ignore)
      (setq first (magent-legacy-queue-active-submission))
      (magent-legacy-queue-submit nil "second" #'ignore)
      (setq second (car magent-legacy-queue--pending))
      (magent-legacy-queue-finish-submission first 'completed)
      (should reentered)
      (should (eq (magent-legacy-queue-active-submission) second))
      (should (eq (magent-legacy-queue-submission-status second) 'running)))))

(ert-deftest magent-test-legacy-received-reentry-preserves-fifo ()
  "A received sink may submit more work without overtaking its current token."
  (require 'magent-legacy-queue)
  (let ((magent-runtime-queue--active nil)
        (magent-runtime-queue--pending nil)
        (magent-runtime-queue--arbiter-active nil)
        (magent-runtime-queue--arbiter-pending nil)
        (magent-legacy-queue--active nil)
        (magent-legacy-queue--pending nil)
        (magent-legacy-queue--current-request-handle nil)
        (magent-lifecycle-events--sinks nil)
        spawned
        order)
    (cl-letf (((symbol-function 'magent-session-get) (lambda () nil))
              ((symbol-function 'magent-session-current-scope)
               (lambda () 'global))
              ((symbol-function 'magent-runtime-activate-scope) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args)
                 'timer)))
      (magent-lifecycle-events-add-sink
       (lambda (event)
         (when (and (eq (plist-get event :type) 'submission-received)
                    (not spawned))
           (setq spawned t)
           (magent-legacy-queue-submit
            nil "second"
            (lambda (_submission) (setq order (append order '(second))))))))
      (magent-legacy-queue-submit
       nil "first"
       (lambda (_submission) (setq order (append order '(first)))))
      (should (equal order '(first)))
      (magent-legacy-queue-finish 'completed)
      (should (equal order '(first second))))))

(ert-deftest magent-test-legacy-interrupt-invalidates-before-sync-abort-callback ()
  "A synchronous abort callback observes the interrupted token as stale."
  (require 'magent-legacy-queue)
  (let ((magent-runtime-queue--active nil)
        (magent-runtime-queue--pending nil)
        (magent-runtime-queue--arbiter-active nil)
        (magent-runtime-queue--arbiter-pending nil)
        (magent-legacy-queue--active nil)
        (magent-legacy-queue--pending nil)
        (magent-legacy-queue--current-request-handle nil)
        active
        queued
        abort-saw-active)
    (cl-letf (((symbol-function 'magent-session-get) (lambda () nil))
              ((symbol-function 'magent-session-current-scope)
               (lambda () 'global))
              ((symbol-function 'magent-runtime-activate-scope) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (&rest _args) 'timer)))
      (magent-legacy-queue-submit nil "active" #'ignore)
      (setq active (magent-legacy-queue-active-submission))
      (magent-legacy-queue-set-current-request-handle 'handle active)
      (magent-legacy-queue-submit nil "queued" #'ignore)
      (setq queued (car magent-legacy-queue--pending))
      (magent-legacy-queue-interrupt
       (lambda (_handle)
         (setq abort-saw-active (magent-legacy-queue-active-submission))
         (magent-legacy-queue-finish-submission active 'completed))))
    (should-not abort-saw-active)
    (should (eq (magent-legacy-queue-submission-status active) 'interrupted))
    (should (eq (magent-legacy-queue-submission-status queued) 'dropped))
    (should-not (magent-legacy-queue-active-submission))
    (should-not (magent-runtime-queue-arbiter-owner))))

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
      (should-error (magent-runtime-prepare-command-context "/b")
                    :type 'user-error)
      (should-not activated)
      (magent-runtime-prepare-command-context "/a")
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
             (lambda (err &optional _raw) (setq failure err)))
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

(ert-deftest magent-test-agent-shell-runtime-session-uses-buffer-scope ()
  "Agent-shell resolves duplicate ids with its own buffer's project scope."
  (require 'magent-agent-shell)
  (let ((buffer (generate-new-buffer "*magent-agent-shell-scope-test*"))
        (runtime-b (magent-runtime-session-create :id "same" :scope "/b"))
        lookup)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq major-mode 'agent-shell-mode)
            (setq-local agent-shell--state
                        '((:agent-config . ((:identifier . magent)))
                          (:session . ((:id . "same"))))))
          (cl-letf (((symbol-function 'magent-session-scope-from-directory)
                     (lambda (_directory) "/b"))
                    ((symbol-function 'magent-runtime-session-from-id)
                     (lambda (session-id scope)
                       (setq lookup (list session-id scope))
                       runtime-b))
                    ((symbol-function 'magent-acp--runtime-session-by-id)
                     (lambda (&rest _args)
                       (error "unexpected ACP fallback"))))
            (should (eq (magent-agent-shell--runtime-session buffer) runtime-b)))
          (should (equal lookup '("same" "/b"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-ui-sync-completion-does-not-store-stale-handle ()
  "A synchronously completed legacy turn cannot attach its handle to the next."
  (require 'magent-ui-legacy)
  (let* ((magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-legacy-queue--active nil)
         (magent-legacy-queue--pending nil)
         (magent-legacy-queue--current-request-handle nil)
         (magent-ui--request-generation 0)
         (magent--current-request-handle nil)
         (item (magent-ui--request-create :prompt "first" :source 'prompt))
         timers
         first
         second)
    (cl-letf (((symbol-function 'magent-session-get) (lambda () nil))
              ((symbol-function 'magent-session-current-scope)
               (lambda () 'global))
              ((symbol-function 'magent-runtime-activate-scope) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (setq timers (append timers (list (cons fn args))))
                 'timer))
              ((symbol-function 'magent-ui-display-buffer) #'ignore)
              ((symbol-function 'magent-ui-render-history) #'ignore)
              ((symbol-function 'magent-log) #'ignore)
              ((symbol-function 'magent-agent-process)
               (lambda (_prompt callback &rest _args)
                 (funcall callback "done")
                 'stale-handle))
              ((symbol-function 'magent-ui--finish-processing)
               (lambda (_response _submission-id submission)
                 (magent-legacy-queue-finish-submission
                  submission 'completed "done"))))
      (magent-legacy-queue-submit
       nil item
       (lambda (submission)
         (magent-ui--run-item
          item (magent-legacy-queue-submission-id submission))))
      (setq first (magent-legacy-queue-active-submission))
      (magent-legacy-queue-submit nil "second" #'ignore)
      (setq second (car magent-legacy-queue--pending))
      (apply (caar timers) (cdar timers)))
    (should (magent-legacy-queue-submission-finalized first))
    (should (eq (magent-legacy-queue-active-submission) second))
    (should-not magent--current-request-handle)
    (should-not (magent-legacy-queue-current-request-handle))))

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
             (lambda (err &optional _raw) (setq failure err)))
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
              ((symbol-function 'magent-runtime-prepare-command-context)
               (lambda (scope) (setq prepared scope))))
      (magent-acp--handle-request
       nil
       '((:method . "session/load")
         (:params . ((sessionId . "candidate") (cwd . "/target"))))
       #'ignore
       (lambda (err &optional _raw) (setq failure err))))
    (should failure)
    (should-not prepared)))

(ert-deftest magent-test-agent-shell-runtime-session-prefers-client-binding ()
  "Agent-shell lookup does not drift when its buffer directory changes."
  (require 'magent-agent-shell)
  (let* ((magent-acp--client-session-scopes
          (make-hash-table :test #'eq :weakness 'key))
         (magent-runtime-api--sessions (make-hash-table :test #'equal))
         (buffer (generate-new-buffer "*magent-agent-shell-fixed-scope*"))
         (client `((:context-buffer . ,buffer)))
         (runtime
          (magent-runtime-session-create
           :id "fixed" :scope "/explicit"
           :magent-session (magent-session-create :id "fixed"))))
    (unwind-protect
        (progn
          (puthash (list "/explicit" "fixed") runtime
                   magent-runtime-api--sessions)
          (magent-acp--bind-client-session client runtime)
          (with-current-buffer buffer
            (setq major-mode 'agent-shell-mode)
            (setq-local default-directory "/changed/")
            (setq-local agent-shell--state
                        `((:agent-config . ((:identifier . magent)))
                          (:client . ,client)
                          (:session . ((:id . "fixed"))))))
          (should (eq (magent-agent-shell--runtime-session buffer) runtime)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
              ((symbol-function 'magent-capability-clear-local-overrides)
               #'ignore))
      (magent-runtime-session-clear runtime))
    (should reentrant-error)
    (should-not (magent-runtime-queue-active-submission))
    (should (= (magent-runtime-pending-count runtime) 0))
    (should-not (magent-session-messages session))))

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

(ert-deftest magent-test-legacy-clear-terminalizes-all-before-drop-sinks ()
  "The first dropped sink observes every detached token as terminal."
  (require 'magent-legacy-queue)
  (let* ((magent-legacy-queue--submission-metadata
          (make-hash-table :test #'eq :weakness 'key))
         (magent-legacy-queue--active nil)
         (magent-legacy-queue--pending nil)
         (magent-lifecycle-events--sinks nil)
         (first (magent-legacy-queue-submission-create :id "first"))
         (second (magent-legacy-queue-submission-create :id "second"))
         observed)
    (setq magent-legacy-queue--pending (list first second))
    (magent-lifecycle-events-add-sink
     (lambda (event)
       (when (and (eq (plist-get event :type) 'submission-dropped)
                  (equal (plist-get event :submission-id) "first"))
         (setq observed
               (list (magent-legacy-queue-submission-finalized second)
                     (magent-legacy-queue-submission-status second))))))
    (should (= (magent-legacy-queue-clear) 2))
    (should (equal observed '(t dropped)))))

(ert-deftest magent-test-session-id-validation-and-filename-consistency ()
  "Unsafe ids cannot escape storage and mismatched files cannot load."
  (let* ((directory (make-temp-file "magent-session-id-" t))
         (magent-session-directory directory)
         (bad (magent-session-create
               :id "../escape"
               :messages '(((role . user) (content . "unsafe")))))
         (mismatch (expand-file-name "filename.json" directory))
         (missing (expand-file-name "missing.json" directory))
         (legacy (expand-file-name "legacy.json" directory))
         logs)
    (unwind-protect
        (progn
          (should-error (magent-session-save-for-session bad 'global)
                        :type 'magent-session-schema-error)
          (with-temp-file mismatch
            (insert
             "{\"id\":\"other\",\"schema-version\":5,\"scope\":\"global\",\"messages\":[]}"))
          (with-temp-file missing
            (insert
             "{\"schema-version\":5,\"scope\":\"global\",\"messages\":[]}"))
          (with-temp-file legacy
            (insert
             "{\"scope\":\"global\",\"messages\":[{\"role\":\"user\",\"content\":\"ok\"}]}"))
          (cl-letf (((symbol-function 'magent-log)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) logs))))
            (should-not (magent-session-read-file mismatch))
            (should-not (magent-session-read-file missing)))
          (should (cl-some
                   (lambda (line)
                     (string-match-p "does not match filename" line))
                   logs))
          (let ((loaded (magent-session-read-file legacy)))
            (should loaded)
            (should (equal (plist-get loaded :id) "legacy")))
          (should (equal (magent-session-list-files) (list legacy))))
      (delete-directory directory t))))

(ert-deftest magent-test-queue-finish-public-contract-returns-id-or-nil ()
  "Public finish APIs never leak the arbiter's internal handled sentinel."
  (require 'magent-legacy-queue)
  (let ((magent-runtime-queue--active nil)
        (magent-runtime-queue--pending nil)
        (magent-runtime-queue--arbiter-active nil)
        (magent-runtime-queue--arbiter-pending nil)
        (magent-legacy-queue--active nil)
        (magent-legacy-queue--pending nil)
        (magent-legacy-queue--current-request-handle nil))
    (let ((first (magent-runtime-submission-create :id "runtime-first"))
          (second (magent-runtime-submission-create :id "runtime-second")))
      (magent-runtime-queue-submit first #'ignore)
      (magent-runtime-queue-submit second #'ignore)
      (should (equal (magent-runtime-queue-finish-active 'completed)
                     "runtime-second"))
      (should-not (magent-runtime-queue-finish-active 'completed)))
    (cl-letf (((symbol-function 'magent-session-get) (lambda () nil))
              ((symbol-function 'magent-session-current-scope)
               (lambda () 'global))
              ((symbol-function 'magent-runtime-activate-scope) #'ignore)
              ((symbol-function 'magent-lifecycle-events-emit) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (&rest _args) 'timer)))
      (let ((first-id (magent-legacy-queue-submit nil "first" #'ignore))
            second-id)
        (ignore first-id)
        (setq second-id
              (magent-legacy-queue-submit nil "second" #'ignore))
        (should (equal (magent-legacy-queue-finish 'completed) second-id))
        (should-not (magent-legacy-queue-finish 'completed))))))

(ert-deftest magent-test-agent-shell-interrupt-honors-force-argument ()
  "Magent forwards nil and non-nil FORCE without forcing confirmation off."
  (require 'magent-agent-shell)
  (let ((buffer (generate-new-buffer "*magent-agent-shell-interrupt*"))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'magent-agent-shell--buffer)
                   (lambda (&optional _no-create) buffer))
                  ((symbol-function 'agent-shell-interrupt)
                   (lambda (force) (push force calls))))
          (magent-agent-shell-interrupt nil)
          (magent-agent-shell-interrupt 'force)
          (should (equal (nreverse calls) '(nil force))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magent-test-live-reload-struct-layouts-remain-stable ()
  "Runtime side metadata must not invalidate objects created before reload."
  (require 'magent-agent-loop)
  (require 'magent-legacy-queue)
  (should (= (length (magent-lifecycle-events-context-create)) 5))
  (should (= (length (magent-request-context-create)) 27))
  (should (= (length (magent-agent-loop-create)) 19))
  (should (= (length (magent-runtime-submission-create)) 21))
  (should (= (length (magent-legacy-queue-submission-create)) 10)))

(ert-deftest magent-test-live-reload-adopts-preserved-runtime-queue ()
  "Pre-arbiter active work keeps its lease and queued work starts once."
  (require 'magent-runtime-queue)
  (let* ((active
          (magent-runtime-submission-create
           :id "pre-reload-active" :status 'running :started-at 1.0))
         (pending
          (magent-runtime-submission-create
           :id "pre-reload-pending" :status 'queued :submitted-at 2.0))
         (magent-runtime-queue--active active)
         (magent-runtime-queue--pending (list pending))
         (magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-runtime-queue--submission-starters
          (make-hash-table :test #'eq :weakness 'key))
         starts)
    (magent-runtime-queue--set-submission-starter
     pending (lambda (_submission) (push 'pending starts)))
    (magent-runtime-queue--bootstrap-preserved-backends)
    (should (eq (magent-runtime-queue-active-submission) active))
    (should (eq (magent-runtime-queue-arbiter-owner) 'runtime))
    (should-not starts)
    (should (equal (magent-runtime-queue-finish-active 'completed)
                   "pre-reload-pending"))
    (should (equal starts '(pending)))
    (should (eq (magent-runtime-queue-active-submission) pending))
    (should-not magent-runtime-queue--pending)))

(provide 'magent-test)
;;; magent-test.el ends here
