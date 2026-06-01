;;; magent-live-test.el --- Live Emacs tests for Magent  -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests run inside an already running Emacs instance via emacsclient.
;; The :magent-live tests use the real gptel provider configured in that
;; Emacs instance and may consume tokens.  The :magent-live-smoke tests keep a
;; deterministic stub transport while still exercising live timers, buffers,
;; mode state, and asynchronous callbacks.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(defconst magent-live-test--root-directory
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root for the live test suite.")

(defvar magent-live-test--latest-results nil
  "Result records from the most recent live ERT run.")

;; Loading this file into a long-lived Emacs process should not leave renamed
;; smoke tests in ERT's symbol-property registry.
(put 'magent-live-test-fsm-runs-emacs-eval-and-continues 'ert--test nil)

(defun magent-live-test--add-load-path (directory)
  "Add DIRECTORY to `load-path' when it exists."
  (when (and directory (file-directory-p directory))
    (add-to-list 'load-path (file-truename directory))))

(defun magent-live-test--first-elpa-directory (pattern)
  "Return the first ELPA directory matching PATTERN, or nil."
  (let ((elpa-dir (expand-file-name "elpa" user-emacs-directory)))
    (when (file-directory-p elpa-dir)
      (car (directory-files elpa-dir t pattern t)))))

(defun magent-live-test--prepare-load-path ()
  "Add the repo and package dependency directories to `load-path'."
  (magent-live-test--add-load-path magent-live-test--root-directory)
  (dolist (pattern '("\\`gptel-[0-9]"
                     "\\`magit-[0-9]"
                     "\\`magit-section-"
                     "\\`spinner-"
                     "\\`transient-"
                     "\\`cond-let-"
                     "\\`evil-"
                     "\\`yaml-[0-9]"
                     "\\`llama-"
                     "\\`with-editor-"))
    (magent-live-test--add-load-path
     (magent-live-test--first-elpa-directory pattern))))

(defun magent-live-test--load-source-file (file)
  "Load FILE from the repository root, bypassing stale .elc files."
  (load (expand-file-name file magent-live-test--root-directory) nil t))

(defun magent-live-test-reload-source ()
  "Reload Magent source files into the live Emacs instance."
  (interactive)
  (magent-live-test--prepare-load-path)
  (dolist (file '("magent-config.el"
                  "magent-agent-job.el"
                  "magent-llm.el"
                  "magent-llm-gptel.el"
                  "magent-agent-loop.el"
                  "magent-events.el"
                  "magent-audit.el"
                  "magent-session.el"
                  "magent-approval.el"
                  "magent-file-loader.el"
                  "magent-runtime.el"
                  "magent-agent-registry.el"
                  "magent-agent-info.el"
                  "magent-agent-types.el"
                  "magent-permission.el"
                  "magent-tools.el"
                  "magent-tool-registry.el"
                  "magent-tool-orchestrator.el"
                  "magent-turn.el"
                  "magent-agent-file.el"
                  "magent-md2org.el"
                  "magent-agent.el"
                  "magent-skills.el"
                  "magent-capability.el"
                  "magent-ui.el"
                  "magent.el"))
    (magent-live-test--load-source-file file)))

(defun magent-live-test--wait-until (predicate &optional timeout message)
  "Wait until PREDICATE returns non-nil, or fail after TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 5.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (redisplay))
    (unless value
      (ert-fail (or message "Timed out waiting for live Magent condition")))
    value))

(defun magent-live-test--debug-state ()
  "Return compact live Magent state for assertion failures."
  (format "processing=%S current-scope=%S magent-buffer=%S log-tail=%S"
          (and (fboundp 'magent-ui-processing-p)
               (magent-ui-processing-p))
          (and (fboundp 'magent-session-current-scope)
               (magent-session-current-scope))
          (when (fboundp 'magent-ui-get-buffer)
            (let ((buffer (magent-ui-get-buffer (magent-session-current-scope))))
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (buffer-substring-no-properties
                   (max (point-min) (- (point-max) 1200))
                   (point-max))))))
          (when (and (boundp 'magent-log-buffer-name)
                     (get-buffer magent-log-buffer-name))
            (with-current-buffer magent-log-buffer-name
              (buffer-substring-no-properties
               (max (point-min) (- (point-max) 1200))
               (point-max))))))

(defun magent-live-test--wait-for-assistant (&optional timeout)
  "Wait for a completed assistant message and return its text."
  (magent-live-test--wait-until
   (lambda ()
     (let* ((session (magent-session-get))
            (messages (magent-session-get-messages session))
            (last-msg (car (last messages)))
            (content (and last-msg (magent-msg-content last-msg))))
       (and (not (magent-ui-processing-p))
            (eq (and last-msg (magent-msg-role last-msg)) 'assistant)
            (stringp content)
            (not (string-empty-p (string-trim content)))
            content)))
   (or timeout 120)
   (format "Timed out waiting for a live assistant response: %s"
           (magent-live-test--debug-state))))

(defun magent-live-test--require-real-gptel ()
  "Fail with a clear message unless live gptel is configured."
  (require 'gptel)
  (unless (and (boundp 'gptel-backend) gptel-backend)
    (ert-fail "No live `gptel-backend' is configured in the running Emacs"))
  (unless (and (boundp 'gptel-model) gptel-model)
    (ert-fail "No live `gptel-model' is configured in the running Emacs")))

(defun magent-live-test--kill-magent-test-buffers ()
  "Kill live-test Magent buffers left by previous runs."
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*magent-live-test" (buffer-name buffer))
      (kill-buffer buffer))))

(defmacro magent-live-test--with-isolated-runtime (&rest body)
  "Run BODY with isolated Magent session, audit, and UI state."
  (declare (indent 0))
  `(let* ((magent-buffer-name "*magent-live-test*")
          (magent-log-buffer-name "*magent-live-test-log*")
          (magent-audit-buffer-name "*magent-live-test-audit*")
          (magent-session-directory (make-temp-file "magent-live-sessions-" t))
          (magent-audit-directory (make-temp-file "magent-live-audit-" t))
          (magent-enable-audit-log nil)
          (magent-enable-capabilities nil)
          (magent-auto-context nil)
          (magent-auto-scroll nil)
          (magent-by-pass-permission nil)
          (magent-ui-batch-insert-delay 0.01)
          (magent-ui--processing nil)
          (magent-ui--request-generation 0)
          (magent--current-request-handle nil)
          (magent--current-session nil)
          (magent-turn--active nil)
          (magent-turn--queue nil)
          (magent-turn--current-request-handle nil)
          (magent-session--current-scope 'global)
          (magent-session--scoped-sessions (make-hash-table :test #'equal))
          (magent-runtime--active-project-scope nil)
          (magent-load-custom-agents nil))
     (unwind-protect
         (progn
           (magent-live-test--kill-magent-test-buffers)
           (magent-session-activate 'global)
           ,@body)
       (when (and (fboundp 'magent-interrupt)
                  (boundp 'magent-ui--processing)
                  magent-ui--processing)
         (ignore-errors (magent-interrupt)))
       (magent-live-test--kill-magent-test-buffers)
       (when (file-directory-p magent-session-directory)
         (delete-directory magent-session-directory t))
       (when (file-directory-p magent-audit-directory)
         (delete-directory magent-audit-directory t)))))

(ert-deftest magent-live-test-buffer-input-submits-through-timer ()
  "Submit editable buffer input through the real live UI dispatch timer."
  :tags '(:magent-live-smoke)
  (require 'magent)
  (magent-live-test--with-isolated-runtime
    (let ((response nil))
      (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                ((symbol-function 'magent-runtime-activate-scope) #'ignore)
                ((symbol-function 'magent-capability-capture-context) (lambda () nil))
                ((symbol-function 'magent-capability-resolve-for-turn) (lambda (&rest _) nil))
                ((symbol-function 'magent-agent-process)
                 (lambda (_prompt callback &rest _args)
                   (run-at-time
                    0 nil
                    (lambda ()
                      (magent-ui-start-streaming)
                      (magent-ui-insert-streaming "live response")
                      (magent-ui-finish-streaming-fontify)
                      (setq response "live response")
                      (funcall callback "live response")))
                   'magent-live-test-loop)))
        (let ((buffer (magent-ui-get-buffer 'global)))
          (with-current-buffer buffer
            (magent-output-mode)
            (magent-ui--insert-input-prompt 'global)
            (goto-char (point-max))
            (insert "hello from live buffer")
            (magent-input-submit))
          (magent-live-test--wait-until
           (lambda ()
             (and response (not (magent-ui-processing-p))))
           5 "Magent live buffer input did not finish")
          (with-current-buffer buffer
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "^\\* USER" text))
              (should (string-match-p "hello from live buffer" text))
              (should (string-match-p "^\\* ASSISTANT" text))
              (should (string-match-p "live response" text))
              (should magent-ui--input-marker)
              (should (= (marker-position magent-ui--input-marker) (point-max))))))))))

(ert-deftest magent-live-test-loop-runs-emacs-eval-and-continues ()
  "Run a live Magent turn through loop tool execution and continuation."
  :tags '(:magent-live-smoke)
  (require 'magent)
  (require 'gptel)
  (magent-live-test--with-isolated-runtime
    (let ((call-count 0)
          (final-response nil)
          (magent-by-pass-permission t)
          (magent-enable-tools '(emacs_eval)))
      (cl-letf (((symbol-function 'magent-capability-capture-context) (lambda () nil))
                ((symbol-function 'magent-capability-resolve-for-turn) (lambda (&rest _) nil))
                ((symbol-function 'gptel-request)
                 (lambda (_prompt &rest kwargs)
                   (cl-incf call-count)
                   (let ((callback (plist-get kwargs :callback)))
                     (pcase call-count
                       (1
                        (let* ((tool-spec
                                (cl-find-if
                                 (lambda (tool)
                                   (equal (gptel-tool-name tool) "emacs_eval"))
                                 gptel-tools))
                               (args '(:sexp "(length (buffer-list))"
                                             :reason "count live buffers"))
                               (raw-call (list :id "call_live_1"
                                               :name "emacs_eval"
                                               :args args))
                               (info (list :content ""
                                           :tool-use (list raw-call))))
                          (run-at-time
                           0 nil
                           (lambda ()
                             (funcall callback "Checking buffers. " info)
                             (funcall callback
                                      (cons 'tool-call
                                            (list (list tool-spec args nil raw-call)))
                                      info)))))
                       (2
                        (let ((info (list :content "Done.")))
                          (run-at-time
                           0 nil
                           (lambda ()
                             (funcall callback "Done." info)
                             (funcall callback t info)))))
                       (_
                        (run-at-time
                         0 nil
                         (lambda ()
                           (funcall callback nil
                                    (list :error "unexpected extra request")))))))
                   nil)))
        (magent-ui-dispatch-prompt "count live buffers" 'send-prompt nil nil t)
        (magent-live-test--wait-until
         (lambda ()
           (let* ((session (magent-session-get))
                  (messages (magent-session-get-messages session))
                  (last-msg (car (last messages))))
             (when (and last-msg
                        (eq (magent-msg-role last-msg) 'assistant))
               (setq final-response (magent-msg-content last-msg)))))
         8 "Magent live loop tool turn did not finish")
        (should (equal final-response "Checking buffers. Done."))
        (should (= call-count 2))
        (let* ((messages (magent-session-get-messages (magent-session-get)))
               (tool-msg (cl-find-if
                          (lambda (msg) (eq (magent-msg-role msg) 'tool))
                          messages))
               (tool-content (magent-msg-content tool-msg)))
          (should tool-msg)
          (should (equal (plist-get tool-content :id) "call_live_1"))
          (should (equal (plist-get tool-content :name) "emacs_eval"))
          (should (string-match-p "\\`[0-9]+\\'"
                                  (plist-get tool-content :result))))
        (with-current-buffer (magent-ui-get-buffer (magent-session-current-scope))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "#\\+begin_tool emacs_eval" text))
            (should (string-match-p "Checking buffers\\." text))
            (should (string-match-p "Done\\." text))
            (should magent-ui--input-marker)))))))

(ert-deftest magent-live-test-timer-renders-symbol-tool-args ()
  "Render non-string tool args from a timer without leaking JSON errors."
  :tags '(:magent-live-smoke)
  (require 'magent-ui)
  (magent-live-test--with-isolated-runtime
    (let ((done nil))
      (run-at-time
       0 nil
       (lambda ()
         (magent-ui-insert-tool-call "emacs_eval"
                                     '(:tool emacs_eval :args [emacs_eval]))
         (setq done t)))
      (magent-live-test--wait-until
       (lambda () done)
       3 "Magent live timer did not render symbol tool args")
      (with-current-buffer (magent-ui-get-buffer 'global)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "#\\+begin_tool emacs_eval" text))
          (should (string-match-p "emacs_eval" text)))))))

(ert-deftest magent-live-test-real-simple-prompt ()
  "Send a real non-tool request through the configured gptel provider."
  :tags '(:magent-live)
  (require 'magent)
  (magent-live-test--require-real-gptel)
  (magent-live-test--with-isolated-runtime
    (let ((magent-enable-tools nil)
          (magent-include-reasoning 'ignore)
          (prompt "Reply with exactly MAGENT_LIVE_OK and no other text."))
      (magent-ui-dispatch-prompt prompt 'live-test nil nil t)
      (let ((response (magent-live-test--wait-for-assistant 120)))
        (should (string-match-p "MAGENT_LIVE_OK" response))
        (with-current-buffer (magent-ui-get-buffer (magent-session-current-scope))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "^\\* USER" text))
            (should (string-match-p "^\\* ASSISTANT" text))
            (should (string-match-p "MAGENT_LIVE_OK" text))))))))

(ert-deftest magent-live-test-real-emacs-eval-tool ()
  "Send a real tool-use request through gptel and execute emacs_eval."
  :tags '(:magent-live)
  (require 'magent)
  (magent-live-test--require-real-gptel)
  (magent-live-test--with-isolated-runtime
    (let ((magent-enable-tools '(emacs_eval))
          (magent-by-pass-permission t)
          (magent-include-reasoning 'ignore)
          (prompt (concat
                   "Use the emacs_eval tool exactly once to evaluate this "
                   "Emacs Lisp form: (+ 20 22). After the tool result, "
                   "reply exactly MAGENT_TOOL_OK=42 and no other text. "
                   "Do not answer from memory; call the tool.")))
      (magent-ui-dispatch-prompt prompt 'live-test nil nil t)
      (let ((response (magent-live-test--wait-for-assistant 180)))
        (should (string-match-p "MAGENT_TOOL_OK=42" response))
        (let* ((messages (magent-session-get-messages (magent-session-get)))
               (tool-msg (cl-find-if
                          (lambda (msg)
                            (and (eq (magent-msg-role msg) 'tool)
                                 (equal (plist-get (magent-msg-content msg) :name)
                                        "emacs_eval")))
                          messages))
               (tool-content (and tool-msg (magent-msg-content tool-msg))))
          (should tool-msg)
          (should (equal (plist-get tool-content :result) "42")))
        (with-current-buffer (magent-ui-get-buffer (magent-session-current-scope))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "#\\+begin_tool emacs_eval" text))
            (should (string-match-p "MAGENT_TOOL_OK=42" text))))))))

(defun magent-live-test--result-description (result)
  "Return a compact description for ERT RESULT."
  (cond
   ((ert-test-failed-p result)
    (error-message-string (ert-test-failed-condition result)))
   ((ert-test-skipped-p result)
    (format "%S" (ert-test-skipped-condition result)))
   (t
    (format "%S" result))))

(defun magent-live-test--unexpected-results (records)
  "Return unexpected test result descriptions from test result RECORDS."
  (let ((records (or records magent-live-test--latest-results))
        unexpected)
    (dolist (record records)
      (let ((test (car record))
            (result (cdr record)))
        (unless (or (and result
                         (ert-test-result-expected-p test result))
                    (and result
                         (ert-test-skipped-p result)))
          (push (format "%s: %s"
                        (ert-test-name test)
                        (magent-live-test--result-description result))
                unexpected))))
    (nreverse unexpected)))

(defun magent-live-test--listener (event &rest args)
  "Report live ERT progress for EVENT, STATS, and ARGS."
  (pcase event
    ('run-started
     (message "Magent live tests: running %d tests" (car args)))
    ('test-ended
     (let ((test (nth 0 args))
           (result (nth 1 args)))
       (message "Magent live test %s: %s"
                (ert-test-name test)
                (cond
                 ((ert-test-passed-p result) "passed")
                 ((ert-test-skipped-p result) "skipped")
                 (t "failed")))))
    ('run-ended
     (pcase-let ((`(,passed ,failed ,skipped) args))
       (message "Magent live tests finished: %d passed, %d failed, %d skipped"
                passed failed skipped)))))

(defun magent-live-test--run-selected-tests (selector)
  "Run live tests selected by SELECTOR and return result records."
  (let ((tests (ert-select-tests selector t))
        records)
    (magent-live-test--listener 'run-started (length tests))
    (dolist (test tests)
      (let ((result (ert-run-test test)))
        (push (cons test result) records)
        (magent-live-test--listener 'test-ended test result)))
    (setq records (nreverse records)
          magent-live-test--latest-results records)
    (let ((passed 0)
          (failed 0)
          (skipped 0))
      (dolist (record records)
        (let ((test (car record))
              (result (cdr record)))
          (cond
           ((ert-test-skipped-p result)
            (cl-incf skipped))
           ((ert-test-result-expected-p test result)
            (cl-incf passed))
           (t
            (cl-incf failed)))))
      (magent-live-test--listener 'run-ended passed failed skipped))
    records))

(defun magent-live-test-run (&optional selector)
  "Reload Magent source and run real live tests selected by SELECTOR.
Signals an error when any live test has an unexpected result so
`emacsclient' and `make' receive a non-zero exit code."
  (interactive)
  (magent-live-test-reload-source)
  (let* ((selector (or selector '(tag :magent-live)))
         (records (magent-live-test--run-selected-tests selector))
         (unexpected (magent-live-test--unexpected-results records)))
    (if unexpected
        (error "Magent live tests failed: %s"
               (string-join unexpected "; "))
      (message "Magent live tests passed")
      'magent-live-tests-passed)))

(defun magent-live-test-run-smoke (&optional selector)
  "Reload Magent source and run deterministic live smoke tests.
Smoke tests run inside the live Emacs instance but stub gptel transport, so
they do not consume tokens."
  (interactive)
  (magent-live-test-run (or selector '(tag :magent-live-smoke))))

(provide 'magent-live-test)
;;; magent-live-test.el ends here
