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

(defvar magent-live-test--async-status-file nil
  "Status file for the most recent async live test run.")

(defvar magent-live-test--trace-file nil
  "Trace file for gptel request-stage diagnostics.")

(defvar magent-live-test--trace-installed nil
  "Non-nil when live-test trace advice has been installed.")

;; Loading this file into a long-lived Emacs process should not leave renamed
;; smoke tests in ERT's symbol-property registry.
(put 'magent-live-test-fsm-runs-emacs-eval-and-continues 'ert--test nil)

(defun magent-live-test--add-load-path (directory)
  "Add DIRECTORY to `load-path' when it exists."
  (when (and directory (file-directory-p directory))
    (setq load-path
          (cons (file-truename directory)
                (delete (file-truename directory) load-path)))))

(defun magent-live-test--first-elpa-directory (pattern)
  "Return the first ELPA directory matching PATTERN, or nil."
  (let ((elpa-dir (expand-file-name "elpa" user-emacs-directory)))
    (when (file-directory-p elpa-dir)
      (car (directory-files elpa-dir t pattern t)))))

(defun magent-live-test--prepare-load-path ()
  "Add the repo and package dependency directories to `load-path'."
  (dolist (pattern '("\\`gptel-[0-9]"
                     "\\`magit-[0-9]"
                     "\\`magit-section-"
                     "\\`transient-"
                     "\\`acp-"
                     "\\`shell-maker-"
                     "\\`agent-shell-"
                     "\\`cond-let-"
                     "\\`evil-"
                     "\\`yaml-[0-9]"
                     "\\`llama-"
                     "\\`with-editor-"))
    (magent-live-test--add-load-path
     (magent-live-test--first-elpa-directory pattern)))
  (magent-live-test--add-load-path magent-live-test--root-directory))

(defun magent-live-test--load-source-file (file)
  "Load FILE from the repository root, bypassing stale .elc files."
  (load (expand-file-name file magent-live-test--root-directory) nil t t))

(defconst magent-live-test--source-files
  '("magent-config.el"
    "magent-json.el"
    "magent-approval.el"
    "magent-lifecycle-events.el"
    "magent-events.el"
    "magent-protocol.el"
    "magent-ledger.el"
    "magent-thread.el"
    "magent-agent-job.el"
    "magent-session.el"
    "magent-audit.el"
    "magent-file-loader.el"
    "magent-runtime.el"
    "magent-runtime-queue.el"
    "magent-permission.el"
    "magent-agent-info.el"
    "magent-agent-builtins.el"
    "magent-agent-registry.el"
    "magent-agent-types.el"
    "magent-agent-file.el"
    "magent-llm.el"
    "magent-llm-gptel.el"
    "magent-tools.el"
    "magent-tool-runtime.el"
    "magent-tool-registry.el"
    "magent-tool-orchestrator.el"
    "magent-agent-loop.el"
    "magent-legacy-queue.el"
    "magent-turn.el"
    "magent-transcript-context.el"
    "magent-context.el"
    "magent-markdown-to-org.el"
    "magent-md2org.el"
    "magent-agent.el"
    "magent-runtime-api.el"
    "magent-acp.el"
    "magent-agent-shell.el"
    "magent-skills.el"
    "magent-capability.el"
    "magent-ui.el"
    "magent.el")
  "Magent source files that live tests must load from this checkout.")

(defun magent-live-test--feature-source (feature)
  "Return the loaded source file for FEATURE, or nil."
  (cl-loop for entry in load-history
           when (member (cons 'provide feature) (cdr entry))
           return (file-truename (car entry))))

(defun magent-live-test--assert-repo-source-loaded ()
  "Fail unless every Magent feature was loaded from the repository root."
  (let ((root (file-name-as-directory
               (file-truename magent-live-test--root-directory)))
        mismatches)
    (dolist (file magent-live-test--source-files)
      (let* ((feature (intern (file-name-base file)))
             (source (magent-live-test--feature-source feature)))
        (unless (and source
                     (string-prefix-p root source))
          (push (format "%s loaded from %S" feature source) mismatches))))
    (when mismatches
      (ert-fail
       (format "Live test did not load Magent from repo %s: %s"
               root
               (string-join (nreverse mismatches) "; "))))))

(defun magent-live-test--dispatch-active-turn-now ()
  "Dispatch the active Magent turn immediately when tests block timer dispatch.
`magent-legacy-queue--start' normally defers dispatch with a zero-delay timer.  When a
live smoke test itself is running synchronously under `emacsclient --eval', the
test can end up waiting for the turn before that timer is serviced.  This
helper cancels the pending dispatcher timer for the active submission and runs
the same dispatcher directly."
  (when-let* ((submission (and (boundp 'magent-legacy-queue--active)
                               magent-legacy-queue--active))
              (dispatcher (magent-legacy-queue-submission-dispatcher submission)))
    (when (and (eq (magent-legacy-queue-submission-status submission) 'running)
               (null (magent-legacy-queue-current-request-handle)))
      (dolist (timer (copy-sequence timer-list))
        (when (memq submission (timer--args timer))
          (cancel-timer timer)))
      (funcall dispatcher submission))))

(defun magent-live-test-reload-source ()
  "Reload Magent source files into the live Emacs instance."
  (interactive)
  (magent-live-test--prepare-load-path)
  (dolist (file magent-live-test--source-files)
    (magent-live-test--load-source-file file))
  (magent-live-test--assert-repo-source-loaded))

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

(defun magent-live-test--redact-sensitive-text (text)
  "Return TEXT with likely provider secrets redacted."
  (when (stringp text)
    (let ((redacted text))
      (dolist (pattern '("\\(Authorization:[ \t]*\\)[^\n\r]+"
                         "\\(api-key[\"':= \t]+\\)[^\"' \t\n\r,)}]+"
                         "\\(x-api-key:[ \t]*\\)[^\n\r]+"
                         "\\(Bearer[ \t]+\\)[A-Za-z0-9._~+/-]+"))
        (setq redacted
              (replace-regexp-in-string
               pattern "\\1[REDACTED]" redacted t t)))
      redacted)))

(defun magent-live-test--buffer-tail (name &optional limit)
  "Return redacted tail text from buffer NAME, or nil when absent."
  (when-let ((buffer (get-buffer name)))
    (with-current-buffer buffer
      (magent-live-test--redact-sensitive-text
       (buffer-substring-no-properties
        (max (point-min) (- (point-max) (or limit 1200)))
        (point-max))))))

(defun magent-live-test--gptel-error-summary ()
  "Return a redacted, filtered `*gptel-log*' diagnostic summary."
  (when-let ((tail (magent-live-test--buffer-tail "*gptel-log*" 6000)))
    (let* ((lines (split-string tail "\n" t))
           (interesting
            (cl-remove-if-not
             (lambda (line)
               (string-match-p
                (rx (or "error" "Error" "ERROR" "failed" "Failed"
                        "wrong-type" "json" "status" "HTTP"))
                line))
             lines)))
      (mapconcat #'identity
                 (last interesting (min 20 (length interesting)))
                 "\n"))))

(defun magent-live-test--repo-source-summary ()
  "Return the loaded source files for core Magent features."
  (list :magent (magent-live-test--feature-source 'magent)
        :llm-gptel (magent-live-test--feature-source 'magent-llm-gptel)
        :agent-loop (magent-live-test--feature-source 'magent-agent-loop)
        :agent (magent-live-test--feature-source 'magent-agent)
        :ui (magent-live-test--feature-source 'magent-ui)))

(defun magent-live-test--debug-state ()
  "Return compact live Magent state for assertion failures."
  (format (concat "processing=%S current-scope=%S"
                  " turn-active=%S turn-queue=%S session-scopes=%S"
                  " current-messages=%S current-turns=%S"
                  " timers=%S"
                  " magent-buffer=%S log-tail=%S"
                  " messages-tail=%S backtrace-tail=%S gptel-summary=%S")
          (and (fboundp 'magent-ui-processing-p)
               (magent-ui-processing-p))
          (and (fboundp 'magent-session-current-scope)
               (magent-session-current-scope))
          (and (boundp 'magent-legacy-queue--active)
               magent-legacy-queue--active
               (magent-legacy-queue-submission-status magent-legacy-queue--active))
          (and (fboundp 'magent-legacy-queue-length)
               (magent-legacy-queue-length))
          (and (boundp 'magent-session--scoped-sessions)
               (hash-table-p magent-session--scoped-sessions)
               (let (scopes)
                 (maphash (lambda (scope session)
                            (push (list scope
                                        :messages
                                        (length
                                         (magent-session-messages session))
                                        :turns
                                        (length
                                         (magent-thread-turns
                                          (magent-session-thread-ledger
                                           session))))
                                  scopes))
                          magent-session--scoped-sessions)
                 (nreverse scopes)))
          (and (fboundp 'magent-session-get)
               (magent-session-messages (magent-session-get))
               (length (magent-session-messages (magent-session-get))))
          (and (fboundp 'magent-session-get)
               (magent-session-thread-ledger (magent-session-get))
               (length
                (magent-thread-turns
                 (magent-session-thread-ledger (magent-session-get)))))
          (mapcar
           (lambda (timer)
             (list :idle (timer--idle-delay timer)
                   :until (ignore-errors (timer-until timer))
                   :function (let ((fn (timer--function timer)))
                               (cond
                                ((symbolp fn) fn)
                                ((byte-code-function-p fn) 'byte-code-function)
                                ((functionp fn) 'function)
                                (t (type-of fn))))))
           timer-list)
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
               (point-max))))
          (magent-live-test--buffer-tail "*Messages*" 2000)
          (magent-live-test--buffer-tail "*Backtrace*" 2000)
          (magent-live-test--gptel-error-summary)))

(defun magent-live-test--async-status-path ()
  "Return a fresh status path for an async live test run."
  (expand-file-name
   (format "magent-live-test-%s-%d.el"
           (format-time-string "%Y%m%d-%H%M%S")
           (emacs-pid))
   temporary-file-directory))

(defun magent-live-test--write-async-status (file status &rest fields)
  "Write STATUS and FIELDS to FILE as a readable Lisp plist."
  (let ((payload (append (list :status status
                               :updated-at (format-time-string "%FT%T%z")
                               :pid (emacs-pid))
                         fields)))
    (with-temp-file file
      (let ((print-length nil)
            (print-level nil))
        (prin1 payload (current-buffer))
        (insert "\n")))))

(defun magent-live-test--append-trace (phase &rest fields)
  "Append PHASE and FIELDS to `magent-live-test--trace-file'."
  (when magent-live-test--trace-file
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents magent-live-test--trace-file)
          (goto-char (point-max))
          (let ((print-length 12)
                (print-level 4))
            (prin1 (append (list :phase phase
                                 :time (format-time-string "%FT%T%z")
                                 :pid (emacs-pid))
                           fields)
                   (current-buffer))
            (insert "\n"))
          (write-region (point-min) (point-max)
                        magent-live-test--trace-file nil 'silent))
      (error nil))))

(defun magent-live-test--trace-summary (info)
  "Return a compact, non-secret summary for gptel INFO."
  (let* ((data (and (listp info) (plist-get info :data)))
         (messages (and (listp data) (plist-get data :messages)))
         (tools (and (listp data) (plist-get data :tools))))
    (list :backend (and (plist-get info :backend)
                        (ignore-errors
                          (gptel-backend-name (plist-get info :backend))))
          :model (plist-get info :model)
          :stream (plist-get info :stream)
          :data-keys (and (listp data)
                          (cl-loop for (key _value) on data by #'cddr
                                   collect key))
          :messages (and (vectorp messages) (length messages))
          :tools (and (vectorp tools) (length tools))
          :json-bytes (and (listp data)
                           (ignore-errors
                             (length
                              (encode-coding-string
                               (gptel--json-encode data) 'utf-8))))
          :log-level (and (boundp 'gptel-log-level) gptel-log-level))))

(defun magent-live-test--trace-gptel-request (orig-fn &rest args)
  "Trace ORIG-FN gptel request call with ARGS."
  (let ((keys (cl-loop for (key _value) on (cdr args) by #'cddr
                       when (keywordp key)
                       collect key)))
    (magent-live-test--append-trace
     'gptel-request :event 'enter :keys keys)
    (prog1 (apply orig-fn args)
      (magent-live-test--append-trace
       'gptel-request :event 'leave :keys keys))))

(defun magent-live-test--trace-realize-query (orig-fn fsm)
  "Trace ORIG-FN `gptel--realize-query' for FSM."
  (magent-live-test--append-trace 'gptel-realize-query :event 'enter)
  (prog1 (funcall orig-fn fsm)
    (magent-live-test--append-trace
     'gptel-realize-query
     :event 'leave
     :info (magent-live-test--trace-summary (gptel-fsm-info fsm)))))

(defun magent-live-test--trace-handle-wait (orig-fn fsm)
  "Trace ORIG-FN `gptel--handle-wait' for FSM."
  (let ((use-curl (and (boundp 'gptel-use-curl) gptel-use-curl)))
    (magent-live-test--append-trace
     'gptel-handle-wait :event 'enter :use-curl use-curl)
    (prog1 (funcall orig-fn fsm)
      (magent-live-test--append-trace
       'gptel-handle-wait :event 'leave :use-curl use-curl))))

(defun magent-live-test--trace-curl-get-response (orig-fn fsm)
  "Trace ORIG-FN `gptel-curl-get-response' for FSM."
  (magent-live-test--append-trace
   'gptel-curl-get-response
   :event 'enter
   :info (magent-live-test--trace-summary (gptel-fsm-info fsm)))
  (prog1 (funcall orig-fn fsm)
    (magent-live-test--append-trace
     'gptel-curl-get-response
     :event 'leave
     :info (magent-live-test--trace-summary (gptel-fsm-info fsm)))))

(defun magent-live-test--trace-curl-get-args (orig-fn info &rest args)
  "Trace ORIG-FN `gptel-curl--get-args' for INFO and ARGS."
  (magent-live-test--append-trace
   'gptel-curl-get-args
   :event 'enter
   :info (magent-live-test--trace-summary info))
  (let ((result (apply orig-fn info args)))
    (magent-live-test--append-trace
     'gptel-curl-get-args
     :event 'leave
     :arg-count (length result)
     :url (car (last result)))
    result))

(defun magent-live-test--trace-make-process (orig-fn &rest args)
  "Trace ORIG-FN `make-process' call with ARGS."
  (let* ((name (plist-get args :name))
         (command (plist-get args :command)))
    (magent-live-test--append-trace
     'make-process
     :event 'enter
     :name name
     :argv0 (car-safe command)
     :argc (length command))
    (let ((process (apply orig-fn args)))
      (magent-live-test--append-trace
       'make-process
       :event 'leave
       :name name
       :process (and (processp process) (process-name process)))
      process)))

(defun magent-live-test-install-trace (&optional file)
  "Install gptel request tracing and write events to FILE."
  (interactive)
  (setq magent-live-test--trace-file
        (or file
            (expand-file-name "magent-live-trace.el"
                              temporary-file-directory)))
  (with-temp-file magent-live-test--trace-file)
  (unless magent-live-test--trace-installed
    (advice-add 'gptel-request
                :around #'magent-live-test--trace-gptel-request)
    (advice-add 'gptel--realize-query
                :around #'magent-live-test--trace-realize-query)
    (advice-add 'gptel--handle-wait
                :around #'magent-live-test--trace-handle-wait)
    (advice-add 'gptel-curl-get-response
                :around #'magent-live-test--trace-curl-get-response)
    (advice-add 'gptel-curl--get-args
                :around #'magent-live-test--trace-curl-get-args)
    (advice-add 'make-process
                :around #'magent-live-test--trace-make-process)
    (setq magent-live-test--trace-installed t))
  magent-live-test--trace-file)

(defun magent-live-test--short-text (value &optional width)
  "Return VALUE as one line, truncated to WIDTH columns."
  (let ((text (cond
               ((null value) nil)
               ((stringp value) value)
               (t (format "%S" value)))))
    (when text
      (truncate-string-to-width
       (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text))
       (or width 240) nil nil "..."))))

(defun magent-live-test--latest-assistant-message (messages)
  "Return the newest assistant message from MESSAGES."
  (cl-find-if (lambda (msg) (eq (magent-msg-role msg) 'assistant))
              (reverse messages)))

(defun magent-live-test--latest-tool-message (messages &optional name)
  "Return the newest tool message from MESSAGES, optionally matching NAME."
  (cl-find-if
   (lambda (msg)
     (and (eq (magent-msg-role msg) 'tool)
          (or (null name)
              (equal (plist-get (magent-msg-content msg) :name) name))))
   (reverse messages)))

(defun magent-live-test--turn-state ()
  "Return compact state for the currently running live turn."
  (let* ((session (and (fboundp 'magent-session-get)
                       (ignore-errors (magent-session-get))))
         (messages (and session (magent-session-get-messages session)))
         (last-msg (car (last messages)))
         (assistant-msg (magent-live-test--latest-assistant-message messages))
         (tool-msg (magent-live-test--latest-tool-message messages))
         (loop (or (and (boundp 'magent--current-request-handle)
                        magent--current-request-handle)
                   (and (fboundp 'magent-legacy-queue-current-request-handle)
                        (magent-legacy-queue-current-request-handle)))))
    (list :processing (and (fboundp 'magent-ui-processing-p)
                           (magent-ui-processing-p))
          :turn-active (and (fboundp 'magent-legacy-queue-processing-p)
                            (magent-legacy-queue-processing-p))
          :turn-pending (and (fboundp 'magent-legacy-queue-pending-p)
                             (magent-legacy-queue-pending-p))
          :loop-status (and (fboundp 'magent-agent-loop-p)
                            (magent-agent-loop-p loop)
                            (magent-agent-loop-status loop))
          :loop-error (and (fboundp 'magent-agent-loop-p)
                           (magent-agent-loop-p loop)
                           (magent-agent-loop-error loop))
          :message-count (length messages)
          :roles (vconcat (mapcar (lambda (msg)
                                     (symbol-name (magent-msg-role msg)))
                                   messages))
          :last-role (and last-msg (magent-msg-role last-msg))
          :last-content (magent-live-test--short-text
                         (and last-msg (magent-msg-content last-msg)))
          :assistant (magent-live-test--short-text
                      (and assistant-msg
                           (magent-msg-content assistant-msg)))
          :tool (and tool-msg
                     (let ((content (magent-msg-content tool-msg)))
                       (list :name (plist-get content :name)
                             :result (magent-live-test--short-text
                                      (plist-get content :result))))))))

(defun magent-live-test--reset-async-runtime ()
  "Reset Magent globals for an async live diagnostic run."
  (magent-live-test-reload-source)
  (setq magent-buffer-name "*magent-live-test*"
        magent-log-buffer-name "*magent-live-test-log*"
        magent-audit-buffer-name "*magent-live-test-audit*"
        magent-session-directory (make-temp-file "magent-live-sessions-" t)
        magent-audit-directory (make-temp-file "magent-live-audit-" t)
        magent-enable-audit-log nil
        magent-enable-capabilities nil
        magent-auto-context nil
        magent-auto-scroll nil
        magent-bypass-permission nil
        magent-ui-batch-insert-delay 0.01
        magent-ui--processing nil
        magent-ui--request-generation 0
        magent--current-request-handle nil
        magent--current-session nil
        magent-legacy-queue--active nil
        magent-legacy-queue--pending nil
        magent-legacy-queue--current-request-handle nil
        magent-session--current-scope 'global
        magent-session--scoped-sessions (make-hash-table :test #'equal)
        magent-runtime--active-project-scope nil
        magent-load-custom-agents nil)
  (magent-live-test--kill-magent-test-buffers)
  (magent-session-activate 'global))

(defun magent-live-test--async-real-kind (selector)
  "Return async real diagnostic kind for SELECTOR, or nil."
  (pcase selector
    ('magent-live-test-real-simple-prompt 'simple)
    ('magent-live-test-real-emacs-eval-tool 'emacs-eval-tool)
    (_ nil)))

(defun magent-live-test-run-real-async (kind &optional status-file timeout)
  "Start a non-blocking real live diagnostic of KIND.
KIND is `simple' or `emacs-eval-tool'.  Write status to STATUS-FILE and
return that path."
  (let* ((file (or status-file (magent-live-test--async-status-path)))
         (timeout (or timeout
                      (pcase kind
                        ('emacs-eval-tool 240)
                        (_ 150))))
         (deadline (+ (float-time) timeout))
         timer)
    (setq magent-live-test--async-status-file file)
    (condition-case err
        (progn
          (magent-live-test--reset-async-runtime)
          (magent-live-test--require-real-gptel)
          (pcase kind
            ('simple
             (setq magent-enable-tools nil
                   magent-include-reasoning 'ignore)
             (magent-ui-dispatch-prompt
              "Reply with exactly MAGENT_LIVE_OK and no other text."
              'live-test nil nil t))
            ('emacs-eval-tool
             (setq magent-enable-tools '(emacs_eval)
                   magent-bypass-permission t
                   magent-include-reasoning 'ignore)
             (magent-ui-dispatch-prompt
              (concat
               "Use the emacs_eval tool exactly once to evaluate this "
               "Emacs Lisp form: (+ 20 22). After the tool result, "
               "reply exactly MAGENT_TOOL_OK=42 and no other text. "
               "Do not answer from memory; call the tool.")
              'live-test nil nil t))
            (_ (error "Unknown async live diagnostic kind: %S" kind)))
          (magent-live-test--write-async-status
           file 'running
           :kind kind
           :phase 'dispatched
           :debug-on-error debug-on-error
           :repo-source (magent-live-test--repo-source-summary)
           :state (magent-live-test--turn-state))
          (cl-labels
              ((finish
                (status &rest fields)
                (when timer
                  (cancel-timer timer)
                  (setq timer nil))
                (apply #'magent-live-test--write-async-status
                       file status
                       :kind kind
                       :repo-source (magent-live-test--repo-source-summary)
                       fields))
               (poll
                ()
                (condition-case poll-err
                    (let* ((state (magent-live-test--turn-state))
                           (session (magent-session-get))
                           (messages (magent-session-get-messages session))
                           (assistant-msg
                            (magent-live-test--latest-assistant-message
                             messages))
                           (assistant
                            (and assistant-msg
                                 (magent-msg-content assistant-msg)))
                           (tool-msg
                            (magent-live-test--latest-tool-message
                             messages "emacs_eval"))
                           (tool-content
                            (and tool-msg (magent-msg-content tool-msg)))
                           (processing (plist-get state :processing)))
                      (cond
                       ((and (eq kind 'simple)
                             (stringp assistant)
                             (string-match-p "MAGENT_LIVE_OK" assistant))
                        (finish 'passed :state state))
                       ((and (eq kind 'emacs-eval-tool)
                             (stringp assistant)
                             (string-match-p "MAGENT_TOOL_OK=42" assistant)
                             tool-msg
                             (equal (plist-get tool-content :result) "42"))
                        (finish 'passed :state state))
                       ((and (not processing) assistant)
                        (finish 'failed
                                :error "Live turn finished without expected output"
                                :state state
                                :debug-state (magent-live-test--debug-state)))
                       ((>= (float-time) deadline)
                        (when (and (fboundp 'magent-interrupt) processing)
                          (ignore-errors (magent-interrupt)))
                        (finish 'timeout
                                :error "Timed out waiting for live turn"
                                :state state
                                :debug-state (magent-live-test--debug-state)))
                       (t
                        (magent-live-test--write-async-status
                         file 'running
                         :kind kind
                         :phase (plist-get state :loop-status)
                         :state state))))
                  (error
                   (finish 'failed
                           :error (error-message-string poll-err)
                           :debug-state (magent-live-test--debug-state))))))
            (setq timer (run-at-time 1 1 #'poll)))
          file)
      (error
       (magent-live-test--write-async-status
        file 'failed
        :kind kind
        :repo-source (magent-live-test--repo-source-summary)
        :error (error-message-string err)
        :debug-state (magent-live-test--debug-state))
       file))))

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
          (magent-bypass-permission nil)
          (magent-ui-batch-insert-delay 0.01)
          (magent-ui--processing nil)
          (magent-ui--request-generation 0)
          (magent--current-request-handle nil)
          (magent--current-session nil)
          (magent-legacy-queue--active nil)
          (magent-legacy-queue--pending nil)
          (magent-legacy-queue--current-request-handle nil)
          (magent-runtime-queue--active nil)
          (magent-runtime-queue--pending nil)
          (magent-runtime-api--sessions (make-hash-table :test #'equal))
          (magent-session--current-scope 'global)
          (magent-session--scoped-sessions (make-hash-table :test #'equal))
          (magent-runtime--active-project-scope nil)
          (magent-ui-backend 'legacy)
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

(ert-deftest magent-live-test-aa-agent-shell-router-dispatches-without-legacy ()
  "Dispatch through the default agent-shell backend without loading legacy UI."
  :tags '(:magent-live-smoke)
  (require 'magent)
  (magent-live-test--with-isolated-runtime
    (let ((magent-ui-backend 'agent-shell)
          (sent nil)
          (legacy-loaded nil))
      (cl-letf (((symbol-function 'magent--ensure-initialized) #'ignore)
                ((symbol-function 'magent-agent-shell-send-prompt)
                 (lambda (prompt &rest _args)
                   (setq sent prompt)))
                ((symbol-function 'magent-ui--require-legacy)
                 (lambda ()
                   (setq legacy-loaded t)
                   (error "Legacy UI should not load"))))
        (magent-ui-dispatch-prompt "hello from agent-shell" 'live-test nil nil t)
        (should (equal sent "hello from agent-shell"))
        (should-not legacy-loaded)))))

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
                      (magent-session-add-message
                       (magent-session-get) 'assistant "live response")
                      (setq response "live response")
                      (funcall callback "live response")))
                   'magent-live-test-loop)))
        (let ((buffer (magent-ui-get-buffer 'global))
              (compose (magent-ui-compose-buffer 'global)))
          (with-current-buffer compose
            (erase-buffer)
            (insert "hello from live buffer")
            (magent-input-submit))
          (magent-live-test--wait-until
           (lambda ()
             (and response (not (magent-ui-processing-p))))
           5 "Magent live buffer input did not finish")
          (with-current-buffer buffer
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "  - Prompt" text))
              (should (string-match-p "hello from live buffer" text))
              (should (string-match-p "  - Response" text))
              (should (string-match-p "live response" text))
              (should-not (string-match-p "^\\* USER" text))
              (should-not (string-match-p "^\\* ASSISTANT" text)))))))))

(ert-deftest magent-live-test-loop-runs-emacs-eval-and-continues ()
  "Run a live Magent turn through loop tool execution and continuation."
  :tags '(:magent-live-smoke)
  (require 'magent)
  (require 'gptel)
  (require 'gptel-openai)
  (magent-live-test--with-isolated-runtime
    (let ((call-count 0)
          (final-response nil)
          (gptel-backend (gptel-make-openai "magent-live-stub"
                                           :key "test-key"))
          (gptel-model 'gpt-4o-mini)
          (magent-bypass-permission t)
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
        (magent-live-test--dispatch-active-turn-now)
        (magent-live-test--wait-until
         (lambda ()
           (when (not (magent-ui-processing-p))
             (let* ((session (magent-session-get))
                    (messages (magent-session-get-messages session))
                    (assistant-msg
                     (cl-find-if
                      (lambda (msg)
                        (eq (magent-msg-role msg) 'assistant))
                      (reverse messages))))
               (when assistant-msg
                 (setq final-response
                       (magent-msg-content assistant-msg))))))
         20
         (format "Magent live loop tool turn did not finish: %s"
                 (magent-live-test--debug-state)))
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
            (should (string-match-p "Tool emacs_eval" text))
            (should (string-match-p "Checking buffers\\." text))
            (should (string-match-p "Done\\." text))))))))

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
          (should (string-match-p "Tool emacs_eval running" text))
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
            (should (string-match-p "  - Prompt" text))
            (should (string-match-p "  - Response" text))
            (should-not (string-match-p "^\\* USER" text))
            (should-not (string-match-p "^\\* ASSISTANT" text))
            (should (string-match-p "MAGENT_LIVE_OK" text))))))))

(ert-deftest magent-live-test-real-emacs-eval-tool ()
  "Send a real tool-use request through gptel and execute emacs_eval."
  :tags '(:magent-live)
  (require 'magent)
  (magent-live-test--require-real-gptel)
  (magent-live-test--with-isolated-runtime
    (let ((magent-enable-tools '(emacs_eval))
          (magent-bypass-permission t)
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
            (should (string-match-p "Tool emacs_eval" text))
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

(defun magent-live-test-run-async (&optional selector status-file)
  "Start live tests selected by SELECTOR asynchronously.
Write progress and redacted diagnostics to STATUS-FILE and return that path.
This is intended for `emacsclient --eval' so the client can return immediately
while a live gptel run continues inside the target Emacs."
  (interactive)
  (let* ((selector (or selector '(tag :magent-live)))
         (file (or status-file (magent-live-test--async-status-path))))
    (if-let ((kind (magent-live-test--async-real-kind selector)))
        (magent-live-test-run-real-async kind file)
      (setq magent-live-test--async-status-file file)
      (magent-live-test--write-async-status
       file 'running
       :selector selector
       :debug-on-error debug-on-error
       :started-at (format-time-string "%FT%T%z"))
      (run-at-time
       0 nil
       (lambda ()
         (condition-case err
             (let ((result (magent-live-test-run selector)))
               (magent-live-test--write-async-status
                file 'passed
                :selector selector
                :result result))
           (error
            (magent-live-test--write-async-status
             file 'failed
             :selector selector
             :error (error-message-string err)
             :debug-state (magent-live-test--debug-state))))))
      file)))

(provide 'magent-live-test)
;;; magent-live-test.el ends here
