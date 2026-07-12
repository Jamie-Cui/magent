;;; magent-doctor.el --- Safe probe-based Magent diagnostics  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Magent doctor collects bounded diagnostics through trusted, read-only probe
;; functions and sends one sanitized, tool-free request to the current provider.
;; Custom probes are trusted Emacs Lisp; this module does not sandbox them.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magent-command)
(require 'magent-config)
(require 'magent-json)
(require 'magent-llm)
(require 'magent-llm-gptel)
(require 'magent-prompt)
(require 'magent-redaction)
(require 'magent-session)

(declare-function flymake-diagnostic-beg "flymake")
(declare-function flymake-diagnostic-end "flymake")
(declare-function flymake-diagnostic-text "flymake")
(declare-function flymake-diagnostic-type "flymake")
(declare-function flymake-diagnostics "flymake")
(declare-function flycheck-error-filename "flycheck")
(declare-function flycheck-error-id "flycheck")
(declare-function flycheck-error-level "flycheck")
(declare-function flycheck-error-line "flycheck")
(declare-function flycheck-error-message "flycheck")
(declare-function gptel-abort "gptel-request")
(declare-function gptel-backend-name "gptel")
(declare-function magent-agent-info-name "magent-agent-info")
(declare-function magent-approval-pending-count "magent-approval")
(declare-function magent-runtime-pending-count "magent-runtime-api")
(declare-function magent-runtime-queue-active-submission
                  "magent-runtime-queue")
(declare-function magent-runtime-submission-id "magent-runtime-queue")
(declare-function magent-runtime-submission-session-id "magent-runtime-queue")
(declare-function magent-runtime-submission-status "magent-runtime-queue")

(defvar flycheck-current-errors)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-temperature)
(defvar magent-mode)
(defvar magent-ui-backend)

(define-error 'magent-doctor-probe-timeout "Doctor probe timed out")
(define-error 'magent-doctor-security-error "Doctor data failed validation")

(cl-defstruct (magent-doctor-probe
               (:constructor magent-doctor-probe-create)
               (:copier nil))
  id
  description
  predicate
  collector
  timeout
  data-categories
  required
  manual-only)

(cl-defstruct (magent-doctor-state
               (:constructor magent-doctor-state-create)
               (:copier nil))
  context
  project-root
  deadline
  current-process
  request-handle
  request-timer
  cancelled-p)

(defvar magent-doctor--registry (make-hash-table :test #'equal)
  "Registered Magent doctor probes keyed by probe id.")

(defconst magent-doctor--output-headings
  '("* Diagnosis" "** Summary" "** Findings"
    "** Recommended Actions" "** Limitations")
  "Required headings in a structured doctor response.")

(defun magent-doctor--normalize-id (id)
  "Return ID as a stable probe registry key."
  (if (symbolp id) (symbol-name id) (format "%s" id)))

(cl-defun magent-doctor-register-probe
    (id &key description predicate collector timeout data-categories
        required manual-only)
  "Register a trusted read-only doctor probe ID.
PREDICATE and COLLECTOR receive the command context; COLLECTOR also receives
the current `magent-doctor-state'.  Probe output must contain only JSON-safe
values.  Custom probes execute as trusted Emacs Lisp and are not sandboxed."
  (let ((key (magent-doctor--normalize-id id)))
    (unless (string-match-p "\\`[a-z][a-z0-9_-]\\{0,63\\}\\'" key)
      (error "Invalid doctor probe id: %s" key))
    (unless (functionp collector)
      (error "Doctor probe %s has no collector" key))
    (when (and predicate (not (functionp predicate)))
      (error "Doctor probe %s has an invalid predicate" key))
    (when (and timeout
               (not (and (numberp timeout) (> timeout 0))))
      (error "Doctor probe %s has an invalid timeout" key))
    (let ((probe (magent-doctor-probe-create
                  :id key
                  :description (or description key)
                  :predicate predicate
                  :collector collector
                  :timeout timeout
                  :data-categories data-categories
                  :required required
                  :manual-only manual-only)))
      (puthash key probe magent-doctor--registry)
      probe)))

(defun magent-doctor-list-probes ()
  "Return registered doctor probes sorted by id."
  (let (probes)
    (maphash (lambda (_id probe) (push probe probes)) magent-doctor--registry)
    (sort probes
          (lambda (a b)
            (string< (magent-doctor-probe-id a)
                     (magent-doctor-probe-id b))))))

(defun magent-doctor--project-root (context)
  "Return the project root captured by command CONTEXT, or nil."
  (let ((scope (magent-command-context-origin-scope context)))
    (cond
     ((stringp scope) (directory-file-name (expand-file-name scope)))
     ((magent-command-context-origin-directory context)
      (when-let* ((root (magent-project-root
                         (magent-command-context-origin-directory context) t)))
        (directory-file-name (expand-file-name root)))))))

(defun magent-doctor--origin-buffer (context)
  "Return CONTEXT's live origin buffer, or nil."
  (let ((buffer (magent-command-context-origin-buffer context)))
    (and (buffer-live-p buffer) buffer)))

(defun magent-doctor--json-bool (value)
  "Return VALUE as a JSON boolean sentinel."
  (if value t :json-false))

(defun magent-doctor--safe-provider-name ()
  "Return the current provider name without printing its backend object."
  (or (and (boundp 'gptel-backend)
           gptel-backend
           (fboundp 'gptel-backend-name)
           (ignore-errors (gptel-backend-name gptel-backend)))
      "gptel"))

(defun magent-doctor--feature-source (feature)
  "Return the library path for FEATURE, or nil."
  (ignore-errors (locate-library (symbol-name feature))))

(defun magent-doctor--core-collector (context _state)
  "Collect bounded Magent runtime facts for CONTEXT."
  (let* ((parent (magent-command-context-parent-session context))
         (thread (and parent (magent-session-thread-ledger parent)))
         (agent (and parent (magent-session-agent parent)))
         (active (and (fboundp 'magent-runtime-queue-active-submission)
                      (magent-runtime-queue-active-submission)))
         (origin-buffer (magent-doctor--origin-buffer context)))
    `((emacs-version . ,emacs-version)
      (system-type . ,system-type)
      (magent-mode
       . ,(magent-doctor--json-bool
           (and origin-buffer
                (buffer-local-value 'magent-mode origin-buffer))))
      (ui-backend . ,(and (boundp 'magent-ui-backend)
                          magent-ui-backend))
      (origin-scope . ,(magent-command-context-origin-scope context))
      (parent-session-id
       . ,(and parent (magent-session-get-id parent)))
      (parent-thread-status . ,(and thread (magent-thread-status thread)))
      (active-agent
       . ,(and agent
               (fboundp 'magent-agent-info-name)
               (magent-agent-info-name agent)))
      (runtime-queue
       . ((pending-count
           . ,(if (fboundp 'magent-runtime-pending-count)
                  (magent-runtime-pending-count)
                0))
          (active
           . ,(and active
                   `((submission-id
                      . ,(magent-runtime-submission-id active))
                     (session-id
                      . ,(magent-runtime-submission-session-id active))
                     (status
                      . ,(magent-runtime-submission-status active)))))))
      (active-internal-commands
       . ,(mapcar
           (lambda (command-context)
             (magent-command-spec-name
              (magent-command-context-spec command-context)))
           (magent-command-active-contexts)))
      (pending-approvals
       . ,(if (fboundp 'magent-approval-pending-count)
              (magent-approval-pending-count)
            0))
      (provider . ,(magent-doctor--safe-provider-name))
      (model . ,(and (boundp 'gptel-model) gptel-model))
      (loaded-sources
       . ,(delq nil
                (mapcar
                 (lambda (feature)
                   (when-let* ((source (magent-doctor--feature-source feature)))
                     (cons feature source)))
                 '(magent magent-command magent-memory magent-doctor
                   magent-agent-loop magent-acp)))))))

(defun magent-doctor--buffer-collector (context _state)
  "Collect content-free facts about CONTEXT's origin buffer."
  (if-let* ((buffer (magent-doctor--origin-buffer context)))
      (with-current-buffer buffer
        `((name . ,(buffer-name))
          (major-mode . ,major-mode)
          (file . ,buffer-file-name)
          (default-directory . ,default-directory)
          (line . ,(line-number-at-pos))
          (point . ,(point))
          (modified . ,(magent-doctor--json-bool (buffer-modified-p)))
          (read-only . ,(magent-doctor--json-bool buffer-read-only))
          (remote . ,(magent-doctor--json-bool
                      (or (file-remote-p default-directory)
                          (and buffer-file-name
                               (file-remote-p buffer-file-name)))))))
    '((status . "origin buffer is no longer live"))))

(defun magent-doctor--project-predicate (context)
  "Return non-nil when CONTEXT belongs to a project."
  (and (magent-doctor--project-root context) t))

(defun magent-doctor--project-indicators (root)
  "Return known project indicator files present under ROOT."
  (let ((names '(".git" "compile_commands.json" "CMakeLists.txt"
                 "meson.build" "Makefile" "Cargo.toml" "go.mod"
                 "package.json" "pyproject.toml")))
    (seq-filter (lambda (name) (file-exists-p (expand-file-name name root)))
                names)))

(defun magent-doctor-run-process (state program args &optional timeout directory)
  "Run PROGRAM with ARGS for a trusted probe in STATE.
No shell is used.  TIMEOUT defaults to `magent-doctor-process-timeout'.
DIRECTORY defaults to STATE's project root.  Return exit and output data."
  (let* ((executable (or (and (file-name-absolute-p program) program)
                         (executable-find program)
                         (error "Doctor executable not found: %s" program)))
         (default-directory (or directory
                                (magent-doctor-state-project-root state)
                                default-directory))
         (buffer (generate-new-buffer " *magent-doctor-process*"))
         (limit (min (or timeout magent-doctor-process-timeout)
                     (max 0.0
                          (- (magent-doctor-state-deadline state)
                             (float-time)))))
         (deadline (+ (float-time) limit))
         process)
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name "magent-doctor-probe"
                 :buffer buffer
                 :stderr buffer
                 :command (cons executable args)
                 :connection-type 'pipe
                 :noquery t))
          (setf (magent-doctor-state-current-process state) process)
          (while (process-live-p process)
            (when (magent-doctor-state-cancelled-p state)
              (signal 'quit nil))
            (when (>= (float-time) deadline)
              (signal 'magent-doctor-probe-timeout (list program)))
            (accept-process-output process 0.05))
          `((program . ,program)
            (exit-code . ,(process-exit-status process))
            (output . ,(with-current-buffer buffer (buffer-string)))))
      (when (process-live-p process)
        (delete-process process))
      (when (eq (magent-doctor-state-current-process state) process)
        (setf (magent-doctor-state-current-process state) nil))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun magent-doctor--project-collector (context state)
  "Collect generic read-only project diagnostics for CONTEXT using STATE."
  (let* ((root (magent-doctor--project-root context))
         (remote (and root (file-remote-p root)))
         (git-result
          (when (and root
                     (not remote)
                     (file-exists-p (expand-file-name ".git" root))
                     (executable-find "git"))
            (condition-case err
                (magent-doctor-run-process
                 state "git"
                 '("status" "--short" "--branch" "--untracked-files=no")
                 magent-doctor-process-timeout root)
              (magent-doctor-probe-timeout
               '((status . "git status timed out")))
              (error
               `((status . ,(error-message-string err))))))))
    `((root . ,root)
      (remote . ,(magent-doctor--json-bool remote))
      (indicators . ,(and root (magent-doctor--project-indicators root)))
      (git-status . ,git-result))))

(defun magent-doctor--diagnostics-predicate (context)
  "Return non-nil when CONTEXT has a live origin buffer."
  (and (magent-doctor--origin-buffer context) t))

(defun magent-doctor--flymake-diagnostics ()
  "Return bounded Flymake diagnostics for the current buffer."
  (when (and (bound-and-true-p flymake-mode)
             (fboundp 'flymake-diagnostics))
    (mapcar
     (lambda (diagnostic)
       (let ((beg (flymake-diagnostic-beg diagnostic)))
         `((line . ,(line-number-at-pos beg))
           (end-line . ,(line-number-at-pos
                         (flymake-diagnostic-end diagnostic)))
           (type . ,(flymake-diagnostic-type diagnostic))
           (message . ,(flymake-diagnostic-text diagnostic)))))
     (seq-take (flymake-diagnostics) 50))))

(defun magent-doctor--flycheck-diagnostics ()
  "Return bounded Flycheck diagnostics for the current buffer."
  (when (and (bound-and-true-p flycheck-mode)
             (boundp 'flycheck-current-errors))
    (mapcar
     (lambda (diagnostic)
       `((file . ,(and (fboundp 'flycheck-error-filename)
                       (flycheck-error-filename diagnostic)))
         (line . ,(and (fboundp 'flycheck-error-line)
                       (flycheck-error-line diagnostic)))
         (level . ,(and (fboundp 'flycheck-error-level)
                        (flycheck-error-level diagnostic)))
         (id . ,(and (fboundp 'flycheck-error-id)
                     (flycheck-error-id diagnostic)))
         (message . ,(and (fboundp 'flycheck-error-message)
                          (flycheck-error-message diagnostic)))))
     (seq-take flycheck-current-errors 50))))

(defun magent-doctor--diagnostics-collector (context _state)
  "Collect existing diagnostics for CONTEXT without running a build."
  (if-let* ((buffer (magent-doctor--origin-buffer context)))
      (with-current-buffer buffer
        `((flymake . ,(magent-doctor--flymake-diagnostics))
          (flycheck . ,(magent-doctor--flycheck-diagnostics))
          (eglot
           . ((managed-mode . ,(magent-doctor--json-bool
                                (bound-and-true-p eglot-managed-mode)))
              (connected
               . ,(magent-doctor--json-bool
                   (and (fboundp 'eglot-current-server)
                        (ignore-errors (eglot-current-server)))))))))
    '((status . "origin buffer is no longer live"))))

(defun magent-doctor--buffer-tail (buffer &optional lines)
  "Return the last LINES lines from BUFFER without text properties."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-max))
        (forward-line (- (or lines magent-doctor-log-max-lines)))
        (buffer-substring-no-properties (point) (point-max))))))

(defun magent-doctor--filtered-log-tail (name filter)
  "Return bounded lines from buffer NAME matching FILTER."
  (when-let* ((buffer (get-buffer name)))
    (let* ((case-fold-search t)
           (matches
            (seq-filter (lambda (line) (string-match-p filter line))
                        (split-string
                         (magent-doctor--buffer-tail
                          buffer (* 4 magent-doctor-log-max-lines))
                         "\n" t))))
      (mapconcat
       #'identity
       (last matches magent-doctor-log-max-lines)
       "\n"))))

(defun magent-doctor--logs-collector (_context _state)
  "Collect allowlisted and filtered Magent-related log tails."
  (let ((filter "magent\\|gptel\\|agent-shell\\|acp"))
    `((magent-log
       . ,(when-let* ((buffer (get-buffer "*magent-log*")))
            (magent-doctor--buffer-tail buffer)))
      (warnings . ,(magent-doctor--filtered-log-tail "*Warnings*" filter))
      (messages . ,(magent-doctor--filtered-log-tail "*Messages*" filter)))))

(defun magent-doctor--compilation-predicate (context)
  "Return non-nil when CONTEXT has project compilation buffers."
  (when-let* ((root (magent-doctor--project-root context)))
    (cl-some
     (lambda (buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'compilation-mode)
              (ignore-errors (file-in-directory-p default-directory root)))))
     (buffer-list))))

(defun magent-doctor--compilation-collector (context _state)
  "Collect bounded existing compilation output for CONTEXT."
  (let ((root (magent-doctor--project-root context))
        results)
    (dolist (buffer (buffer-list))
      (when (and (< (length results) 3)
                 (with-current-buffer buffer
                   (and (derived-mode-p 'compilation-mode)
                        (ignore-errors
                          (file-in-directory-p default-directory root)))))
        (push `((buffer . ,(buffer-name buffer))
                (tail . ,(magent-doctor--buffer-tail buffer)))
              results)))
    (nreverse results)))

(defun magent-doctor--source-context-collector (context _state)
  "Collect a bounded source excerpt from CONTEXT's origin buffer."
  (if-let* ((buffer (magent-doctor--origin-buffer context)))
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (let* ((center (line-number-at-pos))
                 (start-line (max 1 (- center 40)))
                 (end-line (+ center 40))
                 start end text)
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- start-line))
              (setq start (point))
              (goto-char (point-min))
              (forward-line end-line)
              (setq end (point)))
            (setq text (buffer-substring-no-properties start end))
            (when (> (length text) magent-doctor-source-context-max-chars)
              (setq text
                    (concat
                     (substring text 0 magent-doctor-source-context-max-chars)
                     "\n[truncated]")))
            `((file . ,buffer-file-name)
              (start-line . ,start-line)
              (end-line . ,(line-number-at-pos end))
              (content . ,text)))))
    '((status . "origin buffer is no longer live"))))

(defun magent-doctor--probe-applicable-p (probe context)
  "Return non-nil when PROBE applies to CONTEXT."
  (or (null (magent-doctor-probe-predicate probe))
      (condition-case nil
          (funcall (magent-doctor-probe-predicate probe) context)
        (error nil))))

(defun magent-doctor--automatic-probes (context)
  "Return automatically selected probes for CONTEXT."
  (seq-filter
   (lambda (probe)
     (and (not (magent-doctor-probe-manual-only probe))
          (magent-doctor--probe-applicable-p probe context)))
   (magent-doctor-list-probes)))

(defun magent-doctor--select-probes (context)
  "Select probes for command CONTEXT, prompting for a manual run."
  (let* ((automatic (magent-doctor--automatic-probes context))
         (manual (plist-get (magent-command-context-arguments context)
                            :select-probes)))
    (if (not manual)
        automatic
      (let* ((available
              (seq-filter
               (lambda (probe)
                 (magent-doctor--probe-applicable-p probe context))
               (magent-doctor-list-probes)))
             (choices
              (mapcar
               (lambda (probe)
                 (cons (format "%s  %s"
                               (magent-doctor-probe-id probe)
                               (magent-doctor-probe-description probe))
                       probe))
               available))
             (selected-labels
              (completing-read-multiple
               "Doctor probes (empty uses automatic): "
               (mapcar #'car choices) nil t))
             (selected
              (if selected-labels
                  (mapcar (lambda (label) (cdr (assoc label choices)))
                          selected-labels)
                automatic))
             (required
              (seq-filter #'magent-doctor-probe-required available)))
        (delete-dups (append required selected))))))

(defun magent-doctor--source-preview (context)
  "Return source range disclosure text for CONTEXT, when applicable."
  (when-let* ((buffer (magent-doctor--origin-buffer context)))
    (with-current-buffer buffer
      (when buffer-file-name
        (format "%s around line %d (max %d characters)"
                buffer-file-name
                (line-number-at-pos)
                magent-doctor-source-context-max-chars)))))

(defun magent-doctor--preflight-text (context probes)
  "Return a local-only preflight disclosure for CONTEXT and PROBES."
  (let* ((root (magent-doctor--project-root context))
         (source-selected
          (cl-find "source-context" probes
                   :key #'magent-doctor-probe-id :test #'equal))
         (categories
          (delete-dups
           (apply #'append
                  (mapcar #'magent-doctor-probe-data-categories probes)))))
    (string-join
     (append
      (list "Magent Doctor Preflight"
            ""
            (format "Provider: %s" (magent-doctor--safe-provider-name))
            (format "Model: %s"
                    (if (boundp 'gptel-model) gptel-model "default"))
            (format "Project root: %s" (or root "none"))
            ""
            "Probes:")
      (mapcar
       (lambda (probe)
         (format "- %s: %s"
                 (magent-doctor-probe-id probe)
                 (magent-doctor-probe-description probe)))
       probes)
      (list ""
            (format "Data categories: %s"
                    (if categories
                        (mapconcat (lambda (item) (format "%s" item))
                                   categories ", ")
                      "none")))
      (when source-selected
        (list (format "Source context: %s"
                      (or (magent-doctor--source-preview context)
                          "unavailable"))))
      (list ""
            "All persisted and provider-bound diagnostics are bounded, path-normalized, and redacted."
            "No provider tools are enabled."))
     "\n")))

(defun magent-doctor--confirm (context probes)
  "Confirm the doctor collection plan for CONTEXT and PROBES."
  (if magent-bypass-permission
      t
    (magent--with-display-buffer "*Magent Doctor Preflight*"
      (insert (magent-doctor--preflight-text context probes)))
    (yes-or-no-p "Run Magent Doctor with these probes? ")))

(defun magent-doctor--truncate (string limit)
  "Return STRING capped to LIMIT characters."
  (if (<= (length string) limit)
      string
    (concat (substring string 0 (max 0 (- limit 13))) "\n[truncated]")))

(defun magent-doctor--sanitize-value (value state)
  "Return VALUE encoded, path-normalized, redacted, and bounded for STATE."
  (condition-case nil
      (let* ((redacted (magent-redaction-value value t))
             (encoded (magent-json-encode redacted))
             (normalized
              (magent-redaction-normalize-paths
               encoded (magent-doctor-state-project-root state)))
             ;; Structured values were already redacted strictly.  A second
             ;; strict pass over encoded JSON would mistake safe key names for
             ;; unparsed secret-bearing lines and erase the whole object.
             (final (magent-redaction-string normalized nil)))
        (magent-doctor--truncate final magent-doctor-max-probe-chars))
    (magent-redaction-unsafe-value
     (signal 'magent-doctor-security-error '("Probe output rejected")))))

(defun magent-doctor--safe-error (error-data state)
  "Return ERROR-DATA as a safe bounded message for STATE."
  (condition-case nil
      (magent-doctor--truncate
       (magent-redaction-string
        (magent-redaction-normalize-paths
         (error-message-string error-data)
         (magent-doctor-state-project-root state))
        t)
       1000)
    (magent-redaction-unsafe-value
     (signal 'magent-doctor-security-error '("Probe error rejected")))))

(defun magent-doctor--run-probe (probe context state)
  "Run one PROBE for CONTEXT and return a sanitized result using STATE."
  (when (>= (float-time) (magent-doctor-state-deadline state))
    (signal 'magent-doctor-probe-timeout '("total collection timeout")))
  (let* ((remaining (- (magent-doctor-state-deadline state) (float-time)))
         (timeout (min (or (magent-doctor-probe-timeout probe)
                           magent-doctor-probe-timeout)
                       remaining))
         (id (magent-doctor-probe-id probe)))
    (magent-command-notify context (format "Running doctor probe %s..." id))
    (condition-case err
        (let* ((raw
                (with-timeout
                    (timeout
                     (signal 'magent-doctor-probe-timeout (list id)))
                  (funcall (magent-doctor-probe-collector probe)
                           context state)))
               (safe (magent-doctor--sanitize-value raw state))
               (result `((id . ,id)
                         (status . "completed")
                         (data . ,safe))))
          (magent-command-record-tool
           context "doctor_probe" (list :probe id) safe
           (list :doctor-detail t :probe-id id :probe-status 'completed))
          result)
      (magent-doctor-security-error (signal (car err) (cdr err)))
      (magent-redaction-unsafe-value
       (signal 'magent-doctor-security-error '("Probe output rejected")))
      (magent-doctor-probe-timeout
       (let ((message "Probe timed out"))
         (magent-command-record-tool
          context "doctor_probe" (list :probe id) message
          (list :doctor-detail t :probe-id id :probe-status 'failed))
         `((id . ,id) (status . "failed") (error . ,message))))
      (quit (signal 'quit nil))
      (error
       (let ((message (magent-doctor--safe-error err state)))
         (magent-command-record-tool
          context "doctor_probe" (list :probe id) message
          (list :doctor-detail t :probe-id id :probe-status 'failed))
         `((id . ,id) (status . "failed") (error . ,message)))))))

(defun magent-doctor--collect (probes context state)
  "Run PROBES serially for CONTEXT using STATE."
  (let (results)
    (dolist (probe probes (nreverse results))
      (when (magent-doctor-state-cancelled-p state)
        (signal 'quit nil))
      (push (magent-doctor--run-probe probe context state) results))))

(defun magent-doctor--bounded-bundle (results)
  "Return RESULTS capped to `magent-doctor-max-diagnostic-chars'."
  (let ((remaining magent-doctor-max-diagnostic-chars)
        included omitted)
    (dolist (result results)
      (let* ((encoded (magent-json-encode result))
             (size (length encoded)))
        (if (<= size remaining)
            (progn
              (push result included)
              (setq remaining (- remaining size)))
          (push (cdr (assq 'id result)) omitted))))
    `((probes . ,(vconcat (nreverse included)))
      (omitted-probes . ,(vconcat (nreverse omitted)))
      (truncated . ,(magent-doctor--json-bool omitted)))))

(defun magent-doctor--structured-output-p (text)
  "Return non-nil when TEXT contains the required headings in order."
  (let ((position 0)
        (valid t))
    (dolist (heading magent-doctor--output-headings valid)
      (if (string-match (concat "^" (regexp-quote heading) "[[:space:]]*$")
                        text position)
          (setq position (match-end 0))
        (setq valid nil)))))

(defun magent-doctor--normalize-output (text context state)
  "Return safe structured doctor TEXT for CONTEXT using STATE."
  (let* ((safe
          (condition-case nil
              (magent-redaction-string
               (magent-redaction-normalize-paths
                text (magent-doctor-state-project-root state))
               t)
            (magent-redaction-unsafe-value
             (signal 'magent-doctor-security-error
                     '("Doctor response rejected")))))
         (bounded (magent-doctor--truncate
                   safe magent-doctor-max-diagnostic-chars)))
    (if (magent-doctor--structured-output-p bounded)
        bounded
      (magent-session-set-metadata-value
       (magent-command-context-session context)
       'warning "unstructured-model-output")
      (concat
       "* Diagnosis\n"
       "** Unstructured Model Output\n"
       bounded
       "\n\n** Limitations\n"
       "- The provider response did not follow the requested output schema."))))

(defun magent-doctor--cancel-request-timer (state)
  "Cancel STATE's request timeout timer."
  (when-let* ((timer (magent-doctor-state-request-timer state)))
    (cancel-timer timer)
    (setf (magent-doctor-state-request-timer state) nil)))

(defun magent-doctor--abort-request (state)
  "Abort STATE's active provider request and clean its request buffer."
  (when-let* ((handle (magent-doctor-state-request-handle state)))
    (when (and (bufferp handle) (buffer-live-p handle))
      (when (fboundp 'gptel-abort)
        (with-demoted-errors "Magent doctor abort error: %S"
          (gptel-abort handle)))
      (when (buffer-live-p handle)
        (kill-buffer handle)))
    (setf (magent-doctor-state-request-handle state) nil)))

(defun magent-doctor--cancel (state)
  "Cancel all work associated with doctor STATE."
  (unless (magent-doctor-state-cancelled-p state)
    (setf (magent-doctor-state-cancelled-p state) t)
    (when-let* ((process (magent-doctor-state-current-process state)))
      (when (process-live-p process)
        (delete-process process)))
    (magent-doctor--cancel-request-timer state)
    (magent-doctor--abort-request state)
    (let ((context (magent-doctor-state-context state)))
      (unless (magent-command-context-completed-p context)
        (magent-command-complete context 'cancelled "Doctor cancelled")))
    t))

(defun magent-doctor--request-callback (context state event)
  "Handle one tool-free doctor EVENT for CONTEXT and STATE."
  (let ((debug-on-error nil)
        (debug-on-quit nil)
        (debug-on-signal nil))
    (unless (or (magent-doctor-state-cancelled-p state)
                (magent-command-context-completed-p context))
      (pcase (magent-llm-event-type event)
      ('completed
       (magent-doctor--cancel-request-timer state)
       (setf (magent-doctor-state-request-handle state) nil)
       (let ((text (or (magent-llm-event-text event) "")))
         (if (string-empty-p (string-trim text))
             (magent-command-complete
              context 'failed "Doctor analysis returned an empty response")
           (condition-case nil
               (let ((result
                      (magent-doctor--normalize-output text context state)))
                 (magent-command-record-message
                  context 'assistant result nil
                  (list :source 'magent-doctor-final))
                 (magent-command-complete
                  context 'completed "Doctor diagnosis complete"
                  :record-message nil))
             (magent-doctor-security-error
              (magent-command-complete
               context 'failed "Doctor response failed security validation"))))))
      ('error
       (magent-doctor--cancel-request-timer state)
       (setf (magent-doctor-state-request-handle state) nil)
       (condition-case nil
           (magent-command-complete
            context 'failed
            (format "Doctor analysis failed: %s"
                    (magent-doctor--safe-error
                     (list 'error
                           (format "%s" (magent-llm-event-message event)))
                     state)))
         (magent-doctor-security-error
          (magent-command-complete
           context 'failed "Doctor analysis failed with a redacted error"))))))))

(defun magent-doctor--start-analysis (context state results)
  "Start one tool-free provider analysis for CONTEXT over RESULTS."
  (let* ((bundle (magent-doctor--bounded-bundle results))
         (prompt (concat (magent-prompt-read "internal/doctor-user.org")
                         "\n\n"
                         (magent-json-encode bundle)))
         (request
          (magent-llm-request-create
           :prompt prompt
           :system (magent-prompt-read "internal/doctor-system.org")
           :tools nil
           :stream nil
           :backend (and (boundp 'gptel-backend) gptel-backend)
           :model (and (boundp 'gptel-model) gptel-model)
           :metadata (list :temperature
                           (and (boundp 'gptel-temperature)
                                gptel-temperature)
                           :disable-provider-tools t
                           :include-reasoning nil
                           :magent-doctor t)
           :callback
           (lambda (event)
             (magent-doctor--request-callback context state event)))))
    (magent-command-notify context "Analyzing sanitized diagnostics...")
    (let ((handle (magent-llm-gptel-sample request)))
      (if (or (magent-doctor-state-cancelled-p state)
              (magent-command-context-completed-p context))
          (when (and (bufferp handle) (buffer-live-p handle))
            (kill-buffer handle))
        (setf (magent-doctor-state-request-handle state) handle)
        (setf (magent-doctor-state-request-timer state)
              (run-at-time
               magent-request-timeout nil
               (lambda ()
                 (unless (or (magent-doctor-state-cancelled-p state)
                             (magent-command-context-completed-p context))
                   (magent-doctor--abort-request state)
                   (magent-command-complete
                    context 'failed "Doctor analysis timed out")))))))))

(defun magent-doctor--runner (context)
  "Run the safe probe-based doctor pipeline for command CONTEXT."
  (let* ((probes (magent-doctor--select-probes context))
         (state
          (magent-doctor-state-create
           :context context
           :project-root (magent-doctor--project-root context))))
    (magent-command-set-cancel-function
     context (lambda () (magent-doctor--cancel state)))
    (magent-session-set-metadata-value
     (magent-command-context-session context)
     'selected-probes
     (vconcat (mapcar #'magent-doctor-probe-id probes)))
    (if (not (magent-doctor--confirm context probes))
        (magent-command-complete context 'cancelled "Doctor cancelled")
      (setf (magent-doctor-state-deadline state)
            (+ (float-time) magent-doctor-total-timeout))
      (condition-case nil
          (let ((results (magent-doctor--collect probes context state)))
            (unless (magent-doctor-state-cancelled-p state)
              (magent-doctor--start-analysis context state results)))
        (magent-doctor-security-error
         (magent-command-complete
          context 'failed "Doctor diagnostics failed security validation"))
        (magent-doctor-probe-timeout
         (magent-command-complete
          context 'failed "Doctor local collection timed out"))))))

(defun magent-doctor--register-builtins ()
  "Register built-in Magent doctor probes."
  (magent-doctor-register-probe
   "core-runtime"
   :description "Magent, session, queue, approval, provider, and source state"
   :collector #'magent-doctor--core-collector
   :data-categories '(runtime session provider)
   :required t)
  (magent-doctor-register-probe
   "current-buffer"
   :description "Current buffer metadata without buffer contents"
   :predicate #'magent-doctor--diagnostics-predicate
   :collector #'magent-doctor--buffer-collector
   :data-categories '(buffer metadata))
  (magent-doctor-register-probe
   "project"
   :description "Project indicators and read-only Git status"
   :predicate #'magent-doctor--project-predicate
   :collector #'magent-doctor--project-collector
   :timeout magent-doctor-process-timeout
   :data-categories '(project vc filesystem))
  (magent-doctor-register-probe
   "diagnostics"
   :description "Existing Flymake, Flycheck, and Eglot diagnostics"
   :predicate #'magent-doctor--diagnostics-predicate
   :collector #'magent-doctor--diagnostics-collector
   :data-categories '(diagnostics lsp))
  (magent-doctor-register-probe
   "compilation"
   :description "Existing project compilation buffer tails"
   :predicate #'magent-doctor--compilation-predicate
   :collector #'magent-doctor--compilation-collector
   :data-categories '(diagnostics compilation logs))
  (magent-doctor-register-probe
   "magent-logs"
   :description "Filtered Magent-related log and warning tails"
   :collector #'magent-doctor--logs-collector
   :data-categories '(logs warnings))
  (magent-doctor-register-probe
   "source-context"
   :description "Bounded source excerpt around point"
   :predicate #'magent-doctor--diagnostics-predicate
   :collector #'magent-doctor--source-context-collector
   :data-categories '(source-code)
   :manual-only t))

;;;###autoload
(defun magent-run-doctor (&optional select-probes)
  "Run Magent Doctor in an isolated internal session.
With prefix argument SELECT-PROBES, review probe selection in the minibuffer."
  (interactive "P")
  (let ((debug-on-error nil)
        (debug-on-quit nil)
        (debug-on-signal nil))
    (magent-command-run
     "doctor"
     :arguments (list :select-probes (and select-probes t)))))

(magent-doctor--register-builtins)

(magent-command-register
 "doctor"
 :description "Collect safe local evidence and diagnose Magent-related issues."
 :title "Run Magent Doctor"
 :runner-type 'diagnostic-pipeline
 :runner #'magent-doctor--runner)

(provide 'magent-doctor)
;;; magent-doctor.el ends here
