;;; magent-action-builtin-doctor.el --- Built-in Magent Doctor Action  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Implementation backing the built-in Doctor Action.  It collects bounded
;; diagnostics through trusted, read-only probe functions and sends one
;; sanitized, tool-free request to the current provider.  Custom probes are
;; trusted Emacs Lisp; this module does not sandbox them.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'lisp-mnt)
(require 'seq)
(require 'subr-x)
(require 'gptel-request)
(require 'magent-action)
(require 'magent-action-session)
(require 'magent-config)
(require 'magent-json)
(require 'magent-sampling)
(require 'magent-sampling-gptel)
(require 'magent-prompt)
(require 'magent-redaction)
(require 'magent-session)

(declare-function flymake-diagnostic-beg "flymake" t t)
(declare-function flymake-diagnostic-end "flymake" t t)
(declare-function flymake-diagnostic-text "ext:flymake" t t)
(declare-function flymake-diagnostic-type "flymake" t t)
(declare-function flymake-diagnostics "flymake")
(declare-function flycheck-error-filename "ext:flycheck" t t)
(declare-function flycheck-error-id "ext:flycheck" t t)
(declare-function flycheck-error-level "ext:flycheck" t t)
(declare-function flycheck-error-line "ext:flycheck" t t)
(declare-function flycheck-error-message "ext:flycheck" t t)
(declare-function magent-agent-info-name "magent-agent-info" t t)
(declare-function magent-action-open-session "magent-action-session-view" t t)
(declare-function magent-approval-pending-count "magent-approval")
(declare-function magent-runtime-pending-count "magent-runtime-api")
(declare-function magent-runtime-session-effective-model-route
                  "magent-runtime-api" t t)
(declare-function magent-runtime-queue-active-submission
                  "magent-runtime-queue")
(declare-function magent-runtime-submission-id
                  "magent-runtime-queue" t t)
(declare-function magent-runtime-submission-session-id
                  "magent-runtime-queue" t t)
(declare-function magent-runtime-submission-status
                  "magent-runtime-queue" t t)
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-cycle-content "org-cycle" (&optional arg))
(declare-function org-fold-show-entry "org-fold" (&optional hide-drawers))
(declare-function org-fold-show-subtree "org-fold")
(declare-function org-link-make-string "ol" (link &optional description))
(declare-function org-link-set-parameters "ol" (type &rest parameters))
(declare-function org-mode "org")

(defvar acp-package-version)
(defvar agent-shell--version)
(defvar flycheck-current-errors)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-temperature)
(defvar gptel-version)
(defvar org-todo-keywords)

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
  route
  deadline
  current-process
  request-handle
  request-timer
  done
  cancelled-p)

(defvar magent-doctor--registry (make-hash-table :test #'equal)
  "Registered Magent doctor probes keyed by probe id.")

(defconst magent-doctor--output-headings
  '("* Diagnosis" "** Summary" "** Findings"
    "** Recommended Actions" "** Limitations")
  "Required headings in a structured doctor response.")

(defconst magent-doctor--interactive-todo-keywords
  '((sequence "TODO" "|" "DONE" "FAIL" "KILL"))
  "Buffer-local task states used by the interactive Doctor view.")

(defvar-local magent-doctor--interactive-status "Starting"
  "Status shown in the current interactive Doctor buffer.")

(defvar-local magent-doctor--interactive-preflight ""
  "Preflight disclosure retained in the current Doctor buffer.")

(defvar-local magent-doctor--interactive-tasks nil
  "Structured tasks shown in the current interactive Doctor buffer.")

(defvar-local magent-doctor--interactive-runtime-info nil
  "Runtime version information shown in the current Doctor buffer.")

(defvar-local magent-doctor--interactive-model-info nil
  "Safe model route information shown in the current Doctor buffer.")

(defvar-local magent-doctor--interactive-result ""
  "Result text shown in the current interactive Doctor buffer.")

(defvar-local magent-doctor--interactive-session-id nil
  "Isolated Action session id shown in the current Doctor buffer.")

(defvar-local magent-doctor--interactive-invocation nil
  "Active Action invocation controlled by the current Doctor buffer.")

(defun magent-doctor--interactive-format-value (value)
  "Return a concise display string for Doctor metadata VALUE."
  (cond
   ((null value) "unavailable")
   ((eq value :json-false) "no")
   ((eq value t) "yes")
   ((vectorp value)
    (mapconcat #'magent-doctor--interactive-format-value
               (append value nil) ", "))
   ((and (listp value) (not (stringp value)))
    (mapconcat #'magent-doctor--interactive-format-value value ", "))
   (t (format "%s" value))))

(defun magent-doctor--interactive-task-keyword (status)
  "Return the Org keyword representing Doctor task STATUS."
  (pcase status
    ((or 'completed 'done) "DONE")
    ((or 'failed 'fail) "FAIL")
    ((or 'cancelled 'killed 'kill) "KILL")
    (_ "TODO")))

(defun magent-doctor--interactive-task-terminal-p (status)
  "Return non-nil when Doctor task STATUS is terminal."
  (memq status '(completed done failed fail cancelled killed kill)))

(defun magent-doctor--interactive-status-text ()
  "Return the Doctor status with task completion counts when available."
  (let ((total (length magent-doctor--interactive-tasks)))
    (if (zerop total)
        magent-doctor--interactive-status
      (format "%s (%d/%d)"
              magent-doctor--interactive-status
              (cl-count-if
               (lambda (task)
                 (magent-doctor--interactive-task-terminal-p
                  (plist-get task :status)))
               magent-doctor--interactive-tasks)
              total))))

(defun magent-doctor--interactive-insert-info (heading values)
  "Insert Doctor info HEADING followed by alist VALUES."
  (insert (format "** %s\n" heading))
  (dolist (entry values)
    (insert (format "- %s: %s\n"
                    (car entry)
                    (magent-doctor--interactive-format-value (cdr entry))))))

(defun magent-doctor--interactive-insert-task (task)
  "Insert one structured Doctor TASK in the current buffer."
  (let* ((id (plist-get task :id))
         (status (plist-get task :status))
         (keyword (magent-doctor--interactive-task-keyword status))
         (heading-start (point)))
    (insert (format "** %s %s\n" keyword (plist-get task :title)))
    (add-text-properties
     heading-start (1- (point))
     (list 'magent-doctor-task-id id
           'rear-nonsticky '(magent-doctor-task-id)))
    (when-let* ((description (plist-get task :description)))
      (insert description "\n"))
    (when (eq (plist-get task :llm-state) 'omitted)
      (insert "- Analysis: omitted because the total size limit was reached.\n"))
    (when-let* ((buffers (plist-get task :related-buffers)))
      (insert "- Related buffers: ")
      (let ((first t))
        (dolist (name buffers)
          (when (get-buffer name)
            (unless first
              (insert ", "))
            (setq first nil)
            (insert-text-button
             name
             'follow-link t
             'help-echo (format "Open buffer %s" name)
             'magent-doctor-buffer-name name
             'action #'magent-doctor--interactive-open-related-buffer)))
        (when first
          (insert "none currently live")))
      (insert "\n"))
    (when-let* ((note (plist-get task :note)))
      (insert "- " note "\n"))))

(defun magent-doctor--interactive-open-related-buffer (button)
  "Open the live Doctor-related buffer named by BUTTON."
  (let* ((name (button-get button 'magent-doctor-buffer-name))
         (buffer (and (stringp name) (get-buffer name))))
    (unless (buffer-live-p buffer)
      (user-error "Doctor-related buffer is no longer live: %s" name))
    (pop-to-buffer buffer)))

(defun magent-doctor--interactive-session-file (&optional session-id)
  "Return the persisted file for Doctor SESSION-ID, or nil.
SESSION-ID defaults to the session shown in the current Doctor buffer."
  (let ((id (or session-id magent-doctor--interactive-session-id)))
    (when (stringp id)
      (magent-session-validate-id id)
      (let ((file
             (expand-file-name
              (concat id ".json")
              (magent-session-action-directory "doctor"))))
        (and (file-exists-p file) file)))))

(defun magent-doctor--interactive-open-session-link (session-id arg)
  "Open the persisted Doctor Action session named by SESSION-ID.
ARG is accepted for the Org custom-link follow protocol."
  (ignore arg)
  (let ((file (magent-doctor--interactive-session-file session-id)))
    (unless file
      (user-error "Persisted Doctor session is no longer available"))
    (require 'magent-action-session-view)
    (magent-action-open-session file)))

(defun magent-doctor--interactive-register-session-link ()
  "Register the Org link used for persisted Doctor sessions."
  (org-link-set-parameters
   "magent-session"
   :follow #'magent-doctor--interactive-open-session-link))

(defun magent-doctor--interactive-update-task (update)
  "Apply one structured Doctor task UPDATE to the current buffer state."
  (let ((id (plist-get update :id)))
    (setq magent-doctor--interactive-tasks
          (mapcar
           (lambda (task)
             (if (not (equal (plist-get task :id) id))
                 task
               (let ((updated (copy-tree task)))
                 (dolist (key '(:status :llm-state :note))
                   (when (plist-member update key)
                     (setq updated
                           (plist-put updated key (plist-get update key)))))
                 updated)))
           magent-doctor--interactive-tasks))))

(defun magent-doctor--interactive-finish-pending-tasks (action-status)
  "Finish nonterminal Doctor tasks according to ACTION-STATUS.
On failure, mark the running task failed and tasks not yet started cancelled."
  (setq magent-doctor--interactive-tasks
        (mapcar
         (lambda (task)
           (let ((status (plist-get task :status)))
             (if (magent-doctor--interactive-task-terminal-p status)
                 task
               (plist-put
                (copy-tree task) :status
                (pcase action-status
                  ('completed 'completed)
                  ('cancelled 'cancelled)
                  (_ (if (eq status 'running) 'failed 'cancelled)))))))
         magent-doctor--interactive-tasks)))

(defun magent-doctor--interactive-render (buffer &optional focus)
  "Render the current interactive Doctor state in BUFFER.
FOCUS may name a top-level section to reveal and move point to."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (previous-point (point)))
        (erase-buffer)
        (insert "#+title: Magent Doctor\n#+startup: content\n\n")
        (insert (format "Status: %s\n"
                        (magent-doctor--interactive-status-text)))
        (when magent-doctor--interactive-session-id
          (insert "Session: ")
          (if (magent-doctor--interactive-session-file)
              (insert
               (org-link-make-string
                (concat "magent-session:"
                        magent-doctor--interactive-session-id)
                magent-doctor--interactive-session-id))
            (insert magent-doctor--interactive-session-id))
          (insert "\n"))
        (insert "Mode: isolated Action; provider tools disabled.\n")
        (when (member magent-doctor--interactive-status
                      '("Running" "Finishing"))
          (insert (concat "Cancel: C-c C-t on a TODO task (marks KILL), "
                          "or M-x magent-action-cancel.\n")))
        (insert "\n* Tasks\n")
        (insert (concat "Probe results are bounded and redacted before "
                        "analysis unless marked omitted.\n"))
        (if magent-doctor--interactive-tasks
            (dolist (task magent-doctor--interactive-tasks)
              (magent-doctor--interactive-insert-task task))
          (insert "** TODO Preparing Doctor task plan\n"))
        (insert "\n")
        (if (string-empty-p magent-doctor--interactive-result)
            (insert "* Diagnosis\n")
          (unless (string-match-p
                   "\\`\\* Diagnosis[ \t]*\\(?:\n\\|\\'\\)"
                   magent-doctor--interactive-result)
            (insert "* Diagnosis\n"))
          (insert magent-doctor--interactive-result)
          (unless (string-suffix-p "\n" magent-doctor--interactive-result)
            (insert "\n")))
        (insert "\n* Environment\n")
        (magent-doctor--interactive-insert-info
         "Runtime" magent-doctor--interactive-runtime-info)
        (insert "\n")
        (magent-doctor--interactive-insert-info
         "Doctor model" magent-doctor--interactive-model-info)
        (unless (string-empty-p magent-doctor--interactive-preflight)
          (insert "\n* Preflight\n#+begin_example\n"
                  magent-doctor--interactive-preflight)
          (unless (string-suffix-p
                   "\n" magent-doctor--interactive-preflight)
            (insert "\n"))
          (insert "#+end_example\n"))
        (org-cycle-content)
        (if (and focus
                 (progn
                   (goto-char (point-min))
                   (re-search-forward
                    (format "^\\* %s[ \t]*$" (regexp-quote focus)) nil t)))
            (progn
              (beginning-of-line)
              (pcase focus
                ("Tasks" (org-fold-show-entry))
                ((or "Diagnosis" "Preflight")
                 (org-fold-show-subtree))))
          (goto-char (min previous-point (point-max))))))))

(defun magent-doctor--interactive-buffer ()
  "Create, initialize, and display a buffer for an interactive Doctor run."
  (let ((buffer (generate-new-buffer "*Magent Doctor*")))
    (with-current-buffer buffer
      (require 'org)
      (magent-doctor--interactive-register-session-link)
      (let ((org-todo-keywords magent-doctor--interactive-todo-keywords))
        (org-mode))
      (local-set-key (kbd "C-c C-t") #'magent-doctor-task-kill)
      (setq buffer-read-only t)
      (setq-local magent-doctor--interactive-status "Starting"
                  magent-doctor--interactive-preflight ""
                  magent-doctor--interactive-tasks nil
                  magent-doctor--interactive-runtime-info
                  (magent-doctor--runtime-info)
                  magent-doctor--interactive-model-info
                  '(("Status" . "Resolving route"))
                  magent-doctor--interactive-result ""
                  magent-doctor--interactive-session-id nil
                  magent-doctor--interactive-invocation nil)
      (magent-doctor--interactive-render buffer))
    (display-buffer buffer)
    buffer))

(defun magent-doctor--interactive-show-preflight (buffer text)
  "Show Doctor preflight TEXT in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq magent-doctor--interactive-status "Awaiting confirmation"
            magent-doctor--interactive-preflight text)
      (magent-doctor--interactive-render buffer "Preflight"))))

(defun magent-doctor-task-kill ()
  "Mark the Doctor task at point KILL and cancel its Action invocation."
  (interactive)
  (unless (magent-action-invocation-p magent-doctor--interactive-invocation)
    (user-error "No active Doctor invocation is attached to this buffer"))
  (unless (eq (magent-action-invocation-status
               magent-doctor--interactive-invocation)
              'active)
    (user-error "The Doctor invocation is no longer active"))
  (let ((id
         (save-excursion
           (org-back-to-heading t)
           (get-text-property (line-beginning-position)
                              'magent-doctor-task-id))))
    (unless id
      (user-error "Point is not on a Doctor task"))
    (let ((task (cl-find id magent-doctor--interactive-tasks
                         :key (lambda (item) (plist-get item :id))
                         :test #'equal)))
      (unless (and task
                   (memq (plist-get task :status)
                         '(pending running todo)))
        (user-error "Doctor task %s is already terminal" id)))
    (magent-doctor--interactive-update-task
     (list :id id :status 'cancelled
           :note "Cancelled by the user; the Doctor run is stopping."))
    (magent-doctor--interactive-render (current-buffer))
    (magent-action-cancel
     magent-doctor--interactive-invocation
     (format "Doctor task %s killed by user" id))))

(defun magent-doctor--interactive-status-label (status)
  "Return a display label for terminal Doctor STATUS."
  (pcase status
    ('completed "Completed")
    ('cancelled "Cancelled")
    ('failed "Failed")
    (_ (capitalize (format "%s" status)))))

(defun magent-doctor--interactive-observe (buffer event)
  "Project Doctor EVENT into the interactive Doctor BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let (focus)
        (pcase (plist-get event :type)
          ('action-progress
           (setq magent-doctor--interactive-status "Running")
           (when-let* ((update (plist-get event :doctor-update)))
             (pcase (plist-get update :kind)
               ('plan
                (setq magent-doctor--interactive-tasks
                      (copy-tree (plist-get update :tasks))
                      magent-doctor--interactive-model-info
                      (copy-tree (plist-get update :model-info))
                      focus "Tasks"))
               ('task
                (magent-doctor--interactive-update-task update)))))
          ('assistant-delta
           (setq magent-doctor--interactive-status "Finishing")
           (when-let* ((text (plist-get event :text)))
             (setq magent-doctor--interactive-result
                   (concat magent-doctor--interactive-result text))))
          ('action-completed
           (let ((status (plist-get event :status)))
             (setq magent-doctor--interactive-status
                   (magent-doctor--interactive-status-label status))
             (magent-doctor--interactive-finish-pending-tasks status))
           (when (string-empty-p magent-doctor--interactive-result)
             (when-let* ((result (plist-get event :result)))
               (setq magent-doctor--interactive-result
                     (or (magent-execution-result-content-string result) ""))))
           (setq focus "Diagnosis"))
          (_ nil))
        (magent-doctor--interactive-render buffer focus)))))

(defun magent-doctor--normalize-id (id)
  "Return ID as a stable probe registry key."
  (if (symbolp id) (symbol-name id) (format "%s" id)))

(cl-defun magent-doctor-register-probe
    (id &key description predicate collector timeout data-categories
        required manual-only)
  "Register a trusted read-only doctor probe ID.
PREDICATE and COLLECTOR receive the Action invocation; COLLECTOR also receives
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
               (not (and (numberp timeout) (>= timeout 0))))
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
  "Return the project root captured by Action CONTEXT, or nil."
  (let ((scope (magent-action-invocation-origin-scope context)))
    (cond
     ((stringp scope) (directory-file-name (expand-file-name scope)))
     ((magent-action-invocation-origin-directory context)
      (when-let* ((root (magent-project-root
                         (magent-action-invocation-origin-directory context) t)))
        (directory-file-name (expand-file-name root)))))))

(defun magent-doctor--origin-buffer (context)
  "Return CONTEXT's live origin buffer, or nil."
  (let ((buffer (magent-action-invocation-origin-buffer context)))
    (and (buffer-live-p buffer) buffer)))

(defun magent-doctor--json-bool (value)
  "Return VALUE as a JSON boolean sentinel."
  (if value t :json-false))

(defun magent-doctor--safe-provider-name (&optional backend)
  "Return BACKEND's provider name without printing its live object.
When BACKEND is nil, use the current global gptel backend."
  (let ((selected (or backend
                      (and (boundp 'gptel-backend) gptel-backend))))
    (or (and selected
           (fboundp 'gptel-backend-p)
           (gptel-backend-p selected)
           (fboundp 'gptel-backend-name)
           (gptel-backend-name selected))
        "gptel")))

(defun magent-doctor--feature-source (feature)
  "Return the library path for FEATURE, or nil."
  (and (symbolp feature)
       (locate-library (symbol-name feature))))

(defun magent-doctor--library-version (library)
  "Return the package header version for LIBRARY, or nil."
  (when-let* ((located (locate-library library))
              (source (if (string-suffix-p ".elc" located)
                          (string-remove-suffix "c" located)
                        located))
              ((file-readable-p source)))
    (condition-case nil
        (lm-with-file source
          (or (lm-header "package-version")
              (lm-header "version")))
      (file-error nil))))

(defun magent-doctor--bound-version (symbol library)
  "Return version variable SYMBOL or LIBRARY's package header version."
  (or (and (boundp symbol)
           (let ((value (symbol-value symbol)))
             (and value (format "%s" value))))
      (magent-doctor--library-version library)
      "unavailable"))

(defun magent-doctor--runtime-info ()
  "Return safe runtime version information for Doctor display and probes."
  `(("Magent version" . ,(or (magent-doctor--library-version "magent")
                              "unavailable"))
    ("gptel version" . ,(magent-doctor--bound-version
                          'gptel-version "gptel"))
    ("agent-shell version" . ,(magent-doctor--bound-version
                                'agent-shell--version "agent-shell"))
    ("ACP version" . ,(magent-doctor--bound-version
                        'acp-package-version "acp"))
    ("Emacs version" . ,emacs-version)
    ("System" . ,system-type)))

(defun magent-doctor--model-info (route)
  "Return safe detailed Doctor model metadata for ROUTE."
  (if (not (magent-model-route-p route))
      '(("Status" . "No model route"))
    (let* ((backend (magent-model-route-backend route))
           (model (magent-model-route-model route))
           (description (and (symbolp model) (get model :description)))
           (capabilities (and (symbolp model) (get model :capabilities)))
           (capability-list
            (cond
             ((vectorp capabilities) (append capabilities nil))
             ((listp capabilities) capabilities)
             (capabilities (list capabilities))))
           (context-window (and (symbolp model) (get model :context-window))))
      `(("Backend" . ,(magent-doctor--safe-provider-name backend))
        ("Backend type" . ,(format "%s" (type-of backend)))
        ("Model" . ,(format "%s" model))
        ("Description" . ,(and description
                                (magent-doctor--truncate
                                 (format "%s" description) 500)))
        ("Capabilities" . ,(and capability-list
                                 (vconcat
                                  (mapcar (lambda (item) (format "%s" item))
                                          capability-list))))
        ("Context window" . ,context-window)
        ("Route source" . ,(magent-model-route-source route))
        ("Route phase" . ,(magent-model-route-phase route))
        ("Temperature" . ,(and (boundp 'gptel-temperature)
                                gptel-temperature))
        ("Streaming" . t)
        ("Provider tools" . "disabled")
        ("Reasoning" . "disabled")
        ("Request timeout seconds" . ,magent-request-timeout)))))

(defun magent-doctor--core-collector (context state)
  "Collect bounded Magent runtime facts for CONTEXT."
  (let* ((parent (magent-action-invocation-parent-session context))
         (thread (and parent (magent-session-thread-ledger parent)))
         (agent (and parent (magent-session-agent parent)))
         (active (and (fboundp 'magent-runtime-queue-active-submission)
                      (magent-runtime-queue-active-submission))))
    `((runtime-versions . ,(magent-doctor--runtime-info))
      (doctor-model . ,(magent-doctor--model-info
                        (magent-doctor-state-route state)))
      (emacs-version . ,emacs-version)
      (system-type . ,system-type)
      (origin-scope . ,(magent-action-invocation-origin-scope context))
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
      (active-commands
       . ,(mapcar
           (lambda (command-context)
             (magent-action-spec-name
              (magent-action-invocation-spec command-context)))
           (magent-action-session-active-invocations)))
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
                 '(magent magent-action magent-action-session
                   magent-action-builtin-doctor
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
Zero disables the process-specific timeout; the total collection deadline,
when enabled, still applies.
DIRECTORY defaults to STATE's project root.  Doctor probes are local-only;
remote project probes must use Emacs file APIs instead.  Return exit and
output data."
  (let* ((default-directory (file-name-as-directory
                             (expand-file-name
                              (or directory
                                  (magent-doctor-state-project-root state)
                                  default-directory))))
         (_ (when (file-remote-p default-directory)
              (error "Doctor process probes are local-only: %s"
                     default-directory)))
         (executable (or (and (file-name-absolute-p program) program)
                         (executable-find program)
                         (error "Doctor executable not found: %s" program)))
         (buffer (generate-new-buffer " *magent-doctor-process*"))
         (configured (if (null timeout)
                         magent-doctor-process-timeout
                       timeout))
         (remaining (when (magent-doctor-state-deadline state)
                      (max 0.0
                           (- (magent-doctor-state-deadline state)
                              (float-time)))))
         (limit (cond
                 ((and (> configured 0) remaining)
                  (min configured remaining))
                 ((> configured 0) configured)
                 (remaining remaining)
                 (t nil)))
         (deadline (and limit (+ (float-time) limit)))
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
            (when (and deadline (>= (float-time) deadline))
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
                   (and (bound-and-true-p eglot-managed-mode)
                        (fboundp 'eglot-current-server)
                        (eglot-current-server))))))))
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

(defun magent-doctor--directory-in-project-p (directory root)
  "Return non-nil when DIRECTORY is within project ROOT.
Return nil when either path is unavailable or cannot be inspected."
  (and (stringp directory)
       (stringp root)
       (condition-case nil
           (file-in-directory-p directory root)
         (file-error nil))))

(defun magent-doctor--compilation-predicate (context)
  "Return non-nil when CONTEXT has project compilation buffers."
  (when-let* ((root (magent-doctor--project-root context)))
    (cl-some
     (lambda (buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'compilation-mode)
              (magent-doctor--directory-in-project-p
               default-directory root))))
     (buffer-list))))

(defun magent-doctor--compilation-collector (context _state)
  "Collect bounded existing compilation output for CONTEXT."
  (let ((root (magent-doctor--project-root context))
        results)
    (dolist (buffer (buffer-list))
      (when (and (< (length results) 3)
                 (with-current-buffer buffer
                   (and (derived-mode-p 'compilation-mode)
                        (magent-doctor--directory-in-project-p
                         default-directory root))))
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
  "Select probes for Action CONTEXT, prompting for a manual run."
  (let* ((automatic (magent-doctor--automatic-probes context))
         (manual
          (or (plist-get (magent-action-invocation-options context)
                         :select-probes)
              (equal (magent-action-invocation-argument context) "select"))))
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

(defun magent-doctor--preflight-text (context probes route)
  "Return a local-only preflight disclosure for CONTEXT, PROBES, and ROUTE."
  (let* ((root (magent-doctor--project-root context))
         (model-info (magent-doctor--model-info route))
         (source-selected
          (cl-find "source-context" probes
                   :key #'magent-doctor-probe-id :test #'equal))
         (categories
          (delete-dups
           (apply #'append
                  (mapcar #'magent-doctor-probe-data-categories probes)))))
    (string-join
     (append
      (list (format "Provider: %s" (cdr (assoc "Backend" model-info)))
            (format "Model: %s" (cdr (assoc "Model" model-info)))
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

(defun magent-doctor--confirm (context probes route)
  "Confirm the doctor collection plan for CONTEXT, PROBES, and ROUTE."
  (if magent-bypass-permission
      t
    (let ((buffer
           (or (let ((candidate
                      (plist-get (magent-action-invocation-options context)
                                 :interactive-buffer)))
                 (and (buffer-live-p candidate) candidate))
               (magent-doctor--interactive-buffer))))
      (magent-doctor--interactive-show-preflight
       buffer (magent-doctor--preflight-text context probes route)))
    (yes-or-no-p "Run Magent Doctor with these probes? ")))

(defun magent-doctor--task-related-buffers (context probe)
  "Return existing local buffer names relevant to CONTEXT and PROBE."
  (let ((id (magent-doctor-probe-id probe)))
    (pcase id
      ((or "current-buffer" "diagnostics" "source-context")
       (when-let* ((buffer (magent-doctor--origin-buffer context)))
         (list (buffer-name buffer))))
      ("magent-logs"
       (seq-filter #'get-buffer
                   '("*magent-log*" "*Warnings*" "*Messages*")))
      ("compilation"
       (when-let* ((root (magent-doctor--project-root context)))
         (seq-take
          (delq nil
                (mapcar
                 (lambda (buffer)
                   (with-current-buffer buffer
                     (and (derived-mode-p 'compilation-mode)
                          (magent-doctor--directory-in-project-p
                           default-directory root)
                          (buffer-name buffer))))
                 (buffer-list)))
          3))))))

(defun magent-doctor--task-plan (context probes)
  "Return interactive Doctor tasks for CONTEXT, PROBES, and final analysis."
  (append
   (mapcar
    (lambda (probe)
      (list :id (magent-doctor-probe-id probe)
            :kind 'probe
            :title (format "probe %s" (magent-doctor-probe-id probe))
            :description (magent-doctor-probe-description probe)
            :related-buffers
            (magent-doctor--task-related-buffers context probe)
            :status 'pending
            :llm-state 'pending
            :note nil))
    probes)
   (list
    (list :id "analysis"
          :kind 'analysis
          :title "Analyze collected results"
          :description
          "Ask the selected Doctor model to analyze bounded, redacted probe results."
          :status 'pending
          :llm-state nil
          :note nil))))

(defun magent-doctor--report-plan (context probes route)
  "Report the structured Doctor task plan for CONTEXT, PROBES, and ROUTE."
  (magent-action-progress
   context "Prepared Doctor task plan."
   :doctor-update
   (list :kind 'plan
         :tasks (magent-doctor--task-plan context probes)
         :model-info (magent-doctor--model-info route))))

(defun magent-doctor--report-task (context id status &rest properties)
  "Report Doctor task ID with STATUS and optional PROPERTIES for CONTEXT."
  (magent-action-progress
   context
   (format "Doctor task %s %s." id status)
   :doctor-update
   (append (list :kind 'task :id id :status status) properties)))

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
  (when (and (magent-doctor-state-deadline state)
             (>= (float-time) (magent-doctor-state-deadline state)))
    (signal 'magent-doctor-probe-timeout '("total collection timeout")))
  (let* ((remaining (when (magent-doctor-state-deadline state)
                      (max 0.0
                           (- (magent-doctor-state-deadline state)
                              (float-time)))))
         (configured (if (null (magent-doctor-probe-timeout probe))
                         magent-doctor-probe-timeout
                       (magent-doctor-probe-timeout probe)))
         (timeout (cond
                   ((and (> configured 0) remaining)
                    (min configured remaining))
                   ((> configured 0) configured)
                   (remaining remaining)
                   (t nil)))
         (id (magent-doctor-probe-id probe)))
    (magent-doctor--report-task context id 'running)
    (condition-case err
        (let* ((raw
                (if timeout
                    (with-timeout
                        (timeout
                         (signal 'magent-doctor-probe-timeout (list id)))
                      (funcall (magent-doctor-probe-collector probe)
                               context state))
                  (funcall (magent-doctor-probe-collector probe)
                           context state)))
               (safe (magent-doctor--sanitize-value raw state))
               (result `((id . ,id)
                         (status . "completed")
                         (data . ,safe))))
          result)
      (magent-doctor-security-error (signal (car err) (cdr err)))
      (magent-redaction-unsafe-value
       (signal 'magent-doctor-security-error '("Probe output rejected")))
      (magent-doctor-probe-timeout
       (let ((message "Probe timed out"))
         `((id . ,id) (status . "failed") (error . ,message))))
      (quit (signal 'quit nil))
      (error
       (let ((message (magent-doctor--safe-error err state)))
         `((id . ,id) (status . "failed") (error . ,message)))))))

(defun magent-doctor--collect (probes context state)
  "Run PROBES serially for CONTEXT using STATE."
  (let (results)
    (dolist (probe probes (nreverse results))
      (when (magent-doctor-state-cancelled-p state)
        (signal 'quit nil))
      (let* ((result (magent-doctor--run-probe probe context state))
             (status (if (equal (cdr (assq 'status result)) "completed")
                         'completed
                       'failed)))
        (magent-doctor--report-task
         context (magent-doctor-probe-id probe) status)
        (push result results)))))

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

(defun magent-doctor--report-bundle-membership (context results bundle)
  "Report which sanitized RESULTS enter Doctor BUNDLE for CONTEXT."
  (let ((omitted (append (cdr (assq 'omitted-probes bundle)) nil)))
    (dolist (result results)
      (let* ((id (cdr (assq 'id result)))
             (status (if (equal (cdr (assq 'status result)) "completed")
                         'completed
                       'failed)))
        (if (member id omitted)
            (magent-doctor--report-task
             context id status
             :llm-state 'omitted)
          (magent-doctor--report-task
           context id status
           :llm-state 'included))))))

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
       (magent-action-invocation-session context)
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
    t))

(defun magent-doctor--done (state status value)
  "Complete Doctor STATE once with STATUS and VALUE."
  (when-let* ((done (magent-doctor-state-done state)))
    (setf (magent-doctor-state-done state) nil)
    (funcall done status value)))

(defun magent-doctor--request-callback (context state event)
  "Handle one tool-free doctor EVENT for CONTEXT and STATE."
  (let ((debug-on-error nil)
        (debug-on-quit nil)
        (debug-on-signal nil))
    (unless (or (magent-doctor-state-cancelled-p state)
                (not (eq (magent-action-invocation-status context) 'active)))
      (pcase (magent-sampling-event-type event)
      ('completed
       (magent-doctor--cancel-request-timer state)
       (setf (magent-doctor-state-request-handle state) nil)
       (let ((text (or (magent-sampling-event-text event) "")))
         (if (string-empty-p (string-trim text))
             (magent-doctor--done
              state 'failed "Doctor analysis returned an empty response")
           (condition-case nil
               (let ((result
                      (magent-doctor--normalize-output text context state)))
                 (magent-doctor--done state 'completed result))
             (magent-doctor-security-error
              (magent-doctor--done
               state 'failed
               "Doctor response failed security validation"))))))
      ('error
       (magent-doctor--cancel-request-timer state)
       (setf (magent-doctor-state-request-handle state) nil)
       (condition-case nil
           (magent-doctor--done
            state 'failed
            (format "Doctor analysis failed: %s"
                    (magent-doctor--safe-error
                     (list 'error
                           (format "%s" (magent-sampling-event-message event)))
                     state)))
         (magent-doctor-security-error
          (magent-doctor--done
           state 'failed
           "Doctor analysis failed with a redacted error"))))))))

(defun magent-doctor--analysis-route (context)
  "Return the effective model route for Doctor CONTEXT.
Slash invocations retain their originating runtime session as the Action's
control session even though Doctor itself runs in an isolated session.  Use
that route so Doctor follows the user's active model selection.  Interactive
invocations without a control session use the current gptel defaults."
  (if-let* ((runtime-session
             (magent-action-invocation-control-session context)))
      (magent-runtime-session-effective-model-route
       runtime-session nil 'doctor)
    (magent-sampling-gptel-default-route)))

(defun magent-doctor--start-analysis (context state results)
  "Start one tool-free provider analysis for CONTEXT over RESULTS."
  (let* ((bundle (magent-doctor--bounded-bundle results))
         (bundle-json (magent-json-encode bundle))
         (route (magent-doctor-state-route state))
         (prompt (concat (magent-prompt-read "internal/doctor-user.org")
                         "\n\n"
                         bundle-json))
         (request
          (magent-sampling-request-create
           :prompt prompt
           :system (magent-prompt-read "internal/doctor-system.org")
           :tools nil
           :stream t
           :backend (magent-model-route-backend route)
           :model (magent-model-route-model route)
           :metadata (list :temperature
                           (and (boundp 'gptel-temperature)
                                gptel-temperature)
                           :disable-provider-tools t
                           :include-reasoning nil
                           :magent-doctor t)
           :callback
           (lambda (event)
             (magent-doctor--request-callback context state event)))))
    (magent-doctor--report-bundle-membership context results bundle)
    (magent-doctor--report-task
     context "analysis" 'running
     :note (format (concat "Sending %d characters of bounded, redacted probe "
                           "results; provider tools are disabled.")
                   (length bundle-json)))
    (let ((handle (magent-sampling-gptel-sample request)))
      (if (or (magent-doctor-state-cancelled-p state)
              (not (eq (magent-action-invocation-status context) 'active)))
          (when (and (bufferp handle) (buffer-live-p handle))
            (kill-buffer handle))
        (setf (magent-doctor-state-request-handle state) handle)
        (when (> magent-request-timeout 0)
          (setf (magent-doctor-state-request-timer state)
                (run-at-time
                 magent-request-timeout nil
                 (lambda ()
                   (unless (or (magent-doctor-state-cancelled-p state)
                               (not (eq (magent-action-invocation-status
                                         context)
                                        'active)))
                     (magent-doctor--abort-request state)
                     (magent-doctor--done
                      state 'failed "Doctor analysis timed out"))))))))))

(defun magent-doctor--start (context done)
  "Start the safe Doctor pipeline for CONTEXT, completing through DONE."
  (unless (member (magent-action-invocation-argument context)
                  '("" "select"))
    (user-error "Usage: /doctor [select]"))
  (let* ((probes (magent-doctor--select-probes context))
         (route (magent-doctor--analysis-route context))
         (state
          (magent-doctor-state-create
           :context context
           :done done
           :route route
           :project-root (magent-doctor--project-root context))))
    (magent-session-set-metadata-value
     (magent-action-invocation-session context)
     'selected-probes
     (vconcat (mapcar #'magent-doctor-probe-id probes)))
    (if (not (magent-doctor--confirm context probes route))
        (magent-doctor--done state 'cancelled "Doctor cancelled")
      (magent-doctor--report-plan context probes route)
      (setf (magent-doctor-state-deadline state)
            (and (> magent-doctor-total-timeout 0)
                 (+ (float-time) magent-doctor-total-timeout)))
      (condition-case nil
          (let ((results (magent-doctor--collect probes context state)))
            (unless (magent-doctor-state-cancelled-p state)
              (magent-doctor--start-analysis context state results)))
        (magent-doctor-security-error
         (magent-doctor--done
          state 'failed "Doctor diagnostics failed security validation"))
        (magent-doctor-probe-timeout
         (magent-doctor--done
          state 'failed "Doctor local collection timed out"))))
    (lambda () (magent-doctor--cancel state))))

(magent-define-workflow magent-doctor--workflow (context)
  "Run Doctor as one cancellable callback Step."
  (magent-workflow-callback
      "Diagnose Magent"
      (lambda (done) (magent-doctor--start context done))))

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
   ;; The collector reads `magent-doctor-process-timeout' at run time.  Do not
   ;; freeze its Custom value when this file is loaded; the total deadline
   ;; still bounds the whole probe.
   :timeout 0
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
(defun magent-action-run-doctor (&optional select-probes)
  "Run Magent Doctor in an isolated Action session.
With prefix argument SELECT-PROBES, review probe selection in the minibuffer."
  (interactive "P")
  (let ((buffer (magent-doctor--interactive-buffer))
        (debug-on-error nil)
        (debug-on-quit nil)
        (debug-on-signal nil))
    (condition-case err
        (let ((invocation
               (magent-action-run
                "doctor"
                :options (list :select-probes (and select-probes t)
                               :interactive-buffer buffer)
                :observer
                (lambda (event)
                  (magent-doctor--interactive-observe buffer event)))))
          (when (magent-action-invocation-p invocation)
            (with-current-buffer buffer
              (setq magent-doctor--interactive-invocation invocation)))
          (when-let* (((magent-action-invocation-p invocation))
                      (session (magent-action-invocation-session invocation)))
            (with-current-buffer buffer
              (setq magent-doctor--interactive-session-id
                    (magent-session-get-id session))
              (magent-doctor--interactive-render buffer)))
          invocation)
      (error
       (magent-doctor--interactive-observe
        buffer
        (list :type 'action-completed
              :status 'failed
              :result (magent-execution-result-failed
                       (error-message-string err))))
       (signal (car err) (cdr err))))))

(magent-doctor--register-builtins)

(defun magent-action-builtin-doctor-register ()
  "Register the core Doctor Action."
  (magent-action-register
   "doctor"
   :description "Collect safe local evidence and diagnose Magent-related issues."
   :title "Run Magent Doctor"
   :exposure '(slash interactive)
   :session-policy 'isolated
   :workflow #'magent-doctor--workflow
   :source-layer 'core))

(provide 'magent-action-builtin-doctor)
;;; magent-action-builtin-doctor.el ends here
