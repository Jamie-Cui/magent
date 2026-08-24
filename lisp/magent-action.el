;;; magent-action.el --- Unified Magent user actions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6

;;; Commentary:

;; Public registration and invocation API for Magent user actions.  An action
;; may be exposed through agent-shell slash input, an interactive M-x wrapper,
;; or both.  It may use the current conversation or an isolated durable
;; Action session.

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'subr-x)
(require 'url-util)
(require 'magent-config)
(require 'magent-log)
(require 'magent-prompt)
(require 'magent-protocol)
(require 'magent-session)

(declare-function magent-action-builtins-register "magent-action-builtins")
(declare-function magent-action-session-cancel "magent-action-session")
(declare-function magent-action-session-finalize "magent-action-session")
(declare-function magent-action-session-finalize-workflow-turn
                  "magent-action-session")
(declare-function magent-action-session-initialize "magent-action-session")
(declare-function magent-action-session-read-active-id "magent-action-session")
(declare-function magent-action-session-record-message "magent-action-session")
(declare-function magent-action-session-finish-step "magent-action-session")
(declare-function magent-action-session-untrack "magent-action-session")
(declare-function magent-action-session-start-step "magent-action-session")
(declare-function magent-runtime-active-project-scope "magent-runtime")
(declare-function magent-runtime-context-scope "magent-runtime")
(declare-function magent-runtime-ensure-initialized "magent-runtime")
(declare-function magent-runtime-prepare-context "magent-runtime")
(declare-function magent-runtime-cancel-submission "magent-runtime-api")
(declare-function magent-runtime-session-current "magent-runtime-api")
(declare-function magent-runtime-session-magent-session
                  "magent-runtime-api" t t)
(declare-function magent-runtime-session-pending-skills
                  "magent-runtime-api" t t)
(declare-function magent-runtime-session-scope "magent-runtime-api" t t)
(declare-function magent-runtime-submit "magent-runtime-api")
(define-error 'magent-action-step-error "Magent Action Step failed")
(define-error 'magent-action-process-error "Magent Action process Step failed"
  'magent-action-step-error)
(define-error 'magent-action-agent-error "Magent Action agent Step failed"
  'magent-action-step-error)
(define-error 'magent-action-callback-error "Magent Action callback Step failed"
  'magent-action-step-error)

(cl-defstruct (magent-action-invocation
               (:constructor magent-action-invocation-create)
               (:copier nil))
  "Runtime state for one Magent Action Invocation."
  id
  spec
  control-session
  runtime-session
  session
  scope
  origin-buffer
  origin-directory
  origin-scope
  parent-session
  parent-scope
  parent-session-id
  options
  turn-id
  response-recorded-p
  interactive-p
  raw-input
  argument
  request-context
  resource-blocks
  observer
  approval-provider
  completion-function
  submission-adapter
  iterator
  current-step
  current-step-id
  current-submission-id
  (generation 0)
  step-cancel-function
  (status 'active)
  result)

(cl-defstruct (magent-action-step
               (:constructor magent-action-step-create)
               (:copier nil))
  "One asynchronous boundary yielded by an Action Workflow."
  type
  name
  options
  terminal-p)

(cl-defstruct (magent-action-step-outcome
               (:constructor magent-action-step-outcome-create)
               (:copier nil))
  "Value resumed into a Workflow after one Step finishes."
  status
  value
  condition)

(cl-defstruct (magent-action-process-result
               (:constructor magent-action-process-result-create)
               (:copier nil))
  "Complete result from one Action process Step."
  name
  argv
  directory
  exit-status
  stdout
  stderr
  duration-ms
  timed-out-p)

(defmacro magent-define-workflow (name arglist &rest body)
  "Define NAME as a sequential Action Workflow accepting ARGLIST."
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-list lambda-doc &rest sexp)))
  `(iter-defun ,name ,arglist ,@body))

(defun magent-action--normalize-step-name (name)
  "Return NAME as a non-empty Step label."
  (unless (and (stringp name) (not (string-empty-p (string-trim name))))
    (error "Magent Action Step requires a non-empty name: %S" name))
  name)

(defun magent-action--validate-result-mode (mode)
  "Return validated Step result MODE."
  (unless (memq mode '(value full))
    (error "Invalid Magent Action Step result mode: %S" mode))
  mode)

(cl-defun magent-action--make-process-step
    (name argv &key directory environment
          (timeout magent-action-process-timeout)
          (check t) (result 'value)
          (record-command t) (record-output t))
  "Create a process Step named NAME for ARGV."
  (magent-action--normalize-step-name name)
  (unless (and (proper-list-p argv) argv
               (cl-every #'stringp argv))
    (error "Magent Action process requires non-empty string argv: %S" argv))
  (unless (or (null timeout) (and (numberp timeout) (>= timeout 0)))
    (error "Invalid Magent Action process timeout: %S" timeout))
  (unless (or (null environment)
              (and (proper-list-p environment)
                   (cl-every
                    (lambda (entry)
                      (and (consp entry)
                           (stringp (car entry))
                           (or (null (cdr entry)) (stringp (cdr entry)))))
                    environment)))
    (error "Expected environment alist, got: %S" environment))
  (magent-action--validate-result-mode result)
  (magent-action-step-create
   :type 'process
   :name name
   :options
   (list :argv (copy-sequence argv)
         :directory (file-name-as-directory
                     (expand-file-name (or directory default-directory)))
         :process-environment (copy-sequence process-environment)
         :environment (copy-tree environment)
         :timeout timeout
         :check (and check t)
         :result result
         :record-command (and record-command t)
         :record-output (and record-output t))))

(defmacro magent-workflow-process (name argv &rest options)
  "Run ARGV as process Step NAME and return its selected result."
  (declare (indent 2))
  `(magent-action--unwrap-step-outcome
    (iter-yield
     (magent-action--make-process-step ,name ,argv ,@options))))

(cl-defun magent-action--make-agent-step
    (name prompt &key agent skills buffers append-argument-p tools
          (result 'value) request-context resource-blocks terminal-p)
  "Create an agent or terminal Answer Step named NAME for PROMPT."
  (magent-action--normalize-step-name name)
  (unless (stringp prompt)
    (error "Magent Action agent Step requires a string prompt: %S" prompt))
  (unless (or (null agent) (symbolp agent) (stringp agent))
    (error "Expected Magent Action agent name, got: %S" agent))
  (unless (or (null skills) (proper-list-p skills))
    (error "Expected Magent Action skill list, got: %S" skills))
  (unless (or (null buffers) (proper-list-p buffers))
    (error "Expected Magent Action buffer configuration list, got: %S"
           buffers))
  (unless (memq append-argument-p '(nil t))
    (error "Expected append-argument-p boolean, got: %S"
           append-argument-p))
  (unless (proper-list-p tools)
    (error "Expected exact tool list, got: %S" tools))
  (magent-action--validate-result-mode result)
  (magent-action-step-create
   :type (if terminal-p 'answer 'agent)
   :name name
   :terminal-p terminal-p
   :options
   (list :prompt prompt
         :agent agent
         :skills skills
         :buffers buffers
         :append-argument-p (and append-argument-p t)
         :tools tools
         :result (if terminal-p 'full result)
         :request-context request-context
         :resource-blocks resource-blocks)))

(defmacro magent-workflow-agent-turn (name prompt &rest options)
  "Run PROMPT as intermediate agent Step NAME and return its result."
  (declare (indent 2))
  `(magent-action--unwrap-step-outcome
    (iter-yield
     (magent-action--make-agent-step ,name ,prompt ,@options))))

(defmacro magent-workflow-answer (name prompt &rest options)
  "Run PROMPT as terminal Answer Step NAME.
The Invocation ends when the Step finishes; forms after this call do not run."
  (declare (indent 2))
  `(iter-yield
    (magent-action--make-agent-step
     ,name ,prompt ,@options :terminal-p t)))

(cl-defun magent-action--make-callback-step
    (name start &key activity-input activity-formatter)
  "Create callback Step NAME using START."
  (magent-action--normalize-step-name name)
  (unless (functionp start)
    (error "Magent Action callback Step requires START function: %S" start))
  (unless (or (null activity-formatter) (functionp activity-formatter))
    (error "Expected callback activity formatter, got: %S"
           activity-formatter))
  (magent-action-step-create
   :type 'callback
   :name name
   :options (list :start start
                  :activity-input activity-input
                  :activity-formatter activity-formatter)))

(defmacro magent-workflow-callback (name start &rest options)
  "Wait for callback Step NAME started by START and return its value.
START receives one DONE function, called as (DONE STATUS VALUE), and returns
an optional zero-argument cancellation function."
  (declare (indent 2))
  `(magent-action--unwrap-step-outcome
    (iter-yield
     (magent-action--make-callback-step ,name ,start ,@options))))

(defun magent-action--unwrap-step-outcome (outcome)
  "Return completed OUTCOME value or signal its typed failure."
  (unless (magent-action-step-outcome-p outcome)
    (error "Workflow resumed with invalid Step outcome: %S" outcome))
  (pcase (magent-action-step-outcome-status outcome)
    ('completed (magent-action-step-outcome-value outcome))
    ('failed
     (let ((condition (magent-action-step-outcome-condition outcome)))
       (if (and (consp condition) (symbolp (car condition)))
           (signal (car condition) (cdr condition))
         (signal 'magent-action-step-error
                 (list (format "%s" condition))))))
    (_
     (error "Workflow received non-resumable Step status: %S"
            (magent-action-step-outcome-status outcome)))))

(defun magent-action--step-option (step key)
  "Return STEP option KEY."
  (plist-get (magent-action-step-options step) key))

(defun magent-action--step-activity-input (step)
  "Return safe default activity input for STEP."
  (pcase (magent-action-step-type step)
    ('process
     (append
      (when (magent-action--step-option step :record-command)
        (list :argv (magent-action--step-option step :argv)))
      (list :directory (magent-action--step-option step :directory))
      (when-let* ((environment
                   (magent-action--step-option step :environment)))
        (list :environment-keys (mapcar #'car environment)))))
    ('callback (magent-action--step-option step :activity-input))
    ((or 'agent 'answer)
     (list :agent (magent-action--step-option step :agent)
           :terminal (eq (magent-action-step-type step) 'answer)))
    (_ nil)))

(defun magent-action--workflow-bounded-output (text)
  "Return TEXT bounded for durable Step activity."
  (let ((value (or text ""))
        (limit magent-action-step-output-max-chars))
    (if (or (null limit) (<= (length value) limit))
        value
      (concat
       (format "[... truncated %d leading characters ...]\n"
               (- (length value) limit))
       (substring value (- (length value) limit))))))

(defun magent-action--step-activity-output (step status value)
  "Return durable activity output for STEP STATUS and VALUE."
  (pcase (magent-action-step-type step)
    ('process
     (when (and (magent-action--step-option step :record-output)
                (magent-action-process-result-p value))
       (magent-action--workflow-bounded-output
        (string-join
         (delq nil
               (list
                (and (not (string-empty-p
                           (magent-action-process-result-stdout value)))
                     (concat "stdout:\n"
                             (magent-action-process-result-stdout value)))
                (and (not (string-empty-p
                           (magent-action-process-result-stderr value)))
                     (concat "stderr:\n"
                             (magent-action-process-result-stderr value)))))
         "\n"))))
    ('callback
     (when-let* ((formatter
                  (magent-action--step-option step :activity-formatter)))
       (condition-case err
           (let ((formatted (funcall formatter status value)))
             (and formatted
                  (magent-action--workflow-bounded-output
                   (if (stringp formatted)
                       formatted
                     (format "%s" formatted)))))
         (error
          (magent-log "WARN Action callback activity formatter failed: %s"
                      (error-message-string err))
          nil))))
    (_ nil)))

(defun magent-action--workflow-process-environment (step)
  "Return process environment captured by STEP with overrides applied."
  (let ((process-environment
         (copy-sequence
          (magent-action--step-option step :process-environment))))
    (dolist (entry (magent-action--step-option step :environment))
      (setenv (car entry) (cdr entry)))
    process-environment))

(defun magent-action--workflow-buffer-text (buffer)
  "Return BUFFER text, or an empty string if BUFFER is dead."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun magent-action--workflow-start-process (step done)
  "Start process STEP and call DONE once.  Return its cancel function."
  (let* ((stdout (generate-new-buffer " *magent-action-stdout*"))
         (stderr (generate-new-buffer " *magent-action-stderr*"))
         (argv (magent-action--step-option step :argv))
         (directory (magent-action--step-option step :directory))
         (timeout (magent-action--step-option step :timeout))
         (check (magent-action--step-option step :check))
         (started-at (float-time))
         (finished-p nil)
         process stderr-process timer)
    (cl-labels
        ((cleanup
          ()
          (when timer
            (cancel-timer timer)
            (setq timer nil))
          (when (and (processp stderr-process)
                     (process-live-p stderr-process))
            (delete-process stderr-process))
          (when (buffer-live-p stdout) (kill-buffer stdout))
          (when (buffer-live-p stderr) (kill-buffer stderr)))
         (finish
          (timed-out-p)
          (unless finished-p
            (setq finished-p t)
            (let* ((exit-status
                    (if timed-out-p
                        124
                      (if (processp process)
                          (process-exit-status process)
                        1)))
                   (result
                    (magent-action-process-result-create
                     :name (magent-action-step-name step)
                     :argv argv
                     :directory directory
                     :exit-status exit-status
                     :stdout (magent-action--workflow-buffer-text stdout)
                     :stderr (magent-action--workflow-buffer-text stderr)
                     :duration-ms
                     (truncate (* 1000 (- (float-time) started-at)))
                     :timed-out-p timed-out-p)))
              (cleanup)
              (funcall done
                       (if (or (and (not timed-out-p) (zerop exit-status))
                               (not check))
                           'completed
                         'failed)
                       result))))
         (sentinel
          (child _event)
          (when (and (eq child process)
                     (memq (process-status child) '(exit signal)))
            (finish nil))))
      (condition-case err
          (let ((default-directory directory)
                (process-environment
                 (magent-action--workflow-process-environment step)))
            (setq stderr-process
                  (make-pipe-process
                   :name "magent-action-process-stderr"
                   :buffer stderr
                   :coding 'utf-8-unix
                   :noquery t
                   :sentinel #'ignore))
            (setq process
                  (make-process
                   :name "magent-workflow-process"
                   :buffer stdout
                   :stderr stderr-process
                   :command argv
                   :connection-type 'pipe
                   :coding 'utf-8-unix
                   :noquery t
                   :sentinel #'ignore))
            (set-process-sentinel process #'sentinel)
            (when (and timeout (> timeout 0))
              (setq timer
                    (run-at-time
                     timeout nil
                     (lambda ()
                       (when (and (processp process)
                                  (process-live-p process))
                         ;; Claim timeout before deletion can synchronously
                         ;; run the process sentinel.
                         (set-process-sentinel process #'ignore)
                         (delete-process process))
                       (finish t)))))
            (when (memq (process-status process) '(exit signal))
              (sentinel process "finished\n")))
        (error
         (cleanup)
         (signal (car err) (cdr err))))
      (lambda ()
        (unless finished-p
          (setq finished-p t)
          (when (and (processp process) (process-live-p process))
            (delete-process process))
          (cleanup))))))

(defun magent-action--workflow-step-condition (step value)
  "Return typed condition data for failed STEP VALUE."
  (pcase (magent-action-step-type step)
    ('process
     (if (magent-action-process-result-p value)
         (let* ((result value)
                (detail
                 (if (magent-action-process-result-timed-out-p result)
                     "timed out"
                   (format
                    "exited with status %s"
                    (magent-action-process-result-exit-status result))))
                (output
                 (string-trim
                  (concat (magent-action-process-result-stderr result)
                          "\n"
                          (magent-action-process-result-stdout result)))))
           (list 'magent-action-process-error
                 (format "%s %s%s"
                         (magent-action-step-name step)
                         detail
                         (if (string-empty-p output)
                             ""
                           (concat ": " output)))
                 result))
       (list 'magent-action-process-error
             (if (and (consp value) (symbolp (car value))
                      (get (car value) 'error-conditions))
                 (error-message-string value)
               (format "%s" value))
             value)))
    ((or 'agent 'answer)
     (list 'magent-action-agent-error
           (cond
            ((magent-execution-result-p value)
             (magent-execution-result-content-string value))
            ((and (consp value) (symbolp (car value))
                  (get (car value) 'error-conditions))
             (error-message-string value))
            (t (format "%s" value)))
           value))
    ('callback
     (list 'magent-action-callback-error
           (if (and (consp value) (symbolp (car value))
                    (get (car value) 'error-conditions))
               (error-message-string value)
             (format "%s" value))
           value))
    (_ (list 'magent-action-step-error (format "%s" value) value))))

(defun magent-action--workflow-selected-value (step value)
  "Return convenience or full VALUE selected by STEP."
  (if (eq (magent-action--step-option step :result) 'full)
      value
    (pcase (magent-action-step-type step)
      ('process
       (if (magent-action-process-result-p value)
           (magent-action-process-result-stdout value)
         value))
      ('agent
       (if (magent-execution-result-p value)
           (magent-execution-result-content-string value)
         value))
      (_ value))))

(defun magent-action--workflow-active-p (invocation generation)
  "Return non-nil when INVOCATION still owns GENERATION."
  (and (eq (magent-action-invocation-status invocation) 'active)
       (= generation (magent-action-invocation-generation invocation))))

(defun magent-action--workflow-close-iterator (invocation)
  "Close INVOCATION iterator, logging cleanup errors."
  (when-let* ((iterator (magent-action-invocation-iterator invocation)))
    (setf (magent-action-invocation-iterator invocation) nil)
    (condition-case err
        (iter-close iterator)
      (error
       (magent-log "ERROR Action Workflow iterator cleanup failed: %s"
                   (error-message-string err))))))

(defun magent-action--workflow-finish-step
    (invocation generation status value)
  "Finish INVOCATION Step GENERATION with STATUS and VALUE."
  (when (magent-action--workflow-active-p invocation generation)
    (let ((step (magent-action-invocation-current-step invocation))
          (item-id (magent-action-invocation-current-step-id invocation)))
      (setf (magent-action-invocation-current-step invocation) nil
            (magent-action-invocation-current-step-id invocation) nil
            (magent-action-invocation-step-cancel-function invocation) nil)
      (magent-action--workflow-step-finish
       invocation step item-id status value)
      (if (magent-action-step-terminal-p step)
          (progn
            (magent-action--workflow-close-iterator invocation)
            (pcase status
              ('completed (magent-action--finish-answer invocation value))
              ('cancelled
               (magent-action--finish-cancelled invocation value))
              (_ (magent-action--finish-failed
                  invocation
                  (magent-action--workflow-step-condition step value)))))
        (pcase status
          ('cancelled
           (magent-action--finish-cancelled invocation value))
          ((or 'completed 'failed)
           (magent-action--workflow-resume
            invocation
            (magent-action-step-outcome-create
             :status status
             :value (if (eq status 'completed)
                        (magent-action--workflow-selected-value step value)
                      value)
             :condition (and (eq status 'failed)
                             (magent-action--workflow-step-condition
                              step value)))))
          (_
           (magent-action--finish-failed
            invocation (format "Invalid Action Step status: %S" status))))))))

(defun magent-action--workflow-start-step (invocation step)
  "Start STEP for INVOCATION with synchronous-callback protection."
  (unless (magent-action-step-p step)
    (error "Workflow yielded invalid Step: %S" step))
  (let* ((generation (1+ (magent-action-invocation-generation invocation)))
         (item-id (magent-action--workflow-step-start invocation step))
         (starter-returned-p nil)
         pending
         cancel)
    (setf (magent-action-invocation-generation invocation) generation
          (magent-action-invocation-current-step invocation) step
          (magent-action-invocation-current-step-id invocation) item-id)
    (cl-labels
        ((done
          (status value)
          (unless (memq status '(completed failed cancelled))
            (let ((invalid status))
              (setq status 'failed
                    value
                    (format "Invalid callback Step status: %S" invalid))))
          (if starter-returned-p
              (magent-action--workflow-finish-step
               invocation generation status value)
            (unless pending
              (setq pending (cons status value))))))
      (condition-case err
          (setq cancel
                (pcase (magent-action-step-type step)
                  ('process
                   (magent-action--workflow-start-process step #'done))
                  ((or 'agent 'answer)
                   (magent-action--start-agent-step invocation step #'done))
                  ('callback
                   (funcall (magent-action--step-option step :start) #'done))
                  (_ (error "Unknown Action Step type: %S"
                            (magent-action-step-type step)))))
        (error
         (setq pending (cons 'failed err))))
      (unless (or (null cancel) (functionp cancel))
        (setq pending
              (cons 'failed
                    (format "Step %s returned invalid cancel function: %S"
                            (magent-action-step-name step) cancel))
              cancel nil))
      (setq starter-returned-p t)
      (when (magent-action--workflow-active-p invocation generation)
        (setf (magent-action-invocation-step-cancel-function invocation)
              cancel))
      (when pending
        (magent-action--workflow-finish-step
         invocation generation (car pending) (cdr pending))))))

(defun magent-action--workflow-resume
    (invocation &optional outcome initial-p)
  "Resume INVOCATION iterator with OUTCOME, or start it when INITIAL-P."
  (when (eq (magent-action-invocation-status invocation) 'active)
    (condition-case condition
        (let ((step
               (if initial-p
                   (iter-next
                    (magent-action-invocation-iterator invocation))
                 (iter-next
                  (magent-action-invocation-iterator invocation) outcome))))
          (magent-action--workflow-start-step invocation step))
      (iter-end-of-sequence
       (let ((result (cdr condition)))
         (setf (magent-action-invocation-iterator invocation) nil)
         (if (or (null result) (stringp result))
             (magent-action--finish-completed invocation result)
           (magent-action--finish-failed
            invocation
            (format "Workflow returned invalid result: %S" result)))))
      (quit
       (magent-action--finish-cancelled invocation "Action cancelled"))
      (error
       (magent-action--finish-failed invocation condition)))))

(defun magent-action--start-workflow (invocation workflow)
  "Start WORKFLOW for INVOCATION and return INVOCATION."
  (unless (functionp workflow)
    (error "Expected Action Workflow function, got: %S" workflow))
  (let ((iterator (funcall workflow invocation)))
    (unless (functionp iterator)
      (error "Action Workflow did not return an iterator: %S" iterator))
    (setf (magent-action-invocation-iterator invocation) iterator
          (magent-action-invocation-generation invocation) 0)
    (magent-action--workflow-resume invocation nil t))
  invocation)

(defun magent-action--cleanup-workflow (invocation reason)
  "Cancel INVOCATION's current Step and close its iterator for REASON."
  (when-let* ((step (magent-action-invocation-current-step invocation)))
    (magent-action--workflow-step-finish
     invocation step
     (magent-action-invocation-current-step-id invocation)
     'cancelled reason))
  (setf (magent-action-invocation-current-step invocation) nil
        (magent-action-invocation-current-step-id invocation) nil)
  (when-let* ((cancel
              (magent-action-invocation-step-cancel-function invocation)))
    (setf (magent-action-invocation-step-cancel-function invocation) nil)
    (condition-case err
        (funcall cancel)
      (error
       (magent-log "ERROR Action Step cancellation failed: %s"
                   (error-message-string err)))))
  (magent-action--workflow-close-iterator invocation)
  invocation)


(cl-defstruct (magent-action-spec
               (:constructor magent-action-spec-create)
               (:copier nil))
  "One registered Magent Action definition."
  name
  description
  title
  exposure
  session-policy
  workflow
  source-layer
  source-scope
  requires
  registration-id
  sequence)

(defvar magent-action--registry nil
  "Layered list of registered `magent-action-spec' objects.")

(defvar magent-action--sequence 0
  "Monotonic sequence used to resolve same-layer registrations.")

(defvar magent-action--active-invocations (make-hash-table :test #'eq)
  "Active invocations keyed by their frontend control session identity.")

(defvar magent-action-registry-changed-hook nil
  "Hook run after the effective Action registry may have changed.")

(defvar magent-action--suppress-registry-hooks nil
  "Non-nil while applying one atomic Action registry refresh.")

(defvar magent-action--allow-core-registration nil
  "Non-nil only while Magent registers reserved core Actions.")

(defun magent-action--registry-changed ()
  "Notify Action registry consumers unless notifications are suppressed."
  (unless magent-action--suppress-registry-hooks
    (run-hooks 'magent-action-registry-changed-hook)))

(defconst magent-action--layer-ranks
  '((builtin . 10)
    (package . 20)
    (user . 30)
    (project . 40)
    (core . 50))
  "Precedence ranks for Action definition layers.")

(defconst magent-action--exposures '(slash interactive)
  "Supported public Action exposure kinds.")

(defconst magent-action--session-policies '(current isolated)
  "Supported Action execution session policies.")

(defun magent-action--normalize-name (name)
  "Return NAME as a validated Action string."
  (let ((value (if (symbolp name) (symbol-name name) name)))
    (unless (and (stringp value)
                 (string-match-p "\\`[[:alnum:]_-]+\\'" value))
      (error "Invalid Magent Action name: %S" name))
    value))

(defun magent-action--layer-rank (layer)
  "Return numeric precedence for Action LAYER."
  (or (alist-get layer magent-action--layer-ranks)
      (error "Invalid Magent Action source layer: %S" layer)))

(defun magent-action--resolution-scope (&optional scope)
  "Return canonical Action resolution scope for optional SCOPE.
When SCOPE is nil, use the currently active project overlay."
  (magent-session-canonical-scope
   (or scope
       (and (fboundp 'magent-runtime-active-project-scope)
            (magent-runtime-active-project-scope))
       'global)))

(defun magent-action--visible-in-scope-p (spec scope)
  "Return non-nil when Action SPEC is visible in canonical SCOPE."
  (let ((source-scope
         (magent-session-canonical-scope
          (magent-action-spec-source-scope spec))))
    (or (null source-scope)
        (equal source-scope scope))))

(defun magent-action--same-slot-p (left right)
  "Return non-nil when Action specs LEFT and RIGHT share a registry slot."
  (and (equal (magent-action-spec-name left)
              (magent-action-spec-name right))
       (eq (magent-action-spec-source-layer left)
           (magent-action-spec-source-layer right))
       (equal (magent-action-spec-source-scope left)
              (magent-action-spec-source-scope right))))

(defun magent-action--normalize-exposure (exposure)
  "Return validated Action EXPOSURE with duplicates removed."
  (let ((value (or exposure '(slash))))
    (unless (and (proper-list-p value) value)
      (error "Magent Action exposure must be a non-empty list: %S" exposure))
    (dolist (kind value)
      (unless (memq kind magent-action--exposures)
        (error "Invalid Magent Action exposure: %S" kind)))
    (delete-dups (copy-sequence value))))

(cl-defun magent-action-register
    (name &key description title exposure session-policy workflow
          (source-layer 'package) source-scope requires)
  "Register Magent Action NAME and return its registration token.

WORKFLOW must be a generator Workflow function receiving one
`magent-action-invocation'.  SOURCE-LAYER is one of `builtin', `package',
`user', `project', or reserved `core'.  NAME, SOURCE-LAYER, and SOURCE-SCOPE
identify one replaceable registration slot.  REQUIRES is a feature symbol or
list of feature symbols loaded with `require' before the Workflow or an
isolated session starts.  EXPOSURE is a non-empty list containing `slash',
`interactive', or both.  SESSION-POLICY must be explicitly `current' or
`isolated'."
  (unless (functionp workflow)
    (error "Magent Action %S requires a :workflow function" name))
  (unless (memq session-policy magent-action--session-policies)
    (error "Invalid Magent Action session policy: %S" session-policy))
  (let* ((normalized-requires
          (cond
           ((null requires) nil)
           ((symbolp requires) (list requires))
           ((and (proper-list-p requires) (cl-every #'symbolp requires))
            (delete-dups (copy-sequence requires)))
           (t (error "Expected :requires feature symbol or list, got: %S"
                     requires))))
         (key (magent-action--normalize-name name))
         (layer (or source-layer 'package))
         (_rank (magent-action--layer-rank layer))
         (registration-scope
          (magent-session-canonical-scope source-scope))
         (spec (magent-action-spec-create
                :name key
                :description description
                :title (or title (and (stringp description) description) key)
                :exposure (magent-action--normalize-exposure exposure)
                :session-policy session-policy
                :workflow workflow
                :source-layer layer
                :source-scope registration-scope
                :requires normalized-requires
                :registration-id (magent-protocol-generate-id "action")
                :sequence (cl-incf magent-action--sequence))))
    (when (and (eq layer 'core)
               (not magent-action--allow-core-registration))
      (error "The Magent Action core layer is reserved"))
    (setq magent-action--registry
          (cl-remove-if (lambda (candidate)
                          (magent-action--same-slot-p candidate spec))
                        magent-action--registry))
    (push spec magent-action--registry)
    (magent-action--registry-changed)
    spec))

(defun magent-action-unregister (registration)
  "Unregister exact Action REGISTRATION and return non-nil when removed."
  (unless (magent-action-spec-p registration)
    (error "Expected a Magent Action registration, got: %S" registration))
  (when (and (eq (magent-action-spec-source-layer registration) 'core)
             (not magent-action--allow-core-registration))
    (error "The Magent Action core layer is reserved"))
  (let ((before (length magent-action--registry)))
    (setq magent-action--registry
          (delq registration magent-action--registry))
    (when (/= before (length magent-action--registry))
      (magent-action--registry-changed)
      t)))

(defun magent-action-remove-source (source-layer &optional source-scope)
  "Remove registrations matching SOURCE-LAYER and SOURCE-SCOPE.
Nil SOURCE-SCOPE acts as a wildcard.  Return the removal count."
  (when (and (eq source-layer 'core)
             (not magent-action--allow-core-registration))
    (error "The Magent Action core layer is reserved"))
  (let ((removal-scope
         (and source-scope
              (magent-session-canonical-scope source-scope)))
        (before (length magent-action--registry)))
    (setq magent-action--registry
          (cl-remove-if
           (lambda (spec)
             (and (eq source-layer (magent-action-spec-source-layer spec))
                  (or (null source-scope)
                      (equal removal-scope
                             (magent-action-spec-source-scope spec)))))
           magent-action--registry))
    (let ((removed (- before (length magent-action--registry))))
      (when (> removed 0)
        (magent-action--registry-changed))
      removed)))

(defun magent-action--better-spec-p (left right)
  "Return non-nil when LEFT has precedence over RIGHT."
  (let ((left-rank
         (magent-action--layer-rank
          (magent-action-spec-source-layer left)))
        (right-rank
         (magent-action--layer-rank
          (magent-action-spec-source-layer right))))
    (if (/= left-rank right-rank)
        (> left-rank right-rank)
      (> (magent-action-spec-sequence left)
         (magent-action-spec-sequence right)))))

(defun magent-action-get (name &optional scope exposure)
  "Return effective Action NAME for SCOPE and EXPOSURE, or nil.
EXPOSURE defaults to `slash'.  When SCOPE is nil, resolve against the
currently active project overlay."
  (let ((key (magent-action--normalize-name name))
        (resolution-scope (magent-action--resolution-scope scope))
        (kind (or exposure 'slash))
        winner)
    (unless (memq kind magent-action--exposures)
      (error "Invalid Magent Action exposure: %S" kind))
    (dolist (spec magent-action--registry winner)
      (when (and (equal key (magent-action-spec-name spec))
                 (memq kind (magent-action-spec-exposure spec))
                 (magent-action--visible-in-scope-p spec resolution-scope)
                 (or (null winner)
                     (magent-action--better-spec-p spec winner)))
        (setq winner spec)))))

(defun magent-action-list (&optional scope exposure)
  "Return effective Action specs for SCOPE and EXPOSURE sorted by name.
EXPOSURE defaults to `slash'.  When SCOPE is nil, resolve against the
currently active project overlay."
  (let ((resolution-scope (magent-action--resolution-scope scope))
        (kind (or exposure 'slash))
        names)
    (unless (memq kind magent-action--exposures)
      (error "Invalid Magent Action exposure: %S" kind))
    (dolist (spec magent-action--registry)
      (when (and (memq kind (magent-action-spec-exposure spec))
                 (magent-action--visible-in-scope-p spec resolution-scope))
        (cl-pushnew (magent-action-spec-name spec) names :test #'equal)))
    (mapcar (lambda (name)
              (magent-action-get name (or resolution-scope 'global) kind))
            (sort names #'string<))))

(defun magent-action-load-project-scope (scope)
  "Load Action definitions for project SCOPE.
Project Action files are not currently supported."
  (ignore scope))

(defun magent-action-remove-project-scope (scope)
  "Remove Action definitions for project SCOPE.
Project Action files are not currently supported."
  (ignore scope))

(defun magent-action-initialize-static ()
  "Register bundled actions."
  (require 'magent-action-builtins)
  (magent-action-builtins-register))

(defun magent-action-parse (input &optional scope)
  "Parse slash command INPUT for SCOPE and return (SPEC . ARGUMENT), or nil."
  (let ((trimmed (string-trim (or input ""))))
    (when (string-match "\\`/\\([[:alnum:]_-]+\\)\\(?:[[:space:]]+\\(.*\\)\\)?\\'"
                        trimmed)
      (when-let* ((spec (magent-action-get (match-string 1 trimmed) scope 'slash)))
        (cons spec (string-trim (or (match-string 2 trimmed) "")))))))

(defun magent-action--notify (invocation type &rest props)
  "Send invocation event TYPE with PROPS to INVOCATION's observer."
  (when-let* ((observer (magent-action-invocation-observer invocation)))
    (condition-case err
        (funcall observer
                 (append (list :type type
                               :action
                               (magent-action-spec-name
                                (magent-action-invocation-spec invocation))
                               :invocation-id
                               (magent-action-invocation-id invocation))
                         props))
      (error
       (magent-log "ERROR Action observer failed: %s"
                   (error-message-string err))))))

(defun magent-action-progress (invocation message)
  "Report progress MESSAGE for active Action INVOCATION."
  (unless (eq (magent-action-invocation-status invocation) 'active)
    (error "Magent Action invocation is no longer active"))
  (magent-action--notify invocation 'action-progress :text message)
  invocation)

(defun magent-action--record-message
    (invocation role content &optional phase metadata)
  "Record ROLE message CONTENT in INVOCATION's Action session."
  (unless (magent-action-invocation-session invocation)
    (error "Magent Action invocation has no session"))
  (require 'magent-action-session)
  (magent-action-session-record-message
   invocation role content phase metadata))

(defun magent-action--respond (invocation content &optional metadata)
  "Record assistant CONTENT and send it to INVOCATION's frontend observer."
  (unless (eq (magent-action-invocation-status invocation) 'active)
    (error "Magent Action invocation is no longer active"))
  (when (magent-action-invocation-session invocation)
    (magent-action--record-message
     invocation 'assistant content nil
     (append metadata (list :source 'magent-action-final))))
  (setf (magent-action-invocation-response-recorded-p invocation) t)
  (magent-action--notify invocation 'assistant-delta :text content)
  invocation)

(defun magent-action--claim-finish (invocation status result)
  "Claim terminal STATUS and RESULT for active INVOCATION."
  (unless (memq status '(completed failed cancelled))
    (error "Invalid Magent Action completion status: %S" status))
  (unless (magent-execution-result-p result)
    (signal 'wrong-type-argument (list 'magent-execution-result-p result)))
  (unless (eq status (magent-execution-result-status result))
    (error "Action status %S disagrees with result status %S"
           status (magent-execution-result-status result)))
  (when (eq (magent-action-invocation-status invocation) 'active)
    (setf (magent-action-invocation-status invocation) status
          (magent-action-invocation-result invocation) result)
    t))

(defun magent-action--publish-finish (invocation)
  "Publish the terminal state already claimed by INVOCATION."
  (let ((status (magent-action-invocation-status invocation))
        (result (magent-action-invocation-result invocation))
        (control-session
         (magent-action-invocation-control-session invocation))
        fallback-response
        finalization-error)
    (when (eq (magent-action-spec-session-policy
               (magent-action-invocation-spec invocation))
              'isolated)
      (condition-case err
          (progn
            (require 'magent-action-session)
            (setq fallback-response
                  (magent-action-session-finalize invocation status result)))
        (error
         (when (fboundp 'magent-action-session-untrack)
           (magent-action-session-untrack invocation))
         (setq finalization-error err))))
    (unless (eq (magent-action-spec-session-policy
                 (magent-action-invocation-spec invocation))
                'isolated)
      (condition-case err
          (progn
            (require 'magent-action-session)
            (magent-action-session-finalize-workflow-turn
             invocation status result))
        (error
         (setq finalization-error err))))
    (when finalization-error
      (let ((original-status status)
            (message
             (format "Action finalization failed: %s"
                     (error-message-string finalization-error))))
        (setq status 'failed
              result
              (magent-execution-result-failed
               message
               (list :status 'finalization-error
                     :original-status original-status))
              fallback-response nil)
        (setf (magent-action-invocation-status invocation) status
              (magent-action-invocation-result invocation) result)
        (magent-log "ERROR %s" message)))
    ;; A stale callback must not clear a newer invocation installed for the
    ;; same control session after live reload or extension-managed recovery.
    (when (and control-session
               (eq (gethash control-session magent-action--active-invocations)
                   invocation))
      (remhash control-session magent-action--active-invocations))
    (when fallback-response
      (magent-action--notify invocation 'assistant-delta
                              :text fallback-response))
    (magent-action--notify invocation 'action-completed
                            :status status :result result)
    (when-let* ((completion
                 (magent-action-invocation-completion-function invocation)))
      (condition-case err
          (funcall completion status result)
        (error
         (magent-log "ERROR Action completion callback failed: %s"
                     (error-message-string err)))))
    t))

(defun magent-action--failure-result (error)
  "Return normalized Action failure result for ERROR."
  (if (magent-execution-result-p error)
      error
    (magent-execution-result-failed
     (if (and (consp error) (symbolp (car error)))
         (error-message-string error)
       error)
     (list :status 'action-error))))

(defun magent-action--finish-completed (invocation value)
  "Complete INVOCATION with Workflow return VALUE."
  (let ((content (or value "")))
    (when (and (stringp value) (not (string-empty-p value)))
      (magent-action--respond invocation value))
    (when (magent-action--claim-finish
           invocation 'completed (magent-execution-result-completed content))
      (magent-action--publish-finish invocation))))

(defun magent-action--finish-answer (invocation result)
  "Finish terminal Answer Step for INVOCATION with agent RESULT."
  (unless (magent-execution-result-p result)
    (setq result (magent-execution-result-completed (format "%s" result))))
  (when (magent-action--claim-finish
         invocation (magent-execution-result-status result) result)
    (magent-action--publish-finish invocation)))

(defun magent-action--finish-failed (invocation error)
  "Fail INVOCATION once with ERROR and cancel remaining Workflow state."
  (let ((result (magent-action--failure-result error)))
    (when (magent-action--claim-finish invocation 'failed result)
      (magent-action--cleanup-workflow invocation result)
      (magent-action--publish-finish invocation))))

(defun magent-action--finish-cancelled (invocation reason)
  "Cancel INVOCATION once with REASON."
  (let ((result
         (if (and (magent-execution-result-p reason)
                  (eq (magent-execution-result-status reason) 'cancelled))
             reason
           (magent-execution-result-cancelled
            (or reason "Action cancelled") (list :reason 'cancelled)))))
    (when (magent-action--claim-finish invocation 'cancelled result)
      (magent-action--cleanup-workflow invocation result)
      (magent-action--publish-finish invocation))))

(defun magent-action--workflow-step-start (invocation step)
  "Record STEP start for INVOCATION and return its ledger item id."
  (require 'magent-action-session)
  (magent-action-session-start-step invocation step))

(defun magent-action--workflow-step-finish
    (invocation step item-id status value)
  "Record STEP terminal STATUS and VALUE for INVOCATION ITEM-ID."
  (require 'magent-action-session)
  (magent-action-session-finish-step
   invocation step item-id status value))

;;;###autoload
(defun magent-action-cancel (&optional invocation-or-session-id reason)
  "Cancel an active Action invocation or isolated Action session.
When called interactively, prompt for an active cancellable Action session.
REASON may be a string or a `magent-execution-result' for direct invocations."
  (interactive)
  (cond
   ((magent-action-invocation-p invocation-or-session-id)
    (magent-action--finish-cancelled invocation-or-session-id reason))
   (t
    (require 'magent-action-session)
    (let ((session-id (or invocation-or-session-id
                          (magent-action-session-read-active-id))))
      (magent-action-session-cancel session-id)))))

(defun magent-action-cancel-session (runtime-session)
  "Cancel the active Action for RUNTIME-SESSION, if any."
  (when-let* ((invocation
              (gethash runtime-session magent-action--active-invocations)))
    (magent-action-cancel invocation)))

(defun magent-action--plist-p (value)
  "Return non-nil when VALUE is a proper keyword plist or nil."
  (and (proper-list-p value)
       (zerop (% (length value) 2))
       (cl-loop for (key _item) on value by #'cddr
                always (keywordp key))))

(defun magent-action--validate-invocation (invocation)
  "Validate request data carried by INVOCATION."
  (unless (magent-action--plist-p
           (magent-action-invocation-request-context invocation))
    (error "Expected Magent Action invocation request context plist, got: %S"
           (magent-action-invocation-request-context invocation)))
  (unless (magent-action--plist-p
           (magent-action-invocation-options invocation))
    (error "Expected Magent Action invocation options plist, got: %S"
           (magent-action-invocation-options invocation))))

(defun magent-action--load-requirements (spec)
  "Load Elisp feature requirements declared by Action SPEC."
  (dolist (feature (magent-action-spec-requires spec))
    (let ((loaded
           (condition-case err
               (require feature nil t)
             (error
              (error "Action /%s failed to require `%s': %s"
                     (magent-action-spec-name spec) feature
                     (error-message-string err))))))
      (unless loaded
        (user-error "Action /%s requires unavailable feature `%s'"
                    (magent-action-spec-name spec) feature)))))

(defun magent-action-turn-metadata (invocation &optional step)
  "Return canonical ledger metadata for Action INVOCATION and STEP."
  (list :source 'magent-action
        :action
        (magent-action-spec-name
         (magent-action-invocation-spec invocation))
        :action-invocation-id
        (magent-action-invocation-id invocation)
        :action-argument
        (magent-action-invocation-argument invocation)
        :action-input
        (magent-action-invocation-raw-input invocation)
        :workflow-step-id
        (and step (magent-action-invocation-current-step-id invocation))
        :workflow-step-name
        (and step (magent-action-step-name step))
        :workflow-step-type
        (and step (magent-action-step-type step))))

(defun magent-action--forward-answer-event (invocation event)
  "Forward terminal Answer EVENT and track visible response state."
  (when (eq (plist-get event :type) 'assistant-delta)
    (setf (magent-action-invocation-response-recorded-p invocation) t))
  (when-let* ((observer (magent-action-invocation-observer invocation)))
    (funcall observer event)))

(defun magent-action--start-agent-step (invocation step done)
  "Start agent STEP for INVOCATION and call DONE on completion."
  (unless (eq (magent-action-invocation-status invocation) 'active)
    (error "Cannot start a Step for a completed Action invocation"))
  (let* ((prompt (magent-action--step-prompt step invocation))
         (agent (magent-action--step-option step :agent))
         (skills (magent-action--step-option step :skills))
         (request-context
          (magent-action--step-option step :request-context))
         (_context-valid
          (unless (magent-action--plist-p request-context)
            (error "Expected Magent Action request context plist, got: %S"
                   request-context)))
         (buffers (magent-action--resolve-step-buffers step invocation))
         (additional-resources
          (append (magent-action--buffer-resource-blocks buffers)
                  (magent-action--step-option step :resource-blocks)
                  nil))
         (frontend-resources
          (append (magent-action-invocation-resource-blocks invocation) nil))
         (default-blocks
          (and (or additional-resources frontend-resources)
               (vconcat
                (cons `((type . "text") (text . ,prompt))
                      (append additional-resources frontend-resources)))))
         (adapter (magent-action-invocation-submission-adapter invocation))
         (adapted
          (if adapter
              (funcall adapter prompt additional-resources)
            (list :prompt prompt :content-blocks default-blocks)))
         (effective-prompt (or (plist-get adapted :prompt) prompt))
         (content-blocks (plist-get adapted :content-blocks))
         (terminal-p (eq (magent-action-step-type step) 'answer))
         (metadata
          (append
           (magent-action-turn-metadata invocation step)
           (unless terminal-p (list :workflow-activity t))
           (and content-blocks (list :content-blocks content-blocks))))
         submission-id)
    (setq submission-id
          (magent-runtime-submit
           (magent-action-invocation-runtime-session invocation)
           effective-prompt
           :skills skills
           :tools (magent-action--step-option step :tools)
           :agent agent
           :context
           (append request-context
                   (magent-action-invocation-request-context invocation))
           :turn-metadata metadata
           :observer
           (and terminal-p
                (lambda (event)
                  (magent-action--forward-answer-event invocation event)))
           :approval-provider
           (magent-action-invocation-approval-provider invocation)
           :on-complete done))
    (when (and (eq (magent-action-invocation-status invocation) 'active)
               (eq step (magent-action-invocation-current-step invocation)))
      (setf (magent-action-invocation-current-submission-id invocation)
            submission-id))
    (lambda ()
      (when (and (magent-action-invocation-runtime-session invocation)
                 (magent-action-invocation-current-submission-id invocation))
        (magent-runtime-cancel-submission
         (magent-action-invocation-runtime-session invocation)
         (magent-action-invocation-current-submission-id invocation))))))

(defun magent-action--normalize-buffer-config (entry)
  "Return normalized popwin-style Action buffer configuration ENTRY."
  (let* ((bare-p (or (bufferp entry)
                     (stringp entry)
                     (symbolp entry)
                     (functionp entry)))
         (pattern (if bare-p entry (car-safe entry)))
         (keywords (if bare-p nil (cdr-safe entry))))
    (unless (or bare-p
                (and (consp entry)
                     (magent-action--plist-p keywords)))
      (error "Invalid Magent Action buffer configuration: %S" entry))
    (unless (or (bufferp pattern)
                (stringp pattern)
                (symbolp pattern)
                (functionp pattern))
      (error "Invalid Magent Action buffer pattern: %S" pattern))
    (cl-loop for (key _value) on keywords by #'cddr
             unless (memq key '(:required-p :regexp :predicate
                                 :project-only-p))
             do (error "Unknown Magent Action buffer keyword: %S" key))
    (dolist (key '(:required-p :regexp :predicate :project-only-p))
      (when (and (plist-member keywords key)
                 (not (memq (plist-get keywords key) '(nil t))))
        (error "Expected Magent Action buffer %S boolean, got: %S"
               key (plist-get keywords key))))
    (when (and (plist-get keywords :regexp)
               (not (stringp pattern)))
      (error "Magent Action :regexp requires a string pattern: %S" pattern))
    (when (and (plist-get keywords :predicate)
               (not (and (symbolp pattern) (fboundp pattern))))
      (error "Magent Action :predicate requires a function symbol: %S"
             pattern))
    (when (and (plist-get keywords :regexp)
               (plist-get keywords :predicate))
      (error "Magent Action buffer pattern cannot be regexp and predicate"))
    (when (plist-get keywords :regexp)
      (condition-case err
          (string-match-p pattern "")
        (invalid-regexp
         (error "Invalid Magent Action buffer regexp %S: %s"
                pattern (error-message-string err)))))
    (let* ((kind
            (cond
             ((bufferp pattern) 'buffer)
             ((plist-get keywords :regexp) 'regexp)
             ((plist-get keywords :predicate) 'predicate)
             ((stringp pattern) 'name)
             ((symbolp pattern) 'mode)
             ((functionp pattern) 'predicate)))
           (selector-p (memq kind '(mode regexp predicate)))
           (required-p
            (if (plist-member keywords :required-p)
                (plist-get keywords :required-p)
              t))
           (project-only-p
            (if (plist-member keywords :project-only-p)
                (plist-get keywords :project-only-p)
              selector-p)))
      (list :pattern pattern
            :kind kind
            :required-p required-p
            :project-only-p project-only-p))))

(defun magent-action--path-in-project-p (path root base-directory)
  "Return non-nil when PATH under BASE-DIRECTORY belongs to ROOT."
  (when (and (stringp path) (stringp root))
    (condition-case nil
        (let ((expanded (expand-file-name path base-directory))
              (project-root (file-name-as-directory root)))
          (or (equal (directory-file-name expanded)
                     (directory-file-name project-root))
              (file-in-directory-p expanded project-root)))
      (error nil))))

(defun magent-action--buffer-in-project-p (buffer root)
  "Return non-nil when BUFFER belongs to project ROOT."
  (and
   (buffer-live-p buffer)
   (with-current-buffer buffer
     (let ((base default-directory))
       (or (magent-action--path-in-project-p buffer-file-name root base)
           (magent-action--path-in-project-p default-directory root base))))))

(defun magent-action--buffer-pattern-match-p (buffer config)
  "Return non-nil when live BUFFER matches normalized CONFIG."
  (let ((pattern (plist-get config :pattern)))
    (pcase (plist-get config :kind)
      ('mode (eq (buffer-local-value 'major-mode buffer) pattern))
      ('regexp (string-match-p pattern (buffer-name buffer)))
      ('predicate (funcall pattern buffer))
      (_ nil))))

(defun magent-action--matching-buffers (config invocation)
  "Return live buffers matching normalized CONFIG for INVOCATION."
  (let* ((runtime-session
          (magent-action-invocation-runtime-session invocation))
         (origin
          (magent-session-scope-origin
           (magent-runtime-session-scope runtime-session)))
         (project-only-p (plist-get config :project-only-p))
         (project-root (and (stringp origin)
                            (magent-session-canonical-scope origin)))
         (kind (plist-get config :kind))
         (pattern (plist-get config :pattern))
         (candidates
          (pcase kind
            ('buffer (and (buffer-live-p pattern) (list pattern)))
            ('name (when-let* ((buffer (get-buffer pattern))) (list buffer)))
            (_ (cl-remove-if-not
                (lambda (buffer)
                  (and (or (not project-only-p)
                           (not project-root)
                           (magent-action--buffer-in-project-p
                            buffer project-root))
                       (magent-action--buffer-pattern-match-p buffer config)))
                (buffer-list))))))
    (if (and project-only-p project-root (memq kind '(buffer name)))
        (cl-remove-if-not
         (lambda (buffer)
           (magent-action--buffer-in-project-p buffer project-root))
         candidates)
      candidates)))

(defun magent-action--resolve-step-buffers (step invocation)
  "Resolve and deduplicate STEP buffer patterns for INVOCATION."
  (let ((entries (magent-action--step-option step :buffers))
        (seen (make-hash-table :test #'eq))
        buffers)
    (unless (or (null entries) (proper-list-p entries))
      (error "Expected Step buffer configuration list, got: %S" entries))
    (dolist (entry entries)
      (let* ((config (magent-action--normalize-buffer-config entry))
             (matches (magent-action--matching-buffers config invocation)))
        (when (null matches)
          (if (plist-get config :required-p)
              (user-error "Action /%s required buffer pattern matched nothing: %S"
                          (magent-action-spec-name
                           (magent-action-invocation-spec invocation))
                          (plist-get config :pattern))
            (magent-log
             "INFO Action /%s optional buffer pattern matched nothing: %S"
             (magent-action-spec-name
              (magent-action-invocation-spec invocation))
             (plist-get config :pattern))))
        (dolist (buffer matches)
          (unless (gethash buffer seen)
            (puthash buffer t seen)
            (push buffer buffers)))))
    (nreverse buffers)))

(defun magent-action--truncate-buffer-content
    (text source-start source-point budget)
  "Return truncation data for TEXT around SOURCE-POINT within BUDGET.
SOURCE-START is the absolute position corresponding to the start of TEXT."
  (let* ((length (length text))
         (keep (if budget (min length budget) length))
         (anchor (max 0 (min length (- source-point source-start))))
         (window-start
          (if (= keep length)
              0
            (max 0 (min (- anchor (/ keep 2)) (- length keep)))))
         (window-end (+ window-start keep)))
    (list :text (substring text window-start window-end)
          :original-length length
          :retained-length keep
          :retained-start (+ source-start window-start)
          :retained-end (+ source-start window-end)
          :omitted-before window-start
          :omitted-after (- length window-end)
          :truncated-p (< keep length))))

(defun magent-action--buffer-resource-block (buffer budget)
  "Return (RESOURCE-BLOCK . RETAINED-CHARS) for BUFFER within BUDGET."
  (with-current-buffer buffer
    (let* ((region-p (use-region-p))
           (accessible-start (point-min))
           (accessible-end (point-max))
           (source-start
            (if region-p
                (max accessible-start (region-beginning))
              accessible-start))
           (source-end
            (if region-p
                (min accessible-end (region-end))
              accessible-end))
           (source-point (point))
           (raw (buffer-substring-no-properties source-start source-end))
           (truncation
            (magent-action--truncate-buffer-content
             raw source-start source-point budget))
           (name (buffer-name buffer))
           (retained-start (plist-get truncation :retained-start))
           (retained-end (plist-get truncation :retained-end))
           (notice
            (and (plist-get truncation :truncated-p)
                 (format "\n[Buffer content truncated: original %d characters; retained bounds %d..%d; omitted %d before and %d after.]\n"
                  (plist-get truncation :original-length)
                  retained-start retained-end
                  (plist-get truncation :omitted-before)
                  (plist-get truncation :omitted-after))))
           (resource-text
            (format "Buffer name: %s\nMajor mode: %s\nFile: %s\nModified: %s\nPoint: %d\nSelection: %s\nSelected bounds: %d..%d\nRetained bounds: %d..%d\nNarrowed: %s\n%s\nContent:\n%s"
             name major-mode (or buffer-file-name "<none>")
             (if (buffer-modified-p) "true" "false") source-point
             (if region-p "active-region" "accessible-buffer")
             source-start source-end retained-start retained-end
             (if (buffer-narrowed-p) "true" "false")
             (or notice "")
             (plist-get truncation :text)))
           (block
            `((type . "resource")
              (resource
               . ((uri . ,(concat "emacs-buffer:///"
                                  (url-hexify-string name)))
                  (name . ,name)
                  (mimeType . "text/plain")
                  (text . ,resource-text))))))
      (cons block (plist-get truncation :retained-length)))))

(defun magent-action--buffer-resource-blocks (buffers)
  "Return model-visible snapshot resource blocks for BUFFERS."
  (unless (or (null magent-action-buffer-context-max-chars)
              (natnump magent-action-buffer-context-max-chars))
    (error "Expected non-negative Action buffer context budget, got: %S"
           magent-action-buffer-context-max-chars))
  (let ((remaining magent-action-buffer-context-max-chars)
        blocks)
    (dolist (buffer buffers)
      (let* ((snapshot (magent-action--buffer-resource-block buffer remaining))
             (used (cdr snapshot)))
        (push (car snapshot) blocks)
        (when remaining
          (setq remaining (max 0 (- remaining used))))))
    (nreverse blocks)))

(defun magent-action--step-prompt (step invocation)
  "Return STEP prompt expanded for INVOCATION."
  (let ((base (magent-action--step-option step :prompt))
        (argument (magent-action-invocation-argument invocation)))
    (when (string-blank-p base)
      (error "Magent Action agent Step prompt is empty"))
    (if (or (not (magent-action--step-option step :append-argument-p))
            (string-empty-p argument))
        base
      (concat
       base "\n\n"
       (magent-prompt-render
        "internal/additional-instruction.org"
        `((instruction . ,argument)))))))

(defun magent-action--execute (invocation)
  "Validate and execute INVOCATION, returning it immediately."
  (let ((control-session
         (magent-action-invocation-control-session invocation)))
    (when (and control-session
               (gethash control-session magent-action--active-invocations))
      (user-error "A Magent Action is already active in this session"))
    (condition-case err
        (progn
          (magent-action--validate-invocation invocation)
          (magent-action--load-requirements
           (magent-action-invocation-spec invocation))
          (when (eq (magent-action-spec-session-policy
                     (magent-action-invocation-spec invocation))
                    'isolated)
            (require 'magent-action-session)
            (magent-action-session-initialize invocation))
          (when control-session
            (puthash control-session invocation
                     magent-action--active-invocations))
          (magent-action--start-workflow
           invocation
           (magent-action-spec-workflow
            (magent-action-invocation-spec invocation))))
      (quit
       (magent-action--finish-cancelled invocation "Action cancelled"))
      (error
       (magent-action--finish-failed invocation err)))
    invocation))

(cl-defun magent-action--make-invocation
    (spec &key control-session origin-buffer origin-directory origin-scope
          parent-session raw-input argument options request-context
          resource-blocks observer approval-provider on-complete
          submission-adapter interactive-p)
  "Create a fully described invocation for SPEC."
  (let* ((current-p (eq (magent-action-spec-session-policy spec) 'current))
         (runtime-session (and current-p control-session))
         (session (and runtime-session
                       (magent-runtime-session-magent-session runtime-session)))
         (scope (and runtime-session
                     (magent-runtime-session-scope runtime-session))))
    (unless (or (not current-p) runtime-session)
      (error "Current-session Action %s requires a runtime session"
             (magent-action-spec-name spec)))
    (magent-action-invocation-create
     :id (magent-protocol-generate-id "invocation")
     :spec spec
     :control-session control-session
     :runtime-session runtime-session
     :session session
     :scope scope
     :origin-buffer origin-buffer
     :origin-directory origin-directory
     :origin-scope origin-scope
     :parent-session parent-session
     :parent-scope origin-scope
     :parent-session-id (and parent-session
                             (magent-session-get-id parent-session))
     :options options
     :interactive-p interactive-p
     :raw-input (or raw-input (concat "/" (magent-action-spec-name spec)))
     :argument (string-trim (or argument ""))
     :request-context request-context
     :resource-blocks resource-blocks
     :observer observer
     :approval-provider approval-provider
     :completion-function on-complete
     :submission-adapter submission-adapter)))

(cl-defun magent-action-invoke
    (action runtime-session &key raw-input argument request-context
             resource-blocks observer approval-provider on-complete
             submission-adapter)
  "Invoke slash-exposed ACTION for RUNTIME-SESSION."
  (let* ((scope (magent-runtime-session-scope runtime-session))
         (spec
          (if (magent-action-spec-p action)
              (let ((effective
                     (magent-action-get
                      (magent-action-spec-name action) scope 'slash)))
                (unless (eq action effective)
                  (error "Magent action /%s is unavailable in session scope"
                         (magent-action-spec-name action)))
                action)
            (or (magent-action-get action scope 'slash)
                (error "Unknown Magent slash command: %s" action))))
         (origin-scope (magent-session-scope-origin scope))
         (parent-session
          (magent-runtime-session-magent-session runtime-session)))
    (magent-action--execute
     (magent-action--make-invocation
      spec
      :control-session runtime-session
      :origin-buffer (current-buffer)
      :origin-directory default-directory
      :origin-scope origin-scope
      :parent-session parent-session
      :raw-input raw-input
      :argument argument
      :request-context request-context
      :resource-blocks resource-blocks
      :observer observer
      :approval-provider approval-provider
      :on-complete on-complete
      :submission-adapter submission-adapter))))

(cl-defun magent-action-run
    (action &key argument options on-complete)
  "Run interactive-exposed ACTION from the current Emacs context."
  (magent-runtime-ensure-initialized)
  (let* ((origin-buffer (current-buffer))
         (origin-directory default-directory)
         (origin-scope (magent-runtime-context-scope))
         (_prepared (magent-runtime-prepare-context origin-scope))
         (spec (or (magent-action-get action origin-scope 'interactive)
                   (error "Unknown interactive Magent action: %s" action)))
         (parent-session (magent-session-get-if-present origin-scope))
         (control-session
          (and (eq (magent-action-spec-session-policy spec) 'current)
               (magent-runtime-session-current origin-scope))))
    (magent-action--execute
     (magent-action--make-invocation
      spec
      :control-session control-session
      :origin-buffer origin-buffer
      :origin-directory origin-directory
      :origin-scope origin-scope
      :parent-session parent-session
      :argument argument
      :options options
      :on-complete on-complete
      :interactive-p t))))

(provide 'magent-action)
;;; magent-action.el ends here
