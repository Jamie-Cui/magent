;;; magent-command-workflow.el --- Sequential command Workflows  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6

;;; Commentary:

;; Generator-backed Workflow DSL for `magent-command'.  Trusted Elisp owns
;; control flow; yielded Steps are the asynchronous waiting, cancellation,
;; progress, and activity-recording boundary.

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'subr-x)
(require 'magent-config)
(require 'magent-log)
(require 'magent-protocol)

(declare-function magent-command--finish-answer "magent-command")
(declare-function magent-command--finish-cancelled "magent-command")
(declare-function magent-command--finish-completed "magent-command")
(declare-function magent-command--finish-failed "magent-command")
(declare-function magent-command--start-agent-step "magent-command")
(declare-function magent-command--workflow-step-finish "magent-command")
(declare-function magent-command--workflow-step-start "magent-command")
(define-error 'magent-command-step-error "Magent command Step failed")
(define-error 'magent-command-process-error "Magent command process Step failed"
  'magent-command-step-error)
(define-error 'magent-command-agent-error "Magent command agent Step failed"
  'magent-command-step-error)
(define-error 'magent-command-callback-error "Magent command callback Step failed"
  'magent-command-step-error)

(cl-defstruct (magent-command-invocation
               (:constructor magent-command-invocation-create)
               (:copier nil))
  "Runtime state for one Magent command Invocation."
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

(cl-defstruct (magent-command-step
               (:constructor magent-command-step-create)
               (:copier nil))
  "One asynchronous boundary yielded by a command Workflow."
  type
  name
  options
  terminal-p)

(cl-defstruct (magent-command-step-outcome
               (:constructor magent-command-step-outcome-create)
               (:copier nil))
  "Value resumed into a Workflow after one Step finishes."
  status
  value
  condition)

(cl-defstruct (magent-command-process-result
               (:constructor magent-command-process-result-create)
               (:copier nil))
  "Complete result from one command process Step."
  name
  command
  directory
  exit-status
  stdout
  stderr
  duration-ms
  timed-out-p)

(defmacro magent-command-defworkflow (name arglist &rest body)
  "Define NAME as a sequential command Workflow accepting ARGLIST."
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-list lambda-doc &rest sexp)))
  `(iter-defun ,name ,arglist ,@body))

(defun magent-command--normalize-step-name (name)
  "Return NAME as a non-empty Step label."
  (unless (and (stringp name) (not (string-empty-p (string-trim name))))
    (error "Magent command Step requires a non-empty name: %S" name))
  name)

(defun magent-command--validate-result-mode (mode)
  "Return validated Step result MODE."
  (unless (memq mode '(value full))
    (error "Invalid Magent command Step result mode: %S" mode))
  mode)

(cl-defun magent-command--make-process-step
    (name command &key directory environment
          (timeout magent-command-process-timeout)
          (check t) (result 'value)
          (record-command t) (record-output t))
  "Create a process Step named NAME for argv COMMAND."
  (magent-command--normalize-step-name name)
  (unless (and (proper-list-p command) command
               (cl-every #'stringp command))
    (error "Magent command process requires non-empty string argv: %S"
           command))
  (unless (or (null timeout) (and (numberp timeout) (>= timeout 0)))
    (error "Invalid Magent command process timeout: %S" timeout))
  (unless (or (null environment)
              (and (proper-list-p environment)
                   (cl-every
                    (lambda (entry)
                      (and (consp entry)
                           (stringp (car entry))
                           (or (null (cdr entry)) (stringp (cdr entry)))))
                    environment)))
    (error "Expected environment alist, got: %S" environment))
  (magent-command--validate-result-mode result)
  (magent-command-step-create
   :type 'process
   :name name
   :options
   (list :command (copy-sequence command)
         :directory (file-name-as-directory
                     (expand-file-name (or directory default-directory)))
         :process-environment (copy-sequence process-environment)
         :environment (copy-tree environment)
         :timeout timeout
         :check (and check t)
         :result result
         :record-command (and record-command t)
         :record-output (and record-output t))))

(defmacro magent-command-process (name command &rest options)
  "Run argv COMMAND as process Step NAME and return its selected result."
  (declare (indent 2))
  `(magent-command--unwrap-step-outcome
    (iter-yield
     (magent-command--make-process-step ,name ,command ,@options))))

(cl-defun magent-command--make-agent-step
    (name prompt &key agent skills buffers append-argument-p required-tools
          (result 'value) request-context resource-blocks terminal-p)
  "Create an agent or terminal Answer Step named NAME for PROMPT."
  (magent-command--normalize-step-name name)
  (unless (stringp prompt)
    (error "Magent command agent Step requires a string prompt: %S" prompt))
  (unless (or (null agent) (symbolp agent) (stringp agent))
    (error "Expected Magent command agent name, got: %S" agent))
  (unless (or (null skills) (proper-list-p skills))
    (error "Expected Magent command skill list, got: %S" skills))
  (unless (or (null buffers) (proper-list-p buffers))
    (error "Expected Magent command buffer configuration list, got: %S"
           buffers))
  (unless (memq append-argument-p '(nil t))
    (error "Expected append-argument-p boolean, got: %S"
           append-argument-p))
  (unless (or (null required-tools) (proper-list-p required-tools))
    (error "Expected required tool list, got: %S" required-tools))
  (magent-command--validate-result-mode result)
  (magent-command-step-create
   :type (if terminal-p 'answer 'agent)
   :name name
   :terminal-p terminal-p
   :options
   (list :prompt prompt
         :agent agent
         :skills skills
         :buffers buffers
         :append-argument-p (and append-argument-p t)
         :required-tools required-tools
         :result (if terminal-p 'full result)
         :request-context request-context
         :resource-blocks resource-blocks)))

(defmacro magent-command-agent (name prompt &rest options)
  "Run PROMPT as intermediate agent Step NAME and return its result."
  (declare (indent 2))
  `(magent-command--unwrap-step-outcome
    (iter-yield
     (magent-command--make-agent-step ,name ,prompt ,@options))))

(defmacro magent-command-answer (name prompt &rest options)
  "Run PROMPT as terminal Answer Step NAME.
The Invocation ends when the Step finishes; forms after this call do not run."
  (declare (indent 2))
  `(iter-yield
    (magent-command--make-agent-step
     ,name ,prompt ,@options :terminal-p t)))

(cl-defun magent-command--make-callback-step
    (name start &key activity-input activity-formatter)
  "Create callback Step NAME using START."
  (magent-command--normalize-step-name name)
  (unless (functionp start)
    (error "Magent command callback Step requires START function: %S" start))
  (unless (or (null activity-formatter) (functionp activity-formatter))
    (error "Expected callback activity formatter, got: %S"
           activity-formatter))
  (magent-command-step-create
   :type 'callback
   :name name
   :options (list :start start
                  :activity-input activity-input
                  :activity-formatter activity-formatter)))

(defmacro magent-command-callback (name start &rest options)
  "Wait for callback Step NAME started by START and return its value.
START receives one DONE function, called as (DONE STATUS VALUE), and returns
an optional zero-argument cancellation function."
  (declare (indent 2))
  `(magent-command--unwrap-step-outcome
    (iter-yield
     (magent-command--make-callback-step ,name ,start ,@options))))

(defun magent-command--unwrap-step-outcome (outcome)
  "Return completed OUTCOME value or signal its typed failure."
  (unless (magent-command-step-outcome-p outcome)
    (error "Workflow resumed with invalid Step outcome: %S" outcome))
  (pcase (magent-command-step-outcome-status outcome)
    ('completed (magent-command-step-outcome-value outcome))
    ('failed
     (let ((condition (magent-command-step-outcome-condition outcome)))
       (if (and (consp condition) (symbolp (car condition)))
           (signal (car condition) (cdr condition))
         (signal 'magent-command-step-error
                 (list (format "%s" condition))))))
    (_
     (error "Workflow received non-resumable Step status: %S"
            (magent-command-step-outcome-status outcome)))))

(defun magent-command--step-option (step key)
  "Return STEP option KEY."
  (plist-get (magent-command-step-options step) key))

(defun magent-command-workflow-step-activity-input (step)
  "Return safe default activity input for STEP."
  (pcase (magent-command-step-type step)
    ('process
     (append
      (when (magent-command--step-option step :record-command)
        (list :command (magent-command--step-option step :command)))
      (list :directory (magent-command--step-option step :directory))
      (when-let* ((environment
                   (magent-command--step-option step :environment)))
        (list :environment-keys (mapcar #'car environment)))))
    ('callback (magent-command--step-option step :activity-input))
    ((or 'agent 'answer)
     (list :agent (magent-command--step-option step :agent)
           :terminal (eq (magent-command-step-type step) 'answer)))
    (_ nil)))

(defun magent-command-workflow--bounded-output (text)
  "Return TEXT bounded for durable Step activity."
  (let ((value (or text ""))
        (limit magent-command-step-output-max-chars))
    (if (or (null limit) (<= (length value) limit))
        value
      (concat
       (format "[... truncated %d leading characters ...]\n"
               (- (length value) limit))
       (substring value (- (length value) limit))))))

(defun magent-command-workflow-step-activity-output (step status value)
  "Return durable activity output for STEP STATUS and VALUE."
  (pcase (magent-command-step-type step)
    ('process
     (when (and (magent-command--step-option step :record-output)
                (magent-command-process-result-p value))
       (magent-command-workflow--bounded-output
        (string-join
         (delq nil
               (list
                (and (not (string-empty-p
                           (magent-command-process-result-stdout value)))
                     (concat "stdout:\n"
                             (magent-command-process-result-stdout value)))
                (and (not (string-empty-p
                           (magent-command-process-result-stderr value)))
                     (concat "stderr:\n"
                             (magent-command-process-result-stderr value)))))
         "\n"))))
    ('callback
     (when-let* ((formatter
                  (magent-command--step-option step :activity-formatter)))
       (condition-case err
           (let ((formatted (funcall formatter status value)))
             (and formatted
                  (magent-command-workflow--bounded-output
                   (if (stringp formatted)
                       formatted
                     (format "%s" formatted)))))
         (error
          (magent-log "WARN command callback activity formatter failed: %s"
                      (error-message-string err))
          nil))))
    (_ nil)))

(defun magent-command-workflow--process-environment (step)
  "Return process environment captured by STEP with overrides applied."
  (let ((process-environment
         (copy-sequence
          (magent-command--step-option step :process-environment))))
    (dolist (entry (magent-command--step-option step :environment))
      (setenv (car entry) (cdr entry)))
    process-environment))

(defun magent-command-workflow--buffer-text (buffer)
  "Return BUFFER text, or an empty string if BUFFER is dead."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun magent-command-workflow--start-process (step done)
  "Start process STEP and call DONE once.  Return its cancel function."
  (let* ((stdout (generate-new-buffer " *magent-command-stdout*"))
         (stderr (generate-new-buffer " *magent-command-stderr*"))
         (command (magent-command--step-option step :command))
         (directory (magent-command--step-option step :directory))
         (timeout (magent-command--step-option step :timeout))
         (check (magent-command--step-option step :check))
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
                    (magent-command-process-result-create
                     :name (magent-command-step-name step)
                     :command command
                     :directory directory
                     :exit-status exit-status
                     :stdout (magent-command-workflow--buffer-text stdout)
                     :stderr (magent-command-workflow--buffer-text stderr)
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
                 (magent-command-workflow--process-environment step)))
            (setq stderr-process
                  (make-pipe-process
                   :name "magent-command-process-stderr"
                   :buffer stderr
                   :coding 'utf-8-unix
                   :noquery t
                   :sentinel #'ignore))
            (setq process
                  (make-process
                   :name "magent-command-process"
                   :buffer stdout
                   :stderr stderr-process
                   :command command
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

(defun magent-command-workflow--step-condition (step value)
  "Return typed condition data for failed STEP VALUE."
  (pcase (magent-command-step-type step)
    ('process
     (if (magent-command-process-result-p value)
         (let* ((result value)
                (detail
                 (if (magent-command-process-result-timed-out-p result)
                     "timed out"
                   (format
                    "exited with status %s"
                    (magent-command-process-result-exit-status result))))
                (output
                 (string-trim
                  (concat (magent-command-process-result-stderr result)
                          "\n"
                          (magent-command-process-result-stdout result)))))
           (list 'magent-command-process-error
                 (format "%s %s%s"
                         (magent-command-step-name step)
                         detail
                         (if (string-empty-p output)
                             ""
                           (concat ": " output)))
                 result))
       (list 'magent-command-process-error
             (if (and (consp value) (symbolp (car value))
                      (get (car value) 'error-conditions))
                 (error-message-string value)
               (format "%s" value))
             value)))
    ((or 'agent 'answer)
     (list 'magent-command-agent-error
           (cond
            ((magent-agent-result-p value)
             (magent-agent-result-content-string value))
            ((and (consp value) (symbolp (car value))
                  (get (car value) 'error-conditions))
             (error-message-string value))
            (t (format "%s" value)))
           value))
    ('callback
     (list 'magent-command-callback-error
           (if (and (consp value) (symbolp (car value))
                    (get (car value) 'error-conditions))
               (error-message-string value)
             (format "%s" value))
           value))
    (_ (list 'magent-command-step-error (format "%s" value) value))))

(defun magent-command-workflow--selected-value (step value)
  "Return convenience or full VALUE selected by STEP."
  (if (eq (magent-command--step-option step :result) 'full)
      value
    (pcase (magent-command-step-type step)
      ('process
       (if (magent-command-process-result-p value)
           (magent-command-process-result-stdout value)
         value))
      ('agent
       (if (magent-agent-result-p value)
           (magent-agent-result-content-string value)
         value))
      (_ value))))

(defun magent-command-workflow--active-p (invocation generation)
  "Return non-nil when INVOCATION still owns GENERATION."
  (and (eq (magent-command-invocation-status invocation) 'active)
       (= generation (magent-command-invocation-generation invocation))))

(defun magent-command-workflow--close-iterator (invocation)
  "Close INVOCATION iterator, logging cleanup errors."
  (when-let* ((iterator (magent-command-invocation-iterator invocation)))
    (setf (magent-command-invocation-iterator invocation) nil)
    (condition-case err
        (iter-close iterator)
      (error
       (magent-log "ERROR command Workflow iterator cleanup failed: %s"
                   (error-message-string err))))))

(defun magent-command-workflow--finish-step
    (invocation generation status value)
  "Finish INVOCATION Step GENERATION with STATUS and VALUE."
  (when (magent-command-workflow--active-p invocation generation)
    (let ((step (magent-command-invocation-current-step invocation))
          (item-id (magent-command-invocation-current-step-id invocation)))
      (setf (magent-command-invocation-current-step invocation) nil
            (magent-command-invocation-current-step-id invocation) nil
            (magent-command-invocation-step-cancel-function invocation) nil)
      (magent-command--workflow-step-finish
       invocation step item-id status value)
      (if (magent-command-step-terminal-p step)
          (progn
            (magent-command-workflow--close-iterator invocation)
            (pcase status
              ('completed (magent-command--finish-answer invocation value))
              ('cancelled
               (magent-command--finish-cancelled invocation value))
              (_ (magent-command--finish-failed
                  invocation
                  (magent-command-workflow--step-condition step value)))))
        (pcase status
          ('cancelled
           (magent-command--finish-cancelled invocation value))
          ((or 'completed 'failed)
           (magent-command-workflow--resume
            invocation
            (magent-command-step-outcome-create
             :status status
             :value (if (eq status 'completed)
                        (magent-command-workflow--selected-value step value)
                      value)
             :condition (and (eq status 'failed)
                             (magent-command-workflow--step-condition
                              step value)))))
          (_
           (magent-command--finish-failed
            invocation (format "Invalid command Step status: %S" status))))))))

(defun magent-command-workflow--start-step (invocation step)
  "Start STEP for INVOCATION with synchronous-callback protection."
  (unless (magent-command-step-p step)
    (error "Workflow yielded invalid Step: %S" step))
  (let* ((generation (1+ (magent-command-invocation-generation invocation)))
         (item-id (magent-command--workflow-step-start invocation step))
         (starter-returned-p nil)
         pending
         cancel)
    (setf (magent-command-invocation-generation invocation) generation
          (magent-command-invocation-current-step invocation) step
          (magent-command-invocation-current-step-id invocation) item-id)
    (cl-labels
        ((done
          (status value)
          (unless (memq status '(completed failed cancelled))
            (let ((invalid status))
              (setq status 'failed
                    value
                    (format "Invalid callback Step status: %S" invalid))))
          (if starter-returned-p
              (magent-command-workflow--finish-step
               invocation generation status value)
            (unless pending
              (setq pending (cons status value))))))
      (condition-case err
          (setq cancel
                (pcase (magent-command-step-type step)
                  ('process
                   (magent-command-workflow--start-process step #'done))
                  ((or 'agent 'answer)
                   (magent-command--start-agent-step invocation step #'done))
                  ('callback
                   (funcall (magent-command--step-option step :start) #'done))
                  (_ (error "Unknown command Step type: %S"
                            (magent-command-step-type step)))))
        (error
         (setq pending (cons 'failed err))))
      (unless (or (null cancel) (functionp cancel))
        (setq pending
              (cons 'failed
                    (format "Step %s returned invalid cancel function: %S"
                            (magent-command-step-name step) cancel))
              cancel nil))
      (setq starter-returned-p t)
      (when (magent-command-workflow--active-p invocation generation)
        (setf (magent-command-invocation-step-cancel-function invocation)
              cancel))
      (when pending
        (magent-command-workflow--finish-step
         invocation generation (car pending) (cdr pending))))))

(defun magent-command-workflow--resume
    (invocation &optional outcome initial-p)
  "Resume INVOCATION iterator with OUTCOME, or start it when INITIAL-P."
  (when (eq (magent-command-invocation-status invocation) 'active)
    (condition-case condition
        (let ((step
               (if initial-p
                   (iter-next
                    (magent-command-invocation-iterator invocation))
                 (iter-next
                  (magent-command-invocation-iterator invocation) outcome))))
          (magent-command-workflow--start-step invocation step))
      (iter-end-of-sequence
       (let ((result (cdr condition)))
         (setf (magent-command-invocation-iterator invocation) nil)
         (if (or (null result) (stringp result))
             (magent-command--finish-completed invocation result)
           (magent-command--finish-failed
            invocation
            (format "Workflow returned invalid result: %S" result)))))
      (quit
       (magent-command--finish-cancelled invocation "Command cancelled"))
      (error
       (magent-command--finish-failed invocation condition)))))

(defun magent-command-workflow-start (invocation workflow)
  "Start WORKFLOW for INVOCATION and return INVOCATION."
  (unless (functionp workflow)
    (error "Expected command Workflow function, got: %S" workflow))
  (let ((iterator (funcall workflow invocation)))
    (unless (functionp iterator)
      (error "Command Workflow did not return an iterator: %S" iterator))
    (setf (magent-command-invocation-iterator invocation) iterator
          (magent-command-invocation-generation invocation) 0)
    (magent-command-workflow--resume invocation nil t))
  invocation)

(defun magent-command-workflow-cleanup (invocation reason)
  "Cancel INVOCATION's current Step and close its iterator for REASON."
  (when-let* ((step (magent-command-invocation-current-step invocation)))
    (magent-command--workflow-step-finish
     invocation step
     (magent-command-invocation-current-step-id invocation)
     'cancelled reason))
  (setf (magent-command-invocation-current-step invocation) nil
        (magent-command-invocation-current-step-id invocation) nil)
  (when-let* ((cancel
              (magent-command-invocation-step-cancel-function invocation)))
    (setf (magent-command-invocation-step-cancel-function invocation) nil)
    (condition-case err
        (funcall cancel)
      (error
       (magent-log "ERROR command Step cancellation failed: %s"
                   (error-message-string err)))))
  (magent-command-workflow--close-iterator invocation)
  invocation)

(provide 'magent-command-workflow)
;;; magent-command-workflow.el ends here
