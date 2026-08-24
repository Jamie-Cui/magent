;;; magent-action-session.el --- Durable isolated Action sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6

;;; Commentary:

;; Persistence, ledger projection, lifecycle, and cancellation for isolated
;; `magent-action' invocations.  Inspection UI lives in
;; `magent-action-session-view'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magent-action)
(require 'magent-ledger)
(require 'magent-protocol)
(require 'magent-session)

(declare-function magent-runtime-session-create "magent-runtime-api" t t)

(defvar magent-action-session--active-invocations
  (make-hash-table :test #'equal)
  "Active isolated invocations keyed by their action session ids.")

(defun magent-action-session--status-string (status)
  "Return action STATUS as a string."
  (cond
   ((stringp status) status)
   ((symbolp status) (symbol-name status))
   ((null status) "unknown")
   (t (format "%s" status))))

(defun magent-action-session--save (invocation)
  "Persist INVOCATION's action session without changing the active session."
  (magent-session-save-for-session
   (magent-action-invocation-session invocation)
   (magent-action-invocation-scope invocation)))

(defun magent-action-session--with-current-session (invocation function)
  "Call FUNCTION with INVOCATION's session and scope temporarily active."
  (let ((previous-session magent--current-session)
        (previous-scope magent-session--current-scope))
    (unwind-protect
        (progn
          (setq magent--current-session
                (magent-action-invocation-session invocation)
                magent-session--current-scope
                (magent-action-invocation-scope invocation))
          (funcall function))
      (setq magent--current-session previous-session
            magent-session--current-scope previous-scope))))

(defun magent-action-session--ensure-turn (invocation thread input)
  "Ensure INVOCATION has an Action ledger turn for INPUT."
  (or (magent-action-invocation-turn-id invocation)
      (let ((turn (magent-thread-queue-turn
                   thread input nil
                   (list :source 'magent-action
                         :action
                         (magent-action-spec-name
                          (magent-action-invocation-spec invocation))
                         :action-invocation-id
                         (magent-action-invocation-id invocation)
                         :workflow-control t))))
        (magent-thread-start-turn thread (magent-thread-turn-id turn))
        (setf (magent-action-invocation-turn-id invocation)
              (magent-thread-turn-id turn))
        (magent-thread-turn-id turn))))

(defun magent-action-session-start-step (invocation step)
  "Record STEP start for INVOCATION and return its item id."
  (magent-action-session--with-current-session
   invocation
   (lambda ()
     (let* ((session (magent-action-invocation-session invocation))
            (thread (magent-session-thread-ledger session))
            (turn-id
             (magent-action-session--ensure-turn
              invocation thread
              (magent-action-spec-title
               (magent-action-invocation-spec invocation))))
            (item
             (magent-thread-start-item
              thread turn-id 'workflow-step
              :name (magent-action-step-name step)
              :input (magent-action--step-activity-input step)
              :metadata
              (list :source 'magent-action
                    :action-invocation-id
                    (magent-action-invocation-id invocation)
                    :step-type (magent-action-step-type step)))))
       (magent-action-session--save invocation)
       (magent-thread-item-id item)))))

(defun magent-action-session--step-error-string (value)
  "Return durable error text for failed Step VALUE."
  (cond
   ((magent-execution-result-p value)
    (magent-execution-result-content-string value))
   ((and (consp value) (symbolp (car value)))
    (error-message-string value))
   (t (format "%s" value))))

(defun magent-action-session-finish-step
    (invocation step item-id status value)
  "Record STEP ITEM-ID terminal STATUS and VALUE for INVOCATION."
  (when item-id
    (magent-action-session--with-current-session
     invocation
     (lambda ()
       (let* ((session (magent-action-invocation-session invocation))
              (thread (magent-session-thread-ledger session))
              (item
               (cl-find item-id (magent-thread-all-items thread)
                        :key #'magent-thread-item-id :test #'equal))
              (output
               (magent-action--step-activity-output
                step status value))
              (metadata
               (and item
                    (append
                     (magent-thread-item-metadata item)
                     (when-let* ((submission-id
                                  (magent-action-invocation-current-submission-id
                                   invocation)))
                       (list :submission-id submission-id))))))
         (when item
           (pcase status
             ('completed
              (magent-thread-complete-item
               thread item :output output :metadata metadata))
             ('cancelled
              (magent-thread-cancel-item
               thread item (magent-action-session--step-error-string value)))
             (_
              (magent-thread-fail-item
               thread item (magent-action-session--step-error-string value)
               :output output :metadata metadata)))
           (setf (magent-action-invocation-current-submission-id invocation)
                 nil)
           (magent-action-session--save invocation)))))))

(defun magent-action-session-finalize-workflow-turn
    (invocation status result)
  "Finalize INVOCATION's control turn with STATUS and RESULT."
  (when-let* ((turn-id (magent-action-invocation-turn-id invocation)))
    (magent-action-session--with-current-session
     invocation
     (lambda ()
       (let* ((session (magent-action-invocation-session invocation))
              (thread (magent-session-thread-ledger session))
              (turn (magent-thread-find-turn thread turn-id))
              (message (magent-execution-result-content-string result)))
         (unless (or (null turn) (magent-thread-terminal-turn-p turn))
           (pcase status
             ('completed (magent-thread-complete-turn thread turn-id))
             ('cancelled (magent-thread-interrupt-turn thread turn-id message))
             (_ (magent-thread-fail-turn thread turn-id message)))
           (magent-action-session--save invocation)))))))

(defun magent-action-session-record-message
    (invocation role content &optional phase metadata)
  "Record ROLE message CONTENT in INVOCATION's ledger."
  (magent-action-session--with-current-session
   invocation
   (lambda ()
     (let* ((session (magent-action-invocation-session invocation))
            (thread (magent-session-thread-ledger session))
            (turn-id (magent-action-session--ensure-turn
                      invocation thread
                      (magent-action-spec-title
                       (magent-action-invocation-spec invocation)))))
       (magent-thread-record-message
        thread turn-id role content phase
        (append metadata (list :source 'magent-action)))
       (magent-action-session--save invocation)))))

(defun magent-action-session--session-id (invocation)
  "Return INVOCATION's action session id."
  (magent-session-get-id (magent-action-invocation-session invocation)))

(defun magent-action-session-initialize (invocation)
  "Create and attach an isolated durable session to INVOCATION."
  (let* ((spec (magent-action-invocation-spec invocation))
         (origin-scope (magent-action-invocation-origin-scope invocation))
         (session (magent-session-create))
         (id (magent-session-get-id session))
         (scope (magent-session-action-scope
                 id (magent-action-spec-name spec) origin-scope))
         ;; Do not register through `magent-runtime-session-register': Doctor
         ;; must remain usable while an unrelated runtime queue lease exists.
         (runtime-session
          (magent-runtime-session-create
           :id id :scope scope :magent-session session)))
    (setf (magent-action-invocation-session invocation) session
          (magent-action-invocation-scope invocation) scope
          (magent-action-invocation-runtime-session invocation) runtime-session)
    (dolist (entry `((kind . "action")
                     (action . ,(magent-action-spec-name spec))
                     (title . ,(magent-action-spec-title spec))
                     (status . "running")
                     (session-policy . "isolated")
                     (origin-scope . ,origin-scope)
                     (invocation-id . ,(magent-action-invocation-id invocation))
                     ,@(when-let* ((parent-id
                                    (magent-action-invocation-parent-session-id
                                     invocation)))
                         `((parent-session-id . ,parent-id)))))
      (magent-session-set-metadata-value session (car entry) (cdr entry)))
    (let ((previous-session magent--current-session)
          (previous-scope magent-session--current-scope))
      (unwind-protect
          (magent-session-install scope session)
        (setq magent--current-session previous-session
              magent-session--current-scope previous-scope)))
    (puthash id invocation magent-action-session--active-invocations)
    invocation))

(defun magent-action-session-active-invocations (&optional cancellable-only)
  "Return active isolated invocations.
When CANCELLABLE-ONLY is non-nil, omit invocations without owned work."
  (let (invocations)
    (maphash
     (lambda (_id invocation)
       (when (and (eq (magent-action-invocation-status invocation) 'active)
                  (or (not cancellable-only)
                      (magent-action-invocation-current-step invocation)))
         (push invocation invocations)))
     magent-action-session--active-invocations)
    (sort invocations
          (lambda (left right)
            (string< (magent-action-session--session-id left)
                     (magent-action-session--session-id right))))))

(defun magent-action-session--record-parent-breadcrumb
    (invocation status message)
  "Record INVOCATION completion as a compact parent-session breadcrumb."
  (when-let* ((parent (magent-action-invocation-parent-session invocation)))
    (let* ((parent-scope (or (magent-action-invocation-parent-scope invocation)
                             (magent-action-invocation-origin-scope invocation)))
           (session-id (magent-action-session--session-id invocation))
           (name (magent-action-spec-name
                  (magent-action-invocation-spec invocation)))
           (title (format "Action: %s" name))
           (result-text
            (format "%s %s: %s"
                    name
                    (magent-action-session--status-string status)
                    (or message session-id)))
           (result
            (magent-tool-result-create
             :name name
             :output result-text
             :success (eq status 'completed)
             :status (if (eq status 'completed) 'completed 'failed)
             :error (unless (eq status 'completed) result-text)
             :metadata
             (list :action-status
                   (magent-action-session--status-string status))))
           (previous-session magent--current-session)
           (previous-scope magent-session--current-scope))
      (unwind-protect
          (progn
            (setq magent--current-session parent
                  magent-session--current-scope parent-scope)
            (let* ((thread (magent-session-thread-ledger parent))
                   (turn (magent-thread-queue-turn
                          thread title nil
                          (list :source 'magent-action-breadcrumb
                                :action-session-id session-id
                                :action name))))
              (magent-thread-start-turn thread (magent-thread-turn-id turn))
              (magent-thread-record-user-message-if-needed
               thread (magent-thread-turn-id turn) title nil
               (list :source 'magent-action-breadcrumb))
              (magent-thread-record-tool-result
               thread (magent-thread-turn-id turn)
               (magent-protocol-generate-id "action-ref")
               name
               (list :action-session-id session-id
                     :status (magent-action-session--status-string status))
               result
               (list :source 'magent-action-breadcrumb)
               magent-session--current-scope)
              (pcase status
                ('completed
                 (magent-thread-complete-turn thread (magent-thread-turn-id turn)))
                ('cancelled
                 (magent-thread-interrupt-turn
                  thread (magent-thread-turn-id turn) result-text))
                (_
                 (magent-thread-fail-turn
                  thread (magent-thread-turn-id turn) result-text)))
              (magent-session-save-deferred-for-session
               parent parent-scope 0)))
        (setq magent--current-session previous-session
              magent-session--current-scope previous-scope)))))

(defun magent-action-session-finalize (invocation status result)
  "Finalize INVOCATION and return a fallback frontend response string."
  (let* ((session (magent-action-invocation-session invocation))
         (message (magent-execution-result-content-string result))
         (fallback
          (and (not (magent-action-invocation-response-recorded-p invocation))
               (not (string-empty-p (string-trim message)))
               message)))
    (magent-session-set-metadata-value
     session 'status (magent-action-session--status-string status))
    (magent-action-session--with-current-session
     invocation
     (lambda ()
       (let* ((thread (magent-session-thread-ledger session))
              (turn-id (magent-action-session--ensure-turn
                        invocation thread
                        (magent-action-spec-title
                         (magent-action-invocation-spec invocation)))))
         (when fallback
           (magent-thread-record-message
            thread turn-id 'assistant fallback nil
            (list :source 'magent-action-final
                  :status (magent-action-session--status-string status)))
           (setf (magent-action-invocation-response-recorded-p invocation) t))
         (unless (when-let* ((turn (magent-thread-find-turn thread turn-id)))
                   (magent-thread-terminal-turn-p turn))
           (pcase status
             ('completed (magent-thread-complete-turn thread turn-id))
             ('cancelled (magent-thread-interrupt-turn thread turn-id message))
             (_ (magent-thread-fail-turn thread turn-id message))))
         (magent-action-session--save invocation))))
    (magent-action-session--record-parent-breadcrumb invocation status message)
    (magent-action-session-untrack invocation)
    (when (magent-action-invocation-interactive-p invocation)
      (message "Magent %s %s: %s"
               (magent-action-spec-name
                (magent-action-invocation-spec invocation))
               (magent-action-session--status-string status)
               (or message (magent-action-session--session-id invocation))))
    fallback))

(defun magent-action-session-untrack (invocation)
  "Stop tracking isolated action INVOCATION."
  (when (magent-action-invocation-session invocation)
    (remhash (magent-action-session--session-id invocation)
             magent-action-session--active-invocations)))

(defun magent-action-session-cancel (session-id)
  "Cancel the active isolated action SESSION-ID."
  (let ((invocation
         (gethash session-id magent-action-session--active-invocations)))
    (unless invocation
      (user-error "Magent: action session is not active: %s" session-id))
    (magent-action-cancel invocation)))

(defun magent-action-session--active-label (invocation)
  "Return a unique completion label for active INVOCATION."
  (let ((spec (magent-action-invocation-spec invocation)))
    (format "%s  %s  <%s>"
            (magent-action-spec-name spec)
            (magent-action-spec-title spec)
            (magent-action-session--session-id invocation))))

(defun magent-action-session-read-active-id ()
  "Read and return the id of an active cancellable action session."
  (let ((invocations (magent-action-session-active-invocations t)))
    (unless invocations
      (user-error "Magent: no cancellable isolated actions are active"))
    (let* ((choices
            (mapcar
             (lambda (invocation)
               (cons (magent-action-session--active-label invocation)
                     (magent-action-session--session-id invocation)))
             invocations))
           (selected
            (completing-read "Cancel action: " (mapcar #'car choices) nil t)))
      (cdr (assoc selected choices)))))

(provide 'magent-action-session)
;;; magent-action-session.el ends here
