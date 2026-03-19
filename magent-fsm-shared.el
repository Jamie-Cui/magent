;;; magent-fsm-shared.el --- Shared FSM helpers for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Shared data structures and helper functions used by both the gptel
;; backend and the legacy native FSM backend.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magent-config)
(require 'magent-approval)
(require 'magent-events)

(declare-function gptel-request "gptel")
(declare-function gptel-make-tool "gptel")
(declare-function gptel-tool-async "gptel-request")
(declare-function gptel-tool-function "gptel-request")
(declare-function gptel-tool-args "gptel-request")
(declare-function gptel-tool-name "gptel-request")
(declare-function magent-ui-insert-tool-call "magent-ui")
(declare-function magent-ui-insert-tool-result "magent-ui")
(declare-function magent-audit-record-permission-decision "magent-audit")
(declare-function magent-tools-permission-key "magent-tools")
(declare-function magent-permission-bypass-p "magent-permission")
(declare-function magent-permission-resolve "magent-permission")
(declare-function magent-permission-session-override "magent-permission")
(declare-function magent-permission-set-session-override "magent-permission")

(cl-defstruct (magent-fsm (:constructor magent-fsm-native-create)
                          (:copier magent-fsm-copy))
  "Finite state machine for Magent LLM requests."
  (state 'INIT)
  session
  agent
  backend
  model
  prompt-list
  system-prompt
  tools
  streaming-p
  pending-tools
  (accumulated-text "")
  (streamed-chunks "")
  (reasoning-text "")
  in-reasoning-block
  http-status
  http-message
  error
  callback
  ui-callback
  request-buffer
  process
  permission
  aborted)

(defun magent-fsm--show-tool-call-format (name args)
  "Format tool call NAME with ARGS into a display string."
  (cond
   ((string= name "skill_invoke")
    (let ((skill-name (plist-get args :skill_name))
          (operation (plist-get args :operation)))
      (format "%s/%s" (or skill-name "?") (or operation "?"))))
   ((string= name "delegate")
    (let ((agent (plist-get args :agent)))
      (format "agent: %s" (or agent "?"))))
   ((string= name "bash")
    (let ((cmd (plist-get args :command)))
      (if cmd
          (truncate-string-to-width cmd magent-ui-tool-input-max-length nil nil "...")
        "?")))
   ((member name '("read_file" "write_file" "edit_file"))
    (or (plist-get args :path) "?"))
   ((string= name "grep")
    (let ((pattern (plist-get args :pattern))
          (path (plist-get args :path)))
      (format "%s in %s" (or pattern "?") (or path "?"))))
   ((string= name "glob")
    (or (plist-get args :pattern) "?"))
   ((string= name "emacs_eval")
    (let ((sexp (plist-get args :sexp)))
      (if sexp
          (truncate-string-to-width sexp magent-ui-tool-input-max-length nil nil "...")
        "?")))
   (t
    (if args
        (truncate-string-to-width (format "%s" args) 60 nil nil "...")
      ""))))

(defun magent-fsm--show-tool-call (name args)
  "Show tool call NAME with ARGS in the UI."
  (require 'magent-ui)
  (magent-ui-insert-tool-call name (magent-fsm--show-tool-call-format name args)))

(defun magent-fsm--show-tool-result (name result)
  "Show tool RESULT for tool NAME in the UI."
  (require 'magent-ui)
  (let* ((result-str (if (stringp result) result (format "%s" result)))
         (truncated (if (> (length result-str) magent-ui-result-max-length)
                        (format "%s... [%d bytes]"
                                (substring result-str 0 magent-ui-result-preview-length)
                                (length result-str))
                      result-str)))
    (magent-ui-insert-tool-result name truncated)))

(defun magent-fsm--args-to-plist (args-spec arg-values)
  "Build a keyword plist from ARGS-SPEC names and ARG-VALUES."
  (cl-loop for spec in args-spec
           for val in arg-values
           append (list (intern (concat ":" (plist-get spec :name))) val)))

;;; Tool render queue

(cl-defstruct (magent-fsm--tool-queue
               (:constructor magent-fsm--tool-queue-create))
  items
  busy)

(defun magent-fsm--tool-queue-push (queue item)
  "Push ITEM onto QUEUE and start processing if idle."
  (setf (magent-fsm--tool-queue-items queue)
        (nconc (magent-fsm--tool-queue-items queue) (list item)))
  (magent-fsm--tool-queue-run queue))

(defun magent-fsm--tool-queue-run (queue)
  "Process the next queued tool if the queue is idle."
  (unless (magent-fsm--tool-queue-busy queue)
    (when-let ((item (pop (magent-fsm--tool-queue-items queue))))
      (setf (magent-fsm--tool-queue-busy queue) t)
      (let ((name (plist-get item :name))
            (input (plist-get item :input))
            (call-id (plist-get item :call-id))
            (args-plist (plist-get item :args-plist))
            (fn (plist-get item :fn))
            (async-p (plist-get item :async))
            (callback (plist-get item :callback))
            (args (plist-get item :args)))
        (require 'magent-ui)
        (magent-events-emit 'tool-call-start
                            :call-id call-id
                            :tool-name name
                            :title name
                            :summary input
                            :description input
                            :args args-plist)
        (magent-ui-insert-tool-call name input)
        (let ((completion
               (lambda (result)
                 (magent-fsm--show-tool-result name result)
                 (magent-events-emit 'tool-call-end
                                     :call-id call-id
                                     :tool-name name
                                     :result result)
                 (setf (magent-fsm--tool-queue-busy queue) nil)
                 (funcall callback result)
                 (magent-fsm--tool-queue-run queue))))
          (if async-p
              (apply fn completion args)
            (funcall completion (apply fn args))))))))

(defun magent-fsm--wrap-tool-function (name args-spec original-fn async-p
                                           &optional queue)
  "Wrap ORIGINAL-FN to render tool UI and events before/after execution."
  (if queue
      (lambda (callback &rest arg-values)
        (let* ((args-plist (magent-fsm--args-to-plist args-spec arg-values))
               (summary (magent-fsm--show-tool-call-format name args-plist)))
          (magent-fsm--tool-queue-push
           queue
           (list :name name
                 :call-id (magent-events-generate-id)
                 :input summary
                 :args-plist args-plist
                 :fn original-fn
                 :async async-p
                 :callback callback
                 :args arg-values))))
    (if async-p
        (lambda (callback &rest arg-values)
          (let* ((args-plist (magent-fsm--args-to-plist args-spec arg-values))
                 (call-id (magent-events-generate-id))
                 (summary (magent-fsm--show-tool-call-format name args-plist)))
            (magent-events-emit 'tool-call-start
                                :call-id call-id
                                :tool-name name
                                :title name
                                :summary summary
                                :description summary
                                :args args-plist)
            (magent-fsm--show-tool-call name args-plist)
            (apply original-fn
                   (lambda (result)
                     (magent-fsm--show-tool-result name result)
                     (magent-events-emit 'tool-call-end
                                         :call-id call-id
                                         :tool-name name
                                         :result result)
                     (funcall callback result))
                   arg-values)))
      (lambda (&rest arg-values)
        (let* ((args-plist (magent-fsm--args-to-plist args-spec arg-values))
               (call-id (magent-events-generate-id))
               (summary (magent-fsm--show-tool-call-format name args-plist))
               result)
          (magent-events-emit 'tool-call-start
                              :call-id call-id
                              :tool-name name
                              :title name
                              :summary summary
                              :description summary
                              :args args-plist)
          (magent-fsm--show-tool-call name args-plist)
          (setq result (apply original-fn arg-values))
          (magent-fsm--show-tool-result name result)
          (magent-events-emit 'tool-call-end
                              :call-id call-id
                              :tool-name name
                              :result result)
          result)))))

(defun magent-fsm--convert-tools-to-gptel (tools &optional permission _event-context)
  "Convert Magent TOOLS to gptel-tool structs with UI wrappers."
  (require 'gptel)
  (let ((queue (magent-fsm--tool-queue-create)))
    (mapcar (lambda (tool)
              (let* ((name (plist-get tool :name))
                     (args-spec (plist-get tool :args))
                     (original-fn (plist-get tool :function))
                     (async-p (plist-get tool :async))
                     (perm-key (plist-get tool :perm-key))
                     (wrapped-fn (magent-fsm--wrap-tool-function
                                  name args-spec original-fn async-p queue))
                     (confirm-fn (when permission
                                   (magent-fsm--make-confirm-function
                                    perm-key permission args-spec))))
                (gptel-make-tool
                 :name name
                 :description (plist-get tool :description)
                 :args args-spec
                 :function wrapped-fn
                 :async t
                 :confirm confirm-fn)))
            tools)))

;;; Permission-aware tool confirmation

(defun magent-fsm--make-confirm-function (perm-key permission args-spec)
  "Create a gptel :confirm function for PERM-KEY under PERMISSION."
  (require 'magent-permission)
  (unless (magent-permission-bypass-p)
    (let* ((base-perm (magent-permission-resolve permission perm-key))
           (file-arg-index (magent-fsm--find-file-arg-index args-spec)))
      (cond
       ((eq base-perm 'ask)
        (if file-arg-index
            (lambda (&rest arg-values)
              (let ((override (magent-permission-session-override perm-key)))
                (if (eq override 'allow)
                    (let ((file-path (nth file-arg-index arg-values)))
                      (when file-path
                        (eq (magent-permission-resolve permission perm-key file-path)
                            'deny)))
                  t)))
          (lambda (&rest _arg-values)
            (not (eq (magent-permission-session-override perm-key) 'allow)))))
       ((eq base-perm 'allow)
        (if file-arg-index
            (lambda (&rest arg-values)
              (let ((file-path (nth file-arg-index arg-values)))
                (when file-path
                  (not (eq (magent-permission-resolve permission perm-key file-path)
                           'allow)))))
          nil))
       ((eq base-perm 'deny)
        (lambda (&rest _arg-values) t))
       (t nil)))))

(defun magent-fsm--find-file-arg-index (args-spec)
  "Return the 0-based index of a file path arg in ARGS-SPEC, if any."
  (cl-loop for spec in args-spec
           for i from 0
           when (member (plist-get spec :name) '("path" "file"))
           return i))

(defun magent-fsm--audit-permission-decision (tool-spec arg-values decision source)
  "Record a permission DECISION for TOOL-SPEC with ARG-VALUES."
  (when (fboundp 'magent-audit-record-permission-decision)
    (let* ((tool-name (gptel-tool-name tool-spec))
           (perm-key (magent-tools-permission-key tool-name))
           (args-plist (magent-fsm--args-to-plist (gptel-tool-args tool-spec)
                                                  arg-values))
           (summary (magent-fsm--show-tool-call-format tool-name args-plist)))
      (magent-audit-record-permission-decision
       tool-name perm-key decision source
       :context (magent-events-current-context)
       :summary summary
       :args args-plist))))

(defun magent-fsm--handle-tool-call-confirmation-with-permission (permission tool-calls)
  "Handle TOOL-CALLS using PERMISSION rules."
  (require 'magent-permission)
  (if (magent-permission-bypass-p)
      (dolist (tc tool-calls)
        (let* ((tool-spec (car tc))
               (arg-values (cadr tc))
               (cb (caddr tc))
               (tool-name (gptel-tool-name tool-spec)))
          (magent-log "PERM bypass allow: %s" tool-name)
          (magent-fsm--audit-permission-decision tool-spec arg-values 'allow 'bypass)
          (magent-fsm--run-tool tool-spec cb arg-values)))
    (let (pending)
      (dolist (tc tool-calls)
        (let* ((tool-spec (car tc))
               (arg-values (cadr tc))
               (cb (caddr tc))
               (tool-name (gptel-tool-name tool-spec))
               (perm-key (magent-tools-permission-key tool-name))
               (file-path (when perm-key
                            (let ((idx (magent-fsm--find-file-arg-index
                                        (gptel-tool-args tool-spec))))
                              (when idx
                                (nth idx arg-values)))))
               (resolved (when (and permission perm-key)
                           (magent-permission-resolve permission perm-key file-path)))
               (override (when perm-key
                           (magent-permission-session-override perm-key))))
          (cond
           ((eq override 'allow)
            (magent-log "PERM auto-allow (session override): %s" tool-name)
            (magent-fsm--audit-permission-decision
             tool-spec arg-values 'allow 'session-override-allow)
            (magent-fsm--run-tool tool-spec cb arg-values))
           ((eq override 'deny)
            (magent-log "PERM auto-deny (session override): %s" tool-name)
            (magent-fsm--audit-permission-decision
             tool-spec arg-values 'deny 'session-override-deny)
            (funcall cb (format "Error: tool '%s' denied by session policy" tool-name)))
           ((eq resolved 'deny)
            (magent-log "PERM auto-deny (file rule): %s %s" tool-name (or file-path ""))
            (magent-fsm--audit-permission-decision
             tool-spec arg-values 'deny 'file-rule-deny)
            (funcall cb (format "Error: access denied for %s on %s"
                                tool-name (or file-path "this resource"))))
           ((and file-path (eq resolved 'allow))
            (magent-log "PERM auto-allow (file rule): %s %s" tool-name file-path)
            (magent-fsm--audit-permission-decision
             tool-spec arg-values 'allow 'file-rule-allow)
            (magent-fsm--run-tool tool-spec cb arg-values))
           (t
            (push tc pending)))))
      (when pending
        (magent-fsm--prompt-next-tool-call (nreverse pending))))))

(defun magent-fsm--handle-tool-call-confirmation (fsm tool-calls)
  "Handle TOOL-CALLS for FSM using its attached permission rules."
  (magent-fsm--handle-tool-call-confirmation-with-permission
   (magent-fsm-permission fsm)
   tool-calls))

(defun magent-fsm--prompt-next-tool-call (tool-calls)
  "Prompt for the next tool call in TOOL-CALLS."
  (when tool-calls
    (let* ((tc (car tool-calls))
           (rest (cdr tool-calls))
           (tool-spec (car tc))
           (arg-values (cadr tc))
           (cb (caddr tc))
           (tool-name (gptel-tool-name tool-spec))
           (perm-key (magent-tools-permission-key tool-name))
           (summary (magent-fsm--summarize-args
                     arg-values (gptel-tool-args tool-spec))))
      (magent-approval-request
       (list :request-id (magent-events-generate-id)
             :context (magent-events-current-context)
             :tool-name tool-name
             :perm-key perm-key
             :summary summary
             :args (magent-fsm--args-to-plist (gptel-tool-args tool-spec) arg-values))
       (lambda (decision)
         (pcase decision
           ('allow-once
            (magent-log "PERM user allowed (once): %s" tool-name)
            (magent-fsm--run-tool tool-spec cb arg-values))
           ('deny-once
            (magent-log "PERM user denied (once): %s" tool-name)
            (funcall cb (format "Error: tool '%s' denied by user" tool-name)))
           ('allow-session
            (magent-log "PERM user always-allow: %s" tool-name)
            (when perm-key
              (magent-permission-set-session-override perm-key 'allow))
            (magent-fsm--run-tool tool-spec cb arg-values))
           ('deny-session
            (magent-log "PERM user always-deny: %s" tool-name)
            (when perm-key
              (magent-permission-set-session-override perm-key 'deny))
            (funcall cb (format "Error: tool '%s' denied by user" tool-name))))
         (magent-fsm--prompt-next-tool-call rest))))))

(defun magent-fsm--run-tool (tool-spec cb arg-values)
  "Execute TOOL-SPEC with ARG-VALUES and call CB with the result."
  (condition-case err
      (if (gptel-tool-async tool-spec)
          (apply (gptel-tool-function tool-spec) cb arg-values)
        (funcall cb (apply (gptel-tool-function tool-spec) arg-values)))
    (error
     (funcall cb (format "Error executing tool: %s"
                         (error-message-string err))))))

(defun magent-fsm--summarize-args (arg-values args-spec)
  "Create a short prompt summary from ARG-VALUES and ARGS-SPEC."
  (let* ((first-val (car arg-values))
         (first-name (when args-spec
                       (plist-get (car args-spec) :name)))
         (display (cond
                   ((null first-val) "")
                   ((stringp first-val)
                    (if (> (length first-val) 60)
                        (concat (substring first-val 0 57) "...")
                      first-val))
                   (t (format "%s" first-val)))))
    (if (and first-name (not (string-empty-p display)))
        display
      "")))

(provide 'magent-fsm-shared)
;;; magent-fsm-shared.el ends here
