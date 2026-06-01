;;; magent-llm-gptel.el --- gptel-request adapter for Magent LLM events  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Thin adapter from `gptel-request' callbacks to Magent's normalized
;; `magent-llm-event' protocol.  This module is the only intended place
;; for gptel FSM/callback details; the Magent-owned agent loop consumes
;; normalized events instead of gptel internals.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-request)
(require 'magent-llm)

(declare-function gptel--error-p "gptel-request")
(declare-function gptel--handle-post "gptel-request")
(declare-function gptel--handle-wait "gptel-request")
(declare-function gptel--tool-use-p "gptel-request")
(declare-function gptel-fsm-info "gptel-request")
(declare-function gptel-make-fsm "gptel-request")
(declare-function gptel-tool-name "gptel-request")
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-tools)
(defvar gptel-use-tools)
(defvar gptel-confirm-tool-calls)
(defvar gptel-include-reasoning)
(defvar gptel-temperature)
(defvar magent-include-reasoning)

(defun magent-llm-gptel--make-state ()
  "Create adapter-local streaming state."
  (let ((state (make-hash-table :test #'eq)))
    (puthash :text-chunks nil state)
    state))

(defun magent-llm-gptel--emit (request event)
  "Emit EVENT through REQUEST's callback."
  (when-let ((callback (magent-llm-request-callback request)))
    (funcall callback event)))

(defun magent-llm-gptel--final-text (state info)
  "Return final response text from STATE and gptel INFO."
  (or (plist-get info :content)
      (apply #'concat (nreverse (copy-sequence
                                 (gethash :text-chunks state))))
      ""))

(defun magent-llm-gptel--metadata (info)
  "Return adapter metadata extracted from gptel INFO."
  (let ((metadata (list :provider 'gptel)))
    (dolist (key '(:status :http-status :error :tokens :stop-reason))
      (when-let ((value (plist-get info key)))
        (setq metadata (append metadata (list key value)))))
    metadata))

(defun magent-llm-gptel--tool-name (tool-spec raw-call)
  "Return the tool name from TOOL-SPEC or RAW-CALL."
  (or (and (fboundp 'gptel-tool-name)
           tool-spec
           (ignore-errors (gptel-tool-name tool-spec)))
      (plist-get raw-call :name)))

(defun magent-llm-gptel--tool-id (raw-call)
  "Return a stable id from RAW-CALL, when present."
  (or (plist-get raw-call :id)
      (plist-get raw-call :call-id)
      (plist-get raw-call :tool-call-id)))

(defun magent-llm-gptel--normalize-tool-call (call &optional metadata)
  "Convert one gptel CALL into a normalized tool-call event.
METADATA is merged into the event metadata."
  (let* ((tool-spec (nth 0 call))
         (args (nth 1 call))
         (raw-call (or (nth 3 call)
                       (list :name (magent-llm-gptel--tool-name tool-spec nil)
                             :args args)))
         (name (magent-llm-gptel--tool-name tool-spec raw-call)))
    (magent-llm-tool-call-event
     (magent-llm-gptel--tool-id raw-call)
     name
     args
     raw-call
     (append (list :provider 'gptel) metadata))))

(defun magent-llm-gptel--handle-tool-use (fsm)
  "Report pending gptel tool calls without executing them."
  (when-let* ((info (gptel-fsm-info fsm))
              (callback (plist-get info :callback))
              (tools (plist-get info :tools))
              (tool-use (cl-remove-if (lambda (tc) (plist-get tc :result))
                                      (plist-get info :tool-use))))
    (let (pending-calls)
      (dolist (tool-call tool-use)
        (let* ((name (plist-get tool-call :name))
               (tool-spec (cl-find-if
                           (lambda (ts)
                             (equal (gptel-tool-name ts) name))
                           tools))
               (args (plist-get tool-call :args)))
          (push (list tool-spec args nil tool-call) pending-calls)))
      (funcall callback (cons 'tool-call (nreverse pending-calls)) info))))

(defun magent-llm-gptel--make-sampling-fsm ()
  "Create a gptel FSM for one model sampling request.
It lets gptel own transport/parsing while preventing gptel from
executing tools or continuing the tool loop."
  (gptel-make-fsm
   :table `((INIT . ((t . WAIT)))
            (WAIT . ((t . TYPE)))
            (TYPE . ((,#'gptel--error-p . ERRS)
                     (,#'gptel--tool-use-p . TOOL)
                     (t . DONE)))
            (TOOL . ((t . DONE))))
   :handlers `((WAIT ,#'gptel--handle-wait)
               (TOOL ,#'magent-llm-gptel--handle-tool-use)
               (DONE ,#'gptel--handle-post)
               (ERRS ,#'gptel--handle-post)
               (ABRT ,#'gptel--handle-post))))

(defun magent-llm-gptel--callback (request state buffer response info)
  "Map gptel RESPONSE and INFO to normalized events for REQUEST."
  (cond
   ((stringp response)
    (push response (gethash :text-chunks state))
    (magent-llm-gptel--emit
     request
     (magent-llm-text-delta-event
      response
      (magent-llm-gptel--metadata info))))
   ((and (consp response) (eq (car response) 'reasoning))
    (magent-llm-gptel--emit
     request
     (if (eq (cdr response) t)
         (magent-llm-reasoning-end-event
          (magent-llm-gptel--metadata info))
       (magent-llm-reasoning-delta-event
        (cdr response)
        (magent-llm-gptel--metadata info)))))
   ((and (consp response) (eq (car response) 'tool-call))
    (let ((calls (cdr response)))
      (cl-loop for call in calls
               for index from 0
               for last-p = (= index (1- (length calls)))
               do (magent-llm-gptel--emit
                   request
                   (magent-llm-gptel--normalize-tool-call
                    call
                    (when last-p '(:last t)))))))
   ((eq response t)
    (if (plist-get info :tool-use)
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
      (magent-llm-gptel--emit
       request
       (magent-llm-completed-event
        (magent-llm-gptel--final-text state info)
        (plist-get info :tokens)
        (plist-get info :stop-reason)
        (magent-llm-gptel--metadata info)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
   ((eq response 'abort)
    (magent-llm-gptel--emit
     request
     (magent-llm-error-event
      "Request aborted"
      (append (magent-llm-gptel--metadata info) '(:status abort))))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
   ((null response)
    (magent-llm-gptel--emit
     request
     (magent-llm-error-event
      (or (plist-get info :status)
          (plist-get info :error)
          "gptel request failed")
      (magent-llm-gptel--metadata info)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))))

(defun magent-llm-gptel-sample (request)
  "Start one gptel sampling request for REQUEST.
Return the request buffer as the abort handle.  REQUEST must be a
`magent-llm-request'."
  (unless (magent-llm-request-p request)
    (error "Expected magent-llm-request, got: %S" request))
  (let ((buffer (generate-new-buffer " *magent-llm-gptel-request*"))
        (state (magent-llm-gptel--make-state))
        (metadata (magent-llm-request-metadata request)))
    (with-current-buffer buffer
      (when (magent-llm-request-backend request)
        (setq-local gptel-backend (magent-llm-request-backend request)))
      (when (magent-llm-request-model request)
        (setq-local gptel-model (magent-llm-request-model request)))
      (when (and (plist-member metadata :temperature)
                 (boundp 'gptel-temperature))
        (setq-local gptel-temperature (plist-get metadata :temperature)))
      (setq-local gptel-tools (magent-llm-request-tools request))
      (setq-local gptel-use-tools (and gptel-tools t))
      (setq-local gptel-confirm-tool-calls t)
      (when (boundp 'magent-include-reasoning)
        (setq-local gptel-include-reasoning magent-include-reasoning))
      (gptel-request
          (magent-llm-request-prompt request)
        :buffer buffer
        :context (list :magent-llm-gptel t)
        :system (magent-llm-request-system request)
        :stream (magent-llm-request-stream request)
        :fsm (magent-llm-gptel--make-sampling-fsm)
        :callback (lambda (response info)
                    (magent-llm-gptel--callback
                     request state buffer response info))))
    buffer))

(provide 'magent-llm-gptel)
;;; magent-llm-gptel.el ends here
