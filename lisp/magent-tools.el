;;; magent-tools.el --- Tool implementations for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Tool implementations that the AI agent can use to interact with files
;; and the system.  Tools are registered as gptel-tool structs for use
;; with gptel's tool calling system.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'gptel-request)
(require 'json)
(require 'subr-x)
(require 'magent-config)
(require 'magent-agent-job)
(require 'magent-agent-registry)
(require 'magent-lifecycle-events)
(require 'magent-permission)
(require 'magent-protocol)
(require 'magent-repo-summary)
(require 'magent-runtime)
(require 'magent-session)

(declare-function magent-skills-invoke "magent-skills")
(declare-function magent-agent-process "magent-agent")
(declare-function magent-agent-loop-abort "magent-agent-loop")
(declare-function magent-agent-loop-p "magent-agent-loop" t t)

(defvar magent--current-session)
(defvar magent-lifecycle-events--subagent-audit-context)
(defvar magent-session--current-scope)

;;; Tool implementations

(defun magent-tools--dom-inner-text (dom)
  "Return the rendered text content of DOM."
  (if (fboundp 'dom-inner-text)
      (dom-inner-text dom)
    (funcall (symbol-function (intern (concat "dom" "-text"))) dom)))

(defvar magent-tools--request-context nil
  "Dynamically bound `magent-request-context' for the current tool call.")

(defvar magent-tools--register-cancel nil
  "Dynamically bound function used to register request abort cleanups.")

(defvar magent-tools--agent-job-runtimes (make-hash-table :test #'equal)
  "Runtime state for active child-agent jobs keyed by job id.
This table intentionally stores non-persistent values such as child sessions,
request contexts, and live loop handles.  Durable job metadata lives in the
parent `magent-session' under `agent-jobs'.")

(defconst magent-tools--read-file-default-line-count 200
  "Default maximum number of lines returned by `read_file'.")

(defconst magent-tools--read-file-page-max-characters 8000
  "Target maximum content characters returned by one `read_file' page.
Pages end at a line boundary when possible.  A single longer line is returned
whole so a subsequent line-based request always makes progress.")

(defun magent-tools--origin-buffer-name ()
  "Return the request origin buffer name for the current tool call."
  (and magent-tools--request-context
       (magent-request-context-origin-buffer-name magent-tools--request-context)))

(defun magent-tools--request-project-root ()
  "Return the current tool request's inherited project root."
  (or (and magent-tools--request-context
           (magent-request-context-project-root magent-tools--request-context))
      (and magent-tools--request-context
           (let ((scope (magent-request-context-scope
                         magent-tools--request-context)))
             (and (stringp scope) scope)))
      (magent-project-root)))

(defun magent-tools--register-cancel-cleanup (cleanup)
  "Register CLEANUP for the current request when supported."
  (when (functionp magent-tools--register-cancel)
    (funcall magent-tools--register-cancel cleanup))
  cleanup)

(defun magent-tools-canonical-resource-path (path &optional project-root)
  "Return the canonical absolute resource path for PATH.
Expands ~ and environment variables first, then resolves relative paths
against PROJECT-ROOT or the current tool request's project root.  Permission
resolution and tool I/O both use this function so they cannot disagree about
absolute versus project-relative spellings of the same resource."
  (unless (stringp path)
    (error "Resource path must be a string (got %S)" path))
  (let* ((path (substitute-in-file-name path))
         (root (or project-root (magent-tools--request-project-root)))
         (canonical-root (and root (file-truename (expand-file-name root))))
         (expanded (if (file-name-absolute-p path)
                       (expand-file-name path)
                     (expand-file-name path canonical-root))))
    ;; `substitute-in-file-name' deliberately leaves references to undefined
    ;; variables untouched.  Such a path is not a stable policy identity: a
    ;; later tool or timer could define the variable before execution and make
    ;; the same string name a different resource.
    (when (string-match-p
           "\\$\\(?:{[[:alpha:]_][[:alnum:]_]*}\\|[[:alpha:]_][[:alnum:]_]*\\)"
           path)
      (error "Resource path contains an unresolved environment variable"))
    ;; `file-truename' also resolves existing symlink ancestors for a target
    ;; that does not exist yet, preventing policy checks and I/O from naming
    ;; different resources through a symlinked directory.
    (file-truename expanded)))

(defun magent-tools--resolve-path (path)
  "Resolve PATH for tool operations."
  (magent-tools-canonical-resource-path path))

(defun magent-tools--read-file-buffer-page
    (start-line line-count source modified)
  "Return a self-describing page from the current buffer.
START-LINE is one-based and LINE-COUNT is the requested maximum number of
lines.  SOURCE identifies how the buffer was obtained and MODIFIED reports
whether a live visiting buffer has unsaved changes."
  (save-excursion
    (save-restriction
      (widen)
      (let ((total-lines (count-lines (point-min) (point-max))))
        (goto-char (point-min))
        (forward-line (1- start-line))
        (let ((begin (point)))
          (forward-line line-count)
          (let ((end (point)))
            (when (> (- end begin)
                     magent-tools--read-file-page-max-characters)
              (goto-char (+ begin
                            magent-tools--read-file-page-max-characters))
              (unless (bolp)
                (beginning-of-line))
              (when (= (point) begin)
                (forward-line 1))
              (setq end (point)))
            (let* ((returned-lines (count-lines begin end))
                   (end-line (and (> returned-lines 0)
                                  (+ start-line returned-lines -1)))
                   (has-more (< end (point-max)))
                   (next-line (and has-more end-line (1+ end-line)))
                   (line-range (if end-line
                                   (format "%d-%d" start-line end-line)
                                 "none"))
                   (header
                    (format "[read_file: source=%s; modified=%s; lines=%s; total_lines=%d; has_more=%s%s]\n"
                     source
                     (if modified "true" "false")
                     line-range
                     total-lines
                     (if has-more "true" "false")
                     (if next-line
                         (format "; next_start_line=%d" next-line)
                       ""))))
              (concat header
                      (buffer-substring-no-properties begin end)))))))))

(defun magent-tools--read-file (callback path &optional start-line line-count)
  "Read contents of file at PATH asynchronously.
When PATH has a visiting buffer, read its live contents.  Otherwise read the
file through a temporary buffer.  START-LINE is an optional one-based starting
line.  LINE-COUNT defaults to `magent-tools--read-file-default-line-count'.
The result includes pagination metadata before the requested file contents.
CALLBACK is called with the file contents or error message."
  (condition-case err
      (progn
        (unless (stringp path)
          (error "Missing required argument 'path' (got %S)" path))
        (when (eq start-line :null)
          (setq start-line nil))
        (when (eq line-count :null)
          (setq line-count nil))
        (unless (or (null start-line)
                    (and (integerp start-line) (> start-line 0)))
          (error "start_line must be a positive integer (got %S)" start-line))
        (unless (or (null line-count)
                    (and (integerp line-count) (> line-count 0)))
          (error "line_count must be a positive integer (got %S)" line-count))
        (let* ((path (magent-tools--resolve-path path))
               (first-line (or start-line 1))
               (maximum-lines
                (or line-count magent-tools--read-file-default-line-count)))
          (funcall
           callback
           (if-let* ((buffer (find-buffer-visiting path)))
               (with-current-buffer buffer
                 (magent-tools--read-file-buffer-page
                  first-line maximum-lines "live-buffer" (buffer-modified-p)))
             (with-temp-buffer
               (insert-file-contents path)
               (magent-tools--read-file-buffer-page
                first-line maximum-lines "temporary-buffer" nil))))))
    (error (funcall callback (format "Error reading file: %s" (error-message-string err))))))

(defun magent-tools--write-file (callback path content)
  "Write CONTENT to file at PATH asynchronously.
Creates parent directories if needed.
CALLBACK is called with success message or error."
  (condition-case err
      (progn
        (unless (stringp path)
          (error "Missing required argument 'path' (got %S)" path))
        (unless (stringp content)
          (error "Missing required argument 'content' (got %S)" content))
        (let ((path (magent-tools--resolve-path path)))
          (let ((dir (file-name-directory path)))
            (when (and dir (not (file-exists-p dir)))
              (make-directory dir t)))
          (with-temp-buffer
            (insert content)
            (write-region (point-min) (point-max) path nil 0))
          (funcall callback (format "Successfully wrote %s" path))))
    (error (funcall callback (format "Error writing file: %s" (error-message-string err))))))

(defun magent-tools--write-repo-summary
    (callback mode content &optional scope scope-files)
  "Write repository summary CONTENT in MODE and call CALLBACK.
SCOPE and SCOPE-FILES describe a scoped summary."
  (condition-case err
      (let* ((root (magent-tools--request-project-root))
             (result (magent-repo-summary-write
                      root mode content scope scope-files)))
        (funcall
         callback
         (format "Successfully %s repository summary: %s%s"
                 (if (plist-get result :created) "created" "updated")
                 (plist-get result :path)
                 (if-let* ((scope-id (plist-get result :scope-id)))
                     (format " (scope %s)" scope-id)
                   ""))))
    (error
     (funcall callback
              (format "Error writing repository summary: %s"
                      (error-message-string err))))))

(defun magent-tools--grep (callback pattern path &optional case-sensitive)
  "Search for PATTERN in files under PATH using ripgrep asynchronously.
If CASE-SENSITIVE is nil, performs case-insensitive search.
CALLBACK is called with matching lines or error message."
  (let (buf proc)
    (condition-case err
        (progn
          (unless (stringp pattern)
            (error "Missing required argument 'pattern' (got %S)" pattern))
          (unless (stringp path)
            (error "Missing required argument 'path' (got %S)" path))
          (let* ((resolved (magent-tools--resolve-path path))
                 (directory-p (file-directory-p resolved))
                 (default-directory
                  (if directory-p
                      (file-name-as-directory resolved)
                    (or (file-name-directory resolved)
                        (magent-tools--request-project-root))))
                 (target (if directory-p "." resolved))
                 (limit (max 1 magent-grep-max-matches))
                 (args (append
                        (list "--no-heading" "--line-number" "--color=never")
                        (unless case-sensitive (list "--ignore-case"))
                        (list "--" pattern target)))
                 (finished nil)
                 (truncated nil))
            (setq buf (generate-new-buffer " *magent-grep*"))
            (cl-labels
                ((finish
                  (process)
                  (unless finished
                    (setq finished t)
                    (let* ((output (if (buffer-live-p buf)
                                       (with-current-buffer buf (buffer-string))
                                     ""))
                           (exit-code (process-exit-status process))
                           (trimmed (string-trim-right output)))
                      (when (buffer-live-p buf)
                        (kill-buffer buf))
                      (funcall
                       callback
                       (cond
                        (truncated
                         (magent-tool-result-create
                          :status 'completed
                          :success t
                          :exit-code 0
                          :metadata (list :truncated t :limit limit)
                          :output (format "%s%s[results truncated after %d matches]"
                                          trimmed
                                          (if (string-empty-p trimmed) "" "\n")
                                          limit)))
                        ((= exit-code 0)
                         (magent-tool-result-create
                          :status 'completed
                          :success t
                          :exit-code exit-code
                          :output (if (string-blank-p output)
                                      "No matches found"
                                    trimmed)))
                        ((= exit-code 1)
                         (magent-tool-result-create
                          :status 'completed
                          :success t
                          :exit-code exit-code
                          :output "No matches found"))
                        (t
                         (let ((message
                                (if (string-blank-p output)
                                    (format "grep failed with exit code %d"
                                            exit-code)
                                  trimmed)))
                           (magent-tool-result-create
                            :status 'failed
                            :success nil
                            :exit-code exit-code
                            :output message
                            :error message)))))))))
              (setq proc
                    (make-process
                     :name "magent-grep"
                     :buffer buf
                     :command (cons magent-grep-program args)
                     :noquery t
                     :filter
                     (lambda (process chunk)
                       (when (buffer-live-p buf)
                         (with-current-buffer buf
                           (goto-char (point-max))
                           (insert chunk)
                           (save-excursion
                             (goto-char (point-min))
                             (forward-line limit)
                             (unless (eobp)
                               (setq truncated t)
                               (delete-region (point) (point-max))))))
                       (when (and truncated (process-live-p process))
                         (delete-process process)))
                     :sentinel
                     (lambda (process _event)
                       (when (memq (process-status process) '(exit signal))
                         (finish process)))))
              (magent-tools--register-cancel-cleanup
               (lambda ()
                 (when (and proc (process-live-p proc))
                   (delete-process proc))
                 (when (buffer-live-p buf)
                   (kill-buffer buf)))))))
      (error
       (when (buffer-live-p buf)
         (kill-buffer buf))
       (funcall callback
                (magent-tool-result-create
                 :status 'failed
                 :success nil
                 :output (format "grep failed: %s" (error-message-string err))
                 :error (error-message-string err)))))))

(defun magent-tools--glob-to-regexp (pattern)
  "Translate glob PATTERN to a regexp with distinct * and ** semantics."
  (let ((index 0)
        (length (length pattern))
        (regexp "\\`"))
    (while (< index length)
      (cond
       ((and (< (+ index 2) length)
             (eq (aref pattern index) ?*)
             (eq (aref pattern (1+ index)) ?*)
             (eq (aref pattern (+ index 2)) ?/))
        (setq regexp (concat regexp "\\(?:.*/\\)?")
              index (+ index 3)))
       ((and (< (1+ index) length)
             (eq (aref pattern index) ?*)
             (eq (aref pattern (1+ index)) ?*))
        (setq regexp (concat regexp ".*")
              index (+ index 2)))
       ((eq (aref pattern index) ?*)
        (setq regexp (concat regexp "[^/]*")
              index (1+ index)))
       ((eq (aref pattern index) ??)
        (setq regexp (concat regexp "[^/]")
              index (1+ index)))
       (t
        (setq regexp (concat regexp
                             (regexp-quote (char-to-string
                                            (aref pattern index))))
              index (1+ index)))))
    (concat regexp "\\'")))

(defun magent-tools--glob (callback pattern path)
  "Find files matching PATTERN under PATH asynchronously.
Supports * and ** wildcards.
CALLBACK is called with list of matching file paths."
  (condition-case err
      (let* ((resolved (magent-tools--resolve-path path))
             (search-root (if (file-directory-p resolved)
                              resolved
                            (or (file-name-directory resolved)
                                (magent-tools--request-project-root))))
             (normalized-pattern
              (string-remove-prefix "./" (subst-char-in-string ?\\ ?/ pattern)))
             (matches
              (if (string-match-p "\\*\\*" pattern)
                  (let ((regexp (magent-tools--glob-to-regexp
                                 normalized-pattern)))
                    (cl-remove-if-not
                     (lambda (file)
                       (let ((relative
                              (subst-char-in-string
                               ?\\ ?/ (file-relative-name file search-root))))
                         (string-match-p regexp relative)))
                     (directory-files-recursively search-root ".")))
                ;; Single * uses file-expand-wildcards
                (file-expand-wildcards
                 (expand-file-name normalized-pattern search-root) t))))
        (funcall callback (mapconcat #'identity (sort matches #'string<) "\n")))
    (error (funcall callback (format "Error during glob: %s" (error-message-string err))))))

(defun magent-tools--edit-file (callback path old-text new-text)
  "Edit file at PATH by replacing OLD-TEXT with NEW-TEXT asynchronously.
OLD-TEXT must match exactly once in the file.
CALLBACK is called with success message or error."
  (condition-case err
      (progn
        (unless (and (stringp old-text) (not (string-empty-p old-text)))
          (error "old_text must be a non-empty string"))
        (unless (stringp new-text)
          (error "new_text must be a string"))
        (let ((path (magent-tools--resolve-path path)))
        (let* ((content (with-temp-buffer
                          (insert-file-contents path)
                          (buffer-string)))
               (count (let ((start 0) (n 0))
                        (while (setq start (string-search old-text content start))
                          (cl-incf n)
                          (setq start (+ start (length old-text))))
                        n)))
          (cond
           ((= count 0)
            (funcall callback (format "Error: old_text not found in %s" path)))
           ((> count 1)
            (funcall callback (format "Error: old_text found %d times in %s (must be unique)" count path)))
           (t
            (let ((new-content (string-replace old-text new-text content)))
              (with-temp-buffer
                (insert new-content)
                (write-region (point-min) (point-max) path nil 0))
              (funcall callback (format "Successfully edited %s" path))))))))
    (error
     (funcall callback (format "Error editing file: %s"
                               (error-message-string err))))))

(defun magent-tools--emacs-eval (callback sexp &optional timeout)
  "Evaluate SEXP string as Emacs Lisp with optional TIMEOUT in seconds.
CALLBACK is called with the result as a readable string, or an error message.
Evaluation runs in the user's context buffer when known
\(see `magent-tools--request-context'), falling back to current buffer."
  (let ((debug-on-error nil)
        (debug-on-quit nil)
        (debug-on-signal nil))
    (condition-case err
      (let* ((timeout (or timeout magent-emacs-eval-timeout))
             (form (car (read-from-string sexp)))
             (cancelled nil)
             (completed nil)
             timer
             worker
             ;; Capture user's buffer at invocation time so the deferred
             ;; evaluator runs in the right context, not the magent output buffer.
             (ctx-buffer (when-let* ((buffer-name (magent-tools--origin-buffer-name)))
                           (get-buffer buffer-name))))
        (cl-labels
            ((finish (result)
               (unless completed
                 (setq completed t)
                 (when timer
                   (cancel-timer timer)
                   (setq timer nil))
                 (unless cancelled
                   (funcall callback result))))
             (interrupt-worker ()
               (when (and (fboundp 'thread-live-p)
                          worker
                          (thread-live-p worker))
                 (thread-signal worker 'quit nil))))
          (setq worker
                (if (fboundp 'make-thread)
                    ;; Run evaluation on a worker thread so abort/timeout can
                    ;; signal it even when the form is busy in Lisp.
                    (make-thread
                     (lambda ()
                       (let ((debug-on-error nil)
                             (debug-on-quit nil)
                             (debug-on-signal nil))
                         (condition-case worker-err
                             (let ((result
                                    (if (and ctx-buffer (buffer-live-p ctx-buffer))
                                        (with-current-buffer ctx-buffer
                                          (eval form t))
                                      (eval form t))))
                               (run-at-time 0 nil
                                            (lambda ()
                                              (finish (prin1-to-string result)))))
                           (quit
                            (run-at-time 0 nil
                                         (lambda ()
                                           (unless (or completed cancelled)
                                             (finish "Error: Evaluation interrupted")))))
                           (error
                            (run-at-time 0 nil
                                         (lambda ()
                                           (unless (or completed cancelled)
                                             (finish
                                              (format "Error evaluating sexp: %s"
                                                      (error-message-string worker-err))))))))))
                     "magent-emacs-eval")
                  (progn
                    (run-at-time
                     0 nil
                     (lambda ()
                       (let ((debug-on-error nil)
                             (debug-on-quit nil)
                             (debug-on-signal nil))
                         (condition-case sync-err
                             (let ((result
                                    (if (and ctx-buffer (buffer-live-p ctx-buffer))
                                        (with-current-buffer ctx-buffer
                                          (eval form t))
                                      (eval form t))))
                               (finish (prin1-to-string result)))
                           (quit
                            (unless (or completed cancelled)
                              (finish "Error: Evaluation interrupted")))
                           (error
                            (unless (or completed cancelled)
                              (finish
                               (format "Error evaluating sexp: %s"
                                       (error-message-string sync-err)))))))))
                    nil)))
          (when (and timeout (> timeout 0))
            (setq timer
                  (run-at-time
                   timeout nil
                   (lambda ()
                     (unless (or completed cancelled)
                       (interrupt-worker)
                       (finish "Error: Evaluation timed out"))))))
          (magent-tools--register-cancel-cleanup
           (lambda ()
             (setq cancelled t)
             (when timer
               (cancel-timer timer)
               (setq timer nil))
             (interrupt-worker)))))
      (error
       (funcall callback
                (format "Error evaluating sexp: %s"
                        (error-message-string err)))))))

(defun magent-tools--bash (callback command &optional timeout)
  "Execute shell COMMAND asynchronously with optional TIMEOUT in seconds.
CALLBACK is called with the command output (stdout + stderr)."
  (if (not (stringp command))
      (funcall callback "Error: 'command' argument is required but was not provided. Please call bash with a valid shell command string.")
    (let* ((timeout (or timeout magent-bash-timeout))
           (default-directory (magent-tools--request-project-root))
           (buf (generate-new-buffer " *magent-bash*"))
           (timer nil)
           (proc nil)
           (finished nil)
           (cleanup
            (lambda ()
              (when timer (cancel-timer timer) (setq timer nil))
              (when (process-live-p proc) (delete-process proc))
              (when (buffer-live-p buf) (kill-buffer buf)))))
      (magent-tools--register-cancel-cleanup cleanup)
      (setq timer
            (run-at-time
             timeout nil
             (lambda ()
               (unless finished
                 (setq finished t)
                 (when (process-live-p proc)
                   (delete-process proc))
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (let ((output (buffer-string)))
                       (funcall cleanup)
                       (funcall callback
                                (let ((message
                                       (if (string-blank-p output)
                                           "Command timed out with no output"
                                         (format
                                          "Command timed out. Partial output:\n%s"
                                          (string-trim-right output)))))
                                  (magent-tool-result-create
                                   :status 'failed
                                   :success nil
                                   :error message
                                   :output message
                                   :metadata (list :timeout t)))))))))))
      (condition-case err
          (let ((process-environment
                 (append '("PAGER=cat"
                           "GIT_PAGER=cat"
                           "MANPAGER=cat"
                           "SYSTEMD_PAGER=cat"
                           "GIT_TERMINAL_PROMPT=0"
                           "DEBIAN_FRONTEND=noninteractive")
                         process-environment)))
            (setq proc
                  (make-process
                   :name "magent-bash"
                   :buffer buf
                   :command (list shell-file-name shell-command-switch command)
                   :sentinel
                   (lambda (p _event)
                     (when (and (memq (process-status p) '(exit signal))
                                (not finished))
                       (setq finished t)
                       (let ((output (if (buffer-live-p buf)
                                         (with-current-buffer buf
                                           (buffer-string))
                                       ""))
                             (exit-code (process-exit-status p)))
                         (funcall cleanup)
                         (funcall callback
                                  (let* ((success (and (eq (process-status p) 'exit)
                                                       (= exit-code 0)))
                                         (message
                                          (if (string-blank-p output)
                                              (if success
                                                  "Command completed with no output"
                                                (format
                                                 "Command failed with exit code %d and no output"
                                                 exit-code))
                                            (string-trim-right output))))
                                    (magent-tool-result-create
                                     :status (if success 'completed 'failed)
                                     :success success
                                     :exit-code exit-code
                                     :output message
                                     :error (unless success message))))))))))
        (error
         (funcall cleanup)
         (funcall callback (format "Error starting process: %s"
                                   (error-message-string err))))))))

(defun magent-tools--parent-session ()
  "Return the parent session for a child-agent tool call."
  (or (and magent-tools--request-context
           (magent-request-context-approval-session
            magent-tools--request-context))
      (and magent-tools--request-context
           (magent-request-context-session magent-tools--request-context))
      (magent-session-get)))

(defun magent-tools--parent-scope ()
  "Return the parent request scope for a child-agent tool call."
  (or (and magent-tools--request-context
           (magent-request-context-scope magent-tools--request-context))
      (magent-session-current-scope)))

(defun magent-tools--render-agent-job-event
    (event job &optional detail context scope deferred)
  "Emit child-agent EVENT for JOB for optional deferred UI projection."
  (let ((emit (lambda ()
                (magent-lifecycle-events-emit
                 'agent-job-event
                 :event event
                 :job job
                 :detail detail
                 :scope scope
                 :ui-visible
                 (magent-request-context-ui-visible-p
                  (or context magent-tools--request-context))))))
      (if deferred
          (run-at-time 0 nil emit)
        (funcall emit))))

(defun magent-tools--persist-parent-session (&optional session scope)
  "Persist SESSION for SCOPE after child-agent job state changes."
  (when (and session
             (magent-session-get-messages session))
    (let ((previous-scope magent-session--current-scope)
          (previous-session magent--current-session)
          (target-scope (or scope (magent-tools--parent-scope))))
      (unwind-protect
          (progn
            (setq magent-session--current-scope target-scope
                  magent--current-session session)
            (magent-session-save))
        (setq magent-session--current-scope previous-scope
              magent--current-session previous-session)))))

(defun magent-tools--agent-depth (&optional context)
  "Return child-agent depth recorded in CONTEXT."
  (or (and context
           (magent-request-context-agent-depth context))
      0))

(defun magent-tools--child-agent-depth (&optional parent-context)
  "Return the depth a child of PARENT-CONTEXT would have."
  (1+ (magent-tools--agent-depth parent-context)))

(defun magent-tools--child-agent-depth-error (&optional parent-context)
  "Return a depth-limit error string when spawning should be blocked."
  (let ((limit magent-child-agent-max-depth)
        (child-depth (magent-tools--child-agent-depth parent-context)))
    (when (and (integerp limit)
               (> child-depth limit))
      (format "Error: child-agent max depth %d exceeded; recursive spawn_agent calls are disabled for this request"
              limit))))

(defun magent-tools--permission-profile-summary (permission)
  "Return a compact JSON-safe summary for PERMISSION."
  (let ((rules (cond
                ((magent-permission-p permission)
                 (magent-permission-rules permission))
                (permission permission)
                (t nil))))
    `((agent . ,(symbol-name (magent-permission-resolve rules 'agent)))
      (bash . ,(symbol-name (magent-permission-resolve rules 'bash)))
      (emacs_eval . ,(symbol-name
                      (magent-permission-resolve rules 'emacs_eval)))
      (read . ,(symbol-name (magent-permission-resolve rules 'read)))
      (write . ,(symbol-name (magent-permission-resolve rules 'write)))
      (edit . ,(symbol-name (magent-permission-resolve rules 'edit)))
      (wildcard . ,(symbol-name (magent-permission-resolve rules '*))))))

(defun magent-tools--effective-child-permission (parent-context agent)
  "Return AGENT permission restricted by PARENT-CONTEXT's profile.
The returned intersection preserves nested file/resource rules from both the
parent and child instead of flattening them to one decision per tool."
  (let ((parent-permission
         (and parent-context
              (magent-request-context-permission-profile parent-context)))
        (child-permission (magent-agent-info-permission agent)))
    (if (not parent-permission)
        child-permission
      (magent-permission-intersect parent-permission child-permission))))

(defun magent-tools--agent-model-name (model)
  "Return MODEL as a JSON-safe string."
  (cond
   ((null model) nil)
   ((symbolp model) (symbol-name model))
   (t (format "%s" model))))

(defun magent-tools--agent-backend-name (backend)
  "Return BACKEND as a JSON-safe name."
  (cond
   ((null backend) nil)
   ((and (fboundp 'gptel-backend-name)
         (ignore-errors (gptel-backend-name backend))))
   (t (format "%s" backend))))

(defun magent-tools--agent-inheritance-metadata
    (parent-context child-context agent child-session)
  "Return persisted inheritance metadata for a child-agent JOB."
  (let* ((capability-context
          (and parent-context
               (magent-request-context-capability-context parent-context)))
         (capability-skills
          (and (listp capability-context)
               (plist-get capability-context :skill-names))))
    `((scope . ,(and child-context
                     (magent-request-context-scope child-context)))
      (,(intern "project-root") . ,(and child-context
                                         (magent-request-context-project-root
                                          child-context)))
      (parent-request-id
       . ,(and parent-context
               (magent-request-context-id parent-context)))
      (agent-depth . ,(magent-tools--agent-depth child-context))
      (child-session-id . ,(magent-session-get-id child-session))
      (ui-visibility
       . ,(symbol-name
           (or (and child-context
                    (magent-request-context-ui-visibility child-context))
               'full)))
      (model . ,(magent-tools--agent-model-name
                 (and child-context
                      (magent-request-context-model child-context))))
      (backend . ,(magent-tools--agent-backend-name
                   (and child-context
                        (magent-request-context-backend child-context))))
      (temperature . ,(and child-context
                           (magent-request-context-temperature child-context)))
      (top-p . ,(and child-context
                     (magent-request-context-top-p child-context)))
      (effort . ,(and child-context
                     (magent-effort-option-string
                      (magent-request-context-effort child-context))))
      (skill-names . ,(vconcat
                       (or (and child-context
                                (magent-request-context-skill-names
                                 child-context))
                           capability-skills
                           nil)))
      (permission-profile
       . ,(magent-tools--permission-profile-summary
           (or (and child-context
                    (magent-request-context-permission-profile
                     child-context))
               (magent-agent-info-permission agent)))))))

(defun magent-tools--agent-job-terminal-p (job)
  "Return non-nil when JOB has reached a terminal lifecycle state."
  (memq (magent-agent-job-status job)
        '(completed failed closed cancelled)))

(defun magent-tools--agent-job-transcript (session)
  "Return a compact transcript for child SESSION."
  (mapcar
   (lambda (msg)
     `((role . ,(symbol-name (magent-msg-role msg)))
       (content . ,(let ((content (magent-msg-content msg)))
                     (if (stringp content)
                         content
                       (format "%S" content))))))
   (magent-session-get-messages session)))

(defun magent-tools--agent-job-status-string (job)
  "Return JOB status as a string."
  (symbol-name (magent-agent-job-status job)))

(defun magent-tools--agent-job-summary (job &optional include-prompt)
  "Return a JSON-safe summary alist for JOB.
When INCLUDE-PROMPT is non-nil, include a prompt preview."
  `((id . ,(magent-agent-job-id job))
    (agent . ,(magent-agent-job-agent-name job))
    (task_name . ,(magent-agent-job-task-name job))
    (status . ,(magent-tools--agent-job-status-string job))
    ,@(when include-prompt
        `((prompt_preview
           . ,(when-let* ((prompt (magent-agent-job-prompt job)))
                (truncate-string-to-width prompt 200 nil nil "...")))))
    (result . ,(magent-agent-job-result job))
    (error . ,(magent-agent-job-error job))
    (created_at . ,(magent-agent-job-created-at job))
    (updated_at . ,(magent-agent-job-updated-at job))))

(defun magent-tools--agent-job-result-json (payload)
  "Encode PAYLOAD as model-visible JSON."
  (let ((json-encoding-pretty-print nil))
    (json-encode payload)))

(defun magent-tools--agent-job-runtime (job-id)
  "Return runtime state for child-agent JOB-ID."
  (gethash job-id magent-tools--agent-job-runtimes))

(defun magent-tools--agent-job-put-runtime (job-id runtime)
  "Store RUNTIME for child-agent JOB-ID."
  (puthash job-id runtime magent-tools--agent-job-runtimes)
  runtime)

(defun magent-tools--agent-job-clear-runtime (job-id)
  "Remove runtime state for child-agent JOB-ID."
  (remhash job-id magent-tools--agent-job-runtimes))

(defun magent-tools--agent-job-ids (job-id job-ids)
  "Normalize JOB-ID and JOB-IDS arguments into a list of ids."
  (let (ids)
    (when (and (stringp job-id) (not (string-empty-p job-id)))
      (push job-id ids))
    (dolist (id (cond
                 ((vectorp job-ids) (append job-ids nil))
                 ((listp job-ids) job-ids)
                 ((and (stringp job-ids)
                       (not (string-empty-p job-ids)))
                  (split-string job-ids "[,[:space:]]+" t))
                 (t nil)))
      (when (and (stringp id) (not (string-empty-p id)))
        (push id ids)))
    (nreverse (cl-remove-duplicates ids :test #'equal :from-end t))))

(defun magent-tools--agent-jobs-for-ids (session ids)
  "Return child-agent jobs from SESSION matching IDS.
When IDS is nil, return all jobs in chronological creation order."
  (if ids
      (mapcar (lambda (id)
                (or (magent-session-agent-job session id)
                    (error "agent job '%s' not found" id)))
              ids)
    (reverse (magent-session-agent-jobs session))))

(defun magent-tools--agent-job-update-from-child
    (job child-session status response &optional error)
  "Update JOB from CHILD-SESSION with STATUS, RESPONSE, and optional ERROR."
  (setf (magent-agent-job-transcript job)
        (magent-tools--agent-job-transcript child-session))
  (magent-agent-job-set-status job status response error)
  job)

(defun magent-tools--agent-job-start
    (job agent prompt child-session parent-context parent-session)
  "Start JOB with AGENT and PROMPT using CHILD-SESSION.
Return the child loop handle when startup succeeds."
  (let* ((agent-name (magent-agent-info-name agent))
         (parent-scope (or (and parent-context
                                (magent-request-context-scope parent-context))
                           (magent-session-current-scope)))
         (title (if-let* ((task-name (magent-agent-job-task-name job)))
                    (format "Agent %s: %s" agent-name task-name)
                  (format "Agent %s" agent-name)))
         (subagent-context
          (let ((magent-lifecycle-events--subagent-audit-context
                 (magent-request-context-audit-snapshot parent-context)))
            (magent-lifecycle-events-create-subagent-context
             title
             (and parent-context
                  (magent-request-context-event-context parent-context)))))
         (effective-permission
          (magent-tools--effective-child-permission parent-context agent))
         (child-request-context
          (magent-request-context-create
           :id (magent-lifecycle-events-generate-id)
           :scope (and parent-context
                       (magent-request-context-scope parent-context))
           :session child-session
           :approval-session parent-session
           :origin-buffer-name (and parent-context
                                    (magent-request-context-origin-buffer-name
                                     parent-context))
           :origin-context (and parent-context
                                (magent-request-context-origin-context
                                 parent-context))
           :ui-visibility 'summary-only
           :parent-request-id (and parent-context
                                   (magent-request-context-id parent-context))
           :agent-depth (magent-tools--child-agent-depth parent-context)
           :project-root (and parent-context
                              (magent-request-context-project-root
                               parent-context))
           :model (and parent-context
                       (magent-request-context-model parent-context))
           :backend (and parent-context
                         (magent-request-context-backend parent-context))
           :temperature (and parent-context
                             (magent-request-context-temperature parent-context))
           :top-p (and parent-context
                       (magent-request-context-top-p parent-context))
           :effort (and parent-context
                        (magent-request-context-effort parent-context))
           :skill-names (and parent-context
                             (copy-sequence
                              (magent-request-context-skill-names
                               parent-context)))
           :capability-context (and parent-context
                                    (copy-tree
                                     (magent-request-context-capability-context
                                      parent-context)))
           :permission-profile effective-permission
           :live-p (and parent-context
                        (magent-request-context-live-p parent-context))
           :event-context subagent-context))
         child-loop)
    (magent-agent-job-set-status job 'running)
    (magent-tools--render-agent-job-event
     'started job prompt parent-context parent-scope t)
    (magent-tools--persist-parent-session parent-session parent-scope)
    (magent-tools--agent-job-put-runtime
     (magent-agent-job-id job)
     (list :session child-session
           :agent agent
           :request-context child-request-context
           :subagent-context subagent-context
           :loop nil))
    (condition-case err
        (progn
          (setq child-loop
                (magent-agent-process
                 prompt
                 (lambda (response)
                   (magent-lifecycle-events-stop-subagent subagent-context)
                   (let* ((success (magent-agent-result-success-p response))
                          (text (magent-agent-result-content-string response))
                          (failed (not success)))
                     (when (string-empty-p text)
                       (setq text "Error: child-agent request failed"))
                     (magent-tools--agent-job-update-from-child
                      job child-session
                      (if failed 'failed 'completed)
                      (unless failed text)
                      (when failed text))
                     (magent-tools--render-agent-job-event
                      (if failed 'failed 'completed)
                      job text parent-context parent-scope nil)
                     (magent-tools--persist-parent-session
                      parent-session parent-scope)))
                 agent
                 nil
                 subagent-context
                 (magent-request-context-origin-context child-request-context)
                 nil
                 nil
                 nil
                 child-request-context))
          (magent-tools--agent-job-put-runtime
           (magent-agent-job-id job)
           (list :session child-session
                 :agent agent
                 :request-context child-request-context
                 :subagent-context subagent-context
                 :loop child-loop))
          (magent-tools--register-cancel-cleanup
           (lambda ()
             (unless (magent-tools--agent-job-terminal-p job)
               (magent-lifecycle-events-stop-subagent subagent-context)
               (setf (magent-agent-job-transcript job)
                     (magent-tools--agent-job-transcript child-session))
               (magent-agent-job-set-status
                job 'cancelled nil "Parent request was aborted")
               (magent-tools--render-agent-job-event
                'cancelled job "Parent request was aborted"
                parent-context parent-scope nil)
               (magent-tools--persist-parent-session parent-session parent-scope)
               (when (and child-loop
                          (fboundp 'magent-agent-loop-p)
                          (magent-agent-loop-p child-loop))
                 (magent-agent-loop-abort child-loop))
               (magent-tools--agent-job-clear-runtime
                (magent-agent-job-id job)))))
          child-loop)
      (error
       (magent-lifecycle-events-stop-subagent subagent-context)
       (magent-tools--agent-job-update-from-child
        job child-session 'failed nil
        (format "Error: child-agent request failed: %s"
                (error-message-string err)))
       (magent-tools--render-agent-job-event
        'failed job (magent-agent-job-error job)
        parent-context parent-scope nil)
       (magent-tools--persist-parent-session parent-session parent-scope)
       (magent-tools--agent-job-clear-runtime (magent-agent-job-id job))
       nil))))

(defun magent-tools--spawn-agent (callback agent-name prompt &optional task-name)
  "Spawn a durable child-agent job using AGENT-NAME and PROMPT."
  (let ((agent (magent-agent-registry-get agent-name)))
    (cond
     ((null agent)
      (funcall callback (format "Error: agent '%s' not found" agent-name)))
     ((not (magent-agent-info-mode-p agent 'subagent))
      (funcall callback (format "Error: agent '%s' is not a subagent" agent-name)))
     ((not (and (stringp prompt) (not (string-empty-p prompt))))
      (funcall callback "Error: prompt is required"))
     (t
      (let* ((parent-context magent-tools--request-context)
             (parent-session (magent-tools--parent-session))
             (parent-scope (magent-tools--parent-scope))
             (child-session (magent-session-create :agent agent))
             (parent-session-id (magent-session-get-id parent-session))
             (depth-error (magent-tools--child-agent-depth-error
                           parent-context))
             (job (magent-agent-job-create
                   :parent-session-id parent-session-id
                   :agent-name agent-name
                   :task-name task-name
                   :prompt prompt
                   :metadata nil)))
        (magent-session-add-agent-job parent-session job)
        (if depth-error
            (progn
              (setf (magent-agent-job-metadata job)
                    `((scope . ,(and parent-context
                                     (magent-request-context-scope
                                      parent-context)))
                      (,(intern "project-root")
                       . ,(and parent-context
                               (magent-request-context-project-root
                                parent-context)))
                      (parent-request-id
                       . ,(and parent-context
                               (magent-request-context-id parent-context)))
                      (agent-depth
                       . ,(magent-tools--child-agent-depth parent-context))
                      (child-session-id
                       . ,(magent-session-get-id child-session))
                      (ui-visibility . "summary-only")
                      (max-depth . ,magent-child-agent-max-depth)))
              (magent-agent-job-set-status job 'failed nil depth-error)
              (magent-tools--render-agent-job-event
               'failed job depth-error parent-context parent-scope t)
              (magent-tools--persist-parent-session parent-session parent-scope))
          (let ((child-loop
                 (magent-tools--agent-job-start
                  job agent prompt child-session parent-context parent-session)))
            (when-let* ((runtime (magent-tools--agent-job-runtime
                                 (magent-agent-job-id job))))
              (setf (magent-agent-job-metadata job)
                    (magent-tools--agent-inheritance-metadata
                     parent-context
                     (plist-get runtime :request-context)
                     agent
                     child-session))
              (magent-tools--persist-parent-session
               parent-session parent-scope))
            child-loop))
        (funcall
         callback
         (magent-tools--agent-job-result-json
          `((status . ,(if (eq (magent-agent-job-status job) 'failed)
                           "failed"
                         "spawned"))
            (job . ,(magent-tools--agent-job-summary job t))))))))))

(defun magent-tools--send-agent-message (callback job-id message)
  "Send follow-up MESSAGE to child-agent JOB-ID."
  (let* ((parent-session (magent-tools--parent-session))
         (parent-context magent-tools--request-context)
         (parent-scope (magent-tools--parent-scope))
         (job (and parent-session
                   (magent-session-agent-job parent-session job-id)))
         (runtime (and job
                       (magent-tools--agent-job-runtime job-id))))
    (cond
     ((null job)
      (funcall callback (format "Error: agent job '%s' not found" job-id)))
     ((memq (magent-agent-job-status job) '(running queued))
      (funcall callback
               (format "Error: agent job '%s' is already running; wait before sending another message"
                       job-id)))
     ((memq (magent-agent-job-status job) '(closed cancelled))
      (funcall callback
               (format "Error: agent job '%s' is %s"
                       job-id (magent-tools--agent-job-status-string job))))
     ((not runtime)
      (funcall callback
               (format "Error: agent job '%s' has no live runtime; resume support is not available yet"
                       job-id)))
     ((not (and (stringp message) (not (string-empty-p message))))
      (funcall callback "Error: message is required"))
     (t
      (let ((agent (plist-get runtime :agent))
            (child-session (plist-get runtime :session)))
        (magent-tools--agent-job-start
         job agent message child-session
         parent-context parent-session)
        (when-let* ((runtime (magent-tools--agent-job-runtime job-id)))
          (setf (magent-agent-job-metadata job)
                (magent-tools--agent-inheritance-metadata
                 parent-context
                 (plist-get runtime :request-context)
                 agent
                 child-session))
          (magent-tools--persist-parent-session parent-session parent-scope))
        (funcall
         callback
         (magent-tools--agent-job-result-json
          `((status . "sent")
            (job . ,(magent-tools--agent-job-summary job))))))))))

(defun magent-tools--list-agents (callback &optional include-closed)
  "List child-agent jobs for the current parent session.
When INCLUDE-CLOSED is non-nil, include terminal closed/cancelled jobs."
  (let* ((session (magent-tools--parent-session))
         (jobs (reverse (magent-session-agent-jobs session)))
         (visible (if include-closed
                      jobs
                    (cl-remove-if
                     (lambda (job)
                       (memq (magent-agent-job-status job)
                             '(closed cancelled)))
                     jobs))))
    (funcall
     callback
     (magent-tools--agent-job-result-json
      `((status . "ok")
        (jobs . ,(vconcat
                  (mapcar (lambda (job)
                            (magent-tools--agent-job-summary job t))
                          visible))))))))

(defun magent-tools--wait-agent (callback &optional job-id job-ids timeout)
  "Wait for one or more child-agent jobs to reach a terminal state."
  (let* ((session (magent-tools--parent-session))
         (parent-context magent-tools--request-context)
         (parent-scope (magent-tools--parent-scope))
         (ids (magent-tools--agent-job-ids job-id job-ids))
         (timeout (or timeout 30)))
    (condition-case err
        (let* ((jobs (magent-tools--agent-jobs-for-ids session ids))
               (deadline (+ (float-time) (max 0 timeout)))
               timer
               done)
          (cl-labels
              ((finish
                (status)
                (unless done
                  (setq done t)
                  (when timer
                    (cancel-timer timer))
                  (dolist (job jobs)
                    (magent-tools--render-agent-job-event
                     (if (magent-tools--agent-job-terminal-p job)
                         'observed
                       'waiting)
                     job status parent-context parent-scope t))
                  (magent-tools--persist-parent-session session parent-scope)
                  (funcall
                   callback
                   (magent-tools--agent-job-result-json
                    `((status . ,status)
                      (jobs . ,(vconcat
                                (mapcar #'magent-tools--agent-job-summary
                                        jobs)))))))
                done)
               (ready-p
                ()
                (cl-every #'magent-tools--agent-job-terminal-p jobs))
               (poll
                ()
                (cond
                 ((ready-p) (finish "completed"))
                 ((>= (float-time) deadline) (finish "timeout")))))
            (if (or (ready-p) (<= timeout 0))
                (finish (if (ready-p) "completed" "timeout"))
              (setq timer (run-at-time 0.1 0.1 #'poll))
              (magent-tools--register-cancel-cleanup
               (lambda ()
                 (when timer
                   (cancel-timer timer)))))))
      (error
       (funcall callback
                (format "Error: wait_agent failed: %s"
                        (error-message-string err)))))))

(defun magent-tools--close-agent (callback job-id &optional close-reason)
  "Close child-agent JOB-ID and abort its live loop when present."
  (let* ((session (magent-tools--parent-session))
         (parent-context magent-tools--request-context)
         (parent-scope (magent-tools--parent-scope))
         (job (and session
                   (magent-session-agent-job session job-id)))
         (runtime (and job
                       (magent-tools--agent-job-runtime job-id))))
    (cond
     ((null job)
      (funcall callback (format "Error: agent job '%s' not found" job-id)))
     ((eq (magent-agent-job-status job) 'closed)
      (funcall
       callback
       (magent-tools--agent-job-result-json
        `((status . "already_closed")
          (job . ,(magent-tools--agent-job-summary job))))))
     (t
      (when-let* ((loop (and (memq (magent-agent-job-status job)
                                  '(queued running waiting))
                            (plist-get runtime :loop))))
        (when (and (fboundp 'magent-agent-loop-p)
                   (magent-agent-loop-p loop))
          (magent-agent-loop-abort loop)))
      (magent-agent-job-set-status
       job 'closed (magent-agent-job-result job)
       (or close-reason (magent-agent-job-error job)))
      (magent-tools--agent-job-clear-runtime job-id)
      (magent-tools--render-agent-job-event
       'closed job (or close-reason "closed") parent-context parent-scope t)
      (magent-tools--persist-parent-session session parent-scope)
      (funcall
       callback
       (magent-tools--agent-job-result-json
        `((status . "closed")
          (job . ,(magent-tools--agent-job-summary job)))))))))

(defun magent-tools--web-search (callback query &optional max-results)
  "Search the web using DuckDuckGo asynchronously.
CALLBACK is called with formatted search results or error message.
QUERY is the search string.
MAX-RESULTS is the maximum number of results to return (default 5)."
  (let ((max-results (or max-results 5))
        (url (format "https://html.duckduckgo.com/html/?q=%s"
                     (url-hexify-string query)))
        request-buffer)
    (condition-case err
        (progn
          (setq request-buffer
                (url-retrieve
                 url
                 (lambda (status)
                   (magent-tools--web-search-callback
                    status callback query max-results))
                 nil t t))
          (magent-tools--register-cancel-cleanup
           (lambda ()
             (when-let* ((proc (and request-buffer
                                   (get-buffer-process request-buffer))))
               (delete-process proc))
             (when (buffer-live-p request-buffer)
               (kill-buffer request-buffer)))))
      (error (funcall callback (format "Error initiating search: %s" (error-message-string err)))))))

(defun magent-tools--web-search-callback (status callback query max-results)
  "Handle HTTP response for web search.
STATUS is the url-retrieve status list.
CALLBACK is called with formatted results.
QUERY is the original search query.
MAX-RESULTS is the maximum number of results."
  (let ((url-buffer (current-buffer)))
    (unwind-protect
        (condition-case err
            (let ((error-status (plist-get status :error)))
              (if error-status
                  (funcall callback (format "HTTP error: %s" error-status))
                (goto-char (point-min))
                (when (re-search-forward "\r?\n\r?\n" nil t)
                  (let* ((html (libxml-parse-html-region (point) (point-max)))
                         (results (magent-tools--parse-ddg-results html max-results)))
                    (if results
                        (funcall callback (magent-tools--format-search-results query results))
                      (funcall callback (format "No results found for: %s" query)))))))
          (error (funcall callback (format "Error parsing results: %s" (error-message-string err)))))
      (when (buffer-live-p url-buffer)
        (kill-buffer url-buffer)))))

(defun magent-tools--parse-ddg-results (dom max-results)
  "Parse DuckDuckGo HTML DOM and extract search results.
Returns list of plists with :title and :url keys, limited to MAX-RESULTS."
  (let ((results nil)
        (count 0))
    (dolist (result (dom-by-class dom "result__a"))
      (when (< count max-results)
        (let ((title (magent-tools--dom-inner-text result))
              (url (dom-attr result 'href)))
          (when (and title url (not (string-blank-p title)))
            (push (list :title (string-trim title) :url url) results)
            (cl-incf count)))))
    (nreverse results)))

(defun magent-tools--format-search-results (query results)
  "Format RESULTS list into readable string for QUERY."
  (concat (format "Search results for \"%s\":\n\n" query)
          (cl-loop for result in results
                   for i from 1
                   collect (format "%d. %s\n   %s"
                                   i
                                   (plist-get result :title)
                                   (plist-get result :url))
                   into parts
                   finally return (mapconcat #'identity parts "\n\n"))))

;;; gptel-tool registrations

(require 'gptel)

(defconst magent-tools--reason-arg
  '(:name "reason"
    :type string
    :description "Brief reason for this tool call (shown in UI)"
    :optional t)
  "Display-only arg appended to every tool's :args list.
The value is shown in the UI but stripped before the tool function is called.
See `magent-agent-loop-filter-display-args'.")

(defvar magent-tools--read-file-tool
  (gptel-make-tool
   :name "read_file"
   :description "Read a bounded page of file contents. If Emacs already has a visiting buffer, returns its live contents including unsaved edits; otherwise reads from disk without leaving a file buffer. Every result reports source, modified state, actual line range, total_lines, has_more, and next_start_line. Continue with next_start_line when has_more is true; do not use shell commands such as wc or sed to determine file size or pagination."
   :args (list '(:name "path"
                       :type string
                       :description "Absolute or relative path to the file")
               '(:name "start_line"
                       :type integer
                       :description "One-based line at which to start reading; defaults to 1"
                       :optional t)
               '(:name "line_count"
                       :type integer
                       :description "Requested maximum number of lines; defaults to a bounded 200-line page and may return fewer to stay within the result budget"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--read-file
   :async t
   :category "magent")
  "gptel-tool struct for read_file.")

(defvar magent-tools--write-file-tool
  (gptel-make-tool
   :name "write_file"
   :description "Write content to a file. Creates parent directories if they don't exist."
   :args (list '(:name "path"
                       :type string
                       :description "Absolute or relative path to the file")
               '(:name "content"
                       :type string
                       :description "The full content to write to the file")
               magent-tools--reason-arg)
   :function #'magent-tools--write-file
   :async t
   :confirm t
   :category "magent")
  "gptel-tool struct for write_file.")

(defvar magent-tools--write-repo-summary-tool
  (gptel-make-tool
   :name "write_repo_summary"
   :description "Create or update the current Git repository's single canonical org-roam summary note. Use only for /summarize. The tool controls the filename, exact repository title, Org ID, Git metadata, and full/scoped subtree upserts. Pass an Org fragment, not a complete document. Do not include the managed heading itself: full-mode headings must start at **, and scoped-mode headings must start at ***."
   :args (list '(:name "mode"
                       :type string
                       :enum ["full" "scoped"]
                       :description "full for the whole repository, scoped for a requested subset")
               '(:name "content"
                       :type string
                       :description "Org fragment placed below the managed summary heading")
               '(:name "scope"
                       :type string
                       :description "Original user scope query; required for scoped mode"
                       :optional t)
               '(:name "scope_files"
                       :type array
                       :items (:type string)
                       :description "Repository-relative files resolved for scoped mode"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--write-repo-summary
   :async t
   :confirm t
   :category "magent")
  "gptel-tool struct for write_repo_summary.")

(defvar magent-tools--edit-file-tool
  (gptel-make-tool
   :name "edit_file"
   :description "Edit a file by replacing an exact text match. The old_text must appear exactly once in the file. Use this for precise, surgical edits instead of rewriting entire files."
   :args (list '(:name "path"
                       :type string
                       :description "Absolute or relative path to the file")
               '(:name "old_text"
                       :type string
                       :minLength 1
                       :description "The exact text to find and replace (must match exactly once)")
               '(:name "new_text"
                       :type string
                       :description "The text to replace old_text with")
               magent-tools--reason-arg)
   :function #'magent-tools--edit-file
   :async t
   :confirm t
   :category "magent")
  "gptel-tool struct for edit_file.")

(defvar magent-tools--grep-tool
  (gptel-make-tool
   :name "grep"
   :description "Search for a regex pattern in files under a directory using ripgrep (rg). Respects .gitignore. Returns matching lines with file paths and line numbers."
   :args (list '(:name "pattern"
                       :type string
                       :description "Regex pattern to search for")
               '(:name "path"
                       :type string
                       :description "Directory or file path to search in")
               '(:name "case_sensitive"
                       :type boolean
                       :description "Whether the search is case-sensitive"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--grep
   :async t
   :category "magent")
  "gptel-tool struct for grep.")

(defvar magent-tools--glob-tool
  (gptel-make-tool
   :name "glob"
   :description "Find files matching a glob pattern. Supports * and ** wildcards."
   :args (list '(:name "pattern"
                       :type string
                       :description "Glob pattern, e.g. *.el or **/*.ts")
               '(:name "path"
                       :type string
                       :description "Root directory to search in")
               magent-tools--reason-arg)
   :function #'magent-tools--glob
   :async t
   :category "magent")
  "gptel-tool struct for glob.")

(defvar magent-tools--bash-tool
  (gptel-make-tool
   :name "bash"
   :description "Execute a shell command. Use for running tests, builds, git operations, etc."
   :args (list '(:name "command"
                       :type string
                       :description "Shell command to execute")
               '(:name "timeout"
                       :type integer
                       :description "Timeout in seconds, defaults to 30"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--bash
   :async t
   :confirm t
   :category "magent")
  "gptel-tool struct for bash.")

(defvar magent-tools--emacs-eval-tool
  (gptel-make-tool
   :name "emacs_eval"
   :description "Evaluate an Emacs Lisp expression. Returns the result as a string. Use for buffer inspection, Emacs state queries, running compilation, navigating code with xref, etc. When you need multiple pieces of Emacs state, batch them in a single call using (let ...) or (list ...) rather than making separate calls — but only after you have gathered enough context to know what you need."
   :args (list '(:name "sexp"
                       :type string
                       :description "Emacs Lisp s-expression to evaluate")
               '(:name "timeout"
                       :type integer
                       :description "Timeout in seconds, defaults to 10"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--emacs-eval
   :async t
   :confirm t
   :category "magent")
  "gptel-tool struct for emacs_eval.")

(defvar magent-tools--spawn-agent-tool
  (gptel-make-tool
   :name "spawn_agent"
   :description "Start a durable child-agent job. Use explore for focused codebase search and general for broader multi-step work. Returns a stable job id that can be listed, waited on, messaged, or closed."
   :args (list '(:name "agent"
                       :type string
                       :description "Name of the subagent to start (e.g. 'explore', 'general')")
               '(:name "prompt"
                       :type string
                       :description "Task description for the child agent")
               '(:name "task_name"
                       :type string
                       :description "Short task name used to identify this child job"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--spawn-agent
   :async t
   :category "magent")
  "gptel-tool struct for spawn_agent.")

(defvar magent-tools--send-agent-message-tool
  (gptel-make-tool
   :name "send_agent_message"
   :description "Send follow-up input to an existing child-agent job after it has completed or failed and still has live runtime state."
   :args (list '(:name "job_id"
                       :type string
                       :description "Child-agent job id returned by spawn_agent")
               '(:name "message"
                       :type string
                       :description "Follow-up instruction for the child agent")
               magent-tools--reason-arg)
   :function #'magent-tools--send-agent-message
   :async t
   :category "magent")
  "gptel-tool struct for send_agent_message.")

(defvar magent-tools--wait-agent-tool
  (gptel-make-tool
   :name "wait_agent"
   :description "Wait for one or more child-agent jobs to finish and return their current status and results. Omit job ids to wait for all current child jobs."
   :args (list '(:name "job_id"
                       :type string
                       :description "Single child-agent job id to wait for"
                       :optional t)
               '(:name "job_ids"
                       :type array
                       :description "Multiple child-agent job ids to wait for"
                       :optional t)
               '(:name "timeout"
                       :type integer
                       :description "Maximum seconds to wait, defaults to 30"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--wait-agent
   :async t
   :category "magent")
  "gptel-tool struct for wait_agent.")

(defvar magent-tools--list-agents-tool
  (gptel-make-tool
   :name "list_agents"
   :description "List durable child-agent jobs for the current session with their ids, task names, status, and results."
   :args (list '(:name "include_closed"
                       :type boolean
                       :description "When true, include closed and cancelled jobs"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--list-agents
   :async t
   :category "magent")
  "gptel-tool struct for list_agents.")

(defvar magent-tools--close-agent-tool
  (gptel-make-tool
   :name "close_agent"
   :description "Close a child-agent job and abort its live request if it is still running."
   :args (list '(:name "job_id"
                       :type string
                       :description "Child-agent job id to close")
               '(:name "close_reason"
                       :type string
                       :description "Optional reason for closing the job"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--close-agent
   :async t
   :category "magent")
  "gptel-tool struct for close_agent.")

(defun magent-tools--skill-invoke (callback skill-name operation &rest args)
  "Invoke OPERATION from SKILL-NAME with ARGS asynchronously.
CALLBACK is called with the result.
Only works for tool-type skills.  Instruction-type skills are
injected separately only when explicitly selected or activated by the
capability resolver."
  (require 'magent-skills)
  (magent-skills-invoke skill-name operation args callback))

(defvar magent-tools--skill-invoke-tool
  (gptel-make-tool
   :name "skill_invoke"
   :description "Invoke a tool-type skill operation. Instruction-type skills cannot be invoked by this tool; they enter the system prompt only when explicitly selected or activated by the capability resolver. Use only a known tool-type skill and operation."
   :args (list '(:name "skill_name"
                       :type string
                       :description "Name of the skill to invoke")
               '(:name "operation"
                       :type string
                       :description "Operation to perform (varies by skill)")
               '(:name "args"
                       :type array
                       :description "Arguments for the operation (varies by operation)"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--skill-invoke
   :async t
   :category "magent")
  "gptel-tool struct for skill_invoke.")

(defvar magent-tools--web-search-tool
  (gptel-make-tool
   :name "web_search"
   :description "Search the web using DuckDuckGo for current external information, documentation, or online resources. Returns result titles and URLs only; it does not fetch result pages, article text, or snippets. Use returned links for discovery and do not claim to have read page content that this tool did not return. Prefer official documentation, specifications, upstream repositories, and release notes when available."
   :args (list '(:name "query"
                       :type string
                       :description "Search query string")
               '(:name "max_results"
                       :type integer
                       :description "Maximum number of results to return (default 5)"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--web-search
   :async t
   :category "magent")
  "gptel-tool struct for web_search.")

;;; Tool filtering by agent permissions

(defvar magent-tools--name-to-permission-key
  '(("read_file"    . read)
    ("write_file"   . write)
    ("write_repo_summary" . write)
    ("edit_file"    . edit)
    ("grep"         . grep)
    ("glob"         . glob)
    ("bash"         . bash)
    ("emacs_eval"   . emacs_eval)
    ("spawn_agent"  . agent)
    ("send_agent_message" . agent)
    ("wait_agent"   . agent)
    ("list_agents"  . agent)
    ("close_agent"  . agent)
    ("skill_invoke" . skill)
    ("web_search"   . web_search))
  "Maps gptel tool names to magent permission key symbols.")

(defun magent-tools-permission-key (tool-name)
  "Return the permission key symbol for TOOL-NAME, or nil if unknown."
  (let ((name (if (symbolp tool-name)
                  (symbol-name tool-name)
                tool-name)))
    (cdr (assoc name magent-tools--name-to-permission-key))))

(defvar magent-tools--all-gptel-tools
  (list magent-tools--read-file-tool
        magent-tools--write-file-tool
        magent-tools--write-repo-summary-tool
        magent-tools--edit-file-tool
        magent-tools--grep-tool
        magent-tools--glob-tool
        magent-tools--bash-tool
        magent-tools--emacs-eval-tool
        magent-tools--spawn-agent-tool
        magent-tools--send-agent-message-tool
        magent-tools--wait-agent-tool
        magent-tools--list-agents-tool
        magent-tools--close-agent-tool
        magent-tools--skill-invoke-tool
        magent-tools--web-search-tool)
  "All magent tools as gptel-tool structs.")

(defun magent-tools-get-gptel-tools-for-permission (permission)
  "Return gptel tools exposed by explicit PERMISSION profile.
Global `magent-enable-tools' filtering still applies.  Tools whose effective
decision is \\='ask remain exposed so the orchestrator can request approval."
  (cl-remove-if-not
   (lambda (tool)
     (let* ((tool-name (gptel-tool-name tool))
            (perm-key (magent-tools-permission-key tool-name)))
       (and
        (or (null perm-key) (memq perm-key magent-enable-tools))
        (or (null permission)
            (magent-permission-tool-available-p permission perm-key)))))
   magent-tools--all-gptel-tools))

(provide 'magent-tools)
;;; magent-tools.el ends here
