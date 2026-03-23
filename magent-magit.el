;;; magent-magit.el --- Safe Magit integration for Magent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai, vc
;; Package-Requires: ((emacs "28.1") (magit "4.0") (gptel "0.9.8"))

;;; Commentary:

;; This package replaces the tiny gptel-magit integration with a request-aware
;; Magit bridge for Magent.  The key design choice is to treat each LLM call as
;; a first-class request object that owns:
;;
;; - the originating repository
;; - the diff snapshot sent to the model
;; - the target buffer that may be updated later
;; - the baseline commit message text at dispatch time
;;
;; That lets us reject stale callbacks instead of blindly inserting text into
;; whichever commit buffer happens to exist when the response arrives.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'magit)
(require 'git-commit)

(declare-function markdown-view-mode "markdown-mode")
(declare-function gfm-view-mode "markdown-mode")

(defgroup magent-magit nil
  "Magit integration for Magent."
  :group 'magent
  :prefix "magent-magit-")

(defconst magent-magit--default-commit-prompt
  (concat
   "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes.

The commit message should be structured as follows:

    <type>(<optional scope>): <description>

    [optional body]

- Commits MUST be prefixed with a type, which consists of one of the followings words: build, chore, ci, docs, feat, fix, perf, refactor, style, test
- The type feat MUST be used when a commit adds a new feature
- The type fix MUST be used when a commit represents a bug fix
- An optional scope MAY be provided after a type. A scope is a phrase describing a section of the codebase enclosed in parenthesis, e.g., fix(parser):
- A description MUST immediately follow the type/scope prefix. The description is a short description of the code changes, e.g., fix: array parsing issue when multiple spaces were contained in string.
- Try to limit the whole subject line to 60 characters
- Capitalize the subject line
- Do not end the subject line with any punctuation
- A longer commit body MAY be provided after the short description, providing additional contextual information about the code changes. The body MUST begin one blank line after the description.
- Use the imperative mood in the subject line
- Keep the body short and concise (omit it entirely if not useful)"
   "A prompt adapted from Conventional Commits (https://www.conventionalcommits.org/en/v1.0.0/)."
   ;; "You write Git commit messages from staged diffs.\n\n"
   ;; "Return only the commit message.  Do not wrap it in code fences.  Do not "
   ;; "add prefaces such as \"Here is the commit message\".\n\n"
   ;; "Write a concise subject line.  Add a body only when it carries real "
   ;; "review value.  Keep the body short and factual.\n\n"
   ;; "Prefer Conventional Commit style when the change naturally fits it:\n"
   ;; "<type>(<optional-scope>): <description>\n\n"
   ;; "Use imperative mood.  Do not end the subject line with punctuation."
   )
  "Default system prompt for commit message generation.")

(defconst magent-magit--default-diff-explain-prompt
  (concat
   "You explain Git diffs for a developer reading them in Magit.\n\n"
   "Answer in Markdown.  Focus on intent, behavior changes, risks, and any "
   "missing follow-up work.  When the diff is ambiguous, say what is certain "
   "and what is inference.")
  "Default system prompt for diff explanation.")

(defcustom magent-magit-commit-prompt
  magent-magit--default-commit-prompt
  "System prompt used for commit message generation."
  :type 'string
  :group 'magent-magit)

(defcustom magent-magit-diff-explain-prompt
  magent-magit--default-diff-explain-prompt
  "System prompt used for explaining the current diff selection."
  :type 'string
  :group 'magent-magit)

(custom-declare-variable
 'magent-magit-model nil
 "Model override for Magent Magit requests.

When nil, use the global default value of `gptel-model'."
 :type (get 'gptel-model 'custom-type)
 :group 'magent-magit)

(custom-declare-variable
 'magent-magit-backend nil
 "Backend override for Magent Magit requests.

When nil, use the global default value of `gptel-backend'."
 :type (get 'gptel-backend 'custom-type)
 :group 'magent-magit)

(defcustom magent-magit-max-diff-chars 120000
  "Maximum diff payload size sent to the model.

Large diffs are truncated after this many characters, but a stat/summary block
is always kept ahead of the patch."
  :type 'integer
  :group 'magent-magit)

(defcustom magent-magit-commit-buffer-wait-seconds 2.0
  "How long `magent-magit-commit-create' waits for the commit buffer."
  :type 'number
  :group 'magent-magit)

(defcustom magent-magit-auto-install t
  "Whether to install Magit transient/keymap entries automatically."
  :type 'boolean
  :group 'magent-magit)

(defcustom magent-magit-explain-buffer-name "*magent-magit explain*"
  "Buffer used to display diff explanations."
  :type 'string
  :group 'magent-magit)

(defcustom magent-magit-preview-buffer-name "*magent-magit preview*"
  "Buffer used to display commit messages that were not applied automatically."
  :type 'string
  :group 'magent-magit)

(defcustom magent-magit-commit-transient-key "g"
  "Key used in `magit-commit' for AI-assisted commit drafting."
  :type 'string
  :group 'magent-magit)

(defcustom magent-magit-diff-transient-key "e"
  "Key used in `magit-diff' for AI diff explanation."
  :type 'string
  :group 'magent-magit)

(defcustom magent-magit-commit-buffer-key (kbd "C-c C-g")
  "Key used in `git-commit-mode' to draft a message with AI."
  :type 'key-sequence
  :group 'magent-magit)

(defcustom magent-magit-cancel-key (kbd "C-c M-k")
  "Key used in `git-commit-mode' to cancel the active AI request."
  :type 'key-sequence
  :group 'magent-magit)

(cl-defstruct (magent-magit-request
               (:constructor magent-magit-request-create)
               (:copier nil))
  "State owned by a single Magent/Magit request."
  id
  kind
  repo-root
  source-buffer
  target-buffer
  request-buffer
  snapshot
  baseline-text
  baseline-tick
  status
  applied-callback)

(defvar magent-magit--installed nil
  "Non-nil once Magent Magit bindings have been installed.")

(defvar magent-magit--request-counter 0
  "Monotonic counter used to build request ids.")

(defvar magent-magit--live-requests (make-hash-table :test 'equal)
  "Table of live request objects keyed by request id.")

(defvar-local magent-magit--active-request-id nil
  "Request id currently owned by this buffer, if any.")

(defvar-local magent-magit--generated-message nil
  "Last commit message inserted by `magent-magit'.")

(defun magent-magit--next-request-id ()
  "Return a fresh Magent Magit request id."
  (format "magent-magit-%06d" (cl-incf magent-magit--request-counter)))

(defun magent-magit--default-backend ()
  "Return the backend to use for this request."
  (or magent-magit-backend
      (default-value 'gptel-backend)))

(defun magent-magit--default-model ()
  "Return the model to use for this request."
  (or magent-magit-model
      (default-value 'gptel-model)))

(defun magent-magit--repo-root (&optional buffer)
  "Return repository root for BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or (magit-toplevel)
        (user-error "Not inside a Git repository"))))

(defun magent-magit--current-branch (repo-root)
  "Return a readable branch or commit identifier for REPO-ROOT."
  (let ((default-directory repo-root))
    (or (magit-git-string "symbolic-ref" "--quiet" "--short" "HEAD")
        (magit-git-string "rev-parse" "--short" "HEAD")
        "HEAD")))

(defun magent-magit--truncate-diff (diff)
  "Truncate DIFF when it is larger than `magent-magit-max-diff-chars'."
  (if (and magent-magit-max-diff-chars
           (> (length diff) magent-magit-max-diff-chars))
      (concat (substring diff 0 magent-magit-max-diff-chars)
              (format
               "\n\n[diff truncated after %d characters by magent-magit]\n"
               magent-magit-max-diff-chars))
    diff))

(defun magent-magit--capture-staged-snapshot (repo-root)
  "Capture staged diff state for REPO-ROOT."
  (let* ((default-directory repo-root)
         (repo-name (file-name-nondirectory
                     (directory-file-name repo-root)))
         (branch (magent-magit--current-branch repo-root))
         (summary (magit-git-output "diff" "--cached"
                                    "--stat=80,120"
                                    "--summary"
                                    "--no-ext-diff"
                                    "--no-color"
                                    "--submodule=diff"
                                    "-M"))
         (patch (magit-git-output "diff" "--cached"
                                  "--patch"
                                  "--no-ext-diff"
                                  "--no-color"
                                  "--submodule=diff"
                                  "-M")))
    (unless (string-match-p "[^[:space:]]" patch)
      (user-error "No staged changes to summarize"))
    (list :repo-root repo-root
          :repo-name repo-name
          :branch branch
          :summary (string-trim-right (or summary ""))
          :patch (string-trim-right patch)
          :payload
          (string-join
           (delq nil
                 (list
                  (format "Repository: %s" repo-name)
                  (format "Branch: %s" branch)
                  (when (string-match-p "[^[:space:]]" (or summary ""))
                    (format "Staged change summary:\n%s"
                            (string-trim-right summary)))
                  (format "Staged patch:\n%s"
                          (magent-magit--truncate-diff patch))))
           "\n\n"))))

(defun magent-magit--capture-diff-snapshot ()
  "Capture the current Magit diff selection as a stable text snapshot."
  (unless (derived-mode-p 'magit-mode)
    (user-error "Not in a Magit buffer"))
  (let* ((repo-root (magent-magit--repo-root))
         (section (magit-current-section))
         (scope (magit-diff-scope section t))
         (diff-type (magit-diff-type section))
         (regionp (eq scope 'region))
         (owner-section (if regionp section section))
         (raw-text (cond
                    ((null scope)
                     (user-error "Point is not on a diff or hunk section"))
                    (regionp
                     ;; When Magit reports a region, explain the enclosing hunk.
                     (buffer-substring-no-properties
                      (oref owner-section start)
                      (oref owner-section end)))
                    (t
                     (buffer-substring-no-properties
                      (oref owner-section start)
                      (oref owner-section end))))))
    (unless (string-match-p "[^[:space:]]" raw-text)
      (user-error "No diff text available at point"))
    (list :repo-root repo-root
          :repo-name (file-name-nondirectory (directory-file-name repo-root))
          :branch (magent-magit--current-branch repo-root)
          :scope scope
          :diff-type diff-type
          :region-derived regionp
          :text (string-trim-right raw-text)
          :payload
          (string-join
           (list
            (format "Repository: %s"
                    (file-name-nondirectory (directory-file-name repo-root)))
            (format "Branch: %s" (magent-magit--current-branch repo-root))
            (format "Scope: %s%s"
                    scope
                    (if regionp " (explaining enclosing hunk)" ""))
            (format "Diff type: %s" diff-type)
            "Diff snapshot:"
            (magent-magit--truncate-diff raw-text))
           "\n\n"))))

(defun magent-magit--find-commit-buffer (repo-root)
  "Return the live commit buffer associated with REPO-ROOT, if any."
  (cl-find-if
   (lambda (buffer)
     (with-current-buffer buffer
       (and git-commit-mode
            (ignore-errors
              (equal (magit-toplevel) repo-root)))))
   (buffer-list)))

(defun magent-magit--wait-for-commit-buffer (repo-root callback &optional remaining)
  "Call CALLBACK with the commit buffer for REPO-ROOT once it exists.
REMAINING tracks how many seconds remain before timing out."
  (let ((remaining (or remaining magent-magit-commit-buffer-wait-seconds)))
    (if-let ((buffer (magent-magit--find-commit-buffer repo-root)))
        (funcall callback buffer)
      (if (<= remaining 0)
          (message "magent-magit: Timed out waiting for commit buffer in %s"
                   repo-root)
        (run-at-time 0.05 nil
                     #'magent-magit--wait-for-commit-buffer
                     repo-root callback (- remaining 0.05))))))

(defun magent-magit--request-for-buffer (&optional buffer)
  "Return the active request owned by BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (and magent-magit--active-request-id
         (gethash magent-magit--active-request-id
                  magent-magit--live-requests))))

(defun magent-magit--register-request (request)
  "Register REQUEST as live and mark its owner buffer."
  (puthash (magent-magit-request-id request)
           request
           magent-magit--live-requests)
  (when-let ((buffer (magent-magit-request-target-buffer request)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local magent-magit--active-request-id
                    (magent-magit-request-id request)))))
  request)

(defun magent-magit--cleanup-request (request)
  "Remove REQUEST from live state and kill its request buffer."
  (remhash (magent-magit-request-id request) magent-magit--live-requests)
  (when-let ((buffer (magent-magit-request-target-buffer request)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (equal magent-magit--active-request-id
                     (magent-magit-request-id request))
          (setq-local magent-magit--active-request-id nil)))))
  (when-let ((request-buffer (magent-magit-request-request-buffer request)))
    (when (buffer-live-p request-buffer)
      (kill-buffer request-buffer))))

(defun magent-magit--request-error-string (info)
  "Extract a readable error message from gptel INFO."
  (or (plist-get info :status)
      (when-let ((error-data (plist-get info :error)))
        (if (stringp error-data)
            error-data
          (format "%s" error-data)))
      "Request failed"))

(defun magent-magit--display-text-buffer (buffer-name text &optional markdown)
  "Show TEXT in BUFFER-NAME.
When MARKDOWN is non-nil, prefer a markdown viewing mode when available."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (cond
         ((and markdown (fboundp 'markdown-view-mode))
          (markdown-view-mode))
         ((and markdown (fboundp 'gfm-view-mode))
          (gfm-view-mode)
          (view-mode 1))
         (t
          (text-mode)
          (view-mode 1)))))
    (pop-to-buffer buffer)
    buffer))

(defun magent-magit--show-commit-preview (request message reason)
  "Display MESSAGE for REQUEST in a preview buffer with REASON."
  (magent-magit--display-text-buffer
   magent-magit-preview-buffer-name
   (string-join
    (list
     "Magent generated a commit message but did not apply it automatically."
     ""
     (format "Reason: %s" reason)
     (format "Repository: %s" (magent-magit-request-repo-root request))
     (format "Request: %s" (magent-magit-request-id request))
     ""
     message)
    "\n")))

(defun magent-magit--unwrap-code-fence (text)
  "Remove a single surrounding fenced code block from TEXT."
  (let* ((trimmed (string-trim text))
         (lines (split-string trimmed "\n")))
    (if (and (>= (length lines) 2)
             (string-prefix-p "```" (string-trim (car lines)))
             (string= "```" (string-trim (car (last lines)))))
        (string-join (butlast (cdr lines)) "\n")
      trimmed)))

(defun magent-magit--normalize-commit-message (text)
  "Normalize model TEXT into a plain commit message string."
  (let* ((unfenced (magent-magit--unwrap-code-fence text))
         (trimmed (string-trim unfenced))
         (lines (split-string trimmed "\n"))
         (lines (if (and lines
                         (string-match-p
                          "\\`\\(?:Suggested \\)?Commit message:?\\'"
                          (string-trim (car lines))))
                    (cdr lines)
                  lines)))
    (string-trim-right (string-join lines "\n"))))

(defun magent-magit--commit-message-region ()
  "Return the region containing the editable commit message body."
  (save-excursion
    (goto-char (point-min))
    (let* ((comment-char (or comment-start "#"))
           (comment-re (concat "^" (regexp-quote comment-char)))
           (cut-re (format "^%s -\\{8,\\} >8 -\\{8,\\}$"
                           (regexp-quote comment-char)))
           (end (point-max)))
      (when (re-search-forward cut-re nil t)
        (setq end (line-beginning-position)))
      (goto-char (point-min))
      (when (re-search-forward comment-re end t)
        (setq end (line-beginning-position)))
      (cons (point-min) end))))

(defun magent-magit--apply-commit-response (request response _info)
  "Apply commit RESPONSE for REQUEST when the target buffer is still safe."
  (let ((message (magent-magit--normalize-commit-message response))
        (buffer (magent-magit-request-target-buffer request)))
    (cond
     ((string-empty-p message)
      (magent-magit--show-commit-preview request response "Model returned an empty message"))
     ((not (buffer-live-p buffer))
      (magent-magit--show-commit-preview request message "Target commit buffer was closed"))
     (t
      (with-current-buffer buffer
        (let ((baseline (or (magent-magit-request-baseline-text request) ""))
              (current (or (git-commit-buffer-message) "")))
          (cond
           ((not (equal magent-magit--active-request-id
                        (magent-magit-request-id request)))
            (magent-magit--show-commit-preview
             request message "A newer request now owns this buffer"))
           ((not (string= baseline current))
            (magent-magit--show-commit-preview
             request message "The commit buffer changed while the request was running"))
           (t
            (pcase-let ((`(,beg . ,end) (magent-magit--commit-message-region)))
              (let ((inhibit-read-only t))
                (delete-region beg end)
                (goto-char beg)
                (insert message)
                (unless (bolp)
                  (insert "\n"))
                (when (< (point) (point-max))
                  (unless (looking-at "\n")
                    (insert "\n")))
                (setq-local magent-magit--generated-message message))
              (message "magent-magit: Commit message inserted"))))))))))

(defun magent-magit--show-diff-explanation (request response _info)
  "Display diff explanation RESPONSE for REQUEST."
  (let* ((snapshot (magent-magit-request-snapshot request))
         (text (string-trim response))
         (body (string-join
                (list
                 (format "# Diff Explanation")
                 ""
                 (format "- Repository: `%s`" (plist-get snapshot :repo-name))
                 (format "- Branch: `%s`" (plist-get snapshot :branch))
                 (format "- Scope: `%s`%s"
                         (plist-get snapshot :scope)
                         (if (plist-get snapshot :region-derived)
                             " (explaining enclosing hunk)"
                           ""))
                 (format "- Diff type: `%s`" (plist-get snapshot :diff-type))
                 ""
                 text)
                "\n")))
    (magent-magit--display-text-buffer
     magent-magit-explain-buffer-name
     body
     t)
    (message "magent-magit: Diff explanation ready")))

(defun magent-magit--handle-response (request response info)
  "Handle gptel RESPONSE/INFO for REQUEST."
  (unwind-protect
      (cond
       ((eq (magent-magit-request-status request) 'cancelled)
        nil)
       ((stringp response)
        (setf (magent-magit-request-status request) 'completed)
        (condition-case err
            (funcall (magent-magit-request-applied-callback request)
                     request response info)
          (error
           (magent-magit--display-text-buffer
            magent-magit-preview-buffer-name
            (format "magent-magit failed to apply a response: %s"
                    (error-message-string err))))))
       ((eq response 'abort)
        (setf (magent-magit-request-status request) 'cancelled)
        (message "magent-magit: Request cancelled"))
       ((null response)
        (setf (magent-magit-request-status request) 'failed)
        (magent-magit--display-text-buffer
         magent-magit-preview-buffer-name
         (format "magent-magit request failed.\n\n%s"
                 (magent-magit--request-error-string info))))
       (t
        (setf (magent-magit-request-status request) 'failed)
        (magent-magit--display-text-buffer
         magent-magit-preview-buffer-name
         (format "magent-magit received an unsupported gptel response:\n\n%s"
                 response))))
    (magent-magit--cleanup-request request)))

(defun magent-magit--dispatch-request (request prompt system-prompt)
  "Send PROMPT for REQUEST with SYSTEM-PROMPT through an isolated gptel buffer."
  (let ((request-buffer (generate-new-buffer " *magent-magit-request*")))
    (setf (magent-magit-request-request-buffer request) request-buffer
          (magent-magit-request-status request) 'running)
    (magent-magit--register-request request)
    (condition-case err
        (with-current-buffer request-buffer
          (let ((gptel-backend (magent-magit--default-backend))
                (gptel-model (magent-magit--default-model))
                (gptel-use-context nil)
                (gptel-context nil)
                (gptel-use-tools nil)
                (gptel-tools nil)
                (gptel-track-response nil)
                (gptel-prompt-transform-functions nil)
                (gptel-include-reasoning nil))
            (gptel-request
                prompt
              :buffer request-buffer
              :system system-prompt
              :stream nil
              :callback (lambda (response info)
                          (magent-magit--handle-response
                           request response info)))))
      (error
       (magent-magit--cleanup-request request)
       (signal (car err) (cdr err))))
    request))

(defun magent-magit--cancel-request (request &optional silent)
  "Cancel REQUEST.
When SILENT is non-nil, do not emit an extra status message."
  (when request
    (setf (magent-magit-request-status request) 'cancelled)
    (if-let ((request-buffer (magent-magit-request-request-buffer request)))
        (if (buffer-live-p request-buffer)
            (gptel-abort request-buffer)
          (magent-magit--cleanup-request request))
      (magent-magit--cleanup-request request))
    (unless silent
      (message "magent-magit: Request cancelled"))))

;;;###autoload
(defun magent-magit-cancel ()
  "Cancel the active Magent request owned by the current buffer."
  (interactive)
  (if-let ((request (magent-magit--request-for-buffer)))
      (magent-magit--cancel-request request)
    (user-error "No active Magent request for this buffer")))

;;;###autoload
(defun magent-magit-generate-message ()
  "Generate a commit message for the current repository's staged changes."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (target-buffer (cond
                         ((derived-mode-p 'git-commit-mode) (current-buffer))
                         ((magent-magit--find-commit-buffer
                           (magent-magit--repo-root source-buffer)))
                         (t nil))))
    (unless target-buffer
      (user-error "No commit buffer is active for this repository"))
    (when-let ((request (magent-magit--request-for-buffer target-buffer)))
      (magent-magit--cancel-request request t))
    (with-current-buffer target-buffer
      (let* ((repo-root (magent-magit--repo-root target-buffer))
             (snapshot (magent-magit--capture-staged-snapshot repo-root))
             (request (magent-magit-request-create
                       :id (magent-magit--next-request-id)
                       :kind 'commit-message
                       :repo-root repo-root
                       :source-buffer source-buffer
                       :target-buffer target-buffer
                       :snapshot snapshot
                       :baseline-text (or (git-commit-buffer-message) "")
                       :baseline-tick (buffer-chars-modified-tick)
                       :applied-callback #'magent-magit--apply-commit-response)))
        (magent-magit--dispatch-request
         request
         (plist-get snapshot :payload)
         magent-magit-commit-prompt)
        (message "magent-magit: Generating commit message...")))))

;;;###autoload
(defun magent-magit-commit-create (&optional args)
  "Open a commit buffer using ARGS and generate a commit message in it."
  (interactive (list (magit-commit-arguments)))
  (let ((repo-root (magent-magit--repo-root)))
    (magit-commit-create args)
    (magent-magit--wait-for-commit-buffer
     repo-root
     (lambda (buffer)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (magent-magit-generate-message)))))
    (message "magent-magit: Opening commit buffer...")))

;;;###autoload
(defun magent-magit-diff-explain ()
  "Explain the current Magit diff selection."
  (interactive)
  (when-let ((request (magent-magit--request-for-buffer)))
    (magent-magit--cancel-request request t))
  (let* ((snapshot (magent-magit--capture-diff-snapshot))
         (request (magent-magit-request-create
                   :id (magent-magit--next-request-id)
                   :kind 'diff-explain
                   :repo-root (plist-get snapshot :repo-root)
                   :source-buffer (current-buffer)
                   :target-buffer (current-buffer)
                   :snapshot snapshot
                   :applied-callback #'magent-magit--show-diff-explanation)))
    (magent-magit--dispatch-request
     request
     (plist-get snapshot :payload)
     magent-magit-diff-explain-prompt)
    (message "magent-magit: Explaining current diff...")))

(defun magent-magit--install-transient (prefix anchor suffix)
  "Install SUFFIX into PREFIX after ANCHOR, logging rather than signaling."
  (condition-case err
      (transient-append-suffix prefix anchor suffix)
    (error
     (message "magent-magit: failed to install %s into %s: %s"
              (car suffix) prefix (error-message-string err)))))

(defun magent-magit--retire-gptel-magit ()
  "Remove legacy `gptel-magit' bindings from the current session."
  (remove-hook 'magit-mode-hook #'gptel-magit-install)
  (define-key git-commit-mode-map (kbd "M-g") nil)
  (ignore-errors
    (when (fboundp 'transient-remove-suffix)
      (transient-remove-suffix 'magit-commit #'gptel-magit-commit-generate)
      (transient-remove-suffix 'magit-diff #'gptel-magit-diff-explain)
      ;; Clean up by historical keys too in case only autoload symbols exist.
      (transient-remove-suffix 'magit-commit "g")
      (transient-remove-suffix 'magit-diff "x"))))

;;;###autoload
(defun magent-magit-install ()
  "Install Magent Magit keybindings and transient entries."
  (interactive)
  (unless magent-magit--installed
    (magent-magit--retire-gptel-magit)
    (define-key git-commit-mode-map magent-magit-commit-buffer-key
                #'magent-magit-generate-message)
    (define-key git-commit-mode-map magent-magit-cancel-key
                #'magent-magit-cancel)
    (magent-magit--install-transient
     'magit-commit
     #'magit-commit-create
     `(,magent-magit-commit-transient-key "AI Draft" magent-magit-commit-create))
    (magent-magit--install-transient
     'magit-diff
     #'magit-stash-show
     `(,magent-magit-diff-transient-key "AI Explain" magent-magit-diff-explain))
    (setq magent-magit--installed t)))

(when magent-magit-auto-install
  (magent-magit-install))

(provide 'magent-magit)
;;; magent-magit.el ends here
