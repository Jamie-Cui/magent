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
(require 'seq)
(require 'subr-x)
(require 'magent-config)
(require 'magent-agent-job)
(require 'magent-agent-registry)
(require 'magent-lifecycle-events)
(require 'magent-permission)
(require 'magent-protocol)
(require 'magent-runtime)
(require 'magent-session)

(declare-function magent-agent-run-turn "magent-agent")
(declare-function magent-agent-loop-abort "magent-agent-loop")
(declare-function magent-agent-loop-p "magent-agent-loop" t t)


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

(defconst magent-tools--read-file-default-line-count 200
  "Default maximum number of lines returned by `read_file'.")

(defconst magent-tools--read-file-page-max-characters 8000
  "Target maximum content characters returned by one `read_file' page.
Pages end at a line boundary when possible.  A single longer line is returned
whole so a subsequent line-based request always makes progress.")

(defconst magent-tools--agent-wait-fallback-timeout 300
  "Fallback wait timeout in seconds when the host request timeout is disabled.")

(defun magent-tools--completed (output &optional exit-code metadata)
  "Return a completed structured tool result."
  (magent-tool-result-create
   :status 'completed
   :success t
   :output output
   :exit-code exit-code
   :metadata metadata))

(defun magent-tools--failed (message &optional exit-code metadata)
  "Return a failed structured tool result for MESSAGE."
  (let ((text (if (stringp message) message (format "%s" message))))
    (magent-tool-result-create
     :status 'failed
     :success nil
     :output text
     :error text
     :exit-code exit-code
     :metadata metadata)))

(defun magent-tools--complete (callback output &optional exit-code metadata)
  "Call CALLBACK with a completed structured tool result."
  (funcall callback (magent-tools--completed output exit-code metadata)))

(defun magent-tools--fail (callback message &optional exit-code metadata)
  "Call CALLBACK with a failed structured tool result."
  (funcall callback (magent-tools--failed message exit-code metadata)))

(defun magent-tools--agent-wait-timeout (&optional timeout)
  "Return explicit child-agent TIMEOUT or the host's finite default."
  (cond
   ((numberp timeout) timeout)
   ((and (numberp magent-request-timeout)
         (> magent-request-timeout 0))
    magent-request-timeout)
   (t magent-tools--agent-wait-fallback-timeout)))

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

(defun magent-tools--local-process-directory ()
  "Return the directory used to start Magent-owned local processes.
Signal an error instead of allowing a remote temporary directory to change
the execution host implicitly."
  (let ((directory (file-name-as-directory
                    (expand-file-name temporary-file-directory))))
    (when (file-remote-p directory)
      (error "Magent local process directory is remote: %s" directory))
    directory))

(defun magent-tools--project-executable (program directory)
  "Resolve PROGRAM for a project process running in DIRECTORY.
Return the host-local executable spelling consumed by `make-process', or nil.
Remote lookup happens only for tools whose declared locality is
`project-process'."
  (condition-case nil
      (when (and (stringp program) (not (string-blank-p program)))
        (let* ((default-directory (file-name-as-directory directory))
               (remote (file-remote-p default-directory)))
          (if (file-name-directory program)
              (let ((candidate (expand-file-name program default-directory)))
                (and (not (file-directory-p candidate))
                     (file-executable-p candidate)
                     (if remote (file-local-name candidate) candidate)))
            (executable-find program remote))))
    (error nil)))

(defun magent-tools--start-project-process (tool-name directory &rest arguments)
  "Start TOOL-NAME's project-side process in DIRECTORY with ARGUMENTS.
This is Magent's only process-launch mechanism allowed to cross a TRAMP
boundary.  Local projects use an ordinary local process; remote projects use
the matching file-name handler."
  (unless (eq (magent-tools-locality tool-name) 'project-process)
    (error "Tool %s is not authorized for project-host execution" tool-name))
  (let* ((default-directory (file-name-as-directory directory))
         (remote (file-remote-p default-directory)))
    (apply #'make-process
           (append arguments (list :file-handler (and remote t))))))

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

(defun magent-tools--read-range (start-line line-count)
  "Validate and normalize optional START-LINE and LINE-COUNT."
  (when (eq start-line :null)
    (setq start-line nil))
  (when (eq line-count :null)
    (setq line-count nil))
  (unless (or (null start-line)
              (and (integerp start-line) (> start-line 0)))
    (error "Invalid input: start_line must be a positive integer (got %S)"
           start-line))
  (unless (or (null line-count)
              (and (integerp line-count) (> line-count 0)))
    (error "Invalid input: line_count must be a positive integer (got %S)"
           line-count))
  (cons (or start-line 1)
        (or line-count magent-tools--read-file-default-line-count)))

(defun magent-tools--buffer-page
    (tool-name start-line line-count metadata &optional page-max-characters)
  "Return a self-describing page from the current buffer.
TOOL-NAME identifies the calling tool.  START-LINE is one-based and LINE-COUNT
is the requested maximum number of lines.  METADATA is the source description
included in the result header.  PAGE-MAX-CHARACTERS defaults to the fixed
`read_file' page budget."
  (setq page-max-characters
        (or page-max-characters
            magent-tools--read-file-page-max-characters))
  (save-excursion
    (save-restriction
      (widen)
      (let ((total-lines (count-lines (point-min) (point-max))))
        (goto-char (point-min))
        (forward-line (1- start-line))
        (let ((begin (point)))
          (forward-line line-count)
          (let ((end (point)))
            (when (> (- end begin) page-max-characters)
              (goto-char (+ begin page-max-characters))
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
                    (format "[%s: %s; lines=%s; total_lines=%d; has_more=%s%s]\n"
                     tool-name
                     metadata
                     line-range
                     total-lines
                     (if has-more "true" "false")
                     (if next-line
                         (format "; next_start_line=%d" next-line)
                       ""))))
              (concat header
                      (buffer-substring-no-properties begin end)))))))))

(defun magent-tools--buffer-revision (&optional buffer)
  "Return a SHA-256 revision for the widened contents of BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (secure-hash 'sha256 (current-buffer) (point-min) (point-max)))))

(defun magent-tools--file-revision (path)
  "Return the decoded-content SHA-256 revision for file PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (magent-tools--buffer-revision)))

(defun magent-tools--assert-file-revision (path expected-revision)
  "Require PATH to match EXPECTED-REVISION or the sentinel \"absent\"."
  (unless (and (stringp expected-revision)
               (not (string-empty-p expected-revision)))
    (error "Invalid input: expected_revision must be a non-empty string"))
  (if (file-exists-p path)
      (progn
        (when (equal expected-revision "absent")
          (error "Error stale_revision: %s already exists" path))
        (let ((actual (magent-tools--file-revision path)))
          (unless (equal actual expected-revision)
            (error "Error stale_revision: expected %s but found %s for %s"
                   expected-revision actual path))))
    (unless (equal expected-revision "absent")
      (error "Error stale_revision: %s no longer exists" path))))

(defun magent-tools--read-file
    (callback path source &optional start-line line-count)
  "Read a bounded page from PATH using explicit SOURCE.
SOURCE is \"disk\" or \"live-buffer\".  START-LINE is one-based and
LINE-COUNT defaults to `magent-tools--read-file-default-line-count'."
  (condition-case err
      (progn
        (unless (stringp path)
          (error "Missing required argument 'path' (got %S)" path))
        (unless (member source '("disk" "live-buffer"))
          (error "Invalid input: source must be either disk or live-buffer"))
        (let* ((path (magent-tools--resolve-path path))
               (range (magent-tools--read-range start-line line-count))
               (buffer (and (equal source "live-buffer")
                            (find-buffer-visiting path))))
          (if (equal source "disk")
              (with-temp-buffer
                (insert-file-contents path)
                (let ((revision (magent-tools--buffer-revision)))
                  (magent-tools--complete
                   callback
                   (magent-tools--buffer-page
                    "read_file" (car range) (cdr range)
                    (format "source=disk; modified=false; revision=%s"
                            revision))
                   nil (list :source "disk" :revision revision))))
            (unless (buffer-live-p buffer)
              (error "Error buffer_not_found: no live buffer is visiting %s"
                     path))
            (with-current-buffer buffer
              (let* ((revision (magent-tools--buffer-revision))
                     (metadata
                      (format
                       "source=live-buffer; buffer=%S; modified=%s; narrowed=%s; revision=%s"
                       (buffer-name buffer)
                       (if (buffer-modified-p) "true" "false")
                       (if (buffer-narrowed-p) "true" "false")
                       revision)))
                (magent-tools--complete
                 callback
                 (magent-tools--buffer-page
                  "read_file" (car range) (cdr range) metadata)
                 nil (list :source "live-buffer" :revision revision
                           :modified (buffer-modified-p))))))))
    (error
     (magent-tools--fail
      callback (format "Error reading file: %s" (error-message-string err))))))

(defun magent-tools--clean-visiting-buffer (path &optional refresh-stale)
  "Return the clean file-visiting buffer for PATH, or nil.
Signal a conflict when the visiting buffer is modified or its file changed on
disk since it was visited.  When REFRESH-STALE is non-nil, refresh a clean
stale buffer from disk without running revert hooks or reinitializing modes."
  (when-let* ((buffer (find-buffer-visiting path)))
    (with-current-buffer buffer
      (when (buffer-modified-p)
        (error "Error buffer_conflict: visiting buffer %S has unsaved changes"
               (buffer-name buffer)))
      (unless (verify-visited-file-modtime buffer)
        (if refresh-stale
            (let ((before-revert-hook nil)
                  (after-revert-hook nil))
              (revert-buffer t t t)
              (unless (verify-visited-file-modtime buffer)
                (error "Error disk_conflict: %s changed while refreshing buffer %S"
                       path (buffer-name buffer))))
          (error "Error disk_conflict: %s changed on disk since buffer %S visited it"
                 path (buffer-name buffer)))))
    buffer))

(defun magent-tools--atomic-write-string (path content &optional coding-system)
  "Atomically replace PATH with CONTENT using CODING-SYSTEM when non-nil."
  (let* ((directory (file-name-directory path))
         (existing-mode (and (file-exists-p path) (file-modes path)))
         (temporary-file
          (make-temp-file (expand-file-name ".magent-write-" directory))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert content)
            (let ((coding-system-for-write coding-system))
              (write-region
               (point-min) (point-max) temporary-file nil 0)))
          (set-file-modes
           temporary-file
           (or existing-mode
               (logand (default-file-modes) #o666)))
          (rename-file temporary-file path t)
          (setq temporary-file nil))
      (when (and temporary-file (file-exists-p temporary-file))
        (delete-file temporary-file)))))

(defun magent-tools--replace-unique-buffer-text (path old-text new-text)
  "Replace the unique OLD-TEXT with NEW-TEXT in the current buffer.
Signal a descriptive error mentioning PATH when the match is not unique."
  (save-restriction
    (widen)
    (let ((count 0))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward old-text nil t)
          (cl-incf count)))
      (cond
       ((= count 0)
        (error "Invalid input: old_text not found in %s" path))
       ((> count 1)
        (error "Invalid input: old_text found %d times in %s (must be unique)"
               count path)))
      (goto-char (point-min))
      (search-forward old-text)
      (replace-match new-text t t))))

(defun magent-tools--write-file (callback path content expected-revision)
  "Write CONTENT to PATH after validating EXPECTED-REVISION.
Creates parent directories if needed.  Rejects a modified visiting buffer;
updates and saves a clean visiting buffer so Emacs and disk remain synchronized.
CALLBACK is called with success message or error."
  (condition-case err
      (progn
        (unless (stringp path)
          (error "Missing required argument 'path' (got %S)" path))
        (unless (stringp content)
          (error "Missing required argument 'content' (got %S)" content))
        (let ((path (magent-tools--resolve-path path)))
          (magent-tools--assert-file-revision path expected-revision)
          (let ((dir (file-name-directory path)))
            (when (and dir (not (file-exists-p dir)))
              (make-directory dir t)))
          (if-let* ((buffer (magent-tools--clean-visiting-buffer path)))
              (with-current-buffer buffer
                (save-excursion
                  (save-restriction
                    (widen)
                    (atomic-change-group
                      (erase-buffer)
                      (insert content)
                      (let ((file-precious-flag t))
                        (save-buffer))))))
            (magent-tools--atomic-write-string path content))
          (magent-tools--complete
           callback (format "Successfully wrote %s" path))))
    (error
     (magent-tools--fail
      callback (format "Error writing file: %s" (error-message-string err))))))

(defun magent-tools--grep-nul-tail-records (output)
  "Parse OUTPUT records shaped as PATH NUL LINE:TEXT newline."
  (let ((position 0)
        (output-length (length output))
        records)
    (while (< position output-length)
      (let ((nul (string-match "\0" output position)))
        (if (null nul)
            (setq position output-length)
          (let* ((newline (or (string-match "\n" output (1+ nul))
                              output-length))
                 (path (substring output position nul))
                 (tail (substring output (1+ nul) newline)))
            (when (string-match "\\`\\([0-9]+\\):\\(.*\\)\\'" tail)
              (push (list path (match-string 1 tail) (match-string 2 tail))
                    records))
            (setq position (min output-length (1+ newline)))))))
    (nreverse records)))

(defun magent-tools--git-grep-records (output)
  "Parse git grep OUTPUT records shaped as PATH NUL LINE NUL TEXT newline."
  (let ((position 0)
        (output-length (length output))
        records)
    (while (< position output-length)
      (let ((path-end (string-match "\0" output position)))
        (if (null path-end)
            (setq position output-length)
          (let ((line-end (string-match "\0" output (1+ path-end))))
            (if (null line-end)
                (setq position output-length)
              (let* ((newline (or (string-match "\n" output (1+ line-end))
                                  output-length))
                     (path (substring output position path-end))
                     (line (substring output (1+ path-end) line-end))
                     (text (substring output (1+ line-end) newline)))
                (when (string-match-p "\\`[0-9]+\\'" line)
                  (push (list path line text) records))
                (setq position (min output-length (1+ newline)))))))))
    (nreverse records)))

(defun magent-tools--grep-records (output &optional backend)
  "Parse backend-specific OUTPUT into (PATH LINE TEXT) records.
BACKEND defaults to `ripgrep'."
  (if (eq backend 'git-grep)
      (magent-tools--git-grep-records output)
    (magent-tools--grep-nul-tail-records output)))

(defun magent-tools--grep-display-output (output &optional backend)
  "Render backend-specific OUTPUT in conventional path:line:text form."
  (if-let* ((records (magent-tools--grep-records output backend)))
      (mapconcat (lambda (record)
                   (format "%s:%s:%s"
                           (nth 0 record) (nth 1 record) (nth 2 record)))
                 records "\n")
    output))

(defun magent-tools--grep-revisions (output directory &optional backend)
  "Return file revisions parsed from backend-specific OUTPUT in DIRECTORY."
  (let ((paths (mapcar #'car (magent-tools--grep-records output backend)))
        revisions)
    (dolist (path (delete-dups paths))
      (let ((absolute (expand-file-name path directory)))
        (when (file-regular-p absolute)
          (push (cons path (magent-tools--file-revision absolute)) revisions))))
    (sort revisions (lambda (left right) (string< (car left) (car right))))))

(defun magent-tools--grep-backend (directory)
  "Return the first available search backend on DIRECTORY's project host.
The fixed preference order is ripgrep, then git grep."
  (or (when-let* ((program (magent-tools--project-executable
                            magent-grep-program directory)))
        (list :name 'ripgrep :program program))
      (when-let* ((program (magent-tools--project-executable "git" directory)))
        (list :name 'git-grep :program program))
      (error (concat "no supported search executable found on project host; "
                     "tried %s and git")
             magent-grep-program)))

(defun magent-tools--grep-command (backend pattern target case-sensitive)
  "Return BACKEND command for PATTERN and TARGET.
CASE-SENSITIVE controls case folding."
  (let ((program (plist-get backend :program)))
    (pcase (plist-get backend :name)
      ('ripgrep
       (cons program
             (append
              (list "--no-heading" "--with-filename" "--null"
                    "--line-number" "--color=never")
              (unless case-sensitive (list "--ignore-case"))
              (list "--" pattern target))))
      ('git-grep
       (cons program
             (append
              (list "--no-pager" "grep" "--no-index" "--exclude-standard"
                    "-I" "-n" "-z" "--no-color" "--extended-regexp")
              (unless case-sensitive (list "--ignore-case"))
              (list "-e" pattern "--" target))))
      (_ (error "Unknown grep backend: %S" (plist-get backend :name))))))

(defun magent-tools--grep-result-metadata
    (backend revisions &optional truncated limit)
  "Return result metadata for BACKEND and REVISIONS.
TRUNCATED and LIMIT describe host-enforced result bounding."
  (append
   (list :backend (symbol-name backend) :revisions revisions)
   (when truncated (list :truncated t :limit limit))))

(defun magent-tools--format-grep-output (matches revisions)
  "Return MATCHES with a deterministic REVISIONS header."
  (if (null revisions)
      matches
    (format "[file revisions]\n%s\n[/file revisions]\n%s"
            (mapconcat (lambda (entry)
                         (format "%s %s" (cdr entry) (car entry)))
                       revisions "\n")
            matches)))

(defun magent-tools--grep (callback pattern path &optional case-sensitive)
  "Search asynchronously for PATTERN in files under PATH.
Prefer ripgrep, then fall back to git grep on the project host.
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
                 (search-directory
                  (if directory-p
                      (file-name-as-directory resolved)
                    (or (file-name-directory resolved)
                        (magent-tools--request-project-root))))
                 (default-directory search-directory)
                 (target (if directory-p "." (file-name-nondirectory resolved)))
                 (limit (max 1 magent-grep-max-matches))
                 (backend (magent-tools--grep-backend search-directory))
                 (backend-name (plist-get backend :name))
                 (command (magent-tools--grep-command
                           backend pattern target case-sensitive))
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
                           (trimmed (string-trim-right output))
                           (revisions
                            (magent-tools--grep-revisions
                             trimmed search-directory backend-name))
                           (matches (magent-tools--grep-display-output
                                     trimmed backend-name))
                           (rendered
                            (magent-tools--format-grep-output
                             matches revisions)))
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
                          :metadata (magent-tools--grep-result-metadata
                                     backend-name revisions t limit)
                          :output (format "%s%s[results truncated after %d matches]"
                                          rendered
                                          (if (string-empty-p rendered) "" "\n")
                                          limit)))
                        ((= exit-code 0)
                         (magent-tool-result-create
                          :status 'completed
                          :success t
                          :exit-code exit-code
                          :metadata (magent-tools--grep-result-metadata
                                     backend-name revisions)
                          :output (if (string-blank-p output)
                                      "No matches found"
                                    rendered)))
                        ((= exit-code 1)
                         (magent-tool-result-create
                          :status 'completed
                          :success t
                          :exit-code exit-code
                          :metadata (magent-tools--grep-result-metadata
                                     backend-name nil)
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
                            :metadata (magent-tools--grep-result-metadata
                                       backend-name nil)
                            :output message
                            :error message)))))))))
              (setq proc
                    (magent-tools--start-project-process
                     "grep" search-directory
                     :name "magent-grep"
                     :buffer buf
                     :command command
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

(defun magent-tools--glob (callback pattern path)
  "Find files matching PATTERN under PATH asynchronously.
Supports * and ** wildcards.  Traversal is sliced across the Emacs event loop
and bounded by `magent-glob-max-results' and
`magent-glob-max-files-scanned'."
  (condition-case err
      (progn
        (unless (and (stringp pattern) (not (string-empty-p pattern)))
          (error "Invalid input: pattern must be a non-empty string"))
        (unless (stringp path)
          (error "Invalid input: path must be a string"))
        (dolist (setting
                 `((magent-glob-max-results . ,magent-glob-max-results)
                   (magent-glob-max-files-scanned
                    . ,magent-glob-max-files-scanned)
                   (magent-glob-batch-size . ,magent-glob-batch-size)))
          (unless (and (integerp (cdr setting)) (> (cdr setting) 0))
            (error "%s must be a positive integer" (car setting))))
        (let* ((resolved (magent-tools--resolve-path path))
             (search-root (if (file-directory-p resolved)
                              resolved
                            (or (file-name-directory resolved)
                                (magent-tools--request-project-root))))
             (normalized-pattern
              (string-remove-prefix "./" (subst-char-in-string ?\\ ?/ pattern)))
             (regexp (magent-permission-glob-to-regexp normalized-pattern))
             (pending (list search-root))
             matches
             (scanned 0)
             timer
             done)
          (cl-labels
              ((finish
                (truncated reason)
                (unless done
                  (setq done t)
                  (when timer
                    (cancel-timer timer)
                    (setq timer nil))
                  (let* ((sorted (sort matches #'string<))
                         (body
                          (if sorted
                              (mapconcat #'identity sorted "\n")
                            "No files matched"))
                         (output
                          (if truncated
                              (format
                               "%s\n[glob truncated: %s; matched=%d; scanned=%d]"
                               body reason (length sorted) scanned)
                            body)))
                    (magent-tools--complete
                     callback output nil
                     (list :truncated truncated
                           :reason reason
                           :matched (length sorted)
                           :scanned scanned)))))
               (schedule
                ()
                (setq timer (run-at-time 0 nil #'step)))
               (step
                ()
                (setq timer nil)
                (condition-case step-error
                    (let ((remaining magent-glob-batch-size))
                      (while (and pending
                                  (> remaining 0)
                                  (< scanned magent-glob-max-files-scanned)
                                  (< (length matches)
                                     magent-glob-max-results))
                        (let ((entry (pop pending)))
                          (unless (equal entry search-root)
                            (cl-incf scanned)
                            (cl-decf remaining))
                          (if (and (file-directory-p entry)
                                   (not (file-symlink-p entry)))
                              (setq pending
                                    (append
                                     (directory-files
                                      entry t
                                      directory-files-no-dot-files-regexp)
                                     pending))
                            (when (and (file-regular-p entry)
                                       (string-match-p
                                        regexp
                                        (subst-char-in-string
                                         ?\\ ?/
                                         (file-relative-name
                                          entry search-root))))
                              (push entry matches)))))
                      (cond
                       ((>= (length matches) magent-glob-max-results)
                        (finish t "result limit reached"))
                       ((>= scanned magent-glob-max-files-scanned)
                        (finish t "scan limit reached"))
                       (pending (schedule))
                       (t (finish nil nil))))
                  (error
                   (setq done t)
                   (magent-tools--fail
                    callback
                    (format "Error during glob: %s"
                            (error-message-string step-error)))))))
            (magent-tools--register-cancel-cleanup
             (lambda ()
               (setq done t)
               (when timer
                 (cancel-timer timer)
                 (setq timer nil))))
            (schedule))))
    (error
     (magent-tools--fail
      callback
      (format "Error during glob: %s" (error-message-string err))))))

(defun magent-tools--edit-file
    (callback path old-text new-text expected-revision)
  "Edit PATH after validating EXPECTED-REVISION.
Replace OLD-TEXT with NEW-TEXT asynchronously.
OLD-TEXT must match exactly once in the file.  Rejects a modified visiting
buffer; refreshes a clean stale buffer without revert hooks, then updates and
saves it so Emacs and disk remain synchronized.
CALLBACK is called with success message or error."
  (condition-case err
      (progn
        (unless (and (stringp old-text) (not (string-empty-p old-text)))
          (error "Invalid input: old_text must be a non-empty string"))
        (unless (stringp new-text)
          (error "Invalid input: new_text must be a string"))
        (let ((path (magent-tools--resolve-path path)))
          (magent-tools--assert-file-revision path expected-revision)
          (if-let* ((buffer (magent-tools--clean-visiting-buffer path t)))
              (with-current-buffer buffer
                (save-excursion
                  (atomic-change-group
                    (magent-tools--replace-unique-buffer-text
                     path old-text new-text)
                    (let ((file-precious-flag t))
                      (save-buffer)))))
            (with-temp-buffer
              (insert-file-contents path)
              (let ((coding-system buffer-file-coding-system))
                (magent-tools--replace-unique-buffer-text
                 path old-text new-text)
                (magent-tools--atomic-write-string
                 path (buffer-string) coding-system))))
          (magent-tools--complete
           callback (format "Successfully edited %s" path))))
    (error
     (magent-tools--fail
      callback (format "Error editing file: %s"
                       (error-message-string err))))))

(defconst magent-tools--emacs-eval-result-marker
  "\n__MAGENT_EVAL_RESULT__"
  "Marker preceding the child evaluator's terminal payload.")

(defconst magent-tools--emacs-eval-output-limit (* 1024 1024)
  "Maximum child evaluator output retained while waiting for its result.")

(defconst magent-tools--emacs-eval-worker-form
  "(progn (require 'subr-x) (let ((debug-on-error nil) (debug-on-quit nil) (debug-on-signal nil) (print-circle t) (print-level 20) (print-length 1000))
     (condition-case err
         (let* ((input (getenv \"MAGENT_EVAL_INPUT\"))
                (project-root (getenv \"MAGENT_EVAL_PROJECT_ROOT\"))
                (default-directory (if (and project-root (not (string-empty-p project-root)))
                                       (file-name-as-directory project-root)
                                     default-directory))
                (sexp (with-temp-buffer (insert-file-contents input) (buffer-string)))
                (parsed (read-from-string sexp))
                (form (car parsed)))
           (unless (string-blank-p (substring sexp (cdr parsed)))
             (error \"emacs_eval accepts exactly one Lisp form\"))
           (let ((value (eval form t)))
             (princ \"\\n__MAGENT_EVAL_RESULT__\")
             (prin1 (list :status 'completed :value (prin1-to-string value)))))
       (error
        (princ \"\\n__MAGENT_EVAL_RESULT__\")
        (prin1 (list :status 'failed :error (error-message-string err)))
        (kill-emacs 2)))))"
  "Fixed expression evaluated by the disposable child Emacs.")

(defun magent-tools--emacs-program ()
  "Return the executable path for a child matching this Emacs."
  (let ((program (expand-file-name invocation-name invocation-directory)))
    (if (file-executable-p program)
        program
      (or (executable-find invocation-name)
          (error "Cannot find Emacs executable %s" invocation-name)))))

(defun magent-tools--emacs-eval-payload (output)
  "Return the terminal child evaluator payload parsed from OUTPUT."
  (let ((start 0)
        found)
    (while (string-match (regexp-quote magent-tools--emacs-eval-result-marker)
                         output start)
      (setq found (match-end 0)
            start (match-end 0)))
    (when found
      (car (read-from-string (substring output found))))))

(defun magent-tools--emacs-eval (callback sexp &optional timeout)
  "Evaluate one SEXP in a fresh `emacs -Q --batch' child process."
  (let ((input-file nil)
        (buffer nil)
        (process nil)
        (timer nil)
        (finished nil)
        (cancelled nil))
    (cl-labels
        ((cleanup ()
           (when timer
             (cancel-timer timer)
             (setq timer nil))
           (when (and process (process-live-p process))
             (delete-process process))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (when (and input-file (file-exists-p input-file))
             (delete-file input-file)))
         (finish (result)
           (unless finished
             (setq finished t)
             (cleanup)
             (unless cancelled
               (funcall callback result)))))
      (condition-case err
          (progn
            (unless (and (stringp sexp) (not (string-blank-p sexp)))
              (error "Invalid input: sexp must be a non-blank string"))
            (setq timeout (or timeout magent-emacs-eval-timeout))
            (unless (and (numberp timeout) (> timeout 0))
              (error "Invalid input: timeout must be a positive number"))
            (setq input-file (make-temp-file "magent-eval-" nil ".el"))
            (with-temp-file input-file
              (let ((coding-system-for-write 'utf-8-unix))
                (insert sexp)))
            (set-file-modes input-file #o600)
            (setq buffer (generate-new-buffer " *magent-emacs-eval*"))
            (let ((default-directory (magent-tools--local-process-directory))
                  (process-environment (copy-sequence process-environment)))
              (setenv "MAGENT_EVAL_INPUT" input-file)
              (setenv "MAGENT_EVAL_PROJECT_ROOT"
                      (magent-tools--request-project-root))
              (setq process
                    (make-process
                     :name "magent-emacs-eval"
                     :buffer buffer
                     :command (list (magent-tools--emacs-program)
                                    "-Q" "--batch" "--eval"
                                    magent-tools--emacs-eval-worker-form)
                     :noquery t
                     :connection-type 'pipe
                     :filter
                     (lambda (_process chunk)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (goto-char (point-max))
                           (insert chunk)
                           (when (> (buffer-size)
                                    magent-tools--emacs-eval-output-limit)
                             (delete-region
                              (point-min)
                              (- (point-max)
                                 magent-tools--emacs-eval-output-limit))))))
                     :sentinel
                     (lambda (child _event)
                       (when (and (memq (process-status child) '(exit signal))
                                  (not finished))
                         (let* ((output
                                 (if (buffer-live-p buffer)
                                     (with-current-buffer buffer (buffer-string))
                                   ""))
                                (payload
                                 (condition-case nil
                                     (magent-tools--emacs-eval-payload output)
                                   (error nil))))
                           (if (eq (plist-get payload :status) 'completed)
                               (finish
                                (magent-tools--completed
                                 (or (plist-get payload :value) "nil")
                                 (process-exit-status child)
                                 (list :execution "child-process")))
                             (finish
                              (magent-tools--failed
                               (or (plist-get payload :error)
                                   (format
                                    "Error: child Emacs exited without a result (status=%s, exit=%s)"
                                    (process-status child)
                                    (process-exit-status child)))
                               (process-exit-status child)
                               (list :execution "child-process"))))))))))
            (setq timer
                  (run-at-time
                   timeout nil
                   (lambda ()
                     (unless finished
                       (finish
                        (magent-tools--failed
                         "Error: Evaluation timed out"
                         nil (list :execution "child-process" :timeout t)))))))
            (magent-tools--register-cancel-cleanup
             (lambda ()
               (setq cancelled t)
               (cleanup))))
        (error
         (finish
          (magent-tools--failed
           (format "Error starting child Emacs: %s"
                   (error-message-string err)))))))))

(defun magent-tools--emacs-eval-live (callback sexp &optional timeout)
  "Evaluate SEXP in the live Emacs with optional TIMEOUT in seconds.
CALLBACK is called with the result as a readable string, or an error message.
Evaluation runs in the user's context buffer when known
\(see `magent-tools--request-context'), falling back to current buffer."
  (let ((debug-on-error nil)
        (debug-on-quit nil)
        (debug-on-signal nil))
    (condition-case err
      (let* ((timeout (or timeout magent-emacs-eval-timeout))
             (parsed (read-from-string sexp))
             (form (car parsed))
             (_single-form
              (unless (string-blank-p (substring sexp (cdr parsed)))
                (error "Invalid input: emacs_eval accepts exactly one Lisp form")))
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
                                              (finish
                                               (magent-tools--completed
                                                (prin1-to-string result))))))
                           (quit
                            (run-at-time 0 nil
                                         (lambda ()
                                           (unless (or completed cancelled)
                                             (finish
                                              (magent-tools--failed
                                               "Error: Evaluation interrupted"))))))
                           (error
                            (run-at-time 0 nil
                                         (lambda ()
                                           (unless (or completed cancelled)
                                             (finish
                                              (magent-tools--failed
                                               (format
                                                "Error evaluating sexp: %s"
                                                (error-message-string
                                                 worker-err)))))))))))
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
                               (finish
                                (magent-tools--completed
                                 (prin1-to-string result))))
                           (quit
                            (unless (or completed cancelled)
                              (finish
                               (magent-tools--failed
                                "Error: Evaluation interrupted"))))
                           (error
                            (unless (or completed cancelled)
                              (finish
                               (magent-tools--failed
                                (format "Error evaluating sexp: %s"
                                        (error-message-string
                                         sync-err))))))))))
                    nil)))
          (when (and timeout (> timeout 0))
            (setq timer
                  (run-at-time
                   timeout nil
                   (lambda ()
                     (unless (or completed cancelled)
                       (interrupt-worker)
                       (finish
                        (magent-tools--failed
                         "Error: Evaluation timed out")))))))
          (magent-tools--register-cancel-cleanup
           (lambda ()
             (setq cancelled t)
             (when timer
               (cancel-timer timer)
               (setq timer nil))
             (interrupt-worker)))))
      (error
       (magent-tools--fail
        callback
        (format "Error evaluating sexp: %s"
                (error-message-string err)))))))

(defun magent-tools--emacs-read-buffer (target)
  "Return an existing buffer named or visited by TARGET."
  (or (and (stringp target) (get-buffer target))
      (and (stringp target)
           (condition-case nil
               (find-buffer-visiting (magent-tools--resolve-path target))
             (error nil)))
      (and (null target)
           (when-let* ((name (magent-tools--origin-buffer-name)))
             (get-buffer name)))))

(defun magent-tools--emacs-read-symbol-name (value)
  "Return a bounded descriptive name for hook VALUE."
  (cond
   ((symbolp value) (symbol-name value))
   ((byte-code-function-p value) "<byte-code-function>")
   ((functionp value) "<lambda-function>")
   (t (format "<%s>" (type-of value)))))

(defun magent-tools--emacs-read-value (operation target)
  "Return the trusted structured result for OPERATION and TARGET."
  (pcase operation
    ("list_buffers"
     (let ((buffers (buffer-list))
           rows)
       (dolist (buffer (seq-take buffers 200))
         (with-current-buffer buffer
           (push (list :name (buffer-name buffer)
                       :file buffer-file-name
                       :major-mode major-mode
                       :modified (buffer-modified-p)
                       :narrowed (buffer-narrowed-p))
                 rows)))
       (list :count (length buffers)
             :returned (length rows)
             :buffers (nreverse rows))))
    ("buffer_info"
     (let ((buffer (magent-tools--emacs-read-buffer target)))
       (unless (buffer-live-p buffer)
         (error "Error buffer_not_found: %s" (or target "request origin")))
       (with-current-buffer buffer
         (list :name (buffer-name buffer)
               :file buffer-file-name
               :major-mode major-mode
               :modified (buffer-modified-p)
               :narrowed (buffer-narrowed-p)
               :point (point)
               :point-min (point-min)
               :point-max (point-max)
               :read-only buffer-read-only))))
    ("current_context"
     (let ((buffer (magent-tools--emacs-read-buffer nil)))
       (unless (buffer-live-p buffer)
         (error "Request origin buffer is unavailable"))
       (with-current-buffer buffer
         (list :buffer (buffer-name buffer)
               :file buffer-file-name
               :major-mode major-mode
               :point (point)
               :mark (and (mark t) (mark t))
               :region-active (use-region-p)
               :region-bounds (and (use-region-p)
                                   (list (region-beginning) (region-end)))
               :narrowed (buffer-narrowed-p)
               :modified (buffer-modified-p)))))
    ("symbol_info"
     (unless (and (stringp target) (not (string-empty-p target)))
       (error "Invalid input: symbol_info requires target"))
     (let ((symbol (intern-soft target)))
       (unless symbol
         (error "Error unknown_symbol: %s" target))
       (list :name (symbol-name symbol)
             :bound (boundp symbol)
             :function-bound (fboundp symbol)
             :command (commandp symbol)
             :function-source (and (fboundp symbol) (symbol-file symbol 'defun))
             :variable-source (and (boundp symbol) (symbol-file symbol 'defvar))
             :variable-documentation
             (documentation-property symbol 'variable-documentation t))))
    ("key_binding"
     (unless (and (stringp target) (not (string-empty-p target)))
       (error "Invalid input: key_binding requires a key sequence target"))
     (let ((buffer (magent-tools--emacs-read-buffer nil)))
       (unless (buffer-live-p buffer)
         (error "Request origin buffer is unavailable"))
       (with-current-buffer buffer
         (let ((binding (key-binding (kbd target) t)))
           (list :key target
                 :binding (and binding (format "%s" binding))
                 :buffer (buffer-name buffer)
                 :major-mode major-mode)))))
    ("hook_members"
     (unless (and (stringp target) (not (string-empty-p target)))
       (error "Invalid input: hook_members requires a hook variable target"))
     (let ((symbol (intern-soft target))
           (buffer (magent-tools--emacs-read-buffer nil)))
       (unless (and symbol (boundp symbol))
         (error "Error unknown_hook: %s" target))
       (unless (buffer-live-p buffer)
         (error "Request origin buffer is unavailable"))
       (with-current-buffer buffer
         (let ((value (symbol-value symbol)))
           (list :hook target
                 :local (local-variable-p symbol)
                 :members
                 (mapcar #'magent-tools--emacs-read-symbol-name
                         (if (listp value) value (list value))))))))
    ("project_info"
     (require 'project)
     (let* ((buffer (magent-tools--emacs-read-buffer nil))
            (default-directory
             (or (and buffer
                      (buffer-local-value 'default-directory buffer))
                 (magent-tools--request-project-root)
                 default-directory))
            (project (project-current nil default-directory)))
       (unless project
         (error "No project for %s" default-directory))
       (list :root (project-root project)
             :buffer-count
             (if (fboundp 'project-buffers)
                 (length (project-buffers project))
               0))))
    (_ (error "Error: unsupported emacs_read operation: %s" operation))))

(defun magent-tools--emacs-read (callback operation &optional target)
  "Run trusted read-only OPERATION against live Emacs state."
  (condition-case err
      (let* ((value (magent-tools--emacs-read-value operation target))
             (print-circle t)
             (print-level 12)
             (print-length 500)
             (output (prin1-to-string value)))
        (when (> (length output) magent-emacs-read-max-characters)
          (setq output
                (concat (substring output 0 magent-emacs-read-max-characters)
                        "\n[emacs_read result truncated]")))
        (magent-tools--complete callback output nil
                                (list :operation operation)))
    (error
     (magent-tools--fail
      callback
      (format "Error reading Emacs state: %s" (error-message-string err))))))

(defun magent-tools--request-storage-identity ()
  "Return exact scope and session id for the current tool request."
  (let ((session
         (or (and magent-tools--request-context
                  (magent-request-context-session magent-tools--request-context))
             (and magent-tools--request-context
                  (magent-request-context-approval-session
                   magent-tools--request-context))
             (magent-session-get))))
    (unless session
      (error "No active Magent session id"))
    (list :scope
          (or (and magent-tools--request-context
                   (magent-request-context-scope
                    magent-tools--request-context))
              (and (magent-session-thread session)
                   (magent-thread-scope (magent-session-thread session)))
              (magent-session-current-scope)
              'global)
          :session-id (magent-session-id session))))

(defun magent-tools--character-range (start-character character-count)
  "Validate and normalize a character page request."
  (when (eq start-character :null)
    (setq start-character nil))
  (when (eq character-count :null)
    (setq character-count nil))
  (unless (or (null start-character)
              (and (integerp start-character) (> start-character 0)))
    (error "Invalid input: start_character must be a positive integer (got %S)"
           start-character))
  (unless (or (null character-count)
              (and (integerp character-count) (> character-count 0)))
    (error "Invalid input: character_count must be a positive integer (got %S)"
           character-count))
  (let ((page-budget (max 1 magent-tool-output-spill-page-characters)))
    (cons (or start-character 1)
          (min (or character-count page-budget)
             page-budget
             (if (and (numberp magent-tool-result-model-max-length)
                      (> magent-tool-result-model-max-length 0))
                 (max 1 (- magent-tool-result-model-max-length 512))
               most-positive-fixnum)))))

(defun magent-tools--buffer-character-page
    (start-character character-count metadata)
  "Return a self-describing character page from the current buffer."
  (save-restriction
    (widen)
    (let* ((total-characters (buffer-size))
           (begin (min (point-max)
                       (+ (point-min) (1- start-character))))
           (end (min (point-max) (+ begin character-count)))
           (returned (- end begin))
           (end-character (and (> returned 0)
                               (+ start-character returned -1)))
           (has-more (< end (point-max)))
           (next-character (and has-more (+ start-character returned)))
           (character-range
            (if end-character
                (format "%d-%d" start-character end-character)
              "none")))
      (concat
       (format
        "[read_tool_output: %s; characters=%s; total_characters=%d; has_more=%s%s]\n"
        metadata character-range total-characters
        (if has-more "true" "false")
        (if next-character
            (format "; next_start_character=%d" next-character)
          ""))
       (buffer-substring-no-properties begin end)))))

(defun magent-tools--read-tool-output
    (callback result-id &optional start-character character-count)
  "Read a bounded character page from spilled RESULT-ID in this session."
  (condition-case err
      (let* ((identity (magent-tools--request-storage-identity))
             (scope (plist-get identity :scope))
             (session-id (plist-get identity :session-id))
             (path (magent-tool-output-spill-file
                    scope session-id result-id))
             (range (magent-tools--character-range
                     start-character character-count)))
        (with-temp-buffer
          (insert-file-contents path)
          (magent-tools--complete
           callback
           (magent-tools--buffer-character-page
            (car range) (cdr range)
            (format "result_id=%s; session_id=%s" result-id session-id))
           nil (list :result-id result-id :session-id session-id
                     :scope scope
                     :start-character (car range)
                     :character-count (cdr range)))))
    (error
     (magent-tools--fail
      callback
      (format "Error reading tool output: %s" (error-message-string err))))))

(defun magent-tools--bash-failure (message &optional exit-code metadata)
  "Return a structured bash failure for MESSAGE.
EXIT-CODE is nil when no process exit status exists.  METADATA is optional."
  (let ((bounded-message message))
    (magent-tool-result-create
     :status 'failed
     :success nil
     :exit-code exit-code
     :error bounded-message
     :output bounded-message
     :metadata metadata)))

(defun magent-tools--bash-executable (directory)
  "Return the configured Bash executable on DIRECTORY's project host."
  (magent-tools--project-executable magent-bash-program directory))

(defun magent-tools--bash (callback command)
  "Execute COMMAND asynchronously with Bash pipefail semantics.
Pipefail is enabled and errexit is not.  CALLBACK receives a structured tool
result containing combined stdout and stderr plus the process exit status."
  (cond
   ((or (not (stringp command)) (string-blank-p command))
    (funcall callback
             (magent-tools--bash-failure
              "Error: 'command' must be a non-blank shell command string.")))
   ((not (and (numberp magent-bash-timeout)
              (> magent-bash-timeout 0)))
    (funcall callback
             (magent-tools--bash-failure
              "Error: magent-bash-timeout must be a positive number.")))
   (t
    (let* ((directory (magent-tools--request-project-root))
           (bash-program (magent-tools--bash-executable directory)))
      (if (not bash-program)
          (funcall callback
                   (magent-tools--bash-failure
                    (format
                     "Error: Bash executable not found: %s. Customize magent-bash-program."
                     magent-bash-program)))
        (let ((buf nil)
              (timer nil)
              (proc nil)
              (finished nil)
              (cleanup nil))
          (setq cleanup
                (lambda ()
                  (when timer
                    (cancel-timer timer)
                    (setq timer nil))
                  (when (process-live-p proc)
                    (delete-process proc))
                  (when (buffer-live-p buf)
                    (kill-buffer buf))))
          (condition-case err
              (let ((timeout magent-bash-timeout))
                (setq buf (generate-new-buffer " *magent-bash*"))
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
                               (let* ((output (buffer-string))
                                      (message
                                       (if (string-blank-p output)
                                           "Command timed out with no output"
                                         (format
                                          "Command timed out. Partial output:\n%s"
                                          (string-trim-right output)))))
                                 (funcall cleanup)
                                 (funcall
                                  callback
                                  (magent-tools--bash-failure
                                   message nil (list :timeout t))))))))))
                (let ((process-environment
                       (copy-sequence process-environment)))
                  (setq process-environment
                        (append '("PAGER=cat"
                                  "GIT_PAGER=cat"
                                  "MANPAGER=cat"
                                  "SYSTEMD_PAGER=cat"
                                  "GIT_TERMINAL_PROMPT=0"
                                  "DEBIAN_FRONTEND=noninteractive")
                                process-environment))
                  (setenv "BASH_ENV" nil)
                  (setq proc
                        (magent-tools--start-project-process
                         "bash" directory
                         :name "magent-bash"
                         :buffer buf
                         :command (list bash-program "-o" "pipefail"
                                        "-c" command)
                         :sentinel
                         (lambda (p _event)
                           (when (and (memq (process-status p) '(exit signal))
                                      (not finished))
                             (setq finished t)
                             (let* ((output
                                     (if (buffer-live-p buf)
                                         (with-current-buffer buf
                                           (buffer-string))
                                       ""))
                                    (exit-code (process-exit-status p))
                                    (success
                                     (and (eq (process-status p) 'exit)
                                          (= exit-code 0)))
                                    (message
                                     (if (string-blank-p output)
                                         (if success
                                             "Command completed with no output"
                                           (format
                                            "Command failed with exit code %d and no output"
                                            exit-code))
                                       (string-trim-right output))))
                               (funcall cleanup)
                               (funcall
                                callback
                                (if success
                                    (magent-tool-result-create
                                     :status 'completed
                                     :success t
                                     :exit-code exit-code
                                     :output message)
                                  (magent-tools--bash-failure
                                   message exit-code))))))))))
            (error
             (setq finished t)
             (funcall cleanup)
             (funcall callback
                      (magent-tools--bash-failure
                       (format "Error starting Bash process: %s"
                               (error-message-string err))))))))))))

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
  "Schedule persistence of SESSION for SCOPE after job state changes."
  (when (and session
             (or (magent-thread-turns
                  (magent-session-thread-ledger session))
                 (magent-session-agent-jobs session)))
    (magent-session-save-deferred-for-session
     session (or scope (magent-tools--parent-scope)))))

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
      (emacs_eval_live . ,(symbol-name
                           (magent-permission-resolve
                            rules 'emacs_eval_live)))
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
   ((and (fboundp 'gptel-backend-p)
         (gptel-backend-p backend)
         (fboundp 'gptel-backend-name))
    (gptel-backend-name backend))
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
   (lambda (entry)
     `((role . ,(symbol-name (cdr (assq 'role entry))))
       (content . ,(let ((content (cdr (assq 'content entry))))
                     (if (stringp content)
                         content
                       (format "%S" content))))))
   (magent-session-context-view session 'transcript)))

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
                    (error "Error: agent job '%s' not found" id)))
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
          (magent-lifecycle-events-create-subagent-context
           title
           (and parent-context
                (magent-request-context-event-context parent-context))
           (magent-request-context-audit-snapshot parent-context)))
         (effective-permission
          (magent-tools--effective-child-permission parent-context agent))
         (child-request-context
          (magent-request-context-create
           :id (magent-lifecycle-events-generate-id)
           :scope (and parent-context
                       (magent-request-context-scope parent-context))
           :session child-session
           :prompt prompt
           :agent agent
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
           :parent-model-route
           (and parent-context
                (magent-request-context-model-route parent-context))
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
    (magent-agent-job-put-runtime
     (magent-agent-job-id job)
     (list :session child-session
           :agent agent
           :request-context child-request-context
           :subagent-context subagent-context
           :loop nil))
    (condition-case err
        (progn
          (setq child-loop
                (magent-agent-run-turn
                 child-request-context
                 :on-complete
                 (lambda (response)
                   (magent-lifecycle-events-stop-subagent subagent-context)
                   (let* ((success (magent-execution-result-success-p response))
                          (text (magent-execution-result-content-string response))
                          (failed (not success)))
                     (magent-tools--agent-job-update-from-child
                      job child-session
                      (if failed 'failed 'completed)
                      (unless failed text)
                      (when failed text))
                     (magent-tools--render-agent-job-event
                      (if failed 'failed 'completed)
                      job text parent-context parent-scope nil)
                     (magent-tools--persist-parent-session
                      parent-session parent-scope)))))
          (magent-agent-job-put-runtime
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
               (magent-agent-job-clear-runtime
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
       (magent-agent-job-clear-runtime (magent-agent-job-id job))
       nil))))

(defun magent-tools--spawn-agent (callback agent-name prompt &optional task-name)
  "Spawn a durable child-agent job using AGENT-NAME and PROMPT."
  (let ((agent (magent-agent-registry-get
                agent-name (magent-tools--parent-scope))))
    (cond
     ((null agent)
      (magent-tools--fail
       callback (format "Error: agent '%s' not found" agent-name)))
     ((not (magent-agent-info-mode-p agent 'subagent))
      (magent-tools--fail
       callback (format "Error: agent '%s' is not a subagent" agent-name)))
     ((not (and (stringp prompt) (not (string-empty-p prompt))))
      (magent-tools--fail callback "Error: prompt is required"))
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
            (when-let* ((runtime (magent-agent-job-runtime
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
        (let ((output
               (magent-tools--agent-job-result-json
                (append
                 `((status . ,(if (eq (magent-agent-job-status job) 'failed)
                                  "failed"
                                "spawned"))
                   (job . ,(magent-tools--agent-job-summary job t)))
                 (unless (eq (magent-agent-job-status job) 'failed)
                   `((next_action
                      . ((tool . "wait_agent")
                         (arguments
                          . ((job_id . ,(magent-agent-job-id job))))))))))))
          (if (eq (magent-agent-job-status job) 'failed)
              (magent-tools--fail callback output)
            (magent-tools--complete callback output))))))))

(defun magent-tools--send-agent-message (callback job-id message)
  "Send follow-up MESSAGE to child-agent JOB-ID."
  (let* ((parent-session (magent-tools--parent-session))
         (parent-context magent-tools--request-context)
         (parent-scope (magent-tools--parent-scope))
         (job (and parent-session
                   (magent-session-agent-job parent-session job-id)))
         (runtime (and job
                       (magent-agent-job-runtime job-id))))
    (cond
     ((null job)
      (magent-tools--fail
       callback (format "Error: agent job '%s' not found" job-id)))
     ((memq (magent-agent-job-status job) '(running queued))
      (magent-tools--fail
       callback
       (format
        "Error: agent job '%s' is already running; wait before sending another message"
        job-id)))
     ((memq (magent-agent-job-status job) '(closed cancelled))
      (magent-tools--fail
       callback
       (format "Error: agent job '%s' is %s"
               job-id (magent-tools--agent-job-status-string job))))
     ((not runtime)
      (magent-tools--fail
       callback
       (format
        "Error: agent job '%s' has no live runtime; resume support is not available yet"
        job-id)))
     ((not (and (stringp message) (not (string-empty-p message))))
      (magent-tools--fail callback "Error: message is required"))
     (t
      (let ((agent (plist-get runtime :agent))
            (child-session (plist-get runtime :session)))
        (magent-tools--agent-job-start
         job agent message child-session
         parent-context parent-session)
        (when-let* ((runtime (magent-agent-job-runtime job-id)))
          (setf (magent-agent-job-metadata job)
                (magent-tools--agent-inheritance-metadata
                 parent-context
                 (plist-get runtime :request-context)
                 agent
                 child-session))
          (magent-tools--persist-parent-session parent-session parent-scope))
        (magent-tools--complete
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
    (magent-tools--complete
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
         (timeout (magent-tools--agent-wait-timeout timeout)))
    (condition-case err
        (let* ((jobs (magent-tools--agent-jobs-for-ids session ids))
               timer
               observer-tokens
               done)
          (cl-labels
              ((cleanup
                ()
                (when timer
                  (cancel-timer timer)
                  (setq timer nil))
                (mapc #'magent-agent-job-remove-observer observer-tokens)
                (setq observer-tokens nil))
               (finish
                (status)
                (unless done
                  (setq done t)
                  (cleanup)
                  (dolist (job jobs)
                    (magent-tools--render-agent-job-event
                     (if (magent-tools--agent-job-terminal-p job)
                         'observed
                       'waiting)
                     job status parent-context parent-scope t))
                  (magent-tools--persist-parent-session session parent-scope)
                  (magent-tools--complete
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
               (observe
                (_job)
                (when (ready-p)
                  (finish "completed"))))
            (if (or (ready-p) (<= timeout 0))
                (finish (if (ready-p) "completed" "timeout"))
              (setq observer-tokens
                    (mapcar
                     (lambda (job)
                       (magent-agent-job-add-observer job #'observe))
                     jobs))
              (setq timer (run-at-time timeout nil #'finish "timeout"))
              (magent-tools--register-cancel-cleanup
               #'cleanup))))
      (error
       (magent-tools--fail
        callback
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
                       (magent-agent-job-runtime job-id))))
    (cond
     ((null job)
      (magent-tools--fail
       callback (format "Error: agent job '%s' not found" job-id)))
     ((eq (magent-agent-job-status job) 'closed)
      (magent-tools--complete
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
      (magent-agent-job-clear-runtime job-id)
      (magent-tools--render-agent-job-event
       'closed job (or close-reason "closed") parent-context parent-scope t)
      (magent-tools--persist-parent-session session parent-scope)
      (magent-tools--complete
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
      (error
       (magent-tools--fail
        callback
        (format "Error initiating search: %s"
                (error-message-string err)))))))

(defun magent-tools--web-search-callback (status callback query max-results)
  "Handle HTTP response for web search.
STATUS is the `url-retrieve' status list.
CALLBACK is called with formatted results.
QUERY is the original search query.
MAX-RESULTS is the maximum number of results."
  (let ((url-buffer (current-buffer)))
    (unwind-protect
        (condition-case err
            (let ((error-status (plist-get status :error)))
              (if error-status
                  (magent-tools--fail
                   callback (format "HTTP error: %s" error-status))
                (goto-char (point-min))
                (when (re-search-forward "\r?\n\r?\n" nil t)
                  (let* ((html (libxml-parse-html-region (point) (point-max)))
                         (results (magent-tools--parse-ddg-results html max-results)))
                    (if results
                        (magent-tools--complete
                         callback
                         (magent-tools--format-search-results query results))
                      (magent-tools--complete
                       callback
                       (format "No results found for: %s" query)))))))
          (error
           (magent-tools--fail
            callback
            (format "Error parsing results: %s"
                    (error-message-string err)))))
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
   :description "Read a bounded page from an explicit source. Use source=disk for saved contents or source=live-buffer for an existing file-visiting buffer including unsaved edits. Every result includes a SHA-256 revision plus pagination metadata."
   :args (list '(:name "path"
                       :type string
                       :description "Absolute or relative path to the file")
               '(:name "source"
                       :type string
                       :enum ["disk" "live-buffer"]
                       :description "Required source of truth")
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
  "Tool definition for `read_file'.")

(defvar magent-tools--write-file-tool
  (gptel-make-tool
   :name "write_file"
   :description "Write full content to a file using atomic disk replacement. Creates missing parent directories. If Emacs has a clean visiting buffer, updates and saves that buffer; fails with buffer_conflict instead of overwriting unsaved buffer edits."
   :args (list '(:name "path"
                       :type string
                       :description "Absolute or relative path to the file")
               '(:name "content"
                       :type string
                       :description "The full content to write to the file")
               '(:name "expected_revision"
                       :type string
                       :description "Required. For an existing file, pass the SHA-256 revision returned by read_file. For a new path, pass the literal string \"absent\". Never omit this field.")
               magent-tools--reason-arg)
   :function #'magent-tools--write-file
   :async t
   :confirm t
   :category "magent")
  "Tool definition for `write_file'.")

(defvar magent-tools--edit-file-tool
  (gptel-make-tool
   :name "edit_file"
   :description "Edit a file by replacing an exact text match using atomic disk replacement. The old_text must appear exactly once. If Emacs has a clean visiting buffer, refreshes stale disk contents without revert hooks, then updates and saves that buffer; fails with buffer_conflict instead of overwriting unsaved buffer edits. Use this for precise, surgical edits instead of rewriting entire files."
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
               '(:name "expected_revision"
                       :type string
                       :description "Required SHA-256 disk revision returned by read_file or grep")
               magent-tools--reason-arg)
   :function #'magent-tools--edit-file
   :async t
   :confirm t
   :category "magent")
  "Tool definition for `edit_file'.")

(defvar magent-tools--grep-tool
  (gptel-make-tool
   :name "grep"
   :description "Search for a regex pattern in files under a directory. Prefers ripgrep (rg) and falls back to git grep with POSIX extended regex on the same project host. Respects Git ignore rules and returns matching lines with file paths and line numbers."
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
  "Tool definition for `grep'.")

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
  "Tool definition for `glob'.")

(defvar magent-tools--bash-tool
  (gptel-make-tool
   :name "bash"
   :description "Execute one synchronous command with Bash pipefail enabled and ordinary non-errexit command sequencing. Commands separated by ; continue after a nonzero status; use && or explicit set -e for fail-fast behavior. A failed pipeline stage makes the tool fail unless handled explicitly. Background jobs started with & do not survive the tool call. Do not hide long-running command progress behind tail."
   :args (list '(:name "command"
                       :type string
                       :description "Shell command to execute")
               magent-tools--reason-arg)
   :function #'magent-tools--bash
   :async t
   :confirm t
   :category "magent")
  "Tool definition for `bash'.")

(defvar magent-tools--emacs-eval-tool
  (gptel-make-tool
   :name "emacs_eval"
   :description "Evaluate one Emacs Lisp expression in a fresh emacs -Q --batch child process. Hangs and crashes are contained to the child, but the code retains the current user's host file, process, and network authority. It cannot access live buffers or the user's loaded Emacs state; use emacs_read for trusted live inspection."
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
  "Tool definition for `emacs_eval'.")

(defvar magent-tools--emacs-read-tool
  (gptel-make-tool
   :name "emacs_read"
   :description "Run one trusted, bounded, read-only query against the live Emacs. This tool does not accept Lisp code or arbitrary function names. Use read_file with source=live-buffer for buffer text."
   :args (list '(:name "operation"
                       :type string
                       :enum ["list_buffers" "buffer_info" "current_context"
                              "symbol_info" "key_binding" "hook_members"
                              "project_info"]
                       :description "Structured live-state query")
               '(:name "target"
                       :type string
                       :description "Buffer, symbol, key sequence, or hook name required by some operations"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--emacs-read
   :async t
   :category "magent")
  "Tool definition for `emacs_read'.")

(defvar magent-tools--read-tool-output-tool
  (gptel-make-tool
   :name "read_tool_output"
   :description "Read a bounded page from a full tool result that Magent spilled for the current session. Accepts only the opaque result_id shown in a truncation notice; it cannot read arbitrary paths or another session's results."
   :args (list '(:name "result_id"
                       :type string
                       :description "Opaque result id from a tool truncation notice")
               '(:name "start_character"
                       :type integer
                       :description "One-based character at which to start; defaults to 1"
                       :optional t)
               '(:name "character_count"
                       :type integer
                       :description "Maximum characters to return; capped by Magent's page budget"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--read-tool-output
   :async t
   :category "magent")
  "Tool definition for `read_tool_output'.")

(defvar magent-tools--emacs-eval-live-tool
  (gptel-make-tool
   :name "emacs_eval_live"
   :description "DANGEROUS: evaluate arbitrary Elisp inside the live Emacs process. This can hang or crash Emacs and mutate global state. Use only when emacs_read and the child-process emacs_eval cannot perform the task. Every call requires fresh user approval."
   :args (list '(:name "sexp"
                       :type string
                       :description "Exactly one Emacs Lisp form to evaluate")
               '(:name "timeout"
                       :type integer
                       :description "Best-effort timeout in seconds; not a safety boundary"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--emacs-eval-live
   :async t
   :confirm t
   :category "magent")
  "Tool definition for `emacs_eval_live'.")

(defvar magent-tools--spawn-agent-tool
  (gptel-make-tool
   :name "spawn_agent"
   :description "Start a durable child-agent job. Use explore for focused codebase search and general for broader multi-step work. Returns a stable job id and a machine-readable next action for wait_agent."
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
  "Tool definition for `spawn_agent'.")

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
  "Tool definition for `send_agent_message'.")

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
                       :description "Maximum seconds to wait; defaults to magent-request-timeout, or 300 when that setting is disabled"
                       :optional t)
               magent-tools--reason-arg)
   :function #'magent-tools--wait-agent
   :async t
   :category "magent")
  "Tool definition for `wait_agent'.")

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
  "Tool definition for `list_agents'.")

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
  "Tool definition for `close_agent'.")

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
  "Tool definition for `web_search'.")

;;; Canonical tool catalog

(defconst magent-tools-catalog
  `((:name "read_file" :tool ,magent-tools--read-file-tool
     :permission read :locality tramp-file)
    (:name "write_file" :tool ,magent-tools--write-file-tool
     :permission write :locality tramp-file)
    (:name "edit_file" :tool ,magent-tools--edit-file-tool
     :permission edit :locality tramp-file)
    (:name "grep" :tool ,magent-tools--grep-tool :permission grep
     :locality project-process)
    (:name "glob" :tool ,magent-tools--glob-tool :permission glob
     :locality tramp-file)
    (:name "bash" :tool ,magent-tools--bash-tool :permission bash
     :locality project-process)
    (:name "emacs_eval" :tool ,magent-tools--emacs-eval-tool
     :permission emacs_eval :approval once-only :locality local)
    (:name "emacs_read" :tool ,magent-tools--emacs-read-tool
     :permission read :locality local)
    (:name "read_tool_output" :tool ,magent-tools--read-tool-output-tool
     :permission read :locality local)
    (:name "emacs_eval_live" :tool ,magent-tools--emacs-eval-live-tool
     :permission emacs_eval_live :approval once-only :locality local)
    (:name "spawn_agent" :tool ,magent-tools--spawn-agent-tool
     :permission agent :locality local)
    (:name "send_agent_message" :tool ,magent-tools--send-agent-message-tool
     :permission agent :locality local)
    (:name "wait_agent" :tool ,magent-tools--wait-agent-tool
     :permission agent :locality local)
    (:name "list_agents" :tool ,magent-tools--list-agents-tool
     :permission agent :locality local)
    (:name "close_agent" :tool ,magent-tools--close-agent-tool
     :permission agent :locality local)
    (:name "web_search" :tool ,magent-tools--web-search-tool
     :permission web_search :locality local))
  "Canonical data catalog for Magent tools.")

(defun magent-tools-catalog-entry (tool-name)
  "Return the catalog entry for TOOL-NAME, or nil."
  (let ((name (if (symbolp tool-name)
                  (symbol-name tool-name)
                tool-name)))
    (cl-find name magent-tools-catalog
             :key (lambda (entry) (plist-get entry :name))
             :test #'equal)))

(defun magent-tools-permission-key (tool-name)
  "Return the permission key symbol for TOOL-NAME, or nil if unknown."
  (plist-get (magent-tools-catalog-entry tool-name) :permission))

(defun magent-tools-approval-policy (tool-name)
  "Return the approval policy for TOOL-NAME, or nil."
  (plist-get (magent-tools-catalog-entry tool-name) :approval))

(defun magent-tools-locality (tool-name)
  "Return TOOL-NAME's execution locality, or nil if unknown.
`local' operations stay on the Emacs host, `tramp-file' operations use local
Emacs file APIs and may access project resources through TRAMP, and
`project-process' operations may start a process on the project host."
  (plist-get (magent-tools-catalog-entry tool-name) :locality))

(defun magent-tools-get-gptel-tools-for-permission
    (permission &optional tool-names)
  "Return tools exposed by PERMISSION and exact TOOL-NAMES.
TOOL-NAMES is `:all' or a list of string or symbol names.  Nil is an empty
exact set.  Unknown names fail immediately.
Global `magent-enable-tools' filtering still applies.  Tools whose effective
decision is \\='ask remain exposed so the orchestrator can request approval."
  (let ((entries
         (if (eq tool-names :all)
             magent-tools-catalog
           (mapcar
            (lambda (name)
              (or (magent-tools-catalog-entry name)
                  (error "Unknown Magent tool: %s" name)))
            tool-names))))
    (cl-loop
     for entry in entries
     for permission-key = (plist-get entry :permission)
     when (and (memq permission-key magent-enable-tools)
               (or (null permission)
                   (magent-permission-tool-available-p
                    permission permission-key)))
     collect (plist-get entry :tool))))

(provide 'magent-tools)
;;; magent-tools.el ends here
