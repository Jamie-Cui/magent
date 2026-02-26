;;; magent-tools.el --- Tool implementations for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; Tool implementations that the AI agent can use to interact with files
;; and the system.  Tools are registered as gptel-tool structs for use
;; with gptel's tool calling system.

;;; Code:

(require 'cl-lib)
(require 'magent-config)
(require 'magent-agent-registry)

;;; Tool implementations

(defun magent-tools--read-file (path)
  "Read contents of file at PATH.
Returns the file contents as a string, or an error message."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))
    (error (format "Error reading file: %s" (error-message-string err)))))

(defun magent-tools--write-file (path content)
  "Write CONTENT to file at PATH.
Creates parent directories if needed.
Returns success message or error."
  (condition-case err
      (progn
        (let ((dir (file-name-directory path)))
          (when (and dir (not (file-exists-p dir)))
            (make-directory dir t)))
        (with-temp-buffer
          (insert content)
          (write-region (point-min) (point-max) path nil 0))
        (format "Successfully wrote %s" path))
    (error (format "Error writing file: %s" (error-message-string err)))))

(defun magent-tools--grep (pattern path &optional case-sensitive)
  "Search for PATTERN in files under PATH using ripgrep.
If CASE-SENSITIVE is nil, performs case-insensitive search.
Returns matching lines with file paths and line numbers."
  (condition-case err
      (let* ((default-directory (if (file-directory-p path)
                                    path
                                  (file-name-directory path)))
             (args (list "--no-heading" "--line-number" "--color=never"
                         "--max-count=100")))
        (unless case-sensitive
          (push "--ignore-case" args))
        (unless (file-directory-p path)
          (push path args))
        (push pattern args)
        (let ((output (with-output-to-string
                        (with-current-buffer standard-output
                          (apply #'call-process magent-grep-program nil t nil
                                 (nreverse args))))))
          (if (string-blank-p output)
              "No matches found"
            (string-trim-right output))))
    (error (format "Error during grep: %s" (error-message-string err)))))

(defun magent-tools--glob (pattern path)
  "Find files matching PATTERN under PATH.
Supports * and ** wildcards.
Returns list of matching file paths."
  (condition-case err
      (let* ((default-directory (if (file-directory-p path)
                                    path
                                  (file-name-directory path)))
             (matches (file-expand-wildcards pattern t)))
        (mapconcat #'identity matches "\n"))
    (error (format "Error during glob: %s" (error-message-string err)))))

(defun magent-tools--edit-file (path old-text new-text)
  "Edit file at PATH by replacing OLD-TEXT with NEW-TEXT.
OLD-TEXT must match exactly once in the file.
Returns success message or error."
  (condition-case err
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
          (format "Error: old_text not found in %s" path))
         ((> count 1)
          (format "Error: old_text found %d times in %s (must be unique)" count path))
         (t
          (let ((new-content (string-replace old-text new-text content)))
            (with-temp-buffer
              (insert new-content)
              (write-region (point-min) (point-max) path nil 0))
            (format "Successfully edited %s" path)))))
    (error (format "Error editing file: %s" (error-message-string err)))))

(defun magent-tools--emacs-eval (sexp &optional timeout)
  "Evaluate SEXP string as Emacs Lisp with optional TIMEOUT in seconds.
Returns the result as a readable string, or an error message."
  (condition-case err
      (let* ((timeout (or timeout 10))
             (form (car (read-from-string sexp)))
             (result (with-timeout (timeout (error "Evaluation timed out"))
                       (eval form t))))
        (prin1-to-string result))
    (error (format "Error evaluating sexp: %s" (error-message-string err)))))

(defun magent-tools--bash (command &optional timeout)
  "Execute shell COMMAND with optional TIMEOUT in seconds.
Returns command output (stdout + stderr)."
  (condition-case err
      (let* ((timeout (or timeout 30))
             (output (with-timeout (timeout (error "Command timed out"))
                       (shell-command-to-string command))))
        (if (string-blank-p output)
            "Command completed with no output"
          (string-trim-right output)))
    (error (format "Error executing command: %s" (error-message-string err)))))

(defun magent-tools--delegate (callback agent-name prompt)
  "Delegate PROMPT to subagent AGENT-NAME asynchronously.
CALLBACK is the gptel process-tool-result function.
Looks up the agent in the registry, validates it is a subagent,
then spawns a nested `gptel-request' with the subagent's configuration."
  (let ((agent (magent-agent-registry-get agent-name)))
    (cond
     ((null agent)
      (funcall callback (format "Error: agent '%s' not found" agent-name)))
     ((not (magent-agent-info-mode-p agent 'subagent))
      (funcall callback (format "Error: agent '%s' is not a subagent" agent-name)))
     (t
      (let* ((system-msg (or (magent-agent-info-prompt agent)
                              magent-system-prompt))
             (tools (magent-tools-get-gptel-tools agent))
             (prompt-list (list (cons 'prompt prompt))))
        (magent-agent-info-apply-gptel-overrides
         agent
         (lambda ()
           (let ((request-buffer (get-buffer-create " *magent-delegate-request*")))
             (with-current-buffer request-buffer
               (setq-local gptel-tools tools)
               (setq-local gptel-use-tools (if tools t nil)))
             (gptel-request
               prompt-list
               :buffer request-buffer
               :system system-msg
               :stream nil
               :callback (lambda (response _info)
                           (funcall callback
                                    (cond
                                     ((stringp response) response)
                                     ((null response) "Error: subagent request failed")
                                     (t (format "%s" response))))))))))))))

;;; Tool execution dispatcher (retained for direct use)

(defun magent-tools-execute (tool-name input)
  "Execute TOOL with INPUT (parsed JSON object).
Returns the result as a string."
  (let ((result
         (pcase tool-name
           ("read_file"
            (magent-tools--read-file (cdr (assq 'path input))))
           ("write_file"
            (magent-tools--write-file (cdr (assq 'path input))
                                       (cdr (assq 'content input))))
           ("edit_file"
            (magent-tools--edit-file (cdr (assq 'path input))
                                      (cdr (assq 'old_text input))
                                      (cdr (assq 'new_text input))))
           ("grep"
            (magent-tools--grep (cdr (assq 'pattern input))
                                 (cdr (assq 'path input))
                                 (cdr (assq 'case_sensitive input))))
           ("glob"
            (magent-tools--glob (cdr (assq 'pattern input))
                                (cdr (assq 'path input))))
           ("bash"
            (magent-tools--bash (cdr (assq 'command input))
                                (cdr (assq 'timeout input))))
           ("emacs_eval"
            (magent-tools--emacs-eval (cdr (assq 'sexp input))
                                       (cdr (assq 'timeout input))))
           ("delegate"
            (format "Error: delegate tool requires async context (use gptel tool calling)"))
           (_
            (format "Unknown tool: %s" tool-name)))))
    result))

;;; gptel-tool registrations

(require 'gptel)

(defvar magent-tools--read-file-tool
  (gptel-make-tool
   :name "read_file"
   :description "Read the contents of a file at the given path. Use this to inspect file contents before making changes."
   :args (list '(:name "path"
                 :type string
                 :description "Absolute or relative path to the file"))
   :function #'magent-tools--read-file
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
                 :description "The full content to write to the file"))
   :function #'magent-tools--write-file
   :confirm t
   :category "magent")
  "gptel-tool struct for write_file.")

(defvar magent-tools--edit-file-tool
  (gptel-make-tool
   :name "edit_file"
   :description "Edit a file by replacing an exact text match. The old_text must appear exactly once in the file. Use this for precise, surgical edits instead of rewriting entire files."
   :args (list '(:name "path"
                 :type string
                 :description "Absolute or relative path to the file")
               '(:name "old_text"
                 :type string
                 :description "The exact text to find and replace (must match exactly once)")
               '(:name "new_text"
                 :type string
                 :description "The text to replace old_text with"))
   :function #'magent-tools--edit-file
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
                 :optional t))
   :function (lambda (pattern path &optional case-sensitive)
               (magent-tools--grep pattern path case-sensitive))
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
                 :description "Root directory to search in"))
   :function #'magent-tools--glob
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
                 :optional t))
   :function (lambda (command &optional timeout)
               (magent-tools--bash command timeout))
   :confirm t
   :category "magent")
  "gptel-tool struct for bash.")

(defvar magent-tools--emacs-eval-tool
  (gptel-make-tool
   :name "emacs_eval"
   :description "Evaluate an Emacs Lisp expression. Returns the result as a string. Use for buffer inspection, Emacs state queries, running compilation, navigating code with xref, etc."
   :args (list '(:name "sexp"
                 :type string
                 :description "Emacs Lisp s-expression to evaluate")
               '(:name "timeout"
                 :type integer
                 :description "Timeout in seconds, defaults to 10"
                 :optional t))
   :function (lambda (sexp &optional timeout)
               (magent-tools--emacs-eval sexp timeout))
   :confirm t
   :category "magent")
  "gptel-tool struct for emacs_eval.")

(defvar magent-tools--delegate-tool
  (gptel-make-tool
   :name "delegate"
   :description "Delegate a task to a subagent. Use 'explore' for codebase search, file finding, and code analysis. Use 'general' for multi-step research and complex tasks. The subagent runs independently and returns its result."
   :args (list '(:name "agent"
                 :type string
                 :description "Name of the subagent to delegate to (e.g. 'explore', 'general')")
               '(:name "prompt"
                 :type string
                 :description "Task description for the subagent"))
   :function #'magent-tools--delegate
   :async t
   :category "magent")
  "gptel-tool struct for delegate.")

;;; Tool filtering by agent permissions

(defconst magent-tools--permission-keys
  '(read write edit grep glob bash emacs_eval delegate)
  "Canonical list of magent tool permission key symbols.
This is the single source of truth for all tool names in the permission system.")

(defvar magent-tools--name-to-permission-key
  '(("read_file"  . read)
    ("write_file" . write)
    ("edit_file"  . edit)
    ("grep"       . grep)
    ("glob"       . glob)
    ("bash"       . bash)
    ("emacs_eval" . emacs_eval)
    ("delegate"   . delegate))
  "Maps gptel tool names to magent permission key symbols.")

(defvar magent-tools--all-gptel-tools
  (list magent-tools--read-file-tool
        magent-tools--write-file-tool
        magent-tools--edit-file-tool
        magent-tools--grep-tool
        magent-tools--glob-tool
        magent-tools--bash-tool
        magent-tools--emacs-eval-tool
        magent-tools--delegate-tool)
  "All magent tools as gptel-tool structs.")

(defun magent-tools-get-gptel-tools (agent-info)
  "Return a list of gptel-tool structs allowed for AGENT-INFO.
Filters by both `magent-enable-tools' global config and agent permissions."
  (let ((permission (and agent-info
                         (magent-agent-info-permission agent-info))))
    (cl-remove-if-not
     (lambda (tool)
       (let* ((tool-name (gptel-tool-name tool))
              (perm-key (cdr (assoc tool-name magent-tools--name-to-permission-key))))
         (and
          ;; Globally enabled
          (or (null perm-key) (memq perm-key magent-enable-tools))
          ;; Agent permission allows it
          (or (null permission)
              (magent-permission-allow-p permission perm-key)))))
     magent-tools--all-gptel-tools)))

(provide 'magent-tools)
;;; magent-tools.el ends here
