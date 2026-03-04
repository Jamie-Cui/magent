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

(declare-function magent-skill-emacs-invoke "magent-skill-emacs")
(declare-function magent-skills-get "magent-skills")
(declare-function magent-skills-list "magent-skills")
(declare-function magent-skills-invoke "magent-skills")

;;; Tool implementations

(defalias 'magent-tools--project-root #'magent-project-root
  "Return the project root directory for tool operations.")

(defun magent-tools--resolve-path (path)
  "Resolve PATH for tool operations.
Expands ~ and environment variables first, then resolves relative
paths against the project root.  Never returns nil."
  (let* ((expanded (expand-file-name (substitute-in-file-name path)))
         (root (magent-tools--project-root)))
    (cond
     ((file-directory-p expanded) expanded)
     ((file-exists-p expanded) expanded)
     (t
      (let ((resolved (expand-file-name path root)))
        (if (or (file-directory-p resolved) (file-exists-p resolved))
            resolved
          root))))))

(defun magent-tools--read-file (callback path)
  "Read contents of file at PATH asynchronously.
CALLBACK is called with the file contents or error message."
  (let ((path (expand-file-name (substitute-in-file-name path))))
    (condition-case err
        (if (file-exists-p path)
            (let ((buf (generate-new-buffer " *magent-read*")))
              (with-current-buffer buf
                (insert-file-contents path)
                (let ((content (buffer-string)))
                  (kill-buffer buf)
                  (funcall callback content))))
          (funcall callback (format "Error: file not found: %s" path)))
      (error (funcall callback (format "Error reading file: %s" (error-message-string err)))))))

(defun magent-tools--write-file (callback path content)
  "Write CONTENT to file at PATH asynchronously.
Creates parent directories if needed.
CALLBACK is called with success message or error."
  (let ((path (expand-file-name (substitute-in-file-name path))))
    (condition-case err
        (progn
          (let ((dir (file-name-directory path)))
            (when (and dir (not (file-exists-p dir)))
              (make-directory dir t)))
          (with-temp-buffer
            (insert content)
            (write-region (point-min) (point-max) path nil 0))
          (funcall callback (format "Successfully wrote %s" path)))
      (error (funcall callback (format "Error writing file: %s" (error-message-string err)))))))

(defun magent-tools--grep (callback pattern path &optional case-sensitive)
  "Search for PATTERN in files under PATH using ripgrep asynchronously.
If CASE-SENSITIVE is nil, performs case-insensitive search.
CALLBACK is called with matching lines or error message."
  (let* ((resolved (magent-tools--resolve-path path))
         (default-directory (if (file-directory-p resolved)
                                resolved
                              (or (file-name-directory resolved)
                                  (magent-tools--project-root))))
         (buf (generate-new-buffer " *magent-grep*"))
         (args (list "--no-heading" "--line-number" "--color=never"
                     (format "--max-count=%d" magent-grep-max-matches))))
    (unless case-sensitive
      (push "--ignore-case" args))
    (unless (file-directory-p resolved)
      (push resolved args))
    (push pattern args)
    (make-process
     :name "magent-grep"
     :buffer buf
     :command (cons magent-grep-program (nreverse args))
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((output (with-current-buffer buf (buffer-string))))
           (kill-buffer buf)
           (funcall callback
                    (if (string-blank-p output)
                        "No matches found"
                      (string-trim-right output)))))))))

(defun magent-tools--glob (callback pattern path)
  "Find files matching PATTERN under PATH asynchronously.
Supports * and ** wildcards.
CALLBACK is called with list of matching file paths."
  (condition-case err
      (let* ((resolved (magent-tools--resolve-path path))
             (default-directory (if (file-directory-p resolved)
                                    resolved
                                  (or (file-name-directory resolved)
                                      (magent-tools--project-root))))
             (matches
              (if (string-match-p "\\*\\*" pattern)
                  ;; ** requires recursive search
                  (let* ((parts (split-string pattern "\\*\\*/?"))
                         (file-regexp (if (> (length parts) 1)
                                          (wildcard-to-regexp (car (last parts)))
                                        nil)))
                    (directory-files-recursively
                     default-directory
                     (or file-regexp ".")))
                ;; Single * uses file-expand-wildcards
                (file-expand-wildcards pattern t))))
        (funcall callback (mapconcat #'identity matches "\n")))
    (error (funcall callback (format "Error during glob: %s" (error-message-string err))))))

(defun magent-tools--edit-file (callback path old-text new-text)
  "Edit file at PATH by replacing OLD-TEXT with NEW-TEXT asynchronously.
OLD-TEXT must match exactly once in the file.
CALLBACK is called with success message or error."
  (let ((path (expand-file-name (substitute-in-file-name path))))
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
            (funcall callback (format "Error: old_text not found in %s" path)))
           ((> count 1)
            (funcall callback (format "Error: old_text found %d times in %s (must be unique)" count path)))
           (t
            (let ((new-content (string-replace old-text new-text content)))
              (with-temp-buffer
                (insert new-content)
                (write-region (point-min) (point-max) path nil 0))
              (funcall callback (format "Successfully edited %s" path))))))
      (error (funcall callback (format "Error editing file: %s" (error-message-string err)))))))

(defun magent-tools--emacs-eval (callback sexp &optional timeout)
  "Evaluate SEXP string as Emacs Lisp with optional TIMEOUT in seconds.
CALLBACK is called with the result as a readable string, or an error message."
  (condition-case err
      (let* ((timeout (or timeout magent-emacs-eval-timeout))
             (form (car (read-from-string sexp)))
             (timer nil)
             (result nil)
             (done nil))
        (setq timer
              (run-at-time
               timeout nil
               (lambda ()
                 (unless done
                   (setq done t)
                   (cancel-timer timer)
                   (funcall callback "Error: Evaluation timed out")))))
        ;; Run evaluation in a timer to avoid blocking
        (run-at-time
         0 nil
         (lambda ()
           (condition-case err
               (progn
                 (setq result (eval form t))
                 (unless done
                   (setq done t)
                   (cancel-timer timer)
                   (funcall callback (prin1-to-string result))))
             (error
              (unless done
                (setq done t)
                (cancel-timer timer)
                (funcall callback (format "Error evaluating sexp: %s" (error-message-string err)))))))))
    (error (funcall callback (format "Error evaluating sexp: %s" (error-message-string err))))))

(defun magent-tools--bash (callback command &optional timeout)
  "Execute shell COMMAND asynchronously with optional TIMEOUT in seconds.
CALLBACK is called with the command output (stdout + stderr)."
  (let* ((timeout (or timeout magent-bash-timeout))
         (buf (generate-new-buffer " *magent-bash*"))
         (timer nil)
         (proc nil)
         (cleanup
          (lambda ()
            (when timer (cancel-timer timer))
            (when (process-live-p proc) (delete-process proc))
            (when (buffer-live-p buf) (kill-buffer buf)))))
    (setq timer
          (run-at-time
           timeout nil
           (lambda ()
             (when (process-live-p proc)
               (delete-process proc))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (let ((output (buffer-string)))
                   (funcall cleanup)
                   (funcall callback
                            (if (string-blank-p output)
                                "Command timed out with no output"
                              (format "Command timed out. Partial output:\n%s"
                                      (string-trim-right output))))))))))
    (setq proc
          (make-process
           :name "magent-bash"
           :buffer buf
           :command (list shell-file-name shell-command-switch command)
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (let ((output (with-current-buffer buf (buffer-string))))
                 (funcall cleanup)
                 (funcall callback
                          (if (string-blank-p output)
                              "Command completed with no output"
                            (string-trim-right output))))))))))

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
           (let ((request-buffer (generate-new-buffer " *magent-delegate-request*")))
             (with-current-buffer request-buffer
               (setq-local gptel-tools tools)
               (setq-local gptel-use-tools (if tools t nil)))
             (gptel-request
                 prompt-list
               :buffer request-buffer
               :system system-msg
               :stream nil
               :callback (lambda (response _info)
                           ;; Defer buffer kill so gptel's sentinel can
                           ;; finish using it after this callback returns.
                           (run-at-time 0 nil
                                        (lambda ()
                                          (when (buffer-live-p request-buffer)
                                            (kill-buffer request-buffer))))
                           (funcall callback
                                    (cond
                                     ((stringp response) response)
                                     ((null response) "Error: subagent request failed")
                                     (t (format "%s" response))))))))))))))

(defun magent-tools--web-search (callback query &optional max-results)
  "Search the web using DuckDuckGo asynchronously.
CALLBACK is called with formatted search results or error message.
QUERY is the search string.
MAX-RESULTS is the maximum number of results to return (default 5)."
  (let ((max-results (or max-results 5))
        (url (format "https://html.duckduckgo.com/html/?q=%s"
                     (url-hexify-string query))))
    (condition-case err
        (url-retrieve
         url
         (lambda (status)
           (magent-tools--web-search-callback status callback query max-results))
         nil t t)
      (error (funcall callback (format "Error initiating search: %s" (error-message-string err)))))))

(defun magent-tools--web-search-callback (status callback query max-results)
  "Handle HTTP response for web search.
STATUS is the url-retrieve status list.
CALLBACK is called with formatted results.
QUERY is the original search query.
MAX-RESULTS is the maximum number of results."
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
    (error (funcall callback (format "Error parsing results: %s" (error-message-string err))))))

(defun magent-tools--parse-ddg-results (dom max-results)
  "Parse DuckDuckGo HTML DOM and extract search results.
Returns list of plists with :title and :url keys, limited to MAX-RESULTS."
  (let ((results nil)
        (count 0))
    (dolist (result (dom-by-class dom "result__a"))
      (when (< count max-results)
        (let ((title (dom-text result))
              (url (dom-attr result 'href)))
          (when (and title url (not (string-blank-p title)))
            (push (list :title (string-trim title) :url url) results)
            (cl-incf count)))))
    (nreverse results)))

(defun magent-tools--format-search-results (query results)
  "Format RESULTS list into readable string for QUERY."
  (concat (format "Search results for \"%s\":\n\n" query)
          (mapconcat
           (lambda (result)
             (format "%d. %s\n   %s"
                     (1+ (cl-position result results :test #'equal))
                     (plist-get result :title)
                     (plist-get result :url)))
           results
           "\n\n")))

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
                       :description "The full content to write to the file"))
   :function #'magent-tools--write-file
   :async t
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
                       :optional t))
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
                       :description "Root directory to search in"))
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
                       :optional t))
   :function #'magent-tools--bash
   :async t
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
   :function #'magent-tools--emacs-eval
   :async t
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

(defun magent-tools--skill-invoke (callback skill-name operation &rest args)
  "Invoke OPERATION from SKILL-NAME with ARGS asynchronously.
CALLBACK is called with the result.
Only works for tool-type skills.  Instruction-type skills are
automatically included in the system prompt."
  (require 'magent-skills)
  (magent-skills-invoke skill-name operation args callback))

(defvar magent-tools--skill-invoke-tool
  (gptel-make-tool
   :name "skill_invoke"
   :description "Invoke a tool-type skill operation. Only tool-type skills can be invoked this way. Instruction-type skills are automatically active in the system prompt. Check available skills and their operations before invoking."
   :args (list '(:name "skill_name"
                       :type string
                       :description "Name of the skill to invoke")
               '(:name "operation"
                       :type string
                       :description "Operation to perform (varies by skill)")
               '(:name "args"
                       :type array
                       :description "Arguments for the operation (varies by operation)"
                       :optional t))
   :function #'magent-tools--skill-invoke
   :async t
   :category "magent")
  "gptel-tool struct for skill_invoke.")

(defvar magent-tools--web-search-tool
  (gptel-make-tool
   :name "web_search"
   :description "Search the web using DuckDuckGo. Returns titles and URLs of search results. Use this to find current information, documentation, or online resources."
   :args (list '(:name "query"
                       :type string
                       :description "Search query string")
               '(:name "max_results"
                       :type integer
                       :description "Maximum number of results to return (default 5)"
                       :optional t))
   :function #'magent-tools--web-search
   :async t
   :category "magent")
  "gptel-tool struct for web_search.")

;;; Tool filtering by agent permissions

(defconst magent-tools--permission-keys
  '(read write edit grep glob bash emacs_eval delegate skill web_search)
  "Canonical list of magent tool permission key symbols.
This is the single source of truth for all tool names in the permission system.")

(defvar magent-tools--name-to-permission-key
  '(("read_file"    . read)
    ("write_file"   . write)
    ("edit_file"    . edit)
    ("grep"         . grep)
    ("glob"         . glob)
    ("bash"         . bash)
    ("emacs_eval"   . emacs_eval)
    ("delegate"     . delegate)
    ("skill_invoke" . skill)
    ("web_search"   . web_search))
  "Maps gptel tool names to magent permission key symbols.")

(defun magent-tools-permission-key (tool-name)
  "Return the permission key symbol for TOOL-NAME, or nil if unknown."
  (cdr (assoc tool-name magent-tools--name-to-permission-key)))

(defvar magent-tools--all-gptel-tools
  (list magent-tools--read-file-tool
        magent-tools--write-file-tool
        magent-tools--edit-file-tool
        magent-tools--grep-tool
        magent-tools--glob-tool
        magent-tools--bash-tool
        magent-tools--emacs-eval-tool
        magent-tools--delegate-tool
        magent-tools--skill-invoke-tool
        magent-tools--web-search-tool)
  "All magent tools as gptel-tool structs.")

(defun magent-tools-get-gptel-tools (agent-info)
  "Return a list of gptel-tool structs available for AGENT-INFO.
Filters by both `magent-enable-tools' global config and agent permissions.
Tools with \\='ask permission are included (they will be confirmed at runtime)."
  (let ((permission (and agent-info
                         (magent-agent-info-permission agent-info))))
    (cl-remove-if-not
     (lambda (tool)
       (let* ((tool-name (gptel-tool-name tool))
              (perm-key (magent-tools-permission-key tool-name)))
         (and
          ;; Globally enabled
          (or (null perm-key) (memq perm-key magent-enable-tools))
          ;; Agent permission allows or asks
          (or (null permission)
              (magent-permission-tool-available-p permission perm-key)))))
     magent-tools--all-gptel-tools)))

(defun magent-tools-get-magent-tools (agent-info)
  "Return a list of magent tool plists allowed for AGENT-INFO.
Filters by both `magent-enable-tools' global config and agent permissions.
Returns tools in magent internal format (plists) instead of gptel-tool structs."
  (let ((gptel-tools (magent-tools-get-gptel-tools agent-info)))
    (mapcar #'magent-tools--gptel-to-magent-tool gptel-tools)))

(defun magent-tools--gptel-to-magent-tool (gptel-tool)
  "Convert a gptel-tool struct to magent tool plist format."
  (let ((tool-name (gptel-tool-name gptel-tool)))
    (list :name tool-name
          :description (gptel-tool-description gptel-tool)
          :args (gptel-tool-args gptel-tool)
          :function (gptel-tool-function gptel-tool)
          :async (gptel-tool-async gptel-tool)
          :perm-key (magent-tools-permission-key tool-name))))

(provide 'magent-tools)
;;; magent-tools.el ends here
