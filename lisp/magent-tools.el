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
  "Search for PATTERN in files under PATH.
If CASE-SENSITIVE is nil, performs case-insensitive search.
Returns matching lines with file paths."
  (let* ((case-fold-search (not case-sensitive))
         (default-directory (if (file-directory-p path)
                                path
                              (file-name-directory path)))
         (matches ()))
    (condition-case err
        (progn
          (dolist (file (directory-files-recursively default-directory ""))
            (when (and (not (file-directory-p file))
                       (not (string-match-p "\\(?:\\.git\\|\\.svn\\|node_modules\\|\\.hg\\)" file)))
              (with-temp-buffer
                (insert-file-contents file)
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward pattern nil t)
                    (let* ((line (buffer-substring (line-beginning-position)
                                                   (line-end-position)))
                           (line-num (line-number-at-pos)))
                      (push (format "%s:%d:%s" file line-num (string-trim line))
                            matches)))))))
          (mapconcat #'identity (nreverse matches) "\n"))
      (error (format "Error during grep: %s" (error-message-string err))))))

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

(defvar magent-tools--grep-tool
  (gptel-make-tool
   :name "grep"
   :description "Search for a regex pattern in files under a directory. Returns matching lines with file paths and line numbers."
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

;;; Tool filtering by agent permissions

(defvar magent-tools--name-to-permission-key
  '(("read_file"  . read)
    ("write_file" . write)
    ("grep"       . grep)
    ("glob"       . glob)
    ("bash"       . bash))
  "Maps gptel tool names to magent permission key symbols.")

(defvar magent-tools--all-gptel-tools
  (list magent-tools--read-file-tool
        magent-tools--write-file-tool
        magent-tools--grep-tool
        magent-tools--glob-tool
        magent-tools--bash-tool)
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
