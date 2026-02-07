;;; magent-tools.el --- Tool implementations for OpenCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Tool implementations that the AI agent can use to interact with files and the system.

;;; Code:

(require 'cl-lib)
(require 'magent-config)

;;; Tool definitions

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

;;; Tool schemas for AI

(defun magent-tools-get-definitions ()
  "Return tool definitions in the format expected by the API."
  (let ((tools nil))
    (when (memq 'read magent-enable-tools)
      (push '((name . "read_file")
              (description . "Read the contents of a file. Use this to see the current state of a file before making changes.")
              (input_schema type-object
                           (properties path)
                           (required path)
                           (additionalProperties . :json-false)))
            tools))
    (when (memq 'write magent-enable-tools)
      (push '((name . "write_file")
              (description . "Write content to a file. Creates parent directories if they don't exist.")
              (input_schema type-object
                           (properties path content)
                           (required path content)
                           (additionalProperties . :json-false)))
            tools))
    (when (memq 'grep magent-enable-tools)
      (push '((name . "grep")
              (description . "Search for a pattern in files under a directory. Uses regex matching.")
              (input_schema type-object
                           (properties pattern path case-sensitive)
                           (required pattern path)
                           (additionalProperties . :json-false)))
            tools))
    (when (memq 'glob magent-enable-tools)
      (push '((name . "glob")
              (description . "Find files matching a pattern. Supports * (any characters) and ** (recursive).")
              (input_schema type-object
                           (properties pattern path)
                           (required pattern path)
                           (additionalProperties . :json-false)))
            tools))
    (when (memq 'bash magent-enable-tools)
      (push '((name . "bash")
              (description . "Execute a shell command. Use for running tests, builds, git operations, etc.")
              (input_schema type-object
                           (properties command timeout)
                           (required command)
                           (additionalProperties . :json-false)))
            tools))
    (nreverse tools)))

;; Tool property definitions
(defvar magent-tools--schema-type-object
  '((type . "object")))

(defvar magent-tools--prop-path
  '((path (type . "string") (description . "Absolute or relative path to the file"))))

(defvar magent-tools--prop-content
  '((content (type . "string") (description . "Content to write to the file"))))

(defvar magent-tools--prop-pattern
  '((pattern (type . "string") (description . "Regex pattern to search for"))))

(defvar magent-tools--prop-search-path
  '((path (type . "string") (description . "Directory to search in"))))

(defvar magent-tools--prop-case-sensitive
  '((case_sensitive (type . "boolean") (description . "Whether search is case-sensitive") (default . :json-false))))

(defvar magent-tools--prop-glob-pattern
  '((pattern (type . "string") (description . "Glob pattern, e.g. *.el or **/*.ts"))))

(defvar magent-tools--prop-command
  '((command (type . "string") (description . "Shell command to execute"))))

(defvar magent-tools--prop-timeout
  '((timeout (type . "integer") (description . "Timeout in seconds") (default . 30))))

;;; Tool execution dispatcher

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

(provide 'magent-tools)
;;; magent-tools.el ends here
