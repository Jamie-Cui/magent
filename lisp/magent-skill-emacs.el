;;; magent-skill-emacs.el --- Built-in Emacs interaction skill  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Built-in skill for interacting with the running Emacs instance.
;; Provides 6 operations: list-functions, describe-function,
;; eval-expression, execute-keys, minibuffer-prompt, current-buffer-state.

;;; Code:

(require 'cl-lib)
(require 'help-fns)

;;; Skill metadata

(defconst magent-skill-emacs--description
  "Interact with the running Emacs instance. Operations: list-functions, describe-function, eval-expression, execute-keys, minibuffer-prompt, current-buffer-state."
  "Description of the built-in emacs skill.")

;;; Skill implementations

(defun magent-skill-emacs--list-functions (prefix)
  "Return a list of interactive function names matching PREFIX."
  (let (result)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (commandp sym)
                  (string-prefix-p prefix (symbol-name sym)))
         (push (symbol-name sym) result))))
    (sort result #'string<)))

(defun magent-skill-emacs--describe-function (name)
  "Return the docstring and argument list for function NAME."
  (let ((sym (intern-soft name)))
    (unless (and sym (fboundp sym))
      (error "Function %s is not defined" name))
    (let ((arglist (help-function-arglist sym t))
          (docstring (documentation sym t)))
      (format "(%s %s)\n\n%s"
              name
              (if arglist (mapconcat #'symbol-name arglist " ") "")
              (or docstring "No documentation available.")))))

(defun magent-skill-emacs--eval-expression (expr)
  "Evaluate EXPR (a string) and return the result as a string."
  (format "%S" (eval (car (read-from-string expr)) t)))

(defun magent-skill-emacs--execute-keys (keys)
  "Execute KEYS as if typed by the user.
KEYS is a string in `kbd' format (e.g. \"C-x C-s\")."
  (execute-kbd-macro (kbd keys))
  (format "Executed: %s" keys))

(defun magent-skill-emacs--minibuffer-prompt ()
  "Return the current minibuffer prompt and contents, or a message if inactive."
  (if (minibufferp (window-buffer (minibuffer-window)))
      (with-current-buffer (window-buffer (minibuffer-window))
        (let ((prompt (minibuffer-prompt))
              (contents (minibuffer-contents)))
          (format "Prompt: %s\nContents: %s" (or prompt "") contents)))
    "Minibuffer is not active."))

(defun magent-skill-emacs--current-buffer-state ()
  "Return the name, major mode, and first few lines of the focused buffer."
  (let ((buf (window-buffer (selected-window))))
    (with-current-buffer buf
      (let ((name (buffer-name))
            (mode (symbol-name major-mode))
            (pt (point))
            (excerpt (buffer-substring-no-properties
                      (point-min)
                      (min (point-max) (+ (point-min) 2000)))))
        (format "Buffer: %s\nMode: %s\nPoint: %d\n---\n%s" name mode pt excerpt)))))

;;; Dispatch

(defun magent-skill-emacs-invoke (operation args)
  "Invoke emacs skill OPERATION with ARGS.
Returns the result as a string."
  (condition-case err
      (pcase operation
        ("list-functions"
         (if (null args)
             "Error: list-functions requires prefix argument"
           (let ((result (magent-skill-emacs--list-functions (car args))))
             (if result
                 (mapconcat #'identity result "\n")
               "No matching functions found."))))
        ("describe-function"
         (if (null args)
             "Error: describe-function requires function-name argument"
           (magent-skill-emacs--describe-function (car args))))
        ("eval-expression"
         (if (null args)
             "Error: eval-expression requires expression argument"
           (magent-skill-emacs--eval-expression (car args))))
        ("execute-keys"
         (if (null args)
             "Error: execute-keys requires keys argument"
           (magent-skill-emacs--execute-keys (car args))))
        ("minibuffer-prompt"
         (magent-skill-emacs--minibuffer-prompt))
        ("current-buffer-state"
         (magent-skill-emacs--current-buffer-state))
        (_
         (format "Error: unknown operation '%s'. Available: list-functions, describe-function, eval-expression, execute-keys, minibuffer-prompt, current-buffer-state"
                 operation)))
    (error (format "Error executing emacs skill: %s" (error-message-string err)))))

(provide 'magent-skill-emacs)
;;; magent-skill-emacs.el ends here
