;;; magent-log.el --- UI-neutral logging for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Small logging mechanism shared by Magent modules.  A built-in buffer sink
;; keeps diagnostics available in Emacs, while warnings and errors still reach
;; `message' when Magent is running headlessly without a sink.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar magent-log--sinks nil
  "Registered Magent log sinks.
Each sink receives the formatted message and its normalized severity.")

(defconst magent-log--level-order
  '((debug . 10)
    (info . 20)
    (warn . 30)
    (error . 40))
  "Priority order for Magent log filtering.")

(defun magent-log-message-level (message)
  "Return normalized severity symbol for MESSAGE."
  (let ((prefix (and (string-match
                      "\\`\\([A-Z]+\\)\\(?:\\s-\\|:\\|$\\)" message)
                     (match-string 1 message))))
    (pcase prefix
      ("DEBUG" 'debug)
      ((or "INFO" "PERM") 'info)
      ("WARN" 'warn)
      ("ERROR" 'error)
      (_ 'info))))

(defun magent-log-add-sink (sink)
  "Register SINK and return it."
  (cl-pushnew sink magent-log--sinks :test #'eq)
  sink)

(defun magent-log-remove-sink (sink)
  "Unregister SINK."
  (setq magent-log--sinks (delq sink magent-log--sinks)))

(define-derived-mode magent-log-mode special-mode "MagentLog"
  "Major mode for the Magent diagnostic log buffer."
  (visual-line-mode 1)
  (setq-local display-fill-column-indicator-column nil)
  (setq-local font-lock-defaults
              '((("\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)\\]"
                   0 font-lock-comment-face)
                  ("\\<\\(ERROR\\|WARNING\\|WARN\\|INFO\\|DEBUG\\|PERM\\)\\>"
                   0 font-lock-keyword-face)))))

(defun magent-log-buffer ()
  "Return the Magent diagnostic log buffer, creating it when needed."
  (let ((buffer
         (get-buffer-create
          (if (boundp 'magent-log-buffer-name)
              magent-log-buffer-name
            "*magent-log*"))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magent-log-mode)
        (magent-log-mode)))
    buffer))

(defun magent-log--buffer-enabled-p (level)
  "Return non-nil when LEVEL should be written to the log buffer."
  (and (if (boundp 'magent-enable-logging) magent-enable-logging t)
       (>= (alist-get level magent-log--level-order)
           (alist-get (if (boundp 'magent-log-level)
                          magent-log-level
                        'info)
                      magent-log--level-order))))

(defun magent-log--buffer-sink (text level)
  "Write formatted log TEXT at LEVEL to the Magent log buffer."
  (when (magent-log--buffer-enabled-p level)
    (with-current-buffer (magent-log-buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "[%s] %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S")
                        text))))))

(magent-log-add-sink #'magent-log--buffer-sink)

(defun magent-log (format-string &rest args)
  "Log FORMAT-STRING with ARGS through registered sinks."
  (let* ((text (apply #'format format-string args))
         (level (magent-log-message-level text)))
    (if magent-log--sinks
        (dolist (sink magent-log--sinks)
          (condition-case err
              (funcall sink text level)
            (error
             (message "magent-log sink error: %s"
                      (error-message-string err)))))
      (when (memq level '(warn error))
        (message "%s" text)))
    text))

(provide 'magent-log)
;;; magent-log.el ends here
