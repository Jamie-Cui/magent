;;; magent-log.el --- UI-neutral logging for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Small logging mechanism shared by core and UI modules.  Presentation is
;; supplied by sinks; warnings and errors still reach `message' when Magent is
;; running headlessly without a sink.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar magent-log--sinks nil
  "Registered Magent log sinks.
Each sink receives the formatted message and its normalized severity.")

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
