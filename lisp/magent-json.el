;;; magent-json.el --- JSON-safe value helpers for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Helpers for converting live Lisp/provider values into data accepted by
;; Emacs JSON encoders and provider payload history.

;;; Code:

(require 'json)

(defun magent-json--symbol-value (value)
  "Return VALUE as a JSON string value."
  (let ((name (symbol-name value)))
    (if (string-prefix-p ":" name)
        (substring name 1)
      name)))

(defun magent-json-safe-name (name &optional fallback)
  "Return NAME as a JSON-safe string.
FALLBACK is used when NAME is nil."
  (cond
   ((stringp name) name)
   ((symbolp name) (magent-json--symbol-value name))
   ((null name) (or fallback "unknown"))
   (t (format "%s" name))))

(defun magent-json--object-key (key)
  "Return KEY as a stable symbol key for JSON object alists."
  (cond
   ((keywordp key)
    (intern (substring (symbol-name key) 1)))
   ((symbolp key) key)
   ((stringp key) (intern key))
   (t (intern (format "%s" key)))))

(defun magent-json--plist-p (value)
  "Return non-nil when VALUE is a keyword plist."
  (and (listp value)
       (let ((tail value)
             (ok t))
         (while (and ok tail)
           (if (and (consp tail)
                    (keywordp (car tail))
                    (consp (cdr tail)))
               (setq tail (cddr tail))
             (setq ok nil)))
         ok)))

(defun magent-json--alist-p (value)
  "Return non-nil when VALUE is a proper alist."
  (and (consp value)
       (let ((tail value)
             (ok t))
         (while (and ok tail)
           (if (and (consp tail)
                    (consp (car tail))
                    (not (keywordp (caar tail))))
               (setq tail (cdr tail))
             (setq ok nil)))
         (and ok (null tail)))))

(defun magent-json-safe-value (value)
  "Return VALUE converted to data accepted by Emacs JSON encoders.
This is used at Magent/gptel boundaries where live provider data can contain
Lisp symbols or lists that `json-serialize' rejects."
  (cond
   ((null value) :null)
   ((or (stringp value) (numberp value)
        (eq value t)
        (eq value :json-false)
        (eq value :null))
    value)
   ((symbolp value)
    (magent-json--symbol-value value))
   ((vectorp value)
    (vconcat (mapcar #'magent-json-safe-value (append value nil))))
   ((hash-table-p value)
    (let ((table (make-hash-table :test #'equal)))
      (maphash (lambda (key val)
                 (puthash (if (stringp key) key (format "%s" key))
                          (magent-json-safe-value val)
                          table))
               value)
      table))
   ((magent-json--plist-p value)
    (let (out)
      (while value
        (let ((key (pop value))
              (val (pop value)))
          (setq out (append out
                            (list key (magent-json-safe-value val))))))
      out))
   ((magent-json--alist-p value)
    (mapcar (lambda (entry)
              (cons (magent-json--object-key (car entry))
                    (magent-json-safe-value (cdr entry))))
            value))
   ((consp value)
    (vconcat (mapcar #'magent-json-safe-value value)))
   (t
    (format "%S" value))))

(defun magent-json-safe-tool-args (args)
  "Return JSON-safe tool ARGS for display, persistence, and prompt reuse.
Nil values in keyword plists are omitted because gptel parses JSON null tool
arguments as nil, and optional nil arguments should not round-trip as `{}`."
  (cond
   ((null args) nil)
   ((magent-json--plist-p args)
    (let (out)
      (while args
        (let ((key (pop args))
              (val (pop args)))
          (unless (null val)
            (setq out (append out
                              (list key (magent-json-safe-value val)))))))
      out))
   (t
    (magent-json-safe-value args))))

(defun magent-json-encode (value)
  "Encode VALUE as JSON after converting it with `magent-json-safe-value'."
  (let ((json-null :null)
        (json-false :json-false))
    (json-encode (magent-json-safe-value value))))

(provide 'magent-json)
;;; magent-json.el ends here
