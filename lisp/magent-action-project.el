;;; magent-action-project.el --- Trusted project Action loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Discover .magent/actions/*.el and load only content approved by the user.
;; Approval is stored outside projects and covers file names and source text.
;; Sources are evaluated from the approved snapshot, with project-scoped
;; registration defaults.  This is trusted live Elisp, not a sandbox: arbitrary
;; Lisp side effects cannot be rolled back when a file fails to load.

;;; Code:

(require 'json)
(require 'magent-action)

(declare-function magent-runtime-context-scope "magent-runtime")

(defvar magent-action-project--loaded (make-hash-table :test #'equal)
  "Fingerprints of project sources currently registered in this Emacs.")

(defvar magent-action-project--registrations (make-hash-table :test #'equal)
  "Exact file-loaded registration tokens owned by each project.")

(defvar magent-action-project--declined (make-hash-table :test #'equal)
  "Project fingerprints declined during this Emacs session.")

(defvar magent-action-project--loading nil
  "Non-nil during project loading, including registry notifications.")

(defun magent-action-project--snapshot (scope)
  "Read sorted project source files for SCOPE as (FILE . TEXT) pairs."
  (let ((directory (expand-file-name ".magent/actions" scope)))
    (when (file-directory-p directory)
      (mapcar
       (lambda (file)
         (unless (file-regular-p file)
           (error "Project Action is not a regular file: %s" file))
         (cons file (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))))
       (directory-files directory t "\\`[^.].*\\.el\\'")))))

(defun magent-action-project--read-trust ()
  "Read the local project trust map, rejecting malformed records."
  (if (file-exists-p magent-action-project-trust-file)
      (with-temp-buffer
        (insert-file-contents magent-action-project-trust-file)
        (let ((records (json-parse-buffer :object-type 'hash-table)))
          (unless (hash-table-p records)
            (error "Invalid project Action trust file: expected an object"))
          (maphash
           (lambda (scope fingerprint)
             (unless (and (stringp scope) (stringp fingerprint)
                          (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" fingerprint))
               (error "Invalid project Action trust record for %S" scope)))
           records)
          records))
    (make-hash-table :test #'equal)))

(defun magent-action-project--write-trust (records)
  "Atomically write approved project RECORDS outside the project."
  (let* ((file (expand-file-name magent-action-project-trust-file))
         (directory (file-name-directory file))
         temporary)
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temporary (make-temp-file (expand-file-name ".action-trust-" directory)))
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (json-serialize records) nil temporary nil 'silent))
          (set-file-modes temporary #o600)
          (rename-file temporary file t)
          (setq temporary nil))
      (when temporary (delete-file temporary)))))

(defun magent-action-remove-project-scope (scope)
  "Remove file-loaded Action registrations and the fingerprint for SCOPE."
  (let ((scope (magent-session-canonical-scope scope))
        (magent-action-project--loading t))
    (when scope
      (remhash scope magent-action-project--loaded)
      (let (changed)
        (let ((magent-action--suppress-registry-hooks t))
          (dolist (registration (gethash scope magent-action-project--registrations))
            (when (magent-action-unregister registration) (setq changed t))))
        (remhash scope magent-action-project--registrations)
        (when changed (magent-action--registry-changed))))))

(defun magent-action-project--approve (scope snapshot fingerprint records)
  "Approve SCOPE's SNAPSHOT with FINGERPRINT using local RECORDS."
  (cond
   ((equal fingerprint (gethash scope records)) t)
   ((equal fingerprint (gethash scope magent-action-project--declined)) nil)
   ((and magent-action-project--allow-prompt (not noninteractive))
    (if (yes-or-no-p
         (format "Trust and execute %d project Action file(s) in %s with full Emacs access (%s)? "
                 (length snapshot) scope
                 (mapconcat (lambda (entry) (file-name-nondirectory (car entry)))
                            snapshot ", ")))
        (progn
          (puthash scope fingerprint records)
          (magent-action-project--write-trust records)
          t)
      (puthash scope fingerprint magent-action-project--declined)
      nil))
   (t nil)))

(defun magent-action-project--evaluate (scope snapshot)
  "Evaluate SCOPE's approved SNAPSHOT and return its staged registry.
All files must succeed before any project registrations are published."
  (let ((magent-action--registry (copy-sequence magent-action--registry))
        (magent-action--project-registration-scope scope)
        (magent-action--suppress-registry-hooks t)
        (default-directory (file-name-as-directory scope)))
    (dolist (entry snapshot)
      (let ((load-file-name (car entry)))
        (condition-case err
            (with-temp-buffer
              (insert (cdr entry))
              (eval-buffer nil nil load-file-name))
          (error
           (error "Project Action %s failed: %s"
                  load-file-name (error-message-string err))))))
    magent-action--registry))

(defun magent-action-load-project-scope (scope)
  "Discover and refresh approved project Action sources for SCOPE.
Changed sources immediately invalidate old project registrations.  Prompt
only from explicit interactive entry points; background discovery never
executes unapproved source.  An unchanged declined snapshot stays disabled
until `magent-action-trust-project' is called."
  (when-let* ((scope (magent-session-canonical-scope scope)))
    (unless magent-action-project--loading
      (let ((magent-action-project--loading t))
        (condition-case err
            (let* ((snapshot (magent-action-project--snapshot scope))
                   (fingerprint
                    (let ((print-length nil)
                          (print-level nil))
                      (secure-hash 'sha256 (prin1-to-string snapshot)))))
              (unless (equal fingerprint (gethash scope magent-action-project--loaded))
                (magent-action-remove-project-scope scope)
                (if (null snapshot)
                    (puthash scope fingerprint magent-action-project--loaded)
                  (let ((records (magent-action-project--read-trust)))
                    (when (magent-action-project--approve scope snapshot fingerprint records)
                      (let* ((previous magent-action--registry)
                             (next (magent-action-project--evaluate scope snapshot)))
                        (puthash scope (cl-set-difference next previous :test #'eq)
                                 magent-action-project--registrations)
                        (setq magent-action--registry next))
                      (puthash scope fingerprint magent-action-project--loaded)
                      (remhash scope magent-action-project--declined)
                      (magent-action--registry-changed))))))
          ((error quit)
           (condition-case cleanup-error
               (magent-action-remove-project-scope scope)
             ((error quit)
              (magent-log "ERROR Project Action cleanup for %s failed: %s"
                          scope (error-message-string cleanup-error))))
           (signal (car err) (cdr err))))))))

(defun magent-action-project--current-scope ()
  "Return the current project scope or report that no project is active."
  (require 'magent-runtime)
  (or (magent-session-canonical-scope (magent-runtime-context-scope))
      (user-error "Project Actions require a project buffer")))

;;;###autoload
(defun magent-action-trust-project ()
  "Review the trust prompt for the current project's Action sources."
  (interactive)
  (let ((scope (magent-action-project--current-scope))
        (magent-action-project--allow-prompt t))
    (remhash scope magent-action-project--declined)
    (magent-action-load-project-scope scope)))

;;;###autoload
(defun magent-action-reload-project ()
  "Reload the current project's Action sources, confirming changed content."
  (interactive)
  (let ((scope (magent-action-project--current-scope))
        (magent-action-project--allow-prompt t))
    (magent-action-remove-project-scope scope)
    (remhash scope magent-action-project--declined)
    (magent-action-load-project-scope scope)))

;;;###autoload
(defun magent-action-forget-project-trust ()
  "Forget approval and remove the current project's Action registrations."
  (interactive)
  (let* ((scope (magent-action-project--current-scope))
         (records (magent-action-project--read-trust)))
    (remhash scope records)
    (magent-action-project--write-trust records)
    (magent-action-remove-project-scope scope)
    (remhash scope magent-action-project--declined)
    (message "Forgot project Action trust for %s" scope)))

(provide 'magent-action-project)
;;; magent-action-project.el ends here
