;;; check-elpa-deps.el --- Report direct ELPA dependency versions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Read a package's Package-Requires header, compare each direct dependency
;; with the newest version advertised by GNU ELPA, NonGNU ELPA, or MELPA,
;; and print a tab-separated report.  Declared versions are compatibility
;; minimums, so newer versions are informational rather than failures.

;;; Code:

(require 'cl-lib)
(require 'package)
(require 'subr-x)

(defconst magent-elpa-deps--archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
    ("melpa" . "https://melpa.org/packages/"))
  "Package archives used by Magent's dependency checks.")

(defun magent-elpa-deps--requirements (package-file)
  "Return direct requirements declared by PACKAGE-FILE."
  (with-temp-buffer
    (insert-file-contents package-file)
    (mapcar
     (lambda (requirement)
       (list (car requirement)
             (package-version-join (cadr requirement))))
     (package-desc-reqs (package-buffer-info)))))

(defun magent-elpa-deps--missing-archives (package-dir)
  "Return archive names that were not downloaded under PACKAGE-DIR."
  (cl-loop for (name . _url) in magent-elpa-deps--archives
           unless (file-readable-p
                   (expand-file-name
                    (format "archives/%s/archive-contents" name)
                    package-dir))
           collect name))

(defun magent-elpa-deps--archive-descriptors (package)
  "Return archive descriptors for PACKAGE across supported Emacs versions."
  (let ((entry (cdr (assq package package-archive-contents))))
    (cond
     ((package-desc-p entry) (list entry))
     ((listp entry) (cl-remove-if-not #'package-desc-p entry))
     (t nil))))

(defun magent-elpa-deps--latest-descriptor (package)
  "Return the newest available archive descriptor for PACKAGE."
  (let ((descriptors (magent-elpa-deps--archive-descriptors package)))
    (when descriptors
      (cl-reduce
       (lambda (left right)
         (if (version-list-< (package-desc-version left)
                             (package-desc-version right))
             right
           left))
       (cdr descriptors)
       :initial-value (car descriptors)))))

(defun magent-elpa-deps--print-row (package minimum candidate source status)
  "Print one dependency report row.

PACKAGE is the dependency name, MINIMUM is its declared version, CANDIDATE is
the checked version, SOURCE identifies its origin, and STATUS is the result."
  (princ (format "%s\t%s\t%s\t%s\t%s\n"
                 package minimum candidate source status)))

(defun magent-elpa-deps--report (requirements)
  "Print a report for REQUIREMENTS and return an exit status.

Return zero when every declared minimum can be satisfied, or one otherwise."
  (let ((failed nil))
    (princ "package\tminimum\tcandidate\tsource\tstatus\n")
    (dolist (requirement requirements)
      (let ((package (car requirement))
            (minimum (cadr requirement)))
        (if (eq package 'emacs)
            (let ((satisfied (not (version< emacs-version minimum))))
              (unless satisfied
                (setq failed t))
              (magent-elpa-deps--print-row
               package minimum emacs-version "runtime"
               (if satisfied "satisfied" "minimum-unavailable")))
          (let ((descriptor (magent-elpa-deps--latest-descriptor package)))
            (if (null descriptor)
                (progn
                  (setq failed t)
                  (magent-elpa-deps--print-row
                   package minimum "-" "-" "unavailable"))
              (let* ((candidate
                      (package-version-join
                       (package-desc-version descriptor)))
                     (status
                      (cond
                       ((version< candidate minimum)
                        (setq failed t)
                        "minimum-unavailable")
                       ((version< minimum candidate) "newer-available")
                       (t "exact"))))
                (magent-elpa-deps--print-row
                 package minimum candidate
                 (or (package-desc-archive descriptor) "unknown")
                 status)))))))
    (if failed 1 0)))

(defun magent-elpa-deps-check-file (package-file)
  "Report direct dependency versions for PACKAGE-FILE.

Archive metadata must already be present in `package-archive-contents'."
  (magent-elpa-deps--report
   (magent-elpa-deps--requirements package-file)))

(defun magent-elpa-deps--refresh-and-run (operation)
  "Refresh archive metadata and return the status from OPERATION."
  (let ((temporary-package-dir
         (make-temp-file "magent-elpa-deps-" t))
        status)
    (unwind-protect
        (setq status
              (condition-case error-data
                  (let ((package-user-dir temporary-package-dir)
                        (package-gnupghome-dir
                         (expand-file-name "gnupg" temporary-package-dir))
                        (package-archives magent-elpa-deps--archives)
                        (package-archive-contents nil))
                    (package-refresh-contents)
                    (let ((missing-archives
                           (magent-elpa-deps--missing-archives
                            temporary-package-dir)))
                      (when missing-archives
                        (error "Failed to refresh package archives: %s"
                               (string-join missing-archives ", "))))
                    (funcall operation))
                (error
                 (message "Dependency check failed: %s"
                          (error-message-string error-data))
                 2)))
      (delete-directory temporary-package-dir t))
    status))

(defun magent-elpa-deps-check-batch (package-file)
  "Refresh archive metadata, check PACKAGE-FILE, and exit Emacs."
  (kill-emacs
   (magent-elpa-deps--refresh-and-run
    (lambda () (magent-elpa-deps-check-file package-file)))))

(provide 'check-elpa-deps)
;;; check-elpa-deps.el ends here
