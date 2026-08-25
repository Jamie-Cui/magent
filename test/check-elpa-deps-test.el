;;; check-elpa-deps-test.el --- Tests for direct dependency checks  -*- lexical-binding: t; -*-

;;; Commentary:

;; Offline tests for scripts/check-elpa-deps.el.

;;; Code:

(require 'ert)
(load (expand-file-name "../scripts/check-elpa-deps.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil t)

(defconst magent-elpa-deps-test--root
  (expand-file-name ".."
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Magent repository root used by dependency-check tests.")

(defun magent-elpa-deps-test--descriptor (name version archive)
  "Return a test descriptor for NAME VERSION from ARCHIVE."
  (package-desc-create
   :name name
   :version (version-to-list version)
   :summary "Test package"
   :reqs nil
   :kind 'single
   :archive archive))

(ert-deftest magent-elpa-deps-test-reads-package-requires ()
  "Read dependency metadata from the canonical package file."
  (let ((requirements
          (magent-elpa-deps--requirements
           (expand-file-name "lisp/magent.el"
                             magent-elpa-deps-test--root))))
    (should (equal (cadr (assq 'emacs requirements)) "29.1"))
    (should (equal (cadr (assq 'gptel requirements)) "0.9.8"))))

(ert-deftest magent-elpa-deps-test-detects-missing-archive-downloads ()
  "Distinguish archive download failures from unavailable packages."
  (let* ((package-dir (expand-file-name "magent-elpa-deps-test"
                                        temporary-file-directory))
         (gnu-contents
          (expand-file-name "archives/gnu/archive-contents" package-dir)))
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (file) (equal file gnu-contents))))
      (should (equal (magent-elpa-deps--missing-archives package-dir)
                     '("nongnu" "melpa"))))))

(ert-deftest magent-elpa-deps-test-reports-newer-version-without-failing ()
  "Treat a newer archive version as information, not an error."
  (let* ((descriptor
          (magent-elpa-deps-test--descriptor 'example "2.0" "gnu"))
         (package-archive-contents `((example ,descriptor)))
         output status)
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (setq status
              (magent-elpa-deps--report
               '((emacs "29.1") (example "1.0"))))
        (setq output (buffer-string))))
    (should (= status 0))
    (should (string-match-p
             "example\t1.0\t2.0\tgnu\tnewer-available"
             output))))

(ert-deftest magent-elpa-deps-test-selects-newest-archive-version ()
  "Select the newest descriptor when archives advertise several versions."
  (let* ((old (magent-elpa-deps-test--descriptor 'example "1.5" "gnu"))
         (new (magent-elpa-deps-test--descriptor 'example "2.0" "melpa"))
         (package-archive-contents `((example ,old ,new))))
    (should (eq (magent-elpa-deps--latest-descriptor 'example) new))))

(ert-deftest magent-elpa-deps-test-fails-for-unavailable-minimum ()
  "Fail when a package is missing or its declared minimum is unavailable."
  (let* ((descriptor
          (magent-elpa-deps-test--descriptor 'old "1.0" "gnu"))
         (package-archive-contents `((old ,descriptor)))
         output status)
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (setq status
              (magent-elpa-deps--report
               '((old "2.0") (missing "1.0"))))
        (setq output (buffer-string))))
    (should (= status 1))
    (should (string-match-p
             "old\t2.0\t1.0\tgnu\tminimum-unavailable"
             output))
    (should (string-match-p
             "missing\t1.0\t-\t-\tunavailable"
             output))))

(provide 'check-elpa-deps-test)
;;; check-elpa-deps-test.el ends here
