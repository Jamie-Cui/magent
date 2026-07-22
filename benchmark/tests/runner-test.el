;;; runner-test.el --- Deterministic Magent benchmark runner test  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'subr-x)

(defconst magent-benchmark-test--root
  (expand-file-name "../.."
                    (file-name-directory (or load-file-name buffer-file-name))))

(load (expand-file-name "benchmark/elisp/runner.el"
                        magent-benchmark-test--root)
      nil t)

(ert-deftest magent-benchmark-add-elpa-load-path-does-not-scan-package-data ()
  (let* ((elpa (make-temp-file "magent-benchmark-elpa-" t))
         (package (expand-file-name "example-1.0" elpa))
         (data (expand-file-name "data" package))
         (load-path (copy-sequence load-path)))
    (unwind-protect
        (progn
          (make-directory data t)
          (with-temp-file (expand-file-name "example-pkg.el" package)
            (insert "(define-package \"example\" \"1.0\" \"Example\")\n"))
          (cl-letf (((symbol-function
                      'normal-top-level-add-subdirs-to-load-path)
                     (lambda () (error "Package data must not be scanned"))))
            (magent-benchmark--add-elpa-load-path elpa))
          (should (member (file-name-as-directory package) load-path)))
      (ignore-errors (delete-directory elpa t)))))

(ert-deftest magent-benchmark-runner-writes-result-and-ledger ()
  (let* ((workspace (make-temp-file "magent-benchmark-workspace-" t))
         (logs (make-temp-file "magent-benchmark-logs-" t))
         (instruction-file (expand-file-name "instruction.txt" logs))
         (elpa (expand-file-name "~/.emacs.d/elpa"))
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (with-temp-file instruction-file (insert "deterministic hello"))
          (setenv "MAGENT_BENCH_SOURCE" magent-benchmark-test--root)
          (setenv "MAGENT_BENCH_ELPA_DIR" elpa)
          (setenv "MAGENT_BENCH_WORKSPACE" workspace)
          (setenv "MAGENT_BENCH_INSTRUCTION_FILE" instruction-file)
          (setenv "MAGENT_BENCH_LOGS_DIR" logs)
          (setenv "MAGENT_BENCH_PROVIDER" "openai")
          (setenv "MAGENT_BENCH_MODEL" "gpt-4o-mini")
          (setenv "MAGENT_BENCH_BASE_URL" "https://api.openai.com/v1")
          (setenv "MAGENT_BENCH_WIRE_API" "responses")
          (setenv "MAGENT_BENCH_API_KEY_ENV" "MAGENT_BENCH_TEST_KEY")
          (setenv "MAGENT_BENCH_TEST_KEY" "not-a-real-key")
          (setenv "MAGENT_BENCH_EFFORT" "auto")
          ;; Load the real transport before replacing `gptel-request'; loading
          ;; gptel inside the cl-letf would overwrite a previously unbound stub.
          (magent-benchmark--add-elpa-load-path elpa)
          (add-to-list 'load-path (expand-file-name "lisp" magent-benchmark-test--root))
          (require 'magent)
          (should (= magent-max-sampling-requests 0))
          (cl-letf (((symbol-function 'gptel-request)
                     (lambda (_prompt &rest kwargs)
                       (let ((callback (plist-get kwargs :callback))
                             (info '(:content "Done."
                                     :tokens (:input_tokens 10
                                              :output_tokens 2))))
                         (run-at-time
                          0 nil
                          (lambda ()
                            (funcall callback "Done." info)
                            (funcall callback t info))))
                       nil)))
            (should (equal (magent-benchmark-run-from-environment) "Done.")))
          (should (file-exists-p (expand-file-name "magent-result.json" logs)))
          (should (file-exists-p (expand-file-name "magent-ledger.json" logs)))
          (should (file-exists-p (expand-file-name "magent-progress.json" logs)))
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (result (json-read-file
                          (expand-file-name "magent-result.json" logs)))
                 (ledger (json-read-file
                          (expand-file-name "magent-ledger.json" logs)))
                 (progress (json-read-file
                            (expand-file-name "magent-progress.json" logs))))
            (should (equal (alist-get 'status result) "completed"))
            (should (equal (alist-get 'output result) "Done."))
            (should (= (length (alist-get 'usage-samples result)) 1))
            (should (= (length (alist-get 'turns ledger)) 1))
            (should (equal (alist-get 'event progress) "completed"))
            (should (= (length (alist-get 'usage-samples progress)) 1))))
      (ignore-errors (delete-directory workspace t))
      (ignore-errors (delete-directory logs t)))))

(provide 'magent-benchmark-runner-test)
;;; runner-test.el ends here
