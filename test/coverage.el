;;; coverage.el --- Batch coverage runner for Magent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'testcover)

(defconst magent-coverage--root-directory
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root for the batch coverage run.")

(defvar magent--prompt-file
  (expand-file-name "prompt.org" magent-coverage--root-directory)
  "Prompt file used while instrumenting `magent-config'.")

(defvar magent-coverage-min 0
  "Minimum total coverage percentage required by the batch coverage run.")

(defvar magent-coverage-directory "coverage"
  "Directory where batch coverage reports are written.")

(defconst magent-coverage--source-files
  '("lisp/magent-config.el"
    "lisp/magent-json.el"
    "lisp/magent-redaction.el"
    "lisp/magent-approval.el"
    "lisp/magent-lifecycle-events.el"
    "lisp/magent-protocol.el"
    "lisp/magent-ledger.el"
    "lisp/magent-thread.el"
    "lisp/magent-agent-job.el"
    "lisp/magent-session.el"
    "lisp/magent-audit.el"
    "lisp/magent-file-loader.el"
    "lisp/magent-runtime.el"
    "lisp/magent-permission.el"
    "lisp/magent-agent-info.el"
    "lisp/magent-agent-builtins.el"
    "lisp/magent-agent-registry.el"
    "lisp/magent-agent-file.el"
    "lisp/magent-llm.el"
    "lisp/magent-llm-gptel.el"
    "lisp/magent-tools.el"
    "lisp/magent-tool-runtime.el"
    "lisp/magent-tool-orchestrator.el"
    "lisp/magent-agent-loop.el"
    "lisp/magent-legacy-queue.el"
    "lisp/magent-transcript-context.el"
    "lisp/magent-markdown-to-org.el"
    "lisp/magent-agent.el"
    "lisp/magent-command.el"
    "lisp/magent-memory.el"
    "lisp/magent-doctor.el"
    "lisp/magent-skills.el"
    "lisp/magent-capability.el"
    "lisp/magent-ui.el"
    "lisp/magent-evil.el"
    "lisp/magent.el")
  "Source files instrumented by the batch coverage run.")

(defconst magent-coverage--test-files
  '("test/magent-test.el")
  "ERT files loaded by the batch coverage run.")

(defvar magent-coverage--instrumented nil
  "Alist mapping source file names to instrumented definition symbols.")

(defun magent-coverage--instrument-file (file)
  "Instrument FILE with `testcover' and remember its definition symbols."
  (let ((source (expand-file-name file magent-coverage--root-directory))
        symbols)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (let ((default-directory magent-coverage--root-directory))
        (testcover-start source)))
    (setq symbols (mapcar #'car edebug-form-data))
    (push (cons file symbols)
          magent-coverage--instrumented)))

(defun magent-coverage--entry-covered-p (entry)
  "Return non-nil if testcover ENTRY is considered covered."
  (or (eq entry 'edebug-ok-coverage)
      (memq (car-safe entry) '(testcover-1value maybe noreturn))))

(defun magent-coverage--symbol-summary (symbol)
  "Return (TOTAL COVERED UNCOVERED) for SYMBOL's coverage vector."
  (let ((coverage (get symbol 'edebug-coverage))
        (total 0)
        (covered 0)
        (uncovered 0))
    (when (vectorp coverage)
      (dotimes (index (length coverage))
        (setq total (1+ total))
        (if (magent-coverage--entry-covered-p
             (aref coverage index))
            (setq covered (1+ covered))
          (setq uncovered (1+ uncovered)))))
    (list total covered uncovered)))

(defun magent-coverage--file-summary (file symbols)
  "Return a plist coverage summary for FILE and SYMBOLS."
  (let ((defs 0)
        (total 0)
        (covered 0)
        (uncovered 0))
    (dolist (symbol symbols)
      (pcase-let ((`(,sym-total ,sym-covered ,sym-uncovered)
                   (magent-coverage--symbol-summary symbol)))
        (when (> sym-total 0)
          (setq defs (1+ defs))
          (setq total (+ total sym-total))
          (setq covered (+ covered sym-covered))
          (setq uncovered (+ uncovered sym-uncovered)))))
    (list :file file
          :defs defs
          :total total
          :covered covered
          :uncovered uncovered
          :percent (if (zerop total)
                       100.0
                     (* 100.0 (/ (float covered) total))))))

(defun magent-coverage--summaries ()
  "Return coverage summaries for all instrumented files."
  (mapcar (lambda (entry)
            (magent-coverage--file-summary
             (car entry)
             (cdr entry)))
          (nreverse magent-coverage--instrumented)))

(defun magent-coverage--total-summary (summaries)
  "Return total coverage summary for SUMMARIES."
  (let ((defs 0)
        (total 0)
        (covered 0)
        (uncovered 0))
    (dolist (summary summaries)
      (setq defs (+ defs (plist-get summary :defs)))
      (setq total (+ total (plist-get summary :total)))
      (setq covered (+ covered (plist-get summary :covered)))
      (setq uncovered (+ uncovered (plist-get summary :uncovered))))
    (list :file "TOTAL"
          :defs defs
          :total total
          :covered covered
          :uncovered uncovered
          :percent (if (zerop total)
                       100.0
                     (* 100.0 (/ (float covered) total))))))

(defun magent-coverage--format-summary (summary)
  "Return a human-readable line for coverage SUMMARY."
  (format "%-32s defs=%3d forms=%5d covered=%5d missed=%5d %6.2f%%"
          (plist-get summary :file)
          (plist-get summary :defs)
          (plist-get summary :total)
          (plist-get summary :covered)
          (plist-get summary :uncovered)
          (plist-get summary :percent)))

(defun magent-coverage--write-tsv (summaries total)
  "Write SUMMARIES and TOTAL to the batch coverage TSV report."
  (make-directory magent-coverage-directory t)
  (let ((report (expand-file-name
                 "testcover-summary.tsv"
                 magent-coverage-directory)))
    (with-temp-file report
      (insert "file\tdefs\tforms\tcovered\tmissed\tpercent\n")
      (dolist (summary (append summaries (list total)))
        (insert
         (format "%s\t%d\t%d\t%d\t%d\t%.2f\n"
                 (plist-get summary :file)
                 (plist-get summary :defs)
                 (plist-get summary :total)
                 (plist-get summary :covered)
                 (plist-get summary :uncovered)
                 (plist-get summary :percent)))))
    report))

(defun magent-coverage-run ()
  "Run ERT tests under `testcover' and write a coverage summary."
  (setq magent-coverage--instrumented nil)
  (dolist (file magent-coverage--source-files)
    (magent-coverage--instrument-file file))
  (when (boundp 'magent-skills--builtin-dir)
    (setq magent-skills--builtin-dir
          (expand-file-name "skills" magent-coverage--root-directory)))
  (when (boundp 'magent-capability--builtin-dir)
    (setq magent-capability--builtin-dir
          (expand-file-name "capabilities" magent-coverage--root-directory)))
  (dolist (file magent-coverage--test-files)
    (load (expand-file-name file magent-coverage--root-directory) nil t))
  (let* ((stats (ert-run-tests-batch t))
         (summaries (magent-coverage--summaries))
         (total (magent-coverage--total-summary summaries))
         (report (magent-coverage--write-tsv summaries total)))
    (princ "\nCoverage summary:\n")
    (dolist (summary summaries)
      (princ (concat (magent-coverage--format-summary summary)
                     "\n")))
    (princ (concat (magent-coverage--format-summary total)
                   "\n"))
    (princ (format "Coverage report: %s\n" report))
    (when (> (ert-stats-completed-unexpected stats) 0)
      (kill-emacs 1))
    (when (< (plist-get total :percent) magent-coverage-min)
      (princ
       (format
        "Coverage %.2f%% is below required minimum %.2f%%\n"
        (plist-get total :percent)
        (float magent-coverage-min)))
      (kill-emacs 1))))

(magent-coverage-run)

;;; coverage.el ends here
