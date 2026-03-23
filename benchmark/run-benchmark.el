;;; run-benchmark.el --- Batch entrypoint for Magent benchmark  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;; emacs -Q --script benchmark/run-benchmark.el -- --repetitions 2

;;; Code:

(let* ((script-file (or load-file-name buffer-file-name))
       (benchmark-dir (file-name-directory script-file))
       (repo-root (expand-file-name ".." benchmark-dir)))
  (add-to-list 'load-path benchmark-dir)
  (add-to-list 'load-path repo-root))

(require 'magent-benchmark)

(defun magent-benchmark-run-script ()
  "Run the benchmark suite from command-line arguments."
  (let ((args command-line-args-left)
        (repetitions 1)
        (output-dir nil)
        (task-regexp nil))
    (while args
      (pcase (pop args)
        ("--repetitions"
         (setq repetitions (string-to-number (or (pop args) "1"))))
        ("--output-dir"
         (setq output-dir (pop args)))
        ("--task-regexp"
         (setq task-regexp (pop args)))))
    (magent-benchmark-run-suite
     :repetitions repetitions
     :output-directory output-dir
     :task-regexp task-regexp)))

(magent-benchmark-run-script)
