;;; install-deps.el --- Install Magent benchmark dependencies  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Batch helper used inside a Harbor task container.  A prebuilt ELPA bundle is
;; preferred for scored runs; this network installer is the portable fallback.

;;; Code:

(require 'package)
(require 'json)
(require 'subr-x)

(setq package-user-dir
      (or (getenv "MAGENT_BENCH_ELPA_DIR") "/installed-agent/elpa")
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/"))
      package-check-signature nil)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(gptel yaml compat acp agent-shell))
  (unless (package-installed-p package)
    (package-install package)))

(let ((output (or (getenv "MAGENT_BENCH_DEPENDENCIES_FILE")
                  "/logs/agent/dependencies.json"))
      versions)
  (dolist (entry package-alist)
    (let* ((name (symbol-name (car entry)))
           (descriptor (cadr entry))
           (version (and descriptor
                         (package-version-join
                          (package-desc-version descriptor)))))
      (push (cons name (or version "unknown")) versions)))
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert (json-encode (sort versions
                               (lambda (a b) (string< (car a) (car b))))))
    (insert "\n")))

;;; install-deps.el ends here
