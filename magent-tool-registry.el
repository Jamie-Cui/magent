;;; magent-tool-registry.el --- Compatibility shim for magent-tool-runtime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Runtime tool adaptation now lives in `magent-tool-runtime.el'.

;;; Code:

(require 'magent-tool-runtime)

(define-obsolete-function-alias
  'magent-tool-registry-from-gptel-tool
  #'magent-tool-runtime-from-gptel-tool
  "0.1.0")
(define-obsolete-function-alias
  'magent-tool-registry-for-agent
  #'magent-tool-runtime-for-agent
  "0.1.0")
(define-obsolete-function-alias
  'magent-tool-registry-runtime-to-plist
  #'magent-tool-runtime-to-plist
  "0.1.0")

(provide 'magent-tool-registry)
;;; magent-tool-registry.el ends here
