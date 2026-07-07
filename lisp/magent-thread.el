;;; magent-thread.el --- Compatibility shim for magent-ledger  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The durable workflow ledger now lives in `magent-ledger.el'.  This feature
;; preserves the historical `(require 'magent-thread)' entry point.

;;; Code:

(require 'magent-ledger)

(provide 'magent-thread)
;;; magent-thread.el ends here
