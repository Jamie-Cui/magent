;;; magent-command-builtins.el --- Bundled Magent commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'magent-command-controls)
(require 'magent-doctor)
(require 'magent-memory)
(require 'magent-command-explain)
(require 'magent-command-fix)
(require 'magent-command-init)
(require 'magent-command-review)
(require 'magent-command-summarize)
(require 'magent-command-test)

(defun magent-command-builtins-register ()
  "Register every bundled Magent command as one atomic refresh."
  (let ((magent-command--allow-core-registration t)
        (magent-command--suppress-registry-hooks t))
    (magent-command-remove-source 'core)
    (magent-command-controls-register)
    (magent-doctor-register-command)
    (magent-memory-register-commands)
    (magent-command-explain-register)
    (magent-command-fix-register)
    (magent-command-init-register)
    (magent-command-review-register)
    (magent-command-summarize-register)
    (magent-command-test-register))
  (magent-command--registry-changed))

(provide 'magent-command-builtins)
;;; magent-command-builtins.el ends here
