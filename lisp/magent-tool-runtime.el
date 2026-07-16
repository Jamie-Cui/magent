;;; magent-tool-runtime.el --- Runtime tool adapter for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Adapter from existing gptel-tool definitions to the runtime-facing Magent
;; tool shape.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'magent-tools)

(cl-defstruct (magent-tool-runtime
               (:constructor magent-tool-runtime-create)
               (:copier nil))
  name
  description
  args
  function
  async
  perm-key
  exposure
  supports-parallel)

(defun magent-tool-runtime-from-gptel-tool (tool)
  "Create a `magent-tool-runtime' from gptel TOOL."
  (let ((name (gptel-tool-name tool)))
    (magent-tool-runtime-create
     :name name
     :description (gptel-tool-description tool)
     :args (gptel-tool-args tool)
     :function (gptel-tool-function tool)
     :async (gptel-tool-async tool)
     :perm-key (magent-tools-permission-key name)
     :exposure 'direct
     :supports-parallel nil)))

(defun magent-tool-runtime-for-permission (permission)
  "Return tool runtimes available under explicit PERMISSION profile."
  (mapcar #'magent-tool-runtime-from-gptel-tool
          (magent-tools-get-gptel-tools-for-permission permission)))

(defun magent-tool-runtime-to-plist (runtime)
  "Convert RUNTIME to the existing Magent tool plist shape."
  (list :name (magent-tool-runtime-name runtime)
        :description (magent-tool-runtime-description runtime)
        :args (magent-tool-runtime-args runtime)
        :function (magent-tool-runtime-function runtime)
        :async (magent-tool-runtime-async runtime)
        :perm-key (magent-tool-runtime-perm-key runtime)
        :exposure (magent-tool-runtime-exposure runtime)
        :supports-parallel
        (magent-tool-runtime-supports-parallel runtime)))

(provide 'magent-tool-runtime)
;;; magent-tool-runtime.el ends here
