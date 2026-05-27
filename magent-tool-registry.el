;;; magent-tool-registry.el --- Tool registry facade for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; A small registry facade over existing Magent tools.  This gives the
;; runtime a Codex-like tool abstraction while preserving the current
;; gptel-tool definitions.

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

(defun magent-tool-registry-from-gptel-tool (tool)
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

(defun magent-tool-registry-for-agent (agent-info)
  "Return tool runtimes available to AGENT-INFO."
  (mapcar #'magent-tool-registry-from-gptel-tool
          (magent-tools-get-gptel-tools agent-info)))

(defun magent-tool-registry-runtime-to-plist (runtime)
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

(provide 'magent-tool-registry)
;;; magent-tool-registry.el ends here
