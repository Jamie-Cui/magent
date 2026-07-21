;;; runner.el --- Headless Harbor runner for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This file is benchmark-only glue.  It configures gptel, invokes Magent's
;; public UI-neutral runtime, and exports the canonical ledger.  It deliberately
;; does not add sandboxing or replace gptel's HTTP/SSE transport.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-parse)

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-curl)

(defvar magent-benchmark--usage-samples nil
  "Provider token usage captured once per Magent sampling request.")

(defun magent-benchmark--env (name &optional required)
  "Return environment variable NAME, signaling when REQUIRED and absent."
  (let ((value (getenv name)))
    (when (and required (string-empty-p (or value "")))
      (error "Missing required environment variable %s" name))
    value))

(defun magent-benchmark--add-elpa-load-path (directory)
  "Add every installed package under DIRECTORY to `load-path'."
  (let ((default-directory (file-name-as-directory directory)))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(defun magent-benchmark--endpoint (base-url wire-api)
  "Return (PROTOCOL HOST ENDPOINT) for BASE-URL and WIRE-API."
  (let* ((parsed (url-generic-parse-url base-url))
         (protocol (url-type parsed))
         (host (url-host parsed))
         (path (string-remove-suffix "/" (or (url-filename parsed) "")))
         (suffix (if (string= wire-api "responses")
                     "/responses"
                   "/chat/completions")))
    (unless (and protocol host)
      (error "Invalid provider base URL: %s" base-url))
    (when (and (string= wire-api "responses")
               (not (string= host "api.openai.com")))
      (error "gptel supports Responses API here only on api.openai.com"))
    (list protocol host
          (if (string-suffix-p suffix path) path (concat path suffix)))))

(defun magent-benchmark--make-backend ()
  "Create the selected gptel backend from benchmark environment variables."
  (require 'gptel)
  (require 'gptel-openai)
  (let* ((model-name (magent-benchmark--env "MAGENT_BENCH_MODEL" t))
         (model (intern model-name))
         (wire-api (magent-benchmark--env "MAGENT_BENCH_WIRE_API" t))
         (base-url (magent-benchmark--env "MAGENT_BENCH_BASE_URL" t))
         (key-name (magent-benchmark--env "MAGENT_BENCH_API_KEY_ENV" t))
         (endpoint (magent-benchmark--endpoint base-url wire-api))
         (capabilities (append '(tool-use json)
                               (when (string= wire-api "responses")
                                 '(reasoning responses-api))))
         (backend
          (gptel-make-openai
              "magent-benchmark"
            :protocol (nth 0 endpoint)
            :host (nth 1 endpoint)
            :endpoint (nth 2 endpoint)
            :stream t
            :key (lambda () (magent-benchmark--env key-name t))
            :models `((,model :capabilities ,capabilities)))))
    (setq-default gptel-backend backend
                  gptel-model model
                  gptel-use-curl t)
    backend))

(defun magent-benchmark--capture-usage (original loop event)
  "Around advice for ORIGINAL LOOP EVENT that records terminal token usage."
  (let* ((type (and (fboundp 'magent-llm-event-type)
                    (magent-llm-event-type event)))
         (usage (or (and (fboundp 'magent-llm-event-usage)
                         (magent-llm-event-usage event))
                    (and (memq type '(tool-call-batch-end error))
                         (fboundp 'magent-llm-event-metadata)
                         (plist-get (magent-llm-event-metadata event) :tokens)))))
    (when (and usage (memq type '(completed tool-call-batch-end error)))
      (push usage magent-benchmark--usage-samples))
    (funcall original loop event)))

(defun magent-benchmark--write-json (path value)
  "Atomically write VALUE as UTF-8 JSON to PATH."
  (make-directory (file-name-directory path) t)
  (let ((temporary (make-temp-file
                    (expand-file-name ".magent-benchmark-"
                                      (file-name-directory path)))))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (set-buffer-file-coding-system 'utf-8-unix)
            (insert (magent-json-encode value))
            (insert "\n"))
          (rename-file temporary path t))
      (when (file-exists-p temporary)
        (delete-file temporary)))))

(defun magent-benchmark--result-alist (status result started finished)
  "Return a JSON-safe benchmark result for STATUS RESULT and timestamps."
  `((status . ,(symbol-name status))
    (success . ,(if (magent-agent-result-success-p result) t :json-false))
    (output . ,(magent-agent-result-content-string result))
    (error . ,(and (magent-agent-result-p result)
                   (magent-agent-result-error result)))
    (model . ,(magent-benchmark--env "MAGENT_BENCH_MODEL"))
    (provider . ,(magent-benchmark--env "MAGENT_BENCH_PROVIDER"))
    (wire-api . ,(magent-benchmark--env "MAGENT_BENCH_WIRE_API"))
    (started-at . ,started)
    (finished-at . ,finished)
    (duration-ms . ,(round (* 1000 (- finished started))))
    (usage-samples . ,(vconcat (nreverse magent-benchmark--usage-samples)))))

;;;###autoload
(defun magent-benchmark-run-from-environment ()
  "Run one Magent benchmark trial using MAGENT_BENCH_* environment values."
  (let* ((source (file-name-as-directory
                  (magent-benchmark--env "MAGENT_BENCH_SOURCE" t)))
         (elpa (file-name-as-directory
                (magent-benchmark--env "MAGENT_BENCH_ELPA_DIR" t)))
         (workspace (file-name-as-directory
                     (expand-file-name
                      (magent-benchmark--env "MAGENT_BENCH_WORKSPACE" t))))
         (instruction-file
          (magent-benchmark--env "MAGENT_BENCH_INSTRUCTION_FILE" t))
         (logs (file-name-as-directory
                (magent-benchmark--env "MAGENT_BENCH_LOGS_DIR" t)))
         (default-directory workspace)
         (user-emacs-directory (expand-file-name "emacs.d/" logs))
         (magent-benchmark--usage-samples nil)
         done final-status final-result)
    (magent-benchmark--add-elpa-load-path elpa)
    (add-to-list 'load-path (expand-file-name "lisp" source))
    (require 'magent)
    (magent-benchmark--make-backend)
    (setq-default
     magent-project-root-function (lambda () workspace)
     magent-enable-tools '(read write edit grep glob bash emacs_eval agent skill)
     magent-bypass-permission t
     magent-enable-capabilities t
     magent-session-directory (expand-file-name "sessions/" logs)
     magent-audit-directory (expand-file-name "audit/" logs)
     magent-request-timeout 0
     magent-max-sampling-requests 0)
    (when-let* ((effort (magent-benchmark--env "MAGENT_BENCH_EFFORT")))
      (unless (string= effort "auto")
        (setq-default magent-default-effort (intern effort))))
    (advice-add 'magent-agent-loop-apply-event :around
                #'magent-benchmark--capture-usage)
    (unwind-protect
        (let* ((instruction
                (with-temp-buffer
                  (insert-file-contents instruction-file)
                  (buffer-string)))
               (started (float-time))
               (scope (magent-session-scope-from-directory workspace))
               (runtime-session (magent-runtime-session-new scope)))
          (magent-runtime-session-set-agent runtime-session "build")
          (magent-runtime-submit
           runtime-session instruction
           :effort (magent-benchmark--env "MAGENT_BENCH_EFFORT")
           :turn-metadata '(:benchmark t)
           :on-complete
           (lambda (status result)
             (setq final-status status final-result result done t)))
          (while (not done)
            (accept-process-output nil 0.1))
          (let* ((finished (float-time))
                 (session (magent-runtime-session-magent-session runtime-session))
                 (thread (magent-session-thread-ledger session))
                 (result-data (magent-benchmark--result-alist
                               final-status final-result started finished)))
            (magent-benchmark--write-json
             (expand-file-name "magent-ledger.json" logs)
             (magent-thread-snapshot-to-alist thread))
            (magent-benchmark--write-json
             (expand-file-name "magent-result.json" logs) result-data)
            (unless (and (eq final-status 'completed)
                         (magent-agent-result-success-p final-result))
              (error "Magent trial failed: %s"
                     (magent-agent-result-content-string final-result)))
            (magent-agent-result-content-string final-result)))
      (advice-remove 'magent-agent-loop-apply-event
                     #'magent-benchmark--capture-usage))))

(provide 'magent-benchmark-runner)
;;; runner.el ends here
