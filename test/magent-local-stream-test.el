;;; magent-local-stream-test.el --- Local HTTP retry test -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Start magent-stream-fixture.py, set `magent-local-stream-test-port' to its
;; local port, and run this test in an isolated Emacs. Uses real gptel/curl,
;; a synthetic tool and no provider credentials. Not part of offline ERT.

;;; Code:

(load (expand-file-name "magent-test.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defvar magent-local-stream-test-port nil
  "Port of the local HTTP stream fixture.")

(ert-deftest magent-local-stream-test-retries-real-curl ()
  "Real curl truncation triggers one retry without dispatching partial tools."
  (should (and (integerp magent-local-stream-test-port)
               (< 0 magent-local-stream-test-port 65536)))
  (let* ((gptel-backend (gptel-make-openai
                          "Local stream fixture" :key "fixture"
                          :host (format "127.0.0.1:%d" magent-local-stream-test-port)
                          :protocol "http" :stream t :models '(fixture)))
         (gptel-model 'fixture)
         (gptel-use-curl t)
         (gptel-proxy "")
         (gptel-curl-extra-args '("--noproxy" "*"))
         (gptel-log-level nil)
         (magent-stream-retry-limit 2)
         (magent-max-sampling-requests 4)
         (magent-request-timeout 10)
         (session (magent-session-create :id "local-http-retry"))
         notices (tool-count 0) result
         (context (magent-request-context-create
                   :session session :scope 'global
                   :observer (lambda (event)
                               (when (eq (plist-get event :type) 'sampling-retry)
                                 (push event notices)))))
         (tool (gptel-make-tool
                :name "read_tool_output" :description "Read synthetic output"
                :function (lambda () (cl-incf tool-count)
                            (magent-test-tool-result "FIXTURE_OUTPUT")))))
    (cl-letf (((symbol-function 'magent-tools-get-gptel-tools-for-permission)
               (lambda (&rest _) (list tool))))
      (magent-test--run-turn
       "Read the fixture and report the result." (lambda (value) (setq result value))
       (magent-agent-info-create :name "build" :permission '(("*" . allow)))
       nil nil nil nil nil nil context)
      (let ((deadline (+ (float-time) 12)))
        (while (and (not result) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should result))
    (should (magent-execution-result-completed-p result))
    (should (equal (magent-execution-result-content-string result) "MAGENT_LOCAL_RETRY_OK"))
    (should (= tool-count 1))
    (let ((items (magent-thread-all-items (magent-session-thread-ledger session))))
      (should (= (cl-count 'tool items :key #'magent-thread-item-type) 1)))
    (should (= (length notices) 1))))

(provide 'magent-local-stream-test)
;;; magent-local-stream-test.el ends here
