;;; magent-agent-info-test.el --- Tests for magent-agent-info  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>

;;; Commentary:

;; Unit tests for magent-agent-info.el

;;; Code:

(require 'ert)
(require 'magent-test-helper)
(require 'magent-agent-info)

;;; Agent info creation tests

(ert-deftest magent-agent-info-test-create ()
  "Test creating agent info structures."
  (let ((agent (magent-agent-info-create
                :name "test-agent"
                :description "A test agent"
                :mode 'primary
                :native t)))
    (should (magent-agent-info-p agent))
    (should (string= "test-agent" (magent-agent-info-name agent)))
    (should (string= "A test agent" (magent-agent-info-description agent)))
    (should (eq 'primary (magent-agent-info-mode agent)))
    (should (magent-agent-info-native agent))))

(ert-deftest magent-agent-info-test-defaults ()
  "Test that optional fields have correct defaults."
  (let ((agent (magent-agent-info-create
                :name "minimal"
                :mode 'all)))
    (should (null (magent-agent-info-hidden agent)))
    (should (null (magent-agent-info-temperature agent)))
    (should (null (magent-agent-info-model agent)))
    (should (null (magent-agent-info-permission agent)))))

;;; Validation tests

(ert-deftest magent-agent-info-test-valid-mode-p ()
  "Test mode validation."
  (should (magent-agent-info-valid-mode-p 'primary))
  (should (magent-agent-info-valid-mode-p 'subagent))
  (should (magent-agent-info-valid-mode-p 'all))
  (should-not (magent-agent-info-valid-mode-p 'invalid))
  (should-not (magent-agent-info-valid-mode-p nil)))

(ert-deftest magent-agent-info-test-valid-p ()
  "Test agent info validation."
  (should (magent-agent-info-valid-p
           (magent-agent-info-create :name "test" :mode 'primary)))
  (should-not (magent-agent-info-valid-p
               (magent-agent-info-create :name "test" :mode 'invalid)))
  (should-not (magent-agent-info-valid-p nil)))

(ert-deftest magent-agent-info-test-mode-p ()
  "Test mode checking."
  (let ((primary-agent (magent-agent-info-create :name "p" :mode 'primary))
        (all-agent (magent-agent-info-create :name "a" :mode 'all)))
    (should (magent-agent-info-mode-p primary-agent 'primary))
    (should-not (magent-agent-info-mode-p primary-agent 'subagent))
    (should (magent-agent-info-mode-p all-agent 'primary))
    (should (magent-agent-info-mode-p all-agent 'subagent))))

;;; gptel override tests

(ert-deftest magent-agent-info-test-apply-gptel-overrides ()
  "Test gptel variable overrides."
  (let ((agent-with-temp (magent-agent-info-create
                          :name "test"
                          :mode 'primary
                          :temperature 0.5))
        (agent-no-overrides (magent-agent-info-create
                             :name "test2"
                             :mode 'primary)))
    ;; Temperature override should take effect
    (magent-agent-info-apply-gptel-overrides
     agent-with-temp
     (lambda ()
       (should (= 0.5 gptel-temperature))))
    ;; Without overrides, gptel-temperature should keep its value
    (let ((gptel-temperature 1.0))
      (magent-agent-info-apply-gptel-overrides
       agent-no-overrides
       (lambda ()
         (should (= 1.0 gptel-temperature)))))))

;;; Display tests

(ert-deftest magent-agent-info-test-display-name ()
  "Test display name formatting."
  (let ((native (magent-agent-info-create :name "native" :mode 'all :native t))
        (custom (magent-agent-info-create :name "custom" :mode 'all :native nil)))
    (should (string-match-p "built-in" (magent-agent-info-display-name native)))
    (should (string= "custom" (magent-agent-info-display-name custom)))))

(ert-deftest magent-agent-info-test-format-for-display ()
  "Test full display formatting."
  (let ((agent (magent-agent-info-create
                :name "test"
                :description "Test agent"
                :mode 'primary
                :hidden t)))
    (let ((formatted (magent-agent-info-format-for-display agent)))
      (should (string-match-p "test" formatted))
      (should (string-match-p "primary" formatted))
      (should (string-match-p "hidden" formatted))
      (should (string-match-p "Test agent" formatted)))))

;;; Merge defaults tests

(ert-deftest magent-agent-info-test-merge-defaults ()
  "Test merging agent info with defaults."
  (let* ((defaults (magent-agent-info-create
                    :name "default"
                    :mode 'all
                    :temperature 0.7
                    :native t))
         (agent (magent-agent-info-create
                 :name "custom"
                 :mode 'primary))
         (merged (magent-agent-info-merge-defaults agent defaults)))
    (should (string= "custom" (magent-agent-info-name merged)))
    (should (eq 'primary (magent-agent-info-mode merged)))
    (should (= 0.7 (magent-agent-info-temperature merged)))))

(ert-deftest magent-agent-info-test-merge-override ()
  "Test that agent values override defaults."
  (let* ((defaults (magent-agent-info-create
                    :name "default"
                    :mode 'all
                    :temperature 0.5))
         (agent (magent-agent-info-create
                 :name "custom"
                 :mode 'primary
                 :temperature 0.9))
         (merged (magent-agent-info-merge-defaults agent defaults)))
    (should (= 0.9 (magent-agent-info-temperature merged)))))

(provide 'magent-agent-info-test)
;;; magent-agent-info-test.el ends here
