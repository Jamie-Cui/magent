;;; magent-agent-registry-test.el --- Tests for magent-agent-registry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>

;;; Commentary:

;; Unit tests for magent-agent-registry.el

;;; Code:

(require 'ert)
(require 'magent-test-helper)
(require 'magent-agent-registry)
(require 'magent-agent-info)

;;; Registry initialization tests

(ert-deftest magent-agent-registry-test-init ()
  "Test registry initialization."
  (magent-test-with-registry
    (should magent-agent-registry--initialized)
    (should (> (magent-agent-registry-count) 0))))

(ert-deftest magent-agent-registry-test-reinit ()
  "Test registry reinitialization."
  (magent-test-with-registry
    (let ((count1 (magent-agent-registry-count)))
      (magent-agent-registry-reinit)
      (should (= count1 (magent-agent-registry-count))))))

;;; Agent registration tests

(ert-deftest magent-agent-registry-test-register ()
  "Test registering an agent."
  (magent-test-with-registry
    (let ((agent (magent-agent-info-create
                  :name "test-agent"
                  :description "Test"
                  :mode 'primary)))
      (magent-agent-registry-register agent)
      (should (magent-agent-registry-exists-p "test-agent"))
      (should (equal agent (magent-agent-registry-get "test-agent"))))))

(ert-deftest magent-agent-registry-test-register-replace ()
  "Test that registering replaces existing agent."
  (magent-test-with-registry
    (let ((agent1 (magent-agent-info-create
                   :name "test"
                   :description "First"
                   :mode 'primary))
          (agent2 (magent-agent-info-create
                   :name "test"
                   :description "Second"
                   :mode 'subagent)))
      (magent-agent-registry-register agent1)
      (magent-agent-registry-register agent2)
      (let ((retrieved (magent-agent-registry-get "test")))
        (should (string= "Second" (magent-agent-info-description retrieved)))
        (should (eq 'subagent (magent-agent-info-mode retrieved)))))))

(ert-deftest magent-agent-registry-test-unregister ()
  "Test unregistering an agent."
  (magent-test-with-registry
    (let ((agent (magent-agent-info-create
                  :name "temp-agent"
                  :mode 'all)))
      (magent-agent-registry-register agent)
      (should (magent-agent-registry-exists-p "temp-agent"))
      (magent-agent-registry-unregister "temp-agent")
      (should-not (magent-agent-registry-exists-p "temp-agent")))))

;;; Agent retrieval tests

(ert-deftest magent-agent-registry-test-get ()
  "Test getting an agent by name."
  (magent-test-with-registry
    (let ((agent (magent-agent-registry-get "build")))
      (should (magent-agent-info-p agent))
      (should (string= "build" (magent-agent-info-name agent))))))

(ert-deftest magent-agent-registry-test-get-nonexistent ()
  "Test getting a nonexistent agent."
  (magent-test-with-registry
    (should (null (magent-agent-registry-get "nonexistent")))))

(ert-deftest magent-agent-registry-test-get-default ()
  "Test getting the default agent."
  (magent-test-with-registry
    (let ((default (magent-agent-registry-get-default)))
      (should (magent-agent-info-p default)))))

(ert-deftest magent-agent-registry-test-set-default ()
  "Test setting the default agent."
  (magent-test-with-registry
    (magent-agent-registry-set-default "plan")
    (let ((default (magent-agent-registry-get-default)))
      (should (string= "plan" (magent-agent-info-name default))))))

;;; Agent listing tests

(ert-deftest magent-agent-registry-test-list ()
  "Test listing all agents."
  (magent-test-with-registry
    (let ((agents (magent-agent-registry-list)))
      (should (> (length agents) 0))
      (should (cl-every #'magent-agent-info-p agents)))))

(ert-deftest magent-agent-registry-test-list-include-hidden ()
  "Test listing agents including hidden ones."
  (magent-test-with-registry
    (let ((agent (magent-agent-info-create
                  :name "hidden-test"
                  :mode 'all
                  :hidden t)))
      (magent-agent-registry-register agent)
      (let ((without-hidden (magent-agent-registry-list nil))
            (with-hidden (magent-agent-registry-list t)))
        (should (< (length without-hidden) (length with-hidden)))
        (should-not (member "hidden-test"
                            (mapcar #'magent-agent-info-name without-hidden)))
        (should (member "hidden-test"
                        (mapcar #'magent-agent-info-name with-hidden)))))))

(ert-deftest magent-agent-registry-test-list-by-mode ()
  "Test filtering agents by mode."
  (magent-test-with-registry
    (let ((primary (magent-agent-registry-list nil 'primary))
          (subagent (magent-agent-registry-list nil 'subagent)))
      (should (> (length primary) 0))
      (should (> (length subagent) 0))
      ;; All primary agents should have mode primary or all
      (dolist (agent primary)
        (should (memq (magent-agent-info-mode agent) '(primary all))))
      ;; All subagents should have mode subagent or all
      (dolist (agent subagent)
        (should (memq (magent-agent-info-mode agent) '(subagent all)))))))

(ert-deftest magent-agent-registry-test-list-names ()
  "Test listing agent names."
  (magent-test-with-registry
    (let ((names (magent-agent-registry-list-names)))
      (should (> (length names) 0))
      (should (cl-every #'stringp names))
      (should (member "build" names)))))

(ert-deftest magent-agent-registry-test-primary-agents ()
  "Test listing primary agents."
  (magent-test-with-registry
    (let ((primary (magent-agent-registry-primary-agents)))
      (should (> (length primary) 0))
      (dolist (agent primary)
        (should (magent-agent-info-mode-p agent 'primary))))))

(ert-deftest magent-agent-registry-test-subagents ()
  "Test listing subagents."
  (magent-test-with-registry
    (let ((subagents (magent-agent-registry-subagents)))
      (should (> (length subagents) 0))
      (dolist (agent subagents)
        (should (magent-agent-info-mode-p agent 'subagent))))))

;;; Agent utilities tests

(ert-deftest magent-agent-registry-test-exists-p ()
  "Test checking agent existence."
  (magent-test-with-registry
    (should (magent-agent-registry-exists-p "build"))
    (should-not (magent-agent-registry-exists-p "nonexistent"))))

(ert-deftest magent-agent-registry-test-count ()
  "Test counting registered agents."
  (magent-test-with-registry
    (let ((count1 (magent-agent-registry-count)))
      (should (> count1 0))
      (let ((agent (magent-agent-info-create
                    :name "temp"
                    :mode 'all)))
        (magent-agent-registry-register agent)
        (should (= (1+ count1) (magent-agent-registry-count)))
        (magent-agent-registry-unregister "temp")
        (should (= count1 (magent-agent-registry-count)))))))

(ert-deftest magent-agent-registry-test-clear ()
  "Test clearing the registry."
  (magent-test-with-registry
    (magent-agent-registry-clear)
    (should (= 0 (hash-table-count magent-agent-registry--agents)))
    (should-not magent-agent-registry--initialized)))

(ert-deftest magent-agent-registry-test-resolve ()
  "Test resolving agent-or-name to agent info."
  (magent-test-with-registry
    (let* ((agent (magent-agent-registry-get "build"))
           (resolved1 (magent-agent-registry-resolve "build"))
           (resolved2 (magent-agent-registry-resolve agent)))
      (should (equal agent resolved1))
      (should (equal agent resolved2))
      (should (null (magent-agent-registry-resolve "nonexistent")))
      (should (null (magent-agent-registry-resolve 123))))))

;;; From config tests

(ert-deftest magent-agent-registry-test-register-from-config ()
  "Test registering agent from config."
  (magent-test-with-registry
    (let ((config '(:description "Custom agent"
                                 :mode primary
                                 :temperature 0.5
                                 :permission ((read . allow) (write . deny)))))
      (magent-agent-registry-register-from-config "custom" config)
      (let ((agent (magent-agent-registry-get "custom")))
        (should (magent-agent-info-p agent))
        (should (string= "custom" (magent-agent-info-name agent)))
        (should (string= "Custom agent" (magent-agent-info-description agent)))
        (should (eq 'primary (magent-agent-info-mode agent)))
        (should (= 0.5 (magent-agent-info-temperature agent)))))))

(provide 'magent-agent-registry-test)
;;; magent-agent-registry-test.el ends here
