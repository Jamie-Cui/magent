;;; magent-agent-file-test.el --- Tests for magent-agent-file  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>

;;; Commentary:

;; Unit tests for magent-agent-file.el

;;; Code:

(require 'ert)
(require 'magent-test-helper)
(require 'magent-agent-file)
(require 'magent-agent-info)
(require 'magent-agent-registry)
(require 'magent-permission)

;;; YAML value parsing tests

(ert-deftest magent-agent-file-test-parse-value-boolean ()
  "Test parsing boolean YAML values."
  (should (eq t (magent-agent-file--parse-value "true")))
  (should (eq nil (magent-agent-file--parse-value "false"))))

(ert-deftest magent-agent-file-test-parse-value-number ()
  "Test parsing numeric YAML values."
  (should (= 42 (magent-agent-file--parse-value "42")))
  (should (= 0.7 (magent-agent-file--parse-value "0.7")))
  (should (= 0 (magent-agent-file--parse-value "0"))))

(ert-deftest magent-agent-file-test-parse-value-string ()
  "Test parsing string YAML values."
  (should (string= "hello" (magent-agent-file--parse-value "hello")))
  ;; Quoted strings strip quotes
  (should (string= "hello" (magent-agent-file--parse-value "\"hello\"")))
  (should (string= "hello" (magent-agent-file--parse-value "'hello'"))))

(ert-deftest magent-agent-file-test-parse-value-list ()
  "Test parsing comma-separated list values."
  (let ((result (magent-agent-file--parse-value "read, write, bash")))
    (should (listp result))
    (should (= 3 (length result)))
    (should (string= "read" (nth 0 result)))
    (should (string= "write" (nth 1 result)))
    (should (string= "bash" (nth 2 result)))))

(ert-deftest magent-agent-file-test-parse-value-whitespace ()
  "Test that whitespace is trimmed from values."
  (should (string= "hello" (magent-agent-file--parse-value "  hello  ")))
  (should (eq t (magent-agent-file--parse-value "  true  "))))

;;; YAML parsing tests

(ert-deftest magent-agent-file-test-parse-yaml-basic ()
  "Test parsing basic key-value YAML."
  (let ((result (magent-agent-file--parse-yaml "description: A test agent\nmode: primary")))
    (should (string= "A test agent" (plist-get result :description)))
    (should (string= "primary" (plist-get result :mode)))))

(ert-deftest magent-agent-file-test-parse-yaml-types ()
  "Test parsing YAML with different value types."
  (let ((result (magent-agent-file--parse-yaml
                 "temperature: 0.5\nhidden: true\ncolor: blue")))
    (should (= 0.5 (plist-get result :temperature)))
    (should (eq t (plist-get result :hidden)))
    (should (string= "blue" (plist-get result :color)))))

(ert-deftest magent-agent-file-test-parse-yaml-empty ()
  "Test parsing empty YAML."
  (should (null (magent-agent-file--parse-yaml ""))))

;;; Frontmatter parsing tests

(ert-deftest magent-agent-file-test-parse-frontmatter ()
  "Test parsing frontmatter from markdown content."
  (let* ((content "---\ndescription: Test\nmode: primary\n---\n\nBody text here.")
         (result (magent-agent-file--parse-frontmatter content)))
    (should (consp result))
    (should (string= "Test" (plist-get (car result) :description)))
    (should (string-match-p "Body text here" (cdr result)))))

(ert-deftest magent-agent-file-test-parse-frontmatter-no-yaml ()
  "Test parsing content with no frontmatter."
  (let* ((content "Just plain markdown.\nNo frontmatter here.")
         (result (magent-agent-file--parse-frontmatter content)))
    (should (null (car result)))
    (should (string= content (cdr result)))))

(ert-deftest magent-agent-file-test-parse-frontmatter-empty-body ()
  "Test parsing frontmatter with empty body."
  (let* ((content "---\nmode: primary\n---\n")
         (result (magent-agent-file--parse-frontmatter content)))
    (should (plist-get (car result) :mode))
    (should (stringp (cdr result)))))

;;; Mode parsing tests

(ert-deftest magent-agent-file-test-parse-mode ()
  "Test parsing mode strings."
  (should (eq 'primary (magent-agent-file--parse-mode "primary")))
  (should (eq 'subagent (magent-agent-file--parse-mode "subagent")))
  (should (eq 'all (magent-agent-file--parse-mode "all")))
  (should (eq 'primary (magent-agent-file--parse-mode "Primary")))
  (should (eq 'all (magent-agent-file--parse-mode "unknown"))))

;;; Permission parsing tests

(ert-deftest magent-agent-file-test-parse-permission-all-allowed ()
  "Test that all tools are allowed when config enables all."
  (let ((rules (magent-agent-file--parse-permission '(:read t :write t))))
    (dolist (tool magent-tools--permission-keys)
      (should (eq 'allow (cdr (assq tool rules)))))))

(ert-deftest magent-agent-file-test-parse-permission-nil-config ()
  "Test that nil config produces all-allow rules."
  (let ((rules (magent-agent-file--parse-permission nil)))
    (dolist (tool magent-tools--permission-keys)
      (should (eq 'allow (cdr (assq tool rules)))))))

(ert-deftest magent-agent-file-test-parse-permission-deny-tool ()
  "Test denying a specific tool via config."
  (let ((rules (magent-agent-file--parse-permission '(:bash nil))))
    (should (eq 'deny (cdr (assq 'bash rules))))
    (should (eq 'allow (cdr (assq 'read rules))))))

;;; File loading tests

(ert-deftest magent-agent-file-test-load-fixture ()
  "Test loading an agent from a fixture file."
  (magent-test-with-registry
    (let ((agent (magent-agent-file-load (magent-test-fixture-path "test-agent.md"))))
      (should (magent-agent-info-p agent))
      (should (string= "test-agent" (magent-agent-info-name agent)))
      (should (string= "A test agent for unit tests"
                        (magent-agent-info-description agent)))
      (should (eq 'primary (magent-agent-info-mode agent)))
      (should (= 0.5 (magent-agent-info-temperature agent)))
      (should (string= "blue" (magent-agent-info-color agent)))
      (should (string-match-p "test agent" (magent-agent-info-prompt agent))))))

(ert-deftest magent-agent-file-test-load-minimal ()
  "Test loading a minimal agent file."
  (magent-test-with-registry
    (let ((agent (magent-agent-file-load (magent-test-fixture-path "minimal-agent.md"))))
      (should (magent-agent-info-p agent))
      (should (string= "minimal-agent" (magent-agent-info-name agent)))
      (should (eq 'all (magent-agent-info-mode agent))))))

(ert-deftest magent-agent-file-test-load-hidden ()
  "Test loading a hidden agent file."
  (magent-test-with-registry
    (let ((agent (magent-agent-file-load (magent-test-fixture-path "hidden-agent.md"))))
      (should (magent-agent-info-p agent))
      (should (magent-agent-info-hidden agent))
      (should (eq 'subagent (magent-agent-info-mode agent)))
      (should (= 0.0 (magent-agent-info-temperature agent))))))

(ert-deftest magent-agent-file-test-load-no-frontmatter ()
  "Test loading a file with no frontmatter returns nil."
  (magent-test-with-registry
    (should (null (magent-agent-file-load
                   (magent-test-fixture-path "no-frontmatter.md"))))))

(ert-deftest magent-agent-file-test-load-nonexistent ()
  "Test loading a nonexistent file returns nil."
  (magent-test-with-registry
    (should (null (magent-agent-file-load "/nonexistent/file.md")))))

(ert-deftest magent-agent-file-test-load-registers-agent ()
  "Test that loading an agent registers it in the registry."
  (magent-test-with-registry
    (should-not (magent-agent-registry-exists-p "test-agent"))
    (magent-agent-file-load (magent-test-fixture-path "test-agent.md"))
    (should (magent-agent-registry-exists-p "test-agent"))))

;;; File listing tests

(ert-deftest magent-agent-file-test-list-files ()
  "Test listing agent files from fixtures directory."
  (let* ((fixture-dir (file-name-directory (magent-test-fixture-path "test-agent.md")))
         ;; magent-agent-file--list-files wraps through --agent-dir which
         ;; appends magent-agent-directory.  Bind it to "." so the fixture
         ;; dir is used directly.
         (magent-agent-directory ".")
         (files (magent-agent-file--list-files fixture-dir)))
    (should (listp files))
    (should (> (length files) 0))
    (should (cl-every (lambda (f) (string-suffix-p ".md" f)) files))))

(ert-deftest magent-agent-file-test-list-files-nonexistent ()
  "Test listing files from nonexistent directory."
  (let ((files (magent-agent-file--list-files "/nonexistent/dir")))
    (should (null files))))

;;; Load all tests

(ert-deftest magent-agent-file-test-load-all ()
  "Test loading all agent files from fixtures."
  (magent-test-with-registry
    (let* ((fixture-dir (file-name-directory (magent-test-fixture-path "test-agent.md")))
           (magent-agent-directory ".")
           (count (magent-agent-file-load-all fixture-dir)))
      ;; 3 valid agents (test-agent, minimal-agent, hidden-agent)
      ;; no-frontmatter should be skipped
      (should (= 3 count))
      (should (magent-agent-registry-exists-p "test-agent"))
      (should (magent-agent-registry-exists-p "minimal-agent"))
      (should (magent-agent-registry-exists-p "hidden-agent")))))

;;; Save/load round-trip tests

(ert-deftest magent-agent-file-test-save-and-load ()
  "Test saving an agent and loading it back."
  (magent-test-with-temp-dir tmpdir
    (magent-test-with-registry
      (let* ((agent (magent-agent-info-create
                     :name "roundtrip"
                     :description "Round-trip test"
                     :mode 'primary
                     :temperature 0.8
                     :prompt "You are a round-trip test agent."))
             (filepath (magent-agent-file-save agent tmpdir)))
        (should (file-exists-p filepath))
        ;; Load it back
        (let ((loaded (magent-agent-file-load filepath)))
          (should (magent-agent-info-p loaded))
          (should (string= "roundtrip" (magent-agent-info-name loaded)))
          (should (string= "Round-trip test"
                            (magent-agent-info-description loaded)))
          (should (eq 'primary (magent-agent-info-mode loaded)))
          (should (= 0.8 (magent-agent-info-temperature loaded)))
          (should (string-match-p "round-trip test agent"
                                  (magent-agent-info-prompt loaded))))))))

;;; Project root tests

(ert-deftest magent-agent-file-test-project-root-default ()
  "Test project root falls back to default-directory."
  (let ((magent-project-root-function nil))
    (should (string= default-directory
                     (magent-agent-file--project-root)))))

(ert-deftest magent-agent-file-test-project-root-custom ()
  "Test project root with custom function."
  (let ((magent-project-root-function (lambda () "/custom/root/")))
    (should (string= "/custom/root/"
                     (magent-agent-file--project-root)))))

(provide 'magent-agent-file-test)
;;; magent-agent-file-test.el ends here
