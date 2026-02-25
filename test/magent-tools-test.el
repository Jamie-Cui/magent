;;; magent-tools-test.el --- Tests for magent-tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>

;;; Commentary:

;; Unit tests for magent-tools.el

;;; Code:

(require 'ert)
(require 'magent-test-helper)
(require 'magent-tools)

;;; read_file tests

(ert-deftest magent-tools-test-read-file ()
  "Test reading a file."
  (magent-test-with-temp-file path "Hello, world!"
    (should (string= "Hello, world!" (magent-tools--read-file path)))))

(ert-deftest magent-tools-test-read-file-multiline ()
  "Test reading a multi-line file."
  (magent-test-with-temp-file path "line1\nline2\nline3"
    (should (string= "line1\nline2\nline3" (magent-tools--read-file path)))))

(ert-deftest magent-tools-test-read-file-nonexistent ()
  "Test reading a nonexistent file returns error string."
  (let ((result (magent-tools--read-file "/nonexistent/file.txt")))
    (should (stringp result))
    (should (string-match-p "Error reading file" result))))

(ert-deftest magent-tools-test-read-file-empty ()
  "Test reading an empty file."
  (magent-test-with-temp-file path ""
    (should (string= "" (magent-tools--read-file path)))))

;;; write_file tests

(ert-deftest magent-tools-test-write-file ()
  "Test writing a file."
  (magent-test-with-temp-dir tmpdir
    (let ((path (expand-file-name "test.txt" tmpdir)))
      (magent-tools--write-file path "Hello!")
      (should (file-exists-p path))
      (should (string= "Hello!" (magent-tools--read-file path))))))

(ert-deftest magent-tools-test-write-file-creates-dirs ()
  "Test that write_file creates parent directories."
  (magent-test-with-temp-dir tmpdir
    (let ((path (expand-file-name "sub/dir/test.txt" tmpdir)))
      (let ((result (magent-tools--write-file path "content")))
        (should (string-match-p "Successfully wrote" result))
        (should (file-exists-p path))))))

(ert-deftest magent-tools-test-write-file-overwrite ()
  "Test that write_file overwrites existing content."
  (magent-test-with-temp-file path "old content"
    (magent-tools--write-file path "new content")
    (should (string= "new content" (magent-tools--read-file path)))))

;;; edit_file tests

(ert-deftest magent-tools-test-edit-file-basic ()
  "Test basic edit_file replacement."
  (magent-test-with-temp-file path "Hello, world!"
    (let ((result (magent-tools--edit-file path "world" "Emacs")))
      (should (string-match-p "Successfully edited" result))
      (should (string= "Hello, Emacs!" (magent-tools--read-file path))))))

(ert-deftest magent-tools-test-edit-file-multiline ()
  "Test edit_file with multi-line old text."
  (magent-test-with-temp-file path "line1\nline2\nline3"
    (let ((result (magent-tools--edit-file path "line1\nline2" "replaced")))
      (should (string-match-p "Successfully edited" result))
      (should (string= "replaced\nline3" (magent-tools--read-file path))))))

(ert-deftest magent-tools-test-edit-file-not-found ()
  "Test edit_file when old text is not found."
  (magent-test-with-temp-file path "Hello, world!"
    (let ((result (magent-tools--edit-file path "nonexistent" "replacement")))
      (should (string-match-p "not found" result)))))

(ert-deftest magent-tools-test-edit-file-multiple-matches ()
  "Test edit_file when old text matches multiple times."
  (magent-test-with-temp-file path "foo bar foo"
    (let ((result (magent-tools--edit-file path "foo" "baz")))
      (should (string-match-p "found 2 times" result))
      ;; File should not be modified
      (should (string= "foo bar foo" (magent-tools--read-file path))))))

(ert-deftest magent-tools-test-edit-file-nonexistent-file ()
  "Test edit_file on nonexistent file."
  (let ((result (magent-tools--edit-file "/nonexistent/file.txt" "old" "new")))
    (should (string-match-p "Error" result))))

(ert-deftest magent-tools-test-edit-file-preserves-rest ()
  "Test that edit_file preserves content around the replacement."
  (magent-test-with-temp-file path "prefix-REPLACE-suffix"
    (magent-tools--edit-file path "REPLACE" "NEW")
    (should (string= "prefix-NEW-suffix" (magent-tools--read-file path)))))

;;; bash tests

(ert-deftest magent-tools-test-bash-echo ()
  "Test executing a simple echo command."
  (let ((result (magent-tools--bash "echo hello")))
    (should (string= "hello" result))))

(ert-deftest magent-tools-test-bash-no-output ()
  "Test command with no output."
  (let ((result (magent-tools--bash "true")))
    (should (string= "Command completed with no output" result))))

(ert-deftest magent-tools-test-bash-multiline ()
  "Test command with multi-line output."
  (let ((result (magent-tools--bash "echo 'a'; echo 'b'")))
    (should (string-match-p "a" result))
    (should (string-match-p "b" result))))

;;; glob tests

(ert-deftest magent-tools-test-glob-match ()
  "Test glob finds matching files."
  (magent-test-with-temp-dir tmpdir
    ;; Create test files
    (magent-tools--write-file (expand-file-name "test.el" tmpdir) "")
    (magent-tools--write-file (expand-file-name "test.md" tmpdir) "")
    (let ((result (magent-tools--glob "*.el" tmpdir)))
      (should (string-match-p "test\\.el" result))
      (should-not (string-match-p "test\\.md" result)))))

(ert-deftest magent-tools-test-glob-no-match ()
  "Test glob when no files match."
  (magent-test-with-temp-dir tmpdir
    (let ((result (magent-tools--glob "*.nonexistent" tmpdir)))
      (should (string= "" result)))))

;;; grep tests (require rg to be installed)

(ert-deftest magent-tools-test-grep-basic ()
  "Test basic grep search."
  (skip-unless (executable-find "rg"))
  (magent-test-with-temp-dir tmpdir
    (magent-tools--write-file (expand-file-name "test.txt" tmpdir) "hello world\nfoo bar\nhello again")
    (let ((result (magent-tools--grep "hello" tmpdir)))
      (should (string-match-p "hello" result))
      (should-not (string-match-p "No matches found" result)))))

(ert-deftest magent-tools-test-grep-no-match ()
  "Test grep with no matches."
  (skip-unless (executable-find "rg"))
  (magent-test-with-temp-dir tmpdir
    (magent-tools--write-file (expand-file-name "test.txt" tmpdir) "hello world")
    (let ((result (magent-tools--grep "nonexistent" tmpdir)))
      (should (string= "No matches found" result)))))

(ert-deftest magent-tools-test-grep-case-insensitive ()
  "Test case-insensitive grep."
  (skip-unless (executable-find "rg"))
  (magent-test-with-temp-dir tmpdir
    (magent-tools--write-file (expand-file-name "test.txt" tmpdir) "Hello World")
    ;; Default is case-insensitive
    (let ((result (magent-tools--grep "hello" tmpdir)))
      (should (string-match-p "Hello" result)))))

(ert-deftest magent-tools-test-grep-case-sensitive ()
  "Test case-sensitive grep."
  (skip-unless (executable-find "rg"))
  (magent-test-with-temp-dir tmpdir
    (magent-tools--write-file (expand-file-name "test.txt" tmpdir) "Hello World")
    ;; Case-sensitive should not match lowercase
    (let ((result (magent-tools--grep "hello" tmpdir t)))
      (should (string= "No matches found" result)))))

;;; Tool execution dispatcher tests

(ert-deftest magent-tools-test-execute-read ()
  "Test dispatcher for read_file."
  (magent-test-with-temp-file path "test content"
    (let ((result (magent-tools-execute "read_file" `((path . ,path)))))
      (should (string= "test content" result)))))

(ert-deftest magent-tools-test-execute-write ()
  "Test dispatcher for write_file."
  (magent-test-with-temp-file path ""
    (magent-tools-execute "write_file" `((path . ,path) (content . "written")))
    (should (string= "written" (magent-tools--read-file path)))))

(ert-deftest magent-tools-test-execute-edit ()
  "Test dispatcher for edit_file."
  (magent-test-with-temp-file path "old text here"
    (magent-tools-execute "edit_file" `((path . ,path)
                                        (old_text . "old text")
                                        (new_text . "new text")))
    (should (string= "new text here" (magent-tools--read-file path)))))

(ert-deftest magent-tools-test-execute-unknown ()
  "Test dispatcher for unknown tool."
  (let ((result (magent-tools-execute "unknown_tool" nil)))
    (should (string-match-p "Unknown tool" result))))

;;; emacs_eval tests

(ert-deftest magent-tools-test-emacs-eval-arithmetic ()
  "Test evaluating a simple arithmetic expression."
  (should (string= "3" (magent-tools--emacs-eval "(+ 1 2)"))))

(ert-deftest magent-tools-test-emacs-eval-string ()
  "Test evaluating a string expression."
  (should (string= "\"hello world\""
                    (magent-tools--emacs-eval "(concat \"hello\" \" \" \"world\")"))))

(ert-deftest magent-tools-test-emacs-eval-list ()
  "Test evaluating an expression that returns a list."
  (should (string= "(1 2 3)"
                    (magent-tools--emacs-eval "(list 1 2 3)"))))

(ert-deftest magent-tools-test-emacs-eval-nil ()
  "Test evaluating an expression that returns nil."
  (should (string= "nil" (magent-tools--emacs-eval "nil"))))

(ert-deftest magent-tools-test-emacs-eval-void-function ()
  "Test evaluating a call to an undefined function."
  (let ((result (magent-tools--emacs-eval "(magent-nonexistent-function-xyz)")))
    (should (string-match-p "Error evaluating sexp" result))))

(ert-deftest magent-tools-test-emacs-eval-parse-error ()
  "Test evaluating an unparseable sexp."
  (let ((result (magent-tools--emacs-eval "((")))
    (should (string-match-p "Error evaluating sexp" result))))

(ert-deftest magent-tools-test-emacs-eval-lexical-binding ()
  "Test that evaluation uses lexical binding."
  (should (string= "10"
                    (magent-tools--emacs-eval
                     "(let ((x 10)) (funcall (lambda () x)))"))))

(ert-deftest magent-tools-test-execute-emacs-eval ()
  "Test dispatcher for emacs_eval."
  (let ((result (magent-tools-execute "emacs_eval" '((sexp . "(* 6 7)")))))
    (should (string= "42" result))))

;;; Permission key mapping tests

(ert-deftest magent-tools-test-permission-keys-complete ()
  "Test that all gptel tools have permission key mappings."
  (dolist (tool magent-tools--all-gptel-tools)
    (let* ((name (gptel-tool-name tool))
           (key (cdr (assoc name magent-tools--name-to-permission-key))))
      (should key)
      (should (memq key magent-tools--permission-keys)))))

(ert-deftest magent-tools-test-permission-keys-in-config ()
  "Test that all permission keys are in the default config."
  (dolist (key magent-tools--permission-keys)
    (should (memq key magent-enable-tools))))

;;; delegate tests

(ert-deftest magent-tools-test-delegate-unknown-agent ()
  "Test delegate with unknown agent returns error."
  (magent-test-with-registry
    (let ((result nil))
      (magent-tools--delegate (lambda (r) (setq result r))
                              "nonexistent-agent"
                              "do something")
      (should (stringp result))
      (should (string-match-p "not found" result)))))

(ert-deftest magent-tools-test-delegate-non-subagent ()
  "Test delegate with a primary-only agent returns error."
  (magent-test-with-registry
    (let ((result nil))
      (magent-tools--delegate (lambda (r) (setq result r))
                              "build"
                              "do something")
      (should (stringp result))
      (should (string-match-p "not a subagent" result)))))

(ert-deftest magent-tools-test-delegate-tool-is-async ()
  "Test that the delegate tool struct has async flag set."
  (should (gptel-tool-async magent-tools--delegate-tool)))

(ert-deftest magent-tools-test-delegate-permission-key ()
  "Test that delegate has a permission key mapping."
  (should (memq 'delegate magent-tools--permission-keys))
  (should (equal 'delegate
                 (cdr (assoc "delegate" magent-tools--name-to-permission-key)))))

(ert-deftest magent-tools-test-delegate-in-tool-list ()
  "Test that delegate tool is in the all-gptel-tools list."
  (should (memq magent-tools--delegate-tool magent-tools--all-gptel-tools)))

(ert-deftest magent-tools-test-delegate-explore-denied ()
  "Test that explore agent cannot use delegate."
  (magent-test-with-registry
    (let* ((explore (magent-agent-registry-get "explore"))
           (tools (magent-tools-get-gptel-tools explore))
           (tool-names (mapcar #'gptel-tool-name tools)))
      (should-not (member "delegate" tool-names)))))

(ert-deftest magent-tools-test-delegate-general-denied ()
  "Test that general agent cannot use delegate."
  (magent-test-with-registry
    (let* ((general (magent-agent-registry-get "general"))
           (tools (magent-tools-get-gptel-tools general))
           (tool-names (mapcar #'gptel-tool-name tools)))
      (should-not (member "delegate" tool-names)))))

(ert-deftest magent-tools-test-delegate-build-allowed ()
  "Test that build agent can use delegate."
  (magent-test-with-registry
    (let* ((build (magent-agent-registry-get "build"))
           (tools (magent-tools-get-gptel-tools build))
           (tool-names (mapcar #'gptel-tool-name tools)))
      (should (member "delegate" tool-names)))))

(ert-deftest magent-tools-test-execute-delegate ()
  "Test dispatcher for delegate (sync fallback)."
  (let ((result (magent-tools-execute "delegate" nil)))
    (should (string-match-p "async context" result))))

(provide 'magent-tools-test)
;;; magent-tools-test.el ends here
