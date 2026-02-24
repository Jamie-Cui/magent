;;; magent-permission-test.el --- Tests for magent-permission  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>

;;; Commentary:

;; Unit tests for magent-permission.el

;;; Code:

(require 'ert)
(require 'magent-test-helper)
(require 'magent-permission)

;;; Permission creation tests

(ert-deftest magent-permission-test-create ()
  "Test creating permission structures."
  (let ((perm (magent-permission-create :rules '((read . allow) (write . deny)))))
    (should (magent-permission-p perm))
    (should (equal '((read . allow) (write . deny))
                   (magent-permission-rules perm)))))

;;; Permission resolution tests

(ert-deftest magent-permission-test-resolve-simple ()
  "Test simple permission resolution."
  (should (eq 'allow (magent-permission-resolve 'allow 'read)))
  (should (eq 'deny (magent-permission-resolve 'deny 'write)))
  (should (eq 'ask (magent-permission-resolve 'ask 'bash))))

(ert-deftest magent-permission-test-resolve-tool ()
  "Test tool-specific permission resolution."
  (let ((rules '((read . allow)
                 (write . deny)
                 (bash . ask))))
    (should (eq 'allow (magent-permission-resolve rules 'read)))
    (should (eq 'deny (magent-permission-resolve rules 'write)))
    (should (eq 'ask (magent-permission-resolve rules 'bash)))))

(ert-deftest magent-permission-test-resolve-wildcard ()
  "Test wildcard permission resolution."
  (let ((rules '((* . allow)
                 (write . deny))))
    (should (eq 'allow (magent-permission-resolve rules 'read)))
    (should (eq 'deny (magent-permission-resolve rules 'write)))
    (should (eq 'allow (magent-permission-resolve rules 'unknown)))))

(ert-deftest magent-permission-test-resolve-file-glob ()
  "Test file pattern matching in permissions."
  (let ((rules '((read . ((* . allow)
                          ("*.env" . deny)
                          ("*.env.*" . deny))))))
    (should (eq 'allow (magent-permission-resolve rules 'read "config.el")))
    (should (eq 'deny (magent-permission-resolve rules 'read ".env")))
    (should (eq 'deny (magent-permission-resolve rules 'read "prod.env")))
    (should (eq 'deny (magent-permission-resolve rules 'read ".env.local")))))

(ert-deftest magent-permission-test-resolve-nested ()
  "Test nested permission rules."
  (let ((rules '((write . (("*.md" . allow)
                           ("*.txt" . allow)
                           ("*" . deny))))))
    (should (eq 'allow (magent-permission-resolve rules 'write "README.md")))
    (should (eq 'allow (magent-permission-resolve rules 'write "notes.txt")))
    (should (eq 'deny (magent-permission-resolve rules 'write "config.el")))))

;;; Permission checking tests

(ert-deftest magent-permission-test-allow-p ()
  "Test allow checking."
  (let ((rules '((read . allow) (write . deny))))
    (should (magent-permission-allow-p rules 'read))
    (should-not (magent-permission-allow-p rules 'write))))

(ert-deftest magent-permission-test-deny-p ()
  "Test deny checking."
  (let ((rules '((read . allow) (write . deny))))
    (should-not (magent-permission-deny-p rules 'read))
    (should (magent-permission-deny-p rules 'write))))

(ert-deftest magent-permission-test-ask-p ()
  "Test ask checking."
  (let ((rules '((read . allow) (bash . ask))))
    (should-not (magent-permission-ask-p rules 'read))
    (should (magent-permission-ask-p rules 'bash))))

;;; Permission merging tests

(ert-deftest magent-permission-test-merge-simple ()
  "Test simple permission merging."
  (let* ((base '((read . allow) (write . deny)))
         (override '((write . allow) (bash . ask)))
         (merged (magent-permission-merge base override)))
    (should (eq 'allow (magent-permission-resolve merged 'read)))
    (should (eq 'allow (magent-permission-resolve merged 'write)))
    (should (eq 'ask (magent-permission-resolve merged 'bash)))))

(ert-deftest magent-permission-test-merge-nested ()
  "Test merging nested permission rules."
  (let* ((base '((write . (("*.el" . allow)
                           ("*" . deny)))))
         (override '((write . (("*.md" . allow)))))
         (merged (magent-permission-merge base override)))
    (should (eq 'allow (magent-permission-resolve merged 'write "test.el")))
    (should (eq 'allow (magent-permission-resolve merged 'write "README.md")))))

(ert-deftest magent-permission-test-merge-multiple ()
  "Test merging multiple rulesets."
  (let* ((r1 '((read . allow)))
         (r2 '((write . deny)))
         (r3 '((bash . ask)))
         (merged (magent-permission-merge r1 r2 r3)))
    (should (eq 'allow (magent-permission-resolve merged 'read)))
    (should (eq 'deny (magent-permission-resolve merged 'write)))
    (should (eq 'ask (magent-permission-resolve merged 'bash)))))

;;; Permission defaults tests

(ert-deftest magent-permission-test-defaults ()
  "Test default permission rules."
  (let ((defaults (magent-permission-defaults)))
    (should (listp defaults))
    (should (magent-permission-allow-p defaults 'read))
    (should (magent-permission-deny-p defaults 'read ".env"))
    (should (magent-permission-deny-p defaults 'read ".env.local"))
    (should (magent-permission-allow-p defaults 'read ".env.example"))))

;;; Tool filtering tests

(ert-deftest magent-permission-test-filter-tools ()
  "Test filtering tools based on permissions."
  (let ((rules '((read . allow)
                 (write . deny)
                 (bash . allow)))
        (tools '(read write bash grep)))
    (let ((allowed (magent-permission-filter-tools rules tools)))
      (should (member 'read allowed))
      (should (member 'bash allowed))
      (should (member 'grep allowed))
      (should-not (member 'write allowed)))))

(ert-deftest magent-permission-test-disabled ()
  "Test getting disabled tools."
  (let ((rules '((read . allow)
                 (write . deny)
                 (bash . deny)))
        (tools '(read write bash grep)))
    (let ((disabled (magent-permission-disabled rules tools)))
      (should (member 'write disabled))
      (should (member 'bash disabled))
      (should-not (member 'read disabled))
      (should-not (member 'grep disabled)))))

;;; From config tests

(ert-deftest magent-permission-test-from-config ()
  "Test creating permissions from config."
  (let* ((config '((read . allow) (write . deny)))
         (perm (magent-permission-from-config config)))
    (should (magent-permission-p perm))
    (should (equal config (magent-permission-rules perm)))))

(provide 'magent-permission-test)
;;; magent-permission-test.el ends here
