;;; magent-action-context-test.el --- Action context tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Regressions for declarative mode applicability and approved project source.

;;; Code:

(require 'ert)
(require 'magent)
(require 'magent-action-project)
(require 'magent-action-builtins)

(defvar magent-context-test--minor nil)
(defvar magent-context-test--loads 0)

(defconst magent-context-test--source
  (concat ";;; -*- lexical-binding: t; -*-\n"
          "(cl-incf magent-context-test--loads)\n"
          "(magent-action-register \"local\" :exposure '(slash interactive) "
          ":session-policy 'isolated "
          ":workflow (iter-lambda (_) \"Local result\"))\n"))

(defmacro magent-context-test--with-project (&rest body)
  "Run BODY with a temporary project and isolated registration state."
  (declare (indent 0))
  `(let* ((storage (make-temp-file "magent-action-context-" t))
          (scope (magent-session-canonical-scope
                  (expand-file-name "project" storage)))
          (directory (expand-file-name ".magent/actions" scope))
          (source (expand-file-name "local.el" directory))
          (magent-action-project-trust-file (expand-file-name "trust.json" storage))
          (magent-action-project--loaded (make-hash-table :test #'equal))
          (magent-action-project--registrations (make-hash-table :test #'equal))
          (magent-action-project--declined (make-hash-table :test #'equal))
          (magent-action-project--allow-prompt t)
          (magent-action--registry nil)
          (magent-action-registry-changed-hook nil)
          (magent-context-test--loads 0)
          (noninteractive nil))
     (unwind-protect
         (progn
           (make-directory directory t)
           (with-temp-file source (insert magent-context-test--source))
           ,@body)
       (delete-directory storage t))))

(ert-deftest magent-context-modes-nesting-inheritance-and-minor-state ()
  (let ((magent-action--registry nil))
    (let ((spec (magent-action-register
                 "modes" :session-policy 'isolated
                 :modes '(or (major org-mode)
                             (and (major prog-mode)
                                  (minor magent-context-test--minor)))
                 :workflow (iter-lambda (_) nil))))
      (with-temp-buffer
        (should-not (magent-action-applicable-p spec))
        (emacs-lisp-mode)
        (should-not (magent-action-applicable-p spec))
        (setq-local magent-context-test--minor t)
        (should (magent-action-applicable-p spec))
        (setq-local magent-context-test--minor nil)
        (should-not (magent-action-applicable-p spec))
        (org-mode)
        (should (magent-action-applicable-p spec))))))

(ert-deftest magent-context-modes-reject-code-and-malformed-conditions ()
  (let ((magent-action--registry nil)
        executed)
    (dolist (condition '((and) (or) (major) (minor t) (minor :keyword)
                         (major text-mode extra) (major . text-mode)
                         (not (major text-mode))
                         (or (major org-mode) (setq executed t))))
      (should-error
       (magent-action-register
        "bad" :session-policy 'isolated :modes condition
        :workflow (iter-lambda (_) nil))))
    (should-not executed)
    (should-not magent-action--registry)))

(ert-deftest magent-context-mode-mismatch-precedes-invocation ()
  (let ((magent-action--registry nil))
    (magent-action-register
     "mode-test" :exposure '(interactive) :modes '(major org-mode)
     :session-policy 'isolated :workflow (iter-lambda (_) nil))
    (cl-letf (((symbol-function 'magent-runtime-ensure-initialized) #'ignore)
              ((symbol-function 'magent-runtime-prepare-context) #'ignore)
              ((symbol-function 'magent-runtime-context-scope) (lambda () 'global))
              ((symbol-function 'magent-action--make-invocation)
               (lambda (&rest _) (ert-fail "Unexpected invocation"))))
      (with-temp-buffer
        (should-error (magent-action-run "mode-test") :type 'user-error)))))

(ert-deftest magent-context-picker-resolves-before-mode-filtering ()
  (let ((magent-action--registry nil)
        (scope "/tmp/action-mode-project")
        origin submitted)
    (magent-action-register
     "same" :exposure '(interactive) :source-layer 'user
     :session-policy 'isolated :workflow (iter-lambda (_) nil))
    (magent-action-register
     "same" :exposure '(interactive) :source-layer 'project :source-scope scope
     :modes '(major org-mode)
     :session-policy 'isolated :workflow (iter-lambda (_) nil))
    (magent-action-builtins-register '(doctor))
    (cl-letf (((symbol-function 'magent-runtime-ensure-initialized) #'ignore)
              ((symbol-function 'magent-runtime-prepare-context) #'ignore)
              ((symbol-function 'magent-runtime-context-scope) (lambda () scope))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (should (equal (all-completions "" table) '("doctor")))
                 (let* ((metadata (completion-metadata "" table nil))
                        (affix (completion-metadata-get metadata 'affixation-function))
                        (row (car (funcall affix '("doctor")))))
                   (should (string-match-p "diagnose Magent" (nth 2 row))))
                 ;; Simulate another buffer becoming current while the menu runs.
                 (set-buffer (get-buffer-create " *action-menu-test*"))
                 "doctor"))
              ((symbol-function 'magent-action-run)
               (lambda (&rest _) (setq submitted (current-buffer)))))
      (unwind-protect
          (with-temp-buffer
            (setq origin (current-buffer))
            (magent-action)
            (should (eq origin submitted)))
        (when-let* ((buffer (get-buffer " *action-menu-test*")))
          (kill-buffer buffer))))))

(ert-deftest magent-context-slash-ignores-mode-conditions ()
  (let* ((magent-action--registry nil)
         (session (magent-session-create :id "mode-slash"))
         (runtime (magent-runtime-session-create
                   :id "mode-slash" :scope 'global :magent-session session)))
    (magent-action-register
     "slash-mode" :modes '(major org-mode) :session-policy 'current
     :workflow (iter-lambda (_) nil))
    (cl-letf (((symbol-function 'magent-action--execute) #'identity))
      (with-temp-buffer
        (should (magent-action-invocation-p
                 (magent-action-invoke "slash-mode" runtime)))))))

(ert-deftest magent-context-project-trust-persists-and-content-changes-reprompt ()
  (magent-context-test--with-project
    (let ((prompts 0))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (cl-incf prompts) t)))
        (magent-action-load-project-scope scope)
        (should (= magent-context-test--loads 1))
        (should (= prompts 1))
        (let ((spec (magent-action-get "local" scope 'interactive)))
          (should (eq (magent-action-spec-source-layer spec) 'project))
          (should (equal (magent-action-spec-source-scope spec) scope))
          (should-not (magent-action-get "local" 'global 'interactive)))
        (magent-action-load-project-scope scope)
        (should (= magent-context-test--loads 1))
        ;; Simulate a fresh runtime while retaining the trust file.
        (magent-action-remove-project-scope scope)
        (magent-action-load-project-scope scope)
        (should (= prompts 1))
        (should (= magent-context-test--loads 2))
        (with-temp-file source (insert magent-context-test--source "; changed\n"))
        (magent-action-load-project-scope scope)
        (should (= prompts 2))
        (should (= magent-context-test--loads 3))
        (delete-file source)
        (magent-action-load-project-scope scope)
        (should-not (magent-action-get "local" scope))
        (should (= prompts 2))))))

(ert-deftest magent-context-project-background-discovery-and-denial-never-execute ()
  (magent-context-test--with-project
    (let ((prompts 0))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (cl-incf prompts) nil)))
        (let ((magent-action-project--allow-prompt nil))
          (magent-action-load-project-scope scope))
        (should (= prompts 0))
        (magent-action-load-project-scope scope)
        (magent-action-load-project-scope scope)
        (should (= prompts 1))
        (should (= magent-context-test--loads 0))
        (should-not (magent-action-get "local" scope))
        (should-not (file-exists-p magent-action-project-trust-file))))))

(ert-deftest magent-context-project-fingerprint-ignores-printer-truncation ()
  (magent-context-test--with-project
    (let ((print-length 1)
          (print-level 1)
          (prompts 0))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (cl-incf prompts) t)))
        (magent-action-load-project-scope scope)
        (with-temp-file source (insert magent-context-test--source "; changed\n"))
        (magent-action-load-project-scope scope))
      (should (= prompts 2))
      (should (= magent-context-test--loads 2)))))

(ert-deftest magent-context-project-changed-source-revokes-old-registration ()
  (magent-context-test--with-project
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (magent-action-load-project-scope scope))
    (with-temp-file source (insert magent-context-test--source "; new\n"))
    (let ((magent-action-project--allow-prompt nil))
      (magent-action-load-project-scope scope))
    (should-not (magent-action-get "local" scope))
    (should (= magent-context-test--loads 1))))

(ert-deftest magent-context-project-executes-approved-snapshot-not-raced-file ()
  (magent-context-test--with-project
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (with-temp-file source (insert "(error \"Unapproved code\")"))
                 t)))
      (magent-action-load-project-scope scope))
    (should (= magent-context-test--loads 1))
    (should (magent-action-get "local" scope))))

(ert-deftest magent-context-project-errors-publish-no-partial-registrations ()
  (magent-context-test--with-project
    (let ((unrelated (magent-action-register
                      "unrelated" :source-layer 'project :source-scope "/tmp/elsewhere"
                      :session-policy 'isolated :workflow (iter-lambda (_) nil))))
      (with-temp-file (expand-file-name "z-broken.el" directory)
        (insert "(error \"Broken file\")"))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (should (string-match-p
                 "z-broken.el.*Broken file"
                 (error-message-string
                  (should-error (magent-action-load-project-scope scope))))))
      (should-not (magent-action-get "local" scope))
      (should (eq (magent-action-get "unrelated" "/tmp/elsewhere") unrelated)))))

(ert-deftest magent-context-project-rejects-escaping-registration-scope ()
  (magent-context-test--with-project
    (with-temp-file source
      (insert "(magent-action-register \"escape\" :source-layer 'user "
              ":session-policy 'isolated :workflow (iter-lambda (_) nil))"))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (should-error (magent-action-load-project-scope scope)))
    (should-not magent-action--registry)))

(ert-deftest magent-context-project-load-keeps-lexical-scope-without-visiting-file ()
  (magent-context-test--with-project
    (with-temp-file source
      (insert ";;; -*- lexical-binding: t; -*-\n"
              "(when buffer-file-name (error \"Unexpected file buffer\"))\n"
              "(let ((answer \"Captured value\"))\n"
              "  (magent-action-register \"lexical\" :session-policy 'isolated\n"
              "    :workflow (iter-lambda (_) (iter-yield answer))))\n"))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (magent-action-load-project-scope scope))
    (let ((iterator (funcall (magent-action-spec-workflow
                             (magent-action-get "lexical" scope)) nil)))
      (unwind-protect
          (should (equal (iter-next iterator) "Captured value"))
        (iter-close iterator)))))

(ert-deftest magent-context-project-preserves-manual-registrations-and-ignores-locks ()
  (magent-context-test--with-project
    (let ((manual (magent-action-register
                   "manual" :source-layer 'project :source-scope scope
                   :session-policy 'isolated :workflow (iter-lambda (_) nil))))
      (with-temp-file (expand-file-name ".#local.el" directory)
        (insert "not Lisp"))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (magent-action-load-project-scope scope))
      (magent-action-remove-project-scope scope)
      (should (eq (magent-action-get "manual" scope) manual))
      (should-not (magent-action-get "local" scope)))))

(ert-deftest magent-context-project-removal-does-not-reenter-discovery ()
  (magent-context-test--with-project
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (magent-action-load-project-scope scope))
    (let ((magent-action-registry-changed-hook
           (list (lambda () (magent-action-load-project-scope scope)))))
      (magent-action-remove-project-scope scope))
    (should-not (magent-action-get "local" scope))
    (should (= magent-context-test--loads 1))))

(ert-deftest magent-context-project-revocation-requires-new-approval ()
  (magent-context-test--with-project
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (magent-action-load-project-scope scope))
    (cl-letf (((symbol-function 'magent-runtime-context-scope) (lambda () scope)))
      (magent-action-forget-project-trust))
    (let ((magent-action-project--allow-prompt nil))
      (magent-action-load-project-scope scope))
    (should-not (magent-action-get "local" scope))
    (should (= magent-context-test--loads 1))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (magent-action-load-project-scope scope))
    (should (= magent-context-test--loads 2))))

(ert-deftest magent-context-project-malformed-trust-never-executes ()
  (magent-context-test--with-project
    (with-temp-file magent-action-project-trust-file
      (insert "{\"project\":true}"))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (ert-fail "Unexpected trust prompt"))))
      (should-error (magent-action-load-project-scope scope)))
    (should (= magent-context-test--loads 0))
    (should-not magent-action--registry)))

(ert-deftest magent-context-project-cleanup-preserves-primary-error ()
  (magent-context-test--with-project
    (let ((notifications 0) logged)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'magent-log)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) logged))))
        (let ((magent-action-registry-changed-hook
               (list (lambda ()
                       (error "Notification %d" (cl-incf notifications))))))
          (should (string-match-p
                   "Notification 1"
                   (error-message-string
                    (should-error (magent-action-load-project-scope scope)))))))
      (should (string-match-p "Notification 2" (car logged)))
      (should-not (magent-action-get "local" scope))
      (should-not (gethash scope magent-action-project--loaded)))))

(provide 'magent-action-context-test)
;;; magent-action-context-test.el ends here
