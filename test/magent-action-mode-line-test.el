;;; magent-action-mode-line-test.el --- Tests for Action mode line  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for active Action tracking, display, tooltip formatting, and the
;; optional global minor mode.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'magent-action-mode-line)

(defun magent-action-mode-line-test--invocation
    (id name &optional step origin status)
  "Create test invocation ID for Action NAME, STEP, ORIGIN, and STATUS."
  (magent-action-invocation-create
   :id id
   :spec (magent-action-spec-create :name name :title name)
   :current-step (and step
                      (magent-action-step-create :type 'callback :name step))
   :origin-directory origin
   :status (or status 'active)))

(ert-deftest magent-action-mode-line-is-an-optional-custom-variable ()
  "The global Action status UI is a disabled-by-default Customize option."
  (should (custom-variable-p 'magent-action-mode-line-mode))
  (should (eq (get 'magent-action-mode-line-mode 'custom-type) 'boolean))
  (should-not
   (eval (car (get 'magent-action-mode-line-mode 'standard-value)) t)))

(ert-deftest magent-action-mode-line-tooltip-lists-one-task-per-line ()
  (let ((magent-action-mode-line--invocations (make-hash-table :test #'eq)))
    (puthash
     (magent-action-mode-line-test--invocation
      "2" "beta" "Read\noutput" "/tmp/beta/")
     t magent-action-mode-line--invocations)
    (puthash
     (magent-action-mode-line-test--invocation
      "1" "alpha" "Write message" "/tmp/alpha/")
     t magent-action-mode-line--invocations)
    (should
     (equal
      (split-string (magent-action-mode-line--tooltip) "\n")
      '("alpha — Write message — /tmp/alpha/"
        "beta — Read output — /tmp/beta/")))))

(ert-deftest magent-action-mode-line-prunes-terminal-invocations ()
  (let* ((magent-action-mode-line--invocations
          (make-hash-table :test #'eq))
         (active
          (magent-action-mode-line-test--invocation
           "1" "active" nil "/tmp/active/"))
         (completed
          (magent-action-mode-line-test--invocation
           "2" "done" nil "/tmp/done/" 'completed)))
    (puthash active t magent-action-mode-line--invocations)
    (puthash completed t magent-action-mode-line--invocations)
    (should (equal (magent-action-mode-line--active-invocations)
                   (list active)))
    (should-not (gethash completed magent-action-mode-line--invocations))))

(ert-deftest magent-action-mode-line-render-shows-count-and-dynamic-help ()
  (let ((magent-action-mode-line--invocations (make-hash-table :test #'eq)))
    (puthash
     (magent-action-mode-line-test--invocation
      "1" "commit" "Generate" "/tmp/repo/")
     t magent-action-mode-line--invocations)
    (let ((segment (magent-action-mode-line--render)))
      (should (equal (substring-no-properties segment) " Magent:1 "))
      (should (eq (get-text-property 1 'face segment)
                  'magent-action-mode-line-active-face))
      (should (eq (get-text-property 1 'help-echo segment)
                  #'magent-action-mode-line--help-echo)))))

(ert-deftest magent-action-mode-line-installs-a-valid-global-sequence ()
  (let ((global-mode-string '(magent-action-mode-line--mode-line))
        (magent-action-mode-line--invocations (make-hash-table :test #'eq)))
    (magent-action-mode-line--install-segment)
    (magent-action-mode-line--install-segment)
    (should
     (equal global-mode-string
            '("" magent-action-mode-line--mode-line)))
    (should (stringp (car global-mode-string)))))

(ert-deftest magent-action-mode-line-custom-setting-toggles-integration ()
  (let ((global-mode-string nil)
        (was-enabled (default-value 'magent-action-mode-line-mode)))
    (unwind-protect
        (progn
          (magent-action-mode-line-mode -1)
          (customize-set-variable 'magent-action-mode-line-mode t)
          (should (default-value 'magent-action-mode-line-mode))
          (should (memq 'magent-action-mode-line--mode-line
                        global-mode-string))
          (should (advice-member-p #'magent-action-mode-line--run-a
                                   'magent-action-run))
          (should (advice-member-p #'magent-action-mode-line--invoke-a
                                   'magent-action-invoke))
          (customize-set-variable 'magent-action-mode-line-mode nil)
          (should-not (default-value 'magent-action-mode-line-mode))
          (should-not (memq 'magent-action-mode-line--mode-line
                            global-mode-string))
          (should-not (advice-member-p #'magent-action-mode-line--run-a
                                       'magent-action-run))
          (should-not (advice-member-p #'magent-action-mode-line--invoke-a
                                       'magent-action-invoke)))
      (magent-action-mode-line-mode (if was-enabled 1 -1)))))

(ert-deftest magent-action-mode-line-preserves-completion-callback ()
  (let* ((magent-action-mode-line--invocations
          (make-hash-table :test #'eq))
         (invocation
          (magent-action-mode-line-test--invocation
           "1" "commit" "Generate" "/tmp/repo/"))
         wrapped-completion
         original-outcome)
    (cl-letf (((symbol-function 'force-mode-line-update) #'ignore))
      (should
       (eq
        (magent-action-mode-line--call-with-tracking
         (lambda (_action &rest arguments)
           (setq wrapped-completion (plist-get arguments :on-complete))
           invocation)
         '("commit")
         (list :on-complete
               (lambda (status result)
                 (setq original-outcome (cons status result)))))
        invocation))
      (should (gethash invocation magent-action-mode-line--invocations))
      (setf (magent-action-invocation-status invocation) 'completed)
      (funcall wrapped-completion 'completed "done")
      (should-not (gethash invocation magent-action-mode-line--invocations))
      (should (equal original-outcome '(completed . "done"))))))

(ert-deftest magent-action-mode-line-handles-synchronous-completion ()
  (let* ((magent-action-mode-line--invocations
          (make-hash-table :test #'eq))
         (invocation
          (magent-action-mode-line-test--invocation
           "1" "instant" nil "/tmp/repo/")))
    (cl-letf (((symbol-function 'force-mode-line-update) #'ignore))
      (magent-action-mode-line--call-with-tracking
       (lambda (_action &rest arguments)
         (setf (magent-action-invocation-status invocation) 'completed)
         (funcall (plist-get arguments :on-complete) 'completed "done")
         invocation)
       '("instant") nil))
    (should-not (gethash invocation magent-action-mode-line--invocations))))

(provide 'magent-action-mode-line-test)
;;; magent-action-mode-line-test.el ends here
