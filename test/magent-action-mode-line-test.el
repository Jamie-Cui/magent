;;; magent-action-mode-line-test.el --- Tests for Action mode line  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for global Action counts, result retention, display, tooltip
;; formatting, and the optional global minor mode.

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
      '("Magent Actions — Running: 2, Failed: 0, Completed: 0"
        "alpha — Write message — /tmp/alpha/"
        "beta — Read output — /tmp/beta/")))))

(ert-deftest magent-action-mode-line-prunes-cancelled-invocations ()
  (let* ((magent-action-mode-line--invocations
          (make-hash-table :test #'eq))
         (active
          (magent-action-mode-line-test--invocation
           "1" "active" nil "/tmp/active/"))
         (cancelled
          (magent-action-mode-line-test--invocation
           "2" "cancelled" nil "/tmp/cancelled/" 'cancelled)))
    (puthash active t magent-action-mode-line--invocations)
    (puthash cancelled t magent-action-mode-line--invocations)
    (should (equal (magent-action-mode-line--invocations-with-status 'active)
                   (list active)))
    (should-not (gethash cancelled magent-action-mode-line--invocations))))

(ert-deftest magent-action-mode-line-render-shows-count-and-dynamic-help ()
  (let ((magent-action-mode-line--invocations (make-hash-table :test #'eq)))
    (puthash
     (magent-action-mode-line-test--invocation
      "1" "commit" "Generate" "/tmp/repo/")
     t magent-action-mode-line--invocations)
    (let ((segment (magent-action-mode-line--render)))
      (should (equal (substring-no-properties segment) " (M: 1, 0, 0) "))
      (should (eq (get-text-property 5 'face segment)
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

(ert-deftest magent-action-mode-line-styles-each-global-count-without-clicks ()
  "Only the three counts receive status faces; the segment is display-only."
  (let ((magent-action-mode-line--invocations (make-hash-table :test #'eq))
        (magent-action-mode-line-label "M"))
    (dolist (state '(active failed completed))
      (magent-action-mode-line--track
       (magent-action-mode-line-test--invocation
        (symbol-name state) "task" nil
        (format "/tmp/%s/" state) state)))
    (let* ((default-directory "/tmp/unrelated-project/")
          (segment (magent-action-mode-line--render)))
      (should (equal (substring-no-properties segment) " (M: 1, 1, 1) "))
      (cl-loop for index in '(5 8 11)
               for face in '(magent-action-mode-line-active-face
                             magent-action-mode-line-failed-face
                             magent-action-mode-line-completed-face)
               do (should (eq (get-text-property index 'face segment) face)))
      (dolist (index '(0 1 2 3 4 6 7 9 10 12 13))
        (should-not (get-text-property index 'face segment)))
      (dotimes (index (length segment))
        (should-not (get-text-property index 'local-map segment))
        (should-not (get-text-property index 'keymap segment))
        (should-not (get-text-property index 'mouse-face segment)))
      (should (string-match-p "Running: 1, Failed: 1, Completed: 1"
                              (magent-action-mode-line--tooltip))))))

(ert-deftest magent-action-mode-line-keeps-zero-counts-visible-and-dimmed ()
  "An idle mode line retains all three zero counts and their hover legend."
  (let ((magent-action-mode-line--invocations (make-hash-table :test #'eq))
        (magent-action-mode-line-label "M"))
    (let ((segment (magent-action-mode-line--render)))
      (should (equal (substring-no-properties segment) " (M: 0, 0, 0) "))
      (dolist (index '(5 8 11))
        (should (eq (get-text-property index 'face segment)
                    'magent-action-mode-line-idle-face)))
      (should (equal (magent-action-mode-line--tooltip)
                     "Magent Actions — Running: 0, Failed: 0, Completed: 0")))))

(ert-deftest magent-action-mode-line-clear-results-keeps-running-actions ()
  "Clear both terminal counts together, then count later results only once."
  (let* ((magent-action-mode-line--invocations (make-hash-table :test #'eq))
         (magent-action-mode-line-label "M")
         (running (magent-action-mode-line-test--invocation "1" "running"))
         (failed (magent-action-mode-line-test--invocation
                  "2" "failed" nil nil 'failed))
         (completed (magent-action-mode-line-test--invocation
                     "3" "completed" nil nil 'completed)))
    (mapc #'magent-action-mode-line--track (list running failed completed))
    (magent-action-mode-line-clear-results)
    (should (equal (substring-no-properties (magent-action-mode-line--render))
                   " (M: 1, 0, 0) "))
    (should (= (hash-table-count magent-action-mode-line--invocations) 1))
    (setf (magent-action-invocation-status running) 'completed)
    (magent-action-mode-line--track running)
    (magent-action-mode-line--track running)
    (should (equal (substring-no-properties (magent-action-mode-line--render))
                   " (M: 0, 0, 1) "))
    (magent-action-mode-line-clear-results)
    (magent-action-mode-line-clear-results)
    (should (= (hash-table-count magent-action-mode-line--invocations) 0))))

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
      (should (equal (magent-action-mode-line--invocations-with-status 'completed)
                     (list invocation)))
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
    (should (equal (magent-action-mode-line--invocations-with-status 'completed)
                   (list invocation)))))

(ert-deftest magent-action-mode-line-retains-failures-until-acknowledged ()
  "Async failures remain visible alongside active work until cleared."
  (let* ((magent-action-mode-line--invocations (make-hash-table :test #'eq))
         (failed (magent-action-mode-line-test--invocation
                  "1" "broken" "Generate" "/tmp/repo/"))
         (active (magent-action-mode-line-test--invocation "2" "running"))
         completion outcome)
    (magent-action-mode-line--track active)
    (magent-action-mode-line--call-with-tracking
     (lambda (_action &rest args)
       (setq completion (plist-get args :on-complete))
       failed)
     '("broken")
     (list :on-complete (lambda (status _result) (setq outcome status))))
    (setf (magent-action-invocation-status failed) 'failed
          (magent-action-invocation-result failed)
          (magent-execution-result-failed "Provider\nfailed"))
    (funcall completion 'failed (magent-action-invocation-result failed))
    (should (eq outcome 'failed))
    (let ((segment (magent-action-mode-line--render)))
      (should (equal (substring-no-properties segment) " (M: 1, 1, 0) "))
      (should (eq (get-text-property 8 'face segment)
                  'magent-action-mode-line-failed-face)))
    (should (string-match-p "broken — Failed: Provider failed"
                            (magent-action-mode-line--tooltip)))
    (magent-action-mode-line-clear-results)
    (should (equal (magent-action-mode-line--invocations-with-status 'active)
                   (list active)))
    (should-not (magent-action-mode-line--invocations-with-status 'failed))))

(ert-deftest magent-action-mode-line-retains-synchronous-failure ()
  "A startup failure is tracked even before the invocation is returned."
  (let* ((magent-action-mode-line--invocations (make-hash-table :test #'eq))
         (invocation (magent-action-mode-line-test--invocation "1" "broken")))
    (magent-action-mode-line--call-with-tracking
     (lambda (_action &rest args)
       (setf (magent-action-invocation-status invocation) 'failed
             (magent-action-invocation-result invocation)
             (magent-execution-result-failed "Startup failed"))
       (funcall (plist-get args :on-complete)
                'failed (magent-action-invocation-result invocation))
       invocation)
     '("broken") nil)
    (should (equal (magent-action-mode-line--invocations-with-status 'failed)
                   (list invocation)))
    (should (string-match-p "(M: 0, 1, 0)"
                            (magent-action-mode-line--render)))))

(ert-deftest magent-action-mode-line-disabled-callback-does-not-retain-failure ()
  "Disabling tracking invalidates callbacks without losing user completion."
  (let* ((magent-action-mode-line--invocations (make-hash-table :test #'eq))
         (global-mode-string nil)
         (invocation (magent-action-mode-line-test--invocation "1" "broken"))
         completion outcome)
    (magent-action-mode-line--call-with-tracking
     (lambda (_action &rest args)
       (setq completion (plist-get args :on-complete))
       invocation)
     '("broken")
     (list :on-complete (lambda (status _result) (setq outcome status))))
    (magent-action-mode-line--disable)
    (setf (magent-action-invocation-status invocation) 'failed)
    (funcall completion 'failed "Failure")
    (should (eq outcome 'failed))
    (should (= (hash-table-count magent-action-mode-line--invocations) 0))))

(provide 'magent-action-mode-line-test)
;;; magent-action-mode-line-test.el ends here
