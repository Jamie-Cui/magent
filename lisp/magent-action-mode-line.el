;;; magent-action-mode-line.el --- Show Magent Action status counts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Display global running, failed, and completed Action counts as (M: 0, 0, 0)
;; in `global-mode-string'.  Finished results accumulate until manually
;; cleared, the mode is disabled, or Emacs exits.  Hovering shows current Steps,
;; failures, and originating directories.  Public Action entry points are
;; advised without depending on Magent's private invocation registries.
;;
;; Customize `magent-action-mode-line-mode' to enable this optional UI.

;;; Code:

(require 'cl-lib)
(require 'magent-action)
(require 'magent-action-session)
(require 'subr-x)

(defgroup magent-action-mode-line nil
  "Mode-line status counts for Magent Actions."
  :group 'magent)

(defcustom magent-action-mode-line-label "M"
  "Label displayed before the running, failed, and completed Action counts."
  :type 'string
  :group 'magent-action-mode-line)

(defface magent-action-mode-line-active-face
  '((t (:inherit warning :weight bold)))
  "Face used for a nonzero running Action count."
  :group 'magent-action-mode-line)

(defface magent-action-mode-line-idle-face
  '((t (:inherit shadow :weight normal)))
  "Face used for zero Action counts."
  :group 'magent-action-mode-line)

(defface magent-action-mode-line-failed-face
  '((t (:inherit error :weight bold)))
  "Face used for a nonzero failed Action count."
  :group 'magent-action-mode-line)

(defface magent-action-mode-line-completed-face
  '((t (:inherit success :weight normal)))
  "Face used for a nonzero completed Action count."
  :group 'magent-action-mode-line)

(defvar magent-action-mode-line--invocations (make-hash-table :test #'eq)
  "Magent Action invocations observed through the public entry points.")

(defvar magent-action-mode-line--mode-line
  '(:eval (magent-action-mode-line--render))
  "Mode-line construct installed in `global-mode-string'.")

(put 'magent-action-mode-line--mode-line 'risky-local-variable t)

(defun magent-action-mode-line--refresh ()
  "Refresh every mode line after Action state changes."
  (force-mode-line-update t))

(defun magent-action-mode-line--track (invocation)
  "Track active, failed, or completed INVOCATION and refresh the mode line."
  (when (magent-action-invocation-p invocation)
    (if (memq (magent-action-invocation-status invocation)
              '(active failed completed))
        (puthash invocation t magent-action-mode-line--invocations)
      (remhash invocation magent-action-mode-line--invocations)))
  (magent-action-mode-line--refresh)
  invocation)

(defun magent-action-mode-line--invocations-with-status (status)
  "Return tracked invocations with STATUS, pruning cancelled entries.
Failed and completed invocations remain until their counts are cleared."
  (let (matches stale)
    (maphash
     (lambda (invocation _present)
       (if (and (magent-action-invocation-p invocation)
                (memq (magent-action-invocation-status invocation)
                      '(active failed completed)))
           (when (eq (magent-action-invocation-status invocation) status)
             (push invocation matches))
         (push invocation stale)))
     magent-action-mode-line--invocations)
    (dolist (invocation stale)
      (remhash invocation magent-action-mode-line--invocations))
    (sort matches
          (lambda (left right)
            (string< (format "%s" (magent-action-invocation-id left))
                     (format "%s" (magent-action-invocation-id right)))))))

(defun magent-action-mode-line-results-p ()
  "Return non-nil when failed or completed Action counts can be cleared."
  (or (magent-action-mode-line--invocations-with-status 'failed)
      (magent-action-mode-line--invocations-with-status 'completed)))

;;;###autoload
(defun magent-action-mode-line-clear-results ()
  "Clear failed and completed Action counts, keeping running Actions.
Saved Action sessions are unaffected."
  (interactive)
  (dolist (invocation
           (append (magent-action-mode-line--invocations-with-status 'failed)
                   (magent-action-mode-line--invocations-with-status 'completed)))
    (remhash invocation magent-action-mode-line--invocations))
  (magent-action-mode-line--refresh))

(defun magent-action-mode-line--one-line (value)
  "Return VALUE as a trimmed string without embedded whitespace runs."
  (string-trim
   (replace-regexp-in-string "[[:space:]\n\r]+" " " (format "%s" value))))

(defun magent-action-mode-line--task-line (invocation)
  "Return one tooltip line describing Action INVOCATION."
  (unless (magent-action-invocation-p invocation)
    (signal 'wrong-type-argument
            (list 'magent-action-invocation-p invocation)))
  (let* ((spec (magent-action-invocation-spec invocation))
         (step (magent-action-invocation-current-step invocation))
         (name
          (if (magent-action-spec-p spec)
              (magent-action-spec-name spec)
            "unknown-action"))
         (step-name
          (if (eq (magent-action-invocation-status invocation) 'failed)
              (let ((result (magent-action-invocation-result invocation)))
                (concat "Failed: "
                        (if (magent-execution-result-p result)
                            (magent-execution-result-content-string result)
                          "Unknown error")))
            (if (magent-action-step-p step)
                (or (magent-action-step-name step) "Starting")
              "Starting")))
         (origin
          (or (magent-action-invocation-origin-directory invocation)
              (magent-action-invocation-origin-scope invocation)
              "unknown origin")))
    (format "%s — %s — %s"
            (magent-action-mode-line--one-line name)
            (magent-action-mode-line--one-line step-name)
            (magent-action-mode-line--one-line
             (if (stringp origin)
                 (abbreviate-file-name origin)
               origin)))))

(defun magent-action-mode-line--tooltip ()
  "Return labeled counts, running Actions, and failures for the tooltip."
  (let* ((active (magent-action-mode-line--invocations-with-status 'active))
         (failed (magent-action-mode-line--invocations-with-status 'failed))
         (completed (magent-action-mode-line--invocations-with-status 'completed))
         (details (append active failed)))
    (concat
     (format "Magent Actions — Running: %d, Failed: %d, Completed: %d"
             (length active) (length failed) (length completed))
     (when details
       (concat "\n" (mapconcat #'magent-action-mode-line--task-line
                               details "\n"))))))

(defun magent-action-mode-line--help-echo (_window _object _position)
  "Return current Action details for a mode-line help request."
  (magent-action-mode-line--tooltip))

(defun magent-action-mode-line--count (status face)
  "Return the count for STATUS styled with FACE, or dimmed when zero."
  (let ((count (length (magent-action-mode-line--invocations-with-status status))))
    (propertize (number-to-string count) 'face
                (if (zerop count) 'magent-action-mode-line-idle-face face))))

(defun magent-action-mode-line--render ()
  "Return three individually styled Action counts with hover help."
  (let ((counts
         (list (magent-action-mode-line--count
                'active 'magent-action-mode-line-active-face)
               (magent-action-mode-line--count
                'failed 'magent-action-mode-line-failed-face)
               (magent-action-mode-line--count
                'completed 'magent-action-mode-line-completed-face))))
    (propertize
     (concat " (" magent-action-mode-line-label ": "
             (string-join counts ", ") ") ")
     'help-echo #'magent-action-mode-line--help-echo)))

(defun magent-action-mode-line--install-segment ()
  "Install a valid Magent Action segment in `global-mode-string'."
  ;; A mode-line list whose first element is a symbol is parsed as a
  ;; conditional construct.  Keep the conventional string prefix so a lone
  ;; Magent entry is instead parsed as a sequence of mode-line constructs.
  (setq global-mode-string
        (cond
         ((null global-mode-string) '(""))
         ((not (listp global-mode-string))
          (list "" global-mode-string))
         ((stringp (car global-mode-string)) global-mode-string)
         (t (cons "" global-mode-string))))
  (add-to-list 'global-mode-string 'magent-action-mode-line--mode-line t))

(defun magent-action-mode-line--call-with-tracking
    (function positional-arguments keyword-arguments)
  "Call FUNCTION with POSITIONAL-ARGUMENTS and KEYWORD-ARGUMENTS.
Track the returned invocation and preserve the original `:on-complete'
callback."
  (let ((original-completion (plist-get keyword-arguments :on-complete))
        (tracked-invocations magent-action-mode-line--invocations)
        invocation)
    (when (and original-completion (not (functionp original-completion)))
      (signal 'wrong-type-argument (list 'functionp original-completion)))
    (setq keyword-arguments
          (plist-put
           keyword-arguments :on-complete
           (lambda (status result)
             (when (and (eq tracked-invocations
                            magent-action-mode-line--invocations)
                        (magent-action-invocation-p invocation))
               (magent-action-mode-line--track invocation))
             (magent-action-mode-line--refresh)
             (when original-completion
               (funcall original-completion status result)))))
    (setq invocation
          (apply function (append positional-arguments keyword-arguments)))
    (when (eq tracked-invocations magent-action-mode-line--invocations)
      (magent-action-mode-line--track invocation))
    invocation))

(defun magent-action-mode-line--run-a (function action &rest arguments)
  "Call advised FUNCTION for interactive ACTION with ARGUMENTS."
  (magent-action-mode-line--call-with-tracking
   function (list action) arguments))

(defun magent-action-mode-line--invoke-a
    (function action runtime-session &rest arguments)
  "Call advised FUNCTION for ACTION in RUNTIME-SESSION with ARGUMENTS."
  (magent-action-mode-line--call-with-tracking
   function (list action runtime-session) arguments))

(defun magent-action-mode-line--seed-isolated-invocations ()
  "Track isolated Actions that were already active when the mode enabled."
  (dolist (invocation (magent-action-session-active-invocations))
    (when (magent-action-invocation-p invocation)
      (puthash invocation t magent-action-mode-line--invocations))))

(defun magent-action-mode-line--enable ()
  "Install Action tracking and the global mode-line segment."
  (unless (advice-member-p #'magent-action-mode-line--run-a
                           'magent-action-run)
    (advice-add 'magent-action-run :around #'magent-action-mode-line--run-a))
  (unless (advice-member-p #'magent-action-mode-line--invoke-a
                           'magent-action-invoke)
    (advice-add 'magent-action-invoke
                :around #'magent-action-mode-line--invoke-a))
  (magent-action-mode-line--seed-isolated-invocations)
  (magent-action-mode-line--install-segment)
  (magent-action-mode-line--refresh))

(defun magent-action-mode-line--disable ()
  "Remove Action tracking and the global mode-line segment."
  (advice-remove 'magent-action-run #'magent-action-mode-line--run-a)
  (advice-remove 'magent-action-invoke #'magent-action-mode-line--invoke-a)
  (setq global-mode-string
        (cl-remove 'magent-action-mode-line--mode-line global-mode-string
                   :test #'eq))
  ;; Invalidate callbacks installed before disabling the mode.
  (setq magent-action-mode-line--invocations (make-hash-table :test #'eq))
  (magent-action-mode-line--refresh))

;;;###autoload
(define-minor-mode magent-action-mode-line-mode
  "Show global running, failed, and completed Action counts.
Results accumulate until `magent-action-mode-line-clear-results' is called,
this mode is disabled, or Emacs exits.  Cancelled Actions are not counted."
  :init-value nil
  :global t
  :group 'magent-action-mode-line
  (if magent-action-mode-line-mode
      (magent-action-mode-line--enable)
    (magent-action-mode-line--disable)))

;; Honor a value customized before Magent was loaded.  Customize calls made
;; after this feature loads use `custom-set-minor-mode' and enter the mode body
;; directly.
(when (default-value 'magent-action-mode-line-mode)
  (magent-action-mode-line-mode 1))

(provide 'magent-action-mode-line)
;;; magent-action-mode-line.el ends here
