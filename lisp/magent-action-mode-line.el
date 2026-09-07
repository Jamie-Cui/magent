;;; magent-action-mode-line.el --- Show active Magent Actions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Display the number of active Magent Actions in `global-mode-string'.
;; Hovering over the segment shows one line per Action with its current Step
;; and originating directory.  Public Action entry points are advised so the
;; package can observe both interactive and slash-exposed Actions without
;; depending on Magent's private invocation registries.
;;
;; Customize `magent-action-mode-line-mode' to enable this optional UI.

;;; Code:

(require 'cl-lib)
(require 'magent-action)
(require 'magent-action-session)
(require 'subr-x)

(defgroup magent-action-mode-line nil
  "Mode-line status for active Magent Actions."
  :group 'magent)

(defcustom magent-action-mode-line-label "Magent"
  "Label displayed before the active Action count."
  :type 'string
  :group 'magent-action-mode-line)

(defface magent-action-mode-line-active-face
  '((t (:inherit mode-line-emphasis :weight bold)))
  "Face used when at least one Magent Action is active."
  :group 'magent-action-mode-line)

(defface magent-action-mode-line-idle-face
  '((t (:inherit shadow)))
  "Face used when no Magent Action is active."
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
  "Track active Magent Action INVOCATION and refresh the mode line."
  (when (magent-action-invocation-p invocation)
    (if (eq (magent-action-invocation-status invocation) 'active)
        (puthash invocation t magent-action-mode-line--invocations)
      (remhash invocation magent-action-mode-line--invocations)))
  (magent-action-mode-line--refresh)
  invocation)

(defun magent-action-mode-line--active-invocations ()
  "Return tracked active invocations and discard terminal entries."
  (let (active stale)
    (maphash
     (lambda (invocation _present)
       (if (and (magent-action-invocation-p invocation)
                (eq (magent-action-invocation-status invocation) 'active))
           (push invocation active)
         (push invocation stale)))
     magent-action-mode-line--invocations)
    (dolist (invocation stale)
      (remhash invocation magent-action-mode-line--invocations))
    (sort active
          (lambda (left right)
            (string< (format "%s" (magent-action-invocation-id left))
                     (format "%s" (magent-action-invocation-id right)))))))

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
          (if (magent-action-step-p step)
              (or (magent-action-step-name step) "Starting")
            "Starting"))
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
  "Return one line per active Magent Action for the mode-line tooltip."
  (let ((invocations (magent-action-mode-line--active-invocations)))
    (if invocations
        (mapconcat #'magent-action-mode-line--task-line invocations "\n")
      "No active Magent Actions")))

(defun magent-action-mode-line--help-echo (_window _object _position)
  "Return current Action details for a mode-line help request."
  (magent-action-mode-line--tooltip))

(defun magent-action-mode-line--render ()
  "Return the propertized Magent Action mode-line segment."
  (let* ((count (length (magent-action-mode-line--active-invocations)))
         (face (if (> count 0)
                   'magent-action-mode-line-active-face
                 'magent-action-mode-line-idle-face)))
    (propertize
     (format " %s:%d " magent-action-mode-line-label count)
     'face face
     'mouse-face 'mode-line-highlight
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
        invocation)
    (when (and original-completion (not (functionp original-completion)))
      (signal 'wrong-type-argument (list 'functionp original-completion)))
    (setq keyword-arguments
          (plist-put
           keyword-arguments :on-complete
           (lambda (status result)
             (when (magent-action-invocation-p invocation)
               (remhash invocation magent-action-mode-line--invocations))
             (magent-action-mode-line--refresh)
             (when original-completion
               (funcall original-completion status result)))))
    (setq invocation
          (apply function (append positional-arguments keyword-arguments)))
    (magent-action-mode-line--track invocation)))

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
  (clrhash magent-action-mode-line--invocations)
  (magent-action-mode-line--refresh))

;;;###autoload
(define-minor-mode magent-action-mode-line-mode
  "Show active Magent Action count and details in the mode line."
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
