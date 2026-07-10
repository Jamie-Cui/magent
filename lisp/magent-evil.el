;;; magent-evil.el --- Evil integration for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai

;;; Commentary:

;; Optional Evil integration for Magent.  This file is intentionally not
;; loaded by `magent' so non-Evil users keep the default Emacs bindings.

;;; Code:

(require 'magent-ui)

(declare-function evil-define-key* "evil-core")
(declare-function evil-insert-state "evil-states")
(declare-function evil-local-mode "evil-core")
(declare-function evil-normal-state "evil-states")
(declare-function evil-visual-state-p "evil-states")

(defvar evil-mode)
(defvar evil-mode-hook)
(defvar evil-input-method)
(defvar evil-local-mode)
(defvar evil-move-beyond-eol)

(defvar magent-evil--enabled nil
  "Non-nil when `magent-evil-mode' is enabled.")

(defun magent-evil--setup-keys ()
  "Install Evil bindings for `magent-output-mode'."
  (when (and magent-evil--enabled
             (fboundp 'evil-define-key*))
    (evil-define-key* 'normal magent-output-mode-map
      (kbd "C-g") nil)
    (evil-define-key* '(insert replace visual motion operator emacs)
        magent-output-mode-map
      (kbd "C-g") nil)
    (evil-define-key* 'normal magent-output-mode-map
      (kbd "?") #'magent-evil--menu)
    (evil-define-key* 'normal magent-output-mode-map
      (kbd "C-c C-c") #'magent-ui-submit-or-interrupt)
    (evil-define-key* 'normal magent-output-mode-map
      (kbd "i") #'magent-ui-compose-from-output)))

(defun magent-evil--unset-keys ()
  "Remove Evil-specific Magent bindings when possible."
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal magent-output-mode-map
      (kbd "?") nil)
    (evil-define-key* 'normal magent-output-mode-map
      (kbd "i") nil)
    (evil-define-key* 'normal magent-output-mode-map
      (kbd "C-g") nil)
    (evil-define-key* '(insert replace visual motion operator emacs)
        magent-output-mode-map
      (kbd "C-g") nil)
    (evil-define-key* 'normal magent-output-mode-map
      (kbd "C-c C-c") nil)))

(defun magent-evil--menu ()
  "Open the Magent menu in Evil normal state."
  (interactive)
  (if magent-evil--enabled
      (call-interactively #'magent-transient-menu)
    (magent-ui-menu-or-insert-question-mark)))

(defun magent-evil--output-mode-setup ()
  "Configure buffer-local Evil behavior for Magent output buffers."
  (when magent-evil--enabled
    (setq-local evil-move-beyond-eol t)))

(defun magent-evil--reset-input-method-state ()
  "Clear Evil's input method state for Magent compose buffers.
Deactivates any active input method (such as rime) and clears Evil's
buffer-local `evil-input-method' so it is not restored the next time
the compose buffer re-enters insert state."
  (when magent-evil-reset-input-method-after-submit
    (when current-input-method
      (deactivate-input-method))
    (when (boundp 'evil-input-method)
      (setq evil-input-method nil))))

(defun magent-evil--input-submit ()
  "Return to Evil normal state after Magent input submission."
  (when (and magent-evil--enabled
             (bound-and-true-p evil-local-mode)
             (fboundp 'evil-normal-state))
    ;; Reset the input method BEFORE switching state.  `evil-normal-state'
    ;; runs `evil-insert-state-exit-hook', which saves the live input
    ;; method into `evil-input-method' for later restore; clearing it
    ;; afterwards races that logic.  Deactivating first means Evil has no
    ;; active input method to remember.
    (magent-evil--reset-input-method-state)
    (evil-normal-state)
    ;; Evil may repopulate `evil-input-method' from the pre-exit value
    ;; while entering normal state; clear the saved copy once more so the
    ;; next insert state does not restore it.
    (when (and magent-evil-reset-input-method-after-submit
               (boundp 'evil-input-method))
      (setq evil-input-method nil))))

(defun magent-evil--dwim ()
  "Enter Evil insert state after `magent-dwim' opens the input area."
  (when (and magent-evil--enabled
             (bound-and-true-p evil-mode)
             (fboundp 'evil-insert-state))
    (evil-insert-state)))

(defun magent-evil--region-active-p ()
  "Return non-nil when Evil visual state marks an active region."
  (and magent-evil--enabled
       (bound-and-true-p evil-local-mode)
       (fboundp 'evil-visual-state-p)
       (evil-visual-state-p)))

;;;###autoload
(define-minor-mode magent-evil-mode
  "Toggle optional Evil integration for Magent.
When enabled, Magent adds Evil-specific state handling and output-buffer
bindings.  Loading `magent' alone does not enable this mode."
  :global t
  :group 'magent
  (setq magent-evil--enabled magent-evil-mode)
  (if magent-evil-mode
      (progn
        (add-hook 'evil-mode-hook #'magent-evil--setup-keys)
        (when (featurep 'evil)
          (magent-evil--setup-keys))
        (add-hook 'magent-output-mode-hook #'magent-evil--output-mode-setup)
        (add-hook 'magent-ui-after-input-submit-hook #'magent-evil--input-submit)
        (add-hook 'magent-dwim-hook #'magent-evil--dwim)
        (add-hook 'magent-ui-region-active-functions
                 #'magent-evil--region-active-p))
    (remove-hook 'magent-output-mode-hook #'magent-evil--output-mode-setup)
    (remove-hook 'magent-ui-after-input-submit-hook #'magent-evil--input-submit)
    (remove-hook 'magent-dwim-hook #'magent-evil--dwim)
    (remove-hook 'evil-mode-hook #'magent-evil--setup-keys)
    (remove-hook 'magent-ui-region-active-functions
                 #'magent-evil--region-active-p)
    (when (featurep 'evil)
      (magent-evil--unset-keys))))

;;;###autoload
(defun magent-evil-setup ()
  "Enable optional Evil integration for Magent."
  (interactive)
  (magent-evil-mode 1))

(when (and magent-evil--enabled
           (featurep 'evil))
  (magent-evil--setup-keys))

(provide 'magent-evil)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; no-byte-compile: nil
;; no-native-compile: nil
;; End:

;;; magent-evil.el ends here
