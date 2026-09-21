;;; magent-action-session-view.el --- Inspect isolated Action sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Read-only inspection UI for durable isolated Action sessions.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-fold)
(require 'subr-x)
(require 'magent-ledger)
(require 'magent-session)

(defun magent-action-session--format-value (value)
  "Return display string for VALUE."
  (cond
   ((null value) "")
   ((vectorp value)
    (mapconcat #'magent-action-session--format-value (append value nil) ", "))
   ((listp value)
    (format "%S" value))
   (t
    (format "%s" value))))

(defvar-local magent-action-session--details-hidden t
  "Whether the current action session viewer hides activity details.")

(defvar magent-action-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'magent-action-session-toggle-section)
    (define-key map (kbd "<backtab>") #'magent-action-session-toggle-all)
    (define-key map (kbd "S-TAB") #'magent-action-session-toggle-all)
    map)
  "Keymap for `magent-action-session-mode'.")

(set-keymap-parent magent-action-session-mode-map org-mode-map)
(define-key magent-action-session-mode-map (kbd "q") #'quit-window)

(define-derived-mode magent-action-session-mode org-mode "Magent-Action"
  "Read-only Org viewer for progressively disclosed Action sessions."
  (setq-local org-src-fontify-natively t)
  (setq-local buffer-read-only t))

(defun magent-action-session-toggle-section ()
  "Toggle the outline section at or above point."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-cycle)))

(defun magent-action-session-toggle-all ()
  "Toggle all detail sections in the current session viewer."
  (interactive)
  (if magent-action-session--details-hidden
      (progn
        (org-fold-show-all)
        (setq magent-action-session--details-hidden nil))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* Activity$" nil t)
        (beginning-of-line)
        (org-fold-hide-subtree)))
    (setq magent-action-session--details-hidden t)))

(defun magent-action-session--shift-headings (text levels)
  "Shift Org-style headings in TEXT by LEVELS for display."
  (replace-regexp-in-string
   "^\\*+ "
   (lambda (heading)
     (concat (make-string levels ?*) heading))
   (or text "")))

(defun magent-action-session--item-metadata-value (item key)
  "Return ITEM metadata value for KEY from a plist or loaded alist."
  (let ((metadata (magent-thread-item-metadata item)))
    (if (and (listp metadata) (keywordp (car metadata)))
        (plist-get metadata key)
      (or (alist-get (intern (substring (symbol-name key) 1)) metadata)
          (alist-get key metadata)))))

(defun magent-action-session--final-item-p (item)
  "Return non-nil when ITEM is an isolated action final result."
  (and (eq (magent-thread-item-type item) 'message)
       (eq (magent-thread-item-role item) 'assistant)
       (member (format "%s"
                       (magent-action-session--item-metadata-value item :source))
               '("magent-action-final" "magent-doctor-final"))))

(defun magent-action-session--insert-item (item &optional level)
  "Insert ledger ITEM into current buffer at outline LEVEL."
  (let ((prefix (make-string (or level 2) ?*)))
  (pcase (magent-thread-item-type item)
    ('message
       (insert (format "%s Message: %s\n"
                       prefix
                       (upcase (symbol-name
                                (or (magent-thread-item-role item)
                                    'message)))))
       (insert (magent-action-session--shift-headings
                (or (magent-thread-item-content item) "")
                (1+ (or level 2)))
               "\n\n"))
    ('tool
       (insert (format "%s Step: %s [%s]\n"
                       prefix
                       (or (magent-thread-item-name item) "tool")
                       (magent-action-session--format-value
                        (magent-thread-item-status item))))
       (when-let* ((input (magent-thread-item-input item)))
         (insert "Input: " (magent-action-session--format-value input) "\n"))
       (insert (or (magent-thread-item-output item)
                   (magent-thread-item-error item)
                   "")
               "\n\n"))
    ('workflow-step
       (insert (format "%s Workflow Step: %s [%s]\n"
                       prefix
                       (or (magent-thread-item-name item) "step")
                       (magent-action-session--format-value
                        (magent-thread-item-status item))))
       (when-let* ((input (magent-thread-item-input item)))
         (insert "Input: " (magent-action-session--format-value input) "\n"))
       (when-let* ((output (or (magent-thread-item-output item)
                               (magent-thread-item-error item))))
         (insert output "\n"))
       (insert "\n"))
    (_
       (insert (format "%s %s [%s]\n%s\n\n"
                       prefix
                       (upcase (symbol-name
                                (magent-thread-item-type item)))
                       (magent-action-session--format-value
                        (magent-thread-item-status item))
                       (or (magent-thread-item-content item)
                           (magent-thread-item-output item)
                           "")))))))

(defun magent-action-session--thread-items (thread)
  "Return all THREAD items in chronological order."
  (apply #'append
         (mapcar #'magent-thread-turn-items
                 (magent-thread-turns thread))))

(defun magent-action-session--result-items (items)
  "Return the final result items selected from ITEMS."
  (or (cl-remove-if-not #'magent-action-session--final-item-p items)
      (last (cl-remove-if-not
             (lambda (item)
               (and (eq (magent-thread-item-type item) 'message)
                    (eq (magent-thread-item-role item) 'assistant)))
             items))))

(defun magent-action-session--session-label (file)
  "Return a unique completion label for action session FILE."
  (let* ((meta (magent-session--read-file-metadata-cached file))
         (time (magent-session--format-display-timestamp file))
         (action (or (plist-get meta :action) "unknown"))
         (status (or (plist-get meta :status) "unknown"))
         (id (file-name-base file))
         (title (or (plist-get meta :title)
                    (plist-get meta :summary-title)
                    id)))
    (format "%s  [%s]  %s  %s  <%s>"
            time status action title id)))

(defun magent-action-open-session (file)
  "Open read-only viewer for isolated action session FILE.
Interactively, offer sessions by last update time, newest first."
  (interactive
   (let ((files (magent-session-list-action-files)))
     (unless files
       (user-error "Magent: no isolated action sessions found"))
     (let* ((choices (mapcar (lambda (file)
                               (cons (magent-action-session--session-label file)
                                     file))
                             files))
            (table (lambda (string predicate action)
                     (if (eq action 'metadata)
                         '(metadata
                           (display-sort-function . identity)
                           (cycle-sort-function . identity))
                       (complete-with-action action choices string predicate))))
            (selected (completing-read "Action session: " table nil t)))
       (list (cdr (assoc selected choices))))))
  (unless (and file (file-exists-p file))
    (user-error "Magent: action session file not found"))
  (let* ((loaded (magent-session-read-file file))
         (session (plist-get loaded :session))
         (meta (magent-session--read-file-metadata-cached file))
         (thread (and session (magent-session-thread-ledger session)))
         (items (and thread (magent-action-session--thread-items thread)))
         (result-items (and items (magent-action-session--result-items items)))
         (buffer (get-buffer-create
                  (format "*Magent Action Session: %s*"
                          (file-name-base file)))))
    (with-current-buffer buffer
      (magent-action-session-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Magent Action Session\n\n")
        (insert (format "Action: %s\n" (or (plist-get meta :action) "")))
        (insert (format "Status: %s\n" (or (plist-get meta :status) "")))
        (insert (format "Title: %s\n" (or (plist-get meta :title) "")))
        (insert (format "Scope: %s\n"
                        (or (plist-get meta :project-root)
                            (plist-get meta :origin-scope)
                            (plist-get meta :scope)
                            "")))
        (when-let* ((parent (plist-get meta :parent-session-id)))
          (insert (format "Parent session: %s\n" parent)))
        (insert (format "File: %s\n\n" file))
        (insert "* Result\n")
        (if result-items
            (dolist (item result-items)
              (insert (magent-action-session--shift-headings
                       (or (magent-thread-item-content item) "")
                       1)
                      "\n\n"))
          (insert "No final result recorded.\n\n"))
        (insert "* Activity\n")
        (if (and thread (magent-thread-turns thread))
            (dolist (turn (magent-thread-turns thread))
              (insert (format "** Turn %s [%s]\n\n"
                              (magent-thread-turn-id turn)
                              (magent-thread-turn-status turn)))
              (dolist (item (magent-thread-turn-items turn))
                (unless (memq item result-items)
                  (magent-action-session--insert-item item 3))))
          (insert "No transcript items.\n"))
        (add-text-properties (point-min) (point-max) '(read-only t))
        (goto-char (point-min))
        (when (re-search-forward "^\\* Activity$" nil t)
          (beginning-of-line)
          (org-fold-hide-subtree)
          (setq magent-action-session--details-hidden t))
        (goto-char (point-min))))
    (display-buffer buffer)))

;;;###autoload
(defun magent-action-list-sessions ()
  "List and inspect Magent action sessions."
  (interactive)
  (call-interactively #'magent-action-open-session))

(provide 'magent-action-session-view)
;;; magent-action-session-view.el ends here
