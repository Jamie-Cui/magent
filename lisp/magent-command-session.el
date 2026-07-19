;;; magent-command-session.el --- Durable isolated command sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6

;;; Commentary:

;; Persistence, ledger projection, inspection, and cancellation support for
;; `magent-command' invocations whose session policy is `isolated'.

;;; Code:

(require 'cl-lib)
(require 'outline)
(require 'subr-x)
(require 'magent-command)
(require 'magent-ledger)
(require 'magent-protocol)
(require 'magent-session)

(declare-function magent-runtime-session-create "magent-runtime-api")

(defun magent-command-session--format-value (value)
  "Return display string for VALUE."
  (cond
   ((null value) "")
   ((vectorp value)
    (mapconcat #'magent-command-session--format-value (append value nil) ", "))
   ((listp value)
    (format "%S" value))
   (t
    (format "%s" value))))

(defvar-local magent-command-session--details-hidden t
  "Whether the current command session viewer hides activity details.")

(defvar magent-command-session-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'magent-command-session-toggle-section)
    (define-key map (kbd "<backtab>") #'magent-command-session-toggle-all)
    (define-key map (kbd "S-TAB") #'magent-command-session-toggle-all)
    map)
  "Keymap for `magent-command-session-mode'.")

(define-derived-mode magent-command-session-mode special-mode "Magent-Command"
  "Major mode for progressively disclosed Magent command sessions."
  (setq-local outline-regexp "\\*+ ")
  (setq-local outline-level
              (lambda () (1- (- (match-end 0) (match-beginning 0)))))
  (outline-minor-mode 1))

(defun magent-command-session-toggle-section ()
  "Toggle the outline section at or above point."
  (interactive)
  (save-excursion
    (unless (looking-at outline-regexp)
      (outline-back-to-heading t))
    (outline-toggle-children)))

(defun magent-command-session-toggle-all ()
  "Toggle all detail sections in the current session viewer."
  (interactive)
  (if magent-command-session--details-hidden
      (progn
        (outline-show-all)
        (setq magent-command-session--details-hidden nil))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* Activity$" nil t)
        (beginning-of-line)
        (outline-hide-subtree)))
    (setq magent-command-session--details-hidden t)))

(defun magent-command-session--shift-headings (text levels)
  "Shift Org-style headings in TEXT by LEVELS for display."
  (replace-regexp-in-string
   "^\\*+ "
   (lambda (heading)
     (concat (make-string levels ?*) heading))
   (or text "")))

(defun magent-command-session--item-metadata-value (item key)
  "Return ITEM metadata value for KEY from a plist or loaded alist."
  (let ((metadata (magent-thread-item-metadata item)))
    (if (and (listp metadata) (keywordp (car metadata)))
        (plist-get metadata key)
      (or (alist-get (intern (substring (symbol-name key) 1)) metadata)
          (alist-get key metadata)))))

(defun magent-command-session--final-item-p (item)
  "Return non-nil when ITEM is an isolated command final result."
  (and (eq (magent-thread-item-type item) 'message)
       (eq (magent-thread-item-role item) 'assistant)
       (member (format "%s"
                       (magent-command-session--item-metadata-value item :source))
               '("magent-command-final" "magent-doctor-final"))))

(defun magent-command-session--insert-item (item &optional level)
  "Insert ledger ITEM into current buffer at outline LEVEL."
  (let ((prefix (make-string (or level 2) ?*)))
  (pcase (magent-thread-item-type item)
    ('message
       (insert (format "%s Message: %s\n"
                       prefix
                       (upcase (symbol-name
                                (or (magent-thread-item-role item)
                                    'message)))))
       (insert (magent-command-session--shift-headings
                (or (magent-thread-item-content item) "")
                (1+ (or level 2)))
               "\n\n"))
    ('tool
       (insert (format "%s Step: %s [%s]\n"
                       prefix
                       (or (magent-thread-item-name item) "tool")
                       (magent-command-session--format-value
                        (magent-thread-item-status item))))
       (when-let* ((input (magent-thread-item-input item)))
         (insert "Input: " (magent-command-session--format-value input) "\n"))
       (insert (or (magent-thread-item-output item)
                   (magent-thread-item-error item)
                   "")
               "\n\n"))
    (_
       (insert (format "%s %s [%s]\n%s\n\n"
                       prefix
                       (upcase (symbol-name
                                (magent-thread-item-type item)))
                       (magent-command-session--format-value
                        (magent-thread-item-status item))
                       (or (magent-thread-item-content item)
                           (magent-thread-item-output item)
                           "")))))))

(defun magent-command-session--thread-items (thread)
  "Return all THREAD items in chronological order."
  (apply #'append
         (mapcar #'magent-thread-turn-items
                 (magent-thread-turns thread))))

(defun magent-command-session--result-items (items)
  "Return the final result items selected from ITEMS."
  (or (cl-remove-if-not #'magent-command-session--final-item-p items)
      (last (cl-remove-if-not
             (lambda (item)
               (and (eq (magent-thread-item-type item) 'message)
                    (eq (magent-thread-item-role item) 'assistant)))
             items))))

(defun magent-command-session--session-label (file)
  "Return a unique completion label for command session FILE."
  (let* ((meta (magent-session--read-file-metadata-cached file))
         (time (magent-session--format-display-timestamp file))
         (command (or (plist-get meta :command) "unknown"))
         (status (or (plist-get meta :status) "unknown"))
         (id (file-name-base file))
         (title (or (plist-get meta :title)
                    (plist-get meta :summary-title)
                    id)))
    (format "%s  [%s]  %s  %s  <%s>"
            time status command title id)))

(defun magent-command-open-session (file)
  "Open read-only viewer for isolated command session FILE."
  (interactive
   (let ((files (magent-session-list-command-files)))
     (unless files
       (user-error "Magent: no isolated command sessions found"))
     (let* ((choices (mapcar (lambda (file)
                               (cons (magent-command-session--session-label file)
                                     file))
                             files))
            (selected (completing-read "Command session: "
                                       (mapcar #'car choices) nil t)))
       (list (cdr (assoc selected choices))))))
  (unless (and file (file-exists-p file))
    (user-error "Magent: command session file not found"))
  (let* ((loaded (magent-session-read-file file))
         (session (plist-get loaded :session))
         (meta (magent-session--read-file-metadata-cached file))
         (thread (and session (magent-session-thread-ledger session)))
         (items (and thread (magent-command-session--thread-items thread)))
         (result-items (and items (magent-command-session--result-items items)))
         (buffer (get-buffer-create
                  (format "*Magent Command Session: %s*"
                          (file-name-base file)))))
    (with-current-buffer buffer
      (magent-command-session-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Magent Command Session\n\n")
        (insert (format "Command: %s\n" (or (plist-get meta :command) "")))
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
              (insert (magent-command-session--shift-headings
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
                  (magent-command-session--insert-item item 3))))
          (insert "No transcript items.\n"))
        (add-text-properties (point-min) (point-max) '(read-only t))
        (goto-char (point-min))
        (when (re-search-forward "^\\* Activity$" nil t)
          (beginning-of-line)
          (outline-hide-subtree)
          (setq magent-command-session--details-hidden t))
        (goto-char (point-min))))
    (display-buffer buffer)))

;;;###autoload
(defun magent-command-list-sessions ()
  "List and inspect Magent command sessions."
  (interactive)
  (call-interactively #'magent-command-open-session))

(defvar magent-command-session--active-invocations
  (make-hash-table :test #'equal)
  "Active isolated invocations keyed by their command session ids.")

(defun magent-command-session--status-string (status)
  "Return command STATUS as a string."
  (cond
   ((stringp status) status)
   ((symbolp status) (symbol-name status))
   ((null status) "unknown")
   (t (format "%s" status))))

(defun magent-command-session--save (invocation)
  "Persist INVOCATION's command session without changing the active session."
  (let ((previous-session magent--current-session)
        (previous-scope magent-session--current-scope))
    (unwind-protect
        (progn
          (setq magent--current-session
                (magent-command-invocation-session invocation)
                magent-session--current-scope
                (magent-command-invocation-scope invocation))
          (magent-session-save))
      (setq magent--current-session previous-session
            magent-session--current-scope previous-scope))))

(defun magent-command-session--with-current-session (invocation function)
  "Call FUNCTION with INVOCATION's session and scope temporarily active."
  (let ((previous-session magent--current-session)
        (previous-scope magent-session--current-scope))
    (unwind-protect
        (progn
          (setq magent--current-session
                (magent-command-invocation-session invocation)
                magent-session--current-scope
                (magent-command-invocation-scope invocation))
          (funcall function))
      (setq magent--current-session previous-session
            magent-session--current-scope previous-scope))))

(defun magent-command-session--ensure-turn (invocation input)
  "Ensure INVOCATION has a command ledger turn for INPUT."
  (or (magent-command-invocation-turn-id invocation)
      (magent-command-session--with-current-session
       invocation
       (lambda ()
         (let* ((session (magent-command-invocation-session invocation))
                (thread (magent-session-thread-ledger session))
                (turn (magent-thread-queue-turn
                       thread input nil
                       (list :source 'magent-command
                             :command
                             (magent-command-spec-name
                              (magent-command-invocation-spec invocation))
                             :command-invocation-id
                             (magent-command-invocation-id invocation)))))
           (magent-thread-start-turn thread (magent-thread-turn-id turn))
           (magent-thread-record-user-message-if-needed
            thread (magent-thread-turn-id turn) input nil
            (list :source 'magent-command
                  :command-invocation-id
                  (magent-command-invocation-id invocation)))
           (setf (magent-command-invocation-turn-id invocation)
                 (magent-thread-turn-id turn))
           (magent-session-refresh-projections session)
           (magent-command-session--save invocation)
           (magent-thread-turn-id turn))))))

(defun magent-command-session-record-tool
    (invocation name args result &optional metadata)
  "Record a tool-like step in INVOCATION's ledger."
  (magent-command-session--with-current-session
   invocation
   (lambda ()
     (let* ((session (magent-command-invocation-session invocation))
            (thread (magent-session-thread-ledger session))
            (turn-id (magent-command-session--ensure-turn
                      invocation
                      (magent-command-spec-title
                       (magent-command-invocation-spec invocation))))
            (call-id (magent-protocol-generate-id "cmdtool")))
       (magent-thread-record-tool-result
        thread turn-id call-id name args result
        (append metadata (list :source 'magent-command)))
       (magent-session-refresh-projections session)
       (magent-command-session--save invocation)))))

(defun magent-command-session-record-message
    (invocation role content &optional phase metadata)
  "Record ROLE message CONTENT in INVOCATION's ledger."
  (magent-command-session--with-current-session
   invocation
   (lambda ()
     (let* ((session (magent-command-invocation-session invocation))
            (thread (magent-session-thread-ledger session))
            (turn-id (magent-command-session--ensure-turn
                      invocation
                      (magent-command-spec-title
                       (magent-command-invocation-spec invocation)))))
       (magent-thread-record-message
        thread turn-id role content phase
        (append metadata (list :source 'magent-command)))
       (magent-session-refresh-projections session)
       (magent-command-session--save invocation)))))

(defun magent-command-session--session-id (invocation)
  "Return INVOCATION's command session id."
  (magent-session-get-id (magent-command-invocation-session invocation)))

(defun magent-command-session-initialize (invocation)
  "Create and attach an isolated durable session to INVOCATION."
  (let* ((spec (magent-command-invocation-spec invocation))
         (origin-scope (magent-command-invocation-origin-scope invocation))
         (session (magent-session-create))
         (id (magent-session-get-id session))
         (scope (magent-session-command-scope
                 id (magent-command-spec-name spec) origin-scope))
         ;; Do not register through `magent-runtime-session-register': Doctor
         ;; must remain usable while an unrelated runtime queue lease exists.
         (runtime-session
          (magent-runtime-session-create
           :id id :scope scope :magent-session session)))
    (setf (magent-command-invocation-session invocation) session
          (magent-command-invocation-scope invocation) scope
          (magent-command-invocation-runtime-session invocation) runtime-session)
    (dolist (entry `((kind . "command")
                     (command . ,(magent-command-spec-name spec))
                     (title . ,(magent-command-spec-title spec))
                     (status . "running")
                     (session-policy . "isolated")
                     (origin-scope . ,origin-scope)
                     (invocation-id . ,(magent-command-invocation-id invocation))
                     ,@(when-let* ((parent-id
                                    (magent-command-invocation-parent-session-id
                                     invocation)))
                         `((parent-session-id . ,parent-id)))))
      (magent-session-set-metadata-value session (car entry) (cdr entry)))
    (let ((previous-session magent--current-session)
          (previous-scope magent-session--current-scope))
      (unwind-protect
          (magent-session-install scope session)
        (setq magent--current-session previous-session
              magent-session--current-scope previous-scope)))
    (puthash id invocation magent-command-session--active-invocations)
    invocation))

(defun magent-command-session-active-invocations (&optional cancellable-only)
  "Return active isolated invocations.
When CANCELLABLE-ONLY is non-nil, omit invocations without owned work."
  (let (invocations)
    (maphash
     (lambda (_id invocation)
       (when (and (eq (magent-command-invocation-status invocation) 'active)
                  (or (not cancellable-only)
                      (magent-command-invocation-cancel-function invocation)
                      (magent-command-invocation-submission-ids invocation)))
         (push invocation invocations)))
     magent-command-session--active-invocations)
    (sort invocations
          (lambda (left right)
            (string< (magent-command-session--session-id left)
                     (magent-command-session--session-id right))))))

(defun magent-command-session--record-parent-breadcrumb
    (invocation status message)
  "Record INVOCATION completion as a compact parent-session breadcrumb."
  (when-let* ((parent (magent-command-invocation-parent-session invocation)))
    (let* ((parent-scope (or (magent-command-invocation-parent-scope invocation)
                             (magent-command-invocation-origin-scope invocation)))
           (session-id (magent-command-session--session-id invocation))
           (name (magent-command-spec-name
                  (magent-command-invocation-spec invocation)))
           (title (format "Command: %s" name))
           (result (format "%s %s: %s"
                           name
                           (magent-command-session--status-string status)
                           (or message session-id)))
           (previous-session magent--current-session)
           (previous-scope magent-session--current-scope))
      (unwind-protect
          (progn
            (setq magent--current-session parent
                  magent-session--current-scope parent-scope)
            (let* ((thread (magent-session-thread-ledger parent))
                   (turn (magent-thread-queue-turn
                          thread title nil
                          (list :source 'magent-command-breadcrumb
                                :command-session-id session-id
                                :command name))))
              (magent-thread-start-turn thread (magent-thread-turn-id turn))
              (magent-thread-record-user-message-if-needed
               thread (magent-thread-turn-id turn) title nil
               (list :source 'magent-command-breadcrumb))
              (magent-thread-record-tool-result
               thread (magent-thread-turn-id turn)
               (magent-protocol-generate-id "cmdref")
               name
               (list :command-session-id session-id
                     :status (magent-command-session--status-string status))
               result
               (list :source 'magent-command-breadcrumb))
              (pcase status
                ('completed
                 (magent-thread-complete-turn thread (magent-thread-turn-id turn)))
                ('cancelled
                 (magent-thread-interrupt-turn
                  thread (magent-thread-turn-id turn) result))
                (_
                 (magent-thread-fail-turn
                  thread (magent-thread-turn-id turn) result)))
              (magent-session-refresh-projections parent)
              (magent-session-save-deferred-for-session
               parent parent-scope 0)))
        (setq magent--current-session previous-session
              magent-session--current-scope previous-scope)))))

(defun magent-command-session-finalize (invocation status result)
  "Finalize INVOCATION and return a fallback frontend response string."
  (let* ((session (magent-command-invocation-session invocation))
         (message (magent-agent-result-content-string result))
         (fallback
          (and (not (magent-command-invocation-response-recorded-p invocation))
               (not (string-empty-p (string-trim message)))
               message)))
    (magent-session-set-metadata-value
     session 'status (magent-command-session--status-string status))
    (magent-command-session--with-current-session
     invocation
     (lambda ()
       (let* ((thread (magent-session-thread-ledger session))
              (turn-id (magent-command-session--ensure-turn
                        invocation
                        (magent-command-spec-title
                         (magent-command-invocation-spec invocation)))))
         (when fallback
           (magent-thread-record-message
            thread turn-id 'assistant fallback nil
            (list :source 'magent-command-final
                  :status (magent-command-session--status-string status)))
           (setf (magent-command-invocation-response-recorded-p invocation) t))
         (unless (when-let* ((turn (magent-thread-find-turn thread turn-id)))
                   (magent-thread-terminal-turn-p turn))
           (pcase status
             ('completed (magent-thread-complete-turn thread turn-id))
             ('cancelled (magent-thread-interrupt-turn thread turn-id message))
             (_ (magent-thread-fail-turn thread turn-id message))))
         (magent-session-refresh-projections session)
         (magent-command-session--save invocation))))
    (magent-command-session--record-parent-breadcrumb invocation status message)
    (magent-command-session-untrack invocation)
    (when (magent-command-invocation-interactive-p invocation)
      (message "Magent %s %s: %s"
               (magent-command-spec-name
                (magent-command-invocation-spec invocation))
               (magent-command-session--status-string status)
               (or message (magent-command-session--session-id invocation))))
    fallback))

(defun magent-command-session-untrack (invocation)
  "Stop tracking isolated command INVOCATION."
  (when (magent-command-invocation-session invocation)
    (remhash (magent-command-session--session-id invocation)
             magent-command-session--active-invocations)))

(defun magent-command-session-cancel (session-id)
  "Cancel the active isolated command SESSION-ID."
  (let ((invocation
         (gethash session-id magent-command-session--active-invocations)))
    (unless invocation
      (user-error "Magent: command session is not active: %s" session-id))
    (magent-command-cancel invocation)))

(defun magent-command-session--active-label (invocation)
  "Return a unique completion label for active INVOCATION."
  (let ((spec (magent-command-invocation-spec invocation)))
    (format "%s  %s  <%s>"
            (magent-command-spec-name spec)
            (magent-command-spec-title spec)
            (magent-command-session--session-id invocation))))

(defun magent-command-session-read-active-id ()
  "Read and return the id of an active cancellable command session."
  (let ((invocations (magent-command-session-active-invocations t)))
    (unless invocations
      (user-error "Magent: no cancellable isolated commands are active"))
    (let* ((choices
            (mapcar
             (lambda (invocation)
               (cons (magent-command-session--active-label invocation)
                     (magent-command-session--session-id invocation)))
             invocations))
           (selected
            (completing-read "Cancel command: " (mapcar #'car choices) nil t)))
      (cdr (assoc selected choices)))))

(provide 'magent-command-session)
;;; magent-command-session.el ends here
