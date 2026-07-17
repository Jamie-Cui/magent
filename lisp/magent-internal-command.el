;;; magent-internal-command.el --- Session-backed Magent workflows  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6, Magent:deepseek-v4-pro

;;; Commentary:

;; Internal command workflows are user-invoked Magent maintenance or diagnosis
;; operations.  They create their own durable sessions instead of running inside
;; ordinary user conversation sessions.

;;; Code:

(require 'cl-lib)
(require 'outline)
(require 'subr-x)
(require 'magent-config)
(require 'magent-protocol)
(require 'magent-ledger)
(require 'magent-runtime)
(require 'magent-session)

(declare-function magent-agent-info-create "magent-agent-info")
(declare-function magent-agent-registry-get "magent-agent-registry")
(declare-function magent-agent-registry-get-default "magent-agent-registry")
(declare-function magent-runtime-cancel "magent-runtime-api")
(declare-function magent-runtime-session-create "magent-runtime-api")
(declare-function magent-runtime-session-register "magent-runtime-api")
(declare-function magent-runtime-submit "magent-runtime-api")
(declare-function magent-runtime-session-set-agent "magent-runtime-api")

(cl-defstruct (magent-internal-command-spec
               (:constructor magent-internal-command-spec-create)
               (:copier nil))
  name
  description
  title
  runner
  runner-type)

(cl-defstruct (magent-internal-command-context
               (:constructor magent-internal-command-context-create)
               (:copier nil))
  spec
  session
  scope
  origin-scope
  origin-buffer
  origin-directory
  arguments
  parent-session
  parent-scope
  parent-session-id
  turn-id
  notify-fn
  on-complete
  previous-session
  previous-scope
  restore-on-complete
  runtime-session
  submission-id
  cancel-function
  completed-p)

(defvar magent-internal-command--registry (make-hash-table :test #'equal)
  "Registered Magent internal command specs keyed by command name.")

(defvar magent-internal-command--active-contexts (make-hash-table :test #'equal)
  "Active internal command contexts keyed by their session ids.")

(defun magent-internal-command--normalize-name (name)
  "Return NAME as a command registry key."
  (cond
   ((stringp name) name)
   ((symbolp name) (symbol-name name))
   (t (format "%s" name))))

(cl-defun magent-internal-command-register
    (name &key description title runner runner-type)
  "Register a Magent internal command NAME.
RUNNER is called with one `magent-internal-command-context' argument.
RUNNER-TYPE is metadata for display and extension, for example `pipeline' or
`agent-loop'."
  (let* ((key (magent-internal-command--normalize-name name))
         (spec (magent-internal-command-spec-create
                :name key
                :description description
                :title (or title key)
                :runner runner
                :runner-type runner-type)))
    (puthash key spec magent-internal-command--registry)
    spec))

(defun magent-internal-command-get (name)
  "Return registered Magent internal command NAME, or nil."
  (gethash (magent-internal-command--normalize-name name) magent-internal-command--registry))

(defun magent-internal-command--metadata-set (session key value)
  "Set SESSION internal command metadata KEY to VALUE."
  (magent-session-set-metadata-value session key value))

(defun magent-internal-command--status-string (status)
  "Return command STATUS as a string."
  (cond
   ((stringp status) status)
   ((symbolp status) (symbol-name status))
   ((null status) "unknown")
   (t (format "%s" status))))

(defun magent-internal-command--save-session (context)
  "Persist CONTEXT's internal command session."
  (let ((previous-session magent--current-session)
        (previous-scope magent-session--current-scope))
    (unwind-protect
        (progn
          (setq magent--current-session (magent-internal-command-context-session context)
                magent-session--current-scope (magent-internal-command-context-scope context))
          (magent-session-save))
      (setq magent--current-session previous-session
            magent-session--current-scope previous-scope))))

(defun magent-internal-command--with-context-current-session (context fn)
  "Call FN with CONTEXT's session and scope temporarily active."
  (let ((previous-session magent--current-session)
        (previous-scope magent-session--current-scope))
    (unwind-protect
        (progn
          (setq magent--current-session (magent-internal-command-context-session context)
                magent-session--current-scope (magent-internal-command-context-scope context))
          (funcall fn))
      (setq magent--current-session previous-session
            magent-session--current-scope previous-scope))))

(defun magent-internal-command--ensure-turn (context input)
  "Ensure CONTEXT has a command ledger turn for INPUT."
  (or (magent-internal-command-context-turn-id context)
      (magent-internal-command--with-context-current-session
       context
       (lambda ()
         (let* ((session (magent-internal-command-context-session context))
                (thread (magent-session-thread-ledger session))
                (turn (magent-thread-queue-turn
                       thread input nil
                       (list :source 'magent-internal-command
                             :command
                             (magent-internal-command-spec-name
                              (magent-internal-command-context-spec context))))))
           (magent-thread-start-turn thread (magent-thread-turn-id turn))
           (magent-thread-record-user-message-if-needed
            thread (magent-thread-turn-id turn) input nil
            (list :source 'magent-internal-command))
           (setf (magent-internal-command-context-turn-id context)
                 (magent-thread-turn-id turn))
           (magent-session-refresh-projections session)
           (magent-internal-command--save-session context)
           (magent-thread-turn-id turn))))))

(defun magent-internal-command-record-tool
    (context name args result &optional metadata)
  "Record a tool-like step in CONTEXT's ledger."
  (magent-internal-command--with-context-current-session
   context
   (lambda ()
     (let* ((session (magent-internal-command-context-session context))
            (thread (magent-session-thread-ledger session))
            (turn-id (magent-internal-command--ensure-turn
                      context
                      (magent-internal-command-spec-title
                       (magent-internal-command-context-spec context))))
            (call-id (magent-protocol-generate-id "cmdtool")))
       (magent-thread-record-tool-result
        thread turn-id call-id name args result
        (append (list :source 'magent-internal-command)
                metadata))
       (magent-session-refresh-projections session)
       (magent-internal-command--save-session context)))))

(defun magent-internal-command-record-message
    (context role content &optional phase metadata)
  "Record ROLE message CONTENT in CONTEXT's ledger."
  (magent-internal-command--with-context-current-session
   context
   (lambda ()
     (let* ((session (magent-internal-command-context-session context))
            (thread (magent-session-thread-ledger session))
            (turn-id (magent-internal-command--ensure-turn
                      context
                      (magent-internal-command-spec-title
                       (magent-internal-command-context-spec context)))))
       (magent-thread-record-message
        thread turn-id role content phase
        (append (list :source 'magent-internal-command) metadata))
       (magent-session-refresh-projections session)
       (magent-internal-command--save-session context)))))

(defun magent-internal-command-notify (context message)
  "Notify CONTEXT progress MESSAGE and record it in the internal session."
  (when-let* ((notify (magent-internal-command-context-notify-fn context)))
    (funcall notify message))
  (magent-internal-command-record-tool
   context "progress" nil message
   (list :progress t)))

(defun magent-internal-command--restore-previous-session (context)
  "Restore CONTEXT's previous active session when appropriate."
  (when (and (magent-internal-command-context-restore-on-complete context)
             (eq magent--current-session
                 (magent-internal-command-context-session context)))
    (setq magent--current-session (magent-internal-command-context-previous-session context)
          magent-session--current-scope
          (or (magent-internal-command-context-previous-scope context) 'global))))

(defun magent-internal-command--context-session-id (context)
  "Return CONTEXT's internal session id."
  (magent-session-get-id (magent-internal-command-context-session context)))

(defun magent-internal-command--track-active (context)
  "Track CONTEXT as an active internal command."
  (puthash (magent-internal-command--context-session-id context)
           context
           magent-internal-command--active-contexts)
  context)

(defun magent-internal-command--untrack-active (context)
  "Stop tracking CONTEXT as an active internal command."
  (remhash (magent-internal-command--context-session-id context)
           magent-internal-command--active-contexts))

(defun magent-internal-command-set-cancel-function (context function)
  "Set CONTEXT's cancellation FUNCTION and return CONTEXT.
FUNCTION is called without arguments and should cancel all work belonging to
the command's internal session."
  (unless (or (null function) (functionp function))
    (error "Expected a cancellation function, got: %S" function))
  (setf (magent-internal-command-context-cancel-function context) function)
  context)

(defun magent-internal-command--record-parent-breadcrumb (context status message)
  "Record CONTEXT completion as a compact breadcrumb in the parent session."
  (when-let* ((parent (magent-internal-command-context-parent-session context)))
    (let* ((parent-scope (or (magent-internal-command-context-parent-scope context)
                             (magent-internal-command-context-origin-scope context)))
           (session-id (magent-session-get-id
                        (magent-internal-command-context-session context)))
           (name (magent-internal-command-spec-name
                  (magent-internal-command-context-spec context)))
           (title (format "Internal command: %s" name))
           (result (format "%s %s: %s"
                           name
                           (magent-internal-command--status-string status)
                           (or message session-id))))
      (let ((previous-session magent--current-session)
            (previous-scope magent-session--current-scope))
        (unwind-protect
            (progn
              (setq magent--current-session parent
                    magent-session--current-scope parent-scope)
              (let* ((thread (magent-session-thread-ledger parent))
                     (turn (magent-thread-queue-turn
                            thread title nil
                            (list :source 'magent-internal-command-breadcrumb
                                  :internal-session-id session-id
                                  :command name))))
                (magent-thread-start-turn thread (magent-thread-turn-id turn))
                (magent-thread-record-user-message-if-needed
                 thread (magent-thread-turn-id turn) title nil
                 (list :source 'magent-internal-command-breadcrumb))
                (magent-thread-record-tool-result
                 thread (magent-thread-turn-id turn)
                 (magent-protocol-generate-id "cmdref")
                 name
                 (list :internal-session-id session-id
                       :status (magent-internal-command--status-string status))
                 result
                 (list :source 'magent-internal-command-breadcrumb))
                (pcase status
                  ('completed
                   (magent-thread-complete-turn
                    thread (magent-thread-turn-id turn)))
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
                magent-session--current-scope previous-scope))))))

(cl-defun magent-internal-command-complete (context status message &key (record-message t))
  "Complete CONTEXT with STATUS and MESSAGE.
When RECORD-MESSAGE is nil, update status metadata without adding MESSAGE as a
new assistant item.  This is useful for agent-loop commands whose transcript is
already recorded by the runtime."
  (unless (magent-internal-command-context-completed-p context)
    (setf (magent-internal-command-context-completed-p context) t)
    (unwind-protect
        (progn
          (magent-internal-command--metadata-set
           (magent-internal-command-context-session context)
           'status (magent-internal-command--status-string status))
          (magent-internal-command--with-context-current-session
           context
           (lambda ()
             (let* ((session (magent-internal-command-context-session context))
                    (thread (magent-session-thread-ledger session))
                    (turn-id (magent-internal-command--ensure-turn
                              context
                              (magent-internal-command-spec-title
                               (magent-internal-command-context-spec context)))))
               (when (and record-message message)
                 (magent-thread-record-message
                  thread turn-id 'assistant message nil
                  (list :source 'magent-internal-command-final
                        :status (magent-internal-command--status-string status))))
               (unless (when-let* ((turn (magent-thread-find-turn thread turn-id)))
                         (magent-thread-terminal-turn-p turn))
                 (pcase status
                   ('completed
                    (magent-thread-complete-turn thread turn-id))
                   ('cancelled
                    (magent-thread-interrupt-turn thread turn-id message))
                   (_
                    (magent-thread-fail-turn thread turn-id message))))
               (magent-session-refresh-projections session)
               (magent-internal-command--save-session context))))
          (magent-internal-command--record-parent-breadcrumb context status message)
          (when-let* ((on-complete
                       (magent-internal-command-context-on-complete context)))
            (funcall on-complete status message context))
          (message "Magent %s %s: %s"
                   (magent-internal-command-spec-name
                    (magent-internal-command-context-spec context))
                   (magent-internal-command--status-string status)
                   (or message
                       (magent-internal-command--context-session-id context))))
      (magent-internal-command--untrack-active context))))

(defun magent-internal-command--context
    (spec &optional notify-fn on-complete arguments)
  "Create a command context for SPEC."
  (magent-runtime-ensure-initialized)
  (let* ((origin-buffer (current-buffer))
         (origin-directory default-directory)
         (origin-scope (magent-runtime-command-scope))
         (_ (magent-runtime-prepare-command-context origin-scope))
         (previous-session magent--current-session)
         (previous-scope magent-session--current-scope)
         (parent-session (magent-session-get-if-present origin-scope))
         (parent-session-id (and parent-session
                                 (magent-session-get-id parent-session)))
         (session (magent-session-create))
         (id (magent-session-get-id session))
         (scope (magent-session-internal-scope
                 id (magent-internal-command-spec-name spec) origin-scope)))
    (dolist (entry `((kind . "internal-command")
                     (command . ,(magent-internal-command-spec-name spec))
                     (title . ,(magent-internal-command-spec-title spec))
                     (status . "running")
                     (runner-type . ,(and (magent-internal-command-spec-runner-type spec)
                                          (symbol-name
                                           (magent-internal-command-spec-runner-type
                                            spec))))
                     (origin-scope . ,origin-scope)
                     ,@(when parent-session-id
                         `((parent-session-id . ,parent-session-id)))))
      (magent-session-set-metadata-value session (car entry) (cdr entry)))
    (magent-session-install scope session)
    (magent-internal-command-context-create
     :spec spec
     :session session
     :scope scope
     :origin-scope origin-scope
     :origin-buffer origin-buffer
     :origin-directory origin-directory
     :arguments arguments
     :parent-session parent-session
     :parent-scope origin-scope
     :parent-session-id parent-session-id
     :notify-fn notify-fn
     :on-complete on-complete
     :previous-session previous-session
     :previous-scope previous-scope
     :restore-on-complete t)))

(cl-defun magent-internal-command-run (name &key notify-fn on-complete arguments)
  "Run registered Magent internal command NAME."
  (let* ((spec (or (magent-internal-command-get name)
                   (error "Unknown Magent command: %s" name)))
         (runner (or (magent-internal-command-spec-runner spec)
                     (error "Magent command %s has no runner"
                            (magent-internal-command-spec-name spec))))
         (context (magent-internal-command--context
                   spec notify-fn on-complete arguments)))
    (magent-internal-command--track-active context)
    (unwind-protect
        (condition-case err
            (funcall runner context)
          (quit
           (magent-internal-command-complete context 'cancelled "Command cancelled")
           (signal 'quit nil))
          (error
           (magent-internal-command-complete
            context 'failed
            (format "Error: %s" (error-message-string err)))))
      (magent-internal-command--restore-previous-session context))
    context))

(defun magent-internal-command--agent-loop-runner (prompt &optional agent-name)
  "Return an agent-loop runner for PROMPT and optional AGENT-NAME."
  (lambda (context)
    (require 'magent-agent-registry)
    (require 'magent-runtime-api)
    (let* ((session (magent-internal-command-context-session context))
           (scope (magent-internal-command-context-scope context))
           (origin-scope (magent-internal-command-context-origin-scope context))
           (runtime-session
            (magent-runtime-session-register scope session))
           (agent (or (and agent-name
                           (magent-agent-registry-get agent-name))
                      (magent-agent-registry-get "build")
                      (magent-agent-registry-get-default))))
      (setf (magent-internal-command-context-runtime-session context) runtime-session)
      (magent-internal-command-set-cancel-function
       context
       (lambda () (magent-runtime-cancel runtime-session)))
      (when agent
        (magent-runtime-session-set-agent runtime-session agent))
      (setf (magent-internal-command-context-submission-id context)
            (magent-runtime-submit
             runtime-session prompt
             :context (and (stringp origin-scope)
                           (list :project-root origin-scope))
             :agent agent
             :observer (lambda (event)
                         (when-let* ((turn-id (plist-get event :turn-id)))
                           (setf (magent-internal-command-context-turn-id context)
                                 turn-id)))
             :on-complete
             (lambda (status result)
               (magent-internal-command-complete
                context status
                (cond
                 ((and (fboundp 'magent-agent-result-p)
                       (magent-agent-result-p result))
                  (or (ignore-errors
                        (magent-agent-result-content-string result))
                      (format "%s" result)))
                 ((stringp result) result)
                 (t (format "%s" result)))
                :record-message nil)))))))

(defun magent-internal-command--session-label (file)
  "Return completion label for internal command session FILE."
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

(defun magent-internal-command-active-contexts (&optional cancellable-only)
  "Return active internal command contexts.
When CANCELLABLE-ONLY is non-nil, omit contexts without a cancellation
function."
  (let (contexts)
    (maphash
     (lambda (_id context)
       (when (or (not cancellable-only)
                 (magent-internal-command-context-cancel-function context))
         (push context contexts)))
     magent-internal-command--active-contexts)
    (sort contexts
          (lambda (a b)
            (string< (magent-internal-command--context-session-id a)
                     (magent-internal-command--context-session-id b))))))

(defun magent-internal-command-cancel (session-id)
  "Cancel active internal command SESSION-ID.
Return the command runner's cancellation result."
  (let ((context (gethash session-id magent-internal-command--active-contexts)))
    (unless context
      (user-error "Magent: internal command session is not active: %s"
                  session-id))
    (unless (magent-internal-command-context-cancel-function context)
      (user-error "Magent: internal command session cannot be cancelled: %s"
                  session-id))
    (funcall (magent-internal-command-context-cancel-function context))))

(defun magent-internal-command--active-label (context)
  "Return a unique completion label for active command CONTEXT."
  (let ((spec (magent-internal-command-context-spec context)))
    (format "%s  %s  <%s>"
            (magent-internal-command-spec-name spec)
            (magent-internal-command-spec-title spec)
            (magent-internal-command--context-session-id context))))

(defun magent-internal-command--format-value (value)
  "Return display string for VALUE."
  (cond
   ((null value) "")
   ((vectorp value)
    (mapconcat #'magent-internal-command--format-value (append value nil) ", "))
   ((listp value)
    (format "%S" value))
   (t
    (format "%s" value))))

(defvar-local magent-internal-command-session--details-hidden t
  "Whether the current internal session viewer hides activity details.")

(defvar magent-internal-command-session-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'magent-internal-command-session-toggle-section)
    (define-key map (kbd "<backtab>") #'magent-internal-command-session-toggle-all)
    (define-key map (kbd "S-TAB") #'magent-internal-command-session-toggle-all)
    map)
  "Keymap for `magent-internal-command-session-mode'.")

(define-derived-mode magent-internal-command-session-mode special-mode "Magent-Command"
  "Major mode for progressively disclosed Magent internal sessions."
  (setq-local outline-regexp "\\*+ ")
  (setq-local outline-level
              (lambda () (1- (- (match-end 0) (match-beginning 0)))))
  (outline-minor-mode 1))

(defun magent-internal-command-session-toggle-section ()
  "Toggle the outline section at or above point."
  (interactive)
  (save-excursion
    (unless (looking-at outline-regexp)
      (outline-back-to-heading t))
    (outline-toggle-children)))

(defun magent-internal-command-session-toggle-all ()
  "Toggle all detail sections in the current session viewer."
  (interactive)
  (if magent-internal-command-session--details-hidden
      (progn
        (outline-show-all)
        (setq magent-internal-command-session--details-hidden nil))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* Activity$" nil t)
        (beginning-of-line)
        (outline-hide-subtree)))
    (setq magent-internal-command-session--details-hidden t)))

(defun magent-internal-command--shift-headings (text levels)
  "Shift Org-style headings in TEXT by LEVELS for display."
  (replace-regexp-in-string
   "^\\*+ "
   (lambda (heading)
     (concat (make-string levels ?*) heading))
   (or text "")))

(defun magent-internal-command--item-metadata-value (item key)
  "Return ITEM metadata value for KEY from a plist or loaded alist."
  (let ((metadata (magent-thread-item-metadata item)))
    (if (and (listp metadata) (keywordp (car metadata)))
        (plist-get metadata key)
      (or (alist-get (intern (substring (symbol-name key) 1)) metadata)
          (alist-get key metadata)))))

(defun magent-internal-command--final-item-p (item)
  "Return non-nil when ITEM is an internal command final result."
  (and (eq (magent-thread-item-type item) 'message)
       (eq (magent-thread-item-role item) 'assistant)
       (member (format "%s"
                       (magent-internal-command--item-metadata-value item :source))
               '("magent-command-final"
                 "magent-internal-command-final"
                 "magent-doctor-final"))))

(defun magent-internal-command--insert-item (item &optional level)
  "Insert ledger ITEM into current buffer at outline LEVEL."
  (let ((prefix (make-string (or level 2) ?*)))
  (pcase (magent-thread-item-type item)
    ('message
       (insert (format "%s Message: %s\n"
                       prefix
                       (upcase (symbol-name
                                (or (magent-thread-item-role item)
                                    'message)))))
       (insert (magent-internal-command--shift-headings
                (or (magent-thread-item-content item) "")
                (1+ (or level 2)))
               "\n\n"))
    ('tool
       (insert (format "%s Step: %s [%s]\n"
                       prefix
                       (or (magent-thread-item-name item) "tool")
                       (magent-internal-command--format-value
                        (magent-thread-item-status item))))
       (when-let* ((input (magent-thread-item-input item)))
         (insert "Input: " (magent-internal-command--format-value input) "\n"))
       (insert (or (magent-thread-item-output item)
                   (magent-thread-item-error item)
                   "")
               "\n\n"))
    (_
       (insert (format "%s %s [%s]\n%s\n\n"
                       prefix
                       (upcase (symbol-name
                                (magent-thread-item-type item)))
                       (magent-internal-command--format-value
                        (magent-thread-item-status item))
                       (or (magent-thread-item-content item)
                           (magent-thread-item-output item)
                           "")))))))

(defun magent-internal-command--thread-items (thread)
  "Return all THREAD items in chronological order."
  (apply #'append
         (mapcar #'magent-thread-turn-items
                 (magent-thread-turns thread))))

(defun magent-internal-command--result-items (items)
  "Return the final result items selected from ITEMS."
  (or (cl-remove-if-not #'magent-internal-command--final-item-p items)
      (last (cl-remove-if-not
             (lambda (item)
               (and (eq (magent-thread-item-type item) 'message)
                    (eq (magent-thread-item-role item) 'assistant)))
             items))))

(defun magent-internal-command-open-session (file)
  "Open read-only viewer for internal command session FILE."
  (interactive
   (let ((files (magent-session-list-internal-files)))
     (unless files
       (user-error "Magent: no internal command sessions found"))
     (let* ((choices (mapcar (lambda (file)
                               (cons (magent-internal-command--session-label file)
                                     file))
                             files))
            (selected (completing-read "Internal session: "
                                       (mapcar #'car choices) nil t)))
       (list (cdr (assoc selected choices))))))
  (unless (and file (file-exists-p file))
    (user-error "Magent: internal session file not found"))
  (let* ((loaded (magent-session-read-file file))
         (session (plist-get loaded :session))
         (meta (magent-session--read-file-metadata-cached file))
         (thread (and session (magent-session-thread-ledger session)))
         (items (and thread (magent-internal-command--thread-items thread)))
         (result-items (and items (magent-internal-command--result-items items)))
         (buffer (get-buffer-create
                  (format "*Magent Internal Session: %s*"
                          (file-name-base file)))))
    (with-current-buffer buffer
      (magent-internal-command-session-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Magent Internal Session\n\n")
        (insert (format "Command: %s\n" (or (plist-get meta :command) "")))
        (insert (format "Status: %s\n" (or (plist-get meta :status) "")))
        (insert (format "Title: %s\n" (or (plist-get meta :title) "")))
        (insert (format "Scope: %s\n"
                        (or (plist-get meta :project-root)
                            (plist-get meta :scope)
                            "")))
        (when-let* ((parent (plist-get meta :parent-session-id)))
          (insert (format "Parent session: %s\n" parent)))
        (insert (format "File: %s\n\n" file))
        (insert "* Result\n")
        (if result-items
            (dolist (item result-items)
              (insert (magent-internal-command--shift-headings
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
                  (magent-internal-command--insert-item item 3))))
          (insert "No transcript items.\n"))
        (add-text-properties (point-min) (point-max) '(read-only t))
        (goto-char (point-min))
        (when (re-search-forward "^\\* Activity$" nil t)
          (beginning-of-line)
          (outline-hide-subtree)
          (setq magent-internal-command-session--details-hidden t))
        (goto-char (point-min))))
    (display-buffer buffer)))

;;;###autoload
(defun magent-list-internal-sessions ()
  "List and inspect Magent internal command sessions."
  (interactive)
  (call-interactively #'magent-internal-command-open-session))

;;;###autoload
(defun magent-cancel-internal-command (&optional session-id)
  "Cancel an active, cancellable internal command SESSION-ID."
  (interactive)
  (let ((target session-id))
    (unless target
      (let ((contexts (magent-internal-command-active-contexts t)))
        (unless contexts
          (user-error "Magent: no cancellable internal commands are active"))
        (let* ((choices
                (mapcar
                 (lambda (context)
                   (cons (magent-internal-command--active-label context)
                         (magent-internal-command--context-session-id context)))
                 contexts))
               (selected
                (completing-read "Cancel internal command: "
                                 (mapcar #'car choices) nil t)))
          (setq target (cdr (assoc selected choices))))))
    (let ((cancelled (magent-internal-command-cancel target)))
      (when (called-interactively-p 'interactive)
        (message "Magent cancellation requested for %s%s"
                 target
                 (if (numberp cancelled)
                     (format " (%d submission%s)"
                             cancelled
                             (if (= cancelled 1) "" "s"))
                   "")))
      cancelled)))

(provide 'magent-internal-command)
;;; magent-internal-command.el ends here
