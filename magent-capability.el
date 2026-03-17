;;; magent-capability.el --- Capability registry and resolver for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Capability registry and progressive-disclosure resolver for magent.
;;
;; Capabilities sit above skills and below user intent.  They model
;; problem-oriented ability surfaces such as "runtime inspection" or
;; "magit workflow" without exposing raw Emacs functions or every
;; installed package directly to the model.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magent-config)

(declare-function magent-log "magent-ui")

(cl-defstruct (magent-capability
               (:constructor magent-capability-create))
  "Capability definition used for progressive disclosure."
  name
  title
  description
  (source-kind 'builtin)
  source-name
  skills
  modes
  features
  files
  prompt-keywords
  (disclosure 'suggested)
  (risk 'low)
  notes
  file-path)

(cl-defstruct (magent-capability-match
               (:constructor magent-capability-match-create))
  "Match result for a capability resolution pass."
  capability
  (score 0)
  reasons
  (status 'hidden))

(cl-defstruct (magent-capability-resolution
               (:constructor magent-capability-resolution-create))
  "Progressive disclosure result for one request."
  prompt
  context
  explicit-skills
  matches
  active-capabilities
  suggested-capabilities
  skill-names)

(defvar magent-capability--registry nil
  "Alist of (capability-name . `magent-capability').")

(defvar magent-capability--last-resolution nil
  "Last `magent-capability-resolution' returned by the resolver.")

(defconst magent-capability--suggest-threshold 2
  "Minimum score required for a capability to be suggested.")

(defconst magent-capability--activate-threshold 4
  "Minimum score required for a capability to auto-activate.")

(defun magent-capability-register (capability)
  "Register CAPABILITY in the registry."
  (let ((name (magent-capability-name capability)))
    (setq magent-capability--registry
          (cl-remove-if (lambda (entry) (equal (car entry) name))
                        magent-capability--registry))
    (push (cons name capability) magent-capability--registry))
  capability)

(defun magent-capability-unregister (name)
  "Remove capability NAME from the registry."
  (setq magent-capability--registry
        (cl-remove-if (lambda (entry) (equal (car entry) name))
                      magent-capability--registry)))

(defun magent-capability-clear ()
  "Clear the capability registry."
  (setq magent-capability--registry nil))

(defun magent-capability-get (name)
  "Return capability NAME from the registry."
  (cdr (assoc name magent-capability--registry)))

(defun magent-capability-list ()
  "Return a sorted list of registered capability names."
  (sort (mapcar #'car magent-capability--registry) #'string<))

(defun magent-capability-enabled-p (capability)
  "Return non-nil when CAPABILITY is enabled."
  (not (member (magent-capability-name capability)
               magent-disabled-capabilities)))

(defun magent-capability-capture-context ()
  "Capture a structured request context from the current buffer.
Returns a plist or nil when the current buffer is not a useful
source for contextual capability resolution."
  (unless (or (derived-mode-p 'magent-output-mode)
              (minibufferp))
    (list :buffer-name (buffer-name)
          :file-path (buffer-file-name)
          :major-mode major-mode
          :project-root (ignore-errors (magent-project-root))
          :region-active (use-region-p)
          :features features)))

(defun magent-capability--parse-context-prompt (prompt)
  "Extract a structured context plist from PROMPT, if present."
  (when (and (stringp prompt)
             (string-match "\\[Context: \\([^]]+\\)\\]" prompt))
    (let ((payload (match-string 1 prompt)))
      (list
       :buffer-name
       (when (string-match "buffer=\"\\([^\"]+\\)\"" payload)
         (match-string 1 payload))
       :file-path
       (when (string-match "file=\"\\([^\"]+\\)\"" payload)
         (match-string 1 payload))
       :major-mode
       (when (string-match "mode=\\([^] ]+\\)" payload)
         (intern (match-string 1 payload)))
       :region-active
       (and (string-match "region=[0-9]+-[0-9]+" payload) t)))))

(defun magent-capability--merge-context (request-context prompt)
  "Merge REQUEST-CONTEXT with structured context extracted from PROMPT."
  (let ((prompt-context (magent-capability--parse-context-prompt prompt)))
    (list :buffer-name (or (plist-get request-context :buffer-name)
                           (plist-get prompt-context :buffer-name))
          :file-path (or (plist-get request-context :file-path)
                         (plist-get prompt-context :file-path))
          :major-mode (or (plist-get request-context :major-mode)
                          (plist-get prompt-context :major-mode))
          :project-root (plist-get request-context :project-root)
          :region-active (or (plist-get request-context :region-active)
                             (plist-get prompt-context :region-active))
          :features (or (plist-get request-context :features)
                        features))))

(defun magent-capability--glob-match-p (pattern path)
  "Return non-nil when PATTERN matches PATH."
  (and pattern path
       (string-match-p (wildcard-to-regexp pattern) path)))

(defun magent-capability--keyword-match-p (keyword prompt)
  "Return non-nil when KEYWORD matches PROMPT case-insensitively."
  (and keyword
       (stringp prompt)
       (string-match-p (regexp-quote (downcase keyword))
                       (downcase prompt))))

(defun magent-capability--score (capability prompt context)
  "Return a `magent-capability-match' for CAPABILITY."
  (let ((score 0)
        reasons
        (mode (plist-get context :major-mode))
        (file (plist-get context :file-path))
        (project-root (plist-get context :project-root))
        (loaded-features (plist-get context :features))
        matched-keyword)
    (when (and mode
               (memq mode (magent-capability-modes capability)))
      (cl-incf score 3)
      (push (format "mode=%s" mode) reasons))
    (when-let* ((matched-feature
                 (cl-find-if (lambda (feature)
                               (memq feature loaded-features))
                             (magent-capability-features capability))))
      (cl-incf score 2)
      (push (format "feature=%s" matched-feature) reasons))
    (when-let* ((matched-file
                 (cl-find-if (lambda (pattern)
                               (or (magent-capability--glob-match-p pattern file)
                                   (magent-capability--glob-match-p pattern project-root)))
                             (magent-capability-files capability))))
      (cl-incf score 2)
      (push (format "path~=%s" matched-file) reasons))
    (dolist (keyword (magent-capability-prompt-keywords capability))
      (when (and (not matched-keyword)
                 (magent-capability--keyword-match-p keyword prompt))
        (setq matched-keyword keyword)
        (cl-incf score 1)
        (push (format "keyword=%s" keyword) reasons)))
    (let ((status
           (cond
            ((or (not (magent-capability-enabled-p capability))
                 (< score magent-capability--suggest-threshold))
             'hidden)
            ((and (>= score magent-capability--activate-threshold)
                  (eq (magent-capability-disclosure capability) 'active))
             'active)
            ((memq (magent-capability-disclosure capability) '(active suggested))
             'suggested)
            (t 'hidden))))
      (magent-capability-match-create
       :capability capability
       :score score
       :reasons (nreverse reasons)
       :status status))))

(defun magent-capability--sort-matches (matches)
  "Sort MATCHES by status, score, then capability name."
  (sort matches
        (lambda (a b)
          (let ((status-a (pcase (magent-capability-match-status a)
                            ('active 2)
                            ('suggested 1)
                            (_ 0)))
                (status-b (pcase (magent-capability-match-status b)
                            ('active 2)
                            ('suggested 1)
                            (_ 0))))
            (or (> status-a status-b)
                (and (= status-a status-b)
                     (or (> (magent-capability-match-score a)
                            (magent-capability-match-score b))
                         (and (= (magent-capability-match-score a)
                                 (magent-capability-match-score b))
                              (string< (magent-capability-name
                                        (magent-capability-match-capability a))
                                       (magent-capability-name
                                        (magent-capability-match-capability b)))))))))))

(defun magent-capability--dedupe-strings (strings)
  "Return STRINGS without duplicates while preserving order."
  (let (seen result)
    (dolist (string strings (nreverse result))
      (when (and string (not (member string seen)))
        (push string seen)
        (push string result)))))

(defun magent-capability-resolve (prompt &optional request-context explicit-skills)
  "Resolve capabilities for PROMPT and REQUEST-CONTEXT.
EXPLICIT-SKILLS are user-selected instruction skills that should
remain active regardless of capability selection."
  (let* ((context (magent-capability--merge-context request-context prompt))
         (matches (magent-capability--sort-matches
                   (mapcar (lambda (entry)
                             (magent-capability--score (cdr entry) prompt context))
                           magent-capability--registry)))
         (active-all (cl-remove-if-not
                      (lambda (match)
                        (eq (magent-capability-match-status match) 'active))
                      matches))
         (active (if (> magent-capability-max-active 0)
                     (cl-subseq active-all 0
                                (min (length active-all)
                                     magent-capability-max-active))
                   nil))
         (suggested (cl-loop for match in matches
                             when (eq (magent-capability-match-status match) 'suggested)
                             collect match))
         (skill-names (magent-capability--dedupe-strings
                       (append explicit-skills
                               (cl-mapcan (lambda (match)
                                            (copy-sequence
                                             (magent-capability-skills
                                              (magent-capability-match-capability match))))
                                          active))))
         (resolution (magent-capability-resolution-create
                      :prompt prompt
                      :context context
                      :explicit-skills explicit-skills
                      :matches matches
                      :active-capabilities active
                      :suggested-capabilities suggested
                      :skill-names skill-names)))
    (setq magent-capability--last-resolution resolution)
    (when magent-enable-capabilities
      (magent-log "INFO capabilities active=[%s] suggested=[%s]"
                  (mapconcat (lambda (match)
                               (magent-capability-name
                                (magent-capability-match-capability match)))
                             active ", ")
                  (mapconcat (lambda (match)
                               (magent-capability-name
                                (magent-capability-match-capability match)))
                             suggested ", ")))
    resolution))

(defun magent-capability-resolve-for-turn (prompt &optional request-context explicit-skills)
  "Resolve capabilities for one turn.
Returns nil when capability auto-disclosure is disabled."
  (when magent-enable-capabilities
    (magent-capability-resolve prompt request-context explicit-skills)))

(defun magent-capability--insert-match (match)
  "Insert a human-readable description of MATCH into current buffer."
  (let* ((capability (magent-capability-match-capability match))
         (source (format "%s:%s"
                         (magent-capability-source-kind capability)
                         (or (magent-capability-source-name capability)
                             (magent-capability-name capability)))))
    (insert (format "- %s [%s] score=%d\n"
                    (magent-capability-name capability)
                    (magent-capability-match-status match)
                    (magent-capability-match-score match)))
    (insert (format "  %s\n" source))
    (when (magent-capability-description capability)
      (insert (format "  %s\n" (magent-capability-description capability))))
    (when (magent-capability-skills capability)
      (insert (format "  Skills: %s\n"
                      (mapconcat #'identity (magent-capability-skills capability) ", "))))
    (when (magent-capability-match-reasons match)
      (insert (format "  Reasons: %s\n"
                      (mapconcat #'identity
                                 (magent-capability-match-reasons match)
                                 ", "))))))

;;;###autoload
(defun magent-list-capabilities ()
  "Display all registered capabilities."
  (interactive)
  (let ((capabilities (mapcar #'cdr magent-capability--registry)))
    (magent--with-display-buffer "*Magent Capabilities*"
      (insert "Registered Capabilities:\n\n")
      (dolist (capability (sort capabilities
                                (lambda (a b)
                                  (string< (magent-capability-name a)
                                           (magent-capability-name b)))))
        (insert (format "- %s [%s]\n"
                        (magent-capability-name capability)
                        (magent-capability-source-kind capability)))
        (when (magent-capability-description capability)
          (insert (format "  %s\n" (magent-capability-description capability))))
        (when (magent-capability-skills capability)
          (insert (format "  Skills: %s\n"
                          (mapconcat #'identity
                                     (magent-capability-skills capability)
                                     ", "))))
        (when (magent-capability-file-path capability)
          (insert (format "  File: %s\n" (magent-capability-file-path capability))))
        (insert "\n"))
      (insert (format "Total: %d capability(s)\n" (length capabilities))))))

;;;###autoload
(defun magent-describe-capability (capability-name)
  "Show detailed information about CAPABILITY-NAME."
  (interactive
   (list (completing-read "Describe capability: "
                          (magent-capability-list) nil t)))
  (let ((capability (magent-capability-get capability-name)))
    (if (not capability)
        (message "Capability '%s' not found" capability-name)
      (magent--with-display-buffer (format "*Magent Capability: %s*" capability-name)
        (insert (format "# Capability: %s\n\n" capability-name))
        (insert (format "Source: %s\n"
                        (magent-capability-source-kind capability)))
        (when (magent-capability-source-name capability)
          (insert (format "Feature/Package: %s\n"
                          (magent-capability-source-name capability))))
        (insert (format "Disclosure: %s\n" (magent-capability-disclosure capability)))
        (insert (format "Risk: %s\n" (magent-capability-risk capability)))
        (when (magent-capability-description capability)
          (insert (format "\n## Description\n\n%s\n"
                          (magent-capability-description capability))))
        (when (magent-capability-skills capability)
          (insert (format "\n## Skills\n\n%s\n"
                          (mapconcat #'identity
                                     (magent-capability-skills capability)
                                     ", "))))
        (when (magent-capability-modes capability)
          (insert (format "\n## Modes\n\n%s\n"
                          (mapconcat #'symbol-name
                                     (magent-capability-modes capability)
                                     ", "))))
        (when (magent-capability-features capability)
          (insert (format "\n## Features\n\n%s\n"
                          (mapconcat #'symbol-name
                                     (magent-capability-features capability)
                                     ", "))))
        (when (magent-capability-files capability)
          (insert (format "\n## File Patterns\n\n%s\n"
                          (mapconcat #'identity
                                     (magent-capability-files capability)
                                     ", "))))
        (when (magent-capability-prompt-keywords capability)
          (insert (format "\n## Prompt Keywords\n\n%s\n"
                          (mapconcat #'identity
                                     (magent-capability-prompt-keywords capability)
                                     ", "))))
        (when (magent-capability-notes capability)
          (insert (format "\n## Notes\n\n%s\n" (magent-capability-notes capability))))
        (when (magent-capability-file-path capability)
          (insert (format "\n## Source\n\n%s\n" (magent-capability-file-path capability))))))))

;;;###autoload
(defun magent-explain-current-capabilities (&optional prompt)
  "Explain capability matching for PROMPT in the current buffer context."
  (interactive)
  (let* ((resolution (magent-capability-resolve
                      (or prompt "")
                      (magent-capability-capture-context)
                      nil))
         (context (magent-capability-resolution-context resolution)))
    (magent--with-display-buffer "*Magent Capability Resolution*"
      (insert "Capability Resolution\n\n")
      (insert (format "Buffer: %s\n"
                      (or (plist-get context :buffer-name) "<none>")))
      (insert (format "File: %s\n"
                      (or (plist-get context :file-path) "<none>")))
      (insert (format "Mode: %s\n\n"
                      (or (plist-get context :major-mode) "<none>")))
      (if (null (magent-capability-resolution-matches resolution))
          (insert "No capabilities registered.\n")
        (dolist (match (magent-capability-resolution-matches resolution))
          (when (not (eq (magent-capability-match-status match) 'hidden))
            (magent-capability--insert-match match)
            (insert "\n")))))))

;;;###autoload
(defun magent-show-active-capabilities ()
  "Show the most recent capability resolution."
  (interactive)
  (if (not magent-capability--last-resolution)
      (message "Magent: no capability resolution recorded yet")
    (magent--with-display-buffer "*Magent Active Capabilities*"
      (insert "Last Capability Resolution\n\n")
      (dolist (match (magent-capability-resolution-active-capabilities
                      magent-capability--last-resolution))
        (magent-capability--insert-match match)
        (insert "\n"))
      (when-let* ((suggested (magent-capability-resolution-suggested-capabilities
                              magent-capability--last-resolution)))
        (insert "Suggested but inactive:\n\n")
        (dolist (match suggested)
          (magent-capability--insert-match match)
          (insert "\n"))))))

(provide 'magent-capability)
;;; magent-capability.el ends here
