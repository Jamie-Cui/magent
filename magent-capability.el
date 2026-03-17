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
;;
;; Capability lifecycle:
;; 1. Discovery: file-backed and built-in capability definitions are
;;    registered into `magent-capability--registry'.
;; 2. Scoring: each request is resolved against structured turn context
;;    and prompt text via deterministic additive match helpers.
;; 3. Suggestion: matches above the suggestion threshold become visible
;;    in resolver output for inspection and UI/debug surfaces.
;; 4. Activation: top scoring `active' disclosure matches up to
;;    `magent-capability-max-active' contribute linked skills.

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
  family
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
  "Match result for a capability resolution pass.

`reasons' is a stable list of human-readable score contributions.
`details' is a plist for tests and debug surfaces, for example:
  (:enabled t
   :contributions ((:kind mode-family :value org-mode :score 3)
                   (:kind keyword :value \"heading\" :score 1)))."
  capability
  (score 0)
  reasons
  details
  (status 'hidden))

(cl-defstruct (magent-capability-resolution
               (:constructor magent-capability-resolution-create))
  "Progressive disclosure result for one request.

The output shape is intentionally inspectable:
- `context' is the normalized plist the resolver scored against.
- `matches' is the full sorted list of `magent-capability-match'.
- `active-capabilities' and `suggested-capabilities' are filtered views.
- `skill-names' is the final deduplicated instruction skill list."
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

(defvar magent-capability--local-disabled-capabilities nil
  "Capability names disabled for the current Emacs session only.")

(defvar magent-capability--local-enabled-capabilities nil
  "Capability names explicitly enabled for the current Emacs session only.")

(defconst magent-capability--suggest-threshold 2
  "Minimum score required for a capability to be suggested.")

(defconst magent-capability--activate-threshold 4
  "Minimum score required for a capability to auto-activate.")

(defconst magent-capability--mode-match-score 3
  "Score contribution for a mode or mode-family match.")

(defconst magent-capability--feature-match-score 2
  "Score contribution for a loaded feature match.")

(defconst magent-capability--file-match-score 2
  "Score contribution for a file or project glob match.")

(defconst magent-capability--keyword-match-score 1
  "Score contribution for a prompt keyword match.")

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
  (let ((name (magent-capability-name capability))
        (family (magent-capability-family capability)))
    (cond
     ((member name magent-capability--local-enabled-capabilities) t)
     ((member name magent-capability--local-disabled-capabilities) nil)
     ((member name magent-disabled-capabilities) nil)
     ((and family
           (member family magent-disabled-capability-families))
      nil)
     (t t))))

(defun magent-capability-clear-local-overrides ()
  "Clear non-persistent capability overrides for the current Emacs session."
  (interactive)
  (setq magent-capability--local-disabled-capabilities nil
        magent-capability--local-enabled-capabilities nil))

(defun magent-capability-locally-disabled-p (capability-name)
  "Return non-nil when CAPABILITY-NAME is locally disabled."
  (member capability-name magent-capability--local-disabled-capabilities))

(defun magent-capability-toggle-locally (capability-name)
  "Toggle CAPABILITY-NAME for the current Emacs session only.
Returns the new symbolic state: either `enabled' or `disabled'."
  (if (magent-capability-enabled-p (or (magent-capability-get capability-name)
                                       (error "Capability '%s' not found" capability-name)))
      (progn
        (setq magent-capability--local-enabled-capabilities
              (delete capability-name magent-capability--local-enabled-capabilities))
        (cl-pushnew capability-name magent-capability--local-disabled-capabilities
                    :test #'equal)
        'disabled)
    (progn
      (setq magent-capability--local-disabled-capabilities
            (delete capability-name magent-capability--local-disabled-capabilities))
      (cl-pushnew capability-name magent-capability--local-enabled-capabilities
                  :test #'equal)
      'enabled)))

(defun magent-capability--mode-family (mode)
  "Return MODE and its derived-mode parents as a list."
  (let ((family nil)
        (current mode))
    (while (and current (symbolp current) (not (memq current family)))
      (push current family)
      (setq current (get current 'derived-mode-parent)))
    (nreverse family)))

(defun magent-capability-capture-context ()
  "Capture a structured request context from the current buffer.
Returns a plist or nil when the current buffer is not a useful
source for contextual capability resolution."
  (unless (or (derived-mode-p 'magent-output-mode)
              (minibufferp))
    (let* ((file-path (buffer-file-name))
           (major-mode-family (magent-capability--mode-family major-mode)))
      (list :buffer-name (buffer-name)
            :file-path file-path
            :file-extension (when file-path
                              (file-name-extension file-path))
            :major-mode major-mode
            :major-mode-family major-mode-family
            :project-root (ignore-errors (magent-project-root))
            :region-active (use-region-p)
            :buffer-modified-p (buffer-modified-p)
            :features features))))

(defun magent-capability--parse-context-prompt (prompt)
  "Extract a structured context plist from PROMPT, if present."
  (when (and (stringp prompt)
             (string-match "\\[Context: \\([^]]+\\)\\]" prompt))
    (let* ((payload (match-string 1 prompt))
           (file-path (when (string-match "file=\"\\([^\"]+\\)\"" payload)
                        (match-string 1 payload)))
           (mode-sym (when (string-match "mode=\\([^] ]+\\)" payload)
                       (intern (match-string 1 payload)))))
      (list
       :buffer-name
       (when (string-match "buffer=\"\\([^\"]+\\)\"" payload)
         (match-string 1 payload))
       :file-path file-path
       :file-extension (when file-path (file-name-extension file-path))
       :major-mode mode-sym
       :major-mode-family (when mode-sym
                            (magent-capability--mode-family mode-sym))
       :region-active
       (and (string-match "region=[0-9]+-[0-9]+" payload) t)
       :buffer-modified-p
       (and (string-match "modified=\\(true\\|false\\)" payload)
            (string= (match-string 1 payload) "true"))))))

(defun magent-capability--merge-context (request-context prompt)
  "Merge REQUEST-CONTEXT with structured context extracted from PROMPT."
  (let ((prompt-context (magent-capability--parse-context-prompt prompt)))
    (list :buffer-name (or (plist-get request-context :buffer-name)
                           (plist-get prompt-context :buffer-name))
          :file-path (or (plist-get request-context :file-path)
                         (plist-get prompt-context :file-path))
          :file-extension (or (plist-get request-context :file-extension)
                              (plist-get prompt-context :file-extension)
                              (when-let* ((path (or (plist-get request-context :file-path)
                                                    (plist-get prompt-context :file-path))))
                                (file-name-extension path)))
          :major-mode (or (plist-get request-context :major-mode)
                          (plist-get prompt-context :major-mode))
          :major-mode-family (or (plist-get request-context :major-mode-family)
                                 (plist-get prompt-context :major-mode-family)
                                 (when-let* ((mode (or (plist-get request-context :major-mode)
                                                       (plist-get prompt-context :major-mode))))
                                   (magent-capability--mode-family mode)))
          :project-root (plist-get request-context :project-root)
          :region-active (or (plist-get request-context :region-active)
                             (plist-get prompt-context :region-active))
          :buffer-modified-p (if (plist-member request-context :buffer-modified-p)
                                 (plist-get request-context :buffer-modified-p)
                               (plist-get prompt-context :buffer-modified-p))
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

(defun magent-capability--mode-match (capability context)
  "Return a plist describing the mode contribution for CAPABILITY and CONTEXT."
  (let* ((mode (plist-get context :major-mode))
         (mode-family (plist-get context :major-mode-family))
         (exact (and mode
                     (memq mode (magent-capability-modes capability))))
         (family (and (not exact)
                      mode-family
                      (cl-find-if (lambda (candidate)
                                    (memq candidate mode-family))
                                  (magent-capability-modes capability)))))
    (cond
     (exact
      (list :kind 'mode
            :value mode
            :score magent-capability--mode-match-score
            :label (format "mode=%s" mode)))
     (family
      (list :kind 'mode-family
            :value family
            :score magent-capability--mode-match-score
            :label (format "mode-family=%s" family))))))

(defun magent-capability--feature-match (capability context)
  "Return a plist describing the feature contribution for CAPABILITY and CONTEXT."
  (when-let* ((matched-feature
               (cl-find-if (lambda (feature)
                             (memq feature (plist-get context :features)))
                           (magent-capability-features capability))))
    (list :kind 'feature
          :value matched-feature
          :score magent-capability--feature-match-score
          :label (format "feature=%s" matched-feature))))

(defun magent-capability--file-match (capability context)
  "Return a plist describing the file contribution for CAPABILITY and CONTEXT."
  (let ((file (plist-get context :file-path))
        (project-root (plist-get context :project-root)))
    (when-let* ((matched-file
                 (cl-find-if (lambda (pattern)
                               (or (magent-capability--glob-match-p pattern file)
                                   (magent-capability--glob-match-p pattern project-root)))
                             (magent-capability-files capability))))
      (list :kind 'file
            :value matched-file
            :score magent-capability--file-match-score
            :label (format "path~=%s" matched-file)))))

(defun magent-capability--keyword-match (capability prompt)
  "Return a plist describing the prompt keyword contribution for CAPABILITY."
  (when-let* ((matched-keyword
               (cl-find-if (lambda (keyword)
                             (magent-capability--keyword-match-p keyword prompt))
                           (magent-capability-prompt-keywords capability))))
    (list :kind 'keyword
          :value matched-keyword
          :score magent-capability--keyword-match-score
          :label (format "keyword=%s" matched-keyword))))

(defun magent-capability--score (capability prompt context)
  "Return a `magent-capability-match' for CAPABILITY."
  (let* ((contributions (delq nil
                              (list (magent-capability--mode-match capability context)
                                    (magent-capability--feature-match capability context)
                                    (magent-capability--file-match capability context)
                                    (magent-capability--keyword-match capability prompt))))
         (score (apply #'+ 0 (mapcar (lambda (entry) (plist-get entry :score))
                                     contributions)))
         (enabled (magent-capability-enabled-p capability))
         (status
          (cond
           ((or (not enabled)
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
       :reasons (mapcar (lambda (entry) (plist-get entry :label)) contributions)
       :details (list :enabled enabled
                      :contributions contributions)
       :status status)))

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

(defun magent-capability-match-to-plist (match)
  "Return a machine-readable plist for MATCH."
  (let ((capability (magent-capability-match-capability match)))
    (list :name (magent-capability-name capability)
          :title (magent-capability-title capability)
          :description (magent-capability-description capability)
          :status (magent-capability-match-status match)
          :score (magent-capability-match-score match)
          :reasons (copy-sequence (magent-capability-match-reasons match))
          :skills (copy-sequence (magent-capability-skills capability))
          :disclosure (magent-capability-disclosure capability)
          :risk (magent-capability-risk capability)
          :source-kind (magent-capability-source-kind capability)
          :source-name (magent-capability-source-name capability))))

(defun magent-capability-resolution-to-plist (resolution)
  "Return a machine-readable plist for RESOLUTION."
  (when resolution
    (list :prompt (magent-capability-resolution-prompt resolution)
          :context (copy-tree (magent-capability-resolution-context resolution))
          :explicit-skills
          (copy-sequence (magent-capability-resolution-explicit-skills resolution))
          :skill-names
          (copy-sequence (magent-capability-resolution-skill-names resolution))
          :active-capabilities
          (mapcar (lambda (match)
                    (magent-capability-name
                     (magent-capability-match-capability match)))
                  (magent-capability-resolution-active-capabilities resolution))
          :suggested-capabilities
          (mapcar (lambda (match)
                    (magent-capability-name
                     (magent-capability-match-capability match)))
                  (magent-capability-resolution-suggested-capabilities resolution))
          :matches
          (mapcar #'magent-capability-match-to-plist
                  (magent-capability-resolution-matches resolution)))))

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
    (when (magent-capability-family capability)
      (insert (format "  Family: %s\n" (magent-capability-family capability))))
    (when (magent-capability-description capability)
      (insert (format "  %s\n" (magent-capability-description capability))))
    (when (magent-capability-skills capability)
      (insert (format "  Skills: %s\n"
                      (mapconcat #'identity (magent-capability-skills capability) ", "))))
    (when (magent-capability-match-reasons match)
      (insert (format "  Reasons: %s\n"
                      (mapconcat #'identity
                                 (magent-capability-match-reasons match)
                                 ", "))))
    (when-let* ((details (magent-capability-match-details match))
                (contributions (plist-get details :contributions)))
      (insert "  Debug: ")
      (insert (mapconcat (lambda (entry)
                           (format "%s:+%d"
                                   (plist-get entry :kind)
                                   (plist-get entry :score)))
                         contributions
                         ", "))
      (insert "\n"))))

(defun magent-capability--insert-resolution (resolution &optional include-hidden)
  "Insert RESOLUTION into current buffer.
When INCLUDE-HIDDEN is non-nil, include hidden matches too."
  (let ((context (magent-capability-resolution-context resolution)))
    (insert (format "Buffer: %s\n"
                    (or (plist-get context :buffer-name) "<none>")))
    (insert (format "File: %s\n"
                    (or (plist-get context :file-path) "<none>")))
    (insert (format "Mode: %s\n"
                    (or (plist-get context :major-mode) "<none>")))
    (insert (format "Mode family: %s\n"
                    (or (when-let* ((family (plist-get context :major-mode-family)))
                          (mapconcat #'symbol-name family ", "))
                        "<none>")))
    (insert (format "File extension: %s\n"
                    (or (plist-get context :file-extension) "<none>")))
    (insert (format "Modified: %s\n\n"
                    (if (plist-get context :buffer-modified-p) "yes" "no")))
    (insert (format "Explicit skills: %s\n"
                    (or (and (magent-capability-resolution-explicit-skills resolution)
                             (mapconcat #'identity
                                        (magent-capability-resolution-explicit-skills resolution)
                                        ", "))
                        "<none>")))
    (insert (format "Final skills: %s\n\n"
                    (or (and (magent-capability-resolution-skill-names resolution)
                             (mapconcat #'identity
                                        (magent-capability-resolution-skill-names resolution)
                                        ", "))
                        "<none>")))
    (if (null (magent-capability-resolution-matches resolution))
        (insert "No capabilities registered.\n")
      (dolist (match (magent-capability-resolution-matches resolution))
        (when (or include-hidden
                  (not (eq (magent-capability-match-status match) 'hidden)))
          (magent-capability--insert-match match)
          (insert "\n"))))))

(defun magent-capability-resolution-summary (resolution)
  "Return a short human-readable summary for RESOLUTION, or nil."
  (when resolution
    (let ((active (mapcar (lambda (match)
                            (magent-capability-name
                             (magent-capability-match-capability match)))
                          (magent-capability-resolution-active-capabilities resolution)))
          (suggested (mapcar (lambda (match)
                               (magent-capability-name
                                (magent-capability-match-capability match)))
                             (magent-capability-resolution-suggested-capabilities resolution))))
      (when (or active suggested)
        (string-join
         (delq nil
               (list (when active
                       (format "Auto capabilities: %s"
                               (mapconcat #'identity active ", ")))
                     (when suggested
                       (format "Suggested: %s"
                               (mapconcat #'identity suggested ", ")))))
         " | ")))))

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
         (buffer-name "*Magent Capability Resolution*"))
    (setq magent-capability--last-resolution resolution)
    (magent--with-display-buffer buffer-name
      (insert "Capability Resolution\n\n")
      (magent-capability--insert-resolution resolution nil))))

;;;###autoload
(defun magent-list-capabilities-for-current-context (&optional prompt)
  "List all capabilities for the current context and optional PROMPT."
  (interactive)
  (let ((resolution (magent-capability-resolve
                     (or prompt "")
                     (magent-capability-capture-context)
                     nil)))
    (magent--with-display-buffer "*Magent Capability Resolution*"
      (insert "Capabilities For Current Context\n\n")
      (magent-capability--insert-resolution resolution t))))

;;;###autoload
(defun magent-explain-last-capability-resolution ()
  "Show the most recently recorded capability resolution in detail."
  (interactive)
  (if (not magent-capability--last-resolution)
      (message "Magent: no capability resolution recorded yet")
    (magent--with-display-buffer "*Magent Last Capability Resolution*"
      (insert "Last Capability Resolution\n\n")
      (magent-capability--insert-resolution
       magent-capability--last-resolution
       t))))

;;;###autoload
(defun magent-toggle-capability-locally (capability-name)
  "Toggle CAPABILITY-NAME on or off for the current Emacs session only."
  (interactive
   (list (completing-read "Toggle capability locally: "
                          (magent-capability-list) nil t)))
  (let ((state (magent-capability-toggle-locally capability-name)))
    (message "Magent: capability '%s' locally %s"
             capability-name
             (pcase state
               ('enabled "enabled")
               (_ "disabled")))))

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
