;;; magent-command.el --- Elisp-native slash commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6

;;; Commentary:

;; Public registration and invocation API for Magent slash commands.  A
;; command is an explicit user action.  It may declare one ordinary agent turn
;; or use an advanced handler to own a longer asynchronous workflow.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-util)
(require 'magent-config)
(require 'magent-log)
(require 'magent-prompt)
(require 'magent-protocol)
(require 'magent-runtime-api)
(require 'magent-session)

(declare-function magent-command-builtins-register "magent-command-builtins")
(declare-function magent-runtime-active-project-scope "magent-runtime")
(declare-function magent-skill-description "magent-skills")
(declare-function magent-skill-file-path "magent-skills")
(declare-function magent-skill-requires-project "magent-skills")
(declare-function magent-skill-source-layer "magent-skills")
(declare-function magent-skill-source-scope "magent-skills")
(declare-function magent-skill-tools "magent-skills")
(declare-function magent-skills-command-names "magent-skills")
(declare-function magent-skills-dedupe-names "magent-skills")
(declare-function magent-skills-default-prompt "magent-skills")
(declare-function magent-skills-get "magent-skills")

(cl-defstruct (magent-command-spec
               (:constructor magent-command-spec-create)
               (:copier nil))
  "One registered slash command definition."
  name
  description
  turn
  handler
  owner
  source-layer
  source-scope
  requires-project
  required-tools
  registration-id
  sequence)

(cl-defstruct (magent-command-turn-spec
               (:constructor magent-command-turn-spec-create)
               (:copier nil))
  "Declarative model input and options for one standard slash command turn.
PROMPT is user-role model input.  BUFFERS is a popwin-style list of buffer
patterns whose invocation-time snapshots are attached as model-visible user
resources.  The invocation owns frontend request context, including project
scope and attached resource paths.  SKILLS and AGENT select runtime behavior,
and APPEND-ARGUMENT-P controls slash argument expansion."
  prompt
  buffers
  skills
  agent
  (append-argument-p t))

(cl-defstruct (magent-command-invocation
               (:constructor magent-command-invocation-create)
               (:copier nil))
  "Runtime state for one slash command invocation."
  id
  spec
  runtime-session
  raw-input
  argument
  request-context
  resource-blocks
  observer
  approval-provider
  completion-function
  submission-adapter
  cancel-function
  submission-ids
  (status 'active)
  deferred-p
  result)

(defvar magent-command--registry nil
  "Layered list of registered `magent-command-spec' objects.")

(defvar magent-command--sequence 0
  "Monotonic sequence used to resolve same-layer registrations.")

(defvar magent-command--active-invocations (make-hash-table :test #'eq)
  "Active command invocations keyed by runtime session identity.")

(defvar magent-command-registry-changed-hook nil
  "Hook run after the effective slash command registry may have changed.")

(defvar magent-command--suppress-registry-hooks nil
  "Non-nil while applying one atomic command registry refresh.")

(defvar magent-command--allow-core-registration nil
  "Non-nil only while Magent registers reserved core commands.")

(defun magent-command--registry-changed ()
  "Notify command registry consumers unless notifications are suppressed."
  (unless magent-command--suppress-registry-hooks
    (run-hooks 'magent-command-registry-changed-hook)))

(defconst magent-command--layer-ranks
  '((builtin . 10)
    (package . 20)
    (user . 30)
    (project . 40)
    (core . 50))
  "Precedence ranks for command definition layers.")

(defun magent-command--normalize-name (name)
  "Return NAME as a validated slash command string."
  (let ((value (if (symbolp name) (symbol-name name) name)))
    (unless (and (stringp value)
                 (string-match-p "\\`[[:alnum:]_-]+\\'" value))
      (error "Invalid Magent command name: %S" name))
    value))

(defun magent-command--layer-rank (layer)
  "Return numeric precedence for command LAYER."
  (or (alist-get layer magent-command--layer-ranks)
      (error "Invalid Magent command source layer: %S" layer)))

(defun magent-command--canonical-scope (scope)
  "Return canonical project origin for SCOPE, or nil for global scope."
  (let ((origin (magent-session-scope-origin scope)))
    (cond
     ((or (null origin) (eq origin 'global)) nil)
     ((stringp origin)
      (condition-case nil
          (file-truename (directory-file-name origin))
        (error (directory-file-name (expand-file-name origin)))))
     (t origin))))

(defun magent-command--resolution-scope (&optional scope)
  "Return canonical command resolution scope for optional SCOPE.
When SCOPE is nil, use the currently active project overlay."
  (magent-command--canonical-scope
   (or scope
       (and (fboundp 'magent-runtime-active-project-scope)
            (magent-runtime-active-project-scope))
       'global)))

(defun magent-command--visible-in-scope-p (spec scope)
  "Return non-nil when command SPEC is visible in canonical SCOPE."
  (let ((source-scope
         (magent-command--canonical-scope
          (magent-command-spec-source-scope spec))))
    (or (null source-scope)
        (equal source-scope scope))))

(defun magent-command--same-owner-p (left right)
  "Return non-nil when command specs LEFT and RIGHT share an owner slot."
  (and (equal (magent-command-spec-name left)
              (magent-command-spec-name right))
       (eq (magent-command-spec-source-layer left)
           (magent-command-spec-source-layer right))
       (equal (magent-command-spec-source-scope left)
              (magent-command-spec-source-scope right))
       (equal (magent-command-spec-owner left)
              (magent-command-spec-owner right))))

(defun magent-command--default-owner ()
  "Return a stable default owner for the current registration site."
  (or load-file-name buffer-file-name 'anonymous))

(cl-defun magent-command-register
    (name &key description turn handler owner (source-layer 'package)
          source-scope requires-project required-tools)
  "Register slash command NAME and return its registration token.

Exactly one of TURN and HANDLER must be provided.  TURN is a
`magent-command-turn-spec' or a function receiving one
`magent-command-invocation' and returning a turn spec.  HANDLER receives the
invocation directly and must complete it synchronously or call
`magent-command-defer' before returning.  SOURCE-LAYER is one of `builtin',
`package', `user', `project', or reserved `core'.  OWNER and SOURCE-SCOPE
identify reloadable definitions.  REQUIRES-PROJECT and REQUIRED-TOOLS are
checked before model work or HANDLER execution."
  (unless (xor (not (null turn)) (not (null handler)))
    (error "Magent command %S requires exactly one of :turn or :handler" name))
  (when (and turn
             (not (or (magent-command-turn-spec-p turn)
                      (functionp turn))))
    (error "Expected Magent command turn spec or builder, got: %S" turn))
  (when (magent-command-turn-spec-p turn)
    (magent-command--validate-turn-spec turn))
  (when (and handler (not (functionp handler)))
    (error "Expected Magent command handler function, got: %S" handler))
  (let* ((key (magent-command--normalize-name name))
         (layer (or source-layer 'package))
         (_rank (magent-command--layer-rank layer))
         (registration-owner (or owner (magent-command--default-owner)))
         (registration-scope
          (magent-command--canonical-scope source-scope))
         (spec (magent-command-spec-create
                :name key
                :description description
                :turn turn
                :handler handler
                :owner registration-owner
                :source-layer layer
                :source-scope registration-scope
                :requires-project requires-project
                :required-tools
                (mapcar (lambda (tool)
                          (if (symbolp tool) tool (intern tool)))
                        required-tools)
                :registration-id (magent-protocol-generate-id "command")
                :sequence (cl-incf magent-command--sequence))))
    (when (and (eq layer 'core)
               (not magent-command--allow-core-registration))
      (error "The Magent command core layer is reserved"))
    (when-let* ((collision
                 (cl-find-if
                  (lambda (candidate)
                    (and (equal key (magent-command-spec-name candidate))
                         (eq layer
                             (magent-command-spec-source-layer candidate))
                         (equal registration-scope
                                (magent-command-spec-source-scope candidate))
                         (not (magent-command--same-owner-p candidate spec))))
                  magent-command--registry)))
      (magent-log
       "WARN slash command /%s layer %s from %S shadows same-layer owner %S"
       key layer registration-owner (magent-command-spec-owner collision)))
    (setq magent-command--registry
          (cl-remove-if (lambda (candidate)
                          (magent-command--same-owner-p candidate spec))
                        magent-command--registry))
    (push spec magent-command--registry)
    (magent-command--registry-changed)
    spec))

(defun magent-command-unregister (registration)
  "Unregister exact command REGISTRATION and return non-nil when removed."
  (unless (magent-command-spec-p registration)
    (error "Expected a Magent command registration, got: %S" registration))
  (when (and (eq (magent-command-spec-source-layer registration) 'core)
             (not magent-command--allow-core-registration))
    (error "The Magent command core layer is reserved"))
  (let ((before (length magent-command--registry)))
    (setq magent-command--registry
          (delq registration magent-command--registry))
    (when (/= before (length magent-command--registry))
      (magent-command--registry-changed)
      t)))

(defun magent-command-remove-source (source-layer &optional source-scope owner)
  "Remove registrations matching SOURCE-LAYER, SOURCE-SCOPE, and OWNER.
Nil SOURCE-SCOPE or OWNER acts as a wildcard.  Return the removal count."
  (when (and (eq source-layer 'core)
             (not magent-command--allow-core-registration))
    (error "The Magent command core layer is reserved"))
  (let ((before (length magent-command--registry)))
    (setq magent-command--registry
          (cl-remove-if
           (lambda (spec)
             (and (eq source-layer (magent-command-spec-source-layer spec))
                  (or (null source-scope)
                      (equal source-scope
                             (magent-command-spec-source-scope spec)))
                  (or (null owner)
                      (equal owner (magent-command-spec-owner spec)))))
           magent-command--registry))
    (let ((removed (- before (length magent-command--registry))))
      (when (> removed 0)
        (magent-command--registry-changed))
      removed)))

(defun magent-command--better-spec-p (left right)
  "Return non-nil when LEFT has precedence over RIGHT."
  (let ((left-rank
         (magent-command--layer-rank
          (magent-command-spec-source-layer left)))
        (right-rank
         (magent-command--layer-rank
          (magent-command-spec-source-layer right))))
    (if (/= left-rank right-rank)
        (> left-rank right-rank)
      (> (magent-command-spec-sequence left)
         (magent-command-spec-sequence right)))))

(defun magent-command-get (name &optional scope)
  "Return effective slash command NAME for SCOPE, or nil.
When SCOPE is nil, resolve against the currently active project overlay."
  (let ((key (magent-command--normalize-name name))
        (resolution-scope (magent-command--resolution-scope scope))
        winner)
    (dolist (spec magent-command--registry winner)
      (when (and (equal key (magent-command-spec-name spec))
                 (magent-command--visible-in-scope-p spec resolution-scope)
                 (or (null winner)
                     (magent-command--better-spec-p spec winner)))
        (setq winner spec)))))

(defun magent-command-list (&optional scope)
  "Return effective slash command specs for SCOPE sorted by name.
When SCOPE is nil, resolve against the currently active project overlay."
  (let ((resolution-scope (magent-command--resolution-scope scope))
        names)
    (dolist (spec magent-command--registry)
      (when (magent-command--visible-in-scope-p spec resolution-scope)
        (cl-pushnew (magent-command-spec-name spec) names :test #'equal)))
    (mapcar (lambda (name)
              (magent-command-get name (or resolution-scope 'global)))
            (sort names #'string<))))

(defun magent-command--skill-source-layer (skill)
  "Return command source layer corresponding to SKILL."
  (pcase (magent-skill-source-layer skill)
    ('builtin 'builtin)
    ('project 'project)
    ('user 'user)
    (_ 'package)))

(defun magent-command--skill-adapter-owner-p (owner)
  "Return non-nil when OWNER identifies a generated skill adapter."
  (and (consp owner) (eq (car owner) 'skill-adapter)))

(defun magent-command-refresh-skill-adapters (&optional scope)
  "Rebuild compatibility commands for effective skills in SCOPE.
Global adapters and each visited project scope are cached independently so
live sessions can resolve commands without switching the active overlay."
  (require 'magent-skills)
  (let ((target-scope (magent-command--canonical-scope scope))
        (magent-command--suppress-registry-hooks t))
    (setq magent-command--registry
          (cl-remove-if
           (lambda (spec)
             (and
              (magent-command--skill-adapter-owner-p
               (magent-command-spec-owner spec))
              (equal target-scope
                     (magent-command--canonical-scope
                      (magent-command-spec-source-scope spec)))))
           magent-command--registry))
    (dolist (name (magent-skills-command-names))
      ;; Give every generated turn builder its own lexical skill-name binding.
      (let ((skill-name name))
        (when-let* ((skill (magent-skills-get skill-name))
                    (prompt (magent-skills-default-prompt skill-name))
                    (scope-matches
                     (equal target-scope
                            (magent-command--canonical-scope
                             (magent-skill-source-scope skill)))))
          (condition-case err
              (magent-command-register
               skill-name
               :description (magent-skill-description skill)
               :turn
               (lambda (invocation)
                 (magent-command-turn-spec-create
                  :prompt prompt
                  :skills
                  (magent-skills-dedupe-names
                   (append
                    (magent-runtime-session-pending-skills
                     (magent-command-invocation-runtime-session invocation))
                    (list skill-name)))))
               :owner (list 'skill-adapter
                            (or (magent-skill-file-path skill) skill-name))
               :source-layer (magent-command--skill-source-layer skill)
               :source-scope (magent-skill-source-scope skill)
               :requires-project (magent-skill-requires-project skill)
               :required-tools (magent-skill-tools skill))
            (error
             (magent-log "WARN skipped skill command adapter %S: %s"
                         skill-name (error-message-string err))))))))
  (magent-command--registry-changed)
  (magent-command-list (or scope 'global)))

(defun magent-command-load-project-scope (scope)
  "Refresh command adapters after loading project SCOPE skills."
  (magent-command-refresh-skill-adapters scope))

(defun magent-command-remove-project-scope (scope)
  "Retain cached command adapters while deactivating project SCOPE.
The next load of SCOPE replaces its cached adapters from current skill data."
  (ignore scope))

(defun magent-command-initialize-static ()
  "Register bundled commands and compatibility skill commands."
  (require 'magent-command-builtins)
  (magent-command-builtins-register)
  (magent-command-refresh-skill-adapters))

(defun magent-command-parse (input &optional scope)
  "Parse slash command INPUT for SCOPE and return (SPEC . ARGUMENT), or nil."
  (let ((trimmed (string-trim (or input ""))))
    (when (string-match "\\`/\\([[:alnum:]_-]+\\)\\(?:[[:space:]]+\\(.*\\)\\)?\\'"
                        trimmed)
      (when-let* ((spec (magent-command-get (match-string 1 trimmed) scope)))
        (cons spec (string-trim (or (match-string 2 trimmed) "")))))))

(defun magent-command--notify (invocation type &rest props)
  "Send invocation event TYPE with PROPS to INVOCATION's observer."
  (when-let* ((observer (magent-command-invocation-observer invocation)))
    (condition-case err
        (funcall observer
                 (append (list :type type
                               :command
                               (magent-command-spec-name
                                (magent-command-invocation-spec invocation))
                               :invocation-id
                               (magent-command-invocation-id invocation))
                         props))
      (error
       (magent-log "ERROR command observer failed: %s"
                   (error-message-string err))))))

(defun magent-command-progress (invocation message)
  "Report progress MESSAGE for active command INVOCATION."
  (unless (eq (magent-command-invocation-status invocation) 'active)
    (error "Magent command invocation is no longer active"))
  (magent-command--notify invocation 'command-progress :text message)
  invocation)

(defun magent-command-set-cancel-function (invocation function)
  "Set additional cancellation FUNCTION for INVOCATION."
  (unless (or (null function) (functionp function))
    (error "Expected a cancellation function, got: %S" function))
  (setf (magent-command-invocation-cancel-function invocation) function)
  invocation)

(defun magent-command-defer (invocation)
  "Mark INVOCATION as asynchronously owned by its handler."
  (unless (eq (magent-command-invocation-status invocation) 'active)
    (error "Cannot defer a completed Magent command invocation"))
  (setf (magent-command-invocation-deferred-p invocation) t)
  invocation)

(defun magent-command--claim-finish (invocation status result)
  "Claim terminal STATUS and RESULT for active INVOCATION."
  (when (eq (magent-command-invocation-status invocation) 'active)
    (setf (magent-command-invocation-status invocation) status
          (magent-command-invocation-result invocation) result)
    t))

(defun magent-command--publish-finish (invocation)
  "Publish the terminal state already claimed by INVOCATION."
  (let ((status (magent-command-invocation-status invocation))
        (result (magent-command-invocation-result invocation))
        (runtime-session
         (magent-command-invocation-runtime-session invocation)))
    ;; A stale callback must not clear a newer invocation installed for the
    ;; same runtime session after live reload or extension-managed recovery.
    (when (eq (gethash runtime-session magent-command--active-invocations)
              invocation)
      (remhash runtime-session magent-command--active-invocations))
    (magent-command--notify invocation 'command-completed
                            :status status :result result)
    (when-let* ((completion
                 (magent-command-invocation-completion-function invocation)))
      (condition-case err
          (funcall completion status result)
        (error
         (magent-log "ERROR command completion callback failed: %s"
                     (error-message-string err)))))
    t))

(defun magent-command-finish (invocation status result)
  "Finish INVOCATION once with STATUS and RESULT.
STATUS must be `completed', `failed', or `cancelled'."
  (unless (memq status '(completed failed cancelled))
    (error "Invalid Magent command completion status: %S" status))
  (when (magent-command--claim-finish invocation status result)
    (magent-command--publish-finish invocation)))

(defun magent-command-complete (invocation &optional result)
  "Complete INVOCATION successfully with RESULT."
  (magent-command-finish invocation 'completed (or result "")))

(defun magent-command--failure-result (error)
  "Return normalized command failure result for ERROR."
  (if (magent-agent-result-p error)
      error
    (magent-agent-result-failed error (list :status 'command-error))))

(defun magent-command-fail (invocation error)
  "Fail INVOCATION with ERROR."
  (magent-command-finish
   invocation 'failed (magent-command--failure-result error)))

(defun magent-command--cleanup-owned-work (invocation &optional force-runtime)
  "Cancel work owned by INVOCATION without publishing terminal state.
Run runtime cancellation when FORCE-RUNTIME is non-nil or the invocation has
recorded submissions.  Cleanup errors are logged and never hide the original
terminal result."
  (let ((cancel (magent-command-invocation-cancel-function invocation))
        (submission-ids
         (magent-command-invocation-submission-ids invocation)))
    ;; Clear ownership first so synchronous cancellation callbacks cannot run
    ;; either cleanup path more than once.
    (setf (magent-command-invocation-cancel-function invocation) nil
          (magent-command-invocation-submission-ids invocation) nil)
    (when cancel
      (condition-case err
          (funcall cancel)
        ((error quit)
         (magent-log "ERROR command cancellation cleanup failed: %s"
                     (error-message-string err)))))
    (when (or force-runtime submission-ids)
      (condition-case err
          (magent-runtime-cancel
           (magent-command-invocation-runtime-session invocation))
        ((error quit)
         (magent-log "ERROR command runtime cancellation failed: %s"
                     (error-message-string err))))))
  invocation)

(defun magent-command--finish-with-cleanup
    (invocation status result &optional force-runtime)
  "Claim INVOCATION STATUS, clean up owned work, then publish RESULT."
  (when (magent-command--claim-finish invocation status result)
    (magent-command--cleanup-owned-work invocation force-runtime)
    (magent-command--publish-finish invocation)))

(defun magent-command--fail-with-cleanup (invocation error)
  "Fail INVOCATION with ERROR after cancelling work it already submitted."
  (magent-command--finish-with-cleanup
   invocation 'failed (magent-command--failure-result error)))

(defun magent-command-cancel (invocation)
  "Cancel active command INVOCATION and return non-nil when cancelled."
  (magent-command--finish-with-cleanup
   invocation 'cancelled
   (magent-agent-result-failed
    "Command cancelled" (list :status 'cancelled))
   t))

(defun magent-command-cancel-session (runtime-session)
  "Cancel the active command for RUNTIME-SESSION, if any."
  (when-let* ((invocation
              (gethash runtime-session magent-command--active-invocations)))
    (magent-command-cancel invocation)))

(defun magent-command--plist-p (value)
  "Return non-nil when VALUE is a proper keyword plist or nil."
  (and (proper-list-p value)
       (zerop (% (length value) 2))
       (cl-loop for (key _item) on value by #'cddr
                always (keywordp key))))

(defun magent-command--validate-invocation (invocation)
  "Validate request context and project requirements for INVOCATION."
  (let* ((spec (magent-command-invocation-spec invocation))
         (runtime-session
          (magent-command-invocation-runtime-session invocation))
         (origin
          (magent-session-scope-origin
           (magent-runtime-session-scope runtime-session))))
    (unless (magent-command--plist-p
             (magent-command-invocation-request-context invocation))
      (error "Expected Magent command invocation request context plist, got: %S"
             (magent-command-invocation-request-context invocation)))
    (when (and (magent-command-spec-requires-project spec)
               (not (stringp origin)))
      (user-error "Command /%s requires a project workspace"
                  (magent-command-spec-name spec)))))

(defun magent-command--validate-required-tools (invocation &optional agent)
  "Validate INVOCATION's required tools for optional turn AGENT."
  (let* ((spec (magent-command-invocation-spec invocation))
         (required (magent-command-spec-required-tools spec))
         (runtime-session
          (magent-command-invocation-runtime-session invocation))
         (available
          (when (or agent required)
            (magent-runtime-session-available-tool-names
             runtime-session agent)))
         (missing (and required
                       (cl-set-difference required available))))
    (when missing
      (user-error "Command /%s requires unavailable tools: %s"
                  (magent-command-spec-name spec)
                  (mapconcat #'symbol-name missing ", ")))))

(defun magent-command-turn-metadata (invocation)
  "Return canonical ledger metadata for command INVOCATION."
  (list :source 'magent-command
        :command
        (magent-command-spec-name
         (magent-command-invocation-spec invocation))
        :command-invocation-id
        (magent-command-invocation-id invocation)
        :command-argument
        (magent-command-invocation-argument invocation)
        :command-input
        (magent-command-invocation-raw-input invocation)))

(cl-defun magent-command-submit
    (invocation prompt &key skills agent request-context turn-metadata
                resource-blocks on-complete)
  "Submit one agent PROMPT owned by command INVOCATION.
REQUEST-CONTEXT supplies optional request-only runtime hints for an advanced
handler.  It is not serialized as model input and takes precedence over the
frontend context captured by INVOCATION.  Standard declarative turns inherit
the invocation context without adding command-local hints.  RESOURCE-BLOCKS
are additional model-visible user resources inserted before frontend
attachments.  TURN-METADATA remains available to advanced handlers only.
ON-COMPLETE receives the normal runtime STATUS and RESULT."
  (unless (eq (magent-command-invocation-status invocation) 'active)
    (error "Cannot submit from a completed Magent command invocation"))
  (unless (magent-command--plist-p request-context)
    (error "Expected Magent command request context plist, got: %S"
           request-context))
  (let* ((adapter (magent-command-invocation-submission-adapter invocation))
         (additional-resources (append resource-blocks nil))
         (frontend-resources
          (append (magent-command-invocation-resource-blocks invocation) nil))
         (default-blocks
          (and (or additional-resources frontend-resources)
               (vconcat
                (cons `((type . "text") (text . ,prompt))
                      (append additional-resources frontend-resources)))))
         (adapted
          (if adapter
              (funcall adapter prompt additional-resources)
            (list :prompt prompt :content-blocks default-blocks)))
         (effective-prompt (or (plist-get adapted :prompt) prompt))
         (content-blocks (plist-get adapted :content-blocks))
         (effective-request-context
          (append request-context
                  (magent-command-invocation-request-context invocation)))
         (metadata
          (append
           (magent-command-turn-metadata invocation)
           turn-metadata
           (and content-blocks (list :content-blocks content-blocks))))
         (submission-id
          (magent-runtime-submit
           (magent-command-invocation-runtime-session invocation)
           effective-prompt
           :skills skills
           :agent agent
           :context effective-request-context
           :turn-metadata metadata
           :observer (magent-command-invocation-observer invocation)
           :approval-provider
           (magent-command-invocation-approval-provider invocation)
           :on-complete (or on-complete #'ignore))))
    (push submission-id
          (magent-command-invocation-submission-ids invocation))
    submission-id))

(cl-defun magent-command-submit-step
    (invocation prompt callback &key skills agent request-context turn-metadata
                resource-blocks)
  "Submit one workflow PROMPT step for INVOCATION, then call CALLBACK.
CALLBACK receives runtime STATUS and RESULT while the completed step still
owns the global FIFO lease.  It may therefore call this function again to
queue the next step without another backend advancing between the two steps.
REQUEST-CONTEXT has the same request-only semantics as in
`magent-command-submit'.  RESOURCE-BLOCKS has the same model-visible
semantics."
  (unless (functionp callback)
    (error "Expected a Magent command step callback, got: %S" callback))
  (magent-command-submit
   invocation prompt
   :skills skills
   :agent agent
   :request-context request-context
   :turn-metadata turn-metadata
   :resource-blocks resource-blocks
   :on-complete
   (lambda (status result)
     (condition-case err
         (funcall callback status result)
       (error
        (magent-command--fail-with-cleanup
         invocation (error-message-string err)))))))

(defun magent-command--normalize-buffer-config (entry)
  "Return normalized popwin-style command buffer configuration ENTRY."
  (let* ((bare-p (or (bufferp entry)
                     (stringp entry)
                     (symbolp entry)
                     (functionp entry)))
         (pattern (if bare-p entry (car-safe entry)))
         (keywords (if bare-p nil (cdr-safe entry))))
    (unless (or bare-p
                (and (consp entry)
                     (magent-command--plist-p keywords)))
      (error "Invalid Magent command buffer configuration: %S" entry))
    (unless (or (bufferp pattern)
                (stringp pattern)
                (symbolp pattern)
                (functionp pattern))
      (error "Invalid Magent command buffer pattern: %S" pattern))
    (cl-loop for (key _value) on keywords by #'cddr
             unless (memq key '(:required-p :regexp :predicate
                                 :project-only-p))
             do (error "Unknown Magent command buffer keyword: %S" key))
    (dolist (key '(:required-p :regexp :predicate :project-only-p))
      (when (and (plist-member keywords key)
                 (not (memq (plist-get keywords key) '(nil t))))
        (error "Expected Magent command buffer %S boolean, got: %S"
               key (plist-get keywords key))))
    (when (and (plist-get keywords :regexp)
               (not (stringp pattern)))
      (error "Magent command :regexp requires a string pattern: %S" pattern))
    (when (and (plist-get keywords :predicate)
               (not (and (symbolp pattern) (fboundp pattern))))
      (error "Magent command :predicate requires a function symbol: %S"
             pattern))
    (when (and (plist-get keywords :regexp)
               (plist-get keywords :predicate))
      (error "Magent command buffer pattern cannot be regexp and predicate"))
    (when (plist-get keywords :regexp)
      (condition-case err
          (string-match-p pattern "")
        (invalid-regexp
         (error "Invalid Magent command buffer regexp %S: %s"
                pattern (error-message-string err)))))
    (let* ((kind
            (cond
             ((bufferp pattern) 'buffer)
             ((plist-get keywords :regexp) 'regexp)
             ((plist-get keywords :predicate) 'predicate)
             ((stringp pattern) 'name)
             ((symbolp pattern) 'mode)
             ((functionp pattern) 'predicate)))
           (selector-p (memq kind '(mode regexp predicate)))
           (required-p
            (if (plist-member keywords :required-p)
                (plist-get keywords :required-p)
              t))
           (project-only-p
            (if (plist-member keywords :project-only-p)
                (plist-get keywords :project-only-p)
              selector-p)))
      (list :pattern pattern
            :kind kind
            :required-p required-p
            :project-only-p project-only-p))))

(defun magent-command--validate-turn-spec (turn)
  "Validate declarative command TURN and return it."
  (unless (magent-command-turn-spec-p turn)
    (error "Expected Magent command turn spec, got: %S" turn))
  (unless (and (stringp (magent-command-turn-spec-prompt turn))
               (not (string-blank-p
                     (magent-command-turn-spec-prompt turn))))
    (error "Magent command turn prompt is empty"))
  (unless (proper-list-p (magent-command-turn-spec-buffers turn))
    (error "Expected Magent command buffer configuration list, got: %S"
           (magent-command-turn-spec-buffers turn)))
  (mapc #'magent-command--normalize-buffer-config
        (magent-command-turn-spec-buffers turn))
  (unless (or (null (magent-command-turn-spec-skills turn))
              (proper-list-p (magent-command-turn-spec-skills turn)))
    (error "Expected Magent command skill list, got: %S"
           (magent-command-turn-spec-skills turn)))
  (unless (or (null (magent-command-turn-spec-agent turn))
              (stringp (magent-command-turn-spec-agent turn))
              (symbolp (magent-command-turn-spec-agent turn)))
    (error "Expected Magent command agent name, got: %S"
           (magent-command-turn-spec-agent turn)))
  (unless (memq (magent-command-turn-spec-append-argument-p turn) '(nil t))
    (error "Expected Magent command append-argument-p boolean, got: %S"
           (magent-command-turn-spec-append-argument-p turn)))
  turn)

(defun magent-command--resolve-turn-spec (turn-or-builder invocation)
  "Resolve TURN-OR-BUILDER for INVOCATION to a validated turn spec."
  (magent-command--validate-turn-spec
   (if (functionp turn-or-builder)
       (funcall turn-or-builder invocation)
     turn-or-builder)))

(defun magent-command--path-in-project-p (path root base-directory)
  "Return non-nil when PATH under BASE-DIRECTORY belongs to ROOT."
  (when (and (stringp path) (stringp root))
    (condition-case nil
        (let ((expanded (expand-file-name path base-directory))
              (project-root (file-name-as-directory root)))
          (or (equal (directory-file-name expanded)
                     (directory-file-name project-root))
              (file-in-directory-p expanded project-root)))
      (error nil))))

(defun magent-command--buffer-in-project-p (buffer root)
  "Return non-nil when BUFFER belongs to project ROOT."
  (and
   (buffer-live-p buffer)
   (with-current-buffer buffer
     (let ((base default-directory))
       (or (magent-command--path-in-project-p buffer-file-name root base)
           (magent-command--path-in-project-p default-directory root base))))))

(defun magent-command--buffer-pattern-match-p (buffer config)
  "Return non-nil when live BUFFER matches normalized CONFIG."
  (let ((pattern (plist-get config :pattern)))
    (pcase (plist-get config :kind)
      ('mode (eq (buffer-local-value 'major-mode buffer) pattern))
      ('regexp (string-match-p pattern (buffer-name buffer)))
      ('predicate (funcall pattern buffer))
      (_ nil))))

(defun magent-command--matching-buffers (config invocation)
  "Return live buffers matching normalized CONFIG for INVOCATION."
  (let* ((runtime-session
          (magent-command-invocation-runtime-session invocation))
         (origin
          (magent-session-scope-origin
           (magent-runtime-session-scope runtime-session)))
         (project-only-p (plist-get config :project-only-p))
         (project-root (and (stringp origin)
                            (magent-command--canonical-scope origin)))
         (kind (plist-get config :kind))
         (pattern (plist-get config :pattern))
         (candidates
          (pcase kind
            ('buffer (and (buffer-live-p pattern) (list pattern)))
            ('name (when-let* ((buffer (get-buffer pattern))) (list buffer)))
            (_ (cl-remove-if-not
                (lambda (buffer)
                  (and (or (not project-only-p)
                           (not project-root)
                           (magent-command--buffer-in-project-p
                            buffer project-root))
                       (magent-command--buffer-pattern-match-p buffer config)))
                (buffer-list))))))
    (if (and project-only-p project-root (memq kind '(buffer name)))
        (cl-remove-if-not
         (lambda (buffer)
           (magent-command--buffer-in-project-p buffer project-root))
         candidates)
      candidates)))

(defun magent-command--resolve-turn-buffers (turn invocation)
  "Resolve and deduplicate TURN buffer patterns for INVOCATION."
  (let ((seen (make-hash-table :test #'eq))
        buffers)
    (dolist (entry (magent-command-turn-spec-buffers turn))
      (let* ((config (magent-command--normalize-buffer-config entry))
             (matches (magent-command--matching-buffers config invocation)))
        (when (null matches)
          (if (plist-get config :required-p)
              (user-error "Command /%s required buffer pattern matched nothing: %S"
                          (magent-command-spec-name
                           (magent-command-invocation-spec invocation))
                          (plist-get config :pattern))
            (magent-log
             "INFO command /%s optional buffer pattern matched nothing: %S"
             (magent-command-spec-name
              (magent-command-invocation-spec invocation))
             (plist-get config :pattern))))
        (dolist (buffer matches)
          (unless (gethash buffer seen)
            (puthash buffer t seen)
            (push buffer buffers)))))
    (nreverse buffers)))

(defun magent-command--truncate-buffer-content
    (text source-start source-point budget)
  "Return truncation data for TEXT around SOURCE-POINT within BUDGET.
SOURCE-START is the absolute position corresponding to the start of TEXT."
  (let* ((length (length text))
         (keep (if budget (min length budget) length))
         (anchor (max 0 (min length (- source-point source-start))))
         (window-start
          (if (= keep length)
              0
            (max 0 (min (- anchor (/ keep 2)) (- length keep)))))
         (window-end (+ window-start keep)))
    (list :text (substring text window-start window-end)
          :original-length length
          :retained-length keep
          :retained-start (+ source-start window-start)
          :retained-end (+ source-start window-end)
          :omitted-before window-start
          :omitted-after (- length window-end)
          :truncated-p (< keep length))))

(defun magent-command--buffer-resource-block (buffer budget)
  "Return (RESOURCE-BLOCK . RETAINED-CHARS) for BUFFER within BUDGET."
  (with-current-buffer buffer
    (let* ((region-p (use-region-p))
           (accessible-start (point-min))
           (accessible-end (point-max))
           (source-start
            (if region-p
                (max accessible-start (region-beginning))
              accessible-start))
           (source-end
            (if region-p
                (min accessible-end (region-end))
              accessible-end))
           (source-point (point))
           (raw (buffer-substring-no-properties source-start source-end))
           (truncation
            (magent-command--truncate-buffer-content
             raw source-start source-point budget))
           (name (buffer-name buffer))
           (retained-start (plist-get truncation :retained-start))
           (retained-end (plist-get truncation :retained-end))
           (notice
            (and (plist-get truncation :truncated-p)
                 (format
                  (concat "\n[Buffer content truncated: original %d characters; "
                          "retained bounds %d..%d; omitted %d before and %d "
                          "after.]\n")
                  (plist-get truncation :original-length)
                  retained-start retained-end
                  (plist-get truncation :omitted-before)
                  (plist-get truncation :omitted-after))))
           (resource-text
            (format
             (concat "Buffer name: %s\nMajor mode: %s\nFile: %s\n"
                     "Modified: %s\nPoint: %d\nSelection: %s\n"
                     "Selected bounds: %d..%d\nRetained bounds: %d..%d\n"
                     "Narrowed: %s\n%s\nContent:\n%s")
             name major-mode (or buffer-file-name "<none>")
             (if (buffer-modified-p) "true" "false") source-point
             (if region-p "active-region" "accessible-buffer")
             source-start source-end retained-start retained-end
             (if (buffer-narrowed-p) "true" "false")
             (or notice "")
             (plist-get truncation :text)))
           (block
            `((type . "resource")
              (resource
               . ((uri . ,(concat "emacs-buffer:///"
                                  (url-hexify-string name)))
                  (name . ,name)
                  (mimeType . "text/plain")
                  (text . ,resource-text))))))
      (cons block (plist-get truncation :retained-length)))))

(defun magent-command--buffer-resource-blocks (buffers)
  "Return model-visible snapshot resource blocks for BUFFERS."
  (unless (or (null magent-command-buffer-context-max-chars)
              (natnump magent-command-buffer-context-max-chars))
    (error "Expected non-negative command buffer context budget, got: %S"
           magent-command-buffer-context-max-chars))
  (let ((remaining magent-command-buffer-context-max-chars)
        blocks)
    (dolist (buffer buffers)
      (let* ((snapshot (magent-command--buffer-resource-block buffer remaining))
             (used (cdr snapshot)))
        (push (car snapshot) blocks)
        (when remaining
          (setq remaining (max 0 (- remaining used))))))
    (nreverse blocks)))

(defun magent-command--turn-prompt (turn invocation)
  "Return TURN's prompt expanded for INVOCATION."
  (let ((base (magent-command-turn-spec-prompt turn))
        (argument (magent-command-invocation-argument invocation)))
    (if (or (not (magent-command-turn-spec-append-argument-p turn))
            (string-empty-p argument))
        base
      (concat
       base "\n\n"
       (magent-prompt-render
        "internal/additional-instruction.org"
        `((instruction . ,argument)))))))

(defun magent-command--run-turn (invocation)
  "Run the standard declarative turn owned by INVOCATION."
  (let* ((spec (magent-command-invocation-spec invocation))
         (turn (magent-command--resolve-turn-spec
                (magent-command-spec-turn spec) invocation))
         (agent (magent-command-turn-spec-agent turn))
         (prompt (magent-command--turn-prompt turn invocation)))
    (magent-command--validate-required-tools invocation agent)
    (let* ((buffers (magent-command--resolve-turn-buffers turn invocation))
           (resource-blocks
            (magent-command--buffer-resource-blocks buffers)))
      (magent-command-defer invocation)
      (magent-command-submit
       invocation prompt
       :skills (magent-command-turn-spec-skills turn)
       :agent agent
       :resource-blocks resource-blocks
       :on-complete
       (lambda (status result)
         (if (eq status 'completed)
             (magent-command-complete invocation result)
           (magent-command-finish invocation status result)))))))

(cl-defun magent-command-invoke
    (command runtime-session &key raw-input argument request-context
             resource-blocks observer approval-provider on-complete
             submission-adapter)
  "Invoke COMMAND for RUNTIME-SESSION and return its invocation.
COMMAND may be a command name or spec.  The remaining keyword arguments are
request-local data and UI-neutral callbacks supplied by a frontend adapter."
  (let* ((scope (magent-runtime-session-scope runtime-session))
         (spec
          (if (magent-command-spec-p command)
              (let ((effective
                     (magent-command-get
                      (magent-command-spec-name command) scope)))
                (unless (eq command effective)
                  (error "Magent command /%s is unavailable in session scope"
                         (magent-command-spec-name command)))
                command)
            (or (magent-command-get command scope)
                (error "Unknown Magent command: %s" command))))
         (invocation
          (magent-command-invocation-create
           :id (magent-protocol-generate-id "invocation")
           :spec spec
           :runtime-session runtime-session
           :raw-input (or raw-input
                          (concat "/" (magent-command-spec-name spec)))
           :argument (string-trim (or argument ""))
           :request-context request-context
           :resource-blocks resource-blocks
           :observer observer
           :approval-provider approval-provider
           :completion-function on-complete
           :submission-adapter submission-adapter)))
    (when (gethash runtime-session magent-command--active-invocations)
      (user-error "A Magent command is already active in this session"))
    (puthash runtime-session invocation magent-command--active-invocations)
    (condition-case err
        (progn
          (magent-command--validate-invocation invocation)
          (if-let* ((handler (magent-command-spec-handler spec)))
              (progn
                (magent-command--validate-required-tools invocation)
                (funcall handler invocation))
            (magent-command--run-turn invocation))
          (when (and (eq (magent-command-invocation-status invocation) 'active)
                     (not (magent-command-invocation-deferred-p invocation)))
            (magent-command-fail
             invocation
             "Command handler returned without completing or deferring")))
      (quit
       (magent-command-cancel invocation))
      (error
       (magent-command--fail-with-cleanup
        invocation (error-message-string err))))
    invocation))

(provide 'magent-command)
;;; magent-command.el ends here
