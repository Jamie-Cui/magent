;;; magent-command.el --- Elisp-native slash commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Codex:GPT-5.6

;;; Commentary:

;; Public registration and invocation API for Magent slash commands.  A
;; command is an explicit user action.  It may use the structured turn handler
;; for one ordinary agent turn or own a longer asynchronous workflow.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
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
  handler
  owner
  source-layer
  source-scope
  requires-project
  tools
  registration-id
  sequence)

(cl-defstruct (magent-command-turn-spec
               (:constructor magent-command-turn-spec-create)
               (:copier nil))
  "Declarative input for one standard slash command turn."
  prompt
  context
  skills
  agent
  turn-metadata
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
    (name &key description handler owner (source-layer 'package) source-scope
          requires-project tools)
  "Register slash command NAME and return its registration token.

HANDLER receives one `magent-command-invocation'.  It must complete the
invocation synchronously or call `magent-command-defer' before returning.
SOURCE-LAYER controls precedence and is one of `builtin', `package', `user',
`project', or reserved `core'.  OWNER and SOURCE-SCOPE identify reloadable
definitions.  REQUIRES-PROJECT and TOOLS are checked before HANDLER runs."
  (unless (functionp handler)
    (error "Magent command %S requires a handler" name))
  (let* ((key (magent-command--normalize-name name))
         (layer (or source-layer 'package))
         (_rank (magent-command--layer-rank layer))
         (registration-owner (or owner (magent-command--default-owner)))
         (registration-scope
          (magent-command--canonical-scope source-scope))
         (spec (magent-command-spec-create
                :name key
                :description description
                :handler handler
                :owner registration-owner
                :source-layer layer
                :source-scope registration-scope
                :requires-project requires-project
                :tools (mapcar (lambda (tool)
                                 (if (symbolp tool) tool (intern tool)))
                               tools)
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
      ;; Give every generated handler its own lexical skill-name binding.
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
               :handler
               (magent-command-turn-handler
                (lambda (invocation)
                  (magent-command-turn-spec-create
                   :prompt prompt
                   :skills
                   (magent-skills-dedupe-names
                    (append
                     (magent-runtime-session-pending-skills
                      (magent-command-invocation-runtime-session invocation))
                     (list skill-name))))))
               :owner (list 'skill-adapter
                            (or (magent-skill-file-path skill) skill-name))
               :source-layer (magent-command--skill-source-layer skill)
               :source-scope (magent-skill-source-scope skill)
               :requires-project (magent-skill-requires-project skill)
               :tools (magent-skill-tools skill))
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

(defun magent-command--validate-invocation (invocation)
  "Validate project and tool requirements for INVOCATION."
  (let* ((spec (magent-command-invocation-spec invocation))
         (runtime-session
          (magent-command-invocation-runtime-session invocation))
         (origin
          (magent-session-scope-origin
           (magent-runtime-session-scope runtime-session))))
    (when (and (magent-command-spec-requires-project spec)
               (not (stringp origin)))
      (user-error "Command /%s requires a project workspace"
                  (magent-command-spec-name spec)))
    (when-let* ((required (magent-command-spec-tools spec)))
      (let* ((available
              (magent-runtime-session-available-tool-names runtime-session))
             (missing (cl-set-difference required available)))
        (when missing
          (user-error "Command /%s requires unavailable tools: %s"
                      (magent-command-spec-name spec)
                      (mapconcat #'symbol-name missing ", ")))))))

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
    (invocation prompt &key skills agent context turn-metadata on-complete)
  "Submit one agent PROMPT owned by command INVOCATION.
ON-COMPLETE receives the normal runtime STATUS and RESULT."
  (unless (eq (magent-command-invocation-status invocation) 'active)
    (error "Cannot submit from a completed Magent command invocation"))
  (let* ((adapter (magent-command-invocation-submission-adapter invocation))
         (adapted (if adapter
                      (funcall adapter prompt)
                    (list :prompt prompt)))
         (effective-prompt (or (plist-get adapted :prompt) prompt))
         (effective-context
          (append context
                  (plist-get adapted :context)
                  (magent-command-invocation-request-context invocation)))
         (metadata
          (append
           (magent-command-turn-metadata invocation)
           turn-metadata
           (plist-get adapted :turn-metadata)))
         (submission-id
          (magent-runtime-submit
           (magent-command-invocation-runtime-session invocation)
           effective-prompt
           :skills skills
           :agent agent
           :context effective-context
           :turn-metadata metadata
           :observer (magent-command-invocation-observer invocation)
           :approval-provider
           (magent-command-invocation-approval-provider invocation)
           :on-complete (or on-complete #'ignore))))
    (push submission-id
          (magent-command-invocation-submission-ids invocation))
    submission-id))

(cl-defun magent-command-submit-step
    (invocation prompt callback &key skills agent context turn-metadata)
  "Submit one workflow PROMPT step for INVOCATION, then call CALLBACK.
CALLBACK receives runtime STATUS and RESULT while the completed step still
owns the global FIFO lease.  It may therefore call this function again to
queue the next step without another backend advancing between the two steps."
  (unless (functionp callback)
    (error "Expected a Magent command step callback, got: %S" callback))
  (magent-command-submit
   invocation prompt
   :skills skills
   :agent agent
   :context context
   :turn-metadata turn-metadata
   :on-complete
   (lambda (status result)
     (condition-case err
         (funcall callback status result)
       (error
        (magent-command--fail-with-cleanup
         invocation (error-message-string err)))))))

(defun magent-command--plist-p (value)
  "Return non-nil when VALUE is a proper keyword plist or nil."
  (and (proper-list-p value)
       (zerop (% (length value) 2))
       (cl-loop for (key _item) on value by #'cddr
                always (keywordp key))))

(defun magent-command--resolve-turn-spec (turn-or-builder invocation)
  "Resolve TURN-OR-BUILDER for INVOCATION to a validated turn spec."
  (let ((turn (if (functionp turn-or-builder)
                  (funcall turn-or-builder invocation)
                turn-or-builder)))
    (unless (magent-command-turn-spec-p turn)
      (error "Expected Magent command turn spec, got: %S" turn))
    (unless (and (stringp (magent-command-turn-spec-prompt turn))
                 (not (string-blank-p
                       (magent-command-turn-spec-prompt turn))))
      (error "Magent command turn prompt is empty"))
    (unless (magent-command--plist-p
             (magent-command-turn-spec-context turn))
      (error "Expected Magent command context plist, got: %S"
             (magent-command-turn-spec-context turn)))
    (unless (magent-command--plist-p
             (magent-command-turn-spec-turn-metadata turn))
      (error "Expected Magent command turn metadata plist, got: %S"
             (magent-command-turn-spec-turn-metadata turn)))
    (unless (or (null (magent-command-turn-spec-skills turn))
                (proper-list-p (magent-command-turn-spec-skills turn)))
      (error "Expected Magent command skill list, got: %S"
             (magent-command-turn-spec-skills turn)))
    (unless (memq (magent-command-turn-spec-append-argument-p turn) '(nil t))
      (error "Expected Magent command append-argument-p boolean, got: %S"
             (magent-command-turn-spec-append-argument-p turn)))
    turn))

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

(defun magent-command-turn-handler (turn-or-builder)
  "Return a standard one-turn handler for TURN-OR-BUILDER.
TURN-OR-BUILDER is a `magent-command-turn-spec' or a function receiving the
command invocation and returning one.  Turn prompts are user-role text;
structured request context and ledger metadata use their dedicated fields.
When `append-argument-p' is non-nil, append the slash command argument as an
Additional instruction block."
  (unless (or (magent-command-turn-spec-p turn-or-builder)
              (functionp turn-or-builder))
    (error "Expected Magent command turn spec or builder, got: %S"
           turn-or-builder))
  (lambda (invocation)
    (let* ((turn (magent-command--resolve-turn-spec
                  turn-or-builder invocation))
           (prompt (magent-command--turn-prompt turn invocation)))
      (magent-command-defer invocation)
      (magent-command-submit
       invocation prompt
       :context (magent-command-turn-spec-context turn)
       :skills (magent-command-turn-spec-skills turn)
       :agent (magent-command-turn-spec-agent turn)
       :turn-metadata (magent-command-turn-spec-turn-metadata turn)
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
          (funcall (magent-command-spec-handler spec) invocation)
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
