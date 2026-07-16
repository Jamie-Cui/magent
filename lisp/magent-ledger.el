;;; magent-ledger.el --- Durable thread/turn/item ledger for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Explicit Codex-style thread, turn, and item state machine for Magent.
;; The thread ledger is the durable source of truth.  Legacy session
;; messages and context items are projections derived from this state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magent-config)
(require 'magent-json)
(require 'magent-protocol)

(defconst magent-thread-statuses
  '(not-loaded idle active system-error closed)
  "Valid Magent thread runtime statuses.")

(defconst magent-turn-statuses
  '(queued in-progress completed interrupted failed dropped)
  "Valid Magent turn lifecycle statuses.")

(defconst magent-item-statuses
  '(pending in-progress completed failed cancelled)
  "Valid Magent item lifecycle statuses.")

(defconst magent-journal-event-types
  '(thread-started
    thread-status-changed
    turn-queued
    turn-started
    turn-status-changed
    turn-completed
    turn-failed
    turn-interrupted
    turn-dropped
    item-started
    item-updated
    item-completed
    item-failed
    item-cancelled)
  "Valid append-only ledger event types.")

(cl-defstruct (magent-thread
               (:constructor magent-thread--create)
               (:copier nil))
  id
  session-id
  scope
  status
  created-at
  updated-at
  preview
  metadata
  turns
  items
  journal
  snapshot-version
  snapshot-created-at
  last-event-seq)

(cl-defstruct (magent-thread-turn
               (:constructor magent-thread-turn--create)
               (:copier nil))
  id
  thread-id
  op-id
  status
  input
  items
  error
  usage
  metadata
  queued-at
  started-at
  completed-at
  duration-ms)

(cl-defstruct (magent-thread-item
               (:constructor magent-thread-item--create)
               (:copier nil))
  id
  turn-id
  type
  status
  role
  content
  name
  call-id
  input
  output
  error
  phase
  metadata
  created-at
  updated-at
  completed-at)

(cl-defstruct (magent-thread-event
               (:constructor magent-thread-event--create)
               (:copier nil))
  seq
  type
  thread-id
  turn-id
  item-id
  payload
  created-at)

(defun magent-thread--symbol-name-or-nil (value)
  "Return VALUE as a string when it is a symbol or string."
  (cond
   ((null value) nil)
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   (t nil)))

(defun magent-thread--intern-or-nil (value)
  "Return VALUE interned when VALUE is a non-empty string or symbol."
  (cond
   ((symbolp value) value)
   ((and (stringp value) (> (length value) 0)) (intern value))
   (t nil)))

(defun magent-thread--now ()
  "Return the current Unix timestamp as a float."
  (float-time))

(defun magent-journal-event-type-p (type)
  "Return non-nil when TYPE is a valid journal event type."
  (memq type magent-journal-event-types))

(defun magent-thread--coerce-status (status statuses fallback kind)
  "Coerce STATUS into STATUSES, defaulting to FALLBACK for KIND."
  (let ((symbol (cond
                 ((null status) fallback)
                 ((symbolp status) status)
                 ((stringp status) (intern status))
                 (t status))))
    (unless (memq symbol statuses)
      (error "Invalid %s status: %S" kind status))
    symbol))

(defun magent-thread--coerce-event-type (type)
  "Return TYPE as a valid journal event type."
  (let ((symbol (cond
                 ((symbolp type) type)
                 ((stringp type) (intern type))
                 (t type))))
    (unless (magent-journal-event-type-p symbol)
      (error "Invalid journal event type: %S" type))
    symbol))

(defun magent-thread-create (&rest args)
  "Create a `magent-thread' from keyword ARGS."
  (let* ((now (or (plist-get args :created-at) (magent-thread--now)))
         (id (or (plist-get args :id)
                 (magent-protocol-generate-id "thread"))))
    (magent-thread--create
     :id id
     :session-id (or (plist-get args :session-id) id)
     :scope (or (plist-get args :scope) 'global)
     :status (magent-thread--coerce-status
              (plist-get args :status)
              magent-thread-statuses
              'idle
              "thread")
     :created-at now
     :updated-at (or (plist-get args :updated-at) now)
     :preview (plist-get args :preview)
     :metadata (plist-get args :metadata)
     :turns (plist-get args :turns)
     :items (plist-get args :items)
     :journal (plist-get args :journal)
     :snapshot-version (or (plist-get args :snapshot-version) 1)
     :snapshot-created-at (plist-get args :snapshot-created-at)
     :last-event-seq (or (plist-get args :last-event-seq) 0))))

(defun magent-thread-turn-create (&rest args)
  "Create a `magent-thread-turn' from keyword ARGS."
  (let* ((now (or (plist-get args :started-at)
                  (plist-get args :queued-at)
                  (magent-thread--now)))
         (id (or (plist-get args :id)
                 (magent-protocol-generate-id "turn"))))
    (magent-thread-turn--create
     :id id
     :thread-id (plist-get args :thread-id)
     :op-id (plist-get args :op-id)
     :status (magent-thread--coerce-status
              (plist-get args :status)
              magent-turn-statuses
              'queued
              "turn")
     :input (plist-get args :input)
     :items (plist-get args :items)
     :error (plist-get args :error)
     :usage (plist-get args :usage)
     :metadata (plist-get args :metadata)
     :queued-at (or (plist-get args :queued-at) now)
     :started-at (plist-get args :started-at)
     :completed-at (plist-get args :completed-at)
     :duration-ms (plist-get args :duration-ms))))

(defun magent-thread-item-create (&rest args)
  "Create a `magent-thread-item' from keyword ARGS."
  (let* ((now (or (plist-get args :created-at) (magent-thread--now)))
         (status (magent-thread--coerce-status
                  (plist-get args :status)
                  magent-item-statuses
                  'pending
                  "item")))
    (magent-thread-item--create
     :id (or (plist-get args :id)
             (plist-get args :call-id)
             (magent-protocol-generate-id "item"))
     :turn-id (plist-get args :turn-id)
     :type (magent-thread--intern-or-nil (plist-get args :type))
     :status status
     :role (magent-thread--intern-or-nil (plist-get args :role))
     :content (plist-get args :content)
     :name (plist-get args :name)
     :call-id (plist-get args :call-id)
     :input (plist-get args :input)
     :output (plist-get args :output)
     :error (plist-get args :error)
     :phase (magent-thread--intern-or-nil (plist-get args :phase))
     :metadata (plist-get args :metadata)
     :created-at now
     :updated-at (or (plist-get args :updated-at) now)
     :completed-at (plist-get args :completed-at))))

(defun magent-thread-event-create (&rest args)
  "Create a `magent-thread-event' from keyword ARGS."
  (magent-thread-event--create
   :seq (plist-get args :seq)
   :type (magent-thread--coerce-event-type (plist-get args :type))
   :thread-id (plist-get args :thread-id)
   :turn-id (plist-get args :turn-id)
   :item-id (plist-get args :item-id)
   :payload (plist-get args :payload)
   :created-at (or (plist-get args :created-at) (magent-thread--now))))

(defun magent-thread--alist-get (key alist)
  "Return KEY from ALIST."
  (cdr (assq key alist)))

(defun magent-thread-event-to-alist (event)
  "Convert journal EVENT to a JSON-serializable alist."
  `((seq . ,(magent-thread-event-seq event))
    (type . ,(magent-thread--symbol-name-or-nil
              (magent-thread-event-type event)))
    (thread-id . ,(magent-thread-event-thread-id event))
    (turn-id . ,(magent-thread-event-turn-id event))
    (item-id . ,(magent-thread-event-item-id event))
    (payload . ,(magent-thread--event-payload-to-alist
                 (magent-thread-event-payload event)))
    (created-at . ,(magent-thread-event-created-at event))))

(defun magent-thread-event-from-alist (alist)
  "Reconstruct a journal event from JSON-decoded ALIST."
  (magent-thread-event-create
   :seq (magent-thread--alist-get 'seq alist)
   :type (magent-thread--alist-get 'type alist)
   :thread-id (magent-thread--alist-get 'thread-id alist)
   :turn-id (magent-thread--alist-get 'turn-id alist)
   :item-id (magent-thread--alist-get 'item-id alist)
   :payload (magent-thread--alist-get 'payload alist)
   :created-at (magent-thread--alist-get 'created-at alist)))

(defun magent-thread--turn-event-payload (turn)
  "Return an immutable JSON-safe payload snapshot for TURN."
  (list :turn (magent-thread-turn-to-alist turn)))

(defun magent-thread--item-event-payload (item)
  "Return an immutable JSON-safe payload snapshot for ITEM."
  (list :item (magent-thread-item-to-alist item)))

(defun magent-thread--event-payload-to-alist (payload)
  "Convert journal PAYLOAD to JSON-safe event payload."
  (cond
   ((null payload) nil)
   ((magent-json--plist-p payload)
    (let (out)
      (while payload
        (let* ((key (pop payload))
               (value (pop payload))
               (safe-value
                (cond
                 ((eq key :turn)
                  (cond
                   ((magent-thread-turn-p value)
                    (magent-thread-turn-to-alist value))
                   ((listp value) value)
                   (t (and value (magent-json-safe-value value)))))
                 ((eq key :item)
                  (cond
                   ((magent-thread-item-p value)
                    (magent-thread-item-to-alist value))
                   ((listp value) value)
                   (t (and value (magent-json-safe-value value)))))
                 (t
                  (and value (magent-json-safe-value value))))))
          (push (cons (magent-json--object-key key) safe-value) out)))
      (nreverse out)))
   (t
    (magent-json-safe-value payload))))

(defun magent-thread-item-to-alist (item)
  "Convert ITEM to a JSON-serializable alist."
  `((id . ,(magent-thread-item-id item))
    (turn-id . ,(magent-thread-item-turn-id item))
    (type . ,(magent-thread--symbol-name-or-nil
              (magent-thread-item-type item)))
    (status . ,(magent-thread--symbol-name-or-nil
                (magent-thread-item-status item)))
    (role . ,(magent-thread--symbol-name-or-nil
              (magent-thread-item-role item)))
    (content . ,(let ((content (magent-thread-item-content item)))
                  (and content (magent-json-safe-value content))))
    (name . ,(magent-thread-item-name item))
    (call-id . ,(magent-thread-item-call-id item))
    (input . ,(let ((input (magent-thread-item-input item)))
                (and input (magent-json-safe-value input))))
    (output . ,(let ((output (magent-thread-item-output item)))
                 (and output (magent-json-safe-value output))))
    (error . ,(let ((error (magent-thread-item-error item)))
                (and error (magent-json-safe-value error))))
    (phase . ,(magent-thread--symbol-name-or-nil
               (magent-thread-item-phase item)))
    (metadata . ,(let ((metadata (magent-thread-item-metadata item)))
                   (and metadata (magent-json-safe-value metadata))))
    (created-at . ,(magent-thread-item-created-at item))
    (updated-at . ,(magent-thread-item-updated-at item))
    (completed-at . ,(magent-thread-item-completed-at item))))

(defun magent-thread-item-from-alist (alist)
  "Reconstruct an item from JSON-decoded ALIST."
  (magent-thread-item-create
   :id (magent-thread--alist-get 'id alist)
   :turn-id (magent-thread--alist-get 'turn-id alist)
   :type (magent-thread--alist-get 'type alist)
   :status (magent-thread--alist-get 'status alist)
   :role (magent-thread--alist-get 'role alist)
   :content (magent-thread--alist-get 'content alist)
   :name (magent-thread--alist-get 'name alist)
   :call-id (magent-thread--alist-get 'call-id alist)
   :input (magent-thread--alist-get 'input alist)
   :output (magent-thread--alist-get 'output alist)
   :error (magent-thread--alist-get 'error alist)
   :phase (magent-thread--alist-get 'phase alist)
   :metadata (magent-thread--alist-get 'metadata alist)
   :created-at (magent-thread--alist-get 'created-at alist)
   :updated-at (magent-thread--alist-get 'updated-at alist)
   :completed-at (magent-thread--alist-get 'completed-at alist)))

(defun magent-thread-turn-to-alist (turn)
  "Convert TURN to a JSON-serializable alist."
  `((id . ,(magent-thread-turn-id turn))
    (thread-id . ,(magent-thread-turn-thread-id turn))
    (op-id . ,(magent-thread-turn-op-id turn))
    (status . ,(magent-thread--symbol-name-or-nil
                (magent-thread-turn-status turn)))
    (input . ,(magent-thread-turn-input turn))
    (items . ,(vconcat
               (mapcar #'magent-thread-item-to-alist
                       (magent-thread-turn-items turn))))
    (error . ,(let ((error (magent-thread-turn-error turn)))
                (and error (magent-json-safe-value error))))
    (usage . ,(let ((usage (magent-thread-turn-usage turn)))
                (and usage (magent-json-safe-value usage))))
    (metadata . ,(let ((metadata (magent-thread-turn-metadata turn)))
                   (and metadata (magent-json-safe-value metadata))))
    (queued-at . ,(magent-thread-turn-queued-at turn))
    (started-at . ,(magent-thread-turn-started-at turn))
    (completed-at . ,(magent-thread-turn-completed-at turn))
    (duration-ms . ,(magent-thread-turn-duration-ms turn))))

(defun magent-thread-turn-from-alist (alist)
  "Reconstruct a turn from JSON-decoded ALIST."
  (magent-thread-turn-create
   :id (magent-thread--alist-get 'id alist)
   :thread-id (magent-thread--alist-get 'thread-id alist)
   :op-id (magent-thread--alist-get 'op-id alist)
   :status (magent-thread--alist-get 'status alist)
   :input (magent-thread--alist-get 'input alist)
   :items (mapcar #'magent-thread-item-from-alist
                  (magent-thread--alist-get 'items alist))
   :error (magent-thread--alist-get 'error alist)
   :usage (magent-thread--alist-get 'usage alist)
   :metadata (magent-thread--alist-get 'metadata alist)
   :queued-at (magent-thread--alist-get 'queued-at alist)
   :started-at (magent-thread--alist-get 'started-at alist)
   :completed-at (magent-thread--alist-get 'completed-at alist)
   :duration-ms (magent-thread--alist-get 'duration-ms alist)))

(defun magent-thread-snapshot-to-alist (thread)
  "Convert THREAD's full materialized state to a JSON snapshot alist."
  `((id . ,(magent-thread-id thread))
    (session-id . ,(magent-thread-session-id thread))
    (scope . ,(let ((scope (magent-thread-scope thread)))
                (if (eq scope 'global) "global" scope)))
    (status . ,(magent-thread--symbol-name-or-nil
                (magent-thread-status thread)))
    (created-at . ,(magent-thread-created-at thread))
    (updated-at . ,(magent-thread-updated-at thread))
    (preview . ,(magent-thread-preview thread))
    (metadata . ,(let ((metadata (magent-thread-metadata thread)))
                   (and metadata (magent-json-safe-value metadata))))
    (turns . ,(vconcat
               (mapcar #'magent-thread-turn-to-alist
                       (magent-thread-turns thread))))
    (snapshot-version . ,(magent-thread-snapshot-version thread))
    (snapshot-created-at . ,(or (magent-thread-snapshot-created-at thread)
                                (magent-thread--now)))
    (last-event-seq . ,(magent-thread-last-event-seq thread))))

(defun magent-thread-snapshot-from-alist (alist)
  "Reconstruct a thread from a JSON snapshot ALIST."
  (let ((scope (magent-thread--alist-get 'scope alist)))
    (magent-thread-create
     :id (magent-thread--alist-get 'id alist)
     :session-id (magent-thread--alist-get 'session-id alist)
     :scope (if (equal scope "global") 'global scope)
     :status (magent-thread--alist-get 'status alist)
     :created-at (magent-thread--alist-get 'created-at alist)
     :updated-at (magent-thread--alist-get 'updated-at alist)
     :preview (magent-thread--alist-get 'preview alist)
     :metadata (magent-thread--alist-get 'metadata alist)
     :turns (mapcar #'magent-thread-turn-from-alist
                    (magent-thread--alist-get 'turns alist))
     :snapshot-version (magent-thread--alist-get 'snapshot-version alist)
     :snapshot-created-at (magent-thread--alist-get 'snapshot-created-at alist)
     :last-event-seq (magent-thread--alist-get 'last-event-seq alist))))

(defun magent-thread--find-turn (thread turn-id)
  "Return THREAD turn TURN-ID, or nil."
  (cl-find turn-id (magent-thread-turns thread)
           :key #'magent-thread-turn-id
           :test #'equal))

(defun magent-thread-find-turn (thread turn-id)
  "Return THREAD turn TURN-ID, or nil."
  (magent-thread--find-turn thread turn-id))

(defun magent-thread--find-item (thread item-id)
  "Return THREAD item ITEM-ID, or nil."
  (cl-loop for turn in (magent-thread-turns thread)
           for item = (cl-find item-id
                               (magent-thread-turn-items turn)
                               :key #'magent-thread-item-id
                               :test #'equal)
           when item return item))

(defun magent-thread-active-turn (thread)
  "Return THREAD's active turn, or nil."
  (cl-find-if
   (lambda (turn)
     (eq (magent-thread-turn-status turn) 'in-progress))
   (magent-thread-turns thread)))

(defun magent-thread-terminal-turn-p (turn)
  "Return non-nil when TURN reached a terminal state."
  (memq (magent-thread-turn-status turn)
        '(completed interrupted failed dropped)))

(defun magent-thread-terminal-item-p (item)
  "Return non-nil when ITEM reached a terminal state."
  (memq (magent-thread-item-status item)
        '(completed failed cancelled)))

(defun magent-thread--replace-turn (thread turn)
  "Replace TURN in THREAD or append it when absent."
  (let ((turns (magent-thread-turns thread))
        replaced)
    (setf (magent-thread-turns thread)
          (mapcar (lambda (existing)
                    (if (equal (magent-thread-turn-id existing)
                               (magent-thread-turn-id turn))
                        (progn
                          (setq replaced t)
                          turn)
                      existing))
                  turns))
    (unless replaced
      (setf (magent-thread-turns thread)
            (nconc (magent-thread-turns thread) (list turn)))))
  turn)

(defun magent-thread--replace-item (turn item)
  "Replace ITEM in TURN or append it when absent."
  (let ((items (magent-thread-turn-items turn))
        replaced)
    (setf (magent-thread-turn-items turn)
          (mapcar (lambda (existing)
                    (if (equal (magent-thread-item-id existing)
                               (magent-thread-item-id item))
                        (progn
                          (setq replaced t)
                          item)
                      existing))
                  items))
    (unless replaced
      (setf (magent-thread-turn-items turn)
            (nconc (magent-thread-turn-items turn) (list item)))))
  item)

(defun magent-thread--event-payload-turn (payload)
  "Return the turn payload from PAYLOAD."
  (let ((turn (magent-thread--event-payload-value :turn payload)))
    (cond
     ((magent-thread-turn-p turn) turn)
     ((listp turn) (magent-thread-turn-from-alist turn))
     ((and (listp payload)
           (magent-thread--alist-get 'id payload))
      (magent-thread-turn-from-alist payload))
     (t nil))))

(defun magent-thread--event-payload-item (payload)
  "Return the item payload from PAYLOAD."
  (let ((item (magent-thread--event-payload-value :item payload)))
    (cond
     ((magent-thread-item-p item) item)
     ((listp item) (magent-thread-item-from-alist item))
     ((and (listp payload)
           (magent-thread--alist-get 'id payload))
      (magent-thread-item-from-alist payload))
     (t nil))))

(defun magent-thread--event-payload-status (payload)
  "Return status from event PAYLOAD."
  (magent-thread--intern-or-nil
   (or (plist-get payload :status)
       (magent-thread--alist-get 'status payload))))

(defun magent-thread--event-payload-value (key payload)
  "Return KEY from plist or alist PAYLOAD."
  (when (listp payload)
    (let ((alist-key (intern (substring (symbol-name key) 1))))
      (or (and (magent-json--plist-p payload)
               (plist-get payload key))
          (magent-thread--alist-get alist-key payload)
          (plist-get payload alist-key)))))

(defun magent-thread--update-timestamp (thread &optional now)
  "Update THREAD's `updated-at' timestamp to NOW."
  (setf (magent-thread-updated-at thread) (or now (magent-thread--now))))

(defun magent-thread-append-event (thread event)
  "Append EVENT to THREAD journal and apply it to materialized state."
  (let* ((seq (1+ (or (magent-thread-last-event-seq thread) 0)))
         (event (if (magent-thread-event-seq event)
                    event
                  (setf (magent-thread-event-seq event) seq)
                  event)))
    (setf (magent-thread-last-event-seq thread)
          (max seq (or (magent-thread-event-seq event) seq)))
    (setf (magent-thread-journal thread)
          (nconc (magent-thread-journal thread) (list event)))
    (magent-thread-apply-event thread event)
    event))

(defun magent-thread-apply-event (thread event)
  "Apply journal EVENT to THREAD and return THREAD."
  (let* ((type (magent-thread-event-type event))
         (payload (magent-thread-event-payload event))
         (now (or (magent-thread-event-created-at event)
                  (magent-thread--now))))
    (pcase type
      ('thread-started
       (setf (magent-thread-status thread) 'idle
             (magent-thread-created-at thread)
             (or (magent-thread-created-at thread) now))
       (magent-thread--update-timestamp thread now))
      ('thread-status-changed
       (setf (magent-thread-status thread)
             (magent-thread--coerce-status
              (magent-thread--event-payload-status payload)
              magent-thread-statuses
              'idle
              "thread"))
       (magent-thread--update-timestamp thread now))
      ('turn-queued
       (let* ((incoming (magent-thread--event-payload-turn payload))
              (turn-id (or (magent-thread-event-turn-id event)
                           (and incoming
                                (magent-thread-turn-id incoming))))
              (turn (or (magent-thread--find-turn thread turn-id)
                        incoming)))
         (unless (magent-thread-turn-p turn)
           (setq turn
                 (magent-thread-turn-create
                  :id turn-id
                  :thread-id (magent-thread-id thread)
                  :input (magent-thread--event-payload-value :input payload)
                  :status 'queued
                  :queued-at now)))
         (when (and incoming
                    (not (eq turn incoming)))
           (unless (magent-thread-turn-op-id turn)
             (setf (magent-thread-turn-op-id turn)
                   (magent-thread-turn-op-id incoming)))
           (unless (magent-thread-turn-input turn)
             (setf (magent-thread-turn-input turn)
                   (magent-thread-turn-input incoming)))
           (unless (magent-thread-turn-metadata turn)
             (setf (magent-thread-turn-metadata turn)
                   (magent-thread-turn-metadata incoming)))
           (unless (magent-thread-turn-items turn)
             (setf (magent-thread-turn-items turn)
                   (magent-thread-turn-items incoming))))
         (setf (magent-thread-turn-thread-id turn) (magent-thread-id thread)
               (magent-thread-turn-status turn) 'queued
               (magent-thread-turn-queued-at turn)
               (or (magent-thread-turn-queued-at turn) now))
         (magent-thread--replace-turn thread turn)
         (unless (magent-thread-preview thread)
           (setf (magent-thread-preview thread)
                 (magent-thread-turn-input turn)))
         (magent-thread--update-timestamp thread now)))
      ('turn-started
       (let* ((incoming (magent-thread--event-payload-turn payload))
              (turn-id (or (magent-thread-event-turn-id event)
                           (and incoming
                                (magent-thread-turn-id incoming))))
              (turn (or (magent-thread--find-turn thread turn-id)
                        incoming)))
         (unless (magent-thread-turn-p turn)
           (setq turn
                 (magent-thread-turn-create
                  :id turn-id
                  :thread-id (magent-thread-id thread)
                  :input (magent-thread--event-payload-value :input payload)
                  :status 'in-progress
                  :started-at now)))
         (when (and incoming
                    (not (eq turn incoming)))
           (unless (magent-thread-turn-op-id turn)
             (setf (magent-thread-turn-op-id turn)
                   (magent-thread-turn-op-id incoming)))
           (unless (magent-thread-turn-input turn)
             (setf (magent-thread-turn-input turn)
                   (magent-thread-turn-input incoming)))
           (unless (magent-thread-turn-metadata turn)
             (setf (magent-thread-turn-metadata turn)
                   (magent-thread-turn-metadata incoming)))
           (unless (magent-thread-turn-items turn)
             (setf (magent-thread-turn-items turn)
                   (magent-thread-turn-items incoming))))
         (setf (magent-thread-turn-thread-id turn) (magent-thread-id thread)
               (magent-thread-turn-status turn) 'in-progress
               (magent-thread-turn-started-at turn)
               (or (magent-thread-turn-started-at turn) now))
         (magent-thread--replace-turn thread turn)
         (setf (magent-thread-status thread) 'active)
         (unless (magent-thread-preview thread)
           (setf (magent-thread-preview thread)
                 (magent-thread-turn-input turn)))
         (magent-thread--update-timestamp thread now)))
      ('turn-status-changed
       (when-let* ((turn (magent-thread--find-turn
                         thread
                         (magent-thread-event-turn-id event))))
         (setf (magent-thread-turn-status turn)
               (magent-thread--coerce-status
                (magent-thread--event-payload-status payload)
                magent-turn-statuses
                (magent-thread-turn-status turn)
                "turn"))
         (magent-thread--update-timestamp thread now)))
      ((or 'turn-completed 'turn-failed 'turn-interrupted 'turn-dropped)
       (when-let* ((turn (magent-thread--find-turn
                         thread
                         (magent-thread-event-turn-id event))))
         (setf (magent-thread-turn-status turn)
               (pcase type
                 ('turn-completed 'completed)
                 ('turn-failed 'failed)
                 ('turn-interrupted 'interrupted)
                 ('turn-dropped 'dropped))
               (magent-thread-turn-completed-at turn) now
               (magent-thread-turn-error turn)
               (or (magent-thread--event-payload-value :error payload)
                   (magent-thread-turn-error turn))
               (magent-thread-turn-usage turn)
               (or (magent-thread--event-payload-value :usage payload)
                   (magent-thread-turn-usage turn)))
         (when-let* ((started (magent-thread-turn-started-at turn)))
           (setf (magent-thread-turn-duration-ms turn)
                 (round (* 1000 (- now started)))))
         (setf (magent-thread-status thread)
               (if (eq type 'turn-failed) 'system-error 'idle))
         (magent-thread--update-timestamp thread now)))
      ('item-started
       (let* ((item (magent-thread--event-payload-item payload))
              (turn-id (or (magent-thread-event-turn-id event)
                           (and item (magent-thread-item-turn-id item)))))
         (when-let* ((turn (magent-thread--find-turn thread turn-id)))
           (unless (magent-thread-item-p item)
             (setq item
                   (magent-thread-item-create
                    :id (magent-thread-event-item-id event)
                    :turn-id turn-id
                    :type (magent-thread--event-payload-value
                           :type payload)
                    :status 'in-progress
                    :created-at now)))
           (setf (magent-thread-item-turn-id item) turn-id
                 (magent-thread-item-status item) 'in-progress
                 (magent-thread-item-created-at item)
                 (or (magent-thread-item-created-at item) now)
                 (magent-thread-item-updated-at item) now)
           (magent-thread--replace-item turn item)
           (magent-thread--update-timestamp thread now))))
      ('item-updated
       (when-let* ((item (magent-thread--find-item
                         thread
                         (magent-thread-event-item-id event))))
         (let ((incoming (magent-thread--event-payload-item payload)))
           (when (magent-thread-item-p incoming)
             (magent-thread--merge-item item incoming))
           (setf (magent-thread-item-updated-at item) now)
           (magent-thread--update-timestamp thread now))))
      ((or 'item-completed 'item-failed 'item-cancelled)
       (when-let* ((item (magent-thread--find-item
                         thread
                         (magent-thread-event-item-id event))))
         (let ((incoming (magent-thread--event-payload-item payload)))
           (when (magent-thread-item-p incoming)
             (magent-thread--merge-item item incoming))
           (setf (magent-thread-item-status item)
                 (pcase type
                   ('item-completed 'completed)
                   ('item-failed 'failed)
                   ('item-cancelled 'cancelled))
                 (magent-thread-item-error item)
                 (or (magent-thread--event-payload-value :error payload)
                     (magent-thread-item-error item))
                 (magent-thread-item-updated-at item) now
                 (magent-thread-item-completed-at item) now)
           (magent-thread--update-timestamp thread now))))))
  thread)

(defun magent-thread--merge-item (target incoming)
  "Merge non-nil fields from INCOMING into TARGET."
  (let ((type (magent-thread-item-type incoming))
        (status (magent-thread-item-status incoming))
        (role (magent-thread-item-role incoming))
        (content (magent-thread-item-content incoming))
        (name (magent-thread-item-name incoming))
        (call-id (magent-thread-item-call-id incoming))
        (input (magent-thread-item-input incoming))
        (output (magent-thread-item-output incoming))
        (error (magent-thread-item-error incoming))
        (phase (magent-thread-item-phase incoming))
        (metadata (magent-thread-item-metadata incoming))
        (completed-at (magent-thread-item-completed-at incoming)))
    (when type (setf (magent-thread-item-type target) type))
    (when status (setf (magent-thread-item-status target) status))
    (when role (setf (magent-thread-item-role target) role))
    (when content (setf (magent-thread-item-content target) content))
    (when name (setf (magent-thread-item-name target) name))
    (when call-id (setf (magent-thread-item-call-id target) call-id))
    (when input (setf (magent-thread-item-input target) input))
    (when output (setf (magent-thread-item-output target) output))
    (when error (setf (magent-thread-item-error target) error))
    (when phase (setf (magent-thread-item-phase target) phase))
    (when metadata (setf (magent-thread-item-metadata target) metadata))
    (when completed-at
      (setf (magent-thread-item-completed-at target) completed-at)))
  target)

(defun magent-thread-replay (snapshot events)
  "Return a thread by applying EVENTS after SNAPSHOT.
SNAPSHOT may be nil, a `magent-thread', or a snapshot alist."
  (let ((thread (cond
                 ((magent-thread-p snapshot) snapshot)
                 ((listp snapshot)
                  (magent-thread-snapshot-from-alist snapshot))
                 (t (magent-thread-create)))))
    (dolist (event events thread)
      (let ((event (if (magent-thread-event-p event)
                       event
                     (magent-thread-event-from-alist event))))
        (let ((already-applied (<= (or (magent-thread-event-seq event) 0)
                                   (or (magent-thread-last-event-seq thread) 0))))
          (setf (magent-thread-journal thread)
                (nconc (magent-thread-journal thread) (list event)))
          (unless already-applied
            (setf (magent-thread-last-event-seq thread)
                  (max (or (magent-thread-last-event-seq thread) 0)
                       (or (magent-thread-event-seq event) 0)))
            (magent-thread-apply-event thread event)))))))

(defun magent-thread-queue-turn
    (thread input &optional op-id metadata)
  "Create a queued turn in THREAD for INPUT.
Return the new `magent-thread-turn'."
  (let* ((now (magent-thread--now))
         (turn (magent-thread-turn-create
                :thread-id (magent-thread-id thread)
                :op-id op-id
                :status 'queued
                :input input
                :metadata metadata
                :queued-at now)))
    (magent-thread-append-event
     thread
     (magent-thread-event-create
      :type 'turn-queued
      :thread-id (magent-thread-id thread)
      :turn-id (magent-thread-turn-id turn)
      :payload (magent-thread--turn-event-payload turn)
      :created-at now))
    (or (magent-thread--find-turn thread (magent-thread-turn-id turn))
        turn)))

(defun magent-thread-start-turn (thread turn-id)
  "Mark THREAD turn TURN-ID in-progress and return it."
  (let* ((now (magent-thread--now))
         (turn (or (magent-thread--find-turn thread turn-id)
                   (magent-thread-turn-create
                    :id turn-id
                    :thread-id (magent-thread-id thread)
                    :status 'in-progress
                    :queued-at now
                    :started-at now))))
    (setf (magent-thread-turn-started-at turn)
          (or (magent-thread-turn-started-at turn) now))
    (magent-thread-append-event
     thread
     (magent-thread-event-create
      :type 'turn-started
      :thread-id (magent-thread-id thread)
      :turn-id turn-id
      :payload (magent-thread--turn-event-payload turn)
      :created-at now))
    (magent-thread--find-turn thread turn-id)))

(defun magent-thread-create-turn
    (thread input &optional op-id metadata)
  "Create and start a new turn in THREAD for INPUT.
Return the new `magent-thread-turn'."
  (let ((turn (magent-thread-queue-turn thread input op-id metadata)))
    (magent-thread-start-turn thread (magent-thread-turn-id turn))))

(defun magent-thread-start-item (thread turn-id type &rest args)
  "Start an item of TYPE in THREAD under TURN-ID using ARGS.
Return the new `magent-thread-item'."
  (let* ((now (magent-thread--now))
         (item (apply #'magent-thread-item-create
                      :turn-id turn-id
                      :type type
                      :status 'in-progress
                      :created-at now
                      args)))
    (magent-thread-append-event
     thread
     (magent-thread-event-create
      :type 'item-started
      :thread-id (magent-thread-id thread)
      :turn-id turn-id
      :item-id (magent-thread-item-id item)
      :payload (magent-thread--item-event-payload item)
      :created-at now))
    (or (magent-thread--find-item thread (magent-thread-item-id item))
        item)))

(defun magent-thread-complete-item (thread item &rest args)
  "Mark ITEM completed in THREAD with ARGS."
  (let ((incoming (apply #'magent-thread-item-create
                         :id (magent-thread-item-id item)
                         :turn-id (magent-thread-item-turn-id item)
                         :type (magent-thread-item-type item)
                         :status 'completed
                         args)))
    (magent-thread-append-event
     thread
     (magent-thread-event-create
      :type 'item-completed
      :thread-id (magent-thread-id thread)
      :turn-id (magent-thread-item-turn-id item)
      :item-id (magent-thread-item-id item)
      :payload (magent-thread--item-event-payload incoming)))
    item))

(defun magent-thread-fail-item (thread item error &rest args)
  "Mark ITEM failed in THREAD with ERROR and ARGS."
  (let ((incoming (apply #'magent-thread-item-create
                         :id (magent-thread-item-id item)
                         :turn-id (magent-thread-item-turn-id item)
                         :type (magent-thread-item-type item)
                         :status 'failed
                         :error error
                         args)))
    (magent-thread-append-event
     thread
     (magent-thread-event-create
      :type 'item-failed
      :thread-id (magent-thread-id thread)
      :turn-id (magent-thread-item-turn-id item)
      :item-id (magent-thread-item-id item)
      :payload (append (magent-thread--item-event-payload incoming)
                       (list :error error))))
    item))

(defun magent-thread-cancel-item (thread item &optional error)
  "Mark ITEM cancelled in THREAD with optional ERROR."
  (let ((incoming (magent-thread-item-create
                   :id (magent-thread-item-id item)
                   :turn-id (magent-thread-item-turn-id item)
                   :type (magent-thread-item-type item)
                   :status 'cancelled
                   :error error)))
    (magent-thread-append-event
     thread
     (magent-thread-event-create
      :type 'item-cancelled
      :thread-id (magent-thread-id thread)
      :turn-id (magent-thread-item-turn-id item)
      :item-id (magent-thread-item-id item)
      :payload (append (magent-thread--item-event-payload incoming)
                       (list :error error))))
    item))

(defun magent-thread-cancel-in-progress-items
    (thread turn-id &optional error)
  "Cancel all in-progress items for THREAD turn TURN-ID.
Return the number of cancelled items."
  (let ((count 0))
    (when-let* ((turn (magent-thread--find-turn thread turn-id)))
      (dolist (item (copy-sequence (magent-thread-turn-items turn)))
        (when (eq (magent-thread-item-status item) 'in-progress)
          (magent-thread-cancel-item thread item error)
          (cl-incf count))))
    count))

(defun magent-thread-complete-turn
    (thread turn-id &optional usage)
  "Mark THREAD turn TURN-ID completed with optional USAGE."
  (magent-thread-append-event
   thread
   (magent-thread-event-create
    :type 'turn-completed
    :thread-id (magent-thread-id thread)
    :turn-id turn-id
    :payload (list :usage usage)))
  (magent-thread--find-turn thread turn-id))

(defun magent-thread-fail-turn (thread turn-id error)
  "Mark THREAD turn TURN-ID failed with ERROR."
  (magent-thread-append-event
   thread
   (magent-thread-event-create
    :type 'turn-failed
    :thread-id (magent-thread-id thread)
    :turn-id turn-id
    :payload (list :error error)))
  (magent-thread--find-turn thread turn-id))

(defun magent-thread-interrupt-turn (thread turn-id &optional detail)
  "Mark THREAD turn TURN-ID interrupted with optional DETAIL."
  (magent-thread-append-event
   thread
   (magent-thread-event-create
    :type 'turn-interrupted
    :thread-id (magent-thread-id thread)
    :turn-id turn-id
    :payload (list :error detail)))
  (magent-thread--find-turn thread turn-id))

(defun magent-thread-drop-turn (thread turn-id &optional detail)
  "Mark THREAD turn TURN-ID dropped with optional DETAIL."
  (magent-thread-append-event
   thread
   (magent-thread-event-create
    :type 'turn-dropped
    :thread-id (magent-thread-id thread)
    :turn-id turn-id
    :payload (list :error detail)))
  (magent-thread--find-turn thread turn-id))

(defun magent-thread-reconcile-after-restart (thread &optional detail)
  "Terminalize non-durable work in THREAD after an Emacs restart.
DETAIL defaults to a stable explanatory message.  Pending or in-progress
items are cancelled, queued turns are dropped, and in-progress turns are
interrupted.  An otherwise active thread is returned to `idle'.  Return the
number of lifecycle objects changed."
  (let ((reason (or detail "Interrupted by Emacs restart"))
        (changed 0))
    (dolist (turn (copy-sequence (magent-thread-turns thread)))
      (dolist (item (copy-sequence (magent-thread-turn-items turn)))
        (when (memq (magent-thread-item-status item) '(pending in-progress))
          (magent-thread-cancel-item thread item reason)
          (cl-incf changed)))
      (pcase (magent-thread-turn-status turn)
        ('queued
         (magent-thread-drop-turn thread (magent-thread-turn-id turn) reason)
         (cl-incf changed))
        ('in-progress
         (magent-thread-interrupt-turn
          thread (magent-thread-turn-id turn) reason)
         (cl-incf changed))))
    (when (eq (magent-thread-status thread) 'active)
      (magent-thread-append-event
       thread
       (magent-thread-event-create
        :type 'thread-status-changed
        :thread-id (magent-thread-id thread)
        :payload (list :status 'idle)))
      (cl-incf changed))
    changed))

(defun magent-thread-all-items (thread)
  "Return all items in THREAD in chronological turn order."
  (apply #'append
         (mapcar #'magent-thread-turn-items
                 (magent-thread-turns thread))))

(defun magent-thread-messages (thread)
  "Return legacy session message projections for THREAD."
  (let (messages)
    (dolist (item (magent-thread-all-items thread) (nreverse messages))
      (pcase (magent-thread-item-type item)
        ('message
         (when (memq (magent-thread-item-role item) '(user assistant))
           (push `((role . ,(magent-thread-item-role item))
                   (content . ,(magent-thread-item-content item)))
                 messages)))
        ('tool
         (when (magent-thread-terminal-item-p item)
           (push `((role . tool)
                   (content . ,(list
                                :id (magent-thread-item-call-id item)
                                :name (magent-json-safe-name
                                       (magent-thread-item-name item))
                                :args (magent-thread--tool-input-plist
                                       (magent-thread-item-input item))
                                :result
                                (let ((output
                                       (magent-thread-item-output item)))
                                  (if (stringp output)
                                      output
                                    (format "%s" output))))))
                 messages)))))))

(defun magent-thread--alist-to-keyword-plist (value)
  "Convert JSON-decoded alist VALUE to a keyword plist recursively."
  (cond
   ((eq value :null) nil)
   ((magent-json--alist-p value)
    (let (out)
      (dolist (entry value out)
        (let ((key (car entry))
              (val (cdr entry)))
          (setq out
                (append out
                        (list (if (keywordp key)
                                  key
                                (intern (concat ":"
                                                (magent-json-safe-name key))))
                              (magent-thread--alist-to-keyword-plist
                               val))))))))
   ((consp value)
    (mapcar #'magent-thread--alist-to-keyword-plist value))
   ((vectorp value)
    (mapcar #'magent-thread--alist-to-keyword-plist (append value nil)))
   (t value)))

(defun magent-thread--legacy-tool-value (value)
  "Return VALUE in the legacy plist/list shape used by session messages."
  (cond
   ((null value) nil)
   ((eq value :null) nil)
   ((symbolp value) (magent-json--symbol-value value))
   ((vectorp value)
    (mapcar #'magent-thread--legacy-tool-value (append value nil)))
   ((magent-json--alist-p value)
    (let (out)
      (dolist (entry value out)
        (setq out
              (append out
                      (list (if (keywordp (car entry))
                                (car entry)
                              (intern (concat ":"
                                              (magent-json-safe-name
                                               (car entry)))))
                            (magent-thread--legacy-tool-value
                             (cdr entry))))))))
   ((magent-json--plist-p value)
    (let (out)
      (while value
        (let ((key (pop value))
              (val (pop value)))
          (unless (null val)
            (setq out
                  (append out
                          (list key
                                (magent-thread--legacy-tool-value
                                 val)))))))
      out))
   ((consp value)
    (mapcar #'magent-thread--legacy-tool-value value))
   (t value)))

(defun magent-thread--tool-input-plist (input)
  "Return tool INPUT in the legacy keyword plist shape."
  (magent-thread--legacy-tool-value
   (magent-thread--alist-to-keyword-plist input)))

(defun magent-thread--model-visible-tool-result (result)
  "Return RESULT bounded for model-visible tool history."
  (let* ((value (if (magent-tool-result-p result)
                    (magent-tool-result-output result)
                  result))
         (safe-result (if (stringp value)
                          value
                        (magent-json-safe-value value)))
         (text (if (stringp safe-result)
                   safe-result
                 (format "%s" safe-result)))
         (max-length magent-tool-result-model-max-length))
    (if (and (numberp max-length)
             (> max-length 0)
             (> (length text) max-length))
        (let* ((preview-length
                (min (max 0 (or magent-tool-result-model-preview-length
                                max-length))
                     max-length
                     (length text)))
               (truncated (- (length text) preview-length)))
          (format "%s\n\n[Tool result truncated: original %d characters, kept first %d, omitted %d. Narrow the command, read a smaller range, or refine the query for more detail.]"
                  (substring text 0 preview-length)
                  (length text)
                  preview-length
                  truncated))
      safe-result)))

(defun magent-thread-record-message
    (thread turn-id role content &optional phase metadata)
  "Record a terminal message item in THREAD for TURN-ID."
  (let ((item (magent-thread-start-item
               thread turn-id 'message
               :role role
               :content content
               :phase phase
               :metadata metadata)))
    (magent-thread-complete-item thread item)
    item))

(defun magent-thread-turn-message-item
    (turn role &optional content)
  "Return TURN's first message item for ROLE.
When CONTENT is non-nil, require an `equal' content match."
  (cl-find-if
   (lambda (item)
     (and (eq (magent-thread-item-type item) 'message)
          (eq (magent-thread-item-role item) role)
          (or (null content)
              (equal (magent-thread-item-content item) content))))
   (magent-thread-turn-items turn)))

(defun magent-thread-record-user-message-if-needed
    (thread turn-id content &optional phase metadata)
  "Record CONTENT as TURN-ID's user message unless one already exists.
Return the existing or newly created item."
  (when-let* ((turn (magent-thread--find-turn thread turn-id)))
    (unless (magent-thread-turn-input turn)
      (setf (magent-thread-turn-input turn)
            (if (stringp content) content (format "%s" content))))
    (or (magent-thread-turn-message-item turn 'user)
        (magent-thread-record-message
         thread turn-id 'user content phase metadata))))

(defun magent-thread-ensure-message-item
    (thread turn-id role &optional phase metadata)
  "Return an in-progress message item for ROLE in TURN-ID.
Create it when needed."
  (when-let* ((turn (magent-thread--find-turn thread turn-id)))
    (or (cl-find-if
         (lambda (item)
           (and (eq (magent-thread-item-type item) 'message)
                (eq (magent-thread-item-role item) role)
                (not (magent-thread-terminal-item-p item))))
         (magent-thread-turn-items turn))
        (magent-thread-start-item
         thread turn-id 'message
         :role role
         :phase phase
         :metadata metadata))))

(defun magent-thread-append-item-content
    (thread item chunk &optional output-p)
  "Append CHUNK to ITEM's content, or output when OUTPUT-P is non-nil.
This updates the materialized snapshot only; callers should complete or
fail the item with a terminal journal event containing the final content."
  (when (and item (stringp chunk) (> (length chunk) 0))
    (let* ((old (if output-p
                    (magent-thread-item-output item)
                  (magent-thread-item-content item)))
           (new (concat (or old "") chunk))
           (now (magent-thread--now)))
      (if output-p
          (setf (magent-thread-item-output item) new)
        (setf (magent-thread-item-content item) new))
      (setf (magent-thread-item-updated-at item) now)
      (magent-thread--update-timestamp thread now)))
  item)

(defun magent-thread-record-tool-result
    (thread turn-id call-id name args result &optional metadata)
  "Record a merged tool call/result lifecycle item in THREAD."
  (let* ((safe-name (magent-json-safe-name name))
         (safe-args (magent-json-safe-tool-args args))
         (normalized (magent-tool-result-normalize result safe-name call-id))
         (status (magent-tool-result-status-value normalized))
         (safe-result (magent-thread--model-visible-tool-result normalized))
         (result-metadata
          (append
           (when (magent-tool-result-exit-code normalized)
             (list :exit-code (magent-tool-result-exit-code normalized)))
           (magent-tool-result-metadata normalized)
           metadata))
         (item (or (magent-thread--find-item thread call-id)
                   (magent-thread-start-item
                    thread turn-id 'tool
                    :id call-id
                    :call-id call-id
                    :name safe-name
                    :input safe-args
                    :metadata result-metadata))))
    (if (not (eq status 'completed))
        (magent-thread-fail-item
         thread item (or (magent-tool-result-error normalized) safe-result)
         :call-id call-id
         :name safe-name
         :input safe-args
         :output safe-result
         :metadata result-metadata)
      (magent-thread-complete-item
       thread item
       :call-id call-id
       :name safe-name
       :input safe-args
       :output safe-result
       :metadata result-metadata))
    item))

(provide 'magent-ledger)
;;; magent-ledger.el ends here
