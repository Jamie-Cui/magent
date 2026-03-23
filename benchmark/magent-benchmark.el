;;; magent-benchmark.el --- Benchmark harness for Magent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;;; Commentary:

;; Offline-replay benchmark harness for comparing Magent with capability
;; progressive disclosure enabled vs disabled.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'subr-x)

(defconst magent-benchmark-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing Magent benchmark files.")

(defconst magent-benchmark-repo-root
  (expand-file-name ".." magent-benchmark-directory)
  "Repository root used for benchmark fixture lookup.")

(add-to-list 'load-path magent-benchmark-directory)
(add-to-list 'load-path magent-benchmark-repo-root)

(require 'magent-agent)
(require 'magent-approval)
(require 'magent-events)
(require 'magent-fsm)
(require 'magent-session)

(declare-function gptel-backend-name "gptel")
(declare-function magent--ensure-initialized "magent")

(cl-defstruct (magent-benchmark-task
               (:constructor magent-benchmark-task-create))
  "One benchmark task definition."
  id
  title
  category
  prompt
  setup-fn
  score-fn
  expected-capabilities
  forbidden-capabilities
  (timeout 120.0)
  description
  tags)

(defconst magent-benchmark-sensitive-tools
  '("bash" "emacs_eval" "write_file" "edit_file" "delegate")
  "Tool names counted as sensitive in benchmark summaries.")

(defvar magent-benchmark-suite nil
  "List of `magent-benchmark-task' objects loaded from the benchmark suite.")

(defun magent-benchmark-load-suite ()
  "Load and return `magent-benchmark-suite'."
  (require 'magent-benchmark-suite)
  magent-benchmark-suite)

(defun magent-benchmark-fixture-path (relative-path)
  "Return absolute benchmark fixture path for RELATIVE-PATH."
  (expand-file-name relative-path
                    (expand-file-name "fixtures" magent-benchmark-directory)))

(defun magent-benchmark-default-output-directory ()
  "Return the default timestamped benchmark output directory."
  (expand-file-name
   (format-time-string "results/%Y%m%d-%H%M%S")
   magent-benchmark-directory))

(defun magent-benchmark-buffer-context (buffer &optional project-root)
  "Build a Magent request context plist for BUFFER."
  (with-current-buffer buffer
    (list :buffer-name (buffer-name)
          :file-path (buffer-file-name)
          :major-mode major-mode
          :project-root (or project-root
                            (ignore-errors (magent-project-root))
                            default-directory)
          :region-active (use-region-p)
          :features (copy-sequence features))))

(defun magent-benchmark-open-file-buffer (relative-path &optional mutate-fn)
  "Open fixture RELATIVE-PATH and return benchmark setup state.
MUTATE-FN, when non-nil, is called in the visiting buffer before the
resulting context is captured."
  (let* ((path (magent-benchmark-fixture-path relative-path))
         (project-root (file-name-directory path))
         (buffer (find-file-noselect path)))
    (with-current-buffer buffer
      (when (buffer-modified-p)
        (set-buffer-modified-p nil))
      (revert-buffer :ignore-auto :noconfirm)
      (setq buffer-read-only nil)
      (when mutate-fn
        (funcall mutate-fn buffer))
      (goto-char (point-min)))
    (list
     :buffer buffer
     :file-path path
     :project-root project-root
     :request-context (magent-benchmark-buffer-context buffer project-root)
     :cleanup
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (buffer-modified-p)
             (set-buffer-modified-p nil)))
         (kill-buffer buffer))))))

(defun magent-benchmark-first-integer (text)
  "Extract the first integer from TEXT, or nil when absent."
  (when (and (stringp text)
             (string-match "\\([-+]?[0-9]+\\)" text))
    (string-to-number (match-string 1 text))))

(defun magent-benchmark-response-matches-p (response regexp)
  "Return non-nil when RESPONSE matches REGEXP case-insensitively."
  (let ((case-fold-search t))
    (and (stringp response)
         (string-match-p regexp response))))

(defun magent-benchmark-count-org-headings (buffer)
  "Return total number of org headings in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward org-heading-regexp nil t)
          (setq count (1+ count)))
        count))))

(defun magent-benchmark-count-org-top-level-headings (buffer)
  "Return number of top-level org headings in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward "^\\* " nil t)
          (setq count (1+ count)))
        count))))

(defun magent-benchmark-count-org-todos (buffer)
  "Return number of TODO headings in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward "^\\*+ +TODO\\b" nil t)
          (setq count (1+ count)))
        count))))

(defun magent-benchmark-buffer-line-count (path)
  "Return line count for file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (count-lines (point-min) (point-max))))

(defun magent-benchmark-file-first-line (path)
  "Return the first line from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties
     (line-beginning-position)
     (line-end-position))))

(cl-defun magent-benchmark-make-score (&key task-success constraint-success
                                            quality-score notes)
  "Return a normalized benchmark score plist."
  (list :task-success (and task-success t)
        :constraint-success (and constraint-success t)
        :quality-score (or quality-score 0)
        :notes (delq nil (copy-sequence notes))))

(defun magent-benchmark-run-tool-used-p (run tool-name)
  "Return non-nil when RUN used TOOL-NAME."
  (cl-find-if (lambda (tool-call)
                (equal (cdr (assq 'tool_name tool-call)) tool-name))
              (plist-get run :tool-calls)))

(cl-defun magent-benchmark-score-integer-answer (run expected
                                                     &key require-tool
                                                     forbid-tools)
  "Score RUN by comparing its first integer answer with EXPECTED."
  (let* ((response (plist-get run :response))
         (actual (magent-benchmark-first-integer response))
         (tool-ok (or (null require-tool)
                      (magent-benchmark-run-tool-used-p run require-tool)))
         (forbidden-hit
          (cl-find-if (lambda (tool-name)
                        (magent-benchmark-run-tool-used-p run tool-name))
                      forbid-tools))
         (task-success (and (numberp actual)
                            (= actual expected)))
         (constraint-success (and tool-ok (not forbidden-hit)))
         (notes nil))
    (unless tool-ok
      (push (format "missing required tool %s" require-tool) notes))
    (when forbidden-hit
      (push (format "used forbidden tool %s" forbidden-hit) notes))
    (unless task-success
      (push (format "expected %s got %s" expected actual) notes))
    (magent-benchmark-make-score
     :task-success task-success
     :constraint-success constraint-success
     :quality-score (cond
                     ((and task-success constraint-success) 2)
                     (task-success 1)
                     (t 0))
     :notes (nreverse notes))))

(cl-defun magent-benchmark-score-regexp-answer (run regexp
                                                    &key require-tool
                                                    forbid-tools)
  "Score RUN by matching REGEXP against the response."
  (let* ((response (plist-get run :response))
         (task-success (magent-benchmark-response-matches-p response regexp))
         (tool-ok (or (null require-tool)
                      (magent-benchmark-run-tool-used-p run require-tool)))
         (forbidden-hit
          (cl-find-if (lambda (tool-name)
                        (magent-benchmark-run-tool-used-p run tool-name))
                      forbid-tools))
         (notes nil))
    (unless task-success
      (push (format "response did not match %s" regexp) notes))
    (unless tool-ok
      (push (format "missing required tool %s" require-tool) notes))
    (when forbidden-hit
      (push (format "used forbidden tool %s" forbidden-hit) notes))
    (magent-benchmark-make-score
     :task-success task-success
     :constraint-success (and tool-ok (not forbidden-hit))
     :quality-score (cond
                     ((and task-success tool-ok (not forbidden-hit)) 2)
                     (task-success 1)
                     (t 0))
     :notes (nreverse notes))))

(defun magent-benchmark--plist-p (value)
  "Return non-nil when VALUE looks like a plist."
  (and (listp value)
       (or (null value)
           (and (keywordp (car value))
                (zerop (% (length value) 2))
                (cl-loop for (key _val) on value by #'cddr
                         always (keywordp key))))))

(defun magent-benchmark--jsonify (value)
  "Convert VALUE into a `json-encode'-friendly structure."
  (cond
   ((eq value t) t)
   ((hash-table-p value)
    (let (pairs)
      (maphash (lambda (key item)
                 (push (cons (format "%s" key)
                             (magent-benchmark--jsonify item))
                       pairs))
               value)
      (nreverse pairs)))
   ((magent-benchmark--plist-p value)
    (let (pairs)
      (while value
        (let ((key (substring (symbol-name (pop value)) 1))
              (item (pop value)))
          (push (cons key (magent-benchmark--jsonify item)) pairs)))
      (nreverse pairs)))
   ((and (listp value)
         (consp value)
         (consp (car value))
         (or (symbolp (caar value))
             (stringp (caar value))))
    (mapcar (lambda (pair)
              (cons (format "%s" (car pair))
                    (magent-benchmark--jsonify (cdr pair))))
            value))
   ((listp value)
    (mapcar #'magent-benchmark--jsonify value))
   ((vectorp value)
    (mapcar #'magent-benchmark--jsonify value))
   ((keywordp value)
    (substring (symbol-name value) 1))
   ((symbolp value)
    (symbol-name value))
   (t value)))

(defun magent-benchmark--write-json-file (path object)
  "Write OBJECT as JSON to PATH."
  (make-directory (file-name-directory path) t)
  (with-temp-buffer
    (insert (json-encode (magent-benchmark--jsonify object)))
    (insert "\n")
    (write-region (point-min) (point-max) path nil 'silent)))

(defun magent-benchmark--append-jsonl-record (path object)
  "Append OBJECT as one JSON line to PATH."
  (make-directory (file-name-directory path) t)
  (with-temp-buffer
    (insert (json-encode (magent-benchmark--jsonify object)))
    (insert "\n")
    (append-to-file (point-min) (point-max) path)))

(defun magent-benchmark--events-of-type (events type)
  "Return EVENTS whose `:type' equals TYPE."
  (cl-remove-if-not (lambda (event)
                      (eq (plist-get event :type) type))
                    events))

(defun magent-benchmark--tool-status (result)
  "Return compact status string for RESULT."
  (let ((text (format "%s" result)))
    (cond
     ((string-match-p "\\`Error\\b" text) "error")
     ((string-match-p "timed out" text) "timeout")
     (t "ok"))))

(defun magent-benchmark--summarize-tool-calls (events)
  "Return tool call summaries derived from EVENTS."
  (let ((starts (make-hash-table :test 'equal))
        calls)
    (dolist (event events)
      (pcase (plist-get event :type)
        ('tool-call-start
         (puthash (plist-get event :call-id) event starts))
        ('tool-call-end
         (let* ((call-id (plist-get event :call-id))
                (start (gethash call-id starts))
                (start-time (plist-get start :time))
                (end-time (plist-get event :time))
                (duration-ms (and start-time end-time
                                  (round (* 1000.0 (- end-time start-time))))))
           (push
            `((call_id . ,call-id)
              (tool_name . ,(plist-get event :tool-name))
              (status . ,(magent-benchmark--tool-status
                          (plist-get event :result)))
              (duration_ms . ,duration-ms)
              (summary . ,(plist-get start :summary))
              (result_preview . ,(when-let ((result (plist-get event :result)))
                                   (truncate-string-to-width
                                    (format "%s" result) 120 nil nil "..."))))
            calls)))))
    (nreverse calls)))

(defun magent-benchmark--summarize-llm-requests (events)
  "Return LLM request summaries derived from EVENTS."
  (let ((starts (make-hash-table :test 'equal))
        requests)
    (dolist (event events)
      (pcase (plist-get event :type)
        ('llm-request-start
         (puthash (plist-get event :request-id) event starts))
        ('llm-request-end
         (let* ((request-id (plist-get event :request-id))
                (start (gethash request-id starts)))
           (push
            `((request_id . ,request-id)
              (status . ,(format "%s" (plist-get event :status)))
              (backend . ,(plist-get event :backend))
              (model . ,(plist-get event :model))
              (prompt_count . ,(plist-get start :prompt-count))
              (tool_count . ,(plist-get start :tool-count))
              (system_prompt_length . ,(plist-get start :system-prompt-length))
              (input_tokens . ,(plist-get event :input-tokens))
              (output_tokens . ,(plist-get event :output-tokens))
              (total_tokens . ,(plist-get event :total-tokens))
              (usage_available . ,(plist-get event :usage-available))
              (request_bytes . ,(plist-get event :request-bytes))
              (response_chars . ,(plist-get event :response-chars))
              (tool_use_count . ,(plist-get event :tool-use-count))
              (stop_reason . ,(when-let ((value (plist-get event :stop-reason)))
                                (format "%s" value)))
              (http_status . ,(when-let ((value (plist-get event :http-status)))
                                (format "%s" value)))
              (error . ,(when-let ((value (plist-get event :error)))
                          (format "%s" value))))
            requests)))))
    (nreverse requests)))

(defun magent-benchmark--sum-number-field (records field)
  "Sum numeric FIELD values across RECORDS.
Return nil when no numeric values are present."
  (let ((sum 0)
        (count 0))
    (dolist (record records)
      (let ((value (cdr (assq field record))))
        (when (numberp value)
          (setq sum (+ sum value)
                count (1+ count)))))
    (when (> count 0) sum)))

(defun magent-benchmark--approval-hook-record (event request-id entry)
  "Return one benchmark approval record from EVENT, REQUEST-ID, and ENTRY."
  (let* ((request (or (plist-get entry :request) entry))
         (tool-name (plist-get request :tool-name))
         (decision (plist-get entry :decision)))
    (list :time (float-time)
          :event event
          :request-id request-id
          :tool-name tool-name
          :decision decision)))

(defun magent-benchmark--capability-resolution (events)
  "Return the last capability resolution payload found in EVENTS."
  (when-let ((event (car (last (magent-benchmark--events-of-type
                                events 'capability-resolution)))))
    (plist-get event :resolution)))

(defun magent-benchmark--activation-summary (task active-capabilities)
  "Return activation summary alist for TASK and ACTIVE-CAPABILITIES."
  (let* ((expected (copy-sequence
                    (magent-benchmark-task-expected-capabilities task)))
         (forbidden (copy-sequence
                     (magent-benchmark-task-forbidden-capabilities task)))
         (missed (cl-set-difference expected active-capabilities :test #'equal))
         (unexpected (cl-set-difference active-capabilities expected :test #'equal))
         (forbidden-hit (cl-intersection forbidden active-capabilities :test #'equal)))
    `((expected . ,expected)
      (active . ,active-capabilities)
      (missed . ,missed)
      (unexpected . ,unexpected)
      (forbidden . ,forbidden)
      (forbidden_hit . ,forbidden-hit)
      (activation_hit . ,(null missed))
      (activation_clean . ,(and (null unexpected)
                                (null forbidden-hit))))))

(defun magent-benchmark--collect-run-metrics (task arm iteration setup-state
                                                   events approvals response
                                                   duration-ms timed-out)
  "Assemble normalized run metrics for one task execution."
  (let* ((llm-requests (magent-benchmark--summarize-llm-requests events))
         (tool-calls (magent-benchmark--summarize-tool-calls events))
         (capability-resolution (magent-benchmark--capability-resolution events))
         (active-capabilities (copy-sequence
                               (plist-get capability-resolution
                                          :active-capabilities)))
         (suggested-capabilities (copy-sequence
                                  (plist-get capability-resolution
                                             :suggested-capabilities)))
         (turn-end (car (last (magent-benchmark--events-of-type events 'turn-end))))
         (turn-status (when turn-end
                        (format "%s" (plist-get turn-end :status))))
         (approval-request-count
          (cl-count-if (lambda (record)
                         (eq (plist-get record :event) 'requested))
                       approvals))
         (sensitive-tool-call-count
          (cl-count-if (lambda (record)
                         (member (cdr (assq 'tool_name record))
                                 magent-benchmark-sensitive-tools))
                       tool-calls))
         (score (funcall (magent-benchmark-task-score-fn task)
                         task
                         (list :task task
                               :arm arm
                               :iteration iteration
                               :response response
                               :events events
                               :tool-calls tool-calls
                               :llm-requests llm-requests
                               :approvals approvals
                               :setup setup-state
                               :expected (plist-get setup-state :expected)
                               :timed-out timed-out
                               :turn-status turn-status
                               :active-capabilities active-capabilities
                               :suggested-capabilities suggested-capabilities)))
         (task-success (plist-get score :task-success))
         (constraint-success (plist-get score :constraint-success))
         (primary-success (and task-success constraint-success (not timed-out)))
         (notes (plist-get score :notes)))
    `((task_id . ,(magent-benchmark-task-id task))
      (title . ,(magent-benchmark-task-title task))
      (category . ,(magent-benchmark-task-category task))
      (arm . ,(symbol-name arm))
      (iteration . ,iteration)
      (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
      (prompt . ,(magent-benchmark-task-prompt task))
      (response . ,response)
      (timed_out . ,timed-out)
      (duration_ms . ,duration-ms)
      (turn_status . ,turn-status)
      (task_success . ,task-success)
      (constraint_success . ,constraint-success)
      (primary_success . ,primary-success)
      (quality_score . ,(plist-get score :quality-score))
      (notes . ,notes)
      (activation . ,(magent-benchmark--activation-summary task active-capabilities))
      (capability_resolution . ,capability-resolution)
      (input_tokens . ,(magent-benchmark--sum-number-field llm-requests 'input_tokens))
      (output_tokens . ,(magent-benchmark--sum-number-field llm-requests 'output_tokens))
      (total_tokens . ,(magent-benchmark--sum-number-field llm-requests 'total_tokens))
      (request_bytes . ,(magent-benchmark--sum-number-field llm-requests 'request_bytes))
      (llm_request_count . ,(length llm-requests))
      (usage_covered_request_count
       . ,(cl-count-if (lambda (record)
                         (cdr (assq 'usage_available record)))
                       llm-requests))
      (tool_call_count . ,(length tool-calls))
      (sensitive_tool_call_count . ,sensitive-tool-call-count)
      (approval_request_count . ,approval-request-count)
      (llm_requests . ,llm-requests)
      (tool_calls . ,tool-calls)
      (approvals . ,(mapcar #'magent-benchmark--jsonify approvals))
      (setup_metadata . ,(plist-get setup-state :metadata)))))

(defun magent-benchmark--mean (values)
  "Return mean of numeric VALUES, or nil."
  (when values
    (/ (apply #'+ values) (float (length values)))))

(defun magent-benchmark--median (values)
  "Return median of numeric VALUES, or nil."
  (when values
    (let* ((sorted (sort (copy-sequence values) #'<))
           (len (length sorted))
           (mid (/ len 2)))
      (if (cl-oddp len)
          (nth mid sorted)
        (/ (+ (nth (1- mid) sorted)
              (nth mid sorted))
           2.0)))))

(defun magent-benchmark--ratio (num den)
  "Return NUM / DEN as float, or nil when DEN is zero."
  (when (> den 0)
    (/ num (float den))))

(defun magent-benchmark--numeric-field-values (records field)
  "Return numeric FIELD values across RECORDS."
  (let (values)
    (dolist (record records (nreverse values))
      (let ((value (cdr (assq field record))))
        (when (numberp value)
          (push value values))))))

(defun magent-benchmark--summarize-records (records)
  "Return aggregate summary alist for benchmark RECORDS."
  (let* ((executed (cl-remove-if (lambda (record)
                                   (cdr (assq 'skipped record)))
                                 records))
         (primary-success-count
          (cl-count-if (lambda (record)
                         (cdr (assq 'primary_success record)))
                       executed))
         (task-success-count
          (cl-count-if (lambda (record)
                         (cdr (assq 'task_success record)))
                       executed))
         (quality-values (magent-benchmark--numeric-field-values
                          executed 'quality_score))
         (token-values (magent-benchmark--numeric-field-values
                        executed 'total_tokens))
         (request-byte-values (magent-benchmark--numeric-field-values
                               executed 'request_bytes))
         (tool-call-values (magent-benchmark--numeric-field-values
                            executed 'tool_call_count))
         (approval-values (magent-benchmark--numeric-field-values
                           executed 'approval_request_count))
         (duration-values (magent-benchmark--numeric-field-values
                           executed 'duration_ms))
         (activation-rows
          (mapcar (lambda (record)
                    (cdr (assq 'activation record)))
                  executed))
         (expected-total
          (apply #'+
                 (mapcar (lambda (activation)
                           (length (cdr (assq 'expected activation))))
                         activation-rows)))
         (missed-total
          (apply #'+
                 (mapcar (lambda (activation)
                           (length (cdr (assq 'missed activation))))
                         activation-rows)))
         (actual-total
          (apply #'+
                 (mapcar (lambda (activation)
                           (length (cdr (assq 'active activation))))
                         activation-rows)))
         (unexpected-total
          (apply #'+
                 (mapcar (lambda (activation)
                           (length (cdr (assq 'unexpected activation))))
                         activation-rows))))
    `((runs . ,(length executed))
      (primary_success_rate
       . ,(magent-benchmark--ratio primary-success-count (length executed)))
      (task_success_rate
       . ,(magent-benchmark--ratio task-success-count (length executed)))
      (mean_quality_score . ,(magent-benchmark--mean quality-values))
      (mean_total_tokens . ,(magent-benchmark--mean token-values))
      (median_total_tokens . ,(magent-benchmark--median token-values))
      (mean_request_bytes . ,(magent-benchmark--mean request-byte-values))
      (mean_tool_call_count . ,(magent-benchmark--mean tool-call-values))
      (mean_approval_request_count . ,(magent-benchmark--mean approval-values))
      (mean_duration_ms . ,(magent-benchmark--mean duration-values))
      (activation_recall
       . ,(magent-benchmark--ratio (- expected-total missed-total)
                                   expected-total))
      (activation_precision
       . ,(magent-benchmark--ratio (- actual-total unexpected-total)
                                   actual-total)))))

(defun magent-benchmark--group-records (records field)
  "Return alist grouping RECORDS by FIELD."
  (let (groups)
    (dolist (record records)
      (let* ((value (cdr (assq field record)))
             (cell (assoc value groups)))
        (if cell
            (setcdr cell (cons record (cdr cell)))
          (push (cons value (list record)) groups))))
    (nreverse groups)))

(defun magent-benchmark--paired-summary (records)
  "Return summary of paired control/treatment deltas for RECORDS."
  (let (pairs)
    (dolist (group (magent-benchmark--group-records records 'task_id))
      (dolist (iteration-group
               (magent-benchmark--group-records (cdr group) 'iteration))
        (let* ((runs (cdr iteration-group))
               (control (cl-find-if (lambda (record)
                                      (equal (cdr (assq 'arm record)) "control"))
                                    runs))
               (treatment (cl-find-if (lambda (record)
                                        (equal (cdr (assq 'arm record)) "treatment"))
                                      runs)))
          (when (and control treatment
                     (not (cdr (assq 'skipped control)))
                     (not (cdr (assq 'skipped treatment))))
            (push
             `((task_id . ,(cdr (assq 'task_id control)))
               (category . ,(cdr (assq 'category control)))
               (iteration . ,(cdr (assq 'iteration control)))
               (primary_success_delta
                . ,(- (if (cdr (assq 'primary_success treatment)) 1 0)
                       (if (cdr (assq 'primary_success control)) 1 0)))
               (total_tokens_delta
                . ,(let ((treatment-value (cdr (assq 'total_tokens treatment)))
                         (control-value (cdr (assq 'total_tokens control))))
                     (when (and (numberp treatment-value)
                                (numberp control-value))
                       (- treatment-value control-value))))
               (request_bytes_delta
                . ,(let ((treatment-value (cdr (assq 'request_bytes treatment)))
                         (control-value (cdr (assq 'request_bytes control))))
                     (when (and (numberp treatment-value)
                                (numberp control-value))
                       (- treatment-value control-value))))
               (tool_call_delta
                . ,(- (or (cdr (assq 'tool_call_count treatment)) 0)
                       (or (cdr (assq 'tool_call_count control)) 0))))
             pairs)))))
    (let ((success-deltas (magent-benchmark--numeric-field-values
                           pairs 'primary_success_delta))
          (token-deltas (magent-benchmark--numeric-field-values
                         pairs 'total_tokens_delta))
          (request-byte-deltas (magent-benchmark--numeric-field-values
                                pairs 'request_bytes_delta))
          (tool-deltas (magent-benchmark--numeric-field-values
                        pairs 'tool_call_delta)))
      `((pairs . ,(nreverse pairs))
        (mean_primary_success_delta . ,(magent-benchmark--mean success-deltas))
        (mean_total_tokens_delta . ,(magent-benchmark--mean token-deltas))
        (mean_request_bytes_delta . ,(magent-benchmark--mean request-byte-deltas))
        (mean_tool_call_delta . ,(magent-benchmark--mean tool-deltas))))))

(defun magent-benchmark--summary-markdown (summary)
  "Render SUMMARY as Markdown."
  (let* ((by-arm (cdr (assq 'by_arm summary)))
         (by-category (cdr (assq 'by_category summary)))
         (paired (cdr (assq 'paired summary))))
    (string-join
     (delq nil
           (list
            "# Magent Progressive Disclosure Benchmark"
            ""
            (format "- Generated: %s" (cdr (assq 'generated_at summary)))
            (format "- Model: `%s`" (or (cdr (assq 'model summary)) "<unknown>"))
            (format "- Backend: `%s`" (or (cdr (assq 'backend summary)) "<unknown>"))
            (format "- Runs: %s" (cdr (assq 'run_count summary)))
            ""
            "## By Arm"
            ""
            "| Arm | Runs | Primary Success | Task Success | Mean Quality | Mean Tokens | Mean Request Bytes | Mean Tool Calls | Mean Approvals |"
            "| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"
            (mapconcat
             (lambda (row)
               (let ((metrics (cdr row)))
                 (format "| %s | %d | %.3f | %.3f | %.3f | %s | %s | %.3f | %.3f |"
                         (car row)
                         (or (cdr (assq 'runs metrics)) 0)
                         (or (cdr (assq 'primary_success_rate metrics)) 0.0)
                         (or (cdr (assq 'task_success_rate metrics)) 0.0)
                         (or (cdr (assq 'mean_quality_score metrics)) 0.0)
                         (or (cdr (assq 'mean_total_tokens metrics)) "n/a")
                         (or (cdr (assq 'mean_request_bytes metrics)) "n/a")
                         (or (cdr (assq 'mean_tool_call_count metrics)) 0.0)
                         (or (cdr (assq 'mean_approval_request_count metrics)) 0.0))))
             by-arm
             "\n")
            ""
            "## By Category"
            ""
            "| Category | Arm | Primary Success | Mean Quality | Mean Tokens | Mean Request Bytes |"
            "| --- | --- | ---: | ---: | ---: | ---: |"
            (mapconcat
             (lambda (row)
               (let ((category (cdr (assq 'category row)))
                     (arm (cdr (assq 'arm row)))
                     (metrics (cdr (assq 'metrics row))))
                 (format "| %s | %s | %.3f | %.3f | %s | %s |"
                         category
                         arm
                         (or (cdr (assq 'primary_success_rate metrics)) 0.0)
                         (or (cdr (assq 'mean_quality_score metrics)) 0.0)
                         (or (cdr (assq 'mean_total_tokens metrics)) "n/a")
                         (or (cdr (assq 'mean_request_bytes metrics)) "n/a"))))
             by-category
             "\n")
            ""
            "## Paired Deltas"
            ""
            (format "- Mean primary-success delta (treatment - control): %.3f"
                    (or (cdr (assq 'mean_primary_success_delta paired)) 0.0))
            (format "- Mean total-token delta (treatment - control): %s"
                    (or (cdr (assq 'mean_total_tokens_delta paired)) "n/a"))
            (format "- Mean request-bytes delta (treatment - control): %s"
                    (or (cdr (assq 'mean_request_bytes_delta paired)) "n/a"))
            (format "- Mean tool-call delta (treatment - control): %.3f"
                    (or (cdr (assq 'mean_tool_call_delta paired)) 0.0))))
     "\n")))

(defun magent-benchmark--run-task (task arm iteration raw-path)
  "Run TASK for ARM and ITERATION, appending the result to RAW-PATH."
  (let ((events nil)
        (approvals nil)
        (magent-buffer-name "*magent-benchmark*")
        (magent-enable-audit-log nil)
        (setup-state nil)
        (fsm nil)
        (response nil)
        (timed-out nil)
        (start-time (float-time))
        (event-sink nil)
        (approval-hook nil)
        record)
    (setq event-sink (lambda (event) (push event events)))
    (setq approval-hook
          (lambda (event request-id entry)
            (push (magent-benchmark--approval-hook-record event request-id entry)
                  approvals)))
    (unwind-protect
        (progn
          (condition-case err
              (progn
                (require 'magent)
                (magent--ensure-initialized)
                (setq setup-state
                      (if-let ((setup-fn (magent-benchmark-task-setup-fn task)))
                          (funcall setup-fn task)
                        nil))
                (if-let ((skip-reason (plist-get setup-state :skip)))
                    (setq record
                          `((task_id . ,(magent-benchmark-task-id task))
                            (title . ,(magent-benchmark-task-title task))
                            (category . ,(magent-benchmark-task-category task))
                            (arm . ,(symbol-name arm))
                            (iteration . ,iteration)
                            (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                            (skipped . t)
                            (skip_reason . ,skip-reason)))
                  (let* ((project-root (plist-get setup-state :project-root))
                         (request-context
                          (or (plist-get setup-state :request-context)
                              (when-let ((buffer (plist-get setup-state :buffer)))
                                (magent-benchmark-buffer-context
                                 buffer project-root))))
                         (scope (or project-root 'global))
                         (magent-enable-capabilities (eq arm 'treatment))
                         (done nil))
                    (magent-events-add-sink event-sink)
                    (add-hook 'magent-approval-state-change-functions approval-hook)
                    (magent-session-activate scope)
                    (magent-session-reset)
                    (magent-session-activate scope)
                    (setq fsm
                          (magent-agent-process
                           (magent-benchmark-task-prompt task)
                           (lambda (value)
                             (setq response value
                                   done t))
                           nil nil nil request-context))
                    (let ((deadline (+ start-time
                                       (magent-benchmark-task-timeout task))))
                      (while (and (not done)
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.1)))
                    (unless done
                      (setq timed-out t
                            done t)
                      (when fsm
                        (magent-fsm-abort fsm)))
                    (setq record
                          (magent-benchmark--collect-run-metrics
                           task arm iteration setup-state
                           (nreverse events) (nreverse approvals) response
                           (round (* 1000.0 (- (float-time) start-time)))
                           timed-out)))))
            (error
             (setq record
                   `((task_id . ,(magent-benchmark-task-id task))
                     (title . ,(magent-benchmark-task-title task))
                     (category . ,(magent-benchmark-task-category task))
                     (arm . ,(symbol-name arm))
                     (iteration . ,iteration)
                     (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                     (failed . t)
                     (error . ,(error-message-string err)))))))
      (when approval-hook
        (remove-hook 'magent-approval-state-change-functions approval-hook))
      (when event-sink
        (magent-events-remove-sink event-sink))
      (when (fboundp 'magent-session-reset)
        (ignore-errors (magent-session-reset)))
      (when-let ((cleanup (plist-get setup-state :cleanup)))
        (ignore-errors (funcall cleanup)))
    (magent-benchmark--append-jsonl-record raw-path record)
    (message "Benchmark %s [%s #%d] %s"
             (magent-benchmark-task-id task)
             arm iteration
             (if (cdr (assq 'primary_success record)) "ok" "done"))
    record)))

(cl-defun magent-benchmark-run-suite (&key tasks
                                           (repetitions 1)
                                           (arms '(control treatment))
                                           output-directory
                                           task-regexp)
  "Run benchmark TASKS and write raw and summarized outputs.
TASKS defaults to `magent-benchmark-suite'."
  (let* ((suite (or tasks (magent-benchmark-load-suite)))
         (filtered
          (if (string-empty-p (or task-regexp ""))
              suite
            (cl-remove-if-not
             (lambda (task)
               (string-match-p task-regexp (magent-benchmark-task-id task)))
             suite)))
         (output-dir (or output-directory
                         (magent-benchmark-default-output-directory)))
         (raw-path (expand-file-name "results-raw.jsonl" output-dir))
         records)
    (make-directory output-dir t)
    (dolist (task filtered)
      (dotimes (index repetitions)
        (dolist (arm arms)
          (push (magent-benchmark--run-task task arm (1+ index) raw-path)
                records))))
    (setq records (nreverse records))
    (let* ((executed (cl-remove-if (lambda (record)
                                     (or (cdr (assq 'skipped record))
                                         (cdr (assq 'failed record))))
                                   records))
           (by-arm
            (mapcar (lambda (group)
                      (cons (car group)
                            (magent-benchmark--summarize-records (cdr group))))
                    (magent-benchmark--group-records executed 'arm)))
           (by-category
            (cl-loop
             for category-group in (magent-benchmark--group-records executed 'category)
             append
             (mapcar (lambda (arm-group)
                       `((category . ,(car category-group))
                         (arm . ,(car arm-group))
                         (metrics . ,(magent-benchmark--summarize-records
                                      (cdr arm-group)))))
                     (magent-benchmark--group-records (cdr category-group) 'arm))))
           (paired (magent-benchmark--paired-summary executed))
           (summary
            `((generated_at . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
              (backend . ,(ignore-errors
                            (gptel-backend-name gptel-backend)))
              (model . ,(format "%s" gptel-model))
              (task_count . ,(length filtered))
              (run_count . ,(length records))
              (repetitions . ,repetitions)
              (by_arm . ,by-arm)
              (by_category . ,by-category)
              (paired . ,paired))))
      (magent-benchmark--write-json-file
       (expand-file-name "summary.json" output-dir)
       summary)
      (with-temp-buffer
        (insert (magent-benchmark--summary-markdown summary))
        (insert "\n")
        (write-region (point-min) (point-max)
                      (expand-file-name "summary.md" output-dir)
                      nil 'silent))
      (magent-benchmark--write-json-file
       (expand-file-name "manifest.json" output-dir)
       `((generated_at . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
         (tasks . ,(mapcar #'magent-benchmark-task-id filtered))
         (arms . ,(mapcar #'symbol-name arms))
         (repetitions . ,repetitions)
         (output_directory . ,output-dir)))
      (message "Benchmark complete: %s" output-dir)
      summary)))

(defun magent-benchmark-run-default (&optional repetitions)
  "Run the default benchmark suite with REPETITIONS per task and arm."
  (interactive "p")
  (magent-benchmark-run-suite :repetitions (or repetitions 1)))

(provide 'magent-benchmark)
;;; magent-benchmark.el ends here
