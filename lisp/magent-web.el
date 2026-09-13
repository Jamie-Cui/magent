;;; magent-web.el --- Search providers and web evidence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools

;;; Commentary:

;; Search adapters return a common result contract, independent of gptel.
;; Public page snapshots reuse the session's spill storage; references survive
;; replay and fork and expire under the existing storage policy.  Network and
;; PDF processes are asynchronous and share cancellation and resource limits.
;; This module does not change the model route or use browser credentials.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'dom)
(require 'json)
(require 'seq)
(require 'shr)
(require 'subr-x)
(require 'url-parse)
(require 'url-expand)
(require 'url-util)
(require 'xml)
(require 'magent-config)
(require 'magent-json)
(require 'magent-ledger)
(require 'magent-protocol)

(define-error 'magent-web-invalid-url "Invalid web URL")

(defvar magent-web--providers nil
  "Registered search adapters, keyed by symbol.")

(defun magent-web-register-search-provider (name function &optional filters)
  "Register search provider NAME with FUNCTION and supported FILTERS.
FUNCTION receives QUERY, OPTIONS plist and CALLBACK; it must return a
zero-argument cancellation function.  OPTIONS has :max-results (1..20),
:domains (list of host names), and :recency (days or nil).
CALLBACK receives (:results RESULTS) or (:error MESSAGE).  RESULTS is a
list of plists with required string :title, :url, :snippet, and optional
string :published-date.  Optional response :filter-note describes limits.
FILTERS is a subset of (domains recency).  Unsupported requested filters
fail before dispatch.  Callbacks after completion or cancellation are ignored.
Adapters are trusted Elisp; re-registering NAME replaces its implementation."
  (unless (and (symbolp name) name (functionp function)
               (listp filters) (seq-every-p (lambda (x) (memq x '(domains recency))) filters))
    (error "Invalid search provider registration"))
  (setf (alist-get name magent-web--providers)
        (list :function function :filters filters)))

(cl-defstruct (magent-web--job (:constructor magent-web--job-create))
  callback raw done timer process buffer directory cleanups)

(defun magent-web--cleanup (job)
  "Release JOB resources, reporting secondary cleanup failures."
  (dolist (cleanup
           (append
            (magent-web--job-cleanups job)
            (list (lambda ()
                    (when (timerp (magent-web--job-timer job))
                      (cancel-timer (magent-web--job-timer job))))
                  (lambda ()
                    (when (and (processp (magent-web--job-process job))
                               (process-live-p (magent-web--job-process job)))
                      (delete-process (magent-web--job-process job))))
                  (lambda ()
                    (when (buffer-live-p (magent-web--job-buffer job))
                      (kill-buffer (magent-web--job-buffer job))))
                  (lambda ()
                    (when (magent-web--job-directory job)
                      (delete-directory (magent-web--job-directory job) t))))))
    (condition-case err (funcall cleanup)
      (error (magent-log "Warning: Web cleanup failed: %s" (error-message-string err))))))

(defun magent-web--finish (job value)
  "Deliver VALUE once and release JOB."
  (unless (magent-web--job-done job)
    (setf (magent-web--job-done job) t)
    (magent-web--cleanup job)
    (funcall (magent-web--job-callback job) value)))

(defun magent-web--error (job message)
  "Fail JOB with MESSAGE, without exposing remote error bodies."
  (magent-web--finish
   job (if (magent-web--job-raw job) (list :error message)
         (magent-tool-result-create :success nil :status 'failed
                                    :output message :error message))))

(defun magent-web--guard (job function)
  "Run FUNCTION at an asynchronous JOB boundary and report errors."
  (unless (magent-web--job-done job)
    (condition-case err (funcall function)
      (error
       (if (magent-web--job-done job)
           (magent-log "Error: Web completion callback failed: %s" (error-message-string err))
         (magent-web--error job (error-message-string err)))))))

(defun magent-web--run (callback start &optional raw)
  "Start a bounded operation with CALLBACK and START, returning cancellation.
START receives a job.  RAW selects adapter rather than tool error results."
  (let ((job (magent-web--job-create :callback callback :raw raw)))
    (setf (magent-web--job-timer job)
          (run-at-time (max 1 magent-web-timeout) nil
                       (lambda () (magent-web--error job "Web operation timed out"))))
    (magent-web--guard job (lambda () (funcall start job)))
    (lambda ()
      (unless (magent-web--job-done job)
        (setf (magent-web--job-done job) t)
        (magent-web--cleanup job)))))

(defun magent-web--directory (job)
  "Return a private local temporary directory for JOB."
  (or (magent-web--job-directory job)
      (setf (magent-web--job-directory job)
            (let ((default-directory temporary-file-directory)
                  (directory (make-temp-file "magent-web-" t)))
              (set-file-modes directory #o700)
              directory))))

(defun magent-web--write (job name text)
  "Write TEXT to a private file NAME belonging to JOB."
  (let ((file (expand-file-name name (magent-web--directory job)))
        (coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent)
    (set-file-modes file #o600)
    file))

(defun magent-web--process (job command callback)
  "Run local COMMAND for JOB and pass bounded output to CALLBACK."
  (let* ((default-directory temporary-file-directory)
         (buffer (generate-new-buffer " *magent-web*"))
         (stderr-bytes 0)
         (stderr (make-pipe-process
                  :name "magent-web-stderr" :noquery t :coding 'binary
                  :filter (lambda (_process chunk)
                            (cl-incf stderr-bytes (string-bytes chunk))
                            (when (> stderr-bytes 65536)
                              (magent-web--error job "Web process diagnostics exceed size limit"))))))
    ;; Never mix converter warnings or transport diagnostics into source text.
    (push (lambda () (when (process-live-p stderr) (delete-process stderr)))
          (magent-web--job-cleanups job))
    (when (buffer-live-p (magent-web--job-buffer job))
      (kill-buffer (magent-web--job-buffer job)))
    (setf (magent-web--job-buffer job) buffer)
    (with-current-buffer buffer (set-buffer-multibyte nil))
    (setf (magent-web--job-process job)
          (make-process
           :name "magent-web" :buffer buffer :command command :stderr stderr
           :connection-type 'pipe :coding 'binary :noquery t
           :filter
           (lambda (_process chunk)
             (magent-web--guard
              job (lambda ()
                    (with-current-buffer buffer
                      (when (> (+ (buffer-size) (string-bytes chunk))
                               magent-web-max-response-bytes)
                        (error "Web response exceeds size limit"))
                      (goto-char (point-max)) (insert chunk)))))
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (magent-web--guard
                job (lambda ()
                      (unless (zerop (process-exit-status process))
                        (error "Web process failed (exit %d)" (process-exit-status process)))
                      (funcall callback
                               (with-current-buffer buffer (buffer-string)))))))))))

(defun magent-web--url (value)
  "Validate an HTTP(S) URL VALUE and return its parsed object.
Reject credentials, control characters, localhost and literal private IPs.
This is a URL guard, not a network sandbox or a DNS rebinding defense."
  (unless (and (stringp value) (< (length value) 8192)
               (not (string-match-p "[[:cntrl:] ]" value)))
    (signal 'magent-web-invalid-url nil))
  (let* ((url (url-generic-parse-url value))
         (host (downcase (string-trim (or (url-host url) "") "\\[" "\\]")))
         (parts (and (string-match-p "\\`[0-9.]+\\'" host)
                     (mapcar #'string-to-number (split-string host "\\."))))
         (private
          (or (string-match-p "\\`\\(?:localhost\\.?\\|.*\\.localhost\\.?\\|.*\\.local\\.?\\|0x[0-9a-f]+\\)\\'" host)
              (and (string-match-p ":" host)
                   (not (string-match-p "\\`[23][0-9a-f]*:" host)))
              (and parts
                   (or (/= (length parts) 4)
                       (seq-some (lambda (n) (> n 255)) parts)
                       (memq (car parts) '(0 10 127)) (>= (car parts) 224)
                       (and (= (car parts) 100) (<= 64 (cadr parts) 127))
                       (and (= (car parts) 169) (= (cadr parts) 254))
                       (and (= (car parts) 192) (= (cadr parts) 168))
                       (and (= (car parts) 172) (<= 16 (cadr parts) 31))
                       (string-match-p "\\`0[0-9]\\|\\.0[0-9]" host))))))
    (unless (and (member (url-type url) '("http" "https"))
                 (not (string-empty-p host))
                 (not (url-user url)) (not (url-password url))
                 (not (string-match-p "%" host)) (not private))
      (signal 'magent-web-invalid-url '("Use HTTP(S) without credentials or a local address")))
    url))

(defun magent-web--http (job url callback &optional payload key redirects)
  "Fetch URL for JOB and call CALLBACK with body, final URL and content type.
PAYLOAD is optional JSON; KEY is a bearer credential sent via a private file.
REDIRECTS tracks a bounded chain, with credentials never following redirects."
  (magent-web--url url)
  (let* ((curl (or (executable-find "curl") (error "Web tools require curl")))
         (headers (magent-web--write job "response-headers" ""))
         (command
          (list curl "--disable" "--silent" "--compressed"
                "--max-time" (number-to-string (max 1 magent-web-timeout))
                "--max-filesize" (number-to-string magent-web-max-response-bytes)
                "--proto" "=http,https" "--dump-header" headers)))
    (when key
      (unless (and (stringp key) (string-match-p "\\`[A-Za-z0-9_-]+\\'" key))
        (error "Invalid Tavily credential format"))
      (setq command
            (append command (list "--header"
                                  (concat "@" (magent-web--write job "authorization"
                                               (concat "Authorization: Bearer " key "\n")))))))
    (when payload
      (setq command
            (append command (list "--header" "Content-Type: application/json"
                                  "--data-binary"
                                  (concat "@" (magent-web--write job "request" payload))))))
    (magent-web--process
     job (append command (list "--url" url))
     (lambda (body)
       (let (status location content-type)
         (with-temp-buffer
           (insert-file-contents headers)
           (goto-char (point-min))
           ;; Proxy CONNECT and informational responses precede the final block.
           (while (re-search-forward "^HTTP/[^ ]+ +\\([0-9]+\\)" nil t)
             (setq status (string-to-number (match-string 1)) location nil content-type nil)
             (while (and (= (forward-line 1) 0) (not (looking-at-p "\r?$")))
               (let ((case-fold-search t))
                 (cond
                  ((looking-at "Location: *\\(.*\\)")
                   (setq location (string-trim (match-string 1))))
                  ((looking-at "Content-Type: *\\(.*\\)")
                   (setq content-type (downcase (string-trim (match-string 1))))))))
           (unless (and status (<= 200 status) (< status 400))
             (error "Web server returned HTTP %s" (or status "unknown"))))
         (if (<= 300 status 399)
             (progn
               (when (or key payload) (error "Search API redirect rejected"))
               (unless location (error "Web redirect has no Location"))
               (when (>= (or redirects 0) 5) (error "Too many web redirects"))
               (magent-web--http job (url-expand-file-name location url) callback
                                 nil nil (1+ (or redirects 0))))
           (funcall callback body url (or content-type "")))))))))

(defun magent-web--text (node)
  "Return whitespace-normalized text from DOM NODE."
  (if (stringp node) node
    (string-trim
     (replace-regexp-in-string
      "[[:space:]]+" " "
      (mapconcat #'magent-web--text (dom-children node) " ")))))

(defun magent-web--parse-xml (body)
  "Parse bounded XML BODY without external entities."
  (with-temp-buffer
    (insert (decode-coding-string body 'utf-8))
    (when (re-search-backward "<!\\(?:DOCTYPE\\|ENTITY\\)" nil t)
      (error "Search XML contains an unsupported declaration"))
    (car (xml-parse-region (point-min) (point-max)))))

(defun magent-web--bing (query options callback)
  "Search Bing's public RSS for QUERY with OPTIONS and CALLBACK."
  (magent-web--run
   callback
   (lambda (job)
     (magent-web--http
      job (concat "https://www.bing.com/search?format=rss&q=" (url-hexify-string query))
      (lambda (body _url _type)
        (let* ((dom (magent-web--parse-xml body))
               (items (dom-by-tag dom 'item)) results)
          (unless (eq (dom-tag dom) 'rss) (error "Bing did not return a search RSS feed"))
          (dolist (item (seq-take items (plist-get options :max-results)))
            (push (list :title (magent-web--text (car (dom-by-tag item 'title)))
                        :url (magent-web--text (car (dom-by-tag item 'link)))
                        :snippet (magent-web--text (car (dom-by-tag item 'description)))) results))
          ;; RSS pubDate is not reliable evidence of the page publication date.
          (magent-web--finish job (list :results (nreverse results)
                                       :filter-note "Public RSS; no date or domain filter guarantee")))))) t))

(defun magent-web--duckduckgo (query options callback)
  "Search DuckDuckGo HTML for QUERY with OPTIONS and CALLBACK."
  (magent-web--run
   callback
   (lambda (job)
     (magent-web--http
      job (concat "https://html.duckduckgo.com/html/?q=" (url-hexify-string query))
      (lambda (body _url _type)
        (let ((text (decode-coding-string body 'utf-8)) results)
          (when (string-match-p "anomaly-modal\\|challenge-form" text)
            (error "DuckDuckGo requires a CAPTCHA; choose another search provider"))
          (unless (libxml-available-p) (error "HTML search requires Emacs libxml2 support"))
          (with-temp-buffer
            (insert text)
            (let ((dom (libxml-parse-html-region (point-min) (point-max))))
              (dolist (node (dom-by-class dom "\\`result\\'"))
                (when-let* ((anchor (car (dom-by-class node "result__a")))
                            (href (dom-attr anchor 'href)))
                  (when (string-match "[?&]uddg=\\([^&]+\\)" href)
                    (setq href (decode-coding-string
                                (url-unhex-string (match-string 1 href)) 'utf-8)))
                  (push (list :title (magent-web--text anchor) :url href
                              :snippet (magent-web--text
                                        (car (dom-by-class node "result__snippet")))) results)))
              (unless (or results (string-match-p "No results found\\|no-results" text))
                (error "DuckDuckGo response was not a recognized result page"))))
          (magent-web--finish job (list :results (seq-take (nreverse results)
                                                         (plist-get options :max-results)))))))) t))

(defun magent-web--tavily-key ()
  "Resolve a Tavily credential without logging it."
  (let* ((configured magent-web-tavily-api-key)
         (key (cond ((functionp configured) (funcall configured))
                    ((stringp configured) configured)
                    ((getenv "TAVILY_API_KEY"))
                    (t (let ((secret (plist-get
                                      (car (auth-source-search :host "api.tavily.com"
                                                               :user "apikey" :require '(:secret)
                                                               :max 1)) :secret)))
                         (if (functionp secret) (funcall secret) secret))))))
    (unless (and (stringp key) (not (string-blank-p key)))
      (error "Tavily needs a key: set TAVILY_API_KEY, use auth-source, or select bing"))
    key))

(defun magent-web--tavily (query options callback)
  "Search Tavily's official API for QUERY with OPTIONS and CALLBACK."
  (magent-web--run
   callback
   (lambda (job)
     (let ((payload (list :query query :max_results (plist-get options :max-results)
                          :search_depth "basic" :auto_parameters :json-false
                          :include_answer :json-false :include_raw_content :json-false))
           (key (magent-web--tavily-key)))
       (when (plist-get options :domains)
         (setq payload (plist-put payload :include_domains (vconcat (plist-get options :domains)))))
       (when (plist-get options :recency)
         (setq payload (plist-put payload :start_date
                                  (format-time-string "%Y-%m-%d"
                                   (time-subtract (current-time)
                                    (days-to-time (plist-get options :recency))) t))))
       (magent-web--http
        job "https://api.tavily.com/search"
        (lambda (body _url _type)
          (let* ((data (json-parse-string (decode-coding-string body 'utf-8)
                                         :object-type 'plist :array-type 'list
                                         :null-object nil :false-object :json-false))
                 (rows (plist-get data :results)))
            (unless (plist-member data :results) (error "Tavily response has no results field"))
            (magent-web--finish
             job (list :results
                       (mapcar (lambda (row)
                                 (list :title (plist-get row :title) :url (plist-get row :url)
                                       :snippet (plist-get row :content)
                                       :published-date (plist-get row :published_date))) rows)))))
        (magent-json-encode payload) key))) t))

(defun magent-web--store (identity snapshot)
  "Store immutable SNAPSHOT for session IDENTITY and return its reference."
  (let* ((magent-session-directory (or (plist-get identity :directory) magent-session-directory))
         (snapshot (append (list :web-version 1
                                 :fetched-at (format-time-string "%FT%TZ" nil t)) snapshot))
         (stored (magent-tool-output-spill-put
                  (plist-get identity :scope) (plist-get identity :session-id)
                  (magent-json-encode snapshot))))
    (or (plist-get stored :result-id) (error "Web snapshot exceeds session storage quota"))))

(defun magent-web--snapshot (identity reference)
  "Read REFERENCE only from session IDENTITY, failing on expired references."
  (let* ((magent-session-directory (or (plist-get identity :directory) magent-session-directory))
         (file (magent-tool-output-spill-file
                (plist-get identity :scope) (plist-get identity :session-id) reference))
         (data (with-temp-buffer
                 (insert-file-contents file)
                 (json-parse-buffer :object-type 'plist :array-type 'array
                                    :null-object nil :false-object :json-false))))
    (unless (equal (plist-get data :web-version) 1) (error "Reference is not a web snapshot"))
    data))

(defun magent-web--result (data references)
  "Create a successful tool result from DATA and snapshot REFERENCES."
  (magent-tool-result-create :success t :status 'completed
                            :output (magent-json-encode data)
                            :metadata (list :web-references (vconcat references))))

(defun magent-web-search (callback identity query &optional max-results domains recency)
  "Search QUERY for session IDENTITY and deliver a tool result to CALLBACK.
MAX-RESULTS defaults to five; DOMAINS and RECENCY are optional filters.
Return a cancellation function."
  (let ((provider magent-web-search-provider)
        (identity (plist-put (copy-sequence identity) :directory
                             (or (plist-get identity :directory) magent-session-directory))))
    (magent-web--run
     callback
     (lambda (job)
       (unless (and (stringp query) (not (string-blank-p query)) (<= (length query) 2000))
         (error "Search query must contain 1..2000 characters"))
       (setq max-results (or max-results 5) domains (append domains nil))
       (unless (and (integerp max-results) (<= 1 max-results 20)
                    (or (null recency) (and (integerp recency) (<= 1 recency 3650)))
                    (<= (length domains) 20)
                    (seq-every-p (lambda (domain)
                                   (and (stringp domain)
                                        (string-match-p "\\`[A-Za-z0-9-]+\\(?:\\.[A-Za-z0-9-]+\\)+\\'" domain))) domains))
         (error "Invalid search count, domains or recency"))
       (let* ((spec (alist-get provider magent-web--providers))
              (function (plist-get spec :function)))
         (unless function (error "Unknown search provider: %s" provider))
         (dolist (filter '(domains recency))
           (when (and (if (eq filter 'domains) domains recency)
                      (not (memq filter (plist-get spec :filters))))
             (error "Search provider %s does not support %s; omit it or select another provider" provider filter)))
         (let ((cancel
                (funcall
                 function query (list :max-results max-results :domains domains :recency recency)
                 (lambda (response)
                   (magent-web--guard
                    job
                    (lambda ()
                      (when (plist-get response :error) (error "%s" (plist-get response :error)))
                      (unless (and (plist-member response :results)
                                   (listp (plist-get response :results)))
                        (error "Invalid search provider response"))
                      (let (results references)
                        (dolist (entry (seq-take (plist-get response :results) max-results))
                          (unless (and (listp entry)
                                       (seq-every-p (lambda (key) (stringp (plist-get entry key)))
                                                    '(:title :url :snippet))
                                       (or (null (plist-get entry :published-date))
                                           (stringp (plist-get entry :published-date))))
                            (error "Invalid search result fields"))
                          (magent-web--url (plist-get entry :url))
                          (let* ((entry (list :title (truncate-string-to-width (plist-get entry :title) 500)
                                              :url (plist-get entry :url)
                                              :snippet (truncate-string-to-width (plist-get entry :snippet) 2000)
                                              :published-date (plist-get entry :published-date)))
                                 (ref (magent-web--store identity (append (list :kind "search-result") entry))))
                            (push ref references)
                            (push (append (list :reference ref) entry) results)))
                        (magent-web--finish
                         job (magent-web--result
                              (list :provider (symbol-name provider) :query query
                                    :kind "search-results" :results (vconcat (nreverse results))
                                    :filter-note (plist-get response :filter-note)) references)))))))))
           (unless (functionp cancel) (error "Search adapter did not return a cancellation function"))
           (if (magent-web--job-done job) (funcall cancel)
             (push cancel (magent-web--job-cleanups job)))))))))

(magent-web-register-search-provider 'bing #'magent-web--bing)
(magent-web-register-search-provider 'duckduckgo #'magent-web--duckduckgo)
(magent-web-register-search-provider 'tavily #'magent-web--tavily '(domains recency))

(provide 'magent-web)
;;; magent-web.el ends here
