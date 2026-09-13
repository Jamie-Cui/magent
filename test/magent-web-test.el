;;; magent-web-test.el --- Web evidence regression tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Deterministic adapters and HTTP fixtures.  Real networking is opt-in through
;; magent-web-live-test.el; these tests neither resolve credentials nor pay APIs.
;;; Code:
(require 'ert)
(require 'magent-web-page)

(defmacro magent-web-test--session (&rest body)
  "Run BODY with private storage and a fixed session identity."
  (declare (indent 0) (debug t))
  `(let* ((magent-session-directory (make-temp-file "magent-web-test-" t))
          (identity '(:scope global :session-id "web-test")))
     (unwind-protect (progn ,@body) (delete-directory magent-session-directory t))))

(defun magent-web-test--decode (result)
  "Decode a successful tool RESULT."
  (should (magent-tool-result-success result))
  (json-parse-string (magent-tool-result-output result) :object-type 'plist
                     :array-type 'list :null-object nil))

(defconst magent-web-test--entry
  '(:title "Example" :url "https://example.org/paper" :snippet "Research snippet"))

(ert-deftest magent-web-test-default-without-key ()
  (magent-web-test--session
    (let ((magent-web-search-provider (eval (car (get 'magent-web-search-provider 'standard-value)) t))
          (magent-web-tavily-api-key nil) result requested)
      (cl-letf (((symbol-function 'magent-web--tavily-key)
                 (lambda () (ert-fail "Default source must not request a key")))
                ((symbol-function 'magent-web--http)
                 (lambda (job url callback &rest _)
                   (setq requested url)
                   (funcall callback "<rss><channel><item><title>Example</title><link>https://example.org</link><description>Snippet</description><pubDate>Yesterday</pubDate></item></channel></rss>" url "text/xml")
                   (should (magent-web--job-done job)))))
        (magent-web-search (lambda (r) (setq result r)) identity "Emacs"))
      (let* ((data (magent-web-test--decode result)) (entry (car (plist-get data :results))))
        (should (string-prefix-p "https://www.bing.com/" requested))
        (should (equal (plist-get data :provider) "bing"))
        (should (equal (plist-get entry :snippet) "Snippet"))
        (should-not (plist-get entry :published-date))
        (should (equal (plist-get (magent-web--snapshot identity (plist-get entry :reference)) :kind)
                       "search-result"))))))

(ert-deftest magent-web-test-provider-extension-and-frozen-selection ()
  (magent-web-test--session
    (let ((magent-web--providers (copy-tree magent-web--providers))
          (magent-web-search-provider 'fixture) callback result options)
      (magent-web-register-search-provider
       'fixture (lambda (_query opts cb) (setq options opts callback cb) #'ignore) '(domains))
      (magent-web-search (lambda (r) (setq result r)) identity "query" 2 ["example.org"])
      (setq magent-web-search-provider 'tavily)
      (funcall callback (list :results (list magent-web-test--entry)))
      (should (equal options '(:max-results 2 :domains ("example.org") :recency nil)))
      (should (equal (plist-get (magent-web-test--decode result) :provider) "fixture")))))

(ert-deftest magent-web-test-invalid-options-fail-before-dispatch ()
  (magent-web-test--session
    (let ((magent-web-search-provider 'bing) result)
      (cl-letf (((symbol-function 'magent-web--http) (lambda (&rest _) (ert-fail "Unexpected HTTP"))))
        (dolist (args '(("x" 0) ("x" 21) ("x" 5 ["https://example.org"]) ("x" 5 nil 1)))
          (apply #'magent-web-search (lambda (r) (setq result r)) identity args)
          (should-not (magent-tool-result-success result)))))))

(ert-deftest magent-web-test-unknown-and-malformed-provider ()
  (magent-web-test--session
    (let ((magent-web-search-provider 'missing) result)
      (magent-web-search (lambda (r) (setq result r)) identity "q")
      (should (string-match-p "Unknown search provider" (magent-tool-result-error result))))
    (let ((magent-web--providers nil) (magent-web-search-provider 'bad) result)
      (magent-web-register-search-provider 'bad (lambda (_q _o cb) (funcall cb '(:results ((:title 2)))) #'ignore))
      (magent-web-search (lambda (r) (setq result r)) identity "q")
      (should-not (magent-tool-result-success result)))))

(ert-deftest magent-web-test-completion-and-cancellation-latch ()
  (magent-web-test--session
    (let ((magent-web--providers nil) (magent-web-search-provider 'fixture)
          callback (calls 0) (cleanups 0))
      (magent-web-register-search-provider
       'fixture (lambda (_q _o cb) (setq callback cb) (lambda () (cl-incf cleanups))))
      (let ((cancel (magent-web-search (lambda (_r) (cl-incf calls)) identity "q")))
        (funcall callback '(:results nil))
        (funcall callback '(:error "late"))
        (funcall cancel)
        (should (= calls 1)) (should (= cleanups 1)))
      (let ((cancel (magent-web-search (lambda (_r) (cl-incf calls)) identity "q")))
        (funcall cancel) (funcall cancel)
        (funcall callback (list :results (list magent-web-test--entry)))
        (should (= calls 1)) (should (= cleanups 2))))))

(ert-deftest magent-web-test-timeout-and-cleanup-failure-visible ()
  (let (timeout result logs)
    (cl-letf (((symbol-function 'run-at-time) (lambda (_secs _repeat fn) (setq timeout fn) nil))
              ((symbol-function 'magent-log) (lambda (fmt &rest args) (push (apply #'format fmt args) logs))))
      (magent-web--run (lambda (r) (setq result r))
                       (lambda (job) (push (lambda () (error "Cleanup fixture"))
                                            (magent-web--job-cleanups job))))
      (funcall timeout)
      (should (string-match-p "timed out" (magent-tool-result-error result)))
      (should (string-match-p "Cleanup fixture" (car logs))))))

(ert-deftest magent-web-test-tavily-contract ()
  (let ((magent-web-tavily-api-key "tvly-test-only") payload result credential)
    (cl-letf (((symbol-function 'magent-web--http)
               (lambda (_job url callback data key &rest _)
                 (should (equal url "https://api.tavily.com/search"))
                 (setq payload (json-parse-string data :object-type 'plist :array-type 'list)
                       credential key)
                 (funcall callback "{\"results\":[{\"title\":\"A\",\"url\":\"https://example.org\",\"content\":\"B\",\"published_date\":\"2026-09-01\"}]}" url "application/json"))))
      (magent-web--tavily "query" '(:max-results 2 :domains ("example.org") :recency 7)
                          (lambda (r) (setq result r))))
    (should (equal credential "tvly-test-only"))
    (should (eq (plist-get payload :auto_parameters) :false))
    (should (eq (plist-get payload :include_answer) :false))
    (should (equal (plist-get payload :include_domains) '("example.org")))
    (should (stringp (plist-get payload :start_date)))
    (should (equal (plist-get (car (plist-get result :results)) :snippet) "B"))))

(ert-deftest magent-web-test-tavily-missing-key ()
  (let ((magent-web-tavily-api-key nil) result)
    (cl-letf (((symbol-function 'getenv) (lambda (&rest _) nil))
              ((symbol-function 'auth-source-search) (lambda (&rest _) nil))
              ((symbol-function 'magent-web--http) (lambda (&rest _) (ert-fail "Unexpected HTTP"))))
      (magent-web--tavily "q" '(:max-results 1) (lambda (r) (setq result r))))
    (should (string-match-p "Tavily needs a key" (plist-get result :error)))))

(ert-deftest magent-web-test-ddg-snippets-and-blocked-pages ()
  (let (result (body "<html><body><div class='result'><a class='result__a' href='//duckduckgo.com/l/?uddg=https%3A%2F%2Fexample.org'>An <b>example</b></a><a class='result__snippet'>Search <b>snippet</b></a></div></body></html>"))
    (cl-letf (((symbol-function 'magent-web--http)
               (lambda (_job url cb &rest _) (funcall cb body url "text/html"))))
      (magent-web--duckduckgo "q" '(:max-results 2) (lambda (r) (setq result r)))
      (should (equal (plist-get (car (plist-get result :results)) :snippet) "Search snippet"))
      (should (equal (plist-get (car (plist-get result :results)) :url) "https://example.org"))
      (dolist (blocked '("<form id='challenge-form'></form>" "<html>Unexpected login page</html>"))
        (setq body blocked)
        (magent-web--duckduckgo "q" '(:max-results 2) (lambda (r) (setq result r)))
        (should (plist-get result :error))))))

(ert-deftest magent-web-test-http-status-redirect-and-credential-isolation ()
  (let ((status "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n")
        (body "{}") command directory result)
    (cl-letf (((symbol-function 'magent-web--process)
               (lambda (job argv callback)
                 (setq command argv directory (magent-web--job-directory job))
                 (magent-web--write job "response-headers" status)
                 (funcall callback body))))
      (magent-web--run
       (lambda (r) (setq result r))
       (lambda (job)
         (magent-web--http job "https://api.tavily.com/search"
                           (lambda (text _url _type) (magent-web--finish job text)) "{}" "tvly-secret")))
      (should (equal result "{}"))
      (should-not (string-match-p "tvly-secret" (prin1-to-string command)))
      (should-not (file-exists-p directory))
      (dolist (response '("HTTP/1.1 429 Too Many Requests\r\n\r\n"
                          "HTTP/1.1 302 Found\r\nLocation: http://127.0.0.1/private\r\n\r\n"))
        (setq status response)
        (magent-web--run
         (lambda (r) (setq result r))
         (lambda (job) (magent-web--http job "https://example.org" #'ignore)))
        (should-not (magent-tool-result-success result))))))

(ert-deftest magent-web-test-url-and-xml-input-boundaries ()
  (dolist (url '("file:///etc/passwd" "http://localhost/a" "http://127.0.0.1" "https://user:password@example.org"
                 "http://192.168.2.1" "http://[::1]" "http://10.0.0.1" "https://example.org/\n"))
    (should-error (magent-web--url url) :type 'magent-web-invalid-url))
  (should-error (magent-web--parse-xml "<!DOCTYPE rss [<!ENTITY a SYSTEM 'file:///etc/passwd'>]><rss>&a;</rss>")))

(ert-deftest magent-web-test-html-extraction-and-links ()
  (let* ((data (magent-web-page--html "<html><title>Research</title><body><nav>MENU</nav><main><h1>Paper</h1><script>evil()</script><p>Evidence <a href='/proof'>Proof</a></p><a href='file:///etc/passwd'>Local</a><img src='/tracking'></main></body></html>" "https://example.org/a"))
         (text (plist-get data :text)))
    (should (equal (plist-get data :title) "Research"))
    (should (string-match-p "Paper" text))
    (should (string-match-p "\\[1\\]" text))
    (should-not (string-match-p "evil\\|MENU\\|tracking" text))
    (should (equal (plist-get (aref (plist-get data :links) 0) :url) "https://example.org/proof"))))

(ert-deftest magent-web-test-snapshots-open-find-refresh-and-isolation ()
  (magent-web-test--session
    (let ((body "First line\nneedle here\nThird line\nNEEDLE again") result ref (requests 0))
      (cl-letf (((symbol-function 'magent-web--http)
                 (lambda (_job url cb &rest _)
                   (cl-incf requests) (funcall cb body url "text/plain"))))
        (magent-web-open (lambda (r) (setq result r)) identity "https://example.org" 1 2)
        (setq ref (plist-get (magent-web-test--decode result) :reference))
        (should (= (plist-get (magent-web-test--decode result) :next-start-line) 3))
        (magent-web-open (lambda (r) (setq result r)) identity ref 3 2 nil :json-false)
        (should (= requests 1))
        (should (= (length (plist-get (magent-web-test--decode result) :lines)) 2))
        (let ((found (magent-web-test--decode (magent-web-find identity ref "needle" 1 1))))
          (should (= (plist-get (car (plist-get found :matches)) :line) 2))
          (should (= (plist-get found :next-start-line) 3)))
        (setq body "Changed")
        (magent-web-open (lambda (r) (setq result r)) identity ref nil nil nil t)
        (should (= requests 2))
        (should-not (equal ref (plist-get (magent-web-test--decode result) :reference)))
        (should (equal (aref (plist-get (magent-web--snapshot identity ref) :lines) 0) "First line")))
      (should-error (magent-web--snapshot '(:scope global :session-id "other-session") ref))
      (magent-tool-output-spill-fork-session 'global "web-test" "web-fork" (list ref))
      (should (equal (plist-get (magent-web--snapshot '(:scope global :session-id "web-fork") ref) :kind) "page"))
      (magent-tool-output-spill-delete-session 'global "web-test")
      (should-error (magent-web--snapshot identity ref)))))

(ert-deftest magent-web-test-search-reference-opens-and-link-follows ()
  (magent-web-test--session
    (let* ((ref (magent-web--store identity (append '(:kind "search-result") magent-web-test--entry)))
           result urls)
      (cl-letf (((symbol-function 'magent-web--http)
                 (lambda (_job url cb &rest _) (push url urls)
                   (funcall cb "<html><body><p>Proof <a href='/second'>next</a></p></body></html>" url "text/html"))))
        (magent-web-open (lambda (r) (setq result r)) identity ref)
        (magent-web-open (lambda (r) (setq result r)) identity
                         (plist-get (magent-web-test--decode result) :reference) nil nil 1)
        (should (equal urls '("https://example.org/second" "https://example.org/paper")))))))

(ert-deftest magent-web-test-pdf-pages-and-output-budget ()
  (let* ((snapshot (magent-web-page--snapshot "https://example.org/a.pdf" "PDF"
                                             "page one\nproof\fpage two\nproof\f" [] t))
         (window (magent-web-page--window snapshot "result-test" nil nil 2)))
    (should (= (plist-get window :total-pages) 2))
    (should (= (plist-get window :start-line) 3))
    (should (equal (plist-get (aref (plist-get window :lines) 0) :text) "page two"))
    (should-error (magent-web-page--window snapshot "result-test" nil nil 3)))
  (let* ((snapshot (magent-web-page--snapshot "https://example.org" "Long" (make-string 20000 ?x) []))
         (window (magent-web-page--window snapshot "result-test" 1 200)))
    (should (= (plist-get window :end-line) 20))
    (should (= (plist-get window :next-start-line) 21)))
  (should-error (magent-web-page--snapshot "https://example.org/a.pdf" "Scan" " \n\f\n " [] t)))

(ert-deftest magent-web-test-ledger-retains-web-references ()
  (cl-letf (((symbol-function 'magent-thread-all-items)
             (lambda (_) (list (magent-thread-item-create
                                :metadata '(("web-references" . ["result-web-one" "result-web-two"])))))))
    (should (equal (magent-thread-spill-result-ids 'fixture) '("result-web-one" "result-web-two")))))

(ert-deftest magent-web-test-tools-share-permission ()
  (require 'magent-tools)
  (dolist (name '("web_search" "web_open" "web_find"))
    (should (eq (magent-tools-permission-key name) 'web_search))
    (should (eq (magent-tools-locality name) 'local)))
  (let ((tools (magent-tools-get-gptel-tools-for-permission '((web_search . deny)) :all)))
    (should-not (seq-some (lambda (tool) (string-prefix-p "web_" (gptel-tool-name tool))) tools))))

(ert-deftest magent-web-test-async-storage-root-is-frozen ()
  (magent-web-test--session
    (let ((magent-web--providers nil) (magent-web-search-provider 'fixture)
          callback result)
      (magent-web-register-search-provider 'fixture
       (lambda (_query _options cb) (setq callback cb) #'ignore))
      (magent-web-search (lambda (r) (setq result r)) identity "q")
      (let ((magent-session-directory "/missing-web-test-directory/"))
        (funcall callback (list :results (list magent-web-test--entry))))
      (let ((ref (plist-get (car (plist-get (magent-web-test--decode result) :results)) :reference)))
        (should (equal (plist-get (magent-web--snapshot identity ref) :kind) "search-result"))))))

(ert-deftest magent-web-test-process-size-limit-and-cancel ()
  (let ((program (expand-file-name invocation-name invocation-directory))
        (magent-web-max-response-bytes 4) result job)
    (magent-web--run
     (lambda (r) (setq result r))
     (lambda (j)
       (setq job j)
       (magent-web--process j (list program "-Q" "--batch" "--eval" "(princ \"123456\")")
                            (lambda (_) (ert-fail "Oversized body reached parser")))))
    (let ((deadline (+ (float-time) 5)))
      (while (and (not result) (< (float-time) deadline)) (accept-process-output nil 0.01)))
    (should (magent-tool-result-p result))
    (should (string-match-p "size limit" (magent-tool-result-error result)))
    (should-not (process-live-p (magent-web--job-process job)))
    (should-not (buffer-live-p (magent-web--job-buffer job)))
    (setq result nil)
    (let ((cancel
           (magent-web--run
            (lambda (r) (setq result r))
            (lambda (j)
              (setq job j)
              (magent-web--process j (list program "-Q" "--batch" "--eval" "(sleep-for 5)") #'ignore)))))
      (funcall cancel)
      (accept-process-output nil 0.01)
      (should-not result)
      (should-not (process-live-p (magent-web--job-process job))))))

(defconst magent-web-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(ert-deftest magent-web-test-real-pdf-converter ()
  (skip-unless (executable-find "pdftotext"))
  (let ((body (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally
                 (expand-file-name "fixtures/web/two-pages.pdf" magent-web-test--directory))
                (buffer-string)))
        result)
    (magent-web--run
     (lambda (r) (setq result r))
     (lambda (job)
       (magent-web-page--extract
        job body "https://example.org/two-pages.pdf" "application/pdf"
        (lambda (snapshot) (magent-web--finish job snapshot)))))
    (let ((deadline (+ (float-time) 5)))
      (while (and (not result) (< (float-time) deadline)) (accept-process-output nil 0.01)))
    (should (equal (plist-get result :kind) "pdf"))
    (should (= (length (plist-get result :pages)) 2))
    (let* ((window (magent-web-page--window result "result-test" nil nil 2))
           (lines (plist-get window :lines)))
      (should (string-match-p "Second page proof" (plist-get (aref lines 0) :text))))))

(provide 'magent-web-test)
;;; magent-web-test.el ends here
