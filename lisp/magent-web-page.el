;;; magent-web-page.el --- Read and find in web snapshots -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools

;;; Commentary:

;; Extract HTML, text and text PDFs into immutable, session-local snapshots.
;; Line numbers identify extracted text, while PDF page numbers identify real
;; form-feed-delimited pages.  No JavaScript, browser login, images or OCR.

;;; Code:

(require 'magent-web)

(defun magent-web-page--lines (text)
  "Split TEXT into stable lines no longer than 400 characters."
  (let (lines)
    (dolist (line (split-string (replace-regexp-in-string "\r" "" text) "\n"))
      (while (> (length line) 400)
        (push (substring line 0 400) lines)
        (setq line (substring line 400)))
      (push line lines))
    (vconcat (nreverse lines))))

(defun magent-web-page--html (body url)
  "Extract text and numbered links from HTML BODY at URL."
  (unless (libxml-available-p) (error "HTML extraction requires Emacs libxml2 support"))
  (with-temp-buffer
    (insert (decode-coding-string body 'utf-8))
    (let* ((dom (libxml-parse-html-region (point-min) (point-max) url))
           (title (magent-web--text (car (dom-by-tag dom 'title))))
           (root (or (car (dom-by-tag dom 'main))
                     (car (dom-by-tag dom 'article))
                     (car (dom-by-tag dom 'body)) dom))
           (index 0) links)
      (dolist (tag '(script style noscript template nav footer form iframe svg))
        (dolist (node (dom-by-tag root tag)) (dom-remove-node root node)))
      (dolist (anchor (dom-by-tag root 'a))
        (when-let* ((href (dom-attr anchor 'href))
                    (absolute (url-expand-file-name href url)))
          (when (and (< index 500) (string-match-p "\\`https?://" absolute))
            (condition-case nil
                (progn
                  (magent-web--url absolute)
                  (push (list :id (cl-incf index) :url absolute
                              :title (truncate-string-to-width (magent-web--text anchor) 200)) links)
                  (nconc anchor (list (format " [%d]" index))))
              (magent-web-invalid-url nil)))))
      (erase-buffer)
      (let ((shr-inhibit-images t) (shr-use-fonts nil) (shr-width 100)
            (shr-external-rendering-functions '((img . ignore))))
        (shr-insert-document root))
      (list :title title :text (string-trim (buffer-substring-no-properties (point-min) (point-max)))
            :links (vconcat (nreverse links))))))

(defun magent-web-page--snapshot (url title text links &optional pdf)
  "Build an extracted snapshot of URL from TITLE, TEXT and LINKS.
PDF means TEXT retains form feeds separating real pages."
  (unless (string-match-p "[^ \t\r\n\f]" text)
    (error "No readable text; scanned PDFs need OCR, which is unsupported"))
  (let ((chunks (if pdf (split-string text "\f") (list text)))
        (line 1) (page 0) lines pages)
    (when (and pdf (> (length chunks) 1) (string-empty-p (car (last chunks))))
      (setq chunks (butlast chunks)))
    (dolist (chunk chunks)
      (let* ((part (magent-web-page--lines chunk)) (end (+ line (length part) -1)))
        (push (list :page (cl-incf page) :start-line line :end-line end) pages)
        (push part lines)
        (setq line (1+ end))))
    (list :kind (if pdf "pdf" "page") :url url :title title
          :sha256 (secure-hash 'sha256 text)
          :lines (apply #'vconcat (nreverse lines)) :links links
          :pages (if pdf (vconcat (nreverse pages)) [])
          :extraction "Extracted text; line numbers refer to this immutable snapshot")))

(defun magent-web-page--extract (job body url content-type callback)
  "Extract BODY at URL of CONTENT-TYPE for JOB and call CALLBACK."
  (cond
   ((or (string-prefix-p "%PDF-" body) (string-match-p "application/pdf" content-type))
    (let* ((program (or (executable-find "pdftotext")
                        (error "PDF reading requires pdftotext (Poppler)")))
           (file (expand-file-name "document.pdf" (magent-web--directory job)))
           (coding-system-for-write 'no-conversion))
      (write-region body nil file nil 'silent)
      (set-file-modes file #o600)
      (magent-web--process
       job (list program "-layout" "-enc" "UTF-8" file "-")
       (lambda (text)
         (funcall callback (magent-web-page--snapshot
                            url url (decode-coding-string text 'utf-8) [] t))))))
   ((or (string-match-p "html" content-type)
        (and (string-empty-p content-type)
             (string-match-p "\\`[[:space:]]*<" body)))
    (let ((data (magent-web-page--html body url)))
      (funcall callback (magent-web-page--snapshot
                         url (plist-get data :title) (plist-get data :text)
                         (plist-get data :links)))))
   ((or (string-prefix-p "text/" content-type)
        (string-match-p "application/\\(?:json\\|xml\\)" content-type))
    (funcall callback (magent-web-page--snapshot url url (decode-coding-string body 'utf-8) [])))
   (t (error "Unsupported web content type: %s" content-type))))

(defun magent-web-page--window (snapshot reference start count &optional page)
  "Read a bounded SNAPSHOT window from START with COUNT lines.
REFERENCE identifies this snapshot; PAGE optionally selects a real PDF page."
  (let* ((lines (plist-get snapshot :lines))
         (pages (plist-get snapshot :pages))
         (page-data (and page (seq-find (lambda (p) (equal (plist-get p :page) page)) pages)))
         (start (or start (and page-data (plist-get page-data :start-line)) 1))
         (count (or count 80))
         (end (min (length lines) (+ start count -1)
                   (if page-data (plist-get page-data :end-line) (length lines))))
         (budget 8000) output)
    (unless (and (vectorp lines) (integerp start) (<= 1 start (length lines))
                 (integerp count) (<= 1 count 200)
                 (or (null page) (and (integerp page) page-data
                                     (<= (plist-get page-data :start-line) start
                                         (plist-get page-data :end-line)))))
      (error "Invalid line range or PDF page"))
    (cl-loop for number from start to end
             for text = (aref lines (1- number))
             while (>= budget (length text))
             do (push (list :line number :text text) output)
             (setq budget (- budget (length text))))
    (setq output (nreverse output) end (+ start (length output) -1))
    (list :reference reference :url (plist-get snapshot :url) :title (plist-get snapshot :title)
          :kind (plist-get snapshot :kind) :fetched-at (plist-get snapshot :fetched-at)
          :sha256 (plist-get snapshot :sha256) :total-lines (length lines)
          :total-pages (length pages) :start-line start :end-line end
          :next-start-line (and (< end (length lines)) (1+ end))
          :lines (vconcat output)
          :links (vconcat
                  (seq-filter
                   (lambda (link)
                     (seq-some (lambda (line)
                                 (string-match-p (regexp-quote (format "[%d]" (plist-get link :id)))
                                                 (plist-get line :text))) output))
                   (plist-get snapshot :links)))
          :pages (vconcat (seq-filter (lambda (p)
                                       (and (<= (plist-get p :start-line) end)
                                            (>= (plist-get p :end-line) start))) pages)))))

(defun magent-web-open (callback identity target &optional start count link-id refresh page)
  "Open URL or reference TARGET for IDENTITY and deliver result to CALLBACK.
START and COUNT select extracted lines; PAGE selects a real PDF page.
LINK-ID follows a stored link.  REFRESH equal to t creates a new snapshot.
Return a cancellation function."
  (magent-web--run
   callback
   (lambda (job)
     (unless (stringp target) (error "Web target must be a URL or reference"))
     (let* ((identity (plist-put (copy-sequence identity) :directory
                                 (or (plist-get identity :directory) magent-session-directory)))
            (reference (and (string-prefix-p "result-" target) target))
            (snapshot (and reference (magent-web--snapshot identity reference)))
            (url (if snapshot (plist-get snapshot :url) target)))
       (when link-id
         (unless (and snapshot (integerp link-id) (> link-id 0))
           (error "Link id requires a page snapshot reference"))
         (let ((link (seq-find (lambda (l) (equal (plist-get l :id) link-id))
                               (plist-get snapshot :links))))
           (unless link (error "Unknown link id in this snapshot"))
           (setq url (plist-get link :url) snapshot nil)))
       (if (and snapshot (not (eq refresh t))
                (member (plist-get snapshot :kind) '("page" "pdf")))
           (magent-web--finish job (magent-web--result
                                   (magent-web-page--window snapshot reference start count page)
                                   (list reference)))
         (magent-web--http
          job url
          (lambda (body final-url type)
            (magent-web-page--extract
             job body final-url type
             (lambda (data)
               (let* ((ref (magent-web--store identity data))
                      (stored (magent-web--snapshot identity ref)))
                 (magent-web--finish job (magent-web--result
                                         (magent-web-page--window stored ref start count page)
                                         (delq nil (list reference ref))))))))))))))

(defun magent-web-find (identity reference pattern &optional start max-matches)
  "Find literal PATTERN in IDENTITY's REFERENCE snapshot, case-insensitively.
START defaults to line one; MAX-MATCHES defaults to 10, at most 20.
Return a bounded tool result with continuation and full snapshot line count."
  (let* ((snapshot (magent-web--snapshot identity reference))
         (lines (plist-get snapshot :lines))
         (start (or start 1)) (max-matches (or max-matches 10))
         (case-fold-search t) matches (next start))
    (unless (and (vectorp lines) (stringp pattern) (<= 1 (length pattern) 500)
                 (integerp start) (<= 1 start (length lines))
                 (integerp max-matches) (<= 1 max-matches 20))
      (error "Find requires a page snapshot, nonempty pattern and valid limits"))
    (cl-loop for number from start to (length lines)
             while (< (length matches) max-matches)
             do (setq next (1+ number))
             when (string-match-p (regexp-quote pattern) (aref lines (1- number)))
             do (push (list :line number :text (aref lines (1- number))
                            :page (plist-get (seq-find
                                              (lambda (p) (<= (plist-get p :start-line) number
                                                              (plist-get p :end-line)))
                                              (plist-get snapshot :pages)) :page)) matches))
    (magent-web--result
     (list :reference reference :url (plist-get snapshot :url) :pattern pattern
           :matches (vconcat (nreverse matches)) :total-lines (length lines)
           :searched-through-line (1- next)
           :next-start-line (and (<= next (length lines)) next)) (list reference))))

(provide 'magent-web-page)
;;; magent-web-page.el ends here
