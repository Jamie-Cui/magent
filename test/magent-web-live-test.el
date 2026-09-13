;;; magent-web-live-test.el --- Web integration live probes -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fixed probes for the Web tools.  Inventory never reads API keys or prints
;; backend objects.  Provider probes are explicit and use only public queries.

;;; Code:

(require 'cl-lib)
(require 'gptel)

(defun magent-web-live-inventory ()
  "Return safe configured route metadata, without inspecting credentials."
  (list
   :emacs-version emacs-version
   :gptel-file (locate-library "gptel")
   :default-model (and (boundp 'gptel-model) (default-value 'gptel-model))
   :routes
   (cl-loop for (name . backend) in gptel--known-backends
            when (gptel-backend-p backend)
            collect (list :name name :type (type-of backend)
                          :official-openai
                          (equal (gptel-backend-host backend) "api.openai.com")
                          :models (mapcar (lambda (model) (format "%s" model))
                                          (gptel-backend-models backend))))))

(require 'magent-web-page)

(defvar magent-web-live-status nil
  "Bounded result of the opt-in, keyless live web probe.")
(defvar magent-web-live--cancel nil
  "Cancellation function for the current live web probe.")

(defun magent-web-live-start ()
  "Run default keyless search, open its first source, and find Emacs.
Use private temporary storage.  Do not access provider credentials or change
any gptel backend/model.  Return immediately; inspect `magent-web-live-status'."
  (when (functionp magent-web-live--cancel) (funcall magent-web-live--cancel))
  (setq magent-web-live-status '(:status running))
  (let* ((directory (make-temp-file "magent-web-live-" t))
         (identity '(:scope global :session-id "web-live"))
         (magent-session-directory directory)
         (magent-web-search-provider 'bing)
         (finish (lambda (value)
                   (setq magent-web-live-status value)
                   (delete-directory directory t))))
    (setq magent-web-live--cancel
          (magent-web-search
           (lambda (result)
             (let ((magent-session-directory directory))
               (if (not (magent-tool-result-success result))
                   (funcall finish (list :status 'failed :stage 'search
                                         :error (magent-tool-result-error result)))
                 (let* ((data (json-parse-string (magent-tool-result-output result)
                                                :object-type 'plist :array-type 'list :null-object nil))
                        (entry (car (plist-get data :results))))
                   (if (null entry)
                       (funcall finish '(:status failed :stage search :error "No results"))
                     (setq magent-web-live--cancel
                           (magent-web-open
                            (lambda (opened)
                              (let ((magent-session-directory directory))
                                (if (not (magent-tool-result-success opened))
                                    (funcall finish (list :status 'failed :stage 'open
                                                          :error (magent-tool-result-error opened)))
                                  (let* ((page (json-parse-string (magent-tool-result-output opened)
                                                                 :object-type 'plist :null-object nil))
                                         (found (magent-web-find identity (plist-get page :reference) "Emacs"))
                                         (matches (json-parse-string (magent-tool-result-output found)
                                                                    :object-type 'plist :array-type 'list :null-object nil)))
                                    (funcall finish
                                             (list :status (if (plist-get matches :matches) 'passed 'failed)
                                                   :provider (plist-get data :provider)
                                                   :result-count (length (plist-get data :results))
                                                   :snippet-characters (length (plist-get entry :snippet))
                                                   :url (plist-get page :url)
                                                   :total-lines (plist-get page :total-lines)
                                                   :match-count (length (plist-get matches :matches))))))))
                            identity (plist-get entry :reference))))))))
           identity "Emacs Lisp manual" 3)))
  magent-web-live-status)

(defconst magent-web-live--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Checkout containing the fixed live web probes.")

(defun magent-web-live-reload-web ()
  "Reload the changed web libraries and catalog in an idle Emacs runtime.
Preserve configured model, search source and active conversation contents.
Return only safe route and tool metadata."
  (when (or (and (fboundp 'magent-runtime-queue-processing-p)
                  (magent-runtime-queue-processing-p))
             (and (fboundp 'magent-runtime-queue-execution-active-p)
                  (magent-runtime-queue-execution-active-p)))
    (user-error "Magent is busy; reload web tools after the active turn finishes"))
  (dolist (file '("magent-config.el" "magent-ledger.el" "magent-web.el" "magent-web-page.el"))
    (load (expand-file-name (concat "lisp/" file) magent-web-live--root) nil t t))
  ;; These defvars are package-owned tool schema objects, not user settings.
  (dolist (variable '(magent-tools--web-search-tool magent-tools--web-open-tool
                      magent-tools--web-find-tool))
    (makunbound variable))
  (load (expand-file-name "lisp/magent-tools.el" magent-web-live--root) nil t t)
  (list :model (default-value 'gptel-model)
        :search-provider magent-web-search-provider
        :web-tools (mapcar (lambda (entry) (plist-get entry :name))
                           (seq-filter (lambda (entry)
                                         (eq (plist-get entry :permission) 'web_search))
                                       magent-tools-catalog))))

(provide 'magent-web-live-test)
;;; magent-web-live-test.el ends here
