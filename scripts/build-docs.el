;;; build-docs.el --- Build Magent Org docs for GitHub Pages -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch builder for docs/*.org.  It executes PlantUML blocks only from
;; docs/_diagrams/*.org, exports page bodies with Babel disabled, wraps them in
;; docs/_templates/default.html, and writes a static site to _site/.

;;; Code:

(require 'cl-lib)
(require 'ob)
(require 'org)
(require 'ox-html)
(require 'subr-x)

(defconst magent-docs--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defconst magent-docs--docs-root
  (expand-file-name "docs/" magent-docs--repo-root))

(defconst magent-docs--site-root
  (expand-file-name "_site/" magent-docs--repo-root))

(defconst magent-docs--template-file
  (expand-file-name "_templates/default.html" magent-docs--docs-root))

(defconst magent-docs--site-title
  "Magent Documentation")

(defconst magent-docs--site-description
  "Emacs-native AI coding agent documentation")

(defconst magent-docs--navigation
  '((en . (("Overview" . "/")
           ("Commands" . "/COMMANDS.html")
           ("Architecture" . "/ARCHITECTURE.html")
           ("Onboarding" . "/ONBOARDING.html")
           ("Agent Workflow" . "/AGENT_WORKFLOW.html")
           ("Child Agents" . "/AGENT_JOBS.html")
           ("UI Backends" . "/UI_BACKENDS.html")
           ("Troubleshooting" . "/TROUBLESHOOTING.html")
           ("Contributing" . "/CONTRIBUTING.html")))
    (zh . (("总览" . "/zh/")
           ("命令" . "/COMMANDS.zh.html")
           ("架构" . "/ARCHITECTURE.zh.html")
           ("入门导览" . "/ONBOARDING.zh.html")
           ("Agent 工作流" . "/AGENT_WORKFLOW.zh.html")
           ("子 Agent" . "/AGENT_JOBS.zh.html")
           ("UI 后端" . "/UI_BACKENDS.zh.html")
           ("故障排查" . "/TROUBLESHOOTING.zh.html")
           ("贡献" . "/CONTRIBUTING.zh.html")))))

(defun magent-docs--external-url-p (url)
  "Return non-nil if URL should not be made site-relative."
  (or (string-prefix-p "#" url)
      (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" url)))

(defun magent-docs--url-file (url)
  "Return the site output file for public URL without its fragment."
  (let ((path (car (split-string url "#" t))))
    (cond
     ((or (null path) (string-empty-p path) (string= path "/"))
      "index.html")
     ((string-suffix-p "/" path)
      (concat (string-remove-prefix "/" path) "index.html"))
     ((string-prefix-p "/" path)
      (string-remove-prefix "/" path))
     (t
      path))))

(defun magent-docs--url-fragment (url)
  "Return URL fragment for URL, including the leading #."
  (when (string-match "#.*\\'" url)
    (match-string 0 url)))

(defun magent-docs--relative-url (current-url target-url)
  "Return TARGET-URL relative to CURRENT-URL's output location."
  (if (magent-docs--external-url-p target-url)
      target-url
    (let* ((current-file (magent-docs--url-file current-url))
           (current-dir (or (file-name-directory current-file) ""))
           (target-file (magent-docs--url-file target-url))
           (fragment (or (magent-docs--url-fragment target-url) ""))
           (from (expand-file-name current-dir magent-docs--site-root))
           (to (expand-file-name target-file magent-docs--site-root)))
      (concat (file-relative-name to from) fragment))))

(defun magent-docs--read-file (path)
  "Return the full contents of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun magent-docs--write-file (path contents)
  "Write CONTENTS to PATH, creating parent directories."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert contents)))

(defun magent-docs--html-escape (value)
  "Escape VALUE for HTML text and attributes."
  (let ((text (format "%s" (or value ""))))
    (setq text (string-replace "&" "&amp;" text))
    (setq text (string-replace "<" "&lt;" text))
    (setq text (string-replace ">" "&gt;" text))
    (setq text (string-replace "\"" "&quot;" text))
    text))

(defun magent-docs--template-replace (template replacements)
  "Apply REPLACEMENTS to TEMPLATE.
REPLACEMENTS is an alist of string placeholders to string values."
  (let ((result template))
    (dolist (replacement replacements result)
      (setq result
            (string-replace (car replacement) (cdr replacement) result)))))

(defun magent-docs--page-metadata (file)
  "Read Magent documentation metadata from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (metadata)
      (goto-char (point-min))
      (while (re-search-forward "^#\\+\\([^: \t]+\\):[ \t]*\\(.*\\)$" nil t)
        (push (cons (downcase (match-string 1))
                    (string-trim (match-string 2)))
              metadata))
      metadata)))

(defun magent-docs--metadata (metadata key &optional fallback)
  "Return KEY from METADATA, or FALLBACK."
  (or (cdr (assoc key metadata)) fallback))

(defun magent-docs--page-url (file metadata)
  "Return public URL for FILE using METADATA."
  (or (magent-docs--metadata metadata "magent_permalink")
      (concat "/" (file-name-base file) ".html")))

(defun magent-docs--output-path (url)
  "Return output path for public URL."
  (cond
   ((string= url "/")
    (expand-file-name "index.html" magent-docs--site-root))
   ((string= url "/zh/")
    (expand-file-name "zh/index.html" magent-docs--site-root))
   ((string-prefix-p "/" url)
    (expand-file-name (substring url 1) magent-docs--site-root))
   (t
    (expand-file-name url magent-docs--site-root))))

(defun magent-docs--page-files ()
  "Return top-level Org page files in documentation order."
  (let ((files (directory-files magent-docs--docs-root t "\\.org\\'"))
        (order '("README.org"
                 "README.zh.org"
                 "COMMANDS.org"
                 "COMMANDS.zh.org"
                 "ARCHITECTURE.org"
                 "ARCHITECTURE.zh.org"
                 "ONBOARDING.org"
                 "ONBOARDING.zh.org"
                 "AGENT_WORKFLOW.org"
                 "AGENT_WORKFLOW.zh.org"
                 "AGENT_JOBS.org"
                 "AGENT_JOBS.zh.org"
                 "UI_BACKENDS.org"
                 "UI_BACKENDS.zh.org"
                 "TROUBLESHOOTING.org"
                 "TROUBLESHOOTING.zh.org"
                 "CONTRIBUTING.org"
                 "CONTRIBUTING.zh.org")))
    (cl-sort files #'<
             :key (lambda (file)
                    (let ((position (cl-position (file-name-nondirectory file)
                                                 order
                                                 :test #'string=)))
                      (or position 999))))))

(defun magent-docs--nav-html (lang current-url)
  "Return navigation HTML for LANG, marking CURRENT-URL."
  (mapconcat
   (lambda (item)
     (let ((title (car item))
           (url (cdr item)))
       (format "<a href=\"%s\"%s>%s</a>"
               (magent-docs--html-escape
                (magent-docs--relative-url current-url url))
               (if (string= url current-url) " aria-current=\"page\"" "")
               (magent-docs--html-escape title))))
   (cdr (assq lang magent-docs--navigation))
   "\n"))

(defun magent-docs--configure-plantuml ()
  "Configure Org Babel PlantUML support."
  (require 'ob-plantuml)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  (let ((jar (or (getenv "PLANTUML_JAR")
                 (let ((candidate (expand-file-name "plantuml.jar" user-emacs-directory)))
                   (and (file-exists-p candidate) candidate)))))
    (cond
     ((executable-find "plantuml")
      (setq org-plantuml-exec-mode 'plantuml)
      (setq org-plantuml-executable-path (executable-find "plantuml")))
     (jar
      (setq org-plantuml-exec-mode 'jar)
      (setq org-plantuml-jar-path jar))
     (t
      (error "No PlantUML executable found and PLANTUML_JAR is unset")))))

(defun magent-docs--execute-diagrams ()
  "Execute PlantUML blocks from docs/_diagrams/*.org."
  (let ((diagram-dir (expand-file-name "_diagrams/" magent-docs--docs-root)))
    (when (file-directory-p diagram-dir)
      (magent-docs--configure-plantuml)
      (dolist (file (directory-files diagram-dir t "\\.org\\'"))
        (message "Executing diagrams in %s" (file-relative-name file magent-docs--repo-root))
        (with-current-buffer (find-file-noselect file)
          (unwind-protect
              (let ((default-directory (file-name-directory file))
                    (org-confirm-babel-evaluate nil)
                    (make-backup-files nil)
                    (backup-inhibited t)
                    (auto-save-default nil))
                (org-with-wide-buffer
                 (goto-char (point-min))
                 (org-babel-map-src-blocks file
                   (when (string= lang "plantuml")
                     (let* ((info (org-babel-get-src-block-info 'light))
                            (params (nth 2 info))
                            (result-file (cdr (assq :file params)))
                            (target (and result-file
                                         (expand-file-name result-file default-directory))))
                       (when target
                         (make-directory (file-name-directory target) t))
                       (org-babel-execute-src-block)
                       (when (and target (not (file-exists-p target)))
                         (error "PlantUML did not generate %s" target))))))
                (save-buffer))
            (kill-buffer (current-buffer))))))))

(defun magent-docs--export-page-body (file)
  "Export FILE to an HTML body string."
  (with-current-buffer (find-file-noselect file)
    (unwind-protect
        (let ((org-export-with-toc nil)
              (org-export-with-section-numbers nil)
              (org-export-with-sub-superscripts nil)
              (org-export-with-smart-quotes t)
              (org-export-with-broken-links t)
              (org-export-use-babel nil)
              (org-export-time-stamp-file nil)
              (org-html-htmlize-output-type nil)
              (org-html-doctype "html5")
              (org-html-html5-fancy t)
              (org-html-head-include-default-style nil)
              (org-html-head-include-scripts nil)
              (org-html-validation-link nil)
              (org-html-self-link-headlines nil)
              (org-html-prefer-user-labels t)
              (org-html-link-org-files-as-html t))
          (org-with-wide-buffer
           (org-export-as 'html nil nil t nil)))
      (kill-buffer (current-buffer)))))

(defun magent-docs--render-page (file)
  "Render Org FILE into the static site."
  (let* ((metadata (magent-docs--page-metadata file))
         (url (magent-docs--page-url file metadata))
         (lang-name (magent-docs--metadata metadata "magent_lang" "en"))
         (lang (if (string= lang-name "zh") 'zh 'en))
         (title (magent-docs--metadata metadata "title" (file-name-base file)))
         (description (magent-docs--metadata metadata "description"
                                             magent-docs--site-description))
         (alt-url (magent-docs--metadata metadata "magent_alt_url"
                                         (if (eq lang 'zh) "/" "/zh/")))
         (template (magent-docs--read-file magent-docs--template-file))
         (body (magent-docs--export-page-body file))
         (document
          (magent-docs--template-replace
           template
           `(("{{lang}}" . ,(symbol-name lang))
             ("{{title}}" . ,(magent-docs--html-escape title))
             ("{{site_title}}" . ,(magent-docs--html-escape magent-docs--site-title))
             ("{{description}}" . ,(magent-docs--html-escape description))
             ("{{stylesheet_href}}" . ,(magent-docs--relative-url
                                         url "/assets/css/site.css"))
             ("{{brand_href}}" . ,(magent-docs--relative-url
                                    url (if (eq lang 'zh) "/zh/" "/")))
             ("{{language_url}}" . ,(magent-docs--relative-url url alt-url))
             ("{{language_label}}" . ,(if (eq lang 'zh) "English" "中文"))
             ("{{nav}}" . ,(magent-docs--nav-html lang url))
             ("{{content}}" . ,body)))))
    (message "Rendering %s -> %s" (file-relative-name file magent-docs--repo-root) url)
    (magent-docs--write-file (magent-docs--output-path url) document)))

(defun magent-docs--copy-assets ()
  "Copy static documentation assets into the site root."
  (let ((source (expand-file-name "assets/" magent-docs--docs-root))
        (target (expand-file-name "assets/" magent-docs--site-root)))
    (when (file-directory-p source)
      (make-directory (file-name-directory target) t)
      (copy-directory source target nil t t))))

(defun magent-docs-build ()
  "Build Magent documentation site."
  (when (file-directory-p magent-docs--site-root)
    (delete-directory magent-docs--site-root t))
  (make-directory magent-docs--site-root t)
  (magent-docs--execute-diagrams)
  (magent-docs--copy-assets)
  (dolist (file (magent-docs--page-files))
    (magent-docs--render-page file))
  (message "Built Magent docs in %s" magent-docs--site-root))

(magent-docs-build)

;;; build-docs.el ends here
