;;; magent-md2org.el --- Markdown to Org conversion helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Small conversion helpers used by Magent's UI when rendering assistant
;; output into an org-mode buffer.

;;; Code:

(require 'subr-x)

(defun magent-md2org-convert-region (beg end)
  "Convert markdown to org-mode in region BEG..END.
Headers are shifted +1 level (# -> **, ## -> ***).
Fenced code blocks (```) become #+begin_src/#+end_src.
Bold (**text**) becomes *text*, italic (*text*) becomes /text/,
inline code (`code`) becomes ~code~.
Preserves #+begin_tool and #+begin_think blocks unchanged."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((in-code nil)
            (in-special nil))
        (while (not (eobp))
          (cond
           ((and (not in-code) (not in-special)
                 (looking-at "#\\+begin_\\(?:tool\\|think\\)"))
            (setq in-special t))
           ((and in-special
                 (looking-at "#\\+end_\\(?:tool\\|think\\)"))
            (setq in-special nil))
           (in-special nil)
           ((looking-at "[ \t]*```")
            (if in-code
                (progn
                  (delete-region (line-beginning-position) (line-end-position))
                  (insert "#+end_src")
                  (setq in-code nil))
              (let ((lang (save-excursion
                            (skip-chars-forward " \t`")
                            (string-trim
                             (buffer-substring-no-properties
                              (point) (line-end-position))))))
                (delete-region (line-beginning-position) (line-end-position))
                (insert (if (string-empty-p lang)
                            "#+begin_src"
                          (concat "#+begin_src " lang)))
                (setq in-code t))))
           (in-code nil)
           ((looking-at "\\(#+\\) +")
            (let* ((level (min (length (match-string 1)) 6))
                   (stars (make-string (1+ level) ?*))
                   (content-start (match-end 0)))
              (delete-region (line-beginning-position) content-start)
              (goto-char (line-beginning-position))
              (insert stars " ")))
           (t (magent-md2org--convert-inline)))
          (forward-line 1))))))

(defun magent-md2org--convert-inline ()
  "Convert inline markdown formatting on the current line.
Handles: `code` -> ~code~, **bold** -> *bold*, *italic* -> /italic/."
  (save-restriction
    (narrow-to-region (line-beginning-position) (line-end-position))
    (let ((bold-open (string 1))
          (bold-close (string 2)))
      (goto-char (point-min))
      (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
        (replace-match (concat "~" (match-string 1) "~") t t))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\([^*\n]+\\)\\*\\*" nil t)
        (replace-match (concat bold-open (match-string 1) bold-close) t t))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\([^*\n]+\\)\\*" nil t)
        (replace-match (concat "/" (match-string 1) "/") t t))
      (goto-char (point-min))
      (while (search-forward bold-open nil t)
        (replace-match "*" t t))
      (goto-char (point-min))
      (while (search-forward bold-close nil t)
        (replace-match "*" t t)))))

(defun magent-md2org-convert-string (text)
  "Return TEXT converted from markdown to org-mode."
  (with-temp-buffer
    (insert text)
    (magent-md2org-convert-region (point-min) (point-max))
    (buffer-string)))

(provide 'magent-md2org)
;;; magent-md2org.el ends here
