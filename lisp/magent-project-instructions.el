;;; magent-project-instructions.el --- Scoped project instruction discovery  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Discover bounded project instruction files such as AGENTS.md from the
;; project root toward request-local files.  Discovery never escapes the
;; project root, including through symlinks.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magent-config)
(require 'magent-prompt)

(defun magent-project-instructions--normalized-directory (directory)
  "Return DIRECTORY as a canonical directory name, or nil."
  (when (and (stringp directory) (file-directory-p directory))
    (file-name-as-directory (file-truename directory))))

(defun magent-project-instructions--inside-root-p (path root)
  "Return non-nil when canonical PATH is inside canonical ROOT."
  (and path root
       (string-prefix-p root
                        (file-name-as-directory
                         (if (file-directory-p path)
                             (file-truename path)
                           (file-name-directory (file-truename path)))))))

(defun magent-project-instructions--context-paths (request-context)
  "Return file paths declared by REQUEST-CONTEXT."
  (delete-dups
   (delq nil
         (append
          (list (plist-get request-context :file-path))
          (copy-sequence (plist-get request-context :resource-paths))))))

(defun magent-project-instructions--directory-chain (root target)
  "Return directories from ROOT through TARGET, inclusive."
  (when (and root target
             (magent-project-instructions--inside-root-p target root))
    (let* ((directory (if (file-directory-p target)
                          (file-name-as-directory (file-truename target))
                        (file-name-as-directory
                         (file-name-directory (file-truename target)))))
           chain)
      (while (and directory (string-prefix-p root directory))
        (push directory chain)
        (setq directory
              (unless (equal directory root)
                (file-name-as-directory
                 (file-name-directory
                  (directory-file-name directory))))))
      chain)))

(defun magent-project-instructions--search-directories (root request-context)
  "Return ordered instruction search directories for ROOT and REQUEST-CONTEXT."
  (let ((targets (magent-project-instructions--context-paths request-context))
        result)
    (if targets
        (dolist (target targets)
          (dolist (directory
                   (magent-project-instructions--directory-chain root target))
            (unless (member directory result)
              (setq result (append result (list directory))))))
      (setq result (list root)))
    (or result (list root))))

(defun magent-project-instructions--safe-file-p (file root)
  "Return non-nil when FILE is a readable regular file contained by ROOT."
  (and (file-regular-p file)
       (file-readable-p file)
       (let ((true-file (file-truename file)))
         (and (string-prefix-p root true-file)
              (not (file-directory-p true-file))))))

(defun magent-project-instructions--read-prefix (file max-bytes)
  "Read at most MAX-BYTES from FILE and return decoded, JSON-safe text.
Replace undecodable raw bytes, including a multibyte character cut by the byte
limit, so the resulting system prompt remains serializable by gptel."
  (with-temp-buffer
    (insert-file-contents file nil 0 max-bytes)
    (string-trim-right
     (mapconcat (lambda (char)
                  (if (eq (char-charset char) 'eight-bit)
                      "�"
                    (string char)))
                (buffer-string)
                ""))))

(defun magent-project-instructions-discover (project-root request-context)
  "Return ordered project instruction entries for PROJECT-ROOT.
REQUEST-CONTEXT may name `:file-path' and `:resource-paths'.  Each result is a
plist with `:file', `:scope', and `:content'."
  (when (and magent-project-instructions-max-bytes
             (> magent-project-instructions-max-bytes 0)
             (stringp project-root))
    (when-let* ((root (magent-project-instructions--normalized-directory
                       project-root)))
      (let ((remaining magent-project-instructions-max-bytes)
            entries)
        (dolist (directory
                 (magent-project-instructions--search-directories
                  root request-context))
          (dolist (name magent-project-instruction-file-names)
            (let ((file (expand-file-name name directory)))
              (when (and (> remaining 0)
                         (magent-project-instructions--safe-file-p file root))
                (let* ((size (file-attribute-size (file-attributes file)))
                       (count (min remaining size))
                       (content
                        (magent-project-instructions--read-prefix file count)))
                  (unless (string-empty-p content)
                    (push (list :file file
                                :scope (file-relative-name directory root)
                                :content content)
                          entries))
                  (setq remaining (- remaining count)))))))
        (nreverse entries)))))

(defun magent-project-instructions-system-message
    (project-root request-context)
  "Return a scoped instruction system message for PROJECT-ROOT, or nil."
  (when-let* ((root (magent-project-instructions--normalized-directory
                     project-root))
              (entries (magent-project-instructions-discover
                        root request-context)))
    (magent-prompt-render
     "internal/project-instructions.org"
     `((instructions
        . ,(mapconcat
            (lambda (entry)
              (format "** %s (scope: %s)\n\n%s"
                      (file-relative-name (plist-get entry :file) root)
                      (plist-get entry :scope)
                      (plist-get entry :content)))
            entries
            "\n\n"))))))

(provide 'magent-project-instructions)
;;; magent-project-instructions.el ends here
