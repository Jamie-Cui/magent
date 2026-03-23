;;; magent-benchmark-suite.el --- Starter task suite for Magent benchmark  -*- lexical-binding: t; -*-

;;; Commentary:

;; Curated starter suite for benchmarking capability progressive disclosure.

;;; Code:

(require 'org)
(require 'magent-benchmark)

(declare-function magit-status-setup-buffer "ext:magit")

(defun magent-benchmark-suite--runtime-buffer-setup (_task)
  "Prepare the runtime inspection fixture buffer."
  (let* ((state (magent-benchmark-open-file-buffer
                 "runtime/benchmark-elisp-runtime.el"
                 (lambda (_buffer)
                   (goto-char (point-max))
                   (insert "\n;; unsaved benchmark change\n"))))
         (buffer (plist-get state :buffer)))
    (plist-put
     state :expected
     (list :buffer-name (buffer-name buffer)
           :mode "emacs-lisp-mode"
           :buffer-count (length (buffer-list))
           :modified "modified"))
    (plist-put
     state :metadata
     (list :buffer-name (buffer-name buffer)
           :file-path (plist-get state :file-path)))
    state))

(defun magent-benchmark-suite--org-buffer-setup (_task)
  "Prepare the org fixture buffer with unsaved live-only changes."
  (let* ((state (magent-benchmark-open-file-buffer
                 "org-suite/benchmark-org-suite.org"
                 (lambda (_buffer)
                   (goto-char (point-max))
                   (insert "\n* TODO Live Benchmark Followup\n")
                   (insert "** TODO Measure disclosure drift\n"))))
         (buffer (plist-get state :buffer)))
    (plist-put
     state :expected
     (list :buffer-name (buffer-name buffer)
           :heading-count (magent-benchmark-count-org-headings buffer)
           :top-level-count (magent-benchmark-count-org-top-level-headings buffer)
           :todo-count (magent-benchmark-count-org-todos buffer)))
    (plist-put
     state :metadata
     (list :buffer-name (buffer-name buffer)
           :file-path (plist-get state :file-path)))
    state))

(defun magent-benchmark-suite--plain-file-setup (relative-path)
  "Prepare a plain file fixture from RELATIVE-PATH."
  (let ((state (magent-benchmark-open-file-buffer relative-path)))
    (plist-put
     state :metadata
     (list :buffer-name (buffer-name (plist-get state :buffer))
           :file-path (plist-get state :file-path)))
    state))

(defun magent-benchmark-suite--magit-buffer-setup (_task)
  "Prepare a temporary Magit status buffer, or skip when unavailable."
  (if (or (not (executable-find "git"))
          (not (require 'magit nil t)))
      (list :skip "magit or git is unavailable")
    (let* ((repo (make-temp-file "magent-benchmark-git-" t))
           (readme (expand-file-name "README.md" repo))
           buffer)
      (call-process "git" nil nil nil "init" "-q" repo)
      (with-temp-file readme
        (insert "# Magit Benchmark Fixture\n"))
      (setq buffer (magit-status-setup-buffer repo))
      (list
       :buffer buffer
       :project-root repo
       :request-context (magent-benchmark-buffer-context buffer repo)
       :expected (list :buffer-name (buffer-name buffer)
                       :mode "magit-status-mode")
       :metadata (list :buffer-name (buffer-name buffer)
                       :project-root repo)
       :cleanup
       (lambda ()
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (delete-directory repo t))))))

(setq magent-benchmark-suite
      (list
       (magent-benchmark-task-create
        :id "runtime-buffer-count"
        :title "Live Emacs buffer count"
        :category "capability-match"
        :prompt "请检查 live Emacs state，告诉我现在一共有多少个 buffer。只回答数字并加一句非常简短的依据。"
        :setup-fn #'magent-benchmark-suite--runtime-buffer-setup
        :expected-capabilities '("emacs-runtime-inspection")
        :score-fn
        (lambda (_task run)
          (magent-benchmark-score-integer-answer
           run
           (plist-get (plist-get run :expected) :buffer-count)
           :require-tool "emacs_eval")))
       (magent-benchmark-task-create
        :id "runtime-buffer-mode"
        :title "Live buffer major mode"
        :category "capability-match"
        :prompt "请检查 live Emacs 里的 buffer `benchmark-elisp-runtime.el`，告诉我它当前的 major mode 是什么。不要只看文件扩展名。"
        :setup-fn #'magent-benchmark-suite--runtime-buffer-setup
        :expected-capabilities '("emacs-runtime-inspection")
        :score-fn
        (lambda (_task run)
          (magent-benchmark-score-regexp-answer
           run "emacs-lisp-mode"
           :require-tool "emacs_eval")))
       (magent-benchmark-task-create
        :id "runtime-buffer-modified"
        :title "Live buffer modified flag"
        :category "capability-ambiguous"
        :prompt "看看 `benchmark-elisp-runtime.el` 这个 buffer 现在是 `modified` 还是 `clean`。只回答 `modified` 或 `clean`，再加一句依据。"
        :setup-fn #'magent-benchmark-suite--runtime-buffer-setup
        :expected-capabilities '("emacs-runtime-inspection")
        :score-fn
        (lambda (_task run)
          (magent-benchmark-score-regexp-answer
           run "\\bmodified\\b"
           :require-tool "emacs_eval")))
       (magent-benchmark-task-create
        :id "org-heading-count-live"
        :title "Live org heading count"
        :category "capability-match"
        :prompt "请按当前 live buffer 内容回答：`benchmark-org-suite.org` 现在一共有多少个 heading？不要只读磁盘文件。"
        :setup-fn #'magent-benchmark-suite--org-buffer-setup
        :expected-capabilities '("org-structure-workflow")
        :score-fn
        (lambda (_task run)
          (magent-benchmark-score-integer-answer
           run
           (plist-get (plist-get run :expected) :heading-count)
           :require-tool "emacs_eval")))
       (magent-benchmark-task-create
        :id "org-top-level-count-live"
        :title "Live org top-level count"
        :category "capability-ambiguous"
        :prompt "`benchmark-org-suite.org` 现在顶层一共有几个？按当前 buffer 回答，不要只读文件。"
        :setup-fn #'magent-benchmark-suite--org-buffer-setup
        :expected-capabilities '("org-structure-workflow")
        :score-fn
        (lambda (_task run)
          (magent-benchmark-score-integer-answer
           run
           (plist-get (plist-get run :expected) :top-level-count)
           :require-tool "emacs_eval")))
       (magent-benchmark-task-create
        :id "org-todo-count-live"
        :title "Live org TODO count"
        :category "capability-match"
        :prompt "请按当前 live buffer 内容回答：`benchmark-org-suite.org` 里现在有几个 TODO heading？"
        :setup-fn #'magent-benchmark-suite--org-buffer-setup
        :expected-capabilities '("org-structure-workflow")
        :score-fn
        (lambda (_task run)
          (magent-benchmark-score-integer-answer
           run
           (plist-get (plist-get run :expected) :todo-count)
           :require-tool "emacs_eval")))
       (magent-benchmark-task-create
        :id "plain-readme-line-count"
        :title "Plain README line count"
        :category "capability-negative"
        :prompt (format "文件 `%s` 一共有多少行？只回答数字和一句极短说明。"
                        (magent-benchmark-fixture-path
                         "plain-project/README.md"))
        :setup-fn
        (lambda (_task)
          (let* ((relative "plain-project/README.md")
                 (state (magent-benchmark-suite--plain-file-setup relative)))
            (plist-put
             state :expected
             (list :line-count (magent-benchmark-buffer-line-count
                                (plist-get state :file-path))))
            state))
        :score-fn
        (lambda (_task run)
          (magent-benchmark-score-integer-answer
           run
           (plist-get (plist-get run :expected) :line-count))))
       (magent-benchmark-task-create
        :id "plain-readme-title"
        :title "Plain README first heading"
        :category "capability-negative"
        :prompt (format "读取 `%s`，告诉我第一行标题是什么。"
                        (magent-benchmark-fixture-path
                         "plain-project/README.md"))
        :setup-fn
        (lambda (_task)
          (let* ((relative "plain-project/README.md")
                 (state (magent-benchmark-suite--plain-file-setup relative)))
            (plist-put
             state :expected
             (list :title (regexp-quote
                           (magent-benchmark-file-first-line
                            (plist-get state :file-path)))))
            state))
        :score-fn
        (lambda (_task run)
          (magent-benchmark-score-regexp-answer
           run
           (plist-get (plist-get run :expected) :title))))
       (magent-benchmark-task-create
        :id "commit-guide-title"
        :title "Commit guide title without Magit"
        :category "potential-misfire"
        :prompt (format "读取 `%s`，告诉我第一行标题是什么。"
                        (magent-benchmark-fixture-path
                         "plain-project/docs/commit-guide.md"))
        :setup-fn
        (lambda (_task)
          (let* ((relative "plain-project/docs/commit-guide.md")
                 (state (magent-benchmark-suite--plain-file-setup relative)))
            (plist-put
             state :expected
             (list :title (regexp-quote
                           (magent-benchmark-file-first-line
                            (plist-get state :file-path)))))
            state))
        :forbidden-capabilities '("magit-workflow")
        :score-fn
        (lambda (_task run)
          (magent-benchmark-score-regexp-answer
           run
           (plist-get (plist-get run :expected) :title))))
       (magent-benchmark-task-create
        :id "status-report-line-count"
        :title "Status report line count without Magit"
        :category "potential-misfire"
        :prompt (format "文件 `%s` 一共有多少行？只回答数字。"
                        (magent-benchmark-fixture-path
                         "plain-project/docs/status-report.md"))
        :setup-fn
        (lambda (_task)
          (let* ((relative "plain-project/docs/status-report.md")
                 (state (magent-benchmark-suite--plain-file-setup relative)))
            (plist-put
             state :expected
             (list :line-count (magent-benchmark-buffer-line-count
                                (plist-get state :file-path))))
            state))
        :forbidden-capabilities '("magit-workflow")
        :score-fn
        (lambda (_task run)
          (magent-benchmark-score-integer-answer
           run
           (plist-get (plist-get run :expected) :line-count))))
       (magent-benchmark-task-create
        :id "magit-buffer-mode-live"
        :title "Live Magit status buffer mode"
        :category "capability-match"
        :prompt "请检查 live Emacs 里的 Magit status buffer，告诉我它当前的 major mode 是什么。"
        :setup-fn #'magent-benchmark-suite--magit-buffer-setup
        :expected-capabilities '("magit-workflow")
        :score-fn
        (lambda (_task run)
          (if (plist-get (plist-get run :setup) :skip)
              (magent-benchmark-make-score
               :task-success nil
               :constraint-success nil
               :quality-score 0
               :notes '("magit fixture skipped"))
            (magent-benchmark-score-regexp-answer
             run "magit-status-mode"
             :require-tool "emacs_eval"))))))

(provide 'magent-benchmark-suite)
;;; magent-benchmark-suite.el ends here
