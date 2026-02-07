;;; magent-agent-fix.el --- Temporary fix file

(defun magent-agent--execute-tools (session agent tool-uses response callback iteration)
  "Execute TOOL-USES and continue agent loop."
  (let ((assistant-msg `((role . "assistant")
                         (content . ,(cdr (assq 'content response))))))
    ;; Add assistant message with tool uses to session
    (magent-session-add-message session 'assistant
                                 (cdr (assq 'content response)))

    ;; Execute each tool with permission check
    (dolist (tool-use tool-uses)
      (let* ((tool-id (cdr (assq 'id tool-use)))
             (tool-name (cdr (assq 'name tool-use)))
             (tool-input (cdr (assq 'input tool-use)))
             (permission (magent-agent-info-permission agent)))
        ;; Check permission before executing
        (if (magent-permission-allow-p permission tool-name)
            (let ((result (magent-tools-execute tool-name tool-input)))
              (magent-session-add-tool-result session tool-id result))
          ;; Permission denied
          (magent-session-add-tool-result session tool-id
                                          (format "Permission denied: tool '%s' not allowed" tool-name)))))

    ;; Continue the loop
    (magent-agent--loop session agent callback (1+ iteration))))
