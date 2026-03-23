# Streaming Performance Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix four concrete performance bottlenecks in magent's streaming UI pipeline to eliminate lag during and after LLM responses.

**Architecture:** All fixes are isolated to `magent-ui.el` and `magent-md2org.el`. No API changes; streaming callbacks and public functions keep their signatures. Fixes are ordered so each task is independently testable and committable.

**Tech Stack:** Emacs Lisp, ERT, `emacsclient` for live verification, `make test` for batch ERT.

---

## File Map

- Modify: `magent-ui.el` — batch buffer accumulation, flush, fontify, `with-insert` macro
- Modify: `magent-md2org.el` — inline conversion single-pass optimization
- Modify: `magent-config.el` — no new defcustoms needed (existing `fontify-threshold`/`fontify-idle-delay` already exist)
- Modify: `test/magent-test.el` — add streaming performance regression tests

---

## Task 1: Fix O(n²) Batch Buffer — List Accumulation

**Problem:** `magent-ui-insert-streaming` does `(concat batch-buffer text)` for every chunk,
copying the entire accumulator each time. A 10 KB response in 200 chunks = ~1 MB of string churn.

**Files:**
- Modify: `magent-ui.el` (lines around `magent-ui--streaming-batch-buffer`, `magent-ui-insert-streaming`, `magent-ui--flush-streaming-batch`, `magent-ui--reset-streaming-state`)
- Modify: `test/magent-test.el`

### Steps

- [ ] **Step 1.1: Write the failing test**

In `test/magent-test.el`, add after the existing streaming tests:

```elisp
(ert-deftest magent-test-streaming-batch-list-accumulation ()
  "insert-streaming accumulates chunks as a list, flush joins them correctly."
  (let ((buf (get-buffer-create " *magent-batch-test*")))
    (unwind-protect
        (with-current-buffer buf
          (magent-output-mode)
          ;; Reset the batch state manually
          (setq magent-ui--streaming-batch-buffer nil) ; will be a list after fix
          ;; Simulate 3 chunk inserts without timers
          (cl-letf (((symbol-function 'run-with-timer) #'ignore))
            (magent-ui-insert-streaming "chunk1")
            (magent-ui-insert-streaming " chunk2")
            (magent-ui-insert-streaming " chunk3"))
          ;; Flush should produce joined text
          (let ((inhibit-read-only t))
            (magent-ui--flush-streaming-batch))
          (should (string-match-p "chunk1 chunk2 chunk3"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max)))))
      (kill-buffer buf))))
```

- [ ] **Step 1.2: Run test to confirm it fails**

```bash
cd /home/jamie/opt/emacs.d/site-lisp/magent
make test 2>&1 | grep -A5 "batch-list-accumulation"
```

Expected: FAILED (behavior not yet changed).

- [ ] **Step 1.3: Change batch buffer type from string to list**

In `magent-ui.el`, update the defvar docstring and the three functions that touch the batch buffer:

**`magent-ui--streaming-batch-buffer`** (defvar, ~line 878):
```elisp
(defvar-local magent-ui--streaming-batch-buffer nil
  "List of text chunks waiting to be inserted (most recent first).
Joined and flushed by `magent-ui--flush-streaming-batch'.")
```

**`magent-ui--reset-streaming-state`** (~line 891):
```elisp
(setq magent-ui--streaming-batch-buffer nil)   ; was ""
```

**`magent-ui-insert-streaming`** (~line 953):
```elisp
(defun magent-ui-insert-streaming (text)
  "Insert streaming TEXT into output buffer.
Small chunks are batched to reduce UI updates."
  (let ((buf (magent-ui-get-buffer)))
    (with-current-buffer buf
      (push text magent-ui--streaming-batch-buffer)
      (unless magent-ui--streaming-batch-timer
        (setq magent-ui--streaming-batch-timer
              (run-with-timer magent-ui-batch-insert-delay nil
                              #'magent-ui--flush-streaming-batch))))))
```

**`magent-ui--flush-streaming-batch`** (~line 926) — change the condition check and the join:
```elisp
(defun magent-ui--flush-streaming-batch ()
  "Flush accumulated streaming text to buffer."
  (let ((buf (magent-ui-get-buffer)))
    (with-current-buffer buf
      (when magent-ui--streaming-batch-buffer
        (let* ((inhibit-read-only t)
               (ro-start (point-max))
               (text (apply #'concat (nreverse magent-ui--streaming-batch-buffer))))
          (setq magent-ui--streaming-batch-buffer nil)
          (goto-char (point-max))
          (condition-case nil
              (progn
                (insert text)
                (setq magent-ui--streaming-has-text t)
                (when (> (point-max) ro-start)
                  (add-text-properties ro-start (point-max) '(read-only t)))
                (when magent-auto-scroll
                  (goto-char (point-max))
                  (let ((win (get-buffer-window buf)))
                    (when win
                      (set-window-point win (point-max))))))
            ((beginning-of-buffer end-of-buffer beginning-of-line end-of-line)
             nil))))
      (when magent-ui--streaming-batch-timer
        (cancel-timer magent-ui--streaming-batch-timer)
        (setq magent-ui--streaming-batch-timer nil)))))
```

- [ ] **Step 1.4: Run test to confirm it passes**

```bash
make test 2>&1 | grep -A5 "batch-list-accumulation"
```

Expected: PASSED.

- [ ] **Step 1.5: Reload in live Emacs and verify streaming**

```bash
emacsclient --eval '(load "/home/jamie/opt/emacs.d/site-lisp/magent/magent-ui.el" nil t)'
```

Then send a short prompt like `"你好"` and confirm streaming text appears normally.

- [ ] **Step 1.6: Commit**

```bash
git add magent-ui.el test/magent-test.el
git commit -m "perf(ui): replace O(n²) string concat in batch buffer with list accumulation"
```

---

## Task 2: Wire Up Async Fontification (fontify-threshold is dead code)

**Problem:** `magent-ui-fontify-threshold` and `magent-ui-fontify-idle-delay` are defined in
`magent-config.el` but never referenced in `magent-ui.el`. The finish function always calls
`magent-md2org-convert-region` synchronously, blocking Emacs for large responses.

**Fix:** In `magent-ui-finish-streaming-fontify`, after the md2org pass, schedule a
`font-lock-ensure` over the new region with `run-with-idle-timer` when the region
exceeds `magent-ui-fontify-threshold`. For the md2org conversion itself, always run it
synchronously (it modifies text, must complete before the next UI operation) but defer
the subsequent org fontification.

**Files:**
- Modify: `magent-ui.el` (`magent-ui-finish-streaming-fontify`)
- Modify: `test/magent-test.el`

### Steps

- [ ] **Step 2.1: Write the failing test**

```elisp
(ert-deftest magent-test-finish-fontify-defers-large-response ()
  "font-lock-ensure is deferred when response exceeds fontify-threshold."
  (let ((fontify-called nil)
        (idle-timer-args nil)
        (magent-ui-fontify-threshold 50))
    (cl-letf (((symbol-function 'font-lock-ensure)
               (lambda (&rest _) (setq fontify-called t)))
              ((symbol-function 'run-with-idle-timer)
               (lambda (delay _repeat fn &rest args)
                 (setq idle-timer-args (list delay fn args))
                 nil)))
      (let ((buf (get-buffer-create " *magent-fontify-test*")))
        (unwind-protect
            (with-current-buffer buf
              (magent-output-mode)
              (let ((inhibit-read-only t))
                (insert (make-string 100 ?x))) ; > threshold
              (setq magent-ui--streaming-has-text t
                    magent-ui--response-body-start (point-min)
                    magent-ui--streaming-start (point-min))
              (cl-letf (((symbol-function 'magent-md2org-convert-region) #'ignore)
                        ((symbol-function 'magent-ui--flush-streaming-batch) #'ignore))
                (magent-ui-finish-streaming-fontify))
              ;; font-lock-ensure should NOT have been called synchronously
              (should (not fontify-called))
              ;; But a timer should have been scheduled
              (should idle-timer-args))
          (kill-buffer buf))))))
```

- [ ] **Step 2.2: Run test to confirm it fails**

```bash
make test 2>&1 | grep -A5 "finish-fontify-defers"
```

Expected: FAILED.

- [ ] **Step 2.3: Implement async fontification in finish-streaming-fontify**

In `magent-ui.el`, update `magent-ui-finish-streaming-fontify`. After the md2org pass,
add a conditional to schedule `font-lock-ensure` asynchronously for large regions:

```elisp
(defun magent-ui-finish-streaming-fontify ()
  "Finalize streaming section.
If no text was streamed (tool-only round), removes the orphaned heading.
When text was streamed, converts markdown to org-mode in the response body,
then fontifies the new region — asynchronously when it exceeds
`magent-ui-fontify-threshold' characters."
  (let ((buf (magent-ui-get-buffer)))
    (with-current-buffer buf
      (magent-ui--flush-streaming-batch)
      (when magent-ui--streaming-start
        (condition-case nil
            (if (not magent-ui--streaming-has-text)
                (when magent-ui--streaming-section-start
                  (let ((inhibit-read-only t))
                    (delete-region magent-ui--streaming-section-start
                                   (min magent-ui--streaming-start (point-max)))))
              (let* ((inhibit-read-only t)
                     (body-start magent-ui--response-body-start)
                     (body-end (point-max)))
                (when body-start
                  (magent-md2org-convert-region body-start body-end))
                (goto-char (point-max))
                (unless (eq (char-before) ?\n)
                  (insert "\n"))
                ;; Fontify: async for large regions, sync for small ones
                (when body-start
                  (let ((region-size (- (point-max) body-start)))
                    (if (> region-size magent-ui-fontify-threshold)
                        (let ((b buf)
                              (bstart body-start)
                              (bend (point-max)))
                          (run-with-idle-timer
                           magent-ui-fontify-idle-delay nil
                           (lambda ()
                             (when (buffer-live-p b)
                               (with-current-buffer b
                                 (let ((inhibit-read-only t))
                                   (font-lock-ensure bstart bend)))))))
                      (font-lock-ensure body-start (point-max)))))))
          ((beginning-of-buffer end-of-buffer beginning-of-line end-of-line)
           (magent-log "DEBUG Suppressed cursor error in streaming finish")
           nil))
        (setq magent-ui--streaming-section-start nil)
        (setq magent-ui--response-body-start nil)
        (magent-ui--reset-streaming-state)))))
```

- [ ] **Step 2.4: Run test to confirm it passes**

```bash
make test 2>&1 | grep -A5 "finish-fontify-defers"
```

Expected: PASSED.

- [ ] **Step 2.5: Run full test suite**

```bash
make test 2>&1 | tail -20
```

Expected: all existing tests still pass.

- [ ] **Step 2.6: Live verification**

```bash
emacsclient --eval '(load "/home/jamie/opt/emacs.d/site-lisp/magent/magent-ui.el" nil t)'
```

Send a prompt that produces a long response (e.g., `"写一篇500字的短文"`). Observe that
Emacs does NOT freeze at the end of streaming. The buffer should update fluidly and
fontification should complete within ~100ms after the response ends.

- [ ] **Step 2.7: Commit**

```bash
git add magent-ui.el test/magent-test.el
git commit -m "perf(ui): defer font-lock-ensure for large responses using fontify-threshold"
```

---

## Task 3: Optimize magent-md2org--convert-inline (Multiple Passes → Single Pass)

**Problem:** `magent-md2org--convert-inline` does 4–5 separate `re-search-forward`/`search-forward`
passes per line:
1. `` `code` `` → `~code~`
2. `**bold**` → placeholder
3. `*italic*` → placeholder
4. placeholder → `*bold*`
5. placeholder → `*bold*`

Passes 4–5 exist only to restore the `*` after pass 3 clobbered `**`. This can be done in a
single pass using a combined regexp with `replace-regexp-in-string` or a `while` loop with
`re-search-forward` and a dispatch on match groups.

**Files:**
- Modify: `magent-md2org.el`
- Modify: `test/magent-test.el`

### Steps

- [ ] **Step 3.1: Write tests for the existing inline behavior (regression anchor)**

```elisp
(ert-deftest magent-test-md2org-inline-basic ()
  "md2org inline converts backtick, bold, italic correctly."
  (should (equal (magent-md2org-convert-string "`code`")
                 "~code~"))
  (should (equal (magent-md2org-convert-string "**bold**")
                 "*bold*"))
  (should (equal (magent-md2org-convert-string "*italic*")
                 "/italic/"))
  (should (equal (magent-md2org-convert-string "**bold** and *italic* and `code`")
                 "*bold* and /italic/ and ~code~")))

(ert-deftest magent-test-md2org-inline-no-clobber ()
  "md2org bold does not clobber italic markers."
  ;; Key regression: **bold** should not turn *italic* into //italic//
  (should (equal (magent-md2org-convert-string "**b** *i*")
                 "*b* /i/"))
  (should (equal (magent-md2org-convert-string "**hello world**")
                 "*hello world*")))
```

- [ ] **Step 3.2: Run tests to confirm they pass with existing code**

```bash
make test 2>&1 | grep -A5 "md2org-inline"
```

Expected: PASSED (these are regression anchors).

- [ ] **Step 3.3: Rewrite magent-md2org--convert-inline as a single-pass function**

Replace the current body of `magent-md2org--convert-inline` in `magent-md2org.el`:

```elisp
(defun magent-md2org--convert-inline ()
  "Convert inline markdown formatting on the current line.
Single-pass: handles `code`, **bold**, *italic* in one sweep.
Converts: `code` → ~code~, **bold** → *bold*, *italic* → /italic/."
  (save-restriction
    (narrow-to-region (line-beginning-position) (line-end-position))
    (goto-char (point-min))
    ;; Combined pattern: backtick-code | double-star-bold | single-star-italic
    ;; Order matters: double-star must be tried before single-star.
    (while (re-search-forward
            "`\\([^`\n]+\\)`\\|\\*\\*\\([^*\n]+\\)\\*\\*\\|\\*\\([^*\n]+\\)\\*"
            nil t)
      (cond
       ((match-beginning 1)                    ; `code`
        (replace-match (concat "~" (match-string 1) "~") t t))
       ((match-beginning 2)                    ; **bold**
        (replace-match (concat "*" (match-string 2) "*") t t))
       ((match-beginning 3)                    ; *italic*
        (replace-match (concat "/" (match-string 3) "/") t t))))))
```

- [ ] **Step 3.4: Run inline tests to confirm they still pass**

```bash
make test 2>&1 | grep -A5 "md2org-inline"
```

Expected: all PASSED.

- [ ] **Step 3.5: Run full suite**

```bash
make test 2>&1 | tail -20
```

Expected: no regressions.

- [ ] **Step 3.6: Commit**

```bash
git add magent-md2org.el test/magent-test.el
git commit -m "perf(md2org): replace 4-pass inline conversion with single combined regexp"
```

---

## Task 4: Suppress Spurious Fontification During Flush (inhibit-modification-hooks)

**Problem:** `magent-ui--flush-streaming-batch` calls `add-text-properties` after each insert,
which triggers `font-lock-after-change-functions` via modification hooks. For a 50ms batch
cycle repeated 20× over a 1s response, that is 20 unnecessary org fontification passes on
already-inserted (non-markdown) text.

**Fix:** Wrap the `add-text-properties` call inside `with-silent-modifications` so text
property changes do not fire modification hooks. The text content itself is added with
`inhibit-read-only t` (already there), and its fontification is handled once at the end
by Task 2's async `font-lock-ensure`.

**Files:**
- Modify: `magent-ui.el` (`magent-ui--flush-streaming-batch`)
- Modify: `test/magent-test.el`

### Steps

- [ ] **Step 4.1: Write the failing test**

```elisp
(ert-deftest magent-test-flush-suppresses-modification-hooks ()
  "flush-streaming-batch does not trigger modification hooks for read-only property."
  (let ((hook-calls 0))
    (let ((buf (get-buffer-create " *magent-hook-test*")))
      (unwind-protect
          (with-current-buffer buf
            (magent-output-mode)
            (setq magent-ui--streaming-batch-buffer (list "hello"))
            (add-hook 'after-change-functions
                      (lambda (&rest _) (cl-incf hook-calls)) nil t)
            (magent-ui--flush-streaming-batch)
            ;; The text insert fires after-change-functions once (expected).
            ;; The add-text-properties for read-only should NOT fire it again.
            (should (<= hook-calls 1)))
        (kill-buffer buf)))))
```

- [ ] **Step 4.2: Run test to observe current hook count**

```bash
make test 2>&1 | grep -A5 "suppresses-modification-hooks"
```

Note the failure reason (hook-calls > 1).

- [ ] **Step 4.3: Wrap add-text-properties in with-silent-modifications**

In `magent-ui--flush-streaming-batch`, change:

```elisp
;; Before (triggers modification hooks):
(add-text-properties ro-start (point-max) '(read-only t))
```

To:

```elisp
;; After (silent):
(with-silent-modifications
  (add-text-properties ro-start (point-max) '(read-only t)))
```

The full updated function body in context (~line 926):

```elisp
(defun magent-ui--flush-streaming-batch ()
  "Flush accumulated streaming text to buffer."
  (let ((buf (magent-ui-get-buffer)))
    (with-current-buffer buf
      (when magent-ui--streaming-batch-buffer
        (let* ((inhibit-read-only t)
               (ro-start (point-max))
               (text (apply #'concat (nreverse magent-ui--streaming-batch-buffer))))
          (setq magent-ui--streaming-batch-buffer nil)
          (goto-char (point-max))
          (condition-case nil
              (progn
                (insert text)
                (setq magent-ui--streaming-has-text t)
                (when (> (point-max) ro-start)
                  (with-silent-modifications
                    (add-text-properties ro-start (point-max) '(read-only t))))
                (when magent-auto-scroll
                  (goto-char (point-max))
                  (let ((win (get-buffer-window buf)))
                    (when win
                      (set-window-point win (point-max))))))
            ((beginning-of-buffer end-of-buffer beginning-of-line end-of-line)
             nil))))
      (when magent-ui--streaming-batch-timer
        (cancel-timer magent-ui--streaming-batch-timer)
        (setq magent-ui--streaming-batch-timer nil)))))
```

Note: This function is now the canonical version incorporating Task 1's list-based change.
Make sure both changes are present before running tests.

- [ ] **Step 4.4: Run test to confirm it passes**

```bash
make test 2>&1 | grep -A5 "suppresses-modification-hooks"
```

Expected: PASSED.

- [ ] **Step 4.5: Full test suite**

```bash
make test 2>&1 | tail -20
```

Expected: all tests pass.

- [ ] **Step 4.6: Byte-compile to catch warnings**

```bash
make compile 2>&1 | grep -i "warning\|error"
```

Expected: no new warnings.

- [ ] **Step 4.7: Live end-to-end verification**

```bash
emacsclient --eval '
(progn
  (load "/home/jamie/opt/emacs.d/site-lisp/magent/magent-ui.el" nil t)
  (load "/home/jamie/opt/emacs.d/site-lisp/magent/magent-md2org.el" nil t))'
```

Test three scenarios:
1. Short non-tool prompt `"你好"` — streaming text appears; no Emacs freeze
2. Long response `"列出20个常见设计模式并简要描述"` — no freeze at end; fontification deferred
3. Tool-use prompt `"帮我看下 emacs 里面有多少 buffer"` — tool blocks fold correctly

- [ ] **Step 4.8: Commit**

```bash
git add magent-ui.el test/magent-test.el
git commit -m "perf(ui): suppress modification hooks on read-only property in flush"
```

---

## Summary of Changes

| File | What changed |
|------|-------------|
| `magent-ui.el` | Batch buffer: `string` → `list`; flush: `apply #'concat nreverse`; finish: async `font-lock-ensure` via idle timer; flush: `with-silent-modifications` on `add-text-properties` |
| `magent-md2org.el` | `convert-inline`: 4-pass → 1-pass combined regexp |
| `test/magent-test.el` | 4 new ERT tests covering each fix |

Expected result: visibly smoother streaming with no freeze on response completion, and
faster per-response md2org conversion (fewer GC allocations + fewer regexp passes).
