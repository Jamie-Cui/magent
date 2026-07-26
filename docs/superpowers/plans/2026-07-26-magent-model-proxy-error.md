# Magent Model Inheritance and Proxy Error Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore nil-model inheritance and make proxied provider HTTP errors visible to Magent so agent-shell no longer completes with an empty reply.

**Architecture:** Keep `gptel-request` as the transport.  Correct the project-owned per-agent dynamic binding, then add one Magent-scoped curl-argument boundary advice that suppresses proxy CONNECT headers without changing unmanaged gptel traffic.

**Tech Stack:** Emacs Lisp, ERT, gptel, curl, agent-shell

## Global Constraints

- Preserve gptel's provider, HTTP, curl, and SSE plumbing.
- Do not modify the installed third-party gptel package or global user configuration.
- Scope proxy behavior to requests recognized by `magent-llm-gptel--managed-info-p`.
- Keep advice installation and curl argument insertion idempotent.
- Follow red-green-refactor for each behavior change.
- Work in the current installed checkout because it is the source loaded by live Emacs.

---

### Task 1: Restore nil-model inheritance

**Files:**

- Modify: `test/magent-test.el:1621`
- Modify: `lisp/magent-agent-info.el:92`

**Step 1: Write the failing regression test**

Add a model assertion to the existing no-overrides test:

```elisp
;; Agent with no overrides
(let ((agent (magent-agent-info-create :name "t2" :mode 'primary)))
  (magent-agent-info-apply-gptel-overrides
   agent
   (lambda ()
     (should (= gptel-temperature 1.0))
     (should (eq gptel-model 'default-model)))))
```

Keep `magent-test-agent-info-symbol-model-overrides-gptel-model` unchanged as
the regression guard for explicit symbol overrides.

**Step 2: Run the test suite and verify the new assertion fails**

Run:

```bash
make EMACS=/Users/jamie/opt/emacs-src/nextstep/Emacs.app/Contents/MacOS/Emacs test-unit
```

Expected: `magent-test-agent-info-apply-gptel-overrides` fails because the
captured dynamic `gptel-model` is `nil`, while the existing symbol override
test still passes.

**Step 3: Implement the minimal model fix**

Change the model selection order in
`magent-agent-info-apply-gptel-overrides`:

```elisp
(gptel-model (cond
              ((and (consp model-field)
                    (symbolp (cdr model-field)))
               (cdr model-field))
              ((null model-field)
               (default-value 'gptel-model))
              ((symbolp model-field) model-field)
              ((stringp model-field) (intern model-field))
              (t (default-value 'gptel-model))))
```

Do not change backend or temperature selection.

**Step 4: Run the test suite and verify it passes**

Run:

```bash
make EMACS=/Users/jamie/opt/emacs-src/nextstep/Emacs.app/Contents/MacOS/Emacs test-unit
```

Expected: all ERT tests pass, including nil inheritance and explicit symbol
override coverage.

**Step 5: Commit the model fix**

```bash
git add lisp/magent-agent-info.el test/magent-test.el
git commit -m "fix: inherit default model for nil agent override"
```

---

### Task 2: Suppress proxy CONNECT headers for Magent requests

**Files:**

- Modify: `test/magent-test.el:2560`
- Modify: `lisp/magent-llm-gptel.el:35`
- Modify: `lisp/magent-llm-gptel.el:177`
- Modify: `lisp/magent-llm-gptel.el:205`

**Step 1: Write failing behavior tests**

Add the following tests near the existing gptel adapter metadata tests:

```elisp
(ert-deftest magent-test-llm-gptel-suppresses-connect-headers-for-managed-proxy ()
  "Managed proxied requests suppress proxy CONNECT response headers."
  (require 'magent-llm-gptel)
  (let ((gptel-proxy "http://127.0.0.1:10808"))
    (should
     (equal
      (magent-llm-gptel--suppress-connect-headers-a
       (lambda (_info) '("--base"))
       '(:magent-llm-gptel t))
      '("--base" "--suppress-connect-headers")))))

(ert-deftest magent-test-llm-gptel-does-not-duplicate-connect-header-argument ()
  "Managed proxy suppression is inserted at most once."
  (require 'magent-llm-gptel)
  (let ((gptel-proxy "http://127.0.0.1:10808"))
    (should
     (equal
      (magent-llm-gptel--suppress-connect-headers-a
       (lambda (_info) '("--base" "--suppress-connect-headers"))
       '(:magent-llm-gptel t))
      '("--base" "--suppress-connect-headers")))))

(ert-deftest magent-test-llm-gptel-leaves-unproxied-managed-curl-args-unchanged ()
  "Managed requests without a proxy keep gptel's curl arguments."
  (require 'magent-llm-gptel)
  (let ((gptel-proxy ""))
    (should
     (equal
      (magent-llm-gptel--suppress-connect-headers-a
       (lambda (_info) '("--base"))
       '(:magent-llm-gptel t))
      '("--base")))))

(ert-deftest magent-test-llm-gptel-leaves-unmanaged-proxy-curl-args-unchanged ()
  "Unmanaged gptel requests keep their original curl arguments."
  (require 'magent-llm-gptel)
  (let ((gptel-proxy "http://127.0.0.1:10808"))
    (should
     (equal
      (magent-llm-gptel--suppress-connect-headers-a
       (lambda (_info) '("--base"))
       '(:other-client t))
      '("--base")))))
```

Add an installation test that stubs the advice API instead of mutating the
process-wide advice chain:

```elisp
(ert-deftest magent-test-llm-gptel-installs-connect-header-advice-once ()
  "Repeated boundary initialization installs proxy advice once."
  (require 'magent-llm-gptel)
  (let (installed
        (add-count 0))
    (cl-letf
        (((symbol-function 'advice-member-p)
          (lambda (function symbol)
            (if (and
                 (eq function
                     #'magent-llm-gptel--suppress-connect-headers-a)
                 (eq symbol 'gptel-curl--get-config-args))
                installed
              t)))
         ((symbol-function 'advice-add)
          (lambda (symbol _where function &optional _props)
            (when (and
                   (eq function
                       #'magent-llm-gptel--suppress-connect-headers-a)
                   (eq symbol 'gptel-curl--get-config-args))
              (setq installed t)
              (cl-incf add-count)))))
      (magent-llm-gptel--install-boundary-advice)
      (magent-llm-gptel--install-boundary-advice))
    (should (= add-count 1))))
```

**Step 2: Run the test suite and verify the new tests fail**

Run:

```bash
make EMACS=/Users/jamie/opt/emacs-src/nextstep/Emacs.app/Contents/MacOS/Emacs test-unit
```

Expected: the five new tests fail with
`void-function magent-llm-gptel--suppress-connect-headers-a` or a missing
advice installation.

**Step 3: Implement the scoped curl argument advice**

Declare the gptel proxy variable with the other gptel declarations:

```elisp
(defvar gptel-proxy)
```

Add the adapter-local around advice:

```elisp
(defun magent-llm-gptel--suppress-connect-headers-a
    (orig-fn info &rest args)
  "Suppress proxy CONNECT headers for Magent-managed curl requests."
  (let ((curl-args (apply orig-fn info args)))
    (if (and (magent-llm-gptel--managed-info-p info)
             (stringp gptel-proxy)
             (not (string-empty-p gptel-proxy))
             (not (member "--suppress-connect-headers" curl-args)))
        (append curl-args '("--suppress-connect-headers"))
      curl-args)))
```

Install it idempotently in
`magent-llm-gptel--install-boundary-advice`:

```elisp
(unless
    (advice-member-p #'magent-llm-gptel--suppress-connect-headers-a
                     'gptel-curl--get-config-args)
  (advice-add 'gptel-curl--get-config-args
              :around
              #'magent-llm-gptel--suppress-connect-headers-a))
```

Keep all existing boundary advice unchanged.

**Step 4: Run the test suite and verify it passes**

Run:

```bash
make EMACS=/Users/jamie/opt/emacs-src/nextstep/Emacs.app/Contents/MacOS/Emacs test-unit
```

Expected: all ERT tests pass.  The new tests prove managed proxy insertion,
duplicate prevention, non-proxy isolation, unmanaged-request isolation, and
idempotent installation.

**Step 5: Commit the proxy fix**

```bash
git add lisp/magent-llm-gptel.el test/magent-test.el
git commit -m "fix: expose proxied provider HTTP errors"
```

---

### Task 3: Compile and verify the installed live path

**Files:**

- Verify: `lisp/magent-agent-info.el`
- Verify: `lisp/magent-llm-gptel.el`
- Verify: `test/magent-test.el`

**Step 1: Run clean compilation**

Run:

```bash
make clean
make EMACS=/Users/jamie/opt/emacs-src/nextstep/Emacs.app/Contents/MacOS/Emacs compile
```

Expected: all production Elisp files compile successfully.  Review all output;
do not treat filtered warnings or a prior `.elc` as proof.

**Step 2: Run the complete unit suite from the compiled checkout**

Run:

```bash
make EMACS=/Users/jamie/opt/emacs-src/nextstep/Emacs.app/Contents/MacOS/Emacs test-unit
```

Expected: all ERT tests pass with zero unexpected results.

**Step 3: Run deterministic live smoke tests**

Run:

```bash
make EMACSCLIENT="emacsclient" test-live-smoke
```

Expected: source reload, ACP request lifecycle, assistant text, tool use, and
queue cleanup smoke checks pass.

**Step 4: Reload the changed source into the running Emacs**

Create a narrow temporary helper under `/private/tmp` that only:

1. loads the absolute paths to `lisp/magent-agent-info.el` and
   `lisp/magent-llm-gptel.el`;
2. removes and reinstalls
   `magent-llm-gptel--suppress-connect-headers-a` so the live function
   definition is the active advice object;
3. clears the current Magent runtime session;
4. returns the effective model for a nil-model build agent.

Load that helper with:

```bash
emacsclient --eval '(load "/private/tmp/magent-model-proxy-live-check.el" nil t)'
```

Expected: the returned effective model is `deepseek-v4-pro`, not `nil` or
`deepseek-reasoner`.

**Step 5: Verify real non-tool and tool prompts**

Using the supported active Magent agent-shell frontend, submit in order:

```text
你好
帮我看下 emacs 里面有多少 buffer
```

Wait for each turn to reach a terminal state before submitting the next.

Expected:

- the first turn renders non-empty assistant text;
- the second turn emits an `emacs_eval` tool lifecycle and renders a non-empty
  assistant answer;
- the active agent-shell request does not remain pending.

**Step 6: Inspect live diagnostics**

Inspect the active Magent agent-shell buffer, `*magent-log*`, `*Messages*`, and
the current session ledger.

Expected:

- logged effective model is `deepseek-v4-pro`;
- no new `provider completed without assistant text`;
- no proxy CONNECT 200 masks an origin error;
- both test turns complete with visible assistant output.

**Step 7: Confirm repository state**

Run:

```bash
git status --short --branch
git log -3 --oneline
```

Expected: only intentional commits are present and the working tree is clean.
