---
title: Troubleshooting Guide
lang: en
alt_url: /TROUBLESHOOTING.zh.html
---

# Troubleshooting Guide

## Active Workflow Notes

If you are debugging agent lifecycle or subagent behavior, first check `docs/AGENT_JOBS.md`. Magent uses durable child-agent jobs through `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, and `close_agent`; the old one-shot `delegate` tool is not part of the current surface. Do not diagnose missing sandbox parity as a Magent bug; Codex sandbox behavior is intentionally out of scope.

## Common Issues

### GitHub Actions Failures

#### melpazoid cannot find `prompt.org`
**Problem:** The melpazoid job fails with errors like:

```text
Opening input file: No such file or directory, /workspace/pkg/prompt.org
```

**Cause:** MELPA-style packaging copies only files matched by the recipe.
Magent needs bundled runtime data (`prompt.org`, `skills/`, and
`capabilities/`) after packaging.

**Solution:** Keep `.github/workflows/melpazoid.yml` aligned with this recipe
shape:

```elisp
(magent :fetcher github :repo "Jamie-Cui/magent"
        :files (:defaults "prompt.org" "skills" "capabilities"))
```

#### package-lint reports ineffective `Package-Requires`
**Problem:** melpazoid reports:

```text
Package-Requires outside the main file have no effect.
```

**Solution:** Keep `Package-Requires` only in `magent.el` and mirror it in
`magent-pkg.el`. Secondary modules should declare normal `(require ...)`
dependencies in code, but not package headers.

#### package license is unknown
**Problem:** melpazoid reports `license unknown` for module files.

**Solution:** Ensure every Elisp source has a formal license header or an SPDX
line, for example:

```elisp
;; SPDX-License-Identifier: GPL-3.0-or-later
```

#### Live smoke test times out on GitHub Actions
**Problem:** `test.yml` fails in `Run live smoke tests` with:

```text
Magent live loop tool turn did not finish
```

**Cause:** CI runners can be slower than a local daemon when timers, tool
callbacks, and UI rendering all run in one live Emacs process.

**Solution:** Keep deterministic live smoke waits long enough for CI and avoid
hard-coding local-machine timing assumptions. Reproduce with:

```bash
emacs --daemon=magent-ci
EMACSCLIENT="emacsclient -s magent-ci" make test-live-smoke
emacsclient -s magent-ci --eval '(kill-emacs)'
```

### Installation Issues

#### "Cannot find gptel"
**Problem:** Magent requires gptel as a dependency.

**Solution:**
```elisp
;; Install gptel from MELPA
(package-install 'gptel)
```

#### Byte-compilation warnings
**Problem:** Warnings about undefined functions during compilation.

**Solution:** Ensure all dependencies are installed and in load-path:
```bash
make compile  # Auto-detects dependencies in ~/.emacs.d/elpa/
```

If dependencies live outside `~/.emacs.d/elpa/`, pass the relevant Makefile
variables explicitly, such as `GPTEL_DIR`, `TRANSIENT_DIR`, `COMPAT_DIR`, or
`YAML_DIR`.

### Runtime Issues

#### Live Emacs tests fail or hang
**Problem:** `make test-live` fails, times out, or reports an async timer error while using the real configured gptel provider.

**Live debugging playbook:**

1. Use an isolated Emacs server for Magent live tests. Do not debug against your main editing Emacs when a hang is possible.
   ```bash
   emacs --daemon=magent-live-test
   ```

2. Always point `emacsclient` or `make` at the isolated server:
   ```bash
   emacsclient -s magent-live-test --eval '(emacs-pid)'
   make EMACSCLIENT="emacsclient -s magent-live-test" test-live-smoke
   ```

3. Before every live run, force this checkout's source to load and assert that ELPA Magent was not used:
   ```bash
   emacsclient -s magent-live-test --eval \
     '(progn
        (setq debug-on-error t)
        (load-file "/path/to/magent/test/magent-live-test.el")
        (magent-live-test-reload-source)
        (list :repo-source (magent-live-test--repo-source-summary)))'
   ```
   Every path in `:repo-source` must be under the checkout, for example `/path/to/magent/magent.el`, not under `~/.emacs.d/elpa/magent/`.

4. Clear stale bytecode before batch or live verification. A warning like `Source file ... newer than byte-compiled file; using older file` means Emacs tested old code.
   ```bash
   make clean
   ```

5. Prefer async status files for real provider runs. They keep `emacsclient` responsive and preserve a compact state snapshot while the provider request continues:
   ```bash
   emacsclient -s magent-live-test --eval \
     '(progn
        (setq debug-on-error t)
        (load-file "/path/to/magent/test/magent-live-test.el")
        (magent-live-test-reload-source)
        (magent-live-test-install-trace "/tmp/magent-live-trace.el")
        (magent-live-test-run-async
         (quote magent-live-test-real-emacs-eval-tool)
         "/tmp/magent-live-tool-final.el"))'

   cat /tmp/magent-live-tool-final.el
   tail -n 80 /tmp/magent-live-trace.el
   ```

6. Check the diagnostic buffers in the isolated server while the test is running and after it finishes:
   - `*Messages*`
   - `*Backtrace*`
   - `*magent-live-test-log*`
   - `*gptel-log*`

   Treat `*gptel-log*` as sensitive. Redact API keys, bearer tokens, request headers, and provider-specific secrets before sharing or committing any excerpts.

7. If the isolated Emacs server becomes unresponsive, interrupt or kill only that server. Do not kill the main Emacs process.
   ```bash
   emacsclient -s magent-live-test --eval '(kill-emacs 0)'
   ```
   If the client cannot connect, identify the `emacs --daemon=magent-live-test` PID and kill that PID only.

**Diagnosis:**
1. Enable backtraces in the live Emacs session before reproducing:
   ```elisp
   (setq debug-on-error t)
   ```
2. Re-run the failing live test from `emacsclient` or `make test-live`.
3. While the run is active and immediately after it finishes, inspect:
   - `*Messages*` for timer, process filter, and byte-code errors
   - `*Backtrace*` when `debug-on-error` opens one
   - `*magent-log*` or the test-local `*magent-live-test-log*`
   - `*gptel-log*` for provider/request failures; redact API keys or headers before sharing
4. For a single real tool test, reload the live suite and run:
   ```elisp
   (progn
     (load-file "/path/to/magent/test/magent-live-test.el")
     (setq debug-on-error t)
     (magent-live-test-run 'magent-live-test-real-emacs-eval-tool))
   ```

**Solution:**
- Fix the first concrete error in `*Backtrace*` or `*Messages*`, then re-run the single failing live test before running the whole live suite.
- Treat `*gptel-log*` as sensitive diagnostic material; summarize it or share only redacted snippets.

**Failure signatures from prior live debugging:**

- If a tool continuation hangs after the first tool result, inspect `/tmp/magent-live-trace.el`. A `gptel-curl-get-args :event enter` with no matching `:event leave` usually means gptel hit a serialization error before curl started. Check tool-call names, tool args, and assistant `tool_calls` history for Lisp symbols or non-JSON-safe values.
- If the first provider request emits a tool call but the second request never starts, check whether Magent recorded the tool result in the session and rebuilt the continuation prompt via `magent-agent-loop-request-for-current-session`.
- If the continuation starts but the UI still gets an empty final answer after tool output, check for the `retrying empty final response after tool output` log line. Magent should attempt exactly one no-tool final-response retry; reasoning-only completions should remain empty assistant text rather than being displayed as the answer.
- If the second request completes but the UI omits the final assistant text, remember that tool-enabled requests may be non-streaming. A non-streaming string callback can be the final completion, not a text delta; the UI must render the completion text that was not already streamed before calling `magent-ui-finish-streaming-fontify`.
- If a live smoke test passes in direct `emacsclient` but `make test-live-smoke` fails, confirm `EMACSCLIENT="emacsclient -s magent-live-test"` is set. The Makefile default may target the main Emacs server.
- If a test unexpectedly loads an older Magent, check `load-history`, `load-path` ordering, and `.elc` files. `test/magent-live-test.el` uses source loading with `nosuffix`; keep that behavior when adding files to the live reload list.

**Known-good verification sequence after fixing live gptel/tool bugs:**

```bash
make clean
make test-unit
make compile
make clean
make EMACSCLIENT="emacsclient -s magent-live-test" test-live-smoke
```

Then run both real async diagnostics in the isolated daemon:

```elisp
(magent-live-test-run-async 'magent-live-test-real-simple-prompt
                            "/tmp/magent-live-simple-final.el")
(magent-live-test-run-async 'magent-live-test-real-emacs-eval-tool
                            "/tmp/magent-live-tool-final.el")
```

Expected final status files include `:status passed`, `:repo-source` paths under the checkout, and for the tool test a tool state like `(:name "emacs_eval" :result "42")` plus final assistant text `MAGENT_TOOL_OK=42`.

#### "No response from LLM"
**Problem:** Request hangs or times out.

**Diagnosis:**
1. Check `*magent-log*` buffer: open the transient menu with `C-c m ?` or `?`, then press `l l`
2. Verify gptel configuration:
   ```elisp
   gptel-model      ; Should show your model
   gptel-api-key    ; Should show your key
   ```
3. Test gptel directly: `M-x gptel`

**Solution:**
- Increase timeout: `(setq magent-request-timeout 300)`
- Check network/proxy settings
- Verify API key is valid

#### "Tool execution failed"
**Problem:** Tool calls return errors.

**Diagnosis:**
1. Check `*magent-log*` for error details
2. Verify tool is enabled: `magent-enable-tools`
3. Check permissions: `M-x magent-show-current-agent`

**Solution:**
- For bash: Ensure command is valid in your shell
- For emacs_eval: Check for syntax errors
- For grep: Verify ripgrep is installed: `which rg`

#### Child-agent behavior is confusing
**Problem:** A child-agent task does not behave like a durable job that can be messaged, waited on, listed, or closed.

**Diagnosis:**
1. Check whether the code path uses the lifecycle tools: `spawn_agent`, `send_agent_message`, `wait_agent`, `list_agents`, and `close_agent`.
2. Review `docs/AGENT_JOBS.md` for the child-agent/job lifecycle contract.
3. Inspect compact `Agent ...` rows in `*magent*`, or run `M-x magent-show-agent-transcript` / transient `S j` to view the persisted child transcript.
4. Check `*magent-log*` for nested request or tool-call errors.
5. After resume, confirm the parent session still has the expected job metadata in `agent-jobs`.

**Solution:**
- Add or update tests around job status, transcript/result storage, parent/child session links, and resume restoration.
- Do not add sandbox-specific checks as part of this workflow fix.

#### "Permission denied" errors
**Problem:** Tool execution blocked by permissions.

**Diagnosis:**
Check agent permissions: `M-x magent-show-current-agent`

**Solution:**
- Temporarily bypass: `M-x magent-toggle-bypass-permission`
- Or customize: `(setq magent-bypass-permission t)`
- Or switch agent from the transient menu with `A A` and select one with appropriate permissions

#### Session not saving
**Problem:** Session state lost between Emacs restarts.

**Diagnosis:**
Check session directory exists and is writable:
```elisp
magent-session-directory  ; Default: ~/.emacs.d/magent/sessions/
```

**Solution:**
```elisp
;; Ensure directory exists
(make-directory magent-session-directory t)
```

### UI Issues

#### Output buffer not updating
**Problem:** Streaming appears stuck.

**Diagnosis:**
1. Check if request is active: Look for `[busy]` in the modeline
2. Check `*Messages*` buffer for errors
3. Press `C-c C-c` in the output buffer and confirm the interrupt

**Solution:**
- Clear from the transient menu with `c`, then retry with `C-c m p`
- Restart Emacs if issue persists

#### Org-mode folding issues
**Problem:** Sections won't fold/unfold.

**Solution:**
- Use `TAB` on section headers
- Use `S-TAB` to toggle all sections
- Ensure you're in the output buffer, not a code buffer

### Performance Issues

#### Slow streaming
**Problem:** Response appears character-by-character.

**Solution:**
```elisp
;; Increase batch delay
(setq magent-ui-batch-insert-delay 0.1)

;; Lower async fontification threshold
(setq magent-ui-fontify-threshold 200)
```

#### High memory usage
**Problem:** Emacs memory grows over time.

**Solution:**
```elisp
;; Reduce history size
(setq magent-max-history 50)

;; Clear old sessions periodically
(magent-clear-session)
```

## Diagnostic Commands

### Self-Check
```elisp
M-x magent-doctor
```
Runs comprehensive self-check and reports issues.

### View Logs
```elisp
M-x magent-show-log  ; or open transient with C-c m ? / ? and press l l
```
Shows API request/response log.

### Check Configuration
```elisp
M-x describe-variable RET magent-enable-tools
M-x describe-variable RET gptel-model
M-x describe-variable RET gptel-api-key
```

## Getting Help

If issues persist:
1. Run `M-x magent-doctor` and save output
2. Check `*magent-log*` and `*Messages*` buffers
3. Open GitHub issue with reproduction steps
4. Include Emacs version: `M-x emacs-version`
