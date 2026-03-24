# Troubleshooting Guide

## Common Issues

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

### Runtime Issues

#### "No response from LLM"
**Problem:** Request hangs or times out.

**Diagnosis:**
1. Check `*magent-log*` buffer: `C-c m l`
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

#### "Permission denied" errors
**Problem:** Tool execution blocked by permissions.

**Diagnosis:**
Check agent permissions: `M-x magent-show-current-agent`

**Solution:**
- Temporarily bypass: `M-x magent-toggle-by-pass-permission`
- Or customize: `(setq magent-by-pass-permission t)`
- Or switch agent: `C-c m A` and select one with appropriate permissions

#### Session not saving
**Problem:** Session state lost between Emacs restarts.

**Diagnosis:**
Check session directory exists and is writable:
```elisp
magent-session-directory  ; Default: ~/.emacs.d/magent-sessions/
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
1. Check if request is active: Look for spinner in modeline
2. Check `*Messages*` buffer for errors
3. Interrupt with `C-g` in output buffer

**Solution:**
- Clear and retry: `C-c m c` then `C-c m p`
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
M-x magent-show-log  ; or C-c m l
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
