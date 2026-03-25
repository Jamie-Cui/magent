# Live Emacs Debug Checklist

Use this file when the bug is runtime-only, intermittent, UI-related, or more trustworthy in live Emacs than in batch mode.

## Core Rule

Treat live Emacs as the primary truth source. A passing batch test does not overrule a live Emacs failure that you can still reproduce.

## Standard Sequence

1. Confirm the runtime target.
   Identify whether an Emacs server is available and whether you can inspect live buffers and evaluate Elisp directly.

2. Inspect the three main buffers first.
   Check `*magent*`, `*magent-log*`, and `*Messages*` before editing code. For vague bug reports, this is the default first pass.

3. Confirm environment facts.
   Capture `emacs-version`, relevant dependency paths or versions such as `gptel`, and whether the edited files are currently loaded.

4. Eliminate stale state.
   Reload the smallest changed `.el` files, clear session state when needed, and distinguish real logic failures from stale runtime state.

5. Reproduce with the smallest useful prompt.
   Start from the shortest prompt that still demonstrates the bug, then expand only if necessary.

6. Re-check the three main buffers after each attempt.
   Look for render failures, permission prompts, tool-call failures, stale overlays, queue stalls, and unexpected Lisp errors.

7. Verify the fix live before batch.
   After the code change, repeat the live repro first. Then run targeted compile or test validation.

## Default Repro Prompts

Use these when you need a known-good sanity ladder and the repo-local instructions do not suggest a better one.

- Non-tool sanity: `你好`
- Tool-use sanity: `帮我看下 emacs 里面有多少 buffer`
- Multi-step sanity: `帮我在 emacs 里面打开 magent 的 magit buffer`

## Stale-State Checklist

Check these before concluding that the code path itself is wrong:

- The edited `.el` file is saved but not reloaded.
- An older `.elc` or previously loaded definition is still active.
- `magent-clear-session` or an equivalent reset is needed.
- The `*magent*` buffer still contains stale overlays, folded state, or old request-generation artifacts.
- A queue or in-flight request from an earlier run is still affecting the result.
- A buffer-local setting differs from the `default-value` path the code actually uses.

## Useful Batch Fallbacks

Use these only after deciding that batch evidence helps:

- `make compile`
- `make test`
- A single targeted ERT run by regexp
- A narrow `emacs -Q --batch` reproduction for one file or one suite

## Reporting Expectations

When reporting results, keep the final structure fixed:

- `症状与复现`
- `根因`
- `改动与验证`
- `剩余风险/下一步`
