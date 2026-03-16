---
name: research-review
description: Use when a security, ZK, or LLM-security paper draft needs iterative review and improvement. Runs adversarial cross-model review loops targeting security-venue criteria (CCS/USENIX/S&P/NDSS/Crypto). Replaces GPU experiments with proof sketches, threat model analysis, and literature gap fixing.
---

# Research Review Loop — Security/LLM/ZK Paper

## Trigger
`/research-review [rounds=4] [human-checkpoint=false]`

## Overview

Adapts ARIS auto-review-loop for **security + ZK + LLM security** research.

Claude Code reads and improves the paper; a **separate adversarial reviewer** (dispatched as a native subagent, or via an alternative LLM backend) critiques from the perspective of a senior security conference PC member.

Core principle: *the same model reviewing its own patterns creates blind spots*. The adversarial reviewer simulates a skeptical CCS/USENIX PC member who does not give benefit of the doubt.

## Constants

- `MAX_ROUNDS = 4`
- `POSITIVE_THRESHOLD`: score ≥ 7/10 AND verdict contains "accept" / "ready for submission"
- `REVIEW_DOC`: `AUTO_REVIEW.md` in project root (cumulative log, appended each round)
- `REVIEW_STATE`: `REVIEW_STATE.json` (for context-compaction recovery)
- `HUMAN_CHECKPOINT`: pause after Phase B if `true` (default: `false` — autonomous)
- `TARGET_VENUE`: auto-detect from paper content, or accept argument override
  - Detected keywords → venue: `zkSNARK/ZKP/verifiable` → `CRYPTO/CCS`, `TEE/SGX/TDX/CoCo` → `CCS/USENIX`, `LLM/jailbreak/prompt injection` → `CCS/USENIX/S&P`, `MCP/agent security` → `CCS/USENIX`

## Reviewer Setup

**Option A — magent native (`emacs_eval` tool available):**

Use `emacs_eval` to call `gptel-request` using magent's already-configured backend. Do NOT specify `:backend` or `:model` — omitting them lets gptel use the global `gptel-backend` and `gptel-model`, which are exactly the values magent is already using successfully.

```elisp
;; Write review request to a temp file using the same gptel backend magent uses
(let* ((output-file "/tmp/magent-review-output.md")
       (reviewer-prompt "...paper content...")
       (system-prompt "You are a senior PC member at {TARGET_VENUE}..."))
  (gptel-request reviewer-prompt
    :system system-prompt
    :callback (lambda (res _info)
                (with-temp-file output-file
                  (insert (or res "ERROR: no response"))))))
```

After calling, poll until `/tmp/magent-review-output.md` is written (use `emacs_eval` to check `(file-exists-p "/tmp/magent-review-output.md")` and read it with the `read_file` tool), then read its content for the reviewer's response.

**Option B — Dashscope API via curl (last resort):**

Use only when `emacs_eval` is not available.

```bash
# Locate API key
if [ -f ~/.dashscope_api_key ]; then
  DASHSCOPE_KEY=$(cat ~/.dashscope_api_key)
elif [ -n "$DASHSCOPE_API_KEY" ]; then
  DASHSCOPE_KEY=$DASHSCOPE_API_KEY
fi

if [ -z "$DASHSCOPE_KEY" ]; then
  echo "ERROR: No Dashscope key found. Set DASHSCOPE_API_KEY or create ~/.dashscope_api_key"
  exit 1
fi

# Call Qwen-max (OpenAI-compatible endpoint)
curl -s https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions \
  -H "Authorization: Bearer $DASHSCOPE_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen-max-latest",
    "messages": [
      {"role": "system", "content": "You are a senior PC member at {TARGET_VENUE}..."},
      {"role": "user",   "content": "Review this paper: ..."}
    ]
  }'
```

**Decision table:**

| Environment | `emacs_eval` available? | → Use |
|---|---|---|
| magent | yes | **Option A** (uses magent's configured backend — no API key needed) |
| Other / API key present | no | **Option B** |
| No tools, no key | no | Error — prompt user to configure |

## Workflow

### Initialization

1. Check `REVIEW_STATE.json`:
   - Does not exist → fresh start
   - `status: "completed"` → fresh start
   - `status: "in_progress"` AND timestamp within 24h → **resume** from saved round
   - `status: "in_progress"` AND timestamp > 24h old → fresh start (stale, delete file)
2. Read all `.tex`, `.org`, `.md` files in the current directory
3. Read `AUTO_REVIEW.md` if exists (prior round context)
4. Detect domain and `TARGET_VENUE`

### Phase A — Self-Assessment (each round)

Evaluate the paper against the security-paper checklist:

**Threat model** (20 pts):
- [ ] Adversary capabilities precisely scoped (what the attacker can/cannot do)
- [ ] TCB (Trusted Computing Base) explicitly defined
- [ ] Attack surface enumerated
- [ ] Out-of-scope threats acknowledged

**Security claims** (25 pts):
- [ ] Claims are formally stated (game-based or simulation-based definition), OR clearly scoped as informal
- [ ] For ZK papers: completeness / soundness / zero-knowledge all proved or sketched
- [ ] Security reductions use standard assumptions (DDH, q-SDH, ROM, etc.)
- [ ] No circular reasoning in security arguments

**Novelty vs. top venues** (20 pts):
- [ ] Delta over state-of-art is clearly articulated
- [ ] All directly related work from last 3 years cited (CCS/USENIX/S&P/NDSS/Crypto/EuroCrypt + IACR ePrint)
- [ ] No overclaiming ("first" / "only" without qualification)

**Evaluation** (20 pts):
- [ ] Concrete performance numbers (proving time, verification time, overhead vs. plaintext)
- [ ] Comparison vs. alternative approaches (TEE, other ZK systems, prior work)
- [ ] For ZK: amortized cost reported (not just single-instance)

**Presentation** (15 pts):
- [ ] Threat model section present before the design
- [ ] Security analysis section present after design
- [ ] Proof sketches/theorems before formal proofs (for readability)

Produce initial score and `WEAKNESSES` list.

### Phase B — Cross-Model Adversarial Review

Call the reviewer per Reviewer Setup (Option A → B in priority order).

Reviewer prompt template (fill in paper content):
```
You are a senior PC member at {TARGET_VENUE} with expertise in {DOMAIN}.
You are known for finding subtle flaws that authors overlooked.
Do NOT give benefit of the doubt. If something is unclear, treat it as a weakness.

Review the following paper and provide:
1. Score: X/10 (1=strong reject, 5=borderline, 7=weak accept, 9=strong accept)
2. Top 5 critical weaknesses (ranked by severity)
3. For each weakness: minimum fix needed to address it
4. Verdict: REJECT / BORDERLINE / ACCEPT
5. Missing related work (papers you would expect to see cited)

Paper:
{PAPER_CONTENT}
```

Save the **full raw response verbatim** in `AUTO_REVIEW.md` inside a `<details>` block.

**If HUMAN_CHECKPOINT=true:** Present score + weaknesses to user, wait for instruction (go / skip N / custom instruction / stop).

**Stop condition:** If score ≥ `POSITIVE_THRESHOLD` AND round ≥ 2, terminate loop early.

### Phase C — Targeted Fixes

For each weakness from Phase B, apply the appropriate fix type:

| Weakness Type | Fix Strategy |
|---|---|
| Threat model incomplete | Revise threat model section; add missing adversary capabilities or out-of-scope acknowledgments |
| Missing security proof | Add proof sketch (informal) or full game-based proof; at minimum state the security theorem formally |
| Missing related work | Use `research-lit` skill to find papers; add ≥3 missing citations with one-sentence differentiation |
| Overclaiming novelty | Qualify claims: "to our knowledge" / "in the setting of" / "for the specific case of" |
| Weak evaluation | Add concrete numbers; if not measurable now, add a "complexity analysis" or "theoretical comparison" subsection |
| Circuit/proof gap (ZK) | Address non-linear op handling; explicitly state lookup tables / range checks used |
| Unclear TCB | Add explicit TCB table: what's trusted, what's not, why |

**Write all changes to the paper files.** Do not simulate fixes — actually edit the document.

### Phase D — Document Round

Append to `AUTO_REVIEW.md`:
```markdown
## Round N — {TIMESTAMP}

**Score:** X/10  |  **Verdict:** BORDERLINE/ACCEPT/REJECT
**Target venue:** {TARGET_VENUE}

### Weaknesses Identified
1. ...

### Fixes Applied
- ...

### Reviewer Raw Response
<details>
<summary>Full reviewer output</summary>
{VERBATIM_REVIEWER_RESPONSE}
</details>
```

Write `REVIEW_STATE.json`:
```json
{
  "round": N,
  "status": "in_progress",
  "last_score": X.X,
  "last_verdict": "...",
  "target_venue": "...",
  "timestamp": "{ISO8601}"
}
```

### Termination

When positive threshold reached OR `MAX_ROUNDS` hit:
1. Update `REVIEW_STATE.json` with `"status": "completed"`
2. Write `REVIEW_SUMMARY.md`:
   - Score progression table
   - Key improvements made per round
   - Remaining open issues (honest list)
   - Suggested venue + rationale
   - Next steps before submission

## Security Venue Standards Reference

| Venue | Tier | Focus | Page limit |
|---|---|---|---|
| CCS, USENIX Security, IEEE S&P, NDSS | 1 | Applied security | 12-18pp |
| CRYPTO, EuroCrypt, AsiaCrypt | 1 | Cryptography/ZK | 30-40pp |
| PKC, TCC | 2 | Cryptography | 30pp |
| PETS | 2 | Privacy | 20pp |

## ZK Paper Checklist (extra, applied in Phase A when domain=zk)

- [ ] Completeness: honest prover always convinces honest verifier
- [ ] Soundness / Knowledge Soundness: no malicious prover can prove false statement (with what probability?)
- [ ] Zero-Knowledge: verifier learns nothing beyond truth of statement
- [ ] Succinctness: proof size and verification time stated (O(1) / O(log n) / O(n)?)
- [ ] Non-linear operations: how are Softmax / LayerNorm / GELU / RELU handled? (lookup tables? sumcheck? approximation?)
- [ ] Setup: trusted / transparent / updateable? What are the assumptions?
- [ ] Prover time complexity stated with concrete numbers for target model size
- [ ] Comparison table: proving time / proof size / verification time vs. prior ZK systems

## Output Files

- `AUTO_REVIEW.md` — cumulative log of all rounds
- `REVIEW_STATE.json` — recovery state
- `REVIEW_SUMMARY.md` — final summary (written at termination)
