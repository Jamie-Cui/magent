---
name: research-lit
description: Use when searching for related work in security, ZK, LLM-security, or confidential computing research. Searches arXiv cs.CR, IACR ePrint, Semantic Scholar, and top security conference proceedings. Also use for novelty checks before submitting or claiming a contribution is novel.
---

# Research Literature — Security + ZK + LLM

## Trigger
`/research-lit [topic] [--novelty] [--survey] [--sources zotero,iacr,web]`

## Overview

Security + ZK focused literature search. Covers arXiv cs.CR, IACR ePrint (critical for unpublished crypto/ZK work), Semantic Scholar with venue filters, and direct search of CCS/USENIX/S&P/NDSS/Crypto proceedings.

Adapted from ARIS `research-lit` + `novelty-check` for security/ZK research contexts.

## Constants

- `ARXIV_MAX = 20` — max arXiv results to retrieve per query
- `EPRINT_MAX = 10` — max IACR ePrint results per query (often has work months before proceedings)
- `YEARS = 2023–2026` — default date range for recent papers
- `NOVELTY_THRESHOLD`: flag as "overlap" if a found paper covers ≥2 of the same core claims

## Source Priority

| Priority | Source | When critical |
|---|---|---|
| 1 | **IACR ePrint** (`eprint.iacr.org`) | Any ZK/crypto paper — may contain months-ahead preprints |
| 2 | **arXiv cs.CR** | LLM security, system security, applied crypto |
| 3 | **arXiv cs.LG** (security angle) | zkML, verifiable inference, adversarial ML |
| 4 | **Semantic Scholar** (venue-filtered) | CCS/USENIX/S&P/NDSS/PETS/Crypto proceedings |
| 5 | **ACM DL / USENIX / IEEE** (direct) | Exact venue search when Semantic Scholar misses |

## Step 1: Parse Arguments

From `$ARGUMENTS`:
- Extract topic/keywords
- Detect `--novelty` flag → run novelty check (Step 4)
- Detect `--survey` flag → output draft Related Work section (Step 5)
- Detect `--sources X,Y` → restrict to listed sources

If no arguments: read all `.tex`, `.org`, `.md` in current directory to auto-extract topic and keywords.

## Step 2: Multi-Source Search (Parallel)

Launch all agents in parallel. Skip sources not in `--sources` filter.

**Agent A — IACR ePrint:**
Search `https://eprint.iacr.org/search?query={keywords}&title=on&abstract=on` via WebSearch or WebFetch.
Extract: ePrint ID, title, authors, year, abstract summary.
Note: ePrint often has ZK papers 6-12 months before they appear in Crypto/CCS proceedings.

**Agent B — arXiv cs.CR + cs.LG:**
Search `https://arxiv.org/search/?query={keywords}&searchtype=all&start=0` with category filter cs.CR.
Also search cs.LG with `security OR verifiable OR adversarial OR zkML` qualifier.
Extract: arXiv ID, title, abstract, date, citation count if available.

**Agent C — Semantic Scholar (venue-filtered):**
Query: `https://api.semanticscholar.org/graph/v1/paper/search?query={keywords}&fields=title,abstract,year,venue,citationCount`
Filter results to target venues only:

```
Security venues:   CCS, IEEE S&P, USENIX Security, NDSS, PETS, SOUPS
Crypto venues:     CRYPTO, EUROCRYPT, ASIACRYPT, PKC, TCC, AFRICACRYPT
Systems venues:    OSDI, SOSP, ATC, EuroSys, NSDI, IMC
ML+Security:       NeurIPS (security track), ICLR (safety track), ICML
```

**Agent D — Google Scholar (coverage check):**
WebSearch: `{keywords} site:dl.acm.org OR site:usenix.org OR site:ieee.org OR site:eprint.iacr.org`
Use this primarily to catch papers that Semantic Scholar misses.

## Step 3: Synthesize Results

Deduplicate across sources (same paper may appear in ePrint + arXiv + proceedings).
Rank by relevance to topic, then recency.

Group into categories (auto-detect from abstracts):
- Direct prior work (same problem, similar approach)
- Related systems (different approach, same problem)
- Building blocks (techniques this paper uses)
- Concurrent work (published within 6 months of today)

**Always flag:** papers from the last 6 months as "**⚠️ concurrent work**" — these must be cited.

Output table (terminal):
```
## Literature Results: {TOPIC}

### Direct Prior Work
| Title | Venue | Year | Key delta from your work |
|---|---|---|---|
...

### Related Systems
...

### Concurrent Work (⚠️ must cite)
...
```

## Step 4: Novelty Check (if --novelty)

For each claimed contribution in the paper:
1. Check if any found paper already does this
2. Flag: "Contribution X may overlap with [Paper Y] at [Venue Z]"

**ZK-specific novelty checks:**
- Search ePrint specifically for: `verifiable {model_type} inference` / `zkLLM` / `zkML {technique}`
- Check IACR ePrint for concurrent preprints (they count as prior art even if unpublished)

Score each contribution:
- `NOVEL`: no direct prior work found
- `INCREMENTAL`: prior work exists but your delta is clear
- `OVERLAP`: prior paper covers same claims — discuss differentiation or reframe contribution

Output:
```markdown
## Novelty Check Report

### Contribution 1: {claim}
Status: NOVEL / INCREMENTAL / OVERLAP
Closest prior work: [Paper] ([Venue Year])
Your delta: ...

### Contribution 2: ...
```

## Step 5: Survey Mode (if --survey)

Generate a structured Related Work section in the paper's format.

**Template for security paper:**
```latex
\section{Related Work}

\paragraph{Verifiable {X}.}
[Group 1 papers] tackle [problem] using [approach]. Unlike these works, we [delta].

\paragraph{Security of {Y}.}
[Group 2 papers] address [problem] but focus on [different aspect]. Our work differs in [delta].

\paragraph{ZK Proofs for {Z}.}
[Group 3 papers] apply ZK techniques to [domain]. We build on [which aspects] but extend [how].
```

For `.org` format, use `** Related Work` heading with `[cite:@key]` citations.

**Only write to file when user explicitly requests it.** Default: output to terminal.

## Security Research Venues Reference

| Tier | Venues | Focus |
|---|---|---|
| 1 — top applied | CCS, USENIX Security, IEEE S&P, NDSS | Applied security, systems |
| 1 — top crypto | CRYPTO, EuroCrypt, AsiaCrypt | Cryptography, ZK proofs |
| 2 | PKC, TCC, PETS, FC | Crypto, privacy, finance |
| arXiv | cs.CR (security), cs.CR+cs.LG (zkML) | Preprints |
| Preprint | IACR ePrint | Crypto preprints (high priority for ZK) |

## Output Rules

- Default: respond in terminal
- Write files only when user explicitly requests (`--save` flag or "save to file")
- For `.org` papers: use `[cite:@key]` citation format
- For `.tex` papers: use `\cite{key}` format
- Confirm target file path before writing
