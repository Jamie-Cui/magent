---
name: research
description: Document-aware research assistant. Reads all document files (.tex, .org, .md, .txt) in the current directory and helps with research tasks — finding related work, analyzing gaps, drafting sections, etc. Works for LaTeX papers, org documents, plain text, or any mix. Security/ZK/LLM-security aware.
---

# Research Assistant Skill

## Trigger
`/research [optional task description]`

## Step 1: Document Discovery

Use the Glob tool to recursively find all document files in the current working directory.

Search for these patterns (run all in parallel):
- `**/*.tex`
- `**/*.org`
- `**/*.md`
- `**/*.txt`

From the results, exclude any paths containing these segments:
- `.git/`
- `_build/`
- `auto/`
- `node_modules/`
- `.elpa/`
- `elpa/`

If no files remain after filtering: output the following and stop:
> "No document files found in the current directory. Please confirm your working directory contains .tex, .org, .md, or .txt files."

## Step 2: Read All Documents

Use the Read tool to read every file discovered in Step 1. Read them in parallel where possible.

If the total file count exceeds 20, or any single file is likely very large (e.g., a full thesis or book), ask the user which files are most relevant before reading all of them. Prefer files at the root of the working directory first.

Do not summarize yet — load the full content into context.

## Step 3: Understand the Documents

From the loaded content, extract and hold in mind:
- Title or topic of each document (if multiple unrelated documents are found, treat each separately and note this in the overview)
- Core research question or thesis (if present)
- Main sections or structure
- Key terminology, methods, and concepts
- **Research domain** — detect if the paper is in one or more of these areas:
  - `security`: LLM security, jailbreak, prompt injection, backdoor, adversarial attacks, threat modeling
  - `zk`: ZK proofs, SNARK, folding schemes, verifiable computation, zkML, zkLLM
  - `cc`: Confidential Computing, TEE, SGX/TDX/SEV/TrustZone, remote attestation, CoCo
  - `mcp`: Model Context Protocol, tool call security, MCP servers
  - `llm`: LLM inference, agent systems, RAG, fine-tuning

No need to output this yet — it is internal context for the following steps.

## Step 4: Mode Branch

### If the user provided a task argument:

Execute the task immediately using the document content as context. Be specific and grounded in the actual paper — reference sections, quote relevant passages, and tailor your response to the document's topic and stage.

When the task requires external literature or recent information, use the WebSearch tool to find relevant papers or sources, then synthesize the results together with the document content.

Example tasks you should handle well:
- `find related work on [topic]` → search web + synthesize relevant literature in context of the paper
- `analyze research gaps` → identify what the paper leaves unaddressed based on its own framing
- `draft an introduction` → write a LaTeX/org/markdown introduction grounded in the paper's content
- `suggest experiments` → propose experiments consistent with the paper's methodology
- `improve the abstract` → rewrite or critique the existing abstract

**Security/ZK/LLM-security specific tasks** (when domain detected in Step 3):
- `check threat model` → evaluate adversary model completeness, TCB scope, assumptions
- `find related work` → search arXiv cs.CR + IACR ePrint + security venues (CCS/USENIX/S&P/NDSS/Crypto). Use the `research-lit` skill for deep multi-source search.
- `check ZK proof` → evaluate completeness/soundness/ZK properties, check if non-linear ops are handled, check amortized overhead
- `check novelty` → compare contributions against recent security venue papers (2023–2026)
- `suggest evaluation` → propose security-appropriate baselines (TEE alternative, comparison vs. prior ZK system, overhead vs. plaintext baseline)
- `auto review` → trigger `research-review` skill for iterative adversarial review loop

### If no argument was provided:

Output a brief document overview in this format:

```
## Document Overview

**Topic:** [inferred title or topic; if multiple unrelated documents, list each separately]
**Type:** [e.g., research paper, technical report, notes]
**Domain:** [security / ZK / CC / MCP / LLM / other — from Step 3 detection]
**Structure:** [list of main sections/files, one per line]
**Core question:** [one sentence summary of the research question or main argument]

## What can I help with?

- Find and summarize related work [security: includes IACR ePrint + CCS/USENIX/S&P/NDSS]
- Identify research gaps or missing citations
- Draft or improve a specific section
- Suggest experiments or evaluation strategies [security: threat model, ZK soundness, overhead vs. alternatives]
- Critique the argument or methodology
- Search for references on a specific topic
- Run iterative review loop (/research-review)

What would you like to work on?
```

Then wait for the user's input. All subsequent conversation in this session uses the document content as context.

## Output Rules

- Default: respond conversationally in the terminal
- Write to file only when the user explicitly requests it (e.g., "save this to notes.md" or "add this to the paper")
- When writing to file, confirm the path with the user before writing; if the user declines or does not confirm, do not write the file
