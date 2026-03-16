---
name: research-explore
description: Investigate complex questions by decomposing into sub-tasks, delegating to
  the explore agent in parallel, and synthesizing findings into a structured report.
  Use for codebase architecture questions, cross-file analysis, and general Q&A that
  requires multi-source investigation.
---

# Research Explore - Deep Investigation

## Trigger
`/research-explore [question]`

## Workflow

### Step 1: DECOMPOSE

Break the research question into specific, answerable sub-questions. Identify which require:
- Codebase exploration (file finding, code tracing, architecture analysis)
- Targeted lookups (specific file reads, grep searches)
- External research (docs, API references, web)

Explain your research plan before starting.

### Step 2: INVESTIGATE

For each sub-question, choose the most efficient approach:

- `delegate` to `explore` for codebase search, file finding, code tracing, and architecture questions. Specify the desired thoroughness level: "quick", "medium", or "very thorough".
- `grep` / `glob` / `read_file` directly for quick targeted lookups when the file path or pattern is already known.
- `web_search` for external lookups (docs, API references, recent information).

Issue multiple `delegate` calls in parallel when sub-tasks are independent. Each delegate prompt must be self-contained — subagents share no context with each other or the parent.

### Step 3: SYNTHESIZE

Aggregate all findings into a structured report:
- Use markdown headings to organize by topic or sub-question
- Use bullet points for key findings
- Use code blocks for file excerpts, function signatures, or examples
- Cite specific files, line numbers, and URLs
- If a sub-task returns insufficient information, refine and retry before concluding

## Constraints

- Read-only: do not create or modify files
- Your output is the research report itself
