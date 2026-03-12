---
name: research-add
description: Add items (research objects) or fields to an existing research outline.
---

# Research Add - Supplement Items or Fields

## Trigger
`/research-add`

## Workflow

### Step 1: Auto-locate Files
Find `*/outline.yaml` and `*/fields.yaml` in current working directory, auto-read both.

### Step 2: Ask What to Supplement
Use request_user_input to ask user:
- **A. Add items** (research objects to outline.yaml)
- **B. Add fields** (field definitions to fields.yaml)
- **C. Both**

---

## A. Add Items

### A1: Get Supplement Sources in Parallel
Simultaneously:
- Ask user: What items to supplement? Any specific names?
- Ask if web search needed: Launch agent to search for more items?

### A2: Merge and Update
- Append new items to outline.yaml
- Display to user for confirmation
- Avoid duplicates
- Save updated outline

---

## B. Add Fields

### B1: Get Supplement Source
Ask user to choose:
- **User direct input**: User provides field names and descriptions
- **Web Search**: Launch agent to search common fields in this domain

### B2: Display and Confirm
- Display suggested new fields list
- User confirms which fields to add
- User specifies field category and detail_level

### B3: Save Update
Append confirmed fields to fields.yaml, save file.

---

## Output
- Updated `{topic}/outline.yaml` (if items added)
- Updated `{topic}/fields.yaml` (if fields added)
