;;; magent-skill-creator.el --- Built-in skill creator skill  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Built-in instruction skill for creating and improving magent skills.
;; When active, this skill's instructions are injected into the system prompt
;; to guide the AI through the skill creation workflow.

;;; Code:

;;; Skill metadata

(defconst magent-skill-creator--description
  "Create new magent skills, modify and improve existing skills. Use when users want to create a skill from scratch, edit an existing skill, or improve a skill's description. Guides through writing SKILL.md, placing it in the right directory, testing via emacs_eval, and iterating."
  "Description of the built-in skill-creator skill.")

;;; Skill prompt

(defconst magent-skill-creator--prompt
  "# Skill Creator

A skill for creating new magent skills and iteratively improving them.

Your job: figure out where the user is in the process and help them progress
through these stages: understand intent → write SKILL.md → test in Emacs → iterate.

---

## Skill File Format

```
---
name: skill-name
description: When to trigger and what this skill does
type: instruction        # 'instruction' or 'tool'
tools: bash, read        # optional: tools this skill needs
---

Markdown body: instructions for the AI...
```

## Skill Types

**instruction**: Markdown body is injected into the system prompt every request.
Use for workflow guidance, coding standards, domain knowledge.
Keep under 200 lines to avoid bloating the system prompt.

**tool**: Invoked explicitly via `skill_invoke` with named operations.
Requires a companion `.el` file defining `magent-skill-<name>-invoke`.
Use for structured operations with discrete inputs/outputs.

## Skill Locations

- User global: `~/.emacs.d/magent-skills/<name>/SKILL.md`
- Project-local: `.magent/skills/<name>/SKILL.md`

Place each skill in its own subdirectory named after the skill.

---

## Creating a Skill

### 1. Capture Intent

Ask:
- What should the skill enable magent to do?
- When should it be used? (what user phrases, what context)
- What type fits: instruction (passive guidance) or tool (active operations)?

### 2. Write the Skill

Key principles:
- **description**: This is the primary trigger mechanism. Be specific about *when*
  to use this skill vs. handling the task directly. Include example phrasings.
  Lean toward being slightly \"pushy\" — Claude tends to undertrigger skills.
- **body**: Use imperative form. Explain the *why* behind steps, not just the *what*.
  Keep instruction-type skills concise. If it grows large, split into sections
  with references to separate files.

Write the SKILL.md using `write_file`:
```
~/.emacs.d/magent-skills/<name>/SKILL.md
```

### 3. Test in Emacs

After writing, reload skills:

```elisp
(magent-reload-skills)
```

Use the `emacs_eval` skill (eval-expression operation) to reload without leaving
the chat. Then test: start a new session or prompt magent with a request that
should trigger the skill. Check `M-x magent-list-skills` to confirm registration.

For tool-type skills, test each operation:
```elisp
(magent-skills-invoke \"skill-name\" \"operation\" (list \"arg\")
                      (lambda (r) (message \"%s\" r)))
```

### 4. Iterate

- If the AI ignores the skill: strengthen the description, add trigger phrases
- If instructions are misunderstood: clarify the why, add examples
- If the skill is too verbose: trim sections that aren't pulling their weight
- If tool operations misbehave: check the companion `.el` file

---

## Tool-type Skill Companion File

For tool-type skills, create `.magent/skills/<name>/<name>.el`:

```elisp
;;; -*- lexical-binding: t; -*-
(defun magent-skill-<name>-invoke (operation args callback)
  \"Invoke <name> skill OPERATION with ARGS.
CALLBACK is called with result string.\"
  (run-at-time
   0 nil
   (lambda ()
     (funcall callback
              (pcase operation
                (\"op-name\" (format \"result: %s\" (car args)))
                (_ (format \"Unknown operation: %s\" operation)))))))
```

The companion file is loaded automatically when the skill file is loaded.

---

## Description Writing Guide

Good description (specific, mentions trigger contexts):
> Create new magent skills or modify existing ones. Use when user says
> \"create a skill\", \"add a skill for X\", \"write a SKILL.md\", or asks
> how to extend magent with custom behavior.

Poor description (too generic):
> Helps with skill creation.

The description field is the only thing Claude sees when deciding whether to
use a skill — make it count."
  "Prompt for the built-in skill-creator skill.")

(provide 'magent-skill-creator)
;;; magent-skill-creator.el ends here
