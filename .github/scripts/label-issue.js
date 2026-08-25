/* SPDX-License-Identifier: GPL-3.0-or-later */

const LABEL_DEFINITIONS = {
  rfc: {
    color: "5319e7",
    description: "Architecture request for comments",
  },
  doc: {
    color: "0075ca",
    description: "Documentation changes",
  },
  runtime: {
    color: "c5def5",
    description: "Runtime or agent loop",
  },
  gptel: {
    color: "c5def5",
    description: "gptel provider transport",
  },
  tools: {
    color: "c5def5",
    description: "Tools, permissions, or approvals",
  },
  agents: {
    color: "c5def5",
    description: "Child-agent lifecycle and jobs",
  },
  actions: {
    color: "c5def5",
    description: "Magent Actions, not GitHub Actions",
  },
  sessions: {
    color: "c5def5",
    description: "Sessions, ledger, or persistence",
  },
  frontend: {
    color: "c5def5",
    description: "ACP or agent-shell frontend",
  },
  skills: {
    color: "c5def5",
    description: "Skills or automatic capabilities",
  },
  packaging: {
    color: "c5def5",
    description: "Packaging or continuous integration",
  },
};

const COMPONENT_LABELS = [
  "runtime",
  "gptel",
  "tools",
  "agents",
  "actions",
  "sessions",
  "frontend",
  "skills",
  "packaging",
];

const BUG_AREA_LABELS = {
  "Runtime or agent loop": "runtime",
  "gptel transport": "gptel",
  "Tools, permissions, or approvals": "tools",
  "Child-agent jobs": "agents",
  Actions: "actions",
  "Sessions, ledger, or persistence": "sessions",
  "ACP or agent-shell": "frontend",
  "Skills or capabilities": "skills",
  "Packaging or CI": "packaging",
  Documentation: "doc",
};

const PROPOSAL_AREA_LABELS = {
  "gptel provider transport": "gptel",
  "ACP or agent-shell frontend": "frontend",
  "Runtime or agent loop": "runtime",
  Actions: "actions",
  "Tools, permissions, or approvals": "tools",
  "Child-agent lifecycle": "agents",
  "Session schema, ledger, or persistence": "sessions",
  "Skills or capabilities": "skills",
  "Packaging or runtime data": "packaging",
};

function issueFormSections(body) {
  const text = body || "";
  const matches = [...text.matchAll(/^### ([^\r\n]+)\r?\n/gm)];
  const sections = new Map();

  for (let index = 0; index < matches.length; index += 1) {
    const match = matches[index];
    const start = match.index + match[0].length;
    const end = matches[index + 1]?.index ?? text.length;
    sections.set(match[1].trim(), text.slice(start, end).trim());
  }

  return sections;
}

function addMappedLabels(desired, section, mapping) {
  for (const [option, label] of Object.entries(mapping)) {
    if (section.includes(option)) {
      desired.add(label);
    }
  }
}

function labelPlan(body) {
  const sections = issueFormSections(body);
  const isBugForm = sections.has("Suspected area");
  const isProposalForm =
    sections.has("Proposal type") && sections.has("Architectural impact");

  if (!isBugForm && !isProposalForm) {
    return null;
  }

  const desired = new Set();
  const managed = new Set(COMPONENT_LABELS);

  if (isBugForm) {
    managed.add("doc");
    addMappedLabels(
      desired,
      sections.get("Suspected area"),
      BUG_AREA_LABELS,
    );
  }

  if (isProposalForm) {
    managed.add("rfc");
    if (sections.get("Proposal type").includes("Architecture RFC")) {
      desired.add("rfc");
    }
    addMappedLabels(
      desired,
      sections.get("Architectural impact"),
      PROPOSAL_AREA_LABELS,
    );
  }

  return {desired, managed};
}

async function ensureLabels(github, context) {
  const existing = new Set(
    (
      await github.paginate(github.rest.issues.listLabelsForRepo, {
        ...context.repo,
        per_page: 100,
      })
    ).map((label) => label.name),
  );

  if (existing.has("documentation") && !existing.has("doc")) {
    await github.rest.issues.updateLabel({
      ...context.repo,
      name: "documentation",
      new_name: "doc",
      ...LABEL_DEFINITIONS.doc,
    });
    existing.delete("documentation");
    existing.add("doc");
  }

  for (const [name, definition] of Object.entries(LABEL_DEFINITIONS)) {
    if (!existing.has(name)) {
      await github.rest.issues.createLabel({
        ...context.repo,
        name,
        ...definition,
      });
    }
  }
}

async function labelIssue({github, context, core}) {
  const issue = context.payload.issue;
  const plan = labelPlan(issue.body || "");

  if (!plan) {
    core.info("Skipping an issue that was not created from a known form");
    return;
  }

  await ensureLabels(github, context);

  const current = new Set(
    issue.labels.map((label) =>
      typeof label === "string" ? label : label.name,
    ),
  );
  const add = [...plan.desired].filter((label) => !current.has(label));
  const remove = [...plan.managed].filter(
    (label) => current.has(label) && !plan.desired.has(label),
  );

  if (add.length > 0) {
    await github.rest.issues.addLabels({
      ...context.repo,
      issue_number: issue.number,
      labels: add,
    });
  }

  for (const name of remove) {
    await github.rest.issues.removeLabel({
      ...context.repo,
      issue_number: issue.number,
      name,
    });
  }

  core.info(
    `Issue labels reconciled (added: ${add.join(", ") || "none"}; ` +
      `removed: ${remove.join(", ") || "none"})`,
  );
}

module.exports = labelIssue;
module.exports.issueFormSections = issueFormSections;
module.exports.labelPlan = labelPlan;
