/* SPDX-License-Identifier: GPL-3.0-or-later */

const assert = require("node:assert/strict");
const test = require("node:test");
const labelIssue = require("./label-issue.js");

function form(fields) {
  return Object.entries(fields)
    .map(([heading, value]) => `### ${heading}\n\n${value}`)
    .join("\n\n");
}

test("ignores blank issues", () => {
  assert.equal(labelIssue.labelPlan("Free-form issue body"), null);
});

test("maps the bug area without reading free text", () => {
  const plan = labelIssue.labelPlan(
    form({
      Summary: "The runtime text mentions Documentation but is not classified.",
      "Suspected area": "Runtime or agent loop",
    }),
  );

  assert.deepEqual([...plan.desired], ["runtime"]);
  assert.equal(plan.managed.has("doc"), true);
  assert.equal(plan.managed.has("rfc"), false);
});

test("maps RFC and multiple proposal areas", () => {
  const plan = labelIssue.labelPlan(
    form({
      "Proposal type": "Architecture RFC",
      "Architectural impact": [
        "- Runtime or agent loop",
        "- Actions",
        "- Skills or capabilities",
      ].join("\n"),
    }),
  );

  assert.deepEqual([...plan.desired], ["rfc", "runtime", "actions", "skills"]);
  assert.equal(plan.managed.has("doc"), false);
});

test("reconciles managed labels and preserves manual labels", async () => {
  const created = [];
  const updated = [];
  const added = [];
  const removed = [];
  const github = {
    paginate: async () => [{name: "runtime"}, {name: "documentation"}],
    rest: {
      issues: {
        listLabelsForRepo: () => {},
        createLabel: async (input) => created.push(input.name),
        updateLabel: async (input) => updated.push(input.new_name),
        addLabels: async (input) => added.push(...input.labels),
        removeLabel: async (input) => removed.push(input.name),
      },
    },
  };
  const context = {
    repo: {owner: "Jamie-Cui", repo: "magent"},
    payload: {
      issue: {
        number: 42,
        body: form({"Suspected area": "Documentation"}),
        labels: [{name: "runtime"}, {name: "security"}, {name: "rfc"}],
      },
    },
  };

  await labelIssue({github, context, core: {info: () => {}}});

  assert.deepEqual(updated, ["doc"]);
  assert.equal(created.includes("doc"), false);
  assert.deepEqual(added, ["doc"]);
  assert.deepEqual(removed, ["runtime"]);
});
