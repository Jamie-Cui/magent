import json
from pathlib import Path

from magent_benchmark.report import (
    classify_failure,
    load_trials,
    paired_bootstrap,
    write_reports,
)


def _result(
    path: Path,
    *,
    task: str,
    agent: str,
    passed: bool,
    start: str,
    model: str = "model-x",
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(
            {
                "task_name": task,
                "trial_name": path.parent.name,
                "source": "swe-bench/swe-bench-verified",
                "agent_info": {
                    "name": agent,
                    "version": "1",
                    "model_info": {"name": model, "provider": "openai"},
                },
                "agent_result": {
                    "n_input_tokens": 100,
                    "n_cache_tokens": 25,
                    "n_output_tokens": 20,
                    "cost_usd": 0.1,
                    "metadata": {"tool_calls": 2},
                },
                "verifier_result": {"rewards": {"reward": 1 if passed else 0}},
                "started_at": start,
                "finished_at": "2026-01-01T00:01:00+00:00",
                "agent_execution": {
                    "started_at": "2026-01-01T00:00:10+00:00",
                    "finished_at": "2026-01-01T00:00:40+00:00",
                },
            }
        ),
        encoding="utf-8",
    )


def test_report_keeps_suite_and_paired_statistics(tmp_path: Path) -> None:
    _result(tmp_path / "a1" / "results.json", task="one", agent="codex", passed=True, start="1")
    _result(tmp_path / "a2" / "results.json", task="two", agent="codex", passed=False, start="1")
    _result(tmp_path / "b1" / "results.json", task="one", agent="magent", passed=False, start="1")
    _result(tmp_path / "b2" / "results.json", task="two", agent="magent", passed=False, start="1")

    trials = load_trials(tmp_path)
    comparisons = paired_bootstrap(trials, iterations=100)
    comparison = next(row for row in comparisons if row["agent_a"] == "codex")
    assert comparison["paired_tasks"] == 2
    assert comparison["pass_at_1_delta"] == 0.5

    outputs = write_reports(tmp_path, tmp_path / "reports", bootstrap=100)
    assert {path.name for path in outputs} == {"trials.csv", "summary.json", "report.md"}
    summary = json.loads((tmp_path / "reports" / "summary.json").read_text())
    assert all(row["pass_at_3"] is None for row in summary["suites"])
    report = (tmp_path / "reports" / "report.md").read_text()
    assert "no composite score" in report
    assert "swe-bench-verified" in report
    assert "Input tok." in report
    assert "—" in report


def test_failure_classification_uses_exception_detail() -> None:
    assert (
        classify_failure(
            "RuntimeError", False, "Docker compose environment build failed"
        )
        == "environment_infrastructure"
    )
    assert (
        classify_failure(
            "NonZeroAgentExitCodeError",
            False,
            "Magent requires Emacs >=29.1, found 27.1",
        )
        == "environment_infrastructure"
    )
    assert classify_failure("", False) == "task_failure"


def test_load_trials_accepts_harbor_0_20_result_filename(tmp_path: Path) -> None:
    result_path = tmp_path / "trial" / "result.json"
    _result(
        result_path,
        task="one",
        agent="magent",
        passed=True,
        start="1",
    )

    trials = load_trials(tmp_path)

    assert len(trials) == 1
    assert trials[0].result_path == str(result_path)
    assert trials[0].passed


def test_pass_at_3_requires_and_uses_three_attempts(tmp_path: Path) -> None:
    for attempt in range(1, 4):
        _result(
            tmp_path / f"attempt-{attempt}" / "results.json",
            task="one",
            agent="magent",
            passed=attempt == 3,
            start=str(attempt),
        )

    write_reports(tmp_path, tmp_path / "reports", bootstrap=0)
    summary = json.loads((tmp_path / "reports" / "summary.json").read_text())
    row = summary["suites"][0]
    assert row["pass_at_1"] == 0
    assert row["pass_at_3"] == 1
    assert row["resolved_rate"] == 1


def test_opencode_provider_namespace_is_normalized_for_pairing(tmp_path: Path) -> None:
    _result(
        tmp_path / "main-codex" / "results.json",
        task="one",
        agent="codex",
        passed=True,
        start="1",
    )
    _result(
        tmp_path / "main-opencode" / "results.json",
        task="one",
        agent="opencode",
        passed=False,
        start="1",
        model="openai/model-x",
    )

    trials = load_trials(tmp_path)
    assert {trial.model for trial in trials} == {"model-x"}
    comparisons = paired_bootstrap(trials, iterations=10)
    assert len(comparisons) == 1
    assert comparisons[0]["paired_tasks"] == 1


def test_tool_calls_fall_back_to_harbor_atif(tmp_path: Path) -> None:
    result_path = tmp_path / "main-codex" / "results.json"
    _result(
        result_path,
        task="one",
        agent="codex",
        passed=True,
        start="1",
    )
    data = json.loads(result_path.read_text())
    data["agent_result"]["metadata"] = {}
    result_path.write_text(json.dumps(data))
    trajectory = result_path.parent / "agent" / "trajectory.json"
    trajectory.parent.mkdir()
    trajectory.write_text(
        json.dumps(
            {
                "steps": [
                    {"source": "agent", "tool_calls": [{"tool_call_id": "1"}]},
                    {
                        "source": "agent",
                        "tool_calls": [
                            {"tool_call_id": "2"},
                            {"tool_call_id": "3"},
                        ],
                    },
                ]
            }
        )
    )

    trials = load_trials(tmp_path)
    assert trials[0].tool_calls == 3
