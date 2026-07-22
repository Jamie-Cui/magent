"""Suite-separated benchmark reports and paired bootstrap comparisons."""

from __future__ import annotations

from collections import Counter, defaultdict
import csv
from dataclasses import asdict, dataclass
from datetime import datetime
import json
from pathlib import Path
import random
from statistics import mean
from typing import Any, Iterable


@dataclass(frozen=True)
class Trial:
    result_path: str
    scoreboard: str
    suite: str
    task: str
    agent: str
    agent_version: str
    model: str
    provider: str
    reward: float
    passed: bool
    started_at: str
    duration_seconds: float | None
    input_tokens: int | None
    cached_tokens: int | None
    output_tokens: int | None
    cost_usd: float | None
    tool_calls: int | None
    exception_type: str
    failure_category: str


def _nested(data: Any, *keys: str, default: Any = None) -> Any:
    value = data
    for key in keys:
        if not isinstance(value, dict):
            return default
        value = value.get(key)
    return default if value is None else value


def _suite(data: dict[str, Any], path: Path) -> str:
    source = str(data.get("source") or "")
    task_source = str(_nested(data, "config", "task", "source", default="") or "")
    joined = f"{source} {task_source} {path}".lower()
    if "swe-bench" in joined:
        return "swe-bench-verified"
    if "terminal-bench" in joined or "terminal_bench" in joined:
        return "terminal-bench-2.1"
    return source or task_source or "unknown"


def _scoreboard(path: Path) -> str:
    lowered = str(path).lower()
    if "compatibility" in lowered:
        return "compatibility"
    if "openai-main" in lowered or "main" in lowered:
        return "main"
    return "unspecified"


def _model(data: dict[str, Any], agent: str) -> tuple[str, str]:
    info = _nested(data, "agent_info", "model_info", default={})
    if not isinstance(info, dict):
        return "unknown", "unknown"
    name = str(info.get("name") or "unknown")
    provider = str(info.get("provider") or "unknown")
    if agent == "opencode" and "/" in name:
        # Profiles add OpenCode's provider namespace to the same canonical
        # model passed to Codex and Magent. Remove exactly that outer segment
        # so reports and paired comparisons group the three products together.
        name = name.split("/", 1)[1]
    return name, provider


def _reward(data: dict[str, Any]) -> float:
    rewards = _nested(data, "verifier_result", "rewards", default={})
    if not isinstance(rewards, dict) or not rewards:
        return 0.0
    value = rewards.get("reward")
    if not isinstance(value, (int, float)) or isinstance(value, bool):
        numeric = [
            item
            for item in rewards.values()
            if isinstance(item, (int, float)) and not isinstance(item, bool)
        ]
        value = max(numeric, default=0.0)
    return float(value)


def _contexts(data: dict[str, Any]) -> list[dict[str, Any]]:
    context = data.get("agent_result")
    if isinstance(context, dict):
        return [context]
    contexts: list[dict[str, Any]] = []
    for step in data.get("step_results") or []:
        candidate = step.get("agent_result") if isinstance(step, dict) else None
        if isinstance(candidate, dict):
            contexts.append(candidate)
    return contexts


def _sum_optional(contexts: list[dict[str, Any]], key: str) -> int | float | None:
    values = [
        context[key]
        for context in contexts
        if isinstance(context.get(key), (int, float))
        and not isinstance(context.get(key), bool)
    ]
    return sum(values) if values else None


def _tool_calls(contexts: list[dict[str, Any]], result_path: Path) -> int | None:
    values = []
    for context in contexts:
        metadata = context.get("metadata")
        if isinstance(metadata, dict) and isinstance(metadata.get("tool_calls"), int):
            values.append(metadata["tool_calls"])
    if values:
        return sum(values)

    trial_dir = result_path.parent
    trajectories = [trial_dir / "agent" / "trajectory.json"]
    trajectories.extend(sorted(trial_dir.glob("steps/*/agent/trajectory.json")))
    count = 0
    found = False
    for trajectory_path in trajectories:
        if not trajectory_path.is_file():
            continue
        try:
            trajectory = json.loads(trajectory_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            continue
        found = True
        for step in trajectory.get("steps") or []:
            if isinstance(step, dict) and isinstance(step.get("tool_calls"), list):
                count += len(step["tool_calls"])
    return count if found else None


def _duration(data: dict[str, Any]) -> float | None:
    started = _nested(data, "agent_execution", "started_at")
    finished = _nested(data, "agent_execution", "finished_at")
    if not started or not finished:
        started, finished = data.get("started_at"), data.get("finished_at")
    try:
        return (
            datetime.fromisoformat(str(finished).replace("Z", "+00:00"))
            - datetime.fromisoformat(str(started).replace("Z", "+00:00"))
        ).total_seconds()
    except (TypeError, ValueError):
        return None


def classify_failure(exception_type: str, passed: bool, detail: str = "") -> str:
    if passed:
        return "passed"
    lowered = f"{exception_type} {detail}".lower().strip()
    if not lowered:
        return "task_failure"
    if "timeout" in lowered:
        return "timeout"
    if any(
        token in lowered for token in ("authentication", "usagelimit", "modelfound")
    ):
        return "configuration"
    if "api" in lowered or "network" in lowered or "connection" in lowered:
        return "provider_or_network"
    if "verifier" in lowered or "reward" in lowered:
        return "verifier_infrastructure"
    if any(
        token in lowered
        for token in ("setup", "build", "environment", "requires emacs")
    ):
        return "environment_infrastructure"
    return "agent_runtime"


def load_trials(jobs_dir: Path) -> list[Trial]:
    root = jobs_dir.resolve()
    result_paths = list(root.rglob("result.json"))
    result_paths.extend(root.rglob("results.json"))

    trials: list[Trial] = []
    for path in sorted(result_paths):
        try:
            data = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            continue
        if not isinstance(data, dict) or "task_name" not in data:
            continue
        reward = _reward(data)
        passed = reward >= 1.0
        exception_type = str(
            _nested(data, "exception_info", "exception_type", default="") or ""
        )
        exception_detail = str(
            _nested(data, "exception_info", "exception_message", default="") or ""
        )
        contexts = _contexts(data)
        agent = str(_nested(data, "agent_info", "name", default="unknown"))
        model, provider = _model(data, agent)
        trials.append(
            Trial(
                result_path=str(path),
                scoreboard=_scoreboard(path),
                suite=_suite(data, path),
                task=str(data.get("task_name")),
                agent=agent,
                agent_version=str(
                    _nested(data, "agent_info", "version", default="unknown")
                ),
                model=model,
                provider=provider,
                reward=reward,
                passed=passed,
                started_at=str(data.get("started_at") or ""),
                duration_seconds=_duration(data),
                input_tokens=_sum_optional(contexts, "n_input_tokens"),
                cached_tokens=_sum_optional(contexts, "n_cache_tokens"),
                output_tokens=_sum_optional(contexts, "n_output_tokens"),
                cost_usd=_sum_optional(contexts, "cost_usd"),
                tool_calls=_tool_calls(contexts, path),
                exception_type=exception_type,
                failure_category=classify_failure(
                    exception_type, passed, exception_detail
                ),
            )
        )
    return trials


def _average(values: Iterable[int | float | None]) -> float | None:
    present = [float(value) for value in values if value is not None]
    return mean(present) if present else None


def _group_summary(trials: list[Trial]) -> list[dict[str, Any]]:
    grouped: dict[tuple[str, str, str, str], list[Trial]] = defaultdict(list)
    for trial in trials:
        grouped[(trial.scoreboard, trial.suite, trial.model, trial.agent)].append(trial)

    rows: list[dict[str, Any]] = []
    for (scoreboard, suite, model, agent), group in sorted(grouped.items()):
        by_task: dict[str, list[Trial]] = defaultdict(list)
        for trial in group:
            by_task[trial.task].append(trial)
        for attempts in by_task.values():
            attempts.sort(key=lambda trial: (trial.started_at, trial.result_path))
        pass1 = [attempts[0].passed for attempts in by_task.values()]
        has_three_attempts = all(len(attempts) >= 3 for attempts in by_task.values())
        pass3 = (
            [any(trial.passed for trial in attempts[:3]) for attempts in by_task.values()]
            if has_three_attempts
            else []
        )
        resolved = [any(trial.passed for trial in attempts) for attempts in by_task.values()]
        rows.append(
            {
                "scoreboard": scoreboard,
                "suite": suite,
                "model": model,
                "agent": agent,
                "tasks": len(by_task),
                "trials": len(group),
                "pass_at_1": mean(pass1) if pass1 else 0.0,
                "pass_at_3": mean(pass3) if pass3 else None,
                "resolved_rate": mean(resolved) if resolved else 0.0,
                "mean_reward": mean(trial.reward for trial in group),
                "mean_input_tokens": _average(t.input_tokens for t in group),
                "mean_cached_tokens": _average(t.cached_tokens for t in group),
                "mean_output_tokens": _average(t.output_tokens for t in group),
                "mean_cost_usd": _average(t.cost_usd for t in group),
                "mean_duration_seconds": _average(t.duration_seconds for t in group),
                "mean_tool_calls": _average(t.tool_calls for t in group),
                "failures": dict(sorted(Counter(t.failure_category for t in group).items())),
            }
        )
    return rows


def _first_attempts(
    trials: list[Trial],
) -> dict[tuple[str, str, str, str], dict[str, bool]]:
    grouped: dict[tuple[str, str, str, str, str], list[Trial]] = defaultdict(list)
    for trial in trials:
        grouped[
            (trial.scoreboard, trial.suite, trial.model, trial.agent, trial.task)
        ].append(trial)
    result: dict[tuple[str, str, str, str], dict[str, bool]] = defaultdict(dict)
    for (scoreboard, suite, model, agent, task), attempts in grouped.items():
        attempts.sort(key=lambda trial: (trial.started_at, trial.result_path))
        result[(scoreboard, suite, model, agent)][task] = attempts[0].passed
    return result


def paired_bootstrap(trials: list[Trial], iterations: int = 10_000) -> list[dict[str, Any]]:
    first = _first_attempts(trials)
    agent_sets: dict[tuple[str, str, str], set[str]] = defaultdict(set)
    for scoreboard, suite, model, agent in first:
        agent_sets[(scoreboard, suite, model)].add(agent)
    rng = random.Random(20260720)
    comparisons: list[dict[str, Any]] = []
    for (scoreboard, suite, model), agents in sorted(agent_sets.items()):
        ordered = sorted(agents)
        for index, agent_a in enumerate(ordered):
            for agent_b in ordered[index + 1 :]:
                a = first[(scoreboard, suite, model, agent_a)]
                b = first[(scoreboard, suite, model, agent_b)]
                tasks = sorted(set(a) & set(b))
                if not tasks:
                    continue
                deltas = [float(a[task]) - float(b[task]) for task in tasks]
                samples = []
                for _ in range(max(0, iterations)):
                    samples.append(mean(rng.choice(deltas) for _ in tasks))
                samples.sort()
                if samples:
                    low = samples[int(0.025 * (len(samples) - 1))]
                    high = samples[int(0.975 * (len(samples) - 1))]
                else:
                    low = high = None
                comparisons.append(
                    {
                        "scoreboard": scoreboard,
                        "suite": suite,
                        "model": model,
                        "agent_a": agent_a,
                        "agent_b": agent_b,
                        "paired_tasks": len(tasks),
                        "pass_at_1_delta": mean(deltas),
                        "ci_95_low": low,
                        "ci_95_high": high,
                    }
                )
    return comparisons


def _fmt(value: Any, *, percent: bool = False) -> str:
    if value is None:
        return "—"
    if isinstance(value, float):
        return f"{100 * value:.1f}%" if percent else f"{value:.3f}"
    return str(value)


def _markdown(summary: list[dict[str, Any]], comparisons: list[dict[str, Any]]) -> str:
    lines = [
        "# Magent benchmark report",
        "",
        "Suites are intentionally reported separately; there is no composite score.",
        "",
    ]
    sections = sorted({(row["scoreboard"], row["suite"]) for row in summary})
    for scoreboard, suite in sections:
        lines.extend(
            [
                f"## {suite} — {scoreboard}",
                "",
                "| Model | Agent | Tasks | Trials | pass@1 | pass@3 | Resolved | Input tok. | Cached tok. | Output tok. | Mean time (s) | Mean tools | Mean cost (USD) |",
                "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|",
            ]
        )
        for row in (
            item
            for item in summary
            if item["suite"] == suite and item["scoreboard"] == scoreboard
        ):
            lines.append(
                "| {model} | {agent} | {tasks} | {trials} | {p1} | {p3} | {resolved} | {input_tokens} | {cached_tokens} | {output_tokens} | {time} | {tools} | {cost} |".format(
                    model=row["model"],
                    agent=row["agent"],
                    tasks=row["tasks"],
                    trials=row["trials"],
                    p1=_fmt(row["pass_at_1"], percent=True),
                    p3=_fmt(row["pass_at_3"], percent=True),
                    resolved=_fmt(row["resolved_rate"], percent=True),
                    input_tokens=_fmt(row["mean_input_tokens"]),
                    cached_tokens=_fmt(row["mean_cached_tokens"]),
                    output_tokens=_fmt(row["mean_output_tokens"]),
                    time=_fmt(row["mean_duration_seconds"]),
                    tools=_fmt(row["mean_tool_calls"]),
                    cost=_fmt(row["mean_cost_usd"]),
                )
            )
        lines.append("")

    if comparisons:
        lines.extend(
            [
                "## Paired pass@1 bootstrap",
                "",
                "Positive delta favors agent A. Intervals resample task instances.",
                "",
                "| Board | Suite | Model | Agent A | Agent B | N | Delta | 95% CI |",
                "|---|---|---|---|---|---:|---:|---:|",
            ]
        )
        for row in comparisons:
            interval = f"[{_fmt(row['ci_95_low'], percent=True)}, {_fmt(row['ci_95_high'], percent=True)}]"
            lines.append(
                f"| {row['scoreboard']} | {row['suite']} | {row['model']} | {row['agent_a']} | {row['agent_b']} | {row['paired_tasks']} | {_fmt(row['pass_at_1_delta'], percent=True)} | {interval} |"
            )
        lines.append("")
    return "\n".join(lines)


def write_reports(jobs_dir: Path, output_dir: Path, *, bootstrap: int = 10_000) -> list[Path]:
    trials = load_trials(jobs_dir)
    if not trials:
        raise ValueError(f"no Harbor trial result files found under {jobs_dir}")
    output_dir = output_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)
    summary = _group_summary(trials)
    comparisons = paired_bootstrap(trials, iterations=bootstrap)

    trial_csv = output_dir / "trials.csv"
    with trial_csv.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(asdict(trials[0])))
        writer.writeheader()
        writer.writerows(asdict(trial) for trial in trials)

    summary_json = output_dir / "summary.json"
    summary_json.write_text(
        json.dumps(
            {"suites": summary, "paired_bootstrap": comparisons},
            indent=2,
            ensure_ascii=False,
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )
    report_md = output_dir / "report.md"
    report_md.write_text(_markdown(summary, comparisons), encoding="utf-8")
    return [trial_csv, summary_json, report_md]
