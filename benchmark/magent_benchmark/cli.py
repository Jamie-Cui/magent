"""Command-line interface for generating, running, and reporting benchmarks."""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import random
import re
import shutil
import subprocess
import sys
import tempfile
import time
from typing import Sequence

from rich.progress import (
    BarColumn,
    MofNCompleteColumn,
    Progress,
    TextColumn,
    TimeElapsedColumn,
)
import yaml

from .job import build_job, write_job
from .profiles import (
    ConfigurationError,
    check_api_key,
    load_config,
    read_manifest,
    require_benchmark,
    require_profile,
    require_suite,
    stage_shape,
)


_HARBOR_PUBLIC_ALLOWLIST_WARNING_FILTER = (
    "ignore:Run-specific allowlist host:UserWarning"
)


def cmd_profiles(args: argparse.Namespace) -> int:
    config = load_config(args.config)
    payload = {
        name: {
            "scoreboard": profile.scoreboard,
            "provider": profile.provider,
            "wire_api": profile.wire_api,
            "agents": list(profile.agents),
            "unsupported": profile.unsupported,
        }
        for name, profile in sorted(config.profiles.items())
    }
    print(json.dumps(payload, indent=2, ensure_ascii=False))
    return 0


def _prepared_job_path(config) -> Path:
    return config.path.parent / "generated" / "benchmark.yaml"


def _fresh_job_name(prepared_name: str) -> str:
    suffix = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    return f"{prepared_name}-{suffix}"


def _agent_key(agent: dict) -> str:
    """Return the stable short name used to group Harbor trials."""
    if agent.get("name"):
        return str(agent["name"])
    class_name = str(agent.get("import_path", "")).rsplit(":", 1)[-1]
    short_name = re.sub(r"(?:Harbor)?Agent$", "", class_name)
    return short_name.lower() or "unknown"


def _agent_label(agent_key: str) -> str:
    return {
        "codex": "Codex",
        "opencode": "OpenCode",
        "magent": "Magent",
    }.get(agent_key, agent_key)


def _agent_progress_specs(job: dict) -> list[tuple[str, str, int]]:
    task_count = len(job.get("tasks") or []) + sum(
        len(dataset.get("task_names") or [])
        for dataset in job.get("datasets") or []
    )
    total = task_count * int(job.get("n_attempts", 1))
    return [
        (key, _agent_label(key), total)
        for agent in job.get("agents") or []
        for key in (_agent_key(agent),)
    ]


def _usage_int(value, *keys: str) -> int:
    if not isinstance(value, dict):
        return 0
    for key in keys:
        candidate = value.get(key)
        if isinstance(candidate, (int, float)) and not isinstance(candidate, bool):
            return int(candidate)
    return 0


def _usage_totals(samples) -> tuple[int, int, int]:
    input_tokens = cache_tokens = output_tokens = 0
    if not isinstance(samples, list):
        return 0, 0, 0
    for sample in samples:
        input_tokens += _usage_int(
            sample, "input", "input_tokens", "prompt_tokens", "prompt"
        )
        cache_tokens += _usage_int(
            sample,
            "cached",
            "cached_tokens",
            "cache_read_input_tokens",
            "cache_read",
        )
        output_tokens += _usage_int(
            sample, "output", "output_tokens", "completion_tokens", "completion"
        )
    return input_tokens, cache_tokens, output_tokens


def _json_lines(path: Path):
    try:
        with path.open(encoding="utf-8", errors="replace") as stream:
            for line in stream:
                try:
                    event = json.loads(line)
                except (ValueError, TypeError):
                    continue
                if isinstance(event, dict):
                    yield event
    except OSError:
        return


def _opencode_live_usage(path: Path) -> tuple[int, int, int]:
    input_tokens = cache_tokens = output_tokens = 0
    for event in _json_lines(path):
        if event.get("type") != "step_finish":
            continue
        part = event.get("part") or {}
        tokens = part.get("tokens") or {}
        cache = tokens.get("cache") or {}
        cache_read = _usage_int(cache, "read")
        # Match Harbor's OpenCode adapter: prompt tokens include cache reads.
        input_tokens += _usage_int(tokens, "input") + cache_read
        cache_tokens += cache_read
        output_tokens += _usage_int(tokens, "output")
    return input_tokens, cache_tokens, output_tokens


def _codex_live_usage(path: Path) -> tuple[int, int, int]:
    input_tokens = cache_tokens = output_tokens = 0
    for event in _json_lines(path):
        if event.get("type") != "turn.completed":
            continue
        usage = event.get("usage") or {}
        input_tokens += _usage_int(usage, "input_tokens", "input")
        cache_tokens += _usage_int(
            usage, "cached_input_tokens", "cache_read_input_tokens", "cached"
        )
        output_tokens += _usage_int(usage, "output_tokens", "output")
    return input_tokens, cache_tokens, output_tokens


def _magent_live_usage(path: Path) -> tuple[int, int, int]:
    try:
        progress = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError, TypeError):
        return 0, 0, 0
    return _usage_totals(
        progress.get("usage-samples", progress.get("usage_samples", []))
    )


def _live_trial_usage(trial_dir: Path, agent_key: str) -> tuple[int, int, int]:
    if agent_key == "opencode":
        return _opencode_live_usage(trial_dir / "agent" / "opencode.txt")
    if agent_key == "codex":
        return _codex_live_usage(trial_dir / "agent" / "codex.txt")
    if agent_key == "magent":
        return _magent_live_usage(trial_dir / "agent" / "magent-progress.json")
    return 0, 0, 0


def _trial_last_activity(trial_dir: Path) -> float | None:
    latest = None
    for relative in (
        "config.json",
        "trial.log",
        "result.json",
        "agent/codex.txt",
        "agent/opencode.txt",
        "agent/magent-progress.json",
        "agent/emacsclient.txt",
        "verifier/reward.txt",
        "verifier/test-stdout.txt",
    ):
        try:
            modified = (trial_dir / relative).stat().st_mtime
        except OSError:
            continue
        latest = modified if latest is None else max(latest, modified)
    return latest


def _empty_agent_progress() -> dict[str, int | float | None]:
    return {
        "completed": 0,
        "running": 0,
        "errors": 0,
        "input_tokens": 0,
        "cache_tokens": 0,
        "output_tokens": 0,
        "last_activity": None,
    }


def _agent_progress_snapshot(
    job_dir: Path, specs: list[tuple[str, str, int]]
) -> dict[str, dict[str, int | float | None]]:
    """Read completed and active trials without descending into trial logs."""
    snapshot = {key: _empty_agent_progress() for key, _label, _total in specs}
    for config_path in job_dir.glob("*/config.json"):
        try:
            trial = json.loads(config_path.read_text(encoding="utf-8"))
            key = _agent_key(trial.get("agent") or {})
        except (OSError, ValueError, TypeError):
            continue
        if key not in snapshot:
            continue

        state = snapshot[key]
        activity = _trial_last_activity(config_path.parent)
        if activity is not None:
            previous = state["last_activity"]
            state["last_activity"] = (
                activity if previous is None else max(float(previous), activity)
            )

        result_path = config_path.with_name("result.json")
        if not result_path.is_file():
            state["running"] += 1
            usage = _live_trial_usage(config_path.parent, key)
            state["input_tokens"] += usage[0]
            state["cache_tokens"] += usage[1]
            state["output_tokens"] += usage[2]
            continue
        try:
            result = json.loads(result_path.read_text(encoding="utf-8"))
        except (OSError, ValueError, TypeError):
            state["running"] += 1
            continue
        agent_result = result.get("agent_result") or {}
        state["input_tokens"] += _usage_int(agent_result, "n_input_tokens")
        state["cache_tokens"] += _usage_int(agent_result, "n_cache_tokens")
        state["output_tokens"] += _usage_int(agent_result, "n_output_tokens")
        state["completed"] += 1
        if result.get("exception_info"):
            state["errors"] += 1
    return snapshot


def _compact_count(value: int | float | None) -> str:
    count = int(value or 0)
    for divisor, suffix in (
        (1_000_000_000, "B"),
        (1_000_000, "M"),
        (1_000, "K"),
    ):
        if count >= divisor:
            return f"{count / divisor:.1f}{suffix}"
    return str(count)


def _compact_age(seconds: float) -> str:
    seconds = max(0, int(seconds))
    if seconds < 60:
        return f"{seconds}s"
    if seconds < 3600:
        return f"{seconds // 60}m{seconds % 60:02d}s"
    return f"{seconds // 3600}h{seconds % 3600 // 60:02d}m"


def _progress_state_text(
    state: dict[str, int | float | None], *, now: float
) -> str:
    rendered = (
        f"run{state['running']} err{state['errors']} · "
        f"tok i{_compact_count(state['input_tokens'])}"
        f"/c{_compact_count(state['cache_tokens'])}"
        f"/o{_compact_count(state['output_tokens'])}"
    )
    if state["running"] and state["last_activity"] is not None:
        rendered += f" · seen{_compact_age(now - float(state['last_activity']))}"
    return rendered


def _stop_process(process: subprocess.Popen) -> None:
    if process.poll() is not None:
        return
    process.terminate()
    try:
        process.wait(timeout=5)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait()


def _run_harbor_job(
    command: list[str], *, environment: dict[str, str], job_dir: Path, job: dict
) -> int:
    """Run Harbor while rendering one aggregate progress row per agent."""
    specs = _agent_progress_specs(job)
    progress = Progress(
        TextColumn("{task.description:>10}"),
        BarColumn(bar_width=16),
        MofNCompleteColumn(),
        TextColumn("{task.fields[state]}"),
        TimeElapsedColumn(),
        refresh_per_second=2,
    )
    overall_id = progress.add_task(
        "Overall",
        total=sum(total for _key, _label, total in specs),
        state=_progress_state_text(_empty_agent_progress(), now=time.time()),
    )
    task_ids = {
        key: progress.add_task(
            label,
            total=total,
            state=_progress_state_text(_empty_agent_progress(), now=time.time()),
        )
        for key, label, total in specs
    }

    with (
        tempfile.TemporaryFile(mode="w+", encoding="utf-8") as child_stdout,
        tempfile.TemporaryFile(mode="w+", encoding="utf-8") as child_stderr,
    ):
        process = subprocess.Popen(
            command,
            env=environment,
            stdout=child_stdout,
            stderr=child_stderr,
            text=True,
        )
        interrupted = False
        try:
            with progress:
                while True:
                    snapshot = _agent_progress_snapshot(job_dir, specs)
                    now = time.time()
                    overall = _empty_agent_progress()
                    for key, _label, _total in specs:
                        state = snapshot[key]
                        progress.update(
                            task_ids[key],
                            completed=state["completed"],
                            state=_progress_state_text(state, now=now),
                        )
                        for field in (
                            "completed",
                            "running",
                            "errors",
                            "input_tokens",
                            "cache_tokens",
                            "output_tokens",
                        ):
                            overall[field] = int(overall[field] or 0) + int(
                                state[field] or 0
                            )
                        activity = state["last_activity"]
                        if activity is not None:
                            previous = overall["last_activity"]
                            overall["last_activity"] = (
                                activity
                                if previous is None
                                else max(float(previous), float(activity))
                            )
                    progress.update(
                        overall_id,
                        completed=overall["completed"],
                        state=_progress_state_text(overall, now=now),
                    )
                    if process.poll() is not None:
                        break
                    time.sleep(0.5)
            returncode = process.wait()
        except KeyboardInterrupt:
            interrupted = True
            _stop_process(process)
            returncode = 130
        finally:
            child_stdout.flush()
            child_stdout.seek(0)
            shutil.copyfileobj(child_stdout, sys.stdout)
            child_stderr.flush()
            child_stderr.seek(0)
            shutil.copyfileobj(child_stderr, sys.stderr)
        return 130 if interrupted else returncode


def _check_local_runtime() -> None:
    for command in ("docker", "harbor"):
        if shutil.which(command) is None:
            raise ConfigurationError(
                f"{command} is required; install it before running "
                "make benchmark-prepare"
            )
    result = subprocess.run(
        ["docker", "info"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        text=True,
        check=False,
    )
    if result.returncode:
        detail = (result.stderr or "").strip().splitlines()
        message = detail[-1] if detail else "Docker daemon is not available"
        raise ConfigurationError(f"Docker is not ready: {message}")


def prepare_elpa_bundle(source: Path, output: Path) -> Path:
    """Copy the exact local Magent dependencies into OUTPUT once."""
    source = source.expanduser().resolve()
    output = output.expanduser().resolve()
    if output.exists():
        return output
    if not source.is_dir():
        raise ConfigurationError(f"ELPA source directory does not exist: {source}")
    packages = (
        "gptel",
        "transient",
        "acp",
        "shell-maker",
        "agent-shell",
        "cond-let",
        "compat",
        "yaml",
        "llama",
        "with-editor",
    )
    selected: dict[str, Path] = {}
    missing: list[str] = []
    for package in packages:
        pattern = re.compile(rf"^{re.escape(package)}-[0-9].*$")
        candidates = sorted(
            (path for path in source.iterdir() if path.is_dir() and pattern.match(path.name)),
            key=lambda path: path.name,
        )
        if candidates:
            selected[package] = candidates[-1]
        else:
            missing.append(package)
    if any(package in missing for package in ("gptel", "acp", "agent-shell", "yaml")):
        raise ConfigurationError(
            "required packages missing from ELPA source: " + ", ".join(missing)
        )
    output.mkdir(parents=True)
    for path in selected.values():
        shutil.copytree(path, output / path.name, symlinks=True)
    (output / "bundle.json").write_text(
        json.dumps(
            {
                "source": str(source),
                "packages": {name: path.name for name, path in selected.items()},
                "not_present": missing,
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )
    return output


def prepare_emacs_bundle(
    dockerfile: Path, output: Path, *, proxy: str | None = None
) -> Path:
    """Build the pinned Emacs prefix without exposing proxy credentials."""
    dockerfile = dockerfile.resolve()
    output = output.resolve()
    bins = tuple(output / "bin" / name for name in ("emacs", "emacsclient"))
    if all(path.is_file() for path in bins):
        return output
    if not dockerfile.is_file():
        raise ConfigurationError(f"Emacs bundle Dockerfile not found: {dockerfile}")

    command = [
        "docker",
        "build",
        "--file",
        str(dockerfile),
        "--build-arg",
        f"OUTPUT_UID={os.getuid()}",
        "--build-arg",
        f"OUTPUT_GID={os.getgid()}",
    ]
    environment = os.environ.copy()
    if proxy:
        environment.update(
            {
                "HTTP_PROXY": proxy,
                "HTTPS_PROXY": proxy,
                "http_proxy": proxy,
                "https_proxy": proxy,
            }
        )
        command.extend(
            [
                "--network",
                "host",
                "--build-arg",
                "HTTP_PROXY",
                "--build-arg",
                "HTTPS_PROXY",
            ]
        )
    command.extend(["--output", f"type=local,dest={output}", str(dockerfile.parent)])
    result = subprocess.run(command, env=environment, check=False)
    if result.returncode:
        raise ConfigurationError("failed to build the portable Emacs bundle")
    if not all(path.is_file() for path in bins):
        raise ConfigurationError(f"Emacs bundle is missing bin/emacs: {output}")
    return output


def prepare_config(config, *, check_runtime: bool = True) -> tuple[Path, Path]:
    """Validate config.toml and write the job used by make benchmark."""
    selected = require_benchmark(config)
    profile = require_profile(config, selected.profile)
    suite = require_suite(config, selected.suite)
    check_api_key(profile)
    if check_runtime:
        _check_local_runtime()
    if selected.elpa_bundle:
        prepare_elpa_bundle(Path("~/.emacs.d/elpa"), selected.elpa_bundle)
    job, fingerprint = build_job(
        config,
        profile,
        suite,
        selected.model,
        selected.stage,
        effort=selected.effort,
        input_price_per_million=selected.input_price_per_million,
        output_price_per_million=selected.output_price_per_million,
        cached_input_price_per_million=selected.cached_input_price_per_million,
        approved_full=selected.approve_full,
        concurrency=selected.concurrency,
        elpa_bundle=selected.elpa_bundle,
        emacs_bundle=selected.emacs_bundle,
        proxy=selected.proxy,
    )
    job["jobs_dir"] = str((config.path.parent / "jobs").resolve())

    # Fail during preparation, before any paid run, if Harbor rejects the job.
    from harbor.models.job.config import JobConfig

    JobConfig.model_validate(job)
    return write_job(_prepared_job_path(config), job, fingerprint)


def cmd_prepare(args: argparse.Namespace) -> int:
    config = load_config(args.config)
    output, fingerprint = prepare_config(config)
    selected = require_benchmark(config)
    profile = require_profile(config, selected.profile)
    task_count, attempts = stage_shape(selected.stage, selected.approve_full)
    print(
        json.dumps(
            {
                "ready": True,
                "profile": selected.profile,
                "model": selected.model,
                "suite": selected.suite,
                "stage": selected.stage,
                "agents": list(profile.agents),
                "trials": task_count * attempts * len(profile.agents),
                "job_config": str(output),
                "fingerprint": str(fingerprint),
                "next": "make benchmark",
            },
            indent=2,
            ensure_ascii=False,
        )
    )
    return 0


def cmd_bench(args: argparse.Namespace) -> int:
    config = load_config(args.config)
    selected = require_benchmark(config)
    profile = require_profile(config, selected.profile)
    check_api_key(profile)
    _check_local_runtime()
    output = _prepared_job_path(config)
    if not output.is_file():
        raise ConfigurationError(
            "benchmark is not prepared; run make benchmark-prepare first"
        )
    if config.path.stat().st_mtime_ns > output.stat().st_mtime_ns:
        raise ConfigurationError(
            "config.toml changed after preparation; run make benchmark-prepare again"
        )

    job = yaml.safe_load(output.read_text(encoding="utf-8"))
    run_name = _fresh_job_name(str(job["job_name"]))
    command = [
        "harbor",
        "jobs",
        "start",
        "--config",
        str(output),
        "--job-name",
        run_name,
        "--quiet",
        "--yes",
    ]
    job_dir = Path(job["jobs_dir"]) / run_name
    report_dir = config.path.parent / "reports" / run_name
    environment = os.environ.copy()
    # A provider host override is required for allowlist tasks but redundant on
    # public tasks.  Harbor warns once per public trial; suppress only that
    # expected warning so Rich can keep its aggregate progress display intact.
    warning_filters = filter(
        None,
        (
            environment.get("PYTHONWARNINGS"),
            _HARBOR_PUBLIC_ALLOWLIST_WARNING_FILTER,
        ),
    )
    environment["PYTHONWARNINGS"] = ",".join(warning_filters)
    returncode = _run_harbor_job(
        command,
        environment=environment,
        job_dir=job_dir,
        job=job,
    )

    from .report import write_reports

    try:
        paths = write_reports(job_dir, report_dir)
    except ValueError:
        paths = []

    if paths:
        print("Benchmark complete:" if returncode == 0 else "Benchmark partial report:")
        for path in paths:
            print(path)
    else:
        print(f"Benchmark incomplete; inspect {job_dir}", file=sys.stderr)
    return returncode if returncode else (0 if paths else 1)


def _read_candidate_ids(path: Path) -> list[str]:
    ids = [
        line.strip()
        for line in path.read_text(encoding="utf-8").splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]
    if len(ids) != len(set(ids)):
        raise ConfigurationError(f"candidate list has duplicate IDs: {path}")
    return ids


def cmd_freeze_manifest(args: argparse.Namespace) -> int:
    config = load_config(args.config)
    suite = require_suite(config, args.suite)
    candidates = _read_candidate_ids(args.from_file)
    if len(candidates) < suite.expected_tasks:
        raise ConfigurationError(
            f"need at least {suite.expected_tasks} eligible candidates, got {len(candidates)}"
        )
    selected = sorted(candidates)
    random.Random(args.seed).shuffle(selected)
    selected = selected[: suite.expected_tasks]
    rendered = (
        f"# Frozen by magent-bench; seed={args.seed}; source={args.from_file}\n"
        + "\n".join(selected)
        + "\n"
    )
    if args.approve_write:
        suite.manifest.write_text(rendered, encoding="utf-8")
        print(suite.manifest)
    else:
        print(rendered, end="")
        print(
            "dry run: pass --approve-write to replace the suite manifest",
            file=sys.stderr,
        )
    return 0


def cmd_report(args: argparse.Namespace) -> int:
    from .report import write_reports

    paths = write_reports(args.jobs_dir, args.output_dir, bootstrap=args.bootstrap)
    for path in paths:
        print(path)
    return 0


def cmd_prepare_elpa(args: argparse.Namespace) -> int:
    output = prepare_elpa_bundle(args.from_dir, args.output)
    print(output)
    return 0


def cmd_prepare_emacs(args: argparse.Namespace) -> int:
    config = load_config(args.config)
    selected = require_benchmark(config)
    if selected.emacs_bundle is None:
        raise ConfigurationError("set benchmark.emacs_bundle in config.toml")
    output = prepare_emacs_bundle(
        config.path.parent / "emacs-bundle.Dockerfile",
        selected.emacs_bundle,
        proxy=selected.proxy,
    )
    print(output)
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="magent-bench")
    sub = parser.add_subparsers(dest="command", required=True)

    prepare = sub.add_parser(
        "prepare", help="validate config.toml and generate the one prepared Harbor job"
    )
    prepare.add_argument("--config", type=Path)
    prepare.set_defaults(func=cmd_prepare)

    bench = sub.add_parser(
        "bench", help="run the prepared Harbor job and write its report"
    )
    bench.add_argument("--config", type=Path)
    bench.set_defaults(func=cmd_bench)

    profiles = sub.add_parser("profiles", help="list provider profiles")
    profiles.add_argument("--config", type=Path)
    profiles.set_defaults(func=cmd_profiles)

    freeze = sub.add_parser(
        "freeze-manifest", help="deterministically select from eligible task IDs"
    )
    freeze.add_argument("--config", type=Path)
    freeze.add_argument("--suite", required=True)
    freeze.add_argument("--from-file", type=Path, required=True)
    freeze.add_argument("--seed", type=int, default=20260720)
    freeze.add_argument("--approve-write", action="store_true")
    freeze.set_defaults(func=cmd_freeze_manifest)

    report = sub.add_parser("report", help="summarize Harbor results without a composite")
    report.add_argument("jobs_dir", type=Path)
    report.add_argument("--output-dir", type=Path, default=Path("reports"))
    report.add_argument("--bootstrap", type=int, default=10_000)
    report.set_defaults(func=cmd_report)

    prepare_elpa = sub.add_parser(
        "prepare-elpa", help="copy exact local Magent dependencies into a run bundle"
    )
    prepare_elpa.add_argument(
        "--from-dir", type=Path, default=Path("~/.emacs.d/elpa")
    )
    prepare_elpa.add_argument("--output", type=Path, required=True)
    prepare_elpa.set_defaults(func=cmd_prepare_elpa)

    prepare_emacs = sub.add_parser(
        "prepare-emacs", help="build the pinned portable Emacs bundle"
    )
    prepare_emacs.add_argument("--config", type=Path)
    prepare_emacs.set_defaults(func=cmd_prepare_emacs)
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        return int(args.func(args))
    except (ConfigurationError, OSError, ValueError) as exc:
        parser.error(str(exc))
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
