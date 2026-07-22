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
from typing import Sequence

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


def _check_local_runtime() -> None:
    for command in ("docker", "harbor"):
        if shutil.which(command) is None:
            raise ConfigurationError(
                f"{command} is required; install it before running make"
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
    """Validate config.toml and write the one Harbor job consumed by make bench."""
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
                "next": "make bench",
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
        raise ConfigurationError("benchmark is not prepared; run make first")
    if config.path.stat().st_mtime_ns > output.stat().st_mtime_ns:
        raise ConfigurationError("config.toml changed after preparation; run make again")

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
    try:
        returncode = subprocess.run(command, check=False).returncode
    except KeyboardInterrupt:
        returncode = 130

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
